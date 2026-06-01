# =============================================================================
# UK Gilt Rates, Sterling & Rolling Correlations
# Built step by step:
#   Step 1: Gilt rates vs GBP/USD (levels)
#   Step 2: Rolling correlation — raw (30 and 60-day)
#   Step 3: Adjusted for global factors (US 10y, DXY)
#   Step 4: SPX experiment — does GBP move with S&P 500?
# =============================================================================

library(fredr)
library(tidyverse)
library(readxl)
library(slider)
library(patchwork)
library(plotly)
# --- 0. API KEY ---------------------------------------------------------------
# Set your FRED API key (or store in ~/.Renviron as FRED_API_KEY=xxxx)
fredr_set_key("5346872b52d8ee5650cfaf478a684491")

# =============================================================================
# PART 0: FUNCTIONS & DATA LOADING
# =============================================================================

# ── BoE loader ────────────────────────────────────────────────────────────────
load_boe_sheet <- function(path, sheet = "fwd curve", maturities = c(5, 10, 20, 25)) {
  available_sheets <- excel_sheets(path)
  sheet_name <- available_sheets[grepl(sheet, available_sheets, fixed = TRUE)][1]
  if (is.na(sheet_name)) stop("Sheet '", sheet, "' not found in ", basename(path))
  message("  Using sheet '", sheet_name, "' in ", basename(path))
  df      <- read_excel(path, sheet = sheet_name, skip = 5, col_names = FALSE)
  ncols   <- ncol(df)
  max_mat <- (ncols - 1) * 0.5
  mats_ok <- maturities[maturities <= max_mat]
  if (length(mats_ok) == 0) { message("    Skipping"); return(NULL) }
  mat_cols  <- as.integer(1 + mats_ok / 0.5)
  mat_names <- paste0("y", mats_ok)
  df |>
    select(c(1, mat_cols)) |>
    set_names(c("date", mat_names)) |>
    mutate(date = as.Date(date)) |>
    filter(!is.na(date)) |>
    filter(if_any(-date, ~ !is.na(.)))
}

load_boe_all <- function(folder = ".", sheet = "fwd curve", maturities = c(5, 10, 20, 25)) {
  files <- list.files(folder, pattern = "GLC Nominal daily.*\\.xlsx$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) stop("No matching files found in: ", folder)
  map_dfr(files, load_boe_sheet, sheet = sheet, maturities = maturities) |>
    arrange(date) |>
    distinct(date, .keep_all = TRUE)
}

# ── FRED helper ───────────────────────────────────────────────────────────────
pull_fred <- function(id, name) {
  fredr(series_id = id, frequency = "d") |>
    select(date, value) |>
    rename(!!name := value)
}

# ── Load BoE Gilt Yields ──────────────────────────────────────────────────────────────────
boe_folder <- "/Users/kavyasaxena/Downloads/glcnominalddata"  # ← your folder
gilt_fwd   <- load_boe_all(boe_folder, sheet = "fwd curve", maturities = c(5, 10, 20, 25))
glimpse(gilt_fwd)
range(gilt_fwd$date)

# Check Gilt Yield Chart:


cat("Forward rates — date range:", format(range(gilt_fwd$date)), "\n")
cat("Rows:", nrow(gilt_fwd), "\n\n")

print(head(gilt_fwd))
print(tail(gilt_fwd))

# Quick sanity plot
gilt_fwd |>
  pivot_longer(-date, names_to = "maturity", values_to = "rate") |>
  mutate(maturity = factor(maturity,
                           levels = paste0("y", c(5, 10, 20, 25)),
                           labels = c("5y", "10y", "20y", "25y"))) |>
  ggplot(aes(x = date, y = rate, colour = maturity)) +
  geom_line(linewidth = 0.6) +
  labs(title  = "UK Instantaneous Nominal Forward Rates",
       y      = "Rate (%)", x = NULL, colour = NULL) +
  theme_minimal(base_size = 11)

# ── Load FRED ─────────────────────────────────────────────────────────────────
message("Pulling FRED data...")
gbp_usd    <- pull_fred("DEXUSUK",  "gbp_usd")       # GBP/USD spot (USD per GBP)
gbp_twi    <- pull_fred("DTWEXBGS", "dxy")            # Broad dollar index (proxy for TWI)
us_yield   <- pull_fred("DGS10",    "us_yield_10y")   # US 10y Treasury
sp500      <- pull_fred("SP500",    "sp500")           # S&P 500
eur_usd <- pull_fred("DEXUSEU", "eur_usd") # EUR/USD spot
message("Done.")

# ── Join everything into one daily dataset ────────────────────────────────────
daily <- gilt_fwd |>
  left_join(gbp_usd,  by = "date") |>
  left_join(gbp_twi,  by = "date") |>
  left_join(us_yield, by = "date") |>
  left_join(sp500,    by = "date") |>
  left_join(eur_usd,    by = "date") |>
  arrange(date) |>
  # --- Daily changes ----------------------------------------------------------
mutate(
  d_y5       = y5  - lag(y5),
  d_y10      = y10 - lag(y10),
  d_y20      = y20 - lag(y20),
  d_y25      = y25 - lag(y25),
  
  # GBP/USD: depreciation = POSITIVE (DEXUSUK falls when GBP weakens)
  d_gbp_dep  = -(gbp_usd - lag(gbp_usd)) / lag(gbp_usd) * 100,
  
  # Dollar index: rise = dollar strengthens = GBP weakens
  d_dxy      = dxy - lag(dxy),
  
  # US 10y Treasury change
  d_us_yield = us_yield_10y - lag(us_yield_10y),
  
  # S&P 500 daily return
  d_sp500    = (sp500 - lag(sp500)) / lag(sp500) * 100
) |>
  filter(!is.na(d_y10), !is.na(d_gbp_dep))

glimpse(daily)
# ── Shared theme ──────────────────────────────────────────────────────────────
theme_clean <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.35),
      axis.line.x        = element_line(colour = "grey60", linewidth = 0.35),
      axis.ticks.x       = element_line(colour = "grey60"),
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(colour = "grey40", size = 10),
      legend.position    = "bottom",
      legend.key.width   = unit(1.2, "cm"),
      plot.caption       = element_text(colour = "grey50", size = 8)
    )
}

# ── Crisis episode lines ──────────────────────────────────────────────────────
crisis_dates <- tribble(
  ~date,                  ~short,
  as.Date("1992-09-16"),  "1992",
  as.Date("2008-09-15"),  "GFC",
  as.Date("2022-09-23"),  "2022"
)

add_crisis <- function(p, ypos = 0.92) {
  p +
    geom_vline(data = crisis_dates, aes(xintercept = date),
               colour = "grey45", linetype = "dotted",
               linewidth = 0.5, inherit.aes = FALSE) +
    geom_label(data = crisis_dates, aes(x = date, y = ypos, label = short),
               inherit.aes = FALSE, size = 2.4, colour = "grey25",
               fill = "white", label.size = 0.2,
               label.padding = unit(0.15, "lines"))
}

# ── Rolling correlation helper ────────────────────────────────────────────────
rolling_cor <- function(x, y, window) {
  slider::slide_dbl(
    .x = seq_along(x),
    .f = ~ cor(x[.x], y[.x], use = "complete.obs"),
    .before   = window - 1,
    .complete = TRUE
  )
}

# =============================================================================
# STEP 1: 10y GILT vs GBP/USD — dual y-axis, interactive
# =============================================================================
# Convention:
#   Left  axis: 10y gilt forward rate (%) — higher = yields rising
#   Right axis: GBP/USD                   — higher = GBP stronger
#
# Confidence crisis signal = yields rise AND GBP/USD falls at the same time
# i.e. blue line goes UP while red line goes DOWN

gbp_range   <- range(daily$gbp_usd, na.rm = TRUE)
yield_range <- range(daily$y10,     na.rm = TRUE)

# Scale GBP/USD onto the yield axis (no inversion)
scale_gbp   <- function(x) {
  yield_range[1] + (yield_range[2] - yield_range[1]) *
    (x - gbp_range[1]) / (gbp_range[2] - gbp_range[1])
}

unscale_gbp <- function(x) {
  gbp_range[1] + (x - yield_range[1]) /
    (yield_range[2] - yield_range[1]) * (gbp_range[2] - gbp_range[1])
}

p1 <- daily |>
  filter(!is.na(y10), !is.na(gbp_usd)) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = y10,                  colour = "10y gilt fwd (left)"),
            linewidth = 0.55, alpha = 0.9) +
  geom_line(aes(y = scale_gbp(gbp_usd),  colour = "GBP/USD (right)"),
            linewidth = 0.55, alpha = 0.85) +
  scale_y_continuous(
    name     = "10y gilt forward rate (%)",
    sec.axis = sec_axis(
      transform = ~ unscale_gbp(.),
      name      = "GBP/USD (higher = stronger GBP)"
    )
  ) +
  scale_colour_manual(values = c(
    "10y gilt fwd (left)" = "#1a6ea8",
    "GBP/USD (right)"     = "#cc3311"
  )) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title    = "UK 10y Gilt Forward Rate vs GBP/USD",
    subtitle = "Crisis signal: blue (yields) rising while red (GBP/USD) falling simultaneously.",
    x        = NULL,
    colour   = NULL,
    caption  = "Sources: Bank of England; FRED (DEXUSUK). DEXUSUK = USD per GBP."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold"),
    axis.title.y.left  = element_text(colour = "#1a6ea8"),
    axis.title.y.right = element_text(colour = "#cc3311")
  )

ggplotly(p1) |> layout(hovermode = "x unified")

# =============================================================================
# STEP 2: ROLLING CORRELATION — interactive
# =============================================================================
# Positive correlation = yields rising while GBP/USD falling (GBP weakening)
# This is the confidence crisis signal

daily <- daily |>
  mutate(
    rcor_30d = rolling_cor(d_y10, d_gbp_dep, 30),
    rcor_60d = rolling_cor(d_y10, d_gbp_dep, 60)
  )

p2 <- daily |>
  select(date, `30-day` = rcor_30d, `60-day` = rcor_60d) |>
  pivot_longer(-date, names_to = "window", values_to = "rho") |>
  filter(!is.na(rho)) |>
  ggplot(aes(x = date, y = rho, colour = window)) +
  geom_hline(yintercept =  0,   colour = "grey20", linewidth = 0.5) +
  geom_hline(yintercept =  0.3, colour = "#cc3311", linewidth = 0.35,
             linetype = "dashed") +
  geom_hline(yintercept = -0.3, colour = "#cc3311", linewidth = 0.35,
             linetype = "dashed") +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  scale_colour_manual(values = c("30-day" = "#e8a838", "60-day" = "#1a6ea8")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  labs(
    title    = "Rolling correlation: Δ10y gilt forward vs Δ GBP depreciation",
    subtitle = "Positive = yields rising while GBP/USD falling (GBP weakening). Dashed lines: ±0.3 threshold.",
    x        = NULL,
    y        = "Pearson ρ",
    colour   = "Window",
    caption  = "Sources: Bank of England; FRED. Depreciation = positive (fall in USD per GBP)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold")
  )

ggplotly(p2) |> layout(hovermode = "x unified")


# =============================================================================
# STEP 3:  ADJUSTED FOR GLOBAL FACTORS
# =============================================================================
# Remove global term premium from gilt moves:  residual of Δy10 ~ ΔUS10y
# Remove dollar factor from GBP:               residual of Δgbp_dep ~ ΔDXY
# Rolling correlation of the two residuals = UK-specific co-movement

fit_yield <- lm(d_y10     ~ d_us_yield, data = daily, na.action = na.exclude)
fit_fx    <- lm(d_gbp_dep ~ d_dxy,      data = daily, na.action = na.exclude)

# Step 1: add residuals only
daily <- daily |>
  mutate(
    y10_resid     = residuals(fit_yield),
    gbp_dep_resid = residuals(fit_fx)
  )

# Check how many NAs in each
sum(is.na(daily$y10_resid))
sum(is.na(daily$gbp_dep_resid))

daily <- daily |>
  mutate(
    y10_resid     = residuals(fit_yield),   # UK-specific gilt move
    gbp_dep_resid = residuals(fit_fx),       # Sterling-specific depreciation
    rcor_adj_30d  = rolling_cor(y10_resid, gbp_dep_resid, 30),
    rcor_adj_60d  = rolling_cor(y10_resid, gbp_dep_resid, 60)
  )

p3 <- daily |>
  select(date,
         `30-day raw`      = rcor_30d,
         `60-day raw`      = rcor_60d,
         `30-day adjusted` = rcor_adj_30d,
         `60-day adjusted` = rcor_adj_60d) |>
  pivot_longer(-date, names_to = "series", values_to = "rho") |>
  mutate(
    window = if_else(str_detect(series, "30"), "30-day", "60-day"),
    type   = if_else(str_detect(series, "adj"), "Adjusted", "Raw")
  ) |>
  filter(!is.na(rho)) |>
  ggplot(aes(x = date, y = rho, colour = type, alpha = window)) +
  geom_hline(yintercept = 0,    colour = "grey20", linewidth = 0.5) +
  geom_hline(yintercept =  0.3, colour = "grey40", linewidth = 0.4,
             linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = c("Raw" = "#1a6ea8", "Adjusted" = "#cc3311")) +
  scale_alpha_manual(values  = c("30-day" = 0.6, "60-day" = 1.0)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  labs(
    title    = "Step 3: Raw vs adjusted rolling correlation (global factors removed)",
    subtitle = "Adjusted: gilt moves residualised on US 10y; GBP moves residualised on broad dollar index.",
    x = NULL, y = "Pearson ρ", colour = "Series", alpha = "Window",
    caption  = "Sources: Bank of England; FRED (DGS10, DTWEXBGS)."
  ) +
  theme_clean()
p3 <- add_crisis(p3)

print(p3)

# =============================================================================
# STEP 4:  SPX EXPERIMENT
# =============================================================================
# Question: does GBP/USD move with the S&P 500?
# If GBP tracks global risk appetite (SPX up = GBP up), then SPX changes
# partly explain GBP moves even after removing the dollar factor.
# Method: replace DXY with SPX as the FX control, then re-run rolling correlation.
#         Compare with DXY-adjusted version — if similar, GBP is a risk-on currency.
#         If different, GBP has a component beyond global risk appetite.

# Regression 1: GBP ~ DXY              (removes dollar factor)
# Regression 2: GBP ~ SPX              (removes global risk appetite)
# Regression 3: GBP ~ DXY + SPX        (removes both — the most conservative)

fit_fx_dxy     <- lm(d_gbp_dep ~ d_dxy,           data = daily, na.action = na.exclude)
fit_fx_spx     <- lm(d_gbp_dep ~ d_sp500,          data = daily, na.action = na.exclude)
fit_fx_both    <- lm(d_gbp_dep ~ d_dxy + d_sp500,  data = daily, na.action = na.exclude)

cat("\n--- How much does SPX explain GBP/USD moves? ---\n")
cat("DXY alone:      R² =", round(summary(fit_fx_dxy)$r.squared,  3), "\n")
cat("SPX alone:      R² =", round(summary(fit_fx_spx)$r.squared,  3), "\n")
cat("DXY + SPX:      R² =", round(summary(fit_fx_both)$r.squared, 3), "\n")

daily <- daily |>
  mutate(
    gbp_dep_resid_spx  = residuals(fit_fx_spx),      # GBP after removing SPX
    gbp_dep_resid_both = residuals(fit_fx_both),      # GBP after removing DXY + SPX
    rcor_spx_30d  = rolling_cor(y10_resid, gbp_dep_resid_spx,  30),
    rcor_spx_60d  = rolling_cor(y10_resid, gbp_dep_resid_spx,  60),
    rcor_both_30d = rolling_cor(y10_resid, gbp_dep_resid_both, 30),
    rcor_both_60d = rolling_cor(y10_resid, gbp_dep_resid_both, 60)
  )

# Plot: compare three FX adjustments side by side (60-day window)
p4 <- daily |>
  select(date,
         `GBP adj. for DXY`      = rcor_adj_60d,
         `GBP adj. for SPX`      = rcor_spx_60d,
         `GBP adj. for DXY+SPX`  = rcor_both_60d) |>
  pivot_longer(-date, names_to = "adjustment", values_to = "rho") |>
  filter(!is.na(rho)) |>
  ggplot(aes(x = date, y = rho, colour = adjustment)) +
  geom_hline(yintercept = 0,    colour = "grey20", linewidth = 0.5) +
  geom_hline(yintercept =  0.3, colour = "grey40", linewidth = 0.4,
             linetype = "dashed") +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  scale_colour_manual(values = c(
    "GBP adj. for DXY"     = "#1a6ea8",
    "GBP adj. for SPX"     = "#e8a838",
    "GBP adj. for DXY+SPX" = "#cc3311"
  )) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  labs(
    title    = "Step 4: SPX experiment — which FX adjustment isolates UK-specific signal?",
    subtitle = paste0("60-day rolling correlation of residual gilt move vs residual GBP depreciation.\n",
                      "Gilt residualised on US 10y throughout. GBP residualised three ways."),
    x = NULL, y = "Pearson ρ", colour = "FX adjustment",
    caption  = "Sources: Bank of England; FRED. SPX = S&P 500 daily return."
  ) +
  theme_clean()
p4 <- add_crisis(p4)

# Also: scatter plot of GBP vs SPX to visualise the relationship directly
p4b <- daily |>
  filter(!is.na(d_sp500), !is.na(d_gbp_dep)) |>
  filter(date >= as.Date("1990-01-01")) |>   # SPX data richer post-1990
  ggplot(aes(x = d_sp500, y = d_gbp_dep)) +
  geom_point(alpha = 0.15, size = 0.6, colour = "#1a6ea8") +
  geom_smooth(method = "lm", colour = "#cc3311", linewidth = 0.8, se = TRUE) +
  labs(
    title    = "GBP/USD daily moves vs S&P 500 daily returns (1990–present)",
    subtitle = "Each dot = one trading day. Slope estimates how much GBP tracks global risk appetite.",
    x = "S&P 500 daily return (%)", y = "GBP depreciation (%)",
    caption  = "FRED: DEXUSUK, SP500. GBP depreciation defined as positive."
  ) +
  theme_clean()

step4 <- (p4 / p4b) +
  plot_annotation(
    title = "Step 4: Does GBP move with the S&P 500?",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

print(step4)
