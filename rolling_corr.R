# =============================================================================
# UK Gilt vs Sterling: Rolling Correlation Exercises
# =============================================================================
# Sign conventions:
#   d_y10  = y10 - lag(y10)                            positive = yields RISING
#   d_gbp  = (gbp_usd - lag(gbp_usd)) / lag(gbp_usd)  positive = GBP UP (appreciating)
#                                                       negative = GBP DOWN (depreciating)
#
# Four quadrants:
#   d_y10 > 0 & d_gbp < 0  → yields up,   GBP down  = BAD  (crisis signal)
#   d_y10 < 0 & d_gbp > 0  → yields down, GBP up    = GOOD (benign)
#   d_y10 > 0 & d_gbp > 0  → yields up,   GBP up    (tightening / growth)
#   d_y10 < 0 & d_gbp < 0  → yields down, GBP down  (growth concerns)
#
# Rolling correlation is NEGATIVE in bad environment (series move in opposite
# directions: yields up while GBP goes down)
# =============================================================================

library(fredr)
library(tidyverse)
library(readxl)
library(slider)
library(plotly)
library(patchwork)

# fredr_set_key("YOUR_KEY_HERE")

# =============================================================================
# BLOCK 1: BOE LOADER
# =============================================================================

load_boe_sheet <- function(path, sheet = "fwd curve", maturities = c(5, 10, 20, 25)) {
  sh  <- excel_sheets(path)
  sn  <- sh[grepl(sheet, sh, fixed = TRUE)][1]
  if (is.na(sn)) stop("Sheet '", sheet, "' not found in ", basename(path))
  df  <- read_excel(path, sheet = sn, skip = 5, col_names = FALSE)
  nc  <- ncol(df)
  mok <- maturities[maturities <= (nc - 1) * 0.5]
  if (!length(mok)) return(NULL)
  df |>
    select(c(1, as.integer(1 + mok / 0.5))) |>
    set_names(c("date", paste0("y", mok))) |>
    mutate(date = as.Date(date)) |>
    filter(!is.na(date), if_any(-date, ~ !is.na(.)))
}

load_boe_all <- function(folder, sheet = "fwd curve", maturities = c(5, 10, 20, 25)) {
  files <- list.files(folder, "GLC Nominal daily.*\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No files found in: ", folder)
  map_dfr(files, load_boe_sheet, sheet = sheet, maturities = maturities) |>
    arrange(date) |> distinct(date, .keep_all = TRUE)
}

# =============================================================================
# BLOCK 2: HELPER FUNCTIONS
# =============================================================================

# Rolling Pearson correlation
roll_cor <- function(x, y, w) {
  slide_dbl(seq_along(x),
            ~ cor(x[.x], y[.x], use = "complete.obs"),
            .before = w - 1, .complete = TRUE)
}

# Fraction of BAD days: yields UP and GBP DOWN
# d_y10 > 0  AND  d_gbp < 0
roll_bad_frac <- function(d_yield, d_gbp, w) {
  slide_dbl(seq_along(d_yield),
            ~ {
              xi <- d_yield[.x]; yi <- d_gbp[.x]
              n  <- sum(!is.na(xi) & !is.na(yi))
              if (n < 5) return(NA)
              sum(xi > 0 & yi < 0, na.rm = TRUE) / n
            },
            .before = w - 1, .complete = TRUE)
}

# Fraction of GOOD days: yields DOWN and GBP UP
# d_y10 < 0  AND  d_gbp > 0
roll_good_frac <- function(d_yield, d_gbp, w) {
  slide_dbl(seq_along(d_yield),
            ~ {
              xi <- d_yield[.x]; yi <- d_gbp[.x]
              n  <- sum(!is.na(xi) & !is.na(yi))
              if (n < 5) return(NA)
              sum(xi < 0 & yi > 0, na.rm = TRUE) / n
            },
            .before = w - 1, .complete = TRUE)
}

# Build shading dataframe
# Red  = bad_frac > threshold  (yields up AND GBP down > 40% of days)
# Green = good_frac > threshold (yields down AND GBP up > 40% of days)
# Random baseline per quadrant ≈ 0.25, so 0.4 is meaningfully elevated
make_shading <- function(dates, bad_frac, good_frac, threshold = 0.4) {
  tibble(date = dates, bad = bad_frac, good = good_frac) |>
    filter(!is.na(bad), !is.na(good)) |>
    mutate(regime = case_when(
      bad  > threshold ~ "bad",
      good > threshold ~ "good",
      TRUE             ~ "neutral"
    ))
}

# Add shading to any ggplot
add_shading <- function(p, shading_df) {
  p +
    geom_rect(data = filter(shading_df, regime == "bad"),
              aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
              fill = "#cc3311", alpha = 0.08, inherit.aes = FALSE) +
    geom_rect(data = filter(shading_df, regime == "good"),
              aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
              fill = "#2e7d32", alpha = 0.08, inherit.aes = FALSE)
}

# Base theme
theme_c <- function() {
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position    = "bottom",
          plot.title         = element_text(face = "bold"),
          plot.subtitle      = element_text(colour = "grey40", size = 10),
          plot.caption       = element_text(colour = "grey50", size = 8))
}

# Standard correlation chart builder
# NOTE: bad signal is now NEGATIVE correlation (yields up, GBP down)
cor_plot <- function(data, title, subtitle = NULL) {
  data |>
    pivot_longer(-date, names_to = "window", values_to = "rho") |>
    filter(!is.na(rho)) |>
    ggplot(aes(x = date, y = rho, colour = window)) +
    geom_hline(yintercept =  0,   colour = "grey20", linewidth = 0.5) +
    geom_hline(yintercept = -0.3, colour = "#cc3311", linewidth = 0.35,
               linetype = "dashed") +   # bad signal threshold
    geom_hline(yintercept =  0.3, colour = "#2e7d32", linewidth = 0.35,
               linetype = "dashed") +   # good signal threshold
    geom_line(linewidth = 0.5, alpha = 0.85) +
    scale_colour_manual(values = c("30-day" = "#e8a838", "60-day" = "#1a6ea8")) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Pearson ρ", colour = "Window") +
    theme_c()
}

# =============================================================================
# BLOCK 3: LOAD DATA
# =============================================================================

boe_folder <- "/Users/kavyasaxena/Downloads/glcnominaldata"
gilt_fwd   <- load_boe_all(boe_folder)

gbp_usd  <- fredr("DEXUSUK", frequency = "d") |> select(date, gbp_usd = value)
eur_usd  <- fredr("DEXUSEU", frequency = "d") |> select(date, eur_usd = value)
us_yield <- fredr("DGS10",   frequency = "d") |> select(date, us10y   = value)
sp500    <- fredr("SP500",   frequency = "d") |> select(date, sp500   = value)

# Sterling ERI from BoE (manually downloaded — higher = stronger sterling)
sterling_eri <- read_csv("XUDLBK67.csv", skip = 1) |>
  rename(date = 1, eri = 2) |>
  mutate(date = as.Date(date, format = "%d %b %Y")) |>
  filter(!is.na(eri))

daily <- gilt_fwd |>
  left_join(gbp_usd,      by = "date") |>
  left_join(eur_usd,      by = "date") |>
  left_join(us_yield,     by = "date") |>
  left_join(sp500,        by = "date") |>
  left_join(sterling_eri, by = "date") |>
  arrange(date) |>
  mutate(
    d_y10    = y10 - lag(y10),
    # Standard sign: positive = GBP UP (appreciating), negative = GBP DOWN
    d_gbp    = (gbp_usd - lag(gbp_usd)) / lag(gbp_usd) * 100,
    d_eri    = (eri     - lag(eri))     / lag(eri)      * 100,
    d_us10y  = us10y - lag(us10y),
    d_eurusd = (eur_usd - lag(eur_usd)) / lag(eur_usd)  * 100,
    d_sp500  = (sp500   - lag(sp500))   / lag(sp500)    * 100
  ) |>
  filter(!is.na(d_y10))

# Sign check: on mini-budget day expect d_y10 > 0 and d_gbp < 0
cat("\n--- Sign check: 2022-09-23 ---\n")
cat("Expect: d_y10 > 0 (yields rose) and d_gbp < 0 (GBP fell)\n")
daily |>
  filter(date == as.Date("2022-09-23")) |>
  select(date, d_y10, d_gbp, gbp_usd) |>
  print()

# =============================================================================
# BLOCK 4: RESIDUALISE ON GLOBAL FACTORS
# =============================================================================

fit_y_ust    <- lm(d_y10  ~ d_us10y,            data = daily, na.action = na.exclude)
fit_gbp_eu   <- lm(d_gbp  ~ d_eurusd,           data = daily, na.action = na.exclude)
fit_gbp_sp   <- lm(d_gbp  ~ d_sp500,            data = daily, na.action = na.exclude)
fit_gbp_both <- lm(d_gbp  ~ d_eurusd + d_sp500, data = daily, na.action = na.exclude)

daily <- daily |>
  mutate(
    y10_resid      = residuals(fit_y_ust),
    gbp_resid_eu   = residuals(fit_gbp_eu),
    gbp_resid_sp   = residuals(fit_gbp_sp),
    gbp_resid_both = residuals(fit_gbp_both)
  )

cat("\nR² — EUR/USD explains GBP:", round(summary(fit_gbp_eu)$r.squared,   3), "\n")
cat("R² — SPX explains GBP:    ", round(summary(fit_gbp_sp)$r.squared,   3), "\n")
cat("R² — Both explain GBP:    ", round(summary(fit_gbp_both)$r.squared, 3), "\n")

# =============================================================================
# BLOCK 5: COMPUTE ALL ROLLING SERIES
# =============================================================================

daily <- daily |>
  mutate(
    # Rolling correlations — negative = bad signal (yields up, GBP down)
    rc_raw_30  = roll_cor(d_y10,      d_gbp,         30),
    rc_raw_60  = roll_cor(d_y10,      d_gbp,         60),
    rc_adj_30  = roll_cor(y10_resid,  gbp_resid_eu,  30),
    rc_adj_60  = roll_cor(y10_resid,  gbp_resid_eu,  60),
    rc_spx_30  = roll_cor(y10_resid,  gbp_resid_sp,  30),
    rc_spx_60  = roll_cor(y10_resid,  gbp_resid_sp,  60),

    # Fraction of bad days:  yields UP   (d_y10 > 0) AND GBP DOWN (d_gbp < 0)
    bad_frac_30  = roll_bad_frac(d_y10, d_gbp, 30),
    bad_frac_60  = roll_bad_frac(d_y10, d_gbp, 60),

    # Fraction of good days: yields DOWN (d_y10 < 0) AND GBP UP   (d_gbp > 0)
    good_frac_30 = roll_good_frac(d_y10, d_gbp, 30),
    good_frac_60 = roll_good_frac(d_y10, d_gbp, 60)
  )

# Shading: directional, based on 60-day fractions
shading <- make_shading(daily$date,
                         daily$bad_frac_60,
                         daily$good_frac_60,
                         threshold = 0.4)

cat("\n--- Shading regime counts ---\n")
print(count(shading, regime))

# =============================================================================
# EXERCISE 1: ROLLING CORRELATIONS WITH SHADING
# =============================================================================

e1a <- cor_plot(select(daily, date, `30-day` = rc_raw_30, `60-day` = rc_raw_60),
                "Raw rolling correlation",
                "Δ10y gilt forward vs Δ GBP/USD. Negative = yields up, GBP down (crisis signal).") |>
  add_shading(shading)

e1b <- cor_plot(select(daily, date, `30-day` = rc_adj_30, `60-day` = rc_adj_60),
                "Adjusted: gilt residual (US 10y) vs GBP residual (EUR/USD)",
                "Removes US term premium from yields; removes EUR/USD factor from GBP.") |>
  add_shading(shading)

e1c <- cor_plot(select(daily, date, `30-day` = rc_spx_30, `60-day` = rc_spx_60),
                "Adjusted: gilt residual (US 10y) vs GBP residual (S&P 500)",
                "Tests whether GBP co-moves with global risk appetite.") |>
  add_shading(shading)

ggplotly(e1a) |> layout(hovermode = "x unified")
ggplotly(e1b) |> layout(hovermode = "x unified")
ggplotly(e1c) |> layout(hovermode = "x unified")

# Static 3-panel
e1a / e1b / e1c +
  plot_annotation(
    title   = "Exercise 1: Rolling correlations with directional shading",
    caption = paste("Red: >40% of days have yields UP and GBP DOWN simultaneously.",
                    "Green: >40% of days have yields DOWN and GBP UP.",
                    "\nCorrelation is negative in crisis (series move in opposite directions)."),
    theme   = theme(plot.title = element_text(face = "bold"))
  )

# =============================================================================
# EXERCISE 2: FRACTION OF DAYS IN BAD vs GOOD QUADRANT
# =============================================================================

e2 <- daily |>
  select(date,
         `Bad: yields↑ GBP↓ (30d)` = bad_frac_30,
         `Bad: yields↑ GBP↓ (60d)` = bad_frac_60,
         `Good: yields↓ GBP↑ (60d)` = good_frac_60) |>
  pivot_longer(-date, names_to = "series", values_to = "frac") |>
  filter(!is.na(frac)) |>
  ggplot(aes(x = date, y = frac, colour = series)) +
  geom_hline(yintercept = 0.25, colour = "grey30", linewidth = 0.5,
             linetype = "dashed") +
  annotate("text", x = as.Date("1983-01-01"), y = 0.27,
           label = "Random baseline (0.25)", size = 2.8, colour = "grey40") +
  geom_rect(data = filter(shading, regime == "bad"),
            aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
            fill = "#cc3311", alpha = 0.07, inherit.aes = FALSE) +
  geom_rect(data = filter(shading, regime == "good"),
            aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
            fill = "#2e7d32", alpha = 0.07, inherit.aes = FALSE) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  scale_colour_manual(values = c(
    "Bad: yields↑ GBP↓ (30d)"  = "#e8a838",
    "Bad: yields↑ GBP↓ (60d)"  = "#cc3311",
    "Good: yields↓ GBP↑ (60d)" = "#2e7d32"
  )) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 0.75), labels = scales::percent_format()) +
  labs(title    = "Exercise 2: Fraction of days — yields up & GBP down vs yields down & GBP up",
       subtitle = "Bad = yields rising AND GBP falling on same day. Good = yields falling AND GBP rising.",
       x = NULL, y = "Fraction of days in window", colour = NULL,
       caption  = "Dashed = 0.25 random baseline. Sources: Bank of England; FRED.") +
  theme_c()

ggplotly(e2) |> layout(hovermode = "x unified")

# =============================================================================
# EXERCISE 3: QUADRANT SCATTER
# =============================================================================

daily_regimes <- daily |>
  filter(!is.na(bad_frac_60), !is.na(d_y10), !is.na(d_gbp)) |>
  mutate(regime = case_when(
    bad_frac_60  > 0.4 ~ "Bad (yields↑, GBP↓)",
    good_frac_60 > 0.4 ~ "Good (yields↓, GBP↑)",
    TRUE               ~ "Neutral"
  ))

e3 <- daily_regimes |>
  ggplot(aes(x = d_y10, y = d_gbp, colour = regime)) +
  # Shade bad quadrant (top-right by old convention, now top-left:
  # x > 0 = yields up, y < 0 = GBP down)
  annotate("rect", xmin = 0,    xmax = Inf,  ymin = -Inf, ymax = 0,
           fill = "#cc3311", alpha = 0.04) +
  annotate("rect", xmin = -Inf, xmax = 0,    ymin = 0,    ymax = Inf,
           fill = "#2e7d32", alpha = 0.04) +
  annotate("text", x =  0.2, y = -2.2,
           label = "Yields ↑, GBP ↓\n(bad)", colour = "#cc3311",
           size = 3, hjust = 0) +
  annotate("text", x = -0.3, y =  2.2,
           label = "Yields ↓, GBP ↑\n(good)", colour = "#2e7d32",
           size = 3, hjust = 0) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_point(alpha = 0.25, size = 0.7) +
  scale_colour_manual(values = c(
    "Bad (yields↑, GBP↓)"  = "#cc3311",
    "Good (yields↓, GBP↑)" = "#2e7d32",
    "Neutral"               = "grey60"
  )) +
  labs(title    = "Exercise 3: Daily yield changes vs GBP changes by regime",
       subtitle = "x > 0 = yields rising. y > 0 = GBP up (appreciating). y < 0 = GBP down (depreciating).",
       x        = "Δ 10y gilt forward rate (pp)",
       y        = "Δ GBP/USD (positive = GBP stronger)",
       colour   = "Regime",
       caption  = "Regime defined by 60-day rolling fraction of bad/good days (threshold 40%).") +
  theme_c() +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))

ggplotly(e3) |> layout(hovermode = "closest")

# =============================================================================
# EXERCISE 4: EPISODE ZOOM — 1992, 2016, 2022
# =============================================================================

episode_plot <- function(name, start, end) {

  ep <- daily |>
    filter(date >= as.Date(start), date <= as.Date(end)) |>
    filter(!is.na(y10), !is.na(gbp_usd))

  # Scale GBP/USD onto yield axis — no inversion, higher = stronger GBP
  gr  <- range(ep$gbp_usd, na.rm = TRUE)
  yr  <- range(ep$y10,     na.rm = TRUE)
  sc  <- function(x) yr[1] + (yr[2]-yr[1]) * (x-gr[1]) / (gr[2]-gr[1])
  usc <- function(x) gr[1] + (x-yr[1]) / (yr[2]-yr[1]) * (gr[2]-gr[1])

  ep_shading <- make_shading(ep$date, ep$bad_frac_60, ep$good_frac_60, threshold = 0.4)

  # Panel A: levels
  pA <- ep |>
    ggplot(aes(x = date)) +
    geom_line(aes(y = y10,         colour = "10y gilt fwd"), linewidth = 0.6) +
    geom_line(aes(y = sc(gbp_usd), colour = "GBP/USD"),      linewidth = 0.6) +
    scale_y_continuous(
      name     = "10y gilt fwd (%)",
      sec.axis = sec_axis(~ usc(.), name = "GBP/USD (higher = stronger GBP)")
    ) +
    scale_colour_manual(values = c("10y gilt fwd" = "#1a6ea8",
                                    "GBP/USD"      = "#cc3311")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    labs(title = name, x = NULL, colour = NULL) +
    theme_c() +
    theme(axis.title.y.left  = element_text(colour = "#1a6ea8"),
          axis.title.y.right = element_text(colour = "#cc3311"))
  pA <- add_shading(pA, ep_shading)

  # Panel B: rolling correlation + bad day fraction
  pB <- ep |>
    select(date,
           `Correlation (60d)`  = rc_raw_60,
           `Bad day frac (60d)` = bad_frac_60) |>
    pivot_longer(-date, names_to = "series", values_to = "value") |>
    filter(!is.na(value)) |>
    ggplot(aes(x = date, y = value, colour = series)) +
    geom_hline(yintercept =  0,    colour = "grey40", linewidth = 0.4) +
    geom_hline(yintercept = -0.3,  colour = "#cc3311", linewidth = 0.35,
               linetype = "dashed") +
    geom_hline(yintercept =  0.25, colour = "grey40", linewidth = 0.4,
               linetype = "dashed") +
    geom_line(linewidth = 0.6) +
    scale_colour_manual(values = c("Correlation (60d)"  = "#1a6ea8",
                                    "Bad day frac (60d)" = "#cc3311")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = NULL, y = "Value", colour = NULL,
         caption = "Dashed lines: -0.3 correlation threshold; 0.25 random baseline for bad day fraction.") +
    theme_c()
  pB <- add_shading(pB, ep_shading)

  print(pA / pB)
}

episode_plot("1992 ERM Crisis",  "1992-01-01", "1993-06-01")
episode_plot("2016 Brexit Vote", "2016-01-01", "2016-12-31")
episode_plot("2022 Mini-Budget", "2022-06-01", "2023-03-01")}

load_boe_all <- function(folder, sheet = "fwd curve", maturities = c(5, 10, 20, 25)) {
  files <- list.files(folder, "GLC Nominal daily.*\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No files found in: ", folder)
  map_dfr(files, load_boe_sheet, sheet = sheet, maturities = maturities) |>
    arrange(date) |> distinct(date, .keep_all = TRUE)
}

# =============================================================================
# BLOCK 2: HELPER FUNCTIONS
# =============================================================================

# Rolling Pearson correlation
roll_cor <- function(x, y, w) {
  slide_dbl(seq_along(x),
            ~ cor(x[.x], y[.x], use = "complete.obs"),
            .before = w - 1, .complete = TRUE)
}

# Fraction of days in bad quadrant (yields up AND GBP depreciating)
roll_bad_frac <- function(x, y, w) {
  slide_dbl(seq_along(x),
            ~ {
              xi <- x[.x]; yi <- y[.x]
              n  <- sum(!is.na(xi) & !is.na(yi))
              if (n < 5) return(NA)
              sum(xi > 0 & yi > 0, na.rm = TRUE) / n
            },
            .before = w - 1, .complete = TRUE)
}

# Build shading dataframe from a correlation vector
# Uses 60-day series to define regimes
make_shading <- function(dates, cor_vec, threshold = 0.3) {
  tibble(date = dates, rho = cor_vec) |>
    filter(!is.na(rho)) |>
    mutate(regime = case_when(
      rho >  threshold ~ "bad",
      rho < -threshold ~ "good",
      TRUE             ~ "neutral"
    ))
}

# Add shading layer to any ggplot
add_shading <- function(p, shading_df) {
  p +
    geom_rect(data = filter(shading_df, regime == "bad"),
              aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
              fill = "#cc3311", alpha = 0.07, inherit.aes = FALSE) +
    geom_rect(data = filter(shading_df, regime == "good"),
              aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
              fill = "#2e7d32", alpha = 0.07, inherit.aes = FALSE)
}

# Base theme
theme_c <- function() {
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor    = element_blank(),
          panel.grid.major.x  = element_blank(),
          legend.position     = "bottom",
          plot.title          = element_text(face = "bold"),
          plot.subtitle       = element_text(colour = "grey40", size = 10),
          plot.caption        = element_text(colour = "grey50", size = 8))
}

# Standard correlation plot (returns ggplot, not printed)
cor_plot <- function(data, title, subtitle = NULL) {
  data |>
    pivot_longer(-date, names_to = "window", values_to = "rho") |>
    filter(!is.na(rho)) |>
    ggplot(aes(x = date, y = rho, colour = window)) +
    geom_hline(yintercept =  0,   colour = "grey20", linewidth = 0.5) +
    geom_hline(yintercept =  0.3, colour = "#cc3311", linewidth = 0.35, linetype = "dashed") +
    geom_hline(yintercept = -0.3, colour = "#2e7d32", linewidth = 0.35, linetype = "dashed") +
    geom_line(linewidth = 0.5, alpha = 0.85) +
    scale_colour_manual(values = c("30-day" = "#e8a838", "60-day" = "#1a6ea8")) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Pearson ρ", colour = "Window") +
    theme_c()
}

# =============================================================================
# BLOCK 3: LOAD DATA
# =============================================================================

boe_folder   <- "/Users/kavyasaxena/Downloads/glcnominaldata"
gilt_fwd     <- load_boe_all(boe_folder)

# FRED pulls
gbp_usd   <- fredr("DEXUSUK", frequency = "d") |> select(date, gbp_usd  = value)
eur_usd   <- fredr("DEXUSEU", frequency = "d") |> select(date, eur_usd  = value)
us_yield  <- fredr("DGS10",   frequency = "d") |> select(date, us10y    = value)
sp500     <- fredr("SP500",   frequency = "d") |> select(date, sp500    = value)

# Sterling ERI from BoE (downloaded manually as CSV)
# Higher = stronger sterling
sterling_eri <- read_csv("XUDLBK67.csv", skip = 1) |>
  rename(date = 1, eri = 2) |>
  mutate(date = as.Date(date, format = "%d %b %Y")) |>
  filter(!is.na(eri))

# Join and compute daily changes
daily <- gilt_fwd |>
  left_join(gbp_usd,      by = "date") |>
  left_join(eur_usd,      by = "date") |>
  left_join(us_yield,     by = "date") |>
  left_join(sp500,        by = "date") |>
  left_join(sterling_eri, by = "date") |>
  arrange(date) |>
  mutate(
    d_y10     = y10 - lag(y10),
    # GBP/USD: positive = depreciation (GBP buying fewer USD)
    d_gbp_dep = (lag(gbp_usd) - gbp_usd) / lag(gbp_usd) * 100,
    # Sterling ERI: positive = depreciation (index falling)
    d_eri_dep = (lag(eri) - eri)          / lag(eri)     * 100,
    d_us10y   = us10y - lag(us10y),
    d_eurusd  = (lag(eur_usd) - eur_usd)  / lag(eur_usd) * 100,
    d_sp500   = (sp500 - lag(sp500))       / lag(sp500)   * 100
  ) |>
  filter(!is.na(d_y10))

# =============================================================================
# BLOCK 4: RESIDUALISE ON GLOBAL FACTORS
# =============================================================================

# Remove global term premium from gilt moves
fit_y_ust  <- lm(d_y10    ~ d_us10y,           data = daily, na.action = na.exclude)
# Remove dollar factor from GBP/USD using EUR/USD
fit_gbp_eu <- lm(d_gbp_dep ~ d_eurusd,         data = daily, na.action = na.exclude)
# Remove global risk appetite from GBP/USD using SPX
fit_gbp_sp <- lm(d_gbp_dep ~ d_sp500,          data = daily, na.action = na.exclude)
# Remove both EUR/USD and SPX from GBP
fit_gbp_both <- lm(d_gbp_dep ~ d_eurusd + d_sp500, data = daily, na.action = na.exclude)

daily <- daily |>
  mutate(
    y10_resid      = residuals(fit_y_ust),    # UK-specific yield move
    gbp_resid_eu   = residuals(fit_gbp_eu),   # GBP after removing dollar factor
    gbp_resid_sp   = residuals(fit_gbp_sp),   # GBP after removing SPX
    gbp_resid_both = residuals(fit_gbp_both)  # GBP after removing both
  )

cat("R² — EUR/USD explains GBP:", round(summary(fit_gbp_eu)$r.squared,   3), "\n")
cat("R² — SPX explains GBP:    ", round(summary(fit_gbp_sp)$r.squared,   3), "\n")
cat("R² — Both explain GBP:    ", round(summary(fit_gbp_both)$r.squared, 3), "\n")

# =============================================================================
# EXERCISE 1: ROLLING CORRELATIONS WITH SHADING
# =============================================================================
# Three versions: raw / UST+EUR adjusted / SPX adjusted

WINDOWS <- c(30, 60)

daily <- daily |>
  mutate(
    # Raw
    rc_raw_30  = roll_cor(d_y10, d_gbp_dep, 30),
    rc_raw_60  = roll_cor(d_y10, d_gbp_dep, 60),
    # Adjusted: UK yield residual vs GBP/EUR residual
    rc_adj_30  = roll_cor(y10_resid, gbp_resid_eu, 30),
    rc_adj_60  = roll_cor(y10_resid, gbp_resid_eu, 60),
    # SPX adjusted
    rc_spx_30  = roll_cor(y10_resid, gbp_resid_sp, 30),
    rc_spx_60  = roll_cor(y10_resid, gbp_resid_sp, 60)
  )

# Shading defined by 60-day raw correlation
shading <- make_shading(daily$date, daily$rc_raw_60)

e1a <- cor_plot(select(daily, date, `30-day` = rc_raw_30, `60-day` = rc_raw_60),
                "Raw rolling correlation",
                "Δ10y gilt forward vs Δ GBP/USD depreciation")
e1a <- add_shading(e1a, shading)

e1b <- cor_plot(select(daily, date, `30-day` = rc_adj_30, `60-day` = rc_adj_60),
                "Adjusted: gilt residual (UST) vs GBP residual (EUR/USD)",
                "Removes US term premium from yields; removes dollar factor from GBP")
e1b <- add_shading(e1b, shading)

e1c <- cor_plot(select(daily, date, `30-day` = rc_spx_30, `60-day` = rc_spx_60),
                "Adjusted: gilt residual (UST) vs GBP residual (S&P 500)",
                "Tests whether GBP moves with global risk appetite")
e1c <- add_shading(e1c, shading)

# Print individually (interactive)
ggplotly(e1a) |> layout(hovermode = "x unified")
ggplotly(e1b) |> layout(hovermode = "x unified")
ggplotly(e1c) |> layout(hovermode = "x unified")

# Or as static 3-panel
e1a / e1b / e1c +
  plot_annotation(title   = "Exercise 1: Rolling correlations — raw vs adjusted",
                  caption = "Red shading: bad regime (ρ > 0.3). Green: benign regime (ρ < −0.3). Based on 60-day raw correlation.",
                  theme   = theme(plot.title = element_text(face = "bold")))

# =============================================================================
# EXERCISE 2: FRACTION OF BAD DAYS IN ROLLING WINDOW
# =============================================================================
# Proportion of days in each window where yields UP and GBP DOWN simultaneously
# Random baseline = 0.25 (one of four quadrants)

daily <- daily |>
  mutate(
    bad_frac_30 = roll_bad_frac(d_y10, d_gbp_dep, 30),
    bad_frac_60 = roll_bad_frac(d_y10, d_gbp_dep, 60)
  )

e2 <- daily |>
  select(date, `30-day` = bad_frac_30, `60-day` = bad_frac_60) |>
  pivot_longer(-date, names_to = "window", values_to = "frac") |>
  filter(!is.na(frac)) |>
  ggplot(aes(x = date, y = frac, colour = window)) +
  geom_hline(yintercept = 0.25, colour = "grey30", linewidth = 0.5,
             linetype = "dashed") +
  annotate("text", x = as.Date("1985-01-01"), y = 0.27,
           label = "Random baseline (0.25)", size = 3, colour = "grey40") +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  add_shading(ggplot(), shading) +   # won't work inline — use below approach
  scale_colour_manual(values = c("30-day" = "#e8a838", "60-day" = "#1a6ea8")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title    = "Exercise 2: Fraction of bad days in rolling window",
       subtitle = "Bad day = yields rising AND GBP depreciating simultaneously. Dashed = random baseline (25%).",
       x = NULL, y = "Fraction of days", colour = "Window",
       caption = "Sources: Bank of England; FRED.") +
  theme_c()

# Add shading manually for exercise 2
e2 <- e2 +
  geom_rect(data = filter(shading, regime == "bad"),
            aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
            fill = "#cc3311", alpha = 0.07, inherit.aes = FALSE) +
  geom_rect(data = filter(shading, regime == "good"),
            aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
            fill = "#2e7d32", alpha = 0.07, inherit.aes = FALSE)

ggplotly(e2) |> layout(hovermode = "x unified")

# =============================================================================
# EXERCISE 3: QUADRANT SCATTER — yield change vs GBP depreciation
# =============================================================================
# Each point = one trading day, coloured by regime
# Top-right quadrant = bad environment

daily_regimes <- daily |>
  filter(!is.na(rc_raw_60)) |>
  mutate(regime = case_when(
    rc_raw_60 >  0.3 ~ "Bad (ρ > 0.3)",
    rc_raw_60 < -0.3 ~ "Good (ρ < −0.3)",
    TRUE             ~ "Neutral"
  ))

e3 <- daily_regimes |>
  filter(!is.na(d_y10), !is.na(d_gbp_dep)) |>
  ggplot(aes(x = d_y10, y = d_gbp_dep, colour = regime)) +
  # Quadrant shading
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#cc3311", alpha = 0.04) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#2e7d32", alpha = 0.04) +
  annotate("text", x =  0.25, y =  2.5, label = "Yields ↑, GBP ↓\n(bad)",
           colour = "#cc3311", size = 3, hjust = 0) +
  annotate("text", x = -0.35, y = -2.5, label = "Yields ↓, GBP ↑\n(good)",
           colour = "#2e7d32", size = 3, hjust = 0) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_point(alpha = 0.3, size = 0.7) +
  scale_colour_manual(values = c("Bad (ρ > 0.3)"  = "#cc3311",
                                  "Good (ρ < −0.3)" = "#2e7d32",
                                  "Neutral"         = "grey60")) +
  labs(title    = "Exercise 3: Daily yield changes vs GBP depreciation",
       subtitle = "Coloured by 60-day rolling correlation regime.",
       x        = "Δ 10y gilt forward rate (pp)",
       y        = "GBP depreciation (%)",
       colour   = "Regime",
       caption  = "Sources: Bank of England; FRED.") +
  theme_c() +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))

ggplotly(e3) |> layout(hovermode = "closest")

# =============================================================================
# EXERCISE 4: EPISODE ZOOM — 1992, 2016, 2022
# =============================================================================

episodes <- list(
  "1992 ERM Crisis"   = c("1992-06-01", "1993-03-01"),
  "2016 Brexit Vote"  = c("2016-01-01", "2016-12-31"),
  "2022 Mini-Budget"  = c("2022-06-01", "2023-03-01")
)

episode_plot <- function(name, dates) {
  df <- daily |>
    filter(date >= as.Date(dates[1]), date <= as.Date(dates[2])) |>
    select(date, y10, gbp_usd,
           `60-day raw` = rc_raw_60, `60-day adj` = rc_adj_60) |>
    filter(!is.na(y10), !is.na(gbp_usd))

  # Dual axis: yield vs GBP/USD
  gr <- range(df$gbp_usd,  na.rm = TRUE)
  yr <- range(df$y10,      na.rm = TRUE)
  sc  <- function(x) yr[1] + (yr[2]-yr[1]) * (x-gr[1]) / (gr[2]-gr[1])
  usc <- function(x) gr[1] + (x-yr[1]) / (yr[2]-yr[1]) * (gr[2]-gr[1])

  p_levels <- df |>
    ggplot(aes(x = date)) +
    geom_line(aes(y = y10,           colour = "10y gilt fwd"), linewidth = 0.6) +
    geom_line(aes(y = sc(gbp_usd),   colour = "GBP/USD"),      linewidth = 0.6) +
    scale_y_continuous(name     = "10y gilt fwd (%)",
                       sec.axis = sec_axis(~ usc(.), name = "GBP/USD")) +
    scale_colour_manual(values = c("10y gilt fwd" = "#1a6ea8", "GBP/USD" = "#cc3311")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    labs(title = name, x = NULL, colour = NULL) +
    theme_c() +
    theme(axis.title.y.left  = element_text(colour = "#1a6ea8"),
          axis.title.y.right = element_text(colour = "#cc3311"))

  p_cor <- df |>
    select(date, `60-day raw`, `60-day adj`) |>
    pivot_longer(-date, names_to = "series", values_to = "rho") |>
    filter(!is.na(rho)) |>
    ggplot(aes(x = date, y = rho, colour = series)) +
    geom_hline(yintercept = 0, colour = "grey30") +
    geom_hline(yintercept = 0.3, linetype = "dashed", colour = "#cc3311") +
    geom_line(linewidth = 0.6) +
    scale_colour_manual(values = c("60-day raw" = "#1a6ea8", "60-day adj" = "#cc3311")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = NULL, y = "ρ", colour = NULL) +
    theme_c()

  (p_levels / p_cor) |> print()
}

# Print each episode
iwalk(episodes, episode_plot)
