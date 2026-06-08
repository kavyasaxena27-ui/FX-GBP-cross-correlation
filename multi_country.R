library(tidyverse)
library(slider)
library(plotly)

# =============================================================================
# LOAD DATA — replace file paths with your actual CSVs from Refinitiv
# =============================================================================

load_refinitiv <- function(path, col_name) {
  read_csv(path, skip = 1) |>
    rename(date = 1, !!col_name := 2) |>
    mutate(date = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y"))) |>
    filter(!is.na(date))
}

# 10y yields
uk_yield  <- load_refinitiv("uk_10y.csv",  "yield")
us_yield  <- load_refinitiv("us_10y.csv",  "yield")
de_yield  <- load_refinitiv("de_10y.csv",  "yield")
jp_yield  <- load_refinitiv("jp_10y.csv",  "yield")
au_yield  <- load_refinitiv("au_10y.csv",  "yield")

# Effective exchange rates (higher = stronger)
uk_eer    <- load_refinitiv("uk_eer.csv",  "eer")
us_eer    <- load_refinitiv("us_eer.csv",  "eer")
de_eer    <- load_refinitiv("de_eer.csv",  "eer")
jp_eer    <- load_refinitiv("jp_eer.csv",  "eer")
au_eer    <- load_refinitiv("au_eer.csv",  "eer")

# =============================================================================
# BUILD DAILY CHANGES PER COUNTRY
# =============================================================================

make_country <- function(yield_df, eer_df) {
  yield_df |>
    left_join(eer_df, by = "date") |>
    arrange(date) |>
    mutate(
      d_yield = yield - lag(yield),
      d_eer   = (eer - lag(eer)) / lag(eer) * 100  # positive = stronger
    ) |>
    filter(!is.na(d_yield), !is.na(d_eer))
}

uk <- make_country(uk_yield, uk_eer)
us <- make_country(us_yield, us_eer)
de <- make_country(de_yield, de_eer)
jp <- make_country(jp_yield, jp_eer)
au <- make_country(au_yield, au_eer)

# =============================================================================
# ROLLING CORRELATION — 60-day
# =============================================================================

roll_cor <- function(x, y, w = 60) {
  slide_dbl(seq_along(x),
            ~ cor(x[.x], y[.x], use = "complete.obs"),
            .before = w - 1, .complete = TRUE)
}

# Negative correlation = bad signal (yields up, EER down)
combined <- bind_rows(
  uk |> mutate(country = "UK",        rc60 = roll_cor(d_yield, d_eer)),
  us |> mutate(country = "US",        rc60 = roll_cor(d_yield, d_eer)),
  de |> mutate(country = "Germany",   rc60 = roll_cor(d_yield, d_eer)),
  jp |> mutate(country = "Japan",     rc60 = roll_cor(d_yield, d_eer)),
  au |> mutate(country = "Australia", rc60 = roll_cor(d_yield, d_eer))
)

# =============================================================================
# PLOT
# =============================================================================

crisis_dates <- tribble(
  ~date,                 ~label,
  as.Date("2008-09-15"), "GFC",
  as.Date("2016-06-23"), "Brexit",
  as.Date("2022-09-23"), "2022"
)

p <- combined |>
  filter(!is.na(rc60)) |>
  ggplot(aes(x = date, y = rc60, colour = country)) +
  geom_hline(yintercept =  0,   colour = "grey20", linewidth = 0.5) +
  geom_hline(yintercept = -0.3, colour = "#cc3311", linewidth = 0.35,
             linetype = "dashed") +
  geom_vline(data = crisis_dates, aes(xintercept = date),
             colour = "grey50", linetype = "dotted", linewidth = 0.5,
             inherit.aes = FALSE) +
  geom_label(data = crisis_dates, aes(x = date, y = 0.95, label = label),
             inherit.aes = FALSE, size = 2.4, colour = "grey30",
             fill = "white", label.size = 0.2) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  scale_colour_manual(values = c(
    "UK"        = "#1a6ea8",
    "US"        = "#cc3311",
    "Germany"   = "#e8a838",
    "Japan"     = "#5b8a3c",
    "Australia" = "#9b4fa6"
  )) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  labs(
    title    = "60-day rolling correlation: Δ10y yield vs Δ effective exchange rate",
    subtitle = "Negative = yields rising while currency weakening. UK vs other advanced economies.",
    x        = NULL, y = "Pearson ρ", colour = NULL,
    caption  = "Sources: Refinitiv. Dashed line: −0.3 threshold. EER = nominal effective exchange rate."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "bottom",
        plot.title         = element_text(face = "bold"))

ggplotly(p) |> layout(hovermode = "x unified")
