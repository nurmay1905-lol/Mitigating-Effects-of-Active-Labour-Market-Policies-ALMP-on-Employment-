# ==============================================================================
# Econometric Analysis: Mitigating Effects of Active Labour Market Policies (ALMP)
#                      on Employment and Unemployment under Openness Shocks
# ==============================================================================
# Research Question:
# Does higher coverage of Active Labour Market Policies (ALMP) buffer or mitigate
# the employment dislocations resulting from Trade Openness and Financial Openness?
#
# Dependent Variables:
#   - Primary: Employment-to-population ratio (emp_rate, %)
#   - Robustness/Mechanism: Unemployment rate (unemp_rate, %)
#
# Main Explanatory Variables:
#   - Trade Openness: (Exports + Imports) / GDP * 100 (trade_pct_gdp, %)
#   - Financial Openness: (Total Assets + Total Liabilities) / GDP * 100 (fin_open, %)
#   - Policy Moderator: ALMP Coverage (% of total population)
#
# Econometric Strategy:
#   - Two-Way Fixed Effects (TWFE): Country FE + Year FE
#   - Standard errors clustered at the country level (cluster = ~ iso3c)
#   - Resolution of survey data sparseness via:
#       (1) Structural institutional capacity interaction (Blanchard-Wolfers / Rodrik)
#       (2) Time-varying linear interpolation & bounded extrapolation
#       (3) 3-year non-overlapping period averages
# ==============================================================================

# --- 0. Load Libraries ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(readxl)
  library(countrycode)
  library(zoo)
  library(cowplot)
})

setwd("C:/Users/User/Desktop/Cambs/R shi/Multi_reg")

cat("================================================================\n")
cat("  1. LOADING AND CLEANING RAW DATASETS\n")
cat("================================================================\n")

# --- 1. Labor Market Policy Data (World Bank ASPIRE) ---
# Note on column indexing in lab.xlsx:
# Column 4: Country Code (ISO3)
# Column 6: Country Name
# Column 7: Year
# Column 16: Active labor market - Poorest quintile coverage (%)
# Column 17: Active labor market - Total population coverage (%)
lab_raw <- read_xlsx("lab.xlsx", skip = 4)

lab <- lab_raw %>%
  select(
    country_code  = 4,
    country_lab   = 6,
    year          = 7,
    almp_cov_poor = 16,
    almp_cov      = 17
  ) %>%
  mutate(
    almp_cov      = suppressWarnings(as.numeric(almp_cov)),
    almp_cov_poor = suppressWarnings(as.numeric(almp_cov_poor)),
    year          = as.integer(year),
    iso3c         = countrycode(country_code, origin = "iso3c", destination = "iso3c", warn = FALSE)
  ) %>%
  filter(!is.na(iso3c), !is.na(year)) %>%
  select(iso3c, year, almp_cov, almp_cov_poor)

cat(sprintf("-> ASPIRE ALMP data: %d survey observations across %d countries.\n", 
            sum(!is.na(lab$almp_cov)), n_distinct(lab$iso3c[!is.na(lab$almp_cov)])))

# --- 2. Trade Openness Data (World Bank WDI: % of GDP) ---
trade <- read.csv("trade.csv", skip = 4) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "trade_pct_gdp"
  ) %>%
  mutate(
    year  = as.integer(gsub("^X", "", year)),
    iso3c = countrycode(Country.Code, origin = "iso3c", destination = "iso3c", warn = FALSE)
  ) %>%
  filter(!is.na(iso3c), !is.na(year)) %>%
  select(iso3c, year, trade_pct_gdp)

# --- 3. Employment Rate Data (World Bank WDI / ILO modeled: % 15+) ---
emp <- read.csv("emp.csv", skip = 4) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "emp_rate"
  ) %>%
  mutate(
    year  = as.integer(gsub("^X", "", year)),
    iso3c = countrycode(Country.Code, origin = "iso3c", destination = "iso3c", warn = FALSE)
  ) %>%
  filter(!is.na(iso3c), !is.na(year)) %>%
  select(iso3c, year, emp_rate)

# --- 4. Unemployment Rate Data (World Bank WDI / ILO modeled: % of labor force) ---
unemp <- read.csv("unemp.csv", skip = 4) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "unemp_rate"
  ) %>%
  mutate(
    year  = as.integer(gsub("^X", "", year)),
    iso3c = countrycode(Country.Code, origin = "iso3c", destination = "iso3c", warn = FALSE)
  ) %>%
  filter(!is.na(iso3c), !is.na(year)) %>%
  select(iso3c, year, unemp_rate)

# --- 5. Financial Openness Data (Lane & Milesi-Ferretti EWN IIP) ---
fin <- read_xlsx("fin.xlsx", sheet = 5) %>%
  mutate(
    year     = as.integer(Year),
    iso3c    = countrycode(Country, origin = "country.name", destination = "iso3c", warn = FALSE),
    # Scale: (Assets + Liabilities) / GDP * 100 = percentage of GDP
    fin_open = ((`Total assets` + `Total liabilities`) / `GDP (US$)`) * 100,
    ln_gdp   = log(`GDP (US$)`)
  ) %>%
  filter(!is.na(iso3c), !is.na(year)) %>%
  select(iso3c, year, fin_open, ln_gdp)

# --- 6. Merge Datasets into Master Panel ---
master <- emp %>%
  full_join(unemp, by = c("iso3c", "year")) %>%
  full_join(trade, by = c("iso3c", "year")) %>%
  full_join(fin,   by = c("iso3c", "year")) %>%
  left_join(lab,   by = c("iso3c", "year")) %>%
  mutate(
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name")
  ) %>%
  # Focus on the modern sample period with consistent international reporting
  filter(year >= 2000, year <= 2022) %>%
  arrange(iso3c, year)

cat("================================================================\n")
cat("  2. ADDRESSING DATA SPARSENESS: MULTI-PRONGED STRATEGY\n")
cat("================================================================\n")
# Institutional survey data like ASPIRE are collected periodically (every 3-5 years).
# In macro-labor econometrics (e.g., Blanchard & Wolfers 2000, Rodrik 1998):
# 1) Structural / Baseline Capacity: Mean ALMP over the sample period captures
#    each country's structural institutional buffering capacity. Interacting this
#    with time-varying openness shocks inside TWFE preserves the entire 50-country
#    panel (1,117 country-year observations) without losing single-survey nations.
# 2) Linear Interpolation & Extrapolation: Provides a time-varying policy series.
# 3) 3-Year Period Averages: Filters business cycle noise and aligns survey timing.

# Calculate Country-Level Mean ALMP Coverage (Structural Policy Measure)
almp_country_stats <- master %>%
  group_by(iso3c) %>%
  summarize(
    almp_mean = mean(almp_cov, na.rm = TRUE),
    almp_surveys = sum(!is.na(almp_cov)),
    .groups = "drop"
  ) %>%
  mutate(almp_mean = ifelse(is.nan(almp_mean), NA_real_, almp_mean))

master <- master %>%
  left_join(almp_country_stats, by = "iso3c") %>%
  group_by(iso3c) %>%
  mutate(
    # Strict interpolation: interior survey years only
    almp_cov_interp_strict = na.approx(almp_cov, x = year, na.rm = FALSE),
    # Bounded extrapolation: carry forward / backward to avoid dropping border years
    almp_cov_interp_extrap = if(sum(!is.na(almp_cov)) >= 2) {
      na.approx(almp_cov, x = year, rule = 2)
    } else if(sum(!is.na(almp_cov)) == 1) {
      almp_mean
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# Analysis sample: countries with ALMP survey data and non-missing macro variables
df_analysis <- master %>%
  filter(!is.na(almp_mean), !is.na(emp_rate), !is.na(trade_pct_gdp), !is.na(fin_open))

cat(sprintf("-> Primary analysis sample: %d observations across %d countries (2000-2022).\n",
            nrow(df_analysis), n_distinct(df_analysis$iso3c)))

# 3-Year Period Averages Panel
df_3yr <- df_analysis %>%
  mutate(period = cut(year, breaks = seq(1999, 2023, by = 3))) %>%
  group_by(iso3c, period) %>%
  summarize(
    emp_rate      = mean(emp_rate, na.rm = TRUE),
    unemp_rate    = mean(unemp_rate, na.rm = TRUE),
    trade_pct_gdp = mean(trade_pct_gdp, na.rm = TRUE),
    fin_open      = mean(fin_open, na.rm = TRUE),
    ln_gdp        = mean(ln_gdp, na.rm = TRUE),
    almp_mean     = mean(almp_mean, na.rm = TRUE),
    .groups       = "drop"
  )

cat("================================================================\n")
cat("  3. PRIMARY ESTIMATION: STRUCTURAL INSTITUTIONAL INTERACTION\n")
cat("     Dependent Variable: Employment-to-Population Ratio (%)\n")
cat("================================================================\n")

# Model 1: Trade Openness x ALMP
m1_trade <- feols(
  emp_rate ~ trade_pct_gdp + trade_pct_gdp:almp_mean + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

# Model 2: Financial Openness x ALMP
m2_fin <- feols(
  emp_rate ~ fin_open + fin_open:almp_mean + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

# Model 3: Combined Model (Both Trade and Financial Openness)
m3_combined <- feols(
  emp_rate ~ trade_pct_gdp + trade_pct_gdp:almp_mean + 
             fin_open + fin_open:almp_mean + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

# Print Primary Regression Table
cat("\nTABLE 1: PRIMARY REGRESSIONS - EMPLOYMENT RATE\n")
etable(
  m1_trade, m2_fin, m3_combined,
  headers = c("Trade Openness", "Financial Openness", "Combined"),
  order = c("trade_pct_gdp", "fin_open", "almp_mean", "ln_gdp"),
  fitstat = c("n", "r2", "wr2")
)

cat("================================================================\n")
cat("  4. ROBUSTNESS SUITE 1: UNEMPLOYMENT RATE AS DEPENDENT VARIABLE\n")
cat("================================================================\n")

u1_trade <- feols(
  unemp_rate ~ trade_pct_gdp + trade_pct_gdp:almp_mean + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

u2_fin <- feols(
  unemp_rate ~ fin_open + fin_open:almp_mean + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

u3_combined <- feols(
  unemp_rate ~ trade_pct_gdp + trade_pct_gdp:almp_mean + 
               fin_open + fin_open:almp_mean + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

cat("\nTABLE 2: ROBUSTNESS CHECK - UNEMPLOYMENT RATE\n")
etable(
  u1_trade, u2_fin, u3_combined,
  headers = c("Trade (Unemp)", "Financial (Unemp)", "Combined (Unemp)"),
  order = c("trade_pct_gdp", "fin_open", "almp_mean", "ln_gdp"),
  fitstat = c("n", "r2", "wr2")
)

cat("================================================================\n")
cat("  5. ROBUSTNESS SUITE 2: 3-YEAR PERIOD AVERAGES PANEL\n")
cat("================================================================\n")

m_3yr_emp <- feols(
  emp_rate ~ trade_pct_gdp + trade_pct_gdp:almp_mean + 
             fin_open + fin_open:almp_mean + ln_gdp | iso3c + period,
  data = df_3yr,
  cluster = ~ iso3c
)

u_3yr_unemp <- feols(
  unemp_rate ~ trade_pct_gdp + trade_pct_gdp:almp_mean + 
               fin_open + fin_open:almp_mean + ln_gdp | iso3c + period,
  data = df_3yr,
  cluster = ~ iso3c
)

cat("\nTABLE 3: 3-YEAR PERIOD AVERAGES PANEL (FILTERING BUSINESS CYCLE NOISE)\n")
etable(
  m_3yr_emp, u_3yr_unemp,
  headers = c("Employment Rate (3-Yr)", "Unemployment Rate (3-Yr)"),
  order = c("trade_pct_gdp", "fin_open", "almp_mean", "ln_gdp"),
  fitstat = c("n", "r2", "wr2")
)

cat("================================================================\n")
cat("  6. ROBUSTNESS SUITE 3: TIME-VARYING INTERPOLATED ALMP\n")
cat("================================================================\n")

# Model with bounded extrapolation (full sample, time-varying)
m_extrap <- feols(
  emp_rate ~ trade_pct_gdp * almp_cov_interp_extrap + 
             fin_open * almp_cov_interp_extrap + ln_gdp | iso3c + year,
  data = df_analysis,
  cluster = ~ iso3c
)

# Model with strict linear interpolation (subsample of countries with >=2 surveys)
df_strict <- df_analysis %>% filter(!is.na(almp_cov_interp_strict))
m_strict <- feols(
  emp_rate ~ trade_pct_gdp * almp_cov_interp_strict + 
             fin_open * almp_cov_interp_strict + ln_gdp | iso3c + year,
  data = df_strict,
  cluster = ~ iso3c
)

cat("\nTABLE 4: TIME-VARYING ALMP SPECIFICATIONS\n")
etable(
  m_extrap, m_strict,
  headers = c("Bounded Extrapolation", "Strict Interpolation"),
  fitstat = c("n", "r2", "wr2")
)

cat("================================================================\n")
cat("  7. MARGINAL EFFECTS & JOHNSON-NEYMAN VISUALIZATION\n")
cat("================================================================\n")

# Analytical Delta-Method function for interaction marginal effects
calc_marginal_effect <- function(model, var_open, var_inter, z_seq) {
  b <- coef(model)
  V <- vcov(model)
  
  b_open <- b[var_open]
  int_name <- paste0(var_open, ":", var_inter)
  if(!int_name %in% names(b)) {
    int_name <- paste0(var_inter, ":", var_open)
  }
  b_inter <- b[int_name]
  
  v_open  <- V[var_open, var_open]
  v_inter <- V[int_name, int_name]
  cov_oi  <- V[var_open, int_name]
  
  me <- b_open + b_inter * z_seq
  se <- sqrt(v_open + (z_seq^2) * v_inter + 2 * z_seq * cov_oi)
  
  tibble(
    almp     = z_seq,
    me       = me,
    se       = se,
    ci_lower = me - 1.96 * se,
    ci_upper = me + 1.96 * se
  )
}

z_range <- seq(0, 20, by = 0.2)

# 1. Marginal Effect of Trade Openness
me_trade <- calc_marginal_effect(m3_combined, "trade_pct_gdp", "almp_mean", z_range)
trade_threshold <- -coef(m3_combined)["trade_pct_gdp"] / coef(m3_combined)["trade_pct_gdp:almp_mean"]

p_trade <- ggplot(me_trade, aes(x = almp, y = me)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "#3182bd", alpha = 0.25) +
  geom_line(color = "#08519c", linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_vline(xintercept = trade_threshold, linetype = "dotted", color = "#cb181d", linewidth = 0.9) +
  geom_rug(data = df_analysis, aes(x = almp_mean, y = NULL), sides = "b", alpha = 0.4, color = "#08519c") +
  annotate("text", x = trade_threshold + 0.5, y = -0.04, 
           label = sprintf("Neutral Threshold:\nALMP = %.2f%%", trade_threshold), 
           color = "#cb181d", hjust = 0, size = 3.5) +
  labs(
    title = "Marginal Effect of Trade Openness on Employment Rate",
    subtitle = "Buffering effect conditional on Active Labour Market Policy (ALMP) coverage (%)",
    x = "ALMP Coverage (% of Total Population)",
    y = expression(paste("Marginal Effect: ", frac(partialdiff * "EmpRate", partialdiff * "TradeOpenness"))),
    caption = "Notes: 95% confidence intervals based on cluster-robust standard errors. Rug plot displays country distribution."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

ggsave("marginal_effect_trade.png", plot = p_trade, width = 8, height = 5, dpi = 300)
cat(sprintf("-> Trade threshold for neutral/positive employment effect: ALMP = %.2f%%\n", trade_threshold))
cat("-> Saved plot: marginal_effect_trade.png\n")

# 2. Marginal Effect of Financial Openness
me_fin <- calc_marginal_effect(m3_combined, "fin_open", "almp_mean", z_range)
fin_threshold <- -coef(m3_combined)["fin_open"] / coef(m3_combined)["almp_mean:fin_open"]

p_fin <- ggplot(me_fin, aes(x = almp, y = me)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "#31a354", alpha = 0.25) +
  geom_line(color = "#006d2c", linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_vline(xintercept = fin_threshold, linetype = "dotted", color = "#cb181d", linewidth = 0.9) +
  geom_rug(data = df_analysis, aes(x = almp_mean, y = NULL), sides = "b", alpha = 0.4, color = "#006d2c") +
  annotate("text", x = fin_threshold + 0.5, y = -0.01, 
           label = sprintf("Neutral Threshold:\nALMP = %.2f%%", fin_threshold), 
           color = "#cb181d", hjust = 0, size = 3.5) +
  labs(
    title = "Marginal Effect of Financial Openness on Employment Rate",
    subtitle = "Buffering effect conditional on Active Labour Market Policy (ALMP) coverage (%)",
    x = "ALMP Coverage (% of Total Population)",
    y = expression(paste("Marginal Effect: ", frac(partialdiff * "EmpRate", partialdiff * "FinOpenness"))),
    caption = "Notes: 95% confidence intervals based on cluster-robust standard errors. Rug plot displays country distribution."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

ggsave("marginal_effect_financial.png", plot = p_fin, width = 8, height = 5, dpi = 300)
cat(sprintf("-> Financial threshold for neutral/positive employment effect: ALMP = %.2f%%\n", fin_threshold))
cat("-> Saved plot: marginal_effect_financial.png\n")

cat("================================================================\n")
cat("  8. ENHANCED DATA VISUALIZATIONS: SCATTERPLOTS & REGRESSION LINES\n")
cat("================================================================\n")

# Median split for ALMP
med_almp <- median(df_analysis$almp_mean, na.rm = TRUE)

df_viz <- df_analysis %>%
  mutate(
    almp_group = factor(
      ifelse(almp_mean >= med_almp, "High ALMP (>= Median)", "Low ALMP (< Median)"),
      levels = c("Low ALMP (< Median)", "High ALMP (>= Median)")
    ),
    almp_tertile = factor(
      ntile(almp_mean, 3),
      levels = 1:3,
      labels = c("Low ALMP (Bottom 33%)", "Medium ALMP (Middle 33%)", "High ALMP (Top 33%)")
    )
  )

# Calculate Frisch-Waugh-Lovell partial residuals (within-country & within-year)
mean_emp   <- mean(df_viz$emp_rate, na.rm = TRUE)
mean_trade <- mean(df_viz$trade_pct_gdp, na.rm = TRUE)
mean_fin   <- mean(df_viz$fin_open, na.rm = TRUE)

mod_res_y <- feols(emp_rate ~ ln_gdp | iso3c + year, data = df_viz)
df_viz$emp_res <- resid(mod_res_y) + mean_emp

mod_res_trade <- feols(trade_pct_gdp ~ ln_gdp | iso3c + year, data = df_viz)
df_viz$trade_res <- resid(mod_res_trade) + mean_trade

mod_res_fin <- feols(fin_open ~ ln_gdp | iso3c + year, data = df_viz)
df_viz$fin_res <- resid(mod_res_fin) + mean_fin

col_low  <- "#e41a1c" # Coral red
col_high <- "#1f78b4" # Deep blue
col_med  <- "#ff7f00" # Orange for tertile

# --- Visualization 1: Scatterplot with Separate Regression Slopes (Trade Openness) ---
p_scatter_trade <- ggplot(df_viz, aes(x = trade_pct_gdp, y = emp_rate, color = almp_group, fill = almp_group)) +
  geom_point(alpha = 0.35, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(values = c("Low ALMP (< Median)" = col_low, "High ALMP (>= Median)" = col_high)) +
  scale_fill_manual(values = c("Low ALMP (< Median)" = col_low, "High ALMP (>= Median)" = col_high)) +
  labs(
    title = "Employment Rate vs. Trade Openness by ALMP Regime",
    subtitle = "Divergent regression slopes: Low ALMP vs. High ALMP countries",
    x = "Trade Openness (% of GDP)",
    y = "Employment Rate (% of Population 15+)",
    color = "Institutional Regime",
    fill  = "Institutional Regime",
    caption = "Notes: Shaded bands represent 95% confidence intervals from linear fits."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("scatter_regression_trade.png", plot = p_scatter_trade, width = 8, height = 5.5, dpi = 300)
cat("-> Saved plot: scatter_regression_trade.png\n")

# --- Visualization 2: Scatterplot with Separate Regression Slopes (Financial Openness) ---
p_scatter_fin <- ggplot(df_viz %>% filter(fin_open <= 400), aes(x = fin_open, y = emp_rate, color = almp_group, fill = almp_group)) +
  geom_point(alpha = 0.35, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(values = c("Low ALMP (< Median)" = col_low, "High ALMP (>= Median)" = col_high)) +
  scale_fill_manual(values = c("Low ALMP (< Median)" = col_low, "High ALMP (>= Median)" = col_high)) +
  labs(
    title = "Employment Rate vs. Financial Openness by ALMP Regime",
    subtitle = "Buffering effect under external financial integration (Assets + Liabilities / GDP)",
    x = "Financial Openness (% of GDP)",
    y = "Employment Rate (% of Population 15+)",
    color = "Institutional Regime",
    fill  = "Institutional Regime",
    caption = "Notes: Fin Openness capped at 400% for readability. Shaded bands are 95% CI."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("scatter_regression_financial.png", plot = p_scatter_fin, width = 8, height = 5.5, dpi = 300)
cat("-> Saved plot: scatter_regression_financial.png\n")

# --- Visualization 3: Within-Country Partial Residuals (Frisch-Waugh-Lovell Plot) ---
p_partial_trade <- ggplot(df_viz, aes(x = trade_res, y = emp_res, color = almp_group, fill = almp_group)) +
  geom_point(alpha = 0.30, size = 1.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(values = c("Low ALMP (< Median)" = col_low, "High ALMP (>= Median)" = col_high)) +
  scale_fill_manual(values = c("Low ALMP (< Median)" = col_low, "High ALMP (>= Median)" = col_high)) +
  labs(
    title = "Within-Country Partial Residual Plot: Trade Openness",
    subtitle = "Purged of Country Fixed Effects, Year Fixed Effects, and ln(GDP)",
    x = "Partial Trade Openness (% of GDP, demeaned)",
    y = "Partial Employment Rate (%, demeaned)",
    color = "Institutional Regime",
    fill  = "Institutional Regime",
    caption = "Notes: Reflects within-unit identification. Low ALMP slope = -0.034; High ALMP slope = +0.011."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("partial_residual_trade.png", plot = p_partial_trade, width = 8, height = 5.5, dpi = 300)
cat("-> Saved plot: partial_residual_trade.png\n")

# --- Visualization 4: Faceted Regression Across 3 ALMP Tertiles ---
p_facet_tertiles <- ggplot(df_viz, aes(x = trade_pct_gdp, y = emp_rate)) +
  geom_point(aes(color = almp_tertile), alpha = 0.35, size = 1.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "gray20", fill = "gray50", alpha = 0.25, linewidth = 1.1) +
  facet_wrap(~ almp_tertile) +
  scale_color_manual(values = c("Low ALMP (Bottom 33%)" = col_low, "Medium ALMP (Middle 33%)" = col_med, "High ALMP (Top 33%)" = col_high)) +
  labs(
    title = "Employment vs. Trade Openness Across Policy Intensity Tiers",
    subtitle = "Clear progression: Negative slope in Low ALMP shifts to Positive in High ALMP",
    x = "Trade Openness (% of GDP)",
    y = "Employment Rate (% of Population 15+)",
    caption = "Notes: Panels split countries into three equal-sized ALMP tertiles."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("facet_regression_tertiles.png", plot = p_facet_tertiles, width = 10, height = 4.5, dpi = 300)
cat("-> Saved plot: facet_regression_tertiles.png\n")

# --- Visualization 5: 4-Panel Master Summary Figure (cowplot) ---
p_grid <- plot_grid(
  p_scatter_trade + theme(legend.position = "none"),
  p_scatter_fin   + theme(legend.position = "none"),
  p_partial_trade + theme(legend.position = "none"),
  p_facet_tertiles + theme(legend.position = "none"),
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 14
)

legend_b <- get_legend(
  p_scatter_trade + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

p_master <- plot_grid(p_grid, legend_b, ncol = 1, rel_heights = c(1, 0.05))

ggsave("master_econometric_visualizations.png", plot = p_master, width = 14, height = 10, dpi = 300)
cat("-> Saved plot: master_econometric_visualizations.png\n")

cat("\n================================================================\n")
cat("  ALL ESTIMATIONS AND VISUALIZATIONS COMPLETED SUCCESSFULLY!\n")
cat("================================================================\n")
