library(data.table)
library(fixest)

# Set paths
ROOT <- "e:/Codex/Tariff_shock_crime_and_Infrastructure"
MAIN_CSV <- file.path(ROOT, "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
CTRL_CSV <- file.path(ROOT, "grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv")
OUT_TABLE <- file.path(ROOT, "table/main")
LOGFILE <- file.path(OUT_TABLE, "ppml_100m_housingmod_uvsplits_alluv_v3a.log")

# Create output directory
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

# Open log
sink(LOGFILE)
cat("Starting PPML analysis: Housing moderators (v3a)\n")
cat("Version 3a: Use existing pre-event variables + construct pre deal cycle from 2015H1-2017H2 mean\n")
cat("Four moderators: pre_list_price, pre_deal_price, pre_deal_cycle, pre_price_gap\n")
cat("Two control specs: ntl_only, ntl_dist (NTL + all infrastructure distances)\n")
cat("All samples (uv_all, uv_urban, uv_nonurban) x All ycats (merged4 + totalcrime)\n")
cat(Sys.time(), "\n\n")

# =========================================================
# Read and merge data
# =========================================================
cat("Reading main data...\n")
main_cols <- c("cell_id", "county_code", "county_city", "period", "is_urban", "is_uv",
               "pop", "us_tariff_exposure_4", "crime_count",
               "cat_stealing", "cat_fraud", "cat_robbery", "cat_extortion",
               "cat_public_security", "cat_violent_crimes", "cat_traffic_felony",
               "cat_smuggling", "cat_ip_infringement", "cat_counterfeiting",
               "cat_bribery", "cat_finance", "cat_prostitution", "cat_gambling",
               "cat_drugs", "cat_migration",
               "dist_to_bus_m", "dist_to_metro_m",
               "dist_to_basic_healthcare_m", "dist_to_mid_healthcare_m", "dist_to_adv_healthcare_m",
               "dist_to_edu_pre_m", "dist_to_edu_primary_m", "dist_to_edu_secondary_m",
               "dist_to_edu_higher_m", "dist_to_edu_vocational_m",
               "dist_to_sme_bank_m", "dist_to_joint_stock_bank_m", "dist_to_state_owned_bank_m")
main_dt <- fread(MAIN_CSV, select = main_cols)
cat(sprintf("Main data: %d observations\n", nrow(main_dt)))

cat("Reading controls (housing prices, NTL)...\n")
ctrl_cols <- c("cell_id", "period", "pre_avg_list_unit_price", "pre_avg_deal_unit_price",
               "pre_avg_price_gap", "avg_deal_cycle_days",
               "ntl_dmsp_like", "ln_ntl", "ln_ntl_pre2017")
ctrl_dt <- fread(CTRL_CSV, select = ctrl_cols)
cat(sprintf("Controls data: %d observations\n", nrow(ctrl_dt)))

cat("Merging data...\n")
dt <- merge(main_dt, ctrl_dt, by = c("cell_id", "period"), all.x = TRUE)
cat(sprintf("Merged data: %d observations\n\n", nrow(dt)))

# =========================================================
# Generate time variables and crime categories
# =========================================================
dt[, year_num := as.integer(substr(period, 1, 4))]
dt[, half_num := as.integer(substr(period, 6, 1))]
dt[, post := as.integer((year_num > 2018) | (year_num == 2018 & half_num >= 2))]

# Create FE variables
dt[, period_id := as.factor(period)]
dt[, city_period := paste0(county_city, "_", period)]
dt[, county_code := as.character(county_code)]

# Standardize tariff exposure
dt[, z_us4 := {
  mu <- mean(us_tariff_exposure_4, na.rm = TRUE)
  sd_val <- sd(us_tariff_exposure_4, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (us_tariff_exposure_4 - mu) / sd_val
}]

# Create ln(pop) offset
dt[, lnpop := log(pop)]

# Create merged categories (4 major categories)
cat("Creating crime categories...\n")
dt[, cat_property := cat_stealing + cat_fraud + cat_robbery + cat_extortion]
dt[, cat_violent := cat_public_security + cat_violent_crimes + cat_traffic_felony]
dt[, cat_corporate := cat_smuggling + cat_ip_infringement + cat_counterfeiting + cat_bribery + cat_finance]
dt[, cat_underground := cat_prostitution + cat_gambling + cat_drugs + cat_migration]

# Replace NA with 0 for all category variables
all_cat_vars <- c("cat_property", "cat_violent", "cat_corporate", "cat_underground")
for (v in all_cat_vars) {
  dt[is.na(get(v)), (v) := 0]
}
cat("Categories created.\n\n")

# =========================================================
# Construct pre-event housing moderators
# =========================================================
cat("Constructing pre-event housing moderators...\n")

# For pre_deal_cycle: compute mean of avg_deal_cycle_days over 2015H1-2017H2 per cell
cat("  Computing pre_deal_cycle (mean of avg_deal_cycle_days over 2015H1-2017H2)...\n")
pre_periods <- c("2015H1", "2015H2", "2016H1", "2016H2", "2017H1", "2017H2")
dt_pre_cycle <- dt[period %in% pre_periods, .(
  pre_deal_cycle_mean = mean(avg_deal_cycle_days, na.rm = TRUE)
), by = cell_id]

# Merge back to main data
dt <- merge(dt, dt_pre_cycle, by = "cell_id", all.x = TRUE)
cat(sprintf("  Pre-deal-cycle computed for %d cells\n", sum(!is.na(dt$pre_deal_cycle_mean))))

# Now standardize all four housing moderators
# Use the pre-event values (which are constant across periods for each cell)

# 1. Pre-average listing price (already pre-computed, constant per cell)
dt[, z_pre_list_price := {
  mu <- mean(pre_avg_list_unit_price, na.rm = TRUE)
  sd_val <- sd(pre_avg_list_unit_price, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (pre_avg_list_unit_price - mu) / sd_val
}]

# 2. Pre-average deal price
dt[, z_pre_deal_price := {
  mu <- mean(pre_avg_deal_unit_price, na.rm = TRUE)
  sd_val <- sd(pre_avg_deal_unit_price, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (pre_avg_deal_unit_price - mu) / sd_val
}]

# 3. Pre-average deal cycle (constructed)
dt[, z_pre_deal_cycle := {
  mu <- mean(pre_deal_cycle_mean, na.rm = TRUE)
  sd_val <- sd(pre_deal_cycle_mean, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (pre_deal_cycle_mean - mu) / sd_val
}]

# 4. Pre-average price gap
dt[, z_pre_price_gap := {
  mu <- mean(pre_avg_price_gap, na.rm = TRUE)
  sd_val <- sd(pre_avg_price_gap, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (pre_avg_price_gap - mu) / sd_val
}]

cat("Housing moderators standardized.\n\n")

# =========================================================
# Define distance control variables
# =========================================================
dist_vars <- c("dist_to_bus_m", "dist_to_metro_m",
               "dist_to_basic_healthcare_m", "dist_to_mid_healthcare_m", "dist_to_adv_healthcare_m",
               "dist_to_edu_pre_m", "dist_to_edu_primary_m", "dist_to_edu_secondary_m",
               "dist_to_edu_higher_m", "dist_to_edu_vocational_m",
               "dist_to_sme_bank_m", "dist_to_joint_stock_bank_m", "dist_to_state_owned_bank_m")

# =========================================================
# Define subsamples
# =========================================================
dt[, sample_group := NA_character_]
dt[is_uv == 1 & is_urban == 1, sample_group := "uv_urban"]
dt[is_uv == 1 & is_urban == 0, sample_group := "uv_nonurban"]
dt[, is_uv_all := (is_uv == 1)]

cat("\n=== Sample Distribution ===\n")
sample_counts <- dt[!is.na(sample_group), .N, by = sample_group]
print(sample_counts)
uv_all_count <- dt[is_uv == 1, .N]
cat(sprintf("uv_all (total UV grids): %d\n\n", uv_all_count))

# =========================================================
# Function to run PPML with triple interaction
# =========================================================
run_ppml_triple <- function(data, yvar, moderator_var, mod_label, ctrlspec) {
  # Filter complete cases based on control specification
  if (ctrlspec == "ntl_only") {
    dt_clean <- data[!is.na(get(yvar)) & !is.na(z_us4) & !is.na(post) &
                     !is.na(lnpop) & !is.na(county_code) & !is.na(city_period) &
                     !is.na(get(moderator_var)) & !is.na(ln_ntl)]
  } else if (ctrlspec == "ntl_dist") {
    dt_clean_temp <- data[!is.na(get(yvar)) & !is.na(z_us4) & !is.na(post) &
                          !is.na(lnpop) & !is.na(county_code) & !is.na(city_period) &
                          !is.na(get(moderator_var)) & !is.na(ln_ntl)]
    # Check all distance variables are not NA
    dist_complete <- dt_clean_temp
    for (dv in dist_vars) {
      dist_complete <- dist_complete[!is.na(get(dv))]
    }
    dt_clean <- dist_complete
  } else {
    return(NULL)
  }

  if (nrow(dt_clean) == 0) {
    return(NULL)
  }

  result <- tryCatch({
    # Build formula based on control specification
    if (ctrlspec == "ntl_only") {
      formula_str <- paste0(yvar, " ~ z_us4 * post * ", moderator_var,
                            " + ln_ntl | county_code + city_period + offset(lnpop)")
    } else if (ctrlspec == "ntl_dist") {
      dist_formula_part <- paste(dist_vars, collapse = " + ")
      formula_str <- paste0(yvar, " ~ z_us4 * post * ", moderator_var,
                            " + ln_ntl + ", dist_formula_part,
                            " | county_code + city_period + offset(lnpop)")
    }

    model <- fepois(
      as.formula(formula_str),
      data = dt_clean,
      cluster = ~county_code
    )

    summ <- summary(model)

    # Extract triple interaction coefficient
    coef_name <- paste0("z_us4:post:", moderator_var)
    if (!(coef_name %in% names(summ$coefficients))) {
      return(NULL)
    }

    b <- as.numeric(summ$coefficients[coef_name])
    se <- as.numeric(summ$se[coef_name])

    if (is.na(b) || is.na(se) || se == 0) {
      return(NULL)
    }

    t_val <- b / se
    p_val <- 2 * pt(-abs(t_val), df = summ$nobs - length(summ$coefficients))

    list(
      coef = b,
      se = se,
      t = t_val,
      p = p_val,
      N = summ$nobs
    )
  }, error = function(e) {
    return(NULL)
  })

  return(result)
}

# Format coefficient with significance stars
format_coef <- function(b, p) {
  if (is.na(b) || is.na(p)) return("NA")

  stars <- ""
  if (p < 0.01) stars <- "***"
  else if (p < 0.05) stars <- "**"
  else if (p < 0.10) stars <- "*"
  else if (p < 0.15) stars <- "+"

  sprintf("%.4f%s", b, stars)
}

# =========================================================
# Run regressions for all outcomes and subsamples
# =========================================================
cat("\n=== Running PPML Regressions ===\n")

subsamples <- c("uv_all", "uv_urban", "uv_nonurban")

# Four housing moderators
moderators <- list(
  list(var = "z_pre_list_price", label = "pre_list_price", desc = "Pre Avg List Price"),
  list(var = "z_pre_deal_price", label = "pre_deal_price", desc = "Pre Avg Deal Price"),
  list(var = "z_pre_deal_cycle", label = "pre_deal_cycle", desc = "Pre Avg Deal Cycle"),
  list(var = "z_pre_price_gap", label = "pre_price_gap", desc = "Pre Avg Price Gap")
)

# Two control specifications
ctrlspecs <- c("ntl_dist", "ntl_only")

# Y-blocks: merged4 (4 categories) + totalcrime
yblocks <- list(
  list(
    name = "merged4",
    yvars = c("cat_property", "cat_violent", "cat_corporate", "cat_underground"),
    ycats = c("cat_property", "cat_violent", "cat_corporate", "cat_underground")
  ),
  list(
    name = "totalcrime",
    yvars = c("crime_count"),
    ycats = c("all")
  )
)

# Store results
results_list <- list()
total_runs <- 0
success_runs <- 0

for (ss in subsamples) {
  cat(sprintf("\n--- Subsample: %s ---\n", ss))

  # Filter data for this subsample
  if (ss == "uv_all") {
    dt_sub <- dt[is_uv_all == TRUE]
  } else if (ss == "uv_urban") {
    dt_sub <- dt[sample_group == "uv_urban"]
  } else if (ss == "uv_nonurban") {
    dt_sub <- dt[sample_group == "uv_nonurban"]
  }

  cat(sprintf("Sample size: %d\n", nrow(dt_sub)))

  # Loop over yblocks
  for (yb in yblocks) {
    yblock_name <- yb$name
    cat(sprintf("\n  Y-block: %s\n", yblock_name))

    for (i in seq_along(yb$yvars)) {
      yvar <- yb$yvars[i]
      ycat <- yb$ycats[i]

      cat(sprintf("    Outcome: %s\n", ycat))

      for (mod in moderators) {
        mod_var <- mod$var
        mod_label <- mod$label

        for (cs in ctrlspecs) {
          total_runs <- total_runs + 1

          res <- run_ppml_triple(dt_sub, yvar, mod_var, mod_label, cs)

          if (!is.null(res)) {
            success_runs <- success_runs + 1
            cat(sprintf("      %s (%s): Coef=%.4f, SE=%.4f, p=%.6f, N=%d\n",
                        mod_label, cs, res$coef, res$se, res$p, res$N))

            results_list[[length(results_list) + 1]] <- data.frame(
              sample = ss,
              yblock = yblock_name,
              ycat = ycat,
              moderator = mod_label,
              ctrlspec = cs,
              b_triple = res$coef,
              se_triple = res$se,
              p_triple = res$p,
              N_model = as.integer(res$N),
              rc = 0,
              stringsAsFactors = FALSE
            )
          } else {
            cat(sprintf("      %s (%s): FAILED\n", mod_label, cs))

            results_list[[length(results_list) + 1]] <- data.frame(
              sample = ss,
              yblock = yblock_name,
              ycat = ycat,
              moderator = mod_label,
              ctrlspec = cs,
              b_triple = NA,
              se_triple = NA,
              p_triple = NA,
              N_model = NA_integer_,
              rc = 1,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
}

cat(sprintf("\n\nTotal runs: %d, Successful: %d, Failed: %d\n",
            total_runs, success_runs, total_runs - success_runs))

# =========================================================
# Export results
# =========================================================
cat("\n\n=== Exporting Results ===\n")

results_df <- rbindlist(results_list)

# Export CSV
out_csv <- file.path(OUT_TABLE, "ppml_100m_housingmod_uvsplits_alluv_v3a.csv")
fwrite(results_df, out_csv)
cat(sprintf("CSV exported to: %s\n", out_csv))

# Print summary table organized by moderator
cat("\n=== Summary Table by Moderator ===\n")
for (mod in moderators) {
  mod_label <- mod$label
  mod_desc <- mod$desc
  cat(sprintf("\n--- %s ---\n", mod_desc))
  cat(sprintf("%-10s %-10s %-15s %-12s %-10s %12s %12s %12s %10s\n",
              "Sample", "Y-block", "Outcome", "CtrlSpec", "Moderator", "Coef", "SE", "p-value", "N"))
  cat(paste(rep("-", 100), collapse = ""), "\n")

  mod_results <- results_df[moderator == mod_label]
  for (i in 1:nrow(mod_results)) {
    row <- mod_results[i, ]
    coef_str <- format_coef(row$b_triple, row$p_triple)
    cat(sprintf("%-10s %-10s %-15s %-12s %-10s %12s %12s %12.6f %10d\n",
                row$sample, row$yblock, row$ycat, row$ctrlspec, row$moderator, coef_str,
                sprintf("(%.4f)", row$se_triple), row$p_triple, row$N_model))
  }
}

cat("\nDone: PPML housing moderation analysis v3a completed.\n")
sink()

cat("Script completed successfully.\n")
