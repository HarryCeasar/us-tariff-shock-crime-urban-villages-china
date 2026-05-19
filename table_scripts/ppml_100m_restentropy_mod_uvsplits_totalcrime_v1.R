library(data.table)
library(fixest)

# Set paths
ROOT <- "."
MAIN_CSV <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
CTRL_CSV <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv")
OUT_TABLE <- file.path(ROOT, "output_tables")
LOGFILE <- file.path(OUT_TABLE, "ppml_100m_restentropy_mod_uvsplits_totalcrime_v1.log")

# Create output directory
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(ROOT, "tmp_cache"), recursive = TRUE, showWarnings = FALSE)

# Open log
sink(LOGFILE)
cat("Starting PPML analysis: Restaurant density and cuisine entropy moderation on total crime\n")
cat("Replicating specification from ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv\n")
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
               "cat_drugs", "cat_migration")
main_dt <- fread(MAIN_CSV, select = main_cols)
cat(sprintf("Main data: %d observations\n", nrow(main_dt)))

cat("Reading controls (cuisine entropy, restaurant density)...\n")
ctrl_cols <- c("cell_id", "period", "code12", "cuisine_entropy_2017", "n_restaurants_2017")
ctrl_dt <- fread(CTRL_CSV, select = ctrl_cols)
cat(sprintf("Controls data: %d observations\n", nrow(ctrl_dt)))

cat("Merging data...\n")
dt <- merge(main_dt, ctrl_dt, by = c("cell_id", "period"), all.x = TRUE)
cat(sprintf("Merged data: %d observations\n\n", nrow(dt)))

# =========================================================
# Generate variables
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
cat(sprintf("Categories created: property, violent, corporate, underground\n\n"))

# Moderators: cuisine entropy and restaurant density
# Standardize moderators for interpretation
dt[, z_entropy := {
  mu <- mean(cuisine_entropy_2017, na.rm = TRUE)
  sd_val <- sd(cuisine_entropy_2017, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (cuisine_entropy_2017 - mu) / sd_val
}]

dt[, z_restdens := {
  mu <- mean(n_restaurants_2017, na.rm = TRUE)
  sd_val <- sd(n_restaurants_2017, na.rm = TRUE)
  if (sd_val == 0) rep(NA_real_, .N) else (n_restaurants_2017 - mu) / sd_val
}]

# Define subsamples (UV splits)
# uv_urban: UV grids in urban areas
# uv_nonurban: UV grids in non-urban areas
# uv_all: all UV grids (union of urban and non-urban UV)
dt[, sample_group := NA_character_]
dt[is_uv == 1 & is_urban == 1, sample_group := "uv_urban"]
dt[is_uv == 1 & is_urban == 0, sample_group := "uv_nonurban"]
# For uv_all, we need to track it separately - create a flag
dt[, is_uv_all := (is_uv == 1)]

cat("\n=== Sample Distribution ===\n")
sample_counts <- dt[!is.na(sample_group), .N, by = sample_group]
print(sample_counts)
uv_all_count <- dt[is_uv == 1, .N]
cat(sprintf("uv_all (total UV grids): %d\n\n", uv_all_count))

# =========================================================
# Function to run PPML with triple interaction
# =========================================================
run_ppml_triple <- function(data, yvar, moderator_var, mod_label) {
  # Filter complete cases
  dt_clean <- data[!is.na(get(yvar)) & !is.na(z_us4) & !is.na(post) &
                   !is.na(lnpop) & !is.na(county_code) & !is.na(city_period) &
                   !is.na(get(moderator_var))]

  if (nrow(dt_clean) == 0) {
    return(NULL)
  }

  result <- tryCatch({
    # Triple interaction: z_us4 * post * moderator
    model <- fepois(
      as.formula(paste0(yvar, " ~ z_us4 * post * ", moderator_var, 
                        " | county_code + city_period + offset(lnpop)")),
      data = dt_clean,
      cluster = ~county_code
    )

    summ <- summary(model)

    # Extract triple interaction coefficient: z_us4:post:moderator
    coef_name <- paste0("z_us4:post:", moderator_var)
    if (!(coef_name %in% names(summ$coefficients))) {
      cat(sprintf("  WARNING: Triple interaction term '%s' not found\n", coef_name))
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
    cat(sprintf("  ERROR: %s\n", e$message))
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
# Run regressions
# =========================================================
cat("\n=== Running PPML Regressions ===\n")

subsamples <- c("uv_all", "uv_urban", "uv_nonurban")

# Define yblocks: merged4 (4 major categories) and totalcrime
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

moderators <- list(
  list(var = "z_entropy", label = "entropy"),
  list(var = "z_restdens", label = "restdens")
)

# Store results
results_list <- list()

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
        
        res <- run_ppml_triple(dt_sub, yvar, mod_var, mod_label)
        
        if (!is.null(res)) {
          cat(sprintf("      %s: Coef=%.4f, SE=%.4f, p=%.6f, N=%d\n",
                      mod_label, res$coef, res$se, res$p, res$N))
          
          results_list[[length(results_list) + 1]] <- data.frame(
            sample = ss,
            yblock = yblock_name,
            ycat = ycat,
            moderator = mod_label,
            b_triple = res$coef,
            se_triple = res$se,
            p_triple = res$p,
            N_model = as.integer(res$N),
            rc = 0,
            stringsAsFactors = FALSE
          )
        } else {
          cat(sprintf("      %s: FAILED\n", mod_label))
          
          results_list[[length(results_list) + 1]] <- data.frame(
            sample = ss,
            yblock = yblock_name,
            ycat = ycat,
            moderator = mod_label,
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

# =========================================================
# Export results
# =========================================================
cat("\n\n=== Exporting Results ===\n")

results_df <- rbindlist(results_list)

# Export CSV
out_csv <- file.path(OUT_TABLE, "ppml_100m_restentropy_mod_uvsplits_totalcrime_v1.csv")
fwrite(results_df, out_csv)
cat(sprintf("CSV exported to: %s\n", out_csv))

# Print summary table
cat("\n=== Summary Table ===\n")
cat(sprintf("%-15s %-12s %12s %12s %12s %10s\n", 
            "Sample", "Moderator", "Coef", "SE", "p-value", "N"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (i in 1:nrow(results_df)) {
  row <- results_df[i, ]
  coef_str <- format_coef(row$b_triple, row$p_triple)
  cat(sprintf("%-15s %-12s %12s %12s %12.6f %10d\n",
              row$sample, row$moderator, coef_str,
              sprintf("(%.4f)", row$se_triple), row$p_triple, row$N_model))
}

cat("\nDone: PPML moderation analysis completed.\n")
sink()

cat("Script completed successfully.\n")
