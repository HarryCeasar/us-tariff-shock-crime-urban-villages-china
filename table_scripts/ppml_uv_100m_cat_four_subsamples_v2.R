library(data.table)
library(fixest)

# Set paths
ROOT <- "."
DATA_CSV <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
OUT_TABLE <- file.path(ROOT, "output_tables")
LOGFILE <- file.path(OUT_TABLE, "ppml_uv_100m_cat_four_subsamples_v2.log")

# Create output directory if needed
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

# Open log
sink(LOGFILE)
cat("Starting PPML analysis: 100m four subsamples (UV, urban non-UV, urban UV, non-urban UV)\n")
cat(Sys.time(), "\n\n")

# Read main data
cat("Reading main data...\n")
main_cols <- c("cell_id", "county_code", "county_city", "period", "is_urban", "is_uv",
               "pop", "us_tariff_exposure_4", "crime_count",
               "cat_stealing", "cat_fraud", "cat_robbery", "cat_extortion",
               "cat_public_security", "cat_violent_crimes", "cat_traffic_felony",
               "cat_smuggling", "cat_ip_infringement", "cat_counterfeiting",
               "cat_bribery", "cat_finance", "cat_prostitution", "cat_gambling",
               "cat_drugs", "cat_migration")

dt <- fread(DATA_CSV, select = main_cols)
cat(sprintf("Total observations: %d\n\n", nrow(dt)))

# Generate time variables
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

# Create merged categories (4 major + 16 subcategories)
cat("Creating crime categories...\n")

# 4 major categories
dt[, cat_property := cat_stealing + cat_fraud + cat_robbery + cat_extortion]
dt[, cat_violent := cat_public_security + cat_violent_crimes + cat_traffic_felony]
dt[, cat_corporate := cat_smuggling + cat_ip_infringement + cat_counterfeiting + cat_bribery + cat_finance]
dt[, cat_underground := cat_prostitution + cat_gambling + cat_drugs + cat_migration]

# Replace NA with 0 for all category variables
all_cat_vars <- c("cat_property", "cat_violent", "cat_corporate", "cat_underground",
                  "cat_stealing", "cat_fraud", "cat_robbery", "cat_extortion",
                  "cat_public_security", "cat_violent_crimes", "cat_traffic_felony",
                  "cat_smuggling", "cat_ip_infringement", "cat_counterfeiting",
                  "cat_bribery", "cat_finance", "cat_prostitution", "cat_gambling",
                  "cat_drugs", "cat_migration")

for (v in all_cat_vars) {
  dt[is.na(get(v)), (v) := 0]
}

# Define 16 subcategories and 4 major categories
subcategories <- c("cat_stealing", "cat_fraud", "cat_robbery", "cat_extortion",
                   "cat_public_security", "cat_violent_crimes", "cat_traffic_felony",
                   "cat_smuggling", "cat_ip_infringement", "cat_counterfeiting",
                   "cat_bribery", "cat_finance", "cat_prostitution", "cat_gambling",
                   "cat_drugs", "cat_migration")

subcategory_labels <- c("Stealing", "Fraud", "Robbery", "Extortion",
                        "Public Security", "Violent Crimes", "Traffic Felony",
                        "Smuggling", "IP Infringement", "Counterfeiting",
                        "Bribery", "Finance", "Prostitution", "Gambling",
                        "Drugs", "Migration")

major_categories <- c("cat_property", "cat_violent", "cat_corporate", "cat_underground")
major_labels <- c("Property", "Violent", "Corporate", "Underground")

# Define four subsamples
subsamples <- c("UV", "Urban non-UV", "Urban UV", "Non-urban UV")

cat("\n=== Sample Distribution ===\n")
dt[, sample_group := NA_character_]
dt[is_uv == 1, sample_group := "UV"]
dt[is_urban == 1 & is_uv == 0, sample_group := "Urban non-UV"]
dt[is_urban == 1 & is_uv == 1, sample_group := "Urban UV"]
dt[is_urban == 0 & is_uv == 1, sample_group := "Non-urban UV"]

sample_counts <- dt[, .N, by = sample_group]
print(sample_counts)
cat("\n")

# =========================================================
# Function to run PPML and extract results
# =========================================================
run_ppml_simple <- function(data, yvar, full_sample_n) {
  # Filter complete cases - keep all observations with valid covariates
  # regardless of whether y is 0 or not
  dt_clean <- data[!is.na(get(yvar)) & !is.na(z_us4) & !is.na(post) &
                   !is.na(lnpop) & !is.na(cell_id) & !is.na(period_id) &
                   !is.na(city_period) & !is.na(county_code)]

  if (nrow(dt_clean) == 0) {
    return(NULL)
  }

  # Run PPML with high-dimensional fixed effects
  # fepois will automatically handle zero outcomes but we track the full sample
  result <- tryCatch({
    model <- fepois(
      as.formula(paste0(yvar, " ~ z_us4 * post | cell_id + period_id + city_period + offset(lnpop)")),
      data = dt_clean,
      cluster = ~county_code
    )

    summ <- summary(model)

    # Extract coefficient for interaction term
    coef_name <- "z_us4:post"
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
      p = p_val,
      N_full = full_sample_n,  # Use full sample size (all grids in subsample)
      N_regression = summ$nobs  # Actual regression sample (may be smaller due to FE dropping)
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
cat("\n=== Running Regressions ===\n")

# Store results: rows=outcomes, cols=subsamples
results_16cat <- matrix(list(), nrow = 16, ncol = 4)
rownames(results_16cat) <- subcategory_labels
colnames(results_16cat) <- subsamples

results_4cat <- matrix(list(), nrow = 4, ncol = 4)
rownames(results_4cat) <- major_labels
colnames(results_4cat) <- subsamples

# Run 16 subcategories
cat("\nRunning 16 subcategories...\n")

# Pre-calculate full sample sizes for each subsample (based on crime_count availability)
full_sample_sizes <- list()
for (j in seq_along(subsamples)) {
  subsample <- subsamples[j]
  if (subsample == "UV") {
    dt_sub <- dt[is_uv == 1 & !is.na(crime_count) & !is.na(z_us4) & !is.na(post) &
                 !is.na(lnpop) & !is.na(cell_id) & !is.na(period_id) & 
                 !is.na(city_period) & !is.na(county_code)]
  } else if (subsample == "Urban non-UV") {
    dt_sub <- dt[is_urban == 1 & is_uv == 0 & !is.na(crime_count) & !is.na(z_us4) & !is.na(post) &
                 !is.na(lnpop) & !is.na(cell_id) & !is.na(period_id) & 
                 !is.na(city_period) & !is.na(county_code)]
  } else if (subsample == "Urban UV") {
    dt_sub <- dt[is_urban == 1 & is_uv == 1 & !is.na(crime_count) & !is.na(z_us4) & !is.na(post) &
                 !is.na(lnpop) & !is.na(cell_id) & !is.na(period_id) & 
                 !is.na(city_period) & !is.na(county_code)]
  } else if (subsample == "Non-urban UV") {
    dt_sub <- dt[is_urban == 0 & is_uv == 1 & !is.na(crime_count) & !is.na(z_us4) & !is.na(post) &
                 !is.na(lnpop) & !is.na(cell_id) & !is.na(period_id) & 
                 !is.na(city_period) & !is.na(county_code)]
  }
  full_sample_sizes[[subsample]] <- nrow(dt_sub)
  cat(sprintf("  Full sample size for %s: %d\n", subsample, nrow(dt_sub)))
}
cat("\n")

for (i in seq_along(subcategories)) {
  yvar <- subcategories[i]
  ylabel <- subcategory_labels[i]
  cat(sprintf("  %s (%d/16)\n", ylabel, i))

  for (j in seq_along(subsamples)) {
    subsample <- subsamples[j]

    # Filter data
    if (subsample == "UV") {
      dt_sub <- dt[is_uv == 1]
    } else if (subsample == "Urban non-UV") {
      dt_sub <- dt[is_urban == 1 & is_uv == 0]
    } else if (subsample == "Urban UV") {
      dt_sub <- dt[is_urban == 1 & is_uv == 1]
    } else if (subsample == "Non-urban UV") {
      dt_sub <- dt[is_urban == 0 & is_uv == 1]
    }

    res <- run_ppml_simple(dt_sub, yvar, full_sample_sizes[[subsample]])
    results_16cat[i, j] <- list(res)
  }
}

# Run 4 major categories
cat("\nRunning 4 major categories...\n")
for (i in seq_along(major_categories)) {
  yvar <- major_categories[i]
  ylabel <- major_labels[i]
  cat(sprintf("  %s (%d/4)\n", ylabel, i))

  for (j in seq_along(subsamples)) {
    subsample <- subsamples[j]

    # Filter data
    if (subsample == "UV") {
      dt_sub <- dt[is_uv == 1]
    } else if (subsample == "Urban non-UV") {
      dt_sub <- dt[is_urban == 1 & is_uv == 0]
    } else if (subsample == "Urban UV") {
      dt_sub <- dt[is_urban == 1 & is_uv == 1]
    } else if (subsample == "Non-urban UV") {
      dt_sub <- dt[is_urban == 0 & is_uv == 1]
    }

    res <- run_ppml_simple(dt_sub, yvar, full_sample_sizes[[subsample]])
    results_4cat[i, j] <- list(res)
  }
}

# =========================================================
# Export Table 1: 16 subcategories x 4 subsamples
# =========================================================
cat("\n\n=== Exporting Table 1: 16 Subcategories ===\n")

out_rtf_16 <- file.path(OUT_TABLE, "ppml_100m_16cat_four_subsamples_v2.rtf")

# Build table content
lines <- c()
lines <- c(lines, "\\rtf1\\ansi\\deff0")
lines <- c(lines, "{\\fonttbl{\\f0\\fswiss\\fcharset0 Arial;}}")
lines <- c(lines, "\\fs24")
lines <- c(lines, "\\pard\\qc\\b PPML DID: 16 Crime Categories x Four Subsamples (100m) \\b0\\par")
lines <- c(lines, "\\pard\\qc Standard errors clustered at county level. Offset: ln(pop). \\par")
lines <- c(lines, "\\pard\\qc Significance: + p<0.15, * p<0.10, ** p<0.05, *** p<0.01 \\par\\par")

# Header row
header <- sprintf("\\trowd\\trgaph70")
for (k in 1:5) {
  if (k == 1) {
    header <- paste0(header, "\\cellx2500")
  } else {
    header <- paste0(header, "\\cellx4500")
  }
}
header <- paste0(header, "\\intbl\\b Crime Category \\cell\\b UV \\cell\\b Urban non-UV \\cell\\b Urban UV \\cell\\b Non-urban UV \\cell\\row")
lines <- c(lines, header)

# Data rows
for (i in 1:16) {
  row <- sprintf("\\trowd\\trgaph70\\cellx2500\\cellx4500\\cellx4500\\cellx4500\\cellx4500")
  row <- paste0(row, "\\intbl ")

  # Category name
  row <- paste0(row, subcategory_labels[i], " \\cell")

  # Four subsamples
  for (j in 1:4) {
    res <- results_16cat[i, j][[1]]
    if (!is.null(res)) {
      coef_str <- format_coef(res$coef, res$p)
      se_str <- sprintf("(%.4f)", res$se)
      cell_content <- paste0(coef_str, "\\line ", se_str)
      row <- paste0(row, cell_content, " \\cell")
    } else {
      row <- paste0(row, "NA \\cell")
    }
  }
  row <- paste0(row, "\\row")
  lines <- c(lines, row)
}

lines <- c(lines, "\\pard\\par")
lines <- c(lines, "}")

writeLines(lines, out_rtf_16, useBytes = TRUE)
cat(sprintf("Table 1 exported to: %s\n", out_rtf_16))

# =========================================================
# Export Table 2: 4 major categories x 4 subsamples
# =========================================================
cat("\n=== Exporting Table 2: 4 Major Categories ===\n")

out_rtf_4 <- file.path(OUT_TABLE, "ppml_100m_4cat_four_subsamples_v2.rtf")

# Build table content
lines <- c()
lines <- c(lines, "\\rtf1\\ansi\\deff0")
lines <- c(lines, "{\\fonttbl{\\f0\\fswiss\\fcharset0 Arial;}}")
lines <- c(lines, "\\fs24")
lines <- c(lines, "\\pard\\qc\\b PPML DID: 4 Major Crime Categories x Four Subsamples (100m) \\b0\\par")
lines <- c(lines, "\\pard\\qc Standard errors clustered at county level. Offset: ln(pop). \\par")
lines <- c(lines, "\\pard\\qc Significance: + p<0.15, * p<0.10, ** p<0.05, *** p<0.01 \\par\\par")

# Header row
header <- sprintf("\\trowd\\trgaph70")
for (k in 1:5) {
  if (k == 1) {
    header <- paste0(header, "\\cellx2500")
  } else {
    header <- paste0(header, "\\cellx4500")
  }
}
header <- paste0(header, "\\intbl\\b Crime Category \\cell\\b UV \\cell\\b Urban non-UV \\cell\\b Urban UV \\cell\\b Non-urban UV \\cell\\row")
lines <- c(lines, header)

# Data rows
for (i in 1:4) {
  row <- sprintf("\\trowd\\trgaph70\\cellx2500\\cellx4500\\cellx4500\\cellx4500\\cellx4500")
  row <- paste0(row, "\\intbl ")

  # Category name
  row <- paste0(row, major_labels[i], " \\cell")

  # Four subsamples
  for (j in 1:4) {
    res <- results_4cat[i, j][[1]]
    if (!is.null(res)) {
      coef_str <- format_coef(res$coef, res$p)
      se_str <- sprintf("(%.4f)", res$se)
      cell_content <- paste0(coef_str, "\\line ", se_str)
      row <- paste0(row, cell_content, " \\cell")
    } else {
      row <- paste0(row, "NA \\cell")
    }
  }
  row <- paste0(row, "\\row")
  lines <- c(lines, row)
}

lines <- c(lines, "\\pard\\par")
lines <- c(lines, "}")

writeLines(lines, out_rtf_4, useBytes = TRUE)
cat(sprintf("Table 2 exported to: %s\n", out_rtf_4))

# =========================================================
# Also export CSV for easy reference
# =========================================================
cat("\n=== Exporting CSV ===\n")

csv_data <- list()

# 16 subcategories
for (i in 1:16) {
  for (j in 1:4) {
    res <- results_16cat[i, j][[1]]
    csv_data[[length(csv_data) + 1]] <- data.frame(
      table_type = "16_subcategories",
      outcome = subcategory_labels[i],
      subsample = subsamples[j],
      coef = ifelse(!is.null(res), res$coef, NA),
      se = ifelse(!is.null(res), res$se, NA),
      p = ifelse(!is.null(res), res$p, NA),
      N_full = ifelse(!is.null(res), as.integer(res$N_full), NA),
      N_regression = ifelse(!is.null(res), as.integer(res$N_regression), NA),
      stringsAsFactors = FALSE
    )
  }
}

# 4 major categories
for (i in 1:4) {
  for (j in 1:4) {
    res <- results_4cat[i, j][[1]]
    csv_data[[length(csv_data) + 1]] <- data.frame(
      table_type = "4_major",
      outcome = major_labels[i],
      subsample = subsamples[j],
      coef = ifelse(!is.null(res), res$coef, NA),
      se = ifelse(!is.null(res), res$se, NA),
      p = ifelse(!is.null(res), res$p, NA),
      N_full = ifelse(!is.null(res), as.integer(res$N_full), NA),
      N_regression = ifelse(!is.null(res), as.integer(res$N_regression), NA),
      stringsAsFactors = FALSE
    )
  }
}

csv_df <- rbindlist(csv_data)
out_csv <- file.path(OUT_TABLE, "ppml_uv_100m_cat_four_subsamples_v2.csv")
fwrite(csv_df, out_csv)
cat(sprintf("CSV exported to: %s\n", out_csv))

cat("\nDone: PPML four subsamples analysis completed (v2 with two big tables).\n")
sink()

cat("Script completed successfully.\n")
