library(data.table)
library(fixest)

# Set paths
ROOT <- "."
MAIN_CSV <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
CTRL_CSV <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv")
OUT_TABLE <- file.path(ROOT, "output_tables")
LOGFILE <- file.path(OUT_TABLE, "ols_did_100m_ntl_pop_housing_four_subsamples_v1.log")

# Create output directory
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

# Open log
sink(LOGFILE)
cat("Starting OLS-DID analysis: NTL, Population, Housing across four subsamples\n")
cat(Sys.time(), "\n\n")

# =========================================================
# Read and merge data
# =========================================================
cat("Reading main data...\n")
main_cols <- c("cell_id", "county_code", "county_city", "period", "is_urban", "is_uv",
               "pop", "us_tariff_exposure_4",
               "avg_list_unit_price", "avg_deal_unit_price", "avg_price_gap", "avg_deal_cycle_days")
main_dt <- fread(MAIN_CSV, select = main_cols)
cat(sprintf("Main data: %d observations\n", nrow(main_dt)))

cat("Reading NTL controls...\n")
ntl_cols <- c("cell_id", "period", "ntl_dmsp_like", "ln_ntl")
ntl_dt <- fread(CTRL_CSV, select = ntl_cols)

cat("Merging data...\n")
dt <- merge(main_dt, ntl_dt, by = c("cell_id", "period"), all.x = TRUE)
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

# Create ln(pop)
dt[, ln_pop := log(pop)]

# Define outcome variables
outcomes <- list(
  list(var = "ln_ntl", label = "ln(NTL)", type = "NTL"),
  list(var = "ntl_dmsp_like", label = "NTL (level)", type = "NTL"),
  list(var = "ln_pop", label = "ln(Population)", type = "Population"),
  list(var = "pop", label = "Population (level)", type = "Population"),
  list(var = "avg_list_unit_price", label = "List Price", type = "Housing"),
  list(var = "avg_deal_unit_price", label = "Deal Price", type = "Housing"),
  list(var = "avg_price_gap", label = "Price Gap", type = "Housing"),
  list(var = "avg_deal_cycle_days", label = "Deal Cycle Days", type = "Housing")
)

# Define four subsamples
subsamples <- list(
  list(name = "UV", cond = dt[is_uv == 1]),
  list(name = "Urban non-UV", cond = dt[is_urban == 1 & is_uv == 0]),
  list(name = "Urban UV", cond = dt[is_urban == 1 & is_uv == 1]),
  list(name = "Non-urban UV", cond = dt[is_urban == 0 & is_uv == 1])
)

cat("\n=== Sample Sizes ===\n")
for (ss in subsamples) {
  cat(sprintf("%s: %d observations\n", ss$name, nrow(ss$cond)))
}
cat("\n")

# =========================================================
# Function to run OLS-DID
# =========================================================
run_ols_did <- function(data, yvar) {
  # Filter complete cases
  dt_clean <- data[!is.na(get(yvar)) & !is.na(z_us4) & !is.na(post) &
                   !is.na(county_code) & !is.na(period_id) & !is.na(city_period)]

  if (nrow(dt_clean) == 0) {
    return(NULL)
  }

  result <- tryCatch({
    model <- feols(
      as.formula(paste0(yvar, " ~ z_us4 * post | county_code + period_id + city_period")),
      data = dt_clean,
      cluster = ~county_code
    )

    summ <- summary(model)
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

    # Get R-squared
    model_output <- capture.output(print(model))
    adj_r2_line <- grep("Adj\\. R2:", model_output, value = TRUE)
    r2_val <- NA_real_
    if (length(adj_r2_line) > 0) {
      r2_str <- regmatches(adj_r2_line, regexpr("(?<=Adj\\. R2: )[0-9.]+", adj_r2_line, perl = TRUE))
      if (length(r2_str) > 0) r2_val <- as.numeric(r2_str)
    }

    list(
      coef = b,
      se = se,
      t = t_val,
      p = p_val,
      N = summ$nobs,
      r2 = ifelse(is.na(r2_val), 0, r2_val)
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
# Run regressions
# =========================================================
cat("\n=== Running Regressions ===\n")

# Store results: rows=outcomes, cols=subsamples
results_matrix <- matrix(list(), nrow = length(outcomes), ncol = length(subsamples))
rownames(results_matrix) <- sapply(outcomes, function(x) x$label)
colnames(results_matrix) <- sapply(subsamples, function(x) x$name)

for (i in seq_along(outcomes)) {
  yvar <- outcomes[[i]]$var
  ylabel <- outcomes[[i]]$label
  cat(sprintf("\n%s (%d/%d):\n", ylabel, i, length(outcomes)))

  for (j in seq_along(subsamples)) {
    ss_name <- subsamples[[j]]$name
    ss_data <- subsamples[[j]]$cond

    res <- run_ols_did(ss_data, yvar)
    results_matrix[i, j] <- list(res)

    if (!is.null(res)) {
      cat(sprintf("  %-15s: coef=%.4f, SE=%.4f, p=%.4f, N=%d\n",
                  ss_name, res$coef, res$se, res$p, res$N))
    } else {
      cat(sprintf("  %-15s: FAILED\n", ss_name))
    }
  }
}

# =========================================================
# Export Table 1: All outcomes x 4 subsamples (RTF)
# =========================================================
cat("\n\n=== Exporting RTF Table ===\n")

out_rtf <- file.path(OUT_TABLE, "ols_did_100m_ntl_pop_housing_four_subsamples_v1.rtf")

lines <- c()
lines <- c(lines, "\\rtf1\\ansi\\deff0")
lines <- c(lines, "{\\fonttbl{\\f0\\fswiss\\fcharset0 Arial;}}")
lines <- c(lines, "\\fs24")
lines <- c(lines, "\\pard\\qc\\b OLS-DID: Tariff Shock Effects on NTL, Population, and Housing (100m) \\b0\\par")
lines <- c(lines, "\\pard\\qc Fixed Effects: county + period + city*period. SE clustered at county level. \\par")
lines <- c(lines, "\\pard\\qc Significance: + p<0.15, * p<0.10, ** p<0.05, *** p<0.01 \\par\\par")

# Header row
header <- sprintf("\\trowd\\trgaph70")
for (k in 1:5) {
  if (k == 1) {
    header <- paste0(header, "\\cellx3000")
  } else {
    header <- paste0(header, "\\cellx4500")
  }
}
header <- paste0(header, "\\intbl\\b Outcome Variable \\cell\\b UV \\cell\\b Urban non-UV \\cell\\b Urban UV \\cell\\b Non-urban UV \\cell\\row")
lines <- c(lines, header)

# Data rows
for (i in 1:length(outcomes)) {
  row <- sprintf("\\trowd\\trgaph70\\cellx3000\\cellx4500\\cellx4500\\cellx4500\\cellx4500")
  row <- paste0(row, "\\intbl ")

  # Outcome name
  row <- paste0(row, outcomes[[i]]$label, " \\cell")

  # Four subsamples
  for (j in 1:4) {
    res <- results_matrix[i, j][[1]]
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

writeLines(lines, out_rtf, useBytes = TRUE)
cat(sprintf("RTF table exported to: %s\n", out_rtf))

# =========================================================
# Export CSV
# =========================================================
cat("\n=== Exporting CSV ===\n")

csv_data <- list()
for (i in 1:length(outcomes)) {
  for (j in 1:4) {
    res <- results_matrix[i, j][[1]]
    csv_data[[length(csv_data) + 1]] <- data.frame(
      outcome = outcomes[[i]]$label,
      outcome_type = outcomes[[i]]$type,
      outcome_var = outcomes[[i]]$var,
      subsample = subsamples[[j]]$name,
      coef = ifelse(!is.null(res), res$coef, NA),
      se = ifelse(!is.null(res), res$se, NA),
      t = ifelse(!is.null(res), res$t, NA),
      p = ifelse(!is.null(res), res$p, NA),
      N = ifelse(!is.null(res), as.integer(res$N), NA),
      r2 = ifelse(!is.null(res), res$r2, NA),
      stringsAsFactors = FALSE
    )
  }
}

csv_df <- rbindlist(csv_data)
out_csv <- file.path(OUT_TABLE, "ols_did_100m_ntl_pop_housing_four_subsamples_v1.csv")
fwrite(csv_df, out_csv)
cat(sprintf("CSV exported to: %s\n", out_csv))

cat("\nDone: OLS-DID analysis completed for NTL, Population, and Housing.\n")
sink()

cat("Script completed successfully.\n")
