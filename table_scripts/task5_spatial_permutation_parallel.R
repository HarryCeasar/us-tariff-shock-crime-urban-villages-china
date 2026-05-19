#!/usr/bin/env Rscript
# Spatial Permutation Test - Parallel Implementation
# Faster alternative to Stata's ppmlhdfe using fixest + parallel

library(data.table)
library(fixest)
library(parallel)

# Configuration
N_PERMUTATIONS <- 1000
N_CORES <- 10  # Use 10 cores for parallel processing
DATA_FILE <- "e:/Codex/Tariff_shock_crime_and_Infrastructure/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
OUTPUT_FILE <- "e:/Codex/Tariff_shock_crime_and_Infrastructure/table/main/ppml_100m_uv_totalcrime_spatial_permutation_v1_r.csv"
PLOT_FILE <- "e:/Codex/Tariff_shock_crime_and_Infrastructure/figure/eventstudy/spatial_permutation_scatter_v1.png"
BASELINE_COEF_FALLBACK <- 0.1760159520875316
TEMP_DATA_FILE <- "e:/Codex/Tariff_shock_crime_and_Infrastructure/table/main/temp_perm_data.rds"
TEMP_COUNTY_FILE <- "e:/Codex/Tariff_shock_crime_and_Infrastructure/table/main/temp_county_tariffs.rds"

set.seed(233)
permutation_seeds <- sample.int(.Machine$integer.max, N_PERMUTATIONS)

cat("========================================\n")
cat("Spatial Permutation Test (R Parallel)\n")
cat(sprintf("Permutations: %d | Cores: %d\n", N_PERMUTATIONS, N_CORES))
cat("========================================\n\n")

# Load and prepare data
cat("Loading data...\n")

header_names <- names(fread(DATA_FILE, nrows = 0))
exposure_name <- if ("us_tariff_exposure_4" %in% header_names) {
  "us_tariff_exposure_4"
} else if ("US_tariff_exposure_4" %in% header_names) {
  "US_tariff_exposure_4"
} else {
  stop("Neither us_tariff_exposure_4 nor US_tariff_exposure_4 exists in input data.")
}

dt <- fread(DATA_FILE, select = c("county_code", "county_city", "cell_id", "period", "year", "half", "is_uv", "crime_count", "pop", exposure_name))
if (exposure_name != "us_tariff_exposure_4") {
  setnames(dt, exposure_name, "us_tariff_exposure_4")
}

cat(sprintf("Loaded data file: %s\n", DATA_FILE))

# Convert types
dt[, county_code := as.integer(county_code)]
dt[, is_uv := as.integer(is_uv)]
dt[, crime_count := as.numeric(crime_count)]
dt[, pop := as.numeric(pop)]
dt[, year := as.integer(year)]
dt[, half := as.integer(half)]

# Create variables
dt[, lnpop := log(pop)]
dt[, post := as.integer((year > 2018) | (year == 2018 & half >= 2))]
dt[, period_id := as.integer(factor(period))]
dt[, city_period := as.integer(factor(paste(county_city, period, sep = "__")))]

# Standardize tariff exposure
dt[, z_us4 := scale(us_tariff_exposure_4)[,1]]

# Filter to UV sample
dt_uv <- dt[is_uv == 1]
cat(sprintf("UV sample size: %d observations\n", nrow(dt_uv)))
cat(sprintf("Unique counties: %d\n", length(unique(dt_uv$county_code))))

# Extract county-level tariff exposures for permutation
county_tariffs <- unique(dt_uv[, .(county_code, us_tariff_exposure_4)])
n_counties <- nrow(county_tariffs)
cat(sprintf("Counties for permutation: %d\n\n", n_counties))

# Estimate baseline coefficient using the same specification as permutation regression
BASELINE_COEF <- BASELINE_COEF_FALLBACK
baseline_model <- tryCatch({
  fepois(crime_count ~ z_us4:post | county_code + city_period,
         data = dt_uv,
         offset = log(dt_uv$pop),
         cluster = ~county_code)
}, error = function(e) {
  cat(sprintf("WARNING: Baseline regression failed, fallback to %.6f. Error: %s\n",
              BASELINE_COEF_FALLBACK, conditionMessage(e)))
  NULL
})

if (!is.null(baseline_model)) {
  baseline_ct <- coeftable(baseline_model)
  baseline_name <- grep("z_us4.*post|post.*z_us4", rownames(baseline_ct), value = TRUE)
  if (length(baseline_name) > 0) {
    BASELINE_COEF <- as.numeric(baseline_ct[baseline_name[1], "Estimate"])
  } else {
    cat(sprintf("WARNING: Baseline coefficient name not found, fallback to %.6f\n", BASELINE_COEF_FALLBACK))
  }
}

cat(sprintf("Baseline coefficient used in plot: %.6f\n\n", BASELINE_COEF))

# Save data to temporary files for workers to load
cat("Saving data to temporary files for parallel workers...\n")
saveRDS(dt_uv, TEMP_DATA_FILE)
saveRDS(county_tariffs, TEMP_COUNTY_FILE)
cat("Data saved successfully.\n\n")

# Function to run one permutation (workers will load data from files)
run_permutation <- function(iter, temp_data_file, temp_county_file) {
  # Load data in each worker
  dt_uv <- readRDS(temp_data_file)
  county_tariffs <- readRDS(temp_county_file)
  n_counties <- nrow(county_tariffs)
  
  # Use a pre-generated seed for each iteration to keep results reproducible
  set.seed(permutation_seeds[iter])
  
  # Shuffle tariff exposures at county level using random permutation
  shuffled_indices <- sample(n_counties, n_counties, replace = FALSE)
  county_tariffs_shuffled <- copy(county_tariffs)
  county_tariffs_shuffled[, us_tariff_shuffled := county_tariffs$us_tariff_exposure_4[shuffled_indices]]
  
  # Merge back to main dataset
  dt_perm <- merge(dt_uv, county_tariffs_shuffled[, .(county_code, us_tariff_shuffled)], 
                   by = "county_code", all.x = TRUE)
  dt_perm[, lnpop := log(pop)]
  
  # Standardize shuffled variable
  dt_perm[, z_us4_shuffled := scale(us_tariff_shuffled)[,1]]
  
  # Run PPML regression with fixed effects
  result <- tryCatch({
    model <- fepois(crime_count ~ z_us4_shuffled:post | county_code + city_period,
                    data = dt_perm,
                    offset = log(dt_perm$pop),
                    cluster = ~county_code)
    
    # Extract coefficient for interaction term
    coef_table <- coeftable(model)
    coef_name <- grep("z_us4_shuffled.*post|post.*z_us4_shuffled", rownames(coef_table), value = TRUE)
    
    if (length(coef_name) > 0) {
      b <- coef_table[coef_name, "Estimate"]
      se <- coef_table[coef_name, "Std. Error"]
      z_stat <- b / se
      p_val <- 2 * pnorm(-abs(z_stat))
      
      return(data.table(
        iter = iter,
        b = as.numeric(b),
        se = as.numeric(se),
        p = as.numeric(p_val)
      ))
    } else {
      cat(sprintf("Iteration %d: Coefficient not found\n", iter))
      write(
        sprintf("Iteration %d coef rows: %s\n", iter, paste(rownames(coef_table), collapse = " | ")),
        file = "e:/Codex/Tariff_shock_crime_and_Infrastructure/table/main/perm_errors.log",
        append = TRUE
      )
      return(data.table(iter = iter, b = NA_real_, se = NA_real_, p = NA_real_))
    }
  }, error = function(e) {
    # Write error to a log file for debugging
    error_msg <- sprintf("Iteration %d ERROR: %s\n", iter, conditionMessage(e))
    write(error_msg, file = "e:/Codex/Tariff_shock_crime_and_Infrastructure/table/main/perm_errors.log", append = TRUE)
    return(data.table(iter = iter, b = NA_real_, se = NA_real_, p = NA_real_))
  })
  
  return(result)
}

# Run permutations in parallel
cat(sprintf("Starting %d permutations using %d cores...\n", N_PERMUTATIONS, N_CORES))
start_time <- Sys.time()

# Use parLapply for Windows compatibility with file-based data loading
cl <- makeCluster(N_CORES)

# Export necessary packages and function to workers
clusterEvalQ(cl, {
  library(data.table)
  library(fixest)
})

# Export the function and file paths to all workers
clusterExport(cl, c("run_permutation", "TEMP_DATA_FILE", "TEMP_COUNTY_FILE", "permutation_seeds"), envir = environment())

# Run permutations
results_list <- parLapply(cl, 1:N_PERMUTATIONS, function(i) {
  run_permutation(i, TEMP_DATA_FILE, TEMP_COUNTY_FILE)
})
stopCluster(cl)

end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))
cat(sprintf("\nCompleted in %.2f minutes (%.2f hours)\n", elapsed, elapsed/60))
cat(sprintf("Average time per iteration: %.2f seconds\n\n", elapsed * 60 / N_PERMUTATIONS))

# Combine results
results_dt <- rbindlist(results_list)

# Clean up temporary files
file.remove(TEMP_DATA_FILE)
file.remove(TEMP_COUNTY_FILE)

# Save results
fwrite(results_dt, OUTPUT_FILE)
cat(sprintf("Results saved to: %s\n", OUTPUT_FILE))

# Summary statistics
cat("\n========================================\n")
cat("Summary Statistics\n")
cat("========================================\n")
valid_count <- sum(!is.na(results_dt$b))
cat(sprintf("Valid iterations: %d / %d\n", valid_count, N_PERMUTATIONS))

if (valid_count > 0) {
  cat(sprintf("Mean coefficient: %.6f\n", mean(results_dt$b, na.rm = TRUE)))
  cat(sprintf("SD coefficient: %.6f\n", sd(results_dt$b, na.rm = TRUE)))
  cat(sprintf("Min coefficient: %.6f\n", min(results_dt$b, na.rm = TRUE)))
  cat(sprintf("Max coefficient: %.6f\n", max(results_dt$b, na.rm = TRUE)))
  cat(sprintf("P-value < 0.05: %d / %d (%.1f%%)\n", 
              sum(results_dt$p < 0.05, na.rm = TRUE),
              valid_count,
              100 * sum(results_dt$p < 0.05, na.rm = TRUE) / valid_count))
  
  # Check for duplicate results (quality control)
  unique_b_values <- length(unique(results_dt$b[!is.na(results_dt$b)]))
  cat(sprintf("\nQuality Check:\n"))
  cat(sprintf("Unique coefficient values: %d (should be close to %d)\n", unique_b_values, valid_count))
  if (unique_b_values < valid_count * 0.9) {
    cat("WARNING: Too many duplicate coefficients! Randomization may have failed.\n")
  } else {
    cat("OK: Sufficient variation in coefficients.\n")
  }
  
  # Generate scatter plot: p-value vs coefficient
  cat("\nGenerating scatter plot...\n")
  
  # Remove NA values for plotting
  plot_data <- results_dt[!is.na(b) & !is.na(p)]
  
  # Create output directory if not exists
  dir.create(dirname(PLOT_FILE), recursive = TRUE, showWarnings = FALSE)
  
  # Create the scatter plot
  # Prefer showtext + explicit font file on Windows for stable Chinese rendering.
  use_showtext <- .Platform$OS.type == "windows" &&
    requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE) &&
    file.exists("C:/Windows/Fonts/msyh.ttc")

  if (use_showtext) {
    try(suppressWarnings(sysfonts::font_add("cnfont", regular = "C:/Windows/Fonts/msyh.ttc", bold = "C:/Windows/Fonts/msyhbd.ttc")), silent = TRUE)
    showtext::showtext_auto(enable = TRUE)
    showtext::showtext_opts(dpi = 150)
    png(PLOT_FILE, width = 1200, height = 800, res = 150)
    par(mar = c(5, 5, 4, 2) + 0.1, family = "cnfont")
  } else if (.Platform$OS.type == "windows") {
    suppressWarnings(windowsFonts(cn = windowsFont("Microsoft YaHei")))
    png(PLOT_FILE, width = 1200, height = 800, res = 150, type = "cairo", family = "cn")
    par(mar = c(5, 5, 4, 2) + 0.1, family = "cn")
  } else {
    png(PLOT_FILE, width = 1200, height = 800, res = 150, type = "cairo")
    par(mar = c(5, 5, 4, 2) + 0.1)
  }

  plot(plot_data$b, plot_data$p,
      xlab = "\u7cfb\u6570\u4f30\u8ba1\u503c",
      ylab = "\u663e\u8457\u6027\u6c34\u5e73",
       pch = 19,
       col = rgb(0.2, 0.4, 0.6, 0.6),
       cex = 1.2,
       xlim = range(c(plot_data$b, BASELINE_COEF), na.rm = TRUE) * c(0.95, 1.05),
       ylim = c(0, 1))
  
  # Add horizontal dashed line at p = 0.1
  abline(h = 0.1, lty = 2, lwd = 2, col = "red")
  
  # Add text label for the significance threshold
  text(min(plot_data$b, na.rm = TRUE) * 0.95, 0.1, "p = 0.1", pos = 4, col = "red", cex = 0.9)
  
  # Add vertical line for baseline coefficient
  abline(v = BASELINE_COEF, lty = 1, lwd = 2.5, col = "darkgreen")
  
  # Add text label for baseline coefficient
  text(BASELINE_COEF, max(plot_data$p, na.rm = TRUE) * 0.95, 
       sprintf("Baseline: %.3f", BASELINE_COEF), 
       pos = 3, col = "darkgreen", cex = 0.9, font = 2)
  
  # Add grid for better readability
  grid(col = "gray80", lty = 3)
  
  dev.off()
  if (use_showtext) {
    showtext::showtext_auto(enable = FALSE)
  }
  
  cat(sprintf("Scatter plot saved to: %s\n", PLOT_FILE))
  
  # Calculate empirical p-value for baseline coefficient
  empirical_p <- (sum(abs(plot_data$b) >= abs(BASELINE_COEF)) + 1) / (nrow(plot_data) + 1)
  cat(sprintf("\nEmpirical p-value for baseline coefficient: %.4f\n", empirical_p))
  cat(sprintf("Number of permutations with |coef| >= |baseline|: %d / %d\n", 
              sum(abs(plot_data$b) >= abs(BASELINE_COEF)), nrow(plot_data)))
} else {
  cat("ERROR: All iterations failed! No valid results.\n")
}

cat("\nDone!\n")