#!/usr/bin/env Rscript
# Descriptive Statistics: Five-sample baseline differences (100m grid level)
# Output: CSV and docx table with grid counts and period counts

library(data.table)
library(tidyverse)
library(officer)

ROOT <- "e:/Codex/Tariff_shock_crime_and_Infrastructure"
OUT_TABLE <- file.path(ROOT, "table/main")
dir.create(OUT_TABLE, showWarnings = FALSE, recursive = TRUE)

# Load data
data_csv <- file.path(ROOT, "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
cat("Loading CSV:", data_csv, "\n")
df <- fread(data_csv, encoding = "UTF-8")

cat("Data dimensions:", nrow(df), "rows,", ncol(df), "cols\n")
cat("Column names:", paste(names(df)[1:10], collapse=", "), "...\n")

# Ensure gridid exists
if (!("gridid" %in% names(df))) {
  if ("cell_id" %in% names(df)) {
    df[, gridid := as.character(cell_id)]
    cat("Created gridid from cell_id\n")
  } else if ("x" %in% names(df) && "y" %in% names(df)) {
    df[, gridid := paste(x, y, sep = "_")]
    cat("Created gridid from x_y\n")
  } else {
    stop("No gridid, cell_id, or x/y found in data")
  }
}

# Create derived variables
df[, ":="(
  year_num = as.numeric(substr(period, 1, 4)),
  half_num = as.numeric(substr(period, 6, 1)),
  post = (year_num > 2018) | (year_num == 2018 & half_num >= 2),
  is_urban = as.numeric(is_urban),
  is_uv = as.numeric(is_uv),
  county_code = as.character(county_code),
  metro5 = county_city %in% c("广州市", "深圳市", "佛山市", "东莞市", "珠海市")
)]

# Variable list
varlist <- c(
  "us_tariff_exposure_4", "pop", "crime_rate_100k", "pct_urban_village",
  "pre_avg_list_unit_price", "pre_avg_deal_unit_price", "pre_avg_price_gap",
  "dist_to_bus_m", "dist_to_metro_m",
  "dist_to_edu_pre_m", "dist_to_edu_primary_m", "dist_to_edu_secondary_m",
  "dist_to_edu_higher_m", "dist_to_edu_vocational_m",
  "dist_to_basic_healthcare_m", "dist_to_mid_healthcare_m", "dist_to_adv_healthcare_m",
  "dist_to_sme_bank_m", "dist_to_joint_stock_bank_m", "dist_to_state_owned_bank_m"
)

# Filter to only existing variables
varlist <- intersect(varlist, names(df))
cat("Processing", length(varlist), "variables\n")

# Initialize result list
results <- list()

# Process each variable
for (v in varlist) {
  cat("Processing:", v, "\n")
  
  # Define five samples
  temp_df <- df[, .(
    gridid = gridid,
    period = period,
    value = get(v),
    full = !is.na(get(v)),
    urban = (is_urban == 1 & is_uv != 1 & !is.na(get(v))),
    uv = (is_uv == 1 & !is.na(get(v))),
    center = (is_urban == 1 & is_uv == 1 & !is.na(get(v))),
    periph = (is_urban == 0 & is_uv == 1 & !is.na(get(v))),
    metro5 = metro5
  )]
  
  # Handle dist_to_metro_m: restrict to metro5 only
  if (v == "dist_to_metro_m") {
    temp_df[!metro5, ":="(full = FALSE, urban = FALSE, uv = FALSE, center = FALSE, periph = FALSE)]
  }
  
  # Compute means for each sample
  mean_full <- mean(temp_df$value[temp_df$full], na.rm = TRUE)
  mean_urban <- mean(temp_df$value[temp_df$urban], na.rm = TRUE)
  mean_uv <- mean(temp_df$value[temp_df$uv], na.rm = TRUE)
  mean_center <- mean(temp_df$value[temp_df$center], na.rm = TRUE)
  mean_periph <- mean(temp_df$value[temp_df$periph], na.rm = TRUE)
  
  # Compute unique grid counts
  grids_full <- temp_df[full == TRUE, .(gridid)] %>% distinct() %>% nrow()
  grids_urban <- temp_df[urban == TRUE, .(gridid)] %>% distinct() %>% nrow()
  grids_uv <- temp_df[uv == TRUE, .(gridid)] %>% distinct() %>% nrow()
  grids_center <- temp_df[center == TRUE, .(gridid)] %>% distinct() %>% nrow()
  grids_periph <- temp_df[periph == TRUE, .(gridid)] %>% distinct() %>% nrow()
  
  # Compute distinct periods
  periods_full <- temp_df[full == TRUE, .(period)] %>% distinct() %>% nrow()
  periods_urban <- temp_df[urban == TRUE, .(period)] %>% distinct() %>% nrow()
  periods_uv <- temp_df[uv == TRUE, .(period)] %>% distinct() %>% nrow()
  periods_center <- temp_df[center == TRUE, .(period)] %>% distinct() %>% nrow()
  periods_periph <- temp_df[periph == TRUE, .(period)] %>% distinct() %>% nrow()
  
  # Store result
  results[[v]] <- data.frame(
    varname = v,
    vlabel = v,  # Could add variable labels here if available
    mean_full = mean_full,
    mean_urban = mean_urban,
    mean_uv = mean_uv,
    mean_center = mean_center,
    mean_periph = mean_periph,
    grids_full = grids_full,
    grids_urban = grids_urban,
    grids_uv = grids_uv,
    grids_center = grids_center,
    grids_periph = grids_periph,
    periods_full = periods_full,
    periods_urban = periods_urban,
    periods_uv = periods_uv,
    periods_center = periods_center,
    periods_periph = periods_periph,
    stringsAsFactors = FALSE
  )
}

# Combine results
res_df <- bind_rows(results)

# Export CSV
csv_out <- file.path(OUT_TABLE, "baseline_diff_uv_vs_urban_nonuv_100m_v1.csv")
write.csv(res_df, csv_out, row.names = FALSE)
cat("Exported CSV:", csv_out, "\n")

# Compute overall grid/period counts (using full dataset, not variable-specific)
grids_all <- df[!is.na(gridid), .(gridid)] %>% distinct() %>% nrow()
grids_urban_all <- df[(is_urban == 1 & is_uv != 1) & !is.na(gridid), .(gridid)] %>% distinct() %>% nrow()
grids_uv_all <- df[(is_uv == 1) & !is.na(gridid), .(gridid)] %>% distinct() %>% nrow()
grids_center_all <- df[(is_urban == 1 & is_uv == 1) & !is.na(gridid), .(gridid)] %>% distinct() %>% nrow()
grids_periph_all <- df[(is_urban == 0 & is_uv == 1) & !is.na(gridid), .(gridid)] %>% distinct() %>% nrow()

periods_all <- df[!is.na(period), .(period)] %>% distinct() %>% nrow()
periods_urban_all <- df[(is_urban == 1 & is_uv != 1) & !is.na(period), .(period)] %>% distinct() %>% nrow()
periods_uv_all <- df[(is_uv == 1) & !is.na(period), .(period)] %>% distinct() %>% nrow()
periods_center_all <- df[(is_urban == 1 & is_uv == 1) & !is.na(period), .(period)] %>% distinct() %>% nrow()
periods_periph_all <- df[(is_urban == 0 & is_uv == 1) & !is.na(period), .(period)] %>% distinct() %>% nrow()

cat("Grid and period counts computed\n")

# Create docx table using officer
doc <- read_docx()

# Add title
doc <- doc %>%
  body_add_par("Descriptive Statistics: Five-sample grid-level counts + means (100m)", 
               style = "Heading 1")

# Create table data frame
table_data <- res_df %>%
  mutate(across(starts_with("mean_"), ~round(., 3))) %>%
  select(varname, mean_full, mean_urban, mean_uv, mean_center, mean_periph) %>%
  rename(
    Variable = varname,
    "全样本" = mean_full,
    "正规街区 (urban=1, uv=0)" = mean_urban,
    "城中村 (uv=1)" = mean_uv,
    "中心城中村 (urban=1 & uv=1)" = mean_center,
    "外围城中村 (urban=0 & uv=1)" = mean_periph
  )

# Add table to docx
doc <- doc %>%
  body_add_table(table_data, style = "Light Grid Accent 1")

# Add observations row
obs_row <- data.frame(
  Variable = "Observations (unique grids)",
  "全样本" = grids_all,
  "正规街区 (urban=1, uv=0)" = grids_urban_all,
  "城中村 (uv=1)" = grids_uv_all,
  "中心城中村 (urban=1 & uv=1)" = grids_center_all,
  "外围城中村 (urban=0 & uv=1)" = grids_periph_all,
  check.names = FALSE
)
doc <- doc %>% body_add_table(obs_row, style = "Light Grid Accent 1")

# Add periods row
periods_row <- data.frame(
  Variable = "Periods",
  "全样本" = periods_all,
  "正规街区 (urban=1, uv=0)" = periods_urban_all,
  "城中村 (uv=1)" = periods_uv_all,
  "中心城中村 (urban=1 & uv=1)" = periods_center_all,
  "外围城中村 (urban=0 & uv=1)" = periods_periph_all,
  check.names = FALSE
)
doc <- doc %>% body_add_table(periods_row, style = "Light Grid Accent 1")

# Add notes
notes <- "Notes: Means are observation-level means; Observations are unique grid counts per sample; Periods are distinct half-year periods in sample; dist_to_metro_m restricted to metro5 cities."
doc <- doc %>% body_add_par(notes, style = "Normal")

# Save docx
docx_out <- file.path(OUT_TABLE, "baseline_diff_uv_vs_urban_nonuv_100m_v1.docx")
print(doc, target = docx_out)
cat("Exported DOCX:", docx_out, "\n")

# Print summary
cat("\n=== Summary ===\n")
cat("Variables processed:", nrow(res_df), "\n")
cat("Grid counts (full sample):", grids_all, "\n")
cat("Period count (full sample):", periods_all, "\n")
cat("Done: baseline difference table exported (csv + docx) with five columns and grid counts.\n")
