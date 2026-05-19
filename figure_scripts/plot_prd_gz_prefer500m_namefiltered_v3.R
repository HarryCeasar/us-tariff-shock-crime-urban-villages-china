suppressPackageStartupMessages({
  pkgs <- c("data.table", "dplyr", "sf", "ggplot2", "viridis", "geodata", "terra")
  need <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(need) > 0) install.packages(need, repos = "https://cloud.r-project.org")
  lapply(pkgs, library, character.only = TRUE)
})

ROOT <- "."
out_dir <- file.path(ROOT, "output_figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

file_100m_h <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
file_100m_base <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
file_100m_crime <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
file_1km_h <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv")
file_1km_c <- file.path(ROOT, "input_data", "grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv")

need_files <- c(file_100m_h, file_100m_base, file_100m_crime, file_1km_h, file_1km_c)
for (f in need_files) if (!file.exists(f)) stop("Missing file: ", f)

first_non_na_num <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) return(NA_real_)
  as.numeric(z[1])
}
first_non_na_chr <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) return(NA_character_)
  as.character(z[1])
}
mean_na <- function(z) {
  m <- mean(z, na.rm = TRUE)
  if (is.nan(m)) return(NA_real_)
  m
}

to_utm_dt <- function(dt, cellsize) {
  dt2 <- as.data.table(dt)
  dt2[, cellsize_m := as.numeric(cellsize)]
  dt2
}

# Quantile binning with raw-value labels.
mk_quantile_bins <- function(x, nbin = 7) {
  x <- as.numeric(x)
  ok <- !is.na(x)
  if (sum(ok) < 2) {
    return(list(f = as.factor(rep(NA_character_, length(x))), labels = character(0)))
  }

  # Force equal-frequency quantile groups using rank-based ntile,
  # then label each quantile by its raw-value range.
  g <- rep(NA_integer_, length(x))
  g[ok] <- dplyr::ntile(rank(x[ok], ties.method = "first"), nbin)

  fmt <- function(v) {
    ifelse(abs(v) >= 1000, format(round(v, 0), big.mark = ",", scientific = FALSE), format(round(v, 3), scientific = FALSE, trim = TRUE))
  }

  use_bins <- sort(unique(g[!is.na(g)]))
  labs <- character(length(use_bins))
  for (j in seq_along(use_bins)) {
    i <- use_bins[j]
    xi <- x[g == i]
    p_lo <- 100 * (i - 1) / nbin
    p_hi <- 100 * i / nbin
    labs[j] <- paste0(
      "Q", i, " (", format(round(p_lo, 1), trim = TRUE), "%-", format(round(p_hi, 1), trim = TRUE), "%): ",
      fmt(min(xi, na.rm = TRUE)), " - ", fmt(max(xi, na.rm = TRUE))
    )
  }

  lvl <- paste0("Q", use_bins)
  f <- factor(ifelse(is.na(g), NA_character_, paste0("Q", g)), levels = lvl, ordered = TRUE)
  levels(f) <- labs

  list(f = f, labels = labs)
}

message("Reading source panels...")
dt_100m_h <- fread(file_100m_h, select = c("cell_id", "x", "y", "county_city", "county_name", "US_tariff_exposure_4", "crime_count", "pop", "is_urban_village_grid", "pre_avg_list_unit_price"))
dt_100m_base <- fread(file_100m_base, select = c("cell_id", "x", "y", "county_city", "county_name", "US_tariff_exposure_4", "crime_count", "pop", "is_urban_village_grid"))
dt_100m_crime <- fread(file_100m_crime, select = c("cell_id", "x", "y", "county_city", "county_name", "crime_count"))
dt_1km_h <- fread(file_1km_h, select = c("cell_id", "x", "y", "county_city", "county_name", "US_tariff_exposure_4", "crime_count", "pop", "is_urban_village_grid", "pre_avg_list_unit_price"))
dt_1km_c <- fread(file_1km_c, select = c("cell_id", "x", "y", "county_city", "county_name", "US_tariff_exposure_4", "crime_count", "pop", "is_urban_village_grid", "cuisine_entropy_2017"))

agg_panel <- function(dt, has_pre = FALSE, has_cuisine = FALSE) {
  out <- dt[, .(
    x = first_non_na_num(x),
    y = first_non_na_num(y),
    county_city = first_non_na_chr(county_city),
    county_name = first_non_na_chr(county_name),
    us_tariff_exposure_4 = first_non_na_num(US_tariff_exposure_4),
    crime_count = mean_na(crime_count),
    pop = mean_na(pop),
    is_urban_village_grid = first_non_na_num(is_urban_village_grid),
    pre_avg_list_unit_price = if (has_pre) first_non_na_num(pre_avg_list_unit_price) else NA_real_,
    cuisine_entropy_2017 = if (has_cuisine) first_non_na_num(cuisine_entropy_2017) else NA_real_
  ), by = .(cell_id)]
  out
}

cell_100m_h <- agg_panel(dt_100m_h, has_pre = TRUE, has_cuisine = FALSE)
cell_100m_base <- agg_panel(dt_100m_base, has_pre = FALSE, has_cuisine = FALSE)
cell_100m_crime <- dt_100m_crime[, .(
  x = first_non_na_num(x),
  y = first_non_na_num(y),
  county_city = first_non_na_chr(county_city),
  county_name = first_non_na_chr(county_name),
  crime_count = sum(as.numeric(crime_count), na.rm = TRUE)
), by = .(cell_id)]
cell_1km_h <- agg_panel(dt_1km_h, has_pre = TRUE, has_cuisine = FALSE)
cell_1km_c <- agg_panel(dt_1km_c, has_pre = FALSE, has_cuisine = TRUE)
cell_1km_crime <- dt_1km_h[, .(
  x = first_non_na_num(x),
  y = first_non_na_num(y),
  county_city = first_non_na_chr(county_city),
  county_name = first_non_na_chr(county_name),
  crime_count = sum(as.numeric(crime_count), na.rm = TRUE)
), by = .(cell_id)]

# Build plotting datasets by variable with preference for 500m.
# For cuisine and pre-list-price, fallback to 1km if 100m missing/unavailable.
base_vars <- c("us_tariff_exposure_4", "crime_count", "pop", "is_urban_village_grid")

plot_data <- list(
  prd = list(),
  gz = list()
)

prd_cn <- c("广州市", "深圳市", "珠海市", "佛山市", "江门市", "东莞市", "中山市", "惠州市", "肇庆市")

# 100m preferred base layer.
pts_100m <- to_utm_dt(cell_100m_base, 100)
pts_100m <- pts_100m[county_city %in% prd_cn]
pts_1km_crime <- to_utm_dt(cell_1km_crime, 1000)
pts_1km_crime <- pts_1km_crime[county_city %in% prd_cn]
pts_1km_h <- to_utm_dt(cell_1km_h, 1000)
pts_1km_h <- pts_1km_h[county_city %in% prd_cn]
pts_1km_c <- to_utm_dt(cell_1km_c, 1000)
pts_1km_c <- pts_1km_c[county_city %in% prd_cn]

for (v in base_vars) {
  d <- pts_100m
  if (v == "crime_count") {
    plot_data$prd[[v]] <- pts_1km_crime
    plot_data$gz[[v]] <- pts_1km_crime[county_city == "广州市"]
  } else {
    plot_data$prd[[v]] <- d
    plot_data$gz[[v]] <- d[county_city == "广州市"]
  }
}

# cuisine: 1km only
plot_data$prd[["cuisine_entropy_2017"]] <- pts_1km_c
plot_data$gz[["cuisine_entropy_2017"]] <- pts_1km_c[county_city == "广州市"]

# pre list price: use 100m if available, else fallback 1km.
pre_nonmiss_100m <- sum(!is.na(cell_100m_h$pre_avg_list_unit_price))
if (pre_nonmiss_100m > 0) {
  pts_pre <- to_utm_dt(cell_100m_h, 100)
  pts_pre <- pts_pre[county_city %in% prd_cn]
  src_pre <- "100m"
} else {
  pts_pre <- pts_1km_h
  src_pre <- "1km"
}
plot_data$prd[["pre_avg_list_unit_price"]] <- pts_pre
plot_data$gz[["pre_avg_list_unit_price"]] <- pts_pre[county_city == "广州市"]

message("Boundary loading and strict city-name filtering...")
boundary_dir <- file.path(root, "data", "boundaries_gadm")
dir.create(boundary_dir, recursive = TRUE, showWarnings = FALSE)
adm2 <- st_as_sf(geodata::gadm(country = "CHN", level = 2, path = boundary_dir)) |> st_transform(4326)
adm3 <- st_as_sf(geodata::gadm(country = "CHN", level = 3, path = boundary_dir)) |> st_transform(4326)

# Strict name filtering: no spatial intersection filtering.
prd_en <- c("Guangzhou", "Shenzhen", "Zhuhai", "Foshan", "Jiangmen", "Dongguan", "Zhongshan", "Huizhou", "Zhaoqing")
adm2_prd <- adm2 |> filter(NAME_1 == "Guangdong", NAME_2 %in% prd_en)
adm3_gz <- adm3 |> filter(NAME_1 == "Guangdong", NAME_2 == "Guangzhou")

# Plot in projected meters so cell rendering can be exact square tiles.
adm2_prd_utm <- st_transform(adm2_prd, 32650)
adm3_gz_utm <- st_transform(adm3_gz, 32650)

plot_binned <- function(pts, boundary_utm, var, outfile, nbin = 7) {
  vv <- as.numeric(pts[[var]])
  pos <- !is.na(vv) & vv > 0

  # Keep zero and missing white; only positive values enter quantile groups.
  bin_chr <- rep("0 / NA", length(vv))
  if (any(pos)) {
    b <- mk_quantile_bins(vv[pos], nbin = nbin)
    labs <- b$labels
    bin_chr[pos] <- as.character(b$f)
    pts$bin <- factor(bin_chr, levels = c("0 / NA", labs), ordered = TRUE)
  } else {
    pts$bin <- factor(bin_chr, levels = "0 / NA", ordered = TRUE)
  }

  bin_levels <- levels(pts$bin)
  q_levels <- setdiff(bin_levels, "0 / NA")
  q_palette <- setNames(grDevices::gray.colors(length(q_levels), start = 0.88, end = 0.22), q_levels)
  bin_palette <- c("0 / NA" = "white", q_palette)

  tile_size <- median(pts$cellsize_m, na.rm = TRUE)
  if (!is.finite(tile_size) || is.na(tile_size)) tile_size <- 500

  p <- ggplot() +
    geom_tile(data = pts, aes(x = x, y = y, fill = bin), width = tile_size, height = tile_size, alpha = 0.98) +
    geom_sf(data = boundary_utm, fill = NA, color = "grey60", linewidth = 0.2) +
    scale_fill_manual(values = bin_palette, drop = FALSE, na.value = "white") +
    labs(fill = NULL) +
    coord_sf(crs = st_crs(32650), datum = NA, expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(size = 9)
    )
  ggsave(outfile, p, width = 9.0, height = 7.0, dpi = 320, bg = "white")
}

plot_binary <- function(pts, boundary_utm, var, outfile) {
  pts[[var]] <- as.factor(ifelse(is.na(pts[[var]]), NA, as.integer(pts[[var]])))

  tile_size <- median(pts$cellsize_m, na.rm = TRUE)
  if (!is.finite(tile_size) || is.na(tile_size)) tile_size <- 500

  p <- ggplot() +
    geom_tile(data = pts, aes(x = x, y = y, fill = .data[[var]]), width = tile_size, height = tile_size, alpha = 0.98) +
    geom_sf(data = boundary_utm, fill = NA, color = "grey60", linewidth = 0.2) +
    scale_fill_manual(values = c("0" = "white", "1" = "grey25"), drop = FALSE, na.value = "white") +
    labs(fill = NULL) +
    coord_sf(crs = st_crs(32650), datum = NA, expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  ggsave(outfile, p, width = 9.0, height = 7.0, dpi = 320, bg = "white")
}

message("Plotting PRD maps (100m preferred, 1km for crime_count, fallback 1km)...")
plot_binned(plot_data$prd[["us_tariff_exposure_4"]], adm2_prd_utm, "us_tariff_exposure_4", file.path(out_dir, "prd_prefer500m_us_tariff_exposure_4.png"))
plot_binned(plot_data$prd[["crime_count"]], adm2_prd_utm, "crime_count", file.path(out_dir, "prd_prefer500m_crime_count_1km.png"))
plot_binned(plot_data$prd[["pop"]], adm2_prd_utm, "pop", file.path(out_dir, "prd_prefer500m_pop.png"))
plot_binary(plot_data$prd[["is_urban_village_grid"]], adm2_prd_utm, "is_urban_village_grid", file.path(out_dir, "prd_prefer500m_urban_village.png"))
plot_binned(plot_data$prd[["cuisine_entropy_2017"]], adm2_prd_utm, "cuisine_entropy_2017", file.path(out_dir, "prd_prefer500m_cuisine_entropy_2017.png"))

message("Plotting Guangzhou maps (100m preferred, 1km for crime_count, fallback 1km)...")
plot_binned(plot_data$gz[["us_tariff_exposure_4"]], adm3_gz_utm, "us_tariff_exposure_4", file.path(out_dir, "gz_prefer500m_us_tariff_exposure_4.png"))
plot_binned(plot_data$gz[["crime_count"]], adm3_gz_utm, "crime_count", file.path(out_dir, "gz_prefer500m_crime_count_1km.png"))
plot_binned(plot_data$gz[["pop"]], adm3_gz_utm, "pop", file.path(out_dir, "gz_prefer500m_pop.png"))
plot_binary(plot_data$gz[["is_urban_village_grid"]], adm3_gz_utm, "is_urban_village_grid", file.path(out_dir, "gz_prefer500m_urban_village.png"))
plot_binned(plot_data$gz[["cuisine_entropy_2017"]], adm3_gz_utm, "cuisine_entropy_2017", file.path(out_dir, "gz_prefer500m_cuisine_entropy_2017.png"))
plot_binned(plot_data$gz[["pre_avg_list_unit_price"]], adm3_gz_utm, "pre_avg_list_unit_price", file.path(out_dir, "gz_prefer500m_pre_avg_list_unit_price.png"))

message("Done. Output dir: ", out_dir)
message("Boundary filter used NAME_2 city names only for PRD and NAME_2='Guangzhou' for county-level lines.")
message("Rendering: square tiles + grayscale + no titles.")
