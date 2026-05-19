# US Tariff Shock, Urban Villages, and Crime in China: Reproducibility Archive

[中文说明](#美国关税冲击城中村与犯罪复现归档包)

本归档包对应 `Writing/report.tex` 的实证结果与配图输出，目标是提供一个结构简洁、可复查的复现材料集合。

## 研究摘要（基于 report.tex）

本文利用 2018 年美国对华关税冲击作为外生事件，基于珠三角 100 米网格半年度面板数据，评估贸易摩擦对犯罪的影响及其空间异质性。核心结论是：关税暴露度上升与城中村网格犯罪增加显著相关，且影响更集中于外围城中村，并在特定案由类型上更明显。机制与稳健性分析进一步考察了住房市场、夜间灯光、餐饮与社会网络等因素。

## 文件夹结构（简化版）

- `input_data/`：复现脚本直接依赖的输入数据（面板/中间输入）
- `table_scripts/`：生成表格结果的脚本
- `figure_scripts/`：生成图片结果的脚本
- `output_tables/`：论文/附录使用的表格输出（csv/rtf）
- `output_figures/`：论文使用的图片输出（png）
- `raw_data/`：涉及的原始数据文件（如 xlsx/tif/zip 等）

## 图片对应关系（tex 图 -> 输出图 -> 脚本）

- `1.描述性证据/分布图/prd_prefer500m_crime_count_1km.png`
  - 输出：`output_figures/prd_prefer500m_crime_count_1km.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_us_tariff_exposure_4.png`
  - 输出：`output_figures/prd_prefer500m_us_tariff_exposure_4.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_pop.png`
  - 输出：`output_figures/prd_prefer500m_pop.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_urban_village.png`
  - 输出：`output_figures/prd_prefer500m_urban_village.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_cuisine_entropy_2017.png`
  - 输出：`output_figures/prd_prefer500m_cuisine_entropy_2017.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `spatial_permutation_scatter_v1.png`
  - 输出：`output_figures/spatial_permutation_scatter_v1.png`
  - 脚本：`figure_scripts/task5_spatial_permutation_parallel.R`
- `1.描述性证据/lda_topic_wordclouds_multiyear_grid_3x6.png`
  - 输出：`output_figures/lda_topic_wordclouds_multiyear_grid_3x6.png`
  - 脚本：`figure_scripts/merge_lda_topic_wordclouds.ipynb`

## 表格索引

表格标签与脚本、输出文件的映射见：
- `table_manifest.csv`

图像映射见：
- `figure_manifest.csv`

## Git 上传说明

本目录下 `.gitignore` 默认忽略大体量或原始数据：
- `input_data/**`
- `raw_data/**`
- 以及常见大文件扩展（如 `*.dta`, `*.xlsx`, `*.tif`, `*.zip`）

因此，默认 `git add .` 不会把数据文件提交到远程仓库，只会提交代码、清单与文档结构。

---

# 美国关税冲击、城中村与犯罪：复现归档包

本归档包对应 `Writing/report.tex` 的实证结果与配图输出，目标是提供一个结构简洁、可复查的复现材料集合。

## 研究摘要（基于 report.tex）

本文利用 2018 年美国对华关税冲击作为外生事件，基于珠三角 100 米网格半年度面板数据，评估贸易摩擦对犯罪的影响及其空间异质性。核心结论是：关税暴露度上升与城中村网格犯罪增加显著相关，且影响更集中于外围城中村，并在特定案由类型上更明显。机制与稳健性分析进一步考察了住房市场、夜间灯光、餐饮与社会网络等因素。

## 文件夹结构（简化版）

- `input_data/`：复现脚本直接依赖的输入数据（面板/中间输入）
- `table_scripts/`：生成表格结果的脚本
- `figure_scripts/`：生成图片结果的脚本
- `output_tables/`：论文/附录使用的表格输出（csv/rtf）
- `output_figures/`：论文使用的图片输出（png）
- `raw_data/`：涉及的原始数据文件（如 xlsx/tif/zip 等）

## 图片对应关系（tex 图 -> 输出图 -> 脚本）

- `1.描述性证据/分布图/prd_prefer500m_crime_count_1km.png`
  - 输出：`output_figures/prd_prefer500m_crime_count_1km.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_us_tariff_exposure_4.png`
  - 输出：`output_figures/prd_prefer500m_us_tariff_exposure_4.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_pop.png`
  - 输出：`output_figures/prd_prefer500m_pop.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_urban_village.png`
  - 输出：`output_figures/prd_prefer500m_urban_village.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `1.描述性证据/分布图/prd_prefer500m_cuisine_entropy_2017.png`
  - 输出：`output_figures/prd_prefer500m_cuisine_entropy_2017.png`
  - 脚本：`figure_scripts/plot_prd_gz_prefer500m_namefiltered_v3.R`
- `spatial_permutation_scatter_v1.png`
  - 输出：`output_figures/spatial_permutation_scatter_v1.png`
  - 脚本：`figure_scripts/task5_spatial_permutation_parallel.R`
- `1.描述性证据/lda_topic_wordclouds_multiyear_grid_3x6.png`
  - 输出：`output_figures/lda_topic_wordclouds_multiyear_grid_3x6.png`
  - 脚本：`figure_scripts/merge_lda_topic_wordclouds.ipynb`

## 表格索引

表格标签与脚本、输出文件的映射见：
- `table_manifest.csv`

图像映射见：
- `figure_manifest.csv`

## Git 上传说明

本目录下 `.gitignore` 默认忽略大体量或原始数据：
- `input_data/**`
- `raw_data/**`
- 以及常见大文件扩展（如 `*.dta`, `*.xlsx`, `*.tif`, `*.zip`）

因此，默认 `git add .` 不会把数据文件提交到远程仓库，只会提交代码、清单与文档结构。
