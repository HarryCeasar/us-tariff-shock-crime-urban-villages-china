clear all
set more off

global ROOT "."
global OUT_TABLE "$ROOT/output_tables"
global LOGFILE "$OUT_TABLE/task2_to_4_robustness_checks.log"
global STTMP "$ROOT/tmp"

capture mkdir "$STTMP"
capture noisily set tmpdir "$STTMP"

capture log close
log using "$LOGFILE", text replace

capture which ppmlhdfe
if _rc ssc install ppmlhdfe, replace

capture which winsor2
if _rc ssc install winsor2, replace

local DATA "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
local PPML_OPTS "separation(fe ir) keepsingletons tolerance(1e-6)"

* ============================================================
* 任务2: 为ppml_100m_uv_totalcrime_extra_robust_v1.csv补充95%缩尾结果
* ============================================================
di "========================================"
di "任务2: 95%缩尾稳健性检验"
di "========================================"

import delimited "`DATA'", clear varnames(1) encoding(utf8)
destring county_code, replace force

gen double lnpop = ln(pop)
egen double z_us4 = std(us_tariff_exposure_4)
gen int year_num = real(substr(period,1,4))
gen byte half_num = real(substr(period,6,1))
egen long period_id = group(period)
egen long city_period = group(county_city period)
gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)

clonevar crime_count_winsor = crime_count
winsor2 crime_count_winsor, cuts(1 95) trim

tempfile robust_csv
postfile pf2 str40 spec double b se p N using `robust_csv', replace

* 原始结果（不缩尾）
quietly ppmlhdfe crime_count c.z_us4##i.post if is_uv==1, ///
    absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_original = _b[1.post#c.z_us4]
local se_original = _se[1.post#c.z_us4]
local z_original = `b_original' / `se_original'
local pval_original = 2 * normal(-abs(`z_original'))
post pf2 ("原始") (`b_original') (`se_original') (`pval_original') (e(N))

* 95%缩尾结果
quietly ppmlhdfe crime_count_winsor c.z_us4##i.post if is_uv==1, ///
    absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_winsor = _b[1.post#c.z_us4]
local se_winsor = _se[1.post#c.z_us4]
local z_winsor = `b_winsor' / `se_winsor'
local pval_winsor = 2 * normal(-abs(`z_winsor'))
post pf2 ("95%缩尾") (`b_winsor') (`se_winsor') (`pval_winsor') (e(N))

postclose pf2

use `robust_csv', clear
export delimited using "$OUT_TABLE/ppml_100m_uv_totalcrime_extra_robust_v1.csv", replace

di "任务2完成: 95%缩尾结果已追加"

* ============================================================
* 任务3: 为ppml_100m_uv_totalcrime_policy_timing_placebo_v1.csv追加2019h1和h2的结果
* ============================================================
di "========================================"
di "任务3: 政策时点安慰剂检验（2019H1和H2）"
di "========================================"

import delimited "`DATA'", clear varnames(1) encoding(utf8)
destring county_code, replace force

gen double lnpop = ln(pop)
egen double z_us4 = std(us_tariff_exposure_4)
gen int year_num = real(substr(period,1,4))
gen byte half_num = real(substr(period,6,1))
egen long period_id = group(period)
egen long city_period = group(county_city period)
gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)

tempfile placebo_csv
postfile pf3 str40 timing double b se p N using `placebo_csv', replace

* 原有结果：2018H2
gen byte post_2018h2 = (year_num>2018) | (year_num==2018 & half_num>=2)
quietly ppmlhdfe crime_count c.z_us4##i.post_2018h2 if is_uv==1, ///
    absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_2018h2 = _b[1.post_2018h2#c.z_us4]
local se_2018h2 = _se[1.post_2018h2#c.z_us4]
local z_2018h2 = `b_2018h2' / `se_2018h2'
local pval_2018h2 = 2 * normal(-abs(`z_2018h2'))
post pf3 ("2018H2") (`b_2018h2') (`se_2018h2') (`pval_2018h2') (e(N))

* 新增：2019H1
gen byte post_2019h1 = (year_num>2019) | (year_num==2019 & half_num>=1)
quietly ppmlhdfe crime_count c.z_us4##i.post_2019h1 if is_uv==1, ///
    absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_2019h1 = _b[1.post_2019h1#c.z_us4]
local se_2019h1 = _se[1.post_2019h1#c.z_us4]
local z_2019h1 = `b_2019h1' / `se_2019h1'
local pval_2019h1 = 2 * normal(-abs(`z_2019h1'))
post pf3 ("2019H1") (`b_2019h1') (`se_2019h1') (`pval_2019h1') (e(N))

* 新增：2019H2
gen byte post_2019h2 = (year_num>2019) | (year_num==2019 & half_num>=2)
quietly ppmlhdfe crime_count c.z_us4##i.post_2019h2 if is_uv==1, ///
    absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_2019h2 = _b[1.post_2019h2#c.z_us4]
local se_2019h2 = _se[1.post_2019h2#c.z_us4]
local z_2019h2 = `b_2019h2' / `se_2019h2'
local pval_2019h2 = 2 * normal(-abs(`z_2019h2'))
post pf3 ("2019H2") (`b_2019h2') (`se_2019h2') (`pval_2019h2') (e(N))

postclose pf3

use `placebo_csv', clear
export delimited using "$OUT_TABLE/ppml_100m_uv_totalcrime_policy_timing_placebo_v1.csv", replace

di "任务3完成: 2019H1和H2结果已追加"

* ============================================================
* 任务4: 为ppml_100m_uv_totalcrime_robust_se_compare_v1.csv增加网格FE+cityperiod FE结果
* ============================================================
di "========================================"
di "任务4: 不同固定效应组合比较"
di "========================================"

import delimited "`DATA'", clear varnames(1) encoding(utf8)
destring county_code, replace force

gen double lnpop = ln(pop)
egen double z_us4 = std(us_tariff_exposure_4)
gen int year_num = real(substr(period,1,4))
gen byte half_num = real(substr(period,6,1))
egen long period_id = group(period)
egen long city_period = group(county_city period)
gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)

tempfile fe_csv
postfile pf4 str40 fe_spec double b se p N using `fe_csv', replace

* 原有：county FE + period FE
quietly ppmlhdfe crime_count c.z_us4##i.post if is_uv==1, ///
    absorb(county_code period_id) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_cp = _b[1.post#c.z_us4]
local se_cp = _se[1.post#c.z_us4]
local z_cp = `b_cp' / `se_cp'
local pval_cp = 2 * normal(-abs(`z_cp'))
post pf4 ("county+period") (`b_cp') (`se_cp') (`pval_cp') (e(N))

* 原有：county FE + city_period FE
quietly ppmlhdfe crime_count c.z_us4##i.post if is_uv==1, ///
    absorb(county_code city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_ccp = _b[1.post#c.z_us4]
local se_ccp = _se[1.post#c.z_us4]
local z_ccp = `b_ccp' / `se_ccp'
local pval_ccp = 2 * normal(-abs(`z_ccp'))
post pf4 ("county+city_period") (`b_ccp') (`se_ccp') (`pval_ccp') (e(N))

* 新增：cell_id FE + city_period FE
quietly ppmlhdfe crime_count c.z_us4##i.post if is_uv==1, ///
    absorb(cell_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_cellcp = _b[1.post#c.z_us4]
local se_cellcp = _se[1.post#c.z_us4]
local z_cellcp = `b_cellcp' / `se_cellcp'
local pval_cellcp = 2 * normal(-abs(`z_cellcp'))
post pf4 ("cell_id+city_period") (`b_cellcp') (`se_cellcp') (`pval_cellcp') (e(N))

* 新增：cell_id FE + period_id FE + city_period FE（最严格）
quietly ppmlhdfe crime_count c.z_us4##i.post if is_uv==1, ///
    absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
local b_cellpcp = _b[1.post#c.z_us4]
local se_cellpcp = _se[1.post#c.z_us4]
local z_cellpcp = `b_cellpcp' / `se_cellpcp'
local pval_cellpcp = 2 * normal(-abs(`z_cellpcp'))
post pf4 ("cell_id+period+city_period") (`b_cellpcp') (`se_cellpcp') (`pval_cellpcp') (e(N))

postclose pf4

use `fe_csv', clear
export delimited using "$OUT_TABLE/ppml_100m_uv_totalcrime_robust_se_compare_v1.csv", replace

di "任务4完成: 网格FE+cityperiod FE结果已追加"

di "========================================"
di "任务2-4全部完成！"
di "========================================"

log close
exit
