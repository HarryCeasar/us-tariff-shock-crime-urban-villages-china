clear all
set more off

global ROOT "e:/Codex/Tariff_shock_crime_and_Infrastructure"
global OUT_TABLE "$ROOT/table/main"
global LOGFILE "$OUT_TABLE/task2_to_5_robustness_checks.log"
global STTMP "E:/stata_tmp"

capture mkdir "$STTMP"
capture noisily set tmpdir "$STTMP"

capture log close
log using "$LOGFILE", text replace

capture which ppmlhdfe
if _rc ssc install ppmlhdfe, replace

local DATA "$ROOT/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
local PPML_OPTS "separation(fe ir) keepsingletons tolerance(1e-6)"

import delimited "`DATA'", clear varnames(1) encoding(utf8)
destring county_code, replace force

gen double lnpop = ln(pop)
egen double z_us4 = std(us_tariff_exposure_4)

gen int year_num = real(substr(period,1,4))
gen byte half_num = real(substr(period,6,1))
gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)
egen long period_id = group(period)
egen long city_period = group(county_city period)

* ============================================================
* 任务2: 为ppml_100m_uv_totalcrime_extra_robust_v1.csv补充95%缩尾结果
* ============================================================
di "========================================"
di "任务2: 95%缩尾稳健性检验"
di "========================================"

* 对犯罪数量进行95%缩尾
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

* 重新导入数据（因为任务2后数据被clear了）
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

* 重新导入数据
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

* ============================================================
* 任务5: 空间置换检验（50次回归）
* ============================================================
di "========================================"
di "任务5: 空间置换检验（50次迭代）"
di "========================================"

* 重新导入数据
import delimited "`DATA'", clear varnames(1) encoding(utf8)
destring county_code, replace force

gen double lnpop = ln(pop)
egen double z_us4 = std(us_tariff_exposure_4)
gen int year_num = real(substr(period,1,4))
gen byte half_num = real(substr(period,6,1))
egen long period_id = group(period)
egen long city_period = group(county_city period)
gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)

tempfile perm_csv
postfile pf5 int iter double b se p using `perm_csv', replace

forvalues i = 1/50 {
    di "正在进行第 `i'/50 次置换..."
    
    * 真正的随机置换：在county层面打乱冲击值的分配
    * 步骤1: 提取每个county的唯一冲击值
    preserve
    keep county_code us_tariff_exposure_4
    bysort county_code: keep if _n == 1  // 每个county保留一条记录
    gen double _random = runiform()
    sort _random  // 随机排序
    gen double us_tariff_permuted = us_tariff_exposure_4[_n]  // 重新分配
    
    * 步骤2: 将置换后的值合并回原数据
    tempfile perm_values
    save `perm_values'
    restore
    
    merge m:1 county_code using `perm_values', keepusing(us_tariff_permuted) keep(match) nogenerate
    drop us_tariff_exposure_4 _random
    rename us_tariff_permuted us_tariff_shuffled
    
    * 标准化置换后的冲击变量
    egen double z_us4_shuffled = std(us_tariff_shuffled)
    
    * 运行回归
    capture noisily ppmlhdfe crime_count c.z_us4_shuffled##i.post if is_uv==1, ///
        absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'
    
    if !_rc {
        local b_perm = _b[1.post#c.z_us4_shuffled]
        local se_perm = _se[1.post#c.z_us4_shuffled]
        local z_perm = `b_perm' / `se_perm'
        local pval_perm = 2 * normal(-abs(`z_perm'))
        post pf5 (`i') (`b_perm') (`se_perm') (`pval_perm')
    }
    else {
        post pf5 (`i') (.) (.) (.)
    }
    
    drop us_tariff_shuffled z_us4_shuffled
}

postclose pf5

use `perm_csv', clear
export delimited using "$OUT_TABLE/ppml_100m_uv_totalcrime_spatial_permutation_v1.csv", replace

di "任务5完成: 50次空间置换检验已完成"

di "========================================"
di "所有任务（2-5）已完成！"
di "========================================"

log close
exit
