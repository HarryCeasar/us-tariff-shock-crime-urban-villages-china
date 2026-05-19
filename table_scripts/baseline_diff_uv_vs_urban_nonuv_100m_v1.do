clear all
set more off

global ROOT "."
global OUT_TABLE "$ROOT/output_tables"
global LOGFILE "$OUT_TABLE/baseline_diff_uv_vs_urban_nonuv_100m_v1.log"

capture mkdir "$ROOT/table"
capture mkdir "$OUT_TABLE"

capture log close
log using "$LOGFILE", text replace

capture which reghdfe
if _rc ssc install reghdfe, replace

local DATA_CSV "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
local DATA_DTA "$ROOT/input_data/grid_halfyear_panel_100m_v3_cache.dta"
capture mkdir "$ROOT/tmp_cache"

if (fileexists("`DATA_DTA'")) {
    use "`DATA_DTA'", clear
}
else {
    import delimited using "`DATA_CSV'", clear varnames(1) encoding(utf8)
    save "`DATA_DTA'", replace
}

capture destring county_code, replace force
capture destring is_uv, replace force
capture destring is_urban, replace force

* Construct baseline period and group tags.
gen int year_num = real(substr(period,1,4))
gen byte half_num = real(substr(period,6,1))
gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)

egen long period_id = group(period)
egen long city_period = group(county_city period)

gen byte g_uv = (is_uv==1)
gen byte g_urban_nonuv = (is_urban==1 & is_uv!=1)
gen byte comp = (g_uv==1 | g_urban_nonuv==1)
gen byte pre = (post==0)
gen byte metro5 = inlist(county_city, "广州市", "深圳市", "佛山市", "东莞市", "珠海市")

gen byte comp_pre = comp & pre

local varlist "us_tariff_exposure_4 pop crime_rate_100k pct_urban_village pre_avg_list_unit_price pre_avg_deal_unit_price pre_avg_price_gap dist_to_bus_m dist_to_metro_m dist_to_edu_pre_m dist_to_edu_primary_m dist_to_edu_secondary_m dist_to_edu_higher_m dist_to_edu_vocational_m dist_to_basic_healthcare_m dist_to_mid_healthcare_m dist_to_adv_healthcare_m dist_to_sme_bank_m dist_to_joint_stock_bank_m dist_to_state_owned_bank_m"

tempfile res
postfile pf str50 varname str80 vlabel double mean_full mean_urban mean_uv mean_center mean_periph ///
                     double grids_full grids_urban grids_uv grids_center grids_periph ///
                     double periods_full periods_urban periods_uv periods_center periods_periph ///
                     using `res', replace

// helper: ensure gridid exists (try to create from x,y or cell_id if needed)
capture confirm variable gridid
if _rc {
    capture confirm variable x
    if _rc { capture confirm variable cell_id }
    if _rc {
        di as error "No grid identifier found (gridid/x/cell_id). Please ensure dataset has gridid or x/y/cell_id."
        exit 198
    }
    capture confirm variable cell_id
    if !_rc {
        gen str32 gridid = string(cell_id)
    }
    else {
        gen str32 gridid = string(x, "%21.0f") + "_" + string(y, "%21.0f")
        replace gridid = subinstr(gridid, " ", "", .)
    }
}

foreach v of local varlist {
    capture confirm variable `v'
    if _rc continue

    // define five samples: full, urban正规(urban1 & uv0), uv(all uv1), center(urban1 & uv1), periphery(urban0 & uv1)
    local s_full "!missing(`v')"
    local s_urban "is_urban==1 & is_uv!=1 & !missing(`v')"
    local s_uv   "is_uv==1 & !missing(`v')"
    local s_center "is_urban==1 & is_uv==1 & !missing(`v')"
    local s_periph "is_urban==0 & is_uv==1 & !missing(`v')"

    // handle metro5-only variable if needed (keep same behavior for dist_to_metro_m)
    if "`v'"=="dist_to_metro_m" {
        local s_full "`s_full' & metro5==1"
        local s_urban "`s_urban' & metro5==1"
        local s_uv   "`s_uv' & metro5==1"
        local s_center "`s_center' & metro5==1"
        local s_periph "`s_periph' & metro5==1"
    }

    // means (observation-level means)
    quietly summarize `v' if `s_full', meanonly
    local m_full = r(mean)
    quietly summarize `v' if `s_urban', meanonly
    local m_urban = r(mean)
    quietly summarize `v' if `s_uv', meanonly
    local m_uv = r(mean)
    quietly summarize `v' if `s_center', meanonly
    local m_center = r(mean)
    quietly summarize `v' if `s_periph', meanonly
    local m_periph = r(mean)

    // grid counts (unique gridid in sample with non-missing v)
    quietly preserve
    keep if `s_full'
    bys gridid: keep if _n==1
    quietly count
    local g_full = r(N)
    restore

    quietly preserve
    keep if `s_urban'
    bys gridid: keep if _n==1
    quietly count
    local g_urban = r(N)
    restore

    quietly preserve
    keep if `s_uv'
    bys gridid: keep if _n==1
    quietly count
    local g_uv = r(N)
    restore

    quietly preserve
    keep if `s_center'
    bys gridid: keep if _n==1
    quietly count
    local g_center = r(N)
    restore

    quietly preserve
    keep if `s_periph'
    bys gridid: keep if _n==1
    quietly count
    local g_periph = r(N)
    restore

    // number of distinct periods in each sample
    quietly preserve
    keep if `s_full' & !missing(period)
    bys period: keep if _n==1
    quietly count
    local p_full = r(N)
    restore

    quietly preserve
    keep if `s_urban' & !missing(period)
    bys period: keep if _n==1
    quietly count
    local p_urban = r(N)
    restore

    quietly preserve
    keep if `s_uv' & !missing(period)
    bys period: keep if _n==1
    quietly count
    local p_uv = r(N)
    restore

    quietly preserve
    keep if `s_center' & !missing(period)
    bys period: keep if _n==1
    quietly count
    local p_center = r(N)
    restore

    quietly preserve
    keep if `s_periph' & !missing(period)
    bys period: keep if _n==1
    quietly count
    local p_periph = r(N)
    restore

    local lab : variable label `v'
    if "`lab'"=="" local lab "`v'"

    post pf ("`v'") ("`lab'") (`m_full') (`m_urban') (`m_uv') (`m_center') (`m_periph') ///
            (`g_full') (`g_urban') (`g_uv') (`g_center') (`g_periph') ///
            (`p_full') (`p_urban') (`p_uv') (`p_center') (`p_periph')
}

postclose pf
use `res', clear

export delimited using "$OUT_TABLE/baseline_diff_uv_vs_urban_nonuv_100m_v1.csv", replace

// Build publication-style descriptive table with five columns:
// Columns: 全样本 | 正规街区(urban=1 & uv=0) | 城中村(uv=1) | 中心城中村(urban=1&uv=1) | 外围城中村(urban=0&uv=1)
gen str40 c1 = cond(missing(mean_full), "", string(mean_full, "%9.3f"))
gen str40 c2 = cond(missing(mean_urban), "", string(mean_urban, "%9.3f"))
gen str40 c3 = cond(missing(mean_uv), "", string(mean_uv, "%9.3f"))
gen str40 c4 = cond(missing(mean_center), "", string(mean_center, "%9.3f"))
gen str40 c5 = cond(missing(mean_periph), "", string(mean_periph, "%9.3f"))

// compute overall grid counts (unique grids per sample, not variable-specific)
quietly preserve
    keep if !missing(gridid)
    bys gridid: keep if _n==1
    quietly count
    local grids_all = r(N)
restore

quietly preserve
    keep if is_urban==1 & is_uv!=1 & !missing(gridid)
    bys gridid: keep if _n==1
    quietly count
    local grids_urban = r(N)
restore

quietly preserve
    keep if is_uv==1 & !missing(gridid)
    bys gridid: keep if _n==1
    quietly count
    local grids_uv = r(N)
restore

quietly preserve
    keep if is_urban==1 & is_uv==1 & !missing(gridid)
    bys gridid: keep if _n==1
    quietly count
    local grids_center = r(N)
restore

quietly preserve
    keep if is_urban==0 & is_uv==1 & !missing(gridid)
    bys gridid: keep if _n==1
    quietly count
    local grids_periph = r(N)
restore

// compute number of distinct periods per sample
quietly preserve
    keep if !missing(period)
    bys period: keep if _n==1
    quietly count
    local periods_all = r(N)
restore

quietly preserve
    keep if is_urban==1 & is_uv!=1 & !missing(period)
    bys period: keep if _n==1
    quietly count
    local periods_urban = r(N)
restore

quietly preserve
    keep if is_uv==1 & !missing(period)
    bys period: keep if _n==1
    quietly count
    local periods_uv = r(N)
restore

quietly preserve
    keep if is_urban==1 & is_uv==1 & !missing(period)
    bys period: keep if _n==1
    quietly count
    local periods_center = r(N)
restore

quietly preserve
    keep if is_urban==0 & is_uv==1 & !missing(period)
    bys period: keep if _n==1
    quietly count
    local periods_periph = r(N)
restore

local K = _N
local R = 2*`K' + 4

putdocx clear
putdocx begin
putdocx paragraph
putdocx text ("Descriptive Statistics: Five-sample grid-level counts + means (100m)"), bold

putdocx table t = (`R', 6), layout(autofitcontents)
putdocx table t(1,1) = ("Variable")
putdocx table t(1,2) = ("全样本")
putdocx table t(1,3) = ("正规街区 (urban=1, uv=0)")
putdocx table t(1,4) = ("城中村 (uv=1)")
putdocx table t(1,5) = ("中心城中村 (urban=1 & uv=1)")
putdocx table t(1,6) = ("外围城中村 (urban=0 & uv=1)")

forvalues i = 1/`K' {
    local r1 = 2*`i'
    local r2 = `r1' + 1

    putdocx table t(`r1',1) = (vlabel[`i'])
    putdocx table t(`r1',2) = (c1[`i'])
    putdocx table t(`r1',3) = (c2[`i'])
    putdocx table t(`r1',4) = (c3[`i'])
    putdocx table t(`r1',5) = (c4[`i'])
    putdocx table t(`r1',6) = (c5[`i'])

    // second row left blank (reserve for sd or notes if desired)
    putdocx table t(`r2',1) = ("")
    putdocx table t(`r2',2) = ("")
    putdocx table t(`r2',3) = ("")
    putdocx table t(`r2',4) = ("")
    putdocx table t(`r2',5) = ("")
    putdocx table t(`r2',6) = ("")
}

local robs = 2*`K' + 1
putdocx table t(`robs',1) = ("Observations (unique grids)")
putdocx table t(`robs',2) = (string(`grids_all', "%12.0f"))
putdocx table t(`robs',3) = (string(`grids_urban', "%12.0f"))
putdocx table t(`robs',4) = (string(`grids_uv', "%12.0f"))
putdocx table t(`robs',5) = (string(`grids_center', "%12.0f"))
putdocx table t(`robs',6) = (string(`grids_periph', "%12.0f"))

local rperiod = `robs' + 1
putdocx table t(`rperiod',1) = ("Periods")
putdocx table t(`rperiod',2) = (string(`periods_all', "%12.0f"))
putdocx table t(`rperiod',3) = (string(`periods_urban', "%12.0f"))
putdocx table t(`rperiod',4) = (string(`periods_uv', "%12.0f"))
putdocx table t(`rperiod',5) = (string(`periods_center', "%12.0f"))
putdocx table t(`rperiod',6) = (string(`periods_periph', "%12.0f"))

local rnote = `rperiod' + 1
putdocx table t(`rnote',1) = ("Notes: Means are observation-level means; Observations are unique grid counts per sample; Periods are distinct half-year periods in sample; dist_to_metro_m restricted to metro5 cities.")

putdocx save "$OUT_TABLE/baseline_diff_uv_vs_urban_nonuv_100m_v1.docx", replace

di "Done: baseline difference table exported (csv + docx) with five columns and grid counts."
log close
