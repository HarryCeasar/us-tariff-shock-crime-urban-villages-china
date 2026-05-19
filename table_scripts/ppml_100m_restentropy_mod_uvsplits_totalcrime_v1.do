* PPML analysis: Restaurant density and cuisine entropy moderation on total crime
* Replicating specification from ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv

clear all
set more off

local ROOT "e:/Codex/Tariff_shock_crime_and_Infrastructure"
local MAIN_CSV "`root'/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
local CTRL_CSV "`root'/grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv"
local OUT_TABLE "`root'/table/main"
local LOGFILE "`out_table'/ppml_100m_restentropy_mod_uvsplits_totalcrime_v1.log"

* Create output directory (assumed to exist)
mkdir "`out_table'" 2>nul

* Open log
log using "`logfile'", replace text

di "Starting PPML analysis: Restaurant density and cuisine entropy moderation on total crime"
di "Replicating specification from ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv"
di "$S_DATE $S_TIME"
di ""

* =========================================================
* Read and merge data
* =========================================================
di "Reading main data..."
import delimited using "`main_csv'", clear varnames(1) stringcols(_all) bindquote(strict) maxquotedrows(1000000)

keep cell_id county_code county_city period is_urban is_uv pop us_tariff_exposure_4 crime_count ///
     cat_stealing cat_fraud cat_robbery cat_extortion ///
     cat_public_security cat_violent_crimes cat_traffic_felony ///
     cat_smuggling cat_ip_infringement cat_counterfeiting ///
     cat_bribery cat_finance cat_prostitution cat_gambling ///
     cat_drugs cat_migration

destring cell_id county_code is_urban is_uv pop crime_count ///
         cat_stealing cat_fraud cat_robbery cat_extortion ///
         cat_public_security cat_violent_crimes cat_traffic_felony ///
         cat_smuggling cat_ip_infringement cat_counterfeiting ///
         cat_bribery cat_finance cat_prostitution cat_gambling ///
         cat_drugs cat_migration, replace force

destring us_tariff_exposure_4, replace force

di "Main data: " _N " observations"

di "Reading controls (cuisine entropy, restaurant density)..."
import delimited using "`ctrl_csv'", clear varnames(1) stringcols(_all) bindquote(strict) maxquotedrows(1000000)

keep cell_id period cuisine_entropy_2017 n_restaurants_2017

destring cuisine_entropy_2017 n_restaurants_2017, replace force

di "Controls data: " _N " observations"

di "Merging data..."
merge 1:1 cell_id period using "`main_csv'", keepusing(cell_id county_code county_city period is_urban is_uv pop us_tariff_exposure_4 crime_count ///
     cat_stealing cat_fraud cat_robbery cat_extortion ///
     cat_public_security cat_violent_crimes cat_traffic_felony ///
     cat_smuggling cat_ip_infringement cat_counterfeiting ///
     cat_bribery cat_finance cat_prostitution cat_gambling ///
     cat_drugs cat_migration) nogen

* Re-destring after merge
destring cell_id county_code is_urban is_uv pop crime_count ///
         cat_stealing cat_fraud cat_robbery cat_extortion ///
         cat_public_security cat_violent_crimes cat_traffic_felony ///
         cat_smuggling cat_ip_infringement cat_counterfeiting ///
         cat_bribery cat_finance cat_prostitution cat_gambling ///
         cat_drugs cat_migration us_tariff_exposure_4 ///
         cuisine_entropy_2017 n_restaurants_2017, replace force

di "Merged data: " _N " observations"
di ""

* =========================================================
* Generate variables
* =========================================================
gen year_num = real(substr(period, 1, 4))
gen half_num = real(substr(period, 6, 1))
gen post = (year_num > 2018) | (year_num == 2018 & half_num >= 2)

* Standardize tariff exposure
egen mu_us4 = mean(us_tariff_exposure_4)
egen sd_us4 = sd(us_tariff_exposure_4)
gen z_us4 = (us_tariff_exposure_4 - mu_us4) / sd_us4 if sd_us4 != 0

* Create ln(pop) offset
gen lnpop = log(pop)

* Create merged categories (4 major categories)
di "Creating crime categories..."
gen cat_property = cat_stealing + cat_fraud + cat_robbery + cat_extortion
gen cat_violent = cat_public_security + cat_violent_crimes + cat_traffic_felony
gen cat_corporate = cat_smuggling + cat_ip_infringement + cat_counterfeiting + cat_bribery + cat_finance
gen cat_underground = cat_prostitution + cat_gambling + cat_drugs + cat_migration

* Replace missing with 0 for category variables
foreach v in cat_property cat_violent cat_corporate cat_underground {
    replace `v' = 0 if missing(`v')
}
di "Categories created: property, violent, corporate, underground"
di ""

* Moderators: cuisine entropy and restaurant density
* Standardize moderators for interpretation
egen mu_entropy = mean(cuisine_entropy_2017)
egen sd_entropy = sd(cuisine_entropy_2017)
gen z_entropy = (cuisine_entropy_2017 - mu_entropy) / sd_entropy if sd_entropy != 0

egen mu_restdens = mean(n_restaurants_2017)
egen sd_restdens = sd(n_restaurants_2017)
gen z_restdens = (n_restaurants_2017 - mu_restdens) / sd_restdens if sd_restdens != 0

* Define subsamples (UV splits)
* uv_urban: UV grids in urban areas
* uv_nonurban: UV grids in non-urban areas
* uv_all: all UV grids (union of urban and non-urban UV)
gen str20 sample_group = ""
replace sample_group = "uv_urban" if is_uv == 1 & is_urban == 1
replace sample_group = "uv_nonurban" if is_uv == 1 & is_urban == 0
gen is_uv_all = (is_uv == 1)

di ""
di "=== Sample Distribution ==="
tab sample_group
count if is_uv == 1
di "uv_all (total UV grids): " r(N)
di ""

* =========================================================
* Function to run PPML with triple interaction
* =========================================================
* We'll use a program-like approach with loops

* Create city_period variable
gen city_period = county_city + "_" + period

* Encode fixed effects
encode county_code, gen(county_code_num)
encode city_period, gen(city_period_num)

* Store results
tempfile results
postfile handle str20 sample str15 yblock str20 ycat str10 moderator b_triple se_triple p_triple N_model rc using "`results'", replace

* =========================================================
* Run regressions
* =========================================================
di ""
di "=== Running PPML Regressions ==="

* Define subsamples
local subsamples "uv_all uv_urban uv_nonurban"

* Define yblocks
local yblock_names "merged4 totalcrime"
local merged4_yvars "cat_property cat_violent cat_corporate cat_underground"
local merged4_ycats "cat_property cat_violent cat_corporate cat_underground"
local totalcrime_yvars "crime_count"
local totalcrime_ycats "all"

* Define moderators
local mod_vars "z_entropy z_restdens"
local mod_labels "entropy restdens"

foreach ss of local subsamples {
    di ""
    di "--- Subsample: `ss' ---"

    * Filter data for this subsample
    if "`ss'" == "uv_all" {
        preserve
        keep if is_uv_all == 1
    }
    else if "`ss'" == "uv_urban" {
        preserve
        keep if sample_group == "uv_urban"
    }
    else if "`ss'" == "uv_nonurban" {
        preserve
        keep if sample_group == "uv_nonurban"
    }

    local sample_n = _N
    di "Sample size: " `sample_n'

    * Loop over yblocks
    foreach ybname in merged4 totalcrime {
        di ""
        di "  Y-block: `ybname'"

        if "`ybname'" == "merged4" {
            local yvars "`merged4_yvars'"
            local ycats "`merged4_ycats'"
        }
        else {
            local yvars "`totalcrime_yvars'"
            local ycats "`totalcrime_ycats'"
        }

        local i = 1
        foreach yvar of local yvars {
            local ycat : word `i' of `ycats'

            di "    Outcome: `ycat'"

            foreach mod_var of local mod_vars {
                local mod_label : word `:list posof "`mod_var'" in mod_vars' of `mod_labels'

                di "      Running: `mod_label'..."

                capture {
                    ppmlhdreg `yvar' c.z_us4##c.post##c.`mod_var' i.county_code_num i.city_period_num, offset(lnpop) cluster(county_code_num)

                    * Extract triple interaction coefficient
                    local coef_name = "c.z_us4#c.post#c.`mod_var'"
                    matrix b = e(b)
                    matrix V = e(V)

                    local colnames : colfullnames b
                    local found = 0
                    local j = 1
                    foreach cname of local colnames {
                        if "`cname'" == "`coef_name'" {
                            local b_triple = b[1, `j']
                            local se_triple = sqrt(V[`j', `j'])
                            local found = 1
                            continue, break
                        }
                        local j = `j' + 1
                    }

                    if `found' == 1 & !missing(`b_triple') & !missing(`se_triple') & `se_triple' != 0 {
                        local t_val = `b_triple' / `se_triple'
                        local p_val = 2 * normal(-abs(`t_val'))
                        local N_model = e(N)

                        di "      `mod_label': Coef=" `b_triple' ", SE=" `se_triple' ", p=" `p_val' ", N=" `N_model'

                        post handle ("`ss'") ("`ybname'") ("`ycat'") ("`mod_label'") (`b_triple') (`se_triple') (`p_val') (`N_model') (0)
                    }
                    else {
                        di "      `mod_label': FAILED (coefficient not found)"
                        post handle ("`ss'") ("`ybname'") ("`ycat'") ("`mod_label'") (. . . .) (1)
                    }
                }
                if _rc != 0 {
                    di "      `mod_label': FAILED (error code " _rc ")"
                    post handle ("`ss'") ("`ybname'") ("`ycat'") ("`mod_label'") (. . . .) (1)
                }

                local i = `i' + 1
            }
        }

        restore
    }
}

postclose handle

* =========================================================
* Export results
* =========================================================
di ""
di ""
di "=== Exporting Results ==="

use "`results'", clear

* Export CSV
export delimited using "`out_table'/ppml_100m_restentropy_mod_uvsplits_totalcrime_v1.csv", replace

di "CSV exported to: `out_table'/ppml_100m_restentropy_mod_uvsplits_totalcrime_v1.csv"

* Print summary table
di ""
di "=== Summary Table ==="
di "%-15s %-12s %-18s %-12s %12s %12s %12s %10s", "Sample", "Y-block", "Outcome", "Moderator", "Coef", "SE", "p-value", "N"
di "%-105s", ""

list sample yblock ycat moderator b_triple se_triple p_triple N_model, clean noobs

di ""
di "Done: PPML moderation analysis completed."

log close

di ""
di "Script completed successfully."
