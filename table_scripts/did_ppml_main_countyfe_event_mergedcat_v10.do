clear all
set more off

global ROOT "."
global OUT_TABLE "$ROOT/output_tables"
global LOGFILE "$OUT_TABLE/did_ppml_main_countyfe_event_mergedcat_v10.log"

capture mkdir "$ROOT/table"
capture mkdir "$OUT_TABLE"

capture log close
log using "$LOGFILE", text replace

capture which ppmlhdfe
if _rc ssc install ppmlhdfe, replace
capture which esttab
if _rc ssc install estout, replace
capture which estadd
if _rc ssc install estout, replace

local PPML_OPTS "separation(fe ir) keepsingletons tolerance(1e-6)"

program define add_common_stats
    capture noisily lincom 1.post#c.z_us4
    if _rc {
        estadd scalar p_main = ., replace
    }
    else {
        estadd scalar p_main = r(p), replace
    }

    capture confirm scalar e(r2_p)
    if _rc {
        estadd scalar r2_p = ., replace
    }
    else {
        estadd scalar r2_p = e(r2_p), replace
    }
end

program define prep_base
    args datafile

    import delimited "`datafile'", clear varnames(1) encoding(utf8)
    destring county_code, replace force

    gen double lnpop = ln(pop)
    egen double z_us4 = std(us_tariff_exposure_4)

    gen int year_num = real(substr(period,1,4))
    gen byte half_num = real(substr(period,6,1))
    gen byte post = (year_num>2018) | (year_num==2018 & half_num>=2)
    egen long period_id = group(period)
    egen long city_period = group(county_city period)

    * event time: baseline at 2018h1 -> evtt = -1
    gen int half_index = (year_num - 2015) * 2 + half_num
    gen int evtt = half_index - 7
end

program define run_main_multiscale_countyfe
    estimates clear

    local f100 "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
    local f500 "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
    local f1k  "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"

    local tags "100m 500m 1km"
    local files `"`f100' `f500' `f1k'"'

    local mlist ""
    local mtitles ""

    forvalues i=1/3 {
        local tag : word `i' of `tags'
        local df  : word `i' of `files'

        prep_base "`df'"

        local samples "full urban uv"
        local cond_full "1"
        local cond_urban "is_urban==1"
        local cond_uv "is_uv==1"

        foreach s in `samples' {
            local cond = "`cond_`s''"
            quietly ppmlhdfe crime_count c.z_us4##i.post if `cond', ///
                absorb(county_code period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'

            add_common_stats
            estadd local fe_spec "county + period + city_period", replace
            estimates store m_`tag'_`s'

            local mlist "`mlist' m_`tag'_`s'"
            local mtitles `"`mtitles' `tag'-`s'"'
        }
    }

    esttab `mlist' using "$OUT_TABLE/ppml_main_multiscale_3x3_usz_big_v9_countyfe.rtf", replace ///
        keep(1.post#c.z_us4) order(1.post#c.z_us4) ///
        coeflabels(1.post#c.z_us4 "Post*z_us4") ///
        mtitle(`mtitles') ///
        b(%9.4f) se(%9.4f) star(+ 0.15 * 0.10 ** 0.05 *** 0.01) compress ///
        stats(N r2_p p_main fe_spec, labels("N" "Pseudo R2" "P-value(Post*z_us4)" "FE spec") fmt(%12.0f %9.4f %9.4f %s)) ///
        title("Main PPML DID (all crimes): county FE + period FE + city-period FE")
end

program define run_event_study_uv_countyfe
    tempfile ev100 ev500 ev1k

    local f100 "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
    local f500 "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
    local f1k  "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"

    local tags "100m 500m 1km"
    local files `"`f100' `f500' `f1k'"'
    local stores `"`ev100' `ev500' `ev1k'"'

    forvalues i=1/3 {
        local tag : word `i' of `tags'
        local df  : word `i' of `files'
        local out : word `i' of `stores'

        prep_base "`df'"

        capture drop evtt2
        gen int evtt2 = evtt + 7
        fvset base 6 evtt2

        quietly ppmlhdfe crime_count i.evtt2##c.z_us4 if is_uv==1, ///
            absorb(county_code period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'

        postfile coefh int evtt double b se lb ub p using `out', replace

        forvalues k=-6/5 {
            if `k'==-1 {
                post coefh (`k') (0) (0) (0) (0) (.)
            }
            else {
                local lev = `k' + 7
                capture noisily lincom `lev'.evtt2#c.z_us4
                if _rc {
                    post coefh (`k') (.) (.) (.) (.) (.)
                }
                else {
                    post coefh (`k') (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
                }
            }
        }
        postclose coefh

        preserve
        use `out', clear
        twoway (rcap ub lb evtt, lcolor(gs8)) ///
               (line b evtt, lcolor(navy) lwidth(medthick)) ///
               (scatter b evtt, mcolor(navy) msymbol(O)), ///
               yline(0, lpattern(dash) lcolor(gs8)) ///
               xline(-0.5, lpattern(dash) lcolor(maroon)) ///
               title("Event study (UV, `tag')") ///
               xtitle("Event time (base = 2018h1)") ///
               ytitle("Coef on z_us4 x event-time") ///
               legend(off) ///
               name(g_`tag', replace)
        graph export "$OUT_TABLE/event_uv_countyfe_`tag'_v10.png", replace width(1800)
        restore
    }

    graph combine g_100m g_500m g_1km, col(1) imargin(2 2 2 2)
    graph export "$OUT_TABLE/event_uv_countyfe_multiscale_v10.png", replace width(1600)
end

program define run_merged_cat_three_scales
    local f100 "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
    local f500 "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"
    local f1k  "$ROOT/input_data/grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv"

    local tags "100m 500m 1km"
    local files `"`f100' `f500' `f1k'"'

    forvalues i=1/3 {
        local tag : word `i' of `tags'
        local df  : word `i' of `files'

        prep_base "`df'"

        foreach y in cat_stealing cat_fraud cat_robbery cat_extortion ///
                     cat_public_security cat_violent_crimes cat_traffic_felony ///
                     cat_smuggling cat_ip_infringement cat_counterfeiting cat_bribery cat_finance ///
                     cat_prostitution cat_gambling cat_drugs cat_migration {
            capture confirm variable `y'
            if _rc gen double `y' = 0
            replace `y' = 0 if missing(`y')
        }

        capture drop cat_property cat_violent cat_corporate cat_underground
        gen double cat_property = cat_stealing + cat_fraud + cat_robbery + cat_extortion
        gen double cat_violent = cat_public_security + cat_violent_crimes + cat_traffic_felony
        gen double cat_corporate = cat_smuggling + cat_ip_infringement + cat_counterfeiting + cat_bribery + cat_finance
        gen double cat_underground = cat_prostitution + cat_gambling + cat_drugs + cat_migration

        capture drop base_uv
        gen byte base_uv = is_uv==1 & !missing(lnpop, z_us4, post, cell_id, period_id, city_period, county_code)
        quietly count if base_uv==1
        local N_fixed = r(N)

        estimates clear
        local mlist ""
        local mtitles "property violent corporate underground"

        foreach y in cat_property cat_violent cat_corporate cat_underground {
            quietly ppmlhdfe `y' c.z_us4##i.post if base_uv==1, ///
                absorb(cell_id period_id city_period) offset(lnpop) vce(cluster county_code) `PPML_OPTS'

            add_common_stats
            estadd scalar N_fixed = `N_fixed', replace
            estimates store m_`y'
            local mlist "`mlist' m_`y'"
        }

        esttab `mlist' using "$OUT_TABLE/ppml_cat_merged_subsample_`tag'_usz_v10.rtf", replace ///
            keep(1.post#c.z_us4) order(1.post#c.z_us4) ///
            coeflabels(1.post#c.z_us4 "Post*z_us4") ///
            mtitle(`mtitles') ///
            b(%9.4f) se(%9.4f) star(+ 0.15 * 0.10 ** 0.05 *** 0.01) compress ///
            stats(N_fixed r2_p p_main, labels("N" "Pseudo R2" "P-value(Post*z_us4)") fmt(%12.0f %9.4f %9.4f)) ///
            title("Merged category PPML DID (`tag', UV sample)")
    }
end

run_main_multiscale_countyfe
run_event_study_uv_countyfe
run_merged_cat_three_scales

di "Done: county-FE main table, UV event-study graphs, and merged-category tables exported."
log close
