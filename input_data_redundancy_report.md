# input_data ??????

??????????? + ???? + ?????????????

## ????
- `grid_halfyear_panel_100m_judicial_exposure_v3_with_housing.csv`: rows=32909352, cols=67, size=19195568121
- `grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv`: rows=32909352, cols=67, size=13593665983
- `grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv`: rows=32909352, cols=19, size=6887688333
- `ppml_100m_uv_totalcrime_spatial_permutation_v1_r.csv`: rows=1000, cols=4, size=61102
- `ppml_100m_housingmod_uvsplits_alluv_v3a.csv`: rows=120, cols=10, size=14315
- `ppml_uv_100m_cat_four_subsamples_v2.csv`: rows=80, cols=8, size=8018
- `baseline_diff_uv_vs_urban_nonuv_100m_v1.csv`: rows=20, cols=22, size=5760
- `ols_did_100m_ntl_pop_housing_four_subsamples_v1.csv`: rows=32, cols=10, size=4287
- `ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv`: rows=28, cols=9, size=3046
- `ppml_100m_restentropy_mod_uvsplits_totalcrime_only_v1.csv`: rows=6, cols=9, size=657
- `ppml_100m_uv_totalcrime_robust_se_compare_v1.csv`: rows=4, cols=5, size=293
- `ppml_100m_uv_totalcrime_policy_timing_placebo_v1.csv`: rows=3, cols=5, size=186
- `ppml_100m_uv_totalcrime_extra_robust_v1.csv`: rows=2, cols=5, size=130

## ????????A ??? ? B ????
- `ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv` ? `ppml_100m_housingmod_uvsplits_alluv_v3a.csv` (cols 9/10, rows 28/120)
- `ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv` ? `ppml_100m_restentropy_mod_uvsplits_totalcrime_only_v1.csv` (cols 9/9, rows 28/6)
- `ppml_100m_restentropy_mod_uvsplits_totalcrime_only_v1.csv` ? `ppml_100m_housingmod_uvsplits_alluv_v3a.csv` (cols 9/10, rows 6/120)
- `ppml_100m_restentropy_mod_uvsplits_totalcrime_only_v1.csv` ? `ppml_100m_restentropy_mod_uvsplits_alluv_totalcrime_housentl_v2.csv` (cols 9/9, rows 6/28)

## ????????
- `grid_halfyear_panel_100m_judicial_exposure_v3_with_housing.csv` vs `grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv` keys=['is_urban_village_grid', 'year', 'period'], sample_intersection=2, overlap=25.00%/33.33%

## ??????
- `grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv` ? `grid_halfyear_panel_100m_judicial_exposure_v3_with_housing_noradiusmerge_v3.csv` ????????? + ??????????????? `grid_halfyear_panel_100m_controls_housing3nn_noradius_cuisine_ntl_v1.csv`?????=12?
- ??????????? `import/use` ??????????