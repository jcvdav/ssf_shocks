# Repository for: 'Impacts of Marine Heatwaves on Small-Scale Fisheries Operating Near Species’ Equatorward Range Edges'

## Replication info

This repository is equipped with an `renv` lockfile recording all versions of R packages used in the analysis. If you wish to replicate our environment, use `renv::restore()` prior to running any portion of the code. A `Makefile` is still being constructed.

## Repository structure 

```
-- 00_collect_externals
   |__01_bring_turf_polygons.R
   |__02_filter_landings_data_and_turf_polygons.R
-- 01_processing
   |__01_fishery
      |__01_clean_cpi_t.R
      |__02_make_centroids.R
      |__03_mean_historical_eu_landings.R
   |__02_ecological
      |__01_clean_invertebrate_transects.R
   |__03_environmental
      |__01_download_daily_sst.R
      |__02_rasterize_daily_sst.R
      |__03_extract_daily_sst.R
      |__04_calculate_mhw.R
      |__05_make_annual_environmental_panel.R
      |__06_depth_and_area.R
   |__04_make_estimation_panels
      |__01_make_env_ecol.R
      |__02_make_env_fish.R
   |__05_climate_projections
      |__01_download_CMIP6_tos_data.R
      |__02_standardize_CMIP6_SST_data.R
      |__03_extract_daily_CMIP_sst.R
      |__04_calculate_CMIP_mhw.R
      |__06_calculate_future_probabilities.R
-- 02_analysis
   |__01_historical_MHW_frequency.R
   |__02_historical_MHW_intensity.R
   |__03_future_MHW.R
   |__04_period_fishery_anovas.R
   |__05_effect_of_mhw_on_fishery.R
   |__06_effect_on_fishery_and_biophysical.R
   |__07_effect_on_density.R
-- 03_content
   |__fig01_study_area_map.R
   |__fig02_mhw_stats.R
   |__fig03_future_mhw_plot.R
   |__fig04_plot_fishery_ts.R
   |__fig05_plot_effect_on_fishery.R
   |__fig06_plot_biophysical_vs_effect.R
   |__fig07_future_mhw_and_coefficients.R
   |__fig08_plot_env_ecol.R
   |__figS01_corplot.R
   |__figS02_mhw_vs_enso.R
   |__figS03_coef_plots_for_eus_many_species.R
   |__figS04_regime_change_test_plot.R
   |__figS05_lag_pairs_plot.R
   |__figS06_fe_vs_re_coefficients.R
   |__tab_S4_hist_max_mhw_ref_points.R
-- img
   |__fig01_study_area_map.pdf
   |__fig01_study_area_map.png
   |__fig02_mhw_stats.pdf
   |__fig02_mhw_stats.png
   |__fig03_future_mhw_plot.pdf
   |__fig03_future_mhw_plot.png
   |__fig04_fishery_ts.pdf
   |__fig04_fishery_ts.png
   |__fig05_effect_on_fishery.pdf
   |__fig05_effect_on_fishery.png
   |__fig06_biophysical_vs_effect.pdf
   |__fig06_biophysical_vs_effect.png
   |__fig07_future_mhw_and_coefficients.pdf
   |__fig07_future_mhw_and_coefficients.png
   |__fig08_env_ecol.pdf
   |__fig08_env_ecol.png
   |__figS01_corrplot.pdf
   |__figS02_mhw_vs_enso.pdf
   |__figS02_mhw_vs_enso.png
   |__figS03_coef_plots_for_eus_many_spp.pdf
   |__figS03_coef_plots_for_eus_many_spp.png
   |__figS04_regime_change_test_plot.R.pdf
   |__figS04_regime_change_test_plot.R.png
   |__figS05_lag_pairs_plot.pdf
   |__figS05_lag_pairs_plot.png
   |__figS06_fe_vs_re_coefficients.pdf
   |__figS06_fe_vs_re_coefficients.png
-- tab
   |__effect_sizes.md
```

---------
