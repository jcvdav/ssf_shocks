# Repository for `Impacts of marine heatwaves on small-scale fisheries`

## Replication info
This repository is equipped with an renv lockfile recording all versions of R packages used in the analysis. If you wish to replicate our environment, use renv::restore() prior to running any portion of the code. A Makefile is still being constructed.

## Repository structure 

```
-- data
   |__estimation_panels
      |__env_eco_panel.rds
      |__env_fish_panel.rds
   |__img
      |__fishing_methods.pdf
      |__icons.pptx
      |__Lobster_90.png
      |__lobster_fishing.jpeg
      |__Lobster.png
      |__Sea cucumber_90.png
      |__Sea cucumber.png
      |__sea_cucumber_fishing.jpg
      |__Urchin.png
   |__output
      |__con_p_mhw_threshold_future.rds
      |__con_p_mhw_threshold.rds
      |__effect_on_fishery_and_biophysical.rds
      |__effect_on_fishery_models.rds
      |__p_mhw_occurs_future.rds
      |__p_mhw_occurs_hindcast.rds
      |__p_mhw_occurs.rds
      |__reference_mhw_by_TURF.rds
      |__total_annual_normalized_landigs.rds
   |__processed
      |__annual_ecological_panel.rds
      |__annual_environmental_panel.rds
      |__annual_fishery_panel.rds
      |__annual_mean_sst_by_turf.rds
      |__centroids.gpkg
      |__CMIP_SST_projections
      |__cpi_t_rates.rds
      |__daily_mean_sst_by_turf.rds
      |__depth_and_area.rds
      |__future_mhw_by_turf.rds
      |__mean_historical_eu_landings.rds
      |__mhw_by_turf.rds
      |__turf_polygons.gpkg
   |__raw
      |__climate_model_output
      |__daily_sst
      |__GEBCO_03_Feb_2023_58106760a1c2
      |__natividad_invert_transects
      |__OECD_CPI_MEXCPIALLMINMEI.csv
      |__std_climate_model_output
      |__turf_polygons.gpkg
-- docs
   |__img
      |__effect_size.pdf
      |__effect_size.png
      |__ero_lobster.jpeg
      |__ero_lobster.jpg
      |__fishery_ts.pdf
      |__fishery_ts.png
      |__focus_study_area.pdf
      |__focus_study_area.png
      |__miguel_lobster.jpeg
      |__panel_figure.pdf
      |__panel_figure.png
      |__sst_anom_turfs.pdf
      |__sst_anom_turfs.png
      |__sst_anom.pdf
      |__sst_anom.png
      |__sst_ts.pdf
      |__sst_ts.png
   |__index.html
   |__index.md
   |__libs
      |__header-attrs-2.14
   |__surf_n_turf.html
   |__surf_n_turf.Rmd
   |__VillasenorDerbez_mhw_ssf_cover_letter.pdf
   |__VillasenorDerbez_mhw_ssf_manuscript.pdf
   |__xaringan-themer.css
-- LICENSE
-- README.md
-- renv
-- renv.lock
   |__activate.R
   |__library
      |__R-4.3
   |__settings.json
   |__staging
-- results
   |__img
      |__fig01_study_area_map.pdf
      |__fig01_study_area_map.png
      |__fig02_mhw_stats.pdf
      |__fig02_mhw_stats.png
      |__fig03_fishery_ts.pdf
      |__fig03_fishery_ts.png
      |__fig04_effect_on_fishery.pdf
      |__fig04_effect_on_fishery.png
      |__fig05_biophysical_vs_effect.pdf
      |__fig05_biophysical_vs_effect.png
      |__fig06_future_mhw_plot.pdf
      |__fig06_future_mhw_plot.png
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
   |__tab
      |__tab01_biophysical_vs_effect.tex
      |__tabS01_mhw_summary_stats.tex
      |__tabS02_CMIP6_info.tex
      |__tabS03_future_MHW_anova.tex
      |__tabS04_future_MHW_hsd.tex
      |__tabS05_hist_max_mhw_ref_points.tex
      |__tabS06_future_extreme_MHW_hsd.tex
-- scripts
   |__00_collect_externals
      |__01_bring_turf_polygons.R
      |__02_filter_landings_data_and_turf_polygons.R
   |__01_processing
      |__01_fishery
      |__02_ecological
      |__03_environmental
      |__04_make_estimation_panels
      |__05_climate_projections
   |__02_analysis
      |__01_historical_MHW_frequency.R
      |__02_historical_MHW_intensity.R
      |__03_period_fishery_anovas.R
      |__04_effect_of_mhw_on_fishery.R
      |__05_effect_on_fishery_and_biophysical.R
      |__06_future_MHW.R
      |__99_effect_on_density.R
   |__03_content
      |__fig01_study_area_map.R
      |__fig02_mhw_stats.R
      |__fig03_plot_fishery_ts.R
      |__fig04_plot_effect_on_fishery.R
      |__fig05_plot_biophysical_vs_effect.R
      |__fig06_future_mhw_plot.R
      |__fig07_future_mhw_and_coefficients.R
      |__fig08_plot_env_ecol.R
      |__figS01_corplot.R
      |__figS02_mhw_vs_enso.R
      |__figS03_coef_plots_for_eus_many_species.R
      |__figS04_regime_change_test_plot.R
      |__figS05_lag_pairs_plot.R
      |__figS06_fe_vs_re_coefficients.R
      |__tabS01_mhw_summary_stats.R
      |__tabS5_hist_max_mhw_ref_points.R
-- ssf_shocks.Rproj
```

---------
