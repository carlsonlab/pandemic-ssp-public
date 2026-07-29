## Targets pipeline for SSP driver panels

suppressPackageStartupMessages(source("packages.R"))

# Load all functions stored in R/.
targets::tar_source()

## Data input
data_input_targets <- tar_plan(
  
  tar_file(drivers_csv, "Data/Drivers-By status.csv"),
  
  drivers = read_csv(drivers_csv) %>%
    janitor::clean_names() %>% 
    rename(drivers_long = drivers_of_pandemic_risk_examples,
           drivers = drivers_of_pandemic_risk_short,
           narr_rep = narrative_representation,
           quant_proj = quantitative_projections),
  
  ssp_cols = c("SSP1" = "#31a354",
               "SSP2" = "#3182bd",
               "SSP3" = "#de2d26",
               "SSP4" = "#fdae6b",
               "SSP5" = "#c51b8a"),

  tar_file_read(pop,
                "Data/iamc_data-fe044e1c-0938-43a6-98be-c8ac4c9f4775.csv",
                read_csv(file = !!.x)),
  tar_file_read(pop65,
                "Data/iamc_data-db0edfb2-d3f5-4d7b-a0a0-ff01e522f2cd.csv",
                read_csv(file = !!.x)),
  tar_file_read(gdp,
                "Data/iamc_data-8482d5d2-b0c2-473d-9721-6c40bcfb2f70.csv",
                read_csv(file = !!.x)),
  tar_file_read(gini,
                "Data/iamc_data-d1a9e616-8570-499a-b2d1-02356124e16b.csv",
                read_csv(file = !!.x)),
  tar_file_read(cardiovascular,
                "Data/iamc_data-233e4d19-a1af-43ee-9315-9af8786b22df.csv",
                read_csv(file = !!.x)),
  tar_file_read(gender,
                "Data/iamc_data-7453dd29-cc85-439c-bfd4-bdfe6e1f422e.csv",
                read_csv(file = !!.x)),
  tar_file_read(builtup,
                "Data/iamc_data-844a52bb-6a5e-496b-83b5-2aba8febdfd3.csv",
                read_csv(file = !!.x)),
  tar_file_read(forest,
                "Data/iamc_data-550a19cd-f6e9-431a-b7e7-13083fa84d11.csv",
                read_csv(file = !!.x)),
  tar_file_read(livestock_prod,
                "Data/iamc_data-2d356d3a-040c-42ef-a51f-8d017eaf605a.csv",
                read_csv(file = !!.x))
  )

## Data processing
data_processing_targets <- tar_plan(
  population_plot_data = pop |>
    lc_and_pivot() |>
    filter_population_world_baseline() |>
    prep_population_plot_data() |>
    rescale_summary_units(1000) |>
    dplyr::mutate(scenario = ssp),

  population65_plot_data = pop65 |>
    lc_and_pivot() |>
    filter_population65_world_baseline() |>
    prep_population65_plot_data(),

  gdp_plot_data = gdp |>
    lc_and_pivot() |>
    filter_gdp_world_ssp() |>
    prep_gdp_plot_data(),

  gini_plot_data = gini |>
    lc_and_pivot() |>
    filter_gini_world_ssp() |>
    prep_gini_plot_data(),

  cardiovascular_plot_data = cardiovascular |>
    lc_and_pivot() |>
    filter_cardiovascular_ssp() |>
    prep_cardiovascular_plot_data(),

  gender_plot_data = gender |>
    lc_and_pivot() |>
    filter_gender_world_ssp() |>
    prep_gender_plot_data(),

  builtup_plot_data = builtup |>
    lc_and_pivot() |>
    filter_builtup_world_baseline() |>
    prep_builtup_plot_data(),

  forest_plot_data = forest |>
    lc_and_pivot() |>
    filter_forest_world_baseline() |>
    prep_forest_plot_data(),

  livestock_plot_data = livestock_prod |>
    lc_and_pivot() |>
    filter_livestock_world_baseline() |>
    prep_livestock_plot_data()
)

##Outputs
outputs_targets <- tar_plan(
  
  fig_dendrogram = plot_drivers_dendrogram(drivers),
  
  fig_barplot = plot_driver_representation(drivers),
  
  p_population = plot_total_population(population_plot_data, ssp_cols),
  p_population65 = plot_population65(population65_plot_data, ssp_cols),
  p_gdp = plot_gdp_ppp(gdp_plot_data, ssp_cols),
  p_gini = plot_gini_income_inequality(gini_plot_data, ssp_cols),
  p_cardiovascular = plot_cardiovascular_disease(cardiovascular_plot_data,
                                                 ssp_cols),
  p_gender = plot_gender_inequality(gender_plot_data, ssp_cols),
  p_builtup = plot_builtup_area(builtup_plot_data, ssp_cols),
  p_forest = plot_forest_area(forest_plot_data, ssp_cols),
  p_livestock = plot_livestock_production(livestock_plot_data, ssp_cols),
  
  figure3_combined = plot_trajectories(p_population, p_gdp, p_gini,
                                       p_population65, p_cardiovascular, p_gender,
                                       p_builtup, p_forest, p_livestock, 
                                       ssp_cols)
)

# Plotting
plotting_targets <- tar_plan(
  
  fig_1_png = ggsave("Figures/Figure1.png",
                     fig_dendrogram,
                     width = 12, height = 12, units = "in", 
                     dpi = 600, bg = "white"),
  
  fig_2_png = ggsave("Figures/Figure2.png",
                     fig_barplot,
                     width = 13, height = 5, units = "in", 
                     dpi = 600, bg = "white"),
  
  fig_3_png = ggsave("Figures/Figure3.png",
                     figure3_combined,
                     width = 4.5, height = 4.5, units = "in", dpi = 600,
                     bg = "white"),
  
  fig_3_pdf = ggsave("Figures/Figure3.pdf",
                     figure3_combined,
                     width = 4.5, height = 4.5, units = "in", 
                     dpi = 600, bg = "white")
  
)

list(
  data_input_targets,
  data_processing_targets,
  outputs_targets,
  plotting_targets
)
