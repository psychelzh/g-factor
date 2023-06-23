library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan", "collapse"),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  error = "null",
  format = "qs",
  controller = crew::crew_controller_local(workers = 10)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

list(
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "objects", "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_combined,
    file_subjs_combined,
    read = as.numeric(read_lines(!!.x))
  ),
  prepare_permute_cpm2(
    config_neural |>
      dplyr::filter(parcel == "nn268", filt == "bandpass", gsr == "with"),
    hypers_cpm, behav_main,
    dir_neural = "data/reg_site",
    tar_name_neural = "file_neural_reg_site",
    subjs_subset = subjs_combined,
    name_suffix = "_reg_site",
    subjs_info = subjs_covariates,
    covars = "site"
  ),
  prepare_permute_cpm2(
    config_neural |>
      dplyr::filter(parcel == "nn268", filt == "bandpass", gsr == "with"),
    hypers_cpm, behav_main,
    subjs_subset = subjs_combined,
    name_suffix = "_reg_sex",
    subjs_info = subjs_covariates,
    covars = "sex"
  ),
  prepare_permute_cpm2(
    config_neural |>
      dplyr::filter(parcel == "nn268", filt == "bandpass", gsr == "with"),
    hypers_cpm, behav_main,
    include_file_targets = FALSE,
    subjs_subset = subjs_combined,
    name_suffix = "_reg_site_orig",
    subjs_info = subjs_covariates,
    covars = "site"
  )
)
