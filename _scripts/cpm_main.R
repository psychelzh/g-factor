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
  controller = crew::crew_controller_local(workers = 8)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "objects", "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_combined,
    file_subjs_combined,
    read = scan(!!.x)
  ),
  prepare_permute_cpm2(
    config_neural,
    hypers_cpm,
    behav_main,
    subjs_subset = subjs_combined,
    name_suffix = "_main"
  ),
  prepare_permute_cpm2(
    config_neural,
    hypers_cpm,
    behav_main,
    dir_neural = "data/reg_covars2",
    tar_name_neural = "file_neural_reg_covars2",
    subjs_subset = subjs_combined,
    name_suffix = "_main_reg_covars2",
    subjs_info = subjs_covariates,
    covars = c("age", "sex")
  )
)
