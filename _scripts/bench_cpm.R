library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan", "collapse"),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  error = "abridge",
  format = "qs",
  controller = crew::crew_controller_local(
    name = "local",
    workers = 8,
    seconds_idle = 60
  )
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    subjs_combined,
    file_subjs_combined,
    read = scan(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    scores_rapm,
    fs::path(store_preproc_behav, "objects", "scores_rapm"),
    read = qs::qread(!!.x)
  ),
  prepare_permute_cpm2(
    dplyr::filter(config, grepl("eq", cond) & acq == "reg"),
    hypers_cpm = hypers_cpm,
    behav = tibble::tibble(scores = list(scores_rapm)),
    subjs_subset = subjs_combined,
    subjs_info = subjs_covariates,
    covars = c("age", "sex")
  )
)
