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

config <- config |>
  dplyr::filter(
    acq == "reg",
    cond == "nbackrun1",
    parcel == "nn268",
    gsr == "with"
  )
hypers_cpm <- hypers_cpm |>
  dplyr::filter(
    thresh_method == "alpha",
    thresh_level == 0.01
  )

cpm_main <- tarchetypes::tar_map(
  values = tibble::tibble(
    trait = names(meas_trait),
    name_scores = paste0("scores_", trait),
  ),
  names = trait,
  list(
    tarchetypes::tar_file_read(
      behav,
      fs::path(store_preproc_behav, "objects", name_scores),
      read = qs::qread(!!.x)
    ),
    prepare_permute_cpm2(
      config,
      hypers_cpm,
      # bifactor model gives more than 1 score, keep the first only
      tibble::tibble(scores = list(behav[, 1:2])),
      include_file_targets = FALSE,
      subjs_subset = subjs_combined,
      subjs_info = subjs_covariates,
      covars = c("age", "sex")
    )
  )
)

# targets pipeline ----
list(
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
  prepare_permute_cpm2(config),
  cpm_main,
  lapply(
    rlang::exprs(
      cpm_pred,
      brain_mask
    ),
    combine_targets,
    targets = cpm_main,
    cols_targets = "trait"
  )
)
