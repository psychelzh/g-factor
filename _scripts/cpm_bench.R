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
    workers = 16,
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
  tarchetypes::tar_file_read(
    scores_spearman,
    fs::path(store_preproc_behav, "objects", "scores_spearman"),
    read = qs::qread(!!.x)
  ),
  prepare_permute_cpm2(
    dplyr::filter(
      config,
      stringr::str_detect(cond, "eq") &
        !stringr::str_detect(cond, "run1rest"),
      acq == "reg",
      parcel == "nn268"
    ),
    hypers_cpm = hypers_cpm,
    behav = tibble::tibble(
      index = c("spearman", "rapm"),
      scores = list(scores_spearman, scores_rapm)
    ),
    subjs_subset = subjs_combined,
    subjs_info = subjs_covariates,
    covars = c("age", "sex")
  ),
  prepare_permute_cpm2(
    dplyr::filter(
      config,
      stringr::str_detect(cond, "eq") &
        !stringr::str_detect(cond, "run1rest"),
      acq == "reg",
      parcel == "nn268",
      gsr == "with"
    ),
    hypers_cpm = hypers_cpm |>
      dplyr::filter(
        thresh_method == "alpha",
        thresh_level == 0.01
      ),
    behav = tibble::tibble(
      index = c("spearman", "rapm"),
      scores = list(scores_spearman, scores_rapm)
    ),
    subjs_subset = subjs_combined,
    subjs_info = subjs_covariates,
    covars = c("age", "sex", "site"),
    include_file_targets = FALSE,
    name_suffix = "reg_site"
  ),
  prepare_permute_cpm2(
    dplyr::filter(
      config,
      stringr::str_detect(cond, "eq") &
        !stringr::str_detect(cond, "run1rest"),
      acq == "reg",
      parcel == "nn268",
      gsr == "with"
    ),
    hypers_cpm = hypers_cpm |>
      dplyr::filter(
        thresh_method == "alpha",
        thresh_level == 0.01
      ),
    behav = tibble::tibble(
      index = c("spearman", "rapm"),
      scores = list(
        shuffle_data(filter(scores_spearman, sub_id %in% subjs_combined)),
        shuffle_data(filter(scores_rapm, sub_id %in% subjs_combined))
      )
    ),
    subjs_subset = subjs_combined,
    subjs_info = subjs_covariates,
    covars = c("age", "sex"),
    include_file_targets = FALSE,
    name_suffix = "perm",
    batches = 100,
    reps = 10
  )
)
