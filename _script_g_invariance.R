library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan", "collapse"),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  error = "null",
  format = "qs"
)
tar_source()
future::plan(future.callr::callr)
store_behav <- fs::path(
  tar_config_get("store", project = "project_behav"),
  "objects"
)
store_fmri <- fs::path(
  tar_config_get("store", project = "project_task_fmri"),
  "objects"
)
list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_behav, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    fc_data_matched,
    fs::path(store_fmri, "fc_data_rest_nn268_without"),
    read = qs::qread(!!.x) |>
      filter(sub_id %in% indices_wider_clean$sub_id)
  ),
  # first column is identifier
  tar_target(data_names_all, names(indices_wider_clean)[-1]),
  tar_target(
    mdl_fitted_full,
    fit_g(indices_wider_clean, all_of(data_names_all))
  ),
  tar_target(
    scores_g_full,
    predict_g_score(indices_wider_clean, mdl_fitted_full)
  ),
  tarchetypes::tar_map_rep(
    result_cpm_g_full,
    command = do_cpm(
      fc_data_rest_nn268_without,
      scores_g_full,
      thresh_method,
      thresh_level
    ),
    values = hypers_thresh_g,
    batches = 2,
    reps = 10
  ),
  tarchetypes::tar_file_read(
    indices_rapm,
    fs::path(store_behav, "indices_rapm"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_map_rep(
    result_cpm_rapm,
    command = do_cpm(
      fc_data_rest_nn268_without,
      indices_rapm,
      thresh_method,
      thresh_level
    ),
    values = hypers_thresh_g,
    batches = 2,
    reps = 10
  ),
  g_invariance
)
