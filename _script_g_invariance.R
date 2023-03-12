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
    var_exp_full,
    calc_var_exp(mdl_fitted_full)
  ),
  tar_target(
    scores_g_full,
    predict_g_score(indices_wider_clean, mdl_fitted_full)
  ),
  tarchetypes::tar_map_rep(
    result_cpm_g_full,
    command = tibble(
      cpm = do_cpm2(
        fc_data_matched,
        scores_g_full,
        thresh_method,
        thresh_level
      ) |> list()
    ),
    values = hypers_thresh_g,
    batches = 4,
    reps = 5
  ),
  tar_target(cpm_pred_g_full, extract_cpm_pred(result_cpm_g_full)),
  tar_target(brain_mask_g_full, extract_brain_mask(result_cpm_g_full)),
  tarchetypes::tar_file_read(
    indices_rapm,
    fs::path(store_behav, "indices_rapm"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_map_rep(
    result_cpm_rapm,
    command = tibble(
      cpm = do_cpm2(
        fc_data_matched,
        indices_rapm,
        thresh_method,
        thresh_level
      ) |> list()
    ),
    values = hypers_thresh_g,
    batches = 4,
    reps = 5
  ),
  tar_target(cpm_pred_rapm, extract_cpm_pred(result_cpm_rapm)),
  tar_target(brain_mask_rapm, extract_brain_mask(result_cpm_rapm)),
  g_invariance,
  tarchetypes::tar_combine(
    data_names,
    g_invariance$data_names,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "data_names",
        c("num_vars", "id_pairs")
      )
  ),
  tarchetypes::tar_combine(
    var_exp,
    g_invariance$var_exp,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "var_exp",
        c("num_vars", "id_pairs")
      )
  ),
  tarchetypes::tar_combine(
    scores_g,
    g_invariance$scores_g,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "scores_g",
        c("num_vars", "id_pairs")
      )
  ),
  tarchetypes::tar_combine(
    cpm_pred,
    g_invariance$cpm_pred,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "cpm_pred",
        c("num_vars", "id_pairs")
      )
  ),
  tarchetypes::tar_combine(
    brain_mask,
    g_invariance$brain_mask,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "brain_mask",
        c("num_vars", "id_pairs")
      )
  )
)
