library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs"
)
tar_source()
source("tar_mate/deprecated-targets.R")
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
    fc_data_rest_nn268_without,
    fs::path(store_fmri, "fc_data_rest_nn268_without"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    mdl_fitted_full,
    fit_model(indices_wider_clean)
  ),
  tar_target(
    var_exp_full,
    semTools::AVE(mdl_fitted_full)[["g"]]
  ),
  tar_target(
    scores_g_full,
    estimate_g_scores(indices_wider_clean)
  ),
  tarchetypes::tar_map_rep(
    result_cpm_g_full,
    command = do_cpm(
      fc_data_rest_nn268_without,
      scores_g_full,
      thresh_method,
      thresh_level
    ),
    values = hypers_thresh,
    batches = 10,
    reps = 10
  ),
  tar_target(
    cpm_pred_g_full,
    result_cpm_g_full |>
      select(edge_type, starts_with("thresh"), starts_with("tar"), cor) |>
      mutate(cor = map_dbl(cor, "estimate"))
  ),
  tar_target(
    mask_g_full,
    result_cpm_g_full |>
      select(edge_type, mask_prop) |>
      filter(!map_lgl(mask_prop, is.null)) |>
      summarise(
        mask = do.call(cbind, mask_prop) |>
          rowMeans() |>
          list(),
        .by = edge_type
      )
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
    values = hypers_thresh,
    batches = 10,
    reps = 10
  ),
  tar_target(
    cpm_pred_rapm,
    result_cpm_rapm |>
      select(edge_type, starts_with("thresh"), starts_with("tar"), cor) |>
      mutate(cor = map_dbl(cor, "estimate"))
  ),
  tar_target(
    mask_rapm,
    result_cpm_rapm |>
      select(edge_type, mask_prop) |>
      filter(!map_lgl(mask_prop, is.null)) |>
      summarise(
        mask = do.call(cbind, mask_prop) |>
          rowMeans() |>
          list(),
        .by = edge_type
      )
  ),
  # used for cpm batching (tar_rep cannot used with pattern)
  tar_target(index_batch_cpm, seq_len(10)),
  tar_target(index_rep_cpm, seq_len(10)),
  g_stability_pairs,
  tarchetypes::tar_combine(
    data_names_pairs,
    g_stability_pairs$data_names_pairs,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "data_names_pairs",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    scores_g_pairs,
    g_stability_pairs$scores_g_pairs,
    command = list(!!!.x) |>
      map(~ map(., bind_pairs) |> bind_rows()) |>
      bind_rows(.id = "id") |>
      clean_combined(
        "scores_g_pairs",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    scores_g_pairs_cor,
    g_stability_pairs$scores_g_pairs_cor,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "scores_g_pairs_cor",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    var_exp_pairs,
    g_stability_pairs$var_exp_pairs,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "var_exp_pairs",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    cpm_pred_pairs,
    g_stability_pairs$cpm_pred_pairs,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "cpm_pred_pairs",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    dice_mask_pairs,
    g_stability_pairs$dice_mask_pairs,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "dice_mask_pairs",
        names(hypers_stability_pairs)
      )
  ),
  g_stability_single,
  tarchetypes::tar_combine(
    data_names_single,
    g_stability_single$data_names_single,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "data_names_single",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    scores_g_single,
    g_stability_single$scores_g_single,
    command = list(!!!.x) |>
      map(bind_rows) |>
      bind_rows(.id = "id") |>
      clean_combined(
        "scores_g_single",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    var_exp_single,
    g_stability_single$var_exp_single,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "var_exp_single",
        names(hypers_stability_pairs)
      )
  ),
  tarchetypes::tar_combine(
    cpm_pred_single,
    g_stability_single$cpm_pred_single,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "cpm_pred_single",
        names(hypers_stability_pairs)
      )
  )
)
