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
  tarchetypes::tar_file_read(
    indices_rapm,
    fs::path(store_behav, "indices_rapm"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    behav_main,
    tribble(
      ~idx, ~scores,
      "g_full", scores_g_full,
      "rapm", indices_rapm
    )
  ),
  tarchetypes::tar_map_rep(
    result_cpm_main,
    command = behav_main |>
      mutate(
        cpm = map(
          scores,
          ~ do_cpm2(
            fc_data_matched,
            .,
            thresh_method = thresh_method,
            thresh_level = thresh_level
          )
        ),
        .keep = "unused"
      ),
    values = hypers_thresh_g,
    batches = 4,
    reps = 5
  ),
  tar_target(cpm_pred_main, extract_cpm_pred(result_cpm_main)),
  tar_target(brain_mask_main, extract_brain_mask(result_cpm_main)),
  g_invariance,
  combine_targets(
    data_names,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    var_exp,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    scores_g,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    cpm_pred,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    brain_mask,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  tar_target(
    dice_mask_pairs,
    brain_mask |>
      filter(n() == 2, .by = c(num_vars, idx_rsmp, starts_with("thresh"))) |>
      pivot_longer(
        c(pos, neg),
        names_to = "edge_type",
        values_to = "mask"
      ) |>
      mutate(mask_bin = map(mask, ~ . > 0.995), .keep = "unused") |>
      pivot_wider(
        id_cols = c(num_vars, idx_rsmp, starts_with("thresh"), edge_type),
        names_from = id_pairs,
        values_from = mask_bin
      ) |>
      mutate(
        dice = map2_dbl(
          `1`, `2`,
          ~ rbind(.x, .y) |>
            proxy::simil(method = "dice") |>
            unclass()
        ),
        .keep = "unused"
      )
  )
)
