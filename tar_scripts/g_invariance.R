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
  controller = crew::crew_controller_local(workers = 8, auto_scale = "one")
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)
store_behav <- fs::path(
  tar_config_get("store", project = "project_behav"),
  "objects"
)
store_modality_comparison <- fs::path(
  tar_config_get("store", project = "project_modality_comparison"),
  "objects"
)

# prepare static branches targets ----
max_num_vars <- 20 # we have 20 indicators in total (can be more)
cfg_rsmp_vars <- withr::with_seed(
  1,
  dplyr::bind_rows(
    tidyr::expand_grid(
      num_vars = round(seq(3, floor(max_num_vars / 2), length.out = 5)),
      idx_rsmp = seq_len(100)
    ) |>
      dplyr::reframe(
        purrr::map(
          num_vars,
          ~ data.frame(
            id_pairs = rep(c(1, 2), .),
            idx_vars = sample.int(max_num_vars, . * 2, replace = FALSE)
          )
        ) |>
          purrr::list_rbind(),
        .by = c(num_vars, idx_rsmp)
      ) |>
      tidyr::chop(idx_vars),
    tidyr::expand_grid(
      num_vars = round(
        seq(floor(max_num_vars / 2) + 1, max_num_vars - 2, length.out = 5)
      ),
      idx_rsmp = seq_len(100)
    ) |>
      dplyr::mutate(
        id_pairs = 1,
        idx_vars = purrr::map(
          num_vars,
          ~ sample.int(max_num_vars, ., replace = FALSE)
        )
      )
  ) |>
    tidyr::chop(c(idx_rsmp, idx_vars))
)

hypers_thresh_g <- dplyr::bind_rows(
  data.frame(
    thresh_method = "alpha",
    thresh_level = 0.01
  )
)
hypers_fc_data <- tidyr::expand_grid(
  modal = c("nbackfull", "rest", "run1rest"),
  parcel = c("Power264"),
  gsr = c("without")
)
hypers_cpm <- tidyr::expand_grid(hypers_thresh_g, hypers_fc_data)

g_invariance <- tarchetypes::tar_map(
  values = cfg_rsmp_vars,
  names = c(num_vars, id_pairs),
  tar_target(
    data_names,
    tibble(
      idx_rsmp = idx_rsmp, # use this to track samples
      tasks = map(idx_vars, ~ data_names_all[.])
    ),
    deployment = "main"
  ),
  tar_target(
    mdl_fitted,
    data_names |>
      mutate(
        mdl = map(
          tasks,
          ~ fit_g(indices_wider_clean, all_of(.))
        ),
        .keep = "unused"
      )
  ),
  tar_target(
    var_exp,
    mdl_fitted |>
      mutate(
        prop = map_dbl(mdl, calc_var_exp),
        .keep = "unused"
      )
  ),
  tar_target(
    scores_g,
    mdl_fitted |>
      mutate(
        scores = map(
          mdl,
          ~ predict_g_score(indices_wider_clean, .)
        ),
        .keep = "unused"
      )
  ),
  tar_fact_perm_cpm(
    result_cpm, scores_g, hypers_cpm,
    store_fc_data = store_modality_comparison
  ),
  tar_target(cpm_pred, extract_cpm_pred(result_cpm)),
  tar_target(
    brain_mask,
    extract_brain_mask(
      result_cpm,
      by = any_of(c(names(cfg_rsmp_vars), names(hypers_cpm)))
    )
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_behav, "indices_wider_clean"),
    read = qs::qread(!!.x)
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
  tar_fact_perm_cpm(
    result_cpm_main, behav_main, hypers_cpm,
    store_fc_data = store_modality_comparison
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
      filter(n() == 2, .by = c(num_vars, idx_rsmp, modal)) |>
      pivot_longer(
        c(pos, neg),
        names_to = "edge_type",
        values_to = "mask"
      ) |>
      mutate(mask_bin = map(mask, ~ . > 0.995), .keep = "unused") |>
      pivot_wider(
        id_cols = c(num_vars, idx_rsmp, modal, edge_type),
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
