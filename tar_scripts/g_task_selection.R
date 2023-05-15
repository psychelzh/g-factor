library(targets)

# targets options ----
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
  tar_config_get("store", project = "project_preproc_behav"),
  "objects"
)
store_modality_comparison <- fs::path(
  tar_config_get("store", project = "project_modality_comparison"),
  "objects"
)
store_g_invariance <- fs::path(
  tar_config_get("store", project = "project_g_invariance"),
  "objects"
)

# prepare static branches targets ----
hypers_strip_n <- data.frame(n_rm = 1:(max_num_vars - 3))
hypers_thresh <- dplyr::bind_rows(
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
hypers_cpm <- tidyr::expand_grid(hypers_thresh, hypers_fc_data)
g_task_selection <- tarchetypes::tar_map(
  values = hypers_strip_n,
  list(
    tar_target(
      data_names,
      tibble(tasks = list(data_names_ordered[1:(max_num_vars - n_rm)]))
    ),
    tar_target(
      mdl_fitted,
      data_names |>
        mutate(
          mdl = map(
            tasks,
            ~ fit_g(indices_wider_clean, .)
          ),
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
    permute_cpm(
      scores_g,
      hypers = hypers_cpm,
      store_neural = store_modality_comparison
    )
  )
)

# targets pipeline ----
list(
  tar_target(
    file_mdl_full,
    fs::path(store_g_invariance, "mdl_fitted_full"),
    format = "file"
  ),
  tar_target(
    data_names_ordered, {
      loadings_mdl <- loadings(qs::qread(file_mdl_full))
      rownames(loadings_mdl)[order(loadings_mdl, decreasing = TRUE)]
    }
  ),
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_behav, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  g_task_selection,
  combine_targets(cpm_pred, g_task_selection, names(hypers_strip_n)),
  tar_target(
    scores_single,
    tibble(
      task = data_names_ordered
    ) |>
      mutate(
        scores = map(
          task,
          ~ indices_wider_clean |>
            select(all_of(c("sub_id", .)))
        )
      )
  ),
  permute_cpm(
    scores_single,
    hypers = hypers_cpm,
    name_suffix = "_single",
    store_neural = store_modality_comparison
  )
)
