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
store_preproc_behav <- fs::path(
  tar_config_get("store", project = "project_preproc_behav"),
  "objects"
)

# prepare static branches targets ----
config_neural <- config_neural |>
  dplyr::filter(parcel == "Power264", gsr == "without")
hypers_cpm <- hypers_cpm |>
  dplyr::filter(thresh_method == "alpha")

hypers_behav <- data.frame(n_rm = 1:(max_num_vars - 3))
g_task_selection <- tarchetypes::tar_map(
  values = hypers_behav,
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
    permute_cpm2(
      scores_g,
      config_neural,
      hypers_cpm,
      subjs_subset = subjs_combined,
      include_file_targets = FALSE
    )
  )
)

# targets pipeline ----
list(
  tar_target(
    file_mdl_full,
    fs::path(store_preproc_behav, "mdl_fitted_full"),
    format = "file"
  ),
  tar_target(
    data_names_ordered, {
      loadings_mdl <- loadings(qs::qread(file_mdl_full))
      rownames(loadings_mdl)[order(loadings_mdl, decreasing = TRUE)]
    }
  ),
  tarchetypes::tar_file_read(
    subjs_combined,
    file_subjs_combined,
    read = as.numeric(read_lines(!!.x))
  ),
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc_behav, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    scores_single,
    tibble(task = data_names_ordered) |>
      mutate(
        scores = map(
          task,
          ~ indices_wider_clean |>
            select(all_of(c("sub_id", .)))
        )
      )
  ),
  permute_cpm2(
    scores_single,
    config_neural,
    hypers_cpm,
    subjs_subset = subjs_combined,
    name_suffix = "_single"
  ),
  g_task_selection,
  combine_targets(cpm_pred, g_task_selection, names(hypers_behav))
)
