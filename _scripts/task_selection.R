library(targets)

# targets options ----
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

# prepare static branches targets ----
config <- config |>
  dplyr::filter(
    parcel == "Power264",
    gsr == "with",
    cond == "nbackrun1",
    acq == "reg"
  )
hypers_cpm <- hypers_cpm |>
  dplyr::filter(
    thresh_method == "alpha",
    thresh_level == 0.01
  )

hypers_behav <- data.frame(n_rm = 1:(max_num_vars - 3))
task_selection <- tarchetypes::tar_map(
  values = hypers_behav,
  list(
    tar_target(
      data_names,
      tibble(tasks = list(data_names_ordered[1:(max_num_vars - n_rm)]))
    ),
    include_g_fitting(
      indices_wider_clean,
      data_names,
      include_comp_rel = FALSE
    ),
    prepare_permute_cpm2(
      config, hypers_cpm, scores_g,
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
    mdl_fitted_full,
    fs::path(store_preproc_behav, "objects", "mdl_fitted_full"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    data_names_ordered, {
      loadings_mdl <- loadings(mdl_fitted_full)
      rownames(loadings_mdl)[order(loadings_mdl, decreasing = TRUE)]
    }
  ),
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
    indices_wider_clean,
    fs::path(store_preproc_behav, "objects", "indices_wider_clean"),
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
  prepare_permute_cpm2(
    config,
    hypers_cpm,
    scores_single,
    subjs_subset = subjs_combined,
    name_suffix = "_single",
    subjs_info = subjs_covariates,
    covars = c("age", "sex")
  ),
  task_selection,
  lapply(
    rlang::exprs(
      scores_g,
      cpm_pred
    ),
    combine_targets,
    targets = task_selection,
    cols_targets = names(hypers_behav)
  )
)
