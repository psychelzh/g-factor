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

prepare_data_names <- function(fit, num_task, direction) {
  sort(
    loadings(fit)[, 1],
    decreasing = direction == "desc"
  )[seq_len(num_task)] |>
    names()
}

# prepare static branches targets ----
schema_tasksel <- tidyr::expand_grid(
  num_task = 3:20,
  direction = c("desc", "asc")
)
gca_tasksel <- tarchetypes::tar_map(
  values = schema_tasksel,
  list(
    tar_target(
      data_names,
      tibble(
        tasks = list(
          prepare_data_names(
            fit_spearman,
            num_task,
            direction
          )
        )
      )
    ),
    include_g_fitting(
      indices_wider_clean,
      data_names,
      include_comp_rel = FALSE
    ),
    tar_target(
      r_with_gca_s,
      tibble(
        r = cor(
          scores_g$scores[[1]]$g,
          scores_spearman$g
        )
      )
    )
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    fit_spearman,
    fs::path(store_preproc_behav, "objects", "fit_spearman"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc_behav, "objects", "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    scores_spearman,
    fs::path(store_preproc_behav, "objects", "scores_spearman"),
    read = qs::qread(!!.x)
  ),
  gca_tasksel,
  combine_targets(
    r_with_gca_s,
    gca_tasksel,
    names(schema_tasksel)
  )
)
