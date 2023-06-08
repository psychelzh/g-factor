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
  controller = crew::crew_controller_local(workers = 10)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

# prepare static branches targets ----
hypers_sex <- data.frame(sex = c("M", "F"))
data_subjs <- tarchetypes::tar_map(
  values = config_neural |>
    config_file_tracking() |>
    dplyr::distinct(cond, .keep_all = TRUE),
  names = modal,
  tar_target(
    subjs_pattern,
    arrow::read_feather(tar_neural)$sub_id
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "objects", "behav_main"),
    read = qs::qread(!!.x)
  ),
  data_subjs,
  tarchetypes::tar_combine(
    subjs_neural,
    data_subjs$subjs_pattern,
    command = list(!!!.x) |>
      reduce(intersect)
  ),
  tar_target(
    subjs_combined,
    intersect(
      subjs_neural,
      behav_main |>
        filter(idx == "g_full") |>
        pluck("scores", 1, "sub_id")
    )
  ),
  tar_target(
    subjs_combined_file,
    {
      write_lines(subjs_combined, file_subjs_combined)
      file_subjs_combined
    },
    format = "file"
  ),
  prepare_permute_cpm2(
    config_neural, hypers_cpm, behav_main,
    subjs_subset = subjs_combined
  ),
  prepare_permute_cpm2(
    config_neural, hypers_cpm, behav_main,
    subjs_subset = subjs_combined,
    name_suffix = "_reg_covars",
    subjs_info = subjs_covariates,
    reg_covars = TRUE
  )
)
