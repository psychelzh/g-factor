library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "abridge",
  format = "qs",
  controller = crew::crew_controller_local(workers = 8)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

config_origin <- config_file_tracking(config_neural)
config_reg_covars <- config_file_tracking(
  config_neural,
  dir_neural = "data/reg_covars",
  tar_name_neural = "file_neural_reg_covars",
  name_suffix = "_reg_covars"
)
config_reg_no_site <- config_file_tracking(
  config_neural,
  dir_neural = "data/reg_covars2",
  tar_name_neural = "file_neural_reg_covars2",
  name_suffix = "_reg_covars2"
)

config_origin_raw <- config_file_tracking(
  config_neural |> dplyr::filter(cond %in% c("nbackrun1", "rest")),
  dir_neural = "data/fc_fisherz_raw",
  tar_name_neural = "file_neural_raw",
  name_suffix = "_raw"
)
config_reg_covars_raw <- config_file_tracking(
  config_neural |> dplyr::filter(cond %in% c("nbackrun1", "rest")),
  dir_neural = "data/reg_covars_raw",
  tar_name_neural = "file_neural_reg_covars_raw",
  name_suffix = "_reg_covars_raw"
)

# prepare static branches targets ----
data_subjs <- tarchetypes::tar_map(
  values = config_origin |>
    dplyr::distinct(cond, .keep_all = TRUE),
  names = cond,
  tar_target(
    subjs_pattern,
    arrow::read_feather(tar_neural)$sub_id
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "objects", "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_origin
  ),
  data_subjs,
  tarchetypes::tar_combine(
    subjs_neural,
    data_subjs$subjs_pattern,
    command = list(!!!.x) |>
      reduce(intersect)
  ),
  tar_target(
    subjs_neural_file,
    {
      write(subjs_neural, file_subjs_neural, ncolumns = 1)
      file_subjs_neural
    },
    format = "file"
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
      write(subjs_combined, file_subjs_combined, ncolumns = 1)
      file_subjs_combined
    },
    format = "file"
  ),
  tarchetypes::tar_eval(
    tar_target(
      tar_neural_reg_covars,
      write_regressed_fc(
        tar_neural, file_reg_covars,
        subjs_info = subjs_covariates,
        cond = cond
      ),
      format = "file_fast"
    ),
    values = dplyr::inner_join(
      config_origin,
      config_reg_covars,
      by = names(config_neural)
    )
  ),
  tarchetypes::tar_eval(
    tar_target(
      tar_neural_reg_covars2,
      write_regressed_fc(
        tar_neural, file_reg_covars2,
        subjs_info = subjs_covariates,
        covars = c("age", "sex"),
        cond = cond
      ),
      format = "file_fast"
    ),
    values = dplyr::inner_join(
      config_origin,
      config_reg_no_site,
      by = names(config_neural)
    )
  )
)
