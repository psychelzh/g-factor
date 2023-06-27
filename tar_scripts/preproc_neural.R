library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs",
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)
write_regressed_fc <- function(origin, dest, ...) {
  if (!fs::dir_exists(fs::path_dir(dest))) {
    fs::dir_create(fs::path_dir(dest))
  }
  arrow::read_feather(origin) |>
    regress_covariates(...) |>
    arrow::write_feather(dest, compression = FALSE)
  dest
}


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
  config_neural |> dplyr::filter(cond != "run1rest"),
  dir_neural = "data/fc_fisherz_raw",
  tar_name_neural = "file_neural_raw",
  name_suffix = "_raw"
)
config_reg_covars_raw <- config_file_tracking(
  config_neural |> dplyr::filter(cond != "run1rest"),
  dir_neural = "data/reg_covars_raw",
  tar_name_neural = "file_neural_reg_covars_raw",
  name_suffix = "_reg_covars_raw"
)

list(
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_origin
  ),
  tarchetypes::tar_eval(
    tar_target(
      tar_neural_reg_covars,
      write_regressed_fc(
        tar_neural, file_reg_covars,
        subjs_info = subjs_covariates,
        cond = cond
      ),
      format = "file"
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
      format = "file"
    ),
    values = dplyr::inner_join(
      config_origin,
      config_reg_no_site,
      by = names(config_neural)
    )
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural_raw, file_raw, format = "file_fast"),
    values = config_origin_raw
  ),
  tarchetypes::tar_eval(
    tar_target(
      tar_neural_reg_covars_raw,
      write_regressed_fc(
        tar_neural_raw, file_reg_covars_raw,
        subjs_info = subjs_covariates,
        covars = c("age", "sex"),
        cond = cond
      ),
      format = "file"
    ),
    values = dplyr::inner_join(
      config_origin_raw,
      config_reg_covars_raw,
      by = names(config_neural)
    )
  )
)
