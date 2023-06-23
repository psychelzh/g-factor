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
config_origin <- config_file_tracking(config_neural)
config_reg_covars <- config_file_tracking(
  config_neural,
  dir_neural = "data/reg_covars",
  tar_name_neural = "file_neural_reg_covars",
  name_suffix = "_reg_covars"
)
config_reg_site <- config_file_tracking(
  config_neural,
  dir_neural = "data/reg_site",
  tar_name_neural = "file_neural_reg_site",
  name_suffix = "_reg_site"
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
      tar_neural_reg_covars, {
        arrow::read_feather(tar_neural) |>
          regress_covariates(subjs_covariates, cond = cond) |>
          arrow::write_feather(file_reg_covars, compression = FALSE)
        file_reg_covars
      },
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
      tar_neural_reg_site, {
        arrow::read_feather(tar_neural) |>
          regress_covariates(
            subjs_covariates,
            covars = "site",
            cond = cond
          ) |>
          arrow::write_feather(file_reg_site, compression = FALSE)
        file_reg_site
      },
      format = "file"
    ),
    values = dplyr::inner_join(
      config_origin,
      config_reg_site,
      by = names(config_neural)
    )
  )
)
