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

list(
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file"),
    values = config_neural
  ),
  tarchetypes::tar_eval(
    tar_target(
      tar_reg_covars, {
        arrow::read_feather(tar_neural) |>
          regress_covariates(subjs_covariates) |>
          arrow::write_feather(file_reg_covars)
        file_reg_covars
      },
      format = "file"
    ),
    values = config_neural
  )
)
