library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
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

write_regressed_fc <- function(origin, dest, ...) {
  arrow::read_feather(origin) |>
    regress_covariates(...) |>
    write_feather_safely(dest)
}

config_regress <- config_file_tracking(config) |>
  tidyr::pivot_wider(
    names_from = acq,
    values_from = c(file, name)
  )

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(name_orig, file_orig, format = "file_fast"),
    values = config_regress
  ),
  tarchetypes::tar_eval(
    tar_target(
      name_reg,
      write_regressed_fc(
        name_orig, file_reg,
        subjs_info = subjs_covariates,
        covars = c("age", "sex"),
        cond = cond
      ),
      format = "file_fast"
    ),
    values = config_regress
  )
)
