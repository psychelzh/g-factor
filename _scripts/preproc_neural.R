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
partial_file_tracking <- purrr::partial(
  config_file_tracking,
  config = config_neural
)

config_neural_dir <- tibble::tribble(
  ~type, ~dir_neural, ~tar_name_neural, ~name_suffix,
  "legacy", "data/neural", "file", "",
  "gretna", "data/neural-gretna", "file_gretna", ""
)
hypers_covars <- tibble::tribble(
  ~reg_site, ~dir_name_suffix, ~covars,
  "yes", "-reg-covars", c("age", "sex", "site"),
  "no", "-reg-nosite", c("age", "sex")
)
config_neural_files_origin <- config_neural_dir |>
  dplyr::mutate(
    values = purrr::pmap(
      list(dir_neural, tar_name_neural, name_suffix),
      partial_file_tracking
    )
  ) |>
  tidyr::unnest(values) |>
  dplyr::select(type, all_of(names(config_neural)), file, tar_neural)
config_neural_files_regcov <- tidyr::expand_grid(
  config_neural_dir, hypers_covars
) |>
  dplyr::mutate(
    dir_neural = paste0(dir_neural, dir_name_suffix),
    tar_name_neural = paste0(
      tar_name_neural,
      gsub("-", "_", dir_name_suffix)
    ),
    values = purrr::pmap(
      list(dir_neural, tar_name_neural, name_suffix = "_regcov"),
      partial_file_tracking
    ),
    .keep = "unused"
  ) |>
  tidyr::unnest(values) |>
  dplyr::select(
    type, reg_site, covars, all_of(names(config_neural)),
    file_regcov, tar_neural_regcov
  )

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    subjs_covariates,
    fs::path(store_preproc_behav, "objects", "subjs_covariates"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_neural_files_origin
  ),
  tarchetypes::tar_map(
    values = config_neural_files_regcov |>
      dplyr::inner_join(
        config_neural_files_origin,
        by = c("type", "cond", "parcel", "filt", "gsr")
      ),
    names = c(type, reg_site, all_of(names(config_neural))),
    list(
      tar_target(
        file_reg,
        write_regressed_fc(
          tar_neural, file_regcov,
          subjs_info = subjs_covariates,
          covars = covars,
          cond = cond
        ),
        format = "file_fast"
      )
    )
  )
)
