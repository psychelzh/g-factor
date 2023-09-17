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

write_feather_safely <- function(data, file) {
  if (!fs::dir_exists(fs::path_dir(file))) {
    fs::dir_create(fs::path_dir(file))
  }
  arrow::write_feather(data, file, compression = FALSE)
  file
}

write_regressed_fc <- function(origin, dest, ...) {
  arrow::read_feather(origin) |>
    regress_covariates(...) |>
    write_feather_safely(dest)
}

output_latent_fc <- function(files_in, file_out) {
  data <- files_in |>
    map(arrow::read_feather) |>
    bind_rows(.id = "src")
  reframe(
    data,
    across(
      starts_with("Var"),
      ~ cbind(.[src == 1], .[src == 2]) |>
        princomp() |>
        pluck("scores") |>
        _[, 1]
    )
  ) |>
    add_column(
      sub_id = data$sub_id[data$src == 1],
      .before = 1L
    ) |>
    write_feather_safely(file_out)
}

# prepare values used in targets pipeline
config_files <- config_file_tracking(config)
config_latent <- config_files |>
  dplyr::filter(
    cond %in% c("nbackrun1", "rest", "resteq"),
    acq == "orig"
  ) |>
  tidyr::pivot_wider(
    id_cols = c(parcel, gsr, acq),
    names_from = cond,
    values_from = name
  ) |>
  dplyr::left_join(
    config_files |>
      dplyr::filter(
        grepl("latent", cond),
        acq == "orig"
      ),
    by = c("parcel", "gsr", "acq")
  )
config_regress <- config_files |>
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
    tar_target(name, file, format = "file_fast"),
    values = config_files |>
      dplyr::filter(!grepl("latent", cond), acq != "reg")
  ),
  tarchetypes::tar_eval(
    tar_target(
      name,
      output_latent_fc(list(nbackrun1, rest), file),
      format = "file_fast"
    ),
    values = config_latent |>
      dplyr::filter(!grepl("eq", cond))
  ),
  tarchetypes::tar_eval(
    tar_target(
      name,
      output_latent_fc(list(nbackrun1, resteq), file),
      format = "file_fast"
    ),
    values = config_latent |>
      dplyr::filter(grepl("eq", cond))
  ),
  tarchetypes::tar_eval(
    tar_target(
      name_reg,
      write_regressed_fc(
        name_orig, file_reg,
        subjs_info = subjs_covariates,
        covars = c("age", "sex"),
        extracov = extracov
      ),
      format = "file_fast"
    ),
    values = config_regress
  )
)
