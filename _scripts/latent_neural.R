library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs",
  controller = crew::crew_controller_local(
    name = "local",
    workers = 4
  )
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

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

config_origin <- config_neural |>
  dplyr::filter(cond %in% c("nbackrun1", "rest")) |>
  config_file_tracking()

config_latent <- config_neural |>
  dplyr::filter(cond == "latent") |>
  config_file_tracking(name_suffix = "_latent") |>
  dplyr::select(-cond, -tar_neural_latent) |>
  dplyr::inner_join(
    dplyr::select(config_origin, -cond, -file),
    by = c("parcel", "filt", "gsr")
  ) |>
  tidyr::chop(tar_neural)

dir_gretna <- "data/neural-gretna"
config_gretna <- config_neural |>
  dplyr::filter(
    cond %in% c("nbackrun1", "rest"),
    filt == "bandpass"
  ) |>
  config_file_tracking(
    dir_neural = dir_gretna,
    tar_name_neural = "file_neural_gretna"
  )
config_latent_gretna <- config_neural |>
  dplyr::filter(cond == "latent") |>
  config_file_tracking(
    dir_neural = dir_gretna,
    name_suffix = "_latent"
  ) |>
  dplyr::select(-cond, -tar_neural_latent) |>
  dplyr::inner_join(
    dplyr::select(config_gretna, -cond, -file),
    by = c("parcel", "filt", "gsr")
  ) |>
  tidyr::chop(tar_neural)

list(
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_origin
  ),
  tarchetypes::tar_map(
    values = config_latent,
    names = c(parcel, filt, gsr),
    tar_target(
      tar_latent,
      output_latent_fc(tar_neural, file_latent),
      format = "file_fast"
    )
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_gretna
  ),
  tarchetypes::tar_map(
    values = config_latent_gretna,
    names = c(parcel, filt, gsr),
    tar_target(
      tar_latent_gretna,
      output_latent_fc(tar_neural, file_latent),
      format = "file_fast"
    )
  )
)
