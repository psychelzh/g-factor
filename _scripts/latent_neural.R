library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs",
  controller = crew::crew_controller_local(workers = 8)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

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

list(
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_origin
  ),
  tarchetypes::tar_map(
    values = config_latent,
    names = c(parcel, filt, gsr),
    tar_target_raw(
      "tar_neural_latent", rlang::expr({
        data <- tar_neural |>
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
          write_feather_safely(file_latent)
      }),
      format = "file"
    )
  )
)
