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

config_files <- config |>
  dplyr::filter(
    cond %in% c("nbackrun1", "rest", "latent"),
    acq == "orig"
  ) |>
  config_file_tracking()

config_latent <- config_files |>
  dplyr::mutate(latent = ifelse(cond == "latent", "latent", "manifest")) |>
  tidyr::pivot_wider(
    id_cols = c(parcel, gsr),
    names_from = latent,
    values_from = c(name, file),
    values_fn = list
  ) |>
  tidyr::unnest(contains("latent"))

list(
  tarchetypes::tar_eval(
    tar_target(name, file, format = "file_fast"),
    values = config_files |> dplyr::filter(cond != "latent")
  ),
  tarchetypes::tar_map(
    values = config_latent,
    names = c(parcel, gsr),
    tar_target(
      tar_latent,
      output_latent_fc(name_manifest, file_latent),
      format = "file_fast"
    )
  )
)
