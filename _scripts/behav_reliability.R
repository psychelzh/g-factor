library(targets)

# targets options -----
tar_option_set(
  packages = "tidyverse",
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

# prepare static branches targets ----
indices <- readr::read_csv(
  "config/indices_selection.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(selected)
config_rel <- readr::read_csv(
  "config/reliability.csv",
  show_col_types = FALSE
) |>
  dplyr::left_join(indices, by = "task") |>
  dplyr::mutate(preproc = rlang::syms(preproc))
split_half <- tarchetypes::tar_map(
  dplyr::filter(config_rel, method == "odd-even"),
  names = disp_name,
  tar_target(
    file,
    fs::path("data", "behav", data_file)
  ),
  tar_target(
    data_splitted,
    arrow::read_feather(file) |>
      wrangle_data(task) |>
      split_data_odd_even()
  ),
  tar_target(
    indices,
    preproc_data_splitted(data_splitted, preproc, index)
  ),
  tar_target(
    reliability,
    calc_split_half(indices, index)
  )
)
stop_signal <- tarchetypes::tar_map(
  dplyr::filter(config_rel, task == "StopSignal"),
  names = disp_name,
  tar_target(
    file,
    fs::path("data", "behav", data_file)
  ),
  tar_target(
    data_splitted,
    arrow::read_feather(file) |>
      wrangle_data(task) |>
      mutate(class_block = block <= 2) |>
      group_split(class_block, .keep = FALSE)
  ),
  tar_target(
    indices,
    preproc_data_splitted(data_splitted, preproc, index)
  ),
  tar_target(
    reliability,
    calc_alpha(select(indices, !id_cols()))
  )
)
keep_track <- tarchetypes::tar_map(
  dplyr::filter(config_rel, task == "keepTrack"),
  names = disp_name,
  tar_target(
    file,
    fs::path("data", "behav", data_file)
  ),
  tar_target(
    indices,
    readr::read_csv(file, show_col_types = FALSE)
  ),
  tar_target(
    reliability,
    calc_alpha(select(indices, contains("score")))
  )
)

list(
  split_half,
  stop_signal,
  keep_track,
  tarchetypes::tar_combine(
    reliability,
    split_half$reliability,
    stop_signal$reliability,
    keep_track$reliability,
    command = bind_rows(!!!.x, .id = "id") |>
      separate_wider_delim(
        id, "_",
        names = c(NA, "disp_name")
      )
  )
)
