# for indices pre-processing
task_preproc <- readr::read_csv("config/task_preproc.csv", show_col_types = FALSE) |>
  tidyr::drop_na() |>
  dplyr::mutate(preproc = rlang::syms(paste0("preproc_", preproc)))
preproc_behav <- tarchetypes::tar_map(
  task_preproc,
  names = task,
  list(
    tar_target(
      indices,
      preproc(data_clean) |>
        pivot_longer(
          -any_of(id_cols()),
          names_to = "index",
          values_to = "score"
        ) |>
        select(-task_datetime)
    ),
    tarchetypes::tar_file_read(
      data,
      fs::path("data/behav", sprintf("%s.arrow", task)),
      read = arrow::read_feather(!!.x)
    ),
    tar_target(data_clean, screen_data(data))
  )
)
