library(targets)

# targets options -----
tar_option_set(
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 8)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)
config_files_all_cond <- config |>
  dplyr::filter(acq == "orig") |>
  config_file_tracking() |>
  dplyr::distinct(cond, .keep_all = TRUE)

# prepare static branches targets ----
data_subjs <- tarchetypes::tar_map(
  values = config_files_all_cond,
  names = cond,
  tar_target(
    subjs_pattern,
    arrow::read_feather(name)$sub_id
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc_behav, "objects", "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(name, file, format = "file_fast"),
    values = config_files_all_cond
  ),
  data_subjs,
  tarchetypes::tar_combine(
    subjs_neural,
    data_subjs$subjs_pattern,
    command = sort(Reduce(intersect, list(!!!.x)))
  ),
  tar_target(
    subjs_neural_file,
    {
      write(subjs_neural, file_subjs_neural, ncolumns = 1)
      file_subjs_neural
    },
    format = "file"
  ),
  tar_target(
    subjs_combined,
    sort(intersect(subjs_neural, indices_wider_clean$sub_id))
  ),
  tar_target(
    subjs_combined_file,
    {
      write(subjs_combined, file_subjs_combined, ncolumns = 1)
      file_subjs_combined
    },
    format = "file"
  )
)
