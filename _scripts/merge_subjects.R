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
config_origin <- config_file_tracking(config_neural) |>
  dplyr::distinct(cond, .keep_all = TRUE)

# prepare static branches targets ----
data_subjs <- tarchetypes::tar_map(
  values = config_origin,
  names = cond,
  tar_target(
    subjs_pattern,
    arrow::read_feather(tar_neural)$sub_id
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "objects", "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file_fast"),
    values = config_origin
  ),
  data_subjs,
  tarchetypes::tar_combine(
    subjs_neural,
    data_subjs$subjs_pattern,
    command = Reduce(intersect, list(!!!.x))
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
    intersect(
      subjs_neural,
      with(behav_main, scores[idx == "g_full"])[[1]]$sub_id
    )
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
