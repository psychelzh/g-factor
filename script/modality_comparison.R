library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan", "collapse"),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  error = "null",
  format = "qs",
  controller = crew::crew_controller_local(workers = 10)
)
tar_source()
future::plan(future.callr::callr)

store_behav <- fs::path(
  tar_config_get("store", project = "project_behav"),
  "objects"
)
store_g_invariance <- fs::path(
  tar_config_get("store", project = "project_g_invariance"),
  "objects"
)

list(
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_g_invariance, "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_info_clean,
    fs::path(store_behav, "subjs_info_clean"),
    read = qs::qread(!!.x)
  ),
  modality_comparison,
  combine_targets(
    cpm_pred,
    modality_comparison,
    names(config_fc_data)
  ),
  tar_target(
    subjs_pattern,
    list(
      append = fc_data_origin_run1rest_nn268_with$sub_id,
      task = fc_data_origin_nbackfull_nn268_with$sub_id,
      rest = fc_data_origin_rest_nn268_with$sub_id,
      behav = behav_main |>
        filter(idx == "g_full") |>
        pluck("scores", 1, "sub_id")
    )
  ),
  tar_target(subjs_combined, subjs_pattern |> reduce(intersect))
)
