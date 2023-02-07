library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan", "conflicted"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs"
)
tar_source()
future::plan(future.callr::callr)
store_behav <- fs::path(
  tar_config_get("store", project = "project_behav"),
  "objects"
)
store_fmri <- fs::path(
  tar_config_get("store", project = "project_task_fmri"),
  "objects"
)
list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_behav, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    fc_data_rest_nn268_without,
    fs::path(store_fmri, "fc_data_rest_nn268_without"),
    read = qs::qread(!!.x)
  ),
  # used for cpm batching (tar_rep cannot used with pattern)
  tar_target(index_batch_cpm, seq_len(10)),
  tar_target(index_rep_cpm, seq_len(10)),
  g_scores_pairs
)
