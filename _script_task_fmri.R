library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan"),
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
list(
  tarchetypes::tar_file_read(
    scores_latent,
    fs::path(store_behav, "scores_latent"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_info_clean,
    fs::path(store_behav, "subjs_info_clean"),
    read = qs::qread(!!.x)
  ),
  targets_cpm,
  tarchetypes::tar_combine(
    cpmcors,
    targets_cpm$cpmcors,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined("cpmcors", names(config_fc_data))
  ),
  tarchetypes::tar_combine(
    cpmcors_sex,
    targets_cpm$cpmcors_sex,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined("cpmcors_sex", names(config_fc_data))
  ),
  targets_cpm_rest2,
  tarchetypes::tar_combine(
    cpmcors_rest2,
    targets_cpm_rest2$cpmcors,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined("cpmcors", names(config_fc_data))
  ),
  tarchetypes::tar_combine(
    cpmcors_sex_rest2,
    targets_cpm_rest2$cpmcors_sex,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined("cpmcors_sex", names(config_fc_data))
  )
)
