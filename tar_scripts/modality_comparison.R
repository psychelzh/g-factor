library(targets)

# targets options -----
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

# targets globals ----
tar_source()
future::plan(future.callr::callr)
store_preproc_behav <- fs::path(
  tar_config_get("store", project = "project_preproc_behav"),
  "objects"
)
store_g_invariance <- fs::path(
  tar_config_get("store", project = "project_g_invariance"),
  "objects"
)

# prepare static branches targets ----
config_fc_data <- tidyr::expand_grid(
  modal = c("nbackfull", "rest", "run1rest"),
  parcel = c("nn268", "Power264"),
  gsr = c("with", "without")
)
hypers_thresh <- dplyr::bind_rows(
  tibble::tibble(
    thresh_method = "alpha",
    thresh_level = 0.01
  ),
  tibble::tibble(
    thresh_method = "sparsity",
    thresh_level = 0.01
  )
)
hypers_sex <- data.frame(sex = c("M", "F"))
modality_comparison <- tarchetypes::tar_map(
  config_fc_data,
  list(
    tarchetypes::tar_file_read(
      fc_data_origin,
      sprintf(
        "data/neural/FC_modal-%s_parcel-%s_gsr-%s.arrow",
        modal, parcel, gsr
      ),
      read = arrow::read_feather(!!.x)
    ),
    tar_target(fc_data, filter(fc_data_origin, sub_id %in% subjs_combined)),
    permute_cpm(result_cpm, behav_main, hypers_thresh, fc_data),
    tar_target(cpm_pred, extract_cpm_pred(result_cpm)),
    tar_target(
      brain_mask,
      extract_brain_mask(
        result_cpm,
        by = starts_with(c("idx", "thresh"))
      )
    ),
    permute_cpm(
      result_cpm_sex,
      behav_main,
      tidyr::expand_grid(hypers_thresh, hypers_sex),
      fc_data,
      split_hyper = "sex"
    ),
    tar_target(cpm_pred_sex, extract_cpm_pred(result_cpm_sex)),
    tar_target(
      brain_mask_sex,
      extract_brain_mask(
        result_cpm_sex,
        by = starts_with(c("idx", "thresh"))
      )
    )
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_g_invariance, "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_info_clean,
    fs::path(store_preproc_behav, "subjs_info_clean"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    subjs_pattern,
    list(
      combined = fc_data_origin_run1rest_nn268_with$sub_id,
      task = fc_data_origin_nbackfull_nn268_with$sub_id,
      rest = fc_data_origin_rest_nn268_with$sub_id,
      behav = behav_main |>
        filter(idx == "g_full") |>
        pluck("scores", 1, "sub_id")
    )
  ),
  tar_target(subjs_combined, reduce(subjs_pattern, intersect)),
  modality_comparison,
  combine_targets(
    cpm_pred,
    modality_comparison,
    names(config_fc_data)
  ),
  combine_targets(
    cpm_pred_sex,
    modality_comparison,
    names(config_fc_data)
  )
)
