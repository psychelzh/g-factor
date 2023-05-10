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
    tarchetypes::tar_map_rep(
      result_cpm,
      command = behav_main |>
        mutate(
          cpm = map(
            scores,
            ~ do_cpm2(
              fc_data,
              .,
              thresh_method = thresh_method,
              thresh_level = thresh_level
            )
          ),
          .keep = "unused"
        ),
      values = hypers_thresh,
      batches = 4,
      reps = 5
    ),
    tar_target(cpm_pred, extract_cpm_pred(result_cpm)),
    tar_target(brain_mask, extract_brain_mask(result_cpm))
  )
)
