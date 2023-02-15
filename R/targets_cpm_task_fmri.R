do_cpm <- function(fc_data, scores, thresh_method, thresh_level) {
  data <- fc_data |>
    tidytable::inner_join(scores, by = "sub_id") |>
    select(-sub_id) |>
    as.matrix()
  result <- cpm(
    data,
    kfolds = 10,
    thresh_method = thresh_method,
    thresh_level = thresh_level
  )
  with(
    result,
    tribble(
      ~edge_type, ~mask_prop, ~behav_pred, ~cor,
      "pos", mask_prop_pos, behav_pred_pos, cor_pos,
      "neg", mask_prop_neg, behav_pred_neg, cor_neg,
      "all", NULL, behav_pred_all, cor_all
    )
  )
}

config_fc_data <- tidyr::expand_grid(
  modal = c("emotion", "facename",
            "Nbackrun1", "Nbackrun2", "nback", "nbackfull",
            "4tasks", "rest", "4tasksrest"),
  parcel = c("nn268", "Power264"),
  gsr = c("with", "without")
)

hypers_behav <- data.frame(
  latent = c("g", "Speed", "WM", "Memory", "Flex")
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

targets_cpm <- tarchetypes::tar_map(
  config_fc_data,
  list(
    tarchetypes::tar_file_read(
      fc_data,
      sprintf(
        "data/neural/FC_modal-%s_parcel-%s_gsr-%s.arrow",
        modal, parcel, gsr
      ),
      read = arrow::read_feather(!!.x)
    ),
    tarchetypes::tar_map_rep(
      result_cpm,
      command = do_cpm(
        fc_data,
        scores_latent[, c("sub_id", latent)],
        thresh_method,
        thresh_level
      ),
      values = tidyr::expand_grid(
        hypers_behav,
        hypers_thresh
      ),
      batches = 10,
      reps = 10
    ),
    tar_target(
      cpmcors,
      result_cpm |>
        select(-mask_prop, -behav_pred) |>
        mutate(
          map_df(cor, broom::tidy),
          .keep = "unused",
          .before = starts_with("tar")
        )
    ),
    tarchetypes::tar_map_rep(
      result_cpm_sex,
      command = {
        scores_latent_match_sex <- scores_latent |>
          tidytable::semi_join(
            filter(subjs_info_clean, .data[["sex"]] == sex),
            by = "sub_id"
          )
        do_cpm(
          fc_data,
          scores_latent_match_sex[, c("sub_id", latent)],
          thresh_method,
          thresh_level
        )
      },
      values = tidyr::expand_grid(
        tibble::tibble(
          latent = "g",
          sex = c("M", "F")
        ),
        hypers_thresh
      ),
      batches = 10,
      reps = 10
    ),
    tar_target(
      cpmcors_sex,
      result_cpm_sex |>
        select(-mask_prop, -behav_pred) |>
        mutate(
          map_df(cor, broom::tidy),
          .keep = "unused",
          .before = starts_with("tar")
        )
    )
  )
)

config_fc_rest_sp <- tidyr::expand_grid(
  modal = "rest",
  modal_name = "rest2",
  parcel = c("nn268", "Power264"),
  gsr = c("with", "without")
)

targets_cpm_rest2 <- tarchetypes::tar_map(
  config_fc_rest_sp,
  names = -modal,
  list(
    tarchetypes::tar_file_read(
      fc_data,
      sprintf(
        "data/neural/FC_modal-%s_parcel-%s_gsr-%s.arrow",
        modal, parcel, gsr
      ),
      read = arrow::read_feather(!!.x) |>
        semi_join(fc_data_4tasks_nn268_with, by = "sub_id")
    ),
    tarchetypes::tar_map_rep(
      result_cpm,
      command = do_cpm(
        fc_data,
        scores_latent[, c("sub_id", latent)],
        thresh_method,
        thresh_level
      ),
      values = tidyr::expand_grid(
        hypers_behav,
        hypers_thresh
      ),
      batches = 10,
      reps = 10
    ),
    tar_target(
      cpmcors,
      result_cpm |>
        select(-mask_prop, -behav_pred) |>
        mutate(
          map_df(cor, broom::tidy),
          .keep = "unused",
          .before = starts_with("tar")
        )
    ),
    tarchetypes::tar_map_rep(
      result_cpm_sex,
      command = {
        scores_latent_match_sex <- scores_latent |>
          tidytable::semi_join(
            filter(subjs_info_clean, .data[["sex"]] == sex),
            by = "sub_id"
          )
        do_cpm(
          fc_data,
          scores_latent_match_sex[, c("sub_id", latent)],
          thresh_method,
          thresh_level
        )
      },
      values = tidyr::expand_grid(
        tibble::tibble(
          latent = "g",
          sex = c("M", "F")
        ),
        hypers_thresh
      ),
      batches = 10,
      reps = 10
    ),
    tar_target(
      cpmcors_sex,
      result_cpm_sex |>
        select(-mask_prop, -behav_pred) |>
        mutate(
          map_df(cor, broom::tidy),
          .keep = "unused",
          .before = starts_with("tar")
        )
    )
  )
)
