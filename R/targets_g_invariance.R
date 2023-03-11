fit_g <- function(data, vars) {
  data_sel <- data |>
    select({{ vars }}) |>
    rename_with(make.names)
  efa(data_sel, std.ov = TRUE, missing = "ml")
}

predict_g_score <- function(data, mdl, id_cols = 1) {
  bind_cols(
    data[, id_cols],
    g = lavPredict(mdl)[, 1]
  )
}

aggregate_masks <- function(cpm, from) {
  aggr <- function(mask_prop) {
    rowMeans(do.call(cbind, mask_prop))
  }
  list(mns = aggr(map(cpm, from)))
}

extract_brain_mask <- function(result_cpm) {
  result_cpm |>
    summarise(
      mask_pos = aggregate_masks(cpm, "mask_prop_pos"),
      mask_neg = aggregate_masks(cpm, "mask_prop_neg"),
      .by = c(id_behav, starts_with("thresh"))
    )
}

max_num_vars <- 20 # we have 20 indicators in total (can be more)
cfg_rsmp_vars <- withr::with_seed(
  1,
  dplyr::bind_rows(
    tidyr::expand_grid(
      num_vars = 3:floor(max_num_vars / 2),
      idx_rsmp = seq_len(100)
    ) |>
      dplyr::reframe(
        purrr::map(
          num_vars,
          ~ data.frame(
            id_pairs = rep(c(1, 2), .),
            idx_vars = sample.int(max_num_vars, . * 2, replace = FALSE)
          )
        ) |>
          purrr::list_rbind(),
        .by = c(num_vars, idx_rsmp)
      ) |>
      tidyr::chop(idx_vars),
    tidyr::expand_grid(
      num_vars = (floor(max_num_vars / 2) + 1):(max_num_vars - 2),
      idx_rsmp = seq_len(100)
    ) |>
      dplyr::mutate(
        id_pairs = 1,
        idx_vars = purrr::map(
          num_vars,
          ~ sample.int(max_num_vars, ., replace = FALSE)
        )
      )
  ) |>
    tidyr::chop(c(idx_rsmp, idx_vars))
)

hypers_thresh_g <- dplyr::bind_rows(
  tidyr::expand_grid(
    data.frame(
      thresh_method = "alpha",
      thresh_level = 0.01
    ),
    tidyr::expand_grid(
      idx_batch = seq_len(2),
      idx_rep = seq_len(10)
    )
  ) |>
    tidyr::chop(idx_rep)
)
targets_cpm_rsmpl <- tar_map_cpm2(
  values = hypers_thresh_g,
  neural = fc_data_rest_nn268_without,
  behav = scores_g
)

g_invariance <- tarchetypes::tar_map(
  values = cfg_rsmp_vars,
  names = c(num_vars, id_pairs),
  tar_target(
    data_names,
    map(idx_vars, ~ data_names_all[.]),
    deployment = "main"
  ),
  tar_target(
    mdl_fitted,
    map(data_names, ~ fit_g(indices_wider_clean, all_of(.)))
  ),
  tar_target(
    scores_g,
    map(mdl_fitted, ~ predict_g_score(indices_wider_clean, .))
  ),
  targets_cpm_rsmpl, # this step will produces data.frames
  tarchetypes::tar_combine(
    result_cpm,
    targets_cpm_rsmpl$result_cpm,
    use_names = FALSE
  ),
  tar_target(
    brain_mask,
    extract_brain_mask(result_cpm)
  )
)
