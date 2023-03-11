fit_g <- function(data, idx_vars) {
  data_sel <- data[, idx_vars + 1] |>
    rename_with(make.names)
  efa(data_sel, std.ov = TRUE, missing = "ml")
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
  )
)

g_invariance <- tarchetypes::tar_map(
  values = cfg_rsmp_vars,
  names = -idx_vars,
  tar_target(
    data_names,
    data_names_all[idx_vars],
    deployment = "main"
  ),
  tar_target(
    mdl_fitted,
    fit_g(indices_wider_clean, idx_vars),
    deployment = "main"
  ),
  tar_target(
    scores_g,
    bind_cols(
      indices_wider_clean[, 1],
      g = lavPredict(mdl_fitted)[, 1]
    ),
    deployment = "main"
  ),
  tarchetypes::tar_map_rep(
    result_cpm,
    command = do_cpm2(
      fc_data_rest_nn268_without,
      scores_g,
      thresh_method,
      thresh_level
    ),
    values = hypers_thresh_g,
    batches = 2,
    reps = 50
  ),
  tar_target(
    brain_mask,
    result_cpm |>
      filter(!map_lgl(mask_prop, is.null)) |>
      summarise(
        mask = do.call(cbind, mask_prop) |>
          rowMeans() |>
          list(),
        .by = c(edge_type, starts_with("tar"), starts_with("thresh"))
      )
  )
)
