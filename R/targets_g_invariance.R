fit_g <- function(data, idx_vars) {
  vars <- names(data)[idx_vars + 1]
  mdl <- paste(
    "g =~",
    paste0("`", vars, "`", collapse = " + ")
  )
  cfa(mdl, data, std.ov = TRUE, missing = "ml")
}

max_num_vars <- 20 # we have 20 indicators in total (can be more)
cfg_rsmp_vars <- withr::with_seed(
  1,
  dplyr::bind_rows(
    tidyr::expand_grid(
      num_vars = 4:floor(max_num_vars / 2),
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
    fit_g(indices_wider_clean, idx_vars)
  ),
  tar_target(
    scores_g,
    bind_cols(
      indices_wider_clean[, 1],
      g = lavPredict(mdl_fitted)[, "g"]
    )
  ),
  tarchetypes::tar_map_rep(
    result_cpm,
    command = do_cpm2(
      fc_data_rest_nn268_without,
      scores_g,
      thresh_method,
      thresh_level
    ),
    values = hypers_thresh,
    batches = 10,
    reps = 10
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
