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

extract_brain_mask <- function(result_cpm, .by) {
  result_cpm |>
    filter(!map_lgl(mask_prop, is.null)) |>
    summarise(
      mask = do.call(cbind, mask_prop) |>
        rowMeans() |>
        list(),
      .by = {{ .by }}
    )
}

hypers_thresh_g <- dplyr::bind_rows(
  tibble::tibble(
    thresh_method = "alpha",
    thresh_level = 0.01
  )
)

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
  tarchetypes::tar_map_rep(
    result_cpm,
    command = map(
      scores_g,
      ~ do_cpm2(
        fc_data_rest_nn268_without,
        .,
        thresh_method,
        thresh_level
      )
    ),
    values = hypers_thresh_g,
    batches = 2,
    reps = 10
  ),
  tar_target(
    brain_mask,
    map(
      result_cpm,
      ~ extract_brain_mask(
        .,
        .by = c(edge_type, starts_with("thresh"))
      )
    ),
    pattern = map(result_cpm)
  )
)
