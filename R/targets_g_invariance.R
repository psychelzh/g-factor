fit_g <- function(data, vars) {
  data_sel <- data |>
    select({{ vars }}) |>
    rename_with(make.names)
  efa(data_sel, std.ov = TRUE, missing = "ml")
}

calc_var_exp <- function(fit) {
  mean(loadings(fit) ^ 2)
}

predict_g_score <- function(data, mdl, id_cols = 1) {
  bind_cols(
    data[, id_cols],
    g = lavPredict(mdl)[, 1]
  )
}

extract_cpm_pred <- function(result_cpm, col_cpm = cpm) {
  extract_cors <- function(cpm, edge_types = c("pos", "neg", "all")) {
    map_dbl(edge_types, ~ cpm[[str_c("cor_", .)]]$estimate) |>
      as_tibble_row(.name_repair = ~ edge_types)
  }
  result_cpm |>
    mutate(
      map({{ col_cpm }}, extract_cors) |>
        list_rbind(),
      .keep = "unused"
    )
}

extract_brain_mask <- function(result_cpm, col_cpm = cpm) {
  aggregate_masks <- function(cpm, edge_types = c("pos", "neg")) {
    map(
      edge_types,
      ~ list(rowMeans(do.call(cbind, map(cpm, str_c("mask_prop_", .)))))
    ) |>
      set_names(edge_types) |>
      as_tibble_row()
  }
  result_cpm |>
    summarise(
      aggregate_masks({{ col_cpm }}),
      .by = c(any_of("idx_rsmp"), starts_with("thresh"))
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
  data.frame(
    thresh_method = "alpha",
    thresh_level = 0.01
  )
)

g_invariance <- tarchetypes::tar_map(
  values = cfg_rsmp_vars,
  names = c(num_vars, id_pairs),
  tar_target(
    data_names,
    tibble(
      idx_rsmp = idx_rsmp, # use this to track samples
      tasks = map(idx_vars, ~ data_names_all[.])
    ),
    deployment = "main"
  ),
  tar_target(
    mdl_fitted,
    data_names |>
      mutate(
        mdl = map(
          tasks,
          ~ fit_g(indices_wider_clean, all_of(.))
        ),
        .keep = "unused"
      )
  ),
  tar_target(
    var_exp,
    mdl_fitted |>
      mutate(
        prop = map_dbl(mdl, calc_var_exp),
        .keep = "unused"
      )
  ),
  tar_target(
    scores_g,
    mdl_fitted |>
      mutate(
        scores = map(
          mdl,
          ~ predict_g_score(indices_wider_clean, .)
        ),
        .keep = "unused"
      )
  ),
  tarchetypes::tar_map_rep(
    result_cpm,
    scores_g |>
      mutate(
        cpm = map(
          scores,
          ~ do_cpm2(
            fc_data_matched,
            .,
            thresh_method = thresh_method,
            thresh_level = thresh_level
          )
        ),
        .keep = "unused"
      ),
    values = hypers_thresh_g,
    batches = 4,
    reps = 5
  ),
  tar_target(cpm_pred, extract_cpm_pred(result_cpm)),
  tar_target(brain_mask, extract_brain_mask(result_cpm))
)
