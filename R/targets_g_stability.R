name_pairs <- c("first", "second")

estimate_g_scores <- function(data, id_cols = NULL) {
  if (is.null(id_cols)) {
    id_cols <- names(data)[[1]]
  }
  vars <- setdiff(names(data), id_cols)
  mdl <- paste(
    "g =~",
    paste0("`", vars, "`", collapse = " + ")
  )
  fitted <- cfa(mdl, data, std.ov = TRUE, missing = "ml")
  data[id_cols] |>
    mutate(g = lavPredict(fitted)[, "g"])
}

resample_vars <- function(data, num_vars, id_cols = NULL, paired = FALSE) {
  if (is.null(id_cols)) {
    id_cols <- names(data)[[1]]
  }
  vars <- setdiff(names(data), id_cols)
  if (!paired) {
    vars_sel <- sample(vars, num_vars)
    select(data, all_of(c(id_cols, vars_sel)))
  } else {
    if (2 * num_vars > length(vars)) {
      stop("Too many variables for each pair.")
    }
    vars_sel <- sample(vars, 2 * num_vars)
    list(
      vars_sel[1:num_vars],
      vars_sel[num_vars + (1:num_vars)]
    ) |>
      set_names(name_pairs) |>
      map(~ select(data, all_of(c(id_cols, .))))
  }
}

correlate_scores_pairs <- function(scores_pairs) {
  data <- scores_pairs[name_pairs]
  meta <- scores_pairs[setdiff(names(scores_pairs), name_pairs)]
  bind_rows(data, .id = "pair") |>
    pivot_wider(
      id_cols = sub_id,
      names_from = pair,
      values_from = g
    ) |>
    summarise(
      cor.test(.data[[name_pairs[[1]]]], .data[[name_pairs[[2]]]]) |>
        broom::tidy()
    ) |>
    mutate(!!!meta)
}

do_cpm_pairs <- function(scores_pairs, ...) {
  data <- scores_pairs[name_pairs]
  meta <- scores_pairs[setdiff(names(scores_pairs), name_pairs)]
  do_cpm_partial <- partial(do_cpm, ...)
  map(data, do_cpm_partial) |>
    bind_rows(.id = "pair") |>
    mutate(!!!meta) |>
    rename_with(~ str_c(., "_last"), starts_with("tar"))
}

hypers_stability_pairs <- tibble::tibble(num_vars = 4:10)
g_scores_pairs <- tarchetypes::tar_map(
  hypers_stability_pairs,
  list(
    tarchetypes::tar_rep(
      data_pairs,
      resample_vars(indices_wider_clean, num_vars, paired = TRUE),
      batches = 10,
      reps = 10,
      iteration = "list"
    ),
    tar_target(
      scores_g_pairs,
      map(
        data_pairs,
        ~ map_at(., name_pairs, estimate_g_scores)
      ),
      pattern = map(data_pairs)
    ),
    tar_target(
      scores_g_pairs_cor,
      map_df(
        scores_g_pairs,
        correlate_scores_pairs
      ),
      pattern = map(scores_g_pairs)
    ),
    tar_target(
      result_cpm_pairs,
      map_df(
        index_rep_cpm,
        ~ scores_g_pairs |>
          map_df(
            ~ do_cpm_pairs(
              .,
              fc_data = fc_data_rest_nn268_without,
              thresh_method = "sparsity",
              thresh_level = 0.01
            )
          ) |>
          add_column(rep = ., batch = index_batch_cpm)
      ),
      pattern = cross(scores_g_pairs, index_batch_cpm)
    ),
    tar_target(
      cpm_pred_pairs,
      result_cpm_pairs |>
        select(pair, edge_type, contains(c("batch", "rep")), cor) |>
        mutate(cor = map_dbl(cor, "estimate")) |>
        pivot_wider(
          id_cols = c(contains(c("batch", "rep")), edge_type),
          names_from = pair,
          values_from = cor
        )
    ),
    tar_target(
      mask_pairs,
      result_cpm_pairs |>
        select(pair, edge_type, ends_with("last"), mask_prop) |>
        filter(!map_lgl(mask_prop, is.null)) |>
        nest(.by = c(pair, edge_type, ends_with("last"))) |>
        mutate(
          mask = map(
            data,
            ~ .$mask_prop |>
              reduce(cbind) |>
              rowMeans()
          ),
          .keep = "unused"
        ) |>
        pivot_wider(
          id_cols = c(ends_with("last"), edge_type),
          names_from = pair,
          values_from = mask
        )
    ),
    tarchetypes::tar_map_rep(
      dice_mask_pairs,
      command = mask_pairs |>
        mutate(across(all_of(name_pairs), ~ map(., ~ . > thr))) |>
        mutate(
          dice = map2_dbl(
            .data[[name_pairs[[1]]]],
            .data[[name_pairs[[2]]]],
            ~ rbind(.x, .y) |>
              proxy::simil(method = "dice") |>
              unclass()
          ),
          .keep = "unused"
        ),
      values = data.frame(thr = 0.9)
    )
  )
)
