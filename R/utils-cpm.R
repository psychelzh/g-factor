#' Perform CPM by correctly join neural and behavioral data
do_cpm2 <- function(neural, behav, kfolds, thresh_method, thresh_level,
                    bias_correct = TRUE, id_cols = "sub_id") {
  neural |>
    inner_join(behav, by = id_cols) |>
    select(-all_of(id_cols)) |>
    drop_na() |> # missing values will cause error
    as.matrix() |>
    cpm2(
      kfolds = kfolds,
      bias_correct = bias_correct,
      thresh_method = thresh_method,
      thresh_level = thresh_level
    )
}

extract_cpm_pred <- function(result_cpm, col_cpm = cpm) {
  extract_cors <- function(cpm, edge_types = c("pos", "neg", "all")) {
    map_dbl(edge_types, ~ cpm[[str_c("cor_", .)]]$estimate) |>
      as_tibble_row(.name_repair = ~edge_types)
  }
  result_cpm |>
    mutate(
      map({{ col_cpm }}, extract_cors) |>
        list_rbind(),
      .keep = "unused"
    )
}

extract_brain_mask <- function(result_cpm, by, col_cpm = cpm) {
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
      .by = {{ by }}
    )
}

calc_mask_dice <- function(mask, thresh_method = c("value", "count"),
                           thresh_level = NULL) {
  thresh_method <- match.arg(thresh_method)
  if (is.null(thresh_level)) {
    thresh_level <- switch(thresh_method,
      value = 0.995,
      count = 100
    )
  }
  thresh_mask <- function(mask) {
    switch(thresh_method,
      value = mask > thresh_level,
      count = {
        thresh_idx <- order(mask, decreasing = TRUE)[seq_len(thresh_level)]
        mask_out <- rep(FALSE, length(mask))
        mask_out[thresh_idx] <- TRUE
        mask_out
      }
    )
  }
  do.call(
    rbind,
    lapply(mask, thresh_mask)
  ) |>
    proxy::simil(method = "dice") |>
    unclass()
}
