#' Calculate Dice coefficient for brain mask
#'
#' @param mask A vector of brain mask, should be the upper triangle of the
#'   original full mask.
#' @param ... Other arguments passed to [binarize_mask()].
#' @returns A numeric value of Dice coefficient.
#' @export
calc_mask_dice <- function(mask, ...) {
  do.call(
    rbind,
    lapply(mask, binarize_mask, ...)
  ) |>
    proxy::simil(method = "dice") |>
    unclass()
}

#' Binarize brain mask
#'
#' @param mask A vector of brain mask, should be the upper triangle of the
#'   original full mask.
#' @param binarize_method A character string of binarization method, should be
#'   one of `"count"` and `"value"`.
#' @param binarize_level A numeric value of binarization level, should be a
#'   number between 0 and 1 (default: `0.95`) if `binarize_method` is `"value"`,
#'   or a number between 1 and length of `mask` (default: `500`) if
#'   `binarize_method` is `"count"`.
#' @returns A logical vector of binarized mask.
#' @export
binarize_mask <- function(mask, binarize_method = c("count", "value"),
                          binarize_level = NULL) {
  binarize_method <- match.arg(binarize_method)
  if (is.null(binarize_level)) {
    binarize_level <- ifelse(binarize_method == "count", 500, 0.95)
  }
  if (binarize_method == "value") {
    mask_out <- mask > binarize_level
  } else if (binarize_method == "count") {
    mask_idx <- order(mask, decreasing = TRUE)[seq_len(binarize_level)]
    mask_out <- rep(FALSE, length(mask))
    mask_out[mask_idx] <- TRUE
  } else {
    stop("`method` must be either 'value' or 'count'")
  }
  mask_out
}

#' Prepare adjacency matrix
#'
#' @name prepare_adjacency
#' @param mask A vector of brain mask, should be the upper triangle of the
#'   original full mask.
#' @param ... Further arguments passed to [binarize_mask()].
#' @param which The index to transform to matrix. Default to `"bin"`, means the
#'   original binary matrix.
#' @param diagonal Value set to diagnol, default to 0.
#' @returns For `prepare_adj_df`, a data frame of adjacency matrix, with columns
#'   `row`, `col`, `bin`, `frac`. For `prepare_adj_mat`, a matrix with values
#'   from `which` is returned.
NULL

#' @rdname prepare_adjacency
#' @export
prepare_adj_df <- function(mask, ...) {
  # prepare adjacency matrix in a data.frame
  size <- (sqrt((8 * length(mask)) + 1) + 1) / 2
  mask_bin <- binarize_mask(mask, ...)
  expand_grid(row = seq_len(size), col = seq_len(size)) |>
    filter(row < col) |> # upper triangle has larger column index
    add_column(
      bin = mask_bin,
      frac = if_else(mask_bin, mask, 0)
    )
}

#' @rdname prepare_adjacency
#' @export
prepare_adj_mat <- function(mask, ..., which = c("bin", "frac"),
                            diagonal = 0) {
  which <- match.arg(which)
  adj_df <- prepare_adj_df(mask, ...) |>
    select(row, col, all_of(which))
  size <- with(adj_df, n_distinct(c(row, col)))
  diags <- expand_grid(row = seq_len(size), col = seq_len(size)) |>
    filter(row == col) |>
    add_column("{which}" := diagonal)
  adj_mat <- bind_rows(adj_df, diags) |>
    pivot_wider(
      names_from = col,
      names_sort = TRUE,
      values_from = all_of(which),
      values_fill = rlang::list2("{which}" := 0)
    ) |>
    column_to_rownames("row") |>
    as.matrix()
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat
}
