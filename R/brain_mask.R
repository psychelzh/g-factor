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
#'   one of `"prob"` and `"count"`.
#' @param binarize_level A numeric value of binarization level, should be a
#'   number between 0 and 1 (default: `0.95`) if `binarize_method` is `"prob"`,
#'   or a number between 1 and length of `mask` (default: `500`) if
#'   `binarize_method` is `"count"`.
#' @returns A logical vector of binarized mask.
#' @export
binarize_mask <- function(mask, binarize_method = c("prob", "count"),
                          binarize_level = NULL) {
  binarize_method <- match.arg(binarize_method)
  if (is.null(binarize_level)) {
    binarize_level <- ifelse(binarize_method == "count", 500, 0.95)
  }
  if (binarize_method == "prob") {
    mask_out <- mask > binarize_level
  } else if (binarize_method == "count") {
    mask_idx <- order(mask, decreasing = TRUE)[seq_len(binarize_level)]
    mask_out <- rep(FALSE, length(mask))
    mask_out[mask_idx] <- TRUE
  } else {
    stop("`method` must be either 'prob' or 'count'")
  }
  mask_out
}
