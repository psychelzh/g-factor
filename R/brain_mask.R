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

#' Visualize brain mask as chord diagram
#'
#' @param adj_df A data frame contains adjacency matrix, should be the upper
#'   triangle of the original full matrix.
#' @param roi_labels A data frame contains ROI labels and colors.
#' @param link_val A character string of link value, should be one of `"degree"`
#'   and `"prop"`.
#' @param link_color A character string of length two, specifying the color of
#'   the lowest and highest link value.
#' @returns Invisible `NULL`.
#' @export
visualize_network <- function(adj_df, roi_labels,
                              link_val = c("degree", "prop"),
                              link_color = c("white", "black")) {
  link_val <- match.arg(link_val)
  # setup grid colors
  grid_colors <- roi_labels |>
    distinct(label_hemi, color_hemi) |>
    deframe()
  # separate into hemispheres
  label_hemis <- unique(roi_labels$label_hemi)
  groups <- setNames(
    str_extract(label_hemis, "left|right"),
    label_hemis
  )
  # plot chord diagram with groups support
  circos.clear()
  chordDiagram(
    select(adj_df, 1, 2, all_of(link_val)),
    grid.col = grid_colors,
    col = colorRamp2(
      range(adj_df[[3]]),
      link_color,
      transparency = 0.5
    ),
    preAllocateTracks = list(
      track.height = strheight("A"),
      track.margin = c(mm_h(6), 0)
    ),
    annotationTrack = "grid",
    group = groups
  )
  circos.track(
    track.index = 2,
    panel.fun = \(x, y) {
      circos.text(
        CELL_META$xcenter,
        max(CELL_META$ylim),
        str_remove(
          CELL_META$sector.index,
          "(left|right)_"
        ),
        facing = "clockwise",
        cex = 0.6,
        adj = c(0, 0.5),
        niceFacing = TRUE
      )
      circos.axis(
        "bottom",
        direction = "inside",
        labels.facing = "reverse.clockwise",
        labels = FALSE,
        major.tick = FALSE
      )
    },
    bg.border = NA
  )
  highlight.sector(
    names(groups)[groups == "left"],
    track.index = 1,
    text = "Left Heimisphere",
    col = NA,
    facing = "bending"
  )
  highlight.sector(
    names(groups)[groups == "right"],
    track.index = 1,
    text = "Right Heimisphere",
    col = NA,
    facing = "bending"
  )
  circos.clear()
  invisible()
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

#' Prepare ROI labels
#'
#' @param atlas A data frame contains ROI labels and colors of a specific atlas.
#' @param ... For future usage. Should be empty.
#' @returns A data frame of ROI labels. Note that the `label_hemi` column is
#'   added as a combination of `hemi` and `label` columns, separated by `"_"`.
#' @export
prepare_roi_labels <- function(atlas, ...) {
  rlang::check_dots_empty()
  dm::dm_flatten_to_tbl(atlas, "roi") |>
    mutate(
      hemi = factor(x.mni < 0, labels = c("left", "right")),
      color_hemi = case_when(
        name == "Uncertain" ~ color_hex,
        hemi == "left" ~ colorspace::lighten(color_hex),
        hemi == "right" ~ colorspace::darken(color_hex)
      ),
      label_hemi = str_c(hemi, label, sep = "_")
    ) |>
    arrange(hemi, network) |>
    mutate(label_hemi = as_factor(label_hemi))
}

#' Prepare adjacency matrix
#'
#' @param mask A vector of brain mask, should be the upper triangle of the
#'   original full mask.
#' @param labels A data frame of ROI labels, should contain `label_hemi` column.
#'   Typically the output of [prepare_roi_labels()].
#' @param ... Further arguments passed to [binarize_mask()].
#' @returns A data frame of adjacency matrix, with columns `row`, `col`,
#'   `degree`, `count` and `prop`.
#' @export
prepare_adjacency <- function(mask, labels, ...) {
  # prepare adjacency matrix in a data.frame
  size <- (sqrt((8 * length(mask)) + 1) + 1) / 2
  mask_bin <- binarize_mask(mask, ...)
  expand_grid(row = seq_len(size), col = seq_len(size)) |>
    filter(row < col) |> # upper triangle has larger column index
    add_column(val = mask_bin) |>
    mutate(
      across(
        c(row, col),
        \(x) labels[x]
      )
    ) |>
    summarise(
      degree = sum(val),
      count = n(),
      prop = degree / count,
      .by = c(row, col)
    )
}
