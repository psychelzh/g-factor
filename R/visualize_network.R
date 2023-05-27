#' Visualize brain mask as chord diagram
#'
#' @param adj_df A data frame contains adjacency matrix, should be the upper
#'   triangle of the original full matrix.
#' @param roi_labels A data frame contains ROI labels and colors.
#' @param link_val <[`tidy-select`][dplyr::dplyr_tidy_select]> Variable names
#'   from `adj_df` used as the value of links. If `NULL`, the link value will be
#'   set as the third column of `adj_df`.
#' @param link_color A character string of length two, specifying the color of
#'   the lowest and highest link value.
#' @param group_by_hemi A logical value indicating whether to group ROIs by
#'   hemisphere.
#' @returns Invisible `NULL`.
#' @import circlize
#' @export
visualize_network <- function(adj_df, roi_labels,
                              link_val = NULL,
                              link_color = c("white", "black"),
                              group_by_hemi = TRUE) {
  if (is.null(link_val)) link_val <- 3
  col_label <- if (group_by_hemi) "label_hemi" else "label"
  col_color <- if (group_by_hemi) "color_hemi" else "color_hex"
  # setup grid colors
  grid_colors <- roi_labels |>
    distinct(pick(all_of(c(col_label, col_color)))) |>
    deframe()
  if (group_by_hemi) {
    # extract hemisphere group names
    label_hemis <- unique(roi_label[[col_label]])
    groups <- setNames(
      str_extract(label_hemis, "left|right"),
      label_hemis
    )
  }
  # plot chord diagram with groups support
  circos.clear()
  chordDiagram(
    select(adj_df, 1, 2, {{ link_val }}),
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
    group = if (group_by_hemi) groups
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
  if (group_by_hemi) {
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
  }
  circos.clear()
  invisible()
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
