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
