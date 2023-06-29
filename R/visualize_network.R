#' Visualize brain mask as chord diagram
#'
#' @param adj_mat A matrix of adjacency.
#' @param roi_labels A data frame contains ROI labels and colors.
#' @param link_val A character string specifying the value of the link.
#' @param link_color A character string of length two, specifying the color of
#'   the lowest and highest link value.
#' @param group_by_hemi A logical value indicating whether to group ROIs by
#'   hemisphere.
#' @returns Invisible `NULL`.
#' @import circlize
#' @export
visualize_network <- function(adj_mat, roi_labels,
                              link_val = c("degree", "relative", "rel_adj"),
                              link_color = c("white", "black"),
                              group_by_hemi = TRUE) {
  link_val <- match.arg(link_val)
  col_label <- if (group_by_hemi) "label_hemi" else "label"
  col_color <- if (group_by_hemi) "color_hemi" else "color_hex"

  data_plot <- summarise_adjacency(adj_mat, roi_labels[[col_label]]) |>
    select(1, 2, all_of(link_val))

  # setup grid colors
  grid_colors <- roi_labels |>
    distinct(pick(all_of(c(col_label, col_color)))) |>
    deframe()
  if (group_by_hemi) {
    # extract hemisphere group names
    label_hemis <- data_plot[data_plot[[3]] != 0, 1:2] |>
      unclass() |>
      list_c() |>
      unique() |>
      sort()
    groups <- setNames(
      str_extract(label_hemis, "left|right"),
      label_hemis
    )
  }

  # plot chord diagram with groups support
  circos.clear()
  chordDiagram(
    data_plot,
    grid.col = grid_colors,
    col = colorRamp2(
      range(data_plot[[3]]),
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
    mutate(
      label = as_factor(label),
      label_hemi = as_factor(label_hemi)
    ) |>
    arrange(index)
}

#' Prepare adjacency matrix
#'
#' @name prepare_adjacency
#' @param mask A vector of brain mask, should be the upper triangle of the
#'   original full mask.
#' @param ... Further arguments passed to [binarize_mask()].
#' @param value A character string specifying the value of the adjacency matrix.
#'  If `"binary"`, the adjacency matrix will be binary. If `"frac"`, the
#'  adjacency matrix will be the fraction of the mask.
#' @param diagonal The value of the diagonal of the adjacency matrix.
#' @returns The adjacency `matrix`.
#' @export
prepare_adjacency <- function(mask, ..., value = c("binary", "frac"),
                              diagonal = NA) {
  value <- match.arg(value)
  mask_out <- binarize_mask(mask, ...)
  if (value == "frac") {
    mask_out <- mask_out * mask
  }
  vec_to_mat(mask_out, diagonal = diagonal)
}

vec_to_mat <- function(vec, diagonal = NA) {
  size <- (sqrt((8 * length(vec)) + 1) + 1) / 2
  mat <- matrix(0, nrow = size, ncol = size)
  mat[upper.tri(mat)] <- vec
  mat <- mat + t(mat)
  diag(mat) <- diagonal
  mat
}

summarise_adjacency <- function(adj_mat, labels) {
  adj_mat |>
    as.data.frame() |>
    rowid_to_column(var = "x") |>
    pivot_longer(
      -x,
      names_to = "y",
      values_to = "val"
    ) |>
    mutate(y = parse_number(y)) |>
    filter(x != y) |>
    mutate(
      across(
        c(x, y),
        ~ labels[.x]
      )
    ) |>
    summarise(
      degree = sum(val),
      n = n(),
      .by = c(x, y)
    ) |>
    mutate(
      across(
        c(degree, n),
        ~ ifelse(x == y, .x / 2, .x)
      ),
      relative = degree / n,
      rel_adj = relative / (n / sum(n))
    )
}
