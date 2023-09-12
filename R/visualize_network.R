#' Visualize brain mask as chord diagram
#'
#' @param adj_mat A matrix of adjacency.
#' @param roi_info A data frame contains ROI labels and colors.
#' @param ... Further arguments passed to [visualize_chord()]. Currently not
#'   used.
#' @param model_type A character string specifying the type of the edge. Can be
#'   `"pos"` or `"neg"`. Note that this argument is exclusive with `link_color`.
#' @param link_val A character string specifying the value of the link.
#' @param link_color A character string of length two, specifying the color of
#'   the lowest and highest link value. If `model_type` is specified, this
#'   argument should not be used, and the color will be set automatically.
#' @param group_by_hemi A logical value indicating whether to group ROIs by
#'   hemisphere.
#' @param thresh A numeric value specifying the threshold of the value to be
#'   visualized. If the value is less than the threshold, it will be set as
#'   0.
#' @param add_legend A logical value indicating whether to add a legend.
#' @returns Invisible `NULL`.
#' @import circlize
#' @export
visualize_chord <- function(adj_mat, roi_info, ..., model_type = NULL,
                            link_val = c("n", "total", "prop", "enrich"),
                            link_color = c("white", "black"),
                            group_by_hemi = TRUE,
                            thresh = NULL,
                            add_legend = FALSE) {
  rlang::check_exclusive(model_type, link_color)
  if (!is.null(model_type)) {
    link_color <- if (model_type == "pos") {
      c("white", "#99000D")
    } else {
      c("white", "#084594")
    }
  }
  link_val <- match.arg(link_val)
  thresh <- if (link_val == "enrich") 1 else 0
  col_label <- if (group_by_hemi) "label_hemi" else "label"
  col_color <- if (group_by_hemi) "color_hemi" else "color_hex"

  data_plot <- summarise_adjacency(adj_mat, roi_info[[col_label]]) |>
    select(x, y, all_of(link_val)) |>
    mutate(
      "{link_val}" := if_else(
        .data[[link_val]] < thresh,
        0, .data[[link_val]]
      )
    )

  # setup grid colors
  grid_colors <- roi_info |>
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
  circos.par(start.degree = 90, clock.wise = FALSE)
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
        cex = 1,
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
  if (add_legend) {
    ComplexHeatmap::Legend(
      at = round(
        seq(min(data_plot[[3]]), max(data_plot[[3]]), length.out = 3),
        digits = 1
      ),
      col_fun = colorRamp2(
        range(data_plot[[3]]),
        link_color,
        transparency = 0.5
      ),
      grid_width = unit(grconvertX(0.005, "npc", "nic"), "npc"),
      grid_height = unit(grconvertX(0.02, "npc", "nic"), "npc")
    ) |>
      ComplexHeatmap::draw(
        x = unit(grconvertX(1, "npc", "nic"), "npc"),
        y = unit(grconvertY(0.15, "npc", "nic"), "npc")
      )
  }
  circos.clear()
  invisible()
}

#' Visualize network as a correlation plot
#'
#' @param adj_mat The adjacency matrix.
#' @param model_type A character string specifying the type of the edge. Can be
#'   `"pos"` or `"neg"`.
#' @param labels A vector of labels. Must have the same length as the number of
#'   rows/columns of the adjacency matrix.
#' @param ... Further arguments passed to [corrplot::corrplot()]. These
#'   arguments are not allowed to be changed: `method`, `type`, `is.corr` and
#'   `col` which are set to `"shade"`, `"upper"`, `FALSE` and `"Reds"` or
#'   `"Blues"` depending on the `model_type`.
#' @param which A character string specifying which value to use.
#' @param thresh A numeric value specifying the threshold of the value to be
#'   visualized. If the value is less than the threshold, it will be set as
#'   `NA`.
#' @param digits A numeric value specifying the number of digits to be rounded
#'   up.
#' @returns See [corrplot::corrplot()].
visualize_corrplot <- function(adj_mat, model_type, labels, ...,
                               which = c("n", "total", "prop", "enrich"),
                               thresh = NULL, digits = NULL) {
  which <- match.arg(which)
  thresh <- if (which == "enrich") 1 else 0
  digits <- if (which == "prop") 2 else 0
  ceiling_dec <- function(x, level = 1) round(x + 5 * 10^(-level - 1), level)
  stats <- summarise_adjacency(adj_mat, labels) |>
    mutate(
      val = if_else(
        .data[[which]] > thresh,
        .data[[which]],
        NA_real_
      )
    ) |>
    pivot_wider(
      id_cols = x,
      names_from = y,
      values_from = val,
      names_sort = TRUE
    ) |>
    arrange(x) |>
    column_to_rownames("x") |>
    as.matrix()
  corrplot::corrplot(
    stats,
    method = "shade",
    type = "upper",
    is.corr = FALSE,
    col = corrplot::COL1(if (model_type == "pos") "Reds" else "Blues"),
    col.lim = c(0, ceiling_dec(max(stats, na.rm = TRUE), digits)),
    na.label = "square",
    na.label.col = "white",
    ...
  )
}

#' Prepare ROI information
#'
#' @param atlas A [dm::dm] object contains ROI labels and colors of a specific
#'   atlas.
#' @param ... For future usage. Should be empty.
#' @returns A data frame of ROI labels. Note that the `label_hemi` column is
#'   added as a combination of `hemi` and `label` columns, separated by `"_"`.
#' @export
prepare_roi_info <- function(atlas, ...) {
  rlang::check_dots_empty()
  dm::dm_flatten_to_tbl(atlas, "roi") |>
    mutate(
      hemi = factor(x.mni < 0, labels = c("left", "right")),
      color_hemi = case_when(
        name == "Uncertain" ~ color_hex,
        hemi == "left" ~ colorspace::lighten(color_hex),
        hemi == "right" ~ colorspace::darken(color_hex)
      ),
      label_hemi = str_c(hemi, label, sep = "_"),
      index_homolog = match_homolog(index, x.mni, y.mni, z.mni)
    ) |>
    arrange(hemi, network) |>
    mutate(
      label = fct_inorder(label, ordered = TRUE),
      label_hemi = fct_inorder(label_hemi, ordered = TRUE)
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

#' Summarise adjacency matrix
#'
#' @param adj_mat A matrix of adjacency.
#' @param labels A vector of labels. Must have the same length as the number of
#'   rows/columns of the adjacency matrix.
#' @returns A data frame of summarised adjacency matrix. It could be converted
#'   as a matrix of upper triangle if `x` and `y` are used as row and column
#'   respectively.
summarise_adjacency <- function(adj_mat, labels) {
  adj_mat |>
    as.data.frame() |>
    rowid_to_column(var = "x_id") |>
    pivot_longer(
      -x_id,
      names_to = "y_id",
      names_transform = parse_number,
      values_to = "val"
    ) |>
    filter(x_id < y_id) |>
    mutate(
      x = pmin(labels[x_id], labels[y_id]),
      y = pmax(labels[x_id], labels[y_id]),
      .keep = "unused"
    ) |>
    summarise(
      n = sum(val),
      total = n(),
      .by = c(x, y)
    ) |>
    mutate(
      prop = n / total,
      enrich = (n / sum(n)) / (total / sum(total))
    )
}

# helper functions ----
vec_to_mat <- function(vec, diagonal = NA) {
  size <- (sqrt((8 * length(vec)) + 1) + 1) / 2
  mat <- matrix(0, nrow = size, ncol = size)
  mat[upper.tri(mat)] <- vec
  mat <- mat + t(mat)
  diag(mat) <- diagonal
  mat
}

match_homolog <- function(index, x.mni, y.mni, z.mni) {
  index_homolog <- integer(length(index))
  for (i in index) {
    pool <- if (x.mni[i] > 0) {
      index[x.mni < 0]
    } else {
      index[x.mni > 0]
    }
    row_matched <- proxy::dist(
      as.matrix(cbind(y.mni[i], z.mni[i])),
      as.matrix(cbind(y.mni[pool], z.mni[pool]))
    ) |>
      which.min()
    index_homolog[i] <- index[pool[row_matched]]
  }
  index_homolog
}
