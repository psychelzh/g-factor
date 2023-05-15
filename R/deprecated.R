#' These functions are all deprecated for certain reasons. Please do not use
#' them now.

do_cpm <- function(fc_data, scores, thresh_method, thresh_level) {
  data <- fc_data |>
    tidytable::inner_join(scores, by = "sub_id") |>
    select(-sub_id) |>
    as.matrix()
  result <- cpm(
    data,
    kfolds = 10,
    thresh_method = thresh_method,
    thresh_level = thresh_level
  )
  with(
    result,
    tribble(
      ~edge_type, ~mask_prop, ~behav_pred, ~cor,
      "pos", mask_prop_pos, behav_pred_pos, cor_pos,
      "neg", mask_prop_neg, behav_pred_neg, cor_neg,
      "all", NULL, behav_pred_all, cor_all
    )
  )
}

#' Perform Connectome-based Predictive Modeling
#'
#' This is just a single run of the whole protocol (not including permutation).
#'
#' @param data A matrix contains connectome data. Observations in row, edges in
#'   column (stretch upper triangular matrix).
#' @param behav A numeric vector contains behavior data. Length must equal to
#'   number of observations in `data`. If `NULL`, the last column of `data` will
#'   be extracted to be `behav`.
#' @param kfolds Folds number of cross-validation. If `NULL`, it will be set to
#'   be equal to the number of observations, i.e., leave-one-subject-out.
#' @param thresh_method,thresh_level The threshold method and level used in edge
#'   selection. Choices are correlation p value (`"alpha"`) and network sparsity
#'   (`"sparsity"`) methods.
#' @return A list contains following fields:
#'   * mask_prop_{pos,neg}: Positive mask and negative mask, values are the
#'   selection proportion.
#'   * behav_pred_{pos,neg,all}: Predicted behavior for positive, negative and
#'   all edges.
#'   * cor_{pos,neg,all}: Correlation test result between predicted and true
#'   behavior.
#' @author Liang Zhang <psychelzh@outlook.com>
#' @export
cpm <- function(data, behav = NULL, kfolds = NULL,
                thresh_method = c("alpha", "sparsity"),
                thresh_level = 0.01) {
  thresh_method <- match.arg(thresh_method)

  # match `neural` and `behav` data
  if (is.null(behav)) {
    neural <- data[, -ncol(data)]
    behav <- data[, ncol(data)]
  } else {
    neural <- data
  }

  # random split into folds
  no_sub <- length(behav)
  if (is.null(kfolds)) kfolds <- no_sub
  folds <- seq_len(no_sub) |>
    cut(breaks = kfolds, labels = FALSE) |>
    sample()

  # pre-allocate predictions
  behav_pred_pos <- numeric(no_sub)
  behav_pred_neg <- numeric(no_sub)
  behav_pred_all <- numeric(no_sub)
  pos_masks <- matrix(numeric(kfolds * ncol(neural)), nrow = kfolds)
  neg_masks <- pos_masks

  for (fold in seq_len(kfolds)) {
    leftout <- folds == fold

    # train models
    neural_train <- neural[!leftout, ]
    behav_train <- behav[!leftout]
    r_mask <- cor(neural_train, behav_train)
    if (thresh_method == "alpha") {
      r_crit <- critical_r(sum(!leftout), thresh_level)
      pos_mask <- r_mask > 0 & r_mask > r_crit
      neg_mask <- r_mask < 0 & r_mask < -r_crit
    }
    if (thresh_method == "sparsity") {
      r_crits <- quantile(r_mask, c(thresh_level, 1 - thresh_level))
      if (r_crits[[1]] > 0 || r_crits[[2]] < 0) {
        warning("Not enough positive or negative correlation values.")
      }
      pos_mask <- r_mask > max(0, r_crits[[2]])
      neg_mask <- r_mask < min(0, r_crits[[1]])
    }
    pos_masks[fold, ] <- pos_mask
    neg_masks[fold, ] <- neg_mask

    train_sumpos <- rowSums(neural_train[, pos_mask])
    train_sumneg <- rowSums(neural_train[, neg_mask])
    fit_pos <- coef(lm(behav_train ~ train_sumpos))
    fit_neg <- coef(lm(behav_train ~ train_sumneg))
    fit_all <- coef(lm(behav_train ~ train_sumpos + train_sumneg))

    # test models
    neural_test <- neural[leftout, ]
    behav_test <- behav[leftout]
    test_sumpos <- rowSums(neural_test[, pos_mask])
    test_sumneg <- rowSums(neural_test[, neg_mask])
    behav_pred_pos[leftout] <- fit_pos[1] + fit_pos[2] * test_sumpos
    behav_pred_neg[leftout] <- fit_neg[1] + fit_neg[2] * test_sumneg
    behav_pred_all[leftout] <- fit_all[1] + fit_all[2] * test_sumpos +
      fit_all[3] * test_sumneg
  }

  cor_pos <- cor.test(behav_pred_pos, behav)
  cor_neg <- cor.test(behav_pred_neg, behav)
  cor_all <- cor.test(behav_pred_all, behav)

  list(
    mask_prop_pos = colMeans(pos_masks),
    mask_prop_neg = colMeans(neg_masks),
    behav_pred_pos = behav_pred_pos,
    cor_pos = cor_pos,
    behav_pred_neg = behav_pred_neg,
    cor_neg = cor_neg,
    behav_pred_all = behav_pred_all,
    cor_all = cor_all
  )
}

fit_model <- function(data, id_cols = NULL) {
  if (is.null(id_cols)) {
    id_cols <- names(data)[[1]]
  }
  vars <- setdiff(names(data), id_cols)
  mdl <- paste(
    "g =~",
    paste0("`", vars, "`", collapse = " + ")
  )
  cfa(mdl, data, std.ov = TRUE, missing = "ml")
}

# TODO: this is deprecated because model object is not returned
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

bind_pairs <- function(pairs) {
  data <- pairs[name_pairs]
  meta <- pairs[setdiff(names(pairs), name_pairs)]
  data |>
    bind_rows(.id = "pair") |>
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

#' Target factory for CPM permutation
#'
#' This will generate batches of CPM permutation for targets to use.
#'
#' @param behav The name for the behavioral data. Should be a symbol.
#' @param neural,store_neural One and only one of these two parameters should be
#'   specified. `neural` must be a symbol referring to the brain data, whereas
#'   `store_neural` must be a character scalar referring to the storage path of
#'   the brain data, in this way, the `"modal"`, `"parcel"` and `"gsr"` fields
#'   must be present in `hypers` to specify which neural data to use.
#' @param hypers A [data.frame()] storing the hyper parameters passed to the
#'   `values` argument of [tarchetypes::tar_map_rep()]. Currently
#'   `"thresh_method"` and `"thresh_level"` are required.
#' @param name_suffix The name suffix for the target.
#' @param by_brain_mask The `by` specification for [extract_brain_mask()].
#'   Default is the names matching those of `behav` and `hypers`, i.e.,
#'   `any_of(c(names(behav), names(hypers)))`.
#' @param split_hyper,subjs_info If one of these two parameters is specified,
#'   the other must be specified, too. `split_hyper` specifies the field used to
#'   split neural data to perform different CPM calculations, e.g., different
#'   gender. Note this field must be present in both `hypers` and `subjs_info`.
#' @param batches,reps The number of batches and repetitions passed to
#'   [tarchetypes::tar_map_rep()].
#' @returns A new target object to calculate the permutation results.
permute_cpm <- function(behav, neural, hypers,
                        name_suffix = "",
                        store_neural = NULL,
                        by_brain_mask = NULL,
                        split_hyper = NULL,
                        subjs_info = NULL,
                        batches = 4, reps = 5) {
  rlang::check_exclusive(neural, store_neural, .require = TRUE)
  if (missing(neural)) {
    stopifnot(all(rlang::has_name(hypers, c("modal", "parcel", "gsr"))))
    neural <- store_neural |>
      fs::path(sprintf("fc_data_%s_%s_%s", modal, parcel, gsr)) |>
      qs::qread() |>
      substitute()
  }
  if (missing(by_brain_mask)) {
    by_brain_mask <- substitute(any_of(c(names(behav), names(hypers))))
  }
  if (!missing(split_hyper)) {
    stopifnot(!missing(subjs_info))
    neural <- .(substitute(neural)) |>
      semi_join(
        filter(
          .(substitute(subjs_info)),
          .data[[.(split_hyper)]] == .(as.name(split_hyper))
        ),
        by = "sub_id"
      ) |>
      bquote()
  }
  name_result_cpm <- paste0("result_cpm", name_suffix)
  name_cpm_pred <- paste0("cpm_pred", name_suffix)
  name_brain_mask <- paste0("brain_mask", name_suffix)
  list(
    tarchetypes::tar_map_rep_raw(
      name_result_cpm,
      behav |>
        mutate(
          cpm = map(
            scores,
            ~ do_cpm2(
              neural,
              .,
              thresh_method = thresh_method,
              thresh_level = thresh_level
            )
          ),
          .keep = "unused"
        ) |>
        substitute(),
      values = hypers,
      batches = batches,
      reps = reps
    ),
    tar_target_raw(
      name_cpm_pred,
      extract_cpm_pred(
        .(as.name(name_result_cpm))
      ) |>
        bquote()
    ),
    tar_target_raw(
      name_brain_mask,
      extract_brain_mask(
        .(as.name(name_result_cpm)),
        by = .(substitute(by_brain_mask))
      ) |>
        bquote()
    )
  )
}
