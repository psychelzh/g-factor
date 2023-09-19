#' Fit a one-g factor model for given observed variables
#'
#' @param data Raw behavior data.
#' @param vars A character vector specifying observed variables.
#' @returns A fitted one-g factor model.
#' @export
#' @import lavaan
fit_g <- function(data, vars) {
  efa(data, ov.names = vars, std.ov = TRUE, missing = "ml")
}

#' Calculate the composite reliability
#'
#' Note based on Flora (2020), we calculated omega reliability as the variance
#' explained by g factor.
#'
#' @param fit A fitted factor model.
#' @returns A numeric vector indicating the omega reliability of all the latent
#'   factors.
#' @export
calc_comp_rel <- function(fit) {
  if (inherits(fit, "efaList")) {
    fit <- fit$nf1
  }
  semTools::compRelSEM(fit)
}

#' Predict g factor scores
#'
#' @param data Raw behavior data.
#' @param mdl A fitted one-g factor model.
#' @param id_cols A numeric vector specifying the column indices of subject
#'   identifiers.
#' @returns A data frame with g factor scores.
#' @export
predict_g_score <- function(data, mdl, id_cols = 1) {
  g <- lavPredict(mdl)[, 1]
  # inverse g if anti-correlated with the largest loading variable
  name_max_loading <- rownames(loadings(mdl))[which.max(abs(loadings(mdl)))]
  if (cor(g, data[[name_max_loading]], use = "pairwise") < 0) {
    g <- -g
  }
  add_column(data[, id_cols], g = g)
}

#' Split data into two equivalent halves by SOLOMON method
#'
#' Details can be found at Lorenzo-Seva, 2021.
#'
#' @param data The data to split.
#' @param id_cols The names for identifier columns.
#' @return A list of two equivalent data.
#' @export
split_data_solomon <- function(data, id_cols = "sub_id") {
  pca_result <- psych::principal(
    select(data, -all_of(id_cols)),
    nfactors = ncol(data) - 1,
    rotate = "none",
    missing = TRUE
  )
  data |>
    add_column(
      d = as.vector(
        pca_result$scores %*% matrix(colMeans(pca_result$loadings^2), ncol = 1)
      )
    ) |>
    mutate(id = row_number(d) %% 2, .keep = "unused") |>
    group_split(id, .keep = FALSE)
}

#' Perform Connectome-based Predictive Modeling
#'
#' This is just a single run of the whole protocol (not including permutation).
#' The number `2` in the function name is initially used to distinguish from the
#' `cpm()` which is now removed, but the name will be kept for historical
#' reasons.
#'
#' @param data A matrix contains connectome data. Observations in row, edges in
#'   column (stretch upper triangular matrix).
#' @param behav A numeric vector contains behavior data. Length must equal to
#'   number of observations in `data`. If `NULL`, the last column of `data` will
#'   be extracted to be `behav`.
#' @param kfolds Folds number of cross-validation. If `NULL`, it will be set to
#'   be equal to the number of observations, i.e., leave-one-subject-out.
#' @param bias_correct Logical value indicating if z-score should be performed
#'   separated in training and test group. This is forced to be `FALSE` for
#'   leave one out case. Otherwise, it is `TRUE` by default.
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
#' @importFrom collapse %r-% %r/% fmean fsd
#' @export
cpm2 <- function(data, behav = NULL, kfolds = NULL,
                 bias_correct = NULL,
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
  if (is.null(kfolds)) {
    kfolds <- no_sub
  }
  if (kfolds == no_sub) {
    bias_correct <- FALSE
  } else {
    if (is.null(bias_correct)) {
      bias_correct <- TRUE
    }
  }
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
    neural_train <- neural[!leftout, , drop = FALSE]
    if (bias_correct) {
      train_mns <- fmean(neural_train)
      train_sds <- fsd(neural_train)
      neural_train <- (neural_train %r-% train_mns) %r/% train_sds
    }
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

    train_sumpos <- rowSums(neural_train[, pos_mask, drop = FALSE])
    train_sumneg <- rowSums(neural_train[, neg_mask, drop = FALSE])
    fit_pos <- coef(lm(behav_train ~ train_sumpos))
    fit_neg <- coef(lm(behav_train ~ train_sumneg))
    fit_all <- coef(lm(behav_train ~ train_sumpos + train_sumneg))

    # test models
    neural_test <- neural[leftout, , drop = FALSE]
    if (bias_correct) {
      neural_test <- (neural_test %r-% train_mns) %r/% train_sds
    }
    behav_test <- behav[leftout]
    test_sumpos <- rowSums(neural_test[, pos_mask, drop = FALSE])
    test_sumneg <- rowSums(neural_test[, neg_mask, drop = FALSE])
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

#' Perform CPM by correctly join neural and behavioral data
#'
#' @param neural A `data.frame` storing the neural data.
#' @param behav A `data.frame` storing the behavioral data.
#' @param kfolds,thresh_method,thresh_level,bias_correct See `cpm2()`.
#' @param id_cols A character vector specifying the column names for subject
#'   identifiers.
#' @returns A list of results from `cpm2()`.
#' @export
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

#' Extract CPM prediction results
#'
#' @param result_cpm A list of results from `cpm2()`.
#' @param col_cpm A character vector specifying the column names for CPM
#'   prediction results.
#' @returns A data frame with CPM prediction results.
#' @export
extract_cpm_pred <- function(result_cpm, col_cpm = cpm) {
  extract_cors <- function(cpm) {
    cor_prefix <- "cor_"
    cpm |>
      select_list(starts_with(cor_prefix)) |>
      map_dbl("estimate") |>
      as_tibble_row() |>
      rename_with(~ str_remove(.x, cor_prefix))
  }
  result_cpm |>
    mutate(
      map({{ col_cpm }}, extract_cors) |>
        list_rbind(),
      .keep = "unused"
    )
}

#' Extract CPM mask results
#'
#' @param result_cpm A list of results from `cpm2()`.
#' @param by A character vector specifying the column names for grouping
#'  variables.
#' @param col_cpm A character vector specifying the column names for CPM
#'  prediction results.
#' @returns A data frame with CPM mask results.
#' @export
extract_brain_mask <- function(result_cpm, by, col_cpm = cpm) {
  aggregate_masks <- function(cpm) {
    mask_prefix <- "mask_prop_"
    cpm |>
      map(~ select_list(.x, starts_with(mask_prefix))) |>
      transpose() |>
      as_tibble() |>
      summarise(
        across(
          everything(),
          ~ list(rowMeans(do.call(cbind, .x)))
        )
      ) |>
      rename_with(~ str_remove(.x, mask_prefix))
  }
  result_cpm |>
    summarise(
      aggregate_masks({{ col_cpm }}),
      .by = {{ by }}
    )
}

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

# helper functions ----
critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

select_list <- function(.l, ...) {
  .l[tidyselect::eval_select(rlang::expr(c(...)), .l)]
}
