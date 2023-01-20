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

critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- qt( alpha/2, df, lower.tail = FALSE )
  sqrt( (ct^2) / ( (ct^2) + df ) )
}
