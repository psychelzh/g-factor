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
  data_names <- rownames(loadings(mdl))
  for (data_name in data_names) {
    test <- cor.test(g, data[[data_name]], use = "pairwise")
    # if g is anti-correlated significantly with any ov, inverse it
    if (test$estimate < 0 && test$p.value < 0.05) {
      g <- -g
      break
    }
  }
  add_column(data[, id_cols], g = g)
}

#' Regress out covariates
#'
#' @param data A data frame with subject identifiers and outcome variables.
#' @param subjs_info A data frame with subject identifiers and covariates. The
#'   FD values should be named as `mean_fd_task` and `mean_fd_rest` for n-back
#'   task and resting state data, respectively. And the first column should be
#'   the subject identifier.
#' @param covars A character vector specifying the column names of covariates to
#'   be included. If set as `TRUE` (default), all covariates will be regressed
#'   out (not including FD values, those are treated in `cond`). If set as
#'   `NULL`, no covariates will be regressed out.
#' @param cond A character string specifying which FD values to be regressed
#'   out. It can be either `nbackrun1`, `rest`, `run1rest` or `latent`. If set
#'   as `NULL` (default), no FD values will be regressed out.
#' @returns A data frame with residuals.
#' @export
regress_covariates <- function(data, subjs_info, covars = TRUE, cond = NULL) {
  # handle user identifier and condition specific FD values
  names_mean_fd <- c("mean_fd_task", "mean_fd_rest")
  if (is.null(covars)) {
    # return the original data
    return(data)
  }
  if (isTRUE(covars)) {
    covars <- setdiff(names(subjs_info)[-1], names_mean_fd)
  }
  if (!is.null(cond)) {
    covars <- c(
      covars,
      switch(cond,
        nbackrun1 = names_mean_fd[[1]],
        rest = names_mean_fd[[2]],
        run1rest = ,
        latent = names_mean_fd,
        stop("Not supported condition!")
      )
    )
  }
  data |>
    left_join(subjs_info, by = "sub_id") |>
    mutate(
      across(
        # the first column is the subject identifier
        all_of(names(data)[-1]),
        ~ paste(cur_column(), "~", paste(covars, collapse = " + ")) |>
          as.formula() |>
          lm(na.action = na.exclude) |>
          residuals.lm() |>
          as.vector()
      )
    ) |>
    select(all_of(names(data)))
}
