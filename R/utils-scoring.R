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

#' Calculate the variance explained by g factor
#'
#' @param fit A fitted one-g factor model.
#' @returns A numeric value indicating the variance explained by g factor.
#' @export
calc_var_exp <- function(fit) {
  mean(loadings(fit)^2)
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
#' @param covars <[`tidy-select`][dplyr_tidy_select]> Quoted expressions
#'   specifying covariates to be regressed out.
#' @param cond A character string specifying which FD values to be regressed
#'   out. It can be either `nbackrun1` (task) or `rest`. If it is `NA`, the
#'   covariates will be regressed out for all conditions.
#' @returns A data frame with residuals.
#' @export
regress_covariates <- function(data, subjs_info, covars = everything(),
                               cond = NA) {
  # handle user identifier and condition specific values
  name_covars <- subjs_info |>
    select(
      {{ covars }} & !1 & !contains("mean_fd"),
      switch(cond,
        nbackrun1 = "mean_fd_task",
        rest = "mean_fd_rest",
        contains("mean_fd")
      )
    ) |>
    names()
  data |>
    left_join(subjs_info, by = "sub_id") |>
    mutate(
      across(
        # the first column is the subject identifier
        all_of(names(data)[-1]),
        ~ lm(
          as.formula(
            paste(cur_column(), "~", paste(name_covars, collapse = " + "))
          ),
          na.action = na.exclude
        ) |> residuals.lm() |> as.vector()
      )
    ) |>
    select(all_of(names(data)))
}
