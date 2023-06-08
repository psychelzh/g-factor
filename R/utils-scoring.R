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
#' @param subjs_info A data frame with subject identifiers and covariates.
#' @param covars A character vector specifying covariates. If `NULL`, all
#'   covariates will be included.
#' @returns A data frame with residuals.
#' @export
regress_covariates <- function(data, subjs_info, covars = NULL) {
  if (is.null(covars)) {
    covars <- names(subjs_info)[-1]
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
          resid() |>
          as.vector()
      )
    ) |>
    select(all_of(names(data)))
}
