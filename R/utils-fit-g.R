fit_g <- function(data, vars) {
  data |>
    select({{ vars }}) |>
    efa(std.ov = TRUE, missing = "ml")
}

calc_var_exp <- function(fit) {
  mean(loadings(fit) ^ 2)
}

predict_g_score <- function(data, mdl, id_cols = 1) {
  bind_cols(
    data[, id_cols],
    g = lavPredict(mdl)[, 1]
  )
}
