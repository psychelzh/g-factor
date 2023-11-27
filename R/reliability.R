split_data_odd_even <- function(data) {
  data |>
    mutate(
      class_trial = row_number() %% 2 == 1,
      .by = id_cols()
    ) |>
    group_split(class_trial, .keep = FALSE)
}

preproc_data_splitted <- function(data_splitted, preproc, index) {
  map(data_splitted, preproc) |>
    bind_rows(.id = "id") |>
    pivot_wider(
      id_cols = id_cols(),
      names_from = id,
      values_from = all_of(index)
    ) |>
    # no valid subject id is above 18000
    filter(sub_id < 18000)
}

calc_split_half <- function(indices, index) {
  indices |>
    summarise(r = cor(`1`, `2`, use = "pairwise")) |>
    transmute(
      type = "split-half",
      # spearman-brown prophecy formula
      # https://en.wikipedia.org/wiki/Spearman%E2%80%93Brown_prediction_formula
      val = 2 * r / (1 + r)
    )
}

calc_alpha <- function(indices) {
  tibble(
    type = "alpha",
    val = indices |>
      psych::alpha(max = Inf) |>
      pluck("total", "std.alpha")
  )
}

preproc_span <- function(data) {
  data |>
    summarise(
      score = sum(span_total),
      .by = id_cols()
    )
}
