calc_spd_acc <- function(data, name_rt = "rt", name_acc = "acc", .by = NULL) {
  data |>
    group_by(across(all_of(.by))) |>
    summarise(
      pe = (sum(.data[[name_acc]] == 0) + 0.5) / (n() + 1),
      # reaction times are not normally distributed
      mrt = median(.data[[name_rt]][.data[[name_acc]] == 1]),
      ies = mrt / (1 - pe),
      .groups = "drop"
    )
}

calc_auc_ltm <- function(data,
                         name_type = "type_sig",
                         name_resp = "resp",
                         .by = NULL) {
  expand_grid(crit = 0:6, data) |>
    mutate(
      acc = if_else(
        .data[[name_type]] == "s",
        .data[[name_resp]] > crit & .data[[name_resp]] != 0,
        .data[[name_resp]] <= crit & .data[[name_resp]] != 0
      )
    ) |>
    group_by(across(all_of(c(.by, "crit", name_type)))) |>
    summarise(pc = mean(acc == 1, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = all_of(name_type),
      names_prefix = "pc_",
      values_from = pc
    ) |>
    mutate(
      p_hit = pc_s,
      p_fa = 1 - pc_n
    ) |>
    group_by(across(all_of(.by))) |>
    summarise(
      auc = DescTools::AUC(p_fa, p_hit),
      .groups = "drop"
    )
}

calc_cost <- function(data,
                      name_acc = "acc",
                      name_rt = "rt",
                      name_switch = "task_switch",
                      value_repeat = "repeat",
                      value_switch = "switch",
                      value_filler = "filler") {
  data |>
    filter(.data[[name_switch]] != value_filler) |>
    calc_spd_acc(
      name_rt = name_rt,
      name_acc = name_acc,
      .by = c(id_cols(), name_switch)
    ) |>
    pivot_longer(
      -any_of(c(id_cols(), name_switch)),
      names_to = "index_name",
      values_to = "index_value"
    ) |>
    pivot_wider(
      names_from = all_of(name_switch),
      values_from = index_value
    ) |>
    mutate(
      cost = .data[[value_switch]] - .data[[value_repeat]],
      .keep = "unused"
    ) |>
    pivot_wider(
      names_from = index_name,
      names_prefix = "switch_cost_",
      values_from = cost
    )
}

summarise_ssd <- function(ssd) {
  peaks <- pracma::findpeaks(ssd)[, 1]
  valleys <- 0 - pracma::findpeaks(-ssd)[, 1]
  keep_length <- min(length(peaks), length(valleys))
  mean(c(tail(peaks, keep_length), tail(valleys, keep_length)))
}

id_cols <- function() {
  c("file", "sub_id", "task_datetime")
}

clean_combined <- function(df, name, to) {
  df |>
    mutate(id = str_remove(id, name)) |>
    separate(id, c(NA, to), convert = TRUE)
}
