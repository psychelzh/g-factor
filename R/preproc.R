# preproc functions ----
preproc_antisac <- function(data) {
  calc_spd_acc(data, .by = id_cols())
}

#' Switch Cost
preproc_shifting <- function(data) {
  data |>
    filter(task_switch != "filler") |>
    calc_spd_acc(
      name_rt = "rt",
      name_acc = "acc",
      .by = c(id_cols(), "task_switch")
    ) |>
    pivot_longer(
      -any_of(c(id_cols(), "task_switch")),
      names_to = "index_name",
      values_to = "index_value"
    ) |>
    pivot_wider(
      names_from = task_switch,
      values_from = index_value
    ) |>
    mutate(
      cost = `switch` - `repeat`,
      .keep = "unused"
    ) |>
    pivot_wider(
      names_from = index_name,
      names_prefix = "switch_cost_",
      values_from = cost
    )
}

preproc_crt <- function(data) {
  calc_spd_acc(
    data,
    name_rt = "Response_Time",
    name_acc = "Correct",
    .by = id_cols()
  )
}
preproc_srt <- function(data) {
  summarise(data, mrt = median(Response), .by = id_cols())
}

preproc_filtering <- function(data) {
  data |>
    group_by(pick(c(id_cols(), "rotated"))) |>
    summarise(
      pc = mean(acc == 1),
      .groups = "drop_last"
    ) |>
    summarise(
      k = 2 * (sum(pc) - 1),
      .groups = "drop"
    )
}

#' Long-term Memory
#'
#' "FNRecog, KRecog"
preproc_ltm <- function(data) {
  data |>
    mutate(type_sig = if_else(type == "old", "s", "n")) |>
    calc_auc_ltm(
      name_type = "type_sig",
      name_resp = "resp",
      .by = id_cols()
    )
}

#' Complex Span
preproc_ospan <- function(data) {
  select(data, c(id_cols(), score = "ospan_total"))
}
preproc_sspan <- function(data) {
  select(data, c(id_cols(), score = "sspan_total"))
}

preproc_nback <- function(data) {
  preproc.iquizoo::nback(data, .by = id_cols())
}

preproc_spst <- function(data) {
  inner_join(
    data |>
      group_by(across(all_of(c(id_cols(), "condition")))) |>
      summarise(p_sim = mean(response == "similar"), .groups = "drop_last") |>
      summarise(
        bps_score = p_sim[condition == "lure"] - p_sim[condition == "foil"],
        .groups = "drop"
      ),
    data |>
      mutate(LureBin = if_else(condition == "foil", 6, LureBin)) |>
      group_by(across(all_of(c(id_cols(), "LureBin")))) |>
      summarise(p_old = mean(response == "old"), .groups = "drop_last") |>
      summarise(auc = DescTools::AUC(LureBin, 1 - p_old), .groups = "drop"),
    by = id_cols()
  )
}

preproc_stopsignal <- function(data) {
  ssd <- data |>
    filter(is_stop == 1) |>
    group_by(across(all_of(c(id_cols(), "stair_case")))) |>
    summarise(ssd = summarise_ssd(ssd), .groups = "drop_last") |>
    summarise(mssd = mean(ssd), .groups = "drop")
  rt_go <- data |>
    filter(is_stop == 1) |>
    summarise(
      pe_stop = mean(resp != "none"),
      .by = id_cols()
    ) |>
    inner_join(
      data |>
        filter(is_stop == 0) |>
        nest(.by = id_cols()),
      by = id_cols()
    ) |>
    mutate(
      rt_nth = map2_dbl(
        data, pe_stop,
        ~ quantile(.x$rt, .y)
      )
    ) |>
    select(-data)
  inner_join(rt_go, ssd, by = id_cols()) |>
    mutate(ssrt = rt_nth - mssd / 1000)
}

preproc_stroop <- function(data) {
  data |>
    calc_spd_acc(
      name_rt = "rt",
      name_acc = "acc",
      .by = c(id_cols(), "type")
    ) |>
    pivot_longer(
      -any_of(c(id_cols(), "type")),
      names_to = "index_name",
      values_to = "index_value"
    ) |>
    pivot_wider(
      names_from = type,
      values_from = index_value
    ) |>
    mutate(
      con_eff = inc - con,
      .keep = "unused"
    ) |>
    pivot_wider(
      names_from = index_name,
      names_prefix = "con_eff_",
      values_from = con_eff
    )
}

preproc_penncnp <- function(data) {
  cpt <- data |>
    select(sub_id = tet_eion.ubid, starts_with("NUM6CPT")) |>
    filter(NUM6CPT.CPN6_SEN > 0.5) |>
    mutate(
      across(
        c(NUM6CPT.CPN6_TP, NUM6CPT.CPN6_FN),
        ~ coalesce(., 0) + 1 / 3
      ),
      across(
        c(NUM6CPT.CPN6_FP, NUM6CPT.CPN6_TN),
        ~ coalesce(., 0) + 2 / 3
      ),
      NUM6CPT.CPN6_HIT = NUM6CPT.CPN6_TP / (NUM6CPT.CPN6_TP + NUM6CPT.CPN6_FN),
      NUM6CPT.CPN6_FA = NUM6CPT.CPN6_FP / (NUM6CPT.CPN6_FP + NUM6CPT.CPN6_TN),
      NUM6CPT.dprime = qnorm(NUM6CPT.CPN6_HIT) - qnorm(NUM6CPT.CPN6_FA)
    ) |>
    select(sub_id, mrt = NUM6CPT.CPN6_TPRT, dprime = NUM6CPT.dprime) |>
    pivot_longer(
      -sub_id,
      names_to = "index",
      values_to = "score"
    ) |>
    add_column(
      task = "NUM6CPT",
      disp_name = "PCPT",
      .after = "sub_id"
    )
  lot <- data |>
    select(sub_id = tet_eion.ubid, score = VSPLOT24.VSPLOT_TC) |>
    add_column(index = "nc", task = "VSPLOT24", disp_name = "PLOT")
  bind_rows(cpt, lot)
}

preproc_existed <- function(data, ..., disp_name) {
  data |>
    select(sub_id = ID, ...) |>
    group_by(sub_id) |>
    filter(row_number() == 1) |>
    ungroup() |>
    pivot_longer(
      -sub_id,
      names_to = "task_index",
      values_to = "score"
    ) |>
    separate(task_index, c("task", "index")) |>
    mutate(index = tolower(index)) |>
    add_column(disp_name = disp_name)
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
#' @param extracov A numeric value indicating if and which FD values should be
#'   included. `0` means no FD values will be included. `1` means only FD values
#'   for n-back task will be included. `2` means only FD values for resting
#'   state data will be included. `NULL` (default) means no FD values will be
#'   included.
#' @returns A data frame with residuals.
#' @export
regress_covariates <- function(data, subjs_info,
                               covars = TRUE, extracov = NULL) {
  # handle user identifier and condition specific FD values
  names_mean_fd <- c("mean_fd_task", "mean_fd_rest")
  if (is.null(covars)) {
    # return the original data
    return(data)
  }
  if (isTRUE(covars)) {
    covars <- setdiff(names(subjs_info)[-1], names_mean_fd)
  }
  if (!is.null(extracov)) {
    if (extracov == 0) {
      covars <- setdiff(covars, names_mean_fd)
    } else {
      covars <- c(covars, names_mean_fd[extracov])
    }
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

# data cleaning functions ----
wrangle_data <- function(data, task) {
  data <- data |>
    distinct(across(all_of(id_cols()))) |>
    group_by(sub_id) |>
    # keep the last commit only
    filter(
      case_when(
        all(is.na(task_datetime)) ~ file == file[[1]],
        all(!is.na(task_datetime)) ~ row_number(desc(task_datetime)) == 1,
        TRUE ~ !is.na(task_datetime)
      )
    ) |>
    ungroup() |>
    left_join(data, by = id_cols())
  switch(task,
    "Filtering" = data |>
      # no response means error
      mutate(acc = coalesce(acc, 0)),
    "ShiftingColor" = ,
    "ShiftNumber" = data |>
      mutate(
        task_switch = case_when(
          is.na(lag(task)) ~ "filler",
          task == lag(task) ~ "repeat",
          .default = "switch"
        ),
        .by = id_cols()
      ),
    "WM3" = data |>
      filter(!is.na(type)) |>
      mutate(
        type = if_else(type == "match", "same", "diff"),
        # no response means error
        acc = coalesce(acc, 0)
      ),
    "spatial_2back" = data |>
      mutate(
        base_loc = lag(loc, 2),
        .by = c(id_cols(), "block")
      ) |>
      mutate(
        type = case_when(
          is.na(base_loc) ~ "filler",
          loc == base_loc ~ "same",
          TRUE ~ "diff"
        ),
        # no response means error
        acc = coalesce(acc, 0)
      ),
    "StopSignal" = data |>
      # nonpositive ssds are artifact and should be actually 100ms
      mutate(ssd = if_else(ssd < 100 & is_stop == 1, 100, ssd)),
    "Stroop" = data |>
      mutate(across(everything(), unname)) |>
      mutate(
        char = case_match(
          char,
          "红" ~ "r",
          "黄" ~ "y",
          "绿" ~ "g",
          "蓝" ~ "b"
        ),
        type = if_else(color == char, "con", "inc")
      ),
    data
  )
}

screen_subjs <- function(data, task, chance) {
  if (!is.na(chance)) {
    return(
      data |>
        summarise(
          is_valid = sum(acc == 1) > qbinom(0.95, n(), chance),
          .by = id_cols()
        )
    )
  }
  switch(task,
    "CRT" = data |>
      summarise(
        is_valid = mean(Correct == 1) > 0.8,
        .by = id_cols()
      ),
    "FNRecog" = ,
    "KRecog" = data |>
      summarise(
        is_valid = mean(is.na(resp) | resp == 0) < 0.1,
        .by = id_cols()
      ),
    "OSpan" = data |>
      summarise(
        is_valid = math_error < 15,
        .by = id_cols()
      ),
    "SSpan" = data |>
      summarise(
        is_valid = symm_error < 9,
        .by = id_cols()
      ),
    "StopSignal" = data |>
      filter(is_stop == 1) |>
      summarise(
        is_valid = between(mean(resp != "none"), 0.25, 0.75),
        .by = id_cols()
      ),
    # treat all subjects as valid by default
    data |>
      summarise(
        is_valid = TRUE,
        .by = id_cols()
      )
  )
}

clean_indices <- function(indices, indices_selection) {
  indices |>
    inner_join(
      filter(indices_selection, selected),
      by = c("task", "index")
    ) |>
    mutate(score_norm = if_else(reversed, -score, score)) |>
    group_by(disp_name, index) |>
    filter(!performance::check_outliers(score, method = "iqr")) |>
    ungroup()
}

#' Correct Subjects' identifier
#'
#' Some subjects have different identifier in behavior and fmri data. Here we
#' correct them as fmri identifiers.
correct_subjs_id <- function(data, sub_id_transform, name_id = "sub_id") {
  data |>
    left_join(sub_id_transform, by = set_names(nm = name_id, "behav_id")) |>
    mutate(sub_id = coalesce(fmri_id, .data[[name_id]]), .keep = "unused")
}

reshape_data_wider <- function(indices, name_score = "score") {
  indices |>
    group_by(task) |>
    mutate(n_indices = n_distinct(index)) |>
    ungroup() |>
    mutate(
      task_index = if_else(
        n_indices == 1,
        disp_name,
        str_c(disp_name, index, sep = ".")
      )
    ) |>
    pivot_wider(
      id_cols = sub_id,
      names_from = task_index,
      values_from = all_of(name_score)
    )
}

# helper functions ----
id_cols <- function() {
  c("file", "sub_id", "task_datetime")
}

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

summarise_ssd <- function(ssd) {
  peaks <- pracma::findpeaks(ssd)[, 1]
  valleys <- 0 - pracma::findpeaks(-ssd)[, 1]
  keep_length <- min(length(peaks), length(valleys))
  mean(c(tail(peaks, keep_length), tail(valleys, keep_length)))
}
