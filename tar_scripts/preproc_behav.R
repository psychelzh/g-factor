library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs",
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

# prepare static branches targets ----
task_preproc <- readr::read_csv(
  here::here("config/task_preproc.csv"),
  show_col_types = FALSE
) |>
  tidyr::drop_na() |>
  dplyr::mutate(preproc = rlang::syms(paste0("preproc_", preproc)))
preproc_behav <- tarchetypes::tar_map(
  task_preproc,
  names = task,
  list(
    tar_target(
      indices,
      preproc(data_clean) |>
        pivot_longer(
          -any_of(id_cols()),
          names_to = "index",
          values_to = "score"
        ) |>
        select(-task_datetime)
    ),
    tarchetypes::tar_file_read(
      data,
      fs::path("data/behav", sprintf("%s.arrow", task)),
      read = arrow::read_feather(!!.x)
    ),
    tar_target(data_clean, screen_data(data))
  )
)

# targets pipeline ----
list(
  # part I: subjs info ----
  tarchetypes::tar_file_read(
    sub_id_transform,
    "config/sub_id_transform.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    data_clean,
    "data/behav/data_clean.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    subjs_info,
    "data/subjs.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    subjs_fd,
    "data/subj_fd.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(
    subjs_info_clean,
    data_clean |>
      rename(sub_id = ID, age_survey = Age, gender_survey = Gender) |>
      correct_subjs_id(sub_id_transform) |>
      full_join(subjs_info, by = "sub_id") |>
      mutate(
        age = coalesce(age, age_survey),
        sex = c("M", "F")[coalesce(gender, gender_survey)],
        site = if_else(sub_id < 10000, "BJ", "CQ")
      ) |>
      select(sub_id, age, sex, site) |>
      filter(!is.na(age), !is.na(sex))
  ),
  tar_target(
    subjs_covariates,
    subjs_info_clean |>
      full_join(subjs_fd, by = "sub_id") |>
      select(sub_id, age, sex, site, mean_fd_rest, mean_fd_task) |>
      drop_na()
  ),
  # part II: indices ----
  preproc_behav,
  tar_target(
    indices_keepTrack,
    preproc_existed(
      data_clean,
      Keeptrack_ScoreAll,
      disp_name = "KPTRK"
    )
  ),
  tar_target(
    indices_FM,
    preproc_existed(
      data_clean,
      starts_with("FM"),
      disp_name = "FM"
    )
  ),
  tarchetypes::tar_file_read(
    indices_penncnp,
    "data/behav/penncnp.csv",
    read = read_csv(!!.x, show_col_types = FALSE) |>
      preproc_penncnp()
  ),
  tar_target(
    indices_Raven,
    preproc_existed(
      data_clean,
      all_of(c(Raven_score = "Raven2")),
      disp_name = "RAPM"
    )
  ),
  tarchetypes::tar_combine(
    indices,
    preproc_behav[[1]],
    command = bind_rows(
      !!!.x,
      .id = "task"
    ) |>
      mutate(task = str_remove(task, "indices_")) |>
      left_join(task_preproc, by = "task") |>
      select(-preproc) |>
      bind_rows(
        indices_keepTrack,
        indices_FM,
        indices_penncnp,
        indices_Raven
      ) |>
      correct_subjs_id(sub_id_transform)
  ),
  tarchetypes::tar_file_read(
    indices_selection,
    "config/indices_selection.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(indices_clean, clean_indices(indices, indices_selection)),
  tar_target(
    indices_rapm,
    indices |>
      filter(task == "Raven", index == "score") |>
      filter(!performance::check_outliers(score, method = "iqr")) |>
      reshape_data_wider()
  ),
  tar_target(
    indices_wider,
    reshape_data_wider(indices_clean, name_score = "score_norm")
  ),
  tar_target(
    indices_wider_clean,
    indices_wider |>
      filter(sub_id < 17000) |>
      rowwise() |>
      filter(mean(is.na(c_across(-sub_id))) < 0.2) |>
      ungroup() |>
      semi_join(subjs_info_clean, by = "sub_id")
  ),
  # part III: factor analysis ----
  tarchetypes::tar_file_read(
    mdl_spec,
    "config/behav.lavaan",
    read = readLines(!!.x)
  ),
  tar_target(
    mdl_fitted,
    cfa(mdl_spec, indices_wider_clean, std.ov = TRUE, missing = "fiml")
  ),
  tar_target(
    scores_latent,
    bind_cols(
      select(indices_wider_clean, sub_id),
      as_tibble(unclass(lavPredict(mdl_fitted)))
    )
  ),
  tar_target(
    var_exp_full,
    calc_var_exp(mdl_fitted_full)
  ),
  tar_target(
    mdl_fitted_full,
    fit_g(indices_wider_clean, names(indices_wider_clean)[-1])
  ),
  tar_target(
    scores_g_full,
    predict_g_score(indices_wider_clean, mdl_fitted_full)
  ),
  tar_target(
    behav_main,
    tribble(
      ~idx, ~scores,
      "g_full", scores_g_full,
      "rapm", indices_rapm
    )
  )
)
