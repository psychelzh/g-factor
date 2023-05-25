library(targets)

# targets options -----
tar_option_set(
  packages = c("tidyverse", "lavaan", "collapse"),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  error = "null",
  format = "qs",
  controller = crew::crew_controller_local(workers = 8)
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)

# prepare static branches targets ----
cfg_rsmp_vars <- dplyr::bind_rows(
  tidyr::expand_grid(
    num_vars = 3:floor(max_num_vars / 2),
    idx_rsmp = seq_len(100)
  ) |>
    dplyr::reframe(
      purrr::map2(
        num_vars, idx_rsmp,
        ~ withr::with_seed(
          as.integer(sprintf("%03d%03d", .x, .y)),
          data.frame(
            id_pairs = rep(c(1, 2), .x),
            idx_vars = sample.int(max_num_vars, . * 2, replace = FALSE)
          )
        )
      ) |>
        purrr::list_rbind(),
      .by = c(num_vars, idx_rsmp)
    ) |>
    tidyr::chop(idx_vars),
  tidyr::expand_grid(
    num_vars = (floor(max_num_vars / 2) + 1):(max_num_vars - 2),
    idx_rsmp = seq_len(100)
  ) |>
    dplyr::mutate(
      id_pairs = 1,
      idx_vars = purrr::map2(
        num_vars, idx_rsmp,
        ~ withr::with_seed(
          as.integer(sprintf("%03d%03d", .x, .y)),
          sample.int(max_num_vars, .x, replace = FALSE)
        )
      )
    )
) |>
  tidyr::chop(c(idx_rsmp, idx_vars))

config_neural <- config_neural |>
  dplyr::filter(parcel == "Power264")
hypers_cpm <- hypers_cpm |>
  dplyr::filter(thresh_method == "alpha")

g_invariance <- tarchetypes::tar_map(
  values = cfg_rsmp_vars,
  names = c(num_vars, id_pairs),
  tar_target(
    data_names,
    tibble(
      idx_rsmp = idx_rsmp, # use this to track samples
      tasks = map(idx_vars, ~ data_names_all[.])
    ),
    deployment = "main"
  ),
  include_g_fitting(
    indices_wider_clean,
    data_names
  ),
  prepare_permute_cpm2(
    config_neural,
    hypers_cpm,
    scores_g,
    subjs_subset = subjs_combined,
    include_file_targets = FALSE
  )
)

mask_dices <- tarchetypes::tar_map(
  values = cfg_rsmp_vars |>
    dplyr::filter(dplyr::n() == 2, .by = num_vars) |>
    dplyr::select(num_vars, id_pairs) |>
    dplyr::mutate(
      name_brain_mask = rlang::syms(
        paste("brain_mask", num_vars, id_pairs, sep = "_")
      )
    ) |>
    tidyr::pivot_wider(
      names_from = id_pairs,
      names_prefix = "pair_",
      values_from = name_brain_mask
    ),
  names = num_vars,
  list(
    tarchetypes::tar_map_rep(
      dice_mask_pairs,
      bind_rows(pair_1, pair_2) |>
        group_by(
          pick(
            c(idx_rsmp, any_of(c(names(config_neural), names(hypers_cpm))))
          )
        ) |>
        summarise(
          across(
            any_of(names(edge_types)),
            list(
              dice = ~ calc_mask_dice(
                .x,
                binarize_method = binarize_method,
                binarize_level = binarize_level
              )
            )
          ),
          .groups = "drop"
        ),
      values = dplyr::bind_rows(
        data.frame(
          binarize_method = "value",
          binarize_level = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.995)
        ),
        data.frame(
          binarize_method = "count",
          binarize_level = seq(100, 1000, 100)
        )
      )
    )
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc_behav, "objects", "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "objects", "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_combined,
    file_subjs_combined,
    read = as.numeric(read_lines(!!.x))
  ),
  # first column is identifier
  tar_target(data_names_all, names(indices_wider_clean)[-1]),
  prepare_permute_cpm2(
    config_neural,
    hypers_cpm,
    behav_main,
    subjs_subset = subjs_combined,
    name_suffix = "_main"
  ),
  g_invariance,
  lapply(
    rlang::exprs(
      data_names,
      var_exp,
      scores_g,
      cpm_pred,
      brain_mask
    ),
    combine_targets,
    targets = g_invariance,
    cols_targets = c("num_vars", "id_pairs")
  ),
  mask_dices,
  combine_targets(
    dice_mask_pairs,
    targets = mask_dices,
    cols_targets = "num_vars"
  )
)
