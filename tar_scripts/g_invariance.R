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
  controller = crew::crew_controller_local(workers = 8, auto_scale = "one")
)

# targets globals ----
tar_source()
future::plan(future.callr::callr)
store_preproc_behav <- fs::path(
  tar_config_get("store", project = "project_preproc_behav"),
  "objects"
)

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
  permute_cpm2(
    scores_g,
    config_neural,
    hypers_cpm,
    subjs_subset = subjs_combined,
    include_file_targets = FALSE
  )
)

# targets pipeline ----
list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc_behav, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    behav_main,
    fs::path(store_preproc_behav, "behav_main"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_combined,
    file_subjs_combined,
    read = as.numeric(read_lines(!!.x))
  ),
  # first column is identifier
  tar_target(data_names_all, names(indices_wider_clean)[-1]),
  permute_cpm2(
    behav_main,
    config_neural,
    hypers_cpm,
    subjs_subset = subjs_combined,
    name_suffix = "_main"
  ),
  g_invariance,
  combine_targets(
    data_names,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    var_exp,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    scores_g,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    cpm_pred,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  combine_targets(
    brain_mask,
    g_invariance,
    c("num_vars", "id_pairs")
  ),
  tar_target(
    dice_mask_pairs,
    brain_mask |>
      filter(n() == 2, .by = c(num_vars, idx_rsmp, modal)) |>
      pivot_longer(
        c(pos, neg),
        names_to = "edge_type",
        values_to = "mask"
      ) |>
      mutate(mask_bin = map(mask, ~ . > 0.995), .keep = "unused") |>
      pivot_wider(
        id_cols = c(num_vars, idx_rsmp, modal, edge_type),
        names_from = id_pairs,
        values_from = mask_bin
      ) |>
      mutate(
        dice = map2_dbl(
          `1`, `2`,
          ~ rbind(.x, .y) |>
            proxy::simil(method = "dice") |>
            unclass()
        ),
        .keep = "unused"
      )
  )
)
