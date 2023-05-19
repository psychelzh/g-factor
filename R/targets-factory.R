#' Target factory to combine targets from different batches
#'
#' @param name The name for the target. Should be a symbol.
#' @param targets A list from which targets to extract.
#' @param cols_targets A character vector specifying the columns in the names of
#'   targets.
#' @returns A new target object to do the given targets combination.
combine_targets <- function(name, targets, cols_targets) {
  name <- deparse1(substitute(name))
  tarchetypes::tar_combine_raw(
    name,
    targets[[name]],
    command = bind_rows(!!!.x, .id = "id") |>
      # note there is delimiter after name should be removed too
      mutate(id = str_remove(id, str_c(name, "."))) |>
      separate(id, cols_targets, convert = TRUE) |>
      substitute()
  )
}

#' Fit and extract g factor scores
#'
#' This will generate targets used to fit a one-g factor model and predict
#' factor scores from it.
#'
#' @param indices Raw behavior data.
#' @param df_ov A [data.frame()] specifying observed variables in the field
#'   `tasks`.
#' @param include_var_exp A logical value indicating if `var_exp` should be
#'   included. `var_exp` means the variance explained targets calculating the
#'   variance explained by g factor.
#' @returns A list of new target objects to fit and extract g factor scores.
#' @export
include_g_fitting <- function(indices, df_ov, include_var_exp = TRUE) {
  list(
    tar_target_raw(
      "mdl_fitted",
      df_ov |>
        mutate(
          mdl = map(
            tasks,
            ~ fit_g(indices, .)
          ),
          .keep = "unused"
        ) |>
        substitute()
    ),
    if (include_var_exp) {
      tar_target_raw(
        "var_exp",
        mdl_fitted |>
          mutate(
            prop = map_dbl(mdl, calc_var_exp),
            .keep = "unused"
          ) |>
          substitute(),
        deployment = "main"
      )
    },
    tar_target_raw(
      "scores_g",
      mdl_fitted |>
        mutate(
          scores = map(
            mdl,
            ~ predict_g_score(indices, .)
          ),
          .keep = "unused"
        ) |>
        substitute()
    )
  )
}

#' Target factory for CPM permutation
#'
#' This will generate batches of CPM permutation for targets to use.
#'
#' @param behav The name for the behavioral data. Should be a symbol.
#' @param config_neural A [data.frame()] storing the specifications of neural
#'   data used. The `tar_neural` and `file` fields must be present to specify
#'   which neural data to use.
#' @param hypers_cpm A [data.frame()] storing the CPM hyper parameters passed to
#'   the `values` argument of [tarchetypes::tar_map_rep()]. The `kfolds`,
#'   `thresh_method` and `thresh_level` are required.
#' @param subjs_subset The subject list to include in CPM analysis.
#' @param name_suffix The name suffix for the CPM targets.
#' @param split_hyper,subjs_info If one of these two parameters is specified,
#'   the other must be specified, too. `split_hyper` specifies the field used to
#'   split neural data to perform different CPM calculations, e.g., different
#'   gender. Note this field must be present in both `hypers` and `subjs_info`.
#' @param batches,reps The number of batches and repetitions passed to
#'   [tarchetypes::tar_map_rep()].
#' @returns A list of new target objects to calculate the permutation results.
#' @export
permute_cpm2 <- function(behav,
                         config_neural,
                         hypers_cpm,
                         subjs_subset = NULL,
                         name_suffix = "",
                         include_file_targets = TRUE,
                         split_hyper = NULL,
                         subjs_info = NULL,
                         batches = 4, reps = 5) {
  neural <- quote(arrow::read_feather(tar_neural))
  if (!missing(subjs_subset)) {
    neural <- substitute(filter(neural, sub_id %in% subjs_subset))
  }
  if (!missing(split_hyper)) {
    stopifnot(!missing(subjs_info))
    neural <- .(substitute(neural)) |>
      semi_join(
        filter(
          .(substitute(subjs_info)),
          .data[[.(split_hyper)]] == .(as.name(split_hyper))
        ),
        by = "sub_id"
      ) |>
      bquote()
  }
  name_result_cpm <- paste0("result_cpm", name_suffix)
  name_cpm_pred <- paste0("cpm_pred", name_suffix)
  name_brain_mask <- paste0("brain_mask", name_suffix)
  list(
    if (include_file_targets) {
      tarchetypes::tar_eval(
        tar_target(tar_neural, file, format = "file"),
        values = config_neural
      )
    },
    tarchetypes::tar_map_rep_raw(
      name_result_cpm,
      behav |>
        mutate(
          cpm = map(
            scores,
            ~ do_cpm2(
              neural,
              .,
              kfolds = kfolds,
              thresh_method = thresh_method,
              thresh_level = thresh_level
            )
          ),
          .keep = "unused"
        ) |>
        substitute(),
      values = tidyr::expand_grid(config_neural, hypers_cpm),
      # `tar_neural` and `file` are constructed from other core elements
      columns = quote(-c(tar_neural, file)),
      batches = batches,
      reps = reps
    ),
    tar_target_raw(
      name_cpm_pred,
      extract_cpm_pred(
        .(as.name(name_result_cpm))
      ) |>
        bquote(),
      deployment = "main"
    ),
    tar_target_raw(
      name_brain_mask,
      extract_brain_mask(
        .(as.name(name_result_cpm)),
        by = .(substitute(
          any_of(c(names(behav), names(config_neural), names(hypers_cpm)))
        ))
      ) |>
        bquote(),
      deployment = "main"
    )
  )
}
