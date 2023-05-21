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
      rlang::expr(
        mutate(
          !!rlang::ensym(df_ov),
          mdl = map(
            tasks,
            ~ fit_g(!!rlang::ensym(indices), .)
          ),
          .keep = "unused"
        )
      )
    ),
    if (include_var_exp) {
      tar_target_raw(
        "var_exp",
        rlang::expr(
          mutate(
            mdl_fitted,
            prop = map_dbl(mdl, calc_var_exp),
            .keep = "unused"
          )
        ),
        deployment = "main"
      )
    },
    tar_target_raw(
      "scores_g",
      rlang::expr(
        mutate(
          mdl_fitted,
          scores = map(
            mdl,
            ~ predict_g_score(!!rlang::ensym(indices), .)
          ),
          .keep = "unused"
        )
      )
    )
  )
}

#' Target factory for CPM permutation
#'
#' This will generate batches of CPM permutation for targets to use.
#'
#' @param config_neural A [data.frame()] storing the specifications of neural
#'   data used. The `tar_neural` and `file` fields must be present to specify
#'   which neural data to use.
#' @param hypers_cpm A [data.frame()] storing the CPM hyper parameters passed to
#'   the `values` argument of [tarchetypes::tar_map_rep()]. Note the names must
#'   be consistent with the argument names of [do_cpm2()]. If this argument is
#'   not specified or is `NULL`, it will only return targets tracking the neural
#'   data.
#' @param behav The expression to get behavioral data.
#' @param subjs_subset The expression to get the subject list to include in CPM
#'   analysis.
#' @param name_suffix A character scalar specifying the name suffix for the CPM
#'   targets.
#' @param split_hyper,subjs_info If one of these two parameters is specified,
#'   the other must be specified, too. `split_hyper` should be a character
#'   scalar specifying the field used to split neural data to perform different
#'   CPM calculations, e.g., different gender. Note this field must be present
#'   in both `hypers_cpm` and `subjs_info`. `subjs_info` is an expression to get
#'   the required subjects' information used to filter out corresponding data to
#'   do CPM calculations.
#' @param batches,reps The number of batches and repetitions passed to
#'   [tarchetypes::tar_map_rep()].
#' @returns A list of new target objects to calculate the permutation results.
#'   If `hypers_cpm` is the not specified or is `NULL`, a list of targets
#'   tracking neural data will be returned.
#' @export
prepare_permute_cpm2 <- function(config_neural,
                                 hypers_cpm = NULL,
                                 behav = NULL,
                                 subjs_subset = NULL,
                                 name_suffix = "",
                                 include_file_targets = TRUE,
                                 split_hyper = NULL,
                                 subjs_info = NULL,
                                 batches = 4, reps = 5) {
  file_targets <- tarchetypes::tar_eval(
    tar_target(tar_neural, file, format = "file"),
    values = config_neural
  )
  if (missing(hypers_cpm) || is.null(hypers_cpm)) {
    return(file_targets)
  }
  neural <- rlang::expr(arrow::read_feather(tar_neural))
  if (!missing(subjs_subset)) {
    subjs_subset <- rlang::enexpr(subjs_subset)
    if (!is.null(subjs_subset)) {
      neural <- rlang::expr(
        filter(!!neural, sub_id %in% !!subjs_subset)
      )
    }
  }
  if (!missing(split_hyper) && !is.null(split_hyper)) {
    stopifnot(!missing(subjs_info))
    subjs_info <- rlang::enexpr(subjs_info)
    stopifnot(!is.null(subjs_info))
    neural <- rlang::expr(
      semi_join(
        !!neural,
        filter(
          !!subjs_info,
          .data[[!!split_hyper]] == !!rlang::ensym(split_hyper)
        ),
        by = "sub_id"
      )
    )
  }
  # prepare additional arguments for `do_cpm2()` based on `hypers_cpm`
  name_args <- intersect(names(formals(do_cpm2)), names(hypers_cpm))
  args_cpm <- setNames(rlang::syms(name_args), name_args)
  name_result_cpm <- paste0("result_cpm", name_suffix)
  name_cpm_pred <- paste0("cpm_pred", name_suffix)
  name_brain_mask <- paste0("brain_mask", name_suffix)
  list(
    if (include_file_targets) file_targets,
    tarchetypes::tar_map_rep_raw(
      name_result_cpm,
      rlang::expr(
        mutate(
          !!rlang::enexpr(behav),
          cpm = map(
            scores,
            ~ do_cpm2(!!neural, ., !!!args_cpm)
          ),
          .keep = "unused"
        )
      ),
      values = tidyr::expand_grid(config_neural, hypers_cpm),
      # `tar_neural` and `file` are constructed from other core elements
      columns = rlang::expr(-c(tar_neural, file)),
      batches = batches,
      reps = reps
    ),
    tar_target_raw(
      name_cpm_pred,
      rlang::expr(extract_cpm_pred(!!rlang::sym(name_result_cpm))),
      deployment = "main"
    ),
    tar_target_raw(
      name_brain_mask,
      rlang::expr(
        extract_brain_mask(
          !!rlang::sym(name_result_cpm),
          by = any_of(
            c(
              names(!!rlang::enexpr(behav)),
              # maybe see https://github.com/r-lib/rlang/issues/1629
              names(!!substitute(config_neural)),
              names(!!substitute(hypers_cpm))
            )
          )
        )
      ),
      deployment = "main"
    )
  )
}
