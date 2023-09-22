#' @import targets
NULL

#' Target factory to combine targets from different batches
#'
#' @param name The name for the target. Should be a symbol.
#' @param targets A list from which targets to extract.
#' @param cols_targets A character vector specifying the columns in the names of
#'   targets.
#' @returns A new target object to do the given targets combination.
combine_targets <- function(name, targets, cols_targets) {
  if (tryCatch(!is.name(name), error = \(e) TRUE)) {
    name <- substitute(name)
  }
  name <- deparse1(name)
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
#' @param include_comp_rel A logical value indicating if `comp_rel` should be
#'   included. `comp_rel` means the composite reliability of latent factors.
#' @returns A list of new target objects to fit and extract g factor scores.
#' @export
include_g_fitting <- function(indices, df_ov,
                              include_comp_rel = TRUE,
                              name_suffix = NULL) {
  add_suffix <- function(name) {
    if (!is.null(name_suffix)){
      name <- paste(name, name_suffix, sep = "_")
    }
    name
  }
  list(
    tar_target_raw(
      add_suffix("mdl_fitted"),
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
    if (include_comp_rel) {
      tar_target_raw(
        add_suffix("comp_rel"),
        rlang::expr(
          mutate(
            !!rlang::sym(add_suffix("mdl_fitted")),
            prop = map_dbl(mdl, calc_comp_rel),
            .keep = "unused"
          )
        ),
        deployment = "main"
      )
    },
    tar_target_raw(
      add_suffix("scores_g"),
      rlang::expr(
        mutate(
          !!rlang::sym(add_suffix("mdl_fitted")),
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
#' @param config A [data.frame()] storing the specifications of analysis.
#' @param ... For extension use. Should be empty.
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
#' @param include_file_targets A logical value indicating if the targets
#'   tracking the neural data should be included.
#' @param subjs_info The expression to get the required subjects' information
#'   used to filter out corresponding data to do CPM calculations.
#' @param split_hyper A character scalar specifying the field used to split
#'   subjects. If this is specified, the subjects will be split by the values
#'   of this field, and the corresponding data will be used to do CPM
#'   calculations. Note this field must also be present in `subjs_info` and
#'   `hypers_cpm`.
#' @param covars A character vector specifying the column names of covariates
#'   to be included. See [regress_covariates()] for more details.
#' @param batches,reps The number of batches and repetitions passed to
#'   [tarchetypes::tar_map_rep()].
#' @returns A list of new target objects to calculate the permutation results.
#'   If `hypers_cpm` is the not specified or is `NULL`, a list of targets
#'   tracking neural data will be returned.
#' @export
prepare_permute_cpm2 <- function(config,
                                 ...,
                                 hypers_cpm = NULL,
                                 behav = NULL,
                                 subjs_subset = NULL,
                                 name_suffix = NULL,
                                 include_file_targets = TRUE,
                                 subjs_info = NULL,
                                 split_hyper = NULL,
                                 covars = NULL,
                                 batches = 4, reps = 5) {
  rlang::check_dots_empty()
  config_neural <- config_file_tracking(config)
  file_targets <- tarchetypes::tar_eval(
    tar_target(name, file, format = "file_fast"),
    values = config_neural
  )
  if (is.null(hypers_cpm)) {
    return(file_targets)
  }
  neural_parsed <- rlang::expr(arrow::read_feather(name))
  if (!missing(subjs_subset)) {
    subjs_subset <- rlang::enexpr(subjs_subset)
    if (!is.null(subjs_subset)) {
      neural_parsed <- rlang::expr(
        filter(!!neural_parsed, sub_id %in% !!subjs_subset)
      )
    }
  }
  behav_parsed <- rlang::enexpr(behav)
  if (!is.null(covars) || !is.null(split_hyper)) {
    stopifnot(
      !missing(subjs_info) &&
        !is.null(subjs_info <- rlang::enexpr(subjs_info))
    )
  }
  if (!is.null(covars)) {
    behav_parsed <- rlang::expr(
      mutate(
        !!behav_parsed,
        scores = map(
          scores,
          ~ regress_covariates(
            .,
            subjs_info = !!subjs_info,
            covars = !!covars,
            extracov = extracov
          )
        )
      )
    )
  }
  if (!is.null(split_hyper)) {
    neural_parsed <- rlang::expr(
      semi_join(
        !!neural_parsed,
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
  name_result_cpm <- paste(c("result_cpm", name_suffix), collapse = "_")
  name_cpm_pred <- paste(c("cpm_pred", name_suffix), collapse = "_")
  name_brain_mask <- paste(c("brain_mask", name_suffix), collapse = "_")
  list(
    if (include_file_targets) file_targets,
    tarchetypes::tar_map_rep_raw(
      name_result_cpm,
      rlang::expr(
        mutate(
          !!behav_parsed,
          cpm = map(
            scores,
            ~ do_cpm2(!!neural_parsed, ., !!!args_cpm)
          ),
          .keep = "unused"
        )
      ),
      values = tidyr::expand_grid(config_neural, hypers_cpm),
      # constructed columns should be removed
      columns = rlang::expr(-c(name, file, extracov)),
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
              names(!!substitute(config)),
              names(!!substitute(hypers_cpm))
            )
          )
        )
      ),
      deployment = "main"
    )
  )
}

#' Configure tracking of neural data files
#'
#' @param config A [data.frame()] storing the specifications of data used.
#' @returns Add two columns to `config` specifying the file name and the target
#'   name for the data.
#' @export
config_file_tracking <- function(config) {
  config |>
    dplyr::mutate(
      file = stringr::str_glue(
        fs::path(
          "data", "neural",
          "cond-{cond}_parcel-{parcel}_gsr-{gsr}_acq-{acq}_fc.arrow"
        )
      ),
      name = rlang::syms(
        stringr::str_glue("file_neural_{cond}_{parcel}_{gsr}_{acq}")
      ),
      extracov = dplyr::case_when(
        cond == "nbackrun1" ~ 1,
        stringr::str_detect(cond, "^rest(eq)?$") ~ 2,
        .default = 0
      )
    )
}
