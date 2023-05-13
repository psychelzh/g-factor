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

#' Target factory for CPM permutation
#'
#' This will generate batches of CPM permutation for targets to use.
#'
#' @param name The name for the target. Should be a symbol.
#' @param behav The name for the behavioral data. Should be a symbol.
#' @param hypers A [data.frame()] storing the hyper parameters passed to the
#'   `values` argument of [tarchetypes::tar_map_rep()]. Currently
#'   `"thresh_method"` and `"thresh_level"` are required.
#' @param neural,store_neural One and only one of the first two should be
#'   specified. `neural` must be a symbol referring to the brain data, whereas
#'   `store_neural` must be a character scalar referring to the storage path of
#'   the brain data, in this way, the `"modal"`, `"parcel"` and `"gsr"` fields
#'   must be present in `hypers` to specify which neural data to use.
#' @param split_hyper,subjs_info These two must be specied simultaneously.
#'   `split_hyper` specifies the field used to split neural data to perform
#'   different CPM calculations, e.g., different gender. Note this field must be
#'   present in both `hypers` and `subjs_info`.
#' @param batches,reps The number of batches and repetitions passed to
#'   [tarchetypes::tar_map_rep()].
#' @returns A new target object to calculate the permutation results.
permute_cpm <- function(name, behav, neural, hypers,
                        store_neural = NULL,
                        split_hyper = NULL,
                        subjs_info = NULL,
                        batches = 4, reps = 5) {
  rlang::check_exclusive(neural, store_neural, .require = TRUE)
  name <- deparse1(substitute(name))
  if (missing(neural)) {
    stopifnot(all(rlang::has_name(hypers, c("modal", "parcel", "gsr"))))
    neural <- store_neural |>
      fs::path(sprintf("fc_data_%s_%s_%s", modal, parcel, gsr)) |>
      qs::qread() |>
      substitute()
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
  tarchetypes::tar_map_rep_raw(
    name,
    behav |>
      mutate(
        cpm = map(
          scores,
          ~ do_cpm2(
            neural,
            .,
            thresh_method = thresh_method,
            thresh_level = thresh_level
          )
        ),
        .keep = "unused"
      ) |>
      substitute(),
    values = hypers,
    batches = batches,
    reps = reps
  )
}
