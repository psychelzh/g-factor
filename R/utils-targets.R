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

tar_fact_perm_cpm <- function(name, behav, hypers_cpm,
                              fc_data = NULL, store_fc_data = NULL,
                              batches = 4, reps = 5) {
  rlang::check_exclusive(fc_data, store_fc_data, .require = TRUE)
  name <- deparse1(substitute(name))
  if (is.null(fc_data)) {
    stopifnot(all(rlang::has_name(hypers_cpm, c("modal", "parcel", "gsr"))))
    fc_data <- substitute(
      qs::qread(
        fs::path(
          store_fc_data,
          sprintf(
            "fc_data_%s_%s_%s",
            modal, parcel, gsr
          )
        )
      )
    )
  }
  tarchetypes::tar_map_rep_raw(
    name,
    behav |>
      mutate(
        cpm = map(
          scores,
          ~ do_cpm2(
            fc_data,
            .,
            thresh_method = thresh_method,
            thresh_level = thresh_level
          )
        ),
        .keep = "unused"
      ) |>
      substitute(),
    values = hypers_cpm,
    batches = batches,
    reps = reps
  )
}

tar_load_here <- function(name) {
  name <- deparse1(substitute(name))
  assign(
    name,
    tar_read_raw_here(name),
    envir = parent.frame()
  )
}

tar_read_here <- function(name) {
  tar_read_raw_here(deparse1(substitute(name)))
}

tar_read_raw_here <- function(name) {
  withr::with_dir(here::here(), targets::tar_read_raw(name))
}
