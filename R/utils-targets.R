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
