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
