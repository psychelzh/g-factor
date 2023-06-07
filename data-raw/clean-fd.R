library(tidyverse)
task_fd <- fs::dir_ls("data-raw/FD/run1_PowerFD") |>
  read_tsv(
    show_col_types = FALSE,
    col_names = FALSE,
    id = "file"
  )
rest_fd <- readxl::read_excel("data-raw/FD/rest_max_rms_fd_headmotion.xlsx") |>
  select(sub_id = ID, mean_fd_rest = fd_head)
task_fd |>
  mutate(sub_id = as.integer(str_extract(file, "(?<=SUB)\\d+"))) |>
  summarise(mean_fd_task = mean(X1), .by = sub_id) |>
  full_join(rest_fd, by = "sub_id") |>
  write_csv("data/subj_fd.csv")
