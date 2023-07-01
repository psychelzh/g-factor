tidyr::expand_grid(
  cond = c("nbackrun1", "rest", "run1rest"),
  rbind(
    data.frame(
      use_gretna = "no",
      filt = c("bandpass", "lowpass")
    ),
    data.frame(
      use_gretna = "yes",
      filt = "bandpass"
    )
  ),
  parcel = c("nn268", "Power264"),
  gsr = c("withGSR", "withoutGSR")
) |>
  dplyr::mutate(status = "todo") |>
  readr::write_csv("prepare_status.csv")
