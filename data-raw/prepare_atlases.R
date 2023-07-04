box::use(dm)
data_power <- lapply(
  1:2,
  \(sheet) {
    readxl::read_excel(
      "data-raw/atlases/Power2011ROI_Module-2hv15xc-clean.xlsx",
      sheet = sheet
    )
  }
) |>
  setNames(c("roi", "network"))
atlas_power <- dm$new_dm(data_power) |>
  dm$dm_add_pk("roi", "ROI.name") |>
  dm$dm_add_pk("network", "network") |>
  dm$dm_add_fk("roi", "network", "network", "network")
saveRDS(atlas_power, "data/atlases/power264.rds")

data_shen <- lapply(
  1:2,
  \(sheet) {
    readxl::read_excel(
      "data-raw/atlases/Shen268_10network.xlsx",
      sheet = sheet
    )
  }
) |>
  setNames(c("roi", "network"))
atlas_shen <- dm$new_dm(data_shen) |>
  dm$dm_add_pk("roi", "ROI.name") |>
  dm$dm_add_pk("network", "network") |>
  dm$dm_add_fk("roi", "network", "network", "network")
saveRDS(atlas_shen, "data/atlases/shen268.rds")
