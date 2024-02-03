# general ----
# we have 20 indicators in total
max_num_vars <- 20
# files tracking subjects ids, see `project_merge_subjects`
file_subjs_combined <- "data/subjs_combined"
file_subjs_neural <- "data/subjs_neural"

# basic configurations for CPM modeling ----
config <- tidyr::expand_grid(
  cond = c(
    "nbackrun1", "rest", "run1rest", "latent",
    "nbackrun1eq", "resteq", "run1resteq", "latenteq"
  ),
  parcel = c("nn268", "Power264"),
  gsr = c("with", "without"),
  acq = c("orig", "reg")
)
hypers_cpm <- tidyr::expand_grid(
  tibble::tibble(kfolds = 10),
  dplyr::bind_rows(
    tibble::tibble(
      thresh_method = "alpha",
      thresh_level = c(0.05, 0.01, 0.005, 0.001)
    ),
    tibble::tibble(
      thresh_method = "sparsity",
      thresh_level = c(0.01, 0.025, 0.05, 0.1)
    )
  )
)
hypers_binarize <- dplyr::bind_rows(
  data.frame(
    binarize_method = "prob",
    binarize_level = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.995)
  ),
  data.frame(
    binarize_method = "count",
    binarize_level = seq(100, 1000, 100)
  )
)

# scales for parameters ----
# scales for demographic variables
sexes <- c(M = "Male", F = "Female")
scale_sexes <- list(
  name = "Sex",
  limits = names(sexes),
  labels = sexes
)

sites <- c(BJ = "Beijing", CQ = "Chongqing")
scale_sites <- list(
  name = "Site",
  limits = names(sites),
  labels = sites
)

# scales for neural parameters
# note these are all discrete values, so `limits` is used to specify the order
conds <- c(
  nbackrun1 = "Task",
  rest = "Resting",
  latent = "Latent"
)
scale_conds <- list(
  name = "Task Condition",
  limits = names(conds),
  labels = conds
)
conds_cn <- c(
  nbackrun1 = "任务态",
  rest = "静息态",
  latent = "潜在网络"
)
scale_conds_cn <- list(
  name = "任务条件",
  limits = names(conds_cn),
  labels = conds_cn
)
parcels <- c(
  nn268 = "Shen268",
  Power264 = "Power264"
)
scale_parcels <- list(
  name = "Parcellation",
  limits = names(parcels),
  labels = parcels
)
filts <- c(
  bandpass = "Bandpass",
  lowpass = "Lowpass"
)
scale_filts <- list(
  name = "Filtering Method",
  limits = names(filts),
  labels = filts
)
gsrs <- c(
  with = "With GSR",
  without = "Without GSR"
)
scale_gsrs <- list(
  name = "Global Signal Regression",
  limits = names(gsrs),
  labels = gsrs
)
gsrs_cn <- c(
  with = "有GSR",
  without = "无GSR"
)
scale_gsrs_cn <- list(
  name = "全局信号回归",
  limits = names(gsrs),
  labels = gsrs_cn
)

# scales for CPM parameters
thresh_methods <- c(
  alpha = "Alpha",
  sparsity = "Sparsity"
)
scale_thresh_methods <- list(
  name = "Edge Threshold Method",
  limits = names(thresh_methods),
  labels = thresh_methods
)
thresh_methods_cn <- c(
  alpha = "置信水平",
  sparsity = "稀疏度"
)
scale_thresh_methods_cn <- list(
  name = "阈值方法",
  limits = names(thresh_methods),
  labels = thresh_methods_cn
)
model_types <- c(
  all = "Combined Model",
  pos = "Pos-Cor Networks",
  neg = "Anti-Cor Networks"
)
scale_model_types <- list(
  name = "Model Type",
  limits = names(model_types),
  labels = model_types
)

# scales for behavioral parameters
meas_trait <- c(
  spearman = "GCA-S",
  bifac = "GCA-B",
  rapm = "RAPM"
)
color_traits <- c(
  spearman = "#1b9e77",
  bifac = "#d95f02",
  rapm = "#7570b3"
)
lty_traits <- c(
  spearman = "dashed",
  bifac = "dotted",
  rapm = "dotdash"
)
scale_meas_trait <- function(index = NULL) {
  index <- index %||% seq_along(meas_trait)
  meas_trait <- meas_trait[index]
  list(
    name = "Trait Measure",
    limits = names(meas_trait),
    labels = meas_trait
  )
}

# scales for binarize parameters
binarize_methods <- c(
  prob = "Minimal Proportion",
  count = "Top N Edges"
)
scale_binarize_methods <- list(
  name = "Network Binarize Method",
  limits = names(binarize_methods),
  labels = binarize_methods
)

# meta data for targets projects ----
get_store_path <- function(project) {
  here::here() |>
    withr::with_dir(
      targets::tar_config_get("store", project = project)
    ) |>
    here::here()
}
store_preproc_behav <- get_store_path("project_preproc_behav")
store_preproc_neural <- get_store_path("project_preproc_neural")
store_merge_subjects <- get_store_path("project_merge_subjects")
store_cpm_bench <- get_store_path("project_cpm_bench")
store_cpm_main <- get_store_path("project_cpm_main")
store_cpm_tasksel <- get_store_path("project_cpm_tasksel")
store_gca_numtasks <- get_store_path("project_gca_numtasks")
store_gca_tasksel <- get_store_path("project_gca_tasksel")
