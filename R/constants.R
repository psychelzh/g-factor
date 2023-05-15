sexes <- c(M = "Male", F = "Female")
scale_sexes <- list(
  name = "Sex",
  limits = names(sexes),
  labels = sexes
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

thresh_methods <- c(
  alpha = "Alpha",
  sparsity = "Sparsity"
)
scale_thresh_methods <- list(
  name = "Edge Threshold Method",
  limits = names(thresh_methods),
  labels = thresh_methods
)

edge_types <- c(
  all = "Combined",
  neg = "Anti-Corr Networks",
  pos = "Pos-Corr Networks"
)
scale_edge_types <- list(
  name = "Edge Type",
  limits = names(edge_types),
  labels = edge_types
)

modalities <- c(
  run1rest = "Combined",
  nbackfull = "Task",
  rest = "Resting"
)
scale_modalities <- list(
  name = "Modality",
  limits = names(modalities),
  labels = modalities
)

meas_behav <- c(
  g_full = "G-Factor",
  rapm = "RAPM"
)
scale_meas_behv <- list(
  name = "Behavioral Measure",
  limits = names(meas_behav),
  labels = meas_behav
)
