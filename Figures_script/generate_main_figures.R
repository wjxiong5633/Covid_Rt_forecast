#!/usr/bin/env Rscript

# Generate all main-text figures produced in R.
# Figure 2 is an externally prepared workflow diagram and is not generated here.

required_files <- c(
  "data/sim_data1.rds",
  "data/sim_rt1.rds",
  "Figures_script/Figure1_sim.R",
  "Figures/Figure2_diagram.png",
  "Figures_script/Figure3_wave56.R",
  "Figures_script/Figure4_case_error.R",
  "Figures_script/Figure5_rt_error.R",
  "Figures_script/Figure6_ensemble.R"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Run this script from the repository root. Missing files: ",
    paste(missing_files, collapse = ", ")
  )
}

dir.create("figures_clean", showWarnings = FALSE, recursive = TRUE)

# Run each figure in a clean R session. Several legacy scripts use both plyr
# and dplyr, so sourcing them all into one session can change function dispatch.
rscript <- file.path(R.home("bin"), "Rscript.exe")
if (!file.exists(rscript)) {
  rscript <- file.path(R.home("bin"), "Rscript")
}

figure_scripts <- c(
  "Figure 1" = "Figures_script/Figure1_sim.R",
  "Figure 3" = "Figures_script/Figure3_wave56.R",
  "Figure 4" = "Figures_script/Figure4_case_error.R",
  "Figure 5" = "Figures_script/Figure5_rt_error.R",
  "Figure 6" = "Figures_script/Figure6_ensemble.R"
)

for (figure_name in names(figure_scripts)) {
  message("Generating ", figure_name)
  status <- system2(rscript, figure_scripts[[figure_name]])
  if (!identical(status, 0L)) {
    stop(figure_name, " failed with exit status ", status)
  }
}

message("Copying Figure 2 diagram")
if (!file.copy(
  "Figures/Figure2_diagram.png",
  "figures_clean/Figure2_diagram.png",
  overwrite = TRUE
)) {
  stop("Unable to copy Figure 2 diagram to figures_clean/.")
}

expected_outputs <- file.path(
  "figures_clean",
  c(
    "Figure1_sim.png",
    "Figure2_diagram.png",
    "Figure3_wave5to6.png",
    "Figure4_case_error.png",
    "Figure5_rt_error.png",
    "Figure6_ensemble.png"
  )
)

missing_outputs <- expected_outputs[!file.exists(expected_outputs)]
if (length(missing_outputs) > 0) {
  stop("Figure generation did not create: ", paste(missing_outputs, collapse = ", "))
}

message("Successfully generated Figure 1 and Figures 3-6 in figures_clean/.")
