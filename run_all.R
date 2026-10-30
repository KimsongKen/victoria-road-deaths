# Run the whole pipeline and render the report.
#   Rscript run_all.R            (from the project folder)
# Set CRASH_DATA_DIR if the CSVs are not in ../data/data.

steps <- c("R/01_prepare.R", "R/02_cluster.R", "R/03_compare_dca.R", "R/04_profile_stories.R")
for (s in steps) {
  cat("\n==========", s, "==========\n")
  source(s, local = new.env(), echo = FALSE)
}

# rmarkdown needs pandoc; RStudio ships one, so point to it if none is on PATH.
if (!rmarkdown::pandoc_available()) {
  candidates <- c("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
                  "C:/Program Files/RStudio/resources/app/bin/pandoc",
                  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")
  hit <- candidates[file.exists(file.path(candidates, c("pandoc.exe", "pandoc")[1 + (.Platform$OS.type != "windows")]))]
  if (length(hit)) Sys.setenv(RSTUDIO_PANDOC = hit[1])
}
if (rmarkdown::pandoc_available()) {
  cat("\n========== Rendering report.Rmd ==========\n")
  # Render beside report.Rmd so figure paths resolve, then move the self-contained HTML.
  rmarkdown::render("report.Rmd", quiet = TRUE)
  file.rename("report.html", file.path("output", "report.html"))
  cat("Report: output/report.html\n")
} else {
  message("pandoc not found: open report.Rmd in RStudio and click Knit.")
}
