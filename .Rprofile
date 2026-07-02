# Automatically activate renv when R starts in this directory
# This ensures reproducible package versions are used

if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
} else {
  message("Note: renv not initialized. Run source('setup_renv.R') to set up frozen dependencies.")
}
