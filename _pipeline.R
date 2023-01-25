library(devtools)
load_all()
library(covidfn)

pipeline <- function() {
  # Update global parameters
  global_param()
  
  # Get area of interest 
  get_aoi()
  
  # Integrate data 
  pipeflow("./data/data-config/pipedat.yml")
  
  # Single bibtex file 
  getBib()
  
  # Prepare data 
  data_prep()
  
  # Cluster analysis
  make_cluster()
  
  # First figures
  scatter()
  
  # Report 
  suppressWarnings(bookdown::render_book("index.Rmd", "bookdown::gitbook"))
  
  # Remove figures from folders
  unlink("figures", recursive = TRUE)
}