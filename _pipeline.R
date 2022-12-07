library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_param()
  
  # Get area of interest 
  get_aoi()
  
  # Integrate data 
  pipedat::pipeflow("./data/data-config/pipedat.yml")
  
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