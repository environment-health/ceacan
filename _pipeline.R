library(devtools)
load_all()
library(pipefn)

pipeline <- function() {
  # Get area of interest 
  get_aoi()
  
  # Get and format data
  pipeflow()
    
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