library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_param()
  
  # Get area of interest 
  get_aoi()
  
  # Make grid 
  aoi <- sf::st_read("data/data-basemap/aoi.geojson")
  pipedat::pipegrid(aoi, cellsize = 0.01)
  
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