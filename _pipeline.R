library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_param()
  
  # Get area of interest 
  get_aoi()
  
  # Make grid 
  # pipedat::pipegrid(x = pipedat:::basemap$can, cellsize = 1, crs = 4326)

  # Integrate data 
  pipedat::pipeflow("./data/data-config/pipedat.yml")
  
  # Single bibtex file 
  getBib()
  
  # Prepare data 
  data_prep()
  
  # First figures
  scatter()
  
  # Report 
  suppressWarnings(bookdown::render_book("index.Rmd", "bookdown::gitbook"))
  
  # Remove figures from folders
  unlink("figures", recursive = TRUE)
}