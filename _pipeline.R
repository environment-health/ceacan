library(devtools)
load_all()
library(pipedat)

pipeline <- function() {
  # Get area of interest 
  get_aoi("canadian_eez")
  
  # Get and format data
  pipeflow()
  
  # Report 
  render_report()
}