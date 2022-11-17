#' Script to prepare the data for analysis
#'
#' @export
scatter <- function() {
  ex <- sf::st_read(here::here("data","data-format","exposure.geojson"))
  ex$cols <- viridis::viridis(3, alpha = .9)[as.numeric(as.factor(ex$type))]
  ex <- sf::st_drop_geometry(ex)
  ex$cases <- ex$cases / ex$pop
  ex$deaths <- ex$deaths / ex$pop
  ex$cumul <- rowSums(ex[,7:21])
  
  for(i in 7:21) {
    uid <- order(ex[, i])
    dat <- ex[uid,]
    plot(dat[,i], cex = 2, pch = 20, main = colnames(ex)[i], col = dat$cols)
  }

  # Cases
  for(i in 7:21) {
    plot(x = ex[,i], y = ex$cases, cex = 2, pch = 20, main = colnames(ex)[i], col = ex$cols)
  }
  plot(x = ex$cumul, y = ex$cases, cex = 2, pch = 20, main = colnames(ex)[i], col = ex$cols)
  
  # Deaths
  for(i in 7:21) {
    plot(x = ex[,i], y = ex$cases, cex = 2, pch = 20, main = colnames(ex)[i], col = ex$cols)
  }
  plot(x = ex$cumul, y = ex$cases, cex = 2, pch = 20, main = colnames(ex)[i], col = ex$cols)
  

}