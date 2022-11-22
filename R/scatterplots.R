#' Script to prepare the data for analysis
#'
#' @export
scatter <- function() {
  ex <- sf::st_read(here::here("data","data-format","exposure.geojson"))
  ex$cumul <- rowSums(ex[,c(7:19,23:24), drop = TRUE], na.rm = TRUE)
  
  # Spatial figure
  dat <- ex$cumul
  x <- round((dat/max(dat)) * 100,0)
  ex$cols <- viridis::viridis(100, alpha = 0.9)[x]
  
  
  can <- pipedat:::basemap$can |>
       sf::st_make_valid()
  
  png(
    "figures/cumulative_exposure.png", 
    res = 400, width = 200, height = 200, 
    units = "mm", pointsize = 24
  )
   par(mar = c(0,0,0,0))
   plot(sf::st_geometry(can), border = "#000000")
   plot(
     sf::st_geometry(ex), col = "#00000000", 
     add = TRUE, pch = 21, bg = ex$cols, 
     cex = .3
   )
  dev.off()


  ex$cols <- viridis::viridis(3, alpha = .9)[as.numeric(as.factor(ex$type))]
  ex$cases <- ex$cases / ex$pop
  ex$deaths <- ex$deaths / ex$pop
  ex$cumul_cols <- viridis::viridis(100, alpha = .9)[100*(ex$cumul/max(ex$cumul))]
   
     mapview(ex[,"cumul"])


  ex <- sf::st_drop_geometry(ex)
  for(i in 7:21) {
    uid <- order(ex[, i])
    dat <- ex[uid,]
    plot(dat[,i], cex = 2, pch = 20, main = colnames(ex)[i], col = dat$cols)
  }

  # Cases
  for(i in 7:21) {
    plot(x = ex[,i], y = ex$cases, cex = 2, pch = 20, main = colnames(ex)[i], col = ex$cols)
  }
  plot(x = ex$cumul, y = ex$cases, cex = 2, pch = 20, main = "Cumulative vulnerability", col = ex$cols, xlab = "Cumulative vulnerability", ylab = "Cumulative cases")
  
  # Deaths
  for(i in 7:21) {
    plot(x = ex[,i], y = ex$deaths, cex = 2, pch = 20, main = colnames(ex)[i], col = ex$cols)
  }
  plot(x = ex$cumul, y = ex$deaths, cex = 2, pch = 20, main = "Cumulative vulnerability", col = ex$cols, xlab = "Cumulative vulnerability", ylab = "Cumulative deaths")
  

}