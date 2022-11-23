#' Script to prepare the data for analysis
#'
#' @export
scatter <- function() {
  if(!file.exists("figures")) dir.create("figures")
  ex <- sf::st_read(here::here("data","data-format","exposure.geojson"))
  ex$cases <- ex$cases / ex$pop
  ex$deaths <- ex$deaths / ex$pop
  colid <- c(7:19,23:24)
  colnm <- c(colnames(ex)[colid], "cumul")
  ex$cumul <- rowSums(ex[,colid, drop = TRUE], na.rm = TRUE)
  
  # Canada outline
  can <- pipedat:::basemap$can |>
         sf::st_make_valid()

  # =============================================================================================
  # Spatial figures
  ## Cumulative vulnerability
  dat <- ex$cumul
  x <- round((dat/max(dat)) * 100,0)
  cols <- viridis::viridis(100, alpha = 0.9)[x]  
  png(
    "figures/cumulative_vulnerability.png", 
    res = 400, width = 200, height = 200, 
    units = "mm", pointsize = 24
  )
   par(mar = c(0,0,2,0))
   plot(sf::st_geometry(can), border = "#000000", main = "Cumulative vulnerability")
   plot(
     sf::st_geometry(ex), col = "#00000000", 
     add = TRUE, pch = 21, bg = cols, 
     cex = .3
   )
  dev.off()

  # ---------------------------------------------------------------------------------------------
  ## Cumulative cases
  dat <- ex$cases
  x <- round((dat/max(dat)) * 100,0)
  cols <- viridis::magma(100, alpha = 0.9)[x]  
  png(
    "figures/cumulative_cases.png", 
    res = 400, width = 200, height = 200, 
    units = "mm", pointsize = 24
  )
   par(mar = c(0,0,2,0))
   plot(sf::st_geometry(can), border = "#000000", main = "Cumulative cases")
   plot(
     sf::st_geometry(ex), col = "#00000000", 
     add = TRUE, pch = 21, bg = cols, 
     cex = .3
   )
  dev.off()

  # ---------------------------------------------------------------------------------------------
  ## Cumulative deaths
  dat <- ex$deaths
  x <- round((dat/max(dat)) * 100,0)
  cols <- viridis::magma(100, alpha = 0.9)[x]  
  png(
    "figures/cumulative_deaths.png", 
    res = 400, width = 200, height = 200, 
    units = "mm", pointsize = 24
  )
   par(mar = c(0,0,3,0))
   plot(sf::st_geometry(can), border = "#000000", main = "Cumulative deaths")
   plot(
     sf::st_geometry(ex), col = "#00000000", 
     add = TRUE, pch = 21, bg = cols, 
     cex = .3
   )
  dev.off()

  # =============================================================================================
  # Scatterplots
  ## "Type" of communities
  dat <- sf::st_drop_geometry(ex)
  scat_plot <- function(datX, datY, datT, output, xlab, ylab) {
    png(
      glue::glue("figures/{output}.png"), 
      res = 400, width = 200, height = 200, 
      units = "mm", pointsize = 12
    )
    par(mfrow = c(2,2))
    nm <- c("Geolocated placenames", "First Nations", "Inuit communities")
    cols <- viridis::viridis(3, alpha = .4)[1:3]
    uid <- list(
      datT == nm[1],
      datT == nm[2],
      datT == nm[3]
    )
    for(j in 1:length(uid)) {
      plot(
        x = datX[uid[[j]]], y = datY[uid[[j]]], 
        cex = 1.5, pch = 21, bg = cols[j],
        main = nm[j], ylab = ylab, xlab = xlab,
        xlim = range(datX, na.rm = TRUE), 
        ylim = range(datY, na.rm = TRUE)
      )      
    }
    dev.off()
  }

  # Cases 
  for(i in colnm) {
    scat_plot(
      datX = dat[, i], 
      datY = dat$cases,
      datT = dat$type,
      output = glue::glue("{i}-cases"),
      xlab = i,
      ylab = "Cumulative COVID-19 cases"
    )
  }

  # Cases 
  for(i in colnm) {
    scat_plot(
      datX = dat[, i], 
      datY = dat$deaths,
      datT = dat$type,
      output = glue::glue("{i}-deaths"),
      xlab = i,
      ylab = "Cumulative COVID-19 deaths"
    )
  }
}