#' Script to run a cluster analysis
#'
#' @export
make_cluster <- function() {
  # Load and prepare data
  dat <- sf::st_read("data/data-format/exposure.geojson") |>
         sf::st_drop_geometry() |>
         dplyr::select(
           employment_prox,
           pharma_prox,
           childcare_prox,
           health_prox,
           grocery_prox,
           educpri_prox,
           educsec_prox,
           library_prox,
           park_prox,
           transit_prox,
           household_not_suitable,
           major_repairs_needed,
           below_thresholds,
           cases,
           deaths,
           waste_treatment_disposal,
           water_waster_systems  
         ) |>
         as.matrix() |>
         scale()

  # ~~~~~~~~~~~~~~~~~~~ k-medoids clustering ~~~~~~~~~~~~~~~~~~~ #
  out <- here::here("data","data-clustering")
  chk_create(out)
  exist <- file.exists(here::here(out, "kmedoid.RData"))

  if (!exist) {
    library(cluster)
    # Cluster analysis
    nSample <- 100
    sampleSize <- 10000
    k <- 2:10
    kmedoid <- vector('list', length(k)+1)
    for(i in k) {
      cat("   k: ", i, "\r")
      kmedoid[[i]] <- clara(dat,
                            k = i,
                            sampsize = sampleSize,
                            samples = nSample,
                            metric="manhattan",
                            stand = TRUE,
                            keep.data = FALSE)
    save(kmedoid, file = here::here(out, "kmedoid.RData"))
    }
    kmedoid[[1]] <- NULL
    save(kmedoid, file = here::here(out, "kmedoid.RData"))

    # ~~~~~~~~~~~~~~~~~~~ Average silhouette width ~~~~~~~~~~~~~~~~~~~ #
    aswMedoid <- unlist(lapply(kmedoid, function(x) x$silinfo$avg.width))
    save(aswMedoid, file = here::here(out, "aswMedoid.RData"))
    
    # ~~~~~~~~~~~~~~~~~~~ Elbow method ~~~~~~~~~~~~~~~~~~~ #
    # Verify number of clusters by using the elbow method,
    # i.e. the inflexion point in the sum of squared error (SSE) scree plot
    # Extract within cluster sum of squared error (WSS) from each clustering run
    # Wss function
    wss <- function(x) (nrow(x)-1) * sum(apply(x,2,var))

    # Measure for each k and each cluster
    wssMedoid <- wss(dr)
    for(i in 1:length(kmedoid)) { # For each k value tested
      wssTemp <- numeric()
      for(j in 1:k[i]) { # For each cluster
        id <- kmedoid[[i]]$clustering == j # Identify observations in cluster j
        wssTemp[j] <- wss(dr[id, ])
      }
      wssMedoid[k[i]] <- sum(wssTemp)
    }
    save(wssMedoid, file = here::here(out,"wssMedoid.RData"))
  }

  # ~~~~~~~~~~~~~~~~~~~ cluster validation ~~~~~~~~~~~~~~~~~~~ #

  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  #
  # Clusters are validated visually using the Average silhouette width and the
  # Elbow method
  #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  exist <- file.exists(here::here("figures", "validation.jpg"))
  if (!exist) {
    k <- 2:10

    # ~~~~~~~~~~~~~~~~~~~ Data ~~~~~~~~~~~~~~~~~~~ #
    load(here::here(out,'aswMedoid.RData'))
    load(here::here(out,'wssMedoid.RData'))
    
    # ~~~~~~~~~~~~~~~~~~~ Figure ~~~~~~~~~~~~~~~~~~~ #
    jpeg('./figures/validation.jpg', width = 920, height = 920, res = 200, pointsize = 7)
    layout(matrix(1:2, ncol = 1))
    par(mar = c(5,5,2,1))
    # k-medoid
      # Silhouette
      plot(aswMedoid ~ k,
           pch = 20,
           ylab = 'Average Silhouette Width',
           type = 'b',
           frame = F,
           xlab = '',
           xlim = c(0,10),
           cex = 1.5,
           cex.axis = .8)
      mtext(side = 3, 'k-medoids clustering', font = 2, line = .75)

      # WSS
      plot(x = c(1,k),
           y = wssMedoid,
           type="b",
           frame = F,
           xlab="Number of Clusters (k)",
           ylab="Total within-clusters sum of squares",
           pch = 20,
           xlim = c(0,10),
           cex = 1.5,
           cex.axis = .8)
    dev.off()  
  }

  # ~~~~~~~~~~~~~~~~~~~ select cluster ~~~~~~~~~~~~~~~~~~~ #
  exist <- file.exists(here::here(out, "clusters.RData"))
  if (!exist) {
    # Number of clusters selected
    # WARNING: This is selected and adjusted manually
    k <- 6

    # Object with only selected clustering results
    clMed <- kmedoid[[k-1]]$clustering

    # Export simplified dataset
    save(k, file = here::here(out, 'k.RData'))
    save(clMed, file = here::here(out, 'clusters.RData'))
  }



  # ~~~~~~~~~~~~~~~~~~~ select cluster ~~~~~~~~~~~~~~~~~~~ #
  exist <- file.exists(here::here(out, 'simperMean.RData'))
  if (!exist) {
    # ~~~~~~~~~~~~~~~~~~~ DATA ~~~~~~~~~~~~~~~~~~~ #
    load(here::here(out, 'k.RData'))
    load(here::here(out, 'Clusters.RData'))

    # ~~~~~~~~~~~~~~~~~~~ INTER-CLUSTER SIMILARITY ~~~~~~~~~~~~~~~~~~~ #
    # ~~~~~~~~~~~~~~~~~~~ SIMPER ~~~~~~~~~~~~~~~~~~~ #
    # Run iterative simper analysis
    iter <- 300
    prop <- .05
    system.time(bootSimper <- bootstrapSimper(dr, clMed, iter = iter, prop = prop))
    save(bootSimper, file = here::here(out, 'bootSimper.RData'))

    # ~~~~~~~~~~~~~~~~~~~ SIMPER SUMMARY ~~~~~~~~~~~~~~~~~~~ #
    # Summarize in array format
    iter <- length(bootSimper) # To delete
    nList <- length(bootSimper[[1]])

    # Name of variables to extract from simper analyses
    varNames <- c('contr','sd','ava','avb')

    # Empty list to store summaries
    simperSummary <- vector('list', nList)
    names(simperSummary) <- names(bootSimper[[1]])

    # Empty arrays to store inter-cluster dissimilarity results
    for(i in 1:nList) {
      simperSummary[[i]] <- array(data = 0,
                                  dim = c(ncol(dr), length(varNames), iter),
                                  dimnames = list(drNames$accr, varNames))
    }

    # Extract summary for simper analyses interations
    for(i in 1:iter) {
      temp <- bootSimper[[i]]
      for(j in 1:nList) {
        simperSummary[[j]][,,i] <- as.matrix(temp[[j]][, varNames])
      }
    }

    # Summarize the results over all iterations
    simperMean <- vector('list', nList)
    names(simperMean) <- names(simperSummary)
    for(i in 1:nList) {
      simperMean[[i]] <- apply(X = simperSummary[[i]],
                               MARGIN = c(1,2),
                               FUN = mean,
                               na.rm = T)
    }

    # Export
    save(simperMean, file = here::here(out, 'simperMean.RData'))
    
    # ~~~~~~~~~~~~~~~~~~~ INTRA-CLUSTER SIMILARITY ~~~~~~~~~~~~~~~~~~~ #
    # As with the simper analysis, the dataset is mostly too large to use
    # regular function in packages available in R. Also, while the software
    # PRIMER automatically measures the intra-cluster similarity using the
    # Bray-Curtis similarity, `vegan::simper` does not do it.
    # I therefore create a custom function that allows me to extract the
    # intra-cluster Bray-Curtis similarity and that does it using a bootstrap
    # procedure when the cluster has too many observations.
    #
    sim <- vector('list', k)
    for(i in 1:k) {
      cat("   k: ", i, "\r")
      id <- clMed == i
      sim[[i]] <- bootIntraSim(dr[id, ], iter = 50, samp = .25)
      save(sim, file = here::here(out, 'intraSimilarity.RData'))
    }

    # Extract species, contribution and sd
    # Summarize in array format
    iter <- length(sim[[4]])
    varNames <- c('contr')
    simSummary <- vector('list', k)
    for(i in 1:k) simSummary[[i]] <- array(data = 0, dim = c(ncol(dr), 1, iter), dimnames = list(drNames$accr, varNames))

    for(i in 1:k) {
      # Check if there are multiple iterations or not.
      # If length(sim[[i]]) == 2 it means that there were no iterations and the whole cluster was parsed through the simper2 function
      # If not, then sim[[i]] should be equal to the number of iterations
      if(i %in% c(1,2,3)) {
        simSummary[[i]] <- matrix(ncol = 1,
                                  data = sim[[i]]$contr,
                                  dimnames = list(drNames$accr, 'contr'))
      } else {
        for(j in 1:iter) {
          simSummary[[i]][,'contr',j] <- sim[[i]][[j]]$contr
        }
      }
    }

    # Summarize the results over all iterations
    similarityMean <- vector('list', k)
    for(i in 1:k) similarityMean[[i]] <- apply(X = simSummary[[i]], MARGIN = c(1,2), FUN = mean, na.rm = T)

    # Export
    save(similarityMean, file = here::here(out, 'similarityMean.RData'))
  }
}
