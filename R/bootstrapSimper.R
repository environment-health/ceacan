#' Function to run an iterative SIMPER analysis 
#'
#' @export

# iter <- 10
# prop <- .02 # .05: ~500s/iter | .01: ~20s/iter | .02: ~80s/iter
# The analysis being very long, we will proceed using a
# bootstrap procedure, i.e. iterative simper analysis
bootstrapSimper <- function(x, cl, iter = 100, prop = .1, accr = '') {
  # List to store simper results for post-processing
  bootSimper <- vector('list', iter)

  # Determine the number of data points to sample from each cluster
    clSub <- ceiling(table(cl) * prop)

  # Repeat the process for the desired number of iterations
  for(i in 1:iter) {
    cat("   i: ", i, ' of ', iter, "\r")

    # Subsampled dataset
    samp <- vector('list', length(clSub))
    for(j in 1:length(clSub)) samp[[j]] <- sample(x = which(cl == j), size = clSub[j])
    samp <- unlist(samp)

    # Run simper analysis on subset of data
    bootSimper[[i]] <- simperMan(x[samp, ], cl[samp])

    # Save file
    save(bootSimper, file = paste0('./data/bootSimper', accr, '.RData'))
  }
  return(bootSimper)
}
