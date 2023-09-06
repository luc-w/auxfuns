# plot results from lsem 
# v0.5

plot_lsem <- function(lsemmodel, parameter, ylim = c(NULL, NULL)){
  
  # compute effective age at focal point
  eff <- NULL
  for(i in 1:ncol(lsemmodel$weights)){
    eff[i] <- weighted.mean(x= t(lsemmodel$data[lsemmodel$moderator]), w = lsemmodel$weights[,i])
  }
  
  # subset focal age points to effective age in lsem results
  lsemmodel$parameters$moderator <- eff
  
  # plot
  lsemmodel$parameters %>%
  filter(par == parameter) %>%
  ggplot(., aes(x=moderator, y = est)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper), alpha = 0.2) +
    coord_cartesian(ylim = ylim) +
    ylab(parameter)
}

