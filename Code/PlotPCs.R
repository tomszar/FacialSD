PlotPCs <- function(dataset, group, minPC, maxPC)
{
  # Function that will plot joy plots according to grouping factor
  # from a minPC to maxPC
  # Dataset must contain only PC values, or numeric
  
  require(tidyr)
  require(ggplot2)
  require(ggjoy)
  
  newdat       <- cbind(group, dataset)
  newdat$group <- as.factor(newdat$group)
  longdb <- gather(newdat, "PC", "value", -group)
  vals   <- as.numeric(sapply(longdb$PC, function(x) {strsplit(x, "PC")[[1]][2]}))
  longdb[order(vals), ]
  
  fromPC <- min(grep(minPC, longdb$PC))
  toPC   <- max(grep(maxPC, longdb$PC))
  
  #fromval <- min(longdb$value[fromPC:toPC])
  #toval   <- max(longdb$value[fromPC:toPC])
  
  fromval <- mean(longdb$value[fromPC:toPC]) - 2*sd(longdb$value[fromPC:toPC])
  toval   <- mean(longdb$value[fromPC:toPC]) + 2*sd(longdb$value[fromPC:toPC])

  p1 <- ggplot(longdb[fromPC:toPC , ], aes(y = PC )) + 
        geom_joy(aes(x = value, fill = group), alpha = .2, color = "white", from = fromval, to = toval) +
        theme_joy(grid = FALSE)
  p1 <- ggpar(p1, palette = "jco")
  
  return(p1)
}