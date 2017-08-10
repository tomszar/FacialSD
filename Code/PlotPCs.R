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

plotvect <- function(matrix, factors, vectors, x1, y1, scale=3)
{
  #Plot of vector as arrows with the scatter plot as background
  #You can define how much to scale vectors with scale
  p1 <- ggplot(matrix, aes_string(x = x1, y = y1)) +
            geom_point(alpha = 0.05) + 
            geom_segment(data = vectors, 
                                aes_string(x = 0, y = 0,
                                xend = paste(x1, "*",scale, sep = ""), 
                                yend = paste(y1, "*",scale, sep = ""),
                                color = "factors"), 
                         arrow = arrow(length = unit(0.03, "npc")), size = 1) +
            scale_color_discrete(name = "Groups") + 
            theme_pubr()
  p1 <- ggpar(p1, palette = "jama")
  return(p1)
  
}