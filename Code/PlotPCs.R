PlotPCs <- function(dataset, minPC, maxPC)
{
  longdb <- gather(dataset, "PC", "value", -Sex)
  vals   <- as.numeric(sapply(longdb$PC, function(x) {strsplit(x, "PC")[[1]][2]}))
  longdb[order(vals), ]
  
  fromPC <- min(grep(minPC, longdb$PC))
  toPC   <- max(grep(maxPC, longdb$PC))
  
  fromval <- min(longdb$value[fromPC:toPC])
  toval   <- max(longdb$value[fromPC:toPC])

  p1 <- ggplot(longdb[fromPC:toPC , ], aes(y = PC )) + 
        geom_joy(aes(x = value, fill = Sex), alpha = .6, color = "white", from = fromval, to = toval) +
        theme_joy(grid = FALSE)
  
  return(p1)
}