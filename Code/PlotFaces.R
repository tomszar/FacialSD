Plot2Faces <- function(face1, face2, facets)
{
  # Plot two faces overlaid in the same plot
  # The input needs to be two vectors with x,y.z consecutive coordinates
  
  rface1 <- matrix(face1, ncol=3, byrow=T)
  rface2 <- matrix(face2, ncol=3, byrow=T)
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  scene <- list(
    xaxis = ax, 
    yaxis = ax, 
    zaxis = ax, 
    camera = list(
      eye = list(
        x = 0, 
        y = -0.1, 
        z = 1.7)))
  
  facecolor1 <- rep("turquoise", nrow(facets)) 
  facecolor2 <- rep("wheat", nrow(facets))
  
  myPlot <- plot_ly() %>% 
    add_trace(data = rface1,  x=rface1[,1], y=rface1[,2], z=rface1[,3], i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1, 
              type="mesh3d", opacity=0.6, flatshading=F, facecolor = facecolor1,
              lightposition=list(x=100000, y=10000, z=100), lighting=list(ambient=0.6, specular=0.1, diffuse=0.5) ) %>%
    
    add_trace(data = rface2,  x=rface2[,1], y=rface2[,2], z=rface2[,3], i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1, 
              type="mesh3d", opacity=0.6, flatshading=F, facecolor = facecolor2,
              lightposition=list(x=100000, y=10000, z=100), lighting=list(ambient=0.6, specular=0.1, diffuse=0.5) ) %>%
    layout(scene = scene)
  return(myPlot)
}


Plot1Face <- function(face1, facets, colormap=NULL)
{
  # Plots one face, with the corresponding facets
  # If colormap is defined, it will be plotted
  rface1 <- matrix(face1, ncol=3, byrow=T)
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  scene <- list(
    xaxis = ax, 
    yaxis = ax, 
    zaxis = ax, 
    camera = list(
      eye = list(
        x = 0, 
        y = -0.1, 
        z = 1.7)))  
  
  if(is.null(colormap)){
    myPlot <- plot_ly(
      x = rface1[,1], y = rface1[,2], z = rface1[,3],
      i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1,
      facecolor = "seashell3", colors="seashell3", intensity=c(0,0,0), opacity = 0.8,
      type = "mesh3d"
    ) %>% 
      layout(scene = scene)
  } else {
    myPlot <- plot_ly(
      x = rface1[,1], y = rface1[,2], z = rface1[,3],
      i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1,
      colors = colorRamp(c("red", "blue")), intensity = rescale(colormap), opacity = 0.8,
      type = "mesh3d"
    ) %>% 
      layout(scene = scene)
  }
  
  return(myPlot)
}