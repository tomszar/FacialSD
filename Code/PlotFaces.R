Plot2Faces <- function(face1, face2, facets, title = NULL )
{
  # Plot two faces overlaid in the same plot
  # The input needs to be two vectors with x,y.z consecutive coordinates
  # and the faces as a matrix
  
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
              type="mesh3d", opacity=0.5, flatshading=F, facecolor = facecolor1,
              lightposition=list(x=100000, y=10000, z=100), lighting=list(ambient=0.6, specular=0.1, diffuse=0.5) ) %>%
    
    add_trace(data = rface2,  x=rface2[,1], y=rface2[,2], z=rface2[,3], i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1, 
              type="mesh3d", opacity=0.5, flatshading=F, facecolor = facecolor2,
              lightposition=list(x=100000, y=10000, z=100), lighting=list(ambient=0.6, specular=0.1, diffuse=0.5) ) %>%
    layout(scene = scene, title = title)
  return(myPlot)
}


Plot1Face <- function(face1, facets, colormap=NULL, title=NULL)
{
  # Plots one face, with the corresponding facets
  # If colormap is defined, it will be plotted
  require(grDevices)
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
    facecolor1 <- rep("turquoise", nrow(facets)) 
    myPlot <- plot_ly(
      x = rface1[,1], y = rface1[,2], z = rface1[,3],
      i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1,
      facecolor = facecolor1, opacity = 0.8,
      type = "mesh3d"
    ) %>% 
      layout(scene = scene, title = title)
  } else {

    myPlot <- plot_ly(
      x = rface1[,1], y = rface1[,2], z = rface1[,3],
      i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1,
      #colorscale = list(c(0, 'rgb(255, 0, 0)'),
      #                  c(0.5, 'rgb(0, 255, 0)'),
      #                  c(1, 'rgb(0, 0, 255)')),
      colors = colorRamp(c("yellow", "red", "black")), 
      intensity = colormap, opacity = 1,
      type = "mesh3d"
    ) %>% 
      layout(scene = scene, title = title)
  }
  #colorRamp(c("white", "black"))
  #scales::rescale(colormap)
  return(myPlot)
}