Plot2Faces <- function(face1, face2, facets, title = NULL )
{
  # Plot two faces overlaid in the same plot
  # Face1 and face2 are vectors with x,y.z consecutive coordinates.
  # Facets is a matrix with the surface information.
  
  require(plotly)
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
              lightposition=list(x=100000, y=10000, z=100), lighting=list(ambient=0.6, specular=0.1, diffuse=0.5),
              hoverinfo = "none") %>%
    
    add_trace(data = rface2,  x=rface2[,1], y=rface2[,2], z=rface2[,3], i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1, 
              type="mesh3d", opacity=0.5, flatshading=F, facecolor = facecolor2,
              lightposition=list(x=100000, y=10000, z=100), lighting=list(ambient=0.6, specular=0.1, diffuse=0.5),
              hoverinfo = "none") %>%
    layout(scene = scene, title = title)
  return(myPlot)
}

Plot1Face <- function(face1, facets, colormap=NULL, title=NULL)
{
  # Plot one face, with the corresponding facets and colormap if given.
  # face1 is a numeric vector of length n with consecutive x,y,z coordiantes of n/3 landmarks
  # facets is a matrix with surface information
  # Colormap is a numeric vector of size n/3 with values to be mapped onto face1. 
  # Note that colormap needs to be in range 0 to 1
  
  #require(grDevices)
  if(!require(plotly)){ #Loading packages
    install.packages("plotly")
    library(plotly)
  }
  rface1 <- matrix(face1, ncol=3, byrow=T)
  
  ax <- list( #Creating axes
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  scene <- list( #Creating scene
    xaxis = ax, 
    yaxis = ax, 
    zaxis = ax, 
    camera = list( #Change the position of camera
      eye = list(
        x = 0, 
        y = -0.1, 
        z = 1.7),
      center = list(
        x = 0,
        y = 0,
        z = 0),
      up = list(
        x = 0,
        y = 1,
        z = 0)
      )
  )
  
  if(is.null(colormap)){
    facecolor1 <- rep("lightgray", nrow(facets))
    myPlot <- plot_ly(
      x = rface1[,1], y = rface1[,2], z = rface1[,3],
      i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1,
      facecolor = facecolor1, opacity = 1,
      hoverinfo = "none",
      lighting = list(specular=0.05, ambient=0.3, diffuse=0.8, fresnel=0.1, roughness=0.1),  
      lightposition = list(x = 100, y = 200, z = 0), 
      type = "mesh3d"
    ) %>% 
      layout(scene = scene, title = title)
  } else {

    myPlot <- plot_ly(
      x = rface1[,1], y = rface1[,2], z = rface1[,3],
      i = facets[, 1]-1, j = facets[, 2]-1, k = facets[, 3]-1,
      #color = colormap, 
      cmax = 1, cmin = 0, 
      #colorscale = list(c(0, 'rgb(255, 0, 0)'),
      #                  c(0.5, 'rgb(0, 255, 0)'),
      #                  c(1, 'rgb(0, 0, 255)')),
      colors = colorRamp(c("yellow", "red", "black")), #Colormap colors
      intensity = colormap, 
      opacity = 1,
      hoverinfo = "none",
      flatshading = TRUE,
      type = "mesh3d"
    ) %>% 
      layout(scene = scene, title = title)
  }
  #colorRamp(c("white", "black"))
  #scales::rescale(colormap)
  return(myPlot)
}

PlotMultipleFaces <- function(face1, facets, distances)
{
  # Plots repeteadly the same face1 with different distances values mapped onto the face.
  # Face1 is a vector of size n with the x,y,z coordiantes, and facets is the matrix with
  # surface information.
  # Distances is a matrix size n/3, m with the values to be mapped onto the faces
  
  require(plotly)
  
  nfaces <- ncol(distances)
  ns     <- 1:nfaces
  scenes <- paste("scene", ns, sep="")
  plots  <- list()
  
  for( i in 1:nfaces)
  {
    plots[[i]] <- Plot1Face(face1, facets, colormap=distances[,i])
    plots[[i]]$x$layoutAttrs      <- NULL
    plots[[i]]$x$attrs[[1]]$scene <- scenes[i]
  }
  
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
        z = 3.0)))
  
  mysubplot <- subplot(plots) 
  
  if(nfaces > 4)
  {
    y1 <- c(0, 0.5)
    y2 <- c(0.5, 1)
    cubs  <- (nfaces %/% 2) + (nfaces %% 2)
    cuads <- round(seq(0, 1, 1 / cubs), digits = 2)
    x     <- list()
    for(i in 1:cubs)
    {
      x[[i]] <- c(cuads[[i]], cuads[[i+1]])
    }
    domain <- list()
    for(i in 1:nfaces)
    {
      if(i <= cubs)
      {
        domain[[i]] <- list(x = unlist(x[i]), y = y1)
      } else{
        domain[[i]] <- list(x = unlist(x[i-3]), y = y2)
      }
    }
  }
    
  
  for (i in 1:nfaces)
  {
    if(nfaces > 4)
    {
      scene$domain <- domain[[i]]
      myoptions    <- paste(scenes[[i]], "= scene", sep = " ")
      mysubplot    <- eval(parse(text = paste("layout(mysubplot,", myoptions, ")")))
      mysubplot
    }
    myoptions <- paste(scenes[[i]], "= scene", sep = " ")
    mysubplot <- eval(parse(text = paste("layout(mysubplot,", myoptions, ")")))
    mysubplot
  }
  
  return(mysubplot)
}

PlotComparisonFaces <- function(faces, facets){
  #function that creates a plot with comparisons of two faces,
  #and the euclidean distance map on a third column
  source("Distances.R")
  require(plotly)
  
  nfaces <- ncol(faces)
  ns     <- 1:(nfaces + nfaces/2)
  scenes <- paste("scene", ns, sep="")
  plots  <- list()
  distances <- list()
  el        <- 1
  range01   <- function(x){(x-min(x))/(max(x)-min(x))}
  for(i in seq(1,nfaces, 2)){
    distances[el]  <- getDistance(faces[,i], faces[,i+1])
    el <- el + 1
  }
  
  for(i in 1:length(distances)){
    distances[[i]] <- range01(distances[[i]])
  }
  
  for(i in 1:nfaces){
    plots[[i]] <- Plot1Face(faces[,i], facets)
    plots[[i]]$x$layoutAttrs      <- NULL
    plots[[i]]$x$attrs[[1]]$scene <- scenes[i]
  }
  
  el  <- nfaces + 1
  dis <- 1
  for(i in seq(1,nfaces, 2)){
    plots[[el]] <- Plot1Face(faces[,i], facets, colormap = c(distances[[dis]]))
    plots[[el]]$x$layoutAttrs      <- NULL
    plots[[el]]$x$attrs[[1]]$scene <- scenes[el]
    el  <- el +1
    dis <- dis+1
  }
  
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
        z = 3.0)))
  
  mysubplot <- subplot(plots) 
  
  ylims <- round(seq(0, 1, 1 / (nfaces/2)), digits = 2)
  x1 <- c(0, 0.33)
  x2 <- c(0.33, 0.66)
  x3 <- c(0.66, 1)
  ys <- list()
  for(i in 1:(nfaces/2)){
    ys[[i]] <- c(ylims[[i]], ylims[[i+1]])
  }
  
  domain <- list()
  coun   <- 1
  for(i in seq(1,nfaces,2)){
      domain[[i]] <- list(x = x1, y = unlist(ys[coun]))
      coun <- coun + 1
  }
  
  coun <- 1
  for(i in seq(2,nfaces,2)){
    domain[[i]] <- list(x = x2, y = unlist(ys[coun]))
    coun <- coun + 1
  }
  
  coun <- 1
  for(i in (nfaces+1):length(plots)){
    domain[[i]] <- list(x = x3, y = unlist(ys[coun]))
    coun <- coun + 1
  }
  
  for(i in ns){
    scene$domain <- domain[[i]]
    myoptions    <- paste(scenes[[i]], "= scene", sep = " ")
    mysubplot    <- eval(parse(text = paste("layout(mysubplot,", myoptions, ")")))
    mysubplot
  }  
  
  return(mysubplot)
}
  
SaveObj <- function(landmarks, facets, filename){
  facets <- as.data.frame(facets)
  face   <- matrix(landmarks, ncol=3, byrow=T)
  rows   <- nrow(facets) + nrow(face)
  cols   <- 4
  obj    <- as.data.frame(matrix(0, ncol = cols, nrow = rows))
  obj[1:nrow(face),2:4] <- face
  obj[1:nrow(face),1]   <- "v"
  
  obj[(nrow(face)+1): nrow(obj),2:4] <- as.data.frame(facets)
  obj[(nrow(face)+1): nrow(obj),1]   <- "f"
  
  write.table(obj, file = filename, quote = F, row.names = F, col.names = F)
}