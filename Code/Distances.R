getDistance <- function(face1, face2)
{
 # Compute the euclidean distance between two 3d coordiantes vector
  rface1 <- matrix(face1, ncol=3, byrow=T)
  rface2 <- matrix(face2, ncol=3, byrow=T)
  
  eucdist = data.frame(dist=nrow(rface1))
  for(row in 1:nrow(rface1)){
    d = sqrt( (rface1[row,1] - rface2[row,1])^2 + (rface1[row,2]- rface2[row,2])^2 + (rface1[row,3]- rface2[row,3])^2 )
    eucdist[row,] = d
  }
  
  return(eucdist)
}

getAngle <- function(x1, x2)
{
  #Computes the angle between two vectors, x1 and x2
  
  x1 <- as.numeric(x1)
  x2 <- as.numeric(x2)
  dot.prod <- x1 %*% x2 
  norm.x1  <- norm(x1, type="2")
  norm.x2  <- norm(x2, type="2")
  theta    <- acos(dot.prod / (norm.x1 * norm.x2))
  theta    <- round(as.numeric(theta), digits = 4)
  return(theta)
}

getPAngle <- function(x)
{
  #Computes pairwise angles from matrix x,
  #were each row is a vector
  
  x <- as.matrix(x)
  ptheta <- matrix(ncol = nrow(x), nrow = nrow(x))
  for(r in 1:nrow(x))
  {
    for(r2 in 1:nrow(x))
    {
      angle <- getAngle(x[r,], x[r2,])
      ptheta[r,r2] <- angle
    }
    
  }
  ptheta[is.nan(ptheta)] <- 0
  return(ptheta)
}