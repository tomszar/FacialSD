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

getGDistance <- function(matrix, grouping)
{
  #Compute the euclidean distance between groups 
  
  require(dplyr)
  averages   <- matrix %>% group_by(as.character(grouping)) %>% summarise_all(funs(mean))
  distmatrix <- dist(averages[,2:ncol(averages)])
  return(distmatrix)
  
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

getPAngle <- function(X)
{
  #Computes pairwise angles from matrix X,
  #were each row is a vector
  
  X <- as.matrix(X)
  ptheta <- matrix(ncol = nrow(X), nrow = nrow(X))
  for(r in 1:nrow(X))
  {
    for(r2 in 1:nrow(X))
    {
      angle <- getAngle(X[r,], X[r2,])
      ptheta[r,r2] <- angle
    }
    
  }
  ptheta[is.nan(ptheta)] <- 0
  return(ptheta)
}

getCosine <- function(x1, x2)
{
  #Computes the cosine similarity between two vectors
  x1 <- as.numeric(x1)
  x2 <- as.numeric(x2)
  cosine <- sum(x1*x2)/sqrt(sum(x1^2)*sum(x2^2))
  cosine  <- round(as.numeric(cosine), digits = 4)
  return(cosine)
}

getPCosine <- function(X)
{
  #Computes pairwise cosine similarities from matrix X
  #were each row is a vector
  
  X <- as.matrix(X)
  pcos <- matrix(ncol = nrow(X), nrow = nrow(X))
  for(r in 1:nrow(X))
  {
    for(r2 in 1:nrow(X))
    {
      cos <- getCosine(X[r,], X[r2,])
      pcos[r,r2] <- cos
    }
    
  }
  pcos[is.nan(pcos)] <- 0
  return(pcos)
}

getAngleperm <- function(matrix, factor, groups, perm = 1000)
{
  #Computes a null distribution of angles to test the significance of groups
  #Matrix is the set of numerical values, one observation per row.
  #Factor is the grouping factor to obtain the vector.
  #Groups is the grouping factor to compare the angles.
  
  row     <- 1
  ngroups <- length(levels(groups))
  matrix  <- as.data.frame(matrix)
  nullmatrix <- matrix(ncol = perm, nrow = (ngroups * (ngroups-1) /2) )
  set.seed(10)
  for(r in 1:(ngroups-1))
  {
    for(r2 in 2:ngroups)
    {
      if(r < r2){
      logic     <- (groups == levels(groups)[r] | groups == levels(groups)[r2])
      submatrix <- dplyr::filter(matrix, logic)
      subgroups <- groups[logic]
      subfactor <- factor[logic]
      
      nullvals <- replicate(perm,
                            {
                              resgroups <- sample(subgroups)
                              averages  <- submatrix %>% group_by(interaction(subfactor, resgroups)) %>% 
                                               summarise_all(funs(mean(., na.rm=T)))
                              vector1   <- averages[1, 2:ncol(averages)] - averages[2, 2:ncol(averages)]
                              vector2   <- averages[3, 2:ncol(averages)] - averages[4, 2:ncol(averages)]
                              return(getAngle(vector1, vector2))
                            }
      )
      nullmatrix[row, ] <- nullvals
      row <- row + 1
      }
    }
  }
  return(nullmatrix)
}