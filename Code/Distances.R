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