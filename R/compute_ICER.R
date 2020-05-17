
#
compute_ICER <- function(delta.e, delta.c) {
  
  ##TODO: how doees n.comparitors relate to delta?
  
  if(n.comparisons == 1) {
    ICER <- mean(delta.c)/mean(delta.e)
  }
  if(n.comparisons > 1) {
    ICER <- colMeans(delta.c)/colMeans(delta.e) #apply(delta.c,2,mean)/apply(delta.e,2,mean)
  }
  
  ICER
}

