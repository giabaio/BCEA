#####multi.ce##################################################################################################
multi.ce <- function(he){
  # Cost-effectiveness analysis for multiple comparison 
  # Identifies the probability that each comparator is the most cost-effective as well as the
  # cost-effectiveness acceptability frontier
  cl <- colors()
  # choose colors on gray scale
  color <- cl[floor(seq(262,340,length.out=he$n.comparators))]	
  
  rank <- most.ce <- array(NA,c(he$n.sim,length(he$k),he$n.comparators))
  for (t in 1:he$n.comparators) {
    for (j in 1:length(he$k)) {
      rank[,j,t] <- apply(he$U[,j,]<=he$U[,j,t],1,sum)
      most.ce[,j,t] <- rank[,j,t]==he$n.comparators
    }
  }
  m.ce <- apply(most.ce,c(2,3),mean)		# Probability most cost-effective
  ceaf <- apply(m.ce,1,max)			# Cost-effectiveness acceptability frontier
  
  # Output of the function
  list(
    m.ce=m.ce,ceaf=ceaf,n.comparators=he$n.comparators,k=he$k,interventions=he$interventions
  )
}
