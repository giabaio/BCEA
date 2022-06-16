# Loads all utility functions

#########################################################################################################################
mcmc2bugs <- function(x, model.file = NULL, program = "", DIC = FALSE, 
                      DICOutput = NULL, n.iter = NULL, n.burnin = 0, n.thin = 1){
  
  ### R script (mainly derived from the function jags in the R2jags package to load coda & put it in bugs/rjags format)
  ## Here's how you do it:
  # library(coda)
  # library(R2OpenBUGS)
  # source("utilJags.R")
  # coda <- read.openbugs("~/Dropbox/bcea/Shiny")
  # bugs <- mcmc2bugs(coda,program="jags")
  # Then this bugs object is in the class bugs --- and it has the right elements to post process in BCEAweb
  
  parameter.names <- dimnames(x[[1]])[[2]]
  n.keeps <- dim(x[[1]])[1]
  n.chains <- summary(x)[["nchain"]]
  n.parameters.to.save <- length(parameter.names)
  sims.array <- array(NA, c(n.keeps, n.chains, n.parameters.to.save))
  dimnames(sims.array)[[3]] <- parameter.names
  for (i in 1:n.chains){
    sims.array[,i,] <- x[[i]]
  }
  ans <- as.bugs.array2(sims.array, model.file=model.file, program=program, DIC=DIC, 
                                DICOutput=DICOutput, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
  return(ans)
}

as.bugs.array2 <- function(sims.array, model.file=NULL, program="jags",
                           DIC=FALSE, DICOutput=NULL, n.iter=NULL, n.burnin=0, n.thin=1){
  ## 'sims.array' is supposed to be a 3-way array with
  # n.sims*n.chains*n.parameters simulations, and
  # the 3rd component of dimnames(x) should have the parameter names.
  
  ## From Andrew Gelman's bugs.r function
  ## a couple of lines commented out by Eduardo Leoni (see comment below)
  #require("R2WinBUGS")
  d <- dim(sims.array)
  n.keep       <- d[1]
  n.chains     <- d[2]
  n.parameters <- d[3]
  n.sims       <- n.keep*n.chains
  if (is.null(n.iter)){
    n.iter <- (n.keep + n.burnin)*n.thin
  }
  #
  parameter.names <- dimnames(sims.array)[[3]]
  if (is.null(parameter.names)) {
    parameter.names <- paste("P", 1:n.parameters, sep="")
    dimnames(sims.array)[[3]] <- parameter.names
  }
  parameters.to.save <- unique(sapply(strsplit(parameter.names, "\\["), "[", 1))
  #
  sims <- matrix(NA, n.sims, n.parameters)
  root.long <- character(n.parameters)
  indexes.long <- vector(n.parameters, mode = "list")
  for (i in 1:n.parameters) {
    temp <- .decode.parameter.name(parameter.names[i])
    root.long[i] <- temp$root
    indexes.long[[i]] <- temp$indexes
  }
  n.roots <- length(parameters.to.save)
  left.bracket.short <- as.vector(regexpr("[[]", parameters.to.save))
  right.bracket.short <- as.vector(regexpr("[]]", parameters.to.save))
  root.short <- ifelse(left.bracket.short == -1, parameters.to.save,
                       substring(parameters.to.save, 1, left.bracket.short -1))
  dimension.short <- rep(0, n.roots)
  indexes.short <- vector(n.roots, mode = "list")
  n.indexes.short <- vector(n.roots, mode = "list")
  long.short <- vector(n.roots, mode = "list")
  length.short <- numeric(n.roots)
  for (j in 1:n.roots) {
    long.short[[j]] <- (1:n.parameters)[root.long == root.short[j]]
    length.short[j] <- length(long.short[[j]])
    if (length.short[j] == 0){
      stop(paste("parameter", root.short[[j]], "is not in the model"))
    }
    else if (length.short[j] > 1) {
      dimension.short[j] <- length(indexes.long[[long.short[[j]][1]]])
      n.indexes.short[[j]] <- numeric(dimension.short[j])
      for (k in 1:dimension.short[j]){
        n.indexes.short[[j]][k] <- length(unique(unlist(lapply(indexes.long[long.short[[j]]], .subset, k))))
      }
      length.short[j] <- prod(n.indexes.short[[j]])
      ## Modified by Eduardo Leoni
      ## this check fails if you take out a part of the simulations
      ## (for example, you don't want the array to have some of the
      ## parameters) so I took them out.
      
      ## if (length(long.short[[j]]) != length.short[j])
      ##   stop(paste("error in parameter", root.short[[j]],
      ##   "in parameters.to.save"))
      indexes.short[[j]] <- as.list(numeric(length.short[j]))
      for (k in 1:length.short[j]){
        indexes.short[[j]][[k]] <- indexes.long[[long.short[[j]][k]]]
      }
    }
  }
  rank.long <- unlist(long.short)
  # -----
  # yes, it's inefficient to do this, but for now I'm just letting this be as it is:
  for (k in 1:n.parameters) {
    sims[,k] <- as.vector(sims.array[,,k])
  }
  # ----
  dimnames(sims) <- list(NULL, parameter.names)
  summary <- R2OpenBUGS::monitor(sims.array, n.chains, keep.all = TRUE)
  last.values <- as.list(numeric(n.chains))
  
  for(nn in 1:length(n.indexes.short)){
    if(is.null(n.indexes.short[[nn]])){
      n.indexes.short[[nn]] <- 1
    }
  }
  
  
  for (i in 1:n.chains) {
    n.roots.0 <- if (DIC){
      n.roots - 1
    }
    else{
      n.roots
    }
    last.values[[i]] <- as.list(numeric(n.roots.0))
    names(last.values[[i]]) <- root.short[1:n.roots.0]
    for (j in 1:n.roots.0) {
      if (dimension.short[j] <= 1) {
        last.values[[i]][[j]] <- sims.array[n.keep, i, long.short[[j]]]
        names(last.values[[i]][[j]]) <- NULL
      }
      if (program=="jags"){
        last.values[[i]][[j]] <- array(sims.array[n.keep, i, long.short[[j]]], n.indexes.short[[j]])
      }
      ## only winbugs have to permute the array.
      else {
        last.values[[i]][[j]] <- aperm(array(sims.array[n.keep, i, long.short[[j]]], rev(n.indexes.short[[j]])), dimension.short[j]:1)
      }
    }
  }
  sims <- sims[sample(n.sims), , drop = FALSE]
  sims.list <- summary.mean <- summary.sd <- summary.median <- summary.025 <-  summary.975 <- vector(n.roots, mode = "list")
  names(sims.list) <- names(summary.mean) <- names(summary.sd) <- names(summary.median) <- names(summary.025) <- names(summary.975) <- root.short
  for (j in 1:n.roots) {
    if (length.short[j] == 1) {
      sims.list[[j]] <- sims[, long.short[[j]]]
      summary.mean[[j]] <- summary[long.short[[j]], "mean"]
      summary.sd[[j]] <- summary[long.short[[j]], "sd"]
      summary.median[[j]] <- summary[long.short[[j]], "50%"]
      ##ell: added 025 and 975
      summary.025[[j]] <- summary[long.short[[j]], "2.5%"]
      summary.975[[j]] <- summary[long.short[[j]], "97.5%"]
    }
    if (program=="bugs") {
      temp2 <- dimension.short[j]:1
      sims.list[[j]] <- aperm(array(sims[, long.short[[j]]], c(n.sims, rev(n.indexes.short[[j]]))), c(1, (dimension.short[j] + 1):2))
      summary.mean[[j]] <- aperm(array(summary[long.short[[j]], "mean"], rev(n.indexes.short[[j]])), temp2)
      summary.sd[[j]] <- aperm(array(summary[long.short[[j]], "sd"], rev(n.indexes.short[[j]])), temp2)
      summary.median[[j]] <- aperm(array(summary[long.short[[j]], "50%"], rev(n.indexes.short[[j]])), temp2)
      ##ell: added 025 and 975
      #      summary.025[[j]] <- aperm(array(summary[long.short[[j]], "2.5%"], rev(n.indexes.short[[j]])), temp2)
      #      summary.975[[j]] <- aperm(array(summary[long.short[[j]], "97.5%"], rev(n.indexes.short[[j]])), temp2)
    }
    if (program=="jags") {
      ##fix this list
      #sims.list[[j]] <- aperm(array(sims[, long.short[[j]]], c(n.sims, rev(n.indexes.short[[j]]))), c(1, (dimension.short[j] + 1):2))
      sims.list[[j]] <- array(sims[, long.short[[j]]], c(n.sims, n.indexes.short[[j]]))
      #sims.list[[j]] <- sims[, long.short[[j]]]
      summary.mean[[j]] <- array(summary[long.short[[j]],"mean"],n.indexes.short[[j]])
      summary.sd[[j]] <- array(summary[long.short[[j]],"sd"],n.indexes.short[[j]])
      summary.median[[j]] <- array(summary[long.short[[j]],"50%"],n.indexes.short[[j]])
      ##ell: added 025 and 975
      #      summary.025[[j]] <- array(summary[long.short[[j]],"2.5%"],n.indexes.short[[j]])
      #      summary.975[[j]] <- array(summary[long.short[[j]],"97.5%"],n.indexes.short[[j]])
    }
  }
  summary <- summary[rank.long, ]
  #all <- list(n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin,
  #        n.thin = n.thin, n.keep = n.keep, n.sims = n.sims,
  #        sims.array = sims.array[, , rank.long, drop = FALSE], sims.list = sims.list,
  #        sims.matrix = sims[, rank.long], summary = summary, mean = summary.mean,
  #        sd = summary.sd, median = summary.median, root.short = root.short,
  #        long.short = long.short, dimension.short = dimension.short,
  #        indexes.short = indexes.short, last.values = last.values,
  #        is.DIC=DIC, p02.5=summary.025, p97.5=summary.975)
  
  all <- list(n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin,
              n.thin = n.thin, n.keep = n.keep, n.sims = n.sims,
              sims.array = sims.array[,,rank.long,drop = FALSE], sims.list = sims.list,
              sims.matrix = sims[, rank.long], summary = summary, mean = summary.mean,
              sd = summary.sd, median = summary.median, root.short = root.short,
              long.short = long.short, dimension.short = dimension.short,
              indexes.short = indexes.short, last.values = last.values, program=program,
              model.file=model.file)
  
  if(DIC && is.null(DICOutput)) { ## calculate DIC from deviance
    deviance <- all$sims.array[, , "deviance", drop = FALSE]
    dim(deviance) <- dim(deviance)[1:2]
    pD <- numeric(n.chains)
    DIC <- numeric(n.chains)
    for (i in 1:n.chains) {
      pD[i] <- var(deviance[, i])/2
      DIC[i] <- mean(deviance[, i]) + pD[i]
    }
    all <- c(all, list(isDIC=TRUE, DICbyR=TRUE,  pD=mean(pD), DIC=mean(DIC)))
  }
  else if(DIC && !is.null(DICOutput)) { ## use DIC from BUGS
    all <- c(all, list(isDIC=TRUE, DICbyR=FALSE,
                       pD=DICOutput[nrow(DICOutput),4],
                       DIC=DICOutput[nrow(DICOutput),3]))
  }
  else {
    all <- c(all, isDIC=FALSE)
  }
  class(all) <- "bugs"
  all
}

.decode.parameter.name <- function (a) 
{
  left.bracket <- regexpr("[[]", a)
  if (left.bracket == -1) {
    root <- a
    dimension <- 0
    indexes <- NA
  }
  else {
    root <- substring(a, 1, left.bracket - 1)
    right.bracket <- regexpr("[]]", a)
    a <- substring(a, left.bracket + 1, right.bracket - 1)
    indexes <- as.numeric(unlist(strsplit(a, ",")))
    dimension <- length(indexes)
  }
  list(root = root, dimension = dimension, indexes = indexes)
}

#trace plot
mytraceplot <- function(node,model=m,title="",lab=""){
  ## node is a string with the name of the node to be plotted
  ## model is the name of the object containing the MCMC simulations
  xlab <- "Iteration"
  ## this way works with R2jags as well as with R2WinBUGS
  ###  cmd <- ifelse(class(model)=="rjags",mdl <- model$BUGSoutput,mdl <- model)
  mdl <- model$BUGSoutput
  col <- colors()
  if (mdl$n.chains==1) {
    plot(mdl$sims.array[,1,node],t="l",col=col[which(col=="blue")],xlab=xlab,
         ylab=lab,main=title)
  }
  if (mdl$n.chains==2) {
    cols <- c("blue","red")
    plot(mdl$sims.array[,1,node],t="l",col=col[which(col==cols[1])],xlab=xlab,
         ylab=lab,main=title,ylim=range(mdl$sims.array[,1:2,node]))
    points(mdl$sims.array[,2,node],t="l",col=col[which(col==cols[2])])
  }
  if (mdl$n.chains>2) {
    cols <- c("blue","red","green","magenta","orange","brown","azure")
    plot(mdl$sims.array[,1,node],t="l",col=col[which(col==cols[1])],xlab=xlab,
         ylab=lab,main=title,
         ylim=range(mdl$sims.array[,1:mdl$n.chains,node]))
    for (i in 2:mdl$n.chains) {
      points(mdl$sims.array[,i,node],t="l",col=col[which(col==cols[i])])
    }
  }
}

# plotGR
plotGR <- function(m,ind) {
  # Produces a plot of the values of the Gelman Rubin stats to determine
  # visually convergence (and see clearly which node has reached it)
  # m is an object in the class jags or bugs
  ##cmd <- ifelse(class(m)=="rjags",mdl <- m$BUGSoutput,mdl <- m)
  mdl <- m$BUGSoutput
  ind2 <- 1:dim(mdl$summary)[1]
  plot(mdl$summary[,"Rhat"],xlab="Saved parameters",ylab="Gelman-Rubin diagnostic",
       axes=F,col="white")
  thresh <- 1.1
  abline(h=thresh,col="dark grey",lwd=2)
  text(ind2[-ind],mdl$summary[ind2[-ind],"Rhat"],
       row.names(mdl$summary)[ind2[-ind]],cex=.8)
  text(ind,mdl$summary[ind,"Rhat"],
       row.names(mdl$summary)[ind],col="red",cex=1,font=2)
  axis(2)
  box()
}

## n.eff plot
plot.neff <- function(m,ind) {
  ##cmd <- ifelse(class(m)=="rjags",mdl <- m$BUGSoutput,mdl <- m)
  mdl <- m$BUGSoutput
  ind2 <- 1:dim(mdl$summary)[1]
  plot(ind2,mdl$summary[,"n.eff"],
       xlab="Saved parameters",ylab="Effective sample size",
       axes=F,col="white",ylim=c(1,mdl$n.sims))
  text(ind2[-ind],mdl$summary[ind2[-ind],"n.eff"],
       row.names(mdl$summary)[ind2[-ind]],cex=.8)
  text(ind,mdl$summary[ind,"n.eff"],rownames(mdl$summary)[ind],
       col="red",cex=1,font=2)
  axis(2)
  box()
}

## ACF
acf.plot <- function(m,ind,title="Autocorrelation function") {
  ##cmd <- ifelse(class(m)=="rjags",mdl <- m$BUGSoutput,mdl <- m)
  mdl <- m$BUGSoutput
  ind2 <- 1:dim(mdl$summary)[1]
  if (var(mdl$sims.matrix[,ind])==0) {return(invisible())}
  acf(mdl$sims.matrix[,ind],ylab="Autocorrelation",main=title)
}

## quiet --- allows to disable the cat messages (useful for the evppi function)
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 