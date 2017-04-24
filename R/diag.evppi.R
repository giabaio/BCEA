######diag.evppi################################################################################################
diag.evppi <- function(x,y,diag=c("residuals","qqplot"),int=1){
  # x = an evppi object
  # y = a bcea object
  # diag = the type of diagnostics required
  # int = the comparison to be assessed (default determined by the BCEA object)
  if (int>1 & dim(x$fitted.costs)[2]==1) {stop("There is only one comparison possible, so 'int' should be set to 1 (default)")}
  res <- ifelse(isTRUE(pmatch(diag,c("residuals","qqplot"))==2),FALSE,TRUE)
  if(res){
    par(mfrow=c(1,2))
    n <- dim(x$fitted.costs)[1]
    fitted <- x$fitted.costs[,int]
    residual <- as.matrix(y$delta.c)[x$select,int]-fitted
    plot(fitted,residual,xlab="Fitted values",
         ylab="Residuals",main="Residual plot for costs",cex=.8);abline(h=0)
    fitted <- x$fitted.effects[,int]
    residual <- as.matrix(y$delta.e)[x$select,int]-fitted
    plot(fitted,residual,xlab="Fitted values",
         ylab="Residuals",main="Residual plot for effects",cex=.8);abline(h=0)
  }else{
    par(mfrow=c(1,2))
    qqnorm(x$fitted.costs[,int],main="Normal Q-Q plot \n(costs)"); qqline(x$fitted.costs[,int])
    qqnorm(x$fitted.effects[,int],main="Normal Q-Q plot \n(effects)"); qqline(x$fitted.effects[,int])
  }
  
}
