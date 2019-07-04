evppi.default<-function (parameter, input, he, N = NULL, plot = F, residuals = T,...) {

  # This function has been completely changed and restructured to make it possible to change regression method.
  # The method arguement can now be given as a list. The first element element in the list is a vector giving the
  # regression method for the effects. The second gives the regression method for the costs. The `method' argument
  # can also be given as before which then uses the same regression method for all curves. All other exArgs can be
  # given as before. 'int.ord' can be updated using the list forumlation above to give the interactions for each
  # different curve. The formula arguement for GAM can only be given once, either 'te()' or 's()+s()' as this is
  # for computational reasons rather than to aid fit. You can still plot the INLA mesh elements but not output the meshes.

  if (is.null(colnames(input))) {
    colnames(input) <- paste0("theta",1:dim(input)[2])
  }
  if (class(parameter[1]) == "numeric" | class(parameter[1]) == "integer") {
    parameters = colnames(input)[parameter]
  }
  else {
    parameters = parameter
    for (i in 1:length(parameters)) {
      parameter[i] <- which(colnames(input) == parameters[i])
    }
    class(parameter)<-"numeric"
  }
  if (is.null(N)) {
    N <- he$n.sim
  }

  robust <- NULL
  exArgs <- list(...)
  if (!exists("suppress.messages", where=exArgs)) {
    suppress.messages=FALSE
  } else {
    suppress.messages=exArgs$suppress.messages
  }

  if (!exists("select", where=exArgs) & N == he$n.sim) {
    exArgs$select <- 1:he$n.sim
  }
  if (!exists("select", where=exArgs) & N < he$n.sim) {
    exArgs$select <- sample(1:he$n.sim, size = N, replace = F)
  }
  inputs <- data.frame(input)[exArgs$select,]


  # Sets default for method of calculation. If number of parameters <=4, then use GAM, if not defaults to INLA/SPDE
  if (length(parameter) <= 4 & !exists("method", where = exArgs)) {
    exArgs$method <- list(rep("GAM",he$n.comparators-1),rep("GAM",he$n.comparators-1))
  }
  if (length(parameter) > 4 & !exists("method", where = exArgs)) {
    exArgs$method <- list(rep("INLA",he$n.comparators-1),rep("INLA",he$n.comparators-1))
  }
  if(class(exArgs$method)!="list"){
    if(exArgs$method=="sad"|exArgs$method=="so"){
      exArgs$method<-exArgs$method
    }
    else{
      if(length(exArgs$method)>1){
        exArgs$method <- list(exArgs$method,exArgs$method)
      }
      if(length(exArgs$method)==1){
        exArgs$method <- list(rep(exArgs$method,he$n.comparators-1),rep(exArgs$method,he$n.comparators-1))
      }
    }
  }

  if(class(exArgs$method)=="list"){
    if(length(exArgs$method[[1]])+length(exArgs$method[[2]])!=2*(he$n.comparators-1)){
      stop(paste("The argument 'method' must be a list of length 2 with",he$n.comparators-1,"elements each."))
    }
  }

  if(!exists("int.ord",where=exArgs)){
    exArgs$int.ord <- list(rep(1,he$n.comparators-1),rep(1,he$n.comparators-1))
  }
  if(class(exArgs$int.ord)!="list"){
    exArgs$int.ord <- list(rep(exArgs$int.ord[1],he$n.comparators-1),rep(exArgs$int.ord[2],he$n.comparators-1))
  }

  prep.x<-function(he,select,k,l){
    if(k==1){
      x<-as.matrix(he$delta.e)[select,l]
    }
    if(k==2){
      x<-as.matrix(he$delta.c)[select,l]
    }
    return(x)
  }

  ###GAM Fitting
  fit.gam <- function(parameter, inputs, x, form) {
    tic <- proc.time()
    N<-nrow(inputs)
    p<-length(parameter)
    model <- mgcv::gam(update(formula(x ~ .),
                              formula(paste(".~", form))), data = data.frame(inputs))
    hat <- model$fitted
    formula <- form
    fitted <- matrix(hat, nrow = N, ncol = p)
    fit <- model
    toc <- proc.time() - tic
    time <- toc[3]
    names(time) = "Time to fit GAM regression (seconds)"
    list(fitted=hat,formula = formula, fit = model,time = time)
  }

  ###GP Fitting
  post.density <- function(hyperparams, parameter, x, input.matrix) {
    dinvgamma <- function(x, alpha, beta) {
      (beta^alpha)/gamma(alpha) * x^(-alpha - 1) *
        exp(-beta/x)
    }
    N <- length(x)
    p <- length(parameter)
    H <- cbind(1, input.matrix)
    q <- ncol(H)
    a.sigma <- 0.001
    b.sigma <- 0.001
    a.nu <- 0.001
    b.nu <- 1
    delta <- exp(hyperparams)[1:p]
    nu <- exp(hyperparams)[p + 1]
    A <- exp(-(as.matrix(dist(t(t(input.matrix)/delta),
                              upper = TRUE, diag = TRUE))^2))
    Astar <- A + nu * diag(N)
    T <- chol(Astar)
    y <- backsolve(t(T),(x), upper.tri = FALSE)
    x. <- backsolve(t(T), H, upper.tri = FALSE)
    tHAstarinvH <- t(x.) %*% (x.)
    betahat <- solve(tHAstarinvH) %*% t(x.) %*% y
    residSS <- y %*% y - t(y) %*% x. %*% betahat - t(betahat) %*%
      t(x.) %*% y + t(betahat) %*% tHAstarinvH %*% betahat
    prior <- prod(dnorm(log(delta), 0, sqrt(1e+05))) *
      dinvgamma(nu, a.nu, b.nu)
    l <- -sum(log(diag(T))) - 1/2 * log(det(tHAstarinvH)) -
      (N - q + 2 * a.sigma)/2 * log(residSS/2 + b.sigma) +
      log(prior)
    names(l) <- NULL
    return(l)
  }
  estimate.hyperparameters <- function(x, input.matrix, parameter,n.sim) {
    p <- length(parameter)
    initial.values <- rep(0, p + 1)
    repeat {
      log.hyperparameters <- optim(initial.values,
                                   fn = post.density,parameter=parameter, x = x[1:n.sim],
                                   input.matrix = input.matrix[1:n.sim, ],
                                   method = "Nelder-Mead", control = list(fnscale = -1,
                                                                          maxit = 10000, trace = 0))$par
      if (sum(abs(initial.values - log.hyperparameters)) <
          0.01) {
        hyperparameters <- exp(log.hyperparameters)
        break
      }
      initial.values <- log.hyperparameters
    }
    return(hyperparameters)
  }
  fit.gp <- function(parameter, inputs, x, n.sim) {
    tic <- proc.time()
    p <- length(parameter)
    input.matrix <- as.matrix(inputs[, parameter, drop = FALSE])
    colmin <- apply(input.matrix, 2, min)
    colmax <- apply(input.matrix, 2, max)
    colrange <- colmax - colmin
    input.matrix <- sweep(input.matrix, 2, colmin, "-")
    input.matrix <- sweep(input.matrix, 2, colrange,
                          "/")
    N <- nrow(input.matrix)
    H <- cbind(1, input.matrix)
    q <- ncol(H)
    hyperparameters <- estimate.hyperparameters(x = x,input = input.matrix, parameter = parameter, n.sim = n.sim)
    delta.hat <- hyperparameters[1:p]
    nu.hat <- hyperparameters[p + 1]
    A <- exp(-(as.matrix(dist(t(t(input.matrix)/delta.hat),
                              upper = TRUE, diag = TRUE))^2))
    Astar <- A + nu.hat * diag(N)
    Astarinv <- chol2inv(chol(Astar))
    rm(Astar)
    gc()
    AstarinvY <- Astarinv %*% x
    tHAstarinv <- t(H) %*% Astarinv
    tHAHinv <- solve(tHAstarinv %*% H)
    betahat <- tHAHinv %*% (tHAstarinv %*% x)
    Hbetahat <- H %*% betahat
    resid <- x - Hbetahat
    fitted<- Hbetahat + A %*% (Astarinv %*%
                                 resid)
    AAstarinvH <- A %*% t(tHAstarinv)
    sigmasqhat <- as.numeric(t(resid) %*% Astarinv %*%
                               resid)/(N - q - 2)
    rm(A, Astarinv, AstarinvY, tHAstarinv, tHAHinv,
       Hbetahat, resid, sigmasqhat)
    gc()
    toc <- proc.time() - tic
    time <- toc[3]
    names(time) = "Time to fit GP regression (seconds)"
    list(fitted = fitted,time = time, fit=NULL,formula = NULL)
  }

  ###INLA Fitting
  make.proj <- function(parameter,inputs, x,k,l) {
    tic <- proc.time()
    scale<-8/(range(x)[2]-range(x)[1])
    scale.x <- scale*x -mean(scale*x)
    bx<-ldr::bf(scale.x,case="poly",2)
    fit1<-ldr::pfc(scale(inputs[,parameter]),scale.x,bx,structure="iso")
    fit2<-ldr::pfc(scale(inputs[,parameter]),scale.x,bx,structure="aniso")
    fit3<-ldr::pfc(scale(inputs[,parameter]),scale.x,bx,structure="unstr")
    struc<-c("iso","aniso","unstr")[which(c(fit1$aic,fit2$aic,fit3$aic)==min(fit1$aic,fit2$aic,fit3$aic))]
    AIC.deg<-array()
    for(i in 2:7){
      bx<-ldr::bf(scale.x,case="poly",i)
      fit<-ldr::pfc(scale(inputs[,parameter]),scale.x,bx,structure=struc)
      AIC.deg[i]<-fit$aic}
    deg<-which(AIC.deg==min(AIC.deg,na.rm=T))
    d<-min(dim(inputs[,parameter])[2],deg)
    by<-ldr::bf(scale.x,case="poly",deg)
    comp.d<-ldr::ldr(scale(inputs[,parameter]),scale.x,bx,structure=struc,model="pfc",numdir=d,numdir.test=T)
    dim.d<-which(comp.d$aic==min(comp.d$aic))-1
    comp<-ldr::ldr(scale(inputs[,parameter]),scale.x,bx,structure=struc,model="pfc",numdir=2)
    toc <- proc.time() - tic
    time <- toc[3]
    if(dim.d>2){
      cur<-c("effects","costs")
      warning(paste("The dimension of the sufficient reduction for the incremental",cur[k],", column",l,", is",dim.d,".
                    Dimensions greater than 2 imply that the EVPPI approximation using INLA may be inaccurate.
                    Full residual checking using diag.evppi is required."))}
    names(time) = "Time to fit find projections (seconds)"
    list(data = comp$R, time = time,dim=dim.d)
    }
  plot.mesh <- function(mesh, data, plot) {
    if (plot == TRUE || plot == T) {
      cat("\n")
      choice <- select.list(c("yes", "no"), title = "Would you like to save the graph?",
                            graphics = F)
      if (choice == "yes") {
        exts <- c("jpeg", "pdf", "bmp", "png", "tiff")
        ext <- select.list(exts, title = "Please select file extension",
                           graphics = F)
        name <- paste0(getwd(), "/mesh.", ext)
        txt <- paste0(ext, "('", name, "')")
        eval(parse(text = txt))
        plot(mesh)
        points(data, col = "blue", pch = 19, cex = 0.8)
        dev.off()
        txt <- paste0("Graph saved as: ", name)
        cat(txt)
        cat("\n")
      }
      cat("\n")
      plot(mesh)
      points(data, col = "blue", pch = 19, cex = 0.8)
    }
  }
  make.mesh <- function(data, convex.inner, convex.outer,
                        cutoff,max.edge) {
    tic <- proc.time()
    inner <- suppressMessages({
      INLA::inla.nonconvex.hull(data, convex = convex.inner)
    })
    outer <- INLA::inla.nonconvex.hull(data, convex = convex.outer)
    mesh <- INLA::inla.mesh.2d(
      loc=data, boundary=list(inner,outer),
      max.edge=c(max.edge,max.edge),cutoff=c(cutoff))
    toc <- proc.time() - tic
    time <- toc[3]
    names(time) = "Time to fit determine the mesh (seconds)"
    list(mesh = mesh, pts = data, time = time)
  }
  fit.inla <- function(parameter, inputs, x, mesh,
                       data.scale, int.ord, convex.inner, convex.outer,
                       cutoff, max.edge,h.value,family) {
    tic <- proc.time()
    inputs.scale <- scale(inputs, apply(inputs, 2, mean), apply(inputs, 2, sd))
    scale<-8/(range(x)[2]-range(x)[1])
    scale.x <- scale*x -mean(scale*x)
    A <- INLA::inla.spde.make.A(mesh = mesh, loc = data.scale, silent = 2L)
    spde <- INLA::inla.spde2.matern(mesh = mesh, alpha = 2)
    stk.real <- INLA::inla.stack(tag = "est", data = list(y=scale.x), A = list(A, 1),
                                 effects = list(s = 1:spde$n.spde,
                                                data.frame(b0 = 1, x = cbind(data.scale, inputs.scale))))
    data <- INLA::inla.stack.data(stk.real)
    ctr.pred <- INLA::inla.stack.A(stk.real)
    inp <- names(stk.real$effects$data)[parameter + 4]
    form <- paste(inp, "+", sep = "", collapse = "")
    formula <- paste("y~0+(", form, "+0)+b0+f(s,model=spde)",
                     sep = "", collapse = "")
    if (int.ord[1] > 1) {
      formula <- paste("y~0+(", form, "+0)^", int.ord[1],
                       "+b0+f(s,model=spde)", sep = "", collapse = "")
    }
    Result <- suppressMessages({
      INLA::inla(as.formula(formula), data = data,
                 family = family, control.predictor = list(A = ctr.pred,link = 1),
                 control.inla = list(h = h.value),
                 control.compute = list(config = T))
    })
    fitted <- (Result$summary.linear.predictor[1:length(x),"mean"]+mean(scale*x))/scale
    fit <- Result
    toc <- proc.time() - tic
    time <- toc[3]
    names(time) = "Time to fit INLA/SPDE (seconds)"
    list(fitted = fitted, model = fit, time = time, formula = formula,
         mesh = list(mesh = mesh, pts = data.scale))
  }

  compute.evppi <- function(he,fit.full) {
    EVPPI <- array()
    tic <- proc.time()
    for (i in 1:length(he$k)) {
      NB.k <- -(he$k[i]*fit.full[[1]]-fit.full[[2]])
      EVPPI[i] <- (mean(apply(NB.k, 1, max, na.rm = T)) -
                     max(apply(NB.k, 2, mean, na.rm = T)))
    }
    toc <- proc.time() - tic
    time <- toc[3]
    names(time) = "Time to compute the EVPPI (in seconds)"
    list(EVPPI = EVPPI, time = time)
  }

  prepare.output <- function(parameters, inputs) {
    if (length(parameter) == 1) {
      if (class(parameter) == "numeric") {
        name = colnames(inputs)[parameter]
      }
      else {
        name = parameter
      }
    }
    else {
      if (class(parameter) == "numeric") {
        n.param <- length(parameter)
        end <- colnames(input)[parameter[n.param]]
        name.mid <- paste(colnames(inputs)[parameter[1:n.param -
                                                       1]], ", ", sep = "", collapse = " ")
        name <- paste(name.mid, "and ", end, sep = "",
                      collapse = " ")
      }
      else {
        n.param <- length(parameter)
        end <- parameter[n.param]
        name.mid <- paste(parameter[1:n.param - 1],
                          ", ", sep = "", collapse = " ")
        name <- paste(name.mid, "and ", end, sep = "",
                      collapse = " ")
      }
    }
    return(name)
  }

  if(class(exArgs$method)!="list"){
    if (exArgs$method == "sal"||exArgs$method=="sad") {
      method = "Sadatsafavi et al"
      n.blocks = NULL
      if (!exists("n.seps", where = exArgs)) {
        n.seps <- 1
      }
      else {
        n.seps <- exArgs$n.seps
      }
      if (length(parameters) == 1) {
        d <- he$n.comparators
        n <- he$n.sim
        w <- parameters
        param <- inputs[, w]
        o <- order(param)
        param <- param[o]
        nSegs <- matrix(1, d, d)
        nSegs[1, 2] <- n.seps
        nSegs[2, 1] <- n.seps
        res <- segPoints <- numeric()
        for (k in 1:length(he$k)) {
          nbs <- he$U[, k, ]
          nbs <- nbs[o, ]
          for (i in 1:(d - 1)) {
            for (j in (i + 1):d) {
              cm <- cumsum(nbs[, i] - nbs[, j])/n
              if (nSegs[i, j] == 1) {
                l <- which.min(cm)
                u <- which.max(cm)
                if (cm[u] - max(cm[1], cm[n]) > min(cm[1],
                                                    cm[n]) - cm[l]) {
                  segPoint <- u
                }
                else {
                  segPoint <- l
                }
                if (segPoint > 1 && segPoint < n) {
                  segPoints <- c(segPoints, segPoint)
                }
              }
              if (nSegs[i, j] == 2) {
                distMaxMin <- 0
                distMinMax <- 0
                minL <- Inf
                maxL <- -Inf
                for (sims in 1:n) {
                  if (cm[sims] > maxL) {
                    maxLP <- sims
                    maxL <- cm[sims]
                  }
                  else {
                    if (maxL - cm[sims] > distMaxMin) {
                      distMaxMin <- maxL - cm[sims]
                      segMaxMinL <- maxLP
                      segMaxMinR <- sims
                    }
                  }
                  if (cm[sims] < minL) {
                    minLP <- sims
                    minL <- cm[sims]
                  }
                  else {
                    if (cm[sims] - minL > distMinMax) {
                      distMinMax <- cm[sims] - minL
                      segMinMaxL <- minLP
                      segMinMaxR <- sims
                    }
                  }
                }
                siMaxMin <- cm[segMaxMinL] + distMaxMin +
                  (cm[n] - cm[segMaxMinR])
                siMinMax <- -cm[segMaxMinL] + distMinMax -
                  (cm[n] - cm[segMinMaxR])
                if (siMaxMin > siMinMax) {
                  segPoint <- c(segMaxMinL, segMaxMinR)
                }
                else {
                  segPoint <- c(segMinMaxL, segMinMaxR)
                }
                if (segPoint[1] > 1 && segPoint[1] <
                    n) {
                  segPoints <- c(segPoints, segPoint[1])
                }
                if (segPoint[2] > 1 && segPoint[2] <
                    n) {
                  segPoints <- c(segPoints, segPoint[2])
                }
              }
            }
          }
          if (length(segPoints) > 0) {
            segPoints2 <- unique(c(0, segPoints[order(segPoints)],
                                   n))
            res[k] <- 0
            for (j in 1:(length(segPoints2) - 1)) {
              res[k] <- res[k] + max(colSums(matrix(nbs[(1 +
                                                           segPoints2[j]):segPoints2[j + 1], ],
                                                    ncol = d)))/n
            }
            res[k] <- res[k] - max(colMeans(nbs))
          }
          else {
            res[k] <- 0
          }
        }
      }
      if (length(parameters) > 1) {
        res <- list()
        for (lp in 1:length(parameters)) {
          d <- he$n.comparators
          n <- he$n.sim
          w <- parameters[lp]
          param <- inputs[, w]
          o <- order(param)
          param <- param[o]
          nSegs <- matrix(1, d, d)
          nSegs[1, 2] <- n.seps
          nSegs[2, 1] <- n.seps
          temp <- segPoints <- numeric()
          for (k in 1:length(he$k)) {
            nbs <- he$U[, k, ]
            nbs <- nbs[o, ]
            for (i in 1:(d - 1)) {
              for (j in (i + 1):d) {
                cm <- cumsum(nbs[, i] - nbs[, j])/n
                if (nSegs[i, j] == 1) {
                  l <- which.min(cm)
                  u <- which.max(cm)
                  if (cm[u] - max(cm[1], cm[n]) > min(cm[1],
                                                      cm[n]) - cm[l]) {
                    segPoint <- u
                  }
                  else {
                    segPoint <- l
                  }
                  if (segPoint > 1 && segPoint < n) {
                    segPoints <- c(segPoints, segPoint)
                  }
                }
                if (nSegs[i, j] == 2) {
                  distMaxMin <- 0
                  distMinMax <- 0
                  minL <- Inf
                  maxL <- -Inf
                  for (sims in 1:n) {
                    if (cm[sims] > maxL) {
                      maxLP <- sims
                      maxL <- cm[sims]
                    }
                    else {
                      if (maxL - cm[sims] > distMaxMin) {
                        distMaxMin <- maxL - cm[sims]
                        segMaxMinL <- maxLP
                        segMaxMinR <- sims
                      }
                    }
                    if (cm[sims] < minL) {
                      minLP <- sims
                      minL <- cm[sims]
                    }
                    else {
                      if (cm[sims] - minL > distMinMax) {
                        distMinMax <- cm[sims] - minL
                        segMinMaxL <- minLP
                        segMinMaxR <- sims
                      }
                    }
                  }
                  siMaxMin <- cm[segMaxMinL] + distMaxMin +
                    (cm[n] - cm[segMaxMinR])
                  siMinMax <- -cm[segMaxMinL] + distMinMax -
                    (cm[n] - cm[segMinMaxR])
                  if (siMaxMin > siMinMax) {
                    segPoint <- c(segMaxMinL, segMaxMinR)
                  }
                  else {
                    segPoint <- c(segMinMaxL, segMinMaxR)
                  }
                  if (segPoint[1] > 1 && segPoint[1] <
                      n) {
                    segPoints <- c(segPoints, segPoint[1])
                  }
                  if (segPoint[2] > 1 && segPoint[2] <
                      n) {
                    segPoints <- c(segPoints, segPoint[2])
                  }
                }
              }
            }
            if (length(segPoints) > 0) {
              segPoints2 <- unique(c(0, segPoints[order(segPoints)],
                                     n))
              temp[k] <- 0
              for (j in 1:(length(segPoints2) - 1)) {
                temp[k] <- temp[k] + max(colSums(matrix(nbs[(1 +
                                                               segPoints2[j]):segPoints2[j + 1], ],
                                                        ncol = d)))/n
              }
              temp[k] <- temp[k] - max(colMeans(nbs))
            }
            else {
              temp[k] <- 0
            }
          }
          res[[lp]] <- temp
        }
        names(res) <- parameters
      }

      res <- list(evppi = res, index = parameters, parameters = parameters,
                  k = he$k, evi = he$evi, method = method)
    }
    if (exArgs$method == "so") {
      method = "Strong & Oakley (univariate)"
      n.seps = NULL
      if (!exists("n.blocks", where = exArgs)) {
        stop("Please specify the parameter 'n.blocks' to use the Strong and Oakley univariate method")
      }
      else {
        n.blocks <- exArgs$n.blocks
      }
      S <- he$n.sim
      J <- S/exArgs$n.blocks
      check <- S%%exArgs$n.blocks
      if (check > 0) {
        stop("number of simulations/number of blocks must be an integer. Please select a different value for n.blocks \n")
      }
      D <- he$n.comparators
      if (length(parameter) == 1) {
        sort.order <- order(inputs[, parameters])
        sort.U <- array(NA, dim(he$U))
        evpi <- res <- numeric()
        for (i in 1:length(he$k)) {
          evpi[i] <- he$evi[i]
          sort.U[, i, ] <- he$U[sort.order, i, ]
          U.array <- array(sort.U[, i, ], dim = c(J,
                                                  exArgs$n.blocks, D))
          mean.k <- apply(U.array, c(2, 3), mean)
          partial.info <- mean(apply(mean.k, 1, max))
          res[i] <- partial.info - max(apply(he$U[, i,
                                                  ], 2, mean))
        }
      }
      if (length(parameter) > 1) {
        res <- list()
        for (j in 1:length(parameter)) {
          sort.order <- order(inputs[, parameters[j]])
          sort.U <- array(NA, dim(he$U))
          evpi <- evppi.temp <- numeric()
          for (i in 1:length(he$k)) {
            evpi[i] <- he$evi[i]
            sort.U[, i, ] <- he$U[sort.order, i, ]
            U.array <- array(sort.U[, i, ], dim = c(J,
                                                    n.blocks, D))
            mean.k <- apply(U.array, c(2, 3), mean)
            partial.info <- mean(apply(mean.k, 1, max))
            evppi.temp[i] <- partial.info - max(apply(he$U[,
                                                           i, ], 2, mean))
          }
          res[[j]] <- evppi.temp
        }
        names(res) <- parameters
      }

      res <- list(evppi = res, index = parameters, parameters = parameters,
                  k = he$k, evi = he$evi, method = method)
    }
  }
  if(class(exArgs$method)=="list"){
    time<-list()
    time[[1]]<-list()
    time[[2]]<-list()

    fit.full<-list()
    fit.full[[1]]<-matrix(data=0,nrow=length(exArgs$select),ncol=he$n.comparators)
    fit.full[[2]]<-matrix(data=0,nrow=length(exArgs$select),ncol=he$n.comparators)
    for(k in 1:2){
      for(l in 1:he$n.comparisons){
        x<-prep.x(he=he,select=exArgs$select,k=k,l=l)
        method<-exArgs$method[[k]][l]
        if (method == "GAM" || method == "gam" ||
            method == "G" || method == "g") {
          method <- "GAM"
          mesh <- robust <- NULL
          if (!isTRUE(requireNamespace("mgcv", quietly = TRUE))) {
            stop("You need to install the package 'mgcv'. Please run in your R terminal:\n install.packages('mgcv')")
          }
          if (isTRUE(requireNamespace("mgcv", quietly = TRUE))) {
            if(suppress.messages==FALSE) {
            cat("\n")
            cat("Calculating fitted values for the GAM regression \n")
            }

            inp <- names(inputs)[parameter]
            if (exists("formula", where = exArgs)) {
              form <- exArgs$formula
            }
            else {
              form <- paste("te(", paste(inp, ",", sep = "",
                                         collapse = ""), "bs='cr')")
            }
            fit <- fit.gam(parameter = parameter, inputs = inputs,
                           x = x, form = form)
          }
        }
        if (method == "gp" || method == "GP") {
          method <- "GP"
          mesh <- robust <- NULL
          if(suppress.messages==FALSE) {
          cat("\n")
          cat("Calculating fitted values for the GP regression \n")
          # If the number of simulations to be used to estimate the hyperparameters is set then use that, else use N/2
          }
          if (!exists("n.sim", where = exArgs)) {
            n.sim = N/2
          }
          else {
            n.sim = exArgs$n.sim
          }
          fit <- fit.gp(parameter = parameter, inputs = inputs, x = x, n.sim = n.sim)
        }
        if (method == "INLA") {
          method <- "INLA"
          if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
            stop("You need to install the packages 'INLA' and 'splancs'. Please run in your R terminal:\n install.packages('INLA', repos='http://www.math.ntnu.no/inla/R/stable')\n and\n install.packages('splancs')")
          }
          if (!isTRUE(requireNamespace("ldr", quietly = TRUE))) {
            stop("You need to install the package 'ldr'. Please run in your R terminal:\n install.packages('ldr')")
          }
          if (isTRUE(requireNamespace("ldr", quietly = TRUE))) {

            if (isTRUE(requireNamespace("INLA", quietly = TRUE))) {
              if (!is.element("INLA", (.packages()))) {
                attachNamespace("INLA")
              }
              if(length(parameter)<2){
                stop("The INLA method can only be used with 2 or more parameters")
              }
              if(suppress.messages==FALSE) {
              cat("\n")
              cat("Finding projections \n")
              }
              projections <- make.proj(parameter=parameter,inputs = inputs,x=x,k=k,l=l)
              data <- projections$data
              if(suppress.messages==FALSE) {
              cat("Determining Mesh \n")
              }
              if (!exists("cutoff", where = exArgs)) {
                cutoff = 0.3
              }
              else {
                cutoff = exArgs$cutoff
              }
              if (!exists("convex.inner", where = exArgs)) {
                convex.inner = -0.4
              }
              else {
                convex.inner = exArgs$convex.inner
              }
              if (!exists("convex.outer", where = exArgs)) {
                convex.outer = -0.7
              }
              else {
                convex.outer = exArgs$convex.outer
              }
              if (!exists("max.edge", where = exArgs)) {
                max.edge = 0.7
              }
              else {
                max.edge = exArgs$max.edge
              }
              mesh <- make.mesh(data = data, convex.inner = convex.inner,
                                convex.outer = convex.outer, cutoff = cutoff,max.edge=max.edge)
              plot.mesh(mesh = mesh$mesh, data = data,
                        plot = plot)
              if(suppress.messages==FALSE) {
              cat("Calculating fitted values for the GP regression using INLA/SPDE \n")
              }
              if (exists("h.value", where = exArgs)) {
                h.value = exArgs$h.value
              }
              else {
                h.value = 5e-05
              }
              if (exists("robust", where = exArgs)) {
                if (exArgs$robust == TRUE) {
                  family = "T"
                  robust = TRUE
                }
                else {
                  family = "gaussian"
                  robust = FALSE
                }
              }
              else {
                family = "gaussian"
                robust = FALSE
              }
              if (exists("int.ord", where = exArgs)) {
                int.ord = exArgs$int.ord[[k]][l]
              }
              else {
                int.ord = 1
              }
              fit <- fit.inla(parameter = parameter, inputs = inputs,
                              x = x, mesh = mesh$mesh, data.scale = data, int.ord = int.ord,
                              convex.inner = convex.inner, convex.outer = convex.outer,
                              cutoff = cutoff, max.edge = max.edge, h.value = h.value,family=family)
            }
          }
        }
        fit.full[[k]][,l]<-fit$fitted
        ###Calculating Time Taken
        if (method == "INLA") {
          time. <- c(projections$time, mesh$time, fit$time)
          time. <- sum(time.)
          time[[k]][l] <- (time.)
        }
        else {
          time. <- fit$time
          time[[k]][l] <- time.
        }
      }
    }
    if(suppress.messages==FALSE) {cat("Calculating EVPPI \n")}
    comp <- compute.evppi(he = he, fit.full = fit.full)
    name <- prepare.output(parameter=parameters, inputs=inputs)
    time[[3]]<-comp$time
    names(time)<-c("Fitting for Effects","Fitting for Costs","Calculating EVPPI")
    names(exArgs$method)<-c("Methods for Effects","Methods for Costs")

    if (residuals == TRUE || residuals == T) {
      res <- list(evppi = comp$EVPPI, index = parameters,
                  k = he$k, evi = he$evi, parameters = name, time = time,
                  method = exArgs$method, fitted.costs = fit.full[[2]],
                  fitted.effects = fit.full[[1]],select=exArgs$select)
    }
    else {
      res <- list(evppi = comp$EVPPI, index = parameters,
                  k = he$k, evi = he$evi, parameters = name, time = time, method = exArgs$method)
    }

  }

  class(res) <- "evppi"
  return(res)
  }
