options(shiny.maxRequestSize=1024*1024^2)
source("utils.R")

# Checks if the inputs to the launcher functions are defined
if (exists(".parameters")) {
  parameters <- .parameters
} else {
  parameters <- get(".parameters", envir = BCEA:::.bcea_env)  
}
if (exists(".e")) {
  e <- .e
} else {
  e <- get(".e", envir = BCEA:::.bcea_env)  
}
if (exists(".c")) {
  c <- .c
} else {
  c <- get(".c", envir = BCEA:::.bcea_env)  
}

# Shiny Server
function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  ###########################
  ### CHECK ASSUMPTIONS TAB #
  ###########################
  # Creates the list of parameter names
  shiny::observe({
    if(!is.null(parameters)){
      shiny::updateSelectInput(session,'parameter',choices=names(parameters))
    }
    if(input$from=="Spreadsheet"){
      rm(list=ls())
      shiny::updateSelectInput(session,'parameter',choices="")
      inFile_params <- input$par_sims_csv
      shiny::req(inFile_params)
      params <- read.csv(inFile_params$datapath, sep=",")
      shiny::updateSelectInput(session,'parameter',choices=names(params))
    }
  })
  
  param <- shiny::reactive({
    if(input$from=="R") {
      shiny::updateSelectInput(session,'parameter',choices="")
      parameters=parameters
      shiny::updateSelectInput(session,'parameter',choices=names(parameters))
    }
    if(input$from=="Spreadsheet"){
      shiny::req(input$par_sims_csv)
      inFile_params <- input$par_sims_csv
      parameters <- read.csv(inFile_params$datapath, sep=',')
    }
    if(input$from=="BUGS"){
      parameters <- NULL
    }
    return(parameters)
  })
  
  nm <- shiny::reactive({
    nms <- colnames(param())
  })
  
  colm <- shiny::reactive ({
    column <- which(nm() == input$parameter)
  })
  
  # Histogram and summary spreadsheet & R
  output$hist <- shiny::renderPlot ({
    shiny::req(param(),colm())
    if(length(var(param()[,colm()]))==0) {return(invisible())}
    if (var(param()[,colm()])==0) {return(invisible())}
    ylim=NULL
    shiny::withProgress("Creating plot", value = 1,message = "Creating plot,", detail = "Please wait...", {
      hist(param()[,colm()], breaks= seq(min(param()[,colm()]),max(param()[,colm()]),l = input$bins+1),
           main = paste("Histogram of",input$parameter),
           xlab = input$parameter, ylim = ylim)
      points(c(quantile(param()[,colm()],0.025),quantile(param()[,colm()],0.025),
               quantile(param()[,colm()],0.975)),c(0,0,0),lwd=5,type="l")
    })
  })
  
  output$summary <- shiny::renderTable({
    shiny::req(param(),colm())
    if(is.integer(colm()) && length(colm()) == 0L) {return(NULL)} else {
      mu <- mean(param()[,colm()]) #mean value
      sd <- sd(param()[,colm()])   #sd value
      q1<-quantile(param()[,colm()],0.025, names = FALSE)
      q2<-quantile(param()[,colm()],0.975, names = FALSE)
      med <- quantile(param()[,colm()],0.5, names = FALSE)
      ## Need to check the definition of the MCSE here!
      mcse <- sqrt(-((sum(param()[,colm()])/length(param()[,colm()]))^2 -
                       sum((param()[,colm()]^2)/length(param()[,colm()]))))/sqrt(length(param()[,colm()]))
      summary2<-data.frame(mu,sd,q1,med,q2,mcse)
      names(summary2) <- c("Mean","  Standard deviation","  2.5%","Median"," 97.5%","Monte Carlo SE")
      rownames(summary2) <- input$parameter
      quiet(print(format(summary2,digit=5,nsmall=3)))
    }
  })
  
  ##########################
  # Specific output for BUGS
  obj <- shiny::reactive({
    if (input$from!="BUGS") {
      obj <- list()
    }
    if (input$from=="BUGS") {
      # Checks if coda is installed (and if not, asks for them)
      if(!isTRUE(requireNamespace("coda",quietly=TRUE))) {
        stop("You need to install the R package 'coda'. Please run in your R terminal:\n install.packages('coda')")
      }
      rm(list=ls())
      shiny::updateSelectInput(session,'parameter',choices="")
      reqs <- c("input$codaIndex",paste0("input[['coda_chain",1:input$nchains,"']]"))
      shiny::req(eval(parse(text=reqs)))
      coda_obj <- vector("list", input$nchains)
      inp <- paste0(input$codaIndex$datapath)
      out <- unlist(lapply(1:input$nchains, function(i) input[[paste0('coda_chain',i)]]$datapath))
      for (i in 1:input$nchains) {
        coda_obj[[i]] <- coda::read.coda(out[i],inp,quiet=TRUE)
      }
      obj <- list(BUGSoutput=mcmc2bugs(coda::mcmc.list(coda_obj),program="jags"))
      shiny::updateSelectInput(session,'parameter',choices = colnames(obj$BUGSoutput$sims.matrix))
    }
    return(obj)
  })
  
  # Creates file input boxes --- as many as the user specifies chains from BUGS
  output$coda <- shiny::renderUI({
    lapply(1:input$nchains, function(i) {
      list(shiny::fileInput(paste0("coda_chain",i), paste("Choose coda file for chain",i), accept='.txt'))
    })
  })
  
  param2 <- shiny::reactive({
    shiny::req(obj())
    parameters <- obj()$BUGSoutput$sims.matrix
  })
  
  nm2 <- shiny::reactive({
    shiny::req(param2())
    nms <- colnames(param2())
  })
  
  colm2 <- shiny::reactive ({
    column <- which(nm2() == input$parameter)
  })
  
  # Histogram for BUGS
  output$hist2 <- shiny::renderPlot({
    shiny::req(param2(),colm2())
    if(length(var(param2()[,colm2()]))==0) {return(invisible())}
    if (var(param2()[,colm2()])==0) {return(invisible())}
    ylim=NULL
    shiny::withProgress("Creating plot", value = 1,message = "Creating plot,", detail = "Please wait...", {
      hist(param2()[,colm2()], breaks= seq(min(param2()[,colm2()]),max(param2()[,colm2()]),l = input$bins+1),
           main = paste("Histogram of",input$parameter),
           xlab = input$parameter, ylim = ylim)
      points(c(quantile(param2()[,colm2()],0.025),quantile(param2()[,colm2()],0.025),
               quantile(param2()[,colm2()],0.975)),c(0,0,0),lwd=5,type="l")
    })
  })
  
  output$trace <- shiny::renderPlot ({
    shiny::req(param2(),colm2())
    mytraceplot(input$parameter,obj())
  })
  
  output$gr <- shiny::renderPlot ({
    shiny::req(param2(),colm2())
    if(input$nchains>1) {plotGR(obj(),colm2())}
  })
  
  output$neff <- shiny::renderPlot({
    shiny::req(param2(),colm2())
    if(input$nchains>1){plot.neff(obj(),colm2())}
  })
  
  output$acf <- shiny::renderPlot({
    shiny::req(param2(),colm2())
    acf.plot(obj(),colm2())
  })
  
  output$summary2 <- shiny::renderTable({
    shiny::req(param2(),colm2())
    if(is.integer(colm2()) && length(colm2()) == 0L) {return(NULL)} else {
      tmp <- obj()$BUGSoutput$summary[colm2(),]
      summary2 <- data.frame(t(tmp))[-c(4,6)]
      rownames(summary2) <- input$parameter
      if(input$nchains>1){
        summary2[8] <- obj()$BUGSoutput$summary[colm2(),"sd"]/sqrt(obj()$BUGSoutput$summary[colm2(),"n.eff"])
        colnames(summary2) <- c("Mean","Standard deviation","2.5%","Median","97.5%","Rhat","n.eff","Monte Carlo SE")
      } else {
        colnames(summary2) <- c("Mean","Standard deviation","2.5%","Median","97.5%")
      }
      quiet(print(format(summary2,digit=5,nsmall=3)))
    }
  })
  
  
  ##########
  #BCEA tab#
  ##########
  shiny::observe({
    if(!is.null(e) & !is.null(c)){
      inputs=shiny::reactive({
        out=cbind(e,c)
      })
    }
    if(input$data=="R" & is.null(e) & is.null(c)){
      inputs=shiny::reactive({
        out=NULL
      })
    }
    if(input$data=="Spreadsheet"){
      inputs  <- shiny::reactive({
        inFile1 <- input$file1
        req(input$file1)
        ws = read.csv(inFile1$datapath,sep = ',', header = TRUE)
      })
    }
    if (input$data=="Model parameters (from Parameter simulations)") {
      inputs  <- shiny::reactive({
        if(input$from=="Spreadsheet" || input$from=="R") {
          psa.data <- param()
        }
        if(input$from=="BUGS") {
          psa.data <- param2()
        }
        input$import_e_c
        cs <- es <- character()
        for (i in 1:input$n.ints) {
          es[i] <- eval(parse(text=paste0("input$e",i)))
          cs[i] <- eval(parse(text=paste0("input$c",i)))
        }
        wc <- which(colnames(psa.data)%in%cs)
        we <- which(colnames(psa.data)%in%es)
        out <- cbind(psa.data[,wc],psa.data[,we])
      })
    }
    if (input$data=="BUGS") {
      inputs <- shiny::reactive({
        rm(list=ls())
        # Checks if coda is installed (and if not, asks for them)
        if(!isTRUE(requireNamespace("coda",quietly=TRUE))) {
          stop("You need to install the R package 'coda'. Please run in your R terminal:\n install.packages('coda')")
        }
        
        shiny::updateSelectInput(session,'parameter1',choices="")
        reqs <- c("input$codaIndex1",paste0("input[['coda_chain_no",1:input$nchains1,"']]"))
        shiny::req(eval(parse(text=reqs)))
        coda_obj1 <- vector("list", input$nchains1)
        inp1 <- paste0(input$codaIndex1$datapath)
        out1 <- unlist(lapply(1:input$nchains1, function(i) input[[paste0('coda_chain_no',i)]]$datapath))
        for (i in 1:input$nchains1) {
          coda_obj1[[i]] <- coda::read.coda(out1[i],inp1,quiet=TRUE)
        }
        tmp <- do.call(rbind,coda_obj1); row.names(tmp) <- NULL
        inputs <- tmp
        return(inputs)
      })
    }
    # Creates file input boxes --- as many as the user specifies chains from BUGS
    output$coda1 <- shiny::renderUI({
      lapply(1:input$nchains1, function(i) {
        list(shiny::fileInput(paste0("coda_chain_no",i), paste("Choose coda file for chain",i), accept='.txt'))
      })
    })
    
    ### Uploads the economic output (e,c) from the PSA runs
    output$select.e <- shiny::renderUI({
      if (input$from=="Spreadsheet" || input$from=="R") {names <- nm()}
      if (input$from=="BUGS") {names <- nm2()}
      lapply(1:input$n.ints, function(i) {
        list(shiny::selectInput(paste0("e",i),paste("Select e for intervention",i),
                                choices=names))
      })
    })
    output$select.c <- shiny::renderUI({
      if (input$from=="Spreadsheet" || input$from=="R") {names <- nm()}
      if (input$from=="BUGS") {names <- nm2()}
      lapply(1:input$n.ints, function(i) {
        list(shiny::selectInput(paste0("c",i),paste("Select c for intervention",i),
                                choices=names))
      })
    })
    
    # Defines dynamically the intervention labels
    labs <- shiny::reactive({
      if(input$data=="Model parameters (from Parameter simulations)") {
        numcols <- input$n.ints
        index <- seq(1,numcols)
        l <- unlist(lapply(index,function(i){input[[paste0('intervention_',i)]]}))
      } else {
        shiny::req(inputs())
        numcols <- ncol(inputs())
        index <- seq(1,numcols/2)
        l <- unlist(lapply(index,function(i){input[[paste0('intervention_',i)]]}))
      }
    })
    
    # Computes dynamically the BCEA object
    m <- shiny::eventReactive(
      input$buttonsum, {
        shiny::req(input$buttonsum,input$step,inputs(),input$min,input$max)
        wtp <- seq(input$min,input$max,by=input$step)
        n.cols <- ncol(inputs())
        odds <- seq(1,n.cols,by=2)
        even <- seq(2,n.cols,by=2)
        if(input$data=="R"){
          e <- e
          c <- c
        }
        if(input$data=="Spreadsheet"){
          e <- as.matrix(inputs()[,odds])
          c <- as.matrix(inputs()[,even])
        }
        if(input$data=="BUGS" || input$data=="Model parameters (from Parameter simulations)") {
          c <- as.matrix(inputs()[,1:(n.cols/2)])
          e <- as.matrix(inputs()[,((n.cols/2)+1):n.cols])
        }
        mm <- bcea(e,c,ref=as.numeric(value2()),interventions=labs(),wtp=wtp)
      }
    )
    
    output$grid_step <- shiny::renderUI({
      if (input$min==input$max) {
        shiny::numericInput("step","step",value=1,min=1, step=1)
      } else {
        shiny::numericInput("step","step",value = 100,min = 0, step = 100)
      }
    })
    
    # The step by which the wtp threshold can be changed depends on the grid step
    output$step <- shiny::renderUI({
      shiny::req(input$step,input$min,input$max)
      if (input$min==input$max) {
        shiny::numericInput("value1","3. Define value for the wtp threshold (eg £)",value=input$min,min=input$min,max=input$min)
      } else {
        if (input$max<25000) {
          shiny::numericInput("value1","3. Define value for the wtp threshold (eg £)",value=input$max,min=input$min,step=input$step)
        } else {
          shiny::numericInput("value1","3. Define value for the wtp threshold (eg £)",value=25000,
                              min=input$min,max=input$max,step=input$step)
        }
      }
    })
    
    # Dynamically defines the labels for the interventions
    output$int_labels <- shiny::renderUI({
      if(input$data=="Model parameters (from Parameter simulations)") {
        numcols <- input$n.ints
        lapply(1:(numcols), function(i) {
          list(shiny::textInput(paste0("intervention_",i),
                                label = "", #h5(strong("4. Interventions labels"))
                                value = paste0("Intervention",(i))))
        })
      } else {
        shiny::req(inputs())
        numcols <- ncol(inputs())  # number of columns = 2 x number of interventions
        lapply(1:(numcols/2), function(i) {
          list(shiny::textInput(paste0("intervention_",i),
                                label = "", #h5(strong("4. Interventions labels"))
                                value = paste0("Intervention",(i))))
        })
      }
    })
    
    # Selects the reference intervention
    output$sel_ref <- shiny::renderUI({
      shiny::req(inputs())
      numcols <- ncol(inputs())  # number of columns = 2 x number of interventions
      shiny::selectInput("value_ref",shiny::h5(shiny::strong("5. Select reference intervention")),
                         choices=labs(),selected=labs()[1])
    })
    
    value2 <- shiny::reactive({
      v <- which(input$value_ref==labs())
    })
    
    
    # runs BCEA
    output$analysis <- shiny::renderPrint({
      input$buttonsum
      shiny::req(inputs(),m())
      shiny::withProgress(
        shiny::isolate(
          summary(m(),wtp = input$value1)
        ),
        value = 1, message = "Making the analysis,", detail = "Please wait..."
      )
    })
    
    #CE-plane for varying WTP values -> wtp= value
    output$cep <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(m(),inputs(),input$value1)
      if (m()$n.comparators>2) {
        if (input$which_comparison==comparisons()[1]) {
          shiny::withProgress(
            suppressMessages(contour2(m(),wtp = input$value1, graph="gg")),  # uses ggplot2 for multiple treatments
            value = 1, message = "Creating plot,", detail = "Please wait..."
          )
        } else {
          shiny::withProgress(
            suppressMessages(contour2(m(),wtp = input$value1,
                                      comparison=(which(comparisons()==input$which_comparison)-1),graph="gg")),
            value = 1, message = "Creating plot,", detail = "Please wait..."
          )
        }
      } else {
        shiny::withProgress(
          (suppressMessages(contour2(m(),wtp = input$value1))),
          value = 1, message = "Creating plot,", detail = "Please wait..."
        )
      }
    })
    
    comparisons <- shiny::reactive({
      cc <- c("All",paste(labs()[m()$ref]," vs ",labs()[m()$comp],collate="",sep=""))
    })
    output$other_CEA <- shiny::renderUI({
      shiny::req(m())
      if (m()$n.comparisons>1) {
        shiny::selectInput("which_comparison","Select comparison to plot",
                           choices=comparisons(),selected=comparisons()[1])
      }
    })
    
    # EIB plot
    output$eib <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs(),m(),input$max,input$step)
      # If the willingness to pay grid is only one number then don't print the plots
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      shiny::withProgress(
        eib.plot(m()),
        value = 1, message = "Creating plot,", detail = "Please wait..."
      )
      legend("topleft",lty = 2, "95 % CI")
    })
    
    #ceef plot
    output$ceef <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs(),m())
      shiny::withProgress(
        suppressMessages(ceef.plot(m(), print.summary = FALSE, print.plot=TRUE)),
        value = 1, message = "Creating plot,", detail = "Please wait..."
      )
    })
    
    # ceef output
    output$analysisc <- shiny::renderPrint({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs(),m())
      shiny::withProgress(
        suppressMessages(ceef.plot(m(),print.plot=FALSE,print.summary=TRUE)),
        value = 1, message = "Creating plot,", detail = "Please wait..."
      )
    })
    
    #########
    #PSA tab#
    #########
    
    #CEAC for varying WTP values
    output$ceac <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs(),input$step,input$min,input$max)
      # If the willingness to pay grid is only one number then don't print the plots
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      shiny::withProgress(
        ceac.plot(m()),
        value = 1, message = "Creating plot,", detail = "Please wait..."
      )
    })
    
    output$multi_ceac <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs())
      # If the willingness to pay grid is only one number then don't print the plots
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      shiny::withProgress(
        mce.plot(mce()),
        value = 1, message = "Creating plot,", detail = "Please wait..."
      )
    })
    
    #ceaf
    mce <- shiny::reactive({
      mc <- multi.ce(m())
    })
    output$ceaf <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs())
      # If the willingness to pay grid is only one number then don't print the plots
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      shiny::withProgress({
        suppressMessages(ceaf.plot(mce()))},
        value = 1, message = "Creating plot,", detail = "Please wait...")
    })
    
    tab_ceac <- shiny::reactive({
      tab_ceac <- data.frame("Willingness to pay"=m()$k,"CEAC"=m()$ceac)
    })
    output$download_CEAC_table <- shiny::downloadHandler(
      filename = function() { "ceac_tab.csv" },
      content = function(file) {
        write.csv(tab_ceac(), file,row.names=FALSE)
      }
    )
    
    tab_ceaf <- shiny::reactive({
      tab_ceaf <- data.frame("Willingness to pay"=mce()$k,"CEAF"=mce()$ceaf)
    })
    output$download_CEAF_table <- shiny::downloadHandler(
      filename = function() { "ceaf_tab.csv" },
      content = function(file) {
        write.csv(tab_ceaf(), file,row.names=FALSE)
      }
    )
    
    
    ##########################
    #Value of information tab#
    ##########################
    
    #EVPI
    output$evpi <- shiny::renderPlot({
      if(input$data=="Spreadsheet"){shiny::req(input$file1)}
      shiny::req(inputs())
      # If the willingness to pay grid is only one number then don't print the plots
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      shiny::withProgress(evi.plot(m()), value = 1, message = "Creating plot,", detail = "Please wait...")
    })
    
    output$evppi_pars <- shiny::renderUI({
      shiny::req(inputs())
      if (input$from=="Spreadsheet" || input$from=="R") {names <- nm()}
      if (input$from=="BUGS") {names <- nm2()}
      if (input$run_info_rank==1) {names <- as.character(make_info_rank()$rank[,1])}
      shiny::selectInput("evppi_parameters","2. Select parameters to compute the EVPPI",
                         choices=names,selected=NULL,multiple=TRUE)
    })
    
    compute_evppi <- shiny::reactive({
      input$run_evppi
      shiny::isolate({
        shiny::withProgress({
          if (is.null(inputs())) {return(invisible())}
          if (is.null(input$evppi_parameters)) {return(invisible())}
          if (input$run_evppi==0) {return(invisible())}
          if (input$from=="Spreadsheet" || input$from=="R") {
            input_data <- param()
            n_sims <- dim(param())[1]
          }
          if (input$from=="BUGS") {
            input_data <- param2()
            n_sims <- dim(param2())[1]
          }
          ## Checks for the method used
          met <- "INLA"
          # Forces method to GAM even if INLA-SPDE is specified, when the number of parameters is 1
          check1a <- input$which_method=="INLA-SPDE"
          check1b <- length(input$evppi_parameters)<2
          check2 <- input$which_method=="GP regression"
          check3a <- input$which_method=="GAM regression (up to 5 parameters)"
          check3b <- length(input$evppi_parameters)<6
          if ((check1a & check1b)==TRUE) {
            shiny::updateSelectInput(session,"which_method",selected="GAM regression (up to 5 parameters)")
            met="GAM"
          }
          if ((check3a & check3b)==TRUE) {
            met="GAM"
          }
          # Allows the user to use the standard GP regression
          if (check2==TRUE) {
            met="GP"
          }
          if (input$formula_gam=="Full interaction") {
            regr.mod <- paste("te(",paste(input$evppi_parameters,",",sep="",collapse=""),"bs='cr')")
          }
          if (input$formula_gam=="Separate") {
            regr.mod <- paste("s(",input$evppi_parameters,")",collapse="+")
          }
          form <- gsub("[",".",regr.mod,fixed=TRUE)
          form <- gsub("]",".",form,fixed=TRUE)
          
          # This prevents shiny from showing the output of the cat texts from the call to BCEA::evppi
          quiet(x <- evppi(input$evppi_parameters,input_data,m(),method=met,
                           residuals=TRUE,N=input$how_many_sims,
                           # These are the method-specific options
                           n.sim=input$sim_hyper,
                           formula=form,
                           int.ord=c(input$int.ord1,input$int.ord2), #input$formula_inla,
                           cutoff=(2*0.3-input$cutoff_inla),
                           convex.outer=-input$convex_out,
                           convex.inner=-input$convex_in,
                           h.value = (1-input$h_value)/1000))
        },
        value = 1, message = "Running analysis,", detail = "Please wait...")
      })
    })
    
    output$evppi <- shiny::renderPlot({
      shiny::req(inputs(),input$evppi_parameters)
      # If the willingness to pay grid is only one number then don't print the plots
      shiny::req(input$min,input$max,input$step)
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      shiny::req(compute_evppi())
      plot(compute_evppi())
      reset_num_sims()
    })
    
    output$diag_evppi <- shiny::renderPlot({
      shiny::req(inputs(),input$evppi_parameters,input$which_diag)
      # If the willingness to pay grid is only one number then don't print the plots
      wtp <- seq(input$min,input$max,by=input$step)
      if (length(wtp)==1) {return(invisible)}
      #        if (input$which_diag!="Mesh") {
      if (input$which_diag=="Residual plot") {diag_type="residuals"} else {diag_type="qqplot"}
      shiny::withProgress(
        diag.evppi(compute_evppi(), m(), diag=diag_type),
        min=0, max=1, value = 1, message = "Creating plot,", detail = "Please wait..."
      )
    })
    
    output$num_sims <- shiny::renderUI({
      shiny::req(inputs(),input$evppi_parameters)
      
      if (input$from=="Spreadsheet" || input$from=="R") {
        n_sims <- dim(param())[1]
      }
      if (input$from=="BUGS") {
        n_sims <- dim(param2())[1]
      }
      n_sims2 <- ifelse(n_sims>1000,1000,n_sims)
      shiny::numericInput("how_many_sims",
                          paste0("3. Select the number of PSA runs to be used (max=",n_sims,")"),
                          value=n_sims2,max=n_sims,step=input$step)
    })
    reset_num_sims <- shiny::reactive({
      # Prevents from selecting more PSA runs than there are available
      if (input$how_many_sims > m()$n.sim) {
        shiny::updateNumericInput(session,'how_many_sims',value=m()$n.sim)
      }
    })
    
    output$option_GP <- shiny::renderUI({
      shiny::numericInput("sim_hyper","a. Number of PSA runs to estimate the hyperparameters",
                          value=500,
                          min=m()$n.sim/2,max=m()$n.sim)
    })
    
    output$wtp_values <- shiny::renderUI({
      shiny::req(inputs(),input$evppi_parameters)
      if (input$run_evppi==0) {return(invisible())}
      shiny::selectInput("wtp_grid","Value of the wtp grid (eg £)",choices=m()$k,selected=input$value1)
    })
    
    output$wtp_values2 <- shiny::renderUI({
      shiny::req(inputs(),m())
      shiny::selectInput("wtp_grid2","Value of the wtp grid (eg £)",choices=m()$k,selected=input$value1)
    })
    
    output$wtp_values3 <- shiny::renderUI({
      shiny::req(inputs(),mce())
      shiny::selectInput("wtp_grid3","Value of the wtp grid (eg £)",choices=mce()$k,selected=input$value1)
    })
    
    output$wtp_values4 <- renderUI({
      shiny::req(inputs(),m())
      shiny::selectInput("wtp_grid4","Value of the wtp grid (eg £)",choices=m()$k,
                         selected=input$value1)
    })
    
    output$wtp_values5 <- shiny::renderUI({
      shiny::req(inputs())
      shiny::selectInput("wtp_grid5", "2. Select the wtp",choices=m()$k,
                         selected=m()$k[min(which(m()$k>=m()$ICER))])
    })
    
    output$N_values <- shiny::renderUI({
      shiny::req(inputs())
      if (input$from=="Spreadsheet" || input$from=="R") {
        n_sims <- dim(param())[1]
      }
      if (input$from=="BUGS") {
        n_sims <- dim(param2())[1]
      }
      n_sims_ir <- ifelse(n_sims>1000,1000,n_sims)
      shiny::req(n_sims)
      shiny::numericInput("how_many_sims_ir",
                          paste0("3. Select number of PSA runs (max=",n_sims,")"),
                          value=n_sims_ir,max=n_sims,step=input$step)
    })
    
    output$ceac_values <- shiny::renderPrint({
      shiny::req(inputs(),m())
      shiny::HTML(
        paste0(shiny::p(shiny::h5(shiny::strong("Value of the CEAC"))),'</p>',format(m()$ceac[which(m()$k==input$wtp_grid2)],digit=6,nsmall=4))
      )
    })
    
    output$evpi_values <- shiny::renderPrint({
      shiny::req(inputs(),m())
      shiny::HTML(
        paste0(shiny::p(shiny::h5(shiny::strong("Value of the EVPI"))),'</p>',format(m()$evi[which(m()$k==input$wtp_grid4)],digit=6,nsmall=4))
      )
    })
    
    output$ceaf_values <- shiny::renderPrint({
      shiny::req(inputs(),mce())
      shiny::HTML(
        paste0(shiny::p(shiny::h5(shiny::strong("Value of the CEAF"))),'</p>',format(mce()$ceaf[which(mce()$k==input$wtp_grid3)],digit=6,nsmall=4))
      )
    })
    
    output$evppi_values <- shiny::renderPrint({
      shiny::req(inputs(),input$evppi_parameters)
      if (input$run_evppi==0) {return(invisible())}
      shiny::tags$div(
        shiny::tags$h5(shiny::tags$strong("Value of the EVPPI")),
        format(compute_evppi()$evppi[which(compute_evppi()$k==input$wtp_grid)],digit=6,nsmall=4)
      )
    })
    
    output$details_evppi <- shiny::renderPrint({
      shiny::req(inputs(),input$evppi_parameters)
      if (input$run_evppi==0) {return(invisible())}
      shiny::tags$table(
        style="padding:3px",
        #class = "table-condensed table-bordered",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Running time (seconds)"),
            shiny::tags$th("")
          )
        ),
        shiny::tags$tbody(
          shiny::tags$tr(
            shiny::tags$td("Fitting for effects"),
            shiny::tags$td(
              style="text-align:right",
              format(compute_evppi()$time[1],digits=6,nsmall=4))
          ),
          shiny::tags$tr(
            shiny::tags$td("Fitting for costs"),
            shiny::tags$td(
              style="text-align:right",
              format(compute_evppi()$time[2],digits=6,nsmall=4))
          ),
          shiny::tags$tr(
            style="border-bottom: 1pt solid black",
            shiny::tags$td("Computing EVPPI"),
            shiny::tags$td(
              style="text-align:right",
              format(compute_evppi()$time[3],digits=6,nsmall=4))
          ),
          shiny::tags$tr(
            shiny::tags$td("Total"),
            shiny::tags$td(
              style="text-align:right",
              format(sum(unlist(compute_evppi()$time)),digits=6,nsmall=4))
          )
        )
      )
    })
    
    tab <- shiny::reactive({
      tab <- data.frame("Willingness to pay"=compute_evppi()$k,"EVPPI"=compute_evppi()$evppi,"EVPI"=compute_evppi()$evi)
    })
    output$download_EVPPI_table <- shiny::downloadHandler(
      filename = function() { "evppi_tab.csv" },
      content = function(file) {
        write.csv(tab(), file,row.names=FALSE)
      }
    )
    
    tab_evpi <- shiny::reactive({
      tab_evpi <- data.frame("Willingness to pay"=m()$k,"EVPI"=m()$evi)
    })
    output$download_EVPI_table <- shiny::downloadHandler(
      filename = function() { "evpi_tab.csv" },
      content = function(file) {
        write.csv(tab_evpi(), file,row.names=FALSE)
      }
    )
    
    output$select_diag <- shiny::renderUI({
      shiny::req(inputs(),input$evppi_parameters)
      if (input$run_evppi==0) {return(invisible())}
      if (input$which_method=="INLA-SPDE") {
        shiny::selectInput("which_diag","Select diagnostic tool",
                           choices=c("Residual plot","QQ-plot","Mesh"),
                           selected="Residual plot")
      } else {
        shiny::selectInput("which_diag","Select diagnostic tool",
                           choices=c("Residual plot","QQ-plot"),
                           selected="Residual plot")
      }
    })

    output$info_rank_pars <- shiny::renderUI({
      shiny::req(inputs())
      if (input$from=="Spreadsheet" || input$from=="R") {names <- nm()}
      if (input$from=="BUGS") {names <- nm2()}
      shiny::selectInput("info_rank_parameters","1. Select relevant parameters",
                         choices=c("All parameters",names),selected="NULL",multiple=TRUE)
    })
    
    make_info_rank <- shiny::eventReactive(
      input$run_info_rank, {
        shiny::req(input$run_info_rank,inputs(),input$info_rank_parameters)
        shiny::isolate({
          if (input$from=="Spreadsheet" || input$from=="R") {
            input_data_ir <- param()
            names.par <- colnames(input_data_ir)
          }
          if (input$from=="BUGS") {
            input_data_ir <- param2()
            names.par <- colnames(input_data_ir)
          }
          if(length(input$info_rank_parameters)==1){
            if(input$info_rank_parameters=="All parameters") {
              rel.pars <- names.par
            } else {
              rel.pars <- input$info_rank_parameters
            }
          } else {
            rel.pars <- input$info_rank_parameters
          }
          wtp <- as.numeric(input$wtp_grid5)
          info.rank(parameter=rel.pars,input=input_data_ir,he=m(),wtp=wtp,cn=.8,ca=.8,N=input$how_many_sims_ir)
        })
      }
    )

    output$ir <- shiny::renderPlot({
      shiny::req(inputs(),input$info_rank_parameters)
      shiny::withProgress({make_info_rank()},
                          min=0, max=1, value = 1,
                          message = "Creating plot,", detail = "Please wait..."
      )
    })
    
    output$details_ir <- shiny::renderPrint({ #Table
      shiny::req(inputs(),input$info_rank_parameters)
      tab <- make_info_rank()$rank
      npars <- dim(tab)[1]
      nrows.to.show <- min(npars,6)
      tab <- tab[1:nrows.to.show,]
      shiny::tags$table(
        style="padding:3px",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Parameter"),
            shiny::tags$th("Info value",
                           style="text-align:right"),
            style="border-bottom: 1pt solid black"
          )
        ),
        shiny::tags$tbody(
          lapply(1:nrows.to.show, function(i) {
            shiny::tags$tr(
              shiny::tags$td(tab[i,1],
                             style="padding: 0px 15px 0px 0px;"),
              shiny::tags$td(
                style="text-align:right",
                format(tab[i,2],digits=6,nsmall=4)
              )
            )
          })
        )
      )
    })
    
    tab_ir <- shiny::reactive({
      tab_ir <- make_info_rank()$rank
    })
    output$download_IR_table <- shiny::downloadHandler(
      filename = function() { "InfoRank_tab.csv" },
      content = function(file) {
        write.csv(tab_ir(), file,row.names=FALSE)
      }
    )
    
    ############
    #Report tab#
    ############
    {output$downloadReport <- shiny::downloadHandler(
      filename = function() {
        paste('BCEAweb-report', sep = '.',
              switch(input$format, PDF = 'pdf', Word = 'docx'
              ))
      },
      
      content = function(file) {
        # Checks if knitr is installed (and if not, asks for it)
        if(!isTRUE(requireNamespace("knitr",quietly=TRUE))) {
          stop("You need to install the R package 'knitr'.Please run in your R terminal:\n install.packages('knitr')")
        }
        knitr::opts_knit$set(progress = FALSE, verbose = FALSE)
        # Checks if rmarkdown is installed (and if not, asks for it)
        if(!isTRUE(requireNamespace("rmarkdown",quietly=TRUE))) {
          stop("You need to install the R package 'rmarkdown'.Please run in your R terminal:\n install.packages('rmarkdown')")
        }
        
        src <- normalizePath('report.Rmd')
        out <- quiet(rmarkdown::render('report.Rmd',
                                       switch(input$format,PDF = rmarkdown::pdf_document(), Word = rmarkdown::word_document(),envir=.bcea_env)
        ))
        file.copy(out, file)
      }
    )}
  })
}
