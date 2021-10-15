pkgname <- "BCEA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "BCEA-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('BCEA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CEriskav_assign")
### * CEriskav_assign

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CEriskav_assign
### Title: Cost-effectiveness Analysis Including a Parameter of Risk
###   Aversion
### Aliases: CEriskav_assign CEriskav<- CEriskav CEriskav.default
###   CEriskav<-.bcea CEriskav<-.default

### ** Examples


# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000            # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
)

# Define the vector of values for the risk aversion parameter, r, eg:
r <- c(1e-10, 0.005, 0.020, 0.035) 

# Run the cost-effectiveness analysis accounting for risk aversion
## No test: 
# uses the results of the economic evaluation 
# if more than 2 interventions, selects the 
#  pairwise comparison

CEriskav(m) <- r
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CEriskav_assign", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Smoking")
### * Smoking

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Smoking
### Title: Data set for the Bayesian model for the cost-effectiveness of
###   smoking cessation interventions
### Aliases: Smoking c data e life.years pi smoking smoking_output treats
### Keywords: datasets

### ** Examples


data(Smoking)

## No test: 
m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Smoking", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Vaccine")
### * Vaccine

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Vaccine
### Title: Data set for the Bayesian model for the cost-effectiveness of
###   influenza vaccination
### Aliases: Vaccine c.pts cost.GP cost.hosp cost.otc cost.time.off
###   cost.time.vac cost.travel cost.trt1 cost.trt2 cost.vac e.pts N
###   N.outcomes N.resources QALYs.adv QALYs.death QALYs.hosp QALYs.inf
###   QALYs.pne vaccine vaccine_mat
### Keywords: datasets

### ** Examples


data(Vaccine)

## No test: 
m <- bcea(e, c, ref = 1, interventions = treats)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Vaccine", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bcea")
### * bcea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bcea
### Title: Create Bayesian Cost-Effectiveness Analysis Object
### Aliases: bcea bcea.default bcea.rjags bcea.rstan bcea.bugs
### Keywords: manip

### ** Examples

# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(
      e=e,
      c=c,                  # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
      plot=TRUE             # plots the results
)

# Creates a summary table
summary(
      m,         # uses the results of the economic evaluation 
                 #  (a "bcea" object)
      wtp=25000	# selects the particular value for k 
)

## No test: 

# Plots the cost-effectiveness plane using base graphics
ceplane.plot(
      m,             # plots the Cost-Effectiveness plane
      comparison=1,  # if more than 2 interventions, selects the
                     #  pairwise comparison 
      wtp=25000,     # selects the relevant willingness to pay 
                     #  (default: 25,000) 
      graph="base"   # selects base graphics (default)
)

# Plots the cost-effectiveness plane using ggplot2
if (requireNamespace("ggplot2")) {
ceplane.plot(
      m,             # plots the Cost-Effectiveness plane
      comparison=1,  # if more than 2 interventions, selects the
                     #  pairwise comparison 
      wtp=25000,     # selects the relevant willingness to pay 
                     #  (default: 25,000) 
      graph="ggplot2"# selects ggplot2 as the graphical engine
)

# Some more options
ceplane.plot(
      m,
      graph="ggplot2",
      pos="top",
      size=5,
      ICER_size=1.5,
      label.pos=FALSE,
      opt.theme=ggplot2::theme(text=ggplot2::element_text(size=8))
)
}

# Plots the contour and scatterplot of the bivariate 
# distribution of (Delta_e,Delta_c)
contour(
      m,            # uses the results of the economic evaluation 
                    #  (a "bcea" object)
      comparison=1, # if more than 2 interventions, selects the 
                    #  pairwise comparison 
      nlevels=4,    # selects the number of levels to be 
                    #  plotted (default=4)
      levels=NULL,  # specifies the actual levels to be plotted 
                    #  (default=NULL, so that R will decide)
      scale=0.5,    # scales the bandwidths for both x- and 
                    #  y-axis (default=0.5)
      graph="base"  # uses base graphics to produce the plot
)

# Plots the contour and scatterplot of the bivariate 
#   distribution of (Delta_e,Delta_c)
contour2(
      m,          # uses the results of the economic evaluation 
                  #  (a "bcea" object)
      wtp=25000,  # selects the willingness-to-pay threshold
      xlim=NULL,    # assumes default values
      ylim=NULL     # assumes default values
)

# Using ggplot2
if (requireNamespace("ggplot2")) {
contour2(
      m,              # uses the results of the economic evaluation 
                      #  (a "bcea" object)
      graph="ggplot2",# selects the graphical engine
      wtp=25000,      # selects the willingness-to-pay threshold
      xlim=NULL,      # assumes default values
      ylim=NULL,      # assumes default values
      label.pos=FALSE # alternative position for the wtp label
)
}

# Plots the Expected Incremental Benefit for the "bcea" object m
eib.plot(m)

# Plots the distribution of the Incremental Benefit
ib.plot(
    m,            # uses the results of the economic evaluation 
                  #  (a "bcea" object)
    comparison=1, # if more than 2 interventions, selects the 
                  #  pairwise comparison 
    wtp=25000,    # selects the relevant willingness 
                  #  to pay (default: 25,000)
    graph="base"  # uses base graphics
)

# Produces a plot of the CEAC against a grid of values for the 
# willingness to pay threshold
ceac.plot(m)

# Plots the Expected Value of Information for the "bcea" object m
evi.plot(m)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bcea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ceac.plot")
### * ceac.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ceac.plot.bcea
### Title: Cost-Effectiveness Acceptability Curve (CEAC) Plot
### Aliases: ceac.plot.bcea ceac.plot
### Keywords: hplot

### ** Examples

data("Vaccine")
he <- BCEA::bcea(e, c)
ceac.plot(he)

ceac.plot(he, graph = "base")
ceac.plot(he, graph = "ggplot2")
ceac.plot(he, graph = "plotly")

ceac.plot(he, graph = "ggplot2",
          title = "my title",
          line = list(colors = "green"),
          theme = ggplot2::theme_dark())

## more interventions
he2 <- BCEA::bcea(cbind(e, e - 0.0002), cbind(c, c + 5))
mypalette <- RColorBrewer::brewer.pal(3, "Accent")
ceac.plot(he2, graph = "ggplot2",
          title = "my title",
          theme = ggplot2::theme_dark(),
          pos = TRUE,
          line = list(colors = mypalette))
ceac.plot(he, graph = "base", title = "my title", line = list(colors = "green"))
ceac.plot(he2, graph = "base")

ceac.plot(he2, graph = "plotly", pos = "bottom")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ceac.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ceaf.plot")
### * ceaf.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ceaf.plot.pairwise
### Title: Cost-Effectiveness Acceptability Frontier (CEAF) plot
### Aliases: ceaf.plot.pairwise ceaf.plot
### Keywords: hplot

### ** Examples


# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(
      e=e,
      c=c,                  # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
      plot=FALSE            # inhibits graphical output
)

## No test: 
mce <- multi.ce(m)          # uses the results of the economic analysis 
## End(No test)

## No test: 
ceaf.plot(mce)              # plots the CEAF 
## End(No test)

## No test: 
ceaf.plot(mce, graph = "g") # uses ggplot2 
## End(No test)

## No test: 
# Use the smoking cessation dataset
data(Smoking)
m <- bcea(e, c, ref = 4, intervention = treats, Kmax = 500, plot = FALSE)
mce <- multi.ce(m)
ceaf.plot(mce)
## End(No test)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ceaf.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ceef.plot")
### * ceef.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ceef.plot.bcea
### Title: Cost-Effectiveness Efficiency Frontier (CEAF) Plot
### Aliases: ceef.plot.bcea ceef.plot

### ** Examples


## create the bcea object m for the smoking cessation example
data(Smoking, package = "BCEA")
m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)

## produce plot
ceef.plot(m, graph = "base")

## No test: 
## tweak the options
## flip axis
ceef.plot(m,
          flip = TRUE,
          dominance = FALSE,
          start.from.origins = FALSE,
          print.summary = FALSE,
          graph = "base")
          
## or use ggplot2 instead
if(require(ggplot2)){
ceef.plot(m,
          dominance = TRUE,
          start.from.origins = FALSE,
          pos = TRUE,
          print.summary = FALSE,
          graph = "ggplot2")
 }
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ceef.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ceplane.plot")
### * ceplane.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ceplane.plot.bcea
### Title: Cost-effectiveness Plane Plot
### Aliases: ceplane.plot.bcea ceplane.plot
### Keywords: hplot

### ** Examples

## create the bcea object for the smoking cessation example
data(Smoking)

m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)

## produce the base plot
ceplane.plot(m, wtp = 200, graph = "base")

## select only one comparator
ceplane.plot(m, wtp = 200, graph = "base", comparison = 3)

## use ggplot2
if (requireNamespace("ggplot2")) {
   ceplane.plot(m, wtp = 200, pos = "right", ICER_size = 2, graph = "ggplot2")
}

## plotly
ceplane.plot(m, wtp = 200, graph = "plotly")
ceplane.plot(m, wtp = 200, comparison = 1, graph = "plotly")
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ceplane.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ceplane_plot_graph")
### * ceplane_plot_graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ceplane_plot_graph
### Title: Cost-Effectiveness Plane Plot By Graph Device
### Aliases: ceplane_plot_graph ceplane_plot_base.bcea ceplane_plot_base
###   ceplane_plot_ggplot.bcea ceplane_plot_ggplot ceplane_plot_plotly.bcea
###   ceplane_plot_plotly
### Keywords: hplot

### ** Examples

# single comparator
data(Vaccine, package = "BCEA")

he <- bcea(e, c)
ceplane.plot(he, graph = "base")

## Not run: 
##D # need to provide all the defaults because thats what
##D # ceplane.plot() does
##D 
##D graph_params <-  list(xlab = "x-axis label",
##D                       ylab = "y-axis label",
##D                       title = "my title",
##D                       xlim = c(-0.002, 0.001),
##D                       ylim = c(-13, 5),
##D                       point = list(sizes = 1,
##D                                    colors = "darkgrey"),
##D                       area = list(color = "lightgrey"))
##D                       
##D he$delta_e <- as.matrix(he$delta_e)
##D he$delta_c <- as.matrix(he$delta_c)
##D 
##D BCEA::ceplane_plot_base(he, graph_params = graph_params)
##D 
##D ## single non-default comparator
##D 
##D 
##D ## multiple comparators
##D data(Smoking, package = "BCEA")
##D 
##D graph_params <-  list(xlab = "x-axis label",
##D                       ylab = "y-axis label",
##D                       title = "my title",
##D                       xlim = c(-1, 2.5),
##D                       ylim = c(-1, 160),
##D                       point = list(sizes = 0.5,
##D                                    colors = grey.colors(3, start = 0.1, end = 0.7)),
##D                       area = list(color = "lightgrey"))
##D                                    
##D he <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
##D 
##D BCEA::ceplane_plot_base(he,
##D                        wtp = 200,
##D                        pos_legend = FALSE,
##D                        graph_params = graph_params)
## End(Not run)


data(Vaccine)
he <- bcea(e, c)

ceplane.plot(he, graph = "ggplot2")

data(Smoking)
he <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)

ceplane.plot(he, graph = "ggplot2")

ceplane.plot(he,
             wtp = 200,
             pos = "right",
             ICER_size = 2,
             graph = "ggplot2")
   
ceplane.plot(he,
             wtp = 200,
             pos = TRUE,
             graph = "ggplot2")

ceplane.plot(he,
             graph = "ggplot2",
             wtp=200,
             theme = ggplot2::theme_linedraw())
             



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ceplane_plot_graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("contour")
### * contour

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: contour.bcea
### Title: Contour Plots for the Cost-Effectiveness Plane
### Aliases: contour.bcea contour
### Keywords: "Bayesian "Health economic evaluation" model"

### ** Examples

data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(e=e,
          c=c,              # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=TRUE             # plots the results
)

contour(m)
contour(m, graph = "ggplot2")

# Plots the contour and scatterplot of the bivariate 
# distribution of (Delta_e, Delta_c)
contour(m,          # uses the results of the economic evaluation 
                    #  (a "bcea" object)
      comparison=1, # if more than 2 interventions, selects the 
                    #  pairwise comparison 
      nlevels=4,    # selects the number of levels to be 
                    #  plotted (default=4)
      levels=NULL,  # specifies the actual levels to be plotted 
                    #  (default=NULL, so that R will decide)
      scale=0.5,    # scales the bandwidths for both x- and 
                    #  y-axis (default=0.5)
      graph="base"  # uses base graphics to produce the plot
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("contour", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("contour2")
### * contour2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: contour2.bcea
### Title: Specialised CE-plane Contour Plot
### Aliases: contour2.bcea contour2
### Keywords: "Bayesian "Health economic evaluation" model"

### ** Examples

## create the bcea object m for the smoking cessation example
data(Smoking)
m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

## produce the plot
contour2(m,
         wtp = 200,
         graph_type = "base")

## No test: 
## or use ggplot2 to plot multiple comparisons
contour2(m,
         wtp = 200,
         ICER_size = 2,
         graph_type = "ggplot2")
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("contour2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("eib.plot")
### * eib.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: eib.plot.bcea
### Title: Expected Incremental Benefit (EIB) Plot
### Aliases: eib.plot.bcea eib.plot
### Keywords: "Expected "Health Benefit" Incremental economic evaluation"

### ** Examples

data(Vaccine)
 
# Runs the health economic evaluation using BCEA
m <- bcea(
      e=e,
      c=c,                  # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
      plot=FALSE             # plots the results
)
eib.plot(m)
eib.plot(m, graph = "ggplot2") + ggplot2::theme_linedraw()

data(Smoking)
treats <- c("No intervention", "Self-help",
            "Individual counselling", "Group counselling")
m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
eib.plot(m)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("eib.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evi.plot")
### * evi.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evi.plot.bcea
### Title: Expected Value of Information (EVI) Plot
### Aliases: evi.plot.bcea evi.plot
### Keywords: Expected Health economic evaluation information of value

### ** Examples

data(Vaccine)
m <- bcea(
      e=e,
      c=c,                  # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
      plot=FALSE            # plots the results
)
evi.plot(m)

data(Smoking)
treats <- c("No intervention", "Self-help",
            "Individual counselling", "Group counselling")
m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
evi.plot(m)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evi.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evi.plot.mixedAn")
### * evi.plot.mixedAn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evi.plot.mixedAn
### Title: EVI Plot of the Health Economic Analysis For Mixed Analysis
### Aliases: evi.plot.mixedAn

### ** Examples


# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem
#
# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=FALSE            # inhibits graphical output
)

mixedAn(m) <- NULL      # uses the results of the mixed strategy 
                        #  analysis (a "mixedAn" object)
                        # the vector of market shares can be defined 
                        #  externally. If NULL, then each of the T 
                        #  interventions will have 1/T market share
                        # produces the plots
evi.plot(m)

evi.plot(m, graph="base")

# Or with ggplot2
if (require(ggplot2)) {
   evi.plot(m, graph="ggplot2")
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evi.plot.mixedAn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evppi")
### * evppi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evppi
### Title: Expected Value of Perfect Partial Information (EVPPI) for
###   Selected Parameters
### Aliases: evppi evppi.default evppi.bcea

### ** Examples

# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine, package = "BCEA")
treats <- c("Status quo", "Vaccination")

# Run the health economic evaluation using BCEA
m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)

# Compute the EVPPI for a bunch of parameters
inp <- createInputs(vaccine)

EVPPI <- evppi(m, c("beta.1." , "beta.2."), inp$mat)
plot(EVPPI)

# deprecated (single parameter) methods
EVPPI.so <- evppi(m, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
EVPPI.sad <- evppi(m, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)

##TODO:
#plot(EVPPI.so)
#plot(EVPPI.sad)

# Compute the EVPPI using INLA/SPDE
x_inla <- evppi(he = m, 39:40, input = inp$mat)

# using GAM regression
x_gam <- evppi(he = m, 39:40, input = inp$mat, method = "GAM")

# using Strong et al GP regression
x_gp <- evppi(he = m, 39:40, input = inp$mat, method = "GP")

# plot results
plot(x_inla)
points(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
points(x_gam$k, x_gam$evppi, type = "l", col = "red")
points(x_gp$k, x_gp$evppi, type = "l", col = "blue")

plot(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
points(x_gam$k, x_gam$evppi, type = "l", col = "red")
points(x_gp$k, x_gp$evppi, type = "l", col = "blue")

data(Smoking)
treats <- c("No intervention", "Self-help",
"Individual counselling", "Group counselling")
m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
inp <- createInputs(smoking_output)
EVPPI <- evppi(m, c(2,3), inp$mat, h.value = 0.0000005)
plot(EVPPI)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evppi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("info.rank")
### * info.rank

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: info.rank.bcea
### Title: Information-Rank Plot for bcea Class
### Aliases: info.rank.bcea info.rank
### Keywords: dplot models

### ** Examples

data("Vaccine")
m <- bcea(e,c)
inp <- createInputs(vaccine)
info.rank(m, inp)
info.rank(m, inp, graph = "base")
info.rank(m, inp, graph = "plotly")
info.rank(m, inp, graph = "ggplot2")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("info.rank", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make.report")
### * make.report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make.report
### Title: Make Report
### Aliases: make.report

### ** Examples


## Not run: 
##D   data(Vaccine, package = "BCEA")
##D   m <- bcea(e, c, ref = 2)
##D   make.report(m)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make.report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mixedAn-set")
### * mixedAn-set

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mixedAn<-
### Title: Cost-Effectiveness Analysis When Multiple (Possibly
###   Non-Cost-Effective) Interventions are Present on the Market
### Aliases: mixedAn<- mixedAn mixedAn.default

### ** Examples


# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
      plot=FALSE)           # inhibits graphical output

mixedAn(m) <- NULL      # uses the results of the mixed strategy 
                        #  analysis (a "mixedAn" object)
                        # the vector of market shares can be defined 
                        #  externally. If NULL, then each of the T 
                        #  interventions will have 1/T market share
                        # produces the plots
evi.plot(m)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mixedAn-set", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("multi.ce")
### * multi.ce

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: multi.ce
### Title: Cost-effectiveness Analysis With Multiple Comparison
### Aliases: multi.ce multi.ce.bcea
### Keywords: "Health "Multiple comparison" economic evaluation"

### ** Examples

# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)
 
# Runs the health economic evaluation using BCEA

m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=FALSE            # inhibits graphical output
)

mce <- multi.ce(m)          # uses the results of the economic analysis

ceac.plot(mce)
ceaf.plot(mce)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("multi.ce", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.CEriskav")
### * plot.CEriskav

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.CEriskav
### Title: Plots EIB and EVPI for the Risk Aversion Case
### Aliases: plot.CEriskav
### Keywords: Health Risk aversion economic evaluation

### ** Examples


# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem
#
# Load the processed results of the MCMC simulation model
data(Vaccine)
# 
# Runs the health economic evaluation using BCEA
m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=FALSE            # inhibits graphical output
)
#
# Define the vector of values for the risk aversion parameter, r, eg:
r <- c(1e-10, 0.005, 0.020, 0.035) 
#
# Run the cost-effectiveness analysis accounting for risk aversion
## No test: 
   CEriskav(m) <- r
## End(No test)
#
# produce the plots
## No test: 
   plot(m)
## End(No test)
## Alternative options, using ggplot2
## No test: 
   plot(m, graph = "ggplot2")
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.CEriskav", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.bcea")
### * plot.bcea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.bcea
### Title: Summary Plot of the Health Economic Analysis
### Aliases: plot.bcea
### Keywords: "Health economic evaluation" hplot

### ** Examples

# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
he <- bcea(
       e=e, c=c,             # defines the variables of 
                             #  effectiveness and cost
       ref=2,                # selects the 2nd row of (e,c) 
                             #  as containing the reference intervention
       interventions=treats, # defines the labels to be associated 
                             #  with each intervention
       Kmax=50000,           # maximum value possible for the willingness 
                             #  to pay threshold; implies that k is chosen 
                             #  in a grid from the interval (0,Kmax)
       plot=FALSE            # does not produce graphical outputs
      )

# Plots the summary plots for the "bcea" object m using base graphics
plot(he, graph = "base")

# Plots the same summary plots using ggplot2
if(require(ggplot2)){
plot(he, graph = "ggplot2")

##### Example of a customized plot.bcea with ggplot2
plot(he,
  graph = "ggplot2",                                      # use ggplot2
  theme = theme(plot.title=element_text(size=rel(1.25))), # theme elements must have a name
  ICER_size = 1.5,                                        # hidden option in ceplane.plot
  size = rel(2.5)                                         # modifies the size of k = labels
  )                                                       # in ceplane.plot and eib.plot
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.bcea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.evppi")
### * plot.evppi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.evppi
### Title: Plot Expected Value of Partial Information With Respect to a Set
###   of Parameters
### Aliases: plot.evppi
### Keywords: "Expected "Health economic evaluation" information" of value

### ** Examples


data(Vaccine, package = "BCEA")
treats <- c("Status quo", "Vaccination")

# Run the health economic evaluation using BCEA
m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)

# Compute the EVPPI for a bunch of parameters
inp <- createInputs(vaccine)

# Compute the EVPPI using INLA/SPDE
x0 <- evppi(m, c("beta.1." , "beta.2."), input = inp$mat)
x1 <- evppi(m, c(32,48,49), input = inp$mat)

plot(x0, pos = c(0,1))
plot(x1, pos = "topright")

plot(x0, col = c("black", "red"), pos = "topright")
plot(x0, col = c(2,3), pos = "bottomright")

plot(x0, pos = c(0,1), graph = "ggplot2")
plot(x1, pos = "top", graph = "ggplot2")

plot(x0, col = c("black", "red"), pos = "right", graph = "ggplot2")
plot(x0, col = c(2,3), size = c(1,2), pos = "bottom", graph = "ggplot2")

plot(x0, graph = "ggplot2", theme = ggplot2::theme_linedraw())

if (FALSE)
 plot(x0, col = 3, pos = "topright")
# The vector 'col' must have the number of elements for an EVPI
# colour and each of the EVPPI parameters. Forced to black




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.evppi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.bcea")
### * print.bcea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.bcea
### Title: bcea Print Method
### Aliases: print.bcea
### Keywords: print

### ** Examples

data("Vaccine")
he <- BCEA::bcea(e, c)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.bcea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim_table")
### * sim_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim_table
### Title: Table of Simulation Statistics for the Health Economic Model
### Aliases: sim_table sim_table.bcea
### Keywords: Health economic evaluation

### ** Examples

# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(e=e,                  # defines the variables of 
          c=c,                  # effectiveness and cost
          ref=2,                # selects the 2nd row of (e, c) 
                                # as containing the reference intervention
          interventions=treats, # defines the labels to be associated 
                                # with each intervention
          Kmax=50000)           # maximum value possible for the willingness 
                                # to pay threshold; implies that k is chosen 
                                # in a grid from the interval (0, Kmax)

# Now can save the simulation exercise in an object using sim_table()
sim_table(m,         # uses the results of the economic evaluation 
                     #  (a 'bcea' object)
          wtp=25000) # selects the particular value for k
               



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.bcea")
### * summary.bcea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.bcea
### Title: Summary Method for Objects of Class 'bcea'
### Aliases: summary.bcea
### Keywords: Health economic evaluation

### ** Examples

data(Vaccine)

he <- bcea(e, c, interventions = treats, ref = 2)
summary(he)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.bcea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.mixedAn")
### * summary.mixedAn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.mixedAn
### Title: Summary Methods For Objects in the Class 'mixedAn' (Mixed
###   Analysis)
### Aliases: summary.mixedAn
### Keywords: Health Mixed analysis economic evaluation

### ** Examples


# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000            # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
)

mixedAn(m) <- NULL      # uses the results of the mixed strategy 
                        #  analysis (a "mixedAn" object)
                        # the vector of market shares can be defined 
                        #  externally. If NULL, then each of the T 
                        #  interventions will have 1/T market share

# Prints a summary of the results
summary(m,         # uses the results of the mixed strategy analysis 
        wtp=25000) #  (a "mixedAn" object)
                   # selects the relevant willingness to pay 
                   #  (default: 25,000)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.mixedAn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
