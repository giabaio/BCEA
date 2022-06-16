shiny::shinyUI(
  {shiny::tagList(
    shiny::tags$head(shiny::tags$link(rel='shortcut icon', href='favicon.ico')),
    shiny::tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }",
                      "div {text-align:justify;}"
    ),
    # These preserve the plots proportionality (to 70% height & width of the whole webpage)
    shiny::tags$head(shiny::tags$style(".shiny-plot-output{height:70vh !important;}")),
    shiny::tags$head(shiny::tags$style(".shiny-plot-output{width:70vh !important;}")),
    ## commented chunk overrides windows title and favicon if logo is included in the navbar
    #shiny::tags$div(
    #  style="height:0px;", # hack to override icon and window title when including logo in top-left corner of navbar
    #  fluidPage(
    #    list(tags$head(HTML('<link rel="icon", href="favicon.ico"/>'))),
    #    titlePanel(
    #      title="", windowTitle="BCEAweb"
    #    )
    #  )
    #),
    shiny::navbarPage(theme=shinythemes::shinytheme('united'),
                      title = "BCEAweb",
                      ## includes UCL logo in navbar
                      #title = shiny::div("BCEAweb",shiny::tags$img(src="ucl.png",height="36px"),
                      #                   style = "text-align: center;"), # horrible hack as above
                      ##################
                      #introduction tab#
                      ##################
                      {shiny::tabPanel("Welcome",
                                       shiny::fluidRow(
                                         shiny::column(
                                           width = 10, offset = 1,
                                           shiny::HTML(paste0(
                                             shiny::tags$h1("Welcome to BCEAweb"),
                                             shiny::tags$br(),
                                             shiny::tags$strong("BCEAweb"),
                                             " provides a web interface to the ",
                                             shiny::tags$a(href="http://www.cran.r-project.org","R",target='_blank'),
                                             " package ",
                                             shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA","BCEA",target='_blank'),
                                             ", designed to post-process the results of a statistical model and standardise 
                                                            health economic evaluations, as described in the following graph.",
                                             shiny::tags$br(),shiny::tags$br(),
                                             shiny::tags$a(href="ModelScheme.png",target="_blank",
                                                           shiny::tags$img(src='ModelScheme.png',width='60%',
                                                                           ## This aligns the graphs ###
                                                                           style="display: block; margin-left: auto; margin-right: auto;",
                                                                           alt='A schematic representation of the process of health economic evaluation')),
                                             shiny::tags$br(),
                                             "First, a statistical model is constructed and fitted to estimate relevant population 
                                                            parameters (the red rounded box). These are then fed to an economic model (the grey box), 
                                                            which combines them to obtain suitable summaries that quantify the incremental population 
                                                            average for clinical benefits (e.g. QALYs) and costs (e.g. &pound;). These are the fundamental
                                                            quantities used to make the decision analysis (orange box). And this is the process that ",
                                             shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA","BCEA",target='_blank'),
                                             " and ", 
                                             shiny::tags$strong("BCEAweb"), 
                                             " can perform, by producing standardise output to aid in the assessment of the economic 
                                                            evaluation. In addition, provided suitable data are provided by the user, ",
                                             shiny::tags$strong("BCEAweb"),
                                             " can also perform ",
                                             shiny::tags$i("Probabilistic Sensitivity Analysis"),
                                             " i.e. the process of analysing the impact of (parameter or model) uncertainty on the
                                                            results of the decision analysis (the olive box).",
                                             shiny::tags$br(),shiny::tags$br(),
                                             shiny::tags$strong("BCEAweb"),
                                             " assumes that the results of the statistical model are available in the form of a
                                                            large number of simulations for all the relevant model parameters. These can be stored
                                                            and uploaded by the user using three different formats:",
                                             shiny::tags$ol(
                                               shiny::tags$li(shiny::HTML(paste0(
                                                 "A spreadsheet, in ",shiny::tags$a(href='https://en.wikipedia.org/wiki/Comma-separated_values','.csv',target='_blank'),
                                                 " format, e.g. a file produced by MS Excel. Download an example ", 
                                                 shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA/Vaccine_spreadsheet.csv","here",target="_blank"),";"))),
                                               shiny::tags$li(shiny::HTML(paste0(
                                                 "Files in 'coda' format. These are typically saved as the results of running ",
                                                 shiny::tags$a(href="https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo","MCMC",target="_blank"), 
                                                 " software such as ",shiny::tags$a(href="http://www.openbugs.net/w/FrontPage","OpenBUGS",target="_blank"),
                                                 ". Coda produces an 'index' file and one output file for each Markov Chain used in the
                                                                  analysis. Download a .zip file with an example ",
                                                 shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA/Vaccine_BUGS.zip","here",target="_blank"),"."))),
                                               shiny::tags$li(shiny::HTML(paste0(
                                                 "A R object, available in the current session. This can be a spreadsheet imported in R (e.g. using the ", shiny::tags$code("read.csv"),
                                                 " function). Or the output of a full Bayesian analysis (e.g. performed using ",shiny::tags$a(href="http://www.openbugs.net/w/FrontPage","OpenBUGS",target="_blank"), 
                                                 "). The resulting data will be pre-processed to eliminate linear dependency across the variables.")))
                                             ),
                                             "The parameters simulations are uploaded at the 'Parameter simulations' tab. Once the simulations 
                                                            are uploaded, ",
                                             shiny::tags$strong("BCEAweb"),
                                             " will produce graphical summaries and tables so that the user can assess whether the results
                                                            are consistent with the assumptions or, in the case of a full Bayesian analysis, analysis 
                                                            convergence of the MCMC process through suitable diagnostics. ",
                                             shiny::tags$strong("BCEAweb"),
                                             " assumes that the user has saved simulations for the measures of effectiveness and costs for each of the interventions
                                                            being assessed in a .csv file. The order of the variables in this file ", 
                                             shiny::tags$strong(shiny::tags$i("needs")),
                                             " to be like in the following picture (e.g. effectiveness and costs for each intervention, in sequence).",shiny::tags$br(),shiny::tags$br(),
                                             shiny::tags$a(href="PSA_data2.png",target="_blank",
                                                           shiny::tags$img(src='PSA_data2.png',width='55%',
                                                                           ## This aligns the graphs ###
                                                                           style="display: block; margin-left: auto; margin-right: auto;")),
                                             shiny::tags$br(),
                                             "An example of such a file can be obtained ",
                                             shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA/PSA_vaccine.csv","here",target="_blank"),
                                             ". These simulations are uploaded in the 'Economic analysis' tab, where the user can specify some options.",
                                             " Clicking the button ",shiny::tags$code("Run the analysis")," in the 'Economic analysis tab' will run ",
                                             tags$a(href="http://www.statistica.it/gianluca/BCEA","BCEA",target="_blank"),
                                             " in the background and create all the relevant economic summaries, including a detailed 
                                                            Probabilistic Sensitivity Analysis. The tab 'Value of information' also automatically computes the 
                                                            Expected Value of Perfect Information and allows the user to run an analysis of the Expected Value of ",
                                             shiny::tags$i("Partial "), "Perfect Information. This is computed using computational efficient methods 
                                                            and provides a valuable tool to assess the impact of current uncertainty on the decision-making process 
                                                            and to determine research prioritisation. Crucially, these methods cannot be implemented in non-specialised
                                                            software (e.g. MS Excel) and thus the use of statistical programmes such as ",
                                             shiny::tags$a(href="http://www.cran.r-project.org","R",target='_blank'),", is essential. The results of the 
                                                            economic evaluation performed using ",
                                             shiny::tags$strong("BCEAweb"),
                                             " can be exported in either .pdf or .doc
                                                            format. The resulting report contains some pre-formatted text, aimed at guiding the user through
                                                            the interpretation of the results.",
                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
                                             "Copyright: ",
                                             shiny::tags$a(href='mailto: g.baio@ucl.ac.uk','Gianluca Baio'),
                                             ", Polina Hadjipanayiotou, ",
                                             shiny::tags$a(href='mailto: Andrea.Berardi@parexel.com','Andrea Berardi'),", ",
                                             shiny::tags$a(href='mailto: anna.heath.14@ucl.ac.uk','Anna Heath')
                                           )
                                           )))
                      )},
                      
                      ###########
                      #Check tab#
                      ###########
                      {shiny::tabPanel("1. Parameter simulations",
                                       {shiny::sidebarPanel(
                                         shiny::tags$div(
                                           "In this panel, the user can upload the simulations for the relevant model parameters.",
                                           shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                         ),
                                         shiny::selectInput("from","1. Import parameters simulation data from:",
                                                            choices=c("R","Spreadsheet","BUGS"),
                                                            selected="R"),
                                         
                                         # Panel upload from spreadsheet (CSV file)
                                         shiny::conditionalPanel(condition="input.from=='Spreadsheet'",
                                                                 shiny::fileInput('par_sims_csv', 'Choose CSV File',
                                                                                  accept=c('text/csv', 
                                                                                           'text/comma-separated-values,text/plain',
                                                                                           '.csv'))
                                         ),
                                         
                                         # Panel data import from BUGS (full Bayesian MCMC)
                                         shiny::conditionalPanel(condition="input.from=='BUGS'",
                                                                 # Asks for the number of chains used
                                                                 shiny::numericInput("nchains","Number of Markov chains",
                                                                                     value=2,min=1),
                                                                 # Selects the coda index
                                                                 shiny::fileInput("codaIndex", 'Choose coda index file', accept='.txt'),
                                                                 # Selects the coda for each chain
                                                                 shiny::uiOutput("coda")
                                         ),
                                         
                                         ## Rest of the side bar (common to all cases)
                                         shiny::selectInput('parameter',
                                                            "2. Select parameter of interest for checking",
                                                            ""),
                                         shiny::tags$br(),
                                         shiny::sliderInput('bins',
                                                            "3. Select the number of bins for histogram",
                                                            min=5,max=25,value=15,ticks=FALSE),
                                         width='4'
                                       )},
                                       
                                       {shiny::mainPanel(
                                         # THIS CODE PREVENTS FROM SHOWING THE ERROR MESSAGES --- THEY WILL STILL SHOW IN THE R TERMINAL, THOUGH
                                         shiny::tags$style(type="text/css",
                                                           ".shiny-output-error { visibility: hidden; }",
                                                           ".shiny-output-error:before { visibility: hidden; }"),
                                         
                                         # Plots for R & spreadsheet
                                         {shiny::conditionalPanel(condition="input.from=='Spreadsheet' || input.from=='R'",
                                                                  shiny::tabsetPanel(
                                                                    shiny::tabPanel("1.1. Plot and summary",
                                                                                    shiny::plotOutput('hist'),#,width="800px", height="800px"),
                                                                                    shiny::tableOutput("summary"))
                                                                  )
                                         )},
                                         
                                         # Plots for BUGS
                                         {shiny::conditionalPanel(condition="input.from=='BUGS'",
                                                                  shiny::tabsetPanel(
                                                                    shiny::tabPanel("1.1. Plot and summary",
                                                                                    shiny::plotOutput('hist2'),#,width="800px", height="800px"),
                                                                                    shiny::tableOutput("summary2")
                                                                    ),
                                                                    shiny::tabPanel("1.2. Trace plots",
                                                                                    shiny::plotOutput('trace'),#,width="800px", height="800px"),
                                                                                    shiny::p("NB check visually if the Bayesian model has converged")
                                                                    ),
                                                                    shiny::tabPanel("1.3. GR plot",
                                                                                    shiny::plotOutput('gr'),#,width="800px", height="800px"),
                                                                                    shiny::p("Check visually the value of the Gelman-Rubin statistic. Values below 1.1 are considered to suggest convergence for a given parameter")),
                                                                    shiny::tabPanel("1.4. Effective sample size",
                                                                                    shiny::plotOutput('neff')#,width="800px", height="800px")
                                                                    ),
                                                                    shiny::tabPanel("1.5. Autocorrelation",
                                                                                    shiny::plotOutput('acf')#,width="800px", height="800px")
                                                                    )
                                                                  )
                                         )}
                                       )}
                      )},
                      
                      ##########
                      #BCEA tab#
                      ##########
                      {shiny::tabPanel("2. Economic analysis",shiny::sidebarPanel(
                        shiny::tags$div(
                          shiny::HTML(paste0(
                            "In this panel, the user can upload the simulation data for the economic output. These are defined in terms of a vector of simulations for the effectiveness variable and a vector of simulations for the cost variable, for each of the interventions being assessed.",
                            shiny::tags$br(),shiny::tags$br(),
                            "The user can also specify the range and default value for the willingness-to-pay parameter, as well as the labels associated with each interventions. Clicking the ",
                            shiny::tags$code("Run analysis"), " button will run ",shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA","BCEA",target="_blank"),
                            " in the background to perform the economic analysis.",
                            shiny::tags$br(),shiny::tags$br()))),
                        shiny::tags$div(
                          "In this panel, the user can upload the (e,c) data for the relevant model parameters.",
                          shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                        ),
                        shiny::selectInput("data","1. Import the simulations for (e,c) from:",
                                           choices=c("R","Spreadsheet","BUGS","Model parameters (from Parameter simulations)"),
                                           selected="R"),
                        
                        # Panel data import from a Spreadsheet
                        shiny::conditionalPanel(condition="input.data=='Spreadsheet'",
                                                shiny::fileInput('file1', 'Choose .CSV File',
                                                                 accept=c('text/csv', 'text/comma-separated-values,text/plain', c('.csv','.xlsx')))),
                        
                        # # Panel data import from R
                        # shiny::conditionalPanel(condition="input.data=='R'",
                        #                         shiny::fileInput('file2','Choose RDS File',
                        #                                          accept='.RDS')),
                        
                        # Panel data import from BUGS (full Bayesian MCMC)
                        shiny::conditionalPanel(condition="input.data=='BUGS'",
                                                # Asks for the number of chains used
                                                shiny::numericInput("nchains1","Number of Markov chains",
                                                                    value=2,min=1),
                                                # Selects the coda index
                                                shiny::fileInput("codaIndex1", 'Choose coda index file', accept='.txt'),
                                                # Selects the coda for each chain
                                                shiny::uiOutput("coda1")#,
                                                # Imports data using the coda package
                                                #shiny::actionButton("import_coda1","Import using coda")
                        ),
                        
                        # Panel data import from 1. Check assumptions (parameters simulations)
                        shiny::conditionalPanel(condition="input.data=='Model parameters (from Parameter simulations)'",
                                                shiny::numericInput("n.ints","Number of interventions",value=2,min=2),
                                                shiny::tags$br(),
                                                shiny::fluidRow(
                                                  shiny::column(6,shiny::uiOutput("select.e")),
                                                  shiny::column(6,shiny::uiOutput("select.c"))
                                                ),
                                                shiny::actionButton("import_e_c","Make selection"),
                                                shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                        ),
                        
                        shiny::p(shiny::h5(shiny::strong("2. Define the grid of values for the willingness to pay (wtp)"))),
                        shiny::fluidRow(
                          shiny::column(4,
                                        shiny::numericInput("min","min",value = 0,min = 0, step = 100)),
                          shiny::column(4,
                                        shiny::numericInput("max","max",value = 50000,min = 0, step = 100)),
                          shiny::column(4,
                                        shiny::uiOutput("grid_step")
                          )
                        ),
                        
                        shiny::uiOutput("step"),
                        shiny::p(shiny::h5(shiny::strong("4. Define intervention labels"))),
                        shiny::uiOutput("int_labels"),
                        shiny::uiOutput("sel_ref"),
                        shiny::tags$br(),
                        shiny::actionButton("buttonsum","Run the analysis"),width = '4'),
                        shiny::mainPanel(
                          # AB 20180713: not required anymore: moved up 
                          # ## THIS CODE PREVENTS FROM SHOWING THE ERROR MESSAGES --- THEY WILL STILL SHOW IN THE R TERMINAL, THOUGH
                          # shiny::tags$style(type="text/css",
                          #                   ".shiny-output-error { visibility: hidden; }",
                          #                   ".shiny-output-error:before { visibility: hidden; }"),
                          shiny::tabsetPanel(
                            shiny::tabPanel("2.1. Cost-Effectiveness Analysis",
                                            shiny::verbatimTextOutput('analysis')
                            ),
                            shiny::tabPanel("2.2. Cost-Effectiveness plane", 
                                            shiny::fluidRow( shiny::column(8,shiny::plotOutput('cep')),
                                                             shiny::column(4,shiny::uiOutput("other_CEA"))
                                            )
                            ),
                            shiny::tabPanel("2.3 Expected Incremental Benefit",
                                            shiny::plotOutput('eib')
                            ),
                            shiny::tabPanel("2.4 Cost-Effectiveness Efficiency Frontier",
                                            shiny::fluidRow(
                                              shiny::column(7,
                                                            shiny::mainPanel(shiny::plotOutput('ceef'))
                                              ),
                                              shiny::column(10,
                                                            shiny::br(),shiny::br(),shiny::br(),
                                                            shiny::verbatimTextOutput('analysisc')
                                              )
                                              ##### AB check this #####
                                              #),
                                              #width='12'
                                            )
                            )
                          )
                        )
                      )},
                      
                      
                      #########
                      #PSA tab#
                      #########
                      {shiny::tabPanel("3. Uncertainty Analysis",
                                       ## THIS CODE PREVENTS FROM SHOWING THE ERROR MESSAGES --- THEY WILL STILL SHOW IN THE R TERMINAL, THOUGH
                                       shiny::tags$style(type="text/css",
                                                         ".shiny-output-error { visibility: hidden; }",
                                                         ".shiny-output-error:before { visibility: hidden; }"),
                                       shiny::tabsetPanel(
                                         shiny::tabPanel("3.1. CEAC",
                                                         shiny::sidebarPanel(
                                                           shiny::tags$div(
                                                             "This plot shows the probability that the reference intervention is
                                         cost-effective. For each value of the willingness-to-pay grid, it is
                                         possible to visualise the resulting probability (the CEAC).",
                                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::column(5,shiny::downloadButton('download_CEAC_table','Download CEAC table'))
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::br(),
                                                             shiny::column(5,shiny::uiOutput("wtp_values2")),
                                                             shiny::column(5,shiny::uiOutput("ceac_values"))
                                                           ),
                                                           width = 4
                                                         ),
                                                         shiny::mainPanel(
                                                           shiny::plotOutput('ceac')
                                                         )
                                         ),
                                         shiny::tabPanel("3.2 Multi-comparison CEAC",
                                                         shiny::sidebarPanel(
                                                           shiny::tags$div(shiny::HTML(paste0(
                                                             "This plot shows the probability that each intervention under assessement is
                                         the ",shiny::tags$i("most"), " cost-effective.",
                                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ))),
                                                           shiny::fluidRow(
                                                             #column(5,downloadButton('download_CEAC_table','Download CEAC table'))
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::br()
                                                             #column(5,uiOutput("wtp_values2")),
                                                             #column(5,uiOutput("ceac_values"))
                                                           ),
                                                           width = 4
                                                         ),
                                                         shiny::mainPanel(
                                                           shiny::plotOutput('multi_ceac')
                                                         )
                                         ),
                                         shiny::tabPanel("3.3 CEAF",
                                                         shiny::sidebarPanel(
                                                           shiny::tags$div(
                                                             "This plot shows the cost-effectiveness frontier. For each value of the
                                         willingness-to-pay threshold, this quantifies the probability of cost-effectiveness
                                         for the 'best' intervention. For each value of the willingness-to-pay grid, it is
                                         possible to visualise the resulting probability (the CEAC).",
                                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::column(5,shiny::downloadButton('download_CEAF_table','Download CEAF table'))
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::br(),
                                                             shiny::column(5,shiny::uiOutput("wtp_values3")),
                                                             shiny::column(5,shiny::uiOutput("ceaf_values"))
                                                           ),
                                                           width = 4
                                                         ),
                                                         shiny::mainPanel(
                                                           shiny::plotOutput('ceaf')
                                                         )
                                         )
                                       )
                      )},
                      
                      
                      ##########################
                      #Value of information tab#
                      ##########################
                      {shiny::tabPanel("4. Value of information",
                                       shiny::tabsetPanel(
                                         shiny::tabPanel("4.1. EVPI",
                                                         shiny::sidebarPanel(
                                                           shiny::tags$div(
                                                             "This plot shows the analysis of the Expected Value of Perfect Information (EVPI). The EVPI can be visualised for each value of the willingness-to-pay grid.",
                                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::column(5,shiny::downloadButton('download_EVPI_table','Download EVPI table'))
                                                           ),
                                                           shiny::fluidRow(
                                                             shiny::br(),
                                                             shiny::column(5,shiny::uiOutput("wtp_values4")),
                                                             shiny::column(5,shiny::uiOutput("evpi_values"))
                                                           ),
                                                           width = 4
                                                         ),
                                                         
                                                         shiny::mainPanel(
                                                           shiny::plotOutput('evpi')
                                                         )
                                         ),
                                         
                                         shiny::tabPanel("4.2. Info-rank",
                                                         shiny::sidebarPanel(
                                                           shiny::tags$div(
                                                             "This is the 'Info-rank' plot, an extension of 'Tornado plots', based on
                                         the analysis of the EVPPI.",
                                                             shiny::tags$br(),shiny::tags$br(),
                                                             "For each parameter and value of the willingness-to-pay threshold, a barchart is
                                         plotted to describe the ratio of EVPPI (specific to that parameter) to EVPI. This
                                         represents the relative 'importance' of each parameter in terms of the expected
                                         value of information.",
                                                             shiny::tags$br(),shiny::tags$br(),
                                                             "Notice that the ranking provided by considering the parameters separately may
                                         be very different to that obtained considering sub-sets of parameters. Thus, it
                                         is recommended that a full analysis is performed using the tab '4.3 EVPPI'",
                                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           
                                                           shiny::uiOutput("info_rank_pars"),
                                                           
                                                           shiny::fluidRow(
                                                             shiny::column(4,shiny::uiOutput("wtp_values5")),
                                                             shiny::column(8,shiny::uiOutput("N_values"))
                                                           ),
                                                           
                                                           shiny::fluidRow(
                                                             shiny::br(),
                                                             shiny::column(4,shiny::actionButton("run_info_rank","Run")),
                                                             shiny::column(5,shiny::downloadButton('download_IR_table', 'Download Info-rank table'))
                                                           ),
                                                           
                                                           shiny::fluidRow(
                                                             shiny::br(),shiny::br(),
                                                             shiny::column(8,
                                                                           shiny::uiOutput("details_ir")
                                                             )
                                                           ),
                                                           width = 4
                                                         ),
                                                         
                                                         shiny::mainPanel(
                                                           shiny::plotOutput('ir')
                                                         )
                                         ),
                                         
                                         
                                         shiny::tabPanel("4.3. EVPPI",
                                                         shiny::sidebarPanel(
                                                           shiny::selectInput("which_method","1. Select method of calculation",
                                                                              choices=c("GAM regression (up to 5 parameters)","GP regression","INLA-SPDE"),
                                                                              selected="INLA-SPDE"),
                                                           
                                                           shiny::uiOutput("evppi_pars"),
                                                           
                                                           shiny::uiOutput("num_sims"),
                                                           
                                                           # Here should put some conditional panels which allows the user to select method-specific options
                                                           shiny::conditionalPanel(condition="input.which_method=='GAM regression (up to 5 parameters)'",
                                                                                   shiny::p(shiny::h5(shiny::strong("4. Method-specific options:"))),
                                                                                   shiny::selectInput("formula_gam","a. Model",
                                                                                                      choices=c("Full interaction","Separate"),
                                                                                                      selected="Full interaction"),
                                                                                   shiny::p(shiny::h5(shiny::strong("Reference"),": Strong M, Oakley JE, Brennan A. Estimating multi-parameter partial
                                                                                 Expected Value of Perfect Information from a probabilistic sensitivity analysis sample:
                                                                                 a non-parametric regression approach. Medical Decision Making. 2014;34(3):311-26.
                                                                                 Available open access",
                                                                                                      shiny::tags$a(href="http://mdm.sagepub.com/content/34/3/311","here",target='_blank'),".")),
                                                                                   shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           
                                                           shiny::conditionalPanel(condition="input.which_method=='GP regression'",
                                                                                   shiny::p(shiny::h5(shiny::strong("4. Method-specific options:"))),
                                                                                   shiny::uiOutput("option_GP"),
                                                                                   shiny::p(shiny::h5(shiny::strong("Reference"),": Strong M, Oakley JE, Brennan A. Estimating multi-parameter partial
                                                                                 Expected Value of Perfect Information from a probabilistic sensitivity analysis sample:
                                                                                 a non-parametric regression approach. Medical Decision Making. 2014;34(3):311-26.
                                                                                 Available open access",
                                                                                                      shiny::tags$a(href="http://mdm.sagepub.com/content/34/3/311","here",target='_blank'),".")),
                                                                                   shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           
                                                           shiny::conditionalPanel(condition="input.which_method=='INLA-SPDE'",
                                                                                   shiny::p(shiny::h5(shiny::strong("4. Method-specific options:"))),
                                                                                   #                                                              fluidRow(
                                                                                   #                                                                column(12,
                                                                                   #                                                                       numericInput("formula_inla",
                                                                                   #                                                                                    "a. Interactions order (select higher values to deal with non-linearity)",
                                                                                   #                                                                                    value=1,min=1,max=5)
                                                                                   #                                                                ),
                                                                                   shiny::fluidRow(
                                                                                     shiny::column(12,
                                                                                                   shiny::p(shiny::h5(shiny::strong("a. Interactions order (select higher values to deal with non-linearity)")))
                                                                                     ),
                                                                                     shiny::column(6,
                                                                                                   shiny::numericInput("int.ord1",
                                                                                                                       "Effects",
                                                                                                                       value=1,min=1,max=5)
                                                                                     ),
                                                                                     shiny::column(6,
                                                                                                   shiny::numericInput("int.ord2",
                                                                                                                       "Costs",
                                                                                                                       value=1,min=1,max=5)
                                                                                                   
                                                                                     ),
                                                                                     shiny::column(12,
                                                                                                   shiny::p(shiny::h5(shiny::strong("b. Mesh controls (smaller values = faster but LESS accurate)")))
                                                                                     ),
                                                                                     shiny::column(3,
                                                                                                   shiny::sliderInput('cutoff_inla',ticks=F,
                                                                                                                      "Density of points",
                                                                                                                      min=.1,max=.5,value = .3)
                                                                                     ),
                                                                                     shiny::column(3,
                                                                                                   shiny::sliderInput('convex_in',
                                                                                                                      "Inner boundary",ticks=F,
                                                                                                                      value=0.5,max=0.7,min=0.3,step=.01)
                                                                                     ),
                                                                                     shiny::column(3,
                                                                                                   shiny::sliderInput('convex_out',
                                                                                                                      "Outer boundary",ticks=FALSE,
                                                                                                                      value=0.75,max=1,min=0.5,step=.01
                                                                                                   )
                                                                                     ),
                                                                                     shiny::column(3,
                                                                                                   shiny::sliderInput('h_value',
                                                                                                                      "INLA step size",ticks=FALSE,
                                                                                                                      value=(1-0.05),max=1,min=0,step=.01
                                                                                                   )
                                                                                     )
                                                                                   ),
                                                                                   shiny::p(shiny::h5(shiny::strong("NB:"),"Unlike in the R terminal version of BCEA, here, for simplicity the mesh controls
                                                                                 have been standardised so that lower values",shiny::strong("always"),"produce a faster but potentially
                                                                                 less accurate estimates.")),
                                                                                   shiny::p(shiny::h5("Some more complex options are available when using the R terminal version of BCEA. If you cannot
                                                                                 fit your model, please consider using it, or contact",
                                                                                                      shiny::tags$a(href='mailto: g.baio@ucl.ac.uk','us'),".")),
                                                                                   shiny::tags$br(),shiny::tags$br(),
                                                                                   shiny::p(shiny::h5(shiny::strong("References"))),
                                                                                   shiny::p(shiny::h5("Heath A, Manolopoulou I and Baio G. Estimating the expected value of
                                                                                 partial perfect information in health economic evaluations using
                                                                                 Integrated Nested Laplace Approximation. Statistics in Medicine.
                                                                                 Apr 2015. Available online",
                                                                                                      shiny::tags$a(href="http://onlinelibrary.wiley.com/doi/10.1002/sim.6983/full","here",target='_blank'),".")),
                                                                                   shiny::p(shiny::h5("Baio G, Berardi A and Heath A. Bayesian Cost-Effectiveness Analysis with the R package BCEA. Springer (2017)",
                                                                                                      shiny::tags$a(href="http://www.statistica.it/gianluca/BCEA_book","More details here",target='_blank'),".")),
                                                                                   # p(h5(strong("References"),": Heath A, Manolopoulou I and Baio G. Estimating the expected value of
                                                                                   #                             partial perfect information in health economic evaluations using
                                                                                   #                             Integrated Nested Laplace Approximation. Statistics in Medicine.
                                                                                   #                             Apr 2015. Available online",
                                                                                   #       tags$a(href="http://onlinelibrary.wiley.com/doi/10.1002/sim.6983/full","here",target='_blank'),".")),
                                                                                   shiny::tags$br(),shiny::tags$br(),shiny::tags$br()
                                                           ),
                                                           
                                                           
                                                           shiny::fluidRow(
                                                             shiny::column(5,shiny::actionButton("run_evppi","Run")),
                                                             shiny::column(5,shiny::downloadButton('download_EVPPI_table', 'Download EVPPI table'))
                                                           ),
                                                           
                                                           shiny::fluidRow(
                                                             shiny::br(),shiny::br(),
                                                             shiny::column(8,
                                                                           shiny::uiOutput("details_evppi")
                                                             )
                                                           ),
                                                           
                                                           shiny::fluidRow(
                                                             shiny::br(),
                                                             shiny::column(6,
                                                                           shiny::uiOutput("wtp_values")
                                                             ),
                                                             shiny::column(6,
                                                                           shiny::uiOutput("evppi_values")
                                                             )
                                                           ),
                                                           
                                                           shiny::conditionalPanel(condition="input.evppi_tab=='Diagnostics'",
                                                                                   shiny::uiOutput("select_diag")
                                                           ),
                                                           
                                                           width = 4
                                                         ),
                                                         
                                                         shiny::mainPanel(
                                                           shiny::tabsetPanel(
                                                             shiny::tabPanel("Analysis",
                                                                             shiny::plotOutput('evppi')
                                                             ),
                                                             shiny::tabPanel("Diagnostics",
                                                                             shiny::plotOutput('diag_evppi')
                                                             ),
                                                             id="evppi_tab"
                                                           )
                                                         )
                                         )
                                         
                                       )
                      )},
                      
                      ############
                      #Report tab#
                      ############
                      {shiny::tabPanel("5. Report",
                                       shiny::sidebarPanel(
                                         shiny::tags$div(
                                           shiny::tags$strong("Please select the required output and the document format:"),
                                           shiny::tags$br(),shiny::tags$br()
                                         ),
                                         shiny::checkboxGroupInput('assumption', "Model checking",choices = ("Plots and summaries"), selected = NULL, inline = FALSE),
                                         shiny::checkboxGroupInput('bcea', "Economic Analysis", choices = c("Cost-effectiveness analysis",
                                                                                                            "Cost-effectiveness plane",
                                                                                                            "Expected Incremental Benefit",
                                                                                                            "Efficiency frontier"),
                                                                   selected = NULL, inline = FALSE),
                                         shiny::checkboxGroupInput('psa', "PSA & Value of information",choices =  c("CEAC",
                                                                                                                    "Multi-comparison CEAC",
                                                                                                                    "CEAF","EVPI","Info-rank","EVPPI"),
                                                                   selected = NULL, inline = FALSE),
                                         shiny::radioButtons('format', 'Document format', c('PDF', 'Word'),inline = TRUE),
                                         shiny::downloadButton('downloadReport', 'Download report'),
                                         shiny::br(),
                                         shiny::p("NB: generating the document can take some time.")
                                       )
                      )}
    )
  )}
)
