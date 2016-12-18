library(markdown)
library(googleCharts)
library(shinythemes)
# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = 0,
  max = length(unique(test$metric))+1
)
ylim <- list(
  min = 0,
  max = 1.1
)
#theme = shinytheme("darkly"),
navbarPage(title=div(#img(src="logo.png",width = "100px", height = "35px"),
                      "Anomaly Detection"), 
           
           ############# GLOBAL ANALYSIS PANEL ###########
           tabPanel("Global Analysis",

                    sidebarLayout(
                     mainPanel(
                    googleChartsInit(),
                    
                    # Use the Google webfont "Source Sans Pro"
                    tags$link(
                      href=paste0("http://fonts.googleapis.com/css?",
                                  "family=Source+Sans+Pro:300,600,300italic"),
                      rel="stylesheet", type="text/css"),
                    tags$style(type="text/css",
                               "body {font-family: 'Source Sans Pro'}"
                    ),
                    
                    googleBubbleChart("chart",
                                      width="100%", height = "475px",
                                      # Set the default options for this chart; they can be
                                      # overridden in server.R on a per-update basis. See
                                      # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                                      # for option documentation.
                                      options = list(
                                        fontName = "Source Sans Pro",
                                        fontSize = 13,
                                        # Set axis labels and ranges
                                        hAxis = list(
                                          title = "Metric",
                                          viewWindow = xlim,
                                          ticks=lapply(unique(test$metric), function(x) list(v=as.numeric(unique(x)),f=unique(as.character(x))))
                                          
                                        ),
                                        vAxis = list(
                                          title = "Prediction accuracy",
                                          viewWindow = ylim
                                        ),
                                        # The default padding is a little too spaced out
                                        chartArea = list(
                                          top = 50, left = 75, right=100,
                                          height = "75%", width = "75%"
                                        ),
                                        # Allow pan/zoom
                                        explorer = list(),
                                        # Set bubble visual props
                                        bubble = list(
                                          opacity = 0.4, stroke = "none",
                                          # Hide bubble label
                                          textStyle = list(
                                            color = "none"
                                          )
                                        ),
                                        # Set fonts
                                        titleTextStyle = list(
                                          fontSize = 16
                                        ),
                                        tooltip = list(
                                          textStyle = list(
                                            fontSize = 12
                                          )
                                        )
                                      )
                    )
                    ),
                    # fluidRow(
                    #   shiny::column(4, offset = 4,
                    #                 sliderInput("prediction_error", "Prediction Error",
                    #                             min = 0, max = 3,
                    #                             value = 0, animate = FALSE)
                    #   )
                    # ),
                    # sidebarPanel(
                    #   
                    wellPanel(
                      tags$style(type="text/css", '#rightPanel { width:200px; float:left;}'),
                      id = "rightPanel",
                      
                      selectInput("aa", label = h5("Metric"), 
                                  choices =as.list(unique(test$metric))
                                  , 
                                  
                                  selected = 1),
                      sliderInput("prediction_error", "Prediction Error",
                                  min = 0, max = 3,
                                  value = 0, animate = FALSE),
                      selectInput("ss", label = h5("Partition"), 
                                  choices =as.list(unique(test$partition)))
                      ,
                   
                      
                      uiOutput("input_ui"), width=2
                    )
                    
                    
                    
                    )
                    
           ),
           ############## DETAIL PANEL ###################
           tabPanel("Graphs",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("metric", label = h5("Metric"), 
                                    choices =as.list(unique(test$metric))
                                    , 
                                    selected = 1),
                        selectInput("partition", label = h5("Partition"), 
                                    choices =as.list(unique(test$partition)))
                        ,
                        sliderInput("decimal", "Model Error:",
                                    min = 0, max = 0.4, value = 0.05,step= 0.01)
                      ),
                      mainPanel(
                        
                        plotOutput("plotos"),
                        plotOutput("original_ts")
                      )
                    )
           ),
           ############### LOGS PANEL ###################
           tabPanel("Table",
                  dataTableOutput('tablea')
                 
           ),
           ##### More
           navbarMenu("More",
                      tabPanel("About",
                         
                          print("Anomaly detection is based upon automated model selection based on AIC. \n
                                The candidate models for each time series are ARIMA models of different
                                orders (seasonal and non seasonal)"),
                                print("Code available in github: ----------")
                      ),
                      tabPanel("References",
                               fluidRow(
                                 column(6,
                                     'asd'  # includeMarkdown("about.md")
                                 ),
                                 column(3,
                                     
                                        tags$small(
                                          "Work in Progress"
                                
                                        )
                                 )
                               )
                      )
           )
)










