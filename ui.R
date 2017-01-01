library(markdown)
# getOption("download.file.method")
# options(download.file.method = "wininet")

# packageVersion("curl")
# packageVersion("httr")

# devtools::install_github("jcheng5/googleCharts")
# devtools::install_git("https://github.com/jcheng5/googleCharts.git") 
library(googleCharts)
library(shinythemes)
# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = 0,
  max = (length(unique(meta_frame$metric))+1)
)
ylim <- list(
  min = 0,
  max = 1.1
)

navbarPage(title=div(#img(src="logo.png",width = "100px", height = "35px"),
  "KPI Monitoring"), theme = shinytheme("darkly"),
  
  ############# OVERVIEW PANEL ###########
  tabPanel("Overview",
           
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
                                   backgroundColor='#222222',
                                   # legend.textStyle={color: 'black', 
                                   #   fontName: 'Source Sans Pro', fontSize: 8},
                                   legend=list(textStyle= list(color= 'grey', fontSize= 13)),
                                   fontSize = 13,
                                   # Set axis labels and ranges
                                   hAxis = list(
                                     title = "Metric",
                                     viewWindow = xlim,
                                     ticks=lapply(unique(meta_frame$metric), function(x) list(v=as.numeric(unique(x)),f=unique(as.character(x))))
                                     ,   titleTextStyle= list(color= 'white'),
                                     textStyle=list(color= 'white')
                                   ),
                                   vAxis = list(
                                     title = "Prediction accuracy",
                                     viewWindow = ylim,
                                     titleTextStyle= list(color= 'white'),
                                     textStyle=list(color= 'white')
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
                                     fontSize = 16,
                                     color='white'
                                   ),
                                   tooltip = list(
                                     textStyle = list(
                                       fontSize = 12
                                     )
                                   )
                                 )
               )
             ),
             fluidRow(
               
               wellPanel(
                 tags$style(type="text/css", '#rightPanel { width:400px; float:left;}'),
                 id = "rightPanel",
                 textOutput('title_alert'),
                 br(),
                 uiOutput("alert_level_input"),
                 br(),
                 br(),
                 textOutput('title_dim'),
                 br(),
                 uiOutput("input_ui"),
                 
                 width=2
               ),
               shiny::column(4, offset = 3,
                             sliderInput("min_vol", "Minimum volume",
                                         min = 0, max = 10,
                                         value = 0, animate = FALSE)
               )
               
               
               
             )
             
             
             
           )
           
  ),
  ############## Graphs PANEL ###################
  tabPanel("Graphs",
           sidebarLayout(
             sidebarPanel(
               selectInput("metric", label = h5("Metric"), 
                           choices =as.list(unique(test$metric))
                           , 
                           selected = 1),
               selectInput("partition", label = h5("Partition"), 
                           choices =as.list(unique(test$partition))),
               br(),br(),
               textOutput('plot_alert'),br(),
               textOutput('plot_real'),br(),
               textOutput('plot_upper'),br(),
               textOutput('plot_under'),br()                        
             ),
             mainPanel(
               textOutput('plot_title1'),
               br(),
               plotlyOutput("plotos"),
               br(),br(),
               textOutput('plot_title2'),
               br(),
               plotOutput("original_ts"),
               br(),br()
             )
           )
  ),
  ############### Table PANEL ###################
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









