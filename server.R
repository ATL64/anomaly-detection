load('exampleSpace.RData')
library(dplyr)
library(plotly)
library(ggplot2)
library(googleCharts)
library(shinythemes)
function(input, output, session) {
  
  output$plotos <-
    renderPlotly({
      
      
      df<-as.data.frame(rbind(
        original=test[which(test$metric==input$metric&test$partition==input$partition),63:92],
        
        under=predictions[which(test$metric==input$metric&test$partition==input$partition),]-
          (test[which(test$metric==input$metric&test$partition==input$partition),][63:92])*(
            meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_mean_error+
              2*meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_std_dev)
        ,
        over=predictions[which(test$metric==input$metric&test$partition==input$partition),]+
          (test[which(test$metric==input$metric&test$partition==input$partition),][63:92])*(
            meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_mean_error+
              2*meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_std_dev)
        ,
        date=names(test)[63:92]
      )) 
      
      
      dft<-as.data.frame(t(df))
      dft$original<-as.numeric(as.character(dft$original))
      dft$under<-as.numeric(as.character(dft$under))
      dft$over<-as.numeric(as.character(dft$over))
      
      
      ggplot(dft) + geom_line(aes(y=original, x=date, colour = "original",group=1))+
        geom_ribbon(aes(ymin=under, ymax=over, x=date, fill = "band",group=1), alpha = 0.3)+
        scale_colour_manual("",values="blue")+
        scale_fill_manual("",values="grey12")+
        scale_x_discrete(name =' ',breaks=as.character(dft$date)[which(as.numeric(dft$date)%%7==0)],
                         labels=as.character(dft$date)[which(as.numeric(dft$date)%%7==0)]) +
        scale_y_continuous(name=' ', limits=c(min(dft$under)/3,1.5*max(dft$over)))+
        theme(legend.position="none",plot.background = element_rect(
          fill = "#222222",
          colour = "#222222",
          size = 2,
          linetype = "longdash"
        )
        ,
        axis.text = element_text(colour = "white",size=8),
        axis.title.x = element_text(colour = "white",size=8),
        axis.title.y = element_text(colour = "white",size=8))
      
    })
  
  
  output$original_ts <-
    renderPlot({
      dfa<-as.data.frame(rbind(
        original=test[which(test$metric==input$metric&test$partition==input$partition),3:92],
        date=names(test)[3:92]
      ))
      
      
      
      dfo<-as.data.frame(t(dfa))
      dfo$original<-as.numeric(as.character(dfo$original))
      tallOriginal <- with(dfo,
                           data.frame(Value = original,
                                      variable = factor(rep(c("original"),
                                                            each = NROW(dfo))),
                                      Dates = rep(as.Date(date), 1)))
      
      ggplot(tallOriginal, aes(Dates, Value, colour = variable))+ geom_line()+
        theme(
          plot.background = element_rect(
            fill = "#222222",
            colour = "#222222",
            size = 2,
            linetype = "longdash"
          )
          ,legend.position="none"
          ,axis.text = element_text(colour = "white",size=12)
          ,axis.title.x = element_text(colour = "white",size=12),
          axis.title.y = element_text(colour = "white",size=12)
          
        ) +  labs(x=" ",y=" ")
      
    }, bg="transparent")  
  
  
  output$tablea <-renderDataTable({
    meta_frame[,-which(names(meta_frame) %in% c('prediction_perc_error_abs'))]
  },options = list(lengthMenu = c(5,10, 25, 50,100), pageLength = 5))
  
  
  
  #Checkboxes
  vector_checkbox_all<-rep(1,length(test_factors_names))
  lapply(test_factors_names, function(part) { 
    observeEvent(input[[paste("all",as.character(part),sep='_')]], {  
      if (is.null(input[[paste("all",as.character(part),sep='_')]])) {
        updateCheckboxGroupInput(
          session = session, inputId = part, selected =  ""
        )
      } else { 
        if(vector_checkbox_all[which(as.character(part) %in% test_factors_names)]%% 2 == 0){
          
          updateCheckboxGroupInput(
            
            session = session, inputId = as.character(part) , selected = unique(test[, which(names(test) == as.character(part))])
            
          )
          vector_checkbox_all[which(as.character(part) %in% test_factors_names)]<<-vector_checkbox_all[which(as.character(part) %in% test_factors_names)]+1
          
        } else {
          updateCheckboxGroupInput(
            
            session = session, inputId = as.character(part) , selected = ""
          )
          vector_checkbox_all[which(as.character(part) %in% test_factors_names)]<<-vector_checkbox_all[which(as.character(part) %in% test_factors_names)]+1
          
        }
        
        
      }})})   
  
  
  output$input_ui <- renderUI({  
    lapply(test_factors_names, function(part) {
      
      dropdownButton(label = as.character(part), status = "default", width = 100,br(),
                     actionButton(inputId = paste("all",as.character(part),sep='_'), label = "Select all"),
                     checkboxGroupInput(inputId = as.character(part), label = "Choose", 
                                        choices = unique(test[, which(names(test) == as.character(part))]),
                                        selected = unique(test[, which(names(test) == as.character(part))])
                                        
                     )
      )
      
    })
    
  })
  
  
  
  output$alert_level_input <- renderUI({  
    
    dropdownButton(label = 'alert_level', status = "default", width = 100,br(),
                   checkboxGroupInput(inputId = 'alert_level', label = "Choose", 
                                      choices = unique(meta_frame[, which(names(meta_frame) == 'alert_level')]),
                                      selected = unique(meta_frame[, which(names(meta_frame) =='alert_level')])
                                      
                   )
    ) 
  })
  
  
  ###############################################################################################################
  ###############################################################################################################
  ###############################################################################################################
  ###############################################################################################################
  
  
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#3a9659", "#e5e843", "#d38b1f", "#a03416")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = c("Great","Ok","Regular","Bad")
  )
  as.numeric(meta_frame$metric)
  yearData <- reactive({   
    # Filter to the desired checkboxes , and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by "alert_level"
    # so that Google Charts orders and colors the "alert_level"
    # consistently. 
    
    
    df <- meta_frame %>% na.omit %>% 
      filter(Reduce("&", lapply(factor_names, function(part) 
        (as.character(meta_frame[,which(names(meta_frame)==as.character(part))]) %in% input[[as.character(part)]]) )
      )) %>% filter(alert_level %in% input[['alert_level']]) %>%
      select(metric_partition,  metric_num, prediction_accuracy, alert_level,  #os
             model_mean_error) %>%
      arrange(alert_level)
    
    
  })   
  
  output$chart <- reactive({
    # Return the data and options
    if(is.null(input[[as.character(factor_names[1])]])){
      list(
        data = googleDataTable(yearData()),
        options = list(
          title ="Metric vs Score",
          # input$prediction_error
          series = series
        )
      )} else {
        list(
          data = googleDataTable(yearData()),
          options = list(
            title ="Metric vs Score",
            # input$prediction_error
            series = series
          ))
        
      }
  })
  
  
  output$title_alert <- renderText({
    'Select alert levels to be displayed:'
  })
  
  output$title_dim <- renderText({
    'Select dimensions to be displayed:'
  })
  output$plot_title1 <- renderText({
    paste("30 days ",input$metric,input$partition," with baseline:",sep='-')
  })
  
  output$plot_title2 <- renderText({
    paste(paste("Original 90 days ",input$metric,input$partition,sep='-'),':')
  })
  
  output$plot_alert <- renderText({
    paste("Alert Level: ",meta_frame[which(input$metric==meta_frame$metric& meta_frame$partition==input$partition),]$alert_level,sep='')
  })
  output$plot_real <- renderText({
    paste("Real Value: ",meta_frame[which(input$metric==meta_frame$metric& meta_frame$partition==input$partition),]$real_value,sep='')
  })
  output$plot_upper <- renderText({
    paste("Baseline (high): ",predictions[which(test$metric==input$metric&test$partition==input$partition),30]+
            (test[which(test$metric==input$metric&test$partition==input$partition),92])*(
              meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_mean_error+
                2*meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_std_dev)
          ,sep='')
    
  })
  
  output$plot_under <- renderText({
    paste("Baseline (low): ",predictions[which(test$metric==input$metric&test$partition==input$partition),30]-
            (test[which(test$metric==input$metric&test$partition==input$partition),92])*(
              meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_mean_error+
                2*meta_frame[which(meta_frame$metric==input$metric&meta_frame$partition==input$partition),]$model_std_dev)
          ,sep='')
    
  })
}
