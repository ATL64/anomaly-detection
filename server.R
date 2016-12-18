library(dplyr)
function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$plotos <-
    renderPlot({
      
        df<-as.data.frame(rbind(
        original=test[which(test$metric==input$metric&test$partition==input$partition),63:92],
        prediction=predictions[which(test$metric==input$metric&test$partition==input$partition),],
        date=names(test)[63:92]
      ))
        


      dft<-as.data.frame(t(df))
      dft$original<-as.numeric(as.character(dft$original))
      dft$prediction<-as.numeric(as.character(dft$prediction))
      tall <- with(dft,
                   data.frame(value = c(original, prediction),
                              variable = factor(rep(c("original","prediction"),
                                                    each = NROW(dft))),
                              Dates = rep(as.Date(date), 2)))
      
      ggplot(tall, aes(Dates, value, colour = variable))+ geom_line()
      
      }, bg="transparent")
  

  output$original_ts <-
    renderPlot({
        dfa<-as.data.frame(rbind(
        original=test[which(test$metric==input$metric&test$partition==input$partition),3:92],
        date=names(test)[3:92]
      ))
        

      
      dfo<-as.data.frame(t(dfa))
      dfo$original<-as.numeric(as.character(dfo$original))
      tallOriginal <- with(dfo,
                   data.frame(value = original,
                              variable = factor(rep(c("original"),
                                                    each = NROW(dfo))),
                              Dates = rep(as.Date(date), 1)))
      
      ggplot(tallOriginal, aes(Dates, value, colour = variable))+ geom_line()
      
      #   theme(axis.text.x = element_text(size=8, angle=0))
      
      }, bg="transparent") 

  
  output$tablea <-renderDataTable({
    meta_frame[,-which(names(meta_frame) %in% c('metric','prediction_perc_error_abs'))]
  },options = list(lengthMenu = c(10, 25, 50,100), pageLength = 10))
  
  
  lapply(test_factors_names, function(part) { 
    observeEvent(input[[paste("all",as.character(part),sep='_')]], {  
        if (is.null(input[[as.character(part)]])) {
      updateCheckboxGroupInput(
        session = session, inputId = part, selected = unique(test[, which(names(test) == as.character(part))]))
      
    } else {
  updateCheckboxGroupInput(
    session = session, inputId = as.character(part) , selected = " "
  )
  }})})   
 
  output$input_ui <- renderUI({
    lapply(test_factors_names, function(part) {
 
                        dropdownButton(label = as.character(part), status = "default", width = 100,br(),
                                       actionButton(inputId = paste("all",as.character(part),sep='_'), label = "(Un)select all"),
                      checkboxGroupInput(inputId = as.character(part), label = "Choose", 
                                         choices = unique(test[, which(names(test) == as.character(part))]))
     )
    })
 
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
          )) %>%
          select(metric_partition,  metric_num, prediction_accuracy, alert_level,  #os
             model_std_dev) %>%
          arrange(alert_level)
  
    
  })   

  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Model historical error vs today's error",
          # input$prediction_error
          0),
        series = series
      )
    )
  })
  
}

