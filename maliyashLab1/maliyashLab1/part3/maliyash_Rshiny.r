library(shiny)
options(rsconnect.max.bundle.size=3145728000)
u <- shinyUI(fluidPage(
  titlePanel(title = 'Heat Map Comparision',),
  sidebarLayout(sidebarPanel(
    tabsetPanel(type = c('tabs'), 
                tabPanel('Options', selectInput('HMap', label = 'Choose a Heat Map', 
                                                       choices = c('CDC', 'All Keyword Tweets', 'CDC and All Keyword Tweets','Two Keyword Tweets')), 
                         uiOutput('option1'), 
                         uiOutput('option2'), 
                         uiOutput('option3'),
                         uiOutput('option4')
                         
                )
    )
    
    
  ),
  mainPanel(imageOutput('plot1', width="auto", height="auto"), imageOutput('plot2', width="auto", height="auto"),
            textOutput("selected_var")
            
  )
  
  
  
  
  )
))
s <- function(input, output) {
  
  
 
  output$selected_var <- renderText({ 
    "You have selected this"
  })
  
  
  output$option1 = renderUI({
    if (input$HMap == 'CDC') {
      output$plot1 <- renderImage({
        return(list(
          src = "All_tweets.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      
      output$plot2 <- renderImage({
        return(list(
          src = "blank.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      
    }})
  output$option2 = renderUI({
    if (input$HMap == 'All Keyword Tweets') {
      output$plot1 <- renderImage({
        return(list(
          src = "HeatMap_For_all_words.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      
      output$plot2 <- renderImage({
        return(list(
          src = "blank.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      
      
      
    }})
  
  
  
  
 
  output$option3 = renderUI({
    if (input$HMap == 'CDC and All Keyword Tweets') {
      output$plot1 <- renderImage({
        return(list(
          src = "All_tweets.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      output$plot2 <- renderImage({
        return(list(
          src = "HeatMap_For_all_words.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
    }
  })
  
  output$option4 = renderUI({
    if (input$HMap == 'Two Keyword Tweets') {
      output$plot1 <- renderImage({
        return(list(
          src = "Heat_map_for_2_keywords.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      
      output$plot2 <- renderImage({
        return(list(
          src = "blank.png",
          contentType = "image\\png",
          alt = "Image Not Found"
        ))
      }, deleteFile = FALSE)
      
      
      
    }})
  

  
}
shinyApp(ui = u, server = s)


