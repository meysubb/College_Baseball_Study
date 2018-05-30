#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(highcharter)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  output$mytable <- renderDataTable({
    standings_df <- pyth_raw %>% filter(conference == input$conf) %>% select(-conference)
    
    datatable(standings_df, rownames = FALSE,
              colnames = c('Team' = 1, 'W %' = 4,'Pyth W %'=5),
              options = list(order = list(list(3, 'desc'))
              )) %>% formatPercentage('W %', 3) %>% formatPercentage('Pyth W %',3)  
  })
   
  output$plot2 <- renderPlot({
    plot_confrence(input$conf)
  })  
  
  output$plot3 <- renderPlot({
    run_dist(input$conf)
  })
  
  output$plot4 <- renderPlot({
    if(input$select==1){
      typ="conf"
    }else{
      typ="team"
    }
    rolling_plots(type = typ,conference_name = input$conf2,team = input$team)
  })
  
  output$plot5 <- renderHighchart({
    df_pca <- pca_func(input$conf2)
    km_df <- k_means_cluster(df_pca)
    opg_hc_plot(km_df)
  })
  
  
})
