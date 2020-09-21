
library("ggplot2")
library("dplyr")
library("shiny")

library("tidyverse")

df_trans <- read_csv(file.choose()) %>% as_tibble()

ui <- fluidPage(
  titlePanel("Ploting Exrecise!!"),
    mainPanel(
             plotOutput(outputId = "plotgraph"),
             plotOutput(outputId = "plotgraph2"),
             plotOutput(outputId = "plotgraph3"),
             plotOutput(outputId = "plotgraph5")
      
    )
)

server <- function(input, output, session) {
  output$plotgraph <- renderPlot({
   
    df_trans %>% group_by(mid,hr) %>% summarise(success_rate=sum(success)/sum(t)) %>% ggplot(aes(hr,success_rate,color=mid)) + geom_line()
    
  }, res = 96)
  
  output$plotgraph2 <- renderPlot({
    
    df_trans %>% group_by(pmt,hr) %>% summarise(success_rate=sum(success)/sum(t)) %>% ggplot(aes(hr,success_rate,color=pmt)) + geom_line()
    
  }, res = 96)
  
  output$plotgraph3 <- renderPlot({
    
    df_trans %>% group_by(pg,hr) %>% summarise(success_rate=sum(success)/sum(t)) %>% ggplot(aes(hr,success_rate,color=pg)) + geom_line()
    
  }, res = 96)
  
  output$plotgraph5 <- renderPlot({
    
    df_trans %>% group_by(sub_type,hr) %>% summarise(success_rate=sum(success)/sum(t)) %>% ggplot(aes(hr,success_rate,color=sub_type)) + geom_line()
    
  }, res = 96)
}

shinyApp(ui,server)