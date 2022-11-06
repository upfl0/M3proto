#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Cairo)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(ggwordcloud)
options(shiny.usecairo=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("M3proto"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput('pruefer', 
                     'Prüfer', 
                     choices = c("Frey", "Seyfried"), 
                     multiple = TRUE,
                     selected = "Frey"
      ),
      sliderInput("top_n",
                  "Anzahl Top Themen:",
                  min = 1,
                  max = 20,
                  value = 10),
      selectizeInput('skill_type', 
                     'Theoretisch (Wissen) oder Praktisch (Voruntersuchen)', 
                     choices = c("Theoretisch", "Praktisch"), 
                     multiple = TRUE,
                     selected = "Theoretisch"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot_1"),
      plotlyOutput("plot_2"),
      plotOutput("plot_3")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  IN <- readxl::read_xlsx("Protokolle-Auswertung.xlsx")
  myColors <- colorRampPalette(brewer.pal(12, "Paired") )(length(unique(IN$tags)))
  names(myColors) <- unique(IN$tags)
  
  output$plot_1 <- renderPlotly({
    
    p1_data <- IN %>% 
      filter(pruefer %in% input$pruefer) %>% 
      filter(skill %in% input$skill_type) %>%
      slice_max(n, n = input$top_n) %>% 
      arrange(desc(n)) %>% 
      mutate(thema = factor(thema, ordered = TRUE, levels = thema))

    plot_ly(p1_data, 
            x = ~thema, 
            y = ~n, 
            color = ~tags, 
            colors = myColors,
            text = ~specials,
            meta = ~pruefer,
            hovertext = ~skill,
            hovertemplate = paste('%{label}, n = %{y}',
                                  '<br>%{hovertext}',
                                  '<br><b>Prüfer</b>: %{meta}',
                                  '<br><i>Specials</i>: %{text}'),
            type = "bar") %>%
      layout(xaxis = list(title = "Thema", tickangle=45), yaxis = list(title = "Häufigkeit"), legend=list(title=list(text='<b> Kategorie </b>')))
  })
  
  output$plot_2 <- renderPlotly({
    
    p2_data <- IN %>% 
      filter(pruefer %in% input$pruefer) %>% 
      filter(skill %in% input$skill_type) %>%
      group_by(tags) %>%
      summarise(m = sum(n)) %>%
      arrange(desc(m)) %>% 
      mutate(tags = factor(tags, ordered = TRUE, levels = tags))
    
    plot_ly(p2_data, 
            x = ~tags, 
            y = ~m, 
            color = ~tags, 
            colors = myColors,
            type = "bar") %>%
      layout(xaxis = list(title = "Kategorie", tickangle=45), yaxis = list(title = "Häufigkeit"), showlegend = FALSE)
  })
  
  output$plot_3 <- renderPlot({
  
    ggplot(IN, aes(label = thema, color = tags, size = n)) +
      geom_text_wordcloud() +
      scale_color_manual(values = myColors) +
      theme_minimal() +
      theme(legend.position="bottom")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
