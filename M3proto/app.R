#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Rtsne)
library(ggplot2)
library(Cairo)
options(shiny.usecairo=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("6.419x Module 2 Problem 3.2 by florian_rumpf"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_pc",
                  "Number of PC's chosen for clustering:",
                  min = 10,
                  max = 511,
                  value = 50),
      sliderInput("perplexity",
                  "T-SNE perplexity:",
                  min = 5,
                  max = 100,
                  value = 40,
                  step = 5),
      sliderInput("theta",
                  "T-SNE learning rate:",
                  min = 0,
                  max = 1,
                  value = 0.5, 
                  step = 0.1),
      sliderInput("num_clusters",
                  "BONUS: Number of clusters for Kmeans clustering:",
                  min = 2,
                  max = 8,
                  value = 3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("tsnePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tsnePlot <- renderPlot({
    # load data
    load("p1_X_log_pca.RData")
    
    
    #original code
    tsne_PCA <- Rtsne(pca_log$x[,1:input$num_pc], dims = 2, perplexity = input$perplexity, theta = input$theta, verbose = TRUE, max_iter = 500, pca = FALSE)
    
    plot_data_tsne_PCA <- as.data.frame(tsne_PCA$Y)
    
    plot_data_tsne_PCA$Cluster <- as.factor(kmeans(tsne_PCA$Y, centers = input$num_clusters)$cluster)
    
    print(ggplot(data = plot_data_tsne_PCA, aes(V1, V2, fill = Cluster, color = Cluster)) +
            geom_point(shape = 21, alpha = 0.5, size = 2) +
            coord_equal(ratio = 1) +
            theme_classic() +
            scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
            scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
            stat_ellipse() +
            labs(title = "TSNE")
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
