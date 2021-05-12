
library(shiny)
library(ggplot2)
library(ggpubr)

heart_data = read.csv("heart.csv")
categorical_data <- c('age', 'trestbps', 'chol', 'thalach')
vars <- setdiff(names(heart_data), c("sex", "cp", "fbs", "restecg", "exang", "oldpeak"))

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Descriptive Statistics for the Heart Data"),
    
    # Different tabs
    tabsetPanel(type = "tabs",
                
                # data overview tab
                tabPanel("Data",
                         h3("Data preview:"),
                         br(),
                         DT::dataTableOutput("data_table"),
                ),
                
                # dataset description tab
                tabPanel("Description",
                         includeMarkdown("heartStuff.Rmd"),
                ),
                
                # analysis tab
                tabPanel("Analysis",
                         fluidRow(
                             column(width = 6,
                                    h3("Linearity:"),
                                    p("Explore how including certain points can influence the correlation coefficient and confidence interval band!"),
                                    p("Try clicking on a data point to include/exclude it."),
                                    br(),
                                    sidebarPanel(
                                        selectInput("x_axis",
                                                    label = h4("Select an x-axis variable:"),
                                                    choices = categorical_data,
                                                    selected = categorical_data[2],
                                        ),
                                        selectInput("y_axis",
                                                    label = h4("Select a y-axis variable:"),
                                                    choices = categorical_data,
                                                    selected = categorical_data[1],
                                        )
                                    ),
                                    mainPanel(
                                        plotOutput("plot1", height = 350,
                                                   click = "plot1_click",
                                                   brush = brushOpts(
                                                       id = "plot1_brush"
                                                   )
                                        ),
                                        actionButton("exclude_toggle", "Toggle points"),
                                        actionButton("exclude_reset", "Reset"),
                                    ),
                             ),
                             column(width = 6,
                                    h3("K-means clustering"),
                                    p("Explore how certain data points are grouped together by certain similarities!"),
                                    br(),
                                    p("Try changing the number of clusters."),
                                    br(),
                                    sidebarPanel(
                                        selectInput('xcol', 'Select an x-axis variable:', vars),
                                        selectInput('ycol', 'Select a y-axis variable:', vars, selected = vars[[2]]),
                                        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
                                    ),
                                    mainPanel(
                                        plotOutput('plot2')
                                    )
                             ),
                         ),
                ),
                
                # visualization tab
                tabPanel("Visualization", 
                         h4("Histogram"),
                         selectInput("col_dropdown", 
                                     label = h5("Select a column:"), 
                                     choices = colnames(heart_data), 
                                     selected = colnames(heart_data)[1]),
                         plotOutput("distPlot"),
                         
                         h4("Box Plot"),
                         selectInput("col_dropdown2", 
                                     label = h5("Select a column:"), 
                                     choices = colnames(heart_data), 
                                     selected = colnames(heart_data)[1]),
                         plotOutput("distPlot2")
                )
                
    )
)

# Define server logic required to draw a histogram/bar chart
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        col_input = input$col_dropdown
        x = heart_data[ , col_input]
        
        fig = ggplot(data=heart_data, aes_string(x=col_input))
        
        fig = fig + geom_histogram(color="black", fill="lightblue")
        plot_title = paste("A histogram of the", col_input)
 
        fig = fig + ggtitle(plot_title) +
            theme(plot.title = element_text(hjust = 0.5, size=22),
                  axis.title=element_text(size=15,face="bold"))
        fig
    })
    
    output$distPlot2 <- renderPlot({
        
        col_input = input$col_dropdown2
        x = heart_data[ , col_input]
        
        boxplot(x, data=heart_data, xlab = col_input, ylab = "frequency", col="coral", main = paste("A boxplot of the", col_input))
    })
    
    # data preview
    output$data_table <- DT::renderDataTable({
        heart_data
    })
    
    # For storing which rows have been excluded
    vals <- reactiveValues(
        keeprows = rep(TRUE, nrow(heart_data))
    )
    
    output$plot1 <- renderPlot({
        # Plot the kept and excluded points as two separate data sets
        keep    <- heart_data[ vals$keeprows, , drop = FALSE]
        exclude <- heart_data[!vals$keeprows, , drop = FALSE]
        
        ggplot(keep, aes_string(input$x_axis, input$y_axis)) + geom_point() +
             geom_smooth(method = lm, fullrange = TRUE, color = "red") +
             geom_point(data = exclude, shape = 21, fill = NA, color = "red", alpha = 0.25) +
             stat_cor()
    })
    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
        res <- nearPoints(heart_data, input$plot1_click, allRows = TRUE)
        
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
        res <- brushedPoints(heart_data, input$plot1_brush, allRows = TRUE)
        
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
        vals$keeprows <- rep(TRUE, nrow(heart_data))
    })
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        heart_data[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot2 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
