# Import the necessary libraries. 
library(shiny)
library(shinyWidgets)

# Create the ui.
ui <- fluidPage(
    titlePanel("Exploring the Central Limit Theorem"),
    br(),
    
    # Create a tab for the description and a tab for the app.
    tabsetPanel(type = "tabs",
                # Description tab.
                tabPanel("Description",
                         br(),
                         h4("This app explores the central limit theorem when looking at the
                            distribution of sample means on the chi-square distribution."),
                         br(),
                         h4("The user is allowed to pick the mean, the sample size, and number of observations."),
                         br(),
                         h4("A graph for a single observation and one with multiple observations is displayed to give the 
                         user a visual understanding of the central limit theorem. 
                         The graph with the multiple observations shows how the distribution becomes normally distributed as the number of observations is increased."),
                         br(),
                         h4("For each graph, the sample parameters, such as mean, median, and standard deviation, are included.")
                ),
                
                # App tab.
                tabPanel("App",
                         # First row contains the user input for mean and sample size.
                         # Include the graph and descriptive statistics.
                         fluidRow(
                             column(4, 
                                    setSliderColor(c("mediumturquoise", "mediumturquoise", "mediumturquoise"), c(1, 2, 3)),
                                    
                                    h4("The Chi-square Distribution"),
                                    br(),
                                    
                                    sliderInput("mean", "Mean:", step = 0.1,
                                                min = 0, max = 50,
                                                value = 10),
                                    
                                    sliderInput("size", "Sample Size:",
                                                min = 0, max = 500,
                                                value = 50),
                             ),
                             
                             column(5,
                                    plotOutput("sampleDist"),
                             ),
                             
                             column(3,
                                    br(),
                                    h5("Population Parameters: "),
                                    htmlOutput("popstats"),
                                    br(),
                                    h5("Sample Parameters: "),
                                    htmlOutput("samplestats")
                             )
                         ), 
                         
                         # Second row contains the number of observations the user inputs.
                         # Include the graph and descriptive statistics.
                         fluidRow(
                             column(4,
                                    numericInput("obs", "Number of repetitions to view:", 1000),
                             ),
                             column(5,
                                    plotOutput("repeatedDist"),
                             ),
                             column(3,
                                    br(),
                                    h5("Theoretical Parameters: "),
                                    htmlOutput("theostats"),
                                    br(),
                                    h5("Sample Parameters: "),
                                    htmlOutput("repstats"),
                             )
                         )
                )
    )
)

server <- function(input, output) {

    # Output the plot with multiple observations.
    output$repeatedDist <- renderPlot ({
        data = matrix(rchisq(input$obs * input$size, input$mean), input$size)
        x_bar = colMeans(data)
        hist(x_bar, main = "Distribution of the sample mean with varied repetitions", prob = T, xlab = expression(bar(x)))
        c = density(x_bar)
        lines(c$x, c$y, col = "mediumturquoise", lwd = 3)
        curve(dnorm(x, input$mean, sqrt(2 * input$mean)/sqrt(input$size)), input$mean - 3 * sqrt(2 * input$mean)/sqrt(input$size),
              input$mean + 3 * sqrt(2 * input$mean)/sqrt(input$size), add = T, lwd = 3, col = "palevioletred1")
        legend("topright", legend = c("Normal Distribution", "Kernel Density"), 
               lty = 1, lwd = 3, col = c("palevioletred1", "mediumturquoise"))

    })
    
    # Print the theoretical statistics. 
    output$theostats <- renderUI({
        str1 <- paste("mean: ", format(round(input$mean, 2), nsmall = 2))
        str2 <- paste("median: ", format(round(input$mean, 2), nsmall = 2))
        str3 <- paste("standard deviation: ", format(round(sqrt(2 * input$mean)/sqrt(input$size), 2), nsmall = 2))
        HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
    # Print the statistics from taking multiple observations. 
    output$repstats <- renderUI({
        data = matrix(rchisq(input$obs * input$size, input$mean), input$size)
        x_bar = colMeans(data)
        str1 <- paste("mean: ", format(round(mean(x_bar), 2), nsmall = 2))
        str2 <- paste("median: ", format(round(median(x_bar), 2), nsmall = 2))
        str3 <- paste("standard deviation: ", format(round(sd(x_bar), 2), nsmall = 2))
        HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    # Display the plot from the single sample. 
    output$sampleDist <- renderPlot({
        data <- rchisq(input$size, input$mean)
        c = density(data)
        hist(data, main = "Distribution of the sample mean with one repetition", prob = T, 
             xlim = c(0, max(c$x)), xlab = expression(bar(x)))
        lines(c$x, c$y, col = "mediumturquoise", lwd = 3)
        curve(dchisq(x, input$mean), add = T, lwd = 3, col = "palevioletred1")
        legend("topright", legend = c("Chi-square Distribution", "Kernel Density"), 
               lty = 1, lwd = 3, col = c("palevioletred1", "mediumturquoise"))
    })
    
    # Display the statistics from the population. 
    output$popstats <- renderUI({
        str1 <- paste("mean: ", format(round(input$mean, 2), nsmall = 2))
        str2 <- paste("median: ", format(round(input$mean*((1-2/(9*input$mean))^3), 2), nsmall = 2))
        str3 <- paste("standard deviation: ", format(round(sqrt(2 * input$mean), 2), nsmall = 2))
        HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
    # Display the statistics from the sample. 
    output$samplestats <- renderUI({
        data <- rchisq(input$size, input$mean)
        str1 <- paste("mean: ", format(round(mean(data), 2), nsmall = 2))
        str2 <- paste("median: ", format(round(median(data), 2), nsmall = 2))
        str3 <- paste("standard deviation: ", format(round(sd(data), 2), nsmall = 2))
        HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    # Display the slider values.
    sliderValues <- reactive({
        data.frame(
            Name = c("Mean","Sample Size"),
            Value = as.character(c(input$mean,
                                   input$size, 
                                   )),
            stringsAsFactors = FALSE)
        
    })
    
}

# Run the app.
shinyApp(ui, server)