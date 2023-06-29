library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Clinical Trial Analytics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Select Variable:",
        choices = c("Age", "Gender", "Treatment", "BMI", "BloodPressure", "Cholesterol"),
        selected = "Age"
      ),
      checkboxGroupInput(
        inputId = "filter",
        label = "Filter by:",
        choices = c("Gender", "Treatment"),
        selected = c("Gender", "Treatment")
      ),
      selectInput(
        inputId = "plot_type",
        label = "Select Plot Type:",
        choices = c("Histogram", "Boxplot", "Density"),
        selected = "Histogram"
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Generate random data for demonstration purposes
  set.seed(123)
  n <- 500
  data <- data.frame(
    Age = rnorm(n, mean = 50, sd = 10),
    Gender = sample(c("Male", "Female"), size = n, replace = TRUE),
    Treatment = sample(c("A", "B", "C"), size = n, replace = TRUE),
    BMI = rnorm(n, mean = 25, sd = 5),
    BloodPressure = rnorm(n, mean = 120, sd = 10),
    Cholesterol = rnorm(n, mean = 200, sd = 30)
  )
  
  # Create filtered data based on selected variables
  filtered_data <- reactive({
    data <- data
    if ("Gender" %in% input$filter) {
      data <- data[data$Gender == input$gender, ]
    }
    if ("Treatment" %in% input$filter) {
      data <- data[data$Treatment == input$treatment, ]
    }
    data
  })
  
  # Create plot based on selected variable, plot type, and filtered data
  output$plot <- renderPlot({
    data <- filtered_data()
    if (input$plot_type == "Histogram") {
      ggplot(data, aes_string(x = input$variable)) +
        geom_histogram(fill = "lightblue", color = "black") +
        labs(x = input$variable, y = "Count") +
        theme_minimal()
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = input$variable, y = "")) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(x = input$variable, y = "") +
        theme_minimal()
    } else if (input$plot_type == "Density") {
      ggplot(data, aes_string(x = input$variable)) +
        geom_density(fill = "lightblue", color = "black") +
        labs(x = input$variable, y = "Density") +
        theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
