

pacman::p_load(shiny, shinythemes, tidyverse, rpart,rpart.plot, shinyWidgets)
set.seed(123)

# loading data
hotel_data <- read.csv('data/hotel.csv')

# split train and test
index <- sample(1:601, size = trunc(.8 * 601))
train_df <- hotel_data %>%
  filter(row_number() %in% index)

test_df <- hotel_data %>%
  filter(!(row_number() %in% index))

var_list <- names(hotel_data)[3:33]
var_list_default <- var_list[1:3]


ui <- shinyUI(fluidPage(
  sidebarPanel(
    pickerInput(
      inputId = "model_vars",
      label = NULL,  # label given in outer code
      choices = var_list,
      selected = var_list_default,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),
    actionButton(
      inputId = "createModel",
      label = "Create Model",
      class = "btn-primary"  # makes it blue!
    ),
    h4("Minimum Split"),
    helpText(
      "If at a given node N is below this value, that node cannot",
      "be split any further: it is a terminal node of the tree."
    ),
    sliderInput(
      inputId = "min_split",
      label = NULL,  # label given in outer code
      min = 2,       # two is the smallest that could be split
      max = 10,      # chosen to not make the models too wild
      value = 2      # defaults to not having an artifical minimum
    ),
    
    br(),
    
    h4("Minimum Bucket Size"),
    helpText(
      "If creating a given split would cause N₁ or N₂ to fall below",
      "this minimum, then that split isn't made part of the",
      "decision tree."
    ),
    sliderInput(
      inputId = "min_bucket",
      label = NULL,  # label given in outer code
      min = 1,       # can't have buckets of size zero
      max = 30,      # rpart default is minbucket = 3*minsplit
      value = 1     # defaults to not having an artifical minimum
    ),
    br(),
    h4("Maximum Tree Depth"),
    helpText(
      "Control the maximum depth that the decision tree can reach.",
      "Note that, depending on what features are being used and the",
      "values of the other parameters, you may end up with a tree",
      "much shallower than the maximum."
    ),
    sliderInput(
      inputId = "max_depth",
      label = NULL,  # label given in outer code
      min = 2,       # a min of 2 allows for at least one split
      max = 30,      # rpart can't do 31+ depth on 32-bit machines
      value = 5      # chosen to not make the default too wild
    )
  ),
  mainPanel(
    plotOutput("plot")
  )
))

server <- function(input, output) {
  
  # Subset the data based on user input
  data <- reactive({
    train_df[, c(input$model_vars, "IsCanceled")]
  })
  
  # Train the model based on user input
  model = eventReactive(
    eventExpr = input$createModel,
    valueExpr = rpart(
      as.formula(paste("IsCanceled ~."), data, type = "class")
  )
  )
  
  # Plot the results
  output$plot <- renderPlot({
    plot(model()$finalModel)
  })
  
}

# ------- Run the application -------
shinyApp(ui = ui, server = server)