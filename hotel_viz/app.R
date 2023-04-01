# ISSS608 Visual Analytics and Applications
# Group Project: Hotel Data Analytics Dashboard
# Authors: Liang Minghao, Ashley Choi, Christiana Lim
# Affliated to: Singapore Management University


### --------- Getting Started ---------

pacman:: p_load(ggplot2, plotly, tidyverse, shiny, gt, gtsummary, ggalluvial, gmodels,
                DT, ggstatsplot, shinyWidgets, ggiraph, billboarder, networkD3, rpart, rpart.plot, caret)
set.seed(123)

# loading data
# hotel_data <- read.csv('data/hotel.csv')
hotel_data <- read.csv('data/hotel_data_merged.csv')

# only subset 50000 rows due to limit of the server
size=nrow(hotel_data)
index <- sample(1:size, size = 50000)
hotel_data <- hotel_data %>%
  filter(row_number() %in% index)

# Dataset for paralll sets  
par_data <- hotel_data[,c(31, 20, 24, 37)] %>%
  select(CustomerType, DistributionChannel, ReservedRoomType,HotelType) %>%
  count(CustomerType, DistributionChannel, ReservedRoomType,HotelType)

# split train and test

hotel_data <- hotel_data %>% 
  mutate(Meal = str_trim(Meal)) %>% 
  select(-Agent,-Company)

data_size=nrow(hotel_data)
index <- sample(1:data_size, size = 25000)
train_df <- hotel_data %>%
  filter(row_number() %in% index)

test_df <- hotel_data %>%
  filter(!(row_number() %in% index))

# for decision tree
var_list <- list(
  "No. of Non-Canceled Previous Bookings"="PreviousBookingsNotCanceled", 
  "No. of Previous Cancallations"="PreviousCancellations",  
  "No. of Special Requests"="TotalOfSpecialRequests",
  "No. of Changes Made"="BookingChanges",  
  "Hotel Type"="HotelType",    
  "Customer Type"="CustomerType", 
  "Deposit Type"="DepositType",   
  "Lead Time"="LeadTime",                   
  "Distribution Channel"="DistributionChannel", 
  "Reserved Room Type"="ReservedRoomType", 
  "Assigned Room Type"="AssignedRoomType",
  "Avg. Daily Rate"="ADR",
  "No. of Adults"="Adults",
  "No. of Babies"="Babies",
  "No. of Children"="Children",
  "No. of Days in Waiting List"="DaysInWaitingList",          
  "Is Repeated Guest?"="IsRepeatedGuest",             
  "Required CarParking Spaces?"="RequiredCarParkingSpaces"
)
var_list_default <- var_list[1:5]

# for LR model
lr_var_list <- list(
  "No. of Non-Canceled Previous Bookings"="PreviousBookingsNotCanceled", 
  "No. of Previous Cancallations"="PreviousCancellations",  
  "No. of Special Requests"="TotalOfSpecialRequests",
  "No. of Changes Made"="BookingChanges",  
  "Hotel Type"="HotelType",    
  "Customer Type"="CustomerType", 
  "Deposit Type"="DepositType",   
  "Lead Time"="LeadTime",                   
  "Distribution Channel"="DistributionChannel", 
  "Reserved Room Type"="ReservedRoomType", 
  "Assigned Room Type"="AssignedRoomType",
  "Avg. Daily Rate"="ADR",
  "No. of Adults"="Adults",
  "No. of Babies"="Babies",
  "No. of Children"="Children",
  "No. of Days in Waiting List"="DaysInWaitingList",          
  "Is Repeated Guest?"="IsRepeatedGuest",             
  "Required CarParking Spaces?"="RequiredCarParkingSpaces"
)
lr_var_list_default <- lr_var_list[1:5]



### --------- functions --------

para_plot <- function(variables, color){
  data <- par_data
  len <- length(variables)
  if(len==2){
    g <- ggplot(data = par_data,
                aes(axis1 = variables[1], y = n))
  }
  else if(len==3){
    g <- ggplot(data = par_data,
                aes(axis1 = !!sym(variables[1]), axis2 = !!sym(variables[2]), y = n))
  }
  else if(len==4){
    g <- ggplot(data = par_data,
                aes(axis1 = !!sym(variables[1]), axis2 = !!sym(variables[2]), axis3 = !!sym(variables[3]), y = n))
  }
  else {
    g <- ggplot(data = par_data,
                aes(axis1 = !!sym(variables[1]), axis2 = !!sym(variables[2]), axis3 = !!sym(variables[3]),
                    axis4 = !!sym(variables[4]), y = n))
  }
  g <- g +
    ylab("No. of Bookings") +
    geom_alluvium(aes(fill = !!sym(color))) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    ggtitle("Parallel Plot")
  
  return(g)
}

# Train Tree Model
makeTree = function(model_vars, min_split, min_bucket, max_depth) {
  # Takes a list of model variables (strings), a minimum split parameter
  # (int), a minimum bucket size parameter (int), and a maximum tree depth
  # parameter (int) as inputs and then returns an rpart classification tree
  # created with those parameters using Gini-indexes without any pruning.
  
  train_dat = train_df
  # create an rpart compatible formula for the model from the chosen vars
  f = paste("IsCanceled ~ ", paste(model_vars, collapse = " + "))
  # rpart is performs the split calculations and returns the tree
  tree = rpart(
    as.formula(f),
    method = "class",  # sets it up as a classification problem
    data = train_dat,
    parms = list(prior = c(.5,.5), split = "information"),  # ensures rpart uses gini indexes
    minsplit = min_split,
    minbucket = min_bucket,
    maxdepth = max_depth,
    cp = 0  # complexity parameter, at zero prevents pruning on branches
  )

  return(tree)
}

# Predict Tree
useTree = function(tree,df) {
  # Takes a tree generated by rpart and a filename (string) as input and
  # then predicts the labels of the data in that file using the tree. It
  # returns a dataframe with two bool (0,1) columns: prediction and truth.
  
  data = df
  prediction = predict(tree, data, type = "class")
  results = as.data.frame(prediction)
  results$truth = data$IsCanceled
  
  return(results)
}

# Evaluation
calcScores = function(results) {
  
  results = table(results)
  
  # Calculate accuracy
  accuracy <- round((results[1,1]+results[2,2])/sum(results),3)*100
  
  # Calculate precision
  precision <- round(results[2,2]/sum(results[2,]),3)*100
  
  # Calculate recall
  recall <- round(results[2,2]/sum(results[,2]),3)*100
  
  # the collapse argument removes the spacing which would otherwise be there
  return(list(
    paste(c("Overall Accuracy: ",   accuracy, "%"), collapse = ""),
    paste(c("Precision: ", precision, "%"), collapse = ""),
    paste(c("Recall: ", recall, "%"), collapse = "")
  ))
}

# Confusion Matrix
resultsTable = function(results) {

  data = table(results)
  Outcomes = c("Predicted Not Cancelled", "Predicted Cancelled", "Total")
  # reconstruct the columns of R's table(...) CLI display
  c1 = c(data[, 1], sum(data[, 1]))  #TN, FN, Sum(TN, FN)
  c2 = c(data[, 2], sum(data[, 2]))  #FP, TP, Sum(TP, FP)
  c3 = c(sum(data[, 1]), sum(data[2, ]), sum(data))
  
  # turn these columns back into a dataframe but with proper headers
  output = data.frame(Outcomes)
  output$"Actually Not Cancelled" = c1
  output$"Actually Cancelled" = c2
  output$"Total" = c3
  
  return(output)
}

# Corr Heatmap
corrplot_num = function(df){
  df <- na.omit(df)
  numeric_var <- names(df)[sapply(df[names(df)], is.numeric)]
  num_df <- df[, numeric_var[2:length(numeric_var)]]
  
  # calculate the correlation matrix
  corr_mat <- cor(num_df)
  
  # Create correlation heatmap with plotly
  p <- plot_ly(z = corr_mat, type = "heatmap", colorscale = "RdBu", reversescale = TRUE,
          x = colnames(corr_mat), y = colnames(corr_mat)) %>%
    layout(title = "Correlation Heatmap of Numerical Variables",
           xaxis = list(tickangle = 30),
           yaxis = list(showticklabels = FALSE))
  
  return(p)
}

# Logistic Regression
makeLR = function(model_vars){
  data = train_df
  
  data$HotelType=as.factor(data$HotelType)  
  data$ArrivalDateMonth=as.factor(data$ArrivalDateMonth)  
  data$Meal=as.factor(data$Meal)
  data$DistributionChannel=as.factor(data$DistributionChannel)  
  data$ReservedRoomType=as.factor(data$ReservedRoomType)  
  data$AssignedRoomType=as.factor(data$AssignedRoomType)  
  data$DepositType=as.factor(data$DepositType)   
  data$CustomerType=as.factor(data$CustomerType)
  
  # create an rpart compatible formula for the model from the chosen vars
  f = paste("IsCanceled ~ ", paste(model_vars, collapse = " + "))
  
  lr_model=glm(as.formula(f),data=data,family=binomial(link="logit"))
  
  return(lr_model)
}

useLR = function(model, df) {
  # Takes a tree generated by rpart and a filename (string) as input and
  # then predicts the labels of the data in that file using the tree. It
  # returns a dataframe with two bool (0,1) columns: prediction and truth.
  
  data = df
  prediction = predict(model, data, type = "response")
  prediction = as.factor(ifelse(prediction>0.5,1,0))
  results = as.data.frame(prediction)
  results$truth = data$IsCanceled

  return(results)
}

# Plot importance of variables
var_imp_plot = function(varImp){
  
  # calculate variable importance
  varImp <- varImp %>% 
    arrange(desc(Overall)) %>%
    rownames_to_column(var = "Variable") %>%
    slice_head(n = 10)  %>%
    mutate(Variable = str_trim(Variable))

  
  # create a bar chart of the variable importance measures
  p <- plot_ly(data = varImp, y = ~Variable, x = ~Overall, type = "bar", color = "#eab676") %>%
    layout(title = "Variable Importance Measures",
           yaxis = list(
             title = "Predictor Variable",
             categoryorder = "total ascending"),
           xaxis = list(title = "Importance")
    )
  
  return(p)
}


# ------- Shiny UI --------

ui <- navbarPage(
  
  title = "HotelViz",
  fluid = TRUE,
  theme='simplex',
  id = "navbarID",
  
  tabPanel(
    "Welcome Page",
     icon = icon('person-chalkboard'),
     mainPanel(
       h2("HotelViz - The Hotel Data Analytical Application"),
       hr(),
       '- The Hotel Data Analytical Application aims to provide visualizations for data analytics, leading to better decision-making. ', 
       br(),
       tags$a(href="https://github.com/mtlmh34/Visual-Analytics-Grp-Project/blob/main/", 
              "- Github Repository"),
       br(),br(),
       
       h3('About The Application'),hr(),
       fluidRow(
         column(4, HTML(paste("<center> <b>", 'Know Your Customers', "</b></center> ")),
                br(),
                ' - Data visualizations about customer\'s activities, demographic informations, etc. '),
         column(4, HTML(paste("<center> <b>", 'Know Your Business', "</b></center> ")),
                br(),
                '- Data visualizations about key metrics related to the hotel\'s performance, such as average booking price and cancellation rate. '),
         column(4, HTML(paste("<center> <b>", 'Predictive Analysis', "</b></center> ")),
                br(),
                '- Contains two type of machine learning model (Decicion Tree/ Logistic Regression) to predict the possible cancellation of customers. ')
       ), 
       br(),br(),
       
       h3('Other Links'),hr(),
       fluidRow(
         column(5, tags$a(href="https://visual-analytics-2023jan-group1.netlify.app/", 
                "- Project HomePage"),br(),
         tags$a(href="https://visual-analytics-2023jan-group1.netlify.app/user_guide.html", 
                "- User Guide"))
       ),
       br(),br(),hr(),
       helpText('Hope you enjoy our App!')
     )
  ),
  
  ######### Page 1
  navbarMenu("Know Your Customers",
             icon = icon('address-card'),
             tabPanel("Exploratory Data Analysis",
                      titlePanel("Exploratory Data Analysis"), 
                      sidebarLayout(
                        sidebarPanel(
                          h4("Variable Selection"),
                          selectInput("HotelType", "Select Hotel:",
                                      choices = c("All", unique(hotel_data$HotelType))),
                          selectInput("ArrivalDateMonth", "Select Month:",
                                      choices = c("All", unique(hotel_data$ArrivalDateMonth))),
                          selectInput("Continent", "Select Continent:",
                                      choices = c("All", unique(hotel_data$Continent))),
                          selectInput("IsCanceled", "Select Booking Status:",
                                      choices = c("All", "Canceled", "Not Canceled")),
                          submitButton("Apply Changes")
                          
                        ),
                        mainPanel(
                          h2("What do we know about our Guests?", style = "font-sil10pt"),
                          br(),
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Summary", 
                                     fluidRow(
                                       column(4, gt_output("summary_table")),
                                       h4("Reactive Summary Table"),
                                       column(6, DTOutput("summary_table1"))
                                     )
                            ),
                            tabPanel("Understand", 
                                     fluidRow(
                                       column(10, plotlyOutput("bubble_plot"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(4,girafeOutput("special_barplot")), 
                                       column(4,girafeOutput("carpark_barplot")),
                                       column(4,girafeOutput("meals_barplot"))
                                     )
                            ),
                            tabPanel("Customer Type", 
                                     fluidRow(
                                       column(6, billboarderOutput("pie")),
                                       column(6, billboarderOutput("pie1"))
                                     )
                            ),
                            tabPanel("Read Me",
                                     p("This is the Read Me explanation on how to navigate the EDA section of this app."), 
                                     strong("Summary Tab"),
                                     p("In this tab, two tables are displayed.The left table is a static table displaying overall summary statistic of the Hotel Dataset while the right table is a reactive table that displays subset of data according to user input filters."),
                                     p("1. Select desired variables to explore for Hotel, Month, Continent, Booking Status."),
                                     p("2. Click Apply Changes to see the updated summary table under 'Reactive Summary Table'"),
                                     strong("Understand Tab"),
                                     p("In this tab, interactive bubble plots and barplots are displayed to understand the requests of guests"),
                                     p("1. Select desired variables to explore for Hotel, Month, Continent, Booking Status."),
                                     p("2. Click Apply Changes to see the updated information on Bubble Chart as well as bar plots to assess operational needs."), 
                                     strong("Customer Type Tab"),
                                     p("In this tab, pie charts are used to show the proportion of customer types according to whether they are transient, contract, group and to explore the variation in customers who opt for No Deposit, Non-Refund and Refundable bookings"),
                                     p("After selection of desired variables to explore, pie chart will be updated. Values can be better accessed by hovering over the pie sections.")
                            )
                          )
                        )
                      )
             ),
             tabPanel("Parallel Sets",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h2("Select Desired Variables"),
                          pickerInput( inputId = "picker",
                                       label = "Please select n and 2+ other variables", 
                                       choices = names(par_data),
                                       selected = names(par_data)[4:5],
                                       options = list(`actions-box` = TRUE), 
                                       multiple = TRUE
                          ),
                          awesomeRadio(
                            inputId = "color",
                            label = "Select the category for color", 
                            choices = names(par_data)[1:4],
                            selected = names(par_data)[1],
                            checkbox = TRUE
                          ),
                          submitButton("Apply Changes")
                          
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Guests", 
                                     plotOutput("parallelPlot"))
                            
                          )
                        )
                      )
             ),
             tabPanel("Inferential Analysis",
                      titlePanel("Inferential Analysis (Impact on LeadTime)"), 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "x",
                                      label = "Select Variable:",
                                      choices = c("Customer Type" = "CustomerType",
                                                  "Hotel Type" = "HotelType",
                                                  "Deposit Status" = "DepositType")),
                          # insert select for test type 
                          radioButtons("y", "Select Test Statistic Type:",
                                       c("Non-Parametric" = "np",
                                         "Baeyes Factor" = "bf")), 
                          submitButton("Apply Changes")
                        ),
                        mainPanel(plotOutput("LeadTime_Anova"))
                      )
             )
  ),
  
  ######### Page 2
  navbarMenu(
    "Know Your Business",
    icon = icon("business-time"),
    tabPanel(
      "Average Booking Price",
      sidebarLayout(
        sidebarPanel(
          h3("ADR Over Time"),
          helpText("The ... plot is used to reveal the timely trend of Average Daily Rate. "),
          br(),
          
          selectInput("hotel_type", "Select Hotel:",
                      choices = c("All", unique(hotel_data$HotelType))),
          # Meal, customer, distribution channel filter
          selectInput("meal", "Meal", choices = c("All", unique(hotel_data$Meal))),
          selectInput("customer_type", "Customer Type", choices = c("All", unique(hotel_data$CustomerType))),
          selectInput("distribution_channel", "Distribution Channel", choices = c("All", unique(hotel_data$DistributionChannel))),
          
          h3("ANOVA Test"),
          helpText("The ANOVA test is used to discover the relationship between the selected variable and the ADR (price). "),
          br(),
          
          # for anova
          selectInput(inputId = "x",
                      label = "Predictor variable:",
                      choices = c("Customer Type" = "CustomerType",
                                  "Hotel Type" = "HotelType",
                                  "Deposit Status" = "DepositType")),
          # insert select for test type 
          radioGroupButtons(
            "y", "Anova Test Type:",
            c("Non-Parametric" = "np",
              "Bayes Factor" = "bf"),
            status = "primary"
          ), 
          
          submitButton("Apply Changes")
        ), #sidebarpanel
        
        mainPanel(
          # Plot1
          h3('Visualizations to Understand Your Business'),
          fluidRow(
            plotOutput("hotel_avr"),
          ),
          fluidRow(
            plotOutput("AnovaPlot")
          )
        ) #mainpanel
      ) #sidebarlayout
    ), #tabpanel
    
    # ------ Tab 2. Filters affecting cancellation rate plot
    
    tabPanel(
      "Cancellation Rate",
      sidebarLayout(
        sidebarPanel(
          
          selectInput("HotelType", "Select Hotel:",
                      choices = c("All", unique(hotel_data$HotelType))),
          
          # Lead time range filter
          sliderInput("LeadTime", "Lead Time Range (Days)", 
                      min = 0, max = max(hotel_data$LeadTime), value = c(0, max(hotel_data$LeadTime))),
          
          # Number of booking changes range filter
          sliderInput("BookingChanges", "Number of Booking Changes", 
                      min = 0, max = max(hotel_data$BookingChanges), value = c(0, max(hotel_data$BookingChanges))),
          
          # Checkbox filter for deposit type
          checkboxGroupInput("DepositType", "Deposit Type", 
                             choices = unique(hotel_data$DepositType), selected = unique(hotel_data$DepositType)),
          
          # Only one option can be selected in the Repeated Guest checkbox
          radioGroupButtons(
            "IsRepeatedGuest", "Repeated Guest",
            choices = c("All", "Yes", "No"), selected = "All",
            status = "primary"
          ), 
          
          # YYYY-MM filter slicer
          dateRangeInput("YearMonth", "YYYY-MM", 
                         start = as.Date("2015-01-01"), 
                         end = as.Date("2017-09-30"), 
                         format = "yyyy-mm", separator = " - "),
          
          submitButton("Apply Changes")), #sidebarpanel
        
        mainPanel(
          # Plot2
          plotOutput("hotel_cancellation")
          
        ) # mainpanel
      ), # sidebarlayout
    ), # tabpanel
  ), #Page 2
  
  ######### Page 3
  navbarMenu(
    "Predictive Analysis",
    icon = icon("chart-line"),
    tabPanel(
      "Decision Tree: Cancellation Prediction",
      sidebarLayout(
        sidebarPanel(
          h3("The Controls"),
          br(),
          
          actionBttn(
            inputId = "createTreeModel",
            label = "Create Tree",
            class = "primary"  # makes it blue!
          ),
          actionBttn(
            inputId = "testTreeModel",
            label = "Test Tree",
            color = "danger"  # makes it red!
          ),
          
          br(),
          
          h3("Model Features"),
          helpText(
            'Here are the choices of the predictors to choose:'
          ),
          pickerInput(
            inputId = "model_vars",
            label = NULL,  # label given in outer code
            choices = var_list,
            selected = var_list_default,
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          ),
          
          br(),
          "Here are some hyperparameters for tuning the DT: ",
          
          h4("Minimum Split"),
          helpText(
            "Control the minimum size allowed for the node to split further"
          ),
          sliderInput(
            inputId = "min_split",
            label = NULL,  # label given in outer code
            min = 10,       # two is the smallest that could be split
            max = 200,      # chosen to not make the models too wild
            value = 50,      # defaults to not having an artifical minimum,
            step=10
          ),
          
          br(),
          
          h4("Minimum Bucket Size"),
          helpText(
            "Control the minimum size allowed in the terminal node"
          ),
          sliderInput(
            inputId = "min_bucket",
            label = NULL,  # label given in outer code
            min = 10,       # can't have buckets of size zero
            max = 100,      # rpart default is minbucket = 3*minsplit
            value = 20,     # defaults to not having an artifical minimum
            step = 10
          ),
          br(),
          h4("Maximum Tree Depth"),
          helpText(
            "Control the maximum depth that the decision tree can reach"
          ),
          sliderInput(
            inputId = "max_depth",
            label = NULL,  # label given in outer code
            min = 2,       # a min of 2 allows for at least one split
            max = 15,      # rpart can't do 31+ depth on 32-bit machines
            value = 5      # chosen to not make the default too wild
          )
        ), #SidebarPanel
        mainPanel(
          fluidRow(
            label = NULL,
            column(6,
                   h3("Training Results"),
                   helpText("training size is limited due to server memory capacity"),
                   br(),
                   # training accuracy, precision, recall
                   tagAppendAttributes(
                     textOutput("tree_training_scores"),
                     # allow linebreaks between scores, larger font here
                     style = "white-space: pre-wrap; font-size: 17px;"
                   ), 
                   # training results table matches layout from presentation
                   tableOutput("tree_training_table")
            ), # column 1
            column(6,
                   h3("Test Results"),
                   br(),
                   # test accuracy, precision, recall
                   tagAppendAttributes(
                     textOutput("tree_test_scores"),
                     # allow linebreaks between scores, larger font here
                     style = "white-space: pre-wrap; font-size: 17px;"
                   ),
                   # training results table matches layout from presentation
                   tableOutput("tree_test_table")
            ) # column 2
          ), # fluidRow
          fluidRow(
            column(8,
                   h3("Decision Tree"),
                   helpText(
                     "Class 0: Predicted not Canceled; Class 1: Predicted Canceled"
                   ),
                   plotOutput(outputId = "tree_plot")
            ), #column 1
            column(4,
                   h3("Classification"),
                   "- The tree graph on the left dicpicts the suggested method by a trained decision tree model,
                                   which classfies whether a customer is likely to cancelled the order. "
                   ,
                   br(),br(),
                   "- The accuracy of the model can be referred by the training and testing accuracy on the top.",
                   br(),br(),
                   "- Operation team can utilise this model as a guide and pay extra attentions to the customer who are likely to churn."
            ) # column 2
          ) #FluidRow
        )#mainPanel
      )#SidebarLayout
    ), #tabPanel 1
    
   tabPanel(
     "Logistic Regression: Cancellation Prediction",
     sidebarLayout(
       sidebarPanel(
         h3("The Controls"),
         br(),
         
         actionBttn(
           inputId = "createLRModel",
           label = "Create Model",
           class = "primary"  # makes it blue!
         ),
         actionBttn(
           inputId = "testLRModel",
           label = "Test Model",
           color = "danger"  # makes it red!
         ),
         
         br(),
         
         h3("Model Features"),
         helpText(
           'Here are the choices of the predictors to choose:'
         ),
         pickerInput(
           inputId = "lr_model_vars",
           label = NULL,  # label given in outer code
           choices = lr_var_list,
           selected = lr_var_list_default,
           options = list(`actions-box` = TRUE),
           multiple = TRUE
         ), 
         
         br(),
         
         h3("Test For Multi-Collinearity"),
         helpText("Below shows the correlations between each of the numerical variables.
                  Highly correlated pairs need to be excluded from the logistic regression model."),
         plotlyOutput(outputId = "num_corr_plot"),
       ),#sidebarPanel
       mainPanel(
         fluidRow(
           label = NULL,
           column(6,
                  h3("Training Results"),
                  helpText("training size is limited due to server memory capacity"),
                  br(),
                  # training accuracy, precision, recall
                  tagAppendAttributes(
                    textOutput("lr_training_scores"),
                    # allow linebreaks between scores, larger font here
                    style = "white-space: pre-wrap; font-size: 17px;"
                  ), 
                  # training results table matches layout from presentation
                  tableOutput("lr_training_table")
           ), # column 1
           column(6,
                  h3("Test Results"),
                  br(),
                  # test accuracy, precision, recall
                  tagAppendAttributes(
                    textOutput("lr_test_scores"),
                    # allow linebreaks between scores, larger font here
                    style = "white-space: pre-wrap; font-size: 17px;"
                  ),
                  # training results table matches layout from presentation
                  tableOutput("lr_test_table")
           ) # column 2
         ), # fluidRow
         fluidRow(
           column(9,
                  h3("Importance of Variables"),
                  plotlyOutput("lr_var_importance_bar")
                  )
         )
       )#MainPanel
     )#SidebarLayout
   )#TabPanel
  )#navbarMenu
)#NavbarPage



# ------- server --------
server <- function(input, output) {
  
  # Create a reactive data frame for EDA 
  filtered_data_1 <- reactive({
    hotel_data %>%
      filter(if (input$HotelType == "All") TRUE else HotelType == input$HotelType) %>%
      filter(if (input$ArrivalDateMonth == "All") TRUE else ArrivalDateMonth == input$ArrivalDateMonth) %>%
      filter(if (input$Continent == "All") TRUE else Continent == input$Continent) %>%
      filter(if (input$IsCanceled == "All") TRUE else IsCanceled == (input$IsCanceled == "Canceled"))
  })
  
  # Create a summary table for EDA
  output$summary_table1 <- renderDT({
    filtered_data_1() %>%
      group_by(HotelType, IsCanceled) %>%
      summarize(total_bookings = n(),
                mean_adr = mean(ADR),
                mean_lead_time = mean(LeadTime)) %>%
      datatable()
  })
  
  # Create GTsummary table for EDA
  output$summary_table <- render_gt({
    hotel_data %>%
      tbl_summary(
        by = HotelType, 
        type = c(ADR, LeadTime) ~ "continuous",
        include = c(ADR, LeadTime, IsCanceled),
        statistic = all_continuous() ~ "{mean}",
        missing = "no"
      ) %>%
      add_stat_label() %>%
      add_n() %>%
      as_gt() %>%
      tab_header(md("**Overall Summary Table of Hotel Dataset**"))
  })
  
  #Create an Interactive filter for bubble plot 
  
  bubblefiltered <- reactive({
    filtered_data_1()%>% 
      select(Adults, Children, Babies, Continent) %>%
      group_by(Continent) %>%
      summarize(Adults = sum(Adults), Children = sum(Children), Babies= sum(Babies)) %>%
      ungroup() %>% 
      as_tibble()
    
  })
  
  # Create an Interactive bubble plot for EDA
  output$bubble_plot <- renderPlotly({
    plot_ly(bubblefiltered(), x = ~Adults, y = ~Children, size = ~Adults + Children + Babies, color = ~Continent) %>% 
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5,
                           y = 1.8))         # put legend in center of xaxis 
  }) 
  
  # Create Specialrequests by repeat or non repeat 
  output$special_barplot <- renderGirafe({
    
    q <- filtered_data_1() %>% 
      group_by(IsRepeatedGuest) %>%
      summarize(sum = sum(TotalOfSpecialRequests)) %>% 
      ggplot(aes(x = IsRepeatedGuest, y = sum , fill = IsRepeatedGuest)) +
      labs(x = "New Vs Repeated Guests", title = "Total Special Requests") + 
      scale_x_continuous(breaks=seq(from=0,to=1, by = 1), labels=c("First Time", "Repeat")) + 
      geom_bar_interactive(stat = "identity", aes(tooltip = sum , data_id = sum ), show.legend = FALSE) 
    # geom_point_interactive(aes(tooltip = sum , data_id = sum )) 
    
    girafe(ggobj = q, width_svg = 4)
    
  })
  
  
  # Create Carpark space requests by repeat or non repeat 
  output$carpark_barplot <- renderGirafe({
    
    v <- filtered_data_1() %>% 
      group_by(IsRepeatedGuest) %>%
      summarize(sum = sum(RequiredCarParkingSpaces)) %>% 
      ggplot(aes(x = IsRepeatedGuest, y = sum , fill = IsRepeatedGuest)) +
      # theme(legend.positon = "none") + 
      labs( x = "New Vs Repeated Guests", title = "Total Parking Spaces Required") + 
      scale_x_continuous(breaks=seq(from=0,to=1, by = 1), labels=c("First Time", "Repeat")) + 
      geom_bar_interactive(stat = "identity", aes(tooltip = sum , data_id = sum ), show.legend = FALSE) 
    #geom_point_interactive(aes(tooltip = sum , data_id = sum )) 
    
    
    girafe(ggobj = v, width_svg = 4)
    
  })
  
  # Create Meals Barplot for EDA
  output$meals_barplot <- renderGirafe({
    
    z <- filtered_data_1() %>% 
      subset(Meal != "Undefined") %>% 
      group_by(Meal) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = Meal, y = count, fill = Meal)) +
      labs(title = "Number of Meals", x = "Meal Type") + 
      geom_bar_interactive(stat = "identity", aes(tooltip = count , data_id = count ), show.legend = FALSE) 
    #geom_point_interactive(aes(tooltip = count , data_id = count ))  
    
    girafe(ggobj = z, width_svg = 4)
    
  })
  
  # Create pie chart of customer type for EDA 
  output$pie <- renderBillboarder({
    piedata <-filtered_data_1() %>%
      group_by(CustomerType) %>%
      summarize(n = n()) 
    
    billboarder() %>% 
      bb_piechart(data = piedata)
    
  })
  
  # Create pie chart of Deposit Type for EDA 
  output$pie1 <- renderBillboarder({
    piedata1 <-filtered_data_1() %>%
      group_by(DepositType) %>%
      summarize(n = n()) 
    
    billboarder() %>% 
      bb_piechart(data = piedata1)
    
  })
  # ---------------------------------------------------------------------------
  
  # Create reactive dataset filter for parallel set input selection 
  
  # Filter data based on picker input
  filtered_data_2 <- reactive({
    par_data %>% 
      select(all_of(input$picker))
  })
  
  # Create alluvial plot
  output$parallelPlot <- renderPlot({
    para_plot(input$picker, input$color)
  })
  
  # ------------------------------------------------------------------------------
  # Create Anova Plots
  output$LeadTime_Anova <- renderPlot({
    ggbetweenstats(hotel_data, !!input$x, y = LeadTime, 
                   xlab = input$x, 
                   type = input$y, 
                   pairwise.display = "significant",
                   ylab = "Lead Time", 
                   title = "Differences in means of Lead Time",
                   ggplot.component = list(theme(plot.subtitle = element_text(size = 12, face = "bold"),
                                                 plot.title = element_text(size = 15, face = "bold"))))
  })
  
  
  filter_data_3 <- reactive({
    hotel_data %>% 
      filter(if (input$hotel_type == "All") TRUE else HotelType == input$hotel_type) %>%
      filter(if (input$meal == "All") TRUE else Meal == input$meal) %>%
      filter(if (input$customer_type == "All") TRUE else CustomerType == input$customer_type) %>%
      filter(if (input$distribution_channel == "All") TRUE else DistributionChannel == input$distribution_channel) %>%
      mutate(ArrivalDateMonthWeek = paste(ArrivalDateMonth, "W", sprintf("%02d", ArrivalDateWeekNumber), sep = "-")) %>%
      mutate(ArrivalDateMonthWeek = paste(ArrivalDateMonth, "W", sprintf("%02d", ArrivalDateWeekNumber), sep = "-")) %>%
      group_by(ArrivalDateYear, ArrivalDateMonth, ArrivalDateMonthWeek) %>%
      summarize(avg_ADR = mean(ADR), sd_ADR = sd(ADR), n = n())
  })
  
  # Filter the data based on user input
  
  filter_data_4 <- reactive({
    hotel_data %>%
      filter(LeadTime >= input$LeadTime[1], LeadTime <= input$LeadTime[2],
             BookingChanges >= input$BookingChanges[1], BookingChanges <= input$BookingChanges[2],
             if (input$IsRepeatedGuest == "All") TRUE else IsRepeatedGuest == (input$IsRepeatedGuest == "Yes"),
             DepositType %in% input$DepositType,
             if (!is.null(input$YearMonth)) ReservationStatusDate >= input$YearMonth[1] & ReservationStatusDate <= input$YearMonth[2])
  })
  
  # cancellation rate based on filtered data
  
  cancellation_rate <- reactive({
    filter_data_4() %>%
      mutate(ArrivalDateMonthWeek = paste(ArrivalDateMonth, "W", sprintf("%02d", ArrivalDateWeekNumber), sep = "-"),
             ReservationStatusYearMonth = format(as.Date(ReservationStatusDate), "%Y-%m")) %>%
      group_by(ReservationStatusYearMonth) %>%
      summarize(CancellationRate = mean(IsCanceled))
  }) 
  
  
  # Tab 1: Average Rate
  # Plot a line chart with upper and lower bound based on YYYY-MM ADR
  
  output$hotel_avr <- renderPlot({
    filter_data_3() %>%
      ggplot(aes(x = factor(ArrivalDateMonth,levels = month.name), y = avg_ADR, color = factor(ArrivalDateYear))) +
      #geom_line() +
      geom_ribbon(aes(ymin = avg_ADR - 1.96 * sd_ADR / sqrt(n), ymax = avg_ADR + 1.96 * sd_ADR / sqrt(n)), alpha = 0.2) +
      scale_color_discrete(name = "Year") +
      labs(x = "Month", y = "Average Daily Rate") +
      ggtitle("Average Daily Rate by Month and Year")
  }) 
  
  
  # Tab 2: Cancellation rate
  # Plot cancellation rate trend
  
  output$hotel_cancellation <- renderPlot({
    cancellation_rate() %>%
      ggplot(aes(x = ReservationStatusYearMonth, y = CancellationRate)) +
      geom_bar(stat = "identity") +
      ggtitle("Hotel Cancellation Rate") +
      xlab("Year and Month") +
      ylab("Cancellation Rate") +
      scale_fill_discrete(name = "Year and Month") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  # Tab 3: Inferential Analysis
  # Create Anova Plots for ADR means by input
  
  output$AnovaPlot <- renderPlot({
    hotel_data%>%
      ggbetweenstats(!!input$x, y = ADR, xlab = input$x, 
                     type = input$y, 
                     ylab = "ADR", 
                     title = "Differences in means between Average Daily Rate",
                     ggplot.component = list(theme(plot.subtitle = element_text(size = 12, face = "bold"),
                                                   plot.title = element_text(size = 13, face = "bold")))) +
      scale_y_continuous(
        limits = c(0, 1000),
        breaks = seq(from = 0, to = 1000, by = 50)
      )
  }) # AnovaPlot

#---------- TAB 3 ---------------
  
  #------ INPUT EVENT REACTIONS -----
  #------ Decision Tree --------
  # reconstruct the tree every time createModel is pressed
  tree = eventReactive(
    eventExpr = input$createTreeModel,
    valueExpr = makeTree(
      model_vars = input$model_vars,
      input$min_split, input$min_bucket, input$max_depth
    )
  )
  
  # regenerate training results every time createModel is pressed
  tree_training_results = eventReactive(
    eventExpr = input$createTreeModel,
    valueExpr = useTree(tree(), train_df)
  )
  
  # regenerate test results every time createModel is pressed
  tree_test_results = eventReactive(
    eventExpr = input$testTreeModel,
    valueExpr = useTree(tree(), test_df)
  )
  
  #------ Logistic Regresion --------
  lr = eventReactive(
    eventExpr = input$createLRModel,
    valueExpr = makeLR(input$lr_model_vars)
  )
  
  # regenerate training results every time createModel is pressed
  lr_training_results = eventReactive(
    eventExpr = input$createLRModel,
    valueExpr = useLR(lr(), train_df)
  )
  
  # regenerate test results every time createModel is pressed
  lr_test_results = eventReactive(
    eventExpr = input$testLRModel,
    valueExpr = useLR(lr(), test_df)
  )
  
  lr_var_importance = eventReactive(
    eventExpr = input$createLRModel,
    valueExpr = varImp(lr(), scale = FALSE)
  )
  
  #------ OUTPUT DISPLAY PREP ------
  # assessment scores are each collapsed to display on a new line
  output$tree_training_scores = renderText(
    paste(calcScores(tree_training_results()), collapse = "\n")
  )
  output$tree_test_scores = renderText(
    paste(calcScores(tree_test_results()), collapse = "\n")
  )
  
  # tables of outcome breakdows are static widgets
  output$tree_training_table = renderTable(
    resultsTable(tree_training_results()),
    align = "lccc",  # left-align first column, centre rest
    striped = TRUE
  )
  output$tree_test_table = renderTable(
    resultsTable(tree_test_results()),
    align = "lccc",  # left-align first column, centre rest
    striped = TRUE
  )
  # frame for a plot of the decision tree
  output$tree_plot = renderPlot(
    prp(
      tree(), roundint = FALSE,
      # neaten up the nodes and edges, remove detailed labels
      extra = 0, branch = 0, varlen = 0,
      # colours spam terminals in red, non-spam terminals in blue
      box.col = c("cornflowerblue", "tomato")[tree()$frame$yval]
    )
  )
  #correlation heatmap
  output$num_corr_plot = renderPlotly(
    corrplot_num(hotel_data)
  )
  
  output$lr_training_scores = renderText(
    paste(calcScores(lr_training_results()), collapse = "\n")
  )
  output$lr_test_scores = renderText(
    paste(calcScores(lr_test_results()), collapse = "\n")
  )
  
  # tables of outcome breakdows are static widgets
  output$lr_training_table = renderTable(
    resultsTable(lr_training_results()),
    align = "lccc",  # left-align first column, centre rest
    striped = TRUE
  )
  output$lr_test_table = renderTable(
    resultsTable(lr_test_results()),
    align = "lccc",  # left-align first column, centre rest
    striped = TRUE
  )
  
  output$lr_var_importance_bar = renderPlotly(
    var_imp_plot(lr_var_importance())
  )
  
}



# ------- Run the application -------
shinyApp(ui = ui, server = server)
