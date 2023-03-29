


pacman:: p_load(parallelPlot, ggplot2, plotly, tidyverse, shiny, gt, gtsummary, ggalluvial, gmodels, readxl, DT, ggstatsplot, shinyWidgets)
set.seed(123)

# loading data
hotel_data <- read.csv('data/hotel.csv')
hotel_data <- hotel_data %>% 
  mutate(Meal = str_trim(Meal))


ui <- navbarPage(
  title = "Hotel Data Analytical Dashboard",
  fluid = TRUE,
  theme='simplex',
  id = "navbarID",
  
  navbarMenu(
    "Know Your Business",
    icon = icon('address-card'),
    tabPanel(
      "Average Booking Price",
      sidebarLayout(
        sidebarPanel(
          h3("ADR Over Time"),
          helpText("The ... plot is used to reveal the timely trend of Average Daily Rate. "),
          br(),
          
          selectInput("hotel_type", "Select Hotel:",
                      choices = c("All", unique(hotel_data$hotelType))),
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
                                  "Hotel Type" = "hotelType",
                                  "Deposit Status" = "DepositType")),
          # insert select for test type 
          radioButtons("y", "Anova Test Type:",
                       c("Non-Parametric" = "np",
                         "Baeyes Factor" = "bf")), 
          submitButton("Apply Changes")
        ), #sidebarpanel
        
        mainPanel(
          # Plot1
          h3('Main title'),
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
                      choices = c("All", unique(hotel_data$hotelType))),
          
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
          radioButtons("IsRepeatedGuest", "Repeated Guest",
                       choices = c("All", "Yes", "No"), selected = "All"),
          
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
  )
)

server <- function(input, output) {
  
  filter_data_1 <- reactive({
    hotel_data %>% 
            filter(if (input$hotel_type == "All") TRUE else hotelType == input$hotel_type) %>%
            filter(if (input$meal == "All") TRUE else Meal == input$meal) %>%
            filter(if (input$customer_type == "All") TRUE else CustomerType == input$customer_type) %>%
            filter(if (input$distribution_channel == "All") TRUE else DistributionChannel == input$distribution_channel) %>%
            mutate(ArrivalDateMonthWeek = paste(ArrivalDateMonth, "W", sprintf("%02d", ArrivalDateWeekNumber), sep = "-")) %>%
            mutate(ArrivalDateMonthWeek = paste(ArrivalDateMonth, "W", sprintf("%02d", ArrivalDateWeekNumber), sep = "-")) %>%
            group_by(ArrivalDateYear, ArrivalDateMonth, ArrivalDateMonthWeek) %>%
            summarize(avg_ADR = mean(ADR), sd_ADR = sd(ADR), n = n())
  })
  
  # Filter the data based on user input
  
  filtered_data2 <- reactive({
    hotel_data %>%
      filter(LeadTime >= input$LeadTime[1], LeadTime <= input$LeadTime[2],
             BookingChanges >= input$BookingChanges[1], BookingChanges <= input$BookingChanges[2],
             if (input$IsRepeatedGuest == "All") TRUE else IsRepeatedGuest == (input$IsRepeatedGuest == "Yes"),
             DepositType %in% input$DepositType,
             if (!is.null(input$YearMonth)) ReservationStatusDate >= input$YearMonth[1] & ReservationStatusDate <= input$YearMonth[2])
  })
  
  # cancellation rate based on filtered data
  
  cancellation_rate <- reactive({
    filtered_data2() %>%
      mutate(ArrivalDateMonthWeek = paste(ArrivalDateMonth, "W", sprintf("%02d", ArrivalDateWeekNumber), sep = "-"),
             ReservationStatusYearMonth = format(as.Date(ReservationStatusDate), "%Y-%m")) %>%
      group_by(ReservationStatusYearMonth) %>%
      summarize(CancellationRate = mean(IsCanceled))
  }) 
  
  
  # Tab 1: Average Rate
  # Plot a line chart with upper and lower bound based on YYYY-MM ADR
  
  output$hotel_avr <- renderPlot({
    filter_data_1() %>%
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
  

}

# ------- Run the application -------
shinyApp(ui = ui, server = server)