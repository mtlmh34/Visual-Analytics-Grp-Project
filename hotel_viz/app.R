#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)

city_data <- read.csv('data/City.csv')
resort_data <- read.csv('data/Resort.csv')



# ------- Shiny UI --------

ui <- navbarPage(
  title = "Hotel Data Analytical Dashboard",
  fluid = TRUE,
  theme='simplex',
  id = "navbarID",
  
  tabPanel("User Guide",
           icon = icon('person-chalkboard'),
           h1("Welcome to our App!"),
           mainPanel(
             tags$a(href="https://github.com/mtlmh34/Visual-Analytics-Grp-Project/blob/main/README.md", "Click Here for user guide!")
             )
           ),
  
  ######### Page 1
  navbarMenu("Know Your Business", 
             icon = icon('briefcase'),
             tabPanel("Rates",
                      ),
             tabPanel("Cancellations",
                      ),
             tabPanel("Revenue by Rank",
                      )
  ),
  
  ######### Page 2
  navbarMenu("Know Your Customers",
             icon = icon('address-card'),
             tabPanel("Demographics",
                      ),
             tabPanel("Preference",
             )
  ),

  ######### Page 3
  navbarMenu("Predictive Analysis", 
             icon = icon("chart-line"),
             
             tabPanel("Predict For Cancellation",)
             
             )
  )
                                       

# ------- server --------
server <- function(input, output) {


}

# ------- Run the application -------
shinyApp(ui = ui, server = server)
