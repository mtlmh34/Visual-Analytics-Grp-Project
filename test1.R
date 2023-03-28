pacman::p_load(shiny, shinythemes, tidyverse, rpart,rpart.plot, shinyWidgets)

set.seed(123)

# loading data
hotel_data <- read.csv('hotel_viz/data/hotel.csv')

# split train and test
data_size=nrow(hotel_data)
index <- sample(1:data_size, size = trunc(.8 * data_size))
train_df <- hotel_data %>%
  filter(row_number() %in% index)

test_df <- hotel_data %>%
  filter(!(row_number() %in% index))

var_list <- list(
  "No. of Non-Canceled Previous Bookings"="PreviousBookingsNotCanceled", 
  "No. of Previous Cancallations"="PreviousCancellations",  
  "No. of Special Requests"="TotalOfSpecialRequests",
  "No. of Changes Made"="BookingChanges",  
  "Reserved Room Type"="ReservedRoomType", 
  "Assigned Room Type"="AssignedRoomType",
  "Avg. Daily Rate"="ADR",
  "No. of Adults"="Adults",
  "No. of Babies"="Babies",
  "No. of Children"="Children",
  "Customer Type"="CustomerType",                
  "No. of Days in Waiting List"="DaysInWaitingList",          
  "Deposit Type"="DepositType",                 
  "Distribution Channel"="DistributionChannel",         
  "Hotel Type"="hotelType",                   
  "Is Repeated Guest?"="IsRepeatedGuest",             
  "Lead Time"="LeadTime",                   
  "Market Segment"="MarketSegment",               
  "Meal"="Meal",                        
  "Required CarParking Spaces?"="RequiredCarParkingSpaces", 
  "No of Weekend Nights"="StaysInWeekendNights",        
  "No. of Week Nights"="StaysInWeekNights"
)

hotel_data[hotel_data==""]<-NA
hotel_data[hotel_data=="NULL"]<-NA
colSums(is.na(hotel_data))

hotel_data$hotelType=as.factor(hotel_data$hotelType)  
hotel_data$ArrivalDateMonth=as.factor(hotel_data$ArrivalDateMonth)  
hotel_data$Meal=as.factor(hotel_data$Meal)  
hotel_data$MarketSegment=as.factor(hotel_data$MarketSegment)  
hotel_data$DistributionChannel=as.factor(hotel_data$DistributionChannel)  
hotel_data$ReservedRoomType=as.factor(hotel_data$ReservedRoomType)  
hotel_data$AssignedRoomType=as.factor(hotel_data$AssignedRoomType)  
hotel_data$DepositType=as.factor(hotel_data$DepositType)   
hotel_data$CustomerType=as.factor(hotel_data$CustomerType)

create_lr_model <- function(data, var_list){
  
  data$hotelType=as.factor(data$hotelType)  
  data$arrival_date_month=as.factor(data$arrival_date_month)  
  data$meal=as.factor(data$meal)  
  data$market_segment=as.factor(data$market_segment)  
  data$distribution_channel=as.factor(data$distribution_channel)  
  data$reserved_room_type=as.factor(data$reserved_room_type)  
  data$assigned_room_type=as.factor(data$assigned_room_type)  
  data$deposit_type=as.factor(data$deposit_type)   
  data$customer_type=as.factor(data$customer_type)  
  data$reservation_status=as.factor(data$reservation_status)
  
  f = paste("IsCanceled ~ ", paste(var_list, collapse = " + "))
  
  lr_model=glm(formula,data=data,family=binomial(link="logit"))
  return(lr_model)
  
}

str(hotel_data)[2]


# Load required libraries
library(Hmisc)
library(caret)

pacman::p_load(Hmisc,caret)

# Select numerical and categorical variables separately
num_vars <- c("IsCanceled", "LeadTime", "StaysInWeekendNights", "StaysInWeekNights", "Adults", "Children", "Babies", "PreviousCancellations", "PreviousBookingsNotCanceled", "BookingChanges", "DaysInWaitingList", "ADR", "RequiredCarParkingSpaces", "TotalOfSpecialRequests")
cat_vars <- c("IsCanceled", "ArrivalDateMonth", "Meal", "MarketSegment", "DistributionChannel", "IsRepeatedGuest", "ReservedRoomType", "AssignedRoomType", "DepositType", "CustomerType", "hotelType")

# Create dummy variables for categorical variables
hotel_data_dummy <- predict(dummyVars(~ ., data = hotel_data[cat_vars]), newdata = hotel_data[cat_vars])

# Compute correlation matrix for numerical variables
corr_num <- rcorr(as.matrix(hotel_data[num_vars]), type = "pearson")

# Plot correlation heatmap for numerical variables
corrplot(corr_num$r, method = "color", type = "lower", tl.cex = 0.8, tl.col = "black")

# Compute correlation matrix for categorical variables
corr_cat <- rcorr(as.matrix(hotel_data_dummy[, !names(hotel_data_dummy) %in% num_vars]), type = "spearman")

# Plot correlation heatmap for categorical variables
corrplot(corr_cat$r, method = "color", type = "lower", tl.cex = 0.8, tl.col = "black")


# Load necessary packages
library(corrplot)
library(plotly)

# Select numerical variables
num_vars <- c("IsCanceled", "LeadTime", "StaysInWeekendNights", "StaysInWeekNights", "Adults", "Children", "Babies", 
              "PreviousCancellations", "PreviousBookingsNotCanceled", "BookingChanges", "DaysInWaitingList", 
              "ADR", "RequiredCarParkingSpaces", "TotalOfSpecialRequests")
hotel_data <- na.omit(hotel_data)
# Compute correlation matrix
corr_mat <- cor(hotel_data[, num_vars])

# Create correlation heatmap with plotly
plot_ly(z = corr_mat, type = "heatmap", colorscale = "RdBu", reversescale = TRUE,
        x = colnames(corr_mat), y = colnames(corr_mat)) %>%
  layout(title = "Correlation Heatmap of Numerical Variables")


numeric_var <- names(hotel_data)[sapply(hotel_data[names(hotel_data)], is.numeric)]
