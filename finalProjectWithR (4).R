# Lama Alzahrani, Ruba Alkhattabi, Reema Alotaibi, Ahmed Altowairqi

# Data source: https://www.kaggle.com/jessemostipak/hotel-booking-demand

# The packages:
library(shiny)
library(tidyverse)
library(shinydashboard)

# Read data
my_data <- read_csv("/Users/ahmadbasha/Desktop/Internship/hotel_bookings.csv")
View(my_data)

# Convert variables to factor.
my_data$is_canceled <- as.factor(my_data$is_canceled) # canceled (1) or not (0)
my_data$arrival_date_year<- as.factor(my_data$arrival_date_year)
my_data$arrival_date_month <- as.factor(my_data$arrival_date_month)
my_data$country <- as.factor(my_data$country)

# arrange the month name 
my_data$arrival_date_month <- factor(my_data$arrival_date_month,levels=month.name)

# Rename the level of canceled variable
levels(my_data$is_canceled) <- c("not canceled","canceled")

# removing NA values
dim(my_data)
my_data <- na.omit(my_data)
dim(my_data)

# Select the country with freqency heigher than 50 times. 
country_table<-table(my_data$country, exclude="NULL")>50 # NULL is missing value, so we drop it.
names(country_table[country_table==TRUE]) # to check 



# Here start using shiny app 
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Hotel Bookings"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      
      box(width=12,
          # First plot 
          title = "The Frequency of The Hotels by Year and  Month", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          selectInput(inputId = "year", 
                      label = "Choose the year:",
                      selected = 2016,
                      choices = c("2015" = 2015,
                                  "2016" = 2016,
                                  "2017" = 2017 )),
          
          selectInput(inputId = "month", label = "Choose the month:",
                      selected = "All months",
                      choices = c("January" = "January",
                                  "February" = "February",
                                  "March" = "March",
                                  "April" = "April",
                                  "May" = "May",
                                  "June" = "June",
                                  "July" = "July",
                                  "August" = "August",
                                  "September" = "September",
                                  "October" = "October",
                                  "November" = "November",
                                  "December" = "December",
                                  "All months" = "All months" )),
          helpText("Note: there some months on a specific year don't have any booking hotels."),
          plotOutput("barTwo"),
      )),
    fluidRow(
      box(width=12,
          # Second plot 
          title = "The State of The Booking in hotels", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          radioButtons(inputId="num",
                       label = "Choose The State of The Booking in Hotels:",
                       choices=c("Canceled" = "canceled", 
                                 "Not Canceled"= "not canceled")),
          
          radioButtons(inputId="cat1",
                       label="Select The Hotel Type:",
                       choices=c("Resort Hotel" = "Resort Hotel", 
                                 "City Hotel"= "City Hotel")),
          
          selectInput(inputId="cat2",
                      label="Select The Country Code:",
                      choices= names(country_table[country_table==TRUE]),
                      selected = "USA",
          ),
          helpText("Note: there some countries don't have all market segment.",
                   "And will appear an empty plot cause there is no data for your choice."), 
          
          plotOutput("bar"),
          
      )),
    fluidRow(
      box(width=12,
          # Third plot
          title = "The Category of Visitors by Month", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          selectInput(inputId = "numTwo", label = "Select The Category of Visitors:",
                      choices = c("Adults" = "adults",
                                  "Children" = "children",
                                  "Babies" = "babies" )),
          plotOutput("barThree" )
          
      ))
  )
  
)

server <- function(input, output) {
  #first plot
  output$barTwo <- renderPlot({
    title <- "The Frequency of The Hotels by Year and  Month"
    # If user choose all months then return the plot below
    if (input$month == "All months")
    {
      my_data %>%
        select(arrival_date_year , arrival_date_month , hotel)  %>%
        filter(arrival_date_year == input$year)  %>%
        ggplot(aes(arrival_date_month )) +
        geom_bar(aes(fill = hotel), position = "dodge") + 
        labs(title=title, x='The Month', y='The Count', fill="Hotel") + # Names
        scale_fill_manual(values=c('lightsteelblue2','lightsteelblue3')) # Colors
    } else {
      my_data %>%
        select(arrival_date_year , arrival_date_month , hotel)  %>%
        filter(arrival_date_year == input$year  & arrival_date_month == input$month )  %>%
        ggplot(aes(arrival_date_month )) +
        geom_bar(aes(fill = hotel), position = "dodge") + 
        labs(title=title, x='The Month', y='The Count', fill="Hotel") + # Names
        scale_fill_manual(values=c('lightsteelblue2','lightsteelblue3')) # Colors
      
    }
  })
  
  #second plot
  title <- "The State of The Booking in hotels"
  output$bar <- renderPlot({
    my_data%>%
      filter(is_canceled == input$num & my_data$hotel == input$cat1 & my_data$country == input$cat2) %>%
      ggplot(aes(is_canceled)) +
      geom_bar(aes(fill = market_segment), position = "dodge") +
      labs(title=title, x='The State of The Booking', y='The Count', fill="Market Segment") +
      scale_fill_manual(values=c('lightsteelblue','lightsteelblue1','lightskyblue2','lightsteelblue2','lightsteelblue3','lightskyblue3','lightsteelblue4')) # Colors
  })
  
  
  #third Plot
  output$barThree <- renderPlot({ 
    title <- "The Category of Visitors by Month"
    if (input$numTwo == "adults") {
      my_data %>%
        select(adults , arrival_date_month , hotel)  %>%
        ggplot(aes(arrival_date_month ,adults )) +
        geom_bar(aes(fill = hotel), position = "dodge", stat = 'identity') + 
        labs(title=title, x='The Month', y='The Count', fill="Hotel") + # Names
        scale_fill_manual(values=c('lightsteelblue2','lightsteelblue3'))  # Colors
    } else if (input$numTwo == "children") {
      my_data %>%
        select(children , arrival_date_month , hotel)  %>%
        ggplot(aes(arrival_date_month , children)) +
        geom_bar(aes(fill = hotel), position = "dodge", stat = 'identity') + 
        ylim(0,11) +
        labs(title=title, x='The Month', y='The Count', fill="Hotel") + # Names
        scale_fill_manual(values=c('lightsteelblue2','lightsteelblue3')) # Colors
    } else {
      my_data %>%
        select(babies , arrival_date_month , hotel)  %>%
        ggplot(aes(arrival_date_month , babies)) +
        geom_bar(aes(fill = hotel), position = "dodge", stat = 'identity') + 
        ylim(0,11) +
        labs(title=title, x='The Month', y='The Count', fill="Hotel") + # Names
        scale_fill_manual(values=c('lightsteelblue2','lightsteelblue3'))  # Colors
      
    } 
  } )  
  
}

shinyApp(ui, server)



  


