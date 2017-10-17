library(shiny)
library(ggplot2)
library(gridExtra)
source('workingScript.R')

fluidPage(
  titlePanel(''),
  sidebarLayout(
    sidebarPanel(
      
      # Row 1
      fluidRow(
        
        # Column 1
        column(width = 6,
               selectInput("selectBedrooms", label = strong("Bedrooms"),
                           choices = list(choices = sort(unique(houseDataClean$bedrooms))),
                           selected = 1),
               selectInput("selectBathrooms", label = strong("Bathrooms"),
                           choices = list(choices = sort(unique(houseDataClean$bathrooms))),
                           selected = 1),
               selectInput("selectFloors", label = strong("Floors"),
                           choices = list(choices = sort(unique(houseDataClean$floors))),
                           selected = 1),
               radioButtons("waterFront", label = strong("Is there a waterfront ?"),
                            choices = list("Yes" = 1, "No" = 0), selected = 0, inline = TRUE)
        ),
        
        # Column 2
        column(width = 6,
               numericInput("livingSqFt", label = strong("Living area (Sq.ft)"), value = min(houseDataClean$sqft_living),
                            max = max(houseDataClean$sqft_living), min = min(houseDataClean$sqft_living)),
               numericInput("basementSqFt", label = strong("Basement area (Sq.ft)"), value = min(houseDataClean$sqft_basement),
                            max = max(houseDataClean$sqft_basement), min = min(houseDataClean$sqft_basement)),
               numericInput("aboveSqFt", label = strong("Area above (Sq.ft)"), value = min(houseDataClean$sqft_above),
                            max = max(houseDataClean$sqft_above), min = min(houseDataClean$sqft_above)),
               numericInput("neighborSqFt", label = strong("Neighboring area (Sq.ft)"), value = min(houseDataClean$sqft_living15),
                            max = max(houseDataClean$sqft_living15), min = min(houseDataClean$sqft_living15))
               
        )
      ),
      
      hr(),
      
      # SecondRow
      fluidRow(
        
        # Column 1
        column(width = 6,
               sliderInput("view", label = strong("How good is view of the House"), min = min(houseDataClean$view),
                           max = max(houseDataClean$view), value = 2),
               radioButtons("isRenovated", label = strong("Is Renovated?"),
                            choices = list("Yes" = 1, "No" = 0), selected = 1, inline = TRUE),
               selectInput("zipcode", label = strong("Zipcode"), choices = sort(unique(houseDataClean$zipcode)))
               
               
        ),
        
        # Column 2
        column(width = 6,
               
               sliderInput("grade", label = strong("Quality of the Construction and Design"), min = 0,
                           max = max(houseDataClean$grade), value = 3),
               
               conditionalPanel(
                 condition = "input.isRenovated == 1",
                 numericInput("date", label = strong("Year Renovated"), value = 2000)),
               
               conditionalPanel(
                 condition = "input.isRenovated == 0",
                 numericInput("date", label = strong("Year Built"), value = 2000))
               
               
        )
        
      )
    ),
    
    mainPanel(
      h2("Estimate the House Price in King County, Washington"),
      hr(),
      h5("Developed by: Harish K Rongala"),
      h5("Date: October 14th, 2017"),
      hr(),
      
      tabsetPanel(
        tabPanel("Predict House Price", 
      
                 
      fluidRow(
        tags$br(),
        column(4,
               strong("Predicted Price: "),
               verbatimTextOutput("predVal")
        ),
        
        column(4,
               strong("Lower Estimate: "),
               verbatimTextOutput("lwrPredVal")
        ),
        
        column(4,
               strong("Upper Estimate: "),
               verbatimTextOutput("uprPredVal")
        )
        
      ),
      
      fluidRow(
        hr(),
        h4("Checkout the factors that affect the House Price"),
        plotOutput("barPlot")
      )
      
        ),
      
      tabPanel("ReadMe",
               tags$br(),
               p("The prediction model is fairly self-explanatory. 
                  However, few variable names may need some explanation. 
                  The dataset can be found at "),
               tags$a("https://www.kaggle.com/harlfoxem/housesalesprediction"),
               tags$br(),
               p("After a few statistical tests, the following variables seemed to be significant in predicting 
                 the price of the House."),
               tags$br(),
               tags$ul(
                 tags$li("Bedrooms: Total number of bedrooms in the House"),
                 tags$li("Bathrooms: There will be four major components in the bathroom. They are, toilet, sink, bathtub and shower. 
                         If a bathroom has just 2 components then it a half bathroom, indicated as 0.5. Similarly, there exists a quarter, 
                         half and three quarter bathrooms."),
                 tags$li("Floors: Total number of floors in a house."),
                 tags$li("Waterfront: Is there a overlooking waterfront."),
                 tags$li("Living area: Total interior living area in Square feet."),
                 tags$li("Basement area: Total interior area under the ground level."),
                 tags$li("Above area: Total interior area above the ground level."),
                 tags$li("Neighboring area: Total interior living area for the nearest 15 neighbors"),
                 tags$li("View: How the house looks like on a scale of 0-4. 4 being a great view." ),
                 tags$li("Quality and design: Rate the grade of construction and design on a scale of 1-13. 1-3 being low, 7 is an average, 
                         while anything between 11 and 13 shows a high quality."),
                 tags$li("Year Built or Renovated: Self-explanatory"),
                 tags$li("Zipcode: Self-explanatory")
                 
               )
               )
      )
    )
  )
)
