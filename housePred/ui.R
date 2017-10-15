library(shiny)
library(ggplot2)
library(gridExtra)
load('workingScript.R')

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
      p("`apple`"),
      h5("Developed by: Harish K Rongala"),
      h5("Date: October 14th, 2017"),
      hr(),
      fluidRow(
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
        h3("Checkout the factors that affect the House Price"),
        plotOutput("barPlot")
      )

    )
  )
)
