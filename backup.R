library(shiny)
library(ggplot2)
library(gridExtra)
load('workingScript.R')

ui<-shinyUI(fluidPage(
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

      h3("Estimate the House price in King County"),
      h5("Developed by: Harish K Rongala"),
      hr(),
      fluidRow(
        column(3,
      strong("Predicted Price: "),
      verbatimTextOutput("predVal"),
      strong("Lower Estimate: "),
      verbatimTextOutput("lwrPredVal"),
      strong("Upper Estimate: "),
      verbatimTextOutput("uprPredVal")
        )

      ),

      fluidRow(
        plotOutput("barPlot")
      )

    )
  )
))

server<-function(input, output){

  dataHandled<-reactive({
    bedRooms<-input$selectBedrooms
    bathRooms<-input$selectBathrooms
    floors<-input$selectFloors
    hasWaterFront<-input$waterFront
    # Area
    living<-input$livingSqFt
    basement<-input$basementSqFt
    above<-input$aboveSqFt
    neighbor<-input$neighborSqFt
    # View, Grade
    view<-input$view
    grade<-input$grade

    yr<-input$date
    zip<-input$zipcode

    isRenovated<-input$isRenovated
    yearRenovated<- if(isRenovated==1) yr else 0
    #yearRenovated<-0

    df<-data.frame("sqft_living" = living, "grade" = grade, "sqft_above" = above, "bathrooms" = bathRooms, "sqft_basement" = basement,
                   "bedrooms" = bedRooms, "floors" = floors, "waterfront" = hasWaterFront, "howOld" = 2015, "view" = view,
                   "sqft_living15" = neighbor, "zipcode" = zip, "yr_renovated" = yearRenovated)

    pred<-predict(fit, newdata = df, interval = "confidence")

    dd<-data.frame(whichPred = c('Lower Estimate', 'Predicted Price', 'Upper Estimate'), predVals = c(pred[2], pred[1], pred[3]))
    return(list(pr = dd))
  })


  output$predVal<-renderPrint({cat(dataHandled()$pr[1,2])})
  output$lwrPredVal<-renderPrint({cat(dataHandled()$pr[2,2])})
  output$uprPredVal<-renderPrint({cat(dataHandled()$pr[3,2])})
  output$barPlot<-renderPlot({
    p1<-ggplot(data = houseDataClean, aes(x = grade, y = price)) + geom_smooth(method = 'lm');
    p2<-ggplot(data = houseDataClean, aes(x = view, y = price)) + geom_smooth(method = 'lm');
    p3<-ggplot(data = houseDataClean, aes(x = sqft_above, y = price)) + geom_smooth(method = 'lm');
    p4<-ggplot(data = houseDataClean, aes(x = sqft_living, y = price)) + geom_smooth(method = 'lm');
    p5<-ggplot(data = houseDataClean, aes(x = sqft_living15, y = price)) + geom_smooth(method = 'lm');
    p6<-ggplot(data = houseDataClean, aes(x = sqft_basement, y = price)) + geom_smooth(method = 'lm');
    p7<-ggplot(data = houseDataClean, aes(x = howOld, y = price)) + geom_smooth(method = 'lm');
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 2, ncol = 4)
    })

}

shinyApp(ui = ui, server = server)
