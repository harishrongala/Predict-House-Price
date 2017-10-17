library(shiny)
library(ggplot2)
library(gridExtra)
source('workingScript.R')

function(input, output){

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

    df<-data.frame("sqft_living" = living, "grade" = grade, "sqft_above" = above, "bathrooms" = bathRooms, "sqft_basement" = basement,
                   "bedrooms" = bedRooms, "floors" = floors, "waterfront" = hasWaterFront, "howOld" = 2015, "view" = view,
                   "sqft_living15" = neighbor, "zipcode" = zip, "yr_renovated" = yearRenovated)

    pred<-predict(fit, newdata = df, interval = "confidence")

    dd<-data.frame(whichPred = c('Lower Estimate', 'Predicted Price', 'Upper Estimate'), predVals = c(pred[2], pred[1], pred[3]))
    return(list(pr = dd))
  })


  output$predVal<-renderPrint({cat(dataHandled()$pr[2,2])})
  output$lwrPredVal<-renderPrint({cat(dataHandled()$pr[1,2])})
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
