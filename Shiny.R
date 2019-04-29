################# PLot ########################

shinyUI(fluidPage(
    titlePanel("Plot Random Numbers"),
    sidebarLayout(
        sidebarPanel(
            numericInput("numeric", "How Many Random Numbers Should be Plotted?", 
                         value = 1000, min = 1, max = 1000, step = 1),
            sliderInput("sliderX", "Pick Minimum and Maximum X Values",
                        -100, 100, value = c(-50, 50)),

            sliderInput("sliderY", "Pick Minimum and Maximum Y Values",
                        -100, 100, value = c(-50, 50)),
            checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
            checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
            checkboxInput("show_title", "Show/Hide Title")
        ),
        mainPanel(
            h3("Graph of Random Points"),
            plotOutput("plot1")
        )
    )
))

shinyServer(function(input, output) {
    output$plot1 <- renderPlot({
        set.seed(2016-05-25)
        number_of_points <- input$numeric
        minX <- input$sliderX[1]
        maxX <- input$sliderX[2]
        minY <- input$sliderY[1]
        maxY <- input$sliderY[2]
        Title1<-input$T
        dataX <- runif(number_of_points, minX, maxX)
        dataY <- runif(number_of_points, minY, maxY)
        xlab <- ifelse(input$show_xlab, "X Axis", "")
        ylab <- ifelse(input$show_ylab, "Y Axis", "")
        main <- ifelse(input$show_title,  ifelse(Title1=="","Plot",Title1), "")
        plot(dataX, dataY, xlab = xlab, ylab = ylab, main = main,
             xlim = c(-100, 100), ylim = c(-100, 100))
    })
})

####################### PLot2 ###########################

library(shiny)
shinyUI(fluidPage(
    titlePanel("Predict Horsepower from MPG"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
            checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
            checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),
            submitButton("Submit")
        ),
        mainPanel(
            plotOutput("plot1"),
            h3("Predicted Horsepower from Model 1:"),
            textOutput("pred1"),
            h3("Predicted Horsepower from Model 2:"),
            textOutput("pred2")
        )
    )
))

shinyServer(function(input, output) {
    model1 <- lm(hp ~ mpg, data = mtcars)
    model2 <- lm(hp ~ mpg + I(mpg^2), data = mtcars)
    model1pred <- reactive({
        mpgInput <- input$sliderMPG
        predict(model1, newdata = data.frame(mpg = mpgInput))
    })
    model2pred <- reactive({
        mpgInput <- input$sliderMPG
        predict(model2, newdata = data.frame(mpg = mpgInput))
    })
    output$plot1 <- renderPlot({
        mpgInput <- input$sliderMPG
        plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
             ylab = "Horsepower", bty = "n", pch = 16, xlim = c(10, 35), ylim = c(50, 350))
        if(input$showModel1){
            abline(model1, col = "red", lwd = 2)
        }
        if(input$showModel2){
            model2lines <- predict(model2,newdata = data.frame(mpg=10:35))
            lines(10:35, model2lines, col = "blue", lwd = 2)
        }
        legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
               col = c("red", "blue"), bty = "n", cex = 1.2)
        points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2)
        points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2)
    })
    output$pred1 <- renderText({
        model1pred()
    })
    output$pred2 <- renderText({
        model2pred()
    })
})

