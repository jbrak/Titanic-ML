#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Import Libraries
library(shiny)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(naniar)

#Download Data
set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)

#Data Wrangling
titanic_clean <- titanic%>%
    distinct %>%
    select(-c(name, cabin, home.dest, x,ticket)) %>%
    replace_with_na(replace = list(age = "?", parch = "?", sibsp = "?", fare = "?", sex = "?", survived = "?", pclass = "?", embarked = "?")) %>%
    na.omit() %>%
    mutate(pclass=factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
           survived = factor(survived, levels = c(0,1), labels = c("Die","Survive")),
           age = as.numeric(age),
           fare = as.numeric(fare))

set.seed(43)
rows <- sample(nrow(titanic_clean))
titanic_clean <- titanic_clean[rows,]

titanic_train <- head(titanic_clean,800)
titanic_test <- tail(titanic_clean,243)
head(titanic_train)
head(titanic_test)

values <- reactiveValues(accurasy=0,model = rpart(survived~., data = titanic_train, method = 'class'))

# Defining the accuracy test
accurasy_test <- function(model) {
    
    prediction <- predict(model,titanic_test, type = "class")
    
    results <- table(titanic_test$survived, prediction)
    
    positives <- c(results["Survive","Survive"], results["Die", "Survive"])
    lbleP <- c("True   \nPositives\n   ", "   False\n   Positives\n   ")
    percentage <- round(positives / sum(positives) *100)
    lbleP <- paste(lbleP,percentage)
    lbleP <- paste(lbleP,"%","   ", sep = "")
    
    negatives <- c(results["Die","Die"], results["Survive", "Die"])
    lbleN <- c("True   \nNegatives   \n", " False\n Negatives\n  ")
    percentage <- round(negatives / sum(negatives) *100)
    lbleN <- paste(lbleN,percentage)
    lbleN <- paste(lbleN,"%","   ", sep = "")
    
    par(mfrow=c(1,2))
    pie(positives, labels = lbleP,col=rainbow(2))
    pie(negatives, labels = lbleN, col=rainbow(2))
    
    values$accurasy <- paste(round(sum(diag(results))/sum(results)*100),"%",sep = "")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Who Survives? - ML"),
    includeHTML("./Project-Info.html"),
    # Sidebar with a slider input for Parameters
    sidebarLayout(
        sidebarPanel(
            sliderInput("mindepth",
                        "Minimum Depth:",
                        min = 1,
                        max = 30,
                        value = 3),
            sliderInput("complexity",
                        "Complexity:",
                        min = 0,
                        max = 0.05,
                        value = 0),
            actionButton("reset", "Reset", width = "100%", alignment = "center"),
            h3(textOutput("accurasy"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("modelPlot"),
           plotOutput("resultsPlot")
        ),
),
includeHTML("./Resources-Used.html"),
)

# Creating and Drawing the model
server <- function(input, output) {
    
    observeEvent({input$mindepth | input$complexity},{
        control <- rpart.control(minsplit = 10,
                                 minbucket = round(5/3),
                                 maxdepth = input$mindepth,
                                 cp = input$complexity)
        values$model <- rpart(survived~., data = titanic_train, method = 'class', control = control)
    })
    
    observeEvent(input$reset,{
        updateSliderInput(session = getDefaultReactiveDomain(),"mindepth",val = 3)
        updateSliderInput(session = getDefaultReactiveDomain(), "complexity",val = 0)
    })
    
    output$modelPlot <- renderPlot({
        rpart.plot(values$model)
    })
    
    output$resultsPlot <- renderPlot({
        accurasy_test(values$model)
    })
    
    output$accurasy <- renderText({paste("Accuracy =",values$accurasy)})
}

# Run the application 
shinyApp(ui = ui, server = server)
