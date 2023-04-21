#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

      my_germ <- read_excel("german credit card.xls")

      my_germ$binary <- gsub("good", "1", my_germ$good_bad) 
      my_germ$binary <- gsub("bad", "0", my_germ$binary)
      my_germ$binary <- as.numeric(my_germ$binary)
      
      training_idx<- sample(1:1000, size= 0.8*nrow(my_germ)) 
      my_germ_train<- my_germ[training_idx,]
      my_germ_test<- my_germ[-training_idx,] 
      
      library(rpart)
      library(rpart.plot)
      my_tree<- rpart(binary ~ age+amount+checking+coapp+savings+amount, data = my_germ_train, method = "class", cp = input$CPvalue)
      rpart.plot(my_tree, type=1, extra=1)
      
    })

}

