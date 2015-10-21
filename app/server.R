shinyServer(function(input, output){
        output$plot1 <- renderPlot({p1})
        output$plot2 <- renderPlot({p2})
})