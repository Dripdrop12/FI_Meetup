shinyServer(function(input, output){
        output$plot1 <- renderPlot({p1})
        output$plot2 <- renderPlot({p2})
        output$dt1 <- renderDataTable({sum_cfpb}, options = list(pageLength = 10, 
                                                                  orderClasses = TRUE,
                                                                  order = list(1,'desc'),
                                                                  lengthMenu = c(5,10,15,20,50),
                                                                  rowNames = FALSE))
        output$dt2 <- renderDataTable({both3}, options = list(pageLength = 10, 
                                                                orderClasses = TRUE,
                                                                order = list(3,'desc'),
                                                                lengthMenu = c(5,10,15,20,50),
                                                                rowNames = FALSE))    

})