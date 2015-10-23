shinyUI(
        navbarPage("Consumer Complaints", theme = "mycss.css",
                tabPanel("CFPB Alone",
                        fluidPage(fluidRow(column(width=10,offset = 1,
                                p("Given the number of complaints per company in their consumer complaints database, the Consumer Financial Protection Bureau (CFPB) cannot tell if that number is indicative of a company's negligence or its size.  The best they can often do is draw an arbitrary cut-off, such as the mean number of complaints per company and judge each company's total number against that baseline, starting at the top and working their way down."),
                                br(),
                                plotOutput("plot1"),
                                br(),
                                dataTableOutput('dt1')
                                )
                        )
                )
        ),
                tabPanel("With SEC",
                         fluidPage(fluidRow(column(width=10,offset = 1,
                                p("By incorporating information from the Securities and Exchange Commission (SEC), the CFPB could standardize the number of complaints by the size of a company's assets.  Then the CFPB could focus resources on companies that are more likely to violate consumer rights given the way that company has deployed its current assets."),
                                br(),
                                plotOutput("plot2"),
                                br(),
                                dataTableOutput('dt2')
                                )
                        )
                )
        ), 
        navbarMenu("About", 
            tabPanel("Backend",
                 fluidPage(
                     includeMarkdown("meetup_app_backend.Rmd")
                     )
                 ),
        tabPanel("SEC Readme",
                 fluidPage(
                     h1("SEC Readme")
                )
            )
        )
    )
)