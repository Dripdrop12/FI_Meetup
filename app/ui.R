shinyUI(
    navbarPage("Consumer Complaints", theme = "mycss.css", 
        tabPanel("Introduction", 
            fluidPage(fluidRow(column(includeMarkdown("about.Rmd"), width = 7),
                               column(br(), includeMarkdown("menus.Rmd"), width = 4, offset = 1)
            ))
        ),
        navbarMenu("CFPB",
            tabPanel("CFPB Alone",
                fluidPage(fluidRow(column(width = 10, offset = 1,
                    plotOutput("plot1"),
                    br(),
                    dataTableOutput('dt1'),
                    br()
                    ))
                )
            )
        ),
        navbarMenu("SEC",
            tabPanel("CFPB with SEC",
                fluidPage(fluidRow(column(width = 10, offset = 1,
                    plotOutput("plot2"),
                    br(),
                    dataTableOutput('dt2'),
                    br()
                    ))
                )
            )
        ),
        navbarMenu("About",
                   tabPanel("Backend",
                            fluidPage(includeMarkdown("meetup_app_backend.Rmd"))),
                   tabPanel("CFPB API",
                            fluidPage(includeMarkdown("meetup_app_backend_cfpb_api.Rmd"))),
                   tabPanel("Financial Statements Data",
                            fluidPage(includeMarkdown("meetup_app_backend_FS.Rmd"))),
                   tabPanel("The Merge",
                            fluidPage(includeMarkdown("meetup_app_backend_merge.Rmd")))
        )
    )
)
