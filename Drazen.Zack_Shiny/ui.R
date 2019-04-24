
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Navbar",
                   tabPanel("Numeric Variables",
                            fluidPage( theme = shinytheme("yeti"),
                                       sidebarLayout(
                                         sidebarPanel( conditionalPanel( 'input.tabs == "Scatter Plot"',
                                           radioButtons("x", "Select X-axis:",
                                                        list("age", "cigsPerDay", "totChol","sysBP", "diaBP","BMI","heartRate", "glucose"),
                                                        selected = "age"),
                                           radioButtons("y","Select Y-axis:",
                                                        list("age", "cigsPerDay", "totChol","sysBP", "diaBP","BMI","heartRate","glucose"),
                                                        selected = "glucose"),
                                           selectInput("col", "Choose to filter by:",
                                                       choices = list("sex",
                                                                      "education" ,
                                                                      "currentSmoker",
                                                                      "BPMeds",
                                                                      "prevalentStroke",
                                                                      "prevalentHyp",
                                                                      "diabetes",
                                                                      "TenYearCHD"), selected ="TenYearCHD" )),
                                           conditionalPanel('input.tabs == "Table"', 
                                                            checkboxGroupInput("show_vars", "Columns to show:",
                                                                               names(df1), selected = names(df1)),
                                                            downloadButton("downloadData", "Download")),
                                           conditionalPanel( 'input.tabs == "Histograms"',
                                                             selectInput("histx", "Select X-axis:",
                                                                          list("age", "cigsPerDay", "totChol","sysBP", "diaBP","BMI","heartRate", "glucose")), 
                                                             selectInput("col2", "Choose to filter by:",
                                                                         choices = list("sex",
                                                                                        "education" ,
                                                                                        "currentSmoker",
                                                                                        "BPMeds",
                                                                                        "prevalentStroke",
                                                                                        "prevalentHyp",
                                                                                        "diabetes",
                                                                                        "TenYearCHD"), selected ="TenYearCHD"), 
                                           sliderInput("binx","Number of bins:", min = 1, max = 50, value = 30)
                                          )
                                         ),
                                         mainPanel(
                                           tabsetPanel( id = "tabs",
                                                       tabPanel("Scatter Plot",h3(strong("Scatter Plot")),plotOutput("plot", brush = brushOpts(id = "user_brush")), 
                                                                h5("Correlation"),verbatimTextOutput("cor"),
                                                                h5("Correlation Test"),verbatimTextOutput("cor_test")),
                                                       tabPanel("Table",h3(strong("Table of Highlighted Data")),dataTableOutput("tab"), h3(strong("Slope and Intercept of Highlighted Data")), 
                                                                h5("Slope"), textOutput("sout"),
                                                                h5("Intercept"), textOutput("int")), 
                                                       tabPanel("Histograms", h3(strong("Histogram")), plotOutput("hist1"),
                                                                h3(strong("Histogram by Categorical Variable ")), plotOutput("hist2")
                                                                )
                                           )
                                         )
                                       )
                            )
                   ),
                   tabPanel("Categorical Variables",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("xx", "Select X-axis:",
                                            choices = list("sex","education" , "currentSmoker","BPMeds","prevalentStroke","prevalentHyp","diabetes",
                                                           "TenYearCHD"), selected = "sex"),
                                selectInput("fill", "Select to fill by:",
                                            choices = list("sex","education" , "currentSmoker","BPMeds","prevalentStroke","prevalentHyp","diabetes",
                                                           "TenYearCHD"), selected = "TenYearCHD")
                              ),
                              mainPanel(
                                tabsetPanel(id = "tab2",
                                            tabPanel("Bar Chart",
                                h3(strong("Bar Chart")), 
                                plotOutput("plot2"),
                                h3(strong("Tables")),
                                verbatimTextOutput("view")),
                                tabPanel("Cross Table",
                                         h3(strong("Cross Table")),
                                         verbatimTextOutput("cs"))
                                )
                              )
                            )),
                   tabPanel("Numeric and Categorical Variables",
                            sidebarLayout(
                              sidebarPanel( 
                                selectInput("x3", "Select X-axis:",
                                            choices = list("sex","education" , "currentSmoker","BPMeds","prevalentStroke","prevalentHyp","diabetes",
                                                           "TenYearCHD"), selected = "sex"),
                                selectInput("y3","Select Y-axis:",
                                            choices = list("age", "cigsPerDay", "totChol","sysBP", "diaBP","BMI","heartRate","glucose"),
                                            selected = "age")
                              ),
                              mainPanel(
                                tabsetPanel(id = "tab3",
                                            tabPanel("Boxplot",
                                h3(strong("Boxplot")),
                                plotOutput("plot3"),
                                h3(strong("Summary")),
                                tableOutput("table3")),
                                tabPanel("Anova",
                                h3(strong("ANOVA Test")),
                                verbatimTextOutput("anova_test")
                                )
                              )
                              )
                              
                            )
                   ),
                   tabPanel("Binomial Regression",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("dep", "Select Dependent Variable:",
                                            choices = list("sex", "currentSmoker","BPMeds","prevalentStroke","prevalentHyp","diabetes",
                                                         "TenYearCHD"), selected = "sex"),
                                selectInput("ref", "Dependent Variable Reference:", choices = unique(df1$sex), selected = "Male"),
                                checkboxGroupInput("inp","Select Independent Variables:",
                                                   choices = colnames(df1), selected = "age")
                              ),
                                 mainPanel(
                                   tabsetPanel(id= "tabs4",
                               tabPanel("Model", h3(strong("Summary")),
                                verbatimTextOutput("sum"),
                                h3(strong("Pseudo R Square Indicators")),
                                verbatimTextOutput("r2")),
                                tabPanel("Antilogarithms of the Coefficients",
                                         h3(strong("Antilogarithms")),
                                verbatimTextOutput("ant"),
                                h3(strong("Confidence Interval of the Antilogarithms")),
                                verbatimTextOutput("co_ant"))
                              )
                            )
                            )
                            )
))
