library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "ComplexHeatmap"),
  dashboardSidebar(
    fileInput("filename","Choose File to Upload:", accept = c(".csv")),
    sidebarMenu(
      menuItem("Quickly visualize", tabName = "quicklyplot"),
      menuItem("Complex visualize", tabName = "complexplot")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("quicklyplot",
              fluidRow(
                column(width = 9,
                       box(width = NULL,solidHeader = TRUE,
                           plotOutput("simpleplot", height = 500)
                       ),
                       fluidRow(
                         column(width = 6,
                                box(width = NULL, status = "info",
                                    title = "The gene list on the row",
                                    tableOutput("genelist") 
                                )
                         ),
                         column(width = 5, offset = 1,
                                box(width = NULL, status = "warning",
                                    downloadButton("downloadlist",label = "Download Gene List")    
                                )
                         )
                       )
                ),
                column(width = 3,
                       box(width = NULL,status = "warning",
                           numericInput("simpleheigh",label = "Graph heigh value",value = 3000),
                           numericInput("simplewidth",label = "Graph width value",value = 4000),
                           radioButtons("downlist",label = "Select the graph type",,choices = c("png","pdf")),
                           downloadButton("downloadsimple",label = "Download Heatmap")
                       )
                )
              )
      ),
      tabItem("complexplot",
              fluidRow(
                column(width = 8,
                       box(width = NULL,solidHeader = TRUE,
                           plotOutput("complexmap",height = 500)
                       ),
                       fluidRow(
                         column(width = 6,
                                box(width = NULL, status = "info",
                                    title = "The gene list on the row",
                                    tableOutput("complexgenelist")
                                )
                         ),
                         column(width = 5, offset = 1,
                                box(title = "Download",
                                    solidHeader = T, status = "info",
                                    width = NULL,
                                    h5("Choose height and width for heatmap"),
                                    fluidRow(
                                      column(width = 6,
                                             numericInput("complexheigh",label = "Graph heigh value",value = 3000)
                                      ),
                                      column(width = 6,
                                             numericInput("complexwidth",label = "Graph width value",value = 4000)
                                      ),
                                      column(width = 12,
                                             radioButtons("complextype",label = "Select the graph type",choices = c("png","pdf"))
                                      )
                                    )
                                ),
                                
                                box(width = NULL,status = "warning",
                                    downloadButton("downloadcomplexlist",label = "Download Gene List"),
                                    downloadButton("downloadcomplex",label = "Download Heatmap")
                                )
                         )
                       )
                ),
                
                # box(title = "Submit parameter",
                #     solidHeader = T,status = "info",
                #     width = 4,
                #     actionButton("datasubmit", label = "Submit"),
                #     tags$style("button#datasubmit {margin-left:auto;margin-right:auto;display:block; 
                #                        background-color:#00CCFF; padding: 5px 25px; font-family:Andika, Arial, sans-serif; 
                #                        font-size:1.5em; letter-spacing:0.05em; text-transform:uppercase;
                #                        color:white; text-shadow: 0px 1px 10px #000;border-radius: 15px;
                #                        box-shadow: rgba(0, 0, 0, .55) 0 1px 6px;}")
                #     ),
                
                box(title = "The parameter for complex heatmap",
                    solidHeader = T, status = "info",
                    collapsible = T, collapsed = F,
                    width = 4,
                    
                    fluidRow(
                      box(title = "Expression To Color Option",
                          solidHeader = T, status = "info",
                          width = 12,
                          h5("The left is expression value, the right is color"),
                          fluidRow(
                            column(width = 6,
                                   numericInput("lowvalue", label = "Low Value", value = NULL,-5,0)
                            ),
                            column(width = 6,
                                   selectInput("lowcol", label = "Low color", choices = c("green","blue", "purple", "red", 
                                                                                          "orange", "yellow", "white"),
                                               selected = "green")
                            ),
                            column(width = 6,
                                   numericInput("highvalue", label = "High Value", value = NULL,0,5)
                            ),
                            column(width = 6,
                                   selectInput("highcol", label = "High color", choices = c("red", "orange", "yellow", 
                                                                                            "green", "blue", "purple", "white"),
                                               selected = "red")
                            )
                          )
                      ),
                      
                      box(title = "Heatmap Name",
                          solidHeader = T, status = "info",
                          width = 12,
                          h5("Please choose the name option for heatmap"),
                          fluidRow(
                            column(width = 6,
                                   selectInput("requirename", label = "Legend or not", c("TRUE", "FALSE"))
                                   ),
                            column(width = 6,
                                   textInput("nametxt", label = "Legend Name", value = "expression")
                                   )
                          )
                      ),
                      
                      box(title = "Row Relative Option",
                          solidHeader = T, status = "info",
                          width = 12,
                          h5("Choose orientation for row name and the height for dend"),
                          fluidRow(
                            column(width = 6,
                                   selectInput("ori_rowname", label = "Select orientation for row name",
                                               c("right","left","none"))
                            ),
                            column(width = 6,
                                   numericInput("width_rowdend",label = "Select height for row dend",
                                                value = 4,1,10)
                            )
                          )
                      ),
                      
                      box(title = "column Relative Option",
                          solidHeader = T, status = "info",
                          width = 12,
                          h5("Choose orientation for column name and the height for dend"),
                          fluidRow(
                            column(width = 6,
                                   selectInput("ori_columnname", label = "Select orientation for column name",
                                               c("bottom","top","none"))
                            ),
                            column(width = 6,
                                   numericInput("height_columndend",label = "Select height for column dend",
                                                value = 2,1,10)
                            )
                          )                          
                      ),
                      
                      box(title = "Cluster Relative Option",
                          solidHeader = T, status = "info",
                          width = 12,
                          h5("Choose distance and algorithm to cluster"),
                          fluidRow(
                            column(width = 6,
                                   selectInput("distance", label = "Select distance to cluster",
                                               c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"))
                            ),
                            column(width = 6,
                                   selectInput("algorithm", label = "Select algorithm to cluster",
                                               c("complete", "single", "average", "centroid", "median"))
                            )
                          )
                      )
                    )
                )
              )
      )
    )
  )
)