library(shiny)
library(shinyBS)
library(colourpicker)
library(dplyr)
library(DT)
library(ComplexHeatmap)
library(circlize)
library(shinyalert)


options(shiny.maxRequestSize=100*1024^2) 

ui <- fluidPage(
  useShinyalert(),
  titlePanel("Heatmap Plot Module"),
  fluidRow(
    column(width = 3,
           wellPanel(
             fileInput(inputId = "cluster_list", label = "Cluster File Input:", accept = c(".txt")),
             selectInput(inputId = "style", label = "Choose style of this Heatmap Plot",
                         choices = c("Normal", "Advanced")
             ),
             selectInput(inputId = "normalize", label = "Whether to standardize or not", c("TRUE", "FALSE"), selected = FALSE),
             conditionalPanel(
               condition = "input.normalize == 'TRUE'",
               radioButtons(inputId = "normalized_style", label = "Normalized Method :", 
                            choices = c("None", "Z-score(labelfree/metabolism)", "Log2(iTRAQ)"), 
                            selected = "None")
             ),
             actionButton(inputId = "button", label = "Submit! Go!", icon = icon("refresh"), style = "margin-bottom:20px; margin-right:20px;"),
             actionButton("tabBut", "User Manual", style = "margin-bottom:20px"),
             br(),
             downloadButton(outputId = "download_plot", label = "Download Heatmap", icon = icon("download")),
             radioButtons(inputId = "plot_style", label = NULL, choices = c("png", "pdf"), selected = "png")
             # br(),
             
           ),
           
           wellPanel(
             tags$h4("Left is expression value, Right is color value"),
             fluidRow(
               column(
                 width = 5,
                 numericInput("lowvalue", label = "Low value", value = -1,-5,0)
               ),
               column(
                 width = 6, offset = 1,
                 colourpicker::colourInput(inputId = "lowcol", label = "Low corlor (e.g., blue)", value = "blue", palette = "limited", returnName = TRUE)
               ),
               column(
                 width = 5, 
                 numericInput("highvalue", label = "High value", value = 1,0,5)
               ),
               column(
                 width = 6, offset = 1,
                 colourpicker::colourInput(inputId = "highcol", label = "High corlor (e.g., red)", value = "red", palette = "limited", returnName = TRUE)
               ),
               column(
                 width = 5,
                 numericInput("medianvalue", label = "Median value", value = 0,-5,5)
               ),
               column(
                 width = 6, offset = 1,
                 colourpicker::colourInput(inputId = "mediancol", label = "Median corlor (e.g., white)", value = "white", palette = "limited", returnName = TRUE)
               )
             )
           ),
           
           wellPanel(
             tags$h4("Choose orientation for row name and the height for dend"),
             fluidRow(
               column(
                 width = 6, 
                 selectInput("ori_rowname", label = "Orientation of row name:", c("right", "left"))
               ),
               column(
                 width = 6, #offset = 1,
                 numericInput("width_rowdend",label = "Height of row dend:", value = 4,1,10)
               )
             ),
             tags$h4("Choose orientation for column name and the height for dend"),
             fluidRow(
               column(
                 width = 6,
                 selectInput("ori_columnname", label = "Orientation of col name:", c("bottom", "top"))
               ),
               column(
                 width = 6,
                 numericInput("height_columndend",label = "Height of col dend:", value = 2,1,10)
               )
             )
           ),
           
           wellPanel(
             # tags$h4("Width of the single heatmap"),
             fluidRow(
               column(
                 width = 6,
                 sliderInput(inputId = "cluster_width", label = "Width of heatmap:", min = 1, max = 100, value = 13, step = 1)
               ),
               column(
                 width = 6,
                 sliderInput(inputId = "col_font_size", label = "Size of sample font:", min = 1, max = 20, value = 10, step = 1)
               ),
               column(
                 width = 6,
                 sliderInput(inputId = "row_font_size", label = "Size of id font:", min = 1, max = 20, value = 10, step = 1)
               ),
               column(
                 width = 5,
                 selectInput("legend_local", label = "Move legend to bottom", choices = c(TRUE, FALSE), selected = FALSE)
               )
             )
           ),
           
           wellPanel(
             tags$h4("Whether cluster or not, for row and colmun"),
             fluidRow(
               column(
                 width = 6,
                 selectInput("cluster_row", label = "Clutser protein or not", c("TRUE", "FALSE"))
               ),
               column(
                 width = 6,
                 selectInput("show_row", label = "Show protein id or not", choices = c("TRUE", "FALSE"))
               ),
               column(
                 width = 6,
                 selectInput("cluster_col", label = "Clutser sample or not", c("TRUE", "FALSE"))
               ),
               column(
                 width = 6,
                 selectInput("show_col", label = "Show sample id or not", c("TRUE", "FALSE"))
               )
             )
           ),
           
           wellPanel(
             tags$h4("Choose distance and algorithm to cluster"),
             fluidRow(
               column(
                 width = 6,
                 selectInput("distance", label = "Select distance to cluster:", c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), selected = "euclidean")
               ),
               column(
                 width = 6,
                 selectInput("algorithm", label = "Select algorithm to cluster:", c("complete", "single", "average", "centroid", "median"), selected = "average")
               )
             )
           )
    ),
    
    mainPanel(
      column(
        width = 11, offset = 1,
        fluidRow(
          column(
            width = 4,
            sliderInput(inputId = "plotheight", label = "Height of Total Heatmap Plot:", min = 1, max = 100, value = 20, step = 2)
          ),
          column(
            width = 4,
            sliderInput(inputId = "plotwidth", label = "Width of Total Heatmap Plot:", min = 1, max = 100, value = 24, step = 2)
          ),
          column(
            width = 4,
            sliderInput(inputId = "plotres", label = "DPI of Total Heatmap Plot:", min = 100, max = 800, value = 300, step = 100)
          )
        ),
        
        uiOutput(outputId = "outplot")
      )
    )
  ),
  
  bsModal(
    id = "readme", title = "Use Mannul", trigger = "tabBut", size = "large",
    tags$h2("Please read me !!!", style = "color:#47a3da; font-family: sans-serif"),
    br(),
    p("聚类图主输入文件如下（主要来源于生信分析结果中cluster文件夹下的cluster.txt文件）："),
    DTOutput("example_input"),
    hr(),
    p("本工具支持可修改的参数有：1. 上下调颜色，2. 树杈位置及高度，3. 图形宽度和字体大小，4. 蛋白/样本是否聚类，5. 聚类算法选择"),
    # p("如果客户想选择其他类型聚类图，则选择Advanced模式"),
    p("选择好参数后，点击Submit! Go!运行工具，出具图片；每次修改后均需要点击Submit按钮才能更新参数"),
    p("点击Download Heatmap即可下载图片")
  )
)

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$cluster_list
    req(infile$datapath)
    data <- read.table(file = infile$datapath, sep = "\t", header = T, stringsAsFactors = F, quote = "", fill = T, check.names = F)
    names(data)[1] <- "ID"
    
    if (nrow(data) != length(unique(data$ID))){
      shinyalert("Error!", "There are duplicate ID in your protein file.", type = "error")
    }
    
    rownames(data) <- data$ID
    data <- data[,-1]
    
    if(input$normalized_style == "Z-score(labelfree/metabolism)"){
      data_scale <- as.data.frame(t(apply(data,1,scale)))
      colnames(data_scale) <- colnames(data)
      return(data_scale)
    }else if (input$normalized_style == "Log2(iTRAQ)"){
      data_scale <- log2(data)
      return(data_scale)
    }else{
      return(data)
    }
  })
  
  example_file <- reactive({
    read.table(file = "example_cluster.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
  })
  
  
  #============= Function ===============#
  plot_fun <- eventReactive(input$button, {
    if (input$legend_local == FALSE){
      Heatmap(filedata(),
              name = "",
              col = colorRamp2(c(input$lowvalue, input$medianvalue, input$highvalue), c(input$lowcol, input$mediancol, input$highcol)),
              
              row_names_side = input$ori_rowname,
              column_names_side = input$ori_columnname,
              
              row_dend_width = unit(input$width_rowdend, "cm"),
              column_dend_height = unit(input$width_rowdend, "cm"),
              
              show_row_names = as.logical(input$show_row),
              show_column_names = as.logical(input$show_col),
              
              # show_row_dend = FALSE,
              # show_row_names = FALSE,
              column_names_gp = gpar(fontsize = input$col_font_size, fontface = "plain"),
              row_names_gp = gpar(fontsize = input$row_font_size),
              width = unit(input$cluster_width, "cm"),
              heatmap_legend_param = list(color_bar="continuous",legend_height=unit(4,"cm"),
                                          grid_width = unit(0.5, "cm")),
              
              cluster_rows = as.logical(input$cluster_row),
              clustering_distance_rows = input$distance,
              clustering_method_rows = input$algorithm,
              
              cluster_columns = as.logical(input$cluster_col),
              clustering_distance_columns = input$distance,
              clustering_method_columns = input$algorithm
      )
    }else{
      p <- Heatmap(filedata(),
                   name = "",
                   col = colorRamp2(c(input$lowvalue, input$medianvalue, input$highvalue), c(input$lowcol, input$mediancol, input$highcol)),
                   
                   row_names_side = input$ori_rowname,
                   column_names_side = input$ori_columnname,
                   
                   row_dend_width = unit(input$width_rowdend, "cm"),
                   column_dend_height = unit(input$width_rowdend, "cm"),
                   
                   show_row_names = as.logical(input$show_row),
                   show_column_names = as.logical(input$show_col),

                   column_names_gp = gpar(fontsize = input$col_font_size, fontface = "plain"),
                   row_names_gp = gpar(fontsize = input$row_font_size),
                   width = unit(input$cluster_width, "cm"),
                   heatmap_legend_param = list(color_bar="continuous", legend_width = unit(4,"cm"), 
                                               title_position = "topcenter", legend_direction = "horizontal"),
                   
                   cluster_rows = as.logical(input$cluster_row),
                   clustering_distance_rows = input$distance,
                   clustering_method_rows = input$algorithm,
                   
                   cluster_columns = as.logical(input$cluster_col),
                   clustering_distance_columns = input$distance,
                   clustering_method_columns = input$algorithm
      )
      p <- draw(p, heatmap_legend_side="bottom")
      p
    }
  })
  
  observeEvent(input$tabBut, {
    output$example_input <- renderDT(example_file(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
  })
  
  # observeEvent(input$style, {
  #   if (input$style == "Advanced"){
  #     showModal(
  #       modalDialog(
  #         h4("Please select width or height:"),
  #         fileInput(inputId = "prolist", label = "Protein-list Input:", accept = c(".txt"))
  #       )
  #     )
  #   }
  # })
  
  output$outplot <- renderUI({
    plotOutput("plot", height = paste0(input$plotheight, "cm"), width = paste0(input$plotwidth, "cm"))
  })
  
  output$plot <- renderPlot({
    plot_fun()
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste("cluster", input$plot_style, sep = ".")
    },
    # contentType = "image/png",
    content = function(file){
      if (input$plot_style == "png"){
        png(filename = file, width = input$plotwidth, height = input$plotheight, units = "cm", res = input$plotres)
      }else{
        pdf(file = file, width = input$plotwidth/2, height = input$plotheight/2)
      }
      print(plot_fun())
      dev.off()
    }
  )
  
}

shinyApp(ui, server)
