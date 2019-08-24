library(shiny)
library(corrplot)
library(colourpicker)
library(shinyBS)
library(DT)

col1 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col2 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue","#00007F"))

ui <- fluidPage(
  headerPanel("Correlation heatmap Module"),
  
  sidebarPanel(
    width = 4,
    fileInput(inputId = "exprlist", label = "Protein File Input:", accept = c(".txt"), width = "70%"),
    radioButtons(inputId = "cor_method", label = "Method of  correlation coefficient", choices = c("pearson", "spearman", "kendall", "none"), width = "70%"),
    radioButtons(inputId = "shape", label = "Shape", 
                       choices = c("circle", "color", "square", "ellipse", "pie", "number", "shade"), 
                       selected = "circle"),
    
    actionButton(inputId = "button", label = "Submit! Go!", icon = icon("refresh"), style = "margin-bottom: 15px; margin-top: 15px; margin-right: 30px;"),
    downloadButton(outputId = "download_plot", label = "Download Plot", style = "margin-bottom: 15px; margin-top: 15px;"),
    br(),
    actionButton("tabBut", "User Manual"),
    
    hr(),
    
    h4("More Optional Parameters: ", style = "margin-bottom: 25px;"),
    
    selectInput(inputId = "plot_color", label = "User-defined color", choices = c("Palettes1", "Palettes2", "Palettes3", "Palettes4" ,"costom"), selected = FALSE, width = "40%"),
    conditionalPanel(
      condition = "input.plot_color == 'costom'",
      fluidRow(
        column(
          width = 4, 
          colourpicker::colourInput(inputId = "highcol", label = "High color(blue)", value = "blue", palette = "limited", returnName = TRUE)
        ),
        column(
          width = 4,
          colourpicker::colourInput(inputId = "mediancol", label = "Median corlor(white)", value = "white", palette = "limited", returnName = TRUE)
        ),
        column(
          width = 4,
          colourpicker::colourInput(inputId = "lowcol", label = "Low corlor(red)", value = "red", palette = "limited", returnName = TRUE)
        )
      )
    ),
    selectInput(inputId = "multi_graph", label = "Combination of two graph", choices = c(TRUE, FALSE), selected = FALSE, width = "40%"),
    conditionalPanel(
      condition = "input.multi_graph == 'TRUE'",
      fluidRow(
        column(
          width = 6, 
          radioButtons(inputId = "upper", label = "Upper graph", choices = c("circle", "color", "square", "ellipse", "pie", "number", "shade", "none"))
        ),
        column(
          width = 6,
          radioButtons(inputId = "lower", label = "Lower graph", choices = c("circle", "color", "square", "ellipse", "pie", "number", "shade", "none"))
        )
      )
    ),
    fluidRow(
      column(
        width = 5,
        selectInput(inputId = "show_number", label = "Show coefficient", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      column(
        width = 5,
        selectInput(inputId = "show_grid", label = "Show grid", choices = c(TRUE, FALSE), selected = FALSE)
      )
    ),
    
    fluidRow(
      column(
        width = 5,
        colourpicker::colourInput(inputId = "numbercol", label = "Number color", value = "gray40", palette = "limited", returnName = TRUE)
      ),
      column(
        width = 5,
        colourpicker::colourInput(inputId = "fontcol", label = "x/y label color", value = "black", palette = "limited", returnName = TRUE)
      ),
      column(
        width = 5,
        sliderInput(inputId = "range_cor", label = "Min of coefficient", min = -1, max = 1, value = 0.8, step = 0.05)
      ),
      column(
        width = 5,
        sliderInput(inputId = "number_cex", label = "font size of coefficient", min = 0, max = 2, value = 1, step = 0.1)
      )
    )
  ),
  
  mainPanel(
    column(
      width = 11, offset = 1,
      
      fluidRow(
        column(
          width = 4,
          sliderInput(inputId = "plotheight", label = "Height of Total Plot:", min = 1, max = 100, value = 20, step = 2)
        ),
        column(
          width = 4,
          sliderInput(inputId = "plotwidth", label = "Width of Total Plot:", min = 1, max = 100, value = 24, step = 2)
        ),
        column(
          width = 4,
          sliderInput(inputId = "plotres", label = "DPI of Total Plot:", min = 100, max = 800, value = 300, step = 100)
        )
      ),
      
      br(),
      
      plotOutput("plot", height = "600px")
      # uiOutput(outputId = "outplot")
    )
  ),
  
  bsModal(
    id = "readme", title = "Use Mannul", trigger = "tabBut", size = "large",
    tags$h2("Please read me !!!", style = "color:#47a3da; font-family: sans-serif"),
    br(),
    p("相关热图矩阵所需表达谱示例文件如下(须有表头的)："),
    DTOutput("example_input"),
    br(),
    p("选择好参数后，点击Submit! Go!运行工具，出具图片；每次修改后均需要点击Submit按钮才能更新参数"),
    p("点击Download Plot即可下载图片")
  )
)

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$exprlist
    req(infile$datapath)
    data <- read.table(file = infile$datapath, sep = "\t", header = T, stringsAsFactors = F, quote = "", check.names = F, row.names = 1)
    return(data)
  })
  
  cor_m <- reactive({
    data <- filedata()
    data <- data[complete.cases(data),]
    data <- log2(data + 1)
    res <- cor(data, use = "everything", method = input$cor_method)
    return(as.matrix(res))
  })
  
  min_cor <- reactive({
    cor_value <- round(min(cor_m()), 1)
    if (cor_value > min(cor_m())){
      cor_value <- cor_value - 0.1
      if (cor_value < -1){
        cor_value <- -1
      }
    }else if (cor_value > 0.95){
      cor_value <- 0.95
    }else{
      cor_value <- cor_value
    }
    return(cor_value)
  })
  
  observe({
    updateSliderInput(session, inputId = "range_cor", label = "Min of coefficient", min = -1, max = 1, value = min_cor(), step = 0.05)
  })
  
  plot_graph <- eventReactive(input$button, {
    if (input$plot_color == "Palettes1"){
      col <- col1(200)
    }else if (input$plot_color == "Palettes2"){
      col <- col2(200)
    }else if (input$plot_color == "Palettes3"){
      col <- terrain.colors(100)
    }else if (input$plot_color == "Palettes4"){
      col <- cm.colors(100)
    }else{
      crp <- colorRampPalette(c(input$highcol, input$mediancol, input$highcol))
      col <- crp(200)
    }
    
    if (input$show_number == TRUE){
      coefcol <- input$numbercol
    }else{
      coefcol <- NULL
    }
    if (input$show_grid == TRUE){
      gridcol <- "grey"
    }else{
      gridcol <- NULL
    }
    
    if (input$multi_graph == FALSE){
      corrplot(cor_m(), method = input$shape, is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
               addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, number.cex = input$number_cex)
    }else{
      if (input$upper == "none"){
        if (input$lower == "none"){
          NULL
        }else{
          corrplot(cor_m(), method = input$lower, type = "lower", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                   addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, diag = FALSE)
        }
      }else{
        if (input$lower == "none"){
          corrplot(cor_m(), method = input$upper, type = "upper", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                   addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, diag = FALSE)
        }else{
          # corrplot(cor_m(), method = "circle", type = "upper", is.corr = FALSE, cl.lim = c(0.9,1), col = col1(200), tl.pos = "d")
          # corrplot(cor_m(), add = TRUE, method = "square", type = "lower", is.corr = FALSE, cl.lim = c(0.9,1), col = col1(200), diag = FALSE, tl.pos = "n", cl.pos = "n", addCoef.col = "grey")
          
          corrplot(cor_m(), method = input$upper, type = "upper", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                   addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, tl.pos = "d")
          corrplot(cor_m(), add = TRUE, method = input$lower, type = "lower", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                   addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, diag = FALSE, tl.pos = "n", cl.pos = "n")
        }
      }
    }
  })
  
  output$plot <- renderPlot({
    plot_graph()
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("correlation", ".png")
    },
    contentType = "image/png",
    content = function(file){
      png(filename = file, width = input$plotwidth, height = input$plotheight, units = "cm", res = input$plotres)

      if (input$plot_color == "Palettes1"){
        col <- col1(200)
      }else if (input$plot_color == "Palettes2"){
        col <- col2(200)
      }else if (input$plot_color == "Palettes3"){
        col <- terrain.colors(100)
      }else if (input$plot_color == "Palettes4"){
        col <- cm.colors(100)
      }else{
        crp <- colorRampPalette(c(input$highcol, input$mediancol, input$highcol))
        col <- crp(200)
      }
      
      if (input$show_number == TRUE){
        coefcol <- input$numbercol
      }else{
        coefcol <- NULL
      }
      if (input$show_grid == TRUE){
        gridcol <- "grey"
      }else{
        gridcol <- NULL
      }
      
      if (input$multi_graph == FALSE){
        corrplot(cor_m(), method = input$shape, is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                 addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, number.cex = input$number_cex)
      }else{
        if (input$upper == "none"){
          if (input$lower == "none"){
            NULL
          }else{
            corrplot(cor_m(), method = input$lower, type = "lower", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                     addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, diag = FALSE)
          }
        }else{
          if (input$lower == "none"){
            corrplot(cor_m(), method = input$upper, type = "upper", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                     addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, diag = FALSE)
          }else{
            # corrplot(cor_m(), method = "circle", type = "upper", is.corr = FALSE, cl.lim = c(0.9,1), col = col1(200), tl.pos = "d")
            # corrplot(cor_m(), add = TRUE, method = "square", type = "lower", is.corr = FALSE, cl.lim = c(0.9,1), col = col1(200), diag = FALSE, tl.pos = "n", cl.pos = "n", addCoef.col = "grey")
            
            corrplot(cor_m(), method = input$upper, type = "upper", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                     addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, tl.pos = "d")
            corrplot(cor_m(), add = TRUE, method = input$lower, type = "lower", is.corr = FALSE, cl.lim = c(input$range_cor,1), col = col,
                     addCoef.col = coefcol, tl.col = input$fontcol, addgrid.col = gridcol, diag = FALSE, tl.pos = "n", cl.pos = "n")
          }
        }
      }
      
      dev.off()
    }
  )
  
  example_file <- reactive({
    read.table(file = "example_expr.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
  })
  
  observeEvent(input$tabBut, {
    output$example_input <- renderDT(example_file(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
  })
  
}

shinyApp(ui, server)