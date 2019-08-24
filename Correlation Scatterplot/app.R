library(shiny)
library(colourpicker)
library(shinyalert)
library(dplyr)
library(DT)
library(shinyBS)

ui <- fluidPage(
  useShinyalert(),
  
  headerPanel("Correlation Scatterplot Module"),
  sidebarPanel(
    width = 3,
    fileInput(inputId = "exprlist", label = "Protein File Input:", accept = c(".txt"), width = "70%"),
    
    radioButtons(inputId = "normalized_style", label = "Normalized Method :", 
                 choices = c("None(iTRAQ)", "Specific(metabolism)", "Log2(labelfree/DIA/transcriptome)"), 
                 selected = "Log2(labelfree/DIA/transcriptome)"),
    actionButton(inputId = "button", label = "Submit! Go!", icon = icon("refresh"), style = "margin-bottom: 15px; margin-top: 15px; margin-right: 30px;"),
    downloadButton(outputId = "download_plot", label = "Download Plot", style = "margin-bottom: 15px; margin-top: 15px;"),
    br(),
    actionButton("tabBut", "User Manual"),
    
    hr(),
    
    h4("More Optional Parameters: ", style = "margin-bottom: 25px;"),
    
    tags$div(
      style = "width: 70%",
      colourpicker::colourInput(inputId = "point_col", label = "Scatter color (e.g., black)", value = "black", palette = "limited", returnName = TRUE),
      colourpicker::colourInput(inputId = "sample_font_col", label = "Sample font color (e.g., black)", value = "black", palette = "limited", returnName = TRUE),
      colourpicker::colourInput(inputId = "cor_font_col", label = "Cor font color (e.g., blue)", value = "blue", palette = "limited", returnName = TRUE)
    ),
    
    fluidRow(
      column(
        width = 6,
        sliderInput(inputId = "shape_type", label = "Point shape：", min = 1, max = 25, value = 21, step = 1)
      ),
      column(
        width = 6,
        sliderInput(inputId = "point_size", label = "Point size：", min = 0, max = 2, value = 1.2, step = 0.1)
      ),
      column(
        width = 6,
        sliderInput(inputId = "label_x", label = "Cor font(x axis)：", min = 0, max = 1, value = 0.2, step = 0.1)
      ),
      column(
        width = 6,
        sliderInput(inputId = "label_y", label = "Cor font(y axis)：", min = 0, max = 1, value = 0.9, step = 0.1)
      ),
      column(
        width = 6,
        sliderInput(inputId = "sample_font_size", label = "Size of sample font：", min = 0, max = 3, value = 1.8, step = 0.1)
      ),
      column(
        width = 6,
        sliderInput(inputId = "cor_font_size", label = "Size of cor font：", min = 0, max = 2, value = 1.2, step = 0.1)
      )
    )
  ),
  
  mainPanel(
    column(
      width = 10, offset = 2,
      
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
      
      # plotOutput("plot", height = "500px")
      uiOutput(outputId = "outplot")
    )
  ),
  
  bsModal(
    id = "readme", title = "Use Mannul", trigger = "tabBut", size = "large",
    tags$h2("Please read me !!!", style = "color:#47a3da; font-family: sans-serif"),
    br(),
    p("相关性散点图所需表达谱示例文件如下(须有表头的)："),
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
  
  normalized_data <- eventReactive(input$button, {
    data <- filedata()
    if (input$normalized_style == "Specific(metabolism)"){
      col_sum <- apply(data, 2, sum, na.rm = TRUE) 
      res <- apply(data, 1, function(x){
        x / col_sum * 1000
      })
      res <- t(res)
      res[data == 0] <- NA
      res <- log2(res)
    }else if (input$normalized_style == "Log2(labelfree/DIA/transcriptome)"){
      data[data == 0] <- NA
      res <- log2(data)
    }else{
      res <- data
    }
    return(res)
  })
  
  plot_graph <- eventReactive(input$button, {
    panel.cor <- function(x, y, digits = 3, ...){
      points(x, y, pch = input$shape_type, col = input$point_col, cex = input$point_size)
      r <- cor(x, y, use ="na.or.complete", method="pearson")
      txt <- format(r, digits = digits)
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      text(input$label_x, input$label_y, txt, cex = input$cor_font_size, col = input$cor_font_col)
    }
    
    panel.text <- function(x, y, labels, cex, font, ...){
      text(0.5, 0.5, labels, cex = input$sample_font_size, font = font, col = input$sample_font_size)
    }
    
    pairs(normalized_data(), upper.panel = panel.cor, lower.panel = panel.cor, gap = 0.5, bg = "light blue", text.panel = panel.text)
  })
  
  output$outplot <- renderUI({
    plotOutput("plot", height = paste0(input$plotheight, "cm"), width = paste0(input$plotwidth, "cm"))
  })
  
  output$plot <- renderPlot({
    plot_graph()
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("Scatterplot", ".png")
    },
    contentType = "image/png",
    content = function(file){
      png(filename = file, width = input$plotwidth, height = input$plotheight, units = "cm", res = input$plotres)
      
      panel.cor <- function(x, y, digits = 3, ...){
        points(x, y, pch = input$shape_type, col = input$point_col, cex = input$point_size)
        r <- abs(cor(x, y, use ="na.or.complete", method="pearson"))
        txt <- format(r, digits = digits)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(input$label_x, input$label_y, txt, cex = input$cor_font_size, col = input$cor_font_col)
      }

      panel.text <- function(x, y, labels, cex, font, ...){
        text(0.5, 0.5, labels, cex = input$sample_font_size, font = font, col = input$sample_font_size)
      }

      pairs(normalized_data(), upper.panel = panel.cor, lower.panel = panel.cor, gap = 0.5, bg = "light blue", text.panel = panel.text)
      
      dev.off()

      # ggsave(filename = file, plot = p, device = "png", width = 22, height = 15, units = "cm", dpi = 600)
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