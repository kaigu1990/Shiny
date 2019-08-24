library(shiny)
library(shinyalert)
library(dplyr)
library(DT)
library(shinyBS)
library(stringr)
library(ropls)
library(car)

colpalette <- c("#1d953f","#102b6a","#c77eb5", "#fcf16e", "#2585a6", "purple", "#e0861a", "#d71345", "#6b473c", "#78a355", "#fdb933", "#5e7c85", "#411445", "#c37e00", "#bed742","#009ad6","#9d9087")
colpalette1 <- c("#1d953f","#102b6a")


ui <- fluidPage(
  useShinyalert(),
  
  headerPanel("Metabolomics PCA Module"),
  sidebarPanel(
    width = 3,
    fileInput(inputId = "exprlist", label = "Protein File Input:", accept = c(".txt"), width = "70%"),
    
    fileInput(inputId = "grouplist", label = paste0("Group File Input: ", "(样本名称以xx-1形式结尾，则无需输入)"), accept = c(".txt"), width = "70%"),
    
    radioButtons(inputId = "style", label = "Graph style :", 
                 choices = c("Simac", "Normal(useless)"), 
                 selected = "Simac"),
    
    actionButton(inputId = "button", label = "Submit! Go!", icon = icon("refresh"), style = "margin-bottom: 15px; margin-top: 15px; margin-right: 30px;"),
    downloadButton(outputId = "download_plot", label = "Download Plot", style = "margin-bottom: 15px; margin-top: 15px;"),
    br(),
    actionButton("tabBut", "User Manual"),
    
    hr(),
    
    h4("More Optional Parameters: ", style = "margin-bottom: 25px;"),
    
    # tags$div(
    #   style = "width: 70%",
    #   colourInput(inputId = "point_col", label = "Scatter color (e.g., black)", value = "black", palette = "limited", returnName = TRUE),
    #   colourInput(inputId = "sample_font_col", label = "Sample font color (e.g., black)", value = "black", palette = "limited", returnName = TRUE),
    #   colourInput(inputId = "cor_font_col", label = "Cor font color (e.g., blue)", value = "blue", palette = "limited", returnName = TRUE)
    # ),
    
    conditionalPanel(
      condition = "input.style == 'Simac'",
      fluidRow(
        column(
          width = 6,
          sliderInput(inputId = "samic_axis_lab", label = "Size of aixs-label：", min = 0, max = 5, value = 1.4, step = 0.2)
        ),
        column(
          width = 6,
          sliderInput(inputId = "samic_main", label = "Size of main title：", min = 0, max = 5, value = 2, step = 0.2)
        ),
        column(
          width = 6,
          sliderInput(inputId = "samic_point_size", label = "Point size：", min = 0, max = 5, value = 3, step = 0.2)
        ),
        column(
          width = 6,
          sliderInput(inputId = "samic_legend", label = "Size of legend：", min = 0, max = 3, value = 1.2, step = 0.2)
        ),
        column(
          width = 12,
          selectInput(inputId = "samic_point_shape", label = "Point shape: ", width = "48%", 
                      list(
                        "Suggest" = list("Default", "Random"),
                        "Specific" = c(0:24)
                      ))
        ),
        column(
          width = 12,
          selectInput(inputId = "samic_point_text", label = "Point sign", width = "48%", choices = c(TRUE, FALSE), selected = FALSE)
        ),
        conditionalPanel(
          condition = "input.samic_point_text == 'TRUE'",
          column(
            width = 6,
            sliderInput(inputId = "samic_ponit_text_size", label = "Size of sign：", min = 0, max = 3, value = 1.0, step = 0.2)
          )
        )
      )
    )
  ),
  
  mainPanel(
    column(
      width = 10, offset = 2,
      
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
      
      plotOutput("plot", height = "600px")
      # uiOutput(outputId = "outplot")
    )
  ),
  
  bsModal(
    id = "readme", title = "Use Mannul", trigger = "tabBut", size = "large",
    tags$h2("Please read me !!!", style = "color:#47a3da; font-family: sans-serif"),
    br(),
    p("相关性散点图所需表达谱示例文件如下(须有表头的)："),
    DTOutput("example_input"),
    br(),
    p("样本信息示例文件如下(可选文件：总共两列，第一列组别名，第二列样本名；PS.不需要表头V1/V2以及列名)："),
    DTOutput("example_sample_input"),
    hr(),
    p("选择好参数后，点击Submit! Go!运行工具，出具图片；每次修改后均需要点击Submit按钮才能更新参数"),
    p("点击Download Plot即可下载图片")
  )
)

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$exprlist
    req(infile$datapath)
    data <- read.table(file = infile$datapath, sep = "\t", header = T, stringsAsFactors = F, quote = "", check.names = F)
    if (length(unique(data[,1])) != length(data[,1])){
      res <- data[data[,1] != "",]
      res <- res[!duplicated(res[,1]),]
    }else{
      res <- data
    }
    row.names(res) <- res[,1]
    res <- res[,-1]
    
    return(res)
  })
  
  groupdata <- reactive({
    infile <- input$grouplist
    data <- filedata()
    if (is.null(infile)){
      res <- data.frame(sample = colnames(data), group = str_remove(colnames(data), pattern = "-\\d+$"), stringsAsFactors = F)
      res$group <- factor(res$group)
    }else{
      samplefile <- read.table(file = infile$datapath, sep = "\t", header = F, stringsAsFactors = F)
      names(samplefile) <- c("group", "sample")
      res <- data.frame(sample = colnames(data), stringsAsFactors = F)
      res <- left_join(res, samplefile, by = "sample")
      # res <- res[,c("sample", "group")]
      res$group <- factor(res$group)
    }
    
    return(res)
  })
  
  observe({
    print(groupdata())
  })
  
  normalized_data <- eventReactive(input$button, {
    data <- filedata()
    
    col_sum <- apply(data, 2, sum, na.rm = TRUE) 
    res <- apply(data, 1, function(x){
      x / col_sum * 1000
    })
    
    return(res)
  })
  
  plot_simac <- eventReactive(input$button, {
    colpalette <- c("#1d953f","#102b6a","#c77eb5", "#fcf16e", "#2585a6", "purple", "#e0861a", "#d71345", "#6b473c", "#78a355", "#fdb933", "#5e7c85", "#411445", "#c37e00", "#bed742","#009ad6","#9d9087")
    colpalette1 <- c("#1d953f","#102b6a")
    
    colorss <- as.numeric(groupdata()[,2])
    uniq.cols <- unique(colpalette[colorss])
    
    xname = "t[1]"
    yname = "t[2]"
    
    kfold <- function(x){
      if ( nrow(x) >= 7){
        nn = 7
      }else{
        nn <- nrow(x)
      }
      return(nn)
    }
    
    pca_data <- opls(
      x = normalized_data(), 
      predI = NA,     # 选择预测成分数predI，PCA and PLS(-DA)默认NA自动选择； OPLS(-DA)，通常为1 
      orthoI = 0,     # 正交主成分数，PCA and PLS(-DA)默认为0，其他为OPLS(-DA)，仅限OPLS(-DA)设置为NA自动选择(最多九个)
      testL = FALSE, scaleC= "pareto", plotL = FALSE, crossvalI = kfold(normalized_data())
    )
    
    if (pca_data@typeC == "OPLS-DA"){
      ploMN <- data.frame(p1 = getScoreMN(pca_data),o1 = pca_data@orthoScoreMN[,1])
    }else{
      ploMN <- getScoreMN(pca_data) 
    }
    
    xy <- as.data.frame(dataEllipse(ploMN[,1], ploMN[,2], levels = 0.95, draw = FALSE))
    
    par(
      mar=c(6, 6, 6, 3)+0.1,
      font.lab = 2, font.axis = 1, 
      cex.main = input$samic_main, 
      cex.axis = input$samic_axis_lab, 
      cex.lab = input$samic_axis_lab,
      las=2#, xpd=TRUE
    )
    
    if (input$samic_point_shape == "Default"){
      if (length(unique(as.numeric(groupdata()[,2]))) >6){
        pch_value <- as.numeric(groupdata()[,2])
      }else{
        pch_value <- as.numeric(groupdata()[,2]) + 14
      }
    }else if (input$samic_point_shape == "Random"){
      pch_value <- sample(x = 0:24, size = length(unique(groupdata()[,2])))[as.numeric(groupdata()[,2])]
    }else{
      pch_value <- as.numeric(input$samic_point_shape)
    }
    
    plot(
      ploMN, main=paste0("Scores (",pca_data@typeC,")",sep = ""),
      xlab = xname, ylab = yname,
      xlim = c(-1, 1) * max(1.05 * max(xy$x), max(abs(ploMN[, 1]))),
      ylim =c(-1, 1) *max(1.05 * max(xy$y), max(abs(ploMN[, 2]))),
      col = colpalette[colorss],
      pch = pch_value,
      cex = input$samic_point_size
    )   
    
    abline(v = 0)
    abline(h = 0)
    
    lines(xy$x,xy$y)
    legend.nm <- unique(as.character(groupdata()[,2])) 
    if ( length(uniq.cols) > 1 ) {
      names(uniq.cols) <- legend.nm;
    }
    legend("topright", legend = legend.nm, col = uniq.cols, pch = unique(pch_value), cex = input$samic_legend)
    
    if (input$samic_point_text == TRUE){
      text(ploMN, labels = groupdata()[,1], pos = 3, offset = 0.75, cex = input$samic_ponit_text_size)
    }
  })
  
  output$plot <- renderPlot({
    plot_simac()
  })
  
  output$outplot <- renderUI({
    plotOutput("plot", height = paste0(input$plotheight, "cm"), width = paste0(input$plotwidth, "cm"))
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("PCA", ".png")
    },
    contentType = "image/png",
    content = function(file){
      png(filename = file, width = input$plotwidth, height = input$plotheight, units = "cm", res = input$plotres)
      
      # pairs(normalized_data(), upper.panel = panel.cor, lower.panel = panel.cor, gap = 0.5, bg = "light blue", text.panel = panel.text)
      
      # plot_graph()
      colpalette <- c("#1d953f","#102b6a","#c77eb5", "#fcf16e", "#2585a6", "purple", "#e0861a", "#d71345", "#6b473c", "#78a355", "#fdb933", "#5e7c85", "#411445", "#c37e00", "#bed742","#009ad6","#9d9087")
      colpalette1 <- c("#1d953f","#102b6a")
      
      colorss <- as.numeric(groupdata()[,2])
      uniq.cols <- unique(colpalette[colorss])
      
      xname = "t[1]"
      yname = "t[2]"
      
      kfold <- function(x){
        if ( nrow(x) >= 7){
          nn = 7
        }else{
          nn <- nrow(x)
        }
        return(nn)
      }
      
      pca_data <- opls(
        x = normalized_data(), 
        predI = NA,     # 选择预测成分数predI，PCA and PLS(-DA)默认NA自动选择； OPLS(-DA)，通常为1 
        orthoI = 0,     # 正交主成分数，PCA and PLS(-DA)默认为0，其他为OPLS(-DA)，仅限OPLS(-DA)设置为NA自动选择(最多九个)
        testL = FALSE, scaleC= "pareto", plotL = FALSE, crossvalI = kfold(normalized_data())
      )
      
      if (pca_data@typeC == "OPLS-DA"){
        ploMN <- data.frame(p1 = getScoreMN(pca_data),o1 = pca_data@orthoScoreMN[,1])
      }else{
        ploMN <- getScoreMN(pca_data) 
      }
      
      xy <- as.data.frame(dataEllipse(ploMN[,1], ploMN[,2], levels = 0.95, draw = FALSE, segments = 500))
      
      par(
        mar=c(6, 6, 6, 3)+0.1,
        font.lab = 2, font.axis = 1, 
        cex.main = input$samic_main, 
        cex.axis = input$samic_axis_lab, 
        cex.lab = input$samic_axis_lab,
        las=2#, xpd=TRUE
      )
      
      if (input$samic_point_shape == "Default"){
        if (length(unique(as.numeric(groupdata()[,2]))) >6){
          pch_value <- as.numeric(groupdata()[,2])
        }else{
          pch_value <- as.numeric(groupdata()[,2]) + 14
        }
      }else if (input$samic_point_shape == "Random"){
        pch_value <- sample(x = 0:24, size = length(unique(groupdata()[,2])))[as.numeric(groupdata()[,2])]
      }else{
        pch_value <- as.numeric(input$samic_point_shape)
      }
      
      plot(
        ploMN, main=paste0("Scores (",pca_data@typeC,")",sep = ""),
        xlab = xname, ylab = yname,
        xlim = c(-1, 1) * max(1.05 * max(xy$x), max(abs(ploMN[, 1]))),
        ylim =c(-1, 1) *max(1.05 * max(xy$y), max(abs(ploMN[, 2]))),
        col = colpalette[colorss],
        pch = pch_value,
        cex = input$samic_point_size
      )   
      
      abline(v = 0)
      abline(h = 0)
      
      lines(xy$x,xy$y)
      legend.nm <- unique(as.character(groupdata()[,2])) 
      if ( length(uniq.cols) > 1 ) {
        names(uniq.cols) <- legend.nm;
      }
      legend("topright", legend = legend.nm, col = uniq.cols, pch = unique(pch_value), cex = input$samic_legend)
      
      if (input$samic_point_text == TRUE){
        text(ploMN, labels = groupdata()[,1], pos = 3, offset = 0.75, cex = input$samic_ponit_text_size)
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
  example_trans_file <- reactive({
    read.table(file = "example_sample.txt", sep = "\t", header = F, stringsAsFactors = F)
  })
  observeEvent(input$tabBut, {
    output$example_sample_input <- renderDT(example_trans_file(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
  })
  
}

shinyApp(ui, server)