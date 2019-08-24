library(shiny)
library(FactoMineR)
library(factoextra)
library(shinyjs)
library(shinyalert)
library(dplyr)
library(DT)
library(shinyBS)
library(stringr)

options(shiny.maxRequestSize=500*1024^2)

ui <- fluidPage(
  useShinyalert(),
  shinyjs::useShinyjs(),
  
  headerPanel("PCA Analysis Module"),
  # tags$title(tags$style("font-family: 'Source Sans Pro';color: #F00")),
  sidebarPanel(
    width = 3,
    fileInput(inputId = "exprlist", label = "Protein File Input:", accept = c(".txt"), width = "70%"),
    fileInput(inputId = "grouplist", label = paste0("Group File Input: ", "(样本名称以xx-1形式结尾，则无需输入)"), accept = c(".txt"), width = "70%"),
    
    radioButtons(inputId = "type", label = "iTRAQ or Label-free:", choices = c("iTRAQ", "Label-free"), selected = "iTRAQ"),
    radioButtons(inputId = "shape", label = "Circle shape", choices = c("default", "specific")),
    conditionalPanel(
      condition = "input.shape == 'specific'",
      sliderInput(inputId = "shape_type", label = "specific shape：", min = 1, max = 25, value = 21, step = 1, width = "60%")
    ),
    
    radioButtons(inputId = "addellipses", label = "Draw ellipes", choices = c(TRUE, FALSE), selected = TRUE),
    conditionalPanel(
      condition = "input.addellipses == 'TRUE'",
      radioButtons(inputId = "ellipsetype", label = "Type of ellipes", choices = c("confidence", "convex", "t", "norm", "euclid"))
    ),
    
    sliderInput(inputId = "point_size", label = "Size of shape：", min = 1, max = 10, value = 3, step = 1, width = "60%"),
    sliderInput(inputId = "label_size", label = "Size of label：", min = 0, max = 15, value = 6, step = 1, width = "60%"),

    actionButton(inputId = "button", label = "Submit! Go!", icon = icon("refresh")),
    br(),
    br(),
    downloadButton(outputId = "download_plot", label = "Download PCA Plot"),
    br(),
    br(),
    actionButton("tabBut", "User Manual")
  ),
  
  mainPanel(
    column(
      width = 10, offset = 2,
      plotOutput("plot", height = "500px")
    )
  ),
  
  bsModal(
    id = "readme", title = "Use Mannul", trigger = "tabBut", size = "large",
    tags$h2("Please read me !!!", style = "color:#47a3da; font-family: sans-serif"),
    br(),
    p("PCA表达谱示例文件如下(必须要有表头的)："),
    DTOutput("example_pro_input"),
    br(),
    p("样本信息示例文件如下(可选文件：总共两列，第一列组别名，第二列样本名；PS.不需要表头V1/V2以及列名)："),
    DTOutput("example_sample_input"),
    hr(),
    p("本工具支持可修改的参数有：1. 点形状(Circle shape)，2. 椭圆形状(Type of ellipes)，3. 点和标记文字大小"),
    p("选择好参数后，点击Submit! Go!运行工具，出具图片；每次修改后均需要点击Submit按钮才能更新参数"),
    p("点击load PCA Plot即可下载图片")
  )
)

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$exprlist
    req(infile$datapath)
    data <- read.table(file = infile$datapath, sep = "\t", header = T, stringsAsFactors = F, quote = "", check.names = F, row.names = 1)
    
    return(data)
  })
  
  groupdata <- reactive({
    infile <- input$grouplist
    if (is.null(infile)){
      return(NULL)
    }else{
      # req(infile$datapath)
      data <- read.table(file = infile$datapath, sep = "\t", header = F, stringsAsFactors = F)
      names(data) <- c("group", "sample")
    }
    return(data)
  })
  
  res_data <- reactive({
    data <- filedata()
    if (input$type == "Label-free"){
      data <- log2(data + 1)
    }
    data <- na.omit(data)
    data_t <- data.frame(t(data))
    
    samplefile <- groupdata()
    if (is.null(samplefile)){
      data_t <- data.frame(data_t, group = str_remove(row.names(data_t), pattern = "-\\d+$"), stringsAsFactors = F)
    }else{
      data_t$sample <- row.names(data_t)
      data_t <- left_join(data_t, samplefile, by = "sample")
      data_t$group <- factor(data_t$group)
      rownames(data_t) <- data_t$sample
    }
    
    return(data_t)
  })
  
  plot_fun <- eventReactive(input$button, {
    data <- res_data()
    # if (is.null(groupdata())){
    #   habillage_info <- "none"
    # }else{
    #   habillage_info <- data$group
    # }
    
    habillage_info <- factor(data$group)
    
    
    var_len <- nrow(na.omit(filedata()))
    res.pca <- PCA(res_data()[,1:var_len], graph = FALSE)
    
    if (input$shape == "default"){
      p <- fviz_pca_ind(
        res.pca,
        title = "Principal Component Analysis",
        habillage = habillage_info,
        mean.point = FALSE,
        invisible = "quali",
        show.legend.text = FALSE,
        ellipse.type = input$ellipsetype,
        pointsize = input$point_size,
        labelsize = input$label_size,
        addEllipses = input$addellipses,
        repel = TRUE,
        palette = "jco",
        legend.title = "Groups"
      )
    }else{
      p <- fviz_pca_ind(
        res.pca,
        pointshape = input$shape_type,
        title = "Principal Component Analysis",
        habillage = habillage_info,
        mean.point = FALSE,
        invisible = "quali",
        show.legend.text = FALSE,
        ellipse.type = input$ellipsetype,
        pointsize = input$point_size,
        labelsize = input$label_size,
        addEllipses = input$addellipses,
        repel = TRUE,
        palette = "jco",
        legend.title = "Groups"
      )
    }
    
    return(p)
  })
  
  output$plot <- renderPlot({
    plot_fun()
  })

  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("PCA", ".png")
    },
    contentType = "image/png",
    content = function(file){
      p <- plot_fun()
      ggsave(filename = file, plot = p, device = "png", width = 22, height = 15, units = "cm", dpi = 600)
    }
  )
  
  example_pro_file <- reactive({
    read.table(file = "example_expr.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
  })
  observeEvent(input$tabBut, {
    output$example_pro_input <- renderDT(example_pro_file(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
  })
  
  example_trans_file <- reactive({
    read.table(file = "example_sample.txt", sep = "\t", header = F, stringsAsFactors = F)
  })
  observeEvent(input$tabBut, {
    output$example_sample_input <- renderDT(example_trans_file(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
  })
  
}

shinyApp(ui, server)
