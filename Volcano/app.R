library(shiny)
library(shinyBS)
library(colourpicker)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(DT)

ui <- fluidPage(
  titlePanel("Volcano Plot Module"),
  fluidRow(
    column(3,
           wellPanel(
             fileInput(inputId = "exprlist", label = "Protein File Input:", accept = c(".txt")),
             selectInput(inputId = "style", label = "Choose style of this Volcano Plot",
                         choices = c("Normal", "Advanced")
             ),
             actionButton(inputId = "button", label = "Submit! Go!", icon = icon("refresh"), style = "margin-right:20px"),
             br(),
             downloadButton(outputId = "download_plot", label = "Download VolcanoPlot"),
             br(),
             br(),
             actionButton("tabBut", "User Manual")
           ),
           
           wellPanel(
             h4("More Optional Parameters"),
             br(),
             # textInput(inputId = "upcolor", label = "Up-regulated corlor (e.g., red)", value = "red", placeholder = "For example: red or #FF0000"),
             colourInput(inputId = "upcolor", label = "Up-regulated corlor (e.g., red)", value = "red", palette = "limited", returnName = TRUE),
             colourInput(inputId = "downcolor", label = "Down-regulated corlor (e.g., green)", value = "green", palette = "limited", returnName = TRUE),
             colourInput(inputId = "noncolor", label = "Non-regulated corlor (e.g., black)", value = "black", palette = "limited", returnName = TRUE),
             numericInput(inputId = "foldchange", label = "Threshold of foldchage is:", value = 2, min = 0, max = 10, width = "60%"),
             numericInput(inputId = "padj", label = "Threshold of p-value is:", value = 0.05, min = 0, max = 1, step = 0.01, width = "60%"),
             sliderInput(inputId = "shape", label = "Size of point is:", value = 1.6, min = 0, max = 5, step = 0.2, width = "80%"),
             sliderInput(inputId = "shape_style", label = "specific ponit shape：", min = 1, max = 25, value = 13, step = 1, width = "80%"),
             sliderInput(inputId = "font_size", label = "Size of font is:", min = 1, max = 30, value = 13, step = 1, width = "80%"),
             # radioButtons(inputId = "title", label = "Show title(Group information)", choices = c(TRUE, FALSE), selected = FALSE),
             textInput(inputId = "title", label = "Show title(Group information)", width = "60%", placeholder = "For example A vs B"),
             sliderInput(inputId = "title_font_size", label = "Size of title font is:", min = 1, max = 30, value = 15, step = 1, width = "80%"),
             radioButtons(inputId = "x_axis", label = "Symmetry axis", choices = c(TRUE, FALSE), selected = FALSE),
             selectInput(inputId = "legend", label = "Legend Position", choices = c("None", "right", "top", "left", "bottom"), selected = "right", width = "60%")
           )
    ),
    column(
      width = 6,
      plotOutput("plot", height = 500)
    )
  ),
  
  bsModal(
    id = "readme", title = "Use Mannul", trigger = "tabBut", size = "large",
    tags$h2("Please read me !!!", style = "color:#47a3da; font-family: sans-serif"),
    br(),
    p("火山图主输入文件如下（第一列是蛋白ID，第二列是倍数，第三列是Pvalue；主要来源于附件3的各个比较组的差异列表）："),
    DTOutput("example_input"),
    hr(),
    p("如果想标注一些感兴趣的点，可以在style参数上选择Advance模式，然后按提示输入感兴趣的蛋白ID即可（以.txt格式输入，一行一个蛋白）"),
    p("本工具支持可修改的参数有：1.上下调/非差异的颜色（Up-regulated corlor/Down-regulated corlor/Non-regulated corlor），2.倍数（foldchage），3.Pvalue，4.点圈大小（Size of point），5.字体大小（Size of font）"),
    p("选择好参数后，点击Submit! Go!运行工具，出具图片；每次修改后均需要点击Submit按钮才能更新参数"),
    p("点击Download VolcanoPlot即可下载图片")
  )
  
)

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$exprlist
    req(infile$datapath)
    read.table(file = infile$datapath, sep = "\t", header = T, stringsAsFactors = F, quote = "", fill = T)
  })
  
  prodata <- reactive({
    infile <- input$prolist
    if (is.null(infile)){
      return(NULL)
    }
    read.table(file = infile$datapath, sep = "\t", header = F, stringsAsFactors = F)
  })
  
  example_file <- reactive({
    read.table(file = "example_expr.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
  })
  
  #============= Function ===============#
  
  plot_fun <- eventReactive(input$button, {
    df <- filedata()
    names(df) <- c("geneid", "foldchage", "padj")
    df$change <- as.factor(ifelse(df$padj < input$padj & abs(log2(df$foldchage)) > log2(input$foldchange),
                                  ifelse(df$foldchage > input$foldchange,'UP','DOWN'),'NOT'))

    if (input$style == "Normal"){
      p <- ggplot(data = df, aes(x = log2(foldchage), y = -log10(padj), color = change)) +
        geom_point(alpha=0.8, size = input$shape, position = "jitter", shape = input$shape_style) +
        labs(x = expression(log[2](FoldChange)), y = expression(-log[10](padj))) +
        theme_classic() +
        # theme_bw(base_size = 15) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x = element_text(size = input$font_size,margin = margin(c(10,0,0,0))),
          axis.title.y = element_text(size = input$font_size,margin = margin(c(0,10,0,0))),
          plot.margin = unit(c(1,2,2,2),"lines"),
          legend.position = input$legend,
          legend.margin = margin(c(0,0,0,15)),
          legend.key.height = unit(0.8,"cm"),
          legend.key.width = unit(0.6,"cm"),
          legend.key = element_rect(colour = "black", size = 0.5)
        ) +
        scale_color_manual(name = "", values = c(input$upcolor, input$downcolor, input$noncolor), limits = c("UP", "DOWN", "NOT")) +
        geom_hline(yintercept = -log10(input$padj), linetype = "dashed", color = "grey", size = 1) +
        geom_vline(xintercept = -log2(input$foldchange), linetype = "dashed", color = "grey", size = 1) +
        geom_vline(xintercept = log2(input$foldchange), linetype = "dashed", color = "grey", size = 1)
      
      if (input$title != ""){
        p <- p + ggtitle(input$title) +
          theme(plot.title = element_text(size = input$title_font_size, hjust = 0.5))
      }
      
      if (input$x_axis){
        p <- p + xlim(-max(log2(df$foldchage))*1.05, max(log2(df$foldchage))*1.05)
      }

    }else if (input$style == "Advanced" && !is.null(prodata())){
      proid <- prodata()
      names(proid) <- "pro_id"

      df$sign <- ifelse(df$geneid %in% proid$pro_id, df$geneid, NA)

      p <- ggplot(data = df, aes(x = log2(foldchage), y = -log10(padj), color = change)) +
        geom_point(alpha=0.8, size = input$shape, position = "jitter", shape = input$shape_style) +
        labs(x = expression(log[2](FoldChange)), y = expression(-log[10](Pvalue))) +
        theme_classic() +
        # theme_bw(base_size = 15) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x = element_text(size = input$font_size,margin = margin(c(10,0,0,0))),
          axis.title.y = element_text(size = input$font_size,margin = margin(c(0,10,0,0))),
          plot.margin = unit(c(1,2,2,2),"lines"),
          legend.position = input$legend,
          legend.margin = margin(c(0,0,0,15)),
          legend.key.height = unit(0.8,"cm"),
          legend.key.width = unit(0.6,"cm"),
          legend.key = element_rect(colour = "black", size = 0.5)
        ) +
        scale_color_manual(name = "", values = c(input$upcolor, input$downcolor, input$noncolor), limits = c("UP", "DOWN", "NOT")) +
        geom_text_repel(aes(label = sign), box.padding = unit(0.3, "lines"), point.padding = unit(0.3, "lines"), show.legend = F, size = 3) +
        geom_hline(yintercept = -log10(input$padj), linetype = "dashed", color = "grey", size = 1) +
        geom_vline(xintercept = -log2(input$foldchange), linetype = "dashed", color = "grey", size = 1) +
        geom_vline(xintercept = log2(input$foldchange), linetype = "dashed", color = "grey", size = 1)
      
      if (input$title != ""){
        p <- p + ggtitle(input$title) +
          theme(plot.title = element_text(size = input$title_font_size, hjust = 0.5))
      }
      
      if (input$x_axis){
        p <- p + xlim(-max(log2(df$foldchage))*1.05, max(log2(df$foldchage))*1.05)
      }
    }
    
    p
  })

  
  
  observeEvent(input$tabBut, {
    output$example_input <- renderDT(example_file(), rownames = FALSE, options = list(dom = 't'))
  })
  
  output$plot <- renderPlot({
    plot_fun()
  })
  
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("VolcanoPlot", ".png")
    },
    contentType = "image/png",
    content = function(file){
      p <- plot_fun()
      ggsave(filename = file, plot = p, device = "png", width = 22, height = 18, units = "cm", dpi = 300)
    }
  )

  
  observe(
    if (input$style == "Advanced"){
      showModal(
        modalDialog(
          h4("Specify Parameters for Advanced Style:"),
          fileInput(inputId = "prolist", label = "Protein-list Input:", accept = c(".txt"))
        )
      )
    }
  )
}

shinyApp(ui, server)
