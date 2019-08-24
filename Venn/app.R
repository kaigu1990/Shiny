library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyjs)
library(VennDiagram)
library(DT)

ui <- fluidPage(
  # theme = shinytheme("spacelab"),
  shinyjs::useShinyjs(),
  titlePanel("Venn Analysis Module"),
  
  br(),
  
  bsCollapse(id = "collapse", open = "Venn Analysis Introductions",
             bsCollapsePanel(title = "Venn Analysis Introductions", 
                             "This tools is used to Creates a Venn diagram with 1-4 sets, and determine the groupings of values as they would be presented in the venn diagram !",
                             style = "default")
  ),
  
  
  
  column(
    width = 4,
    
    tags$div(
      id = "content",
      fluidRow(
        column(
          width = 6,
          fluidRow(
            tags$style(type='text/css', '#value1 {background-color: white; color: green; height: 35px; overflow: visible; text-align:left; padding: 8px}'), 
            column(
              width = 6,
              textInput(inputId = "name1", label = NULL, width = "100%", placeholder = "Sample 1")
            ),
            column(
              width = 6,
              verbatimTextOutput("value1")
            )
          ),
          textAreaInput(inputId = "caption1", label = NULL, height = "150px", width = "100%")
        ),
        column(
          width = 6,
          fluidRow(
            tags$style(type='text/css', '#value2 {background-color: white; color: green; height: 35px; overflow: visible; text-align:left; padding: 8px}'),
            column(
              width = 6,
              textInput(inputId = "name2", label = NULL, width = "100%", placeholder = "Sample 2")
            ),
            column(
              width = 6,
              verbatimTextOutput("value2")
            )
          ),
          textAreaInput(inputId = "caption2", label = NULL, height = "150px", width = "100%")
        ),
        column(
          width = 6,
          fluidRow(
            tags$style(type='text/css', '#value3 {background-color: white; color: green; height: 35px; overflow: visible; text-align:left; padding: 8px}'),
            column(
              width = 6,
              textInput(inputId = "name3", label = NULL, width = "100%", placeholder = "Sample 3")
            ),
            column(
              width = 6,
              verbatimTextOutput("value3")
            )
          ),
          textAreaInput(inputId = "caption3", label = NULL, height = "150px", width = "100%")
        ),
        column(
          width = 6,
          fluidRow(
            tags$style(type='text/css', '#value4 {background-color: white; color: green; height: 35px; overflow: visible; text-align:left; padding: 8px}'),
            column(
              width = 6,
              textInput(inputId = "name4", label = NULL, width = "100%", placeholder = "Sample 4")
            ),
            column(
              width = 6,
              verbatimTextOutput("value4")
            )
          ),
          textAreaInput(inputId = "caption4", label = NULL, height = "150px", width = "100%")
        )
      )
    ),
    
    column(
      width = 12,
      fluidRow(
        hr(),
        
        actionButton(inputId = "reset", label = "Reset", icon = icon("refresh"), style = "margin-bottom:20px"),
        br(),
        
        column(
          width = 6,
          sliderInput(inputId = "cat_cex", label = "Size for each category name:", min = 1, max = 2, value = 1.2, step = 0.1)
        ),
        
        column(
          width = 6,
          sliderInput(inputId = "label_cex", label = "Size for each area label:", min = 1, max = 2, value = 1.5, step = 0.1)
        ),
        
        selectInput(inputId = "scaled", label = "Scaled circle size", choices = c(FALSE, TRUE), width = "30%"),
        
        downloadButton(outputId = "download_plot", label = "Download Venn Plot", icon = icon("download"), style = "margin-right:20px"),
        downloadButton(outputId = "download_xls", label = "Download Venn result", icon = icon("download"))
      )
    )
  ),
  
  column(
    width = 5, offset = 1,
    plotOutput("plot", height = 600)
  )
  
  
)

server <- function(input, output, session) {
  observe({
    x1 <- length(unique(strsplit(input$caption1, "\n")[[1]]))
    output$value1 <- renderText({paste("Number : ", x1)})
    x2 <- length(unique(strsplit(input$caption2, "\n")[[1]]))
    output$value2 <- renderText({paste("Number : ", x2)})
    x3 <- length(unique(strsplit(input$caption3, "\n")[[1]]))

    output$value3 <- renderText({paste("Number : ", x3)})
    x4 <- length(unique(strsplit(input$caption4, "\n")[[1]]))
    output$value4 <- renderText({paste("Number : ", x4)})
  })
  
  res <- reactive({
    data <- list()
    if (input$caption1 != ""){
      if (input$name1 != ""){
        data[[input$name1]] <- unique(strsplit(input$caption1, "\n")[[1]])
      }else{
        data[["Sample1"]] <- unique(strsplit(input$caption1, "\n")[[1]])
      }
    }
    
    if (input$caption2 != ""){
      if (input$name2 != ""){
        data[[input$name2]] <- unique(strsplit(input$caption2, "\n")[[1]])
      }else{
        data[["Sample2"]] <- unique(strsplit(input$caption2, "\n")[[1]])
      }
    }
    
    if (input$caption3 != ""){
      if (input$name3 != ""){
        data[[input$name3]] <- unique(strsplit(input$caption3, "\n")[[1]])
      }else{
        data[["Sample3"]] <- unique(strsplit(input$caption3, "\n")[[1]])
      }
    }
    
    if (input$caption4 != ""){
      if (input$name4 != ""){
        data[[input$name4]] <- unique(strsplit(input$caption4, "\n")[[1]])
      }else{
        data[["Sample4"]] <- unique(strsplit(input$caption4, "\n")[[1]])
      }
    }
    data
  })
  
  plot_fun <- reactive({
    if (length(res()) == 2){
      p <- venn.diagram(res(), filename = NULL,
                        fill = c("cornflowerblue", "yellow2"),
                        resolution = 600, cex = input$label_cex,
                        fontfamily = "serif",
                        cat.col = c("cornflowerblue", "yellow2"),
                        cat.fontface = "bold",
                        # height = 5000,
                        # width = 5000,
                        # col = "transparent",
                        cat.pos = c(5, -5),
                        cat.dist = c(0.06, 0.06),
                        alpha = 0.5,
                        scaled = as.logical(input$scaled),
                        cat.cex = input$cat_cex)
    }else if (length(res()) == 3){
      p <- venn.diagram(res(), filename = NULL,
                        fill = c("red", "cornflowerblue", "chartreuse3"),
                        resolution = 600, cex = input$label_cex,
                        fontfamily = "serif",
                        cat.col = c("red", "cornflowerblue", "chartreuse3"),
                        cat.fontface = "bold",
                        # height = 5000,
                        # width = 5000,
                        # col = "transparent",
                        cat.pos = c(0, 0, 180),
                        cat.dist = c(0.05, 0.05, 0.05),
                        cat.default.pos = "text",
                        alpha = 0.5,
                        scaled = as.logical(input$scaled),
                        cat.cex = input$cat_cex)
    }else if (length(res()) == 4){
      p <- venn.diagram(res(), filename = NULL,
                        fill = c("cornflowerblue", "green", "yellow2", "darkorchid1"),
                        resolution = 600, cex = input$label_cex,
                        fontfamily = "serif",
                        cat.col = c("cornflowerblue", "green", "yellow2", "darkorchid1"),
                        cat.fontface = "bold",
                        # height = 5000,
                        # width = 5000,
                        # col = "transparent",
                        cat.dist = c(0.34, 0.34, 0.12, 0.12),
                        cat.pos = c(180, 180, -10, 10),
                        alpha = 0.5,
                        scaled = F,
                        cat.cex = input$cat_cex)
    }
  })
  
  res_xls <- reactive({
    res_overlap <- c()
    if (length(res()) > 1){
      overlap <- calculate.overlap(res())
    }
    
    sample1 <- ifelse(input$name1 != "", input$name1, "Sample1")
    sample2 <- ifelse(input$name2 != "", input$name2, "Sample2")
    sample3 <- ifelse(input$name3 != "", input$name3, "Sample3")
    sample4 <- ifelse(input$name4 != "", input$name4, "Sample4")
    
    if (length(res()) == 4){
      res_overlap <- c(res_overlap, paste(length(overlap$a9), "elements included exclusively in", sample1, ":", sep = " "), overlap$a9, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a14), "elements included exclusively in", sample2, ":", sep = " "), overlap$a14, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a1), "elements included exclusively in", sample3, ":", sep = " "), overlap$a1, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a3), "elements included exclusively in", sample4, ":", sep = " "), overlap$a3, "")
      
      res_overlap <- c(res_overlap, paste(length(overlap$a15), "common elements in", sample1, "and", sample2, ":", sep = " "), overlap$a15, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a4), "common elements in", sample1, "and", sample3, ":", sep = " "), overlap$a4, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a10), "common elements in", sample1, "and", sample4, ":", sep = " "), overlap$a10, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a13), "common elements in", sample2, "and", sample3, ":", sep = " "), overlap$a13, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a8), "common elements in", sample2, "and", sample4, ":", sep = " "), overlap$a8, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a2), "common elements in", sample3, "and", sample4, ":", sep = " "), overlap$a2, "")
      
      res_overlap <- c(res_overlap, paste(length(overlap$a12), "common elements in", sample1, sample2, "and", sample3, ":", sep = " "), overlap$a12, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a11), "common elements in", sample1, sample2, "and", sample4, ":", sep = " "), overlap$a11, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a5), "common elements in", sample1, sample3, "and", sample4, ":", sep = " "), overlap$a5, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a7), "common elements in", sample2, sample3, "and", sample4, ":", sep = " "), overlap$a7, "")
      
      res_overlap <- c(res_overlap, paste(length(overlap$a6), "common elements in", sample1, sample3, sample4, "and", sample2, ":", sep = " "), overlap$a6, "")
      
    }else if (length(res()) == 3){
      res_overlap <- c(res_overlap, paste(length(overlap$a1), "elements included exclusively in", sample1, ":", sep = " "), overlap$a1, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a3), "elements included exclusively in", sample2, ":", sep = " "), overlap$a3, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a7), "elements included exclusively in", sample3, ":", sep = " "), overlap$a7, "")
      
      res_overlap <- c(res_overlap, paste(length(overlap$a2), "common elements in", sample1, "and", sample2, ":", sep = " "), overlap$a2, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a4), "common elements in", sample1, "and", sample3, ":", sep = " "), overlap$a4, "")
      res_overlap <- c(res_overlap, paste(length(overlap$a6), "common elements in", sample2, "and", sample3, ":", sep = " "), overlap$a6, "")
      
      res_overlap <- c(res_overlap, paste(length(overlap$a5), "common elements in", sample1, sample2, "and", sample3, ":", sep = " "), overlap$a5, "")
      
    }else if (length(res()) == 2){
      res_overlap <- c(res_overlap, paste(length(overlap$a1) - length(overlap$a3), "elements included exclusively in", sample1, ":", sep = " "), overlap$a1[!overlap$a1 %in% overlap$a3] , "")
      res_overlap <- c(res_overlap, paste(length(overlap$a2) - length(overlap$a3), "elements included exclusively in", sample2, ":", sep = " "), overlap$a2[!overlap$a2 %in% overlap$a3], "")
      
      res_overlap <- c(res_overlap, paste(length(overlap$a3), "common elements in", sample1, "and", sample2, ":", sep = " "), overlap$a3, "")
      
    }
    res_overlap
  })
  
  
  output$plot <- renderPlot({
    if (length(res()) > 1){
      grid.draw(plot_fun())
      file.remove(list.files(pattern = "VennDiagram*"))
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("Venn", ".png")
    },
    contentType = "image/png",
    content = function(file){
      png(filename = file, width = 22, height = 20, units = "cm", res = 300)
      grid.draw(plot_fun())
      dev.off()
    }
  )
  
  output$download_xls <- downloadHandler(
    filename = function(){
      "Venn.xls"
    },
    content = function(file){
      write.table(res_xls(), file, sep = "\t", row.names = F, col.names = F, quote = F)
    }
  )
  
  observeEvent(input$reset, {
    shinyjs::reset("content")
  }) 
}  

shinyApp(ui, server)