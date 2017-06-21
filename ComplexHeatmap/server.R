library(shiny)
library(ComplexHeatmap)
library(circlize)

server <- function(input, output, session){
  
  # file upload
  filedata <- reactive({
    infile <- input$filename
    if (is.null(infile)){
      return(NULL)
    }
    read.csv(infile$datapath,sep = ",", header = T, row.names = 1)
  })
  
  # choose the max and min number in filedata
  observe({
    if (is.null(filedata())){
      return(NULL)
    }else{
      df <- filedata()
      min_value <- round(min(df))
      max_value <- round(max(df))
      updateNumericInput(session, "lowvalue", value = min_value)
      updateNumericInput(session, "highvalue", value = max_value)
    }
  })
  
  # choose the fit width and height for heatmap
  observe({
    if (input$downlist == "pdf"){
      updateNumericInput(session, "simpleheigh", value = 30)
      updateNumericInput(session, "simplewidth", value = 6)
    }else{
      updateNumericInput(session, "simpleheigh", value = 3000)
      updateNumericInput(session, "simplewidth", value = 4000)
    }
  })

  observe({
    if (input$complextype == "pdf"){
      updateNumericInput(session, "complexheigh", value = 30)
      updateNumericInput(session, "complexwidth", value = 6)
    }else{
      updateNumericInput(session, "complexheigh", value = 3000)
      updateNumericInput(session, "complexwidth", value = 4000)
    }
  })  
  
  
  ##########################################simple heatmap#######################################
  # simple heatmap
  simpleHeatmap <- function(){
    df <- filedata()
    df <- as.data.frame(df)
    plotDataFrame(df,group_names = "Sample")
  }
  
  output$simpleplot <- renderPlot({
    if (!is.null(filedata()))
      simpleHeatmap()
  })
  
  output$genelist <- renderTable({
    if (!is.null(filedata())){
      p <- simpleHeatmap()
      df <- as.data.frame(filedata())
      list <- row.names(df)[row_order(p)[[1]]]
      head(list, 10)
    }
  })
  
  # simple heatmap download
  output$downloadsimple <- downloadHandler(
    filename <- function(){
      paste("cluster", input$downlist, sep = ".")
    },
    
    content <- function(file){
      if (input$downlist == "png")
        png(file)
      else
        pdf(file)
      simpleHeatmap()
      dev.off()
    }
  )
  
  
  
  # gene list download
  output$downloadlist <- downloadHandler(
    filename <- "gene_list.csv",
    content <- function(file){
      if (!is.null(filedata())){
        p <- simpleHeatmap()
        df <- as.data.frame(filedata())
        list <- row.names(df)[row_order(p)[[1]]]
        list <- as.data.frame(list)
        names(list) <- "gene_list"
        write.csv(list, file,row.names = FALSE,quote = FALSE)
      }
    }
  )
  
  ##########################################complex heatmap#######################################
  
  # prepare pre-data
  show_rowname <- reactive({
    rowname <- input$ori_rowname
    if (rowname == "none")
      return("FALSE")
    else
      return("TRUE")
  })
  
  show_columnname <- reactive({
    columnname <- input$ori_columnname
    if (columnname == "none")
      return("FALSE")
    else
      return("TRUE")
  })
  
  row_side <- reactive({
#    rowside <- input$ori_rowname
    if (input$ori_rowname == "none")
      return(NULL)
    else
      input$ori_rowname
  })
  
  column_side <- reactive({
    columnside <- input$ori_columnname
    if (columnside == "none")
      return(NULL)
    else
      input$ori_columnname
  })
  
  # complex heatmap
  complexheatmap <- function(){
    if (!is.na(input$lowvalue) & !is.na(input$highvalue)){
      df <- filedata()
      df <- as.data.frame(df)
      Heatmap(df,
              name = input$nametxt,
              show_heatmap_legend = input$requirename,
              show_row_names = show_rowname(),
              show_column_names = show_columnname(),
              row_names_side = row_side(),
              column_names_side = column_side(),
              col = colorRamp2(c(input$lowvalue, 0, input$highvalue), c(input$lowcol, "white", input$highcol)),
              row_dend_width = unit(input$width_rowdend, "cm"),
              column_dend_height = unit(input$height_columndend, "cm"),
              clustering_distance_rows = input$distance,
              clustering_distance_columns = input$distance,
              clustering_method_rows = input$algorithm
      ) 
    }
  }
  
  output$complexmap <- renderPlot({
    if (!is.null(filedata()))
      complexheatmap()
  })
  
  output$complexgenelist <- renderTable({
    if (!is.null(filedata())){
      if (!is.na(input$lowvalue) & !is.na(input$highvalue)){
        px <- complexheatmap()
        dfx <- as.data.frame(filedata())
        listx <- row.names(dfx)[row_order(px)[[1]]]
        head(listx, 10)
      }
    }
  })
  
  # complex heatmap download
  output$downloadcomplex <- downloadHandler(
    filename <- function(){
      paste("cluster", input$complextype, sep = ".")
    },
    
    content <- function(file){
      if (input$complextype == "png")
        png(file, width = input$complexwidth, height = input$complexheigh, units ="px", res = 300)
      else
        pdf(file, width = input$complexwidth, height = input$complexheigh)
      print(complexheatmap())
      dev.off()
    }

  )  
  
  
  # complex gene list download
  output$downloadcomplexlist <- downloadHandler(
    filename <- "gene_list.csv",
    content <- function(file){
      if (!is.null(filedata())){
        px <- complexheatmap()
        dfx <- as.data.frame(filedata())
        listx <- row.names(dfx)[row_order(px)[[1]]]
        listx <- as.data.frame(listx)
        names(listx) <- "gene_list"
        write.csv(listx, file,row.names = FALSE,quote = FALSE)
      }
    }
  )  
  
}