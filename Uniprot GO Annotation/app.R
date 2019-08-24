library(shiny)
library(DT)
library(shinydashboard)
library(httr)
library(jsonlite)
library(xml2)
library(dplyr)

options(shiny.maxRequestSize=100*1024^2) 

get_url_info <- function(url){
  d <- GET(url, accept("application/json"))
  mydata <- toJSON(content(d))
  mydata <- fromJSON(mydata)
  
  res <- matrix(nrow = nrow(mydata), ncol = 8)
  for(i in 1:nrow(mydata)){
    proteinid <- mydata$accession[[i]]
    proteinname <- ifelse(!is.null(mydata$protein$recommendedName$fullName$value[[i]]), mydata$protein$recommendedName$fullName$value[[i]],
                          ifelse(!is.null(mydata$protein$submittedName[[i]]$fullName$value[[1]]), mydata$protein$submittedName[[i]]$fullName$value[[1]],
                                 ifelse(!is.null(mydata$protein$alternativeName[[i]]$fullName$value[[1]]), mydata$protein$alternativeName[[i]]$fullName$value[[1]],
                                        NA)))
    genename <- ifelse(!is.null(mydata$gene[[i]]$name$value[[1]]), mydata$gene[[i]]$name$value[[1]],
                       ifelse(!is.null(mydata$gene[[i]]$orfNames[[1]]$value[[1]]), mydata$gene[[i]]$orfNames[[1]]$value[[1]],
                              NA))
    temp <- data.frame(type = unlist(mydata$dbReferences[[i]]$type), id = unlist(mydata$dbReferences[[i]]$id), stringsAsFactors = F)
    geneid <- temp$id[which(temp$type == "GeneID")][1]
    
    go_term <- unlist(mydata$dbReferences[[i]]$properties$term)
    go_id <- temp$id[which(temp$type == "GO")]
    
    go_P_index <- grepl(pattern = "^P:", go_term)
    if (any(go_P_index)){
      go_P_Term <- unlist(lapply(go_term[go_P_index], function(x){
        strsplit(x, "P:")[[1]][2]
      }))
      go_P <- paste0(go_P_Term, " ", "[", go_id[go_P_index], "]")
    }else{
      go_P <- NA
    }
    
    go_F_index <- grepl(pattern = "^F:", go_term)
    if (any(go_F_index)){
      go_F_Term <- unlist(lapply(go_term[go_F_index], function(x){
        strsplit(x, "F:")[[1]][2]
      }))
      go_F <- paste0(go_F_Term, " ", "[", go_id[go_F_index], "]")
    }else{
      go_F <- NA
    }
    
    go_C_index <- grepl(pattern = "^C:", go_term)
    if (any(go_C_index)){
      go_C_Term <- unlist(lapply(go_term[go_C_index], function(x){
        strsplit(x, "C:")[[1]][2]
      }))
      go_C <- paste0(go_C_Term, " ", "[", go_id[go_C_index], "]")
    }else{
      go_C <- NA
    }
    
    go_all <- na.omit(c(go_P, go_F, go_C))
    if (length(go_all) == 0){
      go_all <- NA
    }
    
    res[i,] <- c(proteinid, proteinname, geneid, genename, paste(go_P, collapse = ";"), paste(go_F, collapse = ";"), paste(go_C, collapse = ";"), paste(go_all, collapse = ";"))
  }
  
  res <- data.frame(res, stringsAsFactors = F)
  names(res) <- c("ProteinID", "ProteinName", "gene_id", "gene_name", "BP", "MF", "CC", "GO_Term")
  
  return(res)
}


ui <- dashboardPage(
  dashboardHeader(title = "Uniprot GO Tool"),
  
  dashboardSidebar(
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50% + 25px);;
             left: calc(50% + 50px);;
             }"
        )
      )
    ),
    
    width = 250,
    
    fileInput(inputId = "proteinlist", label = "Protein List Input:", accept = c(".txt")),
    checkboxGroupInput(inputId = "rowname", label = "Choose Annotation columns", 
                       choiceNames = c("ProteinName", "GeneID", "GeneName", "BP", "MF", "CC", "GO_Term"), 
                       selected = c("ProteinName", "gene_id", "gene_name", "BP", "MF", "CC", "GO_Term"),
                       choiceValues = c("ProteinName", "gene_id", "gene_name", "BP", "MF", "CC", "GO_Term")),
   
    actionButton(inputId = "action", label = "Search", icon = icon("refresh"), style = "margin-bottom: 25px; margin-top: 15px;"),

    sidebarMenu(
      id = "tabs",
      menuItem("Annotation", tabName = "anno", icon = icon("table")),
      menuItem("README", tabName = "readme", icon = icon("warning"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "anno",
        box(
          width = NULL,
          status = "success",
          title = "Uniprot Annotation result",
          collapsible = T, collapsed = F,
          downloadButton("downloadresult", label = "Download Report"),
          hr(),
          DTOutput("annotation")
        )
      ),
      tabItem(
        tabName = "readme",
        h3("使用须知："),
        br(),
        
        p("1. Protein List Input：输入需要检测的蛋白list文件，格式为txt"),
        p("2. 可根据'Which rowname to show'选项选择输出列"),
        p("3. 点击右边Download按钮下载报告或者表格上方Excel/CSV/PDF/Copy等按钮")
      )
    )
  )
)


server <- function(input, output, session) {
  pro_list <- reactive({
    infile <- input$proteinlist
    req(infile$datapath)
    data <- read.table(infile$datapath, sep = "\t", header = F, stringsAsFactors = F)
    return(data$V1)
  })
  
  annot <- eventReactive(input$action, {
    id <- pro_list()
    
    res <- data.frame(stringsAsFactors = F)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Begin to process files, Please wait...", value = 0)
    
    n <- seq(1, length(id), by = 100)
    for(i in n){
      sub_id <- id[i:(i+99)]
      sub_id <- na.omit(sub_id)
      sub_id <- paste(sub_id, collapse = ",")
      url <- paste0("https://www.ebi.ac.uk/proteins/api/proteins?", "accession=", sub_id)
      
      subres <- get_url_info(url = url)
      res <- dplyr::bind_rows(res, subres)
      
      progress$inc(1/length(n), detail = "Please wait...")
    }
    
    progress$set(message = "Over...", value = 1)
    
    df <- data.frame(ProteinID = id, stringsAsFactors = F)
    df <- left_join(df, res, by = "ProteinID")
    df[is.na(df)] <- "-"
    df[df == "NA"] <- "-"
    
    return(df)
  })
  
  result <- reactive({
    data <- annot()
    data <- dplyr::select(data, one_of("ProteinID", input$rowname))
    return(data)
  })
  
  observe({
    output$annotation <- renderDT(
      result(),
      server = FALSE,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = c('copy', 'csv', 'excel', 'pdf')
      )
    )
  })
  
  output$downloadresult <- downloadHandler(
    filename = function(){
      "GO.xls"
    },
    content = function(file){
      write.table(result(), file = file, sep = "\t", col.names = T, row.names = F, quote = F, na = "-")
    }
  )
  
}


shinyApp(ui, server)
