library(shiny)
library(shinyjs)
library(shinyalert)

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  title = "APT Tools Web",
  
  tags$head(
    includeCSS(file.path('www', 'style.css'))
  ),

  div(
    id = "header",
    div(
      id = "title",
      "APT Online Tools"
    ),
    div(
      id = "subtitle",
      "APT-小工具导航页"
    ),
    div(
      id = "subtitle",
      "现有常规/个性化的生物信息分析结果可视化、PRM评估以及统计分析等模块，后续待补充。。。"
    )
  ),
  
  fluidRow(
    column(
      width = 2,
      class = "col-settings",
      selectInput(
        inputId = "usertype", 
        label = h4("User Type", #span("*", class = "mandatory_star"), 
           style = "font-size: 1.2em;font-weight: bold;margin-bottom: 0px;margin-top: 15px;margin-left: 5px"), 
        choices = list("username1", "username2", "username3"), width = "65%"
      ),
      textInput(
        inputId = "userid", 
        label = h4("User Name", #span("*", class = "mandatory_star"), 
                   style = "font-size: 1.2em;font-weight: bold;margin-bottom: 0px;margin-top: 5px;margin-left: 5px"), 
        placeholder = "...", width = "65%"
      ),
      textInput(
        inputId = "projectid", 
        label = h4("Project Id", #span("*", class = "mandatory_star"), 
                   style = "font-size: 1.2em;font-weight: bold;margin-bottom: 0px;margin-top: 5px;margin-left: 5px"), 
        placeholder = "For example P20180100001", width = "65%"
      ),
      actionButton(inputId = "submit", label = "Submit", icon = icon("user"), style = "margin-top:20px; margin-right:20px;")
    ),
    
    column(
      width = 10,
      class = "col-tabs",
      tags$div(
        id = "list2",
        tabsetPanel(
          id = "mainnav",
          tabPanel(
            title = "Usage",
            icon = icon("question")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    shinyjs::toggleState(id = "submit", condition = !is.null(input$projectid) && input$projectid != "" && !is.null(input$userid) && input$userid != "")
  })
  
  observeEvent(input$submit, {
    appendTab(
      inputId = "mainnav",
      tabPanel(
        title = "Common", 
        icon = icon("toolbox"),
        fluidRow(
          column(
            width = 3,
            div(
              tags$a(id = "venn_count", tags$img(src="venn.png", width = "100%"), href = "http://192.168.130.252:3838/apt-venn/"),
              tags$h3("Venn Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于Venn（韦恩图）分析"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            style = "margin-left: 30px;",
            width = 3,
            div(
              tags$a(id = "heatmap_count", tags$img(src="heatmap.png", width = "100%"), href = "http://192.168.130.252:3838/apt-heatmap/"),
              tags$h3("Heatmap Plot Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制聚类热图/个性化修改图形参数"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            width = 3,
            style = "margin-left: 30px;",
            div(
              tags$a(id = "volcano_count", tags$img(src="volcano.png", width = "100%"), href = "http://192.168.130.252:3838/apt-volcano/"),
              tags$h3("Volcano Plot Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制常规/个性化火山图"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          )
        ),
        
        fluidRow(
          style = "margin-top: 40px;",
          column(
            width = 3,
            div(
              tags$a(id = "keggenrich_count", tags$img(src="kegg_enrichment.png", width = "100%"), href = "http://192.168.130.252:3838/apt-kegg/"),
              tags$h3("KEGG Enrich Plot Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制常规KEGG富集图以及气泡图"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            width = 3,
            style = "margin-left: 30px;",
            div(
              tags$a(id = "scatter_cor_count", tags$img(src="scatter-cor.png", width = "100%"), href = "http://192.168.130.252:3838/apt-scatter-cor/"),
              tags$h3("Cor Scatterplots Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制相关性矩阵散点图"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            width = 3,
            style = "margin-left: 30px;",
            div(
              tags$a(id = "heatmap_cor_count", tags$img(src="heatmap-cor.png", width = "100%"), href = "http://192.168.130.252:3838/apt-heatmap-cor/"),
              tags$h3("Cor Heatmap Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制相关性矩阵热图"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          )
        ),
        
        fluidRow(
          style = "margin-top: 40px;",
          column(
            width = 3,
            div(
              tags$a(id = "statistic_count", tags$img(src="statistic.png", width = "100%"), href = "http://192.168.130.252:3838/apt-statistic/"),
              tags$h3("Statistic Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于统计分析以及文本处理"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          )
        )
      )
    )
    
    appendTab(
      inputId = "mainnav",
      tabPanel(
        title = "Proteome/Transcriptome",
        icon = icon("dna"),
        fluidRow(
          column(
            width = 3,
            div(
              tags$a(id = "prm_count", tags$img(src="prm.png", width = "100%"), href = "http://192.168.130.252:3838/prm/"),
              tags$h3("PRM Analysis Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于iTRAQ/Labelfree项目PRM蛋白/肽段评估"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            style = "margin-left: 30px;",
            width = 3,
            div(
              tags$a(id = "prm_dia_count", tags$img(src="dia-prm.png", width = "100%"), href = "http://192.168.130.252:3838/apt-dia-prm/"),
              tags$h3("PRM(DIA) Analysis Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于DIA项目PRM蛋白/肽段评估"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            style = "margin-left: 30px;",
            width = 3,
            div(
              tags$a(id = "uniprot_annot_count", tags$img(src="uniprot_annot.png", width = "100%"), href = "http://192.168.130.252:3838/apt-uniprot-annot/"),
              tags$h3("Uniprot Annotation Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于Uniprot查库的蛋白功能注释(GO)"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          )
        ),
        
        fluidRow(
          style = "margin-top: 40px;",
          column(
            width = 3,
            div(
              tags$a(id = "gorich_count", tags$img(src="go_enrichment.png", width = "100%"), href = "http://192.168.130.252:3838/apt-go/"),
              tags$h3("GO Enrich Plot Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制常规GO富集图以及气泡图"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            style = "margin-left: 30px;",
            width = 3,
            div(
              tags$a(id = "pca_count", tags$img(src="pca.png", width = "100%"), href = "http://192.168.130.252:3838/apt-pca/"),
              tags$h3("Proteome PCA Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于蛋白组学PCA分析"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          )
        )
      )
    )
    
    appendTab(
      inputId = "mainnav",
      tabPanel(
        title = "Metabolome",
        icon = icon("bar-chart"),
        fluidRow(
          column(
            width = 3,
            div(
              tags$a(id = "boxplot_count", tags$img(src="boxplot.png", width = "100%"), href = "http://192.168.130.252:3838/apt-boxplot/"),
              tags$h3("Boxplot Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制靶向代谢箱线图boxplot"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          ),
          column(
            width = 3,
            div(
              tags$a(id = "metabolism-pca_count", tags$img(src="metabolism-pca.png", width = "100%"), href = "http://192.168.130.252:3838/apt-metabolism-pca/"),
              tags$h3("Metabolism PCA Tools", style = "color:#47a3da; font-family: sans-serif"),
              hr(),
              p("主要用于绘制代谢Simac PCA图"),
              style = "padding: 20px;border: 1px solid #E6E9ED"
            )
          )
        )
      )
    )
    shinyalert("Successfully", "You can choose any analysis in following modules !!!", type = "success")
  })
  
  #============================================================================================#
  shinyjs::onclick(id = "prm_count", {
    log_con <- data.frame("prm_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "volcano_count", {
    log_con <- data.frame("volcano_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "heatmap_count", {
    log_con <- data.frame("heatmap_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "venn_count", {
    log_con <- data.frame("venn_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "gorich_count", {
    log_con <- data.frame("gorich_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "keggenrich_count", {
    log_con <- data.frame("keggenrich_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "statistic_count", {
    log_con <- data.frame("statistic_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "pca_count", {
    log_con <- data.frame("pca_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "boxplot_count", {
    log_con <- data.frame("boxplot_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "metabolism-pca_count", {
    log_con <- data.frame("metabolism-pca_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "uniprot_annot_count", {
    log_con <- data.frame("uniprot_annot_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "prm_dia_count", {
    log_con <- data.frame("prm_dia_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "heatmap_cor_count", {
    log_con <- data.frame("heatmap_cor_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
  shinyjs::onclick(id = "scatter_cor_count", {
    log_con <- data.frame("scatter_cor_count", input$usertype, input$userid, input$projectid, format(Sys.time(), "%Y-%m-%d-%H-%M"))
    write.table(log_con, file = "log/information.log", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  })
}

shinyApp(ui, server)