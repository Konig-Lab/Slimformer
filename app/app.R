library(shiny)
library(plotly)
library(dplyr)
library(stringi)
library(readxl)
library(shinyjs)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

joinRef <- function(data, ref) {
  if("Group" %in% colnames(data)) {
    group_index <- which(colnames(data) == "Group")
    colnames(data)[group_index] <- "Group.X"
  }
  data <- left_join(data, ref, by = "term_id")
  data
}

plotPieChart <- function(data, ref, title="Pie Chart", by_gene_weight = FALSE) {
  groups <- c("cell adhesion",
              "cell cycle",
              "cell death",
              "cellular component organization",
              "ER/endosome/lysosome related process",
              "gene regulation",
              "immune system process",
              "metabolic process",
              "morphogenesis/development",
              "multicellular organismal process",
              "neural related process",
              "other",
              "protein modification/signaling",
              "receptor related process",
              "response to stimulus",
              "stress response",
              "transport",
              "viral related process"
  )
  colors <- c("#3ef5f0","#7030a0","#808080","#b4c7e7","#2e75b6","#f8cbad","#ff0000",
              "#ff00ff","#548235","#be73b8","#009999","#f2f2f2","#cccc00","#cfffc4","#ffff00",
              "#ff8000","#00ff00","#00b0f0"
  )
  
  if(is.null(data)) {
    return(NULL)
  }
  if("Group" %in% colnames(data)) {
    group_index <- which(colnames(data) == "Group")
    colnames(data)[group_index] <- "Group.X"
  }
  data <- left_join(data, ref, by = "term_id")
  group_index <- which(colnames(data) == "Group")
  df <- data.frame(Category = names(table(data[,group_index])), Value = as.vector(unname(table(data[,group_index]))))
  colors_df <- data.frame(groups = groups, colors = colors)[which(groups %in% df$Category),]
  if(by_gene_weight == TRUE) {
    unique_genes <- unique(unlist(stri_split(paste0(data$intersection, collapse = ","), regex = ",")))
    
    annotation_matrix = matrix(0, nrow = nrow(data), ncol = length(unique_genes))
    rownames(annotation_matrix) <- data$term_id
    colnames(annotation_matrix) <- unique_genes
    for(r in 1:nrow(data)) {
      genes <- unlist(stri_split(data$intersection[r], regex = ","))
      annotation_matrix[data$term_id[r],genes] <- 1
    }
    colsums <- colSums(annotation_matrix)
    annotation_matrix <- t(t(annotation_matrix) / colsums)
    tmp_df <- cbind(data, data.frame(value=rowSums(annotation_matrix)))
    df <- data.frame(Category=colors_df$groups, Value=rep(NA, nrow(colors_df)))
    for(i in 1:nrow(colors_df)) {
      code <- colors_df$groups[i]
      df$Value[i] <- sum(tmp_df$value[which(tmp_df$Group == code)])
    }
  }
  fig <- plot_ly(df, labels = ~Category, values = ~Value, type = 'pie',
                 textinfo='percent',
                 hoverinfo = 'text',
                 text = ~paste0(Category),
                 marker = list(colors = colors_df$colors,
                               line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% layout(title = title,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(fig)
}

classification.look.up <- read.csv("www/data/Annotation_Results.csv")
colnames(classification.look.up)[1] <- "term_id"
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Upload and Validate Data"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$style(HTML("
          .sidebar { width: 25% !important; min-width: 200px; }
          .main-panel { width: 75% !important; }
        "))
    ),
    sidebarLayout(
        sidebarPanel(
        class = "sidebar",
        fileInput(
          inputId = "upload_file",
          label = "Upload CSV, TSV, or Excel File",
          accept = c(".csv", ".tsv", ".xls", ".xlsx")
        ),
        
        textInput("termname_col", "Term ID:", value = "term_id"),
        textInput("intersection_col", "Intersection (Optional):", value = "intersection"),
        
        wellPanel(
          strong("Check Input:"),
          div(verbatimTextOutput("check_result"), 
              style = "height: 70px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; background-color: #f9f9f9;")
        ),
        
        checkboxInput("gene_weights", "Gene Weights", value = FALSE),
        
        tags$script("$('#gene_weights').prop('disabled', true);"),
        
        actionButton("check_btn", "Check Columns", class = "btn-primary")
        ),
        mainPanel(
          plotlyOutput('plot'),
          tags$div(id = "spinner", style = "display:none; color: #2c3e50; font-weight: bold; margin-top: 10px;",
                   "Preparing your download..."),
          div(
            downloadButton("download_csv", "Download Annotated CSV"),
            actionButton("prepare_svg", "Prepare & Download SVG"),
          ),
          tags$a(id = "download_link", href = "", download = "merged_image.svg", style = "display:none", "Click to download"),
        )
    ),
    div(
      id="overlay",
      class = "overlay",
      div(
        class = "loader"
      )
    )
)

js_code <- "
Shiny.addCustomMessageHandler('toggleCheckbox', function(enable) {
  $('#gene_weights').prop('disabled', !enable);
});
Shiny.addCustomMessageHandler('showOverlay', function(message) {
  if (message) {
    document.getElementById('overlay').style.display = 'flex!important';
  } else {
    document.getElementById('overlay').style.display = 'none';
  }
});
Shiny.addCustomMessageHandler('getSVG', function(message) {
  document.getElementById('spinner').style.display = 'block';
  var width = document.getElementsByClassName('main-svg').item(0).getAttribute(\"width\")
  var height = document.getElementsByClassName('main-svg').item(0).getAttribute(\"height\")
  var mergedSVG = '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"'+width+'\" height=\"'+height+'\" viewBox=\"0 0 '+width+' '+height+'\">';
  for (var i = 0; i < document.getElementsByClassName('main-svg').length; i++) {
    mergedSVG += document.getElementsByClassName('main-svg').item(i).innerHTML
  }
  
  mergedSVG += '</svg>';
  Shiny.setInputValue('svg_data', mergedSVG, {priority: 'event'});
});

Shiny.addCustomMessageHandler('triggerDownload', function(svg_string) {
  document.getElementById('spinner').style.display = 'none';
  const blob = new Blob([svg_string], { type: 'image/svg+xml' });
  const url = URL.createObjectURL(blob);
  const link = document.getElementById('download_link');
  link.href = url;
  console.log('DOWNLOAD TRIGGERED')
  link.click();
});
"

ui <- tagList(
  tags$head(tags$script(HTML(js_code))),
  ui
)

get_file_extension <- function(filename) {
  sub(".*\\.([^.]+)$", "\\1", filename)
}

server <- function(input, output, session) {

  data_reactive <- reactiveVal()
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    
    file_path <- input$upload_file$datapath
    ext <- tolower(get_file_extension(file_path))
    
    df <- switch(ext,
                 csv  = read.csv(file_path),
                 tsv  = read.delim(file_path),
                 xls  = read_excel(file_path),
                 xlsx = read_excel(file_path),
                 {
                   showNotification("Unsupported file type", type = "error")
                   return(NULL)
                 }
    )
    data_reactive(df)
  })
  
  observeEvent(input$check_btn, {
    
  })
  
  observe({
    req(data_reactive(), input$termname_col)
    
    df_recieved <- data_reactive()
    
    termname_col <- trimws(input$termname_col)
    intersection_col <- trimws(input$intersection_col)
    
    missing_cols <- c()
    
    
    if (termname_col == "" || !(termname_col %in% names(df_recieved))) {
      missing_cols <- c(missing_cols, paste("❌ Mandatory column missing:", termname_col))
    }
    
    
    intersection_present <- FALSE
    if (intersection_col != ""){
      if(!(intersection_col %in% names(df_recieved))) {
        missing_cols <- c(missing_cols, paste("⚠️ Optional column missing:", intersection_col))
        missing_cols <- c(missing_cols, paste("❌ Gene Weight disabled!"))
      } else {
        intersection_present <- TRUE
      }
    }
    
    
    result <- if (length(missing_cols) == 0) {
      "✅ All specified columns are present!"
    } else {
      paste(missing_cols, collapse = "\n")
    }
    
    output$check_result <- renderText({ result })
    if(termname_col != "" && termname_col %in% names(df_recieved)) {
      runjs("document.getElementById('overlay').style.display = 'flex';")
      df_col <- df_recieved
      colnames(df_col)[which(colnames(df_col) == termname_col)] <- "term_id"
      plotly_plot <- plotPieChart(df_col, classification.look.up, title = "", by_gene_weight = input$gene_weights)
      
      runjs("document.getElementById('overlay').style.display = 'none';")
      
      output$plot <- renderPlotly(
        {
          plotly_plot
        }
      )
    }
    session$sendCustomMessage(type = "toggleCheckbox", intersection_present)
  })
  
  svg_content <- reactiveVal("")
  
  observe({
    if (!is.null(input$svg_data)) {
      svg_content(input$svg_data)
    }
  })
  
  observeEvent(input$prepare_svg, {
    session$sendCustomMessage("getSVG", list())
  })
  
  observeEvent(input$svg_data, {
    if (nzchar(input$svg_data)) {
      session$sendCustomMessage("triggerDownload", input$svg_data)
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("annotated_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      df_col <- data_reactive()
      colnames(df_col)[which(colnames(df_col) == termname_col)] <- "term_id"
      data <- joinRef(data_reactive(), classification.look.up)
      colnames(df_col)[which(colnames(df_col) == "term_id")] <- termname_col
      write.csv(data, file, row.names=FALSE)
    },
    contentType = "text/csv"
  )
}

shinyApp(ui = ui, server = server)
