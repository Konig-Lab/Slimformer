################ Init Slimformer ################

library(shiny)
library(bslib)
library(shinyjs)
library(plotly)
library(openxlsx)
library(stringi)
library(colorspace)
library(bslib)

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

pal53 <- read.csv("www/data/pal53.csv", row.names = 1)[,1]

inference <- read.delim("www/data/inference.tsv", sep="\t", row.names = 1)
reference <- read.delim("www/data/reference.tsv", sep="\t", row.names = 1)
som_map <- read.delim("www/data/som_map.tsv", sep="\t", row.names = 1)

################ functions.R ################

readCSV <- function(filepath) {
  tryCatch(
    {
      message("This is the 'try' part. Will read with row.names = 1")
      
      suppressWarnings(read.csv(filepath, row.names = 1))
    },
    error = function(cond) {
      message(paste("No row names...."))
      # Choose a return value in case of error
      read.csv(filepath, row.names = NULL)
    },
    warning = function(cond) {
      NULL
    },
    finally = {
      message(paste("Processed URL:", filepath))
    }
  )
}

readTSV <- function(filepath) {
  tryCatch(
    {
      message("This is the 'try' part. Will read with row.names = 1")
      
      suppressWarnings(read.delim(filepath, row.names = 1, sep = "\t"))
    },
    error = function(cond) {
      message(paste("No row names...."))
      # Choose a return value in case of error
      read.delim(filepath, row.names = NULL, sep = "\t")
    },
    warning = function(cond) {
      NULL
    },
    finally = {
      message(paste("Processed URL:", filepath))
    }
  )
}

readEXCEL <- function(filepath) {
  file <- filepath
  file2 <- tempfile(fileext = ".xlsx")
  
  # Datei manuell in das WebR-Filesystem kopieren
  file.copy(file, file2, overwrite = TRUE)
  tryCatch(
    {
      message("This is the 'try' part. Will read with col_names = TRUE")
      
      openxlsx::readWorkbook(file2)
    },
    error = function(cond) {
      message(paste("No column names...."))
      # Choose a return value in case of error
      openxlsx::readWorkbook(file2)
    },
    warning = function(cond) {
      NULL
    },
    finally = {
      message(paste("Processed URL:", filepath))
    }
  )
}

make_palette_hcl <- function(n, palette = "Dark 3", seed = 1) {
  set.seed(seed)
  qualitative_hcl(n, palette = palette)   # Alternativen: "Set 2", "Set 3", "Dark 3"
}

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  return(tag)
}

joinRef <- function(data, ref, term_id) {
  if("Group" %in% colnames(data) && "Group" %in% colnames(ref)) {
    group_index <- which(colnames(data) == "Group")
    colnames(data)[group_index] <- "Group.X"
  }
  colnames(ref)[1] <- term_id
  data <- dplyr::left_join(data, ref, by = term_id)
  return(data)
}

computeClusterName <- function(data, infer, term_col, intersect_col = NULL) {
  cluster.df <- data.frame(cluster_id = unique(data$Subcluster), cluster_name = "")
  if(!(intersect_col %in% colnames(data))) {
    intersect_col <- NULL
  }
  for(c in 1:nrow(cluster.df)) {
    cluster.id <- cluster.df$cluster_id[c]
    term.ids <- data[which(data$Subcluster == cluster.id), term_col]
    term.sizes <- seq(length(term.ids), 1)
    if(length(term.ids) == 0) {
      next
    }
    if(!is.null(intersect_col)) {
      term.sizes <- lapply(
        stringi::stri_split_regex(data[which(data$Subcluster == cluster.id), intersect_col], ","),
        length
      )
    }
    term.id <- term.ids[which.max(term.sizes)]
    if(term.id %in% rownames(infer)) {
      cluster.df$cluster_name[c] <- infer[term.id, 4]
    }
    
    data$Subcluster[which(data$Subcluster == cluster.id)] <- cluster.df$cluster_name[c]
  }
  
  return(data)
}

getFileExtension <- function(filename) {
  return(sub(".*\\.([^.]+)$", "\\1", filename))
}

firstup <- function(x) {
  if(length(x) > 0) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  }
  return(x)
}

plotPieChart <- function(data, term_col, intersect_col, groups, colors, title = "", use_gene_weight = FALSE) {
  if(is.null(data)) {
    return(NULL)
  }
  group_index <- which(colnames(data) == "Group")
  tbl <- as.data.frame(table(data[,group_index]))
  tbl.df <- data.frame(Category = as.vector(tbl[,1]), Value = as.vector(tbl[,2]))
  colors.df <- data.frame(groups = groups, colors = colors)[which(groups %in% tbl.df$Category),]
  if(use_gene_weight == TRUE) {
    unique_genes <- unique(unlist(stringi::stri_split(paste0(data[, intersect_col], collapse = ","), regex = ",")))
    
    annotation_mat <- matrix(0, nrow = nrow(data), ncol = length(unique_genes))
    rownames(annotation_mat) <- data[, term_col]
    colnames(annotation_mat) <- unique_genes
    for(r in 1:nrow(annotation_mat)) {
      genes <- unlist(stringi::stri_split(data[r, intersect_col], regex = ","))
      annotation_mat[r, genes] <- 1
    }
    colsums <- colSums(annotation_mat)
    annotation_mat <- t(t(annotation_mat) / colsums)
    tmp.df <- cbind(data, data.frame(value = rowSums(annotation_mat)))
    tbl.df <- data.frame(Category = colors.df$groups, Value = rep(NA, nrow(colors.df)))
    for(i in 1:nrow(colors.df)) {
      code <- colors.df$groups[i]
      tbl.df$Value[i] <- sum(tmp.df$value[which(tmp.df[,group_index] == code)])
    }
  }
  tbl.df$Category <- firstup(tbl.df$Category)
  tbl.df <- tbl.df[order(tbl.df$Category),]
  fig <- plotly::plot_ly(tbl.df, 
                         labels = ~Category, 
                         values = ~Value, 
                         type = 'pie',
                         textinfo = 'percent',
                         hoverinfo = 'text',
                         text = ~paste0(Category),
                         marker = list(colors = colors.df$colors,
                                       line = list(color = "#FFFFFF", width = 1))
  )
  fig <- fig |> 
    plotly::layout(
      title = title,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(fig)
}

plotScatterPlot <- function(data, term_col, groups, colors, infer, show_subclusters = FALSE, line_width = 2, subcluster_col = NULL, subcluster_palette = NULL, title="") {
  if(is.null(data)) {
    return(NULL)
  }
  print(data[[term_col]][which(duplicated(data[[term_col]]))])
  rownames(data) <- data[[term_col]]
  pos.df <- infer[rownames(infer) %in% data[[term_col]], , drop = FALSE]
  if (nrow(pos.df) == 0) return(NULL)
  
  # harmonisieren
  pos.df$Class <- firstup(pos.df$Class)
  pos.df$text  <- firstup(pos.df$text)
  
  # Cluster-Farbmapping stabilisieren
  names(colors) <- firstup(groups)
  
  # Subcluster-Spalte bestimmen (Standard: letzte Spalte von 'data')
  if (isTRUE(show_subclusters)) {
    if (is.null(subcluster_col)) {
      subcluster_col <- names(data)[ncol(data)]
    }
    pos.df$Subcluster <- firstup(data[rownames(pos.df), subcluster_col])
    pos.df$Subcluster <- if (is.factor(pos.df$Subcluster)) pos.df$Subcluster else factor(pos.df$Subcluster)
    
    # Palette für Subcluster (nur für Outline)
    if (is.null(subcluster_palette)) {
      # einfache, reproduzierbare Palette (53+ Farben) – gern durch deine Funktion ersetzen
      pal <- pal53
      names(pal) <- levels(pos.df$Subcluster)
      sub_pal <- pal
    } else {
      sub_pal <- subcluster_palette
      if (is.null(names(sub_pal))) names(sub_pal) <- levels(pos.df$Subcluster)
    }
    pos.df$line_col <- unname(sub_pal[as.character(pos.df$Subcluster)])
  }
  
  # Leere Figur starten
  fig <- plotly::plot_ly()
  
  # Für jede Cluster-Kategorie ein eigener Trace -> erzeugt saubere Legende
  cls <- unique(pos.df$Class)
  for (cl in cls) {
    dsub <- pos.df[pos.df$Class == cl, , drop = FALSE]
    # fester Fill je Cluster (kein Vektor!) -> Legendeneintrag
    base_marker <- list(size = 10, color = colors[[cl]])
    if (isTRUE(show_subclusters)) {
      base_marker$line <- list(color = dsub$line_col, width = line_width) # Vektor ok
    }
    
    fig <- fig |>
      plotly::add_trace(
        data = dsub,
        x = ~(-Y), y = ~X,
        type = "scatter", mode = "markers",
        name = cl, showlegend = TRUE,
        marker = base_marker,
        hoverinfo = "text",
        text = ~paste0(
          "Category: ", Class,
          "<br>Name: ", text,
          if (show_subclusters) paste0("<br>Subcluster: ", Subcluster) else ""
        )
      )
  }
  
  # Ein einziger Legenden-Eintrag für den Subcluster-Ring
  if (isTRUE(show_subclusters)) {
    fig <- fig |>
      plotly::add_trace(
        x = NA, y = NA, type = "scatter", mode = "markers",
        name = "Subcluster (Rand)", showlegend = TRUE,
        marker = list(size = 10, color = "white", line = list(color = "black", width = 2)),
        inherit = FALSE
      )
  }
  
  fig <- fig |>
    plotly::layout(
      title = title,
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
  
  return(fig)
}


################ ui.R ################
js_code <- "
Shiny.addCustomMessageHandler('toggleCheckbox', function(enable) {
  $('#gene_weights').prop('disabled', !enable);
});

Shiny.addCustomMessageHandler('showOverlay', function(message) {
  var overlay = document.getElementById('overlay');
  if(message) {
    overlay.style.display = 'flex!important';
  } else {
    overlay.style.display = 'none';
  }
});

Shiny.addCustomMessageHandler('getSVG', function(message) {
  start = 0;
  end = 2;
  var mainSVGs = document.getElementsByClassName('main-svg')
  if(message[0] == 'Scatter Plot') {
    start = mainSVGs.length-3;
    end = mainSVGs.length-1;
  }
  console.log(start);
  console.log(end);
  console.log(mainSVGs)
  document.getElementById('spinner').style.display = 'block';
  const width = mainSVGs.item(0).getAttribute(\"width\");
  const height = mainSVGs.item(0).getAttribute(\"height\");
  var mergedSVG = '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"'+width+'\" height=\"'+height+'\" viewBox=\"0 0 '+width+' '+height+'\">';
  for(var i = start; i < end; i++) {
    mergedSVG += mainSVGs.item(i).innerHTML;
  }
  mergedSVG += '</svg>';
  Shiny.setInputValue('svg_data', mergedSVG, {priority: 'event'});
});

Shiny.addCustomMessageHandler('downloadSVG', function(svg_string) {
  const blob = new Blob([svg_string], { type: 'image/svg+xml' });
  const url = URL.createObjectURL(blob);
  const link = document.getElementById('download_link');
  link.href = url;
  
  const tabs_panel = document.getElementById('tabs_panel');
  for( var i = 0; i < tabs_panel.childElementCount; i++) {
    if(tabs_panel.children[i].children[0].hasAttribute('tabindex') == false) {
      const names = tabs_panel.children[i].children[0].getAttribute('data-value').split(' ');
      let yourDate = new Date()
      let yourDateArr = yourDate.toISOString().split('T').join('_').split(':')
      if(names[0] == 'Scatter') {
        link.download = 'Slimformer'+names[0]+names[1]+'_'+yourDateArr[0]+':'+yourDateArr[1]+'.svg'
      }
      if(names[0] == 'Pie') {
        link.download = 'SlimformerPieChart_'+yourDateArr[0]+':'+yourDateArr[1]+'.svg'
      }
    }
  }
  
  document.getElementById('spinner').style.display = 'none';
  
  link.click();
});
"

# Define UI for application that draws a histogram
ui <- shiny::tagList(
  shiny::tags$head(
    shiny::tags$script(shiny::HTML(js_code)),
    tags$script(src = "xlsx.full.min.js"),
    tags$script(src = "excel_to_tsv.js"),
    shiny::tags$style(HTML("
    .sidebar {
      display: flex;
      flex-direction: column;
      height: 95vh;
    }
    .sidebar-bottom {
      margin-top: auto;
      padding-top: 20px;
      color: #333;
      font-size: 0.9em;
    }
  "))
  ),
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    theme = bs_theme(version = 5),
    # Application title
    titlePanel("Slimformer"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        class = "sidebar",
        style = "height:95vh",
        fileInput(
          inputId = "upload_data",
          label = "Upload CSV, TSV or Excel File:",
          accept = c(".csv", ".tsv", ".xls", ".xlsx")
        ),
        shiny::h6("Term ID column: ",bslib::tooltip(
          span(icon("circle-question"), class = "text-muted ms-1"),
          "The name of the column storing the GO term IDs. 'term_id' is the default for g:Profiler2 output.",
          placement = "right"  # top/bottom/left/right/auto
        ),
        style = "margin-bottom: -10px;"),
        textInput("term_name_col","", value = "term_id"),
        shiny::h6("Intersect column (Optional): ", 
                  bslib::tooltip(
                    span(icon("circle-question"), class = "text-muted ms-1"),
                    "The name of the column storing the Genes associated with the GO terms. 'intersection' is the default for g:Profiler2 output.",
                    placement = "right"  # top/bottom/left/right/auto
                  ),
                  style = "margin-bottom: -10px;"
        ),
        textInput("intersect_col", "", value = "intersection"),
        div(
          shiny::p("Use Gene Weights:",bslib::tooltip(
            span(icon("circle-question"), class = "text-muted ms-1"),
            "Should the sizes of the pie slices be calculated via 'gene weight' or by number of gene sets? For more information on 'gene weight' please see the publication.",
            placement = "right"  # top/bottom/left/right/auto
          )), 
          checkboxInput("gene_weights", "", value = FALSE),
          style = "display: grid; grid-template-columns: 175px 20px;",
        ),
        tags$script("$('#gene_weights').prop('disabled', true);"),
        wellPanel(
          strong("Check Input:"),
          div(verbatimTextOutput("check_result"), 
              style = "min-height: 70px; max-height: 300px; height: auto; overflow-y: auto; border: 1px solid #ddd; padding: 5px; background-color: #f9f9f9;")
        ),
        div(
          style = "padding-top: 20px;",
          shiny::p("Show Subclusters:",bslib::tooltip(
            span(icon("circle-question"), class = "text-muted ms-1"),
            "Should the subclusters of the gene sets be shown. This is for a finer-grained overview only.",
            placement = "right"  # top/bottom/left/right/auto
          )), 
          checkboxInput("show_outline", "", value = FALSE),
          style = "display: grid; grid-template-columns: 175px 20px;",
        ),
        sliderInput("outline_w", "Outline Width", min = 0, max = 5, value = 2, step = 0.5),
        div(
          class = "sidebar-bottom",
          shiny::p("See", shiny::a("README", href='https://github.com/Konig-Lab/Slimformer/blob/main/README.md'), "for Tutorial and example data."),
          shiny::p("If you use Slimformer, please cite: TBA.")
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          id = "tabs_panel",
          tabPanel("Data Table", id = 'data_table', div(DT::DTOutput("data_table", height = "85vh", width = "98%"), style = "margin-top: 10px")),
          tabPanel("Pie Chart", id = 'pie_chart', plotly::plotlyOutput("pie_plot", height = "85vh", width = "98%")),
          tabPanel("Scatter Plot", id = 'scatter_plot', plotly::plotlyOutput("scatter_plot", height = "85vh", width = "98%"))
        ),
        tags$div(id = "spinner", style = "display:none; color: #2C3E50; font-weight: bold; margin-top: 10px;", "Preparing your download..."),
        div(
          downloadButton("download_tsv", "Download Annotated TSV"),
          actionButton("prepare_svg", "Download SVG")
        ),
        tags$a(id = "download_link", href = "", download = "SlimformerPlot.svg", style = "display:none;", "Click to download")
      )
    ),
    div(
      id = "overlay",
      class = "overlay",
      div(class = "loader")
    )
  )
)


################ server.R ################
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  renderData <- function(data, term_col) {
    if(!is.null(data) && (term_col != "" && term_col %in% colnames(data))) {
      if("Group" %in% colnames(data)) {
        data <- data[,-c(which(colnames(data) == "Group"))]
      }
      if("QC" %in% colnames(data)) {
        data <- data[,-c(which(colnames(data) == "QC"))]
      }
      if("Subcluster" %in% colnames(data)) {
        data <- data[,-c(which(colnames(data) == "Subcluster"))]
      }
      data <- joinRef(data, reference, term_col)
      data <- joinRef(data, som_map, term_col)
      data <- computeClusterName(data, inference, term_col, input$intersect_col)
    }
    
    output$data_table <- DT::renderDT(data, extensions = "Buttons", options = list(scrollX = TRUE, scrollY = "65vh", scrollCollapse = TRUE,
                                                                                   columnDefs = list(list(
                                                                                     targets = "_all",
                                                                                     render = DT::JS(
                                                                                       "function(data, type, row, meta) {",
                                                                                       "return type === 'display' && data != null && data.length > 30 ?",
                                                                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                                       "}")
                                                                                   )),
                                                                                   dom = "Bfrtip",
                                                                                   buttons = list(
                                                                                     list(extend = "colvis", text = "Visible Columns")  # Dropdown mit Checkboxen
                                                                                   ),
                                                                                   paging = FALSE),
                                      class = "display")
    return(data)
  }
  
  data_reactive <- shiny::reactiveVal()
  shiny::observeEvent(input$upload_data,{
    shiny::req(input$upload_data)
    session$sendCustomMessage("CheckExcel", input$upload_data)
    if (!is.null(input$excel_tsv) && nzchar(input$excel_tsv)) {
      message("Reading data from JS-converted Excel (tsv-text).")
      tsv_text <- input$excel_tsv
      tbl <- read.delim(text = tsv_text, stringsAsFactors = FALSE)
      tbl<- tbl[-c(which(tbl[[input$term_name_col]] == "")),]
      tbl <- renderData(tbl, input$term_name_col)
      data_reactive(tbl)
      return(tbl)
    }
    if (!is.null(input$upload_data)) {
      message("Reading data direct from input$file$datapath (non-Excel file).")
      file_path <- input$upload_data$datapath
      file_extension <- tolower(getFileExtension(file_path))
      df <- switch(
        file_extension,
        csv = {
          tbl <- NA
          tbl <- readCSV(file_path)
          tbl <- renderData(tbl, input$term_name_col)
          data_reactive(tbl)
          return(tbl)
        },
        tsv = {
          tbl <- NA
          tbl <- readTSV(file_path)
          tbl <- renderData(tbl, input$term_name_col)
          data_reactive(tbl)
          return(tbl)
        },
        {
          shiny::showModal(
            modalDialog(
              title = "Unsupported File Type",
              p("Please use one of the following file types:"),
              p("'.csv', '.tsv', '.xls', '.xlsx'"),
              easyClose = TRUE
            )
          )
          return(NULL)
        }
      )
    }
  })
  
  current_tab <- shiny::reactiveVal()
  shiny::observeEvent(input$tabs_panel, {
    current_tab(input$tabs_panel)
  })
  
  shiny::observe({
    shiny::req(input$term_name_col)
    
    df_recieved <- data_reactive()
    df_recieved <- renderData(df_recieved, input$term_name_col)
    
    if(!is.null(df_recieved)) {
      term_col <- trimws(input$term_name_col)
      intersect_col <- trimws(input$intersect_col)
      
      missing_cols <- c()
      
      if (term_col == "" || !(term_col %in% names(df_recieved))) {
        missing_cols <- c(missing_cols, paste("❌ Mandatory column missing:", term_col))
      }
      
      
      intersection_present <- FALSE
      if (intersect_col != ""){
        if(!(intersect_col %in% names(df_recieved))) {
          missing_cols <- c(missing_cols, paste("⚠️ Optional column missing:", intersect_col))
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
      
      if(term_col != "" && term_col %in% names(df_recieved)) {
        shinyjs::runjs("document.getElementById('overlay').style.display = 'flex';")
        df_col <- df_recieved
        colnames(df_col)[which(colnames(df_col) == term_col)] <- "term_id"
        
        plotly_pie_plot <- plotPieChart(df_col, term_col, intersect_col, groups, colors, title = "", use_gene_weight = input$gene_weights)
        
        plotly_scatter_plot <- plotScatterPlot(df_col, term_col, groups, colors, infer = inference, show_subclusters = input$show_outline, line_width = input$outline_w, title = "")
        
        output$pie_plot <- plotly::renderPlotly(
          {
            plotly_pie_plot
          }
        )
        output$scatter_plot <- plotly::renderPlotly(
          {
            plotly_scatter_plot
          }
        )
        shinyjs::runjs("document.getElementById('overlay').style.display = 'none';")
      }
      session$sendCustomMessage(type = "toggleCheckbox", intersection_present)
    }
  })
  
  svg_content <- reactiveVal("")
  observe({
    if (!is.null(input$svg_data)) {
      svg_content(input$svg_data)
    }
  })
  
  observeEvent(input$prepare_svg, {
    cTab <- current_tab()
    session$sendCustomMessage("getSVG", list(cTab))
  })
  
  observeEvent(input$svg_data, {
    if (nzchar(input$svg_data)) {
      session$sendCustomMessage("downloadSVG", input$svg_data)
    }
  })
  output$download_tsv <- downloadHandler(
    filename = function() {
      paste("SlimformerAnnotated_",head(unlist(stringi::stri_split(input$upload_data$name, regex = "\\.")), -1),"_", Sys.Date(), ".tsv", sep="")
    },
    content = function(file) {
      data <- data_reactive()
      write.table(data, file, sep = "\t", row.names=FALSE)
    },
    contentType = "text/tsv"
  )
}

################ Run Shiny App ################
# Run the application 
shinyApp(ui = ui, server = server)