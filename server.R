


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
    file_path <- input$upload_data$datapath
    file_extension <- tolower(getFileExtension(file_path))
    df <- switch(
      file_extension,
      csv = {
        tbl <- NA
        try(
          {tbl <- read.csv(file_path, row.names = 1)}, silent = TRUE)
        if(is.na(tbl)) {
          tbl <- read.csv(file_path, row.names = NULL)
        }
        tbl <- renderData(tbl, input$term_name_col)
        data_reactive(tbl)
        return(tbl)
      },
      tsv = {
        tbl <- NA
        try(
          {tbl <- read.delim(file_path, sep = "\t", row.names = 1)}, silent = TRUE)
        if(is.na(tbl)) {
          tbl <- read.delim(file_path, sep = "\t", row.names = NULL)
        }
        tbl <- renderData(tbl, input$term_name_col)
        data_reactive(tbl)
        return(tbl)
        },
      xls = {
        tbl <- read_excel(file_path)
        tbl <- renderData(tbl, input$term_name_col)
        data_reactive(tbl)
        return(tbl)
      },
      xlsx = {
        tbl <- read_excel(file_path)
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