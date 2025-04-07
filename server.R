server <- function(input, output, session) {
  
  
  data_reactive <- reactiveVal()
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    
    file_path <- input$upload_file$datapath
    ext <- tolower(file_ext(file_path))
    
    df <- switch(ext,
                 csv  = read.csv(file_path),
                 tsv  = read.delim(file_path),
                 xls  = read_excel(file_path, n_max = 0, skip = 1),
                 xlsx = read_excel(file_path, n_max = 0, skip = 1),
                 {
                   showNotification("Unsupported file type", type = "error")
                   return(NULL)
                 }
    )
    data_reactive(df)
  })
  
  
  output$table <- renderDT({
    req(data_reactive())
    datatable(data_reactive(), options = list(pageLength = 5))
  })
  
  
  observeEvent(input$check_btn, {
    
  })
  
  observe({
    req(data_reactive(), input$termname_col)
    
    df <- data_reactive()
    
    termname_col <- trimws(input$termname_col)
    intersection_col <- trimws(input$intersection_col)
    
    missing_cols <- c()
    
    
    if (termname_col == "" || !(termname_col %in% names(df))) {
      missing_cols <- c(missing_cols, paste("❌ Mandatory column missing:", termname_col))
    }
    
    
    intersection_present <- FALSE
    if (intersection_col != ""){
      if(!(intersection_col %in% names(df))) {
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
    if(termname_col != "" && termname_col %in% names(df)) {
      runjs("document.getElementById('overlay').style.display = 'flex';")
      plotly_plot <- plotPieChart(df, classification.look.up, title = "", by_gene_weight = input$gene_weights)
      
      #runjs("document.getElementById('categorization_done_div').style.visibility = 'visible';")
      runjs("document.getElementById('overlay').style.display = 'none';")
      #runjs("document.getElementById('visible_when_gsea_done').style.visibility = 'visible';")
      
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
  
  output$download_css <- downloadHandler(
    filename = function() {
      "annotated.csv"
    },
    content = function(file) {
      if("Group" %in% colnames(df)) {
        group_index <- which(colnames(df) == "Group")
        colnames(df)[group_index] <- "Group.X"
      }
      data <- left_join(df, ref, by = "term_name")
      write.csv(data, "annotated.csv")
    }
  )
}