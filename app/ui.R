#styleName <- 'style.css'
#style <- HTML(readChar(styleName, file.info(styleName)$size))
scriptName <- 'script.js'
script <- HTML(readChar(scriptName, file.info(scriptName)$size))

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
      
      textInput("termname_col", "Term Name:", value = "term_name"),
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
        actionButton("download_csv", "Download Annotated CSV"),
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
  ),
  tags$script(HTML("
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
      link.click();
    });
  "))
)

js_code <- "
Shiny.addCustomMessageHandler('toggleCheckbox', function(enable) {
  $('#gene_weights').prop('disabled', !enable);
});
"

ui <- tagList(
  tags$head(tags$script(HTML(js_code))),
  ui
)