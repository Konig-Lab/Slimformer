library(bslib)

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
  shiny::tags$head(shiny::tags$script(shiny::HTML(js_code))),
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
        sliderInput("outline_w", "Outline Width", min = 0, max = 5, value = 2, step = 0.5)
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
