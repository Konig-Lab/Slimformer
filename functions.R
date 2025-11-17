library(colorspace)

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
    if(!is.null(intersect_col)) {
      term.sizes <- lapply(
        stringi::stri_split_regex(data[which(data$Subcluster == cluster.id), intersect_col], ","),
        length
        )
    }
    term.id <- term.ids[which.max(term.sizes)]
    cluster.df$cluster_name[c] <- infer[term.id, 4]
    
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
  
  tbl.df <- data.frame(Category = unique(data[,group_index]), Value = as.vector(unname(table(data[,group_index]))))
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
