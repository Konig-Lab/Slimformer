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
  data <- left_join(data, ref, by = "term_name")
  group_index <- which(colnames(data) == "Group")
  df <- data.frame(Category = names(table(data[,group_index])), Value = as.vector(unname(table(data[,group_index]))))
  colors_df <- data.frame(groups = groups, colors = colors)[which(groups %in% df$Category),]
  if(by_gene_weight == TRUE) {
    unique_genes <- unique(unlist(stri_split(paste0(data$intersection, collapse = ","), regex = ",")))
    
    annotation_matrix = matrix(0, nrow = nrow(data), ncol = length(unique_genes))
    for(r in 1:nrow(data)) {
      for(c in 1:length(unique_genes)) {
        if(unique_genes[c] %in% unlist(stri_split(data$intersection[r], regex = ","))) {
          annotation_matrix[r,c] <- 1
        }
      }
    }
    colsums <- colSums(annotation_matrix)
    annotation_matrix <- t(t(annotation_matrix) / colsums)
    tmp_df <- cbind(data, data.frame(value=rowSums(annotation_matrix)))
    df <- data.frame(Category=colors_df$groups, Value=rep(NA, nrow(colors_df)))
    for(i in 1:nrow(colors_df)) {
      code <- colors_df$groups[i]
      df$Value[i] <- sum(tmp_df$value[which(tmp_df$Group == code)])
    }
    #df <- data.frame(Category = names(table(data[,group_index])), Value = as.vector(unname(table(data[,group_index]))))
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