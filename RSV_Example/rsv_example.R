library(data.table)
library(dplyr)
library(edgeR)
library(gprofiler2)

rnaseq.data <- fread("https://ftp.ncbi.nlm.nih.gov/geo/series/GSE161nnn/GSE161846/suppl/GSE161846%5Fmaster%5Fannotated%5Ftranscripts%2Ecsv%2Egz")
rnaseq.data <- rnaseq.data[, -c(1,2, 7:10, 15:28)] #Only keep WT-0H, WT-24H and gene symbols

conditions <- c(rep("control", 4), rep("infected", 4))
design <- model.matrix(~ conditions, as.data.frame(conditions))
y <- DGEList(rnaseq.data[, -9])
y <- calcNormFactors(y)
y <- estimateGLMCommonDisp(y, design = design)
fit <- edgeR::glmQLFit(y, design = design)
fit <- glmQLFTest(fit)
diff.genes <- topTags(fit, n = Inf)
diff.genes$table$Gene <- rnaseq.data$GENENAME[as.numeric(rownames(diff.genes$table))]

diff.up <- filter(filter(diff.genes$table, logFC > 1), FDR < 0.05)
diff.down <- filter(filter(diff.genes$table, logFC < -1), FDR < 0.05)

gs.up <- gost(diff.up$Gene, sources = "GO:BP", evcodes = T)
gs.down <- gost(diff.down$Gene, sources = "GO:BP", evcodes = T)

gs.up <- filter(filter(gs.up$result, term_size > 10), term_size < 500)
gs.down <- filter(filter(gs.down$result, term_size > 10), term_size < 500)

write.csv(as.matrix(gs.up), "RSV_Example/upregulated_gene_sets.csv", row.names = F)
write.csv(as.matrix(gs.down), "RSV_Example/downregulated_gene_sets.csv", row.names = F)
