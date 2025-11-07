install.packages("readxl")
library("readxl")
install.packages("pheatmap")
library("pheatmap")
install.packages("RColorBrewer")
library("RColorBrewer")

# Heatmap DEGs MM-Control

MM_Control_data <- read_excel ("data-heatmap.xlsx")
head(MM_Control_data)
rownames(MM_Control_data) <- MM_Control_data[[1]]
MM_Control_data <- MM_Control_data[,-1]
head(MM_Control_data)
colores_cb <- colorRampPalette(brewer.pal(n=11,name="PRGn"))(100)

png ("Heatmap_replica.png")
pheatmap(
  MM_Control_data,
  scale = "row",  # importante para comparar patrones
  color = colores_cb,
  clustering_distance_rows = "euclidean",     # métrica para genes
  clustering_distance_cols = "euclidean",     # métrica para muestras
  clustering_method = "complete",             # método de agrupamiento
  show_rownames = FALSE,                      # ocultar nombres de genes si son muchos
  show_colnames = TRUE,                       # mostrar nombres de muestras
  fontsize = 10,
  border_color = NA
)
dev.off()
