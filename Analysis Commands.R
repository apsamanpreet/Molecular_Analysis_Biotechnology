# run below command in R studio
# install packages to run commands properly
library(readxl)
library(ggplot2)
library(factoextra)
library(pheatmap)
library(RColorBrewer)



# import your DATASET excel File
newraw <- rawdata[ ,-1]
# here (-1) to omit 1st column and Zero/space for none row to omit
# a (newraw) named file will be saved

analysisfile <- newraw
rownames(analysisfile) <- c(rawdata$Genotypes)
#here (Genotypes) is 1st Column head names of raw excel file
# ignore (Warning message :  Setting row____deprecated.
View(analysisfile)
#
scale.af <- scale(analysisfile)
# check row names changed to Genotype names or not??

require(stats)
distance.af <- dist(x = scale.af, 
                        method = "euclidean")
x <- as.matrix(distance.af)
# [1:6] for 1st six rows and next [1:6] to see 1st six columns
# may round up this DISTANCE MATRIX (eucledian)
round(x, digits = 2)

# optional
View(x)
# 
require(stats)
cluster.af <- hclust(d = distance.af,
                     method = "complete")
# this will plot a Dendrogram (simple rectangular without colours)
plot(x = cluster.af)

fviz_dend(cluster.af, cex = 0.6, lwd = 0.9, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE)
#to plot this rec. dendro into horizontaly use below command
fviz_dend(cluster.af, cex = 1.2, lwd = 0.9, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          horiz = TRUE)

# type = "circular", "rectangle", "phylogenic"
fviz_dend(cluster.af, cex = 0.8, lwd = 0.9, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "circular")
# use (repel = TRUE) in the end when ploting "phylogenic"
# layouts for "phylogenic" (phylo_layout = "layout_as_tree", "layout_with_lgl", "layout_with_drl", "layout.gem", "layout.mds")
# example
fviz_dend(cluster.af, cex = 1.2, lwd = 10, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_as_tree")

# change names first - heatmap title
pheatmap(analysisfile, scale = "row", cutree_row= 5, cluster_rows = TRUE,
         kmeans_k = NA, main = "Change Name in Formula", show_colnames = FALSE,
         show_rownames = TRUE,
         color = colorRampPalette(rev(brewer.pal(n = 5, name ="Paired")))(255))
# color (name) = YlOrRd,YlOrB,YlGnBu,Set3,Set2,RdYlBu,Reds,RdBu,Purples,PuOr,
# PuBuGn,PRGn,Pastel2,Pastel1,Paired,OrRd,Oranges,Greys,Greens,GnBu
#if error than view () file - find "NA/NaN" then edit (breaks = ?) ABOVE
#cutree_row= 5 (number of groups of Genotype) or cutree_cols = 2
# brewer.pal here n = mini 3 and max. depend upon data palette
