## plot a heatmap

## arguments:
##  - x: a matrix
#   - scale (str): scale columns, rows, or none?
##  - title (str): title of the heatmap
##  - Rowv (T/F): if/how row dendrogram should be ordered
##  - dendrogram (str): whether to draw "none", "row", or "both" dendrograms
## returns:
##  myheatmap: a heatmap.2 object (from 'gplots' package)


library('RColorBrewer')
library('gplots')

# create red-yellow-green palette for heatmap
palette = colorRampPalette(brewer.pal(9,"RdYlGn"))(200)


plotheatmap2 = function(x, scale, title, Rowv = F, dendrogram = "none") {
  myheatmap = 
    heatmap.2(x, 
              Rowv = Rowv, Colv = F, 
              dendrogram = dendrogram,
              scale = scale, 
              margins=c(7,8),
              trace = "none",
              key=T, density.info="none",
              col = heat.colors(30),
              na.color="gray",
              lmat=rbind( c(0, 3), c(2,1), c(0,4) ), 
              lhei=c(1.5, 2, 2 ),
              lwid=c(0.1, 4),
              cexRow = 1,
              cexCol = 0.9,
              main = title)
  return(myheatmap)
}