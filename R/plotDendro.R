## plotDendro.R
## PLOT DENDROGRAMS
## 
## INPUT: 
##  -dendro: an object of class hclust
##  -groups: a vector of group labels of the objects clustered in dendro
##  -plottitle: a string, the title of the dendrogram
## OUTPUT: returns a dendrogram ggplot


library('ggdendro')
library('ggplot2')

plotDendro = function(dendro, groups, plottitle="dendrogram", textsize=5) {
  # Convert cluster object to plot with ggplot.
  dendro.gg = dendro_data(dendro, type="rectangle")
  
  dendro.gg$labels$order = factor(groups[dendro$order])
  
  # plot horizontal dendrogram
  dendroplot = 
    ggplot() +
    geom_segment(data=segment(dendro.gg), 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    #geom_text(data=dendro.gg$labels, 
    #         aes(x=x, y=y, label=label, hjust=0, 
    #            col=temp), size=4) +
    geom_text(data=dendro.gg$labels, 
              aes(x=x, y=y, label=label, hjust=0, col=order), size=textsize) +
    coord_flip() + 
    scale_y_reverse(expand=c(0.2,0)) +
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank(),
          legend.position="none",
#           plot.margin = unit(c(0.5,0.5,1,1), "cm")) +
          plot.margin = unit(c(0.5,1.5,0.2,0.2), "cm")) +
    ggtitle(plottitle)
  
  return(dendroplot)
}