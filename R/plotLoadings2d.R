## plotLoadings2d.R
## Takes output from a dimensionality reduction function, e.g. prcomp, and make (2d) scatterplot of
## loadings on analytes for two principal components.

## Note: the loadings are actually eigenvectors (not the eigenvectors scaled by their
## corresponding eigenvalue)

## INPUT:
##  -model.name: string, identifying the class of the model, e.g. "prcomp" from a PCA analysis
##  -model: an object returned by a dimensionality reduction function, e.g. "prcomp" returns a list
##  -PCx: integer, the principal component # whose loadings should be plotted on x-axis
##  -PCy: integer, the principal component # whose loadings should be plotted on y-axis
## OUTPUT:
##  -loadingsplot: an object of ggplot

plotLoadings2d = function(model, PCx=1, PCy=2, model.name="prcomp") {
  if(model.name=="prcomp") {
    df = data.frame(analytes = row.names(model$rotation),
                    loadings.x = model$rotation[,PCx],
                    loadings.y = model$rotation[,PCy],
                    analyteidx = seq(nrow(model$rotation)))
    
    #reorder=stats:::reorder.default
    loadingsplot = 
      ggplot(df, aes(x=loadings.x, y=loadings.y)) +#, label=analytes)) +
      geom_hline(aes(yintercept=0), color="yellow", alpha=0.5) +
      geom_vline(aes(xintercept=0), color="yellow", alpha=0.5) +
      geom_point(aes(color=analytes)) +
      geom_text(aes(x=loadings.x-0.005, label=analyteidx)) +
      theme_bw() +
      scale_color_manual(values=rep("red", nrow(df)), labels= paste(df$analyteidx,": ",df$analytes)) +
      theme(legend.key = element_blank()) +
      xlab(paste0("loadings on PC ",PCx)) +
      ylab(paste0("loadings on PC ",PCy)) +
      ggtitle(paste0("loadings on PC ", PCx," and PC ", PCy)) +
      guides(color = guide_legend(override.aes = list(alpha = 0)))# +
      #guides(col = guide_legend(nrow=41))
    
    print(loadingsplot)
  }
}