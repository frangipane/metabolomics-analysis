## plotLoadings.R
## Takes output from a dimensionality reduction function, e.g. prcomp, and make (1d) barplot of
## loadings on analytes for a principal component.

## Note: the loadings are actually eigenvectors (not the eigenvectors scaled by their
## corresponding eigenvalue)

## INPUT:
##  -model.name: string, identifying the class of the model, e.g. "prcomp" from a PCA analysis
##  -model: an object returned by a dimensionality reduction function, e.g. "prcomp" returns a list
##  -PC: integer, the principal component # whose loadings should be plotted
## OUTPUT:
##  -loadingsplot: an object of ggplot

library("ggplot2")

plotLoadings = function(model, PC, model.name="prcomp") {
  if(model.name=="prcomp") {
    df = data.frame(analytes = row.names(model$rotation),
                    loadings = model$rotation[,PC])
    reorder=stats:::reorder.default
    loadingsplot = 
      ggplot(df, aes(x=reorder(analytes,-loadings), y=loadings)) +
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
      xlab("analyte") +
      ylab("loadings") +
      ggtitle(paste0("loadings of PC ", PC))
  
    print(loadingsplot)
    #return(loadingsplot)
  }
}