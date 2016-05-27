## plotScores.R
## Takes output from a dimensionality reduction function, e.g. prcomp, and make (2d) scatterplot of
## loadings on analytes for two principal components.

## Note: the loadings are actually eigenvectors (not the eigenvectors scaled by their
## corresponding eigenvalue)

## INPUT:
##  -model.name: string, identifying the class of the model, e.g. "prcomp" from a PCA analysis
##  -model: an object returned by a dimensionality reduction function, e.g. "prcomp" returns a list
##  -PCx: integer, the principal component # whose loadings should be plotted on x-axis
##  -PCy: integer, the principal component # whose loadings should be plotted on y-axis
##  -meta: optional dataframe consisting of sample information with columns {Cell.Line, Time, Compound}
##    for plotting
## OUTPUT:
##  -scoresplot: an object of ggplot

library("ggplot2")

plotScores = function(model, PCx=1, PCy=2, model.name="prcomp", meta=NULL) {
  
  if(model.name=="prcomp" & !is.null(meta)) {
    
    if(ncol(meta)==3) {
      ## metadata on samples is supplied (for grouping in plot), including time
      df = data.frame(meta,
                      scores.x = model$x[,PCx],
                      scores.y = model$x[,PCy])
      scoresplot = 
        ggplot(df, aes(x=scores.x, y=scores.y, shape=Compound, color=Cell.Line, size=factor(Time))) +
        geom_hline(aes(yintercept=0), color="yellow", alpha=0.5) +
        geom_vline(aes(xintercept=0), color="yellow", alpha=0.5) +
        geom_point() +
        scale_size_discrete(range=c(4,7)) +
        theme_bw() +
        theme(text = element_text(size=20)) +
        xlab(paste0("scores on PC ",PCx)) +
        ylab(paste0("scores on PC ",PCy)) +
        ggtitle(paste0("scores on PC ", PCx," and PC ", PCy))
    } else {
      ## ncol(meta) = 2
      ## metadata on samples is supplied (for grouping in plot), not including time
      df = data.frame(meta,
                      scores.x = model$x[,PCx],
                      scores.y = model$x[,PCy])
      scoresplot = 
        ggplot(df, aes(x=scores.x, y=scores.y, shape=Compound, color=Cell.Line)) +
        geom_hline(aes(yintercept=0), color="yellow", alpha=0.5) +
        geom_vline(aes(xintercept=0), color="yellow", alpha=0.5) +
        geom_point(size=4) +
        theme_bw() +
        theme(text = element_text(size=20)) +
        xlab(paste0("scores on PC ",PCx)) +
        ylab(paste0("scores on PC ",PCy)) +
        ggtitle(paste0("scores on PC ", PCx," and PC ", PCy))
    }
  } else {
    ## no metadata supplied, take sample labels from rownames of model
    df = data.frame(sample = row.names(model$x),
                    scores.x = model$x[,PCx],
                    scores.y = model$x[,PCy],
                    sampleidx = seq(nrow(model$x)))
    scoresplot = 
      ggplot(df, aes(x=scores.x, y=scores.y)) +
      geom_point(aes(color=sample)) +
      geom_text(aes(x=scores.x-0.2, label=sampleidx)) +
      theme_bw() +
      scale_color_manual(values=rep("red", nrow(df)), labels= paste(df$sampleidx,": ",df$sample)) +
      theme(legend.key = element_blank()) +
      xlab(paste0("scores on PC ",PCx)) +
      ylab(paste0("scores on PC ",PCy)) +
      ggtitle(paste0("scores on PC ", PCx," and PC ", PCy)) +
      guides(color = guide_legend(override.aes = list(alpha = 0))) +
      guides(col = guide_legend(nrow=ceiling(nrow(df)/2)))
  }
  print(scoresplot)
}