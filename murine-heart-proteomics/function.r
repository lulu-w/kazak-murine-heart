numCols <- function(x){
  types = sapply(x, class)
  idx = which(types == "numeric")
  x[, idx]
}


panel.lm <- function (x, y, pch = par("pch"), col.lm = "red", ...){ 
  ymin <- min(y)
  ymax <- max(y)
  xmin <- min(x)
  xmax <- max(x)
  ylim <- c(min(ymin,xmin),max(ymax,xmax))
  xlim <- ylim
  points(x, y, pch = pch, ylim = ylim, xlim= xlim, col = rgb(0,0.5,1,0.1), ...)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    abline(lm(y[ok]~ x[ok]),
           col = col.lm, ...)
  }

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
  vertical <- (par("usr")[3] + par("usr")[4]) / 2;
  text(horizontal, vertical, format(abs(cor(x,y, use = "pairwise.complete.obs")), digits=2))
}


######### NOT USED ###############
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y, use = "pairwise.complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
#   Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
#                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#                    symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt) #cex = cex * r) 
#   text(.8, .8, Signif, cex=cex, col=2) 
}
############# NOT USED ##################

pairwiseReg <- function(data, log.trans = T, title = "Pairwise Correlation") {
  if(log.trans) {
    data[data==0] <- NA
    data = log2(data)
  }
  #pairs(data, lower.panel = panel.smooth, upper.panel = panel.cor)
  #pairs(data, main = "", pch = 16, col = rgb(0,0.5,1,0.1), upper.panel=panel.pearson, lower.panel = panel.lm)
  pairs(data, main = title, pch = 16, col = "red", upper.panel=panel.pearson, lower.panel = panel.lm)
}

plotDistribution <- function(data, col, title = ""){
  require(ggplot2)
  require(reshape)
  
  par(mar=c(8.1,6.1,4.1,2.1))
  par(las=2)
  boxplot(log2(data.frame(data)), col = col, names = (colnames(data)), main = paste(title), ylab = "log intensity")
  data.df <- data.frame(id = rownames(data), data, stringsAsFactors = FALSE)
  mdf <- melt(data.df, id = "id", variable_name = "condition")
  
  ggplot(mdf, aes(log2(value))) +
    geom_density(aes(color = condition)) + 
    title(main = paste(title), xlab = "") + 
    scale_colour_manual(values = col) +
    theme_bw() +
    theme(panel.grid.major = element_blank())
}