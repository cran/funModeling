#' funModeling: Exploratory data analysis, data preparation and model performance
#'
#' funModeling is intimately related to the Data Science Live Book -Open Source- (2017) in the sense that most
#' of its functionality is used to explain different topics addressed by the book.
#'
#' To start using funModeling you can start by the vignette:
#' `browseVignettes(package = "funModeling")`
#'
#' Or you can read the Data Science Live Book, fully accessible at: \url{https://livebook.datascienceheroes.com}
#'
#'

#' @importFrom grDevices dev.off jpeg rainbow
#' @importFrom graphics abline grid
#' @importFrom stats predict frequency
#' @importFrom pander pandoc.table
#' @importFrom  Hmisc cut2
#' @import ggplot2
#' @import dplyr
#' @importFrom reshape2 dcast melt
#' @importFrom utils packageVersion
#' @importFrom scales percent comma
#' @importFrom lazyeval interp
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom ROCR prediction performance plot
#' @importFrom stats cor quantile
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom stats kmeans rbeta hclust cutree dist IQR na.omit sd mad median
#' @importFrom utils head tail
#' @importFrom moments skewness kurtosis
#' @importFrom entropy entropy
#' @importFrom pROC roc ci
"_PACKAGE"

utils::globalVariables(names=c("fum","element_blank","value","ratio","aes","variable","geom_bar","geom_text","position",
															 "guides","labs","theme","element_text","scale_y_continuous","position_dodge","ylim","guide_legend","scale_fill_discrete",
															 "aes_string", "geom_boxplot","stat_summary", "theme_bw", "freq", "geom_vline", "geom_density", "margin",
															 "scale_colour_continuous",'Var1','label','coord_flip','ylab','xlab','geom_label','unit','Population','Gain',
															 'Score.Point','geom_line','geom_point','xlim','geom_segment','Lift', 'Freq', 'sum_pos', 'likelih','.','one_of',
															 'grp_mean', 'mean_target',"'colorRampPalette","head","tail","rbeta","p_10","p_90","sd" ,"std_dev","variation_coef",
															 "iqr", "type","gr","discretize_bins", "cuts","..y..", "colour", "complete.cases", "deciles",
															 "fpr", "gg_pos", "ggplotly", "lm", "max_score", "min_score", "model_metrics", "p_error", "png",
															 "quantile_tag", "real_error", "reorder", "test_auc", "test_ll", "tpr", "train_auc", "train_ll",
															 "trees", "try_require", "values"), package = "funModeling", add = F)
