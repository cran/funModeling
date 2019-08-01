####################################################################
#' Density plot for discrete and continuous values
#'
#' This function plots discrete and continuous values results
#'
#' @family Machine Learning
#' @family Visualization
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Integer. Threshold for selecting binary or regression
#' models: this number is the threshold of unique values we should
#' have in 'tag' (more than: regression; less than: classification)
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_density <- function(tag,
													score,
													thresh = 5,
													model_name = NA,
													subtitle = NA,
													save = FALSE,
													subdir = NA,
													file_name = "viz_distribution.png") {

	if (length(tag) != length(score)) {
		message("The tag and score vectors should be the same length.")
		stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
	}

	if (length(unique(tag)) <= thresh) {

		out <- data.frame(tag = as.character(tag), score = score)

		if (is.numeric(out$score)) {
			if (max(out$score) <= 1) {
				out$score <- score * 100
			}
		}

		p1 <- ggplot(out) +
			geom_density(aes(x = as.numeric(score),
											 group = tag, fill = as.character(tag)),
									 alpha = 0.6, adjust = 0.25) +
			guides(fill = guide_legend(title = "Tag")) +
			labs(title = "Classification Model Results",
					 y = "Density by tag", x = "Score", fill = NULL) +
			theme_lares2(pal = 1) +
			theme(legend.position = "top",
						legend.justification = c(0, 0),
						legend.title = element_blank())

		p2 <- ggplot(out) +
			geom_density(aes(x = score), alpha = 0.9, adjust = 0.25, fill = "deepskyblue") +
			labs(x = NULL, y = "Density") + theme_lares2()

		p3 <- ggplot(out) +
			geom_line(aes(x = as.numeric(score), y = (1 - ..y..), color = as.character(tag)),
								stat = 'ecdf', size = 1) +
			geom_line(aes(x = as.numeric(score), y = (1 - ..y..)),
								stat = 'ecdf', size = 0.5, colour = "black", linetype = "dotted") +
			ylab('Cumulative') + labs(x = NULL) + guides(color = FALSE) + theme_lares2(pal = 2)

		p1 <- p1 + theme(plot.margin = margin(10, 5, 0, 5))
		p2 <- p2 + theme(plot.margin = margin(0, 0, 5, 5))
		p3 <- p3 + theme(plot.margin = margin(0, 5, 5, 0))

		if (!is.na(subtitle)) p1 <- p1 + labs(subtitle = subtitle)

		if (!is.na(model_name)) p1 <- p1 + labs(caption = model_name)

		if (!is.na(subdir)) {
			dir.create(file.path(getwd(), subdir), recursive = TRUE)
			file_name <- paste(subdir, file_name, sep = "/")
		}

		p <- arrangeGrob(p1, p2, p3,
										 ncol = 2, nrow = 2, heights = 2:1,
										 layout_matrix = rbind(c(1,1), c(2,3)))

	} else {

		df <- data.frame(
			rbind(cbind(values = tag, type = "Real"),
						cbind(values = score, type = "Model")))
		df$values <- as.numeric(as.character(df$values))

		p <- ggplot(df) +
			geom_density(aes(x = values, fill = as.character(type)),
									 alpha = 0.6, adjust = 0.25) +
			labs(y = "Density", x = "Continuous values", fill = NULL) +
			guides(colour = FALSE) +
			theme_lares2(pal = 1, legend = "top")

		if (!is.na(model_name)) p <- p + labs(caption = model_name)

		if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)

	}

	if (save) {
		if (!is.na(subdir)) {
			dir.create(file.path(getwd(), subdir), recursive = TRUE)
			file_name <- paste(subdir, file_name, sep = "/")
		}
		p <- p + ggsave(file_name, width = 6, height = 6)
	}

	return(p)
}




####################################################################
#' ROC Curve Plot
#'
#' This function plots ROC Curves with AUC values with 95\% confidence
#' range. It also works for multi-categorical models.
#'
#' @family Machine Learning
#' @family Visualization
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score
#' (only used when more than 2 categories coexist)
#' @param sample Integer. Number of samples to use for rendering plot.
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param interval Numeric. Interval for breaks in plot
#' @param plotly Boolean. Use plotly for plot's output for an interactive plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_roc <- function(tag,
											score,
											multis = NA,
											sample = 1000,
											model_name = NA,
											subtitle = NA,
											interval = 0.2,
											plotly = FALSE,
											save = FALSE,
											subdir = NA,
											file_name = "viz_roc.png") {

	if (is.na(multis)[1]) {
		rocs <- ROC(tag, score)
		ci <- rocs$ci
	} else {
		rocs <- ROC(tag, score, multis)
		ci <- rocs$ci["mean"]
	}
	coords <- rocs$roc

	if (sample < min(table(coords$label))) {
		coords <- coords %>% group_by(label) %>% sample_n(sample)
		message("ROC Curve Plot rendered with sampled data...")
	}

	scale <- function(x) sprintf("%.1f", x)
	p <- ggplot(coords, aes(x = fpr, y = tpr, group = label)) +
		geom_line(colour = "deepskyblue", size = 0.8) +
		geom_point(aes(colour = label), size = 0.7, alpha = 0.8) +
		geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") +
		scale_x_reverse(name = "1 - Specificity [False Positive Rate]", limits = c(1,0),
										breaks = seq(0, 1, interval), expand = c(0.001,0.001),
										labels = scale) +
		scale_y_continuous(name = "Sensitivity [True Positive Rate]", limits = c(0,1),
											 breaks = seq(0, 1, interval), expand = c(0.001, 0.001),
											 labels = scale) +
		coord_equal() +
		theme(axis.ticks = element_line(color = "grey80")) +
		labs(title = "ROC Curve: AUC", colour = NULL) +
		guides(colour = guide_legend(ncol = 3)) +
		annotate("text", x = 0.25, y = 0.10, size = 4.2,
						 label = paste("AUC =", round(100*ci[c(2),],2))) +
		annotate("text", x = 0.25, y = 0.05, size = 2.8,
						 label = paste0("95% CI: ", round(100*ci[c(1),],2),"-",
						 							 round(100*ci[c(3),],2))) +
		theme_lares2(bg_colour = "white", pal = 2, legend = "bottom")

	if (is.na(multis)[1]) p <- p + guides(colour = FALSE)
	if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
	if (!is.na(model_name)) p <- p + labs(caption = model_name)

	if (plotly) {
		try_require("plotly")
		p <- ggplotly(p)
	}

	if (!is.na(subdir)) {
		dir.create(file.path(getwd(), subdir), recursive = TRUE)
		file_name <- paste(subdir, file_name, sep = "/")
	}

	if (save) p <- p + ggsave(file_name, width = 6, height = 6)

	return(p)

}


####################################################################
#' Cuts by quantiles for score plot
#'
#' This function cuts by quantiles any score or prediction
#'
#' @family Machine Learning
#' @family Visualization
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param table Boolean. Do you wish to return a table with results?
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_cuts <- function(score,
											 splits = 10,
											 model_name = NA,
											 subtitle = NA,
											 table = FALSE,
											 save = FALSE,
											 subdir = NA,
											 file_name = "viz_ncuts.png") {

	if (splits > 25) stop("You should try with less splits!")

	deciles <- quantile(score,
											probs = seq((1/splits), 1, length = splits),
											names = TRUE)
	deciles <- data.frame(range = row.names(as.data.frame(deciles)),
												cuts = as.vector(signif(deciles, 6)))
	rownames(deciles) <- NULL

	p <- deciles %>%
		#mutate(label_colours = ifelse(cuts*100 < 50, "1", "m")) %>%
		ggplot(aes(x = reorder(range, cuts), y = cuts * 100)) +
		geom_col(fill = "deepskyblue") +
		xlab('Cumulative volume') + ylab('Score') +
		geom_text(aes(label = round(100 * cuts, 1),
									vjust = ifelse(cuts*100 < 50, -0.3, 1.3)),
							size = 3, colour = "black", inherit.aes = TRUE, check_overlap = TRUE) +
		guides(colour = FALSE) +
		labs(title = paste0("Score cuts (", splits, " equal-sized buckets)")) +
		theme_lares2()

	if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)

	if (!is.na(model_name)) p <- p + labs(caption = model_name)

	if (!is.na(subdir)) {
		dir.create(file.path(getwd(), subdir), recursive = TRUE)
		file_name <- paste(subdir, file_name, sep = "/")
	}

	if (save) p <- p + ggsave(file_name, width = 6, height = 6)

	if(table) {
		return(deciles)
	} else {
		return(p)
	}
}


####################################################################
#' Cuts by quantiles on absolut and percentual errors plot
#'
#' This function cuts by quantiles on absolut and percentual errors
#'
#' @family Machine Learning
#' @family Visualization
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param title Character. Title to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_cuts_error <- function(tag,
														 score,
														 splits = 10,
														 title = NA,
														 model_name = NA,
														 save = FALSE,
														 subdir = NA,
														 file_name = "viz_ncuts_error.png") {

	if (splits > 25) stop("You should try with less splits!")

	if (length(tag) != length(score)) {
		message("The tag and score vectors should be the same length.")
		stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
	}

	df <- data.frame(tag = tag, score = score) %>%
		mutate(real_error = tag - score,
					 abs_error = abs(real_error),
					 p_error = 100 * real_error/tag) %>%
		filter(abs(p_error) <= 150)

	# Useful function
	quants <- function(values, splits = 10, just = 0.3) {
		cuts <- quantile(values,
										 probs = seq((1/splits), 1, length = splits),
										 names = TRUE)
		cuts <- data.frame(deciles = names(cuts), cut = cuts)
		thresh <- max(cuts$cut) / 2
		cuts$gg_pos <- ifelse(cuts$cut > thresh, 1 + just, -just)
		cuts$colour <- ifelse(cuts$gg_pos < 0, "f", "m")
		row.names(cuts) <- NULL
		return(cuts)
	}

	# First: absolute errors
	deciles_abs <- quants(df$abs_error, splits = splits, just = 0.3)
	p_abs <- ggplot(deciles_abs, aes(x = reorder(deciles, cut), y = cut, label = signif(cut, 3))) +
		geom_col(fill = "deepskyblue") +
		xlab('') + ylab('Absolute Error') +
		geom_text(aes(vjust = gg_pos, colour = colour), size = 2.7, inherit.aes = TRUE, check_overlap = TRUE) +
		labs(subtitle = paste("Cuts and distribution by absolute error")) +
		scale_y_continuous(labels = comma) + guides(colour = FALSE) +
		gg_text_customs() + theme_lares2(bg_colour = "white")

	# Second: percentual errors
	deciles_perabs <- quants(abs(df$p_error), splits = splits, just = 0.3)
	p_per <- ggplot(deciles_perabs, aes(x = reorder(deciles, cut), y = cut, label = signif(cut, 3))) +
		geom_col(fill = "deepskyblue") +
		xlab('') + ylab('Percetage Error') +
		geom_text(aes(vjust = gg_pos, colour = colour), size = 2.7, inherit.aes = TRUE, check_overlap = TRUE) +
		labs(subtitle = paste("Cuts and distribution by absolute percentage error")) +
		scale_y_continuous(labels = comma) + guides(colour = FALSE) +
		gg_text_customs() + theme_lares2(bg_colour = "white")

	# Third: errors distribution
	pd_error <- ggplot(df) +
		geom_density(aes(x = p_error), fill = "deepskyblue", alpha = 0.7) +
		xlab('') + ylab('Error Density') +
		scale_x_percent() +
		geom_vline(xintercept = 0, alpha = 0.5, colour = "navy", linetype = "dotted") +
		theme_lares2(bg_colour = "white")

	if (!is.na(title)) p_abs <- p_abs + labs(title = title)

	if (!is.na(model_name)) pd_error <- pd_error + labs(caption = model_name)

	p <- arrangeGrob(
		p_abs, p_per, pd_error,
		heights = c(1.8,1.8,1),
		ncol = 1, nrow = 3)

	if (save) {
		if (!is.na(subdir)) {
			dir.create(file.path(getwd(), subdir), recursive = TRUE)
			file_name <- paste(subdir, file_name, sep = "/")
		}
		png(file_name, height = 1800, width = 1800, res = 300)
		plot(p)
		dev.off()
	}

	return(p)

}


####################################################################
#' Split and compare quantiles plot
#'
#' This function lets us split and compare quantiles on a given prediction to
#' compare different categorical values vs scores grouped by equal sized buckets.
#'
#' @family Machine Learning
#' @family Visualization
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_splits <- function(tag,
												 score,
												 splits = 5,
												 subtitle = NA,
												 model_name = NA,
												 save = FALSE,
												 subdir = NA,
												 file_name = "viz_splits.png") {

	if (length(tag) != length(score)) {
		message("The tag and score vectors should be the same length.")
		stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
	}

	if (splits > 10) stop("You should try with less splits!")

	df <- data.frame(tag, score)
	npersplit <- round(nrow(df)/splits)

	# For continuous tag values
	if (length(unique(tag)) > 6) {
		names <- df %>%
			mutate(tag = as.numeric(tag),
						 quantile = ntile(tag, splits)) %>% group_by(quantile) %>%
			summarise(n = n(),
								max_score = round(max(tag), 1),
								min_score = round(min(tag), 1)) %>%
			mutate(quantile_tag = paste0(quantile," (",min_score,"-",max_score,")"))
		df <- df %>%
			mutate(quantile = ntile(tag, splits)) %>%
			left_join(names, by = c("quantile")) %>% mutate(tag = quantile_tag) %>%
			select(-quantile, -n, -max_score, -min_score)

	} else {
		# For categorical tag values
		names <- df %>%
			mutate(quantile = ntile(score, splits)) %>% group_by(quantile) %>%
			summarise(n = n(),
								max_score = signif(max(score), 2),
								min_score = signif(min(score), 2)) %>%
			mutate(quantile_tag = paste0(quantile," (",
																	 round(100*min_score,1),"-",
																	 round(100*max_score,1),")"))
	}

	p <- df %>%
		mutate(quantile = ntile(score, splits)) %>%
		group_by(quantile, tag) %>% tally() %>%
		ungroup() %>% group_by(tag) %>%
		arrange(desc(quantile)) %>%
		mutate(p = round(100*n/sum(n),2),
					 cum = cumsum(100*n/sum(n))) %>%
		left_join(names, by = c("quantile")) %>%
		ggplot(aes(x = as.character(tag), y = p, label = as.character(p),
							 fill = reorder(as.character(quantile_tag),quantile))) +
		geom_col(position = "stack") +
		geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
		xlab("Tag") + ylab("Total Percentage by Tag") +
		guides(fill = guide_legend(title = paste0("~",npersplit," p/split"))) +
		labs(title = "Tag vs Score Splits Comparison") +
		scale_fill_brewer(palette = "Spectral") +
		theme_lares2()

	if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
	if (!is.na(model_name)) p <- p + labs(caption = model_name)

	if (!is.na(subdir)) {
		dir.create(file.path(getwd(), subdir), recursive = TRUE)
		file_name <- paste(subdir, file_name, sep = "/")
	}

	if (save) p <- p + ggsave(file_name, width = 6, height = 6)

	return(p)

}


####################################################################
#' AUC and LogLoss Plots
#'
#' This function can plot AUC and LogLoss Plots from a h2o_automl results object
#'
#' @family Machine Learning
#' @family Visualization
#' @param results Object. Results object from h2o_automl function
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_metrics <- function(results,
													subtitle = NA,
													model_name = NA,
													save = FALSE,
													subdir = NA,
													file_name = "viz_metrics.png") {

	plots_data <- data.frame(
		trees = results$model@model$scoring_history$number_of_trees,
		train_ll = results$model@model$scoring_history$training_logloss,
		test_ll = results$model@model$scoring_history$validation_logloss,
		train_auc = results$model@model$scoring_history$training_auc,
		test_auc = results$model@model$scoring_history$validation_auc)
	ll <- ggplot(plots_data) +
		geom_hline(yintercept = 0.69315, alpha = 0.5, linetype = 'dotted') +
		geom_line(aes(x = trees, y = train_ll, colour = "Train"), size = 0.5) +
		geom_line(aes(x = trees, y = test_ll, colour = "Test"), size = 1) +
		labs(title = "Logarithmic Loss vs Number of Trees",
				 colour = "Dataset", x = "# of trees", y = "LogLoss") +
		scale_colour_brewer(palette = "Set1") +
		geom_text(aes(x = trees, y = train_ll, colour = "Train",
									label = round(train_ll,2)),
							check_overlap = TRUE, nudge_y = 0.03, size = 3) +
		geom_text(aes(x = trees, y = test_ll, colour = "Test",
									label = round(test_ll, 2)),
							check_overlap = TRUE, nudge_y = 0.03, size = 3) +
		theme_lares2(pal = 1) +
		theme(strip.text.x = element_blank(),
					strip.background = element_rect(colour = "white", fill = "white"),
					legend.position = c(0.1, 0.05))
	au <- ggplot(plots_data) +
		geom_line(aes(x = trees, y = train_auc*100, colour = "Train"), size = 0.5) +
		geom_line(aes(x = trees, y = test_auc*100, colour = "Test"), size = 1) +
		geom_hline(yintercept = 50, alpha = 0.5, linetype = 'dotted', colour = "black") +
		labs(title = "Area Under the Curve vs Number of Trees",
				 colour = "Dataset", x = "# of trees", y = "AUC") +
		scale_colour_brewer(palette = "Set1") + guides(colour = FALSE) +
		geom_text(aes(x = trees, y = train_auc*100, colour = "Train",
									label = round(train_auc*100, 2)),
							check_overlap = TRUE, nudge_y = 3, size = 3) +
		geom_text(aes(x = trees, y = test_auc*100, colour = "Test",
									label = round(test_auc*100,2)),
							check_overlap = TRUE, nudge_y = 3, size = 3) +
		theme_lares2(pal = 1)

	if (!is.na(subtitle)) ll <- ll + labs(subtitle = subtitle)
	if (!is.na(model_name)) au <- au + labs(caption = model_name)

	p <- arrangeGrob(ll, au, ncol = 1, nrow = 2)

	if (save) {

		if (!is.na(subdir)) {
			dir.create(file.path(getwd(), subdir), recursive = TRUE)
			file_name <- paste(subdir, file_name, sep = "/")
		}

		png(file_name, height = 1800, width = 2100, res = 300)
		plot(p)
		dev.off()
	}

	return(plot(p))

}


####################################################################
#' Linear Regression Results Plot
#'
#' This function plots a Linear Regression Result
#'
#' @family Machine Learning
#' @family Visualization
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_lineal <- function(tag,
												 score,
												 subtitle = NA,
												 model_name = NA,
												 save = FALSE,
												 subdir = NA,
												 file_name = "viz_lineal.png") {

	if (length(tag) != length(score)) {
		message("The tag and score vectors should be the same length.")
		stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
	}

	results <- data.frame(tag = tag, score = score, dist = 0)
	results <- results[complete.cases(results), ]

	for (i in 1:nrow(results)) {
		results$dist[i] <- dist2d(c(results$tag[i],results$score[i]), c(0,0), c(1,1))
	}

	fit <- lm(results$score ~ results$tag)
	labels <- paste(
		paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4)),
		#paste("Pval =", signif(summary(fit)$coef[2,4], 3)),
		paste("RMSE =", signif(rmse(results$tag, results$score), 4)),
		paste("MAE =", signif(mae(results$tag, results$score), 4)),
		sep = "\n")

	p <- ggplot(results, aes(x = tag, y = score, colour = dist)) +
		geom_point() + theme_lares2() +
		labs(title = "Regression Model Results",
				 x = "Real value", y = "Predicted value",
				 colour = "Deviation") +
		annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -0.05, label = labels, size = 2.8) +
		scale_x_continuous(labels = comma) +
		scale_y_continuous(labels = comma) +
		scale_colour_continuous(labels = comma) +
		theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
		guides(colour = guide_colorbar(barwidth = 0.9, barheight = 4.5))

	# Draw reference line for correlation
	intercept <- summary(fit)$coefficients[1]
	slope <- summary(fit)$coefficients[2]
	p <- p + geom_abline(slope = slope, intercept = intercept,
											 alpha = 0.5, colour = "orange", size = 0.6)

	if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
	if (!is.na(model_name)) p <- p + labs(caption = model_name)
	if (save) p <- p + ggsave(file_name, width = 6, height = 6)

	return(p)

}


####################################################################
#' MPLOTS Score Full Report Plots
#'
#' This function plots a whole dashboard with a model's results. It will automatically
#' detect if it's a categorical or regression's model by checking how many different
#' unique values the independent variable (tag) has.
#'
#' @family Machine Learning
#' @family Visualization
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result. Must be numeric
#' for categorical binomial models and continuous regression models; must
#' be categorical for multi-categorical models (also need multis param).
#' @param multis Data.frame. Containing columns with each category score
#' (only used when more than 2 categories coexist)
#' @param splits Integer. Numer of separations to plot
#' @param thresh Integer. Threshold for selecting binary or regression
#' models: this number is the threshold of unique values we should
#' have in 'tag' (more than: regression; less than: classification)
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param plot Boolean. Plot results? If not, plot grid object returned
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @author Bernardo Lares
#' @export
mplot_full <- function(tag,
											 score,
											 multis = NA,
											 splits = 8,
											 thresh = 6,
											 subtitle = NA,
											 model_name = NA,
											 plot = TRUE,
											 save = FALSE,
											 subdir = NA,
											 file_name = "viz_full.png") {

	if (length(tag) != length(score)) {
		message("The tag and score vectors should be the same length.")
		stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
	}


	# Categorical Binomial Models
	if (length(unique(tag)) == 2 & is.numeric(score)) {

		p1 <- mplot_density(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
		p2 <- mplot_splits(tag = tag, score = score, splits = splits) +
			theme(plot.margin = margin(10, 8, 5, 0))
		p3 <- mplot_roc(tag = tag, score = score) +
			theme(plot.margin = margin(0, 8, 5, 0))
		p4 <- mplot_cuts(score = score) +
			theme(plot.margin = margin(-3, 0, 5, 8))


		p <- arrangeGrob(
			p1, p2, p3, p4,
			widths = c(1.3,1),
			layout_matrix = rbind(c(1,2), c(1,2),
														c(1,3), c(4,3)))
	}

	# Multi-Categorical Models
	if (length(unique(tag)) > 2 & length(unique(tag)) <= thresh) {
		m <- model_metrics(tag, score, multis)
		p <- arrangeGrob(
			m$plots$conf_matrix +
				labs(title = "Confusion Matrix",
						 caption = if (!is.na(model_name)) model_name),
			m$plots$ROC,
			ncol = 2, nrow = 1)
	}

	# Regression Continuous Models
	if (is.numeric(tag) & is.numeric(score) & length(unique(tag)) > thresh) {

		p1 <- mplot_lineal(tag = tag, score = score, subtitle = subtitle, model_name = model_name) +
			theme_lares2(bg_colour = "white")
		p2 <- mplot_density(tag = tag, score = score)
		p3 <- mplot_cuts_error(tag = tag, score = score, splits = splits)

		p <- arrangeGrob(
			p1, p2, p3,
			heights = c(0.6, 0.4),
			widths = c(0.45, 0.55),
			layout_matrix = rbind(c(1,3), c(2,3)))
	}

	if (save) {
		if (!is.na(subdir)) {
			dir.create(file.path(getwd(), subdir), recursive = TRUE)
			file_name <- paste(subdir, file_name, sep = "/")
		}
		png(file_name, height = 2000, width = 3200, res = 300)
		plot(p)
		dev.off()
	}

	if (plot) plot(p) else return(p)

}




####################################################################
#' lares Theme for ggplot2
#'
#' Based on hrbrthemes' theme_ipsum and customized for lares' use.
#'
#' @md
#' @section Why Arial Narrow?:
#' First and foremost, Arial Narrow is generally installed by default or readily
#' available on any modern system, so it's "free"-ish; plus, it is a condensed font
#' with solid default kerning pairs and geometric numbers.
#'
#' @md
#' @family Visualization
#' @param font,base_size Character and numeric. Base font family and size
#' @param main_colour,hard_colour,soft_colour,bg_colour
#' Character. Main colours for your theme
#' @param legend Character. Legend position: top, right, bottom, left
#' @param mg Numeric. External margin
#' @param pal Integer. 1 for fill and colour palette, 2 for only colour palette,
#' 3 for personal labels-colour palette. 0 or else for nothing.
#' @author Bernardo Lares
#' @export
theme_lares2 <- function(font = "Arial Narrow",
												 base_size = 12.5,
												 main_colour = "darkorange3",
												 hard_colour = "black",
												 soft_colour = "grey30",
												 bg_colour = "white",
												 legend = "right",
												 mg = 15,
												 pal = 0) {

	# Start from theme_minimal()
	ret <- theme_minimal(base_family = font, base_size = base_size)

	# Set default font
	ret <- ret + theme(text = element_text(family = font))

	# Set some defaults
	update_geom_defaults("text", list(colour = hard_colour, family = font))
	update_geom_defaults("label", list(colour = hard_colour, family = font))
	update_geom_defaults("point", list(colour = hard_colour, alpha = 0.95))
	update_geom_defaults("line", list(colour = hard_colour, alpha = 0.95))
	update_geom_defaults("area", list(fill = main_colour, alpha = 0.95))
	update_geom_defaults("rect", list(fill = main_colour, alpha = 0.95))
	update_geom_defaults("density", list(fill = main_colour, alpha = 0.95))
	update_geom_defaults("bar", list(fill = main_colour, alpha = 0.95))
	update_geom_defaults("col", list(fill = main_colour, alpha = 0.95))
	update_geom_defaults("boxplot", list(fill = main_colour, alpha = 0.9))
	#update_geom_defaults("text_repel", list(family = font))

	## USING ASSIGN - IMPROVE:
	# envir <- as.environment(1)
	# assign("scale_x_continuous", function(..., labels = scales::comma) scale_x_continuous(..., labels = labels), envir = envir)
	# assign("scale_y_continuous", function(..., labels = scales::comma) scale_y_continuous(..., labels = labels), envir = envir)
	# assign("scale_colour_discrete", function(..., values = as.vector(colours_pal)) scale_colour_manual(..., values = values), envir = envir)
	# assign("scale_fill_discrete", function(..., values = names(colours_pal)) scale_fill_manual(..., values = values), envir = envir)
	# assign("scale_colour_continuous", function(..., low = names(colours_pal)[2], high = names(colours_pal)[1], na.value = soft_colour) scale_colour_gradient(..., low = low, high = high, na.value = na.value), envir = envir)
	# assign("scale_fill_continuous", function(...,low = names(colours_pal)[2], high = names(colours_pal)[1], na.value = soft_colour) scale_colour_gradient(..., low = low, high = high, na.value = na.value), envir = envir)
	# assign("ggsave", function(..., bg = bg_colour) ggsave(..., bg = bg), envir = envir)

	if (inherits(grid, "character")) {
		grid_col <- "#CCCCCC"
		ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
		ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
		ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))
		if (inherits(grid, "character")) {
			if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
			if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
			if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
			if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
		}
	}

	xj <- switch(tolower(substr("left", 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
	yj <- switch(tolower(substr("left", 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

	# Axis lines
	ret <- ret + theme(axis.line = element_blank())
	# Axis ticks
	ret <- ret + theme(axis.ticks = element_blank(),
										 axis.ticks.x = element_blank(),
										 axis.ticks.y = element_blank())
	# Axis text
	ret <- ret + theme(axis.text.x = element_text(
		size = base_size * 0.85, margin = margin(t = 0), colour = soft_colour))
	ret <- ret + theme(axis.text.y = element_text(
		size = base_size * 0.85, margin = margin(r = 0), colour = soft_colour))
	# Axis titles
	ret <- ret + theme(axis.title = element_text(
		size = base_size * 1, family = font,colour = soft_colour))
	ret <- ret + theme(axis.title.x = element_text(
		hjust = xj, size = base_size * 1, family = font, face = "bold", colour = soft_colour))
	ret <- ret + theme(axis.title.y = element_text(
		hjust = yj, size = base_size * 1, colour = soft_colour,family = font, face = "bold"))
	ret <- ret + theme(axis.title.y.right = element_text(
		hjust = yj, size = base_size * 1, angle = -90, colour = soft_colour, family = font, face = "bold"))
	# facet_grid
	ret <- ret + theme(strip.text = element_text(
		hjust = 0, size = base_size * 1, colour = soft_colour, face = "bold", family = font))
	ret <- ret + theme(panel.spacing = grid::unit(0.8, "lines"))
	# Plot title
	ret <- ret + theme(plot.title = element_text(
		hjust = 0, size = base_size * 1.3, margin = margin(b = base_size * 0.85),
		family = font, face = "bold", color = "black"))
	# Plot subtitle
	ret <- ret + theme(plot.subtitle = element_text(
		hjust = 0, size = base_size * 1, colour = soft_colour,
		margin = margin(b = base_size * 0.7),family = font, face = "italic"))
	# Caption
	ret <- ret + theme(plot.caption = element_text(
		hjust = 1, size = base_size * 0.85, margin = margin(t = base_size * 0.9),
		family = font, face = "bold", color = soft_colour))
	# Legend
	ret <- ret + theme(legend.title = element_text(
		color = soft_colour, size = base_size * 0.9, face = "bold"),
		legend.position = legend,
		legend.justification = c(ifelse(legend %in% c("top","bottom"),0,.5),
														 ifelse(legend == "top",0,.5)),
		legend.margin = margin(-3,0,-5,0),
		legend.key.size = unit(0.4, "cm"))
	# Background
	ret <- ret + theme(
		panel.background = element_rect(fill = "#F1F1F1", colour = NA),
		plot.background = element_rect(fill = bg_colour, colour = NA))
	# External margins
	ret <- ret + theme(plot.margin = margin(mg, mg, mg, mg))

	# Axis scales
	# p <- ggplot(dft, aes(x=Survived, y=Age)) + geom_density()
	# which <- data.frame(p$labels)
	# list <- vector2text(unlist(which), sep = "|", quotes = FALSE)
	# df <- p$data %>% select(matches(list))
	# classes <- data.frame(lapply(df, class))

	# if (grepl("x",tolower(comma))) ret <- ret + scale_x_comma()
	# if (grepl("y",tolower(comma))) ret <- ret + scale_y_comma()
	# if (grepl("x",tolower(percent))) ret <- ret + scale_x_percent()
	# if (grepl("y",tolower(percent))) ret <- ret + scale_y_percent()

	# Palette with fills and colour
	if (pal == 1) {
		colours_pal <- lares_pal()$palette
		ret <- list(ret, scale_fill_manual(values = names(colours_pal)))
		ret <- list(ret, scale_colour_manual(values = as.vector(colours_pal)))
	}
	# Palette without fills
	if (pal == 2) {
		colours_pal <- lares_pal()$palette
		ret <- list(ret, scale_colour_manual(values = names(colours_pal)))
	}
	# Custom Palette Colours
	if (pal == 3) {
		colours_pal <- lares_pal()$labels
		scale_fill_lares <- function(){
			values <- as.character(colours_pal$values)
			names(values) <- colours_pal$fill
			structure(list(scale_fill_manual(values = values)))
		}
		scale_colour_lares <- function(){
			values <- as.character(colours_pal$values)
			names(values) <- colours_pal$colour
			structure(list(scale_color_manual(values = values)))
		}
		ret <- c(ret, scale_fill_lares(), scale_colour_lares())
	}

	return(ret)

}

####################################################################
#' Personal Colours Palette
#'
#' This function plots a list of colours on a specific palette
#'
#' @family Auxiliary
#' @author Bernardo Lares
#' @export
lares_pal <- function() {

	black <- "#000000"
	white <- "#FFFFFF"
	red <- "tomato"
	green <- "#3DA4AB"
	pink <- "lightpink2"
	blue <- "#0E9AA7"
	orange <- "#FE8A71"
	grey <- "azure4"

	colours_list <- data.frame(rbind(
		c("allianz", "#0038A8", black),
		c("equidad", "#52CF44", black),
		c("colpatria", "#EE0606", black),
		c("del estado", "#F37000", black),
		c("suramericana", "#1F6D8C", black),
		c("mapfre", "#34000D", black),
		c("la previsora", "#6F9A45", black),
		c("aig", "#C71585", black),
		c("generali", "#B21F1F", black),
		c("solidaria", "#E69500", black),
		c("liberty", "#4E629A", black),
		c("bolivar", "#F0F206", black),
		c("cia", "#8ACBE5", black),
		c("mundial", "#8ACBE5", black),
		c("puntored", "#FFFF00", grey),
		c("movilred", "#FF1493", black),
		c("moviired", "#FF1493", black),
		c("web", "#290452", white),
		c("somosf1", "#290452", white),
		c("f1", "#290452", white),
		c("funnel-soat4_desktop", "#290452", white),
		c("funnel-ujk2d_desktop", "#8ACBE5", black),
		c("funnel-ujk2m_old_mobile", "#7AC4E1", black),
		c("red", black, black),
		c("m", blue, white),
		c("f", pink, black),
		c("male", blue, white),
		c("female", pink, black),
		c("hombre", blue, white),
		c("mujer", pink, black),
		c("true", green, white),
		c("false", red, black),
		c("TRUE", green, white),
		c("FALSE", red, black),
		c("1", green, white),
		c("0", red, black),
		c("good", green, white),
		c("bad", red, black),
		c("bueno", green, white),
		c("malo", red, black),
		c("spring", green, white),
		c("summer", red, white),
		c("fall", orange, black),
		c("winter", blue, black),
		c("meg1", "071D49", white),
		c("meg2", "EBB600", black),
		c("meg3", "F2F1F0", black),
		c("meg4", "9A9A9A", white),
		c("r5", "#290452", white),
		c("olx", green, black),
		c("virtualllantas", red, black),
		c("eltiempo", "#E5E5E5", black),
		c("autolab", orange, black),
		c("aflore", blue, black),
		c("otro", grey, black),
		c("negative", "#FA4113", black),
		c("fear", "#810806", white),
		c("disgust", "#BF200E", black),
		c("anger", "#FE9B13", black),
		c("sadness", "#838B8B", black),
		c("anticipation", "#FE8A71", black),
		c("surprise", "#F7E565", black),
		c("trust", "#40A4D8", black),
		c("joy", "#BD116F", black),
		c("positive", "#3DA4AB", black)
	))
	colnames(colours_list) <- c("values","fill","colour")

	colours_names <- c(
		"#EBB600" = black,
		"#40A4D8" = black,
		"#5D3A9B" = white,
		"#7ADf90" = black,
		"#D35FB7" = black,
		"#DC3220" = black,
		"#F29595" = black,
		"#32CD32" = black,
		"#2FC9FE" = black,
		"#2FFECC" = black,
		"#290452" = white,
		"#0C7BDC" = black,
		"#817B7B" = black,
		"#F66320" = black,
		"#FEFE95" = black,
		"#005AB5" = white,
		"#9A9A9A" = black,
		"#00008B" = white,
		"#E1BE6A" = black,
		"#40B0A6" = black,
		"#056E00" = white,
		"#E40000" = black,
		"#FE8A71" = black,
		"#8600A1" = white,
		"#A52A2A" = white,
		"#000000" = white,
		"#E69F00" = black,
		"#009E73" = black,
		"#0072B2" = white,
		"#D55E00" = black)

	pal <- list(labels = colours_list, palette = rep(colours_names, 4))
	return(pal)
}

# plot_palette(names(colours_names), colours_names)
# plot_palette(colours_list$fill, colours_list$colour, colours_list$values)

####################################################################
#' Plot Palette Colours
#'
#' This function plots a list of colours
#'
#' @family Auxiliary
#' @param fill Vector. List of colours for fills
#' @param colour Vector. List of colours for colours
#' @param id Vector. ID for each color
#' @author Bernardo Lares
#' @export
plot_palette <- function(fill, colour = "black", id = NA) {

	if (is.na(id)) id <- 1:length(fill)

	data.frame(fill = fill,
						 colour = colour,
						 id = id) %>%
		ggplot(aes(x = reorder(fill, -id), y = 1)) +
		geom_bar(aes(fill = fill), stat = "identity", position = "dodge") +
		geom_text(aes(colour = colour, label = id), hjust = 1.5) +
		scale_fill_identity() +
		scale_colour_identity() +
		coord_flip() + labs(x = "Colours", y = NULL) +
		guides(fill = FALSE, colour = FALSE) +
		theme_lares2() +
		theme(axis.title.x = element_blank(),
					axis.text.x = element_blank(),
					axis.ticks.x = element_blank())
}


####################################################################
#' Theme for ggplot2
#'
#' This function sets some default values into ggplot2 outputs. This
#' function is almost as using gg_colour_customs() + gg_fill_customs()
#' + gg_text_customs = but these won't be mantained any longer.
#'
#' This function is lo longer mantained: use theme_lares2() instead!
#'
#' @family Visualization
#' @param labels Boolean. Add labels to plot?
#' @param colours Boolean. Personalize colour palettes?
#' @param cont Boolean. Is the value continuous? Discrete by default
#' @param xcommas Boolean. Nice format for x continuous values?
#' @param ycommas Boolean. Nice format for y continuous values?
#' @author Bernardo Lares
#' @export
theme_lares <- function(labels = FALSE, colours = TRUE, cont = FALSE,
												xcommas = FALSE, ycommas = FALSE) {

	r <- list(theme_minimal())

	if (labels) {
		r <- c(r, geom_label(show.legend = FALSE, size = 3))
	}

	if (xcommas) {
		r <- c(r, scale_x_continuous(labels = comma))
	}
	if (ycommas) {
		r <- c(r, scale_y_continuous(labels = comma))
	}

	if (colours) {

		colours_list <- lares_pal()$labels

		scale_fill_lares <- function(){
			values <- as.character(t(colours_list$fill)[1,])
			names(values) <- colours_list$values
			structure(list(scale_fill_manual(values = values)))
		}

		scale_colour_lares <- function(){
			values <- as.character(t(colours_list$colour)[1,])
			names(values) <- colours_list$values
			structure(list(scale_color_manual(values = values)))
		}

		if (cont) {
			scale_colour_lares <- function(){
				pal_lares <- lares_pal()$palette
				structure(list(scale_color_gradientn(colours = pal_lares)))
			}
		}
		r <- c(r, scale_fill_lares(), scale_colour_lares())
	}

	return(r)

}


####################################################################
#' Custom colours to use in ggplot as scale_color_manual
#'
#' This function lets the user use pre-defined default colours
#'
#' @family Auxiliary
#' @author Bernardo Lares
#' @export
gg_colour_customs <- function () {

	colours_list <- lares_pal()$labels
	values <- as.character(t(colours_list$colour)[1,])
	names(values) <- colours_list$values

	return(scale_color_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_fill_manual
#'
#' This function lets the user use pre-defined default colours
#'
#' @family Auxiliary
#' @author Bernardo Lares
#' @export
gg_fill_customs <- function () {

	colours_list <- lares_pal()$labels
	values <- as.character(t(colours_list$fill)[1,])
	names(values) <- colours_list$values

	return(scale_fill_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_color_manual on texts
#'
#' This function lets the user use pre-defined default colours
#'
#' @family Auxiliary
#' @author Bernardo Lares
#' @export
gg_text_customs <- function() {

	colours_list <- lares_pal()$labels
	values <- as.character(t(colours_list$colour)[1,])
	names(values) <- colours_list$values

	return(scale_color_manual(values = values))
}

####################################################################
#' ROC Curves
#'
#' This function calculates ROC Curves and AUC values with 95\% confidence
#' range. It also works for multi-categorical models.
#'
#' @family Machine Learning
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score
#' (only used when more than 2 categories coexist)
#' @author Bernardo Lares
#' @export
ROC <- function(tag, score, multis = NA) {

	# require(pROC)

	if (length(tag) != length(score)) {
		message("The tag and score vectors should be the same length.")
		stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
	}

	if (!is.numeric(score) & is.na(multis)[1]) {
		score <- as.numeric(score)
		warning("You should use the multis parameter to add each category's score")
	}

	if (is.na(multis)[1]) {
		roc <- pROC::roc(tag, score, ci = TRUE, quiet = TRUE)
		coords <- data.frame(
			fpr = rev(roc$specificities),
			tpr = rev(roc$sensitivities)) %>%
			mutate(label = "2cats")
		ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))
	} else {
		df <- data.frame(tag = tag, score = score, multis)
		cols <- colnames(df)
		coords <- c(); rocs <- list()
		for (i in 1:(length(cols) - 2)) {
			which <- colnames(df)[2 + i]
			res <- df[,c(which)]
			label <- ifelse(df[,1] == which, which, "other")
			roci <- pROC::roc(label, res, ci = TRUE, quiet = TRUE)
			rocs[[paste(cols[i + 2])]] <- roci
			iter <- data.frame(fpr = rev(roci$specificities),
												 tpr = rev(roci$sensitivities),
												 label = paste(round(100*roci$auc,2), which, sep = "% | "))
			coords <- rbind(coords, iter)
		}
		ci <- data.frame(lapply(rocs, "[[", "ci")) %>% mutate(mean = rowMeans(.))
		row.names(ci) <- c("min","AUC","max")
	}
	ret <- list(ci = ci, roc = coords)
	if (!is.na(multis)[1]) {
		ret[["rocs"]] <- rocs
	}
	return(ret)
}


####################################################################
#' Distance from specific point to line
#'
#' This function lets the user calculate the mathematical linear distance
#' Between a specific point and a line (given geometrical 3 points)
#'
#' @family Calculus
#' @param a Vector. Coordinates of the point from which we want to
#' measure the distance
#' @param b Vector. Coordinates of 1st point over the line
#' @param c Vector. Coordinates of 2st point over the line
#' @author Bernardo Lares
#' @export
dist2d <- function(a, b = c(0, 0), c = c(1, 1)) {
	v1 <- b - c
	v2 <- a - b
	m <- cbind(v1, v2)
	d <- abs(det(m)) / sqrt(sum(v1 * v1))
	return(d)
}


####################################################################
#' Root Mean Squared Error (RMSE)
#'
#' This function lets the user calculate Root Mean Squared Error
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @author Bernardo Lares
#' @export
rmse <- function(tag, score){
	error <- tag - score
	sqrt(mean(error^2))
}


####################################################################
#' Mean Absolute Error (MAE)
#'
#' This function lets the user calculate Mean Absolute Error
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @author Bernardo Lares
#' @export
mae <- function(tag, score){
	error <- tag - score
	mean(abs(error))
}


####################################################################
#' Mean Squared Error (MSE)
#'
#' This function lets the user calculate Mean Squared Error
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @author Bernardo Lares
#' @export
mse <- function(tag, score){
	error <- tag - score
	mean(error^2)
}


####################################################################
#' Mean Absolute Percentage Error (MAPE)
#'
#' This function lets the user calculate Mean Squared Error
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @author Bernardo Lares
#' @export
mape <- function(tag, score){
	error <- (tag - score) / tag
	error <- error[!is.infinite(error)]
	tag <- tag[tag != 0]
	mean(abs(error/tag))
}


####################################################################
#' R Squared
#'
#' This function lets the user calculate r squared
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @author Bernardo Lares
#' @export
rsq <- function(tag, score){
	fit <- lm(score ~ tag)
	signif(summary(fit)$r.squared, 4)
}

####################################################################
#' Adjusted R Squared
#'
#' This function lets the user calculate adjusted r squared
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @author Bernardo Lares
#' @export
rsqa <- function(tag, score){
	fit <- lm(score ~ tag)
	signif(summary(fit)$adj.r.squared, 4)
}


####################################################################
#' Calculate Errors
#'
#' This function lets the user calculate all errors and R squared
#' simultaneously.
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
#' @author Bernardo Lares
errors <- function(tag, score){
	data.frame(
		rmse = rmse(tag, score),
		mae = mae(tag, score),
		mape = mape(tag, score),
		mse = mse(tag, score),
		rsq = rsq(tag, score),
		rsqa = rsqa(tag, score)
	)
}

####################################################################
#' Axis scales format
#'
#' The `_comma` ones set comma format for axis text and the `_percent`
#' ones set percent format for axis text.
#'
#' @inheritDotParams ggplot2::continuous_scale -expand -position
#' @export
scale_x_comma <- function(...) scale_x_continuous(..., labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function(...) scale_y_continuous(..., labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_x_percent <- function(...) scale_x_continuous(..., labels = scales::percent)

#' @rdname scale_x_comma
#' @export
scale_y_percent <- function(...) scale_y_continuous(..., labels = scales::percent)
