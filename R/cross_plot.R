#' @title Cross-plotting input variable vs. target variable
#' @description The cross_plot shows how the input variable is correlated with the target variable, getting the likelihood rates for each input's bin/bucket .
#' @param data data frame source
#' @param input input variable name (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param target variable name to predict
#' @param path_out path directory, if it has a value the plot is saved
#' @param auto_binning indicates the automatic binning of input variable based on equal frequency (function 'equal_freq'), default value=TRUE
#' @param plot_type indicates if the output is the 'percentual' plot, the 'quantity' or 'both' (default).
#' @examples
#' \donttest{
#' ## Example 1:
#' cross_plot(data=heart_disease, input="chest_pain", target="has_heart_disease")
#'
#' ## Example 2: Disabling auto_binning:
#' cross_plot(data=heart_disease, input="oldpeak",
#' 		target="has_heart_disease", auto_binning=FALSE)
#'
#' ## Example 3: Saving the plot into a folder:
#' #cross_plot(data=heart_disease, input="oldpeak",
#' #		target="has_heart_disease", path_out = "my_folder")
#'
#' ## Example 4: Running with multiple input variables at the same time:
#' cross_plot(data=heart_disease, input=c("age", "oldpeak", "max_heart_rate"),
#' 		target="has_heart_disease")
#'}
#' @return cross plot
#' @export
cross_plot <- function(data, input, target, path_out, auto_binning, plot_type='both')
{
	data=as.data.frame(data)

	## Handling missing parameters
  if(missing(auto_binning)) auto_binning=NA
  if(missing(path_out)) path_out=NA

  ## If input then runs for all variables
  if(missing(input))
	{
		## Excluding target variable
  	input=colnames(data)
		input=input[input!=target]
	}

	## Iterator
  for(i in 1:length(input))
  {
    cross_plot_logic(data = data, input=input[i], target=target, path_out = path_out, auto_binning, plot_type)
  }


}



cross_plot_logic <- function(data, input, target, path_out, auto_binning, plot_type)
{
	# data=heart_disease; input="max_heart_rate"; target="has_heart_disease"; auto_binning=TRUE
	  check_target_existence(data, target=target)

		data=remove_na_target(data, target=target)

		check_target_2_values(data, target=target)

		if(!(plot_type %in% c('both','percentual', 'quantity')))
			stop("Value for 'plot_type' is not valid: available values: 'both', 'percentual' or 'quantity'")

	  ## Initial assignments
	  varTarget=data[[as.character(target)]]
	  varInput=data[[as.character(input)]]

	  q_unique_input_values=length(unique(varInput))


	  ## Auto binning #############################
	  if(is.numeric(varInput))
	  {
		  if(!is.na(auto_binning) & auto_binning )
		  {
		    message(sprintf("Plotting transformed variable '%s' with 'equal_freq', (too many values). Disable with 'auto_binning=FALSE'", input))
		    varInput=suppressWarnings(equal_freq(varInput, 10))
		  }

	  	if(is.na(auto_binning) & q_unique_input_values>20)
		  {
		    message(sprintf("Plotting transformed variable '%s' with 'equal_freq', (too many values). Disable with 'auto_binning=FALSE'", input))
		    varInput=suppressWarnings(equal_freq(varInput, 10))
		  }
	  } else {
	  	if(q_unique_input_values>50)
	  		stop(sprintf('Skipping "%s" variable: more than 50 unique values.', input))
	  }
	  #############################################

	  ## Infer the less representative class (commonly the one to predict)
	  df_target=data.frame(target=varTarget)
	  dcount=group_by(df_target, target) %>% summarise(freq=n()) %>% arrange(freq)
	  ## Converting factors to character
	  dcount=data.frame(lapply(dcount, as.character), stringsAsFactors=FALSE)
	  posClass=dcount[1,1]
	  negClass=dcount[2,1]

	  dataCast = dcast(data,varInput~varTarget,fun.aggregate=length, value.var = target)

	  ## Melt data for ggplot
	  dataMelt=melt(dataCast, measure.vars = c(posClass,negClass))

	  ## Converting target into factor
	  dataMelt$variable=factor(dataMelt$variable, c(posClass,negClass))

	  ## Getting percentage numbers
	  m1=group_by(dataMelt, varInput) %>% mutate(ratio = value/sum(value)) %>% select(varInput, ratio) %>% arrange(varInput)

	  ## Order by var input
	  m2 = dataMelt[order(dataMelt$varInput ),]
	  dataGrafPrep=data.frame(m2,ratio=m1$ratio)

	  ## Generating Id indicator for odd/even calculus
	  rownames(dataGrafPrep) = 1:nrow(dataGrafPrep)

	  # Computing if the row is odd/even
	  dataGrafPrep$fum=as.numeric(as.numeric(rownames(dataGrafPrep)) %% 2 == 0)

	  ## Computing middle position in each sub bar
	  dataGrafPrep=group_by(dataGrafPrep, varInput) %>% mutate(position = 0.5*ratio+fum*(sum(value)-value)/sum(value))

	  lGraf=list()

	  ## set factor to print correct bars
	  dataGrafPrep$variable=factor(dataGrafPrep$variable, levels = c(negClass,posClass), ordered = FALSE)

	  ## Percentual
	  lGraf$percentual = ggplot(dataGrafPrep, aes(x=factor(varInput), y=value, fill=variable))+
	  	geom_bar(position="fill",stat="identity") +
	    geom_text(aes(label = sprintf("%0.1f", 100*ratio), y = position)) +
	    guides(fill=FALSE) +
	  	labs(x = input, y = paste(target, " (%)", sep=" ")) +
	    theme_bw() +
	  	theme(axis.text.x=element_text(angle = 45, hjust = 1),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	  				panel.border = element_blank(),
	    			plot.background = element_blank(),
	  		  	axis.title.x=element_text(margin=margin(15,0,0,0)),
						axis.title.y=element_text(margin=margin(0,15,0,0))
	  		) +
	    scale_y_continuous(labels=percent) +
	  	scale_fill_manual(values=c("#00BFC4","#F8766D"))

	  ## Quantity plot
	  lGraf$quantity = ggplot(dataGrafPrep, aes(x=factor(varInput), y=value, fill=variable)) +
	  	geom_bar(position=position_dodge(),stat="identity") +
	    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size=4) +
	  	labs(x = input, y = paste(target, " (count)", sep=" ")) +
	    ylim(0, max(dataGrafPrep$value)+max(dataGrafPrep$value)*0.05) +
			theme_bw() +
	    theme(plot.background = element_blank(),
	    			panel.border = element_blank(),
    				axis.text.x=element_text(angle = 45, hjust = 1),
    				legend.title=element_blank(),
	    			axis.title.x=element_text(margin=margin(15,0,0,0)),
						axis.title.y=element_text(margin=margin(0,15,0,0))) +
	    guides(col = guide_legend(ncol = 1, byrow = TRUE)) +
	  	scale_fill_manual(values=c("#00BFC4","#F8766D"))


	  if(plot_type=='both')
	  {
	  	## Printing both plots
	  	final_plot=grid.arrange(lGraf$percentual, lGraf$quantity, ncol=2)
	  }

	  if(plot_type=='percentual')
	  {
	  	final_plot=lGraf$percentual
	  	plot(final_plot)
	  }

	  if(plot_type=='quantity')
	  {
	  	final_plot=lGraf$quantity
	  	plot(final_plot)
	  }

	  ## Save plot
	  if(!is.na(path_out))
	  {

	  	dir.create(path_out, showWarnings = FALSE)

	    if(dir.exists(path_out))
	    {
	      jpeg(sprintf("%s/%s.jpeg", path_out, input), width= 12.25, height= 6.25, units="in",res=200, quality = 90)

	      plot(final_plot)
	      dev.off()
	    } else {
	      warning(sprintf("The directory '%s' doesn't exists.", path_out))
	    }
	  }
}


#' Equal frequency binning
#' @description Equal frequency tries to put the same quantity of cases per bin when possible. It's a wrapper of function cut2 from Hmisc package.
#' @param var input variable
#' @param n_bins number of bins to split 'var' by equal frequency, if it not possible to calculate for the desired bins, it returns the closest number
#' @examples
#' ## Example 1
#' summary(heart_disease$age)
#' age_2=equal_freq(var=heart_disease$age, n_bins = 10)
#' summary(age_2)
#'
#' ## Example 2
#' age_3=equal_freq(var=heart_disease$age, n_bins = 5)
#' summary(age_3)
#' @return The binned variable.
#' @export
equal_freq <- function(var, n_bins)
{
	n_bins_orig = n_bins
	res = cut2(var, g = n_bins)
	uq=unique(res)
	n_bins_final = length(uq)
	if (n_bins_final != n_bins & sum(is.na(uq))==0)
		warning(sprintf("It's not possible to calculate with n_bins=%s, setting n_bins in: %s.",
										n_bins, n_bins_final))
	return(res)
}





