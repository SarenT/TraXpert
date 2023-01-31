stat_details_UI = function(id){
	ns = NS(id)
	tagList(fluidRow(
		column(6,
			   bsCollapse(id = ns("collapse_data"),
			   		   bsCollapsePanel("Data Structure",
			   		   				tableOutput(ns("data_replicates_Out")),
			   		   				tableOutput(ns("data_tracks_Out")),
			   		   				style = "default")
			   )
		),
		column(6, 
			   bsCollapse(id = ns("collapse_statistics"),
			   		   bsCollapsePanel("Statistics",
			   		   				tableOutput(ns("stat_DF_Out")),
			   		   				textOutput(ns("stat_text_Out")),
			   		   				style = "default")
			   )
		)
	),
	fluidRow(
		column(12,
			   bsCollapse(id = ns("collapse_normality"),
			   		   bsCollapsePanel("Data Distribution, Normality and Variances",
			   		   				fluidPage(fluidRow(
			   		   					column(6, plotOutput(ns("stat_histogram_Out")),
			   		   						   plotOutput(ns("stat_qq_Out"))),
			   		   					column(6, h2("Shapiro-Wilk Test and Distribution Shape"),
			   		   						   withMathJax("Shapiro-Wilk test is a test of normality of data. Null hypothesis is that the sample came from a normally distributed population. If p-value is below a cutoff value (usually 0.05) null hypothesis can be rejected. Otherwise, there is not enough evidence to reject the null hypothesis. Small W values indicate a non-normality. However, only extreme deviations reduce W values. In general, Shapiro-Wilk test is highly sensitive to small deviations from normality with very large sample sizes (n > 200). Refer to Q-Q plots for thorough inspection. Skewness is measured with \\( g_1\\ =\\ m_3\\ / m_2^{3/2}\\ \\) (positive numbers mean right tail long) and kurtosis with \\( g_2\\ =\\ m_4\\ / m_2^{2} - 3\\ \\) (Positive kurtosis means more values are closer to the mean.)."),
			   		   						   tableOutput(ns("stat_normality_Out")),
			   		   						   h2("Levene's Test"),
			   		   						   p("Levene's test is a statistic to test equality of variances. Null hypothesis is that the population variances are equal. If p value is below a certain threshold (typically 0.5), null hypothesis can be rejected and it can be assumed that there is a difference between variances of groups."),
			   		   						   verbatimTextOutput(ns("stat_levene_Out")))
			   		   				)),
			   		   				style = "default")
			   )
		)
	))
}

stat_details_server = function(id, plot_data){
	moduleServer(id, function(input, output, session){
		output$stat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
			#browser()
			if("tbl" %in% class(plot_data()$stat[[1]])){
				plot_data()$stat
			}
		})
		output$stat_text_Out = renderText({
			#browser()
			if("character" %in% class(plot_data()$stat[[1]])){
				#browser()
				statOutText = ""
				for(i in 1:length(plot_data()$stat)){
					statName = names(plot_data()$stat)[i]
					statOutText = paste(statOutText, statName, sep = "\n\n")
					statOutText = paste(statOutText, paste(plot_data()$stat[[i]], collapse = "\n"), sep = "\n")
				}
				
				statOutText
			}
		})
		
		output$data_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
			plot_data()$replicates
		})
		output$data_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
			plot_data()$tracks
		})
		
		output$stat_histogram_Out = renderPlot({
			plotOut = plot_data()
			if(is.list(plotOut)){
				plotOut$histogram
			}else{
				NULL
			}
		})
		
		output$stat_qq_Out = renderPlot({
			plotOut = plot_data()
			if(is.list(plotOut)){
				plotOut$qq
			}else{
				NULL
			}
		})
		
		output$stat_normality_Out = renderTable(spacing = "xs", striped = TRUE, {
			plotOut = plot_data()
			if(is.list(plotOut)){
				plotOut$normality
			}else{
				NULL
			}
		})
		
		output$stat_levene_Out = renderText({
			plotOut = plot_data()
			if(is.list(plotOut)){
				plotOut$levene
			}else{
				NULL
			}
		})
	})
}