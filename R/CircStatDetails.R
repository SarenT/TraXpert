circ_stat_details_UI = function(id){
	ns = NS(id)
	tagList(fluidRow(
		column(6,
			   bsCollapse(id = ns("collapse_data"),
			   		   bsCollapsePanel("Data Structure",
			   		   				tableOutput(ns("circdata_replicates_Out")),
			   		   				tableOutput(ns("circdata_tracks_Out")),
			   		   				style = "default")
			   )
		),
		column(6, 
			   bsCollapse(id = ns("collapse_statistics"),
			   		   bsCollapsePanel("Statistics",
			   		   				tableOutput(ns("circstat_DF_Out")),
			   		   				textOutput(ns("circstat_text_Out")),
			   		   				style = "default")
			   )
		)
	),
	fluidRow(
		column(12,
			   bsCollapse(id = ns("collapse_uniformity"),
			   		   bsCollapsePanel("Data Distribution and Uniformity",
			   		   				#h2("Distribution & Spread"),
			   		   				uiOutput(ns("circstat_histogram_qqplot_Out"))#, plotOutput(ns("circstat_histogram_Out")), # circ package type histogram
			   		   				#plotOutput(ns("circstat_histogram_qqplot_Out"))
			   		   )
			   ),
			   bsCollapse(id = ns("collapse_variances"),
			   		   bsCollapsePanel("Variances & other Measures",
			   		   				#h2("Summary Statistics"),
			   		   				withMathJax("von Mises distribution is analogous to Gaussian distribution for circular data. Most simply circular data can be uniformly distributed. If not it may or may not follow a von Mises distrition. Similar to non-circular data, it may be skewed or have high/low kurtosis."),
			   		   				tableOutput(ns("stat_shape_Out")),
			   		   				h2("Uniformity, von Mises"),
			   		   				p("Rayleigh test can only be applied to single peak (unimodal) deviation from uniformity. Null hypothesis states that the data is uniformly distributed. For low p-values one can reject this. For cell migration, this can be interpreted as directionality. Watson's test is more powerful for non-unimodal deviations from uniformity. Although this would be a very rare use case scenario for migrating cells, directionality in opposite sides have been reported in some assays (e.g. force coupling on nano structures shown here Reversat et al. 2020)."),
			   		   				tableOutput(ns("stat_uniformity_DF_Out")),
			   		   				verbatimTextOutput(ns("stat_uniformity_text_Out")),
			   		   				verbatimTextOutput(ns("stat_vonMisesFit_Out"))
			   		   )
			   )
		)
		
	))
}

circ_stat_details_server = function(id, plot_data){
	moduleServer(id, function(input, output, session){
		
		output$circstat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
			if("tbl" %in% class(plot_data()$stat)){
				plot_data()$stat
			}
		})
		output$circstat_text_Out = renderText({
			#browser()
			if("list" %in% class(plot_data()$stat)){
				statOutText = ""
				for(i in 1:length(plot_data()$stat)){
					statName = names(plot_data()$stat)[i]
					statOutText = paste(statOutText, statName, sep = "\n\n")
					statOutText = paste(statOutText, paste(plot_data()$stat[[i]], collapse = "\n"), sep = "\n") #hTestToString(trackPlot()$stat)
				}
				
				statOutText
			}else if("character" %in% class(plot_data()$stat)){
				#browser()
				statOutText = paste(plot_data()$stat, collapse = "\n\n") #hTestToString(trajectoryPlot()$stat)
				statOutText
			}
			
		})
		
		output$stat_shape_Out = renderTable(spacing = "xs", striped = TRUE, {
			#browser()
			if("tbl" %in% class(plot_data()$summaryStats)){
				plot_data()$summaryStats
			}
		})
		
		output$stat_uniformity_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
			#browser()
			if("tbl" %in% class(plot_data()$uniformityDF)){
				plot_data()$uniformityDF
			}
		})
		
		output$stat_uniformity_text_Out = renderText({
			#browser()
			if(is.list(plot_data())){
				plot_data()$uniformityText
			}
		})
		
		output$stat_vonMisesFit_Out = renderText({
			#browser()
			
			if(is.list(plot_data())){
				plot_data()$vonMisesFit
			}
		})
		
		output$circdata_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
			plot_data()$replicates
		})
		output$circdata_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
			plot_data()$tracks
		})
		
		
		# circstat_histogram_qqplot_Height = function() {
		# 	directionalityHistPlotOut = plot_data()
		# 	browser()
		# 	if(is.list(directionalityHistPlotOut)){
		# 		return(circHistQQCellSize * 2 * directionalityHistPlotOut$circDataModel$nGroups)
		# 	}else{
		# 		return(circHistQQCellSize)
		# 	}
		# }
		
		output$circstat_histogram_qqplot_Out = renderUI({
			# browser()
			ns = session$ns
			cat(paste(c("plotsize:", 
						circHistQQCellSize, 
						circHistQQCellSize * 0.5 * length(histogramData())), collapse="\t")); cat("\n")
			plotOutput(ns("circstat_histogram_qqplot_Contents"), 
					   width = circHistQQCellSize, height = circHistQQCellSize * 0.5 * length(histogramData()))
		})
		
		histogramData = reactive({
			directionalityHistPlotOut = plot_data()
			
			if(is.list(directionalityHistPlotOut)){
				directionalityHistPlotOut$circDataModel
			}else{
				NULL
			}
		})
		
		output$circstat_histogram_qqplot_Contents = renderPlot({
			#browser()
			
			histogramData = histogramData()
			if(!is.null(histogramData)){
				nGroups = length(histogramData)
				par(mfrow=c(nGroups, 2))
				titles = names(histogramData)
				for(title in titles){
					circdat = histogramData[[title]]
					circularHistogram(circdat, title = title, shrink = 1.5, browse = input$dir_browse_In)
					vMQQ(circdat, title = title)
				}
			}
		})
		
	})
}