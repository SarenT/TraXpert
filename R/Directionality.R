directionality_UI = function(id, title, tabColor){
	ns = NS(id)
	
	plot_type = function(){
		bsCollapsePanel("Plot Type and Variables",
						selectInput(ns("type_In"), "Plot Type", 
									choices = list(`Bar/Radar` = "bar", Polygon = "polygon"), 
									selected = "bar"),
						tipify(selectInput(ns("replicate_In"), "Replicates are grouped in", choices = list()), 
							   toolTips$replicate_In, "top"),
						bsTooltip(ns("type_In"), 
								  "Type of plot. Bar plot is also called rose plot. Polygon is also called radar plot.", 
								  "top", "hover"),
						selectInput(ns("summary_fun_In"), "Summary Function", 
									choices = summaryFunctionChoices[2:length(summaryFunctionChoices)], 
									selected = "length"),
						conditionalPanel("input.summary_fun_In == 'length'", ns = ns,
										 checkboxInput(ns("summary_fun_length_percentage_In"), 
										 			  "Percentage", value = TRUE)),
						selectInput(ns("replicate_summary_fun_In"), "Replicates Summary Function", 
									choices = summaryFunctionChoices, selected = "mean"),
						
						tipify(selectInput(ns("track_direction_cat_In"), "Cardinal Direction Variable", 
										   choices = list()), 
							   "Which direction measure should be displayed? Cardinal dirction means binned track angles at 30° bins. e.g. angles -15° to 15° are considered as together. If rotation is applied in \"Operations\" tab, then this can be displayed as well.", 
							   "top", "hover"),
						tipify(selectInput(ns("cumulation_In"), 
										   "Cumulation Variable (upon grouping)", 
										   choices = list(`Each Track` = "EACH_TRACK", 
										   			   `Track Displacement` = "TRACK_DISPLACEMENT")), 
							   "Which measure needs to be used for cumulation. e.g. track displacement can enhance the effect of directionality with speed and persistence.", 
							   "top", "hover"),
						
						bsTooltip(ns("summary_fun_In"), 
								  "Summary function to be applied grouped tracks. Suggested option: length.", 
								  "top", "hover"),
						bsTooltip(ns("summary_fun_length_percentage_In"), 
								  "Display percentage of number of cells to the total number of cells.", 
								  "bottom", "hover"),
						bsTooltip(ns("replicate_summary_fun_In"), 
								  "Summarize replicates with the selected function.", "top", "hover")
		)
	}
	
	tests = function(){
		bsCollapsePanel(
			"Tests", 
			tipify(selectInput(ns("multisample_measure_In"), "Measure", 
							   choices = circMultiSampleTestMeasures), 
				   "Which measure to be used to compare groups. Mean/median direction, directionality (concentration) or distribution (are sample drawn from the same distribution?).", 
				   "top", "hover"),
			conditionalPanel(
				"input.multisample_measure_In == 'mean'", ns = ns,
				tipify(selectInput(ns("multisample_mean_method_In"), "Method", 
								   choices = circMultiSampleTestMeanMethods), 
					   "Watson large non-parametric test assumes number of sampes over 25 otherwise the p value needs to be considered as an approximation.", 
					   "top", "hover")
			),
			conditionalPanel(
				"input.multisample_measure_In == 'median'", ns = ns,
				tipify(selectInput(ns("multisample_median_method_In"), "Method", 
								   choices = circMultiSampleTestMedianMethods), 
					   "Fisher non-parametric test", 
					   "top", "hover")
			),
			conditionalPanel(
				"input.multisample_measure_In == 'conc'", ns = ns,
				tipify(selectInput(ns("multisample_conc_method_In"), "Method", 
								   choices = circMultiSampleTestConcMethods), 
					   "Fisher non-parametric test", 
					   "top", "hover")
			),
			conditionalPanel(
				"input.multisample_measure_In == 'dist'", ns = ns,
				tipify(selectInput(
					ns("multisample_dist_method_In"), "Method", 
					choices = circMultiSampleTestDistMethods), 
					"Fisher non-parametric test", 
					"top", "hover"),
				conditionalPanel(
					"input.multisample_dist_method_In == 'watson.two.test'",
					tipify(
						selectInput(
							ns("stat_watsontwotest_comparison_type_In"), 
							"Pairwise comparisons", 
							choices = trackPairwStatComparisonTypeChoices, 
							selected = "all_combinations"), 
						"Select which pairs need to be selected. Either all combinations of pairs, all groups to a control group or selected pairs.", 
						placement = "top", trigger = "hover")
				)
			),
			conditionalPanel(
				"input.stat_watsontwotest_comparison_type_In == 'to_control'", ns = ns,
				tipify(
					selectInput(
						ns("stat_watsontwotest_comparison_control_In"), 
						"Control Group", choices = list()), 
					"Select the control group to compare with other groups.",
					placement = "top", trigger = "hover")),
			conditionalPanel(
				"input.stat_watsontwotest_comparison_type_In  == 'selected'", ns = ns,
				tags$div(id = 'placeholderDirPairwiseGroupSelect')
			)
		)
	}
	
	ranges_units_labels = function(){
		bsCollapsePanel("Ranges, Units & Labels", 
						checkboxInput(ns("show_y_axis_In"), "Show y axis", value = FALSE),
						bsTooltip(ns("show_y_axis_In"), 
								  "Displays a y axis, which is a vertical line to indicate y scale.", 
								  "bottom", "hover"),
						axis_labels_UI(ns("axis_labs"), 
									   list(x = list(title = "x axis",
									   			  unit = FALSE,
									   			  tooltip = toolTips$xlab_In),
									   	 y = list(title = "y axis",
									   	 		 unit = FALSE,
									   	 		 tooltip = toolTips$ylab_In)
									   ))
		)
	}
	
	display_options = function(){
		bsCollapsePanel(
			"Display Options", 
			sliderInput(ns("line_size_In"), "Line Thickness", 
						min = 0.01, max = 100, value = 0.5, width = "150%"),
			sliderInput(ns("start_angle_In"), "Angle Rotation", 
						min = -180, max = 180, step = 15, value = 0, width = "150%"),
			
			#bsTooltip("", "", "bottom", "hover")
			bsTooltip(ns("line_size_In"), "Line thickness relative to original size.", 
					  "bottom", "hover"),
			bsTooltip(ns("start_angle_In"), 
					  "Starting angle of the directions. This simply rotates the plots, if you want to fix the direction.", 
					  "bottom", "hover")
			
		)
	}
	
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(
			 	fluidRow(
			 		column(4,
			 			   bsCollapse(id = ns("settings"),
			 			   		   titles_UI(ns("title"), textFaceChoices),
			 			   		   plot_type(),
			 			   		   groupings_colors_UI(ns("groupings_colors") , width = "150%", can_dark = TRUE),
			 			   		   tests(),
			 			   		   facet_control_UI(ns("facet"), textFaceChoices),
			 			   		   ranges_units_labels(),
			 			   		   display_options(),
			 			   		   debugging_UI(ns("debug"), 
			 			   		   			 list(skip_radar = list(label = "Skip Radar", value = FALSE),
			 			   		   			 	 skip_degrees = list(label = "Skip Degrees", value = FALSE)))
			 			   )
			 			   
			 			   
			 		),
			 		column(8, 
			 			   plotOutput(outputId = ns("plotOut")),
			 			   circ_stat_details_UI(ns("stats")),
			 			   tags$style(type="text/css", "#circstat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   plot_export_UI(ns("export"))
			 		)
			 	)
			 )
	)
}


directionality_server = function(id, data, features, tracks, trajectories, groupings, 
								 groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty,
								 groupingsChoiceswithEmptywithDoNotDisplay,
								 groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
								 trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
								 trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices){
	moduleServer(id, function(input, output, session){
		plot = reactive({
			#browser
			if(!is.list(groupings()) || length(groupings()) == 0){
				return(NULL)
			}
			if(debugging$browse){
				browser()
			}
			titles = titles = lapply(titles, function(x){x()})
			xlab = axis_labs$x_lab()
			ylab = axis_labs$y_lab()
			
			color_group = groupings_colors$color_group()
			fill_group = groupings_colors$fill_group()
			
			replicateGroup = input$replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
			facetRowGroup = facet$row_group()
			facetColGroup = facet$col_group()
			
			start.angle = ud.convert(input$start_angle_In, "degree", "radian"); start.angle[is.nan(start.angle)] = 0
			statMeasure = input$multisample_measure_In
			statMethod = NULL; statExtras = NULL
			if(statMeasure != "NULL"){
				statMethod = input[[paste("multisample", statMeasure, "method_In", sep = "_")]]
				if(statMethod == "watson.two.test"){
					statExtras = input$stat_watsontwotest_comparison_type_In
					if(statExtras == "to_control"){
						statExtras = c(statExtras, input$stat_watsontwotest_comparison_control_In)
					}else{
						statExtras = c(statExtras, "")
					}
				}
			}else{
				statMeasure = NULL
			}
			
			#browser()
			
			groupsCardinal = unlist(groupingsToNamedList(groupings()$groupings, empty = FALSE), use.names = F)
			plot = plotRadar(dataTracks = tracks(), groups = groupsCardinal, #directionGroupName = input$track_direction_In, 
							 directionCatGroupName = input$track_direction_cat_In, 
							 #colorReverseOrder = input$reverse_order_In,
							 cumulativeGroupName = input$cumulation_In, type = input$type_In, 
							 summary.fun.name = input$summary_fun_In, 
							 percentage = input$summary_fun_length_percentage_In,
							 groupings = groupings()$groupings, start.angle = start.angle, 
							 stat.method = statMethod, stat.measure = statMeasure, stat.extras = statExtras,
							 colorGroupName = color_group, fillGroupName = fill_group, #alphaGroupName = alphaGroup,
							 replicateGroupName = replicateGroup,
							 replicate.summary.fun.name =  input$replicate_summary_fun_In,
							 facet.row = facetRowGroup, facet.col = facetColGroup, 
							 title = titles$title, subtitle = titles$subtitle, 
							 fillAlpha = groupings_colors$fill_alpha(), 
							 colorAlpha = groupings_colors$color_alpha(), 
							 is.dark = groupings_colors$dark(), line.size = input$line_size_In,
							 facet.label.fill.color = facet$label_fill_color(), 
							 facet.text.face = facet$label_face(), 
							 facet.wrap = facet$wrap(),
							 show.y.axis = input$show_y_axis_In, 
							 plot.subtitle.hjust = titles$subtitle_hjust, 
							 plot.subtitle.size = titles$subtitle_size, 
							 plot.subtitle.face = titles$subtitle_text_style,
							 browse = debugging$browse, benchmark = debugging$benchmark, 
							 verbose = debugging$verbose, skip.radar = debugging$skip_radar, 
							 skip.degrees = debugging$skip_degrees
			)
			# browser()
			plot
		})
		output$plotOut = renderPlot({
			# browser()
			#print(plot())
			plot = plot()$plot
			
			plot
		})
		
		
		
		output$plotPreviewOut = renderImage({
			#browser()
			plotOut = plot()
			if(is.list(plotOut)){
				temp_png_file = tempfile(fileext = ".png")
				
				size = export_size()
				
				ggsave(temp_png_file, plotOut$plot, width = size$width, height = size$height, 
					   dpi = 300, units = "cm")
				dim = ggplot2:::plot_dim(dim = unlist(size, use.names = F), dpi = 300, units = "cm") * 300
				#browser()
				list(
					src = temp_png_file,
					contentType = "image/png",
					width = dim[1]/4,
					height = dim[2]/4,
					alt = "Track feature plot"
				)
			}else{
				NULL
			}
		}, deleteFile = FALSE)
		
		
		export_size = reactive({
			width = input$width_In; height = input$height_In
			if(input$auto_width_In){width = NA}
			if(input$auto_height_In){height = NA}
			return(list(width = width, height = height))
		})
		
		#observe({updateSelectInput(session, "track_direction_In", choices = trackDirectionChoiceswithoutEmpty())})
		observe({updateSelectInput(session, "track_direction_cat_In", choices = trackDirectionCatChoiceswithoutEmpty(), 
								   selected = "DIRECTION_CARDINAL")})
		observe({updateSelectInput(session, "cumulation_In", choices = trackChoiceswithoutEmpty(), 
								   selected = "TRACK_ID")})
		observe({updateSelectInput(session, "alpha_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "replicate_In", choices = groupingsChoiceswithEmpty())})
		
		
		observe({
			groupingsList = groupings()
			dataList = data()
			statGroup = input$color_In
			#trackData = tracks()
			if(!is.null(statGroup) && statGroup != ""){
				if(length(groupingsList) > 1){
					groupingsDF = groupingsList$groupings
					
					removeUI(selector = "#placeholderDirPairwiseGroupSelect div", multiple = TRUE)
					#for(rankListOb in rankListObs){rankListOb$destroy()}
					#rankListObs <<- list()
					if(length(dataList) > 1){
						#browser()
						choices = as.character(getGroups(groupingsDF, statGroup))
						labels = as.character(getGLabs(groupingsDF, statGroup))
						#choices = as.list(choices)
						#names(choices) = labels
						
						insertUI(selector = "#placeholderDirPairwiseGroupSelect", 
								 ui = generateBucketList(session$ns, choices),
								 # ui = bucket_list(header = "Select pairs", orientation = "horizontal",
								 # 				 add_rank_list(text = "Compare these", input_id = "track_stat_comparison_select1_In", labels = NULL),
								 # 				 add_rank_list(text = "<- from these ->", input_id = "track_stat_comparison_select_In", labels = choices),
								 # 				 add_rank_list(text = "with these", input_id = "track_stat_comparison_select2_In", labels = NULL)
								 # 				 ), 
								 where = "beforeEnd")
					}
				}
			}
		})
		
		facet = facet_control_server("facet", groupingsChoiceswithEmpty)
		plot_export_server("export", "Directionality", plot)
		circ_stat_details_server("stats", plot)
		groupings_colors = groupings_colors_server("groupings_colors", groupingsChoiceswithoutEmpty)
		debugging = debugging_server("debug")
		titles = titles_server("title")
		axis_labs = axis_labels_server("axis_labs", features, tracks, 
										   groups = list(x = NULL, y = NULL),
										   default_labels = list(x = NULL, y = NULL))
		
	})
}