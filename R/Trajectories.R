trajectories_UI = function(id, title, tabColor){
	ns = NS(id)
	
	plot_type = function(ns){
		bsCollapsePanel("Plot Type and Variables",
						selectInput(ns("xy_In"), "Position", choices = list()),
						tipify(selectInput(ns("replicate_In"), "Replicates are grouped in", choices = list()), 
							   toolTips$replicate_In, "top", "hover"),
						tipify(selectInput(ns("start_point_In"), "Start Point Display", choices = list()), 
							   "", "top", "hover"),
						tipify(selectInput(ns("end_point_In"), "End Point Display", choices = list()), 
							   "", "top", "hover")
		)
	}
	
	groupings_colors = function(ns){
		bsCollapsePanel("Groupings and Colors", 
						tipify(selectInput(ns("color_In"), "Point/Line Color Variable", choices = list()), 
							   "", "top", "hover"),
						conditionalPanel("input.color_In != \"NULL\"", ns = ns,
										 checkboxInput(ns("color_tracks_In"), "Colored Tracks", value = TRUE)),
						
						bsTooltip(ns("color_tracks_In"), 
								  "Should trajectories also be colored? Or only start/end points?", 
								  placement = "bottom", trigger = "hover")
		)
	}
	
	filters = function(ns){
		bsCollapsePanel(
			"Filters", 
			checkboxInput(ns("limit_to_smallest_In"), "Limit # of tracks to smallest group", value = FALSE),
			sliderInput(ns("track_reduced_In"), "Plot every n th Track", 
						min = 1, max = 10, value = 1, step = 1, width = "200%"),
			sliderInput(ns("spot_reduced_In"), "Plot every n th Spot", 
						min = 1, max = 10, value = 1, step = 1, width = "200%"),
			
			bsTooltip(ns("limit_to_smallest_In"), 
					  "If you have groups with extremely differing number of spots, you can limit the number of trajectories displayed to the number of trajectories of the smallest group. More crowded groups may appear more \"migratory\" to the \"eye\".", 
					  "bottom", "hover"),
			bsTooltip(ns("track_reduced_In"), 
					  "Reduce number of trajectories displayed by this number. e.g. 2 means every second track to be displayed for quicker and more clean plotting.", 
					  "bottom", "hover"),
			bsTooltip(ns("spot_reduced_In"), 
					  "Reduce number of trajectories displayed by this number. e.g. 2 means every second track to be displayed for quicker and smoother plotting.", 
					  "bottom", "hover")
		)
	}
	
	ranges_units_labels = function(ns){
		bsCollapsePanel(
			"Ranges, Units & Labels", 
			fluidPage(fluidRow(column(6, checkboxInput(ns("inverse_In"), "Invert y axis", value = TRUE)),
							   column(6, checkboxInput(ns("equal_range_In"), "Equal x/y ranges", value = TRUE)))),
			axis_labels_UI(ns("axis_labs")),
			
			#bsTooltip("", "", "bottom", "hover")
			bsTooltip(ns("xy_In"), 
					  "How should trajectories be placed? Either absolute position, fixed (all tracks start from origin or fixed and rotated (tracks are rotated in the \"Operations\" tab).", 
					  "top", "hover"),
			bsTooltip(ns("inverse_In"), 
					  "Should the y axis be inverted? Imaging y coordinates increase downwards, which is opposite of the cartesian coordinate system. Inverting y axis will make trajectories oriented same way as in the image.", 
					  "bottom", "hover"),
			bsTooltip(ns("equal_range_In"), 
					  "Should the x and y axis have equal ranges? If false, plots will be stretched (maybe useful for highly asymmetric imaging daata).", 
					  "bottom", "hover")
		)
	}
	
	display_options = function(ns){
		bsCollapsePanel("Display Options", 
						sliderInput(ns("color_alpha_In"), "Color Transparency", min = 0, max = 1, value = 1),
						sliderInput(ns("line_size_In"), "Line Thickness", 
									min = 0.01, max = 100, value = 0.5, step = 0.25),
						fluidPage(
							fluidRow(
								column(6,
									   checkboxInput(ns("coord_equal_In"), "Equal axes ranges", value = TRUE),
									   checkboxInput(ns("panel_border_In"), "Panel border", value = FALSE),
									   checkboxInput(ns("h_line_In"), "Horizontal line", value = TRUE),
									   bsTooltip(ns("coord_equal_In"), "", placement = "bottom", trigger = "hover"),
									   bsTooltip(ns("panel_border_In"), "", placement = "bottom", trigger = "hover"),
									   bsTooltip(ns("h_line_In"), "", placement = "bottom", trigger = "hover")
								),
								column(6, 
									   dark_plot_UI(ns("dark")),
									   checkboxInput(ns("panel_grid_major_In"), "Panel major grid", value = FALSE),
									   checkboxInput(ns("v_line_In"), "Vertical line", value = TRUE),
									   bsTooltip(ns("panel_grid_major_In"), "", 
									   		  placement = "bottom", trigger = "hover"),
									   bsTooltip(ns("v_line_In"), "", placement = "bottom", trigger = "hover"))
							)
						),
						
						bsTooltip(ns("color_alpha_In"), 
								  "Should trajectories appear transparent? Particularly useful if there are too many trajectories,", 
								  "bottom", "hover"),
						bsTooltip(ns("line_size_In"), "Line thickness for either too crowded or sparse data.", 
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
			 			   		   plot_type(ns),
			 			   		   groupings_colors(ns),
			 			   		   filters(ns),
			 			   		   facet_control_UI(ns("facet"), textFaceChoices),
			 			   		   ranges_units_labels(ns),
			 			   		   display_options(ns),
			 			   		   debugging_UI(ns("debug"))
			 			   )
			 			   
			 			   
			 		),
			 		column(8, 
			 			   plotOutput(outputId = ns("plotOut")),
			 			   #statDataDetails("track"),
			 			   #tags$style(type="text/css", "#track_stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   actionButton(inputId = ns("plotTrajIn"), label = "Plot Trajectories"),
			 			   plot_export_UI(ns("export"))
			 		)
			 	)
			 )
	)
}

trajectories_server = function(id, data, features, tracks, trajectories, groupings, 
								groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
								groupingsChoiceswithEmptywithDoNotDisplay,
								groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
								trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty){
	moduleServer(id, function(input, output, session){
		plot = eventReactive(input$plotTrajIn, {#renderPlot({
			# browser()
			#plot = reactive({
			#plot = renderPlot({
			#observeEvent(input$plotTrajIn, {
			if(debugging$browse){
				browser()
			}
			titles = lapply(titles, function(x){x()})
			xlab = axis_labs$x_lab()
			ylab = axis_labs$y_lab()
			
			colorGroup = input$color_In; if(colorGroup == "NULL") {colorGroup = NULL}
			startPointGroup = input$start_point_In
			if(startPointGroup == "NA") {
				startPointGroup = NA
			} else if(startPointGroup == "NULL") {
				startPointGroup = NULL
			}
			endPointGroup = input$end_point_In
			if(endPointGroup == "NA") {
				endPointGroup = NA
			} else if(endPointGroup == "NULL") {
				endPointGroup = NULL
			}
			
			replicateGroup = input$replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
			facetRowGroup = facet$row_group()
			facetColGroup = facet$col_group()
			
			initializeProgress = function(max, message){
				progress <<- shiny::Progress$new(max = max)
				if(!is.null(message)){
					progress$set(message = message, value = 0)
				}else{
					progress$set(value = 0)
				}
			}
			
			
			# Close the progress when this reactive exits (even if there's an error)
			#on.exit({progress$close()})
			
			updateProgress = function(value, detail = NULL) {
				if(is.null(detail)){progress$set(value = value)}else{progress$set(value = value, detail = detail)}
			}
			
			closeProgress = function(){progress$close()}
			
			plot = plotTrajectories(data = trajectories(), x = xy_names()$xVarName, y = xy_names()$yVarName, 
									trackGlobalIDName = "track_global_id", 
									groupings = groupings()$groupings, 
									x.unit = axis_labs$x_unit(), y.unit = axis_labs$y_unit(),
									colorGroupName = colorGroup,
									startPointGroupName = startPointGroup, endPointGroupName = endPointGroup, 
									colorTrajectories = input$color_tracks_In, 
									coord_equal = input$coord_equal_In, 
									#fill.legend = fillLegend, color.legend = colorLegend, alpha.legend = alphaLegend, 
									inverse = input$inverse_In, equalRange = input$equal_range_In,
									facet.row = facetRowGroup, facet.col = facetColGroup, facet.wrap = facet$wrap(),
									title = titles$title, subtitle = titles$subtitle, 
									replicateGroupName = replicateGroup, 
									hide.ns = input$stat_hidens_In,
									colorAlpha = input$color_alpha_In, 
									is.dark = dark_plot(),
									limitNTracks = input$limit_to_smallest_In,
									#randomizeTrackSampling = input$limit_to_smallest_In,
									trackReduced = input$track_reduced_In,
									spotReduced = input$spot_reduced_In,
									plot.subtitle.hjust = titles$subtitle_hjust, 
									plot.subtitle.size = titles$subtitle_size, 
									plot.subtitle.face = titles$subtitle_text_style,
									h.line = input$h_line_In, v.line = input$v_line_In, 
									panel.border = input$panel_border_In, 
									panel.grid.major = input$panel_grid_major_In, 
									facet.label.fill.color = facet$label_fill_color(), 
									facet.text.face = facet$label_face(), 
									linesize = input$line_size_In, x.lab = xlab, y.lab = ylab,
									browse = debugging$browse, benchmark = debugging$benchmark, 
									verbose = debugging$verbose,
									initializeProg = initializeProgress, updateProg = updateProgress)
			#browser()
			closeProgress()
			plot
		})
		
		xyLocationswithoutEmpty = reactive({
			#trajectoryPositionNamedList(c("Spot", "Edge"), data()$features, "x", empty = FALSE)
			#browser()
			#namedList = choicesInNamedList(c("Spot", "Edge"), features(), "POSITION_X", empty = FALSE)
			#positionTypes[1:length(namedList)] 
			# Error with new column POSITION_X_DISPLACEMENT, which also matches POSITION_X
			# Actually now that we have POSITION_X_FIX_ROT by default same as POSITION_X_FIX, we dont need length check
			positionTypes
		})
		
		output$plotOut = renderPlot({plot()$plot})
		
		output$stat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
			if("tbl" %in% class(plot()$stat)){
				plot()$stat
			}
		})
		output$stat_text_Out = renderText({
			if("character" %in% class(plot()$stat)){
				#browser()
				statOutText = paste(plot()$stat, collapse = "\n") #hTestToString(plot()$stat)
				statOutText
			}
		})
		output$data_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
			plot()$replicates
		})
		output$data_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
			plot()$tracks
		})
		
		xy_names = reactive({
			posTypeInd = which(input$xy_In == unlist(positionTypes))
			if(length(posTypeInd) > 0){
				return(list(xVarName = allPositionTypes[[posTypeInd]][1], yVarName = allPositionTypes[[posTypeInd]][2]))
			}else{
				return(list(xVarName = NULL, yVarName = NULL))
			}
		})
		
		observe({updateSelectInput(session, "xy_In", choices = xyLocationswithoutEmpty(), 
								   selected = "fixed")})
		observe({updateSelectInput(session, "color_In", choices = groupingsAndFeatureChoiceswithoutEmpty())})
		observe({updateSelectInput(session, "start_point_In", choices = groupingsChoiceswithEmptywithDoNotDisplay())})
		observe({updateSelectInput(session, "end_point_In", choices = groupingsChoiceswithEmptywithDoNotDisplay())})
		observe({updateSelectInput(session, "replicate_In", choices = groupingsChoiceswithEmpty())})
		
		
		facet = facet_control_server("facet", groupingsChoiceswithEmpty)
		plot_export_server("export", "Trajectory", plot)
		debugging = debugging_server("debug")
		dark_plot = dark_plot_server("dark")
		titles = titles_server("title")
		axis_labs = axis_labels_server("axis_labs", features, trajectories, 
											groups = list(x = reactive({xy_names()$xVarName}), 
														  y = reactive({xy_names()$yVarName})),
											default_labels = list(x = reactive({xy_names()$xVarName}), 
																  y = reactive({xy_names()$yVarName})))
	})
}