trajectory_features_UI = function(id, title, tabColor){
	ns = NS(id)
	plot_type = function(){
		bsCollapsePanel(
			"Plot Type and Variables", 
			tipify(selectInput(ns("type_In"), "Plot Type", choices = trajFeaturesChoices, selected = "point", 
							   multiple = TRUE), 
				   "Display method. Smooth plot displays a fit to the data to help with overplotting (too many data points, which overcrowds the space).", 
				   "top"),
			tipify(selectInput(ns("replicate_In"), "Replicates are grouped in", choices = list()), 
				   toolTips$replicate_In, "top"),
			tipify(selectInput(ns("aggr_fun_In"), "Aggregate groups with", 
							   choices = aggregateFunctionChoices, selected = "mean"), 
				   "Method to summarize replicates. Multiple replicates in the same grouped are then represented together (e.g. mean).", 
				   "top"),
			tipify(selectInput(ns("disp_fun_In"), "Display error", 
							   choices = dispersionMeasureChoices[1], selected = "None"), 
				   "What dispersion measure to be displayed. e.g. error bars, min/max, range etc.", 
				   "top"),
			tipify(selectInput(ns("disp_type_In"), "Style", choices = dispersionTypeChoices, selected = "errorbar"), 
				   "Style of dispersion display.", 
				   "top"),
			tipify(selectInput(ns("x_In"), "x Axis Variable", choices = list()), 
				   "Each trajectory feature is to be displayed with x/y coordinates. Both needs to be continious. e.g. speed (y) over time (x).", 
				   "top"),
			tipify(selectInput(ns("y_In"), "y Axis Variable", choices = list()), 
				   "Each trajectory feature is to be displayed with x/y coordinates. Both needs to be continious. e.g. speed (y) over time (x).", 
				   "top")
		)
	}
	
	groupings_colors = function(){
		bsCollapsePanel(
			"Groupings and Colors", 
			tipify(selectInput(ns("color_In"), "Line/Point Color Variable", choices = list()), 
				   "Line/Point colors.", 
				   "top"),
			tipify(selectInput(ns("size_In"), "Size", choices = list()), "Line/Points size measure.", "top"),
			conditionalPanel(
				"input.type_In.indexOf('jitter') > -1 || input.type_In.indexOf('point') > -1 || input.type_In.indexOf('smooth') > -1 || input.type_In.indexOf('area') > -1", 
				ns = ns,
				tipify(selectInput(ns("fill_In"), "Fill Variable", choices = list()), 
					   "Fill color according to a group.", 
					   "top")
			),
			conditionalPanel(
				"input.type_In.indexOf('quantile') > -1 || input.type_In.indexOf('smooth') > -1 || input.type_In.indexOf('area') > -1 || input.type_In.indexOf('line') > -1 || input.type_In.indexOf('step') > -1", 
				ns = ns,
				tipify(selectInput(ns("linetype_In"), "Line type Variable", choices = list()), 
					   "Line type according to a group.", 
					   "top"),
				checkboxInput(ns("smooth_In"), "Smoothen the line", value = FALSE),
				conditionalPanel("input.smooth_In", ns = ns,
								 numericInput(ns("smooth_windowIn"), label = "Smoothing Window", 
								 			 value = 1, min = 1, max = 100)
				)),
			#checkboxInput(ns("group_tracks_In", "Group tracks in the same group or split tracks individually.", value = FALSE)),
			conditionalPanel(
				"input.type_In.indexOf('jitter') > -1 || input.type_In.indexOf('point') > -1", 
				ns = ns,
				tipify(selectInput(ns("shape_In"), "Shape", choices = list()), 
					   "Shape of points based on the selected group.", 
					   "top")
			),
			
			bsTooltip(ns("smooth_In"), 
					  "Smoothen the data with gaussian filter. This is particularly useful for noisy data.", 
					  "bottom", "hover"),
			bsTooltip(ns("smooth_windowIn"), "Gaussian filter window size. The larger windows make smoother data.", 
					  "bottom", "hover")#,
			#bsTooltip(ns("group_tracks_In"), "Group tracks.", "bottom", "hover")
		)
	}
	
	ranges_colors_units = function(){
		bsCollapsePanel(
			"Ranges, Units & Labels", 
			fluidPage(fluidRow(
				column(10, tipify(
					sliderInput(ns("y_range_In"), "y Axis Range", 
								min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE), 
					"y axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only y axis selection is registered).", 
					"top")), 
				column(2, checkboxInput(ns("y_range_check_In"), "", value = TRUE))),
				axis_labels_UI(ns("axis_labs"))
			)
		)
	}
	
	transformations = function(){
		bsCollapsePanel(
			"Transformations", 
			selectInput(ns("data_transform_In"), "Transform data with", 
						choices = dataTransformChoices, selected = "noneTransform"),
			conditionalPanel("input.data_transform_In  == 'logTransform'", ns = ns,
							 shinyWidgets::sliderTextInput(ns("data_logTransform_In"), "\\(\\log_a(x) \\) ... a", 
							 							  choices = c(2, exp(1), 10), selected = exp(1))
			),
			conditionalPanel("input.data_transform_In  == 'powerTransform'", ns = ns,
							 sliderInput(ns("data_powerTransform_In"), "\\(x^a\\) ... a", 
							 			min = 2, max = 5, step = 1, value = 3)
			),
			conditionalPanel("input.data_transform_In  == 'rootTransform'", ns = ns,
							 sliderInput(ns("data_rootTransform_In"), "\\(\\sqrt[a]{x}\\) ... a", 
							 			min = 2, max = 5, step = 1, value = 3)
			),
			conditionalPanel("input.data_transform_In  == ''", ns = ns,
							 sliderInput(ns("data_invTransform_In"), "", min = 1, max = 2, step = 1, value = 1),
							 sliderInput(ns("data_noneTransform_In"), "", min = 1, max = 2, step = 1, value = 1)
			)
		)
	}
	
	display_options = function(){
		bsCollapsePanel(
			"Display Options", 
			sliderInput(ns("disp_alpha_In"), "Error Transparency", min = 0, max = 1, value = 0.5),
			sliderInput(ns("color_alpha_In"), "Color Transparency", min = 0, max = 1, value = 1),
			dark_plot_UI(ns("dark")),
			
			bsTooltip(ns("disp_alpha_In"), "Dispersion display transparency.", "bottom", "hover"),
			bsTooltip(ns("color_alpha_In"), "Line/Point color transparency.", "bottom", "hover"),
			fluidPage(
				conditionalPanel(
					"input.type_In.indexOf('point') > -1 || input.type_In.indexOf('jitter') > -1",
					ns = ns,
					sliderInput(ns("point_size_In"), "Point Size", min = 0.001, max = 100, value = 0.5, step = 0.25),
					hr()				 
				),
				conditionalPanel(
					"input.type_In.indexOf('line') > -1 || input.type_In.indexOf('area') > -1 || input.type_In.indexOf('smooth') > -1 || input.type_In.indexOf('quantile') > -1", 
					ns = ns,
					sliderInput(ns("line_size_In"), "Line Thickness", min = 0.001, max = 100, value = 0.5, step = 0.25),
					hr()
				)
			),
			bsTooltip(ns("point_size_In"), "Relative point size.", "bottom", "hover"),
			bsTooltip(ns("line_size_In"), "Relative line thickness.", "bottom", "hover")
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
			 			   		   groupings_colors(),
			 			   		   # bsCollapsePanel("Tests", 
			 			   		   # 				
			 			   		   # ),
			 			   		   facet_control_UI(ns("facet"), textFaceChoices),
			 			   		   ranges_colors_units(),
			 			   		   transformations(),
			 			   		   display_options(),
			 			   		   debugging_UI(ns("debug"))
			 			   )
			 		),
			 		column(8, 
			 			   plotOutput(outputId = ns("plotOut")),
			 			   stat_details_UI(ns("stats")),
			 			   tags$style(type="text/css", "#stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   actionButton(ns("plotIn"), label = "Plot Trajectory Festures"),
			 			   plot_export_UI(ns("export"))
			 		)
			 	)
			 )
	)
	
}

trajectory_features_server = function(id, data, features, tracks, trajectories, groupings, 
								 groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty,
								 groupingsChoiceswithEmptywithDoNotDisplay,
								 groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
								 trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
								 trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices){
	moduleServer(id, function(input, output, session){
		plot = eventReactive(input$plotIn, {#plot = reactive({
			
			print("output$trajFeatureOut = renderPlot({")
			if(debugging$browse){
				browser()
			}
			
			# Converting title reactives to list of values
			titles = lapply(titles, function(x){x()})
			
			xlab = axis_labs$x_lab()
			ylab = axis_labs$y_lab()
			
			if(input$y_range_check_In){yRange = input$y_range_In}else{yRange = NULL}
			
			colorGroup = input$color_In; if(colorGroup == "NULL") {colorGroup = NULL}
			lineTypeGroup = input$linetype_In; if(lineTypeGroup == "NULL") {lineTypeGroup = NULL}
			sizeVar = input$linetype_In; if(sizeVar == "NULL") {sizeVar = NULL}
			shapeGroup = input$linetype_In; if(shapeGroup == "NULL") {shapeGroup = NULL}
			colorGroup = input$color_In; if(colorGroup == "NULL") {colorGroup = NULL}
			fillGroup = input$fill_In; if(fillGroup == "NULL") {fillGroup = NULL}
			
			replicateGroup = input$replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
			facetRowGroup = facet$row_group()
			facetColGroup = facet$col_group()
			
			if(input$disp_fun_In == "NULL"){dispersion.fun = NULL}else{dispersion.fun = match.fun(input$disp_fun_In)}
			if(input$aggr_fun_In == "NULL"){aggregate.fun = NULL}else{aggregate.fun = match.fun(input$aggr_fun_In)}
			if(input$disp_type_In == "NULL"){
				dispersion.fun.type = NULL
			}else{
				dispersion.fun.type = match.fun(paste0("geom_", input$disp_type_In))
			}
			
			if(input$smooth_In){smoothWindow = input$smooth_windowIn}else{smoothWindow = 1}
			plot = plotTrajFeatures(dataTraj = trajectories(), x = input$x_In, y = input$y_In, 
									type = input$type_In, trackGlobalIDName = "track_global_id",
									x.unit = axis_labs$x_unit(), y.unit = axis_labs$y_unit(), 
									y.Range = yRange,
									colorGroupName = colorGroup, fillGroupName = fillGroup, sizeVarName = sizeVar,
									lineTypeGroupName = lineTypeGroup, shapeGroupName = shapeGroup,
									groupings = groupings()$groupings, 
									facet.row = facetRowGroup, facet.col = facetColGroup, 
									facet.wrap = facet$wrap(),
									title = titles$title, subtitle = titles$subtitle, 
									replicateGroupName = replicateGroup, 
									aggregate.fun = aggregate.fun, 
									dispersion.fun = dispersion.fun, dispersion.type = dispersion.fun.type,
									smooth.window = smoothWindow,
									dispAlpha = input$disp_alpha_In, colorAlpha = input$color_alpha_In, 
									x.lab = xlab, y.lab = ylab, is.dark = dark_plot(),
									facet.text.face = facet$label_face(), 
									facet.label.fill.color = facet$label_fill_color(),
									plot.subtitle.hjust = titles$subtitle_hjust, 
									plot.subtitle.size = titles$subtitle_size, 
									plot.subtitle.face = titles$subtitle_text_style, 
									linesize = input$line_size_In,
									pointsize = input$point_size_In,
									browse = debugging$browse, benchmark = debugging$benchmark, 
									verbose = debugging$verbose)
			plot	
		})
		
		transform = reactive({
			# Data Transform with parameter
			return(list(method = input$data_transform_In, 
						parameter = input[[paste(c("data", input$data_transform_In, "In"), collapse = "_")]]))
		})
		
		export_size = reactive({
			width = input$width_In; height = input$height_In
			if(input$auto_width_In){width = NA}
			if(input$auto_height_In){height = NA}
			return(list(width = width, height = height))
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
		
		output$plotOut = renderPlot({plot()$plot})
		
		
		observe({updateSelectInput(session, "x_In", choices = trajChoiceswithoutEmpty(), selected = "EDGE_TIME")})
		observe({updateSelectInput(session, "y_In", choices = trajChoiceswithoutEmpty(), selected = "VELOCITY")})
		observe({updateSelectInput(session, "fill_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "color_In", choices = groupingsChoiceswithoutEmpty())})
		observe({updateSelectInput(session, "shape_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "linetype_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "size_In", choices = trajChoiceswithEmpty())})
		
		observe({updateSelectInput(session, "stat_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "replicate_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "disp_fun_In", choices = dispersionChoices())})
		
		observe({updateSliderInput(session, "y_range_In", min = getYMin(), max = getYMax(), 
								   step = getYStep(), value = c(getYMin(), getYMax()))})
		
		getYPretty = reactive({
			if(!is.null(tracks()) && !(input$y_In == "")){
				#browser()
				transformFun = match.fun(transform()$method)
				values = trajectories()[[input$y_In]]
				unit = attr(values, "unit")
				
				if(is.null(unit)){
					unit = ""
				}
				
				unitToConvert = axis_labs$y_unit()
				if(unitToConvert == ""){
					unitToConvert = unit
				}
				
				if(!udunits2::ud.are.convertible(unit, unitToConvert)){
					unitToConvert = unit
				}
				if(!is.factor(values)){
					values = transformFun(udunits2::ud.convert(values, unit, unitToConvert), trackTransform()$parameter)
					
					#if()
					pretty(values, 20)
				}else{
					values
				}
			}
		})
		
		getYMin = reactive({
			#browser()
			prettyMin = getYPretty()[1]
			if(!is.null(prettyMin)){
				if(prettyMin > 0){
					return(0)
				}else{
					return(prettyMin * 2)
				}
			}
		})
		getYMax = reactive({
			prettyMax = last(getYPretty())
			if(!is.null(prettyMax)){
				if(prettyMax < 0){
					return(0)
				}else{
					return(prettyMax * 2)
				}
			}
			
		})
		getYStep = reactive({
			y = getYPretty()
			if(!is.null(y)){
				y[2] - y[1]
			}
		})
		
		facet = facet_control_server("facet", groupingsChoiceswithEmpty)
		plot_export_server("export", "Trajectory Feature", plot)
		stat_details_server("stats", plot)
		debugging = debugging_server("debug")
		dark_plot = dark_plot_server("dark")
		titles = titles_server("title")
		axis_labs = axis_labels_server("axis_labs", 
									   features, trajectories,
									   groups = list(x = reactive({input$x_In}), 
									   			  y = reactive({input$y_In})),
									   default_labels = list(x = reactive({input$x_In}), 
									   					  y = reactive({input$y_In})))
	})
}