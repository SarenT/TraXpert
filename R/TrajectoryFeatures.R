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
			fluidPage(
				fluidRow(
					column(10, tipify(
						sliderInput(ns("x_range_In"), "x Axis Range", 
									min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE), 
						"x axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only x axis selection is registered).", 
						"top")), 
					column(2, checkboxInput(ns("x_range_check_In"), "", value = TRUE))),
				fluidRow(
					column(10, tipify(
						sliderInput(ns("y_range_In"), "y Axis Range", 
									min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE), 
						"y axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only y axis selection is registered).", 
						"top")), 
					column(2, checkboxInput(ns("y_range_check_In"), "", value = TRUE))),
				axis_labels_UI(ns("axis_labs")),
				fluidRow(
					axis_transform_UI(ns("axis_transform_x"), "x", 6),
					axis_transform_UI(ns("axis_transform_y"), "y", 6)
				)
			)
		)
	}
	
	transformations = function(){
		bsCollapsePanel(
			"Transformations", 
			fluidPage(
				data_transform_UI(ns("data_transform_x"), "x"),
				data_transform_UI(ns("data_transform_y"), "y")
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
			 			   ),
			 			   feature_schematics_UI(ns("feature_schematics_x")),
			 			   feature_schematics_UI(ns("feature_schematics_y"))
			 		),
			 		column(8, 
			 			   plotOutput(outputId = ns("plotOut")),
			 			   stat_details_UI(ns("stats")),
			 			   tags$style(type="text/css", "#stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   actionButton(ns("plotIn"), label = "Plot Trajectory Features"),
			 			   plot_export_UI(ns("export"))
			 		)
			 	)
			 )
	)
	
}

trajectory_features_server = function(id, data, features, tracks, trajectories, groupings, choices){
	#' Plots trajectory features
	#'
	#' @param dataTraj trajectory features plot
	#' @param x 
	#' @param y 
	#' @param type 
	#' @param trackGlobalIDName 
	#' @param groupings 
	#' @param x.unit 
	#' @param y.unit 
	#' @param y.Range y axis range in a vector c(min, max)
	#' @param fillGroupName 
	#' @param colorGroupName 
	#' @param groupTracks 
	#' @param shapeGroupName 
	#' @param lineTypeGroupName 
	#' @param sizeVarName 
	#' @param color.legend 
	#' @param alpha.legend 
	#' @param inverse 
	#' @param facet.row 
	#' @param facet.col 
	#' @param facet.wrap 
	#' @param title 
	#' @param subtitle 
	#' @param smooth.window 
	#' @param replicateGroupName 
	#' @param aggregate.fun 
	#' @param dispersion.fun 
	#' @param dispersion.type 
	#' @param linesize 
	#' @param pointsize 
	#' @param limitNTracks 
	#' @param randomizeTrackSampling 
	#' @param trackReduced 
	#' @param spotReduced 
	#' @param colorAlpha 
	#' @param fillAlpha 
	#' @param dispAlpha 
	#' @param is.dark 
	#' @param plot.subtitle.hjust 
	#' @param plot.subtitle.size 
	#' @param plot.subtitle.face 
	#' @param h.line 
	#' @param v.line 
	#' @param panel.border 
	#' @param panel.grid.major 
	#' @param facet.label.fill.color 
	#' @param facet.text.face 
	#' @param x.lab 
	#' @param y.lab 
	#' @param browse 
	#' @param verbose 
	#' @param benchmark 
	#' @param initializeProg 
	#' @param updateProg 
	#' @param closeProg 
	#'
	#' @return
	#' @export
	#'
	#' @examples
	plot_data = function(dataTraj, x, y, type, trackGlobalIDName, groupings, x.unit = NULL, y.unit = NULL, 
						 x.Range = NULL, y.Range = NULL,
						 fillGroupName = NULL, colorGroupName = NULL, #colorReverseOrder = FALSE, 
						 groupTracks = FALSE,
						 shapeGroupName = NULL, lineTypeGroupName = NULL, sizeVarName = NULL, 
						 #alphaGroupName = NULL, 
						 coord_trans_x = NULL, coord_trans_y = NULL,
						 color.legend = NULL, alpha.legend = NULL, #fill.legend = NULL, 
						 inverse = FALSE, facet.row = NULL, facet.col = NULL, facet.wrap = FALSE,
						 title = NA, subtitle = NULL,
						 #statGroupName = NULL, stat.label = "..p.signif..", stat.method = "wilcox.test", 
						 #hide.ns = FALSE, 
						 smooth.window = 1,
						 data.x.transform = NULL, data.y.transform = NULL,
						 replicateGroupName = NULL, aggregate.fun = NULL, 
						 dispersion.fun = NULL, dispersion.type = NULL,
						 linesize = 1, pointsize = 1,
						 limitNTracks = FALSE, randomizeTrackSampling = FALSE,
						 trackReduced = 1, spotReduced = 1,
						 colorAlpha = 1.0, fillAlpha = 1.0, dispAlpha = 1.0, is.dark = FALSE, 
						 plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
						 h.line = TRUE, v.line = TRUE, panel.border = FALSE, panel.grid.major = FALSE,
						 facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold", 
						 x.lab = NULL, y.lab = NULL, browse = FALSE, verbose = FALSE, benchmark = FALSE,
						 initializeProg = NULL, updateProg = NULL, closeProg = NULL){
		if(is.function(initializeProg) && is.function(updateProg)){
			initializeProg(message = "Generating trajectory plot...", 
						   max = 4)
		}
		# Hack, turns out that this input isn't even required. Aggregate function automatically means to group tracks.
		if(!is.null(aggregate.fun)){
			groupTracks = TRUE
		}
		
		if(!release && browse) browser()
		if(benchmark) startTime = benchMark()
		if(verbose) cat("Preparing names and expressions...\n")
		
		default.x.Unit = attr(dataTraj[[x]], "unit"); default.y.Unit = attr(dataTraj[[y]], "unit")
		
		dataTraj = dataTraj %>% filter(!is.na(.data[[y]]))
		
		# Issues wirh the log transformations
		if(grepl("log", coord_trans_x$method())){
			dataTraj = dataTraj %>% filter(is.positive(.data[[x]]))
			placeholder.x.Range = ifelse(x.Range[2] <= 1, x.Range[2] / 10, 1)
			x.Range = replace(x.Range, x.Range <= 0, placeholder.x.Range)
		}
		
		if(grepl("log", coord_trans_y$method())){
			dataTraj = dataTraj %>% filter(is.positive(.data[[y]]))
			placeholder.y.Range = ifelse(y.Range[2] <= 1, y.Range[2] / 10, 1)
			y.Range = replace(y.Range, y.Range <= 0, placeholder.y.Range)
		}
		
		x.Breaks = pretty(x.Range)
		y.Breaks = pretty(y.Range)
		
		allGroupswoRep = unique(c(trackGlobalIDName, colorGroupName, facet.row, facet.col, shapeGroupName,
								  lineTypeGroupName, fillGroupName, sizeVarName))
		allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
		
		# Transforming data
		dataTraj = data_transform(dataTraj, x, data.x.transform, default.x.Unit, x.unit)
		xtransformFormulaFun = data.x.transform$formula_func()
		
		dataTraj = data_transform(dataTraj, y, data.y.transform, default.y.Unit, y.unit)
		ytransformFormulaFun = data.y.transform$formula_func()
		
		# Aggregation is required for subtitle but also limiting number of tracks for groups.
		if(is.null(subtitle) || limitNTracks){
			# Aggregate trajectory data to get length for all fields (number of spots in track). Aggregation results in 
			# rows with track global id (unique values) non-repetitive and groupings. Only the columns of all groups 
			# returned. 1 track = 1 row. So "length" info is actually lost. But we can use this to count tracks.
			
			aggrTracks = dataTraj %>% group_by_at(allGroupswRep) %>% summarise(n())
			if(is.null(aggrTracks)){
				aggrTracks = dataTraj
			}
			aggrTracks = aggrTracks[, allGroupswRep]
			
			if(!is.null(replicateGroupName)){
				nReplicates = length(unique(dataTraj[[replicateGroupName]]))
			}else{
				nReplicates = 1
			}
			aggrNTracks = aggrTracks
			# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
			if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
				if(is.function(updateProg)){
					updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")
				}
				# Aggregating groupings to count number of trackGlobalIDName in each group.
				aggrNTracks = aggrNTracks %>% 
					group_by_at(allGroupswRep[!allGroupswRep %in% trackGlobalIDName]) %>% 
					summarise_at(trackGlobalIDName, length)
				
			}
			
			minNTracks = min(aggrNTracks[[trackGlobalIDName]]); maxNTracks = max(aggrNTracks[[trackGlobalIDName]])
			
			if(limitNTracks){
				aggrTracksGrouped = group_by_at(aggrTracks, allGroupswRep[allGroupswRep != trackGlobalIDName])
				sampledTracks = sample_n(aggrTracksGrouped, minNTracks)[[trackGlobalIDName]]
				data = filter(dataTraj, (!!as.name(trackGlobalIDName)) %in% sampledTracks)
				
				aggrTracks = aggregate(reformulateT(allGroupswRep, "."), data = dataTraj, length)[, allGroupswRep]
				#nReplicates = nrow(aggregate(reformulateT(replicateGroupName, "."), data = dataTraj, length))
				
				# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
				if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
					if(is.function(updateProg)){
						updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")
					}
					# Aggregating groupings to count number of trackGlobalIDName in each group.
					aggrNTracks = aggregate(reformulateT(allGroupswRep[!allGroupswRep %in% trackGlobalIDName], "."), 
											data = aggrTracks, length)
				}
				minNTracks = min(aggrNTracks[[trackGlobalIDName]]); maxNTracks = max(aggrNTracks[[trackGlobalIDName]])
			}
			
			firstPart = ""
			if(!is.null(replicateGroupName)){
				if(is.function(updateProg)){updateProg(value = 3, detail = "Aggregating data (3/3) (this may take some time)...")}
				firstPart = paste0("n=", nReplicates, ", ")
			}
			subtitle = paste0(firstPart, "# of tracks each group/replicate=")
			if(minNTracks == maxNTracks){
				subtitle = paste0(subtitle, minNTracks)
			}else{
				subtitle = paste0(subtitle, minNTracks, '-', maxNTracks)
			}
			
		}
		
		if(trackReduced > 1){
			trackGlobalIDs = unique(dataTraj$track_global_id)
			trackGlobalIDs = trackGlobalIDs[seq(1, length(trackGlobalIDs), trackReduced)]
			dataTraj = dataTraj %>% filter(track_global_id %in% trackGlobalIDs)
			if(!is.null(subtitle)){
				if(is.character(subtitle)){
					subtitle = paste0(subtitle, ", displaying 1 in ", trackReduced, " tracks")
				}
			}
		}
		
		if(spotReduced > 1){
			dataTraj = dataTraj %>% group_by(track_global_id) %>% slice(seq(1, n(), by = spotReduced))
			if(!is.null(subtitle)){
				if(trackReduced > 1){
					midText = " and 1 in "
				}else{
					midText = ", displaying 1 in "
				}
				if(is.character(subtitle)){
					subtitle = paste0(subtitle, midText, spotReduced, " spots")
				}
			}
		}
		 
		colorGroup = nameToExpr(colorGroupName)
		shapeGroup = nameToExpr(shapeGroupName); fillGroup = nameToExpr(fillGroupName)
		lineTypeGroup = nameToExpr(lineTypeGroupName); sizeVar = nameToExpr(sizeVarName)
		trackGroup = nameToExpr(trackGlobalIDName)
		
		trackGlobalID = nameToExpr(trackGlobalIDName)
		
		if(benchmark) startTime = benchMark("Units, groups, expressions", startTime)
		
		if(verbose) cat("Units...\n")
		if(is.null(x.unit)) x.unit = default.x.Unit
		if(is.null(y.unit)) y.unit = default.y.Unit
		if(verbose) cat("x/y labels...\n")
		
		if(is.null(x.lab)){
			x.labDisp = ""
		}else{
			x.labDisp = TeX(paste0(xtransformFormulaFun(gsub(" ", "~", x.lab), data.x.transform$parameter()), 
								   " \\[", xtransformFormulaFun(x.unit, data.x.transform$parameter()), "\\]"))
		}
		
		if(is.null(y.lab)){
			y.labDisp = ""
		}else{
			y.labDisp = TeX(paste0(ytransformFormulaFun(gsub(" ", "~", y.lab), data.y.transform$parameter()), 
								   " \\[", ytransformFormulaFun(y.unit, data.y.transform$parameter()), "\\]"))
		}
		
		if(verbose) cat("Generating the plot...\n")
		
		xVar = sym(x)
		
		if(smooth.window == 1){
			yVar = sym(y)
		}else{
			dataTraj = dataTraj %>% mutate_at(y, ~smoothLineWithNA(., smooth.window))
			yVar = sym(y)
		}
		
		if(groupTracks){
			plot = ggplot(dataTraj, 
						  aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
						  	linetype = !!lineTypeGroup, shape = !!shapeGroup))
		}else{
			plot = ggplot(dataTraj, 
						  aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
						  	linetype = !!lineTypeGroup, shape = !!shapeGroup, track = !!trackGlobalID))
		}
		
		if(is.function(aggregate.fun)){
			aggrGroups = c(allGroupswoRep, x)
			if(groupTracks){
				aggrGroups = aggrGroups[aggrGroups != aggrGroups[1]]
			}
			
			dataTrajAggr = aggregate(reformulateT(aggrGroups, y), data = dataTraj, aggregate.fun)
			
			if(is.function(dispersion.fun)){
				dataTrajDisp = aggregate(reformulateT(aggrGroups, y), data = dataTraj, dispersion.fun)
				ranges = dataTrajDisp[, ncol(dataTrajDisp)]
				colnames(dataTrajDisp)[ncol(dataTrajDisp)] = "tx_lower"
				colnames(dataTrajDisp)[ncol(dataTrajDisp) - 1] = "tx_mid"
				colnames(dataTrajDisp)[ncol(dataTrajDisp) - 2] = "tx_upper"
				dataTrajAggr$tx_lower = ranges[, 3]
				dataTrajAggr$tx_mid = ranges[, 2]
				dataTrajAggr$tx_upper = ranges[, 1]
			}
			
			if(groupTracks){
				plot = ggplot(
					dataTrajAggr, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, 
									  size = !!sizeVar, linetype = !!lineTypeGroup, shape = !!shapeGroup))
			}else{
				plot = ggplot(
					dataTrajAggr, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, 
									  size = !!sizeVar, linetype = !!lineTypeGroup, shape = !!shapeGroup, 
									  tracks = !!trackGroup))
			}
			
			
			if(is.function(dispersion.fun)){
				if(is.function(dispersion.type)){
					color = "black"
						if(is.dark){color = "white"}
					plot = plot + 
						dispersion.type(aes(ymin = tx_lower, ymax = tx_upper, fill = !!colorGroup), alpha = dispAlpha)
				}
			}
		}
		
		plot = plot + labs(color = getGLab(groupings, colorGroupName), fill = getGLab(groupings, fillGroupName), 
						   #alpha = getGLab(groupings, alphaGroupName), 
						   shape = getGLab(groupings, shapeGroupName), 
						   linetype = getGLab(groupings, lineTypeGroupName), size = getGLab(groupings, sizeVarName), 
						   x = x.labDisp, y = y.labDisp)
		
		color = "black"
			if(is.dark) color = "white"
			
		if("point" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_point(color = "white", size = pointsize)
			}else{
				plot = plot + geom_point(size = pointsize)
			}
		}
		if("jitter" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_jitter(color = "white", size = pointsize)
			}else{
				plot = plot + geom_jitter(size = pointsize)
			}
		}
		if("quantile" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_quantile(color = "white", size = linesize)
			}else{
				plot = plot + geom_quantile(size = linesize)
			}
		}
		if("smooth" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_smooth(model = lm, color = "white", size = linesize)
			}else{
				plot = plot + geom_smooth(model = lm, size = linesize)
			}
		}
		if("line" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_line(color = "white", size = linesize)
			}else{
				plot = plot + geom_line(size = linesize)
			}
		}
		if("area" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_area(color = "white", size = linesize)
			}else{
				plot = plot + geom_area(size = linesize)
			}
		}
		if("step" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_step(direction = "hv", color = "white", size = linesize)
			}else{
				plot = plot + geom_step(direction = "hv", size = linesize)
			}
		}
		
		plot = plot + theme_classic()
		if(benchmark) startTime = benchMark("Generating labels and plot", startTime)
		if(inverse){
			if(verbose) cat("Inverting...\n")
			plot = plot + scale_y_reverse()
			if(benchmark) startTime = benchMark("Inverting", startTime)
		}
		# If both facet variables are NULL, then don't do any faceting, otherwise check if any of them is null
		
		plot = facetPlot(plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
		
		plot = titlePlot(p = plot, title = title)
		
		if(is.function(updateProg)){
			updateProg(value = 1, detail = "Aggregating data (1/3) (this may take some time)...")
		}
		
		if(benchmark) startTime = benchMark("Facet, title", startTime)
		if(is.null(subtitle)){
			if(verbose) cat("Generating subtitle...\n")
			
			
		}
		if(benchmark) startTime = benchMark("subtitle", startTime)
		plot = subtitlePlot(p = plot, subtitle = subtitle)
		
		if(verbose) cat("Color scales...\n")
		plot = colorPlot(plot, dataTraj, groupings, colorGroupName, colorAlpha, is.dark)
		
		plot = fillPlot(plot, dataTraj, groupings, fillGroupName, fillAlpha, is.dark)
		
		plot = plot + guides(color = color.legend, alpha = alpha.legend)
		# browser()
		plot = plot + scale_x_continuous(breaks = x.Breaks) + 
			scale_y_continuous(breaks = y.Breaks) + 
			coord_trans(x = coord_trans_x$method(), y = coord_trans_y$method(), xlim = x.Range, ylim = y.Range)
		
		if(verbose) cat("Theme...\n")
		plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, 
							facet.label.fill.color, facet.text.face)
		
		if(panel.border) plot = plot + theme(panel.border = element_rect(color = "black", fill=NA, size = 1))
		if(panel.grid.major) plot = plot + theme(panel.grid.major = element_line(color = "black", size = 0.2))
		if(is.dark) plot = plot + theme_black()
		
		if(benchmark) startTime = benchMark("Labels, color scales, theme", startTime)
		
		if(verbose) cat("Plot ready...\n")
		if(is.function(closeProg)){closeProg()}
		return(list(plot = plot, stat = NULL, replicates = NULL, tracks = NULL))
	}
	
	moduleServer(id, function(input, output, session){
		plot = eventReactive(input$plotIn, {#plot = reactive({
			
			print("output$trajFeatureOut = renderPlot({")
			if(!release && debugging$browse) browser()
			
			# Converting title reactives to list of values
			titles = lapply(titles, function(x){x()})
			
			xlab = axis_labs$x_lab()
			ylab = axis_labs$y_lab()
			
			if(input$x_range_check_In){xRange = input$x_range_In}else{xRange = NULL}
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
			plot = plot_data(dataTraj = trajectories(), x = input$x_In, y = input$y_In, 
							 type = input$type_In, trackGlobalIDName = "track_global_id",
							 x.unit = axis_labs$x_unit(), y.unit = axis_labs$y_unit(), 
							 x.Range = xRange, y.Range = yRange,
							 colorGroupName = colorGroup, fillGroupName = fillGroup, sizeVarName = sizeVar,
							 coord_trans_x = x_axis_transform, coord_trans_y = y_axis_transform,
							 lineTypeGroupName = lineTypeGroup, shapeGroupName = shapeGroup,
							 groupings = groupings()$groupings, 
							 facet.row = facetRowGroup, facet.col = facetColGroup, 
							 facet.wrap = facet$wrap(),
							 title = titles$title, subtitle = titles$subtitle, 
							 replicateGroupName = replicateGroup, 
							 aggregate.fun = aggregate.fun, 
							 dispersion.fun = dispersion.fun, dispersion.type = dispersion.fun.type,
							 smooth.window = smoothWindow,
							 data.x.transform = x_transform, data.y.transform = y_transform,
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
		
		
		observe({updateSelectInput(session, "x_In", choices = choices$trajChoiceswithoutEmpty(), 
								   selected = "EDGE_TIME")})
		observe({updateSelectInput(session, "y_In", choices = choices$trajChoiceswithoutEmpty(), 
								   selected = "VELOCITY")})
		observe({updateSelectInput(session, "fill_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "color_In", choices = choices$groupingsChoiceswithoutEmpty())})
		observe({updateSelectInput(session, "shape_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "linetype_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "size_In", choices = choices$trajChoiceswithEmpty())})
		
		observe({updateSelectInput(session, "stat_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "replicate_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "disp_fun_In", choices = choices$dispersionChoices())})
		
		observe({updateSliderInput(session, "y_range_In", min = getYMin(), max = getYMax(), 
								   step = getYStep(), value = c(getYMin(), getYMax()))})
		observe({updateSliderInput(session, "x_range_In", min = getXMin(), max = getXMax(), 
								   step = getXStep(), value = c(getXMin(), getXMax()))})
		
		getYPretty = reactive({
			if(!is.null(trajectories()) && !(input$y_In == "")){
				# browser()
				transformFun = y_transform$func()
				values = trajectories()[[input$y_In]]
				unit = attr(values, "unit")
				
				if(is.null(unit)){
					unit = ""
				}
				
				unitToConvert = axis_labs$y_unit()
				if(unitToConvert == ""){
					unitToConvert = unit
				}
				
				if(!is.null(unit) && !is.null(unitToConvert) && !udunits2::ud.are.convertible(unit, unitToConvert)){
					unitToConvert = unit
				}
				if(!is.factor(values)){
					values = transformFun(udunits2::ud.convert(values, unit, unitToConvert), y_transform$parameter())
					
					#if()
					pretty(values, 20)
				}else{
					values
				}
			}else{
				return(NULL)
			}
		})
		getXPretty = reactive({
			if(!is.null(trajectories()) && !(input$x_In == "")){
				# browser()
				transformFun = x_transform$func()
				values = trajectories()[[input$x_In]]
				unit = attr(values, "unit")
				
				if(is.null(unit)){
					unit = ""
				}
				
				unitToConvert = axis_labs$x_unit()
				if(unitToConvert == ""){
					unitToConvert = unit
				}
				
				if(!is.null(unit) && !is.null(unitToConvert) && !udunits2::ud.are.convertible(unit, unitToConvert)){
					unitToConvert = unit
				}
				if(!is.factor(values)){
					values = transformFun(udunits2::ud.convert(values, unit, unitToConvert), x_transform$parameter())
					
					#if()
					pretty(values, 20)
				}else{
					values
				}
			}else{
				return(NULL)
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
			}else{
				return(0)
			}
		})
		getYMax = reactive({
			# browser()
			prettyMax = getYPretty()
			if(!is.null(prettyMax)){
				prettyMax = last(prettyMax)
				if(prettyMax < 0){
					return(0)
				}else{
					return(prettyMax * 2)
				}
			}else{
				return(1)
			}
			
		})
		getYStep = reactive({
			y = getYPretty()
			if(!is.null(y)){
				y[2] - y[1]
			}else{
				return(0.1)
			}
		})
		
		getXMin = reactive({
			#browser()
			prettyMin = getXPretty()[1]
			if(!is.null(prettyMin)){
				if(prettyMin > 0){
					return(0)
				}else{
					return(prettyMin * 2)
				}
			}else{
				return(0)
			}
		})
		getXMax = reactive({
			# browser()
			prettyMax = getXPretty()
			if(!is.null(prettyMax)){
				prettyMax = last(prettyMax)
				if(prettyMax < 0){
					return(0)
				}else{
					return(prettyMax * 2)
				}
			}else{
				return(1)
			}
			
		})
		getXStep = reactive({
			x = getXPretty()
			if(!is.null(x)){
				x[2] - x[1]
			}else{
				return(0.1)
			}
		})
		
		feature_schematics_server("feature_schematics_x", feature = reactive({input$x_In}))
		feature_schematics_server("feature_schematics_y", feature = reactive({input$y_In}))
		facet = facet_control_server("facet", choices)
		plot_export_server("export", "Trajectory Feature", plot)
		stat_details_server("stats", plot)
		x_transform = data_transform_server("data_transform_x")
		y_transform = data_transform_server("data_transform_y")
		x_axis_transform = axis_transform_server("axis_transform_x")
		y_axis_transform = axis_transform_server("axis_transform_y")
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