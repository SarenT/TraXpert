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
								trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
								trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices){
	
	
	#' Plots trajectories of particles
	#'
	#' @param dataTraj data frame of trajectories
	#' @param x character column name of x coordinates
	#' @param y character column name of y coordinates
	#' @param trackGlobalIDName character column name of global track IDs
	#' @param groupings data frame of the groupings
	#' @param x.unit character unit of the x coordinate to be converted to
	#' @param y.unit character unit of the y coordinate to be converted to
	#' @param colorGroupName character column name of the color group name
	#' @param colorTrajectories logical to whether or not to color trajectories
	#' @param startPointGroupName 
	#' @param endPointGroupName 
	#' @param coord_equal logical whether or not have equal aspect ratio in x and y axis
	#' @param equalRange logical whether or not have equal ranges in x and y axis
	#' @param color.legend not used 
	#' @param alpha.legend not used
	#' @param inverse logical whether or not to invert y axis. Used as image data pixel y coordindate increases by going down
	#' @param facet.row character column name of the facet rows
	#' @param facet.col character column name of the facet columns
	#' @param facet.wrap logical facet wrap option
	#' @param title character to be set as the primary title
	#' @param subtitle character to be set as the secondary title
	#' @param hide.ns logical to hide non-significance indicator
	#' @param replicateGroupName character column name of the replicates group name
	#' @param is.dark logical whether or not to plot in the dark mode
	#' @param limitNTracks numeric to limit number of tracks to be plotted for each group
	#' @param randomizeTrackSampling logical whether or not to randomize track sampling to be used with limitNTracks
	#' @param trackReduced integer ratio of track reduction. Every nth track is plotted
	#' @param spotReduced integer ratio of spot reduction. Every nth spot is plotted for each track
	#' @param colorAlpha numeric color opacity 0-1
	#' @param plot.subtitle.hjust character secondary title horizontal alignment
	#' @param plot.subtitle.size numeric secondary title text size
	#' @param plot.subtitle.face character secondary title text face
	#' @param h.line logical option for horizontal line at y=0
	#' @param v.line logical option for vertical line at x=0 
	#' @param panel.border logical option panel border. This is the plot area between titles, legends and axis.
	#' @param panel.grid.major logical option for a grid on the plot area
	#' @param facet.label.fill.color Facet label background color
	#' @param facet.text.face character Facet label text style
	#' @param linesize numeric line thickness of the trajectories
	#' @param x.lab character to be set as the x axis title
	#' @param y.lab character to be set as the y axis title
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
	plot_data = function(dataTraj, x, y, trackGlobalIDName, groupings, x.unit = NULL, y.unit = NULL, 
								colorGroupName = NULL, colorTrajectories = TRUE, #colorReverseOrder = FALSE, 
								startPointGroupName = NA, endPointGroupName = NA, 
								#alphaGroupName = NULL, 
								coord_equal = TRUE, equalRange = TRUE, #fillGroupName = NULL, 
								color.legend = NULL, alpha.legend = NULL, #fill.legend = NULL, 
								inverse = TRUE, facet.row = NULL, facet.col = NULL, facet.wrap = FALSE,
								title = NA, subtitle = NULL,
								hide.ns = FALSE, 
								replicateGroupName = NULL, is.dark = FALSE, 
								limitNTracks = FALSE, randomizeTrackSampling = FALSE,
								trackReduced = 1, spotReduced = 1,
								colorAlpha = 1.0, #fillAlpha = 1.0, 
								plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
								h.line = TRUE, v.line = TRUE, panel.border = FALSE, panel.grid.major = FALSE,
								facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold", linesize = 1, 
								x.lab = NULL, y.lab = NULL, browse = FALSE, verbose = FALSE, benchmark = FALSE,
								initializeProg = NULL, updateProg = NULL, closeProg = NULL){
		if(is.function(initializeProg) && is.function(updateProg)){
			initializeProg(message = "Generating trajectory plot...", max = 4)
		}
		
		if(browse) browser()
		if(benchmark) startTime = benchMark()
		if(verbose) cat("Preparing names and expressions...\n")
		
		default.x.Unit = attr(dataTraj[[x]], "unit"); default.y.Unit = attr(dataTraj[[y]], "unit")
		
		allGroupswoRep = unique(
			c(trackGlobalIDName, colorGroupName, facet.row, facet.col, startPointGroupName, endPointGroupName))
		colorAsFeature = FALSE
		if(!colorGroupName %in% groupings$names && colorGroupName %in% colnames(dataTraj)){
			colorAsFeature = TRUE
			allGroupswoRep = unique(c(trackGlobalIDName, facet.row, facet.col, startPointGroupName, endPointGroupName))
		}
		
		# Required for start point and end point. Otherwise subsetting data frame for a column with NA throws error.
		allGroupswoRep = allGroupswoRep[!is.na(allGroupswoRep)] 
		allGroupswoRep = allGroupswoRep[!is.null(allGroupswoRep)]
		
		allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
		allGroupswRepwoTrGlobID = allGroupswRep[!(allGroupswRep %in% trackGlobalIDName)]
		allGroupswoRepwoTrGlobID = allGroupswRep[!(allGroupswRep %in% trackGlobalIDName)]
		
		
		aggrSpots = data.frame(); aggrTracks = data.frame()
		aggrSpots = tryCatch(aggregateByGroups(data = dataTraj, groups = allGroupswRep, length), 
							 error = retNULL)
		if(is.null(aggrSpots)){
			aggrSpots = dataTraj
		}
		# to remove identical rows, why should this be possible?
		aggrSpots = unique(aggrSpots[, c(allGroupswRep, y)])
		# Number of spots set to 1 to make each track is actually 1 (naturally)
		aggrTracks = aggrSpots; aggrTracks[[y]] = 1 
		# How many tracks per replicate and group
		aggrTracksPerRep = tryCatch(
			aggregateByGroups(data = aggrTracks, 
							  groups = allGroupswRep[!(allGroupswRep %in% trackGlobalIDName)], length), 
			error = retNULL)
		if(is.null(aggrTracksPerRep)){
			aggrTracksPerRep = aggrSpots
		}
		#aggrSpots = unique(aggrSpots[, c(allGroupswRep, y)])
		# How many tracks per group
		aggrTracksRep = tryCatch(
			aggregateByGroups(data = aggrTracks, 
							  groups = allGroupswoRep[!(allGroupswoRep %in% trackGlobalIDName)], length), 
			error = retNULL)
		
		# Aggregation is required for subtitle but also limiting number of tracks for groups.
		if(is.null(subtitle) || limitNTracks){
			# Aggregate trajectory data to get length for all fields (number of spots in track). Aggregation results in 
			# rows with track global id (unique values) non-repetitive and groupings. Only the columns of all groups 
			# returned. 1 track = 1 row. So "length" info is actually lost. But we can use this to count tracks.
			
			if(!is.null(replicateGroupName)){
				nReplicates = length(unique(dataTraj[[replicateGroupName]]))
			}else{
				nReplicates = 1
			}
			
			# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
			if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
				if(is.function(updateProg)){
					updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")
				}
			}
			
			minNTracks = min(aggrTracksPerRep[[trackGlobalIDName]])
			maxNTracks = max(aggrTracksPerRep[[trackGlobalIDName]])
			
			if(limitNTracks){
				aggrTracksGrouped = group_by_at(aggrTracks, allGroupswRep[allGroupswRep != trackGlobalIDName])
				sampledTracks = sample_n(aggrTracksGrouped, minNTracks)[[trackGlobalIDName]]
				data = filter(dataTraj, (!!as.name(trackGlobalIDName)) %in% sampledTracks)
				
				aggrTracks = aggregate(reformulateT(allGroupswRep, "."), data = dataTraj, length)[, allGroupswRep]
				
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
				if(is.function(updateProg)){
					updateProg(value = 3, detail = "Aggregating data (3/3) (this may take some time)...")
				}
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
		
		colorGroup = nameToExpr(colorGroupName)#; alphaGroup = nameToExpr(alphaGroupName)
		startPointGroup = nameToExpr(startPointGroupName); endPointGroup = nameToExpr(endPointGroupName)
		#statGroup = nameToExpr(statGroupName)
		trackGlobalID = nameToExpr(trackGlobalIDName)
		
		if(benchmark) startTime = benchMark("Units, groups, expressions", startTime)
		#if(inverse)	dataTraj[[y]] = dataTraj[[y]] * (-1)
		
		if(verbose) cat("Units...\n")
		if(is.null(x.unit)) x.unit = default.x.Unit
		if(is.null(y.unit)) y.unit = default.y.Unit
		if(verbose) cat("x/y labels...\n")
		
		if(is.null(x.lab)) x.lab = getGLab(groupings, x)
		if(is.null(y.lab)) y.lab = getGLab(groupings, y)
		
		x.lab = paste0(x.lab, " [", x.unit, "]")
		y.lab = paste0(y.lab, " [", y.unit, "]")
		
		if(verbose) cat("Generating the plot...\n")
		
		
		xVar = unitConversion(defaultUnit = default.x.Unit, unit = x.unit, x)
		
		yVar = unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y)
		
		plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, track = !!trackGlobalID))
		
		color = "black"
			if(is.dark) color = "white"
			
		if(h.line) plot = plot + geom_hline(aes(yintercept = 0), color = color)
		if(v.line) plot = plot + geom_vline(aes(xintercept = 0), color = color)
		if(colorTrajectories){
			plot = plot + geom_path(size = linesize, linejoin="round", lineend = "round")	
		}else{
			plot = plot + geom_path(size = linesize, color = "grey", linejoin="round", lineend = "round")
		}
		
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
		
		if(is.na(x.lab)) x.lab = ""
		if(is.na(y.lab)) y.lab = ""
		if(verbose) cat("Labels...\n")
		plot = plot + labs(x = x.lab, y = y.lab)
		
		if(verbose) cat("Color scales...\n")
		plot = colorPlot(plot, dataTraj, groupings, colorGroupName, colorAlpha, is.dark, colorAsFeature)
		
		plot = plot + guides(color = color.legend, alpha = alpha.legend)
		
		dataStartPoint = dataTraj %>% group_by(!!trackGlobalID) %>% summarise_all(first)
		dataEndPoint = dataTraj %>% group_by(!!trackGlobalID) %>% summarise_all(last)
		if(!is.null(startPointGroupName)){ # Display start points
			if(!is.na(startPointGroupName)){ # Display without color (grey)
				plot = plot + geom_point(data = dataStartPoint, aes(x = !!xVar, y = !!yVar, color = !!colorGroup))
			}
		}else{
			plot = plot + geom_point(data = dataStartPoint, aes(x = !!xVar, y = !!yVar), color = "grey")
		}
		if(!is.null(endPointGroupName)){ # Display end points
			if(!is.na(endPointGroupName)){ # Display without color (grey)
				plot = plot + geom_point(data = dataEndPoint, aes(x = !!xVar, y = !!yVar, color = !!colorGroup))
			}
		}else{
			plot = plot + geom_point(data = dataEndPoint, aes(x = !!xVar, y = !!yVar), color = "grey")
		}
		
		if(equalRange){
			x.max = max(abs(dataTraj[[x]])); x.min = -1 * x.max
			y.max = max(abs(dataTraj[[y]])); y.min = -1 * y.max
			min = min(x.min, y.min); max = max(x.max, y.max)
			plot = plot + xlim(min, max)
			plot = plot + ylim(min, max)
			if(inverse) plot = plot + ylim(max, min)
		}
		
		if(verbose) cat("Theme...\n")
		plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, 
							facet.label.fill.color, facet.text.face)
		
		if(panel.border) plot = plot + theme(panel.border = element_rect(color = "black", fill=NA, size = 1))
		if(panel.grid.major) plot = plot + theme(panel.grid.major = element_line(color = "black", size = 0.2))
		#if(is.dark) plot = plot + theme_black()
		
		if(benchmark) startTime = benchMark("Labels, color scales, theme", startTime)
		
		if(coord_equal) plot = plot + coord_equal()
		if(verbose) cat("Plot ready...\n")
		if(is.function(closeProg)){closeProg()}
		return(list(plot = plot, stat = NULL, replicates = NULL, tracks = NULL))
	}
	moduleServer(id, function(input, output, session){
		plot = eventReactive(input$plotTrajIn, {
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
			
			plot = plot_data(data = trajectories(), x = xy_names()$xVarName, y = xy_names()$yVarName, 
							 trackGlobalIDName = "track_global_id", 
							 groupings = groupings()$groupings, 
							 x.unit = axis_labs$x_unit(), y.unit = axis_labs$y_unit(),
							 colorGroupName = colorGroup,
							 startPointGroupName = startPointGroup, endPointGroupName = endPointGroup, 
							 colorTrajectories = input$color_tracks_In, 
							 coord_equal = input$coord_equal_In, 
							 inverse = input$inverse_In, equalRange = input$equal_range_In,
							 facet.row = facetRowGroup, facet.col = facetColGroup, facet.wrap = facet$wrap(),
							 title = titles$title, subtitle = titles$subtitle, 
							 replicateGroupName = replicateGroup, 
							 hide.ns = input$stat_hidens_In,
							 colorAlpha = input$color_alpha_In, 
							 is.dark = dark_plot(),
							 limitNTracks = input$limit_to_smallest_In,
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