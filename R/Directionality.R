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
			bsTooltip(
				ns("start_angle_In"), 
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


directionality_server = function(id, data, features, tracks, trajectories, groupings, choices){
	
	
	#' Plot directionality plot (aka Nightingale plot)
	#'
	#' @param dataTracks Tracks data frame
	#' @param groups Cardinal grouping groups in a named list e.g. unlist(groupingsToNamedList(groupings()$groupings, empty = FALSE), use.names = F)
	#' @param directionCatGroupName Group name containing directions categories (cardinal direction)
	#' @param cumulativeGroupName Cumulative grouping, summary.fun.name is applied on this group
	#' @param summary.fun.name Name of the summary function
	#' @param type Type of the plot e.g. "polygon" or "bar"
	#' @param groupings Groupings data frame e.g. groupings()$groupings
	#' @param colorGroupName Stroke aesthetic group name
	#' @param fillGroupName Fill aesthetic group name
	#' @param replicateGroupName Replicate aesthetic group name
	#' @param percentage Should the number of tracks be displayed as percentage of total
	#' @param replicate.summary.fun.name Summary fun on replicate group
	#' @param start.angle Rotate plot with angle
	#' @param facet.row Facet aesthetic group name
	#' @param facet.col Facet aesthetic group name
	#' @param title Plot title
	#' @param facet.wrap Wrap facets instead of displaying on a grid (good for odd number of facet groups)
	#' @param colorAlpha Stroke color opacity 0.0-1.0
	#' @param fillAlpha Fill color opacity 0.0-1.0
	#' @param is.dark Dark plot option
	#' @param line.size Line thickness
	#' @param facet.label.fill.color Facet label background color
	#' @param facet.text.face Facet label text style
	#' @param show.y.axis Show y axis (from center towards outside)
	#' @param subtitle Secondary title
	#' @param plot.subtitle.hjust Secondary title horizontal alignment
	#' @param plot.subtitle.size Secondary title text size
	#' @param plot.subtitle.face Secondary title text face
	#' @param browse Breakpoint for debugging
	#' @param verbose Verbose output to console
	#' @param skip.radar Skip radar plot for debugging
	#' @param skip.degrees Skip degress for debugging
	#' @param benchmark Benchmark code
	#'
	#' @return Returns a list with the plot, groups and statistics 	list(plot = ggplot, stat = statOut, replicates = aggrRepMergedNice, tracks = data frame, 
	#' circDataModel = list of plots with titles being names, summaryStats = tibble, uniformityDF = data frame, uniformityText = character, vonMisesFit = character)
	#' @export
	#'
	#' @examples
	plot_data = function(dataTracks, groups, directionCatGroupName, cumulativeGroupName, summary.fun.name = "sum", 
						 type = "polygon", groupings, colorGroupName = NULL, fillGroupName = NULL, 
						 replicateGroupName = NULL, percentage = FALSE, replicate.summary.fun.name = "mean",
						 #colorReverseOrder = FALSE, 
						 start.angle = 0, 
						 stat.measure = NULL, stat.method = NULL, stat.extras = NULL,
						 facet.row = NULL, facet.col = NULL, title = NA, facet.wrap = FALSE,
						 colorAlpha = 1.0, fillAlpha = 0.5, is.dark = FALSE,
						 line.size = NULL, facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold",
						 show.y.axis = FALSE, subtitle = NA,
						 plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
						 browse = 0, verbose = FALSE, skip.radar = FALSE, skip.degrees = FALSE, benchmark = FALSE){
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		if(benchmark) startTime = benchMark()
		if(verbose) cat("Calculating aggrate results...\n")
		
		allGroupswoRep = unique(c(colorGroupName, fillGroupName, facet.row, facet.col))
		allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
		
		
		# Making a numeric direction category for better plotting
		directionCatNumericGroupName = paste(directionCatGroupName, "NUMERIC", sep = "_")
		dataTracks[[directionCatNumericGroupName]] = round( # Intervals are rounded, imprecise, after converting to degrees
			unlist( # We need a vector to insert into data frame
				lapply( # Taking mean of lower and higher bounds
					lapply( # Converting terms to numeric
						# Splitting by the comma after removing brackets "(" and "]"
						strsplit(stringr::str_remove_all(as.character(dataTracks[[directionCatGroupName]]),
														 "[\\(\\]]"), ","), as.numeric), mean)) * 180 / pi)
		
		if(benchmark) startTime = benchMark("Groups...", startTime)
		
		if(!directionCatNumericGroupName %in% groups){groups = c(directionCatNumericGroupName, groups)}
		
		if(!is.null(replicateGroupName)){
			groupswoRep = groups[groups != replicateGroupName]
		}else{
			groupswoRep = groups
		}
		
		by.list = list()
		for(group in groups){
			by.list[[group]] = dataTracks[[group]]
		}
		
		if(is.character(summary.fun.name)){
			if(summary.fun.name != "NULL"){
				summary.fun = match.fun(summary.fun.name)
			}else{
				summary.fun = NULL # Error
			}
		}else{
			summary.fun = NULL # Error
		}
		
		if(is.character(replicate.summary.fun.name)){
			if(replicate.summary.fun.name != "NULL"){
				replicate.summary.fun = match.fun(replicate.summary.fun.name)
			}else{
				replicate.summary.fun = NULL
			}
		}else{
			replicate.summary.fun = NULL
		}
		
		if(benchmark) startTime = benchMark("Aggregates...", startTime)
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		
		if(!is.null(summary.fun)){
			if(summary.fun.name == "length"){
				cumulativeDispl = dataTracks %>% group_by_at(groups) %>% summarise_at(cumulativeGroupName, summary.fun)
			}else{
				cumulativeDispl = dataTracks %>% group_by_at(groups) %>% 
					summarise_at(cumulativeGroupName, summary.fun, na.rm = TRUE)
			}
		}else{
			#Error cumulativeDispl = dataTracks[, cumulativeGroupName]
		}
		
		aggrRep = data.frame(); aggrRepMerged = data.frame(); aggrRepMergedNice = data.frame()
		aggr = data.frame(); aggrNice = data.frame() 
		if(!is.null(replicateGroupName)){
			# All tracks separated by all groupings including replicates
			aggrRep = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(cumulativeGroupName, length)
			# All tracks separated by all groupings WITHOUT replicates, hence replicates are summarised
			aggrRepMerged = aggrRep %>% group_by_at(allGroupswoRep) %>% summarise_at(cumulativeGroupName, length)
			# aggrRepMerged with nicer titles for output table
			aggrRepMergedNice = aggrRepMerged
			newLabels = 
				as.character(as.character(groupings$labels)[match(colnames(aggrRepMergedNice), groupings$names)])
			newLabels[length(newLabels)] = "Number of replicates"
			colnames(aggrRepMergedNice) = newLabels
		}
		
		aggr = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(cumulativeGroupName, length)
		aggrNice = aggr
		colnames(aggrNice) = 
			c(as.character(groupings$labels)[match(colnames(aggrNice)[1:(ncol(aggrNice) - 1)], groupings$names)], 
			  "Number of tracks")
		
		by.list.rep = by.list; by.list.rep[replicateGroupName] = NULL
		if(!is.null(replicate.summary.fun)){
			#cumulativeDispl = aggregate(dataTracks[, cumulativeGroupName], by = by.list, FUN = summary.fun)
			if(replicate.summary.fun.name == "length"){
				cumulativeDispl = cumulativeDispl %>% group_by_at(groupswoRep) %>% 
					summarise_at(cumulativeGroupName, replicate.summary.fun())
			}else{
				cumulativeDispl = cumulativeDispl %>% group_by_at(groupswoRep) %>% 
					summarise_at(cumulativeGroupName, replicate.summary.fun, na.rm = TRUE)
			}
		}else{
			#Error cumulativeDispl = dataTracks[, cumulativeGroupName]
		}
		
		colnames(cumulativeDispl)[length(colnames(cumulativeDispl))] = cumulativeGroupName
		
		
		if(summary.fun.name == "length" && percentage){
			percSummaryGroups = names(by.list.rep)
			percSummaryGroups = percSummaryGroups[percSummaryGroups != directionCatNumericGroupName]
			
			names(cumulativeDispl)[names(cumulativeDispl) == cumulativeGroupName] = "X_PERC"
			cumulativeDispl = cumulativeDispl %>% 
				group_by_at(percSummaryGroups) %>% 
				mutate(X_PERC = 100 * X_PERC / sum(X_PERC))
			names(cumulativeDispl)[names(cumulativeDispl) == "X_PERC"] = cumulativeGroupName
			
		}
		
		if(benchmark) startTime = benchMark("Aggregates...", startTime)
		if(verbose) cat("Computing h- and vlines...\n")
		y_lims = c(0, max(cumulativeDispl[[cumulativeGroupName]]))
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		
		if(benchmark) startTime = benchMark("Group expressions...", startTime)
		directionCatNumericGroup = nameToExpr(directionCatNumericGroupName)
		cumulativeGroup = nameToExpr(cumulativeGroupName)
		colorGroup = nameToExpr(colorGroupName); fillGroup = nameToExpr(fillGroupName)
		
		if(benchmark) startTime = benchMark("Generating plot...", startTime)
		if(verbose) cat("Generating plot...\n")
		breaks = seq(y_lims[1], y_lims[2], by = (y_lims[2] - y_lims[1]) / 4)
		breaksAngle = seq(0, 360-1, by = angleCatBy * 180 / pi)
		plot = ggplot() + 
			scale_x_continuous(limits = c(-15, 345), expand = c(0, 0), breaks = breaksAngle) +
			scale_y_continuous(limits = c(0, y_lims[2]), breaks = breaks)
		
		if(is.null(line.size)){
			line.size = y_lims[2] / 400
		}
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		
		colorGroupIndices = 1:length(levels(cumulativeDispl[[colorGroupName]]))
		#if(colorReverseOrder){colorGroupIndices = rev(colorGroupIndices)}
		
		if(is.dark){
			contrast.color = "white"
		}else{
			contrast.color = "black"
		}
		stat.text.color = contrast.color
		
		#for(i in colorGroupIndices){
		#thisCol = levels(cumulativeDispl[[colorGroupName]])[i]
		if(type == "polygon"){
			hgridlines = expand.grid(x=c(breaksAngle, breaksAngle[1]), y=breaks)
			plot = plot + geom_path(hgridlines, mapping = aes(x = x, y = y, group = y), 
									size = line.size/4, color = contrast.color)
			vgridlines = expand.grid(x=breaksAngle, y=y_lims)
			plot = plot + geom_path(vgridlines, mapping = aes(x = x, y = y, group = x), 
									size = line.size/4, color = contrast.color)
			plot = plot + geom_polygon(data = cumulativeDispl %>% 
									   	complete(DIRECTION_CARDINAL_NUMERIC = seq(0, 330, 30), 
									   			 TMX_GROUP_A = unique(cumulativeDispl$TMX_GROUP_A), 
									   			 fill = list(NUMBER_SPOTS = 0)), 
									   aes(x = !!directionCatNumericGroup, y = !!cumulativeGroup, color = !!colorGroup, 
									   	fill = !!fillGroup, group = !!colorGroup), size = line.size)# + 
			#geom_polygon()
		}else if(type == "bar"){
			plot = plot + geom_hline(yintercept = breaks, color = "grey90", size = line.size, color = contrast.color) +
				geom_vline(xintercept = breaksAngle, color = "grey90", size = line.size, color = contrast.color) + 
				geom_bar(data = cumulativeDispl, width = 30,
						 aes(x = !!directionCatNumericGroup, y = !!cumulativeGroup, color = !!colorGroup, 
						 	fill = !!fillGroup, group = !!colorGroup), 
						 size = line.size, stat = "identity", alpha = fillAlpha, position = "identity")
			
		}
		#}
		
		#return(list(plot = plot, stat = NULL, replicates = NULL, tracks = NULL))
		
		if(benchmark) startTime = benchMark("Facets, titles, colors...", startTime)
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		plot = facetPlot(plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
		
		plot = titlePlot(p = plot, title = title)
		if(is.null(subtitle)){
			firstPart = ""
			if(!is.null(replicateGroupName)){
				firstPart = 
					paste0("n=", nrow(aggregate(reformulateT(replicateGroupName, "."), 
												data = dataTracks, length, na.action = NULL)), " , ")
			}
			aggr = tryCatch(aggregateByGroups(data = dataTracks, 
											  groups = c(groups[groups != directionCatGroupName], replicateGroupName), 
											  length), 
							error = retNULL)[[cumulativeGroupName]]
			
			subtitle = paste0(firstPart, "# of tracks each group/replicate=", min(aggr), '-', max(aggr))
		}
		
		if(!is.na(subtitle)) plot = subtitlePlot(p = plot, subtitle = subtitle)
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		if(verbose) cat("Setting color scales...\n")
		
		plot = colorPlot(plot, dataTracks, groupings, colorGroupName, colorAlpha, is.dark)
		
		plot = fillPlot(plot, dataTracks, groupings, fillGroupName, fillAlpha, is.dark)
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		
		if(verbose) cat("Setting labels...\n")
		if(benchmark) startTime = benchMark("Labels...", startTime)
		plot = plot + xlab("") + ylab("")
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		
		if(!skip.degrees){
			#x.labels = seq(0 + 180, 330 + 180, by = 30) %% 360 - 180; x.labels = x.labels * -1 #x.labels[7] = abs(x.labels[7])
			#plot = plot + scale_x_discrete(breaks = c(levels[1:12]), drop = FALSE, labels = paste0(x.labels, "°"))
		}
		#Legend title
		if(!is.null(colorGroupName)){
			
		}
		if(!is.null(fillGroupName)){
			plot = plot + labs(fill = as.character(getGLab(groupings, fillGroupName)))
		}
		
		if(!skip.radar) {
			if(type == "polygon"){
				plot = plot + ggiraphExtra::coord_radar(start = start.angle - pi * 7/ 12, direction = -1)	
			}else if(type == "bar"){
				# pi * 7 / 12 = 105° required to keep 0 at x axis as norm
				plot = plot + coord_polar(theta = "x", start = start.angle - pi * 7/ 12, direction = -1)
			}
		}
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		if(benchmark) startTime = benchMark("Theme...", startTime)
		plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, 
							facet.label.fill.color, facet.text.face)
		
		if(verbose) cat("Setting theme...\n")
		plot = plot + theme(strip.text.x = element_text(size = rel(0.8)), axis.text.x = element_text(size = rel(0.8)), 
							axis.line.x = element_blank(), panel.grid.major = element_blank())
		
		if(!show.y.axis) plot = plot + theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
											 axis.ticks.y = element_blank())
		dirPlot = plot
		# Histogram of each group to check data distribution
		directionGroupName = cardinalCols[[directionCatGroupName]]
		#browser()
		circDataModel = groupedCircDataModel(dataTracks, directionGroupName, directionCatGroupName, cumulativeGroupName,
											 groupings, allGroupswRep, allGroupswoRep)
		
		summaryStats = data.frame()
		for(title in names(circDataModel)){
			circGroupData = circDataModel[[title]]
			firstMoment = trigonometric.moment(circGroupData, p = 1)
			secondMoment = trigonometric.moment(circGroupData, p = 2)
			secondMomentCentered = trigonometric.moment(circGroupData, p = 2, center = TRUE)
			circV = 1 - firstMoment$rho
			circSkewness = secondMomentCentered$sin / (circV**(3/2))
			circKurtosis = (secondMomentCentered$cos - firstMoment$rho**4)/(circV**2)
			summaryStat = data.frame(`Title` = title, `n` = firstMoment$n, 
									 `μ` = firstMoment$mu * 180/pi, 
									 `median` = median.circular(circGroupData) * 180/pi,
									 # concentration parameter of the distribution. rho must be between 0 and
									 `R` = firstMoment$rho, 
									 `V` = circV, # Sample circular variance
									 `σ` = sd.circular(circGroupData),
									 # The sample circular dispersion
									 `δ` = (1-secondMoment$rho)/(2*(firstMoment$rho)**2), 
									 `w` = range.circular(circGroupData), # Circular range
									 `Angular. Dev.` = angular.deviation(circGroupData),
									 `s` = circSkewness, 
									 `k` = circKurtosis)
			summaryStats = rbind(summaryStats, summaryStat)
		}
		
		uniformityTests = data.frame()
		uniformityOutput = ""
		for(title in names(circDataModel)){
			
			circGroupData = circDataModel[[title]]
			
			rayleigh = rayleigh.test(circGroupData)
			watson = watson.test(circGroupData)
			
			testOutput = capture.output(rayleigh)
			uniformityOutput = paste(c(uniformityOutput, "", title, testOutput[testOutput != ""]), collapse = "\n")
			testOutput = capture.output(watson)
			uniformityOutput = paste(c(uniformityOutput, "", title, testOutput[testOutput != ""]), collapse = "\n")
			
			uniformityTest = data.frame(`Title` = title,
										`Rayleigh statistic` = rayleigh$statistic,
										`Rayleigh p` = rayleigh$p
			)
			uniformityTests = rbind(uniformityTests, uniformityTest)
			
		}
		
		vonMisesGoodnessOfFit = ""
		for(title in names(circDataModel)){
			circGroupData = circDataModel[[title]]
			#browser()
			watson = watson.test(circGroupData, dist = "vonmises")
			testOutput = capture.output(watson)
			vonMisesGoodnessOfFit = 
				paste(c(vonMisesGoodnessOfFit, "", title, testOutput[testOutput != ""]), collapse = "\n")
		}
		
		print("=========================")
		print(stat.measure)
		print(stat.method)
		print(stat.extras)
		print("=========================")
		
		statOut = NULL
		if(!is.null(stat.measure)){
			statMethod = match.fun(stat.method)
			statOut = tryCatch({statMethod(circDataModel)}, error = retNULL)
		}else{
			statOut = NULL
		}
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		return(list(plot = dirPlot, stat = statOut, replicates = aggrRepMergedNice, tracks = aggrNice, 
					circDataModel = circDataModel, summaryStats = summaryStats %>% tibble(), 
					uniformityDF = uniformityTests, uniformityText = uniformityOutput, 
					vonMisesFit = vonMisesGoodnessOfFit))
		#return(plot)
	}
	
	
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
			plot = plot_data(dataTracks = tracks(), groups = groupsCardinal, #directionGroupName = input$track_direction_In, 
							 directionCatGroupName = input$track_direction_cat_In, 
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
		observe({updateSelectInput(session, "track_direction_cat_In", 
								   choices = choices$trackDirectionCatChoiceswithoutEmpty(), 
								   selected = "DIRECTION_CARDINAL")})
		observe({updateSelectInput(session, "cumulation_In", choices = choices$trackChoiceswithoutEmpty(), 
								   selected = "TRACK_ID")})
		observe({updateSelectInput(session, "alpha_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "replicate_In", choices = choices$groupingsChoiceswithEmpty())})
		
		
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
		
		facet = facet_control_server("facet", choices)
		plot_export_server("export", "Directionality", plot)
		circ_stat_details_server("stats", plot)
		groupings_colors = groupings_colors_server("groupings_colors", choices)
		debugging = debugging_server("debug")
		titles = titles_server("title")
		axis_labs = axis_labels_server("axis_labs", features, tracks, 
										   groups = list(x = NULL, y = NULL),
										   default_labels = list(x = NULL, y = NULL))
		
	})
}