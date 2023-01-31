track_features_UI = function(id, title, tabColor){
	ns = NS(id)
	
	plot_type = function(){
		bsCollapsePanel(
			"Plot Type and Variables", 
			selectInput(ns("type_In"), "Plot Type", 
						choices = trackFeaturesChoices, 
						selected = "violin", multiple = TRUE),
			tipify(selectInput(ns("x_In"), "Group by", choices = list()), 
				   "Main grouping. Select a grouping, which is most interesting to compare. Also used for statistics.", 
				   placement = "top"),
			tipify(selectInput(ns("y_In"), "Measure", choices = list()), 
				   "Select the measure you are interested in to compare.", placement = "top"),
			tipify(selectInput(ns("replicate_In"), 
							   "Replicates are grouped in", 
							   choices = list()), 
				   toolTips$replicate_In, placement = "top"),
			bsTooltip(ns("type_In"), 
					  "Display type of plot to use (violin, box plot, dot plot etc.)", 
					  placement = "top", trigger = "hover")
		)
	}
	
	tests = function(ns){
		bsCollapsePanel(
			"Tests", 
			selectInput(ns("multiple_stat_method_In"), 
						"Test - Multiple Groups", 
						choices = trackMultipleStatChoices, 
						selected = "kruskal.test"),
			selectInput(ns("pairwise_stat_method_In"), "Test - Pairwise", 
						choices = trackPairwiseStatChoices, 
						selected = "wilcox.test"),
			selectInput(ns("data_transform_In"), "Transform data with", 
						choices = dataTransformChoices, 
						selected = "noneTransform"),
			conditionalPanel("input.data_transform_In  == 'logTransform'", ns = ns,
							 shinyWidgets::sliderTextInput(
							 	ns("data_logTransform_In"), 
							 	"\\(\\log_a(x) \\) ... a", 
							 	choices = c(2, exp(1), 10), 
							 	selected = exp(1))
			),
			conditionalPanel("input.data_transform_In  == 'powerTransform'", ns = ns,
							 sliderInput(ns("data_powerTransform_In"), 
							 			"\\(x^a\\) ... a", 
							 			min = 2, max = 5, step = 1, value = 3)
			),
			conditionalPanel("input.data_transform_In  == 'rootTransform'", ns = ns,
							 sliderInput(ns("data_rootTransform_In"), 
							 			"\\(\\sqrt[a]{x}\\) ... a", 
							 			min = 2, max = 5, step = 1, value = 3)
			),
			
			conditionalPanel("input.data_transform_In  == ''", ns = ns,
							 sliderInput(ns("data_invTransform_In"), "", 
							 			min = 1, max = 2, step = 1, value = 1),
							 sliderInput(ns("data_noneTransform_In"), "", 
							 			min = 1, max = 2, step = 1, value = 1)
			),
			
			tipify(selectInput(ns("stat_label_In"), 
							   "Stat. Label", 
							   choices = statLabelChoices), 
				   "Label type of pairwise comparisons (stars or p values).", "top"),
			tipify(selectInput(ns("stat_comparison_type_In"), 
							   "Pairwise comparisons", 
							   choices = trackPairwStatComparisonTypeChoices, 
							   selected = "all_combinations"), 
				   "Select which pairs need to be selected. Either all combinations of pairs, all groups to a control group or selected pairs.", 
				   placement = "top", trigger = "hover"),
			fluidPage(fluidRow(
				column(6, checkboxInput(ns("stat_hidens_In"), 
										"Hide Non-Sign.", 
										value = FALSE))#,
				
				#column(6, checkboxInput("stat_pairwise_In", "Pairwise Comparisons", value = TRUE))
			)),
			bsTooltip(ns("stat_method_In"), toolTips$stat_method_In, 
					  placement = "top", trigger = "hover"),
			bsTooltip(ns("stat_label_In"), toolTips$stat_label_In, 
					  placement = "top", trigger = "hover"),
			bsTooltip(ns("stat_hidens_In"), toolTips$stat_hidens_In, 
					  placement = "bottom", trigger = "hover"),
			#bsTooltip("stat_pairwise_In", toolTips$stat_pairwise_In, placement = "bottom", trigger = "hover"),
			
			conditionalPanel("input.multiple_stat_method_In  == 'kruskal.test'", ns = ns,
							 
			),
			conditionalPanel("input.multiple_stat_method_In  == 'anova'", ns = ns,
							 
			),
			conditionalPanel("input.pairwise_stat_method_In  == 'wilcox.test'", ns = ns,
							 
			),
			conditionalPanel("input.pairwise_stat_method_In  == 't.test'", ns = ns,
							 
			)
		)
	}
	
	ranges_units_labels = function(ns){
		bsCollapsePanel(
			"Ranges, Units & Labels", 
			fluidPage(fluidRow(
				column(10, 
					   tipify(
					   	sliderInput(ns("y_range_In"), "y Axis Range", 
					   				min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE),
					   	"y axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only y axis selection is registered).", 
					   	placement = "top")), 
				column(2, checkboxInput(ns("y_range_check_In"), "", value = TRUE))
			)),
			
			axis_labels_UI(ns("axis_labs"), 
						   list(x = list(title = "Grouping axis",
						   			  unit = FALSE,
						   			  tooltip = toolTips$xlab_In),
						   	 y = list(title = "Measure axis",
						   	 		 unit = TRUE,
						   	 		 tooltip = toolTips$ylab_In)
						   )),
			bsTooltip(ns("y_range_check_In"), 
					  "Should the range selected come into effect? Quick way of enable/disable the range. If unchecked, whole data (default range) will be displayed.", 
					  placement = "bottom", trigger = "hover"))
	}
	
	specific_options = function(ns){
		bsCollapsePanel(
			"Specific Options", 
			conditionalPanel(condition = "input.type_In.indexOf('violin') > -1", ns = ns,
							 tipify(selectInput(ns("violin_scale_In"), "Scale", 
							 				   choices = list(Area = "area", Count = "count", Width = "width"), 
							 				   selected = "area"), 
							 	   "Area: all violins will have the same area (before trimming the tails)\\n Width: same maximum width\\n Count: violins are proportionally large to the number of observations.", 
							 	   placement = "top", trigger = "hover")
			),
			conditionalPanel(condition = "input.type_In.indexOf('box') > -1",  ns = ns,
							 checkboxInput(ns("box_notch_In"), "Notch", value = TRUE), 
							 checkboxInput(ns("box_varwidth_In"), "Variable Width", value = FALSE),
							 bsTooltip(ns("box_notch_In"), "Whether or not a notch should be displayed at median.", 
							 		  placement = "bottom", trigger = "hover"),
							 bsTooltip(ns("box_varwidth_In"), 
							 		  "If checked, boxes are drawn with widths proportional to the square-roots of the number of observations in the groups.", 
							 		  placement = "bottom", trigger = "hover")
			),
			conditionalPanel("input.type_In.indexOf('dot') > -1", ns = ns,
							 sliderInput(ns("dot_binwidth_In"), "Bin Width", 
							 			min = -0.1, max = 5, value = 0, step = 0.1),
							 checkboxInput(ns("dot_stackgroups_In"), "Stack Groups", value = FALSE),
							 tipify(
							 	selectInput(ns("dot_method_In"), "Method", 
							 				choices = list(`Dot-density binning` = "dotdensity", 
							 							   `Fixed bin widths` = "histodot"), selected = "dotdensity"), 
							 	"Dot-density binning, or fixed bin widths (like stat_bin)", 
							 	placement = "top", trigger = "hover"),
							 tipify(selectInput(ns("dot_stackdir_In"), "Stack Direction", 
							 				   choices = list(`Up` = "up", `Down` = "down", `Center` = "center", 
							 				   			   `Centered with aligned dots` = "centerwhole"), 
							 				   selected = "up"), 
							 	   "Which direction to stack the dots. \"up\" (default), \"down\", \"center\", \"centerwhole\" (centered, but with dots aligned)", 
							 	   placement = "top", trigger = "hover"),
							 
							 bsTooltip(ns("dot_binwidth_In"), "Dot size", placement = "bottom", trigger = "hover"),
							 bsTooltip(ns("dot_stackgroups_In"), 
							 		  "Whether or not dots should be stacked across groups", 
							 		  placement = "bottom", trigger = "hover"),
							 tags$div(h4("Dot Plot - Options"), p("Bin Width: Dot density (maximum bin width).\n"))
			),
			conditionalPanel("input.stat_label_In  == 'p.signif'", ns = ns,
							 fluidPage(fluidRow(
							 	column(2, shinyjs::disabled(textInput(ns("stat_label_symbols0_In"), 
							 										  label = "-", value = "0 < "))),
							 	column(8, tipify(
							 		textInput(ns("stat_label_symbols_In"), 
							 				  label = "significance symbols", 
							 				  value = "**** < 0.0001 < *** < 0.001 < ** < 0.01 < * < 0.05 < ns", 
							 				  width = "100%"), 
							 		"Use the following format: \"0 < *** < 0.001 < ** < 0.01 < 0.5 < ns < 1\"", 
							 		placement = "top", trigger = "hover")),
							 	column(2, shinyjs::disabled(textInput(ns("stat_label_symbols1_In"), 
							 										  label = "-", value = " < 1")))
							 ))),
			conditionalPanel("input.stat_comparison_type_In  == 'to_control'", ns = ns,
							 tipify(selectInput(ns("stat_comparison_control_In"), "Control Group", choices = list()), 
							 	   "Select the control group to compare with other groups.", 
							 	   placement = "top", trigger = "hover")),
			conditionalPanel("input.stat_comparison_type_In  == 'selected'", ns = ns,
							 tags$div(id = 'placeholderPairwiseGroupSelect')
			)
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
			 			   		   groupings_colors_UI(ns("groupings_colors"), can_dark = TRUE),
			 			   		   tests(ns),
			 			   		   facet_control_UI(ns("facet"), textFaceChoices),
			 			   		   ranges_units_labels(ns),
			 			   		   specific_options(ns),
			 			   		   debugging_UI(ns("debug"))
			 			   )
			 		),
			 		column(8, 
			 			   plotOutput(outputId = ns("plotOut"), dblclick = "plotOut_dblclick", 
			 			   		   brush = brushOpts(id = ns("plotOut_brush"), resetOnNew = TRUE)),
			 			   stat_details_UI(ns("stats")),
			 			   tags$style(type="text/css", "#stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   plot_export_UI(ns("export")), # plotExportSection("track"),
			 			   # h3("Export Preview (1:4)"),
			 			   # imageOutput(outputId = "plotPreviewOut")
			 		)
			 	)
			 )
	)
}

track_features_server = function(id, data, features, tracks, trajectories, groupings, choices){
	
	
	#' Plots track data
	#'
	#' @param dataTracks Tracks data frame
	#' @param x x grouping name
	#' @param y y grouping name
	#' @param type Plot type: "violin", "box", "bar" and "dot"
	#' @param groupings groupings data frame e.g. groupings()$groupings
	#' @param y.unit y axis unit to convert original unit
	#' @param colorGroupName Stroke aesthetic group name
	#' @param fillGroupName Fill aesthetic group name
	#' @param y.range y axis range in a vector c(min, max)
	#' @param quantiles quantiles to be drawn for violin plot
	#' @param facet.row Facet aesthetic group name
	#' @param facet.col Facet aesthetic group name
	#' @param facet.wrap Wrap facets instead of displaying on a grid (good for odd number of facet groups)
	#' @param title Plot title
	#' @param subtitle Secondary title
	#' @param stat.label Types of significance labels: Stars = "p.signif" or p value = "p.format"
	#' @param multiple.stat.method one of: list(`Do not perform` = "NONE", `ANOVA` = "anova", `Kruskal–Wallis Test` = "kruskal.test")
	#' @param pairwise.stat.method one of list(`Do not perform` = "NONE", `Student's t Test` = "t.test", `Wilcoxon/Mann-Whitney Test` = "wilcox.test")
	#' @param hide.ns Hide non significance
	#' @param data.transform How to transform the data with a formula list(method = character, parameter = character)
	#' @param stat.text.color Statistics text color
	#' @param replicateGroupName Replicates group name
	#' @param statPairwiseType How to match pairs for statistics "all_combinations", "to_control" and "selected"
	#' @param statPairwiseControl Name of the control group
	#' @param statPairwiseSelected Numbered list with pairs of groups list(c(group1, group2), ...)
	#' @param statSignSymbols Significance symbols and ranges
	#' @param fillAlpha Fill color opacity 0.0-1.0
	#' @param colorAlpha Stroke color opacity 0.0-1.0
	#' @param is.dark Dark plot option
	#' @param plot.subtitle.hjust Secondary title horizontal alignment
	#' @param plot.subtitle.size Secondary title text size
	#' @param plot.subtitle.face Secondary title text face
	#' @param facet.label.fill.color Facet label background color
	#' @param facet.text.face Facet label text style
	#' @param x.lab x axis label
	#' @param y.lab y axis label
	#' @param violin.scale Scale options for violin plot (e.g. area)
	#' @param box.notch Box plot notch at median
	#' @param box.varwidth \link[ggplot2]{geom_boxplot} varwidth
	#' @param dot.binwidth \link[ggplot2]{geom_dotplot} dotsize
	#' @param dot.stackgroups \link[ggplot2]{geom_dotplot} stackgroups
	#' @param dot.method \link[ggplot2]{geom_dotplot} method
	#' @param dot.stackdir \link[ggplot2]{geom_dotplot} stackdir
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
	plot_data = function(dataTracks, x, y, type, groupings, y.unit = NULL, colorGroupName = NULL, fillGroupName = NULL, 
						#alphaGroupName = NULL, 
						y.range = NULL,
						quantiles = c(0.25, 0.5, 0.75),
						facet.row = NULL, facet.col = NULL, facet.wrap = FALSE,
						title = NA, subtitle = NULL,
						#statGroupName = NULL, 
						stat.label = "..p.signif..", multiple.stat.method = NULL, pairwise.stat.method = NULL, 
						hide.ns = FALSE, data.transform = list(method = "noneTransform", paramter = 1),
						stat.text.color = "black", replicateGroupName = NULL,
						statPairwiseType = "all_combinations", statPairwiseControl = NULL, statPairwiseSelected = NULL, 
						statSignSymbols = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
											   symbols = c("****", "***", "**", "*", "ns")),
						fillAlpha = 1.0, colorAlpha = 1.0, is.dark = FALSE,
						plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
						facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold", 
						x.lab = NULL, y.lab = NULL,
						violin.scale = "area",
						box.notch = TRUE, box.varwidth = FALSE, 
						dot.binwidth = 1, dot.stackgroups = FALSE, dot.method = "dotdensity", dot.stackdir = "up",
						browse = FALSE, verbose = FALSE, benchmark = FALSE,
						#xReverseOrder = FALSE, 
						initializeProg = NULL, updateProg = NULL, closeProg = NULL){
		if(browse) browser()
		if(benchmark) startTime = benchMark()
		if(verbose) cat("Preparing names and expressions...\n")
		default.y.Unit = attr(dataTracks[[y]], "unit")
		if(is.null(default.y.Unit)){
			default.y.Unit = ""
		}
		#Check before
		if(is.null(y.unit)){y.unit = default.y.Unit}
		
		allGroupswoRep = unique(c(x, colorGroupName, fillGroupName, facet.row, facet.col))
		allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
		
		#Transforming data
		transformFun = match.fun(data.transform$method)
		transformFormulaFun = match.fun(paste0(data.transform$method, "Formula"))
		if(!udunits2::ud.are.convertible(default.y.Unit, y.unit)){
			y.unit = default.y.Unit
			#TODO warn about this?
		}
		if(!is.factor(dataTracks[[y]])){
			converted = udunits2::ud.convert(dataTracks[[y]], default.y.Unit, y.unit)
		}else{
			converted = dataTracks[[y]]
		}
		dataTracks[[y]] = transformFun(converted, data.transform$parameter)
		attr(dataTracks[[y]], "unit") = y.unit
		#browser()
		if(!is.factor(dataTracks[[y]])){
			dataRange = c(min(dataTracks[[y]]), max(dataTracks[[y]]))
		}else{
			dataRange = c(1, length(dataTracks[[y]]))
		}
		
		if(is.null(y.lab)){
			y.labDisp = ""
		}else{
			y.labDisp = TeX(paste0(transformFormulaFun(y.lab, data.transform$parameter), 
								   " \\[", transformFormulaFun(y.unit, data.transform$parameter), "\\]"))
		}
		
		#if(!is.null(unit)){  ud.convert(tracks$TRACK_MEAN_SPEED, "μm/sec", "μm/h")
		fillGroup = nameToExpr(fillGroupName); colorGroup = nameToExpr(colorGroupName)
		
		yVar = nameToExpr(y)
		plot = ggplot(dataTracks, aes(x = !!sym(x), y = !!yVar, color = !!colorGroup, fill = !!fillGroup))
		
		if(benchmark) startTime = benchMark("Units, groups, expressions", startTime)
		if(verbose) cat("Units, groups, expressions...\n")
		
		
		if("violin" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_violin(scale = violin.scale, draw_quantiles = quantiles, color = "white")
			}else{
				plot = plot + geom_violin(scale = violin.scale, draw_quantiles = quantiles)
			}
		}
		if("box" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_boxplot(notch = box.notch, varwidth = box.varwidth, color = "white")
			}else{
				plot = plot + geom_boxplot(notch = box.notch, varwidth = box.varwidth)
			}
		}
		if("bar" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_boxplot(notch = box.notch, varwidth = box.varwidth, color = "white")
			}else{
				plot = plot + geom_bar(stat = "identity")
			}
		}
		if("dot" %in% type){
			if(is.dark && is.null(colorGroup)){
				plot = plot + geom_dotplot(binaxis = "y", dotsize = dot.binwidth, stackgroups = dot.stackgroups, 
										   method = dot.method, stackdir = dot.stackdir, color = "white")
			}else{
				plot = plot + geom_dotplot(binaxis = "y", dotsize = dot.binwidth, stackgroups = dot.stackgroups, 
										   method = dot.method, stackdir = dot.stackdir)
			}
		}
		if(benchmark) startTime = benchMark("geom_x", startTime)
		if(verbose) cat("geom_x...\n")
		
		plot = facetPlot(p = plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
		
		plot = titlePlot(p = plot, title = title)
		
		aggrRep = data.frame(); aggrRepMerged = data.frame(); aggrRepMergedNice = data.frame()
		aggr = data.frame(); aggrNice = data.frame() 
		if(!is.null(replicateGroupName)){
			# All tracks separated by all groupings including replicates
			aggrRep = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(y, length)
			# All tracks separated by all groupings WITHOUT replicates, hence replicates are summarised
			aggrRepMerged = aggrRep %>% group_by_at(allGroupswoRep) %>% summarise_at(y, length)
			# aggrRepMerged with nicer titles for output table
			aggrRepMergedNice = aggrRepMerged
			newLabels = 
				as.character(as.character(groupings$labels)[match(colnames(aggrRepMergedNice), groupings$names)])
			newLabels[length(newLabels)] = "Number of replicates"
			colnames(aggrRepMergedNice) = newLabels
		}
		
		aggr = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(y, length)
		aggrNice = aggr
		colnames(aggrNice) = 
			c(as.character(groupings$labels)[match(colnames(aggrNice)[1:(ncol(aggrNice) - 1)], groupings$names)], 
			  "Number of tracks")
		
		if(is.null(subtitle)){
			firstPart = ""
			if(!is.null(replicateGroupName)){
				repCountRange = c(min(aggrRepMerged[[y]]), max(aggrRepMerged[[y]]))
				firstPart = paste0("n=", paste(unique(repCountRange), collapse = "-"), " , ")
			}
			
			subtitle = paste0(firstPart, "# of tracks each group/replicate=", min(aggr[[y]]), '-', max(aggr[[y]]))
		}
		
		if(!is.na(subtitle)) plot = subtitlePlot(p = plot, subtitle = subtitle)
		
		if(benchmark) startTime = benchMark("Titles", startTime)
		if(verbose) cat("Titles...\n")
		
		if(is.null(x.lab)){x.labDisp = getGLab(groupings, x)}else{x.labDisp = x.lab}
		
		
		if(is.na(x.labDisp)) x.labDisp = ""
		#if(is.na(y.labDisp)) y.labDisp = ""
		plot = plot + labs(color = getGLab(groupings, colorGroupName), fill = getGLab(groupings, fillGroupName), 
						   #alpha = getGLab(groupings, alphaGroupName), 
						   x = x.labDisp, y = y.labDisp)
		
		if(benchmark) startTime = benchMark("Labels", startTime)
		if(verbose) cat("Labels...\n")
		
		# Stat color for dark plots
		stat.text.color = "black"
			if(is.dark) stat.text.color = "white"
			
		plot = colorPlot(plot, dataTracks, groupings, colorGroupName, colorAlpha, is.dark)
		
		plot = fillPlot(plot, dataTracks, groupings, fillGroupName, fillAlpha, is.dark)
		
		plot = plot + scale_x_discrete(labels = getGLabs(groupings = groupings, name = x, 
														 order = levels(dataTracks[[x]])))
		
		if(y.unit == "radian"){
			plot = plot + scale_y_continuous(breaks  = c(seq(0, 2*pi, pi/2)), 
											 labels = c("0", "\u03c0/2", "\u03c0", "3\u03c0/2", "2*\u03c0"))
		}
		
		if(benchmark) startTime = benchMark("Colors", startTime)
		if(verbose) cat("Colors...\n")
		
		statOut = list()
		if(!is.null(multiple.stat.method)){
			if(multiple.stat.method != "NONE"){
				if(is.null(y.range)){
					label.y = (dataRange[2] - dataRange[1]) * 0.9 + dataRange[1]
				}else{
					label.y = (y.range[2] - y.range[1]) * 0.9 + y.range[1]
				}
				plot = plot + 
					stat_compare_means(method = multiple.stat.method, label.x = 0.7, label.y = label.y, show.legend = F)
			}
		}
		
		if(!is.null(pairwise.stat.method) && !is.factor(dataTracks[[y]])){
			if(pairwise.stat.method != "NONE"){
				comparisons = list()
				comparisonGroups = as.character(getGroups(groupings = groupings, name = x))
				if(statPairwiseType == "all_combinations"){
					comparisons = combn(comparisonGroups, 2, simplify = F)
				}else if(statPairwiseType == "to_control"){
					i = 1
					for(comparisonGroup in comparisonGroups){
						if(statPairwiseControl != comparisonGroup){
							comparisons[[i]] = c(statPairwiseControl, comparisonGroup)
							i = i + 1
						}
					}
				}else if(statPairwiseType == "selected"){
					comparisons = statPairwiseSelected
				}
				if(stat.label == "p.format"){
					statSignSymbols = NULL
				}
				plot = plot + 
					stat_compare_means(aes(group = !!sym(x)), label = stat.label, method = pairwise.stat.method, 
									   label.x = 1.5, comparisons = comparisons, hide.ns = hide.ns, 
									   show.legend = FALSE, color = stat.text.color, symnum.args = statSignSymbols)
				stat.fun = match.fun(pairwise.stat.method)
				nonStatGroupings = allGroupswoRep[allGroupswoRep != x]
				dataTracksSplits = dataTracks %>% group_by_at(vars(nonStatGroupings)) %>% group_split()
				for(dataTracksSplit in dataTracksSplits){
					withinGroupLabel = apply(dataTracksSplit[ , nonStatGroupings ], 1, paste, collapse = " AND " )[1]
					if(!is.null(comparisons)){
						stat.fun = match.fun(paste0("pairwise.", pairwise.stat.method))
						#browser()
						statOut[[withinGroupLabel]] = capture.output(stat.fun(x = dataTracksSplit[[y]], 
																			  g = dataTracksSplit[[x]]))
					}else{
						statOut[[withinGroupLabel]] = capture.output(stat.fun(reformulateT(x, y), 
																			  data = dataTracksSplit))
					}
				}
				statOut = replaceStatLabels(statOut, getGLab(groupings, x), y.lab)
			}
		}
		
		plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, 
							facet.label.fill.color, facet.text.face)
		
		plot = plot + coord_cartesian(ylim = y.range)
		
		if(benchmark) startTime = benchMark("Stats and theme", startTime)
		if(verbose) cat("Stats and theme...\n")
		
		# Histogram of each group to check data distribution
		histogram = groupHistograms(dataTracks, x, y, default.y.Unit, y.unit, y.labDisp, groupings, 
									colorGroup, fillGroup, colorGroupName, fillGroupName, 
									facet.row, facet.col, facet.wrap, 
									colorAlpha, fillAlpha, 
									is.dark)
		if(!is.factor(dataTracks[[y]])){
			# QQ plots of each group to check data normality
			qq = groupQQ(dataTracks, x, y, default.y.Unit, y.unit, groupings, colorGroup, fillGroup, 
						 colorGroupName, fillGroupName, facet.row, facet.col, facet.wrap, colorAlpha, fillAlpha, 
						 is.dark)
			# Shapiro test
			normality = groupedNormality(dataTracks, groupings, c(x, allGroupswoRep), y, default.y.Unit, y.unit)
			
			# Levene test
			levene = groupedLevene(dataTracks, c(x, allGroupswoRep), y, default.y.Unit, y.unit)
		}else{
			qq = NULL
			normality = NULL
			levene = NULL
		}
		return(list(plot = plot, stat = statOut, replicates = aggrRepMergedNice, tracks = aggrNice, 
					histogram = histogram, qq = qq, normality = normality, levene = levene))
	}
	
	
	moduleServer(id, function(input, output, session){
		plot = reactive({
			if(debugging$browse){
				browser()
			}
			print("output$trackFeatureOut = renderPlot({")
			if(input$x_In == "" || input$y_In == ""){
				"Please select both x and y axis variables."
			}else{
				titles = lapply(titles, function(x){x()})
				yUnit = axis_labs$y_unit(); if(yUnit == ""){yUnit = NULL}
				xlab = axis_labs$x_lab()
				ylab = axis_labs$y_lab()
				
				color_group = groupings_colors$color_group()
				fill_group = groupings_colors$fill_group()
				#statGroup = input$pairwise_stat_In; if(statGroup == "NULL") {statGroup = NULL}
				replicateGroup = input$replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
				
				facetRowGroup = facet$row_group()
				facetColGroup = facet$col_group()
				
				#if(input$x_range_check_In){xRange = input$x_range_In}else{xRange = NULL}
				if(input$y_range_check_In){yRange = input$y_range_In}else{yRange = NULL}
				
				a = input$stat_comparison_select1_In
				b = input$stat_comparison_select2_In
				#browser()
				
				statPairwiseSelectedPairs = list()
				if(length(a) == length(b) && length(a) > 0){
					for(i in 1:min(c(length(a), length(b)))){
						statPairwiseSelectedPairs[[i]] = c(a[i], b[i])
					}
				}
				
				statSignSymbols = paste0(input$stat_label_symbols0_In, 
										 input$stat_label_symbols_In, 
										 input$stat_label_symbols1_In)
				statSignSymbols = strsplit(statSignSymbols, " < ")[[1]]
				
				symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
								   symbols = c("****", "***", "**", "*", "ns"))
				# Must be uneven length as there needs to be 1 extra item in interval values
				if(length(statSignSymbols) %% 2 == 1){
					cutpoints = as.numeric(statSignSymbols[seq(1, length(statSignSymbols), 2)])
					# non-numeric cutpoint can't be accepted
					if(!any(is.na(cutpoints))){
						# only values x accepted if 0 < x < 1
						if(!any(!(0 <= cutpoints & cutpoints <= 1))){
							# cutpoints must be in ascending order
							if(sum(lead(cutpoints) < cutpoints, na.rm = TRUE) == 0){
								symbols = statSignSymbols[seq(2, length(statSignSymbols), 2)]	
								symnum.args = list(cutpoints = cutpoints, symbols = symbols)
							}
						}
					}
				}
				
				plot = plot_data(dataTracks = tracks(), x = input$x_In, y = input$y_In, 
								 type = input$type_In, 
								 y.range = yRange, y.unit = yUnit, 
								 colorGroupName = color_group, fillGroupName = fill_group, 
								 groupings = groupings()$groupings,
								 facet.row = facetRowGroup, facet.col = facetColGroup, 
								 title = titles$title, subtitle = titles$subtitle, replicateGroupName = replicateGroup, 
								 stat.label = input$stat_label_In, hide.ns = input$stat_hidens_In, 
								 multiple.stat.method = input$multiple_stat_method_In, 
								 pairwise.stat.method = input$pairwise_stat_method_In, 
								 data.transform = transform(),
								 statPairwiseType = input$stat_comparison_type_In, 
								 statPairwiseControl = input$stat_comparison_control_In, 
								 statPairwiseSelected = statPairwiseSelectedPairs, 
								 statSignSymbols = symnum.args,
								 fillAlpha = groupings_colors$fill_alpha(), 
								 colorAlpha = groupings_colors$color_alpha(), 
								 x.lab = xlab, y.lab = ylab, is.dark = groupings_colors$dark(),
								 facet.text.face = facet$label_face(), 
								 facet.label.fill.color = facet$label_fill_color(),
								 facet.wrap = facet$wrap(),
								 plot.subtitle.hjust = titles$subtitle_hjust, 
								 plot.subtitle.size = titles$subtitle_size, 
								 plot.subtitle.face = titles$subtitle_text_style,
								 violin.scale = input$violin_scale_In,
								 box.notch = input$box_notch_In,
								 box.varwidth = input$box_varwidth_In,
								 dot.binwidth = input$dot_binwidth_In,
								 dot.stackgroups = input$dot_stackgroups_In,
								 dot.method = input$dot_method_In,
								 dot.stackdir = input$dot_stackdir_In,
								 browse = debugging$browse, 
								 benchmark = debugging$benchmark, 
								 verbose = debugging$verbose)
				plot
			}
			
		})
		
		output$plotOut = renderPlot({
			plotOut = plot()
			if(is.list(plotOut)){
				plotOut$plot
			}else{
				NULL
			}
		})
		
		observeEvent(input$plotOut_dblclick, {
			#browser()
			brush = input$plotOut_brush
			if (!is.null(brush)) {
				#ranges$x = c(brush$xmin, brush$xmax)
				#trackRanges$y = c(brush$ymin, brush$ymax)
				#updateCheckboxInput(session, "track_x_range_check_In", value = TRUE)
				updateCheckboxInput(session, "y_range_check_In", value = TRUE)
				updateSliderInput(session, "y_range_In", min = getYMin(), max = getYMax(), 
								  step = getYStep(), value = c(brush$ymin, brush$ymax))
			} else {
				#ranges$x = NULL
				#trackRanges$y = NULL
				#updateCheckboxInput(session, "x_range_check_In", value = FALSE)
				updateCheckboxInput(session, "y_range_check_In", value = FALSE)
				updateSliderInput(session, "y_range_In", min = getYMin(), max = getYMax(), 
								  step = getYStep(), value = c(getYMin(), getYMax()))
			}
		})
		
		getYPretty = reactive({
			# browser()
			if(!is.null(tracks()) && !(is.null(input$y_In)) && !(input$y_In == "")){
				#browser()
				transformFun = match.fun(transform()$method)
				values = tracks()[[input$y_In]]
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
				#browser()
				if(!is.factor(values)){
					values = transformFun(udunits2::ud.convert(values, unit, unitToConvert), transform()$parameter)
					
					#if()
					pretty(values, 20)
				}else{
					values
				}
			}
		})
		#getXMin = reactive({getXPretty()[1]})
		#getXMax = reactive({x = getXPretty(); x[length(x)]})
		getYMin = reactive({
			#browser()
			prettyMin = getYPretty()[1]
			if(!is.null(prettyMin) && !is.factor(prettyMin)){
				if(prettyMin > 0){
					return(0)
				}else{
					return(prettyMin * 2)
				}
			}
		})
		getYMax = reactive({
			prettyMax = last(getYPretty())
			if(!is.null(prettyMax) && !is.factor(prettyMax)){
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
		
		transform = reactive({
			# Data Transform with parameter
			ns = session$ns
			return(list(method = input$data_transform_In, 
						parameter = input[[ns(paste(c("data", input$data_transform_In, "In"), collapse = "_"))]]))
		})
		
		observe({updateSelectInput(session, "x_In", choices = choices$groupingsChoiceswithoutEmpty())})
		observe({updateSelectInput(session, "y_In", choices = choices$trackChoiceswithoutEmpty(), 
								   selected = "TRACK_MEAN_SPEED")})
		# observe({updateSelectInput(session, "stat_In", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "replicate_In", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "stat_comparison_control_In", choices = statGroups(), 
								   selected = safeSelect(statGroups()))})
		observe({updateSliderInput(session, "y_range_In", min = getYMin(), max = getYMax(), 
								   step = getYStep(), value = c(getYMin(), getYMax()))})
		
		observe({
			groupingsList = groupings()
			dataList = data()
			statGroup = input$x_In
			#trackData = tracks()
			if(!is.null(statGroup) && statGroup != ""){
				if(length(groupingsList) > 1){
					groupingsDF = groupingsList$groupings
					
					removeUI(selector = "#placeholderPairwiseGroupSelect div", multiple = TRUE)
					#for(rankListOb in rankListObs){rankListOb$destroy()}
					#rankListObs <<- list()
					if(length(dataList) > 1){
						#browser()
						choices = as.character(getGroups(groupingsDF, statGroup))
						labels = as.character(getGLabs(groupingsDF, statGroup))
						#choices = as.list(choices)
						#names(choices) = labels
						
						insertUI(selector = "#placeholderPairwiseGroupSelect", 
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
		
		statGroups = reactive({
			#browser()
			statGroup = input$x_In
			#trackData = tracks()
			if(!is.null(statGroup) && statGroup != ""){
				groupingsDF = groupings()$groupings
				groups = as.character(getGroups(groupingsDF, statGroup))
				labels = as.character(getGLabs(groupingsDF, statGroup))
				choices = as.list(groups)
				names(choices) = labels
				return(choices)
			}else{
				return(list())
			}
		})
		
		plot_export_server("export", "Track Feature", plot)
		
		facet = facet_control_server("facet", choices$groupingsChoiceswithEmpty)
		
		stat_details_server("stats", plot)
		
		debugging = debugging_server("debug")
		
		titles = titles_server("title")
		
		groupings_colors = groupings_colors_server("groupings_colors", choices$groupingsChoiceswithEmpty)
		
		axis_labs = axis_labels_server("axis_labs", features, tracks, 
									   groups = list(x = reactive({input$x_In}), 
									   			  y = reactive({input$y_In})),
									   default_labels = list(x = NULL, y = reactive({input$y_In})))
	})
}