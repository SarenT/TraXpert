#rm(list = ls())
# Line below should run before other packages are loaded. See link for the reason:
# https://stackoverflow.com/questions/27153974/how-to-fix-outofmemoryerror-java-gc-overhead-limit-exceeded-in-r
options(java.parameters = "-Xmx4096m")
libs = c("methods", "xml2", "ggplot2", "magrittr", "dplyr", "ggpubr", "RColorBrewer", "udunits2", "sticky", "shiny", 
	  "sortable", "colourpicker", "DT", "svglite", "readxl", "colorspace", "smoother", "shinyBS", "car", "latex2exp")

only.install = c("shinyjs", "htmlwidgets", "shinyWidgets")

# Install missing packages if there is any
#install.packages(only.install[!is.element(only.install, installed.packages())], )
#install.packages(libs[!is.element(libs, installed.packages())])


lapply(libs, require, character.only = TRUE)


options(shiny.port = 7777, shiny.maxRequestSize=1024*1024^2, scipen = 10, browser = "firefox")


defaultWD = "~/"
if(dir.exists(defaultWD)){
	setwd(defaultWD)	
}

source("functions.R")

try(detach("package:plyr", unload=TRUE), silent = TRUE)

subtitleHjustChoices = list(Left = 0, Middle = 0.5, Right = 1)
textFaceChoices = list(Plain = "plain", Italic = "italic", Bold = "bold", `Bold & Italic` = "bold.italic")
aggregateFunctionChoices = list(`Do not aggregate` = "NULL", Sum = "sum", Mean = "mean", Median = "median", Mode = "single.mode", Min = "min", Max = "Max")
summaryFunctionChoices = aggregateFunctionChoices
summaryFunctionChoices[["Number of elements"]] = "length"
dispersionMeasureChoices = list(`Do not display error` = "NULL", `Standard Deviation` = "stddev", `Interquartile Range` = "IQR", `Range (Min/Max)` = "range", `Standard error of the mean` = "se", `CI 95%` = "ci95", `CI 99%` = "ci99")
dispChoicesforAggrFunction = list(`Do not aggregate` = c(1), Sum = c(1), Mean = c(1, 2, 5, 6, 7), 
								  Median = c(1, 3, 4, 6, 7), Mode = c(1), Min = c(1, 4), Max = c(1, 4))
dispersionTypeChoices = list(`Line Range` = "linerange", `Point Range` = "pointrange", `Cross Bar` = "crossbar", `Error Bar` = "errorbar", `Ribbon` = "ribbon")
statLabelChoices = list(Stars = "p.signif", `p value` = "p.format") #, Stars = "..p.signif..", `p Format` = "..p.format..", p = "p", pp = "..p..")
#statLabelChoices = list(Stars = "..p.signif..", `p Format` = "..p.format..", p = "..p..")
trajFeaturesChoices = list(`Point Plot` = "point", `Jitter Plot` = "jitter", `Quantile Plot` = "quantile", `Smooth Plot` = "smooth", `Line Plot` = "line", `Area Plot` = "area", `Step Plot` = "step")
trackFeaturesChoices = list(`Bar Plot` = "bar", `Box Plot` = "box", `Dot Plot` = "dot", `Violin Plot` = "violin")
featureDimensionChoices = list(`Position` = "POSITION", `Time` = "TIME", `N/A` = "NONE", `Velocity/Speed` = "VELOCITY", `Length` = "LENGTH", `Angle` = "ANGLE")
trackMultipleStatChoices = list(`Do not perform` = "NONE", `ANOVA` = "anova", `Kruskal–Wallis Test` = "kruskal.test")
trackPairwiseStatChoices = list(`Do not perform` = "NONE", `Student's t Test` = "t.test", `Wilcoxon/Mann-Whitney Test` = "wilcox.test")
trackPairwStatComparisonTypeChoices = list(`All Combinations` = "all_combinations", `All groups to control group` = "to_control", `Selected Pairs` = "selected")
	
dataTransformChoices = list(`None` = "noneTransform", `Power (left)` = "powerTransform", `Root (right)` = "rootTransform", 
							`Log (right)` = "logTransform", `Inverse (right)` = "invTransform")

trackDirectionalityMeasures = list(`Number of Tracks in Cardinal Groups` = "cardinalNumberOfTracks")

positionTypes = list(Absolute = "unfixed", `Fixed at origin` = "fixed", `Fixed and rotated` = "fixed-rotated")

circMultiSampleTestMeasures = list(`Do not test` = "NULL", `Common Mean Direction` = "mean", 
								   `Common Median Direction` = "median", `Common Concentration` = "conc", 
								   `Common Distribution` = "dist")

circMultiSampleTestMeanMethods = list(`Watson's Large-sample Nonparametric Test` = "watsons.large.sample.nonparametric.test", 
									  `Watson-Williams Test` = "watson.williams.test.2")
circMultiSampleTestMedianMethods = list(`Fisher's Nonparametric Test` = "fisher.nonparametric.test", 
										`Random. Fisher Nonparametric Test` = "PgRandTest")
circMultiSampleTestConcMethods = list(`Wallraff's Nonparametric Test` = "WallraffTest")
circMultiSampleTestDistMethods = list(`Watson Two Sample Test` = "watson.two.test")

circMultiSampleTestMeasureMethods = list(`NULL` = NULL, `meanDir` = circMultiSampleTestMeanMethods, 
										 `medianDir` = circMultiSampleTestMedianMethods, 
										 `concentration` = circMultiSampleTestConcMethods, 
										 `distribution` = circMultiSampleTestDistMethods)
# circMultiSampleTestMeasureInputs = list(`NULL` = NULL, `meanDir` = "dir_multisample_mean_method_In", 
# 										`medianDir` = "dir_multisample_median_method_In", 
# 										`concentration` = "dir_multisample_conc_method_In", 
# 										`distribution` = "dir_multisample_dist_method_In")

formulaChoicesVector = c(" ", "(", ")", "-", "/", "*", "+", "^", "mean(", "first(", "last(", "median(", "%%", "&&", "||", ">", "<", "<=", ">=", "lag(", "lead(", "sqrt(")
formulaChoices = as.list(formulaChoicesVector)
names(formulaChoices) = formulaChoicesVector

circHistQQCellSize = 800

dataModelVersion = 1

tabsID = "tabs"
tabColorImport = "#ffaaaa"; tabColorOperations = "#aaaaff"; tabColorTables = "#ffffaa"; tabColorPlots = "#aaffaa"

titleImportGroupings = "Import/Groupings"; titleOperations = "Operations"; 
titleFiles = "Files" ;titleFeatures = "Features" ;titleTracks = "Tracks"
titleTrajectories = "Trajectories"; titlePlotTrackFeatures = "Plot Track Features"
titlePlotTrajectories = "Plot Trajectories"; titlePlotDirectionality = "Plot Directionality"
titlePlotTrajFeatures = "Plot Trajectory Features"; titleAbout = "About"

tabBGColorCSS = function(title, tabColor = "#cccccc"){
	return(paste0(".navbar-default .navbar-nav > li[class=active] > a[data-value='", title, "'] {background-color: ", tabColor, ";}
	.navbar-default .navbar-nav > li[class=active] > a[data-value='", title, "'] a:hover {background-color: ", tabColor, ";}
	.navbar-default .navbar-nav > li[class=active] > a[data-value='", title, "'] a:focus {background-color: ", tabColor, ";}
		   .navbar-default .navbar-nav > li > a[data-value='", title, "'] {background-color: ", tabColor, ";}"))
}

icon_list = function(x){
	lapply(
		x,
		function(x) {
			tags$div(
				icon("arrows-alt-h"),
				tags$strong(x)
			)
		}
	)
}

dataSessionVersionUpgrade = function(dataSession, version){
	if(version == 0){
		dataSession$version = 1
		version = 1
	}
	#Add new versions as separate if statements for stepwise adaptation for the newest version
	
	if(version == dataModelVersion){
		return(dataSession)
	}else{
		#TODO error reporting
		return(NULL)
	}
	
}

tabPanelImport = function(title = titleImportGroupings, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 sidebarLayout(
			 	sidebarPanel(#h3("TraXplorer"),
			 				 #hr(),
			 				 p("TraXplorer is a tool to automate analysis of tracking files to create plots and simple statistics. TraXplorer is smart enough to understand basic file naming convention and the source of files, where each tracking file has ", 
			 				   strong("\"_\""), 
			 				   "separated groupings."),
			 				 br(),
			 				 p("For instance, files ",
			 				   br(), #em("2019.05.08_WT_", strong("Untreated")),
			 				   HTML("<em>2019.05.08_WT_<strong>Untreated</strong>.xml</em> and <br><em>2019.05.08_WT_<strong>Treated</strong>.xml</em> will be recognized as <br><em>2019.05.08_WT_<strong>group1</strong>.xml</em> and <br><em>2019.05.08_WT_<strong>group2</strong>.xml</em>. Also multiple groupings are supported (treatments, genotypes, etc...")
			 				   ),
			 				 tags$ul(
			 				 	tags$li("Select files with", strong("Choose...")),
			 				 	tags$li("Set group labels and colors"),
			 				 	tags$li("Click ", strong("Process Files"),  "Button for loading/parsing"),
			 				 	tags$li(strong("Browse"), "the data or", strong("generate plots"), 
			 				 			"via the tabs on the top right panel.")
			 				 ),
			 				 fileInput(inputId = "tmFilesIn", label = "Choose files:", multiple = TRUE, 
			 				 		  accept = c("text/xml", ".xml", ".xlsx", ".xls", ".csv", ".txt"), 
			 				 		  placeholder = "Uploading...", buttonLabel = "Choose..."),
			 				 fluidPage(fluidRow(
			 				 	column(7, actionButton(inputId = "processFilesIn", label = "Process Files")), 
			 				 	column(3, checkboxInput("process_recalculate_In", label = "Recalculate", value = FALSE)),
			 					column(2, checkboxInput("process_browse_In", label = "Debug", value = FALSE))
			 				 )),
			 				 p("OR"),
			 				 fileInput(inputId = "sessionFileIn", label = "Choose session file:", multiple = FALSE, 
			 				 		  accept = c("text/tmx", ".tmx"), placeholder = "Uploading...", 
			 				 		  buttonLabel = "Choose..."),
			 				 hr(),
			 				 #bookmarkButton(label = "Save Inputs"),
			 				 #p("OR"),
			 				 downloadButton(outputId = "sessionOut", label = "Download session file"),
			 				 hr()),
			 	mainPanel(
			 		fluidPage(
			 			fluidRow(column(12, DTOutput(outputId = "groupingsOut"))),
			 			fluidRow(
			 				column(6, tags$div(id = 'placeholder')),
			 				column(6, p("Rank groups accordingly. Groups will appear in plots according to these ranks."),
			 					   tags$div(id = 'placeholderRank'))#, uiOutput("groupRankOut"))
			 			)
			 		)
			 	)
			 )
	)
}

rotationPanel = function(){
	column(6, fluidPage(class = "shiny-input-panel", fluidRow(
		column(4, h4("Rotate"), p("Select XY-Rotation angle (pitch) and Z-rotation angle (yaw) and click \"rotate\" button.")),
		column(4, sliderInput("processRotationFixIn", "Rotation (Pitch) Fix Angle", min = -180, max = 180, step = 15, value = 0)),
		column(4, sliderInput("processZRotationFixIn", "Z Rotation (Yaw) Fix Angle", min = -180, max = 180, step = 15, value = 0),
			   checkboxInput("rotate_browse_In", label = "Debug", value = FALSE),
			   actionButton(inputId = "rotateIn", label = "Process Files"))
	)))
}
pointSourcePanel = function(){
	column(6, fluidPage(class = "shiny-input-panel", fluidRow(
		column(3, h4("Point Source"), p("Calculate point source directionality by a providing point source location (physical units NOT pixels)."),
			   checkboxInput("point_source_browse_In", label = "Debug", value = FALSE), 
			   tipify(fileInput(inputId = "pointSourceDefUploadIn", label = "Upload"), 
			   	   "Upload a file to fill up the database", "top", "hover"),
			   actionButton(inputId = "pointSourceIn", label = "Calculate")),
		column(9, DTOutput(outputId = "files_point_source_Out", width = "60%"))
	)))
}

generateBucketList = function(choices, context){
	ui = tagList(fluidPage(fluidRow(
			column(6,
				   tags$div(class = "panel panel-default",	
				   		 tags$div(class = "panel-heading", icon("arrow-right"),	"Select groups"),
				   		 tags$div( class = "panel-body", id = paste(context, "stat_comparison_select_In", sep = "_"), icon_list(choices))
				   ),
				   tags$div(class = "panel panel-default", 
				   		 tags$div(class = "panel-heading", icon("exchange"), "1."),
				   		 tags$div(class = "panel-body", id = paste(context, "stat_comparison_select1_In", sep = "_"))
				   )),
			column(6,
				   tags$div(class = "panel panel-default",
				   		 tags$div(class = "panel-heading", icon("trash"), "Remove item"),
				   		 tags$div(class = "panel-body", id = paste(context, "stat_comparison_selectBin_In", sep = "_"))
				   ),
				   tags$div(class = "panel panel-default",
				   		 tags$div(class = "panel-heading", icon("exchange"), "2."),
				   		 tags$div(class = "panel-body", id = paste(context, "stat_comparison_select2_In", sep = "_"))
				   )
			)
		)),
		sortable_js(paste(context, "stat_comparison_select_In", sep = "_"), options = sortable_options(group = list(
			pull = "clone",	name = paste(context, "stat_comparison_select_Group", sep = "_"), put = FALSE), 
			onSort = sortable_js_capture_input("sort_vars"))),
		sortable_js(paste(context, "stat_comparison_select1_In", sep = "_"), options = sortable_options(group = list(
			pull = TRUE, name = paste(context, "stat_comparison_select_Group", sep = "_"), put = TRUE), 
			onSort = sortable_js_capture_input(paste(context, "stat_comparison_select1_In", sep = "_")))),
		sortable_js(paste(context, "stat_comparison_selectBin_In", sep = "_"), options = sortable_options(group = list(
			pull = TRUE, name = paste(context, "stat_comparison_select_Group", sep = "_"), put = TRUE), 
			onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }"))),
		sortable_js(paste(context, "stat_comparison_select2_In", sep = "_"), options = sortable_options(group = list(
			pull = "clone",	name = paste(context, "stat_comparison_select_Group", sep = "_"), put = TRUE), 
			onSort = sortable_js_capture_input(paste(context, "stat_comparison_select2_In", sep = "_"))))
	)
	return(ui)
}

newFeaturePanel = function(title, description, feature, typeChoices){
	column(6, fluidPage(class = "shiny-input-panel", fluidRow(
		column(4, h4(title),
			   p(description),
			   actionButton(inputId = paste("new", feature, "feat_btn_In", sep = "_"), label = "Calculate"),
			   checkboxInput(inputId = paste("new", feature, "feat_debug_In", sep = "_"), label = "Debug")),
		column(8, fluidPage(
			fluidRow(
				column(6, textInput(inputId = paste("new", feature, "feat_In", sep = "_"), label = "Feature"),
					   textInput(inputId = paste("new", feature, "feat_shortname_In", sep = "_"), label = "Short Name")),
				column(6, textInput(inputId = paste("new", feature, "feat_name_In", sep = "_"), label = "Name"),
					   fluidPage(fluidRow(
					   	column(6, tipify(selectInput(inputId = paste("new", feature, "feat_dimension_In", sep = "_"), label = "Dimension", 
					   								 choices = featureDimensionChoices), 
					   					 "Feature dimenion. This indicates the type of physical information the feature will contain. e.g. displacement is a length, change of displacement over time is velocity etc.", "top")),
					   	column(6, tipify(selectInput(inputId = paste("new", feature, "feat_type_In", sep = "_"), label = "Type", 
					   								 choices = typeChoices), 
					   					 "Type of feature. Options are Track, Spot or Edge. This selection may be limited.", "top"))
					   )))
				), hr(),
			fluidRow(
				column(6, tipify(selectInput(inputId = paste("new", feature, "feat_vars_In", sep = "_"), label = "Variables", 
											 choices = list()), "Existing variables to choose from to include in the formula.", "top")),
				column(6, tipify(textInput(inputId = paste("new", feature, "feat_formula_In", sep = "_"), "Formula"),#, choices = formulaChoices, selected = NULL, multiple = TRUE, options = list(create = TRUE)), 
								 "Formula to calculate the new feature. Every item needs to be selected or added. Plain text will be ignored. The formula needs to be correctly written (all paranthesis closed, mathematical operator between variables etc.).", 
								 "top"))
				)
			))
	)),
	bsTooltip(paste("new", feature, "feat_btn_In", sep = "_"), "Start calculation. All fields must be filled."),
	bsTooltip(paste("new", feature, "feat_In", sep = "_"), "Feature identifier. This needs to start with a letter (a-z or A-Z), may contain letters or numbers and \"_\"."),
	bsTooltip(paste("new", feature, "feat_name_In", sep = "_"), "Visible title/name for the feature."),
	bsTooltip(paste("new", feature, "feat_shortname_In", sep = "_"), "Visible short name for the feature."))
}


tabPanelOperations = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(fluidRow( 
			 	rotationPanel(),
			 	pointSourcePanel()
			 	)),
			 fluidPage(fluidRow( 
			 	newFeaturePanel("New Track Feature", "Calculate a new track feature based on an existing feature. With this, you can calculate new measures by entering the formula.",
			 					"track", list(`Track` = "Track")),
			 	newFeaturePanel("New Trajectory (Spot/Edge) Feature", "Calculate a new spot or edge feature based on an existing feature. With this, you can calculate new measures by entering the formula.",
			 					"traj", list(`Spot` = "Spot", `Edge` = "Edge"))
			 )),
			 fluidPage(fluidRow(
			 	newFeaturePanel("New Track Feature from Trajectories", "Calculate a new track feature based on existing trajectory features. This summarises trajectory information (spots and edges of a track) into a single value per track. e.g. mean track speed is the average of all speeds between spots of a track.",
			 					"track_from_traj", list(`Track` = "Track"))
			 ))
			 		   
	)
}

autoSizeGeneric = function(context, dim){
	tagList(
		fluidPage(fluidRow(
			column(6, checkboxInput(paste(context, "auto", dim,"In", sep = "_"), label = paste("Auto", dim), value = TRUE)),
			column(6, conditionalPanel(paste0("!input.", context, "_auto_", dim, "_In"), 
									   numericInput(paste(context, dim, "In", sep = "_"), label = paste("Plot", dim, "[cm]"), 
									   			 min = 1, step = 1, max = 100, value = 10)))
		)),
		bsTooltip(paste(context, "auto", dim, "In", sep = "_"), paste("Whether or not output image", dim, "(both for SVG and PNG) needs to be automatic or not."), placement = "bottom", trigger = "hover"),
		bsTooltip(paste(context, dim, "In", sep = "_"), paste("If not automatic, image", dim, "in cm at 300dpi."), placement = "bottom", trigger = "hover")
	)
}

plotExportSection = function(context){
	tagList(
		downloadButton(outputId = paste0(context, "_download_PNG_In"), label = "PNG"),
		downloadButton(outputId = paste0(context, "_download_SVG_In"), label = "SVG"),
		autoSizeGeneric(context, "width"),
		autoSizeGeneric(context, "height"),
		bsTooltip(paste0(context, "_download_PNG_In"), toolTips$download_PNG_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_download_SVG_In"), toolTips$download_SVG_In, placement = "bottom", trigger = "hover"),
		
	)
}

facetPanel = function(context){
	tagList(
		tipify(selectInput(paste0(context, "_facet_row_In"), "Facet Row", choices = list()), toolTips$facet_row_In, placement = "top"),
		tipify(selectInput(paste0(context, "_facet_col_In"), "Facet Column", choices = list()), toolTips$facet_col_In, placement = "top"),
		selectInput(paste0(context, "_facet_label_face_In"), "Facet Label Style", choices = textFaceChoices),
		conditionalPanel(paste0("(input.", context, "_facet_row_In == \"NULL\") != (input.", context, "_facet_col_In == \"NULL\")"), 
						 checkboxInput(paste0(context, "_facet_wrap_In"), "Wrap Facets", value = TRUE)),
		colourInput(paste0(context, "_facet_label_fill_color_In"), "Facet Label Background", value = "#FFFFFFFF", allowTransparent = TRUE),
		
		#bsTooltip(paste0(context, "_facet_row_In"), "Facets divide plots into a grid (Rows in this case). This is useful for multidimensional data e.g. when you have genotpyes, drug treatments and temperature treatment. But you are more interested in the genotypes. So you can use facets to divide plots into sections for the treatments.", placement = "bottom", trigger = "hover"),
		#bsTooltip(paste0(context, "_facet_col_In"), "Facets divide plots into a grid (Columns in this case). This is useful for multidimensional data e.g. when you have genotpyes, drug treatments and temperature treatment. But you are more interested in the genotypes. So you can use facets to divide plots into sections for the treatments.", placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_facet_label_face_In"), toolTips$facet_label_face_In, placement = "top", trigger = "hover"),
		bsTooltip(paste0(context, "_facet_wrap_In"), toolTips$facet_wrap_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_facet_label_fill_color_In"), toolTips$facet_label_fill_color_In, placement = "bottom", trigger = "hover")
	)
}

titleSubtitlePanel = function(context){
	tagList(
		textInput(paste0(context, "_title_In"), "Main Title"),
		checkboxInput(paste0(context, "_title_check_In"), "Disable Main Title", value = FALSE),
		textInput(paste0(context, "_subtitle_In"), "Subtitle", placeholder = "Leave empty for automatic subtitle"),
		checkboxInput(paste0(context, "_subtitle_check_In"), "Disable Subtitle", value = FALSE),
		selectInput(paste0(context, "_subtitle_hjust_In"), "Subtitle Alignment", choices = subtitleHjustChoices, selected = 0.5),	
		sliderInput(paste0(context, "_subtitle_size_In"), "Subtitle Size", value = 10, min = 1, max = 100, step = 1),
		selectInput(paste0(context, "_subtitle_text_style_In"), "Subtitle Text Style", choices = textFaceChoices),
		
		bsTooltip(paste0(context, "_subtitle_In"), toolTips$subtitle_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_subtitle_check_In"), toolTips$subtitle_check_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_subtitle_hjust_In"), toolTips$subtitle_hjust_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_subtitle_size_In"), toolTips$subtitle_size_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_subtitle_text_style_In"), toolTips$subtitle_text_style_In, placement = "bottom", trigger = "hover")
	)
}

statDataDetails = function(context){
	tagList(fluidRow(
		column(6,
			   bsCollapse(id = paste("collapse", context, "data", sep = "_"),
			   		   bsCollapsePanel("Data Structure",
			   		   				tableOutput(outputId = paste0(context, "_data_replicates_Out")),
			   		   				tableOutput(outputId = paste0(context, "_data_tracks_Out")),
			   		   				style = "default")
			   		   )
			   ),
		column(6, 
			   bsCollapse(id = paste("collapse", context, "statistics", sep = "_"),
			   					  bsCollapsePanel("Statistics",
			   					  				tableOutput(outputId = paste0(context, "_stat_DF_Out")),
			   					  				textOutput(outputId = paste0(context, "_stat_text_Out")),
			   					  				style = "default")
			   		   )
			   )
		),
	fluidRow(
		column(12,
			   bsCollapse(id = paste("collapse", context, "normality", sep = "_"),
			   		   bsCollapsePanel("Data Distribution, Normality and Variances",
			   		   				fluidPage(fluidRow(
			   		   					column(6, plotOutput(outputId = paste0(context, "_stat_histogram_Out")),
			   		   						   plotOutput(outputId = paste0(context, "_stat_qq_Out"))),
			   		   					column(6, h2("Shapiro-Wilk Test and Distribution Shape"),
			   		   						   withMathJax("Shapiro-Wilk test is a test of normality of data. Null hypothesis is that the sample came from a normally distributed population. If p-value is below a cutoff value (usually 0.05) null hypothesis can be rejected. Otherwise, there is not enough evidence to reject the null hypothesis. Small W values indicate a non-normality. However, only extreme deviations reduce W values. In general, Shapiro-Wilk test is highly sensitive to small deviations from normality with very large sample sizes (n > 200). Refer to Q-Q plots for thorough inspection. Skewness is measured with \\( g_1\\ =\\ m_3\\ / m_2^{3/2}\\ \\) (positive numbers mean right tail long) and kurtosis with \\( g_2\\ =\\ m_4\\ / m_2^{2} - 3\\ \\) (Positive kurtosis means more values are closer to the mean.)."),
			   		   						   tableOutput(outputId = paste0(context, "_stat_normality_Out")),
			   		   						   h2("Levene's Test"),
			   		   						   p("Levene's test is a statistic to test equality of variances. Null hypothesis is that the population variances are equal. If p value is below a certain threshold (typically 0.5), null hypothesis can be rejected and it can be assumed that there is a difference between variances of groups."),
			   		   						   verbatimTextOutput(outputId = paste0(context, "_stat_levene_Out")))
			   		   				)),
			   		   				style = "default")
			   		   )
			   )
	))
}

statCircDataDetails = function(context){
	tagList(fluidRow(
		column(6,
			   bsCollapse(id = paste("collapse", context, "data", sep = "_"),
			   		   bsCollapsePanel("Data Structure",
			   		   				tableOutput(outputId = paste0(context, "_circdata_replicates_Out")),
			   		   				tableOutput(outputId = paste0(context, "_circdata_tracks_Out")),
			   		   				style = "default")
			   )
		),
		column(6, 
			   bsCollapse(id = paste("collapse", context, "statistics", sep = "_"),
			   		   bsCollapsePanel("Statistics",
			   		   				tableOutput(outputId = paste0(context, "_circstat_DF_Out")),
			   		   				textOutput(outputId = paste0(context, "_circstat_text_Out")),
			   		   				style = "default")
			   )
		)
	),
	fluidRow(
		column(12,
			   bsCollapse(id = paste("collapse", context, "uniformity", sep = "_"),
			   		   bsCollapsePanel("Data Distribution and Uniformity",
			   		   				#h2("Distribution & Spread"),
			   		   				uiOutput(outputId = paste0(context, "_circstat_histogram_qqplot_Out"))#, plotOutput(outputId = paste0(context, "_circstat_histogram_Out")), # circ package type histogram
			   		   				#plotOutput(outputId = paste0(context, "_circstat_histogram_qqplot_Out"))
			   		   				)
			   		   ),
			   bsCollapse(id = paste("collapse", context, "variances", sep = "_"),
			   		   bsCollapsePanel("Variances & other Measures",
			   		   				#h2("Summary Statistics"),
			   		   				withMathJax("von Mises distribution is analogous to Gaussian distribution for circular data. Most simply circular data can be uniformly distributed. If not it may or may not follow a von Mises distrition. Similar to non-circular data, it may be skewed or have high/low kurtosis."),
			   		   				tableOutput(outputId = paste0(context, "_stat_shape_Out")),
			   		   				h2("Uniformity, von Mises"),
			   		   				p("Rayleigh test can only be applied to single peak (unimodal) deviation from uniformity. Null hypothesis states that the data is uniformly distributed. For low p-values one can reject this. For cell migration, this can be interpreted as directionality. Watson's test is more powerful for non-unimodal deviations from uniformity. Although this would be a very rare use case scenario for migrating cells, directionality in opposite sides have been reported in some assays (e.g. force coupling on nano structures shown here Reversat et al. 2020)."),
			   		   				tableOutput(outputId = paste0(context, "_stat_uniformity_DF_Out")),
			   		   				verbatimTextOutput(outputId = paste0(context, "_stat_uniformity_text_Out")),
			   		   				verbatimTextOutput(outputId = paste0(context, "_stat_vonMisesFit_Out"))
			   		   				)
			   		   )
		)
		
	))
}

colorFillSection = function(context, width = "100%"){
	tagList(
		fluidPage(fluidRow(
			column(6, 
				   tipify(selectInput(paste0(context, "_fill_In"), "Fill Color Variable", choices = list()), toolTips$fill_In, placement = "top")),
			column(6, 
				   tipify(selectInput(paste0(context, "_color_In"), "Line Color Variable", choices = list()), toolTips$color_In, placement = "top"))
			)),
		sliderInput(paste0(context, "_fill_alpha_In"), "Fill Transparency", min = 0, max = 1, value = 0.5, width = width),
		sliderInput(paste0(context, "_color_alpha_In"), "Color Transparency", min = 0, max = 1, value = 1, width = width),
		bsTooltip(paste0(context, "_fill_alpha_In"), toolTips$fill_alpha_In, placement = "bottom", trigger = "hover"),
		bsTooltip(paste0(context, "_color_alpha_In"), toolTips$color_alpha_In, placement = "bottom", trigger = "hover")
	)
}

plotDebugging = function(context){
	tagList(h3("Debugging"),
			checkboxInput(paste0(context, "_browse_In"), "Debug", value = FALSE),
			checkboxInput(paste0(context, "_benchmark_In"), "Benchmark", value = FALSE),
			checkboxInput(paste0(context, "_verbose_In"), "Verbose", value = FALSE))
}

darkPlot = function(context){
	tagList(
		checkboxInput(paste0(context, "_dark_In"), "Dark Plot", value = FALSE),
		bsTooltip(paste0(context, "_dark_In"), toolTips$dark_In, placement = "bottom", trigger = "hover")
	)
}



tabPanelPlotTrackFeatures = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(
			 	fluidRow(
			 		column(4,
			 			   bsCollapse(id = "track_settings",
			 			   		   bsCollapsePanel("Titles", titleSubtitlePanel("track")),
			 			   		   bsCollapsePanel("Plot Type and Variables", 
			 			   		   				selectInput("track_type_In", "Plot Type", choices = trackFeaturesChoices, selected = "violin", multiple = TRUE),
			 			   		   				tipify(selectInput("track_x_In", "Group by", choices = list()), "Main grouping. Select a grouping, which is most interesting to compare. Also used for statistics.", "top"),
			 			   		   				tipify(selectInput("track_y_In", "Measure", choices = list()), "Select the measure you are interested in to compare.", "top"),
			 			   		   				tipify(selectInput("track_replicate_In", "Replicates are grouped in", choices = list()), toolTips$replicate_In, "top"),
			 			   		   				bsTooltip("track_type_In", "Display type of plot to use (violin, box plot, dot plot etc.)", placement = "top", trigger = "hover")
			 			   		   				),
			 			   		   bsCollapsePanel("Groupings and Colors", 
			 			   		   				colorFillSection("track"),
			 			   		   				darkPlot("track")
			 			   		   ),
			 			   		   bsCollapsePanel("Tests", 
			 			   		   				selectInput("track_multiple_stat_method_In", "Test - Multiple Groups", choices = trackMultipleStatChoices, selected = "kruskal.test"),
			 			   		   				selectInput("track_pairwise_stat_method_In", "Test - Pairwise", choices = trackPairwiseStatChoices, selected = "wilcox.test"),
			 			   		   				selectInput("track_data_transform_In", "Transform data with", choices = dataTransformChoices, selected = "noneTransform"),
			 			   		   				conditionalPanel("input.track_data_transform_In  == 'logTransform'",
			 			   		   								 shinyWidgets::sliderTextInput("track_data_logTransform_In", "\\(\\log_a(x) \\) ... a", choices = c(2, exp(1), 10), selected = exp(1))
			 			   		   				),
			 			   		   				conditionalPanel("input.track_data_transform_In  == 'powerTransform'",
			 			   		   								 sliderInput("track_data_powerTransform_In", "\\(x^a\\) ... a", min = 2, max = 5, step = 1, value = 3)
			 			   		   				),
			 			   		   				conditionalPanel("input.track_data_transform_In  == 'rootTransform'",
			 			   		   								 sliderInput("track_data_rootTransform_In", "\\(\\sqrt[a]{x}\\) ... a", min = 2, max = 5, step = 1, value = 3)
			 			   		   				),
			 			   		   				
			 			   		   				conditionalPanel("input.track_data_transform_In  == ''",
			 			   		   								 sliderInput("track_data_invTransform_In", "", min = 1, max = 2, step = 1, value = 1),
			 			   		   								 sliderInput("track_data_noneTransform_In", "", min = 1, max = 2, step = 1, value = 1)
			 			   		   				),
			 			   		   				
			 			   		   				tipify(selectInput("track_stat_label_In", "Stat. Label", choices = statLabelChoices), "Label type of pairwise comparisons (stars or p values).", "top"),
			 			   		   				tipify(selectInput("track_stat_comparison_type_In", "Pairwise comparisons", choices = trackPairwStatComparisonTypeChoices, selected = "all_combinations"), "Select which pairs need to be selected. Either all combinations of pairs, all groups to a control group or selected pairs.", placement = "top", trigger = "hover"),
			 			   		   				fluidPage(fluidRow(
			 			   		   					column(6, checkboxInput("track_stat_hidens_In", "Hide Non-Sign.", value = FALSE))#,
			 			   		   					
			 			   		   					#column(6, checkboxInput("track_stat_pairwise_In", "Pairwise Comparisons", value = TRUE))
			 			   		   				)),
			 			   		   				bsTooltip("track_stat_method_In", toolTips$stat_method_In, placement = "top", trigger = "hover"),
			 			   		   				bsTooltip("track_stat_label_In", toolTips$stat_label_In, placement = "top", trigger = "hover"),
			 			   		   				bsTooltip("track_stat_hidens_In", toolTips$stat_hidens_In, placement = "bottom", trigger = "hover"),
			 			   		   				#bsTooltip("track_stat_pairwise_In", toolTips$stat_pairwise_In, placement = "bottom", trigger = "hover"),
			 			   		   				
			 			   		   				conditionalPanel("input.track_multiple_stat_method_In  == 'kruskal.test'",
			 			   		   								 
			 			   		   				),
			 			   		   				conditionalPanel("input.track_multiple_stat_method_In  == 'anova'",
			 			   		   								 
			 			   		   				),
			 			   		   				conditionalPanel("input.track_pairwise_stat_method_In  == 'wilcox.test'",
			 			   		   								 
			 			   		   				),
			 			   		   				conditionalPanel("input.track_pairwise_stat_method_In  == 't.test'",
			 			   		   								 
			 			   		   				)
			 			   		   				),
			 			   		   bsCollapsePanel("Facets", facetPanel("track")),
			 			   		   bsCollapsePanel("Ranges, Units & Labels", 
			 			   		   				fluidPage(fluidRow(
			 			   		   					column(10, tipify(sliderInput("track_y_range_In", "y Axis Range", min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE), "y axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only y axis selection is registered).", "top")), 
			 			   		   					column(2, checkboxInput("track_y_range_check_In", "", value = TRUE))
			 			   		   				)),
			 			   		   				textInput("track_unit_In", "Measure Unit"),
			 			   		   				textInput("track_xlab_In", "Grouping axis label"),
			 			   		   				textInput("track_ylab_In", "Measure axis label"),
			 			   		   				
			 			   		   				bsTooltip("track_y_range_check_In", "Should the range selected come into effect? Quick way of enable/disable the range. If unchecked, whole data (default range) will be displayed.", placement = "bottom", trigger = "hover"),
			 			   		   				bsTooltip("track_unit_In", toolTips$unit_In, placement = "bottom", trigger = "hover"),
			 			   		   				bsTooltip("track_xlab_In", toolTips$xlab_In, placement = "bottom", trigger = "hover"),
			 			   		   				bsTooltip("track_ylab_In", toolTips$ylab_In, placement = "bottom", trigger = "hover")),
			 			   		   bsCollapsePanel("Specific Options", 
			 			   		   				conditionalPanel("input.track_type_In.indexOf('violin') > -1",
			 			   		   								 tipify(selectInput("track_violin_scale_In", "Scale", choices = list(Area = "area", Count = "count", Width = "width"), selected = "area"),"Area: all violins will have the same area (before trimming the tails)\\n Width: same maximum width\\n Count: violins are proportionally large to the number of observations.", placement = "top", trigger = "hover")
			 			   		   				),
			 			   		   				conditionalPanel("input.track_type_In.indexOf('box') > -1", 
			 			   		   								 checkboxInput("track_box_notch_In", "Notch", value = TRUE), 
			 			   		   								 checkboxInput("track_box_varwidth_In", "Variable Width", value = FALSE),
			 			   		   								 bsTooltip("track_box_notch_In", "Whether or not a notch should be displayed at median.", placement = "bottom", trigger = "hover"),
			 			   		   								 bsTooltip("track_box_varwidth_In", "If checked, boxes are drawn with widths proportional to the square-roots of the number of observations in the groups.", placement = "bottom", trigger = "hover")
			 			   		   				),
			 			   		   				conditionalPanel("input.track_type_In.indexOf('dot') > -1",
			 			   		   								 sliderInput("track_dot_binwidth_In", "Bin Width", min = -0.1, max = 5, value = 0, step = 0.1),
			 			   		   								 checkboxInput("track_dot_stackgroups_In", "Stack Groups", value = FALSE),
			 			   		   								 tipify(selectInput("track_dot_method_In", "Method", choices = list(`Dot-density binning` = "dotdensity", `Fixed bin widths` = "histodot"), selected = "dotdensity"), "Dot-density binning, or fixed bin widths (like stat_bin)", placement = "top", trigger = "hover"),
			 			   		   								 tipify(selectInput("track_dot_stackdir_In", "Stack Direction", choices = list(`Up` = "up", `Down` = "down", `Center` = "center", `Centered with aligned dots` = "centerwhole"), selected = "up"), "Which direction to stack the dots. \"up\" (default), \"down\", \"center\", \"centerwhole\" (centered, but with dots aligned)", placement = "top", trigger = "hover"),
			 			   		   								 
			 			   		   								 bsTooltip("track_dot_binwidth_In", "Dot size", placement = "bottom", trigger = "hover"),
			 			   		   								 bsTooltip("track_dot_stackgroups_In", "Whether or not dots should be stacked across groups", placement = "bottom", trigger = "hover"),
			 			   		   								 tags$div(h4("Dot Plot - Options"), p("Bin Width: Dot density (maximum bin width).\n"))
			 			   		   				),
			 			   		   				conditionalPanel("input.track_stat_label_In  == 'p.signif'",
			 			   		   								 fluidPage(fluidRow(
			 			   		   								 	column(2, shinyjs::disabled(textInput("track_stat_label_symbols0_In", label = "-", value = "0 < "))),
			 			   		   								 	column(8, tipify(textInput("track_stat_label_symbols_In", label = "significance symbols", value = "**** < 0.0001 < *** < 0.001 < ** < 0.01 < * < 0.05 < ns", width = "100%"), "Use the following format: \"0 < *** < 0.001 < ** < 0.01 < 0.5 < ns < 1\"", placement = "top", trigger = "hover")),
			 			   		   								 	column(2, shinyjs::disabled(textInput("track_stat_label_symbols1_In", label = "-", value = " < 1")))
			 			   		   								 ))),
			 			   		   				conditionalPanel("input.track_stat_comparison_type_In  == 'to_control'",
			 			   		   								 tipify(selectInput("track_stat_comparison_control_In", "Control Group", choices = list()), "Select the control group to compare with other groups.", placement = "top", trigger = "hover")),
			 			   		   				conditionalPanel("input.track_stat_comparison_type_In  == 'selected'",
			 			   		   								 tags$div(id = 'placeholderPairwiseGroupSelect')
			 			   		   				)
			 			   		   ),
			 			   		   bsCollapsePanel("Debugging", 
			 			   		   				plotDebugging("track")
			 			   		   )
			 			   		   
			 			   )
			 		
			 		
			 			   ),
			 		column(8, 
			 			   plotOutput(outputId = "trackFeaturePlotOut", dblclick = "trackFeaturePlotOut_dblclick", 
			 			   		   brush = brushOpts(id = "trackFeaturePlotOut_brush", resetOnNew = TRUE)),
			 			   statDataDetails("track"),
			 			   tags$style(type="text/css", "#track_stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   plotExportSection("track")
			 			   )
			 		)
			 	)
			 )
}


tabPanelPlotTrajectories = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(
			 	fluidRow(
			 		column(4,
			 			   bsCollapse(id = "traj_settings",
			 			   		   bsCollapsePanel("Titles", titleSubtitlePanel("traj")),
			 			   		   bsCollapsePanel("Plot Type and Variables",
			 			   		   				selectInput("traj_xy_In", "Position", choices = list()),
			 			   		   				tipify(selectInput("traj_replicate_In", "Replicates are grouped in", choices = list()), toolTips$replicate_In, "top", "hover"),
			 			   		   				tipify(selectInput("traj_start_point_In", "Start Point Display", choices = list()), "", "top", "hover"),
			 			   		   				tipify(selectInput("traj_end_point_In", "End Point Display", choices = list()), "", "top", "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Groupings and Colors", 
			 			   		   				tipify(selectInput("traj_color_In", "Point/Line Color Variable", choices = list()), "", "top", "hover"),
			 			   		   				conditionalPanel("input.traj_color_In != \"NULL\"", 
			 			   		   								 checkboxInput("traj_color_tracks_In", "Colored Tracks", value = TRUE)),
			 			   		   				
			 			   		   				bsTooltip("traj_color_tracks_In", "Should trajectories also be colored? Or only start/end points?", placement = "bottom", trigger = "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Filters", 
			 			   		   				checkboxInput("traj_limit_to_smallest_In", "Limit # of tracks to smallest group", value = FALSE),
			 			   		   				sliderInput("traj_track_reduced_In", "Plot every n th Track", min = 1, max = 10, value = 1, step = 1, width = "200%"),
			 			   		   				sliderInput("traj_spot_reduced_In", "Plot every n th Spot", min = 1, max = 10, value = 1, step = 1, width = "200%"),
			 			   		   				
			 			   		   				bsTooltip("traj_limit_to_smallest_In", "If you have groups with extremely differing number of spots, you can limit the number of trajectories displayed to the number of trajectories of the smallest group. More crowded groups may appear more \"migratory\" to the \"eye\".", "bottom", "hover"),
			 			   		   				bsTooltip("traj_track_reduced_In", "Reduce number of trajectories displayed by this number. e.g. 2 means every second track to be displayed for quicker and more clean plotting.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_spot_reduced_In", "Reduce number of trajectories displayed by this number. e.g. 2 means every second track to be displayed for quicker and smoother plotting.", "bottom", "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Facets", facetPanel("traj")),
			 			   		   bsCollapsePanel("Ranges, Units & Labels", 
			 			   		   				fluidPage(fluidRow(column(6, checkboxInput("traj_inverse_In", "Invert y axis", value = TRUE)),
			 			   		   								   column(6, checkboxInput("traj_equal_range_In", "Equal x/y ranges", value = TRUE)))),
			 			   		   				fluidPage(fluidRow(column(6, textInput("traj_xlab_In", "x axis label")),
			 			   		   								   column(6, textInput("traj_ylab_In", "y axis label")))),
			 			   		   				textInput("traj_unit_In", "Unit"),
			 			   		   				
			 			   		   				#bsTooltip("", "", "bottom", "hover")
			 			   		   				bsTooltip("traj_xy_In", "How should trajectories be placed? Either absolute position, fixed (all tracks start from origin or fixed and rotated (tracks are rotated in the \"Operations\" tab).", "top", "hover"),
			 			   		   				bsTooltip("traj_inverse_In", "Should the y axis be inverted? Imaging y coordinates increase downwards, which is opposite of the cartesian coordinate system. Inverting y axis will make trajectories oriented same way as in the image.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_equal_range_In", "Should the x and y axis have equal ranges? If false, plots will be stretched (maybe useful for highly asymmetric imaging daata).", "bottom", "hover"),
			 			   		   				bsTooltip("traj_xlab_In", "Label to be displayed on the x axis.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_ylab_In", "Label to be displayed on the y axis.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_unit_In", toolTips$unit_In, placement = "bottom", trigger = "hover")
			 			   		   			),
			 			   		   bsCollapsePanel("Display Options", 
			 			   		   				sliderInput("traj_color_alpha_In", "Color Transparency", min = 0, max = 1, value = 1),
			 			   		   				sliderInput("traj_line_size_In", "Line Thickness", min = 0.01, max = 100, value = 0.5, step = 0.25),
			 			   		   				fluidPage(fluidRow(column(6, checkboxInput("traj_coord_equal_In", "Equal axes ranges", value = TRUE),
			 			   		   										  checkboxInput("traj_panel_border_In", "Panel border", value = FALSE),
			 			   		   										  checkboxInput("traj_h_line_In", "Horizontal line", value = TRUE),
			 			   		   										  bsTooltip("traj_coord_equal_In", "", placement = "bottom", trigger = "hover"),
			 			   		   										  bsTooltip("traj_panel_border_In", "", placement = "bottom", trigger = "hover"),
			 			   		   										  bsTooltip("traj_h_line_In", "", placement = "bottom", trigger = "hover")
			 			   		   				),
			 			   		   				column(6, darkPlot("traj"),
			 			   		   					   checkboxInput("traj_panel_grid_major_In", "Panel major grid", value = FALSE),
			 			   		   					   checkboxInput("traj_v_line_In", "Vertical line", value = TRUE),
			 			   		   					   bsTooltip("traj_panel_grid_major_In", "", placement = "bottom", trigger = "hover"),
			 			   		   					   bsTooltip("traj_v_line_In", "", placement = "bottom", trigger = "hover")))),
			 			   		   				
			 			   		   				bsTooltip("traj_color_alpha_In", "Should trajectories appear transparent? Particularly useful if there are too many trajectories,", "bottom", "hover"),
			 			   		   				bsTooltip("traj_line_size_In", "Line thickness for either too crowded or sparse data.", "bottom", "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Debugging", 
			 			   		   				plotDebugging("traj")
			 			   		   )
			 			   		   
			 			   )
			 			   
			 			   
			 		),
			 		column(8, 
			 			   plotOutput(outputId = "trajectoryPlotOut"),
			 			   #statDataDetails("track"),
			 			   #tags$style(type="text/css", "#track_stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   fluidPage(fluidRow(column(6, actionButton(inputId = "plotTrajIn", label = "Plot Trajectories")),
			 			   				   column(6, plotExportSection("traj"))))
			 		)
			 	)
			 )
	)
}



tabPanelDirectionality = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(
			 	fluidRow(
			 		column(4,
			 			   bsCollapse(id = "track_settings",
			 			   		   bsCollapsePanel("Titles", titleSubtitlePanel("dir")),
			 			   		   bsCollapsePanel("Plot Type and Variables",
			 			   		   				selectInput("dir_type_In", "Plot Type", choices = list(`Bar/Radar` = "bar", Polygon = "polygon"), selected = "bar"),
			 			   		   				tipify(selectInput("dir_replicate_In", "Replicates are grouped in", choices = list()), toolTips$replicate_In, "top"),
			 			   		   				bsTooltip("dir_type_In", "Type of plot. Bar plot is also called rose plot. Polygon is also called radar plot.", "top", "hover"),
			 			   		   				selectInput("dir_summary_fun_In", "Summary Function", choices = summaryFunctionChoices[2:length(summaryFunctionChoices)], selected = "length"),
			 			   		   				conditionalPanel("input.dir_summary_fun_In == 'length'", checkboxInput("dir_summary_fun_length_percentage_In", "Percentage", value = TRUE)),
			 			   		   				selectInput("dir_replicate_summary_fun_In", "Replicates Summary Function", choices = summaryFunctionChoices, selected = "mean"),
			 			   		   				
			 			   		   				tipify(selectInput("dir_track_direction_cat_In", "Cardinal Direction Variable", choices = list()), "Which direction measure should be displayed? Cardinal dirction means binned track angles at 30° bins. e.g. angles -15° to 15° are considered as together. If rotation is applied in \"Operations\" tab, then this can be displayed as well.", "top", "hover"),
			 			   		   				tipify(selectInput("dir_cumulation_In", "Cumulation Variable (upon grouping)", choices = list(`Each Track` = "EACH_TRACK", `Track Displacement` = "TRACK_DISPLACEMENT")), "Which measure needs to be used for cumulation. e.g. track displacement can enhance the effect of directionality with speed and persistence.", "top", "hover"),
			 			   		   				
			 			   		   				bsTooltip("dir_summary_fun_In", "Summary function to be applied grouped tracks. Suggested option: length.", "top", "hover"),
			 			   		   				bsTooltip("dir_summary_fun_length_percentage_In", "Display percentage of number of cells to the total number of cells.", "bottom", "hover"),
			 			   		   				bsTooltip("dir_replicate_summary_fun_In", "Summarize replicates with the selected function.", "top", "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Groupings and Colors", 
			 			   		   				colorFillSection("dir", width = "150%")
			 			   		   ),
			 			   		   bsCollapsePanel("Tests", 
			 			   		   				tipify(selectInput("dir_multisample_measure_In", "Measure", choices = circMultiSampleTestMeasures), "Which measure to be used to compare groups. Mean/median direction, directionality (concentration) or distribution (are sample drawn from the same distribution?).", "top", "hover"),
			 			   		   				conditionalPanel("input.dir_multisample_measure_In == 'mean'",
			 			   		   								 tipify(selectInput("dir_multisample_mean_method_In", "Method", choices = circMultiSampleTestMeanMethods), "Watson large non-parametric test assumes number of sampes over 25 otherwise the p value needs to be considered as an approximation.", "top", "hover")
			 			   		   				),
			 			   		   				conditionalPanel("input.dir_multisample_measure_In == 'median'",
			 			   		   								 tipify(selectInput("dir_multisample_median_method_In", "Method", choices = circMultiSampleTestMedianMethods), "Fisher non-parametric test", "top", "hover")
			 			   		   				),
			 			   		   				conditionalPanel("input.dir_multisample_measure_In == 'conc'",
			 			   		   								 tipify(selectInput("dir_multisample_conc_method_In", "Method", choices = circMultiSampleTestConcMethods), "Fisher non-parametric test", "top", "hover")
			 			   		   				),
			 			   		   				conditionalPanel("input.dir_multisample_measure_In == 'dist'",
			 			   		   								 tipify(selectInput("dir_multisample_dist_method_In", "Method", choices = circMultiSampleTestDistMethods), "Fisher non-parametric test", "top", "hover"),
			 			   		   								 conditionalPanel("input.dir_multisample_dist_method_In == 'watson.two.test'",
			 			   		   								 				 tipify(selectInput("dir_stat_watsontwotest_comparison_type_In", "Pairwise comparisons", choices = trackPairwStatComparisonTypeChoices, selected = "all_combinations"), "Select which pairs need to be selected. Either all combinations of pairs, all groups to a control group or selected pairs.", placement = "top", trigger = "hover")
			 			   		   								 )
			 			   		   				),
			 			   		   				conditionalPanel("input.dir_stat_watsontwotest_comparison_type_In == 'to_control'",
			 			   		   								 tipify(selectInput("dir_stat_watsontwotest_comparison_control_In", "Control Group", choices = list()), "Select the control group to compare with other groups.", placement = "top", trigger = "hover")),
			 			   		   				conditionalPanel("input.dir_stat_watsontwotest_comparison_type_In  == 'selected'",
			 			   		   								 tags$div(id = 'placeholderDirPairwiseGroupSelect')
			 			   		   				)
			 			   		   ),
			 			   		   bsCollapsePanel("Facets",  facetPanel("dir")),
			 			   		   bsCollapsePanel("Ranges, Units & Labels", 
			 			   		   				checkboxInput("dir_show_y_axis_In", "Show y axis", value = FALSE),
			 			   		   				bsTooltip("dir_show_y_axis_In", "Displays a y axis, which is a vertical line to indicate y scale.", "bottom", "hover"),
			 			   		   				textInput("dir_xlab_In", "x axis label"),
			 			   		   				textInput("dir_ylab_In", "y axis label"),
			 			   		   				
			 			   		   				bsTooltip("dir_xlab_In", "x axis label", "bottom", "hover"),
			 			   		   				bsTooltip("dir_ylab_In", "y axis label", "bottom", "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Display Options", 
			 			   		   				sliderInput("dir_line_size_In", "Line Thickness", min = 0.01, max = 100, value = 0.5, width = "150%"),
			 			   		   				sliderInput("dir_start_angle_In", "Angle Rotation", min = -180, max = 180, step = 15, value = 0, width = "150%"),
			 			   		   				darkPlot("dir"),
			 			   		   				
			 			   		   				#bsTooltip("", "", "bottom", "hover")
			 			   		   				bsTooltip("dir_line_size_In", "Line thickness relative to original size.", "bottom", "hover"),
			 			   		   				bsTooltip("dir_start_angle_In", "Starting angle of the directions. This simply rotates the plots, if you want to fix the direction.", "bottom", "hover")
			 			   		   				
			 			   		   ),
			 			   		   bsCollapsePanel("Debugging", 
			 			   		   				plotDebugging("dir"),
			 			   		   				checkboxInput("dir_skip_radar_In", "Skip Radar", value = FALSE),
			 			   		   				checkboxInput("dir_skip_degrees_In", "Skip Degrees", value = FALSE)
			 			   		   )
			 			   		   
			 			   		   
			 			   )
			 			   
			 			   
			 		),
			 		column(8, 
			 			   plotOutput(outputId = "directionalityPlotOut"),
			 			   statCircDataDetails("dir"),
			 			   tags$style(type="text/css", "#dir_circstat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   plotExportSection("dir")
			 		)
			 	)
			 )
	)
}

tabPanelTable = function(title, dtID, downloadButtonID, label, tabColor){
	tabPanel(title, 
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 DTOutput(outputId = dtID), downloadButton(outputId = downloadButtonID, label = label))
}

tabPanelAbout = function(title){
	tabPanel(title,
			 fluidPage(
			 	fluidRow(
			 		column(4),
			 		column(4, 
			 h1("About TraXplorer"),
			 p(paste("TraXplorer is a particle tracking analyzing software developed by me (Saren Tasciyan).",
			 "Particle tracking data contains a lot of information about the motion of particles.", 
			 "Such data can be very useful in life sciences. As I am studying cell migration,", 
			 "I have realized the lack of a data analysis tool for tracking data.", 
			 "There have been already numoreous tools for tracking objects in imaging data.",  
			 "But making sense of this tracking data requires an extra step: data analysis.",
			 "This can be tricky and often requires advanced programming skills for data analysis (e.g. R or python).", 
			 "Combined with suboptimal data analysis with simpler tools (e.g. Excel) can be dangerous.", 
			 "And of course not everyone must know how to program.", 
			 "This was my motivation to create this application.", 
			 "To help scientists to analyze their tracking data as easily as possible.", 
			 "Therefore, most of the functions in TraXplorer are as automatic as possible.")),
			 p(paste("It relies on grouped and well named, calibrated files.", 
			 "Main goal was to make it compatible with TrackMate but Imaris and text files (e.g. for Chemotaxis Tool) ", 
			 "are also somewhat supported.", 
			 "Simple import your named files into TraXplorer and it should recognize your groupings and groups.", 
			 "Most common plot types and features are already available (e.g. trajectory plots, radar plots...).", 
			 "For simple statistics, you can use track feature plots.", 
			 "For dynamics or relationships, you can use trajectory feature plots.", 
			 "You may as well calculate new features or point source directionality under operations.")),
			 p(paste("If you want to use your own code to do more advanced analysis, you can also simply use TraXplorer as", 
			 "a conversion tool!", "Then simply download the table files to avoid dealing with XML structures.", 
			 "Finally, if you need some more features, please don't hesitate to contact me:")), 
			 a("saren.tasciyan@ist.ac.at", href = "mailto:saren.tasciyan@ist.ac.at")
			 		),
			 column(4)
			 	)
			 )
	)
}

tabPanelPlotTrajectoryFeatures = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(
			 	fluidRow(
			 		column(4,
			 			   bsCollapse(id = "traj_feat_settings",
			 			   		   bsCollapsePanel("Titles", titleSubtitlePanel("traj_feat")),
			 			   		   bsCollapsePanel("Plot Type and Variables",
			 			   		   				tipify(selectInput("traj_feat_type_In", "Plot Type", choices = trajFeaturesChoices, selected = "point", multiple = TRUE), "Display method. Smooth plot displays a fit to the data to help with overplotting (too many data points, which overcrowds the space).", "top"),
			 			   		   				tipify(selectInput("traj_feat_replicate_In", "Replicates are grouped in", choices = list()), toolTips$replicate_In, "top"),
			 			   		   				tipify(selectInput("traj_feat_aggr_fun_In", "Aggregate groups with", choices = aggregateFunctionChoices, selected = "mean"), "Method to summarize replicates. Multiple replicates in the same grouped are then represented together (e.g. mean).", "top"),
			 			   		   				tipify(selectInput("traj_feat_disp_fun_In", "Display error", choices = dispersionMeasureChoices[1], selected = "None"), "What dispersion measure to be displayed. e.g. error bars, min/max, range etc.", "top"),
			 			   		   				tipify(selectInput("traj_feat_disp_type_In", "Style", choices = dispersionTypeChoices, selected = "errorbar"), "Style of dispersion display.", "top"),
			 			   		   				tipify(selectInput("traj_feat_x_In", "x Axis Variable", choices = list()), "Each trajectory feature is to be displayed with x/y coordinates. Both needs to be continious. e.g. speed (y) over time (x).", "top"),
			 			   		   				tipify(selectInput("traj_feat_y_In", "y Axis Variable", choices = list()), "Each trajectory feature is to be displayed with x/y coordinates. Both needs to be continious. e.g. speed (y) over time (x).", "top")
			 			   		   ),
			 			   		   bsCollapsePanel("Groupings and Colors", 
			 			   		   				tipify(selectInput("traj_feat_color_In", "Line/Point Color Variable", choices = list()), "Line/Point colors.", "top"),
			 			   		   				tipify(selectInput("traj_feat_size_In", "Size", choices = list()), "Line/Points size measure.", "top"),
			 			   		   				conditionalPanel("input.traj_feat_type_In.indexOf('jitter') > -1 || input.traj_feat_type_In.indexOf('point') > -1 || input.traj_feat_type_In.indexOf('smooth') > -1 || input.traj_feat_type_In.indexOf('area') > -1", 
			 			   		   								 tipify(selectInput("traj_feat_fill_In", "Fill Variable", choices = list()), "Fill color according to a group.", "top")
			 			   		   				),
			 			   		   				conditionalPanel("input.traj_feat_type_In.indexOf('quantile') > -1 || input.traj_feat_type_In.indexOf('smooth') > -1 || input.traj_feat_type_In.indexOf('area') > -1 || input.traj_feat_type_In.indexOf('line') > -1 || input.traj_feat_type_In.indexOf('step') > -1", 
			 			   		   								 tipify(selectInput("traj_feat_linetype_In", "Line type Variable", choices = list()), "Line type according to a group.", "top"),
			 			   		   								 checkboxInput("traj_feat_smooth_In", "Smoothen the line", value = FALSE),
			 			   		   								 conditionalPanel("input.traj_feat_smooth_In",
			 			   		   								 				 numericInput(inputId = "traj_feat_smooth_windowIn", label = "Smoothing Window", value = 1, min = 1, max = 100)
			 			   		   								 )),
			 			   		   				#checkboxInput("traj_feat_group_tracks_In", "Group tracks in the same group or split tracks individually.", value = FALSE)),
			 			   		   				conditionalPanel("input.traj_feat_type_In.indexOf('jitter') > -1 || input.traj_feat_type_In.indexOf('point') > -1", 
			 			   		   								 tipify(selectInput("traj_feat_shape_In", "Shape", choices = list()), "Shape of points based on the selected group.", "top")
			 			   		   				),
			 			   		   				
			 			   		   				bsTooltip("traj_feat_smooth_In", "Smoothen the data with gaussian filter. This is particularly useful for noisy data.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_feat_smooth_windowIn", "Gaussian filter window size. The larger windows make smoother data.", "bottom", "hover")#,
			 			   		   				#bsTooltip("traj_feat_group_tracks_In", "Group tracks.", "bottom", "hover")
			 			   		   ),
			 			   		   # bsCollapsePanel("Tests", 
			 			   		   # 				
			 			   		   # ),
			 			   		   bsCollapsePanel("Facets",  facetPanel("traj_feat")),
			 			   		   bsCollapsePanel("Ranges, Units & Labels", 
			 			   		   				fluidPage(fluidRow(
			 			   		   					column(10, tipify(sliderInput("traj_feat_y_range_In", "y Axis Range", min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE), "y axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only y axis selection is registered).", "top")), 
			 			   		   					column(2, checkboxInput("traj_feat_y_range_check_In", "", value = TRUE))),
			 			   		   				fluidRow(
			 			   		   					column(6, textInput("traj_feat_xlab_In", "x axis label"),
			 			   		   						   textInput("traj_feat_x_unit_In", "x Axis Unit")
			 			   		   					),
			 			   		   					column(6, textInput("traj_feat_ylab_In", "y axis label"),
			 			   		   						   textInput("traj_feat_y_unit_In", "y Axis Unit")
			 			   		   					)
			 			   		   				)),
			 			   		   				bsTooltip("traj_feat_xlab_In", "x axis label", "bottom", "hover"),
			 			   		   				bsTooltip("traj_feat_ylab_In", "y axis label", "bottom", "hover"),
			 			   		   				bsTooltip("traj_feat_x_unit_In", toolTips$unit_In, placement = "bottom", trigger = "hover"),
			 			   		   				bsTooltip("traj_feat_y_unit_In", toolTips$unit_In, placement = "bottom", trigger = "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Transformations", 
										selectInput("traj_feat_data_transform_In", "Transform data with", choices = dataTransformChoices, selected = "noneTransform"),
										conditionalPanel("input.traj_feat_data_transform_In  == 'logTransform'",
														 shinyWidgets::sliderTextInput("traj_feat_data_logTransform_In", "\\(\\log_a(x) \\) ... a", choices = c(2, exp(1), 10), selected = exp(1))
										),
										conditionalPanel("input.traj_feat_data_transform_In  == 'powerTransform'",
														 sliderInput("traj_feat_data_powerTransform_In", "\\(x^a\\) ... a", min = 2, max = 5, step = 1, value = 3)
										),
										conditionalPanel("input.traj_feat_data_transform_In  == 'rootTransform'",
														 sliderInput("traj_feat_data_rootTransform_In", "\\(\\sqrt[a]{x}\\) ... a", min = 2, max = 5, step = 1, value = 3)
										),
										conditionalPanel("input.traj_feat_data_transform_In  == ''",
														 sliderInput("traj_feat_data_invTransform_In", "", min = 1, max = 2, step = 1, value = 1),
														 sliderInput("traj_feat_data_noneTransform_In", "", min = 1, max = 2, step = 1, value = 1)
										)
			 			   		   ),
			 			   		   bsCollapsePanel("Display Options", 
			 			   		   				sliderInput("traj_feat_disp_alpha_In", "Error Transparency", min = 0, max = 1, value = 0.5),
			 			   		   				sliderInput("traj_feat_color_alpha_In", "Color Transparency", min = 0, max = 1, value = 1),
			 			   		   				darkPlot("traj_feat"),
			 			   		   				
			 			   		   				bsTooltip("traj_feat_disp_alpha_In", "Dispersion display transparency.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_feat_color_alpha_In", "Line/Point color transparency.", "bottom", "hover"),
			 			   		   				fluidPage(
			 			   		   					conditionalPanel("input.traj_feat_type_In.indexOf('point') > -1 || input.traj_feat_type_In.indexOf('jitter') > -1",
			 			   		   									 sliderInput("traj_feat_point_size_In", "Point Size", min = 0.001, max = 100, value = 0.5, step = 0.25),
			 			   		   									 hr()				 
			 			   		   					),
			 			   		   					conditionalPanel("input.traj_feat_type_In.indexOf('line') > -1 || input.traj_feat_type_In.indexOf('area') > -1 || input.traj_feat_type_In.indexOf('smooth') > -1 || input.traj_feat_type_In.indexOf('quantile') > -1", 
			 			   		   									 sliderInput("traj_feat_line_size_In", "Line Thickness", min = 0.001, max = 100, value = 0.5, step = 0.25),
			 			   		   									 hr()
			 			   		   					)
			 			   		   				),
			 			   		   				bsTooltip("traj_feat_point_size_In", "Relative point size.", "bottom", "hover"),
			 			   		   				bsTooltip("traj_feat_line_size_In", "Relative line thickness.", "bottom", "hover")
			 			   		   ),
			 			   		   bsCollapsePanel("Debugging", 
			 			   		   				plotDebugging("traj_feat")
			 			   		   )
			 			   		   
			 			   		   
			 			   )
			 			   
			 			   
			 		),
			 		column(8, 
			 			   plotOutput(outputId = "trajFeaturePlotOut"),
			 			   statDataDetails("traj_feat"),
			 			   tags$style(type="text/css", "#traj_feat_stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   fluidPage(fluidRow(column(6, actionButton(inputId = "plotTrajFeatIn", label = "Plot Trajectory Festures")),
			 			   				   column(6, plotExportSection("traj_feat"))))
			 		)
			 	)
			 )
	)
}

# Define UI
ui = function(request){
	
	fluidPage(shinyjs::useShinyjs(), shinyjs::extendShinyjs(functions = "shinyjs.init", 
		text = paste0("shinyjs.init = function(){
		  $('#tabs li a[data-value=", titleFiles ,"]').parents('li').hide(); $('#tabs li a[data-value=", titleFeatures,"]').parents('li').hide();
		  $('#tabs li a[data-value=", titleTracks ,"]').parents('li').hide(); $('#tabs li a[data-value=", titleTrajectories,"]').parents('li').hide();
		  $('#tabs li a[data-value=\"", titlePlotTrackFeatures ,"\"]').parents('li').hide(); $('#tabs li a[data-value=\"", titlePlotTrajectories,"\"]').parents('li').hide();
		  $('#tabs li a[data-value=\"", titlePlotDirectionality ,"\"]').parents('li').hide(); $('#tabs li a[data-value=\"", titlePlotTrajFeatures,"\"]').parents('li').hide();
		}")),
			  title = titlePanel("TraXplorer"),
		tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;} .action-button {background-color: #77FF77}
								  .navbar-default .navbar-nav > li[class=active] {font-weight: bold;}"))),
		fluidRow(
			tags$head(tags$style(HTML(".tab-content { height: 90vh; overflow-y: auto !important; }"))),
			#column(12,
			navbarPage(title = "TraXplorer", id = tabsID, 
					   #tabsetPanel(type = "tabs",
						tabPanelImport(title = titleImportGroupings, tabColor = tabColorImport),
						tabPanelOperations(titleOperations, tabColorOperations),
						tabPanelTable(titleFiles, "filesOut", "downloadFilesTable", "Download files table as CSV", tabColorTables),
						tabPanelTable(titleFeatures, "featuresOut", "downloadFeaturesTable", "Download features table as CSV", tabColorTables),
						tabPanelTable(titleTracks, "tracksOut", "downloadTracksTable", "Download tracks table as CSV", tabColorTables),
						tabPanelTable(titleTrajectories, "trajectoriesOut", "downloadTrajectoriesTable", "Download trajectories table as CSV", tabColorTables),
						tabPanelPlotTrackFeatures(titlePlotTrackFeatures, tabColorPlots),
						tabPanelPlotTrajectories(titlePlotTrajectories, tabColorPlots),
						tabPanelDirectionality(titlePlotDirectionality, tabColorPlots),
						tabPanelPlotTrajectoryFeatures(titlePlotTrajFeatures, tabColorPlots),
						tabPanelAbout(titleAbout)
					)
			#)
		),
		fluidRow(column(12, fluidPage(fluidRow( style="background-color:#eee; border: 1px solid #aaa; padding: 10px;border-radius:4px",
												column(4, align = "left", "by Saren Tasciyan"),
												column(4, align = "center", "All rights reserved"),
												column(4, align = "right", "v1.0")))
		)
		)
	)
}
server = function(input, output, session) {
	source("functions.R")
	
	labelRVs = list()
	# Save extra values in state$values when we bookmark
	onBookmark(function(state) {
		state$values$data = data
	})
	
	# Read values from state$values when we restore
	onRestore(function(state) {
		data = state$values$data
	})
	
	#observe({toggleState("plot_width_In", condition = input$plot_autowidth_In)})
	#observe({toggleState("plot_height_In", condition = input$plot_autoheight_In)})
	
	observeEvent(input$tmFilesIn, {
		tmFiles = input$tmFilesIn
		tmFiles[pointSourceColumns] = NA
		
		for(pointSourceColumn in pointSourceColumns){
			tmFiles[[pointSourceColumn]] = as.numeric(tmFiles[[pointSourceColumn]])
		}
		
		data(list(files = tmFiles))
		
		groupings(prepareBareGroupings(tmFiles$name))
		hideTab(tabsID, titleFiles); hideTab(tabsID, titleFeatures); 
		hideTab(tabsID, titleTracks); hideTab(tabsID, titleTrajectories)
		hideTab(tabsID, titlePlotTrackFeatures); hideTab(tabsID, titlePlotTrajectories); 
		hideTab(tabsID, titlePlotDirectionality); hideTab(tabsID, titlePlotTrajFeatures)
	})
	
	observeEvent(input$sessionFileIn, {
		#browser()
		sessionFilePath = input$sessionFileIn$datapath[1]
		sessionFile = file(paste0("file://", sessionFilePath))
		open(sessionFile, "rb")
		dataSession = unserialize(connection = sessionFile)
		close(sessionFile)
		
		if(!is.null(dataSession$version)){
			version(dataSession$version)
		}else{
			version(0)
		}
		
		dataSession = dataSessionVersionUpgrade(dataSession, version())
		
		if(!is.null(dataSession$groupings)){
			groupings(dataSession$groupings)
		}
		
		if(!is.null(dataSession$data)){
			data(dataSession$data)
		}
		
		if(!is.null(dataSession$version)){
			version(dataSession$version)
		}
		
		showTab(inputId = tabsID, target = titleTracks)
		showTab(inputId = tabsID, target = titleFiles)
		showTab(inputId = tabsID, target = titleFeatures)
		showTab(inputId = tabsID, target = titleTrajectories)
		showTab(inputId = tabsID, target = titlePlotTrajFeatures)
		showTab(inputId = tabsID, target = titlePlotTrackFeatures)
		showTab(inputId = tabsID, target = titlePlotTrajectories)
		showTab(inputId = tabsID, target = titlePlotDirectionality)
		showTab(inputId = tabsID, target = titlePlotTrajFeatures)
		#data(list())
		#browser()
		#serialize(dataObj, NULL)
	})
	
	groupings = reactiveVal({
		#browser()
		print("groupings = reactiveVal({")
		prepareBareGroupings(NULL)
	})
	
	#filesList = reactiveVal(NULL)
	version = reactiveVal(NULL)
	data = reactiveVal(NULL)
	
	tracks = reactive({data()$tracks}); trajectories = reactive({data()$trajectories}); 
	files = reactive({data()$files}); features = reactive({data()$features})
	
	pointSourceColumnDisable = reactive({
		colsToShow = c("name", pointSourceColumns)
		colsToDisable = which(!(colnames(files()) %in% colsToShow)) - 1
		#print(colsToDisable)
		colsToDisable
		
	})
	parseParameters = reactiveVal(NULL)
	
	#userUnitInputs = reactiveVal(NULL)
	observeEvent(input$pointSourceDefUploadIn, {
		
		if(endsWith(input$pointSourceDefUploadIn$datapath[1], ".csv")){
			psData = read.csv(input$pointSourceDefUploadIn$datapath[1])
			psData = psData %>% select_at(vars("name", pointSourceColumns))
			
			if((psData %>% summarise_all(is.numeric) %>% select(-name) %>% 
				mutate(all = all(c_across())) %>% select(all))$all[1]){
				
				dataList = data()
				dataList$files = dataList$files %>% select_at(vars(!contains(pointSourceColumns))) %>% 
					left_join(psData, by = "name")
				data(dataList)
			}
		}
		#dataList = data()
	})
	observeEvent(input$pointSourceIn, {
		if(input$point_source_browse_In){
			browser()
		}
		initializeProgress = function(max, message){
			progress <<- shiny::Progress$new(max = max)
			if(!is.null(message)){
				progress$set(message = message, value = 0)
			}else{
				progress$set(value = 0)
			}
		}
		updateProgress = function(value, detail = NULL) {
			if(is.null(detail)){progress$set(value = value)}else{progress$set(value = value, detail = detail)}
		}
		closeProgress = function(){progress$close()}
		
		dataList = data()
		#TODO  remove below line
		#dataList$files[pointSourceColumns] = 1000
		if(sum(is.na(dataList$files[pointSourceColumns])) == 0){
			data(pointSource(dataList, updateProgress, initializeProgress, closeProgress, 
							 browse = input$point_source_browse_In))
		}
	})
	
	observeEvent(input$rotateIn, {
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
		
		dataDF = data()
		
		data(rotateTracktms(dataDF, updateProgress, initializeProgress, closeProgress, 
						  rotation_fix = input$processRotationFixIn, rotation_z_fix = input$processZRotationFixIn, 
						  browse = input$rotate_browse_In))
	})
	
	observeEvent(input$processFilesIn, {
	#dataTMFiles = eventReactive(input$processFilesIn, {
		# Create a Progress object
		recalculate = input$process_recalculate_In
		browse = input$process_browse_In
		if(browse){
			browser()
		}
		click = input$processFilesIn
		
		uploadedFiles = files()
		if(!is.null(uploadedFiles)){
			initializeProgress = function(max, message){
				progress <<- shiny::Progress$new(max = max)
				if(!is.null(message)){
					progress$set(message = message, value = 0)
				}else{
					progress$set(value = 0)
				}
			}
			
			userUnitInput = function(failed = FALSE){
				modalDialog(
					numericInput(inputId = "pixelWidthIn", label = "Enter pixel width (x) size in μm", 
								 value = 2.301, min = 0.0000000001),
					numericInput(inputId = "pixelHeightIn", label = "Enter pixel height (y) size in μm", 
								 value = 2.301, min = 0.0000000001),
					numericInput(inputId = "voxelThicknessIn", label = "Enter voxel thickness (z) size in μm", 
								 value = 1, min = 0.0000000001),
					numericInput(inputId = "frameIntervallIn", label = "Enter frame intervall (t) in sec", 
								 value = 60, min = 0.0000000001),
					span('Manual tracking files don\'t include pixel size and frame intervall information. Please provide it here'),
					if (failed)
						div(tags$b("Invalid input. Please use \".\" as decimal and all fields are mandatory. Use 1 for voxel thickness for 2D images.", style = "color: red;")),
					
					footer = tagList(
						modalButton("Cancel"),
						actionButton("userUnitInputOKIn", "OK")
					)
				)
			}
			# Close the progress when this reactive exits (even if there's an error)
			#on.exit({progress$close()})
			
			updateProgress = function(value, detail = NULL) {
				if(is.null(detail)){progress$set(value = value)}else{progress$set(value = value, detail = detail)}
			}
				
			closeProgress = function(){progress$close()}
			
			groupingsDF = groupings()$groupings
			groupsDF = groupings()$groups
			if(any(endsWith(uploadedFiles$datapath, suffix = ".csv") | endsWith(uploadedFiles$datapath, suffix = ".txt"))){
				parseParameters(list(files = uploadedFiles$datapath, groupings = groupingsDF, groups = groupsDF, 
									 fileNames = uploadedFiles$name, 
					 updateProgress = updateProgress, closeProgress = closeProgress, initializeProgress = initializeProgress, 
					 browse = browse))
				observeEvent(input$userUnitInputOKIn, {
					# Check that data object exists and is data frame.
					# browser()
					if (!is.null(input$pixelWidthIn) && !is.null(input$pixelHeightIn) && 
						!is.null(input$voxelThicknessIn) && !is.null(input$frameIntervallIn)) {
						userUnits = list(pixelWidth = input$pixelWidthIn, pixelHeight = input$pixelHeightIn, 
										 voxelThickness = input$voxelThicknessIn, 
										 frameIntervall = input$frameIntervallIn)
						dataDF = parseFiles(uploadedFiles, groupingsDF, groupsDF, updateProgress, 
											closeProgress, initializeProgress, browse = browse, 
											calibrationUnits = userUnits)
						if(!is.null(dataDF)){
							data(processData(dataDF, groupsDF, groupingsDF, updateProgress, initializeProgress, closeProgress, 
											 recalculate = recalculate, browse = browse))
							
						}else{
							data(NULL)
						}
						removeModal()
					} else {
						showModal(userUnitInput(failed = TRUE))
					}
				})
				showModal(userUnitInput())
			}else{
				dataDF = parseFiles(uploadedFiles, groupingsDF, groupsDF, updateProgress, 
									closeProgress, initializeProgress, recalculate = recalculate, browse = browse)
				data(processData(dataDF, groupsDF, groupingsDF, updateProgress, initializeProgress, closeProgress, 
								 recalculate = recalculate, browse = browse))
				#addClass(selector = "#tabs li a[data-value = tab1]", class = 'showtab')
				#browser()
				showTab(inputId = tabsID, target = titleTracks)
				showTab(inputId = tabsID, target = titleFiles)
				showTab(inputId = tabsID, target = titleFeatures)
				showTab(inputId = tabsID, target = titleTrajectories)
				showTab(inputId = tabsID, target = titlePlotTrajFeatures)
				showTab(inputId = tabsID, target = titlePlotTrackFeatures)
				showTab(inputId = tabsID, target = titlePlotTrajectories)
				showTab(inputId = tabsID, target = titlePlotDirectionality)
				showTab(inputId = tabsID, target = titlePlotTrajFeatures)
			}
		}else{
			showModal(modalDialog(title = "File Upload Error", "Something went wrong during file upload."))
		}
	})
	
	observe({updateSelectInput(session, "track_x_In", choices = groupingsChoiceswithoutEmpty())})
	observe({updateSelectInput(session, "track_y_In", choices = trackChoiceswithoutEmpty(), 
							   selected = "TRACK_MEAN_SPEED")})
	observe({updateSelectInput(session, "track_fill_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "track_color_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "track_facet_row_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "track_facet_col_In", choices = groupingsChoiceswithEmpty())})
	#observe({updateSelectInput(session, "track_stat_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "track_replicate_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "track_stat_comparison_control_In", choices = trackStatGroups(), 
							   selected = safeSelect(trackStatGroups()))})
	
	observe({updateSelectInput(session, "traj_xy_In", choices = trajectoryXYLocationswithoutEmpty(), 
							   selected = "fixed")})
	#observe({updateSelectInput(session, "traj_x_In", choices = trajectoryXLocationswithoutEmpty(), selected = "POSITION_X_FIX")})
	#observe({updateSelectInput(session, "traj_y_In", choices = trajectoryYLocationswithoutEmpty(), selected = "POSITION_Y_FIX")})
	#observe({updateSelectInput(session, "traj_fill_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_color_In", choices = groupingsAndFeatureChoiceswithoutEmpty())})
	observe({updateSelectInput(session, "traj_start_point_In", choices = groupingsChoiceswithEmptywithDoNotDisplay())})
	observe({updateSelectInput(session, "traj_end_point_In", choices = groupingsChoiceswithEmptywithDoNotDisplay())})
	#observe({updateSelectInput(session, "traj_alpha_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_facet_row_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_facet_col_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_replicate_In", choices = groupingsChoiceswithEmpty())})
	
	#observe({updateSelectInput(session, "dir_track_direction_In", choices = trackDirectionChoiceswithoutEmpty())})
	observe({updateSelectInput(session, "dir_track_direction_cat_In", choices = trackDirectionCatChoiceswithoutEmpty(), 
							   selected = "DIRECTION_CARDINAL")})
	observe({updateSelectInput(session, "dir_cumulation_In", choices = trackChoiceswithoutEmpty(), selected = "TRACK_ID")})
	observe({updateSelectInput(session, "dir_color_In", choices = groupingsChoiceswithoutEmpty())})
	observe({updateSelectInput(session, "dir_fill_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "dir_alpha_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "dir_facet_row_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "dir_facet_col_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "dir_replicate_In", choices = groupingsChoiceswithEmpty())})
	
	observe({updateSelectInput(session, "traj_feat_x_In", choices = trajChoiceswithoutEmpty(), selected = "EDGE_TIME")})
	observe({updateSelectInput(session, "traj_feat_y_In", choices = trajChoiceswithoutEmpty(), selected = "VELOCITY")})
	observe({updateSelectInput(session, "traj_feat_fill_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_color_In", choices = groupingsChoiceswithoutEmpty())})
	observe({updateSelectInput(session, "traj_feat_shape_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_linetype_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_size_In", choices = trajChoiceswithEmpty())})
	
	observe({updateSelectInput(session, "traj_feat_facet_row_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_facet_col_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_stat_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_replicate_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_disp_fun_In", choices = dispersionChoices())})
	
	observe({updateSelectInput(session, "new_track_feat_vars_In", choices = allTrackMeasures())})
	observe({updateSelectInput(session, "new_traj_feat_vars_In", choices = allTrajectoryMeasures())})
	observe({updateSelectInput(session, "new_track_from_traj_feat_vars_In", choices = allTrajectoryMeasures())})
	
	
	#trackRanges = reactiveValues(y = NULL)
	
	observeEvent(input$new_track_feat_vars_In, {
		#browser()
		formula = input$new_track_feat_formula_In
		formula = paste0(formula, input$new_track_feat_vars_In)
		updateTextInput(session = session, inputId = "new_track_feat_formula_In", value = formula)
	})
	
	observeEvent(input$new_traj_feat_vars_In, {
		#browser()
		formula = input$new_traj_feat_formula_In
		formula = paste0(formula, input$new_traj_feat_vars_In)
		updateTextInput(session = session, inputId = "new_traj_feat_formula_In", value = formula)
	})
	
	observeEvent(input$new_track_from_traj_feat_vars_In, {
		#browser()
		formula = input$new_track_from_traj_feat_formula_In
		formula = paste0(formula, input$new_track_from_traj_feat_vars_In)
		updateTextInput(session = session, inputId = "new_track_from_traj_feat_formula_In", value = formula)
	})
	
	#observe({updateSliderInput(session, "track_x_range_In", min = getXMin(), max = getXMax(), step = getXStep(), value = c(getXMin(), getXMax()))})
	observe({updateSliderInput(session, "track_y_range_In", min = getTrackYMin(), max = getTrackYMax(), 
							   step = getTrackYStep(), value = c(getTrackYMin(), getTrackYMax()))})
	
	observe({updateSliderInput(session, "traj_feat_y_range_In", min = getTrajFeatYMin(), max = getTrajFeatYMax(), 
							   step = getTrajFeatYStep(), value = c(getTrajFeatYMin(), getTrajFeatYMax()))})
	
	observeEvent(input$trackFeaturePlotOut_dblclick, {
		#browser()
		brush = input$trackFeaturePlotOut_brush
		if (!is.null(brush)) {
			#ranges$x = c(brush$xmin, brush$xmax)
			#trackRanges$y = c(brush$ymin, brush$ymax)
			#updateCheckboxInput(session, "track_x_range_check_In", value = TRUE)
			updateCheckboxInput(session, "track_y_range_check_In", value = TRUE)
			#updateSliderInput(session, "track_x_range_In", min = getXMin(), max = getXMax(), step = getXStep(), value = c(brush$xmin, brush$xmax))
			updateSliderInput(session, "track_y_range_In", min = getTrackYMin(), max = getTrackYMax(), 
							  step = getTrackYStep(), value = c(brush$ymin, brush$ymax))
		} else {
			#ranges$x = NULL
			#trackRanges$y = NULL
			#updateCheckboxInput(session, "track_x_range_check_In", value = FALSE)
			updateCheckboxInput(session, "track_y_range_check_In", value = FALSE)
			#updateSliderInput(session, "track_x_range_In", min = getXMin(), max = getXMax(), step = getXStep(), value = c(getXMin(), getXMax()))
			updateSliderInput(session, "track_y_range_In", min = getTrackYMin(), max = getTrackYMax(), 
							  step = getTrackYStep(), value = c(getTrackYMin(), getTrackYMax()))
		}
	})
	
	getTrackYPretty = reactive({
		if(!is.null(tracks()) && !(input$track_y_In == "")){
			#browser()
			transformFun = match.fun(trackTransform()$method)
			values = tracks()[[input$track_y_In]]
			unit = attr(values, "unit")
			
			if(is.null(unit)){
				unit = ""
			}
			
			unitToConvert = input$track_unit_In
			if(unitToConvert == ""){
				unitToConvert = unit
			}
			
			if(!udunits2::ud.are.convertible(unit, unitToConvert)){
				unitToConvert = unit
			}
			values = transformFun(udunits2::ud.convert(values, unit, unitToConvert), trackTransform()$parameter)
			
			#if()
			pretty(values, 20)
		}
	})
	#getXMin = reactive({getXPretty()[1]})
	#getXMax = reactive({x = getXPretty(); x[length(x)]})
	getTrackYMin = reactive({
		#browser()
		prettyMin = getTrackYPretty()[1]
		if(!is.null(prettyMin)){
			if(prettyMin > 0){
				return(0)
			}else{
				return(prettyMin * 2)
			}
		}
	})
	getTrackYMax = reactive({
		prettyMax = last(getTrackYPretty())
		if(!is.null(prettyMax)){
			if(prettyMax < 0){
				return(0)
			}else{
				return(prettyMax * 2)
			}
		}
		
	})
	getTrackYStep = reactive({
		y = getTrackYPretty()
		if(!is.null(y)){
			y[2] - y[1]
		}
	})
	
	
	getTrajFeatYPretty = reactive({
		if(!is.null(tracks()) && !(input$traj_feat_y_In == "")){
			#browser()
			transformFun = match.fun(trajFeatTransform()$method)
			values = trajectories()[[input$traj_feat_y_In]]
			unit = attr(values, "unit")
			
			if(is.null(unit)){
				unit = ""
			}
			
			unitToConvert = input$traj_feat_y_unit_In
			if(unitToConvert == ""){
				unitToConvert = unit
			}
			
			if(!udunits2::ud.are.convertible(unit, unitToConvert)){
				unitToConvert = unit
			}
			values = transformFun(udunits2::ud.convert(values, unit, unitToConvert), trackTransform()$parameter)
			
			#if()
			pretty(values, 20)
		}
	})
	
	getTrajFeatYMin = reactive({
		#browser()
		prettyMin = getTrajFeatYPretty()[1]
		if(!is.null(prettyMin)){
			if(prettyMin > 0){
				return(0)
			}else{
				return(prettyMin * 2)
			}
		}
	})
	getTrajFeatYMax = reactive({
		prettyMax = last(getTrajFeatYPretty())
		if(!is.null(prettyMax)){
			if(prettyMax < 0){
				return(0)
			}else{
				return(prettyMax * 2)
			}
		}
		
	})
	getTrajFeatYStep = reactive({
		y = getTrajFeatYPretty()
		if(!is.null(y)){
			y[2] - y[1]
		}
	})
	
	# Setting default unit upon y axis selection
	observe({
		#browser()
		#print(input$track_y_In)
		req(input$track_y_In)
		updateTextInput(session = session, inputId = "track_unit_In", 
						value = attr(tracks()[[input$track_y_In]], "unit"))
	})
	
	observe({
		#browser()
		#print(input$track_y_In)
		req(input$traj_feat_x_In)
		updateTextInput(session = session, inputId = "traj_feat_x_unit_In", 
						value = attr(trajectories()[[input$traj_feat_x_In]], "unit"))
	})
	
	observe({
		#browser()
		req(input$traj_feat_y_In)
		updateTextInput(session = session, inputId = "traj_feat_y_unit_In", 
						value = attr(trajectories()[[input$traj_feat_y_In]], "unit"))
	})
	
	
	trackChoiceswithEmpty = reactive({
		#featuresToNamedList("Track", data()$features, empty = TRUE)
		choicesInNamedList("Track", features(), empty = TRUE)
	})
	
	trajectoryXYLocationswithoutEmpty = reactive({
		#trajectoryPositionNamedList(c("Spot", "Edge"), data()$features, "x", empty = FALSE)
		#browser()
		#namedList = choicesInNamedList(c("Spot", "Edge"), features(), "POSITION_X", empty = FALSE)
		#positionTypes[1:length(namedList)] 
		# Error with new column POSITION_X_DISPLACEMENT, which also matches POSITION_X
		# Actually now that we have POSITION_X_FIX_ROT by default same as POSITION_X_FIX, we dont need length check
		positionTypes
	})
	
	# trajectoryXLocationswithoutEmpty = reactive({
	# 	#trajectoryPositionNamedList(c("Spot", "Edge"), data()$features, "x", empty = FALSE)
	# 	choicesInNamedList(c("Spot", "Edge"), features(), "POSITION_X", empty = FALSE)
	# })
	# 
	# trajectoryYLocationswithoutEmpty = reactive({
	# 	#trajectoryPositionNamedList(c("Spot", "Edge"), data()$features, "POSITION_Y", empty = FALSE)
	# 	choicesInNamedList(c("Spot", "Edge"), features(), "POSITION_Y", empty = FALSE)
	# })
	
	allTrackMeasures = reactive({
		choices = list()
		choices[["Track Features"]] = c(c(` ` = ""), as.vector(choicesInNamedList(c("Track"), features(), 
																				  empty = FALSE)))
		choices[["Formula Items"]] = as.vector(formulaChoices)
		return(choices)
	})
	
	allTrajectoryMeasures = reactive({
		choices = list()
		choices[["Trajectory Features"]] = c(c(` ` = ""), as.vector(choicesInNamedList(c("Spot", "Edge"), features(), 
																					   empty = FALSE)))
		choices[["Formula Items"]] = as.vector(formulaChoices)
		return(choices)
	})
	
	trackChoiceswithoutEmpty = reactive({
		#featuresToNamedList("Track", data()$features, empty = FALSE)
		#browser()
		choicesInNamedList("Track", features(), empty = FALSE)
	})
	
	trajChoiceswithoutEmpty = reactive({
		#featuresToNamedList("Track", data()$features, empty = FALSE)
		#browser()
		choicesInNamedList(c("Spot", "Edge"), features(), empty = FALSE)
	})
	
	trajChoiceswithEmpty = reactive({
		#featuresToNamedList("Track", data()$features, empty = FALSE)
		#browser()
		choicesInNamedList(c("Spot", "Edge"), features(), empty = TRUE)
	})
	
	groupingsChoiceswithEmpty = reactive({
		req(groupings())
		#browser()
		groupingsToNamedList(groupings()$groupings, empty = TRUE)
	})
	
	trackStatGroups = reactive({
		#browser()
		statGroup = input$track_x_In
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
	
	groupingsChoiceswithEmptywithDoNotDisplay = reactive({
		req(groupings())
		#browser()
		groupingsToNamedList(groupings()$groupings, empty = TRUE, doNotDisplay = TRUE)
	})
	
	groupingsAndFeatureChoiceswithoutEmpty = reactive({
		#browser()
		choices = list(`Grouping Variables` = as.vector(groupingsChoiceswithoutEmpty()),
				`Measures` = as.vector(trajChoiceswithoutEmpty()))
		choices
	})
	
	groupingsChoiceswithoutEmpty = reactive({
		req(groupings())
		groupingsToNamedList(groupings()$groupings, empty = FALSE)
	})
	
	trackDirectionChoiceswithoutEmpty = reactive({
		#req(groupings())
		#featuresToNamedList(type = "Track", features = data()$features, empty = FALSE)
		choicesInNamedList(type = "Track", features(), "TRACK_DIRECTION", empty = FALSE)
	})
	
	trackDirectionCatChoiceswithoutEmpty = reactive({
		#req(groupings())
		#featuresToNamedList(type = "Track", features = data()$features, empty = FALSE)
		#browser()
		a = choicesInNamedList(type = "Track", features(), "CARDINAL", empty = FALSE)
		print("a:")
		print(a)
		return(a)
		
	})
	
	dispersionChoices = reactive({
		#featuresToNamedList("Track", data()$features, empty = FALSE)
		#browser()
		# According to the chosen aggregate function, the index of the choice is found (which). Then the available 
		# choices is found (in dispChoicesforAggrFunction). Finally names of available dispersion functions are 
		# found (in dispersionMeasureChoices).
		if(!is.null(input$traj_feat_aggr_fun_In)){
			dispersionMeasureChoices[dispChoicesforAggrFunction[[
				which(aggregateFunctionChoices == input$traj_feat_aggr_fun_In)]]]
		}
		#choicesInNamedList(c("Spot", "Edge"), inp, empty = FALSE)
	})
	

	
	observeEvent(input$groupingsOut_cell_clicked, {
		info = input$groupingsOut_cell_clicked
		if(!is.null(info$row)){
			#browser()
			groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
			grouping = groupingsDF[[info$row, 1]]
			groupingLabel = groupingsDF[[info$row, 2]]
			groups = groupingsDF[[info$row, 3]]
			groupLabels = groupingsDF[[info$row, 4]]
			groupColors = groupingsDF[[info$row, 5]]
			
			removeUI(selector = '#placeholder input', multiple = TRUE)
			removeUI(selector = '#placeholder textarea', multiple = TRUE)
			removeUI(selector = '#placeholder label', multiple = TRUE)
			id = paste0("groupLabel_", info$row)

			insertUI(selector = '#placeholder', ui = textInput(id, label = "Grouping label:", value = groupingLabel))
			for(labelRV in labelRVs){labelRV$destroy()}
			print(labelRVs)
			#browser()
			labelRVs <<- list()
			labelRVs[[id]] <<- observeEvent(input[[id]], {
				groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
				groupingsDF[, 2] = as.vector(groupingsDF[, 2])
				groupingsDF[[info$row, 2]] = input[[id]]
				groupingsDF[, 2] = as.factor(groupingsDF[, 2])
				#cat(paste(id, info$row, "\n"))
				#cat(paste("\t", input[[id]], paste(groupingsDF[info$row, 4], collapse = "-"), "\n"))
				groupings(list(groupings = groupingsDF, groups = groupsDF))
			})
			lapply(1:length(groups), function(i){
			#for(i in 1:length(groups)){
				insertUI(selector = '#placeholder',
						 ui = textAreaInput(paste0('label_', sprintf("%03d", i)), 
						 				   label = paste0("Group label for ", groups[i], ":"), value = groupLabels[i]))
				labelRVs[[paste0('label_', sprintf("%03d", i))]] <<- observeEvent(input[[paste0('label_', sprintf("%03d", i))]], {
					groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
					listVals = as.character(groupingsDF[[info$row, 4]])
					listVals[i] = input[[paste0('label_', sprintf("%03d", i))]]
					groupingsDF[[info$row, 4]] = as.factor(listVals)
					#cat(paste("---", id, info$row, "\n"))
					#cat(paste("---", "\t", input[[paste0('label_', sprintf("%03d", i))]], paste(groupingsDF[info$row, 4], collapse = "-"), "\n"))
					groupings(list(groupings = groupingsDF, groups = groupsDF))
				})
				insertUI(selector = "#placeholder", 
						 ui = colourInput(paste0('color_', sprintf("%03d", i)), label = paste0("Group color for ", groups[i], ":"), 
						 				 value = groupColors[i]))
				labelRVs[[paste0('color_', sprintf("%03d", i))]] <<- observeEvent(input[[paste0('color_', sprintf("%03d", i))]], {
					groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
					#groupingsDF[, 2] <<- as.vector(groupingsDF[, 2])
					groupingsDF[[info$row, 5]][i] = input[[paste0('color_', sprintf("%03d", i))]]
					#groupingsDF[, 2] <<- as.factor(groupingsDF[, 2])
					#cat(paste("+++", id, info$row, "\n"))
					#cat(paste("+++", "\t", input[[paste0('color_', sprintf("%03d", i))]], paste(groupingsDF[info$row, 5], collapse = "-"), "\n"))
					groupings(list(groupings = groupingsDF, groups = groupsDF))
				})
			})
			print(names(labelRVs))
			cat("========================================================\n")
		}
	})
	
	observe({
		groupingsList = groupings()
		dataList = data()
		statGroup = input$track_x_In
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
							 ui = generateBucketList(choices, context = "track"),
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
	
	observe({
		groupingsList = groupings()
		dataList = data()
		statGroup = input$dir_color_In
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
							 ui = generateBucketList(choices, "dir"),
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
	#groupRankListOut = list()
	
	#output$groupRankOut = renderUI({groupRankListOut()})
	
	rankListObs = list() # observeEvents for rank_list objects
	observe({
		groupingsList = groupings()
		dataList = data()
		#browser()
		#uiList = list()
		if(length(groupingsList) > 1){
			#browser()
			
			groupingsDF = groupingsList$groupings
			
			removeUI(selector = "#placeholderRank div", multiple = TRUE)
			for(rankListOb in rankListObs){rankListOb$destroy()}
			rankListObs <<- list()
			if(length(dataList) > 1){
			
				lapply(1:nrow(groupingsDF), function(i){
				#for(i in 1:nrow(groupingsDF)){
					#browser()
					
					#uiList[[i]] = 
					#insertUI(selector = "#placeholderRank", ui =  textAreaInput(inputId = paste("rankGroupahaha",  sprintf("%03d", i), sep = "_"), 
					#															label = "Grouping label:", value = paste(i)))
					insertUI(selector = "#placeholderRank", ui = rank_list(text = as.character(groupingsDF$labels[i]),
							  labels = as.list(as.character(groupingsDF$groupLabels[[i]])),
							  input_id = paste("rankGroup", sprintf("%03d", i), sep = "_")), where = "beforeEnd")
					cat(i, as.character(groupingsDF$labels[i]),  paste("rankGroup", sprintf("%03d", i), sep = "_"), "\n")
					#beepr::beep()
					rankListObs[[paste("rankGroup", sprintf("%03d", i), sep = "_")]] <<- observeEvent(input[[paste("rankGroup",  sprintf("%03d", i), sep = "_")]], {
						groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
						#browser()
						newOrder = input[[paste('rankGroup', sprintf("%03d", i), sep = "_")]]
						#cat(newOrder); cat("\n")
						#cat(as.character(groupingsDF$groupLabels[[i]])); cat("\n")
						#cat(as.character(groupingsDF$groupLabels[[i]]) == newOrder); cat("\n")
						
						# There seems to be a change in rank order
						
						if(!all(as.character(groupingsDF$groupLabels[[i]]) == newOrder)){
							ranks = match(newOrder, groupingsDF$groupLabels[[i]])
							
							
							# NA means some labels are changed, this triggers due to above statement but user actually didn't update anything
							if(!any(is.na(ranks))){	
								#browser()
								groupingsDF$groupLabels[[i]] = factor(newOrder, 
																	  levels = as.character(groupingsDF$groupLabels[[i]])[ranks])
								
								groupingsDF$groups[[i]] = factor(as.character(groupingsDF$groups[[i]])[ranks], 
																 levels = as.character(groupingsDF$groups[[i]])[ranks])
								groupingsDF$colors[[i]] = as.character(groupingsDF$colors[[i]])[ranks]
								
								group = as.character(groupingsDF$names[i])
								
								dataList = data()
								for(dfName in names(dataList)){
									if(!is.null(dataList[[dfName]][[group]])){
										dataList[[dfName]][[group]] = factor(dataList[[dfName]][[group]], 
																		 levels = groupingsDF$groups[[i]])
									}
								}
								
								data(dataList)
								groupsDF[[group]] = factor(groupsDF[[group]], levels = groupingsDF$groups[[i]])
								groupings(list(groupings = groupingsDF, groups = groupsDF))
							}
						}
					})
				})
			}
		}
		
	})
	
	output$groupingsOut = renderDT({groupings()$groupings}, rownames = FALSE, editable = FALSE, selection = 'single', 
								   colnames = c('Groupings', 'Grouping Labels', 'Groups', 'Group Labels', 'Group Colors'))
	output$files_point_source_Out = renderDT(data()$files, rownames = FALSE, 
										  editable = list(target = c("cell"), disable = list(columns = c(0))), 
										  selection = 'none', 
											 options = list(dom = 't', autoWidth = TRUE, 
											 			   columnDefs = list(list(visible = FALSE, 
											 			   					   targets = pointSourceColumnDisable())
											 			   				  )
											 			   )
											 )#c(1, 2, 3))))
	
	observeEvent(input$files_point_source_Out_cell_edit, {
		#browser()
		info = input$files_point_source_Out_cell_edit
		info$col = info$col + 1
		if(info$value[1] != ""){
			dataDF = data()
			dataDF$files = editData(files(), info, "files_point_source_Out")
			data(dataDF)	
		}
	})
	
	observeEvent(input$filesOut_cell_edit, {
		#browser()
		info = input$filesOut_cell_edit
		
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$files = editData(files(), info, "files_point_source_Out")
		data(dataDF)
	})
	
	observeEvent(input$featuresOut_cell_edit, {
		#browser()
		info = input$featuresOut_cell_edit
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$features = editData(features(), info, "featuresOut")
		data(dataDF)
	})
	
	observeEvent(input$tracksOut_cell_edit, {
		#browser()
		info = input$tracksOut_cell_edit
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$tracks = editData(tracks(), info, "tracksOut")
		data(dataDF)
	})
	
	observeEvent(input$trajecoriesOut_cell_edit, {
		#browser()
		info = input$trajecoriesOut_cell_edit
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$trajectories = editData(trajectories(), info, "trajecoriesOut")
		data(dataDF)
	})
	
	output$filesOut = renderDT({files()}, filter = 'top', rownames = FALSE, editable = "cell", selection = 'single', class = "compact")
	output$featuresOut = renderDT({features()}, filter = 'top', rownames = FALSE, editable = "cell", selection = 'single', class = "compact")
	output$tracksOut = renderDT({tracks()}, filter = 'top', rownames = FALSE, editable = "cell", selection = 'single', class = "compact")
	output$trajectoriesOut = renderDT({trajectories()}, filter = 'top', rownames = FALSE, editable = "cell", selection = 'single', class = "compact")
	
	trackQQPlot = reactive({
		
	})

	trackTransform = reactive({
		# Data Transform with parameter
		return(list(method = input$track_data_transform_In, parameter = input[[paste(c("track", "data", input$track_data_transform_In, "In"), collapse = "_")]]))
	})
	
	trajFeatTransform = reactive({
		# Data Transform with parameter
		return(list(method = input$traj_feat_data_transform_In, parameter = input[[paste(c("track", "data", input$traj_feat_data_transform_In, "In"), collapse = "_")]]))
	})
	
	trackFeaturePlot = reactive({
		if(input$track_browse_In){
			browser()
		}
		print("output$trackFeatureOut = renderPlot({")
		if(input$track_x_In == "" || input$track_y_In == ""){
			"Please select both x and y axis variables."
		}else{
			titles = setTitleInputs(input$track_title_In, input$track_title_check_In, input$track_subtitle_In, input$track_subtitle_check_In)
			yUnit = input$track_unit_In; if(yUnit == ""){yUnit = NULL}
			xlab = input$track_xlab_In; if(xlab == ""){xlab = NULL}
			ylab = input$track_ylab_In; if(ylab == ""){ylab = getFeatureLab(features = features(), name = input$track_y_In)}
			
			colorGroup = input$track_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
			fillGroup = input$track_fill_In; if(fillGroup == "NULL") {fillGroup = NULL}
			#statGroup = input$track_pairwise_stat_In; if(statGroup == "NULL") {statGroup = NULL}
			replicateGroup = input$track_replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
			facetRowGroup = input$track_facet_row_In; if(facetRowGroup == "NULL") {facetRowGroup = NULL}
			facetColGroup = input$track_facet_col_In; if(facetColGroup == "NULL") {facetColGroup = NULL}

			#if(input$track_x_range_check_In){xRange = input$track_x_range_In}else{xRange = NULL}
			if(input$track_y_range_check_In){yRange = input$track_y_range_In}else{yRange = NULL}
			
			a = input$track_stat_comparison_select1_In
			b = input$track_stat_comparison_select2_In
			#browser()
			
			statPairwiseSelectedPairs = list()
			if(length(a) == length(b) && length(a) > 0){
				for(i in 1:min(c(length(a), length(b)))){
					statPairwiseSelectedPairs[[i]] = c(a[i], b[i])
				}
			}
			
			statSignSymbols = paste0(input$track_stat_label_symbols0_In, input$track_stat_label_symbols_In, input$track_stat_label_symbols1_In)
			statSignSymbols = strsplit(statSignSymbols, " < ")[[1]]
			
			symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
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
			
			plot = plotData(dataTracks = tracks(), x = input$track_x_In, y = input$track_y_In, type = input$track_type_In, 
							y.range = yRange, y.unit = yUnit, 
							colorGroupName = colorGroup, fillGroupName = fillGroup, groupings = groupings()$groupings,
							#xReverseOrder = input$track_reverse_order_In,
							facet.row = facetRowGroup, facet.col = facetColGroup, 
							title = titles$title, subtitle = titles$subtitle, replicateGroupName = replicateGroup, 
							stat.label = input$track_stat_label_In, hide.ns = input$track_stat_hidens_In, 
							#statGroupName = statGroup, 
							multiple.stat.method = input$track_multiple_stat_method_In, 
							pairwise.stat.method = input$track_pairwise_stat_method_In, 
							data.transform = trackTransform(),
							statPairwiseType = input$track_stat_comparison_type_In, 
							statPairwiseControl = input$track_stat_comparison_control_In, 
							statPairwiseSelected = statPairwiseSelectedPairs, 
							statSignSymbols = symnum.args,
							fillAlpha = input$track_fill_alpha_In, colorAlpha = input$track_color_alpha_In, 
							x.lab = xlab, y.lab = ylab, is.dark = input$track_dark_In,
							facet.text.face = input$track_facet_label_face_In, 
							facet.label.fill.color = input$track_facet_label_fill_color_In,
							facet.wrap = input$track_facet_wrap_In,
							plot.subtitle.hjust = input$track_subtitle_hjust_In, 
							plot.subtitle.size = input$track_subtitle_size_In, 
							plot.subtitle.face = input$track_subtitle_text_style_In,
							violin.scale = input$track_violin_scale_In,
							box.notch = input$track_box_notch_In,
							box.varwidth = input$track_box_varwidth_In,
							dot.binwidth = input$track_dot_binwidth_In,
							dot.stackgroups = input$track_dot_stackgroups_In,
							dot.method = input$track_dot_method_In,
							dot.stackdir = input$track_dot_stackdir_In,
							browse = input$track_browse_In, benchmark = input$track_benchmark_In, verbose = input$track_verbose_In)
			plot
		}
		
	})
	output$trackFeaturePlotOut = renderPlot({
		trackFeaturePlotOut = trackFeaturePlot()
		if(is.list(trackFeaturePlotOut)){
			trackFeaturePlotOut$plot
		}else{
			NULL
		}
		})
	output$track_stat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
		#browser()
		if("tbl" %in% class(trackFeaturePlot()$stat[[1]])){
			trackFeaturePlot()$stat
		}
		})
	output$track_stat_text_Out = renderText({
		#browser()
		if("character" %in% class(trackFeaturePlot()$stat[[1]])){
			#browser()
			statOutText = ""
			for(i in 1:length(trackFeaturePlot()$stat)){
				statName = names(trackFeaturePlot()$stat)[i]
				statOutText = paste(statOutText, statName, sep = "\n\n")
				statOutText = paste(statOutText, paste(trackFeaturePlot()$stat[[i]], collapse = "\n"), sep = "\n") #hTestToString(trackFeaturePlot()$stat)
			}
			
			statOutText
		}
		})
	output$track_data_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
		trackFeaturePlot()$replicates
	})
	output$track_data_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
		trackFeaturePlot()$tracks
	})
	
	output$track_stat_histogram_Out = renderPlot({
		trackFeaturePlotOut = trackFeaturePlot()
		if(is.list(trackFeaturePlotOut)){
			trackFeaturePlotOut$histogram
		}else{
			NULL
		}
	})
	
	output$track_stat_qq_Out = renderPlot({
		trackFeaturePlotOut = trackFeaturePlot()
		if(is.list(trackFeaturePlotOut)){
			trackFeaturePlotOut$qq
		}else{
			NULL
		}
	})
	
	output$track_stat_normality_Out = renderTable(spacing = "xs", striped = TRUE, {
		trackFeaturePlotOut = trackFeaturePlot()
		if(is.list(trackFeaturePlotOut)){
			trackFeaturePlotOut$normality
		}else{
			NULL
		}
	})
	
	output$track_stat_levene_Out = renderText({
		trackFeaturePlotOut = trackFeaturePlot()
		if(is.list(trackFeaturePlotOut)){
			trackFeaturePlotOut$levene
		}else{
			NULL
		}
	})
	
	trajectoryPlot = eventReactive(input$plotTrajIn, {#renderPlot({
	#trajectoryPlot = reactive({
	#trajectoryPlot = renderPlot({
	#observeEvent(input$plotTrajIn, {
		if(input$traj_browse_In){
			browser()
		}
		titles = setTitleInputs(input$traj_title_In, input$traj_title_check_In, input$traj_subtitle_In, input$traj_subtitle_check_In)
		xlab = input$traj_xlab_In; if(xlab == ""){xlab = NULL}
		ylab = input$traj_ylab_In; if(ylab == ""){ylab = NULL}
		
		colorGroup = input$traj_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
		startPointGroup = input$traj_start_point_In; if(startPointGroup == "NA") {startPointGroup = NA} else if(startPointGroup == "NULL") {startPointGroup = NULL}
		endPointGroup = input$traj_end_point_In; if(endPointGroup == "NA") {endPointGroup = NA} else if(endPointGroup == "NULL") {endPointGroup = NULL}
		
		replicateGroup = input$traj_replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
		facetRowGroup = input$traj_facet_row_In; if(facetRowGroup == "NULL") {facetRowGroup = NULL}
		facetColGroup = input$traj_facet_col_In; if(facetColGroup == "NULL") {facetColGroup = NULL}
		
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
		
		
		posTypeInd = which(input$traj_xy_In == unlist(positionTypes))
		xVarName = allPositionTypes[[posTypeInd]][1]; yVarName = allPositionTypes[[posTypeInd]][2]
		
		plot = plotTrajectories(data = trajectories(), x = xVarName, y = yVarName, 
						 trackGlobalIDName = "track_global_id", 
						 #colorReverseOrder = input$traj_reverse_order_In,
						 groupings = groupings()$groupings, x.unit = input$traj_x_unit_In, y.unit = input$traj_y_unit_In, 
						 colorGroupName = colorGroup, #alphaGroupName = alphaGroup, #fillGroupName = fillGroup, alphaGroupName = alphaGroup,
						 startPointGroupName = startPointGroup, endPointGroupName = endPointGroup, 
						 colorTrajectories = input$traj_color_tracks_In, 
						 coord_equal = input$traj_coord_equal_In, #inverse = input$traj_invert_y_axis_In,
						 #fill.legend = fillLegend, color.legend = colorLegend, alpha.legend = alphaLegend, 
						 inverse = input$traj_inverse_In, equalRange = input$traj_equal_range_In,
						 facet.row = facetRowGroup, facet.col = facetColGroup, facet.wrap = input$traj_facet_wrap_In,
						 title = titles$title, subtitle = titles$subtitle, 
						 replicateGroupName = replicateGroup, 
						 hide.ns = input$traj_stat_hidens_In,
						 colorAlpha = input$traj_color_alpha_In, #fillAlpha = input$traj_fill_alpha_In, 
						 is.dark = input$traj_dark_In,
						 limitNTracks = input$traj_limit_to_smallest_In,
						 #randomizeTrackSampling = input$traj_limit_to_smallest_In,
						 trackReduced = input$traj_track_reduced_In,
						 spotReduced = input$traj_spot_reduced_In,
						 plot.subtitle.hjust = input$traj_subtitle_hjust_In, 
						 plot.subtitle.size = input$traj_subtitle_size_In, 
						 plot.subtitle.face = input$traj_subtitle_text_style_In,
						 h.line = input$traj_h_line_In, v.line = input$traj_v_line_In, 
						 panel.border = input$traj_panel_border_In, 
						 panel.grid.major = input$traj_panel_grid_major_In, 
						 facet.label.fill.color = input$traj_facet_label_fill_color_In, 
						 facet.text.face = input$traj_facet_label_face_In, 
						 linesize = input$traj_line_size_In, x.lab = xlab, y.lab = ylab,
						 browse = input$traj_browse_In, benchmark = input$traj_benchmark_In, 
						 verbose = input$traj_verbose_In,
						 initializeProg = initializeProgress, updateProg = updateProgress)
		#browser()
		closeProgress()
		plot
	})
	output$trajectoryPlotOut = renderPlot({trajectoryPlot()$plot})
	
	output$traj_stat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
		if("tbl" %in% class(trajectoryPlot()$stat)){
			trajectoryPlot()$stat
		}
	})
	output$traj_stat_text_Out = renderText({
		if("character" %in% class(trajectoryPlot()$stat)){
			#browser()
			statOutText = paste(trajectoryPlot()$stat, collapse = "\n") #hTestToString(trajectoryPlot()$stat)
			statOutText
		}
	})
	output$traj_data_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
		trajectoryPlot()$replicates
	})
	output$traj_data_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
		trajectoryPlot()$tracks
	})
	
	directionalityPlot = reactive({
		#browser
		if(!is.list(groupings()) || length(groupings()) == 0){
			return(NULL)
		}
		if(input$dir_browse_In){
			browser()
		}
		titles = setTitleInputs(input$dir_title_In, input$dir_title_check_In, input$dir_subtitle_In, input$dir_subtitle_check_In)
		xlab = input$dir_xlab_In; if(xlab == ""){xlab = NULL}
		ylab = input$dir_ylab_In; if(ylab == ""){ylab = NULL}
		
		colorGroup = input$dir_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
		fillGroup = input$dir_fill_In; if(fillGroup == "NULL") {fillGroup = NULL}
		
		replicateGroup = input$dir_replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
		facetRowGroup = input$dir_facet_row_In; if(facetRowGroup == "NULL") {facetRowGroup = NULL}
		facetColGroup = input$dir_facet_col_In; if(facetColGroup == "NULL") {facetColGroup = NULL}
		
		start.angle = ud.convert(input$dir_start_angle_In, "degree", "radian"); start.angle[is.nan(start.angle)] = 0
		statMeasure = input$dir_multisample_measure_In
		statMethod = NULL; statExtras = NULL
		if(statMeasure != "NULL"){
			statMethod = input[[paste("dir_multisample", statMeasure, "method_In", sep = "_")]]
			if(statMethod == "watson.two.test"){
				statExtras = input$dir_stat_watsontwotest_comparison_type_In
				if(statExtras == "to_control"){
					statExtras = c(statExtras, input$dir_stat_watsontwotest_comparison_control_In)
				}else{
					statExtras = c(statExtras, "")
				}
			}
		}else{
			statMeasure = NULL
		}
		
		#browser()
		
		groupsCardinal = unlist(groupingsToNamedList(groupings()$groupings, empty = FALSE), use.names = F)
		plot = plotRadar(dataTracks = tracks(), groups = groupsCardinal, #directionGroupName = input$dir_track_direction_In, 
						 directionCatGroupName = input$dir_track_direction_cat_In, 
						 #colorReverseOrder = input$dir_reverse_order_In,
						 cumulativeGroupName = input$dir_cumulation_In, type = input$dir_type_In, 
						 summary.fun.name = input$dir_summary_fun_In, 
						 percentage = input$dir_summary_fun_length_percentage_In,
						 groupings = groupings()$groupings, start.angle = start.angle, 
						 stat.method = statMethod, stat.measure = statMeasure, stat.extras = statExtras,
						 colorGroupName = colorGroup, fillGroupName = fillGroup, #alphaGroupName = alphaGroup,
						 replicateGroupName = replicateGroup,
						 replicate.summary.fun.name =  input$dir_replicate_summary_fun_In,
						 facet.row = facetRowGroup, facet.col = facetColGroup, 
						 title = titles$title, subtitle = titles$subtitle, 
						 fillAlpha = input$dir_fill_alpha_In, colorAlpha = input$dir_color_alpha_In, 
						 is.dark = input$dir_dark_In, line.size = input$dir_line_size_In,
						 facet.label.fill.color = input$dir_facet_label_fill_color_In, 
						 facet.text.face = input$dir_facet_label_face_In, 
						 facet.wrap = input$dir_facet_wrap_In,
						 show.y.axis = input$dir_show_y_axis_In, 
						 plot.subtitle.hjust = input$dir_subtitle_hjust_In, 
						 plot.subtitle.size = input$dir_subtitle_size_In, 
						 plot.subtitle.face = input$dir_subtitle_text_style_In, 
						 browse = input$dir_browse_In, benchmark = input$dir_benchmark_In, 
						 verbose = input$dir_verbose_In, skip.radar = input$dir_skip_radar_In, 
						 skip.degrees = input$dir_skip_degrees_In
		)
		#browser()
		plot
	})
	output$directionalityPlotOut = renderPlot({
		#browser()
		#print(directionalityPlot())
		plot = directionalityPlot()$plot
		
		plot
	})
	
	output$dir_circstat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
		if("tbl" %in% class(directionalityPlot()$stat)){
			directionalityPlot()$stat
		}
	})
	output$dir_circstat_text_Out = renderText({
		#browser()
		if("list" %in% class(directionalityPlot()$stat)){
			statOutText = ""
			for(i in 1:length(directionalityPlot()$stat)){
				statName = names(directionalityPlot()$stat)[i]
				statOutText = paste(statOutText, statName, sep = "\n\n")
				statOutText = paste(statOutText, paste(directionalityPlot()$stat[[i]], collapse = "\n"), sep = "\n") #hTestToString(trackFeaturePlot()$stat)
			}
			
			statOutText
		}else if("character" %in% class(directionalityPlot()$stat)){
			#browser()
			statOutText = paste(directionalityPlot()$stat, collapse = "\n\n") #hTestToString(trajectoryPlot()$stat)
			statOutText
		}
		
	})
	
	output$dir_stat_shape_Out = renderTable(spacing = "xs", striped = TRUE, {
		#browser()
		if("tbl" %in% class(directionalityPlot()$summaryStats)){
			directionalityPlot()$summaryStats
		}
	})
	
	output$dir_stat_uniformity_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
		#browser()
		if("tbl" %in% class(directionalityPlot()$uniformityDF)){
			directionalityPlot()$uniformityDF
		}
	})
	
	output$dir_stat_uniformity_text_Out = renderText({
		#browser()
		if(is.list(directionalityPlot())){
			directionalityPlot()$uniformityText
		}
	})
	
	output$dir_stat_vonMisesFit_Out = renderText({
		#browser()
		
		if(is.list(directionalityPlot())){
			directionalityPlot()$vonMisesFit
		}
	})
	
	output$dir_circdata_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
		directionalityPlot()$replicates
	})
	output$dir_circdata_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
		directionalityPlot()$tracks
	})
	
	
	dir_circstat_histogram_qqplot_Height = function() {
		directionalityHistPlotOut = directionalityPlot()
		browser()
		if(is.list(directionalityHistPlotOut)){
			return(circHistQQCellSize * 2 * directionalityHistPlotOut$circDataModel$nGroups)
		}else{
			return(circHistQQCellSize)
		}
		
	}
	
	output$dir_circstat_histogram_qqplot_Out = renderUI({
		#browser()
		cat(paste(c("plotsize:", circHistQQCellSize, circHistQQCellSize * 0.5 * length(dirHistogramData())), collapse="\t")); cat("\n")
		plotOutput("dir_circstat_histogram_qqplot_Contents", width = circHistQQCellSize, height = circHistQQCellSize * 0.5 * length(dirHistogramData()))
	})
	
	dirHistogramData = reactive({
		directionalityHistPlotOut = directionalityPlot()
		
		if(is.list(directionalityHistPlotOut)){
			directionalityHistPlotOut$circDataModel
		}else{
			NULL
		}
	})
	output$dir_circstat_histogram_qqplot_Contents = renderPlot({
		#browser()
		
		histogramData = dirHistogramData()
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
	# output$dir_circstat_histogram_qqplot_Out = renderPlot({
	# 	histogramData = dirHistogramData()
	# 	if(!is.null(histogramData)){
	# 		nGroups = length(histogramData)
	# 		par(mfrow=c(nGroups * 2, 1))
	# 		titles = names(histogramData)
	# 		for(title in titles){
	# 			circdat = histogramData[[title]]
	# 			circularHistogram(circdat, title = title)
	# 			vMQQ(circdat, title = title)
	# 		}
	# 	}
	# })
	
	
	
	trajFeaturePlot = eventReactive(input$plotTrajFeatIn, {#trajFeaturePlot = reactive({
		
		print("output$trajFeatureOut = renderPlot({")
		if(input$traj_feat_browse_In){
			browser()
		}
		titles = setTitleInputs(input$traj_feat_title_In, input$traj_feat_title_check_In, input$traj_feat_subtitle_In, input$traj_feat_subtitle_check_In)
		xlab = input$traj_feat_xlab_In; if(xlab == ""){xlab = getFeatureLab(features = features(), name = input$traj_feat_x_In)}
		ylab = input$traj_feat_ylab_In; if(ylab == ""){ylab = getFeatureLab(features = features(), name = input$traj_feat_y_In)}
		
		if(input$traj_feat_y_range_check_In){yRange = input$traj_feat_y_range_In}else{yRange = NULL}
		
		colorGroup = input$traj_feat_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
		lineTypeGroup = input$traj_feat_linetype_In; if(lineTypeGroup == "NULL") {lineTypeGroup = NULL}
		sizeVar = input$traj_feat_linetype_In; if(sizeVar == "NULL") {sizeVar = NULL}
		shapeGroup = input$traj_feat_linetype_In; if(shapeGroup == "NULL") {shapeGroup = NULL}
		colorGroup = input$traj_feat_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
		fillGroup = input$traj_feat_fill_In; if(fillGroup == "NULL") {fillGroup = NULL}
		
		replicateGroup = input$traj_feat_replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
		facetRowGroup = input$traj_feat_facet_row_In; if(facetRowGroup == "NULL") {facetRowGroup = NULL}
		facetColGroup = input$traj_feat_facet_col_In; if(facetColGroup == "NULL") {facetColGroup = NULL}
		
		if(input$traj_feat_disp_fun_In == "NULL"){dispersion.fun = NULL}else{dispersion.fun = match.fun(input$traj_feat_disp_fun_In)}
		if(input$traj_feat_aggr_fun_In == "NULL"){aggregate.fun = NULL}else{aggregate.fun = match.fun(input$traj_feat_aggr_fun_In)}
		if(input$traj_feat_disp_type_In == "NULL"){dispersion.fun.type = NULL}else{dispersion.fun.type = match.fun(paste0("geom_", input$traj_feat_disp_type_In))}
		
		if(input$traj_feat_smooth_In){smoothWindow = input$traj_feat_smooth_windowIn}else{smoothWindow = 1}
		plot = plotTrajFeatures(dataTraj = trajectories(), x = input$traj_feat_x_In, y = input$traj_feat_y_In, 
								type = input$traj_feat_type_In, trackGlobalIDName = "track_global_id",
								x.unit = input$traj_feat_x_unit_In, y.unit = input$traj_feat_y_unit_In, y.Range = yRange,
								colorGroupName = colorGroup, fillGroupName = fillGroup, sizeVarName = sizeVar,
								lineTypeGroupName = lineTypeGroup, shapeGroupName = shapeGroup,
								groupings = groupings()$groupings, #colorReverseOrder = input$traj_feat_reverse_order_In, 
								facet.row = facetRowGroup, facet.col = facetColGroup, 
								facet.wrap = input$traj_feat_facet_wrap_In,
								title = titles$title, subtitle = titles$subtitle, replicateGroupName = replicateGroup, 
								#groupTracks = input$traj_feat_group_tracks_In, 
								aggregate.fun = aggregate.fun, 
								dispersion.fun = dispersion.fun, dispersion.type = dispersion.fun.type,
								#stat.label = input$traj_feat_stat_label_In, hide.ns = input$traj_feat_stat_hidens_In, 
								#statGroupName = statGroup, stat.method = input$traj_feat_stat_method_In, 
								#statPairwise = input$traj_feat_stat_pairwise_In, 
								smooth.window = smoothWindow,
								dispAlpha = input$traj_feat_disp_alpha_In, colorAlpha = input$traj_feat_color_alpha_In, 
								x.lab = xlab, y.lab = ylab, is.dark = input$traj_feat_dark_In,
								facet.text.face = input$traj_feat_facet_label_face_In, 
								facet.label.fill.color = input$traj_feat_facet_label_fill_color_In,
								plot.subtitle.hjust = input$traj_feat_subtitle_hjust_In, 
								plot.subtitle.size = input$traj_feat_subtitle_size_In, 
								plot.subtitle.face = input$traj_feat_subtitle_text_style_In, 
								linesize = input$traj_feat_line_size_In,
								pointsize = input$traj_feat_point_size_In,
								browse = input$traj_feat_browse_In, benchmark = input$traj_feat_benchmark_In, 
								verbose = input$traj_feat_verbose_In)
		plot	
	})
	output$trajFeaturePlotOut = renderPlot({trajFeaturePlot()$plot})
	
	output$traj_feat_stat_DF_Out = renderTable(spacing = "xs", striped = TRUE, {
		if("tbl" %in% class(trajFeaturePlot()$stat)){
			trajFeaturePlot()$stat
		}
	})
	output$traj_feat_stat_text_Out = renderText({
		if("character" %in% class(trajFeaturePlot()$stat)){
			statOutText = paste(trajFeaturePlot()$stat, collapse = "\n") #hTestToString(trajectoryPlot()$stat)
			statOutText
		}
	})
	output$traj_feat_data_replicates_Out = renderTable(spacing = "xs", striped = TRUE, {
		trajFeaturePlot()$replicates
	})
	output$traj_feat_data_tracks_Out = renderTable(spacing = "xs", striped = TRUE, {
		trajFeaturePlot()$tracks
	})
	
	observeEvent(input$new_track_feat_btn_In, {
		#plyr::rbind.fill?
		if(input$new_track_feat_debug_In){
			browser()
		}
		
		feat = input$new_track_feat_In
		
		# The feature identifier needs to be compliant with variable names
		if(isSuitableVarName(feat)){
			featName = input$new_track_feat_name_In
			featShortName = input$new_track_feat_shortname_In
			featDimension = input$new_track_feat_dimension_In
			featType = "Track"
			formula = paste(input$new_track_feat_formula_In, collapse = " ")
			
			if(!isEmpty(featName) && !isEmpty(featShortName) && !isEmpty(featDimension) && !isEmpty(featType) && !isEmpty(formula)){
				data = data()
				tracks = data()$tracks
				
				tracks = tryCatch({
					tracks %>% group_by(track_global_id) %>% mutate(!!feat := !!parse_expr(formula))
				}, error = function(e){
					print(e)
					NULL
				})
				if(!is.null(tracks)){
					# Adding feature definition to the features df
					feats = features()
					feats = appendNewFeatures(feats, feat, featName, featShortName, featDimension, FALSE, featType, 
									  groupings()$groups, groupings()$groupings$names)
					# groupsDF = groupings()$groups
					# newFeatDF = data.frame(feature = feat,  name = featName, shortname = featShortName, dimension = featDimension, isint = FALSE, type = featType)
					# newFeatDF = cbind(newFeatDF, groupsDF)
					# newFeatDF$group_id = apply(newFeatDF[, as.character(groupings()$groupings$names), drop = F], 1, paste, collapse = "_")
					# feats = plyr::rbind.fill(feats, newFeatDF)
					
					data$tracks = tracks
					data$features = feats
					
					data(data)
				}else{
					#TODO report error
				}
			}
		}
		
	})
	
	observeEvent(input$new_traj_feat_btn_In, {
		#plyr::rbind.fill?
		if(input$new_traj_feat_debug_In){
			browser()
		}
		
		feat = input$new_traj_feat_In
		
		
		if(isSuitableVarName(feat)){
			featName = input$new_traj_feat_name_In
			featShortName = input$new_traj_feat_shortname_In
			featDimension = input$new_traj_feat_dimension_In
			featType = input$new_traj_feat_type_In
			formula = paste(input$new_traj_feat_formula_In, collapse = " ")
			
			if(!isEmpty(featName) && !isEmpty(featShortName) && !isEmpty(featDimension) && !isEmpty(featType) && !isEmpty(formula)){
				data = data()
				trajectories = trajectories()
				
				trajectories = tryCatch({
					trajectories %>% select_at(vars(-one_of(feat))) %>% group_by(track_global_id) %>% mutate(!!feat := !!parse_expr(formula))
				}, error = function(e){
					print(e)
					NULL
				})
				
				if(!is.null(trajectories)){
					feats = features()
					feats = appendNewFeatures(feats, feat, featName, featShortName, featDimension, FALSE, featType, 
											  groupings()$groups, groupings()$groupings$names)
					
					# groupsDF = groupings()$groups
					# newFeatDF = data.frame(feature = feat,  name = featName, shortname = featShortName, dimension = featDimension, isint = FALSE, type = featType)
					# newFeatDF = cbind(newFeatDF, groupsDF)
					# newFeatDF$group_id = apply(newFeatDF[, as.character(groupings()$groupings$names), drop = F], 1, paste, collapse = "_")
					# feats = plyr::rbind.fill(feats, newFeatDF)
					
					data$trajectories = trajectories
					data$features = feats
					
					data(data)
				}else{
					#TODO error report
				}
			}
		}
		
	})
	
	observeEvent(input$new_track_from_traj_feat_btn_In, {
		#plyr::rbind.fill?
		if(input$new_track_from_traj_feat_debug_In){
			browser()
		}
		
		feat = input$new_track_from_traj_feat_In
		
		
		if(isSuitableVarName(feat)){
			featName = input$new_track_from_traj_feat_name_In
			featShortName = input$new_track_from_traj_feat_shortname_In
			featDimension = input$new_track_from_traj_feat_dimension_In
			featType = input$new_track_from_traj_feat_type_In
			formula = paste(input$new_track_from_traj_feat_formula_In, collapse = " ")
			
			if(!isEmpty(featName) && !isEmpty(featShortName) && !isEmpty(featDimension) && !isEmpty(featType) && !isEmpty(formula)){
				data = data()
				tracks = tracks()
				
				tracks = tryCatch({
					tracks %>% select_at(vars(-one_of(feat))) %>% 
						left_join(trajectories() %>% group_by(track_global_id) %>% summarise(!!feat := !!parse_expr(formula)), by = "track_global_id")
					
				}, error = function(e){
					print(e)
					NULL
				})
				
				if(!is.null(tracks)){
					feats = features()
					feats = appendNewFeatures(feats, feat, featName, featShortName, featDimension, FALSE, featType, 
											  groupings()$groups, groupings()$groupings$names)
					
					# groupsDF = groupings()$groups
					# newFeatDF = data.frame(feature = feat,  name = featName, shortname = featShortName, dimension = featDimension, isint = FALSE, type = featType)
					# newFeatDF = cbind(newFeatDF, groupsDF)
					# newFeatDF$group_id = apply(newFeatDF[, as.character(groupings()$groupings$names), drop = F], 1, paste, collapse = "_")
					# feats = plyr::rbind.fill(feats, newFeatDF)
					
					data$tracks = tracks
					data$features = feats
					
					data(data)
				}else{
					#TODO error report
				}
			}
		}
		
	})
	
	output$sessionOut = downloadHandler(
		
		# This function returns a string which tells the client
		# browser what name to use when saving the file.
		filename = function() {
			"session.tmx"
		},
		
		# This function should write data to a file given to it by
		# the argument 'file'.
		content = function(file) {
			dataObj = list()
			dataObj$version = dataModelVersion
			dataObj$groupings = groupings()
			dataObj$data = data()
			#browser()
			# Write to a file specified by the 'file' argument
			dataObjSerial = serialize(dataObj, NULL)
			writeBin(object = dataObjSerial, con = file)
		}
	)

	output$downloadFilesTable = downloadHandler(
		filename = function() {
			paste("files", "csv", sep = ".")
		},
		content = function(file) {
			write.csv(x = files(), file = file, append = FALSE, quote = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
		},
		contentType = paste("text", "csv", sep = "/")
	)
	output$downloadFeaturesTable = downloadHandler(
		filename = function() {
			paste("features", "csv", sep = ".")
		},
		content = function(file) {
			write.csv(x = features(), file = file, append = FALSE, quote = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
		},
		contentType = paste("text", "csv", sep = "/")
	)
	output$downloadTracksTable = downloadHandler(
		filename = function() {
			paste("tracks", "csv", sep = ".")
		},
		content = function(file) {
			write.csv(x = tracks(), file = file, append = FALSE, quote = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
		},
		contentType = paste("text", "csv", sep = "/")
	)
	output$downloadTrajectoriesTable = downloadHandler(
		filename = function() {
			paste("trajectories", "csv", sep = ".")
		},
		content = function(file) {
			write.csv(x = trajectories(), file = file, append = FALSE, quote = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
		},
		contentType = paste("text", "csv", sep = "/")
	)
	output$track_download_SVG_In = downloadHandler(
		filename = function() {
			paste("Track Features", "svg", sep = ".")
		},
		content = function(file) {
			print("Downloading track feature plot in SVG.")
			width = input$track_width_In; height = input$track_height_In
			if(input$track_auto_width_In){width = NA}
			if(input$track_auto_height_In){height = NA}
			ggsave(filename = file, plot = trackFeaturePlot()$plot, width = width, height = height, dpi = 300, units = "cm", fix_text_size = FALSE)
		},
		contentType = paste("image", "svg", sep = "/")
	)
	output$track_download_PNG_In = downloadHandler(
		filename = function() {
			paste("Track Features", "png", sep = ".")
		},
		content = function(file) {
			print("Downloading track feature plot in PNG.")
			width = input$track_width_In; height = input$track_height_In
			if(input$track_auto_width_In){width = NA}
			if(input$track_auto_height_In){height = NA}
			ggsave(filename = file, plot = trackFeaturePlot()$plot, width = width, height = height, dpi = 300, units = "cm")
		},
		contentType = paste("image", "png", sep = "/")
	)
	output$traj_download_SVG_In = downloadHandler(
		filename = function() {
			paste("Trajectories", "svg", sep = ".")
		},
		content = function(file) {
			print("Downloading trajectories plot in SVG.")
			width = input$traj_width_In; height = input$traj_height_In
			if(input$traj_auto_width_In){width = NA}
			if(input$traj_auto_height_In){height = NA}
			ggsave(filename = file, plot = trajectoryPlot()$plot, width = width, height = height, dpi = 300, units = "cm", fix_text_size = FALSE)
		},
		contentType = paste("image", "svg", sep = "/")
	)
	output$traj_download_PNG_In = downloadHandler(
		filename = function() {
			paste("Trajectories", "png", sep = ".")
		},
		content = function(file) {
			print("Downloading trajectories plot in PNG.")
			width = input$traj_width_In; height = input$traj_height_In
			if(input$traj_auto_width_In){width = NA}
			if(input$traj_auto_height_In){height = NA}
			ggsave(filename = file, plot = trajectoryPlot()$plot, width = width, height = height, dpi = 300, units = "cm")
		},
		contentType = paste("image", "png", sep = "/")
	)
	output$dir_download_SVG_In = downloadHandler(
		filename = function() {
			paste("Directionality", "svg", sep = ".")
		},
		content = function(file) {
			print("Downloading directionality plot in SVG.")
			width = input$dir_width_In; height = input$dir_height_In
			if(input$dir_auto_width_In){width = NA}
			if(input$dir_auto_height_In){height = NA}
			ggsave(filename = file, plot = directionalityPlot()$plot, width = width, height = height, dpi = 300, units = "cm", fix_text_size = FALSE)
		},
		contentType = paste("image", "svg", sep = "/")
	)
	output$dir_download_PNG_In = downloadHandler(
		filename = function() {
			paste("Directionality", "png", sep = ".")
		},
		content = function(file) {
			print("Downloading directionality plot in PNG.")
			width = input$dir_width_In; height = input$dir_height_In
			if(input$dir_auto_width_In){width = NA}
			if(input$dir_auto_height_In){height = NA}
			ggsave(filename = file, plot = directionalityPlot()$plot, width = width, height = height, dpi = 300, units = "cm")
		},
		contentType = paste("image", "png", sep = "/")
	)
	output$traj_feat_download_SVG_In = downloadHandler(
		filename = function() {
			paste("Trajectory Features", "svg", sep = ".")
		},
		content = function(file) {
			print("Downloading trajectory features plot in SVG.")
			width = input$traj_feat_width_In; height = input$traj_feat_height_In
			if(input$traj_feat_auto_width_In){width = NA}
			if(input$traj_feat_auto_height_In){height = NA}
			ggsave(filename = file, plot = trajFeaturePlot()$plot, width = width, height = height, dpi = 300, units = "cm", fix_text_size = FALSE)
		},
		contentType = paste("image", "svg", sep = "/")
	)
	output$traj_feat_download_PNG_In = downloadHandler(
		filename = function() {
			paste("Trajectory Features", "png", sep = ".")
		},
		content = function(file) {
			print("Downloading trajectory features plot in PNG.")
			width = input$traj_feat_width_In; height = input$traj_feat_height_In
			if(input$traj_feat_auto_width_In){width = NA}
			if(input$traj_feat_auto_height_In){height = NA}
			ggsave(filename = file, plot = trajFeaturePlot()$plot, width = width, height = height, dpi = 300, units = "cm")
		},
		contentType = paste("image", "png", sep = "/")
	)
}
shinyApp(ui = ui, server = server, enableBookmarking = "server")
