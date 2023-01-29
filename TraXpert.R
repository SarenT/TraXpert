#rm(list = ls())
# Line below should run before other packages are loaded. See link for the reason:
# https://stackoverflow.com/questions/27153974/how-to-fix-outofmemoryerror-java-gc-overhead-limit-exceeded-in-r

if(version$major < 4){
	cat("R version 4 is required.\nAborting...\n")
	stopifnot(FALSE)
}

options(java.parameters = "-Xmx4096m")

libs = c("methods", "xml2", "ggplot2", "magrittr", "dplyr", "ggpubr", "RColorBrewer", "udunits2", "sticky", "shiny",
		 "sortable", "colourpicker", "DT", "svglite", "readxl", "colorspace", "smoother", "shinyBS", "car", "latex2exp")

only.install = c("shinyjs", "htmlwidgets", "shinyWidgets")

libs.func = c("dplyr", "tidyr", "purrr", "rlang", "viridisLite", "e1071", "circular")
only.install.func = c("stringr", "ggiraphExtra", "Rmisc", "tools")

all.required.libs = c(libs, only.install, libs.func, only.install.func)
still.needed.libs = all.required.libs[!(all.required.libs %in% row.names(installed.packages()))]

while(length(still.needed.libs)){
	cat("Following packages are still missing: ", paste(still.needed.libs, collapse = ", "), "\n")
	answer = readline(prompt="Do you want to automatically install these packages now? [(y)es, (n)o]: ")
	if(answer == "y"){
		install.packages(still.needed.libs)

		still.needed.libs = all.required.libs[!(all.required.libs %in% row.names(installed.packages()))]
	}else if(answer == "n"){
		stop("Exiting...")
	}else{
		cat("Answer (", answer, ") not recognized only \"y\" for yes or \"n\" for no are allowed.\nExiting...\n")
	}
}

# Install missing packages if there is any
#install.packages(only.install.func[!is.element(only.install.func, installed.packages())])
#install.packages(libs.func[!is.element(libs.func, installed.packages())])

lapply(libs.func, require, character.only = TRUE)

# Install missing packages if there is any
#install.packages(only.install[!is.element(only.install, installed.packages())], )
#install.packages(libs[!is.element(libs, installed.packages())])


lapply(libs, require, character.only = TRUE)


options(shiny.port = 7777, shiny.maxRequestSize=1024*1024^2, scipen = 10, browser = "firefox")


# defaultWD = "~/"
# if(dir.exists(defaultWD)){
# 	setwd(defaultWD)	
# }

source("functions.R")

try(detach("package:plyr", unload=TRUE), silent = TRUE)

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
			 	sidebarPanel(#h3("TraXpert"),
			 				 #hr(),
			 				 p("TraXpert is a tool to automate analysis of tracking files to create plots and simple statistics. TraXpert is smart enough to understand basic file naming convention and the source of files, where each tracking file has ", 
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

tabPanelOperations = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(fluidRow( 
			 	column(3, rotation_UI("rotation")),
			 	column(9, point_source_UI("point_source"))
			 	)),
			 fluidPage(fluidRow( 
			 	feature_calculator_UI("track_new_feat", "New Track Feature", 
			 						  "Calculate a new track feature based on an existing feature. With this, you can calculate new measures by entering the formula.",
			 						  # "track", 
			 						  featureDimensionChoices, list(`Track` = "Track")),
			 	feature_calculator_UI("traj_new_feat", "New Trajectory (Spot/Edge) Feature", 
			 						  "Calculate a new spot or edge feature based on an existing feature. With this, you can calculate new measures by entering the formula.",
			 						  # "traj", 
			 						  featureDimensionChoices, list(`Spot` = "Spot", `Edge` = "Edge")),
			 # )),
			 # fluidPage(fluidRow(
			 	feature_calculator_UI("track_from_traj_new_feat", "New Track Feature from Trajectories", 
			 						  "Calculate a new track feature based on existing trajectory features. This summarises trajectory information (spots and edges of a track) into a single value per track. e.g. mean track speed is the average of all speeds between spots of a track.",
			 						  # "track_from_traj", 
			 						  featureDimensionChoices, list(`Track` = "Track"))
			 ))
			 		   
	)
}

tabPanelPlotTrajectoryFeatures = function(title, tabColor){
	tabPanel(title,
			 tags$style(HTML(tabBGColorCSS(title, tabColor))),
			 fluidPage(
			 	fluidRow(
			 		column(4,
			 			   bsCollapse(id = "traj_feat_settings",
			 			   		   titles_UI("traj_feat_title", textFaceChoices),
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
			 			   		   facet_control_UI("traj_feat_facet", textFaceChoices),
			 			   		   bsCollapsePanel("Ranges, Units & Labels", 
			 			   		   				fluidPage(fluidRow(
			 			   		   					column(10, tipify(sliderInput("traj_feat_y_range_In", "y Axis Range", min = 0, max = 1, step = 0.1, value = c(0, 1), dragRange = TRUE), "y axis range. Slide the knobs or the line between the knobs to set what range to be displayed. You can also select an area and double click on the plot to set the range (only y axis selection is registered).", "top")), 
			 			   		   					column(2, checkboxInput("traj_feat_y_range_check_In", "", value = TRUE))),
			 			   		   					axis_labels_UI("traj_feat_axis_labs"))
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
			 			   		   				dark_plot_UI("dark_traj_feat"),
			 			   		   				
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
			 			   		   debugging_UI("traj_feat_debug")
			 			   )
			 		),
			 		column(8, 
			 			   plotOutput(outputId = "trajFeaturePlotOut"),
			 			   stat_details_UI("traj_feat_stats"),
			 			   tags$style(type="text/css", "#traj_feat_stat_text_Out {white-space: pre-wrap;}"), hr(),
			 			   actionButton(inputId = "plotTrajFeatIn", label = "Plot Trajectory Festures"),
			 			   plot_export_UI("traj_feat_export")
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
			  title = titlePanel("TraXpert"),
		tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;} .action-button {background-color: #77FF77}
								  .navbar-default .navbar-nav > li[class=active] {font-weight: bold;}"))),
		fluidRow(
			tags$head(tags$style(HTML(".tab-content { height: 90vh; overflow-y: auto !important; }"))),
			#column(12,
			navbarPage(title = "TraXpert", id = tabsID, 
					   #tabsetPanel(type = "tabs",
						tabPanelImport(title = titleImportGroupings, tabColor = tabColorImport),
						tabPanelOperations(titleOperations, tabColorOperations),
						tabPanel(titleFiles, 
								 tags$style(HTML(tabBGColorCSS(titleFiles, tabColorTables))),
								 table_output_UI("files_table_out")),
						tabPanel(titleFeatures, 
								 tags$style(HTML(tabBGColorCSS(titleFeatures, tabColorTables))),
								 table_output_UI("features_table_out")),
						tabPanel(titleTracks, 
								 tags$style(HTML(tabBGColorCSS(titleTracks, tabColorTables))),
								 table_output_UI("tracks_table_out")),
						tabPanel(titleTrajectories, 
								 tags$style(HTML(tabBGColorCSS(titleTrajectories, tabColorTables))),
								 table_output_UI("trajectories_table_out")),
						track_features_UI("tracks", titlePlotTrackFeatures, tabColorPlots),
						trajectories_UI("trajectories", titlePlotTrajectories, tabColorPlots),
						directionality_UI("directionality", titlePlotDirectionality, tabColorPlots),
						tabPanelPlotTrajectoryFeatures(titlePlotTrajFeatures, tabColorPlots),
						about_UI("about", titleAbout)
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
	
	parseParameters = reactiveVal(NULL)
	
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
	
	observe({updateSelectInput(session, "traj_feat_x_In", choices = trajChoiceswithoutEmpty(), selected = "EDGE_TIME")})
	observe({updateSelectInput(session, "traj_feat_y_In", choices = trajChoiceswithoutEmpty(), selected = "VELOCITY")})
	observe({updateSelectInput(session, "traj_feat_fill_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_color_In", choices = groupingsChoiceswithoutEmpty())})
	observe({updateSelectInput(session, "traj_feat_shape_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_linetype_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_size_In", choices = trajChoiceswithEmpty())})
	
	observe({updateSelectInput(session, "traj_feat_stat_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_replicate_In", choices = groupingsChoiceswithEmpty())})
	observe({updateSelectInput(session, "traj_feat_disp_fun_In", choices = dispersionChoices())})
	
	#trackRanges = reactiveValues(y = NULL)
	
	#observe({updateSliderInput(session, "track_x_range_In", min = getXMin(), max = getXMax(), step = getXStep(), value = c(getXMin(), getXMax()))})
	
	
	observe({updateSliderInput(session, "traj_feat_y_range_In", min = getTrajFeatYMin(), max = getTrajFeatYMax(), 
							   step = getTrajFeatYStep(), value = c(getTrajFeatYMin(), getTrajFeatYMax()))})
	
	getTrajFeatYPretty = reactive({
		if(!is.null(tracks()) && !(input$traj_feat_y_In == "")){
			#browser()
			transformFun = match.fun(trajFeatTransform()$method)
			values = trajectories()[[input$traj_feat_y_In]]
			unit = attr(values, "unit")
			
			if(is.null(unit)){
				unit = ""
			}
			
			unitToConvert = traj_feat_axis_labs$y_unit()
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
	
	trackQQPlot = reactive({
		
	})
	
	trajFeatTransform = reactive({
		# Data Transform with parameter
		return(list(method = input$traj_feat_data_transform_In, parameter = input[[paste(c("traj_feat", "data", input$traj_feat_data_transform_In, "In"), collapse = "_")]]))
	})
	
	traj_feat_export_size = reactive({
		width = input$traj_feat_width_In; height = input$traj_feat_height_In
		if(input$traj_feat_auto_width_In){width = NA}
		if(input$traj_feat_auto_height_In){height = NA}
		return(list(width = width, height = height))
	})
	
	output$trajFeaturePlotPreviewOut = renderImage({
		#browser()
		trajFeaturePlotOut = trajFeaturePlot()
		if(is.list(trajFeaturePlotOut)){
			temp_png_file = tempfile(fileext = ".png")
			
			size = traj_feat_export_size()
			
			ggsave(temp_png_file, trajFeaturePlotOut$plot, width = size$width, height = size$height, 
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
	
	trajFeaturePlot = eventReactive(input$plotTrajFeatIn, {#trajFeaturePlot = reactive({
		
		print("output$trajFeatureOut = renderPlot({")
		if(debugging_traj_feat$browse){
			browser()
		}
		
		# Converting title reactives to list of values
		titles = lapply(traj_feat_titles, function(x){x()})
		
		xlab = traj_feat_axis_labs$x_lab()
		ylab = traj_feat_axis_labs$y_lab()
		
		if(input$traj_feat_y_range_check_In){yRange = input$traj_feat_y_range_In}else{yRange = NULL}
		
		colorGroup = input$traj_feat_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
		lineTypeGroup = input$traj_feat_linetype_In; if(lineTypeGroup == "NULL") {lineTypeGroup = NULL}
		sizeVar = input$traj_feat_linetype_In; if(sizeVar == "NULL") {sizeVar = NULL}
		shapeGroup = input$traj_feat_linetype_In; if(shapeGroup == "NULL") {shapeGroup = NULL}
		colorGroup = input$traj_feat_color_In; if(colorGroup == "NULL") {colorGroup = NULL}
		fillGroup = input$traj_feat_fill_In; if(fillGroup == "NULL") {fillGroup = NULL}
		
		replicateGroup = input$traj_feat_replicate_In; if(replicateGroup == "NULL") {replicateGroup = NULL}
		facetRowGroup = traj_feat_facet$row_group()
		facetColGroup = traj_feat_facet$col_group()
		
		if(input$traj_feat_disp_fun_In == "NULL"){dispersion.fun = NULL}else{dispersion.fun = match.fun(input$traj_feat_disp_fun_In)}
		if(input$traj_feat_aggr_fun_In == "NULL"){aggregate.fun = NULL}else{aggregate.fun = match.fun(input$traj_feat_aggr_fun_In)}
		if(input$traj_feat_disp_type_In == "NULL"){dispersion.fun.type = NULL}else{dispersion.fun.type = match.fun(paste0("geom_", input$traj_feat_disp_type_In))}
		
		if(input$traj_feat_smooth_In){smoothWindow = input$traj_feat_smooth_windowIn}else{smoothWindow = 1}
		plot = plotTrajFeatures(dataTraj = trajectories(), x = input$traj_feat_x_In, y = input$traj_feat_y_In, 
								type = input$traj_feat_type_In, trackGlobalIDName = "track_global_id",
								x.unit = traj_feat_axis_labs$x_unit(), y.unit = traj_feat_axis_labs$y_unit(), 
								y.Range = yRange,
								colorGroupName = colorGroup, fillGroupName = fillGroup, sizeVarName = sizeVar,
								lineTypeGroupName = lineTypeGroup, shapeGroupName = shapeGroup,
								groupings = groupings()$groupings, 
								facet.row = facetRowGroup, facet.col = facetColGroup, 
								facet.wrap = traj_feat_facet$wrap(),
								title = titles$title, subtitle = titles$subtitle, replicateGroupName = replicateGroup, 
								aggregate.fun = aggregate.fun, 
								dispersion.fun = dispersion.fun, dispersion.type = dispersion.fun.type,
								smooth.window = smoothWindow,
								dispAlpha = input$traj_feat_disp_alpha_In, colorAlpha = input$traj_feat_color_alpha_In, 
								x.lab = xlab, y.lab = ylab, is.dark = dark_plot_traj_feat(),
								facet.text.face = traj_feat_facet$label_face(), 
								facet.label.fill.color = traj_feat_facet$label_fill_color(),
								plot.subtitle.hjust = titles$subtitle_hjust, 
								plot.subtitle.size = titles$subtitle_size, 
								plot.subtitle.face = titles$subtitle_text_style, 
								linesize = input$traj_feat_line_size_In,
								pointsize = input$traj_feat_point_size_In,
								browse = debugging_traj_feat$browse, benchmark = debugging_traj_feat$benchmark, 
								verbose = debugging_traj_feat$verbose)
		plot	
	})
	output$trajFeaturePlotOut = renderPlot({trajFeaturePlot()$plot})
	
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
	
	trackChoiceswithoutEmpty = reactive({
		#featuresToNamedList("Track", data()$features, empty = FALSE)
		#browser()
		choicesInNamedList("Track", features(), empty = FALSE)
	})
	
	table_output_server("files_table_out", files)
	table_output_server("features_table_out", features)
	table_output_server("tracks_table_out", tracks)
	table_output_server("trajectories_table_out", trajectories)
	
	
	traj_feat_facet = facet_control_server("traj_feat_facet", groupingsChoiceswithEmpty)
	
	plot_export_server("traj_feat_export", "Trajectory Feature", trajFeaturePlot)
	
	stat_details_server("traj_feat_stats", trajFeaturePlot)
	
	rotation_server("rotation", data)
	
	feature_calculator_server("track_new_feat", allTrackMeasures, data, features, groupings, 
							  "tracks", "tracks", "TRACK_ID", "track_global_id")
	feature_calculator_server("traj_new_feat", allTrajectoryMeasures, data, features, groupings, 
							  "trajectories", "trajectories", "ID", "track_global_id")
	feature_calculator_server("track_from_traj_new_feat", allTrajectoryMeasures, data, features, groupings, 
							  "trajectories", "tracks", "ID", "track_global_id")
	
	
	debugging_traj_feat = debugging_server("traj_feat_debug")
	
	dark_plot_traj_feat = dark_plot_server("dark_traj_feat")
	
	traj_feat_titles = titles_server("traj_feat_title")
	
	traj_feat_axis_labs = axis_labels_server("traj_feat_axis_labs", features, trajectories,
											 groups = list(x = reactive({input$traj_feat_x_In}), 
											 			  y = reactive({input$traj_feat_y_In})),
											 default_labels = list(x = reactive({input$traj_feat_x_In}), 
											 					  y = reactive({input$traj_feat_y_In})))
	
	point_source = point_source_server("point_source", data)
	
	track_features_server("tracks", data, features, tracks, trajectories, groupings, 
						  groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						  groupingsChoiceswithEmptywithDoNotDisplay,
						  groupingsAndFeatureChoiceswithoutEmpty,trackChoiceswithoutEmpty, 
						  trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty)
	trajectories_server("trajectories", data, features, tracks, trajectories, groupings, 
						groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						groupingsChoiceswithEmptywithDoNotDisplay,
						groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
						trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty)
	directionality_server("directionality", data, features, tracks, trajectories, groupings, 
						  groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						  groupingsChoiceswithEmptywithDoNotDisplay,
						  groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
						  trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty)
}
shinyApp(ui = ui, server = server, enableBookmarking = "server")
