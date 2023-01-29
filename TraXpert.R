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
						track_features_UI("track_features", titlePlotTrackFeatures, tabColorPlots),
						trajectories_UI("trajectories", titlePlotTrajectories, tabColorPlots),
						directionality_UI("directionality", titlePlotDirectionality, tabColorPlots),
						trajectory_features_UI("trajectory_features", titlePlotTrajFeatures, tabColorPlots),
						about_UI("about", titleAbout)
					)
			#)
		),
		fluidRow(
			column(12, 
				   fluidPage(
				   	fluidRow( style="background-color:#eee; border: 1px solid #aaa; padding: 10px;border-radius:4px",
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
						div(tags$b("Invalid input. Please use \".\" as decimal and all fields are mandatory. Use 1 for voxel thickness for 2D images.", 
								   style = "color: red;")),
					
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
					cat(i, 
						as.character(groupingsDF$labels[i]),  paste("rankGroup", sprintf("%03d", i), sep = "_"), 
						"\n")
					#beepr::beep()
					rankListObs[[paste("rankGroup", sprintf("%03d", i), sep = "_")]] <<- 
						observeEvent(input[[paste("rankGroup",  sprintf("%03d", i), sep = "_")]], {
							
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
									groupingsDF$groupLabels[[i]] = 
										factor(newOrder, levels = as.character(groupingsDF$groupLabels[[i]])[ranks])
									
									groupingsDF$groups[[i]] = 
										factor(as.character(groupingsDF$groups[[i]])[ranks], 
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
	
	output$groupingsOut = 
		renderDT({groupings()$groupings}, 
				 rownames = FALSE, editable = FALSE, selection = 'single', 
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
	
	
	rotation_server("rotation", data)
	
	feature_calculator_server("track_new_feat", allTrackMeasures, data, features, groupings, 
							  "tracks", "tracks", "TRACK_ID", "track_global_id")
	feature_calculator_server("traj_new_feat", allTrajectoryMeasures, data, features, groupings, 
							  "trajectories", "trajectories", "ID", "track_global_id")
	feature_calculator_server("track_from_traj_new_feat", allTrajectoryMeasures, data, features, groupings, 
							  "trajectories", "tracks", "ID", "track_global_id")
	
	point_source = point_source_server("point_source", data)
	
	track_features_server("track_features", data, features, tracks, trajectories, groupings, 
						  groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						  groupingsChoiceswithEmptywithDoNotDisplay,
						  groupingsAndFeatureChoiceswithoutEmpty,trackChoiceswithoutEmpty, 
						  trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
						  trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices)
	trajectories_server("trajectories", data, features, tracks, trajectories, groupings, 
						groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						groupingsChoiceswithEmptywithDoNotDisplay,
						groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
						trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
						trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices)
	directionality_server("directionality", data, features, tracks, trajectories, groupings, 
						  groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						  groupingsChoiceswithEmptywithDoNotDisplay,
						  groupingsAndFeatureChoiceswithoutEmpty, trackChoiceswithoutEmpty, 
						  trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
						  trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices)
	trajectory_features_server("trajectory_features", data, features, tracks, trajectories, groupings, 
						  groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty, 
						  groupingsChoiceswithEmptywithDoNotDisplay,
						  groupingsAndFeatureChoiceswithoutEmpty,trackChoiceswithoutEmpty, 
						  trackDirectionChoiceswithoutEmpty, trackDirectionCatChoiceswithoutEmpty,
						  trajChoiceswithoutEmpty, trajChoiceswithEmpty, dispersionChoices)
}
shinyApp(ui = ui, server = server, enableBookmarking = "server")
