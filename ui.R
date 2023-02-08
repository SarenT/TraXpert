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
only.install.func = c("stringr", "ggiraphExtra", "tools")

all.required.libs = c(libs, only.install, libs.func, only.install.func)
still.needed.libs = all.required.libs[!(all.required.libs %in% row.names(installed.packages()))]
# 
# while(length(still.needed.libs)){
# 	cat("Following packages are still missing: ", paste(still.needed.libs, collapse = ", "), "\n")
# 	answer = readline(prompt="Do you want to automatically install these packages now? [(y)es, (n)o]: ")
# 	if(answer == "y"){
# 		install.packages(still.needed.libs)
# 
# 		still.needed.libs = all.required.libs[!(all.required.libs %in% row.names(installed.packages()))]
# 	}else if(answer == "n"){
# 		stop("Exiting...")
# 	}else{
# 		cat("Answer (", answer, ") not recognized only \"y\" for yes or \"n\" for no are allowed.\nExiting...\n")
# 	}
# }

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

require(shinyjs, exclude = "colourInput");require(htmlwidgets);require(shinyWidgets)
require(stringr);require(ggiraphExtra);require(tools)

require(methods)
require(xml2)
require(ggplot2)
require(magrittr);require(dplyr);require(ggpubr);require(RColorBrewer);require(udunits2);require(sticky);require(shiny)
require(sortable);require(DT);require(svglite);require(readxl);require(colorspace)
require(smoother);require(shinyBS);require(car);require(latex2exp);require(colourpicker)

require(dplyr);require(tidyr);require(purrr);require(rlang);require(viridisLite);require(e1071);require(circular)

source("lib/constants.R")
source("lib/functions.R")

try(detach("package:plyr", unload=TRUE), silent = TRUE)

cat("Checkpoint\n")

# Define UI
ui = function(request){
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
				 			column(2, uiOutput("debug_field"))
				 		)),
				 		p("OR"),
				 		fileInput(inputId = "sessionFileIn", label = "Choose session file:", multiple = FALSE, 
				 				  accept = c("text/tmx", ".tmx"), placeholder = "Uploading...", 
				 				  buttonLabel = "Choose..."),
				 		hr(),
				 		downloadButton(outputId = "sessionOut", label = "Download session file"),
				 		hr(),
				 		selectInput("sampleIn", "Sample Files", c("None", list.files("res/")), selected = "None")),
				 	mainPanel(
				 		fluidPage(
				 			fluidRow(column(12, DTOutput(outputId = "groupingsOut"))),
				 			fluidRow(
				 				column(6, tags$div(id = 'placeholder')),
				 				column(
				 					6, 
				 					p("Rank groups accordingly. Groups will appear in plots according to these ranks."),
				 					tags$div(id = 'placeholderRank'))
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
				 	feature_calculator_UI(
				 		"track_new_feat", "New Track Feature", 
				 		"Calculate a new track feature based on an existing feature. With this, you can calculate new measures by entering the formula.",
				 		# "track", 
				 		featureDimensionChoices, list(`Track` = "Track")),
				 	feature_calculator_UI(
				 		"traj_new_feat", "New Trajectory (Spot/Edge) Feature", 
				 		"Calculate a new spot or edge feature based on an existing feature. With this, you can calculate new measures by entering the formula.",
				 		# "traj", 
				 		featureDimensionChoices, list(`Spot` = "Spot", `Edge` = "Edge")),
				 	# )),
				 	# fluidPage(fluidRow(
				 	feature_calculator_UI(
				 		"track_from_traj_new_feat", "New Track Feature from Trajectories", 
				 		"Calculate a new track feature based on existing trajectory features. This summarises trajectory information (spots and edges of a track) into a single value per track. e.g. mean track speed is the average of all speeds between spots of a track.",
				 		# "track_from_traj", 
				 		featureDimensionChoices, list(`Track` = "Track"))
				 ))
				 
		)
	}
	
	fluidPage(shinyjs::useShinyjs(), shinyjs::extendShinyjs(functions = "shinyjs.init", 
		text = paste0("shinyjs.init = function(){
		  $('#tabs li a[data-value=", titleFiles ,"]').parents('li').hide(); $('#tabs li a[data-value=", 
		  titleFeatures,"]').parents('li').hide();
		  $('#tabs li a[data-value=", titleTracks ,"]').parents('li').hide(); $('#tabs li a[data-value=", 
		  titleTrajectories,"]').parents('li').hide();
		  $('#tabs li a[data-value=\"", 
		  titlePlotTrackFeatures ,"\"]').parents('li').hide(); $('#tabs li a[data-value=\"", 
		  titlePlotTrajectories,"\"]').parents('li').hide();
		  $('#tabs li a[data-value=\"", 
		  titlePlotDirectionality ,"\"]').parents('li').hide(); $('#tabs li a[data-value=\"", 
		  titlePlotTrajFeatures,"\"]').parents('li').hide();
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
