plot_type = function(ns){
	bsCollapsePanel("Plot Type and Variables",
					selectInput(ns("xy_In"), "Position", choices = list()),-
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
					
					bsTooltip(ns("color_tracks_In"), "Should trajectories also be colored? Or only start/end points?", 
							  placement = "bottom", trigger = "hover")
	)
}

fitlers = function(ns){
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
					sliderInput(ns("line_size_In"), "Line Thickness", min = 0.01, max = 100, value = 0.5, step = 0.25),
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
								   dark_plot_UI("dark_traj"),
								   checkboxInput(ns("panel_grid_major_In"), "Panel major grid", value = FALSE),
								   checkboxInput(ns("v_line_In"), "Vertical line", value = TRUE),
								   bsTooltip(ns("panel_grid_major_In"), "", placement = "bottom", trigger = "hover"),
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

track_features_UI = function(id, title, tabColor){
	ns = NS(id)
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

track_features_server = function(id, data, features, tracks, groupings, 
								 groupingsChoiceswithEmpty, groupingsChoiceswithoutEmpty){
	moduleServer(id, function(input, output, session){
		#TODO
	})
}