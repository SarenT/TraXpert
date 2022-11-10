export_size_UI = function(id){
	ns = NS(id)
	tagList(
		fluidPage(fluidRow(
			column(6, checkboxInput(ns("auto_width"), label = "Auto width", value = TRUE)),
			column(6, conditionalPanel("!input.auto_width", ns = ns,
									   numericInput(ns("width"), label = "Plot width [cm]", min = 1, step = 1, 
									   			 max = 100, value = 10)))
		),
		fluidRow(
			column(6, checkboxInput(ns("auto_height"), label = "Auto height", value = TRUE)),
			column(6, conditionalPanel("!input.auto_height", ns = ns,
									   numericInput(ns("height"), label = "Plot height [cm]", min = 1, step = 1, 
									   			 max = 100, value = 10)))
		)),
		tool_tip(ns("auto_width"), 
				  "Whether or not output image width (both for SVG and PNG) needs to be automatic or not."),
		tool_tip(ns("width"), "If not automatic, image width in cm at 300dpi."),
		tool_tip(ns("auto_height"), 
				  "Whether or not output image height (both for SVG and PNG) needs to be automatic or not."), 
		tool_tip(ns("height"), "If not automatic, image height in cm at 300dpi.")
	)
}

export_size_server = function(id){
	moduleServer(id, function(input, output, session){
		auto = reactive({
			list(width = input$auto_width,
				 height = input$auto_height)
			})
		
		size = reactive({
			width = input$width; height = input$height
			if(input$auto_width){width = NA}
			if(input$auto_height){height = NA}
			return(list(width = width, height = height))
		})
		
		return(list(auto = auto,
					size = size))
	})
}