dark_plot_UI = function(id){
	ns = NS(id)
	tagList(
		checkboxInput(ns("dark"), "Dark Plot", value = FALSE),
		bsTooltip(ns("dark"), toolTips$dark_In, placement = "bottom", trigger = "hover")
	)
}

dark_plot_server = function(id){
	moduleServer(id, function(input, output, session){
		return(reactive({input$dark}))
	})
}