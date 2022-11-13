debugging_UI = function(id, extras = list()){
	ns = NS(id)
	debug_controls = 
		tagList(h3("Debugging"),
				checkboxInput(ns("browse"), "Debug", value = FALSE),
				checkboxInput(ns("benchmark"), "Benchmark", value = FALSE),
				checkboxInput(ns("verbose"), "Verbose", value = FALSE),
				textOutput(ns("test")))
	
	extra_controls = Map(function(name, item){
		checkboxInput(ns(name), item$label, value = item$value)
	}, names(extras), extras)
	
	bsCollapsePanel("Debugging", c(debug_controls, extra_controls))
}

debugging_server = function(id){
	moduleServer(id, function(input, output, session){
		return(input)
	})
}