debugging_UI = function(id, extras = list()){
	ns = NS(id)
	debug_controls = 
		tagList(h3("Debugging"),
				checkboxInput(ns("verbose"), "Verbose", value = FALSE),
				checkboxInput(ns("benchmark"), "Benchmark", value = FALSE))
	
	if(!release){
		tagAppendChild(debug_controls, checkboxInput(ns("browse"), "Debug", value = FALSE))
	}
	
	extra_controls = Map(function(name, item){
		checkboxInput(ns(name), item$label, value = item$value)
	}, names(extras), extras)
	# browser()
	
	# if(release){
	# 	bsCollapsePanel("Debugging")
	# }else{
	bsCollapsePanel("Debugging", c(debug_controls, extra_controls))
	# }
}

debugging_server = function(id){
	moduleServer(id, function(input, output, session){
		return(input)
	})
}