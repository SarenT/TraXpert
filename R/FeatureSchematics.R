feature_schematics_UI = function(id){
	ns = NS(id)
	uiOutput(ns("schematic"))
}

feature_schematics_server = function(id, feature = reactive({NULL})){
	moduleServer(id, function(input, output, session){
		output$schematic = renderUI({
			tags$img(src = paste("images", paste(feature(), "svg", sep = "."), sep = "/"), 
					 onerror = "this.onerror=null; this.src='images/Default.svg'")
		})
	})
}
