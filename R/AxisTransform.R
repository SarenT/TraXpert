axis_transform_UI = function(id, label, col_width = 12){
	ns = NS(id)
	
	column(col_width, tipify(
		selectInput(ns("method"), paste0("Transform axis scale (", label, ") with"), 
					choices = scaleTransformations, selected = "noneTransform"),
		"Transforms the scale for display only. This does not affect statistics and other calculations.",
		"top", "hover"))
}

axis_transform_server = function(id){
	moduleServer(id, function(input, output, session){
		method = reactive({
			input$method
		})
		return(list(
			method = method
		))
	})
}
