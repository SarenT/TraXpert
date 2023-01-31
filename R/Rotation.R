rotation_UI = function(id, column_size = 4){
	ns = NS(id)
	
	fluidPage(
		class = "shiny-input-panel", 
		fluidRow(
			column(12, 
				   h4("Rotate"), 
				   p("Select XY-Rotation angle (pitch) and Z-rotation angle (yaw) and click \"rotate\" button."))),
		fluidRow(
			column(12, 
				   sliderInput(ns("processRotationFixIn"), "Rotation (Pitch) Fix Angle", 
				   			min = -180, max = 180, step = 15, value = 0))),
		fluidRow(
			column(12, 
				   sliderInput(ns("processZRotationFixIn"), "Z Rotation (Yaw) Fix Angle", 
				   			min = -180, max = 180, step = 15, value = 0))),
		fluidRow(
			column(6, actionButton(ns("rotateIn"), label = "Process Files")),
			column(6, checkboxInput(ns("rotate_browse_In"), label = "Debug", value = FALSE)))
	)
}

rotation_server = function(id, data){
	moduleServer(id, function(input, output, session){
		observeEvent(input$rotateIn, {
			initializeProgress = function(max, message){
				progress <<- shiny::Progress$new(max = max)
				if(!is.null(message)){
					progress$set(message = message, value = 0)
				}else{
					progress$set(value = 0)
				}
			}
			
			# Close the progress when this reactive exits (even if there's an error)
			#on.exit({progress$close()})
			
			updateProgress = function(value, detail = NULL) {
				if(is.null(detail)){progress$set(value = value)}else{progress$set(value = value, detail = detail)}
			}
			
			closeProgress = function(){progress$close()}
			
			dataDF = data()
			
			data(rotateTracks(dataDF, updateProgress, initializeProgress, closeProgress, 
							  rotation_fix = input$processRotationFixIn, rotation_z_fix = input$processZRotationFixIn, 
							  browse = input$rotate_browse_In))
		})
	})
}