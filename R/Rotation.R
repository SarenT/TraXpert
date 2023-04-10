rotation_UI = function(id, column_size = 4){
	ns = NS(id)
	
	debug_checkbox = function(){
		if(!release){
			checkboxInput(ns("rotate_browse_In"), label = "Debug", value = FALSE)
		}else{
			""
		}
	}
	
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
			column(6, debug_checkbox()))
	)
}

rotation_server = function(id, data){
	moduleServer(id, function(input, output, session){
		observeEvent(input$rotateIn, {
			dataDF = data()
			
			data(rotateTracks(dataDF, get_progress_functions(),
							  rotation_fix = input$processRotationFixIn, rotation_z_fix = input$processZRotationFixIn, 
							  browse = !release && input$rotate_browse_In))
		})
	})
}