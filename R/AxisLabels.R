axis_labels_UI = function(id,
						  settings = list(x = list(title = "x axis", unit = TRUE, tooltip = toolTips$xlab_In), 
						  				y = list(title = "y axis", unit = TRUE, tooltip = toolTips$ylab_In)),
						  label = "label", unit = "unit"){
	ns = NS(id)
	labs = Map(function(id, setting){
		column(floor(12 / length(settings)),
			   textInput(ns(paste(id, "lab", sep = "_")), paste(setting$title, label)),
			   bsTooltip(ns(paste(id, "lab", sep = "_")), setting$tooltip, placement = "bottom", trigger = "hover"))
	}, names(settings), settings)
	units = Map(function(id, setting){
		column(floor(12 / length(settings)), 
			   ifelse(setting$unit,
			   	   tagList(
			   	   	textInput(ns(paste(id, "unit", sep = "_")), paste(setting$title, unit)),
			   	   	bsTooltip(ns(paste(id, "unit", sep = "_")), toolTips$unit_In, 
			   	   			  placement = "bottom", trigger = "hover")), ""))
	}, names(settings), settings)
	
	return(tagList(fluidRow(labs), fluidRow(units)))
}

label_check = function(input, features, default_label){
	if(input == ""){
		if(is.null(default_label)){
			return(NULL)
		}else{
			return(getFeatureLab(features = features, name = default_label()))
		}
	}
	return(input)
}

axis_labels_server = function(id, features, data, groups = list(x = NULL, y = NULL), 
							  default_labels = list(x = NULL, y = NULL)){
	moduleServer(id, function(input, output, session){
		x_lab = reactive({
			return(label_check(input[["x_lab"]], features(), default_labels[["x"]]))
		})
		
		y_lab = reactive({
			# browser()
			return(label_check(input[["y_lab"]], features(), default_labels[["y"]]))
		})
		
		# Setting default unit upon x axis selection
		observe({
			# browser()
			req(groups$x)
			if(is.null(groups$x()) || groups$x() == ""){
				return()
			}
			# browser()
			updateTextInput(session = session, inputId = "x_unit", value = attr(data()[[groups$x()]], "unit"))
		})
		# Setting default unit upon y axis selection
		observe({
			req(groups$y)
			# browser()
			if(is.null(groups$y()) || groups$y() == ""){
				return()
			}
			# browser()
			updateTextInput(session = session, inputId = "y_unit", value = attr(data()[[groups$y()]], "unit"))
		})
		
		return(list(
			x_lab = x_lab,
			y_lab = y_lab,
			x_unit = reactive({input$x_unit}),
			y_unit = reactive({input$y_unit})
		))
	})
}