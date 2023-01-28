facet_control_UI = function(id, textFaceChoices){
	ns = NS(id)
	
	bsCollapsePanel("Facets", 
		tipify(selectInput(ns("row"), "Facet Row", choices = list()), toolTips$facet_row_In, placement = "top"),
		tipify(selectInput(ns("col"), "Facet Column", choices = list()), toolTips$facet_col_In, placement = "top"),
		selectInput(ns("label_face"), "Facet Label Style", choices = textFaceChoices),
		conditionalPanel("(input.row == \"NULL\") != (input.col == \"NULL\")", ns = ns, 
						 checkboxInput(ns("wrap"), "Wrap Facets", value = TRUE)),
		colourInput(ns("label_fill_color"), "Facet Label Background", value = "#FFFFFFFF", allowTransparent = TRUE),
		
		#bsTooltip(ns("row"), "Facets divide plots into a grid (Rows in this case). This is useful for multidimensional data e.g. when you have genotpyes, drug treatments and temperature treatment. But you are more interested in the genotypes. So you can use facets to divide plots into sections for the treatments.", placement = "bottom", trigger = "hover"),
		#bsTooltip(ns("col"), "Facets divide plots into a grid (Columns in this case). This is useful for multidimensional data e.g. when you have genotpyes, drug treatments and temperature treatment. But you are more interested in the genotypes. So you can use facets to divide plots into sections for the treatments.", placement = "bottom", trigger = "hover"),
		bsTooltip(ns("label_face"), toolTips$facet_label_face_In, placement = "top", trigger = "hover"),
		bsTooltip(ns("wrap"), toolTips$facet_wrap_In, placement = "bottom", trigger = "hover"),
		bsTooltip(ns("label_fill_color"), toolTips$facet_label_fill_color_In, placement = "bottom", trigger = "hover")
	)
}

facet_control_server = function(id, groupingsChoiceswithEmpty){
	moduleServer(id, function(input, output, session){
		observe({updateSelectInput(session, "row", choices = groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "col", choices = groupingsChoiceswithEmpty())})
		
		row_group = reactive({
			row_group = input$row; if(row_group == "NULL") {row_group = NULL}
			row_group
		})
		
		col_group = reactive({
			col_group = input$col; if(col_group == "NULL") {col_group = NULL}
			col_group
		})
		
		return(list(
			row_group = row_group,
			col_group = col_group,
			label_face = reactive({input$label_face}),
			wrap = reactive({input$wrap}),
			label_fill_color = reactive({input$label_fill_color})
		))
	})
}