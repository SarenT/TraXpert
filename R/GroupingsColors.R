groupings_colors_UI = function(id, width = "100%", can_dark = FALSE){
	ns = NS(id)
	items = tagList(
		fluidPage(fluidRow(
			column(6, 
				   tipify(selectInput(ns("fill"), "Fill Color Variable", choices = list()), 
				   	   toolTips$fill_In, placement = "top")),
			column(6, 
				   tipify(selectInput(ns("color"), "Line Color Variable", choices = list()), 
				   	   toolTips$color_In, placement = "top"))
		)),
		sliderInput(ns("fill_alpha"), "Fill Transparency", min = 0, max = 1, value = 0.5, width = width),
		sliderInput(ns("color_alpha"), "Color Transparency", min = 0, max = 1, value = 1, width = width),
		bsTooltip(ns("fill_alpha"), toolTips$fill_alpha_In, placement = "bottom", trigger = "hover"),
		bsTooltip(ns("color_alpha"), toolTips$color_alpha_In, placement = "bottom", trigger = "hover")
	)
	
	if(can_dark){
		items = c(items, tagList(dark_plot_UI(ns("dark"))))
	}
	bsCollapsePanel("Groupings and Colors", items)
}

groupings_colors_server = function(id, choices){
	moduleServer(id, function(input, output, session){
		observe({updateSelectInput(session, "fill", choices = choices$groupingsChoiceswithEmpty())})
		observe({updateSelectInput(session, "color", choices = choices$groupingsChoiceswithEmpty())})
		
		color_group = reactive({
			group = input$color
			if(group == "NULL") {group = NULL}
			group
		})
		
		fill_group =  reactive({
			group = input$fill
			if(group == "NULL") {group = NULL}
			group
		})
		
		dark_input = dark_plot_server("dark")
		
		dark = reactive({
			if("dark-dark" %in% names(input)){
				return(dark_input())
			}else{
				return(NULL)
			}
		})
		
		return(
			list(color_group = color_group,
				 fill_group = fill_group,
				 fill_alpha = reactive({input$fill_alpha}),
				 color_alpha = reactive({input$color_alpha}),
				 dark = dark)
		)
	})
}