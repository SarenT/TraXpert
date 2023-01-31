subtitleHjustChoices = list(Left = 0, Middle = 0.5, Right = 1)

#' Prepares titles according to user selection
#'
#' @param title_input primary title
#' @param title_check boolean option to exclude primary title
#'
#' @return Returns the title according to user selection
#' @export
#'
#' @examples
title_check = function(title_input, title_check){
	if(title_check){
		title = NA
	}else{
		title = title_input
	}
	
	if(!is.na(title)){
		if(title == ""){
			title = NULL
		}
	}
	return(title)
}

titles_UI = function(id, textFaceChoices){
	ns = NS(id)
	bsCollapsePanel("Titles", 
					textInput(ns("title"), "Main Title"),
					checkboxInput(ns("title_check"), "Disable Main Title", value = FALSE),
					textInput(ns("subtitle"), "Subtitle", placeholder = "Leave empty for automatic subtitle"),
					checkboxInput(ns("subtitle_check"), "Disable Subtitle", value = FALSE),
					selectInput(ns("subtitle_hjust"), "Subtitle Alignment", choices = subtitleHjustChoices, 
								selected = 0.5),	
					sliderInput(ns("subtitle_size"), "Subtitle Size", value = 10, min = 1, max = 100, step = 1),
					selectInput(ns("subtitle_text_style"), "Subtitle Text Style", choices = textFaceChoices),
					
					bsTooltip(ns("subtitle"), toolTips$subtitle_In, placement = "bottom", trigger = "hover"),
					bsTooltip(ns("subtitle_check"), toolTips$subtitle_check_In, placement = "bottom", 
							  trigger = "hover"),
					bsTooltip(ns("subtitle_hjust"), toolTips$subtitle_hjust_In, placement = "bottom", 
							  trigger = "hover"),
					bsTooltip(ns("subtitle_size"), toolTips$subtitle_size_In, placement = "bottom", trigger = "hover"),
					bsTooltip(ns("subtitle_text_style"), toolTips$subtitle_text_style_In, placement = "bottom", 
							  trigger = "hover")
	)
	
}

titles_server = function(id){
	moduleServer(id, function(input, output, session){
		title = reactive({
			return(title_check(input$title, input$title_check))
		})
		
		subtitle = reactive({
			return(title_check(input$subtitle, input$subtitle_check))
		})
		
		return(list(
			title = title,
			subtitle = subtitle,
			subtitle_hjust = reactive({input$subtitle_hjust}),
			subtitle_size = reactive({input$subtitle_size}),
			subtitle_text_style = reactive({input$subtitle_text_style})
		))
	})
}