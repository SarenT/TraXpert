data_transform_UI = function(id, label){
	ns = NS(id)
	
	fluidRow(
		column(6, selectInput(ns("method"), paste0("Transform data with (", label, ")"), 
							  choices = dataTransformChoices, selected = "noneTransform")),
		column(6, 
			   conditionalPanel(
			   	"input.method == 'logTransform'", ns = ns,
			   	shinyWidgets::sliderTextInput(ns("logTransform"), "\\(\\log_a(x) \\) ... a", 
			   								  choices = c(`2` = 2, `e` = exp(1), `10` = 10), selected = exp(1))),
			   conditionalPanel("input.method  == 'powerTransform'", ns = ns,
			   				 sliderInput(ns("powerTransform"), "\\(x^a\\) ... a", 
			   				 			min = 2, max = 5, step = 1, value = 3)
			   ),
			   conditionalPanel("input.method  == 'rootTransform'", ns = ns,
			   				 sliderInput(ns("rootTransform"), "\\(\\sqrt[a]{x}\\) ... a", 
			   				 			min = 2, max = 5, step = 1, value = 3)
			   ),
			   conditionalPanel("input.method  == ''", ns = ns,
			   				 sliderInput(ns("invTransform"), "", min = 1, max = 2, step = 1, value = 1),
			   				 sliderInput(ns("noneTransform"), "", min = 1, max = 2, step = 1, value = 1)
			   ))
	)
}

data_transform_server = function(id){
	moduleServer(id, function(input, output, session){
		# default_transform = list(method = function(){"noneTransform"}, 
		# 						 parameter = function(){1}, 
		# 						 func = function(){noneTransformFormula})
		method = reactive({
			# browser()
			if(is.null(input$method)){
				return("noneTransform")
			}else{
				return(input$method)
			}
		})
		
		parameter = reactive({
			# browser()
			input[[method()]]
		})
		
		func = reactive({
			# browser()
			match.fun(method())
		})
		
		formula_func = reactive({
			match.fun(paste0(method(), "Formula"))
		})
		
		return(list(method = method, parameter = parameter, func = func, formula_func = formula_func))
		
	})
}
