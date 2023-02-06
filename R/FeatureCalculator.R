feature_calculator_UI = function(id, title, description, featureDimensionChoices, typeChoices, column_size = 4){
	ns = NS(id)
	
	debug_checkbox = function(){
		if(!release){
			checkboxInput(ns("debug"), label = "Debug", value = FALSE)
		}else{
			""
		}
	}
	
	#newFeaturePanel = function(title, description, feature, typeChoices){
	tagList(
		column(column_size, fluidPage(class = "shiny-input-panel", fluidRow(
			column(12, h4(title),
				   p(description))),
			fluidRow(column(12, fluidPage(
				fluidRow(
					column(6, textInput(ns("feat"), "Feature"),
						   textInput(ns("shortname"), "Short Name")),
					column(6, textInput(ns("name"), "Name"),
						   fluidPage(fluidRow(
						   	column(6, 
						   		   tipify(selectInput(ns("dimension"), "Dimension", choices = featureDimensionChoices), 
						   		   	   "Feature dimenion. This indicates the type of physical information the feature will contain. e.g. displacement is a length, change of displacement over time is velocity etc.", "top")),
						   	column(6, 
						   		   tipify(selectInput(ns("type"), "Type", choices = typeChoices), 
						   		   	   "Type of feature. Options are Track, Spot or Edge. This selection may be limited.", "top"))
						   )))
				), hr(),
				fluidRow(
					column(6, tipify(selectInput(ns("vars"), "Variables", choices = list()), 
									 "Existing variables to choose from to include in the formula.", "top")),
					column(6, tipify(textInput(ns("formula"), "Formula"),#, choices = formulaChoices, selected = NULL, multiple = TRUE, options = list(create = TRUE)), 
									 "Formula to calculate the new feature. Every item needs to be selected or added. Plain text will be ignored. The formula needs to be correctly written (all paranthesis closed, mathematical operator between variables etc.).", 
									 "top"))
				),
				fluidRow(column(6, actionButton(ns("calculate"), label = "Calculate")),
						 column(6, debug_checkbox()))
			))
		)),
		bsTooltip(ns("btn"), "Start calculation. All fields must be filled."),
		bsTooltip(ns("feat"), "Feature identifier. This needs to start with a letter (a-z or A-Z), may contain letters or numbers and \"_\"."),
		bsTooltip(ns("name"), "Visible title/name for the feature."),
		bsTooltip(ns("shortname"), "Visible short name for the feature."))
	)
	#}
}

feature_calculator_server = function(id, measures, data, features, groupings, 
									 table_from_name, table_to_name, 
									 id_col, track_global_id = "track_global_id"){
	moduleServer(id, function(input, output, session){
		observe({updateSelectInput(session, "vars", choices = measures())})
		
		observeEvent(input$vars, {
			#browser()
			formula = input$formula
			formula = paste0(formula, input$vars)
			updateTextInput(session = session, inputId = "formula", value = formula)
		})
		
		observeEvent(input$calculate, {
			if(!release && input$debug) browser()
			
			feat = input$feat
			
			# The feature identifier needs to be compliant with variable names
			if(isSuitableVarName(feat)){
				featName = input$name
				featShortName = input$shortname
				featDimension = input$dimension
				featType = input$type
				formula = paste(input$formula, collapse = " ")
				
				if(!isEmpty(featName) && !isEmpty(featShortName) && !isEmpty(featDimension) && !isEmpty(featType) && !isEmpty(formula)){
					data_tables = data()
					data_from = data_tables[[table_from_name]]
					data_to = data_tables[[table_to_name]]
					# tracks = data_tables$tracks
					# trajectories = data_tables$trajectories
					# tracks = data_tables$tracks 
					
					
					# data_to = tryCatch({
					# 	data_to %>% select_at(vars(-one_of(feat))) %>% group_by(track_global_id) %>% mutate(!!feat := !!parse_expr(formula))
					if(table_from_name == table_to_name){
						group_bys = c(id_col, track_global_id)
					}else{
						group_bys = c(track_global_id)
					}
					
					data_to = tryCatch({
						data_to %>% select_at(vars(-one_of(feat))) %>% 
							left_join(data_from %>% group_by_at(group_bys) %>% 
									  	summarise(!!feat := !!parse_expr(formula)), by = group_bys)
					# tracks = tryCatch({
					# 	tracks %>% select_at(vars(-one_of(feat))) %>% group_by(track_global_id) %>% mutate(!!feat := !!parse_expr(formula))
					# trajectories = tryCatch({
					# 	trajectories %>% select_at(vars(-one_of(feat))) %>% group_by(track_global_id) %>% mutate(!!feat := !!parse_expr(formula))
					# tracks = tryCatch({
					# 	tracks %>% select_at(vars(-one_of(feat))) %>% left_join(trajectories() %>% group_by(track_global_id) %>% summarise(!!feat := !!parse_expr(formula)), by = "track_global_id")
					}, error = function(e){
						print(e)
						showModal(modalDialog(
							title = "Error",
							paste0("There was an error calculating the new feature.", e, sep = "\n"),
							easyClose = TRUE,
							footer = NULL
						))
					})
					
					if(!is.null(data_to)){
					# if(!is.null(tracks)){
					# if(!is.null(trajectories)){
					# if(!is.null(tracks)){
						# Adding feature definition to the features df
						feats = features()
						feats = appendNewFeatures(feats, feat, featName, featShortName, featDimension, FALSE, featType, 
												  groupings()$groups, groupings()$groupings$names)
						
						data_tables[[table_to_name]] = data_to
						# data_tables$tracks = tracks
						# data_tables$trajectories = trajectories
						# data_tables$tracks = tracks
						data_tables$features = feats
						
						data(data_tables)
					}else{
						print(e)
						showModal(modalDialog(
							title = "Error",
							"There was an unexpected behavior setting the calculated new feature.",
							easyClose = TRUE,
							footer = NULL
						))
					}
				}else{
					showModal(modalDialog(
						title = "Warning",
						"There all fields need to be provided before any calculation.",
						easyClose = TRUE,
						footer = NULL
					))
				}
			}
			
		})
	})
}