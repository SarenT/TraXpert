addResourcePath("images", directoryPath = "images")
server = function(input, output, session) {
	labelRVs = list()
	# Save extra values in state$values when we bookmark
	onBookmark(function(state) {
		state$values$data = data
	})
	
	# Read values from state$values when we restore
	onRestore(function(state) {
		data = state$values$data
	})
	
	observeEvent(input$tmFilesIn, {
		tmFiles = input$tmFilesIn
		
		tmFiles[pointSourceColumns] = NA
		
		for(pointSourceColumn in pointSourceColumns){
			tmFiles[[pointSourceColumn]] = as.numeric(tmFiles[[pointSourceColumn]])
		}
		
		data(list(files = tmFiles))
		
		groupings(prepareBareGroupings(tmFiles$name))
		hideTab(tabsID, titleFiles); hideTab(tabsID, titleFeatures); 
		hideTab(tabsID, titleTracks); hideTab(tabsID, titleTrajectories)
		hideTab(tabsID, titlePlotTrackFeatures); hideTab(tabsID, titlePlotTrajectories); 
		hideTab(tabsID, titlePlotDirectionality); hideTab(tabsID, titlePlotTrajFeatures)
	})
	
	observeEvent(input$sessionFileIn, {
		sessionFilePath = input$sessionFileIn$datapath[1]
		sessionFile = file(paste0("file://", sessionFilePath))
		open(sessionFile, "rb")
		dataSession = unserialize(connection = sessionFile)
		close(sessionFile)
		
		if(!is.null(dataSession$version)){
			version(dataSession$version)
		}else{
			version(0)
		}
		
		upgrade = dataSessionVersionUpgrade(dataSession, version(), get_progress_functions())
		if(upgrade$error){
			#TODO
			return()
		}else{
			if(upgrade$upgraded){
				showModal(modalDialog(title = "File version upgrade", 
									  "Session file version is upgraded. Download again to keep this new version."))
			}
			dataSession = upgrade$dataSession
		}
		
		if(!is.null(dataSession$groupings)){
			groupings(dataSession$groupings)
		}
		
		if(!is.null(dataSession$data)){
			data(dataSession$data)
		}
		
		if(!is.null(dataSession$version)){
			version(dataSession$version)
		}
		
		showTab(inputId = tabsID, target = titleTracks)
		showTab(inputId = tabsID, target = titleFiles)
		showTab(inputId = tabsID, target = titleFeatures)
		showTab(inputId = tabsID, target = titleTrajectories)
		showTab(inputId = tabsID, target = titlePlotTrajFeatures)
		showTab(inputId = tabsID, target = titlePlotTrackFeatures)
		showTab(inputId = tabsID, target = titlePlotTrajectories)
		showTab(inputId = tabsID, target = titlePlotDirectionality)
		showTab(inputId = tabsID, target = titlePlotTrajFeatures)
	})
	
	output$debug_field = renderUI({
		if(!release) checkboxInput("process_browse_In", label = "Debug", value = FALSE)
	})
	
	observeEvent(input$sampleIn, {
		if(input$sampleIn == "None"){
			return()
		}
		sessionFilePath = paste("res", input$sampleIn, sep = "/")
		sessionFile = file(paste0("file://", sessionFilePath))
		open(sessionFile, "rb")
		dataSession = unserialize(connection = sessionFile)
		close(sessionFile)
		
		if(!is.null(dataSession$version)){
			version(dataSession$version)
		}else{
			version(0)
		}
		
		upgrade = dataSessionVersionUpgrade(dataSession, version(), get_progress_functions())
		if(upgrade$error){
			#TODO
			return()
		}else{
			if(upgrade$upgraded){
				showModal(modalDialog(title = "File version upgrade", 
									  "Session file version is upgraded. Download again to keep this new version."))
			}
			dataSession = upgrade$dataSession
		}
		
		if(!is.null(dataSession$groupings)){
			groupings(dataSession$groupings)
		}
		
		if(!is.null(dataSession$data)){
			data(dataSession$data)
		}
		
		if(!is.null(dataSession$version)){
			version(dataSession$version)
		}
		
		showTab(inputId = tabsID, target = titleTracks)
		showTab(inputId = tabsID, target = titleFiles)
		showTab(inputId = tabsID, target = titleFeatures)
		showTab(inputId = tabsID, target = titleTrajectories)
		showTab(inputId = tabsID, target = titlePlotTrajFeatures)
		showTab(inputId = tabsID, target = titlePlotTrackFeatures)
		showTab(inputId = tabsID, target = titlePlotTrajectories)
		showTab(inputId = tabsID, target = titlePlotDirectionality)
		showTab(inputId = tabsID, target = titlePlotTrajFeatures)
	})
	
	groupings = reactiveVal({
		print("groupings = reactiveVal({")
		prepareBareGroupings(NULL)
	})
	
	version = reactiveVal(NULL)
	data = reactiveVal(NULL)
	
	tracks = reactive({data()$tracks}); trajectories = reactive({data()$trajectories}); 
	files = reactive({data()$files}); features = reactive({data()$features})
	
	parseParameters = reactiveVal(NULL)
	
	observeEvent(input$processFilesIn, {
		# Create a Progress object
		recalculate = input$process_recalculate_In
		browse = input$process_browse_In
		if(!release && browse) browser()
		click = input$processFilesIn
		
		uploadedFiles = files()
		if(!is.null(uploadedFiles)){
			progress = get_progress_functions()
			
			userUnitInput = function(failed = FALSE){
				modalDialog(
					numericInput(inputId = "pixelWidthIn", label = "Enter pixel width (x) size in μm", 
								 value = 2.301, min = 0.0000000001),
					numericInput(inputId = "pixelHeightIn", label = "Enter pixel height (y) size in μm", 
								 value = 2.301, min = 0.0000000001),
					numericInput(inputId = "voxelThicknessIn", label = "Enter voxel thickness (z) size in μm", 
								 value = 1, min = 0.0000000001),
					numericInput(inputId = "frameIntervallIn", label = "Enter frame intervall (t) in sec", 
								 value = 60, min = 0.0000000001),
					span('Manual tracking files don\'t include pixel size and frame intervall information. Please provide it here'),
					if (failed)
						div(tags$b("Invalid input. Please use \".\" as decimal and all fields are mandatory. Use 1 for voxel thickness for 2D images.", 
								   style = "color: red;")),
					
					footer = tagList(
						modalButton("Cancel"),
						actionButton("userUnitInputOKIn", "OK")
					)
				)
			}
			
			groupingsDF = groupings()$groupings
			groupsDF = groupings()$groups
			if(any(endsWith(uploadedFiles$datapath, suffix = ".csv") | 
				   endsWith(uploadedFiles$datapath, suffix = ".txt"))){
				parseParameters(list(files = uploadedFiles$datapath, groupings = groupingsDF, groups = groupsDF, 
									 fileNames = uploadedFiles$name, 
									 progress = progress, 
									 browse = browse))
				observeEvent(input$userUnitInputOKIn, {
					# Check that data object exists and is data frame.
					if (!is.null(input$pixelWidthIn) && !is.null(input$pixelHeightIn) && 
						!is.null(input$voxelThicknessIn) && !is.null(input$frameIntervallIn)) {
						userUnits = list(pixelWidth = input$pixelWidthIn, pixelHeight = input$pixelHeightIn, 
										 voxelThickness = input$voxelThicknessIn, 
										 frameIntervall = input$frameIntervallIn)
						dataDF = parseFiles(uploadedFiles, groupingsDF, groupsDF, progress, browse = browse, 
											calibrationUnits = userUnits)
						if(!is.null(dataDF)){
							data(processData(dataDF, groupsDF, groupingsDF, progress, recalculate = recalculate, 
											 browse = browse))
							
						}else{
							data(NULL)
						}
						removeModal()
					} else {
						showModal(userUnitInput(failed = TRUE))
					}
				})
				showModal(userUnitInput())
			}else{
				dataDF = parseFiles(uploadedFiles, groupingsDF, groupsDF, progress, recalculate = recalculate, 
									browse = browse)
				data(processData(dataDF, groupsDF, groupingsDF, progress, recalculate = recalculate, browse = browse))
				
				showTab(inputId = tabsID, target = titleTracks)
				showTab(inputId = tabsID, target = titleFiles)
				showTab(inputId = tabsID, target = titleFeatures)
				showTab(inputId = tabsID, target = titleTrajectories)
				showTab(inputId = tabsID, target = titlePlotTrajFeatures)
				showTab(inputId = tabsID, target = titlePlotTrackFeatures)
				showTab(inputId = tabsID, target = titlePlotTrajectories)
				showTab(inputId = tabsID, target = titlePlotDirectionality)
				showTab(inputId = tabsID, target = titlePlotTrajFeatures)
			}
		}else{
			showModal(modalDialog(title = "File Upload Error", "Something went wrong during file upload."))
		}
	})
	
	allTrackMeasures = reactive({
		choices = list()
		choices[["Track Features"]] = c(c(` ` = ""), as.vector(choicesInNamedList(c("Track"), features(), 
																				  empty = FALSE)))
		choices[["Formula Items"]] = as.vector(formulaChoices)
		return(choices)
	})
	
	allTrajectoryMeasures = reactive({
		choices = list()
		choices[["Trajectory Features"]] = c(c(` ` = ""), as.vector(choicesInNamedList(c("Spot", "Edge"), features(), 
																					   empty = FALSE)))
		choices[["Formula Items"]] = as.vector(formulaChoices)
		return(choices)
	})
	
	trajChoiceswithoutEmpty = reactive({
		choicesInNamedList(c("Spot", "Edge"), features(), empty = FALSE)
	})
	
	trajChoiceswithEmpty = reactive({
		choicesInNamedList(c("Spot", "Edge"), features(), empty = TRUE)
	})
	
	groupingsChoiceswithEmpty = reactive({
		req(groupings())
		groupingsToNamedList(groupings()$groupings, empty = TRUE)
	})
	
	groupingsChoiceswithEmptywithDoNotDisplay = reactive({
		req(groupings())
		groupingsToNamedList(groupings()$groupings, empty = TRUE, doNotDisplay = TRUE)
	})
	
	groupingsAndFeatureChoiceswithoutEmpty = reactive({
		choices = list(`Grouping Variables` = as.vector(groupingsChoiceswithoutEmpty()),
					   `Measures` = as.vector(trajChoiceswithoutEmpty()))
		choices
	})
	
	groupingsChoiceswithoutEmpty = reactive({
		req(groupings())
		groupingsToNamedList(groupings()$groupings, empty = FALSE)
	})
	
	trackDirectionChoiceswithoutEmpty = reactive({
		#req(groupings())
		#featuresToNamedList(type = "Track", features = data()$features, empty = FALSE)
		choicesInNamedList(type = "Track", features(), "TRACK_DIRECTION", empty = FALSE)
	})
	
	trackDirectionCatChoiceswithoutEmpty = reactive({
		#req(groupings())
		#featuresToNamedList(type = "Track", features = data()$features, empty = FALSE)
		a = choicesInNamedList(type = "Track", features(), "CARDINAL", empty = FALSE)
		print("a:")
		print(a)
		return(a)
	})
	
	dispersionChoices = reactive({
		#featuresToNamedList("Track", data()$features, empty = FALSE)

		# According to the chosen aggregate function, the index of the choice is found (which). Then the available 
		# choices is found (in dispChoicesforAggrFunction). Finally names of available dispersion functions are 
		# found (in dispersionMeasureChoices).
		if(!is.null(input$traj_feat_aggr_fun_In)){
			dispersionMeasureChoices[dispChoicesforAggrFunction[[
				which(aggregateFunctionChoices == input$traj_feat_aggr_fun_In)]]]
		}
		#choicesInNamedList(c("Spot", "Edge"), inp, empty = FALSE)
	})
	
	
	
	observeEvent(input$groupingsOut_cell_clicked, {
		info = input$groupingsOut_cell_clicked
		if(!is.null(info$row)){
	
			groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
			grouping = groupingsDF[[info$row, 1]]
			groupingLabel = groupingsDF[[info$row, 2]]
			groups = groupingsDF[[info$row, 3]]
			groupLabels = groupingsDF[[info$row, 4]]
			groupColors = groupingsDF[[info$row, 5]]
			
			removeUI(selector = '#placeholder input', multiple = TRUE)
			removeUI(selector = '#placeholder textarea', multiple = TRUE)
			removeUI(selector = '#placeholder label', multiple = TRUE)
			id = paste0("groupLabel_", info$row)
			
			insertUI(selector = '#placeholder', ui = textInput(id, label = "Grouping label:", value = groupingLabel))
			for(labelRV in labelRVs){labelRV$destroy()}
			print(labelRVs)
	
			labelRVs <<- list()
			labelRVs[[id]] <<- observeEvent(input[[id]], {
				groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
				groupingsDF[, 2] = as.vector(groupingsDF[, 2])
				groupingsDF[[info$row, 2]] = input[[id]]
				groupingsDF[, 2] = as.factor(groupingsDF[, 2])
				#cat(paste(id, info$row, "\n"))
				#cat(paste("\t", input[[id]], paste(groupingsDF[info$row, 4], collapse = "-"), "\n"))
				groupings(list(groupings = groupingsDF, groups = groupsDF))
			})
			lapply(1:length(groups), function(i){
				#for(i in 1:length(groups)){
				insertUI(selector = '#placeholder',
						 ui = textAreaInput(paste0('label_', sprintf("%03d", i)), 
						 				   label = paste0("Group label for ", groups[i], ":"), value = groupLabels[i]))
				labelRVs[[paste0('label_', sprintf("%03d", i))]] <<- 
					observeEvent(input[[paste0('label_', sprintf("%03d", i))]], {
						groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
						listVals = as.character(groupingsDF[[info$row, 4]])
						
						if(!(input[[paste0('label_', sprintf("%03d", i))]] %in% listVals)){
							listVals[i] = input[[paste0('label_', sprintf("%03d", i))]]
							groupingsDF[[info$row, 4]] = as.factor(listVals)
							#cat(paste("---", id, info$row, "\n"))
							#cat(paste("---", "\t", input[[paste0('label_', sprintf("%03d", i))]], paste(groupingsDF[info$row, 4], collapse = "-"), "\n"))
							groupings(list(groupings = groupingsDF, groups = groupsDF))
						} else {
							showNotification(type = "warning", duration = 2, "Duplicate label!")
						}
						
					})
				insertUI(selector = "#placeholder", 
						 ui = colourInput(paste0('color_', sprintf("%03d", i)), 
						 				 label = paste0("Group color for ", groups[i], ":"), 
						 				 value = groupColors[i]))
				labelRVs[[paste0('color_', sprintf("%03d", i))]] <<- 
					observeEvent(input[[paste0('color_', sprintf("%03d", i))]], {
						groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
						#groupingsDF[, 2] <<- as.vector(groupingsDF[, 2])
						groupingsDF[[info$row, 5]][i] = input[[paste0('color_', sprintf("%03d", i))]]
						#groupingsDF[, 2] <<- as.factor(groupingsDF[, 2])
						#cat(paste("+++", id, info$row, "\n"))
						#cat(paste("+++", "\t", input[[paste0('color_', sprintf("%03d", i))]], paste(groupingsDF[info$row, 5], collapse = "-"), "\n"))
						groupings(list(groupings = groupingsDF, groups = groupsDF))
					})
			})
			print(names(labelRVs))
			cat("========================================================\n")
		}
	})
	
	rankListObs = list() # observeEvents for rank_list objects
	observe({
		groupingsList = groupings()
		dataList = data()

		if(length(groupingsList) > 1){
	
			
			groupingsDF = groupingsList$groupings
			
			removeUI(selector = "#placeholderRank div", multiple = TRUE)
			for(rankListOb in rankListObs){rankListOb$destroy()}
			rankListObs <<- list()
			if(length(dataList) > 1){
				
				lapply(1:nrow(groupingsDF), function(i){
					#for(i in 1:nrow(groupingsDF)){
			
					
					#uiList[[i]] = 
					#insertUI(selector = "#placeholderRank", ui =  textAreaInput(inputId = paste("rankGroupahaha",  sprintf("%03d", i), sep = "_"), 
					#															label = "Grouping label:", value = paste(i)))
					insertUI(selector = "#placeholderRank", ui = rank_list(text = as.character(groupingsDF$labels[i]),
																		   labels = as.list(as.character(groupingsDF$groupLabels[[i]])),
																		   input_id = paste("rankGroup", sprintf("%03d", i), sep = "_")), where = "beforeEnd")
					cat(i, 
						as.character(groupingsDF$labels[i]),  paste("rankGroup", sprintf("%03d", i), sep = "_"), 
						"\n")
					#beepr::beep()
					rankListObs[[paste("rankGroup", sprintf("%03d", i), sep = "_")]] <<- 
						observeEvent(input[[paste("rankGroup",  sprintf("%03d", i), sep = "_")]], {
							
							groupingsDF = groupings()$groupings; groupsDF = groupings()$groups
					
							newOrder = input[[paste('rankGroup', sprintf("%03d", i), sep = "_")]]
							#cat(newOrder); cat("\n")
							#cat(as.character(groupingsDF$groupLabels[[i]])); cat("\n")
							#cat(as.character(groupingsDF$groupLabels[[i]]) == newOrder); cat("\n")
							
							# There seems to be a change in rank order
							
							if(!all(as.character(groupingsDF$groupLabels[[i]]) == newOrder)){
								ranks = match(newOrder, groupingsDF$groupLabels[[i]])
								
								
								# NA means some labels are changed, this triggers due to above statement but user actually didn't update anything
								if(!any(is.na(ranks))){	
							
									groupingsDF$groupLabels[[i]] = 
										factor(newOrder, levels = as.character(groupingsDF$groupLabels[[i]])[ranks])
									
									groupingsDF$groups[[i]] = 
										factor(as.character(groupingsDF$groups[[i]])[ranks], 
											   levels = as.character(groupingsDF$groups[[i]])[ranks])
									groupingsDF$colors[[i]] = as.character(groupingsDF$colors[[i]])[ranks]
									
									group = as.character(groupingsDF$names[i])
									
									dataList = data()
									for(dfName in names(dataList)){
										if(!is.null(dataList[[dfName]][[group]])){
											dataList[[dfName]][[group]] = factor(dataList[[dfName]][[group]], 
																				 levels = groupingsDF$groups[[i]])
										}
									}
									
									data(dataList)
									groupsDF[[group]] = factor(groupsDF[[group]], levels = groupingsDF$groups[[i]])
									groupings(list(groupings = groupingsDF, groups = groupsDF))
								}
							}
						})
				})
			}
		}
		
	})
	
	output$groupingsOut = 
		renderDT({groupings()$groupings}, 
				 rownames = FALSE, editable = FALSE, selection = 'single', 
				 colnames = c('Groupings', 'Grouping Labels', 'Groups', 'Group Labels', 'Group Colors'))
	
	output$files_point_source_Out = renderDT(data()$files, rownames = FALSE, 
											 editable = list(target = c("cell"), disable = list(columns = c(0))), 
											 selection = 'none', 
											 options = list(dom = 't', autoWidth = TRUE, 
											 			   columnDefs = list(list(visible = FALSE, 
											 			   					   targets = pointSourceColumnDisable())
											 			   )
											 )
	)
	
	observeEvent(input$files_point_source_Out_cell_edit, {

		info = input$files_point_source_Out_cell_edit
		info$col = info$col + 1
		if(info$value[1] != ""){
			dataDF = data()
			dataDF$files = editData(files(), info, "files_point_source_Out")
			data(dataDF)	
		}
	})
	
	observeEvent(input$filesOut_cell_edit, {

		info = input$filesOut_cell_edit
		
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$files = editData(files(), info, "files_point_source_Out")
		data(dataDF)
	})
	
	observeEvent(input$featuresOut_cell_edit, {

		info = input$featuresOut_cell_edit
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$features = editData(features(), info, "featuresOut")
		data(dataDF)
	})
	
	observeEvent(input$tracksOut_cell_edit, {

		info = input$tracksOut_cell_edit
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$tracks = editData(tracks(), info, "tracksOut")
		data(dataDF)
	})
	
	observeEvent(input$trajecoriesOut_cell_edit, {

		info = input$trajecoriesOut_cell_edit
		info$col = info$col + 1
		
		dataDF = data()
		dataDF$trajectories = editData(trajectories(), info, "trajecoriesOut")
		data(dataDF)
	})
	
	output$sessionOut = downloadHandler(
		
		# This function returns a string which tells the client
		# browser what name to use when saving the file.
		filename = function() {
			"session.tmx"
		},
		
		# This function should write data to a file given to it by
		# the argument 'file'.
		content = function(file) {
			dataObj = list()
			dataObj$version = dataModelVersion
			dataObj$groupings = groupings()
			dataObj$data = data()
	
			# Write to a file specified by the 'file' argument
			dataObjSerial = serialize(dataObj, NULL)
			writeBin(object = dataObjSerial, con = file)
		}
	)
	
	trackChoiceswithoutEmpty = reactive({
		choicesInNamedList("Track", features(), empty = FALSE)
	})
	
	choices = list(groupingsChoiceswithEmpty = groupingsChoiceswithEmpty, 
				   groupingsChoiceswithoutEmpty = groupingsChoiceswithoutEmpty, 
				   groupingsChoiceswithEmptywithDoNotDisplay = 	groupingsChoiceswithEmptywithDoNotDisplay,
				   groupingsAndFeatureChoiceswithoutEmpty = groupingsAndFeatureChoiceswithoutEmpty,
				   trackChoiceswithoutEmpty = trackChoiceswithoutEmpty, 
				   trackDirectionChoiceswithoutEmpty = trackDirectionChoiceswithoutEmpty, 
				   trackDirectionCatChoiceswithoutEmpty = trackDirectionCatChoiceswithoutEmpty,
				   trajChoiceswithoutEmpty = trajChoiceswithoutEmpty, 
				   trajChoiceswithEmpty = trajChoiceswithEmpty, 
				   dispersionChoices = dispersionChoices)
	
	table_output_server("files_table_out", files)
	table_output_server("features_table_out", features)
	table_output_server("tracks_table_out", tracks)
	table_output_server("trajectories_table_out", trajectories)
	
	rotation_server("rotation", data)
	
	feature_calculator_server("track_new_feat", allTrackMeasures, data, features, groupings, 
							  "tracks", "tracks", "TRACK_ID", "track_global_id")
	feature_calculator_server("traj_new_feat", allTrajectoryMeasures, data, features, groupings, 
							  "trajectories", "trajectories", "ID", "track_global_id")
	feature_calculator_server("track_from_traj_new_feat", allTrajectoryMeasures, data, features, groupings, 
							  "trajectories", "tracks", "ID", "track_global_id")
	
	point_source = point_source_server("point_source", data)
	
	track_features_server("track_features", data, features, tracks, trajectories, groupings, choices)
	trajectories_server("trajectories", data, features, tracks, trajectories, groupings, choices)
	directionality_server("directionality", data, features, tracks, trajectories, groupings, choices)
	trajectory_features_server("trajectory_features", data, features, tracks, trajectories, groupings, choices)
}
