point_source_UI = function(id){
	ns = NS(id)
	
	debug_checkbox = function(){
		if(!release){
			checkboxInput(ns("browse"), label = "Debug", value = FALSE)
		}else{
			""
		}
	}
	
	fluidPage(class = "shiny-input-panel", fluidRow(
		column(3, h4("Point Source"), 
			   p("Calculate point source directionality by a providing point source location (physical units NOT pixels)."),
			   debug_checkbox(), 
			   tipify(fileInput(inputId = ns("upload"), label = "Upload"), 
			   				 "Upload a file to fill up the database", "top", "hover"),
			   downloadButton(outputId = ns("download"), label = "Download Template"),
			   actionButton(inputId = ns("calculate"), label = "Calculate")),
		column(9, DTOutput(outputId = ns("table"), width = "60%"))
	))
}

point_source_server = function(id, data){
	moduleServer(id, function(input, output, session){
		observeEvent(input$upload, {
			if(endsWith(input$upload$datapath[1], ".csv")){
				psData = read.csv(input$upload$datapath[1])
				psData = psData %>% select_at(vars("name", pointSourceColumns))
				
				if((psData %>% summarise_all(is.numeric) %>% select(-name) %>% 
					mutate(all = all(c_across())) %>% select(all))$all[1]){
					
					dataList = data()
					dataList$files = dataList$files %>% select_at(vars(!contains(pointSourceColumns))) %>% 
						left_join(psData, by = "name")
					data(dataList)
				}
			}
			#dataList = data()
		})
		
		observeEvent(input$calculate, {
			if(!release && input$browse) browser()
			initializeProgress = function(max, message){
				progress <<- shiny::Progress$new(max = max)
				if(!is.null(message)){
					progress$set(message = message, value = 0)
				}else{
					progress$set(value = 0)
				}
			}
			updateProgress = function(value, detail = NULL) {
				if(is.null(detail)){progress$set(value = value)}else{progress$set(value = value, detail = detail)}
			}
			closeProgress = function(){progress$close()}
			
			dataList = data()
			if(sum(is.na(dataList$files[pointSourceColumns])) == 0){
				data(pointSource(dataList, updateProgress, initializeProgress, closeProgress, 
								 browse = !release && input$browse))
			}
		})
		
		pointSourceColumnDisable = reactive({
			colsToShow = c("name", pointSourceColumns)
			colsToDisable = which(!(colnames(data()$files) %in% colsToShow)) - 1
			#print(colsToDisable)
			colsToDisable
		})
		
		output$table = renderDT(data()$files, rownames = FALSE, 
												 editable = list(target = c("cell"), disable = list(columns = c(0))), 
												 selection = 'none', 
												 options = list(dom = 't', autoWidth = TRUE, 
												 			   columnDefs = list(list(visible = FALSE, 
												 			   					   targets = pointSourceColumnDisable())
												 			   )
												 )
		)#c(1, 2, 3))))
		
		observeEvent(input$table, {
			#browser()
			info = input$table
			info$col = info$col + 1
			if(info$value[1] != ""){
				dataDF = data()
				dataDF$files = editData(data()$files, info, "files_point_source_Out")
				data(dataDF)
			}
		})
		
		output$download = downloadHandler(
			filename = function() {
				paste("Point_Source_Coordinates", "csv", sep = ".")
			},
			content = function(file) {
				write.csv(x = data()$files %>% 
						  	ungroup() %>% 
						  	select(name, pointSourceColumns), 
						  file = file, append = FALSE, quote = TRUE, sep = "\t", dec = ".", 
						  row.names = FALSE, col.names = TRUE)
			},
			contentType = paste("text", "csv", sep = "/")
		)
	})
}