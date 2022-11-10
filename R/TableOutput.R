table_output_UI = function(id){
	ns = NS(id)
	
	tagList(DTOutput(ns("table")), 
			downloadButton(ns("download"), label = "Download table as CSV"))
}

table_output_server = function(id, data){
	moduleServer(id, function(input, output, session){
		output$table = renderDT({data()}, 
								   filter = 'top', rownames = FALSE, editable = "cell", selection = 'single', 
								   class = "compact")
		
		output$download = downloadHandler(
			filename = function() {
				paste("files", "csv", sep = ".")
			},
			content = function(file) {
				write.csv(x = data(), file = file, append = FALSE, quote = TRUE, sep = "\t", dec = ".", 
						  row.names = FALSE, col.names = TRUE)
			},
			contentType = paste("text", "csv", sep = "/")
		)
	})
}