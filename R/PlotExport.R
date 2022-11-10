plot_export_UI = function(id){
	ns = NS(id)
	# browser()
	tagList(
		downloadButton(ns("png"), label = "PNG"),
		downloadButton(ns("svg"), label = "SVG"),
		export_size_UI(ns("export_size")),
		h3("Export Preview (1:4)"),
		imageOutput(ns("preview")),
		tool_tip(ns("png"), 
				 "Download plot in PNG format. This format is for quick diplays. Use SVG for vector graphic format."),
		tool_tip(ns("svg"), 
				 "Download track feature plot in SVG format. This format is more suitable for print and presentations. Also you can edit it easily in a vector graphic application.")
	)
}

plot_export_server = function(id, context, export_plot){
	moduleServer(id, function(input, output, session){
		export_size = export_size_server("export_size")
		plot_size = reactive({export_size$size()})
		
		output$svg = downloadHandler(
			filename = function() {
				paste(context, "svg", sep = ".")
			},
			content = function(file) {
				print("Downloading plot in SVG.")
				ggsave(filename = file, plot = export_plot()$plot,
					   width = plot_size()$width, height = plot_size()$height,
					   dpi = 300, units = "cm", fix_text_size = FALSE)
			},
			contentType = "image/svg"
		)
		
		output$png = downloadHandler(
			filename = function() {
				paste(context, "png", sep = ".")
			},
			content = function(file) {
				print("Downloading plot in PNG.")
				ggsave(filename = file, plot = export_plot()$plot, 
					   width = plot_size()$width, height = plot_size()$height,
					   dpi = 300, units = "cm")
			},
			contentType = "image/png"
		)
		
		output$preview = renderImage({
			plot_out = export_plot()
			if(is.list(plot_out)){
				temp_png_file = tempfile(fileext = ".png")
				
				ggsave(temp_png_file, plot_out$plot, width = plot_size()$width, height = plot_size()$height, 
					   dpi = 300, units = "cm")
				dim = ggplot2:::plot_dim(dim = unlist(plot_size(), use.names = F), dpi = 300, units = "cm") * 300
				#browser()
				list(
					src = temp_png_file,
					contentType = "image/png",
					width = dim[1]/4,
					height = dim[2]/4,
					alt = paste(context, "plot")
				)
			}else{
				NULL
			}
		}, deleteFile = FALSE)
	})
}