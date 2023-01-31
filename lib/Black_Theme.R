invertedColors = function(colors, ignore.alpha = FALSE){
	rgbCol = col2rgb(colors, alpha = TRUE)
	
	if(ignore.alpha){
		rgbCol[1:3, ] = 255 - rgbCol[1:3, ]
	}else{
		rgbCol = 255 - rgbCol
	}
	if(ignore.alpha){
		return(rgb(red = rgbCol[1, ], green = rgbCol[2, ], blue = rgbCol[3, ], maxColorValue = 255))
	}
	return(rgb(red = rgbCol[1, ], green = rgbCol[2, ], blue = rgbCol[3, ], alpha = rgbCol[4, ], maxColorValue = 255))
}

theme_black = function() {
	library(ggplot2)	
	#theme_grey(base_size = base_size, base_family = base_family) %+replace%
		
		theme(
			# Specify axis options
			axis.line = element_line(colour = "white"),  
			axis.text.x = element_text(color = "white"),  
			axis.text.y = element_text(color = "white"),  
			axis.ticks = element_line(color = "white"),  
			axis.title.x = element_text(color = "white"),
			axis.title.y = element_text(color = "white"),
			#axis.ticks.length = unit(0.3, "lines"),   
			# Specify legend options
			legend.background = element_rect(color = NA, fill = "black"),  
			legend.key = element_rect(color = NA,  fill = "black"),  
			#legend.key.size = unit(1.2, "lines"),  
			#legend.key.height = NULL,  
			#legend.key.width = NULL,      
			legend.text = element_text(color = "white"),  
			legend.title = element_text(color = "white"),  
			#legend.position = "right",  
			#legend.text.align = NULL,  
			#legend.title.align = NULL,  
			#legend.direction = "vertical",  
			#legend.box = NULL, 
			# Specify panel options
			panel.background = element_rect(fill = "black", color  =  NA),  
			#panel.border = element_blank(),  
			#panel.grid.major = element_line(color = "grey35"),  
			#panel.grid.minor = element_line(color = "grey20"),  
			#panel.spacing = unit(0.5, "lines"),   
			# Specify facetting options
			strip.background = element_rect(fill = "black", color = "white"),  
			strip.text.x = element_text(color = "white"),  
			strip.text.y = element_text(color = "white"),  
			# Specify plot options
			plot.background = element_rect(color = "black", fill = "black"),  
			plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),  
			#plot.margin = unit(rep(1, 4), "lines"),
			plot.subtitle = element_text(color = "white", hjust = 0.5, face = "italic")
		)
	
}

