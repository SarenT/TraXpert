about_UI = function(id, title){
	ns = NS(id)
	tabPanel(title, fluidPage(fluidRow(
		column(4),
		column(4, 
			   h1("About TraXpert"),
			   p(paste("traxpert is a particle tracking analyzing software developed by me (Saren Tasciyan).",
			   		"Particle tracking data contains a lot of information about the motion of particles.", 
			   		"Such data can be very useful in life sciences. As I am studying cell migration,", 
			   		"I have realized the lack of a data analysis tool for tracking data.", 
			   		"There have been already numoreous tools for tracking objects in imaging data.",  
			   		"But making sense of this tracking data requires an extra step: data analysis.",
			   		"This can be tricky and often requires advanced programming skills for data analysis (e.g. R or python).", 
			   		"Combined with suboptimal data analysis with simpler tools (e.g. Excel) can be dangerous.", 
			   		"And of course not everyone must know how to program.", 
			   		"This was my motivation to create this application.", 
			   		"To help scientists to analyze their tracking data as easily as possible.", 
			   		"Therefore, most of the functions in TraXpert are as automatic as possible.")),
			   p(paste("It relies on grouped and well named, calibrated files.", 
			   		"Main goal was to make it compatible with TrackMate but Imaris and text files (e.g. for Chemotaxis Tool) ", 
			   		"are also somewhat supported.", 
			   		"Simple import your named files into TraXpert and it should recognize your groupings and groups.", 
			   		"Most common plot types and features are already available (e.g. trajectory plots, radar plots...).", 
			   		"For simple statistics, you can use track feature plots.", 
			   		"For dynamics or relationships, you can use trajectory feature plots.", 
			   		"You may as well calculate new features or point source directionality under operations.")),
			   p(paste("If you want to use your own code to do more advanced analysis, you can also simply use TraXpert as", 
			   		"a conversion tool!", "Then simply download the table files to avoid dealing with XML structures.", 
			   		"Finally, if you need some more features, please don't hesitate to contact me:")), 
			   a("saren.tasciyan@ist.ac.at", href = "mailto:saren.tasciyan@ist.ac.at")
		),
		column(4)
	)))
}

about_server = function(id){
	moduleServer(id, function(input, output, session){
		
	})
}