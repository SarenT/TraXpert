if(!("release" %in% ls())) release = TRUE

textFaceChoices = list(Plain = "plain", Italic = "italic", Bold = "bold", `Bold & Italic` = "bold.italic")
aggregateFunctionChoices = list(`Do not aggregate` = "NULL", Sum = "sum", Mean = "mean", Median = "median", 
								Mode = "single.mode", Min = "min", Max = "Max")
summaryFunctionChoices = aggregateFunctionChoices
summaryFunctionChoices[["Number of elements"]] = "length"
dispersionMeasureChoices = list(`Do not display error` = "NULL", `Standard Deviation` = "stddev", 
								`Interquartile Range` = "IQR", `Range (Min/Max)` = "range", 
								`Standard error of the mean` = "se", `CI 95%` = "ci95", `CI 99%` = "ci99")
dispChoicesforAggrFunction = list(`Do not aggregate` = c(1), Sum = c(1), Mean = c(1, 2, 5, 6, 7), 
								  Median = c(1, 3, 4, 6, 7), Mode = c(1), Min = c(1, 4), Max = c(1, 4))
dispersionTypeChoices = list(`Line Range` = "linerange", `Point Range` = "pointrange", `Cross Bar` = "crossbar", 
							 `Error Bar` = "errorbar", `Ribbon` = "ribbon")
statLabelChoices = list(Stars = "p.signif", `p value` = "p.format") 
#, Stars = "..p.signif..", `p Format` = "..p.format..", p = "p", pp = "..p..")
#statLabelChoices = list(Stars = "..p.signif..", `p Format` = "..p.format..", p = "..p..")
trajFeaturesChoices = list(`Point Plot` = "point", `Jitter Plot` = "jitter", `Quantile Plot` = "quantile", 
						   `Smooth Plot` = "smooth", `Line Plot` = "line", `Area Plot` = "area", `Step Plot` = "step")
trackFeaturesChoices = list(`Bar Plot` = "bar", `Box Plot` = "box", `Dot Plot` = "dot", `Violin Plot` = "violin")
featureDimensionChoices = list(`Position` = "POSITION", `Time` = "TIME", `N/A` = "NONE", `Velocity/Speed` = "VELOCITY", 
							   `Length` = "LENGTH", `Angle` = "ANGLE")
trackMultipleStatChoices = list(`Do not perform` = "NONE", `ANOVA` = "anova", `Kruskal–Wallis Test` = "kruskal.test")
trackPairwiseStatChoices = list(`Do not perform` = "NONE", `Student's t Test` = "t.test", 
								`Wilcoxon/Mann-Whitney Test` = "wilcox.test")
trackPairwStatComparisonTypeChoices = list(`All Combinations` = "all_combinations", 
										   `All groups to control group` = "to_control", `Selected Pairs` = "selected")

dataTransformChoices = list(`None` = "noneTransform", `Power (left)` = "powerTransform", 
							`Root (right)` = "rootTransform", `Log (right)` = "logTransform", 
							`Inverse (right)` = "invTransform")

trackDirectionalityMeasures = list(`Number of Tracks in Cardinal Groups` = "cardinalNumberOfTracks")

positionTypes = list(Absolute = "unfixed", `Fixed at origin` = "fixed", `Fixed and rotated` = "fixed-rotated")

circMultiSampleTestMeasures = list(`Do not test` = "NULL", `Common Mean Direction` = "mean", 
								   `Common Median Direction` = "median", `Common Concentration` = "conc", 
								   `Common Distribution` = "dist")

circMultiSampleTestMeanMethods = 
	list(`Watson's Large-sample Nonparametric Test` = "watsons.large.sample.nonparametric.test", 
		 `Watson-Williams Test` = "watson.williams.test.2")
circMultiSampleTestMedianMethods = list(`Fisher's Nonparametric Test` = "fisher.nonparametric.test", 
										`Random. Fisher Nonparametric Test` = "PgRandTest")
circMultiSampleTestConcMethods = list(`Wallraff's Nonparametric Test` = "WallraffTest")
circMultiSampleTestDistMethods = list(`Watson Two Sample Test` = "watson.two.test")

circMultiSampleTestMeasureMethods = list(`NULL` = NULL, `meanDir` = circMultiSampleTestMeanMethods, 
										 `medianDir` = circMultiSampleTestMedianMethods, 
										 `concentration` = circMultiSampleTestConcMethods, 
										 `distribution` = circMultiSampleTestDistMethods)
# circMultiSampleTestMeasureInputs = list(`NULL` = NULL, `meanDir` = "dir_multisample_mean_method_In", 
# 										`medianDir` = "dir_multisample_median_method_In", 
# 										`concentration` = "dir_multisample_conc_method_In", 
# 										`distribution` = "dir_multisample_dist_method_In")

formulaChoicesVector = c(" ", "(", ")", "-", "/", "*", "+", "^", "mean(", "first(", "last(", "median(", "%%", "&&", 
						 "||", ">", "<", "<=", ">=", "lag(", "lead(", "sqrt(")
formulaChoices = as.list(formulaChoicesVector)
names(formulaChoices) = formulaChoicesVector

circHistQQCellSize = 800

dataModelVersion = 1

tabsID = "tabs"
tabColorImport = "#ffaaaa"; tabColorOperations = "#aaaaff"; tabColorTables = "#ffffaa"; tabColorPlots = "#aaffaa"
	
titleImportGroupings = "Import/Groupings"; titleOperations = "Operations"; 
titleFiles = "Files" ;titleFeatures = "Features" ;titleTracks = "Tracks"
titleTrajectories = "Trajectories"; titlePlotTrackFeatures = "Plot Track Features"
titlePlotTrajectories = "Plot Trajectories"; titlePlotDirectionality = "Plot Directionality"
titlePlotTrajFeatures = "Plot Trajectory Features"; titleAbout = "About"


trackDisplacementColumns = paste("TRACK_DISPLACEMENT", c("X", "Y", "Z"), sep = "_")

# TRACK_[XYZ]_LOCATION
trackLocationColumns = paste("TRACK", c("X", "Y", "Z"), "LOCATION", sep = "_")
# EDGE_[XYZ]_LOCATION
edgeLocationColumns = paste("EDGE", c("X", "Y", "Z"), "LOCATION", sep = "_")

tracksColNames = 
	c("NUMBER_SPOTS", "NUMBER_GAPS", "LONGEST_GAP", "NUMBER_SPLITS", "NUMBER_MERGES", "NUMBER_COMPLEX", 
	  "TRACK_DURATION", "TRACK_START", "TRACK_STOP", "TRACK_DISPLACEMENT", "TRACK_INDEX", "TRACK_ID", 
	  trackLocationColumns, "TRACK_MEAN_SPEED", "TRACK_MAX_SPEED", 
	  "TRACK_MIN_SPEED", "TRACK_MEDIAN_SPEED", "TRACK_STD_SPEED", "TRACK_MEAN_QUALITY", "TRACK_MAX_QUALITY", 
	  "TRACK_MIN_QUALITY", "TRACK_MEDIAN_QUALITY", "TRACK_STD_QUALITY")

positionColumns = c("POSITION_X", "POSITION_Y", "POSITION_Z", "POSITION_T")
locationColumns = c("POSITION_X", "POSITION_Y", "POSITION_Z", "POSITION_T", "FRAME")

startPositionColumns = paste("START", positionColumns, sep="_")
endPositionColumns = paste("END", positionColumns, sep="_")
fixedPositionColumns = paste(positionColumns, "FIX", sep = "_")
displacementColumns = paste(positionColumns[1:3], "DISPLACEMENT", sep = "_") # Edge time already exists
rotFixedPositionsColumns = paste(fixedPositionColumns, "ROT", sep = "_")
allPositionTypes = list(positionColumns, fixedPositionColumns, rotFixedPositionsColumns)


edgeDirectionColumns = c("EDGE_DIRECTION_PHI", "EDGE_DIRECTION_THETA")
pointSourceColumns = paste("pointSource", c("x", "y", "z"), sep = "_")
cardinalCols = list(DIRECTION_POINT_SOURCE_PHI_CARDINAL = "DIRECTION_POINT_SOURCE_PHI",
					DIRECTION_POINT_SOURCE_THETA_CARDINAL = "DIRECTION_POINT_SOURCE_THETA",
					EDGE_DIRECTION_POINT_SOURCE_PHI_CARDINAL = "EDGE_DIRECTION_POINT_SOURCE_PHI",
					EDGE_DIRECTION_POINT_SOURCE_THETA_CARDINAL = "EDGE_DIRECTION_POINT_SOURCE_THETA",
					DIRECTION_CARDINAL = "TRACK_DIRECTION", DIRECTION_CARDINAL_ROT = "TRACK_DIRECTION_ROT")

#edgePointSourceDirectionColumns = gsub("DIRECTION", "POINT_SOURCE_DIRECTION", edgeDirectionColumns)
pointSourceDirectionColumns = 
	c("DIRECTION_POINT_SOURCE_PHI", "DIRECTION_POINT_SOURCE_THETA", 
	  "DIRECTION_POINT_SOURCE_PHI_CARDINAL", "DIRECTION_POINT_SOURCE_THETA_CARDINAL", 
	  "MEAN_DEVIATION_FROM_POINT_SOURCE_PHI", "MEAN_DEVIATION_FROM_POINT_SOURCE_THETA",
	  "EDGE_DIRECTION_POINT_SOURCE_PHI", "EDGE_DIRECTION_POINT_SOURCE_THETA",
	  "EDGE_DIRECTION_POINT_SOURCE_PHI_CARDINAL", "EDGE_DIRECTION_POINT_SOURCE_THETA_CARDINAL",
	  "EDGE_DEVIATION_FROM_POINT_SOURCE_PHI", "EDGE_DEVIATION_FROM_POINT_SOURCE_THETA")

trajFeats = 
	data.frame(feature = c(locationColumns, "TRACK_ID", 
						   "SPOT_SOURCE_ID", "SPOT_TARGET_ID", "EDGE_TIME", edgeLocationColumns, "VELOCITY", 
						   "VELOCITY_AVG", "DISPLACEMENT", fixedPositionColumns, rotFixedPositionsColumns[1:3], 
						   displacementColumns, "DISPLACEMENT_FIX", "SQUARE_DISPLACEMENT_FIX", "TURN_ANGLE",
						   edgeDirectionColumns),#, edgePointSourceDirectionColumns),
			   name = c("X", "Y", "Z", "T", "Frame", "Track ID", "Source spot ID", "Target spot ID", "Edge time", 
			   		 "X Location", "Y Location", "Z Location", "Velocity", "Velocity (Avg)", "Displacement", 
			   		 "X fixed at origin", "Y fixed at origin", "Z fixed at origin", "T fixed at origin", 
			   		 "X fixed at origin and rotated", "Y fixed at origin and rotated", "Z fixed at origin and rotated",
			   		 "X displacement", "Y displacement", "Z displacement", 
			   		 "Displacement from origin", "Square of displacement from origin", "Turn angle",
			   		 "Direction φ (XY)", "Direction θ (Z)"),#, 
			   #"Direction to point source φ (XY)", "Direction to point source θ (Z)"),
			   shortname = c("X", "Y", "Z", "T", "Frame", "Track ID", "Source ID", "Target ID", "T", "X", "Y", 
			   			  "Z", "V", "V A", "D", "X fixed", "Y fixed", "Z fixed", "T fixed", 
			   			  "X fixed rotated", "Y fixed rotated", "Z fixed rotated",
			   			  "ΔX", "ΔY", "ΔZ",
			   			  "Displacement", "Displacement squared", "Turn",
			   			  "φ", "θ"),#, "φ (Point Source)", "θ (Point Source)"),
			   dimension = c("POSITION", "POSITION", "POSITION", "TIME", "NONE", "NONE", "NONE", "NONE", "TIME", 
			   			  "POSITION", "POSITION", "POSITION", "VELOCITY", "VELOCITY", "LENGTH", "POSITION", "POSITION", 
			   			  "POSITION", "TIME", "POSITION", "POSITION", "POSITION", "LENGTH", "LENGTH", "LENGTH",
			   			  "LENGTH", "LENGTH", "ANGLE", "ANGLE", "ANGLE"),#, "ANGLE", "ANGLE"),
			   isint = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
			   		  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
			   		  FALSE, FALSE, FALSE, FALSE),#, FALSE, FALSE),
			   type = c("Spot", "Spot", "Spot", "Spot", "Spot", "Spot", "Edge", "Edge", "Edge", "Edge", 
			   		 "Edge", "Edge", "Edge", "Edge", "Edge", "Spot", "Spot", "Spot", "Spot", "Spot", "Spot", 
			   		 "Spot", "Edge", "Edge", "Edge", "Spot", "Spot", "Edge", "Spot", "Spot"))#, "Edge", "Edge"))
trackFeats = 
	data.frame(feature = c("TRACK_ID", "TRACK_DURATION", "TRACK_DISPLACEMENT", trackLocationColumns, "TRACK_MEAN_SPEED", 
						   "TRACK_MAX_SPEED", "TRACK_MIN_SPEED", "TRACK_STD_SPEED", "MEAN_SQUARE_DISPLACEMENT",
						   "MEAN_ANGLE_TURN", trackDisplacementColumns),
			   name = c("Track ID", "Duration of track", "Track displacement", "X Location", 
			   		 "Y Location", "Z Location", "Mean velocity", "Maximal velocity", 
			   		 "Minimal velocity", "Velocity standard deviation", "Mean Square Displacement",
			   		 "Mean angular change/turn", 
			   		 "X Displacement", "Y Displacement", "Z Displacement"),
			   shortname = c("Track ID", "Duration", "Displacement", "X", "Y", "Z", "Mean V", 
			   			  "Max V", "Min V", "V std", "MSD", "Mean Turn", 
			   			  "ΔX", "ΔY", "ΔZ"),
			   dimension = c("NONE", "TIME", "LENGTH", "POSITION", "POSITION", "POSITION", "VELOCITY", 
			   			  "VELOCITY", "VELOCITY", "VELOCITY", "LENGTH", "ANGLE",  
			   			  "LENGTH", "LENGTH", "LENGTH"),
			   isint = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
			   		  FALSE, FALSE, FALSE),
			   type = c("Track", "Track", "Track", "Track", "Track", "Track", "Track", "Track", 
			   		 "Track", "Track", "Track", "Track", "Track", "Track", "Track"))
featsStartEndPositions = 
	data.frame(feature = c(startPositionColumns, endPositionColumns),
			   name = c("Track Start X", "Track Start Y", "Track Start Z", "Track Start T",
			   		 "Track End X", "Track End Y", "Track End Z",  "Track End T"),
			   shortname = c("X1", "Y1", "Z1", "T1", "Xn", "Yn", "Zn", "Tn"),
			   dimension = c("POSITION", "POSITION", "POSITION", "TIME", "POSITION", "POSITION", "POSITION", "TIME"),
			   isint = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
			   type = c("Track", "Track", "Track", "Track", "Track", "Track", "Track", "Track"))
pointSourceFeats = 
	data.frame(feature = pointSourceDirectionColumns,
			   name = c("Direction to point source φ (XY)", "Direction to point source θ (Z)",
			   		 "Cardinal direction to point source φ (XY)", "Cardinal direction to point source θ (Z)",
			   		 "Deviation from point source φ (XY)", "Deviation from point source θ (Z)",
			   		 "Edge direction to point source φ (XY)", "Edge direction to point source θ (Z)",
			   		 "Cardinal edge direction to point source φ (XY)", "Cardinal edge direction to point source θ (Z)",
			   		 "Edge deviation from point source φ (XY)", "Edge deviation from point source θ (Z)"),
			   shortname = c("φ (Point src)", "θ (Point src)",
			   			  "Card. φ (Point src)", "Card. θ (Point src)",
			   			  "Dφ (Point src)", "Dθ (Point src)",
			   			  "φ (Point src)", "θ (Point src)",
			   			  "Card. φ (Point src)", "Card. θ (Point src)",
			   			  "Dφ (Point src)", "Dθ (Point src)"),
			   dimension = c("ANGLE", "ANGLE", "ANGLE", "ANGLE", "ANGLE", "ANGLE", 
			   			  "ANGLE", "ANGLE", "ANGLE", "ANGLE", "ANGLE", "ANGLE"),
			   isint = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
			   		  FALSE, FALSE),
			   type = c("Track", "Track", "Track", "Track", "Track", "Track", 
			   		 "Edge", "Edge", "Edge", "Edge", "Edge", "Edge")
	)
featureTypes = c("Spot", "Edge", "Track")
featsDir = 
	data.frame(feature = c("TRACK_DIRECTION", "TRACK_DIRECTION_ROT", "TRACK_DIRECTION_Z", 
						   "TRACK_DIRECTION_Z_ROT", "DIRECTION_CARDINAL", "DIRECTION_CARDINAL_ROT"), 
			   name = c("Track direction", "Track direction after rotation fix", 
			   		 "Track direction in Z (inclination)", 
			   		 "Track direction in Z (inclination) after rotation fix", 
			   		 "Cardinal track direction", 
			   		 "Cardinal track direction after rotation"), 
			   shortname = c("Direction", "Direction Rotation Fixed", "Inclination", 
			   			  "Inclination Rotation Fixed", "Cardinal Direction", "Cardinal Direction Rotation Fixed"), 
			   dimension = c("ANGLE", "ANGLE", "ANGLE", "ANGLE", "NONE", "NONE"),
			   isint = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
			   type = c("Track", "Track", "Track", "Track", "Track", "Track"))
featsPathLength = data.frame(feature = "PATH_LENGTH", name = "Track path length", shortname = "Path length", 
							 dimension = "LENGTH", isint = FALSE, type = "Track")
featsPersistence = data.frame(feature = c("STRAIGHTNESS", "ANGULAR_PERSISTENCE"), 
							  name = c("Track persistence by straightness", "Angular deviation"), 
							  shortname = c("Persistence/Straightness", "Angular Persistence"), 
							  dimension = c("NONE", "NONE"), isint = c(FALSE, FALSE), type = c("Track", "Track"))
featsVelocity = data.frame(feature = c("TRACK_VELOCITY", "TRACK_MEAN_SPEED_AVG"), 
						   name = c("Track velocity", "Mean track speed (avg)"), 
						   shortname = c("Velocity", "Mean track speed"), 
						   dimension = c("VELOCITY", "VELOCITY"), isint = c(FALSE, FALSE), type = c("Track", "Track"))

# Imaris col name conversions to be used in select

# Trajectories
imarisCoreNames = c(TRACK_ID = "TrackID", SPOT_ID = "ID", FRAME = "Time")

imarisPosNames = c("Position X", "Position Y", "Position Z", "Unit")
names(imarisPosNames) = c(positionColumns[1:3], "spatialunits")

imarisTimeNames = c("Time_", "Unit"); names(imarisTimeNames) = c(positionColumns[4], "timeunits")
imarisDisplNames = c("Displacement Delta X", "Displacement Delta Y", "Displacement Delta Z")
names(imarisDisplNames) = displacementColumns

imarisSpeedNames = c(VELOCITY = "Speed")
imarisFixedPosNames = c("Displacement X", "Displacement Y", "Displacement Z")
names(imarisFixedPosNames) = fixedPositionColumns[1:3]

imarisDisplSquaredNames = c(SQUARE_DISPLACEMENT_FIX = "Displacement^2")
imarisDisplFixNames = c(DISPLACEMENT_FIX = "Displacement Length")
imarisDisplLengthNames = c(DISPLACEMENT = "Displacement Delta Length")
imarisTrajectoryModel = 
	list(list(sheetName = "Displacement Delta", specNames = imarisDisplNames, title = "displacement",
			  exprs = "lead(POSITION_X) - POSITION_X; lead(POSITION_Y) - POSITION_Y; lead(POSITION_Z) - POSITION_Z",
			  names = c("POSITION_X_DISPLACEMENT", "POSITION_Y_DISPLACEMENT", "POSITION_Z_DISPLACEMENT")),
		 list(sheetName = "Speed", specNames = imarisSpeedNames, title = "speed",
		 	 exprs = "sqrt(get(displacementColumns[1]) ** 2 + get(displacementColumns[2]) ** 2 + get(displacementColumns[3]) ** 2)", 
		 	 names = c("VELOCITY")),
		 list(sheetName = "Displacement", specNames = imarisFixedPosNames, title = "fixed position",
		 	 exprs = "fixTrajectoryStartPos(POSITION_X); fixTrajectoryStartPos(POSITION_Y); fixTrajectoryStartPos(POSITION_Z)",
		 	 names = c("POSITION_X_FIX", "POSITION_Y_FIX", "POSITION_Z_FIX")),
		 list(sheetName = "Displacement^2", specNames = imarisDisplSquaredNames, title = "square displacement fix",
		 	 exprs = "get(fixedPositionColumns[1]) ** 2 + get(fixedPositionColumns[2]) ** 2 + get(fixedPositionColumns[3]) ** 2",
		 	 names = c("SQUARE_DISPLACEMENT_FIX")),
		 list(sheetName = "Displacement Length", specNames = imarisDisplFixNames, title = "displacement fix",
		 	 exprs = "sqrt(SQUARE_DISPLACEMENT_FIX)",
		 	 names = c("DISPLACEMENT_FIX")),
		 list(sheetName = "Displacement Delta Length", specNames = imarisDisplLengthNames, title = "displacement length",
		 	 exprs = "sqrt(get(displacementColumns[1]) ** 2 + get(displacementColumns[2]) ** 2 + get(displacementColumns[3]) ** 2)",
		 	 names = c("DISPLACEMENT"))
	)
imarisTrackCoreNames = c(TRACK_ID = "ID")
imarisTrackDisplLengthNames = c(TRACK_DISPLACEMENT = "Track Displacement Length")

imarisTrackDisplNames = paste("Track Displacement", c("X", "Y", "Z"))
names(imarisTrackDisplNames) = trackDisplacementColumns

imarisTrackDuration = c(TRACK_DURATION = "Track Duration")

imarisTrackPosition = paste("Track Position", c("X", "Y", "Z"), "Mean")
names(imarisTrackPosition) = trackLocationColumns

imarisTrackMeanSpeed = c(TRACK_MEAN_SPEED = "Track Speed Mean")
imarisTrackMaxSpeed = c(TRACK_MAX_SPEED = "Track Speed Max")
imarisTrackMinSpeed = c(TRACK_MIN_SPEED = "Track Speed Min")
imarisTrackSTDSpeed = c(TRACK_STD_SPEED = "Track Speed StdDev")

imarisTrackPosStart = paste("Track Position", c("X", "Y", "Z"), "Start")
names(imarisTrackPosStart) = startPositionColumns[1:3]

imarisTrackPosEnd = paste("Track Position", c("X", "Y", "Z"), "End")
names(imarisTrackPosEnd) = endPositionColumns[1:3]

imarisTrackPathLength = c(PATH_LENGTH = "Track Length")
imarisTrackStraightness = c(STRAIGHTNESS = "Track Straightness")
imarisTrackVelocity = c(TRACK_VELOCITY = "Track Velocity")
imarisTrackModel = 
	list(list(sheetName = "Track Displacement", specNames = imarisTrackDisplNames, 
			  title = "displacement", summarize = TRUE,
			  exprs = "last(POSITION_X) - first(POSITION_X); last(POSITION_Y) - first(POSITION_Y); last(POSITION_Z) - first(POSITION_Z)",
			  names = c("TRACK_DISPLACEMENT_X", "TRACK_DISPLACEMENT_Y", "TRACK_DISPLACEMENT_Z")),
		 list(sheetName = "Track Displacement Length", specNames = imarisTrackDisplLengthNames, 
		 	 title = "displacement length", summarize = FALSE,
		 	 exprs = "sqrt(TRACK_DISPLACEMENT_X ** 2 + TRACK_DISPLACEMENT_Y ** 2 + TRACK_DISPLACEMENT_Z ** 2)",
		 	 names = c("TRACK_DISPLACEMENT")),
		 list(sheetName = "Track Length", specNames = imarisTrackPathLength, 
		 	 title = "path length", summarize = TRUE,
		 	 exprs = "sum(DISPLACEMENT, na.rm = TRUE)",
		 	 names = c("PATH_LENGTH")),
		 list(sheetName = "Track Speed Mean", specNames = imarisTrackMeanSpeed, 
		 	 title = "mean speed", summarize = TRUE,
		 	 exprs = "mean(VELOCITY, na.rm = TRUE)",
		 	 names = c("TRACK_MEAN_SPEED")),
		 list(sheetName = "Track Speed Min", specNames = imarisTrackMinSpeed, 
		 	 title = "min speed", summarize = TRUE,
		 	 exprs = "min(VELOCITY, na.rm = TRUE)",
		 	 names = c("TRACK_MIN_SPEED")),
		 list(sheetName = "Track Speed Max", specNames = imarisTrackMaxSpeed, 
		 	 title = "max speed", summarize = TRUE,
		 	 exprs = "max(VELOCITY, na.rm = TRUE)",
		 	 names = c("TRACK_MAX_SPEED")),
		 list(sheetName = "Track Speed StdDev", specNames = imarisTrackSTDSpeed, 
		 	 title = "std dev of speed", summarize = TRUE,
		 	 exprs = "sd(VELOCITY, na.rm = TRUE)",
		 	 names = c("TRACK_STD_SPEED")),
		 list(sheetName = "Track Duration", specNames = imarisTrackDuration, 
		 	 title = "track duration", summarize = TRUE,
		 	 exprs = "last(POSITION_T) - first(POSITION_T)",
		 	 names = c("TRACK_DURATION")),
		 list(sheetName = "Track Position Start", specNames = imarisTrackPosStart, 
		 	 title = "start position", summarize = TRUE,
		 	 exprs = "first(POSITION_X); first(POSITION_Y); first(POSITION_Z); first(POSITION_T)",
		 	 names = c("START_POSITION_X", "START_POSITION_Y", "START_POSITION_Z", "START_POSITION_T")),
		 list(sheetName = "Track Position End", specNames = imarisTrackPosEnd, 
		 	 title = "end position", summarize = TRUE,
		 	 exprs = "last(POSITION_X); last(POSITION_Y); last(POSITION_Z); last(POSITION_T)",
		 	 names = c("END_POSITION_X", "END_POSITION_Y", "END_POSITION_Z", "END_POSITION_T")),
		 list(sheetName = "Track Straightness", specNames = imarisTrackStraightness, 
		 	 title = "straightness", summarize = FALSE,
		 	 exprs = "TRACK_DISPLACEMENT / PATH_LENGTH",
		 	 names = c("STRAIGHTNESS")),
		 list(sheetName = "Track Velocity", specNames = imarisTrackVelocity, 
		 	 title = "velocity", summarize = FALSE, #Mock
		 	 exprs = "TRACK_DISPLACEMENT / TRACK_DURATION",
		 	 names = c("TRACK_VELOCITY"))
		 
	)

angleCatFrom = -pi/12
angleCatTo = 25*pi/12
angleCatBy = pi/6
directionCat = seq(from = angleCatFrom, to = angleCatTo, by = angleCatBy)
directionCatDegree = directionCat * 180 / pi
directionCatMidDegree = (lead(directionCatDegree) + directionCatDegree)/2
directionCatMidDegree = directionCatMidDegree[1:(length(directionCatMidDegree)-2)] # Removing last elements 360°, NA

narrowestPoreSizeColNames = c("NARROWEST_PORE_CROSS_SIZE", "NARROWEST_PORE_CROSS_ANGLE", 
							  "POSITION_NARROWEST_PORE_CROSS_X", "POSITION_NARROWEST_PORE_CROSS_Y")
orthNarrowestPoreSizeColNames = c("ORTH_NARROWEST_PORE_CROSS_SIZE", "ORTH_NARROWEST_PORE_CROSS_ANGLE", 
								  "POSITION_ORTH_NARROWEST_PORE_CROSS_X", "POSITION_ORTH_NARROWEST_PORE_CROSS_Y")
poreSizeColNames = c(narrowestPoreSizeColNames, orthNarrowestPoreSizeColNames)

