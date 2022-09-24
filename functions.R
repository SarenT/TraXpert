source("Black_Theme.R")

trackDisplacementColumns = paste("TRACK_DISPLACEMENT", c("X", "Y", "Z"), sep = "_")

# TRACK_[XYZ]_LOCATION
trackLocationColumns = paste("TRACK", c("X", "Y", "Z"), "LOCATION", sep = "_")
# EDGE_[XYZ]_LOCATION
edgeLocationColumns = paste("EDGE", c("X", "Y", "Z"), "LOCATION", sep = "_")

tracksColNames = c("NUMBER_SPOTS", "NUMBER_GAPS", "LONGEST_GAP", "NUMBER_SPLITS", "NUMBER_MERGES", "NUMBER_COMPLEX", 
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
pointSourceDirectionColumns = c("DIRECTION_POINT_SOURCE_PHI", "DIRECTION_POINT_SOURCE_THETA", 
								"DIRECTION_POINT_SOURCE_PHI_CARDINAL", "DIRECTION_POINT_SOURCE_THETA_CARDINAL", 
								"MEAN_DEVIATION_FROM_POINT_SOURCE_PHI", "MEAN_DEVIATION_FROM_POINT_SOURCE_THETA",
								"EDGE_DIRECTION_POINT_SOURCE_PHI", "EDGE_DIRECTION_POINT_SOURCE_THETA",
								"EDGE_DIRECTION_POINT_SOURCE_PHI_CARDINAL", "EDGE_DIRECTION_POINT_SOURCE_THETA_CARDINAL",
								"EDGE_DEVIATION_FROM_POINT_SOURCE_PHI", "EDGE_DEVIATION_FROM_POINT_SOURCE_THETA")

trajFeats = data.frame(feature = c(locationColumns, "TRACK_ID", 
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
trackFeats = data.frame(feature = c("TRACK_ID", "TRACK_DURATION", "TRACK_DISPLACEMENT", trackLocationColumns, "TRACK_MEAN_SPEED", 
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
featsStartEndPositions = data.frame(feature = c(startPositionColumns, endPositionColumns),
						name = c("Track Start X", "Track Start Y", "Track Start Z", "Track Start T",
								 "Track End X", "Track End Y", "Track End Z",  "Track End T"),
						shortname = c("X1", "Y1", "Z1", "T1", "Xn", "Yn", "Zn", "Tn"),
						dimension = c("POSITION", "POSITION", "POSITION", "TIME", "POSITION", "POSITION", "POSITION", "TIME"),
						isint = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
						type = c("Track", "Track", "Track", "Track", "Track", "Track", "Track", "Track"))
pointSourceFeats = data.frame(feature = pointSourceDirectionColumns,
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
featsDir = data.frame(feature = c("TRACK_DIRECTION", "TRACK_DIRECTION_ROT", "TRACK_DIRECTION_Z", 
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
imarisPosNames = c("Position X", "Position Y", "Position Z", "Unit"); names(imarisPosNames) = c(positionColumns[1:3], "spatialunits")
imarisTimeNames = c("Time_", "Unit"); names(imarisTimeNames) = c(positionColumns[4], "timeunits")
imarisDisplNames = c("Displacement Delta X", "Displacement Delta Y", "Displacement Delta Z"); names(imarisDisplNames) = displacementColumns
imarisSpeedNames = c(VELOCITY = "Speed")
imarisFixedPosNames = c("Displacement X", "Displacement Y", "Displacement Z"); names(imarisFixedPosNames) = fixedPositionColumns[1:3]
imarisDisplSquaredNames = c(SQUARE_DISPLACEMENT_FIX = "Displacement^2")
imarisDisplFixNames = c(DISPLACEMENT_FIX = "Displacement Length")
imarisDisplLengthNames = c(DISPLACEMENT = "Displacement Delta Length")
imarisTrajectoryModel = list(list(sheetName = "Displacement Delta", specNames = imarisDisplNames, title = "displacement",
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
imarisTrackDisplNames = paste("Track Displacement", c("X", "Y", "Z")); names(imarisTrackDisplNames) = trackDisplacementColumns
imarisTrackDuration = c(TRACK_DURATION = "Track Duration")
imarisTrackPosition = paste("Track Position", c("X", "Y", "Z"), "Mean"); names(imarisTrackPosition) = trackLocationColumns
imarisTrackMeanSpeed = c(TRACK_MEAN_SPEED = "Track Speed Mean")
imarisTrackMaxSpeed = c(TRACK_MAX_SPEED = "Track Speed Max")
imarisTrackMinSpeed = c(TRACK_MIN_SPEED = "Track Speed Min")
imarisTrackSTDSpeed = c(TRACK_STD_SPEED = "Track Speed StdDev")
imarisTrackPosStart = paste("Track Position", c("X", "Y", "Z"), "Start"); names(imarisTrackPosStart) = startPositionColumns[1:3]
imarisTrackPosEnd = paste("Track Position", c("X", "Y", "Z"), "End"); names(imarisTrackPosEnd) = endPositionColumns[1:3]
imarisTrackPathLength = c(PATH_LENGTH = "Track Length")
imarisTrackStraightness = c(STRAIGHTNESS = "Track Straightness")
imarisTrackVelocity = c(TRACK_VELOCITY = "Track Velocity")
imarisTrackModel = list(list(sheetName = "Track Displacement", specNames = imarisTrackDisplNames, title = "displacement", summarize = TRUE,
							 exprs = "last(POSITION_X) - first(POSITION_X); last(POSITION_Y) - first(POSITION_Y); last(POSITION_Z) - first(POSITION_Z)",
							 names = c("TRACK_DISPLACEMENT_X", "TRACK_DISPLACEMENT_Y", "TRACK_DISPLACEMENT_Z")),
						list(sheetName = "Track Displacement Length", specNames = imarisTrackDisplLengthNames, title = "displacement length", summarize = FALSE,
							 exprs = "sqrt(TRACK_DISPLACEMENT_X ** 2 + TRACK_DISPLACEMENT_Y ** 2 + TRACK_DISPLACEMENT_Z ** 2)",
							 names = c("TRACK_DISPLACEMENT")),
						list(sheetName = "Track Length", specNames = imarisTrackPathLength, title = "path length", summarize = TRUE,
							 exprs = "sum(DISPLACEMENT, na.rm = TRUE)",
							 names = c("PATH_LENGTH")),
						list(sheetName = "Track Speed Mean", specNames = imarisTrackMeanSpeed, title = "mean speed", summarize = TRUE,
							 exprs = "mean(VELOCITY, na.rm = TRUE)",
							 names = c("TRACK_MEAN_SPEED")),
						list(sheetName = "Track Speed Min", specNames = imarisTrackMinSpeed, title = "min speed", summarize = TRUE,
							 exprs = "min(VELOCITY, na.rm = TRUE)",
							 names = c("TRACK_MIN_SPEED")),
						list(sheetName = "Track Speed Max", specNames = imarisTrackMaxSpeed, title = "max speed", summarize = TRUE,
							 exprs = "max(VELOCITY, na.rm = TRUE)",
							 names = c("TRACK_MAX_SPEED")),
						list(sheetName = "Track Speed StdDev", specNames = imarisTrackSTDSpeed, title = "std dev of speed", summarize = TRUE,
							 exprs = "sd(VELOCITY, na.rm = TRUE)",
							 names = c("TRACK_STD_SPEED")),
						list(sheetName = "Track Duration", specNames = imarisTrackDuration, title = "track duration", summarize = TRUE,
							 exprs = "last(POSITION_T) - first(POSITION_T)",
							 names = c("TRACK_DURATION")),
						list(sheetName = "Track Position Start", specNames = imarisTrackPosStart, title = "start position", summarize = TRUE,
							 exprs = "first(POSITION_X); first(POSITION_Y); first(POSITION_Z); first(POSITION_T)",
							 names = c("START_POSITION_X", "START_POSITION_Y", "START_POSITION_Z", "START_POSITION_T")),
						list(sheetName = "Track Position End", specNames = imarisTrackPosEnd, title = "end position", summarize = TRUE,
							 exprs = "last(POSITION_X); last(POSITION_Y); last(POSITION_Z); last(POSITION_T)",
							 names = c("END_POSITION_X", "END_POSITION_Y", "END_POSITION_Z", "END_POSITION_T")),
						list(sheetName = "Track Straightness", specNames = imarisTrackStraightness, title = "straightness", summarize = FALSE,
							 exprs = "TRACK_DISPLACEMENT / PATH_LENGTH",
							 names = c("STRAIGHTNESS")),
						list(sheetName = "Track Velocity", specNames = imarisTrackVelocity, title = "velocity", summarize = FALSE, #Mock
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

# For left skewed data
powerTransform = function(x, a = 1){return(x ^ a)}
powerTransformFormula = function(x, a = 1){formula = paste0("$\\left(", x, "\\right)^{", a, "}$"); return(formula)}
# For right skewed data
rootTransform = function(x, a = 1){return(x ^ (1/a))}
rootTransformFormula = function(x, a = 1){formula = paste0("$\\left(", x, "\\right)^{", 1, "/", a, "}$"); return(formula)}
logTransform = function(x, a = 1){return(log(x, a))}
logTransformFormula = function(x, a = 1){formula = paste0("$\\log_", a, "\\left(", x, "\\right)$"); return(formula)}
invTransform = function(x, a = 1){x = 1/x; x[is.infinite(x)] = 0; return(x)}
invTransformFormula = function(x, a = 1){formula = paste0("$\\left(", 1, "/", x, "\\right)^{", a, "}$"); return(formula)}
noneTransform = function(x, a = 1){return(x)}
noneTransformFormula = function(x, a = 1){return(x)}


#' Ensures error-free selection by checking if index is within the list., meaning that there are enough number of 
#' elements within the list.
#'
#' @param choices A list to select from
#' @param index Index to select
#'
#' @return Returns the selection if index is within the list. If not, NULL is returned.
#' @export
#'
#' @examples
safeSelect = function(choices, index = 1){
	#browser()
	if(length(choices) >= index){
		return(choices[[index]])
	}else{
		return(NULL)
	}
}

#' Checks if the candidate var name is suitable to be a var name.
#'
#' @param names Name to be checked
#'
#' @return A suitable name will be returned
#' @export
#'
#' @examples
isSuitableVarName = function(name){
	return(grepl("^[[:alpha:]][a-zA-Z0-9_]+$", name))
}

#' Checks whether or not the variable is empty
#'
#' @param x Variable to be checked
#'
#' @return Returns \code{TRUE} if the variable is either \code{NULL} or empty string \code{""} 
#' @export
#'
#' @examples
isEmpty = function(x){
	if(!is.null(x)){
		if(x != ""){
			return(FALSE)
		}
	}
	return(TRUE)
}

# editDataTest = function(data, info, proxy = NULL, rownames = TRUE, resetPaging = FALSE, ...) {
# 	browser()
# 	for (r in split(info, info$col)) {
# 		i = r$row; j = r$col + !rownames; v = r$value
# 		j = j[1]
# 		# the 0-th column is the row names in this case
# 		if (j == 0) {
# 			rownames(data)[i] = v
# 		} else {
# 			data[i, j] = coerceValue(v, data[i, j, drop = TRUE])
# 		}
# 	}
# 	if (is.character(proxy)) proxy = dataTableProxy(proxy)
# 	if (inherits(proxy, 'dataTableProxy')) {
# 		replaceData(proxy, data, resetPaging = resetPaging, rownames = rownames, ...)
# 	}
# 	data
# }

#' Adapted from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#' Returns mode of the vector
#'
#' @param x Vector
#'
#' @return The mode of the vector
#' @export
#'
#' @examples
modes = function(x) {
	ux = unique(x)
	tab = tabulate(match(x, ux))
	ux[tab == max(tab)]
}

#' Calculates the standard deviation range around the mean or the vector
#'
#' @param x Vector
#'
#' @return Returns the range in a named list \code{c(upper = upper, mean = mean, lower = lower)}.
#' @export
#'
#' @examples
stddev = function(x){
	dev = sd(x)
	mean = mean(x)
	return(c(upper = mean + dev, mean = mean, lower = mean - dev))
}

#' Returns the standard error range around the mean or the vector
#'
#' @param x Vector
#'
#' @return Returns the range in a named list \code{c(upper = upper, mean = mean, lower = lower)}.
#' @export
#'
#' @examples
se = function(x){
	mean = mean(x)
	dev = sqrt(var(x)/length(x))
	return(c(upper = mean + dev, mean = mean, lower = mean - dev))
}

#' Calculates the interquantile range around the median
#'
#' @param x Vector
#'
#' @return Returns the range in a named list \code{c(upper = upper, median = median, lower = lower)}.
#' @export
#'
#' @examples
interquantilerange = function(x){
	dev = quantile(x)
	return(c(upper = dev[4], median = dev[3], lower = dev[2]))
}

#' Calculates the confidence intervall of 95%. This is a wrapper function as the calling function can't input parameters
#'
#' @param x Vector
#'
#' @return Confindence intervall of 95%
#' @export
#'
#' @examples
ci95 = function(x){
	return(Rmisc::CI(x = x, ci = 0.95))
}

#' Calculates the confidence intervall of 99%. This is a wrapper function as the calling function can't input parameters
#'
#' @param x Vector
#'
#' @return Confindence intervall of 99%
#' @export
#'
#' @examples
ci99 = function(x){
	return(Rmisc::CI(x = x, ci = 0.99))
}

#' Calcualtes the mean of the modes (in case there are multiple)
#'
#' @param x Vector
#' @param na.rm Remove NAs
#'
#' @return Returns the mean of of modes
#' @export
#'
#' @examples
single.mode = function(x, na.rm = FALSE) {
	mean(modes(x), na.rm = na.rm)
}

#' Calculates the midrange of the vector
#'
#' @param x vector 
#'
#' @return The midrange
#' @export
#'
#' @examples
midrange = function(x){
	mean(min(x), max(x))
}

#' Makes sure the vector starts at 0 and all the other values are referenced to this 0 position.
#'
#' @param x Vector
#'
#' @return Returns the fixed vector
#' @export
#'
#' @examples
fixTrajectoryStartPos = function(x){
	return(x - first(x))
}

#' Rotates the fixed position with 2 Euler angles
#'
#' @param x vector
#' @param y vector
#' @param z vector
#' @param phiRotate Rotation on XY plane
#' @param thetaRotate Rotation on Z
#' @param browse runs `browser()` for debugging
#'
#' @returns the rotated fixed position
#' @export
#'
#' @examples
rotateFixedPosition = function(x, y, z, phiRotate = 0, thetaRotate = 0, browse = FALSE){
	if(browse) browser()
	r = sqrt(x ^ 2 + y ^ 2 + z ^ 2)
	phi = atan2(y = y, x = x)
	phi = phi + phiRotate; phi %% (2*pi)
	
	theta = acos(z / r)
	direction_z = theta[length(theta)] %% (pi)
	theta = acos(z / r)
	
	theta = ifelse(is.nan(theta), pi/2, theta)
	
	theta = theta + thetaRotate
	theta = theta %% pi
	rotatedFix = data.frame(r * cos(phi) * sin(theta), r * sin(phi) * sin(theta), r * cos(theta))
	colnames(rotatedFix) = c(paste(fixedPositionColumns[1], "ROT", sep = "_"), 
							 paste(fixedPositionColumns[2], "ROT", sep = "_"),
							 paste(fixedPositionColumns[3], "ROT", sep = "_"))
	return(rotatedFix)
}

#' Replaces group labels in stat outputs
#'
#' @param statOuts Statistics output
#' @param statGroupName Group name used for statistics
#' @param y.lab y label
#'
#' @return Returns the replaced stat outputs
#' @export
#'
#' @examples
replaceStatLabels = function(statOuts, statGroupName, y.lab){
	for(i in 1:length(statOuts)){
		statOut = statOuts[[i]]
		if(startsWith(statOut[4], "data:")){
			statOutDataSplit = strsplit(statOut[4], split = " ")[[1]]
			# Replacing grouping name with label
			statOutDataSplit[length(statOutDataSplit)] = paste0("\"", statGroupName, "\"")
			# Replacing y/dependent variable with label
			if(!is.null(y.lab)){
				if(!is.na(y.lab)){
					if(y.lab != ""){
						statOutDataSplit[length(statOutDataSplit) - 2] = paste0("\"", y.lab, "\"")
					}
				}	
			}
			
			statOut[4] = paste(statOutDataSplit, collapse = " ")
			statOuts[[i]] = statOut
		}
	}
	return(statOuts)
}


#' Imports imaris trajectory related features from the file
#'
#' @param trajectories Trajectories data frame
#' @param filePath file path
#' @param sheetName Name of the worksheet to get trajectory related data from
#' @param exprs Expression for calculation
#' @param sheets Names of the sheets within the file
#' @param newColnames Names of the new corresponding columns in the data frame
#' @param groupText Group text for verbose output
#' @param recalculate Recalculation option
#' @param coreNames Names of the core columns
#' @param specNames Names of the special columns
#' @param title Title for verbose output
#'
#' @return Returns the data frame with added imported data
#' @export
#'
#' @examples
imarisImportTrajectories = function(trajectories, filePath, sheetName, exprs, sheets, newColnames, groupText, 
									recalculate, coreNames, specNames, title = ""){
	
	if(recalculate || !(sheetName %in% sheets)){
		cat("\t");cat(paste("Calculating trajectory features -", title, groupText));cat("\n")
		trajectories = trajectories %>% group_by(TRACK_ID) %>% mutate(!!!parse_exprs(exprs)) %>% 
			# renaming columns
			rename_at(vars(tail(names(.), length(newColnames))), list(~ newColnames))
	}else{
		cat("\t");cat(paste("Parsing trajectory features -", title, groupText));cat("\n")
		trajectories = trajectories %>% left_join(
			read_excel(path = filePath, sheet = sheetName, skip = 1) %>% 
				select(c(coreNames, specNames)) %>% arrange(TRACK_ID, FRAME), by = names(coreNames))
	}
	return(trajectories)
}

#' Imports imaris track related features from the file
#'
#' @param tracks Tracks data frame
#' @param trajectories Trajectories data frame
#' @param filePath file path
#' @param sheetName Name of the worksheet to get trajectory related data from
#' @param exprs Expression for calculation
#' @param summarize Whether or not summarize trajectory data to calculate features
#' @param sheets Names of the sheets within the file
#' @param newColnames Names of the new corresponding columns in the data frame
#' @param groupText Group text for verbose output
#' @param recalculate Recalculation option
#' @param coreNames Names of the core columns
#' @param specNames Names of the special columns
#' @param title Title for verbose output
#'
#' @return Returns the data frame with added imported data
#' @export
#'
#' @examples
imarisImportTracks = function(tracks, trajectories, filePath, sheetName, exprs, summarize, sheets, newColnames, 
							  groupText, recalculate, coreNames, specNames, title = ""){
	if(recalculate || !(sheetName %in% sheets)){
		cat("\t");cat(paste("Calculating track features -", title, groupText));cat("\n")
		if(summarize){
			tracks = tracks %>% left_join(trajectories %>% group_by(TRACK_ID) %>% 
										  	summarise(!!!parse_exprs(exprs)) %>% 
										  	rename_at(vars(tail(names(.), length(newColnames))), list(~ newColnames)), 
										  by = "TRACK_ID")
		}else{
			tracks = tracks %>% group_by(TRACK_ID) %>% mutate(!!!parse_exprs(exprs)) %>% 
				rename_at(vars(tail(names(.), length(newColnames))), list(~ newColnames))
		}
	}else{
		cat("\t");cat(paste("Parsing track features -", title, groupText));cat("\n")
		tracks = tracks %>% left_join(read_excel(path = filePath, sheet = sheetName, skip = 1) %>%
									  	select(c(coreNames, specNames)) %>% 
									  	arrange(TRACK_ID), by = names(coreNames))
	}
	return(tracks)
}
#' Convert unit of the variable name
#' 
#' @param defaultUnit A string (current unit)
#' @param unit A string (unit to be converted to)
#' @param varName A string (variable name)
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' unitConversion("mm", "μm", "TRACK_LENGTH")
#' unitConversion("min", "s", "TRACK_DURATION")
unitConversion = function(defaultUnit, unit, varName){
	#varNameStr = varName
	if(!is.null(defaultUnit) && !is.null(unit)){
		if(ud.are.convertible(defaultUnit, unit)){
			defaultUnitName = deparse(substitute(defaultUnit))
			unitName = deparse(substitute(unit))
			varExpr = expr(ud.convert(!!sym(varName), !!sym(defaultUnitName), !!sym(unitName)))
			return(varExpr)
		}
	}
	varExpr = expr(!!sym(varName))
	return(varExpr)
}

aggregateByGroups = function(data, groups, func){
	return(aggregate(reformulateT(groups, "."), data = data, func, na.action = NULL))
}

#' Returns \code{NULL} for trycatch
#'
#' @param err Error in character
#' @param data Not used
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
retNULL = function(err, data){
	cat("Error: "); print(err); cat("\n")
	return(NULL)
}

#' Extracts file extensions from file names
#'
#' @param files Character vector
#'
#' @return Returns the unique extensions in a list with the number of files associated with it
#' @export
#'
#' @examples
getTrackingFileType = function(files){
	endings = list()
	for(file in files){
		ending = tail(strsplit(file, "\\.")[[1]], 1)
		if(ending %in% names(endings)){
			endings[[ending]] = endings[[ending]] + 1
		}else{
			endings[[ending]] = 1
		}
	}
	return(endings)
}

#' Generates a group colors for a grouping depending on the number of groups and the order of the grouping
#'
#' @param i Order/index of the grouping
#' @param n Number of groups in the grouping
#'
#' @return Returns a vector of colors
#' @export
#'
#' @examples
generateGroupColor = function(i, n){
	i = (i %% 8) + 1
	if(i == 1){
		return(brewer.pal(n, "RdGy"))
	} else if(i == 2){
		return(rainbow_hcl(n))
	} else if(i == 3){
		return(sequential_hcl(n))
	} else if(i == 4){
		return(diverge_hcl(n))
	} else if(i == 5){
		return(brewer.pal(n, "Set1"))
	} else if(i == 6){
		return(brewer.pal(n, "Dark2"))
	} else if(i == 7){
		return(brewer.pal(n, "Paired"))
	} else if(i == 8){
		return(brewer.pal(n, "Accent"))
	}
}

# hTestToString = function(hTest, digits = 6, prefix = "\t"){
# 	out = paste0("\n", strwrap(hTest$method, prefix = prefix), "\n\ndata:  ", hTest$data.name, "\n")
# 	statPart = c()
# 	if (!is.null(hTest$statistic)) 
# 		statPart = c(statPart, paste(names(hTest$statistic), "=", format(hTest$statistic, 
# 															digits = max(1L, digits - 2L))))
# 	if (!is.null(hTest$parameter)) 
# 		statPart = c(statPart, paste(names(hTest$parameter), "=", format(hTest$parameter, 
# 															digits = max(1L, digits - 2L))))
# 	if (!is.null(hTest$p.value)) {
# 		fp = format.pval(hTest$p.value, digits = max(1L, digits - 
# 												  	3L))
# 		statPart = c(statPart, paste("p-value", if (substr(fp, 1L, 1L) == 
# 										   "<") fp else paste("=", fp)))
# 	}
# 	out = paste0(out, strwrap(paste(statPart, collapse = ", ")), sep = "\n")
# 	alternative = ""
# 	if (!is.null(hTest$alternative)) {
# 		alternative = paste0(alternative, "alternative hypothesis: ")
# 		if (!is.null(hTest$null.value)) {
# 			if (length(hTest$null.value) == 1L) {
# 				alt.char = switch(hTest$alternative, two.sided = "not equal to", 
# 								   less = "less than", greater = "greater than")
# 				alternative = paste0(alternative, "true ", names(hTest$null.value), " is ", alt.char, 
# 					" ", hTest$null.value, "\n", sep = "")
# 			} else {
# 				alternative = paste0(alternative, hTest$alternative, "\nnull values:\n")
# 				alternative = paste0(alternative, hTest$null.value, digits = digits, ...)
# 			}
# 		}else{alternative = paste0(alternative, hTest$alternative, "\n")}
# 	}
# 	out = paste0(out, alternative, sep = "\n")
# 	if (!is.null(hTest$conf.int)){
# 		out = paste0(out, format(100 * attr(hTest$conf.int, "conf.level")), " percent confidence interval:\n", 
# 			" ", paste0(format(hTest$conf.int[1:2], digits = digits), 
# 					   collapse = " "), "\n")
# 	}
# 	if (!is.null(hTest$estimate)) {
# 		out = paste0(out, "sample estimates:\n")
# 		out = paste0(out, outhTest$estimate, digits = digits, ...)
# 	}
# 	out = paste0(out, "\n")
# 	return(out)
# }

# plotDownloadHandler = function(plot, autowidth, width, autoheight, height, fileType, fileName){
# 	customDownloadHandler = downloadHandler(
# 		filename = function() {
# 			paste(fileName, fileType, sep = ".")
# 		},
# 		content = function(file) {
# 			if(autowidth){width = NA}
# 			if(autoheight){height = NA}
# 			browser()
# 			##svg(filename = tempfile(), )
# 			ggsave(filename = file, plot = plot, width = width, height = height, dpi = 300, units = "cm")
# 			#
# 			# Write to a file specified by the 'file' argument
# 			#write()
# 			#writeBin(object = dataObjSerial, con = file)
# 		},
# 		contentType = paste("image", fileType, sep = "/")
# 	)
# 	return(customDownloadHandler)
# }

#' Prepares titles and subtitles according to user selection
#'
#' @param titleIn primary title
#' @param titleCheckIn boolean option to exclude primary title
#' @param subtitleIn secondary title
#' @param subtitleCheckIn boolean option to exclude secondary title
#'
#' @return Returns a list with the chosen primary and secondary titles \code{list(title, subtitle)}
#' @export
#'
#' @examples
setTitleInputs = function(titleIn, titleCheckIn, subtitleIn, subtitleCheckIn){
	titles = list()
	if(titleCheckIn){title = NA}else{title = titleIn}
	if(!is.na(title)){if(title == ""){title = NULL}}
	if(subtitleCheckIn){subtitle = NA}else{subtitle = subtitleIn}
	if(!is.na(subtitle)){if(subtitle == ""){subtitle = NULL}}
	titles$title = title
	titles$subtitle = subtitle
	return(titles)
}

#' Appends new feature to the existing features data frame
#'
#' @param oldFeatures data frame
#' @param newFeatureColName character vector new feature column name
#' @param newFeatureName character vector new feature name
#' @param newFeatureShortName character vector new feature short name
#' @param newFeatureDimension character vector new feature dimension
#' @param newFeatureIsInt logical vector new feature if it is integer
#' @param newFeatureType character vector new feature type (one of \code{c("Track", "Spot", "Edge)})
#' @param groupsDF data frame typically \code{groupings()$groups}
#' @param groupingsNames character vector groupings names typically \code{groupings()$groupings$names}
#'
#' @return Returns the old feature data frame with the newly appended features
#' @export
#'
#' @examples
appendNewFeatures = function(oldFeatures, newFeatureColName, newFeatureName, newFeatureShortName, newFeatureDimension, 
							 newFeatureIsInt, newFeatureType, groupsDF, groupingsNames){
	newFeatDF = data.frame(feature = newFeatureColName,  name = newFeatureName, shortname = newFeatureShortName, 
						   dimension = newFeatureDimension, isint = newFeatureIsInt, type = newFeatureType)
	#browser()
	newFeatDF = merge(newFeatDF, groupsDF)
	#newFeatDF = cbind(newFeatDF, groupsDF)
	newFeatDF$group_id = apply(newFeatDF[, as.character(groupingsNames), drop = F], 1, paste, collapse = "_")
	oldFeatures = plyr::rbind.fill(oldFeatures, newFeatDF)
	
	return(oldFeatures)
}

setFeatureType = function(x, col, features, data_tag){
	is_int = (features %>% filter(feature == col & type == data_tag))$isint
	x = as.character(x)
	
	if(is_int){ # See if type is integer then set
		x = as.integer(x)
	}else{
		x = as.numeric(x)
	}
	return(x)
}

#' Sets the features in the data according to the feature type
#'
#' @param data 
#' @param features 
#' @param dataTag 
#'
#' @return
#' @export
#'
#' @examples
setFeatureTypesinData = function(data, features, dataTag){
	#browser()
	cols = features$feature[which(features$type==dataTag)]
	data = data %>% mutate(across(cols[cols %in% colnames(data)], ~ setFeatureType(., cur_column(), features, dataTag)))
	
	return(data)
}

#' Calculates average spot velocities between frames. This function actually calculates for each spot the velocity 
#' before and after. Let's say spot has the index n. n-1 -> n and n -> n+1 displacements are calculated. Then sum of 
#' these is divided by the time difference between n-1 and n+1. For the first and last spots only one displacement is
#' used.
#'
#' @param x vector
#' @param y vector 
#' @param z vector
#' @param t vector
#'
#' @return Returns a vector of spot velocities
#' @export
#'
#' @examples
avgSpotVelocity = function(x, y, z, t){
	leadDispl = sqrt((lead(x) - x)^2 + (lead(y) - y)^2 + (lead(z) - z)^2)
	lagDispl = sqrt((x - lag(x))^2 + (y - lag(y))^2 + (z - lag(z))^2)
	vel = (leadDispl + lagDispl) / (lead(t) - lag(t))
	
	vel[1] = leadDispl[1]/(t[2] - t[1])
	vel[length(vel)] = last(lagDispl) / (t[length(t)] - t[length(t) - 1])
	
	return(vel)
}

#' Corrects empty values and replaced them with 0 (position) or -1 (others)
#'
#' @param trajs Trajectories data frame
#' @param colNames names of columns
#'
#' @return Returns the corrected data frame, where empty rows are replaced with placeholder values.
#' @export
#'
#' @examples
emptyColCorrect = function(trajs, colNames){
	select = rep(TRUE, nrow(trajs))
	for(colName in colNames){
		if(grepl("POSITION_", colName)){
			nonVal = 0.0
		}else{
			nonVal = -1.0
		}
		select = select & (trajs[[colName]] == nonVal)
	}
	select[is.na(select)] = FALSE
	trajs[select, colNames] = NA
	return(trajs)
}

#' Corrects pore sizes
#'
#' @param trajs Trajectories data frame
#'
#' @return Returns the corrected pore sizes
#' @export
#'
#' @examples
poreSizeCorrect = function(trajs){
	return(emptyColCorrect(emptyColCorrect(trajs, narrowestPoreSizeColNames), orthNarrowestPoreSizeColNames))
}

# Adapted from https://stackoverflow.com/a/21689613/2900822 user Gregor
#' Generates a list of letters like an Excel style column name
#'
#' @param n Number of letters
#' @param depth depth tracking
#'
#' @return Returns the vector of letters
#' @export
#'
#' @examples
letterwrap = function(n, depth = 1) {
	args = lapply(1:depth, FUN = function(x) return(LETTERS))
	x = do.call(expand.grid, args = list(args, stringsAsFactors = F))
	x = x[, rev(names(x)), drop = F]
	x = do.call(paste0, x)
	if (n <= length(x)) return(x[1:n])
	return(c(x, letterwrap(n - length(x), depth = depth + 1)))
}

#' Prepare bare (empty) groupings based on the file names. It automatically detecs variable parts between underscores in
#' file names. Only variable parts are taken as a grouping. Parts themselves become groups.
#'
#' @param files file names in a vector of characters
#'
#' @return Returns a list of groupings and groups \code{list(groupings = groupings, groups = uniqueGroups)}
#' \code{groupings = data.frame(names = c(), labels = c(), groups = list(c()), groupLabels = list(c()), colors = list(c()))}
#' \code{uniqueGroups = data.frame(TMX_GROUP_A, TMX_GROUP_B, TMX_GROUP_C,..., files)}
#' @export
#'
#' @examples
prepareBareGroupings = function(files){
	#browser()
	groupings = data.frame(names = c(), labels = c(), groups = c(), groupLabels = c(), colors = c())
	if(length(files) > 1){
		filesSpl = strsplit(tools::file_path_sans_ext(files), "\\ - |_")
		filesDF = do.call(rbind.data.frame, filesSpl)
		uniqueGroups = data.frame(filesDF[, apply(filesDF, 2, function(x) length(unique(x)) != 1)])
		colnames(uniqueGroups) = paste0("TMX_GROUP_", letterwrap(ncol(uniqueGroups))) #LETTERS[1:ncol(uniqueGroups)]) #seq(1, ncol(uniqueGroups))
		for(i in 1:ncol(uniqueGroups)){
			intermGroups = I(list(unique(uniqueGroups[, i])))
			#Subsetting is done due to minimal value warnings with brewer.pal, which may return more colors than asked for
			intermGroupColors = I(list(generateGroupColor(i, length(intermGroups[[1]]))[1:length(intermGroups[[1]])]))
			groupingsRow = data.frame(names = colnames(uniqueGroups)[i], labels = colnames(uniqueGroups)[i], 
									  groups = intermGroups, groupLabels = intermGroups, 
									  colors = intermGroupColors)
			groupings = rbind(groupings, groupingsRow)
			uniqueGroups[[i]] = factor(uniqueGroups[[i]], levels = groupings$groups[[i]])
		}
		uniqueGroups$files = files
		#browser()
	}else if(length(files) == 1){
		groupings = rbind(groupings, data.frame(names = c("TMX_GROUP_A"), labels = c("Grouping"), 
												groups = list(c("Group")), groupLabels = list(c("Group")), 
												colors = list(c("#000000"))))
		uniqueGroups = data.frame(TMX_GROUP_A = c("Group"), files = files)
	}
	if(is.null(files)){return(groupings)}

	#print(groupings)
	return(list(groupings = groupings, groups = uniqueGroups))
}

toCardinal = function(angles, directionCat, name, browse = FALSE){
	if(browse){browser()}
	anglesCardinal = cut(angles, directionCat)
	anglesCardinal = replace(anglesCardinal, anglesCardinal == last(levels(anglesCardinal)), 
							 first(levels(anglesCardinal)))
	df = tibble(anglesCardinal)
	colnames(df)[1] = name
	return(df)
}

#' Parses files into data structure
#'
#' @param filesDF file data frame from Shiny file upload
#' @param groupings groupings data frame
#' @param groups groups data frame
#' @param recalculate Recalculation option
#' @param browse 
#' @param updateProgress update progress function
#' @param closeProgress close progress function
#' @param initializeProgress initialize progress function
#' @param ... 
#'
#' @return Returns parsed files in a data frame
#' \code{list(tracks = tracks, trajectories = trajectories, features = features, files = fileswUnits)}
#' @export
#'
#' @examples
parseFiles = function(filesDF, groupings, groups, recalculate = FALSE, browse = 0, 
						updateProgress = NULL, closeProgress = NULL, initializeProgress = NULL, ...) {
	
	#browser()
	# Generating file names by first creating all permutations
	#groups = expand.grid(groupings$groups)
	# Setting column names
	#colnames(groups) = groupings$names
	# Finally generating file names
	#groups$files = fileNames
	if(browse == 1){browser()}
	
	files = filesDF$datapath
	fileNames = filesDF$name
	if(is.null(files)){
		cat("No files provided.\n")
		return(NULL)
	}
	
	fileTypes = getTrackingFileType(fileNames)
	if(length(fileTypes) == 1){
		fileType = names(fileTypes)[[1]]
	}else{
		cat("Please provide only one file type.\n")
		return(NULL)
	}
	
	if(length(files) > 0){
		# Creating empty data frames
		features = data.frame()
		trajectories = data.frame()
		tracks = data.frame()
		fileswUnits = data.frame() 
		
		if(is.function(initializeProgress) && is.function(updateProgress)){
			initializeProgress(message = "Parsing TrackMate files...", 
							   max = length(files))
		}
		
		# Loading files...
		for(f in 1:length(files)){
			filePath = files[f]
			
			if(is.function(updateProgress)){
				updateProgress(value = f - 1, detail = paste0("Parsing ", fileNames[f], "..."))
			}
			
			groupText = paste(data.frame(lapply(groups[f, ], as.character), stringsAsFactors=FALSE), collapse = " | ")
			cat(paste(f, "Reading", groupText));cat("\n")
			
			if(fileType == "xml"){
				fileData = parseTMFile(filePath = filePath, groupText = groupText, fileGroup = groups[f, ], 
									   recalculate = recalculate, browse = browse)	
			}else if(fileType == "xlsx" || fileType == "xls"){
				fileData = parseImarisXLSXFile(filePath = filePath, groupText = groupText, fileGroup = groups[f, ], 
											   recalculate = recalculate, browse = browse)
			}else if(fileType == "csv" || fileType == "txt"){
				fileData = parseChemotaxisToolFile(filePath = filePath, groupText = groupText, fileGroup = groups[f, ], 
												   recalculate = recalculate, browse = browse, ...)
			}
			#feats = cbind(feats, groups[r, -ncol(groups)])
			if(is.null(fileData)){
				return(NULL)
			}
			#browser()
			fileswUnits = plyr::rbind.fill(fileswUnits, fileData$unts)
			features = plyr::rbind.fill(features, fileData$feats)
			tracks = plyr::rbind.fill(tracks, fileData$trks)
			trajectories = plyr::rbind.fill(trajectories, fileData$trajs)
			
			if(is.function(updateProgress)){updateProgress(value = f, detail = paste0("Done ", fileNames[f], "..."))}
			
			cat(paste("Reading", groupText, "finished."));cat("\n")
		}
		
		fileswUnits = filesDF %>% left_join(fileswUnits , by = c("name"="files"))
	}
	
	# manual spot and manual edge features have the same name, which causes issues. This is corrected below.
	if(nrow(features[features$type == "Spot" & features$feature == "MANUAL_COLOR", ]) > 0){
		features[features$type == "Spot" & features$feature == "MANUAL_COLOR", ]$feature = "MANUAL_SPOT_COLOR"
	}
	if(nrow(features[features$type == "Edge" & features$feature == "MANUAL_COLOR", ]) > 0){
		features[features$type == "Edge" & features$feature == "MANUAL_COLOR", ]$feature = "MANUAL_EDGE_COLOR"
	}
	
	if(is.function(closeProgress)){closeProgress()}
	return(list(tracks = tracks, trajectories = trajectories, features = features, files = fileswUnits))
}

#' Processes data to calculate missing features
#'
#' @param dataList list of data frames (core data structure)
#' @param groups data frame typically \code{groupings$groups}
#' @param groupings data frame typically \code{groupings$groupings}
#' @param updateProgress update progress function
#' @param initializeProgress initialize progress function
#' @param closeProgress close progress function
#' @param recalculate Recalculate option
#' @param browse 
#' @param benchmark 
#'
#' @return Returns processed data
#' @export
#'
#' @examples
processData = function(dataList, groups, groupings, updateProgress = NULL, initializeProgress = NULL, 
					   closeProgress = NULL, recalculate = FALSE, browse = 0, benchmark = TRUE){#, rotation_fix = 0, rotation_z_fix = 0){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	if(benchmark) startTime = benchMark()
	# Getting data frames
	tracks = dataList$tracks; trajectories = dataList$trajectories; files = dataList$files; features = dataList$features
	tracks$group_id = apply(tracks[, as.character(groupings$names), drop = F], 1, paste, collapse = "_")
	#tracks$group_part_id = apply(tracks[, c(as.character(groupings$names), "Part")], 1, paste, collapse = "_")
	tracks$track_global_id = paste0(tracks$group_id, "_", tracks$TRACK_ID)#tracks$track_global_id = paste0(tracks$group_part_id, "_", tracks$TRACK_ID)
	
	# Setting groups IDs
	files$group_id = apply(files[, as.character(groupings$names), drop = F], 1, paste, collapse = "_")
	features$group_id = apply(features[, as.character(groupings$names), drop = F], 1, paste, collapse = "_")
	
	if(benchmark) startTime = benchMark("Process data - initialization", startTime)
	# Getting features of FIRST group for ALL groups! Preparing fixed positions and rotated positions
	#dimFeatures = positionColumns #features[features$group_id == features$group_id[1], ][2:5, ]$feature 
	#fixedPositionColumns = paste(positionColumns, "FIX", sep = "_")
	#rotFixedPositionsColumns = paste(fixedPositionColumns, "ROT", sep = "_")
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	trajectories$group_id = apply(trajectories[, as.character(groupings$names), drop = F], 1, paste, collapse = "_")
	#trajectories$group_part_id = apply(trajectories[, c(as.character(groupings$names), "Part")], 1, paste, collapse = "_")
	trajectories$track_global_id = paste0(trajectories$group_id, "_", trajectories$TRACK_ID)#trajectories$track_global_id = paste0(trajectories$group_part_id, "_", trajectories$TRACK_ID)
	
	if(is.function(initializeProgress) && is.function(updateProgress)){
		initializeProgress(message = "Processing tracks...", max = length(unique(trajectories$track_global_id)))
	}
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	tracks = tracks %>% group_by(.dots = paste0("`", c(as.character(groupings$names),# "Part", 
													   "group_id"#, "group_part_id"
													   ), "`"))
	features = features %>% group_by(.dots = paste0("`", c(as.character(groupings$names), "group_id"), "`"))
	trajectories = trajectories %>% group_by(.dots = paste0("`", c(as.character(groupings$names), 
																   "group_id", #"group_part_id", 
																   "track_global_id"), "`"))
	files = files %>% group_by(.dots = paste0("`", c(as.character(groupings$names), "group_id"), "`"))
	
	#trajectories2 = trajectories
	#tracks2 = tracks
	
	if(benchmark) startTime = benchMark("Process data - groupings and global id", startTime)
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	cat("Calculating fixed positions of tracks at (0/0)\n")
	
	suppressWarnings({
		# Calculating edge fixed positions, displacements, edge directions, square displacements
		trajectories = trajectories %>% group_by(track_global_id) %>% 
			# element name will be appended automatically and calculation is performed by the element (=function)
			mutate_at(positionColumns, list(FIX = fixTrajectoryStartPos)) %>% 
			mutate_at(positionColumns[1:3], list(DISPLACEMENT = ~lead(.) - .)) %>%
			# Keep -1 * get(.... for y as image pixel coordinates in Y axis are mirrored.
			mutate(EDGE_DIRECTION_PHI = atan2(-1 * get(displacementColumns[2]), 
											  get(displacementColumns[1])) %% (2 * pi), 
				   EDGE_DIRECTION_THETA = acos(get(displacementColumns[3]) / DISPLACEMENT) %% pi)
		trajectories = trajectories %>% group_by(track_global_id) %>% 
			mutate(TURN_ANGLE = lead(EDGE_DIRECTION_PHI) - EDGE_DIRECTION_PHI)
		
		# Calculating trajectory level (square) displacements
		trajectories$SQUARE_DISPLACEMENT_FIX = 
			trajectories[[fixedPositionColumns[1]]] ^ 2 + 
			trajectories[[fixedPositionColumns[2]]] ^ 2 + 
			trajectories[[fixedPositionColumns[3]]] ^ 2
		
		trajectories$DISPLACEMENT_FIX = sqrt(trajectories$SQUARE_DISPLACEMENT_FIX)
		
		sdFeats = trajFeats %>% filter(feature %in% c("SQUARE_DISPLACEMENT_FIX", "DISPLACEMENT_FIX"))
		features = appendNewFeatures(features, sdFeats$feature, sdFeats$name, sdFeats$shortname, sdFeats$dimension,
									 sdFeats$isint, sdFeats$type, groups, groupings$names)
		
		if(benchmark) startTime = benchMark("Process data - traj fixed position, edge displacement and edge directions", startTime)
		
		# Path Length
		if(is.null(tracks$PATH_LENGTH)){
			tracks = tracks %>% 
				left_join(trajectories %>% group_by(track_global_id) %>% 
						  	summarise(PATH_LENGTH = sum(DISPLACEMENT, na.rm = TRUE),
						  			  TRACK_DISPLACEMENT_X = last(POSITION_X) - first(POSITION_X),
						  			  TRACK_DISPLACEMENT_Y = last(POSITION_Y) - first(POSITION_Y),
						  			  TRACK_DISPLACEMENT_Z = last(POSITION_Z) - first(POSITION_Z)), 
						  by = "track_global_id")
		}
		
		# Start and end positions
		for(i in 1:length(positionColumns)){
			if(!(startPositionColumns[i] %in% colnames(tracks))){
				tracks = tracks %>% 
					left_join(trajectories %>% group_by(track_global_id) %>% 
							  summarise(!! startPositionColumns[i] := first(get(positionColumns[i]))), 
							  by = "track_global_id")
			}
		}
		for(i in 1:length(positionColumns)){
			if(!(endPositionColumns[i] %in% colnames(tracks))){
				tracks = tracks %>% 
					left_join(trajectories %>% group_by(track_global_id) %>% 
							  	summarise(!! endPositionColumns[i] := last(get(positionColumns[i]))), 
							  by = "track_global_id")
			}
		}
		
		# Calculating MSD
		tracks = tracks %>%
			left_join(trajectories %>% group_by(track_global_id) %>% 
					  	summarise(MEAN_SQUARE_DISPLACEMENT = mean((lead(DISPLACEMENT_FIX) - DISPLACEMENT_FIX)^2, 
					  													 na.rm = TRUE)), 
					  by = "track_global_id")
		
		msdFeats = trackFeats %>% filter(feature == "MEAN_SQUARE_DISPLACEMENT")
		features = appendNewFeatures(features, msdFeats$feature, msdFeats$name, msdFeats$shortname, msdFeats$dimension,
									 msdFeats$isint, msdFeats$type, groups, groupings$names)
		
		# Calculating Angular Persistence
		tracks = tracks %>%
			left_join(trajectories %>% group_by(track_global_id) %>% 
					  	summarise(ANGULAR_PERSISTENCE = mean(abs(TURN_ANGLE), na.rm = TRUE)),
					  by = "track_global_id")
		angPersFeats = featsPersistence %>% filter(feature == "ANGULAR_PERSISTENCE")
		features = appendNewFeatures(features, angPersFeats$feature, angPersFeats$name, angPersFeats$shortname, 
									 angPersFeats$dimension, angPersFeats$isint, angPersFeats$type, groups, 
									 groupings$names)
																		
		tracks$TRACK_VELOCITY = tracks$TRACK_DISPLACEMENT / tracks$TRACK_DURATION
		tracks$STRAIGHTNESS = tracks$TRACK_DISPLACEMENT / tracks$PATH_LENGTH
		if(benchmark) startTime = benchMark("Process data - path length and velocity", startTime)
		# Calculating rotated fixed position
		trajectories = trajectories %>%
			dplyr::mutate(rotateFixedPosition(
				get(fixedPositionColumns[1]), get(fixedPositionColumns[2]), get(fixedPositionColumns[3])))
		if(benchmark) startTime = benchMark("Process data - traj rotation", startTime)
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		
		# tracks2$TRACK_DIRECTION = NA; tracks2$TRACK_DIRECTION_Z = NA
		# tracks2$PATH_LENGTH = NA; tracks2$TRACK_VELOCITY = NA
		# 
		# trajectories2$POSITION_X_FIX_ROT = trajectories2$POSITION_X_FIX; trajectories2$POSITION_Y_FIX_ROT = trajectories2$POSITION_Y_FIX; trajectories2$POSITION_Z_FIX_ROT = trajectories2$POSITION_Z_FIX 
		# 
		# cat("Calculating rotationally fixed positions of tracks at (0/0)")
		# 
		# trajectories2[, fixedPositionColumns] = NA
		# for(track in unique(trajectories2$track_global_id)){
		# 	trackPos = trajectories2[trajectories2$track_global_id == track, positionColumns]
		# 	trajectories2[trajectories2$track_global_id == track, fixedPositionColumns] =
		# 		trackPos - trackPos[rep(1, nrow(trackPos)), ]
		# }
		# if(benchmark) startTime = benchMark("Process data - for loop fixed pos", startTime)
		# 
		# # Path length
		# #trajectories = a %>% group_by(track_global_id) %>% mutate(EDGE_DIRECTION = )
		# trackNames = unique(trajectories2$track_global_id)
		# if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		# for(i in 1:length(trackNames)){
		# 	track = trackNames[i]
		# 	displacements = trajectories2$DISPLACEMENT[trajectories2$track_global_id == track]
		# 	progress = paste(i, "of", length(trackNames), paste0(round(i*100/length(trackNames), digits = 0), "%"), track, "\n")
		# 	cat(progress)
		# 	if(is.function(initializeProgress) && is.function(updateProgress)){
		# 		updateProgress(value = i - 1, detail = progress)
		# 	}
		# 	trackFixPos = trajectories2[trajectories2$track_global_id == track, fixedPositionColumns]
		# 	trackSel = tracks2$track_global_id == track
		# 	r = sqrt(rowSums(trackFixPos[, 1:3] ^ 2))
		# 	
		# 	phi = atan2(y = trackFixPos$POSITION_Y_FIX, x = trackFixPos$POSITION_X_FIX)
		# 	direction = phi[length(phi)] %% (2*pi)
		# 	#ROT phi = phi + ud.convert(rotation_fix, "degree", "radian"); phi[is.nan(phi)] = 0
		# 	#ROT direction_rot = phi[length(phi)] %% (2*pi)
		# 	
		# 	theta = acos(trackFixPos$POSITION_Z_FIX / r); theta[is.nan(theta)] = 0
		# 	direction_z = theta[length(theta)] %% (pi)
		# 	#ROT theta = theta + ud.convert(rotation_z_fix, "degree", "radian"); theta[is.nan(theta)] = 0
		# 	#ROT direction_z_rot = theta[length(theta)] %% (pi)
		# 	
		# 	#ROT trajSel = trajectories$track_global_id == track
		# 	#ROT trajectories[trajSel, ]$POSITION_X_FIX_ROT = r * cos(phi) * sin(theta)
		# 	#ROT trajectories[trajSel, ]$POSITION_Y_FIX_ROT = r * sin(phi) * sin(theta)
		# 	#ROT trajectories[trajSel, ]$POSITION_Z_FIX_ROT = r * cos(theta)
		# 	
		# 	
		# 	if(nrow(tracks2[trackSel, ]) == 1){
		# 		tracks2[trackSel, ]$TRACK_DIRECTION = direction
		# 		#ROT tracks2[trackSel, ]$TRACK_DIRECTION_ROT = direction_rot
		# 		tracks2[trackSel, ]$TRACK_DIRECTION_Z = direction_z
		# 		#ROT tracks2[trackSel, ]$TRACK_DIRECTION_Z_ROT = direction_z_rot
		# 		
		# 		tracks2[trackSel, ]$PATH_LENGTH = sum(displacements[-length(displacements)])
		# 		tracks2[trackSel, ]$TRACK_VELOCITY = tracks2[trackSel, ]$TRACK_DISPLACEMENT / tracks2[trackSel, ]$TRACK_DURATION
		# 	}
		# 	if(is.function(updateProgress)){updateProgress(value = i, detail = progress)}
		# 	
		# }
		# if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		# tracks2$PERSISTENCE = tracks2$TRACK_DISPLACEMENT / tracks2$PATH_LENGTH
		# if(benchmark) startTime = benchMark("Process data - for loop", startTime)
		#browser()
		
		# Calculating track directions
		tracks = tracks %>% 
			left_join(trajectories %>% 
					  	group_by(track_global_id) %>% 
					  	summarise(TRACK_DIRECTION = atan2(-1 * last(get(fixedPositionColumns[2])), 
					  									  last(get(fixedPositionColumns[1]))) %% (2 * pi)), 
					  by = "track_global_id")
		tracks = tracks %>% 
			left_join(trajectories %>% 
					  	group_by(track_global_id) %>% 
					  	summarise(TRACK_DIRECTION_Z = last(get(fixedPositionColumns[3]))), 
					  by = "track_global_id")
		
		tracks$TRACK_DIRECTION_Z = acos(tracks$TRACK_DIRECTION_Z / tracks$TRACK_DISPLACEMENT) %% pi
		#browser()
		tracks = tracks %>% mutate(toCardinal(get("TRACK_DIRECTION"), directionCat, 
											  unlist(names(cardinalCols)[cardinalCols == "TRACK_DIRECTION"])))
	
		# tracks = tracks %>% 
		# 	mutate(DIRECTION_CARDINAL = cut(TRACK_DIRECTION, directionCat)) %>% #mutate(across(starts_with("DIRECTION_POINT_SOURCE"), ~ cut(., directionCat), .names = "{.col}_CARDINAL")) %>% 
		# 	mutate(DIRECTION_CARDINAL = replace(DIRECTION_CARDINAL, 
		# 										DIRECTION_CARDINAL == last(levels(DIRECTION_CARDINAL)), 
		# 										first(levels(DIRECTION_CARDINAL))))
		# 
		#tracks$DIRECTION_CARDINAL = cut(tracks$TRACK_DIRECTION, directionCat)
		
		
		#ROT tracks$DIRECTION_CARDINAL_ROT = cut(tracks$TRACK_DIRECTION_ROT, directionCat)
		
		# Setting last group to first group
		#tracks = angleCategoryFix(tracks, c("DIRECTION_CARDINAL"))#ROT , "DIRECTION_CARDINAL_ROT"))
		# tracks = tracks %>% 
		# 	mutate(DIRECTION_CARDINAL = replace(DIRECTION_CARDINAL, 
		# 		   			DIRECTION_CARDINAL == last(levels(tracks$DIRECTION_CARDINAL)), 
		# 		   			first(levels(tracks$DIRECTION_CARDINAL))))
		# 
		
		
		tracks$TRACK_DIRECTION_ROT = tracks$TRACK_DIRECTION; tracks$TRACK_DIRECTION_Z_ROT = tracks$TRACK_DIRECTION_Z
		tracks$DIRECTION_CARDINAL_ROT = tracks$DIRECTION_CARDINAL#; tracks$DIRECTION_CARDINAL_Z_ROT = tracks$DIRECTION_CARDINAL_Z
		
		
		if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
		for(grouping in groupings$names){
			tracks[[grouping]] = as.factor(tracks[[grouping]])
		}
	})
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	#browser()
	tracks = setUnits(tracks, features[features$type == "Track", ], files)
	trajectories = setUnits(trajectories, features[features$type == "Spot" | features$type == "Edge", ], files)
	dataList$tracks = tracks; dataList$trajectories = trajectories; dataList$features = features; dataList$files = files
	if(is.function(closeProgress)){closeProgress()}
	return(dataList)
}

#' Sets the stroke color aesthetics of the plot
#'
#' @param plot ggplot object
#' @param data data frame of the data
#' @param groupings groupings data frame
#' @param groupName group column name in the data
#' @param alpha opacity
#' @param is.dark dark plot option
#' @param colorAsFeature whether or not color is a feature measure (e.g. color coding of a measure like speed)
#'
#' @return Returns back the ggplot object
#' @export
#'
#' @examples
colorPlot = function(plot, data, groupings, groupName, alpha, is.dark = FALSE, colorAsFeature = FALSE){
	if(!is.null(groupName)){
		if(!colorAsFeature){
			colorValues = alpha(getGColors(groupings, groupName, order = levels(data[[groupName]])), alpha = alpha)	
			if(is.dark){
				colorValues = invertedColors(colorValues, ignore.alpha = TRUE)
			}
			plot = plot + scale_color_manual(values = colorValues, 
											 labels = getGLabs(groupings = groupings, name = groupName, 
											 				  order = levels(data[[groupName]])))
			plot = plot + labs(color = as.character(getGLab(groupings, groupName)))
		}else{
			colorValues = inferno(n = 256)
			if(is.dark){
				colorValues = invertedColors(colorValues, ignore.alpha = TRUE)
			}
			plot = plot + scale_color_gradientn(colours = colorValues)
		}
	}
	return(plot)
}

#' Sets the fill color aesthetics of the plot
#'
#' @param plot ggplot object
#' @param data data frame of the data
#' @param groupings groupings data frame
#' @param groupName group column name in the data
#' @param alpha opacity
#' @param is.dark dark plot option
#'
#' @return Returns back the ggplot object
#' @export
#'
#' @examples
fillPlot = function(plot, data, groupings, groupName, alpha, is.dark = FALSE){
	if(!is.null(groupName)){
		fillValues = alpha(getGColors(groupings, groupName, order = levels(data[[groupName]])), alpha = alpha)	
		if(is.dark){
			fillValues = invertedColors(fillValues, ignore.alpha = FALSE)
		}
		plot = plot + scale_fill_manual(values = fillValues, 
										 labels = getGLabs(groupings = groupings, name = groupName, 
										 				  order = levels(data[[groupName]])))
		plot = plot + labs(fill = as.character(getGLab(groupings, groupName)))
	}
	return(plot)
}

#Taken from https://rdrr.io/cran/oce/src/R/misc.R
#' Maps 0:360° to -180:0:180°
#'
#' @param theta Angle to map
#'
#' @return Returns remapped angle value
#' @export
#'
#' @examples
angleRemap = function(theta){
	atan2(sin(theta), cos(theta))
}

#' Calculates point source directionalities of the particles
#'
#' @param dataList List of data frames \code{list(tracks, trajectories, files, features)}
#' @param updateProgress 
#' @param initializeProgress 
#' @param closeProgress 
#' @param browse For debugging purposes
#'
#' @return Returns dataList with additional point source data
#' @export
#'
#' @examples
pointSource = function(dataList, updateProgress = NULL, initializeProgress = NULL, closeProgress = NULL, browse = 0){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	tracks = dataList$tracks; trajectories = dataList$trajectories; files = dataList$files; features = dataList$features
	if(is.function(initializeProgress) && is.function(updateProgress)){
		initializeProgress(message = "Calculating point source directionalities...", max = 1)
	}
	suppressWarnings({
		trajsPS = trajectories %>% 
			left_join(files %>%  ungroup() %>% select(name, starts_with("pointSource_")), by = c("files" = "name"), 
					  suffix = c("", ".y")) %>% select(-ends_with(".y")) %>%
			group_by(track_global_id) %>%
			mutate(EDGE_DIRECTION_POINT_SOURCE_PHI = angleRemap(EDGE_DIRECTION_PHI %% (pi*2) - 
																	atan2(POSITION_Y - pointSource_y, 
																		  pointSource_x - POSITION_X) %% (pi*2)),
				   EDGE_DIRECTION_POINT_SOURCE_THETA = EDGE_DIRECTION_THETA - 
				   	acos((get(pointSourceColumns[3]) - get(positionColumns[3])) / DISPLACEMENT))
		trajsPS$EDGE_DEVIATION_FROM_POINT_SOURCE_PHI = abs(trajsPS$EDGE_DIRECTION_POINT_SOURCE_PHI)
		trajsPS$EDGE_DEVIATION_FROM_POINT_SOURCE_THETA = abs(trajsPS$EDGE_DIRECTION_POINT_SOURCE_THETA)
		
		trcksPS = tracks %>%
			left_join(files %>%  ungroup() %>% select(name, starts_with("pointSource_")), by = c("files" = "name"), 
					  suffix = c("", ".y")) %>% select(-ends_with(".y")) %>%
			group_by(track_global_id) %>%
			mutate(DIRECTION_POINT_SOURCE_PHI = angleRemap(TRACK_DIRECTION %% (2*pi) - 
														   	atan2(START_POSITION_Y - pointSource_y, 
														   		  pointSource_x - START_POSITION_X) %% (2*pi)),
				   DIRECTION_POINT_SOURCE_THETA = TRACK_DIRECTION_Z - 
				   	acos((get(pointSourceColumns[3]) - START_POSITION_Z) / 
				   		 	sqrt((get(pointSourceColumns[1]) - START_POSITION_X) ^ 2 + 
				   		 		 	(get(pointSourceColumns[2]) - START_POSITION_Y) ^ 2 + 
				   		 		 	(get(pointSourceColumns[3]) - START_POSITION_Z) ^ 2)
				   )
			)
		
		trcksPS = trcksPS %>% 
			left_join(trajsPS %>% 
					  	group_by(track_global_id) %>% 
					  	summarise(MEAN_DEVIATION_FROM_POINT_SOURCE_PHI = mean(EDGE_DEVIATION_FROM_POINT_SOURCE_PHI, 
					  														  na.rm = TRUE),
					  			  MEAN_DEVIATION_FROM_POINT_SOURCE_THETA = mean(EDGE_DEVIATION_FROM_POINT_SOURCE_THETA, 
					  			  											  na.rm = TRUE)), 
					  by = "track_global_id", suffix = c("", ".y")) %>% select(-ends_with(".y"))
		
		if(is.function(initializeProgress) && is.function(updateProgress)){
			updateProgress(detail = "Point source directons calculated...", value = 1)
		}
		
		trajectories$EDGE_DIRECTION_POINT_SOURCE_PHI = trajsPS$EDGE_DIRECTION_POINT_SOURCE_PHI %% (2 * pi)
		trajectories$EDGE_DIRECTION_POINT_SOURCE_THETA = trajsPS$EDGE_DIRECTION_POINT_SOURCE_THETA %% (2 * pi)
		trajectories$EDGE_DEVIATION_FROM_POINT_SOURCE_PHI = trajsPS$EDGE_DEVIATION_FROM_POINT_SOURCE_PHI# %% (2 * pi)
		trajectories$EDGE_DEVIATION_FROM_POINT_SOURCE_THETA = trajsPS$EDGE_DEVIATION_FROM_POINT_SOURCE_THETA# %% (2 * pi)
		
		
		tracks$DIRECTION_POINT_SOURCE_PHI = trcksPS$DIRECTION_POINT_SOURCE_PHI %% (2 * pi)
		tracks$DIRECTION_POINT_SOURCE_THETA = trcksPS$DIRECTION_POINT_SOURCE_THETA %% (2 * pi)
		tracks$MEAN_DEVIATION_FROM_POINT_SOURCE_PHI = trcksPS$MEAN_DEVIATION_FROM_POINT_SOURCE_PHI# %% (2 * pi)
		tracks$MEAN_DEVIATION_FROM_POINT_SOURCE_THETA = trcksPS$MEAN_DEVIATION_FROM_POINT_SOURCE_THETA# %% (2 * pi)
		#TODO use toCardinal function
		#toCardinal(., directionCat, )
		tracks = tracks %>% 
			mutate(toCardinal(get("DIRECTION_POINT_SOURCE_PHI"), directionCat, 
							  unlist(names(cardinalCols)[cardinalCols == "DIRECTION_POINT_SOURCE_PHI"])))
		tracks = tracks %>% 
			mutate(toCardinal(get("DIRECTION_POINT_SOURCE_THETA"), directionCat, 
							  unlist(names(cardinalCols)[cardinalCols == "DIRECTION_POINT_SOURCE_THETA"])))
		
		trajectories = trajectories %>% 
			mutate(toCardinal(get("EDGE_DIRECTION_POINT_SOURCE_PHI"), directionCat, 
							  unlist(names(cardinalCols)[cardinalCols == "EDGE_DIRECTION_POINT_SOURCE_PHI"])))
		trajectories = trajectories %>% 
			mutate(toCardinal(get("EDGE_DIRECTION_POINT_SOURCE_THETA"), directionCat, 
							  unlist(names(cardinalCols)[cardinalCols == "EDGE_DIRECTION_POINT_SOURCE_THETA"])))
		
		# trajectories = 
		# 		trajectories %>% mutate(across(starts_with("EDGE_DIRECTION_POINT_SOURCE"), ~ cut(., directionCat),
		# 									   .names = "{.col}_CARDINAL")) %>%
		# 		mutate(across(matches("POINT_SOURCE_.*_CARDINAL"), ~ replace(., . == last(levels(.)), first(levels(.)))))
		# tracks = tracks %>% 
		# 	mutate(across(starts_with("DIRECTION_POINT_SOURCE"), ~ cut(., directionCat), .names = "{.col}_CARDINAL")) %>% 
		# 	mutate(across(matches("POINT_SOURCE_.*_CARDINAL"), ~ replace(., . == last(levels(.)), first(levels(.)))))
		# trajectories = 
		# 	trajectories %>% mutate(across(starts_with("EDGE_DIRECTION_POINT_SOURCE"), ~ cut(., directionCat), 
		# 								   .names = "{.col}_CARDINAL")) %>% 
		# 	mutate(across(matches("POINT_SOURCE_.*_CARDINAL"), ~ replace(., . == last(levels(.)), first(levels(.)))))
		
		# Setting last group to first group
		tracks = tracks
		trajectories = trajectories
		
		#TODO cardinal!!!!

		features = features %>% 
			ungroup() %>%
			slice(1) %>%
			select(-names(pointSourceFeats)) %>%  
			tidyr::uncount(nrow(pointSourceFeats))  %>%     
			bind_cols(pointSourceFeats) %>%
			bind_rows(features, .) %>% 
			ungroup() %>%
			complete(nesting(!!! syms(names(pointSourceFeats))), 
					 nesting(!!! syms(setdiff(names(.), names(pointSourceFeats)))))
		
		options(warn=1)
		tracks = setUnits(tracks, features[features$type == "Track", ], files)
		trajectories = setUnits(trajectories, features[features$type == "Spot" | features$type == "Edge", ], files)
	})
	dataList$tracks = tracks; dataList$trajectories = trajectories; dataList$features = features; dataList$files = files
	
	if(is.function(closeProgress)){closeProgress()}
	return(dataList)
}

#' Rotates tracks
#'
#' @param dataList List of data frames \code{list(tracks, trajectories, files, features)}
#' @param updateProgress 
#' @param initializeProgress 
#' @param closeProgress 
#' @param browse 
#' @param rotation_fix Angle to fix directionalities to. This angle will be the new 0° in the rotated tracks (XY)
#' @param rotation_z_fix Angle to fix directionalities to. This angle will be the new 0° in the rotated tracks (Z)
#'
#' @return
#' @export
#'
#' @examples
rotateTracks = function(dataList, updateProgress = NULL, initializeProgress = NULL, closeProgress = NULL, browse = 0, 
			 rotation_fix = 0, rotation_z_fix = 0){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	# Getting data frames
	tracks = dataList$tracks; trajectories = dataList$trajectories; files = dataList$files; features = dataList$features
	if(is.function(initializeProgress) && is.function(updateProgress)){
		initializeProgress(message = "Rotating tracks...", max = length(unique(trajectories$track_global_id)))
	}
	
	cat("Calculating rotations")
	trackNames = unique(trajectories$track_global_id)
	
	tracks$TRACK_DIRECTION_ROT = ((tracks$TRACK_DIRECTION + ud.convert(rotation_fix, "degree", "radian")) %% (2*pi))
	tracks$DIRECTION_CARDINAL_ROT = cut(tracks$TRACK_DIRECTION_ROT, directionCat)
	
	fixedPos = trajectories[, fixedPositionColumns]
	
	r = sqrt(rowSums(fixedPos[, 1:3] ^ 2))
	phi = atan2(y = fixedPos$POSITION_Y_FIX, x = fixedPos$POSITION_X_FIX) + ud.convert(rotation_fix, "degree", "radian")
	phi[is.nan(phi)] = 0; phi = phi %% (2*pi)
	theta = acos(fixedPos$POSITION_Z_FIX / r) + ud.convert(rotation_z_fix, "degree", "radian");
	theta[is.nan(theta)] = 0; theta %% (pi)
	#direction = phi[length(phi)] %% (2*pi)
	#ROT phi = phi + ud.convert(rotation_fix, "degree", "radian"); phi[is.nan(phi)] = 0
	#ROT direction_rot = phi[length(phi)] %% (2*pi)
	trajectories$POSITION_X_FIX_ROT = r * cos(phi) * sin(theta)
	trajectories$POSITION_Y_FIX_ROT = r * sin(phi) * sin(theta)
	trajectories$POSITION_Z_FIX_ROT = r * cos(theta)
	#direction_z = theta[length(theta)] %% (pi)
	# 
	# if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	# for(i in 1:length(trackNames)){
	# 	track = trackNames[i]
	# 	
	# 	#trackFixPos = trajectories[trajectories$track_global_id == track, fixedPositionColumns]
	# 	trackSel = tracks$track_global_id == track
	# 	
	# 	progress = paste(i, "of", length(trackNames), paste0(round(i*100/length(trackNames), digits = 0), "%"), track, "\n")
	# 	cat(progress)
	# 	if(is.function(initializeProgress) && is.function(updateProgress)){
	# 		updateProgress(value = i - 1, detail = progress)
	# 	}
	# 	
	# 	
	# 	phi = unlist(tracks[trackSel, "TRACK_DIRECTION"] + ud.convert(rotation_fix, "degree", "radian")); phi[is.nan(phi)] = 0
	# 	direction_rot = phi[length(phi)] %% (2*pi)
	# 	
	# 	theta = unlist(tracks[trackSel, "TRACK_DIRECTION_Z"]  + ud.convert(rotation_z_fix, "degree", "radian")); theta[is.nan(theta)] = 0
	# 	direction_z_rot = theta[length(theta)] %% (pi)
	# 	
	# 	trajSel = trajectories$track_global_id == track # selection of 1 track only
	# 	r = sqrt(rowSums(trajectories[trajSel, fixedPositionColumns][, 1:3] ^ 2)) # r per each time point of the track
	# 	phi = traj
	# 	trajectories[trajSel, ]$POSITION_X_FIX_ROT = r * cos(phi) * sin(theta)
	# 	trajectories[trajSel, ]$POSITION_Y_FIX_ROT = r * sin(phi) * sin(theta)
	# 	trajectories[trajSel, ]$POSITION_Z_FIX_ROT = r * cos(theta)
	# 	
	# 	if(is.function(updateProgress)){updateProgress(value = i, detail = progress)}
	# }
	# 
	# # Generating features of rotated positions if absent
	# if(nrow(features[grepl("POSITION_.*_ROT", features$feature), ]) > 0){
	# 	rotatedFeatures = features[grepl("POSITION_.*_FIX", features$feature), ]
	# 	rotatedFeatures = rotatedFeatures[rotatedFeatures$dimension == "POSITION", ]
	# 	rotatedFeatures$feature = paste(rotatedFeatures$feature, "ROT", sep = "_")
	# 	rotatedFeatures$name = paste(rotatedFeatures$name, "and rotated")
	# 	rotatedFeatures$shortname = paste(rotatedFeatures$shortname, "rotated")
	# 	features = rbind(features, rotatedFeatures)
	# }
	
	dataList$tracks = tracks; dataList$trajectories = trajectories; dataList$features = features; dataList$files = files
	if(is.function(closeProgress)){closeProgress()}
	return(dataList)
}

#' Generates named list for choices (features data frame) e.g. to be used in Shiny selectInput
#'
#' @param type Type of features (one of "Track", "Edge", "Spot")
#' @param features Features data frame
#' @param include Select features by text based matching. If include is substring of the feature$feature, then only 
#' those features will be included. If not needed leave at the default value \code{NULL}.
#' @param short Option to pick short names as names of the list
#' @param empty Include an empty value \code{list(`Do Not Group` = NULL)}
#'
#' @return Returns a list with name/value pairs
#' @export
#'
#' @examples
choicesInNamedList = function(type, features, include = NULL, short = FALSE, empty = FALSE){
	if(!is.null(features)){
		#browser()
		label = "name"
		if(short){label = "shortname"}
		
		# Selecting rows containing desired columns
		if(is.null(include)){select = rep(TRUE, nrow(features))}else{select = grepl(include, features$feature)}
		
		choicesDF = features[features$type %in% type & select, c("feature", label)] %>% dplyr::distinct()
		
		values = choicesDF$feature
		names = choicesDF[[label]]
		if(empty){
			values = c('NULL', values)
			names = c('Do Not Group', names)
		}
		
		if(length(values) == length(names)){
			names(values) = names
			return(as.list(values))
		}else{
			print("choicesInNamedList: length of values and names are not equal")
		}
	}else{
		print("choicesInNamedList: features is NULL")
	}
	return(list())
}

# featuresToNamedList = function(type, features, short = FALSE, empty = FALSE){
# 	print("featuresToNamedList = function(type, features, short = FALSE, empty = FALSE){")
# 	label = "name"
# 	if(short){label = "shortname"}
# 	choicesDF = features[features$type %in% type, c("feature", label)]
# 	values = unique(choicesDF$feature)
# 	names = unique(choicesDF[[label]])
# 	if(empty){
# 		values = c('NULL', values)
# 		names = c('Do Not Group', names)
# 	}
# 	#browser()
# 	names(values) = names
# 	return(as.list(values))
# }

# trajectoryPositionNamedList = function(type, features, dim, short = FALSE, empty = FALSE){
# 	print("trajectoryPositionNamedList = function(type, features, dim, short = FALSE, empty = FALSE){")
# 	label = "name"
# 	if(short){label = "shortname"}
# 	choicesDF = features[features$type %in% type & grepl(paste("POSITION", toupper(dim), sep = "_"), features$feature), 
# 						 c("feature", label)]
# 	values = unique(choicesDF$feature)
# 	names = unique(choicesDF[[label]])
# 	if(empty){
# 		values = c('NULL', values)
# 		names = c('Do Not Group', names)
# 	}
# 	#browser()
# 	names(values) = names
# 	return(as.list(values))
# }


#' Generates a named list out of groupings names and labels
#'
#' @param groupings Groupings data frame (must contain columns \code{names} and \code{labels})
#' @param empty Include an empty value \code{list(`Do Not Group` = NULL)}
#' @param doNotDisplay Include a placeholder value \code{list(`Do Not Display` = NA)}
#'
#' @return Returns a list with name value pairs.
#' @export
#'
#' @examples
groupingsToNamedList = function(groupings, empty = FALSE, doNotDisplay = FALSE){
	#print("groupingsToNamedList = function(groupings, empty = FALSE){")
	groupingsCols = c("names", "labels")
	# check if all columns exist
	if(all(is.element(groupingsCols, colnames(groupings)))){
		choicesDF = groupings[, ]
		values = as.vector(choicesDF$names)
		names = as.vector(choicesDF$labels)
		if(empty){
			values = c('NULL', values)
			names = c('Do Not Group', names)
		}
		if(doNotDisplay){
			values = c('NA', values)
			names = c('Do Not Display', names)
		}
		#browser()
		names(values) = names
		return(as.list(values))
	}else{
		return(NULL)
	}
}

# testPlot = function(tracks, x, y){
# 	ggplot(data = tracks, aes(x = !!sym(x), y = !!sym(y))) + geom_violin()
# }

# Adapted from http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html
# Erwan Le Pennec January 2016
# coord_radar = function (theta = "x", start = 0, direction = 1) {
# 	theta = match.arg(theta, c("x", "y"))
# 	r = if (theta == "x") "y" else "x"
# 	ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, direction = sign(direction), 
# 			is_linear = function(coord) TRUE)
# }

# angleCategoryFix = function(data, cols){
# 	for(col in cols){
# 		angleCat = data[[col]]
# 		angleLevels = levels(angleCat)
# 		if(length(angleLevels) == 13){
# 			firstAngleCat = angleLevels[1]
# 			lastAngleCat = angleLevels[length(angleLevels)]
# 			data[angleCat == lastAngleCat, col] = firstAngleCat
# 			data[[col]] = droplevels(data[[col]])
# 			levels(data[[col]]) <- angleLevels[1:12]
# 		}
# 	}
# 	return(data)
# }

# permanentAttributes = function(data){
# 	return(data.frame(lapply(data, function(x) {structure(x, class = c("avector", class(x)))})))
# }


#' Formulates a formula based on term labels
#' Current 3.5.3 version reformulate function can't handle termlabels with spaces. This is a simple fix
#'
#' @param termlabels character vector giving the right-hand side of a model formula. Cannot be zero-length.
#' @param response character string, symbol or call giving the left-hand side of a model formula, or NULL.
#' @param intercept logical: should the formula have an intercept?
#'
#' @return Returns a formula
#' @export
#'
#' @examples
reformulateT = function (termlabels, response=NULL, intercept = TRUE) {
	if(!is.character(termlabels) || !length(termlabels))
		stop("'termlabels' must be a character vector of length at least one")
	has.resp <- !is.null(response)
	
	termtext <- paste(if(has.resp) "response", if(has.resp) "~",
					  paste(paste0("`", termlabels, "`"), collapse = "+"),
					  collapse = "")
	if(!intercept) termtext <- paste(termtext, "- 1")
	rval <- eval(parse(text = termtext, keep.source = FALSE)[[1L]])
	if(has.resp) rval[[2L]] <-
		if(is.character(response)) as.symbol(response) else response
	## response can be a symbol or call as  Surv(ftime, case)
	environment(rval) <- parent.frame()
	rval
}

#' Sets units in the data as attributes of columns (time, spatial and related units)
#'
#' @param data data frame to set units of columns
#' @param dims features data frame to derive units (\code{units$group_id}, \code{units$feature} & 
#' \code{units$dimension})
#' @param units data frame to contain units in columns (\code{units$spatialunits} & \code{units$timeunits}) for each 
#' file. However, only first row is used. This is a 
#' limitation. This function doesn't accept different formulas for different files.
#'
#' @return Returns data with attributes set.
#' @export
#'
#' @examples
setUnits = function(data, dims, units){
	dims = dims[dims$group_id == unique(dims$group_id)[1], ]
	spatialUnit = as.character(units$spatialunits[1]); timeUnit = as.character(units$timeunits[1]);
	for(i in 1:nrow(dims)){
		dim = as.character(dims$dimension[i])
		
		if(dim == "POSITION" || dim == "LENGTH"){
			unitstr = spatialUnit
		}else if(dim == "TIME"){
			unitstr = timeUnit
		}else if(dim == "ANGLE"){
			unitstr = "radian"
		}else if(dim == "VELOCITY"){
			#browser()
			unitstr = paste0(spatialUnit, "/", timeUnit)
		}else{
			unitstr = NULL
		}
		
		if(!is.null(unitstr)){
			if(dims$feature[i] %in% colnames(data)){
				data[[dims$feature[i]]] = sticky(structure(data[[dims$feature[i]]]))
				attr(data[[dims$feature[i]]], "unit") = unitstr
			}else{
				warning(paste0(dims$feature[i], "/", dims$dimension[i], " not in data!\n"))
			}
		}
	}
	return(data)
}

#' Get feature label by name
#' 
#' @param features a data frame containing feature information 
#' @param name a string - name of feature
#' @return Label of feature
#' @examples
#' getFeatureLab(features, y)
#' getFeatureLab(features, "TRACK_MEAN_SPEED")
getFeatureLab = function(features, name){
	return(features$name[features$feature==name][1])
}

#' Get grouping label by name
#' 
#' @param groupings a data frame containing grouping information 
#' @param name a string - name of grouping
#' @param order ordering vector
#' @return Label of grouping
#' @examples
#' getGLab(groupings, x)
#' getGLab(groupings, fillGroupName)
getGLab = function(groupings, name, order = NULL){
	if(is.null(order)){
		return(groupings$labels[groupings$names==name])
	}else{
		labels = groupings$labels[groupings$names==name]
		names = groupings$labels[groupings$names==name]
		return(labels[match(order, names)])
	}
}

#' Get group labels of a grouping by name
#' 
#' @param groupings a data frame containing grouping information 
#' @param name a string - name of grouping
#' @param order ordering vector
#' @return Labels of groups of the given grouping
#' @examples
#' getGLabs(groupings, x)
#' getGLabs(groupings, fillGroupName)
getGLabs = function(groupings, name, order = NULL){
	groupLabels = unlist(groupings$groupLabels[groupings$names==name])
	if(is.null(order)){
		return(groupLabels)
	}else{
		groups = unlist(groupings$groups[groupings$names==name])
		groupLabelsOrdered = groupLabels
		
		i = 1
		for(element in order){
			ind = which(groups == element)
			groupLabelsOrdered[i] = groupLabels[ind]
			i = i + 1
		}
		return(groupLabelsOrdered)
	}
}

#' Gets the groups of a grouping
#'
#' @param groupings Groupings data frame
#' @param name Grouping name
#'
#' @return Returns the groups of the grouping
#' @export
#'
#' @examples
getGroups = function(groupings, name){
	return(unlist(groupings$groups[groupings$names==name]))
}

#' Gets the group colors of a grouping
#'
#' @param groupings Groupings data frame
#' @param name Grouping name
#'
#' @return Returns the group colors of the grouping
#' @export
#'
#' @examples
getGColors = function(groupings, name, order = NULL){
	colors = unlist(groupings$colors[groupings$names==name])
	if(is.null(order)){
		return(colors)
	}else{
		#browser()
		groups = unlist(groupings$groups[groupings$names==name])
		colorsOrdered = colors
		
		i = 1
		for(element in order){
			ind = which(groups == element)
			colorsOrdered[i] = colors[ind]
			i = i + 1
		}
		return(colorsOrdered)
	}
}

#' Converts the column name into an expression It will be checked if it is \code{NULL} or \code{NA}
#'
#' @param columnName character to be converted to symbol.
#'
#' @return Returns \code{sym(columnName)}, \code{NULL} or \code{NA}
#' @export
#'
#' @examples
nameToExpr = function(columnName){
	if(!is.null(columnName) && !is.na(columnName)) columnName = sym(columnName)
	return(columnName)
}

# alphaOpaque = function(x){
# 	return(as.numeric(is.na(x)))
# }

#' Adds a faceting to the plot object
#'
#' @param p ggplot2 object
#' @param row Row faceting variable
#' @param col Col faceting variable
#' @param groupings Groupings data frame
#' @param wrap Whether or not wrap instead of grid style
#'
#' @return Returns the p object with the facet added
#' @export
#'
#' @examples
facetPlot = function(p, row, col, groupings = NULL, wrap = FALSE){
	#browser()
	# If both facet variables are NULL, then don't do any faceting, otherwise check if any of them is null
	if(!is.null(col) || !is.null(row)){
		if(is.null(col)) col = "."
		if(is.null(row)) row = "."
		if(!is.null(groupings)){
			rowLabels = as.character(getGLabs(groupings = groupings, name = row))
			if(!is.null(rowLabels)){
				names(rowLabels) = getGroups(groupings = groupings, name = row)
			}else{
				rowLabels = c()
			}
			
			colLabels = as.character(getGLabs(groupings = groupings, name = col))
			if(!is.null(colLabels)) {
				names(colLabels) = getGroups(groupings = groupings, name = col)
			}else{
				colLabels = c()
			}
			if((col == "." || row == ".") && wrap){
				p = p + facet_wrap(reformulateT(col, row), labeller = as_labeller(c(rowLabels, colLabels)))
			}else{
				p = p + facet_grid(reformulateT(col, row), labeller = as_labeller(c(rowLabels, colLabels)))	
			}
		}else{
			if((col == "." || row == ".") && wrap){
				p = p + facet_wrap(reformulateT(col, row))
			}else{
				p = p + facet_grid(reformulateT(col, row))
			}
		}
		
	}
	return(p)
}

#' Adds a primarytitle to the plot object
#'
#' @param p ggplot2 object
#' @param title Title to be added
#'
#' @return Returns the p object with the title added
#' @export
#'
#' @examples
titlePlot = function(p, title){
	if(!is.null(title)){
		if(!is.na(title)){
			p = p + labs(title = title)
		}
	}
	return(p)
}

#' Adds a secondary to the plot object
#'
#' @param p ggplot2 object
#' @param subtitle Subtitle to be added
#'
#' @return Returns the p object with the subtitle added
#' @export
#'
#' @examples
subtitlePlot = function(p, subtitle){
	if(!is.na(subtitle)){
		p = p + labs(subtitle = subtitle)
	}
	return(p)
}

#' Benchmarks by outputting the time passed and returns the current time.
#'
#' @param m character to set the string
#' @param start Time stamp
#'
#' @return Returns the currect time
#' @export
#'
#' @examples
benchMark = function(m = "", start = NULL){
	if(!is.null(start)){
		cat("Benchmark - ")
		cat(m)
		cat(": ")
		cat(Sys.time() - start)
		cat("\n")
	}
	return(Sys.time())
}

#' Plots trajectories of particles
#'
#' @param dataTraj data frame of trajectories
#' @param x character column name of x coordinates
#' @param y character column name of y coordinates
#' @param trackGlobalIDName character column name of global track IDs
#' @param groupings data frame of the groupings
#' @param x.unit character unit of the x coordinate to be converted to
#' @param y.unit character unit of the y coordinate to be converted to
#' @param colorGroupName character column name of the color group name
#' @param colorTrajectories logical to whether or not to color trajectories
#' @param startPointGroupName 
#' @param endPointGroupName 
#' @param coord_equal logical whether or not have equal aspect ratio in x and y axis
#' @param equalRange logical whether or not have equal ranges in x and y axis
#' @param color.legend not used 
#' @param alpha.legend not used
#' @param inverse logical whether or not to invert y axis. Used as image data pixel y coordindate increases by going down
#' @param facet.row character column name of the facet rows
#' @param facet.col character column name of the facet columns
#' @param facet.wrap logical facet wrap option
#' @param title character to be set as the primary title
#' @param subtitle character to be set as the secondary title
#' @param hide.ns logical to hide non-significance indicator
#' @param replicateGroupName character column name of the replicates group name
#' @param is.dark logical whether or not to plot in the dark mode
#' @param limitNTracks numeric to limit number of tracks to be plotted for each group
#' @param randomizeTrackSampling logical whether or not to randomize track sampling to be used with limitNTracks
#' @param trackReduced integer ratio of track reduction. Every nth track is plotted
#' @param spotReduced integer ratio of spot reduction. Every nth spot is plotted for each track
#' @param colorAlpha numeric color opacity 0-1
#' @param plot.subtitle.hjust character secondary title horizontal alignment
#' @param plot.subtitle.size numeric secondary title text size
#' @param plot.subtitle.face character secondary title text face
#' @param h.line logical option for horizontal line at y=0
#' @param v.line logical option for vertical line at x=0 
#' @param panel.border logical option panel border. This is the plot area between titles, legends and axis.
#' @param panel.grid.major logical option for a grid on the plot area
#' @param facet.label.fill.color Facet label background color
#' @param facet.text.face character Facet label text style
#' @param linesize numeric line thickness of the trajectories
#' @param x.lab character to be set as the x axis title
#' @param y.lab character to be set as the y axis title
#' @param browse 
#' @param verbose 
#' @param benchmark 
#' @param initializeProg 
#' @param updateProg 
#' @param closeProg 
#'
#' @return
#' @export
#'
#' @examples
plotTrajectories = function(dataTraj, x, y, trackGlobalIDName, groupings, x.unit = NULL, y.unit = NULL, 
							colorGroupName = NULL, colorTrajectories = TRUE, #colorReverseOrder = FALSE, 
							startPointGroupName = NA, endPointGroupName = NA, 
							#alphaGroupName = NULL, 
							coord_equal = TRUE, equalRange = TRUE, #fillGroupName = NULL, 
							color.legend = NULL, alpha.legend = NULL, #fill.legend = NULL, 
							inverse = TRUE, facet.row = NULL, facet.col = NULL, facet.wrap = FALSE,
							title = NA, subtitle = NULL,
							#statGroupName = NULL, stat.label = "..p.signif..", stat.method = "wilcox.test", 
							hide.ns = FALSE, 
							replicateGroupName = NULL, is.dark = FALSE, 
							limitNTracks = FALSE, randomizeTrackSampling = FALSE,
							trackReduced = 1, spotReduced = 1,
							colorAlpha = 1.0, #fillAlpha = 1.0, 
							plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
							h.line = TRUE, v.line = TRUE, panel.border = FALSE, panel.grid.major = FALSE,
							facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold", linesize = 1, 
							x.lab = NULL, y.lab = NULL, browse = FALSE, verbose = FALSE, benchmark = FALSE,
							initializeProg = NULL, updateProg = NULL, closeProg = NULL){
	if(is.function(initializeProg) && is.function(updateProg)){
		initializeProg(message = "Generating trajectory plot...", max = 4)
	}
	
	if(browse) browser()
	if(benchmark) startTime = benchMark()
	if(verbose) cat("Preparing names and expressions...\n")
	
	default.x.Unit = attr(dataTraj[[x]], "unit"); default.y.Unit = attr(dataTraj[[y]], "unit")
	
	allGroupswoRep = unique(c(trackGlobalIDName, colorGroupName, facet.row, facet.col, startPointGroupName, endPointGroupName)) #fillGroupName, alphaGroupName, 
	colorAsFeature = FALSE
	if(!colorGroupName %in% groupings$names && colorGroupName %in% colnames(dataTraj)){
		colorAsFeature = TRUE
		allGroupswoRep = unique(c(trackGlobalIDName, facet.row, facet.col, startPointGroupName, endPointGroupName)) #fillGroupName, alphaGroupName, 
	}
		
	
	allGroupswoRep = allGroupswoRep[!is.na(allGroupswoRep)] # Required for start point and end point. Otherwise subsetting data frame for a column with NA throws error.
	allGroupswoRep = allGroupswoRep[!is.null(allGroupswoRep)]
	
	allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
	allGroupswRepwoTrGlobID = allGroupswRep[!(allGroupswRep %in% trackGlobalIDName)]
	allGroupswoRepwoTrGlobID = allGroupswRep[!(allGroupswRep %in% trackGlobalIDName)]
	
	
	aggrSpots = data.frame(); aggrTracks = data.frame()
	aggrSpots = tryCatch(aggregateByGroups(data = dataTraj, groups = allGroupswRep, length), 
						  error = retNULL)
	if(is.null(aggrSpots)){
		aggrSpots = dataTraj
	}
	aggrSpots = unique(aggrSpots[, c(allGroupswRep, y)]) # to remove identical rows, why should this be possible?
	aggrTracks = aggrSpots; aggrTracks[[y]] = 1 # Number of spots set to 1 to make each track is actually 1 (naturally)
	# How many tracks per replicate and group
	aggrTracksPerRep = tryCatch(aggregateByGroups(data = aggrTracks, 
											groups = allGroupswRep[!(allGroupswRep %in% trackGlobalIDName)], length), 
						  error = retNULL)
	if(is.null(aggrTracksPerRep)){
		aggrTracksPerRep = aggrSpots
	}
	#aggrSpots = unique(aggrSpots[, c(allGroupswRep, y)])
	# How many tracks per group
	aggrTracksRep = tryCatch(aggregateByGroups(data = aggrTracks, 
											   groups = allGroupswoRep[!(allGroupswoRep %in% trackGlobalIDName)], length), 
							 error = retNULL)
	
	# Aggregation is required for subtitle but also limiting number of tracks for groups.
	if(is.null(subtitle) || limitNTracks){
		# Aggregate trajectory data to get length for all fields (number of spots in track). Aggregation results in 
		# rows with track global id (unique values) non-repetitive and groupings. Only the columns of all groups 
		# returned. 1 track = 1 row. So "length" info is actually lost. But we can use this to count tracks.
		
		if(!is.null(replicateGroupName)){
			nReplicates = length(unique(dataTraj[[replicateGroupName]]))
		}else{
			nReplicates = 1
		}
		
		# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
		if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
			if(is.function(updateProg)){updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")}
			# Aggregating groupings to count number of trackGlobalIDName in each group.
			#aggrNTracks = aggregate(reformulateT(allGroupswRep[!allGroupswRep %in% trackGlobalIDName], "."), 
			#						data = aggrTracksRep, length)
			#aggrNTracks = aggregate(reformulateT(replicateGroupName, "."), 
			#						data = aggrTracksPerRep, sum)
		}
		
		minNTracks = min(aggrTracksPerRep[[trackGlobalIDName]]); maxNTracks = max(aggrTracksPerRep[[trackGlobalIDName]])
		
		if(limitNTracks){
			aggrTracksGrouped = group_by_at(aggrTracks, allGroupswRep[allGroupswRep != trackGlobalIDName])
			sampledTracks = sample_n(aggrTracksGrouped, minNTracks)[[trackGlobalIDName]]
			data = filter(dataTraj, (!!as.name(trackGlobalIDName)) %in% sampledTracks)
			
			aggrTracks = aggregate(reformulateT(allGroupswRep, "."), data = dataTraj, length)[, allGroupswRep]
			#nReplicates = nrow(aggregate(reformulateT(replicateGroupName, "."), data = dataTraj, length))
			
			# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
			if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
				if(is.function(updateProg)){updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")}
				# Aggregating groupings to count number of trackGlobalIDName in each group.
				aggrNTracks = aggregate(reformulateT(allGroupswRep[!allGroupswRep %in% trackGlobalIDName], "."), 
										data = aggrTracks, length)
			}
			minNTracks = min(aggrNTracks[[trackGlobalIDName]]); maxNTracks = max(aggrNTracks[[trackGlobalIDName]])
		}
		
		firstPart = ""
		if(!is.null(replicateGroupName)){
			if(is.function(updateProg)){updateProg(value = 3, detail = "Aggregating data (3/3) (this may take some time)...")}
			firstPart = paste0("n=", nReplicates, ", ")
		}
		subtitle = paste0(firstPart, "# of tracks each group/replicate=")
		if(minNTracks == maxNTracks){
			subtitle = paste0(subtitle, minNTracks)
		}else{
			subtitle = paste0(subtitle, minNTracks, '-', maxNTracks)
		}
		
	}
	
	if(trackReduced > 1){
		trackGlobalIDs = unique(dataTraj$track_global_id)
		trackGlobalIDs = trackGlobalIDs[seq(1, length(trackGlobalIDs), trackReduced)]
		dataTraj = dataTraj %>% filter(track_global_id %in% trackGlobalIDs)
		if(!is.null(subtitle)){
			if(is.character(subtitle)){
				subtitle = paste0(subtitle, ", displaying 1 in ", trackReduced, " tracks")
			}
		}
	}
	
	if(spotReduced > 1){
		dataTraj = dataTraj %>% group_by(track_global_id) %>% slice(seq(1, n(), by = spotReduced))
		if(!is.null(subtitle)){
			if(trackReduced > 1){
				midText = " and 1 in "
			}else{
				midText = ", displaying 1 in "
			}
			if(is.character(subtitle)){
				subtitle = paste0(subtitle, midText, spotReduced, " spots")
			}
		}
	}
	
	#if(!is.null(unit)){  ud.convert(tracks$TRACK_MEAN_SPEED, "μm/sec", "μm/h")
	
	#fillGroup = nameToExpr(fillGroupName); 
	
	colorGroup = nameToExpr(colorGroupName)#; alphaGroup = nameToExpr(alphaGroupName)
	startPointGroup = nameToExpr(startPointGroupName); endPointGroup = nameToExpr(endPointGroupName)
	#statGroup = nameToExpr(statGroupName)
	trackGlobalID = nameToExpr(trackGlobalIDName)
	
	if(benchmark) startTime = benchMark("Units, groups, expressions", startTime)
	#if(inverse)	dataTraj[[y]] = dataTraj[[y]] * (-1)
	
	if(verbose) cat("Units...\n")
	if(is.null(x.unit)) x.unit = default.x.Unit
	if(is.null(y.unit)) y.unit = default.y.Unit
	if(verbose) cat("x/y labels...\n")
	if(is.null(x.lab)) x.lab = paste0(getGLab(groupings, x), " [", x.unit, "]")
	if(is.null(y.lab)) y.lab = paste0(getGLab(groupings, y), " [", y.unit, "]")
	
	if(verbose) cat("Generating the plot...\n")
	
	
	xVar = unitConversion(defaultUnit = default.x.Unit, unit = x.unit, x)
	
	yVar = unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y)
	
	# if(!is.null(default.x.Unit) || !is.null(x.unit)){
	# 	if(ud.are.convertible(default.x.Unit, x.unit)){
	# 		xVar = expr(ud.convert(!!sym(x), default.x.Unit, x.unit))
	# 	}else{
	# 		xVar = expr(x)
	# 	}
	# }
	# if(!is.null(default.y.Unit) || !is.null(y.unit)){
	# 	if(ud.are.convertible(default.y.Unit, y.unit)){
	# 		yVar = expr(ud.convert(!!sym(y), default.y.Unit, y.unit))
	# 	}else{
	# 		yVar = expr(y)
	# 	}
	# }
	
	# if(colorReverseOrder){
	# 	dataTraj[[colorGroupName]] = factor(dataTraj[[colorGroupName]], levels = rev(levels(dataTraj[[colorGroupName]])))
	# 	#plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, track = !!trackGlobalID)) #, fill = !!fillGroup))
	# }
	plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, track = !!trackGlobalID)) #, fill = !!fillGroup))
	
	color = "black"
	if(is.dark) color = "white"
	
	if(h.line) plot = plot + geom_hline(aes(yintercept = 0), color = color)
	if(v.line) plot = plot + geom_vline(aes(xintercept = 0), color = color)
	if(colorTrajectories){
		plot = plot + geom_path(size = linesize, linejoin="round", lineend = "round")	
	}else{
		plot = plot + geom_path(size = linesize, color = "grey", linejoin="round", lineend = "round")
	}
	
	if(benchmark) startTime = benchMark("Generating labels and plot", startTime)
	if(inverse){
		if(verbose) cat("Inverting...\n")
		plot = plot + scale_y_reverse()
		if(benchmark) startTime = benchMark("Inverting", startTime)
	}
	# If both facet variables are NULL, then don't do any faceting, otherwise check if any of them is null
	
	plot = facetPlot(plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
	
	plot = titlePlot(p = plot, title = title)
	
	if(is.function(updateProg)){updateProg(value = 1, detail = "Aggregating data (1/3) (this may take some time)...")}
	
	if(benchmark) startTime = benchMark("Facet, title", startTime)
	if(is.null(subtitle)){
		if(verbose) cat("Generating subtitle...\n")
		
		
	}
	if(benchmark) startTime = benchMark("subtitle", startTime)
	plot = subtitlePlot(p = plot, subtitle = subtitle)
	
	if(is.na(x.lab)) x.lab = ""
	if(is.na(y.lab)) y.lab = ""
	if(verbose) cat("Labels...\n")
	plot = plot + labs(x = x.lab, y = y.lab)#, alpha = getGLab(groupings), alphaGroupName))
	
	# if(!colorAsFeature){
	# 	plot = plot + labs(color = getGLab(groupings, colorGroupName)) #fill = getGLab(groupings, fillGroupName),)
	# }else{
	# 	plot = plot + labs()
	# }
	
	
	if(verbose) cat("Color scales...\n")
	plot = colorPlot(plot, dataTraj, groupings, colorGroupName, colorAlpha, is.dark, colorAsFeature)
	# if(!is.null(colorGroupName)){
	# 	if(!colorAsFeature){
	# 		colorValues = alpha(getGColors(groupings, colorGroupName, levels(dataTraj[[colorGroupName]])), alpha = colorAlpha)
	# 		if(is.dark){
	# 			colorValues = invertedColors(colorValues, ignore.alpha = TRUE)
	# 		}
	# 		# if(colorReverseOrder){
	# 		# 	plot = plot + scale_color_manual(values = colorValues, breaks = levels(dataTraj[[colorGroupName]]), 
	# 		# 									 labels = getGLabs(groupings = groupings, name = colorGroupName, 
	# 		# 									 				  order = levels(dataTraj[[colorGroupName]])))
	# 		# }else{
	# 		plot = plot + scale_color_manual(values = colorValues, 
	# 										 labels = getGLabs(groupings = groupings, name = colorGroupName, 
	# 										 				  order = levels(dataTraj[[colorGroupName]])))	
	# 		#}
	# 	}else{
	# 		colorValues = inferno(n = 256)
	# 		if(is.dark){
	# 			colorValues = invertedColors(colorValues, ignore.alpha = TRUE)
	# 		}
	# 		plot = plot + scale_color_gradientn(colours = colorValues)
	# 	}
	# 	
	# }
	
	
	# if(!is.null(fillGroupName)){
	# 	fillValues = alpha(getGColors(groupings, fillGroupName, levels(dataTraj[[fillGroupName]])), alpha = fillAlpha)
	# 	if(is.dark){
	# 		fillValues = invertedColors(fillValues)
	# 	}
	# 	plot = plot + scale_fill_manual(values = fillValues)
	# }
	
	plot = plot + guides(color = color.legend, alpha = alpha.legend)#, fill = fill.legend)
	
	dataStartPoint = dataTraj %>% group_by(!!trackGlobalID) %>% summarise_all(first)
	dataEndPoint = dataTraj %>% group_by(!!trackGlobalID) %>% summarise_all(last)
	if(!is.null(startPointGroupName)){ # Display start points
		if(!is.na(startPointGroupName)){ # Display without color (grey)
			plot = plot + geom_point(data = dataStartPoint, aes(x = !!xVar, y = !!yVar, color = !!colorGroup))
		}
	}else{
		plot = plot + geom_point(data = dataStartPoint, aes(x = !!xVar, y = !!yVar), color = "grey")
	}
	if(!is.null(endPointGroupName)){ # Display end points
		if(!is.na(endPointGroupName)){ # Display without color (grey)
			plot = plot + geom_point(data = dataEndPoint, aes(x = !!xVar, y = !!yVar, color = !!colorGroup))
		}
	}else{
		plot = plot + geom_point(data = dataEndPoint, aes(x = !!xVar, y = !!yVar), color = "grey")
	}
	
	if(equalRange){
		x.max = max(abs(dataTraj[[x]])); x.min = -1 * x.max
		y.max = max(abs(dataTraj[[y]])); y.min = -1 * y.max
		min = min(x.min, y.min); max = max(x.max, y.max)
		plot = plot + xlim(min, max)
		plot = plot + ylim(min, max)
		if(inverse) plot = plot + ylim(max, min)
	}
	
	if(verbose) cat("Theme...\n")
	plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, facet.label.fill.color, facet.text.face)
	
	if(panel.border) plot = plot + theme(panel.border = element_rect(color = "black", fill=NA, size = 1))
	if(panel.grid.major) plot = plot + theme(panel.grid.major = element_line(color = "black", size = 0.2))
	#if(is.dark) plot = plot + theme_black()
	
	if(benchmark) startTime = benchMark("Labels, color scales, theme", startTime)
	
	if(coord_equal) plot = plot + coord_equal()
	if(verbose) cat("Plot ready...\n")
	if(is.function(closeProg)){closeProg()}
	return(list(plot = plot, stat = NULL, replicates = NULL, tracks = NULL))
	#return(plot)
	#guides(fill=FALSE, color=FALSE, alpha = FALSE) + geom_point(aes(alpha = SPOT_SOURCE_ID)) + 
	#	scale_alpha_continuous(palette = alphaOpaque, na.value = 1.0) + 
	#	scale_color_manual(values = alpha(getGColors(groupings, colorGroupName), alpha = colorAlpha))
}

#' Title
#'
#' @param x 
#' @param smooth.window 
#' @param debug.id 
#'
#' @return
#' @export
#'
#' @examples
smoothLineWithNA = function(x, smooth.window, debug.id = NULL){
	#browser()
	if(!is.null(debug.id)){
		cat(debug.id, "\n")
	}
	appendNA = FALSE
	if(is.na(last(x))){
		appendNA = TRUE
		x = x[1:(length(x) - 1)]
	}
	if(length(x) > smooth.window){
		x = smth(x, window = smooth.window)
	}
	if(appendNA){
		x = c(x, NA)
	}
	return(x)
}

#' Sets the basic common plot themes
#'
#' @param plot ggplot object to set the themes for
#' @param is.dark logical  
#' @param subtitle.hjust 
#' @param subtitle.size 
#' @param subtitle.face 
#' @param facet.label.fill.color 
#' @param facet.text.face 
#'
#' @return
#' @export
#'
#' @examples
setThemeBase = function(plot, is.dark, subtitle.hjust, subtitle.size, subtitle.face, facet.label.fill.color, facet.text.face){
	plot = plot + theme_classic() + 
		theme(plot.title = element_text(hjust = 0.5), 
			  plot.subtitle = element_text(hjust = subtitle.hjust, size = subtitle.size, face = subtitle.face), 
			  strip.background = element_rect(color = NA, fill=facet.label.fill.color), 
			  legend.justification = c(0, 1),
			  strip.text = element_text(face = facet.text.face))
	
	if(is.dark) plot = plot + theme_black()
	return(plot)
}

#TODO
#' Title
#'
#' @param dataTraj 
#' @param x 
#' @param y 
#' @param type 
#' @param trackGlobalIDName 
#' @param groupings 
#' @param x.unit 
#' @param y.unit 
#' @param y.Range y axis range in a vector c(min, max)
#' @param fillGroupName 
#' @param colorGroupName 
#' @param groupTracks 
#' @param shapeGroupName 
#' @param lineTypeGroupName 
#' @param sizeVarName 
#' @param coord_equal 
#' @param color.legend 
#' @param alpha.legend 
#' @param inverse 
#' @param facet.row 
#' @param facet.col 
#' @param facet.wrap 
#' @param title 
#' @param subtitle 
#' @param smooth.window 
#' @param replicateGroupName 
#' @param aggregate.fun 
#' @param dispersion.fun 
#' @param dispersion.type 
#' @param linesize 
#' @param pointsize 
#' @param limitNTracks 
#' @param randomizeTrackSampling 
#' @param trackReduced 
#' @param spotReduced 
#' @param colorAlpha 
#' @param fillAlpha 
#' @param dispAlpha 
#' @param is.dark 
#' @param plot.subtitle.hjust 
#' @param plot.subtitle.size 
#' @param plot.subtitle.face 
#' @param h.line 
#' @param v.line 
#' @param panel.border 
#' @param panel.grid.major 
#' @param facet.label.fill.color 
#' @param facet.text.face 
#' @param x.lab 
#' @param y.lab 
#' @param browse 
#' @param verbose 
#' @param benchmark 
#' @param initializeProg 
#' @param updateProg 
#' @param closeProg 
#'
#' @return
#' @export
#'
#' @examples
plotTrajFeatures = function(dataTraj, x, y, type, trackGlobalIDName, groupings, x.unit = NULL, y.unit = NULL, 
							y.Range = NULL,
							fillGroupName = NULL, colorGroupName = NULL, #colorReverseOrder = FALSE, 
							groupTracks = FALSE,
							shapeGroupName = NULL, lineTypeGroupName = NULL, sizeVarName = NULL, 
							#alphaGroupName = NULL, 
							coord_equal = TRUE, 
							color.legend = NULL, alpha.legend = NULL, #fill.legend = NULL, 
							inverse = FALSE, facet.row = NULL, facet.col = NULL, facet.wrap = FALSE,
							title = NA, subtitle = NULL,
							#statGroupName = NULL, stat.label = "..p.signif..", stat.method = "wilcox.test", 
							#hide.ns = FALSE, 
							smooth.window = 1,
							replicateGroupName = NULL, aggregate.fun = NULL, 
							dispersion.fun = NULL, dispersion.type = NULL,
							linesize = 1, pointsize = 1,
							limitNTracks = FALSE, randomizeTrackSampling = FALSE,
							trackReduced = 1, spotReduced = 1,
							colorAlpha = 1.0, fillAlpha = 1.0, dispAlpha = 1.0, is.dark = FALSE, 
							plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
							h.line = TRUE, v.line = TRUE, panel.border = FALSE, panel.grid.major = FALSE,
							facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold", 
							x.lab = NULL, y.lab = NULL, browse = FALSE, verbose = FALSE, benchmark = FALSE,
							initializeProg = NULL, updateProg = NULL, closeProg = NULL){
	if(is.function(initializeProg) && is.function(updateProg)){
		initializeProg(message = "Generating trajectory plot...", 
					   max = 4)
	}
	# Hack, turns out that this input isn't even required. Aggregate function automatically means to group tracks.
	if(!is.null(aggregate.fun)){
		groupTracks = TRUE
	}
	
	if(browse) browser()
	if(benchmark) startTime = benchMark()
	if(verbose) cat("Preparing names and expressions...\n")
	
	default.x.Unit = attr(dataTraj[[x]], "unit"); default.y.Unit = attr(dataTraj[[y]], "unit")
	
	allGroupswoRep = unique(c(trackGlobalIDName, colorGroupName, facet.row, facet.col, shapeGroupName, #alphaGroupName, 
							  lineTypeGroupName, fillGroupName, sizeVarName))
	allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
	
	# Aggregation is required for subtitle but also limiting number of tracks for groups.
	if(is.null(subtitle) || limitNTracks){
		# Aggregate trajectory data to get length for all fields (number of spots in track). Aggregation results in 
		# rows with track global id (unique values) non-repetitive and groupings. Only the columns of all groups 
		# returned. 1 track = 1 row. So "length" info is actually lost. But we can use this to count tracks.
		
		#aggrTracks = aggregate(reformulateT(allGroupswRep, "."), data = dataTraj, length)[, allGroupswRep]
		#aggrTracks = tryCatch(aggregateByGroups(data = dataTraj, groups = allGroupswRep, length), 
		#					  error = retNULL)
		aggrTracks = dataTraj %>% group_by_at(allGroupswRep) %>% summarise(n())
		if(is.null(aggrTracks)){
			aggrTracks = dataTraj
		}
		aggrTracks = aggrTracks[, allGroupswRep]
		
		#nReplicates = nrow(aggregate(reformulateT(replicateGroupName, "."), data = dataTraj, length))
		if(!is.null(replicateGroupName)){
			nReplicates = length(unique(dataTraj[[replicateGroupName]]))
		}else{
			nReplicates = 1
		}
		aggrNTracks = aggrTracks
		# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
		if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
			if(is.function(updateProg)){updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")}
			# Aggregating groupings to count number of trackGlobalIDName in each group.
			aggrNTracks = aggregate(reformulateT(allGroupswRep[!allGroupswRep %in% trackGlobalIDName], "."), 
									data = aggrTracks, length)
			
		}
		
		minNTracks = min(aggrNTracks[[trackGlobalIDName]]); maxNTracks = max(aggrNTracks[[trackGlobalIDName]])

		if(limitNTracks){
			aggrTracksGrouped = group_by_at(aggrTracks, allGroupswRep[allGroupswRep != trackGlobalIDName])
			sampledTracks = sample_n(aggrTracksGrouped, minNTracks)[[trackGlobalIDName]]
			data = filter(dataTraj, (!!as.name(trackGlobalIDName)) %in% sampledTracks)
			
			aggrTracks = aggregate(reformulateT(allGroupswRep, "."), data = dataTraj, length)[, allGroupswRep]
			#nReplicates = nrow(aggregate(reformulateT(replicateGroupName, "."), data = dataTraj, length))
			
			# If grouping is applied, then there are groups after trackGlobalIDName, in this case aggregate more...
			if(length(allGroupswRep[!allGroupswRep %in% trackGlobalIDName])){
				if(is.function(updateProg)){updateProg(value = 2, detail = "Aggregating data (2/3) (this may take some time)...")}
				# Aggregating groupings to count number of trackGlobalIDName in each group.
				aggrNTracks = aggregate(reformulateT(allGroupswRep[!allGroupswRep %in% trackGlobalIDName], "."), 
										data = aggrTracks, length)
			}
			minNTracks = min(aggrNTracks[[trackGlobalIDName]]); maxNTracks = max(aggrNTracks[[trackGlobalIDName]])
		}
		
		firstPart = ""
		if(!is.null(replicateGroupName)){
			if(is.function(updateProg)){updateProg(value = 3, detail = "Aggregating data (3/3) (this may take some time)...")}
			firstPart = paste0("n=", nReplicates, ", ")
		}
		subtitle = paste0(firstPart, "# of tracks each group/replicate=")
		if(minNTracks == maxNTracks){
			subtitle = paste0(subtitle, minNTracks)
		}else{
			subtitle = paste0(subtitle, minNTracks, '-', maxNTracks)
		}
		
	}
	
	if(trackReduced > 1){
		trackGlobalIDs = unique(dataTraj$track_global_id)
		trackGlobalIDs = trackGlobalIDs[seq(1, length(trackGlobalIDs), trackReduced)]
		dataTraj = dataTraj %>% filter(track_global_id %in% trackGlobalIDs)
		if(!is.null(subtitle)){
			if(is.character(subtitle)){
				subtitle = paste0(subtitle, ", displaying 1 in ", trackReduced, " tracks")
			}
		}
	}
	
	if(spotReduced > 1){
		dataTraj = dataTraj %>% group_by(track_global_id) %>% slice(seq(1, n(), by = spotReduced))
		if(!is.null(subtitle)){
			if(trackReduced > 1){
				midText = " and 1 in "
			}else{
				midText = ", displaying 1 in "
			}
			if(is.character(subtitle)){
				subtitle = paste0(subtitle, midText, spotReduced, " spots")
			}
		}
	}
	
	#if(!is.null(unit)){  ud.convert(tracks$TRACK_MEAN_SPEED, "μm/sec", "μm/h")
	
	#fillGroup = nameToExpr(fillGroupName); 
	colorGroup = nameToExpr(colorGroupName)#; alphaGroup = nameToExpr(alphaGroupName) 
	shapeGroup = nameToExpr(shapeGroupName); fillGroup = nameToExpr(fillGroupName)
	lineTypeGroup = nameToExpr(lineTypeGroupName); sizeVar = nameToExpr(sizeVarName)
	trackGroup = nameToExpr(trackGlobalIDName)#; sizeVar = nameToExpr(trackGlobalIDName)
	#statGroup = nameToExpr(statGroupName)
	trackGlobalID = nameToExpr(trackGlobalIDName)
	
	if(benchmark) startTime = benchMark("Units, groups, expressions", startTime)
	#if(inverse)	dataTraj[[y]] = dataTraj[[y]] * (-1)
	
	if(verbose) cat("Units...\n")
	if(is.null(x.unit)) x.unit = default.x.Unit
	if(is.null(y.unit)) y.unit = default.y.Unit
	if(verbose) cat("x/y labels...\n")
	
	x.lab = paste0(x.lab, " [", x.unit, "]")
	y.lab = paste0(y.lab, " [", y.unit, "]")
	
	if(verbose) cat("Generating the plot...\n")
	
	xVar = unitConversion(defaultUnit = default.x.Unit, unit = x.unit, x)
	
	if(smooth.window == 1){
		yVar = unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y)
	}else{
		#yVar = call("smth", unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y), window = smooth.window)
		#yVar = call("smooth", unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y), "3R")
		#yVar = smooth(unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y), "3R")
		#dataTraj = dataTraj %>% mutate_at(y, ~c(smooth(c(na.omit(.)), "3R"), NA))
		dataTraj = dataTraj %>% mutate_at(y, ~smoothLineWithNA(., smooth.window))
		yVar = unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y)
		#yVar = unitConversion(defaultUnit = default.y.Unit, unit = y.unit, y), window = smooth.window)
	}
	
	# if(colorReverseOrder){
	# 	dataTraj[[colorGroupName]] = factor(dataTraj[[colorGroupName]], levels = rev(levels(dataTraj[[colorGroupName]])))
	# 	#plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, track = !!trackGlobalID)) #, fill = !!fillGroup))
	# }
	
	if(groupTracks){
		plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
								linetype = !!lineTypeGroup, shape = !!shapeGroup)) #, fill = !!fillGroup))
	}else{
		plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
									linetype = !!lineTypeGroup, shape = !!shapeGroup, track = !!trackGlobalID)) #, fill = !!fillGroup))
	}
	
	if(is.function(aggregate.fun)){
		aggrGroups = c(allGroupswoRep, x)
		if(groupTracks){
			aggrGroups = aggrGroups[aggrGroups != aggrGroups[1]]
		}
			
		dataTrajAggr = aggregate(reformulateT(aggrGroups, y), data = dataTraj, aggregate.fun)
		
		
		if(is.function(dispersion.fun)){
			dataTrajDisp = aggregate(reformulateT(aggrGroups, y), data = dataTraj, dispersion.fun)
			ranges = dataTrajDisp[, ncol(dataTrajDisp)]
			colnames(dataTrajDisp)[ncol(dataTrajDisp)] = "tx_lower"
			colnames(dataTrajDisp)[ncol(dataTrajDisp) - 1] = "tx_mid"
			colnames(dataTrajDisp)[ncol(dataTrajDisp) - 2] = "tx_upper"
			dataTrajAggr$tx_lower = ranges[, 3]
			dataTrajAggr$tx_mid = ranges[, 2]
			dataTrajAggr$tx_upper = ranges[, 1]
		}
		
		if(groupTracks){
			plot = ggplot(dataTrajAggr, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
											linetype = !!lineTypeGroup, shape = !!shapeGroup)) #, fill = !!fillGroup))
		}else{
			plot = ggplot(dataTrajAggr, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
											linetype = !!lineTypeGroup, shape = !!shapeGroup, tracks = !!trackGroup)) #, fill = !!fillGroup))
		}
		
		
		if(is.function(dispersion.fun)){
			if(is.function(dispersion.type)){
				color = "black"
				if(is.dark){color = "white"}
				plot = plot + dispersion.type(aes(ymin = tx_lower, ymax = tx_upper, fill = !!colorGroup), alpha = dispAlpha)
			}
		}
	}
	
	#plot = ggplot(dataTraj, aes(x = !!xVar, y = !!yVar, color = !!colorGroup, fill = !!fillGroup, size = !!sizeVar, 
	#							linetype = !!lineTypeGroup, shape = !!shapeGroup, track = !!trackGlobalID)) #, fill = !!fillGroup))
	plot = plot + labs(color = getGLab(groupings, colorGroupName), fill = getGLab(groupings, fillGroupName), 
					   #alpha = getGLab(groupings, alphaGroupName), 
					   shape = getGLab(groupings, shapeGroupName), 
					   linetype = getGLab(groupings, lineTypeGroupName), size = getGLab(groupings, sizeVarName), 
					   x = x.lab, y = y.lab)
	
	color = "black"
	if(is.dark) color = "white"
	
	if("point" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_point(color = "white", size = pointsize)
		}else{
			plot = plot + geom_point(size = pointsize)
		}
	}
	if("jitter" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_jitter(color = "white", size = pointsize)
		}else{
			plot = plot + geom_jitter(size = pointsize)
		}
	}
	if("quantile" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_quantile(color = "white", size = linesize)
		}else{
			plot = plot + geom_quantile(size = linesize)
		}
	}
	if("smooth" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_smooth(model = lm, color = "white", size = linesize)
		}else{
			plot = plot + geom_smooth(model = lm, size = linesize)
		}
	}
	if("line" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_line(color = "white", size = linesize)
		}else{
			plot = plot + geom_line(size = linesize)
		}
	}
	if("area" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_area(color = "white", size = linesize)
		}else{
			plot = plot + geom_area(size = linesize)
		}
	}
	if("step" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_step(direction = "hv", color = "white", size = linesize)
		}else{
			plot = plot + geom_step(direction = "hv", size = linesize)
		}
	}
	
	plot = plot + theme_classic()
	if(benchmark) startTime = benchMark("Generating labels and plot", startTime)
	if(inverse){
		if(verbose) cat("Inverting...\n")
		plot = plot + scale_y_reverse()
		if(benchmark) startTime = benchMark("Inverting", startTime)
	}
	# If both facet variables are NULL, then don't do any faceting, otherwise check if any of them is null
	
	plot = facetPlot(plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
	
	plot = titlePlot(p = plot, title = title)
	
	if(is.function(updateProg)){updateProg(value = 1, detail = "Aggregating data (1/3) (this may take some time)...")}
	
	if(benchmark) startTime = benchMark("Facet, title", startTime)
	if(is.null(subtitle)){
		if(verbose) cat("Generating subtitle...\n")
		
		
	}
	if(benchmark) startTime = benchMark("subtitle", startTime)
	plot = subtitlePlot(p = plot, subtitle = subtitle)
	
	# if(is.na(x.lab)) x.lab = ""
	# if(is.na(y.lab)) y.lab = ""
	# if(verbose) cat("Labels...\n")
	# plot = plot + labs(color = getGLab(groupings, colorGroupName), fill = getGLab(groupings, fillGroupName), 
	# 				   alpha = getGLab(groupings, alphaGroupName), x = x.lab, y = y.lab)
	
	if(verbose) cat("Color scales...\n")
	plot = colorPlot(plot, dataTraj, groupings, colorGroupName, colorAlpha, is.dark)
	# if(!is.null(colorGroupName)){
	# 	colorValues = alpha(getGColors(groupings, colorGroupName, levels(dataTraj[[colorGroupName]])), alpha = colorAlpha)
	# 	if(is.dark){
	# 		colorValues = invertedColors(colorValues, ignore.alpha = TRUE)
	# 	}
	# 	
	# 	# if(colorReverseOrder){
	# 	# 	plot = plot + scale_color_manual(values = colorValues, 
	# 	# 									 labels = getGLabs(groupings = groupings, name = colorGroupName, order = levels(dataTraj[[colorGroupName]])),
	# 	# 									 breaks = levels(dataTraj[[colorGroupName]]))
	# 	# }else{
	# 		plot = plot + scale_color_manual(values = colorValues, 
	# 										 labels = getGLabs(groupings = groupings, name = colorGroupName, order = levels(dataTraj[[colorGroupName]])))	
	# 	#}
	# }
	plot = fillPlot(plot, dataTraj, groupings, fillGroupName, fillAlpha, is.dark)
	# if(!is.null(fillGroupName)){
	# 	colorValues = alpha(getGColors(groupings, fillGroupName, levels(dataTraj[[fillGroupName]])), alpha = colorAlpha)
	# 	if(is.dark){
	# 		colorValues = invertedColors(colorValues, ignore.alpha = TRUE)
	# 	}
	# 	
	# 	plot = plot + scale_fill_manual(values = colorValues, 
	# 									labels = getGLabs(groupings = groupings, name = fillGroupName, order = levels(dataTraj[[fillGroupName]])))	
	# 	
	# }else{
	# 	plot = plot + scale_fill_manual(values = colorValues, 
	# 									 labels = getGLabs(groupings = groupings, name = colorGroupName, order = levels(dataTraj[[colorGroupName]])),
	# 									 breaks = levels(dataTraj[[colorGroupName]]))
	# }
	
	plot = plot + guides(color = color.legend, alpha = alpha.legend)#, fill = fill.legend)
	#if(equalRange){
	#x.max = max(abs(dataTraj[[x]])); x.min = -1 * x.max
	#y.max = max(abs(dataTraj[[y]])); y.min = -1 * y.max
	#min = min(x.min, y.min); max = max(x.max, y.max)
	
	#plot = plot + xlim(min, max)
	#plot = plot + ylim(min, max)
	#if(inverse) plot = plot + ylim(max, min)
	
	plot = plot + coord_cartesian(ylim = y.Range)
	
	if(verbose) cat("Theme...\n")
	plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, facet.label.fill.color, facet.text.face)
	
	if(panel.border) plot = plot + theme(panel.border = element_rect(color = "black", fill=NA, size = 1))
	if(panel.grid.major) plot = plot + theme(panel.grid.major = element_line(color = "black", size = 0.2))
	if(is.dark) plot = plot + theme_black()
	
	if(benchmark) startTime = benchMark("Labels, color scales, theme", startTime)
	
	#if(coord_equal) plot = plot + coord_equal()
	if(verbose) cat("Plot ready...\n")
	if(is.function(closeProg)){closeProg()}
	return(list(plot = plot, stat = NULL, replicates = NULL, tracks = NULL))
	#return(plot)
	#guides(fill=FALSE, color=FALSE, alpha = FALSE) + geom_point(aes(alpha = SPOT_SOURCE_ID)) + 
	#	scale_alpha_continuous(palette = alphaOpaque, na.value = 1.0) + 
	#	scale_color_manual(values = alpha(getGColors(groupings, colorGroupName), alpha = colorAlpha))
}

groupHistograms = function(data, x, y, default.y.Unit, y.unit, y.labDisp, groupings, colorGroup = NULL, fillGroup = NULL, #alphaGroup = NULL,
						   colorGroupName = NULL, fillGroupName = NULL, #alphaGroupName = NULL, 
						   facet.row = NULL, facet.col = NULL, facet.wrap = FALSE, colorAlpha = 1.0, fillAlpha = 1.0,
						   is.dark = FALSE){
	yVar = unitConversion(default.y.Unit, y.unit, y)
	plot = ggplot(data, aes(x = !!yVar, color = !!colorGroup, fill = !!fillGroup))#, group = !!x))
	plot = plot + geom_histogram(bins = 9, position = "identity", alpha = 0.5)
	plot = facetPlot(p = plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
	plot = colorPlot(plot, data, groupings, colorGroupName, colorAlpha, is.dark)
	plot = fillPlot(plot, data, groupings, fillGroupName, fillAlpha, is.dark)
	plot = plot + labs(x = y.labDisp, y = "Count", title = "Group Distributions")
	plot = plot + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
	
	return(plot)
}

circularHistogram = function(x, bw = c(75, 40, 10), lwd = c(2, 2, 2), lty = c(2, 1, 3), bin = 720, bins = 12,
							 prop = 1.3, col = "darkgrey", stack = TRUE, sep = 0.035, shrink = 1, zero = pi/2, 
							 title = "", browse = FALSE){
	#xc = circular(x, units = "radians")
	#browser()
	tryCatch({
		#browser()
		plot(x, cex=1.1, bin=bin, stack=stack, sep=sep,shrink=shrink, col="black", main = title, axes = FALSE, browse = browse)
		ticks = seq(0, 11*pi/6, pi/6)
		labels = paste0(seq(0, 11*pi/6, pi/6) * 180/pi, "°")
		axis.circular(at=circular(ticks), labels=labels, col="black", zero=zero, rotation='clock', cex=1.1)
		ticks.circular(circular(ticks), col="black", zero=zero, rotation='clock', tcl=0.075)
		rose.diag(x, bins = bins, col=col, cex=1.1, prop=prop, border="black", add=TRUE, axes = FALSE)
		for(i in 1:length(bw)){
			lines(density.circular(x, bw=bw[i]), lwd=lwd[((i-1) %% length(lwd)) + 1], lty=lty[((i-1) %% length(lwd)) + 1])
		}
	})
}

# Adapted from Circular statistics in R ISBN: 9780199671137
vMQQ <- function(circdat, mu = NULL, kappa = NULL, title = "") {
	if(is.null(mu) || is.null(kappa)){
		vMmle = mle.vonmises(circdat, bias=TRUE)
		mu = vMmle$mu ; kappa <- vMmle$kappa
	}
	edf = ecdf(circdat)
	tdf = pvonmises(circdat, mu, kappa, from=circular(0), tol = 1e-06)
	tqf = qvonmises(edf(circdat), mu, kappa, from=circular(0), tol = 1e-06)
	#par(mfrow=c(1,2), mai=c(0.90, 1.1, 0.05, 0.1), cex.axis=1.2, cex.lab=1.5)
	#plot.default(tdf, edf(circdat), pch=16, xlim=c(0,1), ylim=c(0,1), xlab = "von Mises distribution function", ylab = "Empirical distribution function")
	#xlim <- c(0,1) ; ylim <- c(0,1) ; lines(xlim, ylim, lwd=2, lty=2)
	
	plot.default(tqf, circdat, pch=16, xlim=c(0,2*pi), ylim=c(0,2*pi), xlab = "von Mises quantile function", ylab = "Empirical quantile function", main = title) 
	xlim = c(0,2*pi) ; ylim <- c(0,2*pi) ; lines(xlim, ylim, lwd=2, lty=2)
}

#' Generates circular model based on the grouped data
#'
#' @param data data frame 
#' @param x character main grouping column name
#' @param y character measure column name
#' @param groupings data frame groupings
#' @param allGroupswRep character vector of grouping column names including the replicate grouping
#' @param allGroupswoRep character vector of grouping column names excluding the replicate grouping
#' @param summary.fun character 
#'
#' @return
#' @export
#'
#' @examples
groupedCircDataModel = function(data, xContinuous, x, y, groupings, allGroupswRep, allGroupswoRep#, 
							   #summary.fun
							   #colorGroup = NULL, fillGroup = NULL, #alphaGroup = NULL,
							   #colorGroupName = NULL, fillGroupName = NULL, #alphaGroupName = NULL, 
							   #facet.row = NULL, facet.col = NULL, facet.wrap = FALSE, 
							   #colorAlpha = 1.0, fillAlpha = 1.0, is.dark = FALSE
							   ){
	#TODO ERROR with paste part here
	#xContinuous = paste("TRACK", gsub("_CARDINAL", "", x), sep = "_")
	data = data %>% group_by_at(c(allGroupswRep))
	nGroups = data %>% n_groups()
	#dev.off()
	#plot.new()
	#par(mfrow=c(nGroups * 2, 1), mar = c(2.2, 0.1, 2.2, 0.1), oma = c(0, 0, 0, 0))
	dataByGroups = data %>% group_split()
	dataToPlot = list()
	for(i in 1:nGroups){
		title = apply(dataByGroups[[i]][1, c(allGroupswRep)], 1, paste, collapse = " ")
		#browser()
		
		circdat = circular(dataByGroups[[i]][[xContinuous]], units = "radians") #TODO error here
		dataToPlot[[title]] = circdat
		#circularHistogram(circdat, title = title)
		#vMQQ(circdat, title = title)
	}
	#p = recordPlot()
	return(dataToPlot)
}

#' Performs group QQ plot
#'
#' @param data data frame
#' @param x character main group column name
#' @param y character measure column name
#' @param default.y.Unit character default measure unit
#' @param y.unit character preferred measure unit
#' @param groupings data frame groupings
#' @param colorGroup symbol color grouping
#' @param fillGroup symbol fill grouping
#' @param colorGroupName character color grouping column name
#' @param fillGroupName character fill grouping column name
#' @param facet.row character facet row grouping column name
#' @param facet.col character facet row grouping column name
#' @param facet.wrap logical option to use facet wrapping
#' @param colorAlpha numeric from 0 to 1 opacity of stroke color
#' @param fillAlpha numeric from 0 to 1 opacity of fill color
#' @param is.dark logical option for dark plots
#'
#' @return Returns QQ plot as ggplot object
#' @export
#'
#' @examples
groupQQ = function(data, x, y, default.y.Unit, y.unit, groupings, colorGroup = NULL, fillGroup = NULL, #alphaGroup = NULL,
				   colorGroupName = NULL, fillGroupName = NULL, #alphaGroupName = NULL, 
				   facet.row = NULL, facet.col = NULL, facet.wrap = FALSE, colorAlpha = 1.0, fillAlpha = 1.0,
				   is.dark = FALSE){
	yVar = unitConversion(default.y.Unit, y.unit, y)
	plot = ggplot(data, aes(sample = !!yVar, color = !!colorGroup, fill = !!fillGroup))#, group = !!x))
	plot = plot + geom_qq() + geom_qq_line()
	plot = facetPlot(p = plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
	plot = colorPlot(plot, data, groupings, colorGroupName, colorAlpha, is.dark)
	plot = fillPlot(plot, data, groupings, fillGroupName, fillAlpha, is.dark)
	plot = plot + labs(x = "Theoretical", y = "Sample", title = "Q-Q Plot")
	plot = plot + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
	return(plot)
}

#' Not implemented yet
#'
#' @param data 
#' @param x 
#' @param y 
#' @param groupings 
#' @param colorGroup 
#' @param fillGroup 
#' @param colorGroupName 
#' @param fillGroupName 
#' @param facet.row 
#' @param facet.col 
#' @param facet.wrap 
#' @param colorAlpha 
#' @param fillAlpha 
#' @param is.dark 
#'
#' @return
#' @export
#'
#' @examples
circQQ = function(data, x, y, groupings, colorGroup = NULL, fillGroup = NULL, #alphaGroup = NULL,
				  colorGroupName = NULL, fillGroupName = NULL, #alphaGroupName = NULL, 
				  facet.row = NULL, facet.col = NULL, facet.wrap = FALSE, colorAlpha = 1.0, fillAlpha = 1.0,
				  is.dark = FALSE){
	circQQ = NULL
	#TODO
	
	return(circQQ)
}

#' Performs grouped normality test
#'
#' @param data data frame
#' @param groupings data frame groupings
#' @param groups character vector with names of columns
#' @param y character measure column name
#' @param default.y.Unit character defaul unit of measure
#' @param y.unit character unit to convert measure to before the test
#'
#' @return Returns data frame with normality test results for each group
#' @export
#'
#' @examples
groupedNormality = function(data, groupings, groups, y, default.y.Unit, y.unit){
	yVar = unitConversion(default.y.Unit, y.unit, y)
	normalityTest = data %>% group_by_at(groups) %>% summarise(W = shapiro.test(!!yVar)$statistic, p.value = shapiro.test(!!yVar)$p.value, Skewness = skewness(!!yVar, na.rm = TRUE, type = 1), Kurtosis = kurtosis(!!yVar, na.rm = TRUE, type = 1))
	#browser()
	labels = as.character(unlist(lapply(groups, FUN = getGLab, groupings = groupings)))
	colnames(normalityTest)[1:length(labels)] = labels
	
	return(normalityTest)
}

#' Not implemented yet
#'
#' @param data 
#' @param groupings 
#' @param groups 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
groupedCircularShape = function(data, groupings, groups, y){
	circularShape = NULL#TODO
	
	return(circularShape)
}



#' Performs Levene test and returns output as character.
#'
#' @param data data frame
#' @param groups character vector with names of columns
#' @param y character measure column name
#' @param default.y.Unit character defaul unit of measure
#' @param y.unit character unit to convert measure to before the test
#'
#' @return Levene test output as character
#' @export
#'
#' @examples
groupedLevene = function(data, groups, y, default.y.Unit, y.unit){
	yVar = unitConversion(default.y.Unit, y.unit, y)
	#browser()
	leveneOut = capture.output(leveneTest(
		as.formula(paste(deparse1(yVar), paste(paste0("`", groups, "`"), collapse=" * "), sep = "~")), 
		data = data))
	leveneOut = paste(leveneOut, collapse = "\n")
	return(leveneOut)
}

#' Plots track data
#'
#' @param dataTracks Tracks data frame
#' @param x x grouping name
#' @param y y grouping name
#' @param type Plot type: "violin", "box", "bar" and "dot"
#' @param groupings groupings data frame e.g. groupings()$groupings
#' @param y.unit y axis unit to convert original unit
#' @param colorGroupName Stroke aesthetic group name
#' @param fillGroupName Fill aesthetic group name
#' @param y.range y axis range in a vector c(min, max)
#' @param quantiles quantiles to be drawn for violin plot
#' @param facet.row Facet aesthetic group name
#' @param facet.col Facet aesthetic group name
#' @param facet.wrap Wrap facets instead of displaying on a grid (good for odd number of facet groups)
#' @param title Plot title
#' @param subtitle Secondary title
#' @param stat.label Types of significance labels: Stars = "p.signif" or p value = "p.format"
#' @param multiple.stat.method one of: list(`Do not perform` = "NONE", `ANOVA` = "anova", `Kruskal–Wallis Test` = "kruskal.test")
#' @param pairwise.stat.method one of list(`Do not perform` = "NONE", `Student's t Test` = "t.test", `Wilcoxon/Mann-Whitney Test` = "wilcox.test")
#' @param hide.ns Hide non significance
#' @param data.transform How to transform the data with a formula list(method = character, parameter = character)
#' @param stat.text.color Statistics text color
#' @param replicateGroupName Replicates group name
#' @param statPairwiseType How to match pairs for statistics "all_combinations", "to_control" and "selected"
#' @param statPairwiseControl Name of the control group
#' @param statPairwiseSelected Numbered list with pairs of groups list(c(group1, group2), ...)
#' @param statSignSymbols Significance symbols and ranges
#' @param fillAlpha Fill color opacity 0.0-1.0
#' @param colorAlpha Stroke color opacity 0.0-1.0
#' @param is.dark Dark plot option
#' @param plot.subtitle.hjust Secondary title horizontal alignment
#' @param plot.subtitle.size Secondary title text size
#' @param plot.subtitle.face Secondary title text face
#' @param facet.label.fill.color Facet label background color
#' @param facet.text.face Facet label text style
#' @param x.lab x axis label
#' @param y.lab y axis label
#' @param violin.scale Scale options for violin plot (e.g. area)
#' @param box.notch Box plot notch at median
#' @param box.varwidth \link[ggplot2]{geom_boxplot} varwidth
#' @param dot.binwidth \link[ggplot2]{geom_dotplot} dotsize
#' @param dot.stackgroups \link[ggplot2]{geom_dotplot} stackgroups
#' @param dot.method \link[ggplot2]{geom_dotplot} method
#' @param dot.stackdir \link[ggplot2]{geom_dotplot} stackdir
#' @param browse 
#' @param verbose 
#' @param benchmark 
#' @param initializeProg 
#' @param updateProg 
#' @param closeProg 
#'
#' @return
#' @export
#'
#' @examples
plotData = function(dataTracks, x, y, type, groupings, y.unit = NULL, colorGroupName = NULL, fillGroupName = NULL, 
					#alphaGroupName = NULL, 
					y.range = NULL,
					quantiles = c(0.25, 0.5, 0.75),
					facet.row = NULL, facet.col = NULL, facet.wrap = FALSE,
					title = NA, subtitle = NULL,
					#statGroupName = NULL, 
					stat.label = "..p.signif..", multiple.stat.method = NULL, pairwise.stat.method = NULL, 
					hide.ns = FALSE, data.transform = list(method = "noneTransform", paramter = 1),
					stat.text.color = "black", replicateGroupName = NULL,
					statPairwiseType = "all_combinations", statPairwiseControl = NULL, statPairwiseSelected = NULL, 
					statSignSymbols = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns")),
					fillAlpha = 1.0, colorAlpha = 1.0, is.dark = FALSE,
					plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
					facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold", 
					x.lab = NULL, y.lab = NULL,
					violin.scale = "area",
					box.notch = TRUE, box.varwidth = FALSE, 
					dot.binwidth = 1, dot.stackgroups = FALSE, dot.method = "dotdensity", dot.stackdir = "up",
					browse = FALSE, verbose = FALSE, benchmark = FALSE,
					#xReverseOrder = FALSE, 
					initializeProg = NULL, updateProg = NULL, closeProg = NULL){
	if(browse) browser()
	if(benchmark) startTime = benchMark()
	if(verbose) cat("Preparing names and expressions...\n")
	default.y.Unit = attr(dataTracks[[y]], "unit")
	if(is.null(default.y.Unit)){
		default.y.Unit = ""
	}
	#Check before
	if(is.null(y.unit)){y.unit = default.y.Unit}
	
	allGroupswoRep = unique(c(x, colorGroupName, fillGroupName, facet.row, facet.col))
	allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
	
	#Transforming data
	transformFun = match.fun(data.transform$method)
	transformFormulaFun = match.fun(paste0(data.transform$method, "Formula"))
	if(!udunits2::ud.are.convertible(default.y.Unit, y.unit)){
		y.unit = default.y.Unit
		#TODO warn about this?
	}
	dataTracks[[y]] = transformFun(udunits2::ud.convert(dataTracks[[y]], default.y.Unit, y.unit), data.transform$parameter)
	attr(dataTracks[[y]], "unit") = y.unit
	
	dataRange = c(min(dataTracks[[y]]), max(dataTracks[[y]]))
	if(is.null(y.lab)){y.labDisp = ""}else{y.labDisp = TeX(paste0(transformFormulaFun(y.lab, data.transform$parameter), " \\[", transformFormulaFun(y.unit, data.transform$parameter), "\\]"))}
	
	#if(!is.null(unit)){  ud.convert(tracks$TRACK_MEAN_SPEED, "μm/sec", "μm/h")
	fillGroup = nameToExpr(fillGroupName); colorGroup = nameToExpr(colorGroupName)
	
	yVar = nameToExpr(y)
	plot = ggplot(dataTracks, aes(x = !!sym(x), y = !!yVar, color = !!colorGroup, fill = !!fillGroup))#, alphaGroup = !!alphaGroup))
	
	if(benchmark) startTime = benchMark("Units, groups, expressions", startTime)
	if(verbose) cat("Units, groups, expressions...\n")
	
	
	if("violin" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_violin(scale = violin.scale, draw_quantiles = quantiles, color = "white")
		}else{
			plot = plot + geom_violin(scale = violin.scale, draw_quantiles = quantiles)
		}
	}
	if("box" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_boxplot(notch = box.notch, varwidth = box.varwidth, color = "white")
		}else{
			plot = plot + geom_boxplot(notch = box.notch, varwidth = box.varwidth)
		}
	}
	if("bar" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_boxplot(notch = box.notch, varwidth = box.varwidth, color = "white")
		}else{
			plot = plot + geom_bar(stat = "identity")
		}
	}
	if("dot" %in% type){
		if(is.dark && is.null(colorGroup)){
			plot = plot + geom_dotplot(binaxis = "y", dotsize = dot.binwidth, stackgroups = dot.stackgroups, 
									   method = dot.method, stackdir = dot.stackdir, color = "white")
		}else{
			plot = plot + geom_dotplot(binaxis = "y", dotsize = dot.binwidth, stackgroups = dot.stackgroups, 
									   method = dot.method, stackdir = dot.stackdir)
		}
	}
	if(benchmark) startTime = benchMark("geom_x", startTime)
	if(verbose) cat("geom_x...\n")
	
	plot = facetPlot(p = plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
	
	plot = titlePlot(p = plot, title = title)
	
	aggrRep = data.frame(); aggrRepMerged = data.frame(); aggrRepMergedNice = data.frame()
	aggr = data.frame(); aggrNice = data.frame() 
	if(!is.null(replicateGroupName)){
		# All tracks separated by all groupings including replicates
		aggrRep = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(y, length)
		# All tracks separated by all groupings WITHOUT replicates, hence replicates are summarised
		aggrRepMerged = aggrRep %>% group_by_at(allGroupswoRep) %>% summarise_at(y, length)
		# aggrRepMerged with nicer titles for output table
		aggrRepMergedNice = aggrRepMerged
		newLabels = as.character(as.character(groupings$labels)[match(colnames(aggrRepMergedNice), groupings$names)])
		newLabels[length(newLabels)] = "Number of replicates"
		colnames(aggrRepMergedNice) = newLabels
	}
	
	aggr = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(y, length)
	aggrNice = aggr
	colnames(aggrNice) = c(as.character(groupings$labels)[match(colnames(aggrNice)[1:(ncol(aggrNice) - 1)], groupings$names)], 
						   "Number of tracks")
	
	if(is.null(subtitle)){
		firstPart = ""
		if(!is.null(replicateGroupName)){
			repCountRange = c(min(aggrRepMerged[[y]]), max(aggrRepMerged[[y]]))
			firstPart = paste0("n=", paste(unique(repCountRange), collapse = "-"), " , ")
		}
		
		subtitle = paste0(firstPart, "# of tracks each group/replicate=", min(aggr[[y]]), '-', max(aggr[[y]]))
	}
	
	if(!is.na(subtitle)) plot = subtitlePlot(p = plot, subtitle = subtitle)
	
	if(benchmark) startTime = benchMark("Titles", startTime)
	if(verbose) cat("Titles...\n")
	
	if(is.null(x.lab)){x.labDisp = getGLab(groupings, x)}else{x.labDisp = x.lab}
	
	
	if(is.na(x.labDisp)) x.labDisp = ""
	#if(is.na(y.labDisp)) y.labDisp = ""
	plot = plot + labs(color = getGLab(groupings, colorGroupName), fill = getGLab(groupings, fillGroupName), 
					   #alpha = getGLab(groupings, alphaGroupName), 
					   x = x.labDisp, y = y.labDisp)
	
	if(benchmark) startTime = benchMark("Labels", startTime)
	if(verbose) cat("Labels...\n")
	
	# Stat color for dark plots
	stat.text.color = "black"
	if(is.dark) stat.text.color = "white"
	
	plot = colorPlot(plot, dataTracks, groupings, colorGroupName, colorAlpha, is.dark)
	
	plot = fillPlot(plot, dataTracks, groupings, fillGroupName, fillAlpha, is.dark)
	
	plot = plot + scale_x_discrete(labels = getGLabs(groupings = groupings, name = x, 
															order = levels(dataTracks[[x]])))
	
	if(benchmark) startTime = benchMark("Colors", startTime)
	if(verbose) cat("Colors...\n")
	
	statOut = list()
	if(!is.null(multiple.stat.method)){
		if(multiple.stat.method != "NONE"){
			if(is.null(y.range)){
				label.y = (dataRange[2] - dataRange[1]) * 0.9 + dataRange[1]
			}else{
				label.y = (y.range[2] - y.range[1]) * 0.9 + y.range[1]
			}
			plot = plot + stat_compare_means(method = multiple.stat.method, label.x = 0.7, label.y = label.y)
		}
	}
	
	if(!is.null(pairwise.stat.method)){
		if(pairwise.stat.method != "NONE"){
			comparisons = list()
			comparisonGroups = as.character(getGroups(groupings = groupings, name = x))
			if(statPairwiseType == "all_combinations"){
				comparisons = combn(comparisonGroups, 2, simplify = F)
			}else if(statPairwiseType == "to_control"){
				i = 1
				for(comparisonGroup in comparisonGroups){
					if(statPairwiseControl != comparisonGroup){
						comparisons[[i]] = c(statPairwiseControl, comparisonGroup)
						i = i + 1
					}
				}
			}else if(statPairwiseType == "selected"){
				comparisons = statPairwiseSelected
			}
			if(stat.label == "p.format"){
				statSignSymbols = NULL
			}
			plot = plot + stat_compare_means(aes(group = !!sym(x)), label = stat.label, method = pairwise.stat.method, 
											 label.x = 1.5, comparisons = comparisons, hide.ns = hide.ns, 
											 show.legend = FALSE, color = stat.text.color, symnum.args = statSignSymbols)
			#statOut = compare_means(data = dataTracks, formula = reformulate(x, y), method = stat.method)
			stat.fun = match.fun(pairwise.stat.method)
			nonStatGroupings = allGroupswoRep[allGroupswoRep != x]
			dataTracksSplits = dataTracks %>% group_by_at(vars(nonStatGroupings)) %>% group_split()
			for(dataTracksSplit in dataTracksSplits){
				withinGroupLabel = apply(dataTracksSplit[ , nonStatGroupings ], 1, paste, collapse = " AND " )[1]
				if(!is.null(comparisons)){
					stat.fun = match.fun(paste0("pairwise.", pairwise.stat.method))
					#browser()
					statOut[[withinGroupLabel]] = capture.output(stat.fun(x = dataTracksSplit[[y]], g = dataTracksSplit[[x]]))
				}else{
					statOut[[withinGroupLabel]] = capture.output(stat.fun(reformulateT(x, y), data = dataTracksSplit))
				}
			}
			
			statOut = replaceStatLabels(statOut, getGLab(groupings, x), y.lab)
		}
	}
	
	plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, facet.label.fill.color, facet.text.face)
	
	plot = plot + coord_cartesian(ylim = y.range)
	
	if(benchmark) startTime = benchMark("Stats and theme", startTime)
	if(verbose) cat("Stats and theme...\n")
	
	# Histogram of each group to check data distribution
	histogram = groupHistograms(dataTracks, x, y, default.y.Unit, y.unit, y.labDisp, groupings, colorGroup, fillGroup, 
								colorGroupName, fillGroupName, facet.row, facet.col, facet.wrap, colorAlpha, fillAlpha, 
								is.dark)
	
	# QQ plots of each group to check data normality
	qq = groupQQ(dataTracks, x, y, default.y.Unit, y.unit, groupings, colorGroup, fillGroup, 
				 colorGroupName, fillGroupName, facet.row, facet.col, facet.wrap, colorAlpha, fillAlpha, is.dark)
	# Shapiro test
	normality = groupedNormality(dataTracks, groupings, c(x, allGroupswoRep), y, default.y.Unit, y.unit)
	
	# Levene test
	levene = groupedLevene(dataTracks, c(x, allGroupswoRep), y, default.y.Unit, y.unit)
	
	return(list(plot = plot, stat = statOut, replicates = aggrRepMergedNice, tracks = aggrNice, 
				histogram = histogram, qq = qq, normality = normality, levene = levene))
}

#' Plot directionality plot (aka Nightingale plot)
#'
#' @param dataTracks Tracks data frame
#' @param groups Cardinal grouping groups in a named list e.g. unlist(groupingsToNamedList(groupings()$groupings, empty = FALSE), use.names = F)
#' @param directionCatGroupName Group name containing directions categories (cardinal direction)
#' @param cumulativeGroupName Cumulative grouping, summary.fun.name is applied on this group
#' @param summary.fun.name Name of the summary function
#' @param type Type of the plot e.g. "polygon" or "bar"
#' @param groupings Groupings data frame e.g. groupings()$groupings
#' @param colorGroupName Stroke aesthetic group name
#' @param fillGroupName Fill aesthetic group name
#' @param replicateGroupName Replicate aesthetic group name
#' @param percentage Should the number of tracks be displayed as percentage of total
#' @param replicate.summary.fun.name Summary fun on replicate group
#' @param start.angle Rotate plot with angle
#' @param facet.row Facet aesthetic group name
#' @param facet.col Facet aesthetic group name
#' @param title Plot title
#' @param facet.wrap Wrap facets instead of displaying on a grid (good for odd number of facet groups)
#' @param colorAlpha Stroke color opacity 0.0-1.0
#' @param fillAlpha Fill color opacity 0.0-1.0
#' @param is.dark Dark plot option
#' @param line.size Line thickness
#' @param facet.label.fill.color Facet label background color
#' @param facet.text.face Facet label text style
#' @param show.y.axis Show y axis (from center towards outside)
#' @param subtitle Secondary title
#' @param plot.subtitle.hjust Secondary title horizontal alignment
#' @param plot.subtitle.size Secondary title text size
#' @param plot.subtitle.face Secondary title text face
#' @param browse Breakpoint for debugging
#' @param verbose Verbose output to console
#' @param skip.radar Skip radar plot for debugging
#' @param skip.degrees Skip degress for debugging
#' @param benchmark Benchmark code
#'
#' @return Returns a list with the plot, groups and statistics 	list(plot = ggplot, stat = statOut, replicates = aggrRepMergedNice, tracks = data frame, 
#' circDataModel = list of plots with titles being names, summaryStats = tibble, uniformityDF = data frame, uniformityText = character, vonMisesFit = character)
#' @export
#'
#' @examples
plotRadar = function(dataTracks, groups, directionCatGroupName, cumulativeGroupName, summary.fun.name = "sum", 
					 type = "polygon", groupings, colorGroupName = NULL, fillGroupName = NULL, #alphaGroupName = NULL, 
					 replicateGroupName = NULL, percentage = FALSE, replicate.summary.fun.name = "mean",
					 #colorReverseOrder = FALSE, 
					 start.angle = 0, 
					 stat.measure = NULL, stat.method = NULL, stat.extras = NULL,
					 facet.row = NULL, facet.col = NULL, title = NA, facet.wrap = FALSE,
					 colorAlpha = 1.0, fillAlpha = 0.5, is.dark = FALSE,
					 line.size = NULL, facet.label.fill.color = "#FFFFFF00", facet.text.face = "bold",
					 show.y.axis = FALSE, subtitle = NA,
					 plot.subtitle.hjust = 0.5, plot.subtitle.size = 10, plot.subtitle.face = "italic",
					 browse = 0, verbose = FALSE, skip.radar = FALSE, skip.degrees = FALSE, benchmark = FALSE){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	if(benchmark) startTime = benchMark()
	if(verbose) cat("Calculating aggrate results...\n")
	
	allGroupswoRep = unique(c(colorGroupName, fillGroupName, facet.row, facet.col))
	allGroupswRep = unique(c(allGroupswoRep, replicateGroupName))
	
	
	# Making a numeric direction category for better plotting
	directionCatNumericGroupName = paste(directionCatGroupName, "NUMERIC", sep = "_")
	dataTracks[[directionCatNumericGroupName]] = round( # Intervals are rounded, imprecise, after converting to degrees
			unlist( # We need a vector to insert into data frame
				lapply( # Taking mean of lower and higher bounds
					lapply( # Converting terms to numeric
						# Splitting by the comma after removing brackets "(" and "]"
						strsplit(stringr::str_remove_all(as.character(dataTracks[[directionCatGroupName]]),
														 "[\\(\\]]"), ","), as.numeric), mean)) * 180 / pi)
	
	if(benchmark) startTime = benchMark("Groups...", startTime)
	
	if(!directionCatNumericGroupName %in% groups){groups = c(directionCatNumericGroupName, groups)}
	
	if(!is.null(replicateGroupName)){
		groupswoRep = groups[groups != replicateGroupName]
	}else{
		groupswoRep = groups
	}
	
	by.list = list()
	for(group in groups){
		by.list[[group]] = dataTracks[[group]]
	}
	
	if(is.character(summary.fun.name)){
		if(summary.fun.name != "NULL"){
			summary.fun = match.fun(summary.fun.name)
		}else{
			summary.fun = NULL # Error
		}
	}else{
		summary.fun = NULL # Error
	}

	if(is.character(replicate.summary.fun.name)){
		if(replicate.summary.fun.name != "NULL"){
			replicate.summary.fun = match.fun(replicate.summary.fun.name)
		}else{
			replicate.summary.fun = NULL
		}
	}else{
		replicate.summary.fun = NULL
	}
	
	if(benchmark) startTime = benchMark("Aggregates...", startTime)
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	
	if(!is.null(summary.fun)){
		#cumulativeDispl = aggregate(dataTracks[, cumulativeGroupName], by = by.list, FUN = summary.fun)
		if(summary.fun.name == "length"){
			cumulativeDispl = dataTracks %>% group_by_at(groups) %>% summarise_at(cumulativeGroupName, summary.fun)
		}else{
			cumulativeDispl = dataTracks %>% group_by_at(groups) %>% summarise_at(cumulativeGroupName, summary.fun, na.rm = TRUE)
		}
	}else{
		#Error cumulativeDispl = dataTracks[, cumulativeGroupName]
	}
	
	aggrRep = data.frame(); aggrRepMerged = data.frame(); aggrRepMergedNice = data.frame()
	aggr = data.frame(); aggrNice = data.frame() 
	if(!is.null(replicateGroupName)){
		# All tracks separated by all groupings including replicates
		aggrRep = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(cumulativeGroupName, length)
		# All tracks separated by all groupings WITHOUT replicates, hence replicates are summarised
		aggrRepMerged = aggrRep %>% group_by_at(allGroupswoRep) %>% summarise_at(cumulativeGroupName, length)
		# aggrRepMerged with nicer titles for output table
		aggrRepMergedNice = aggrRepMerged
		newLabels = as.character(as.character(groupings$labels)[match(colnames(aggrRepMergedNice), groupings$names)])
		newLabels[length(newLabels)] = "Number of replicates"
		colnames(aggrRepMergedNice) = newLabels
	}
	
	aggr = dataTracks %>% group_by_at(allGroupswRep) %>% summarise_at(cumulativeGroupName, length)
	aggrNice = aggr
	colnames(aggrNice) = c(as.character(groupings$labels)[match(colnames(aggrNice)[1:(ncol(aggrNice) - 1)], groupings$names)], 
						   "Number of tracks")
	
	by.list.rep = by.list; by.list.rep[replicateGroupName] = NULL
	if(!is.null(replicate.summary.fun)){
		#cumulativeDispl = aggregate(dataTracks[, cumulativeGroupName], by = by.list, FUN = summary.fun)
		if(replicate.summary.fun.name == "length"){
			cumulativeDispl = cumulativeDispl %>% group_by_at(groupswoRep) %>% 
				summarise_at(cumulativeGroupName, replicate.summary.fun())
		}else{
			cumulativeDispl = cumulativeDispl %>% group_by_at(groupswoRep) %>% 
				summarise_at(cumulativeGroupName, replicate.summary.fun, na.rm = TRUE)
		}
	}else{
		#Error cumulativeDispl = dataTracks[, cumulativeGroupName]
	}
	
	colnames(cumulativeDispl)[length(colnames(cumulativeDispl))] = cumulativeGroupName
	
	
	if(summary.fun.name == "length" && percentage){
		percSummaryGroups = names(by.list.rep)
		percSummaryGroups = percSummaryGroups[percSummaryGroups != directionCatNumericGroupName]
		
		names(cumulativeDispl)[names(cumulativeDispl) == cumulativeGroupName] = "X_PERC"
		cumulativeDispl = cumulativeDispl %>% 
			group_by_at(percSummaryGroups) %>% 
			mutate(X_PERC = 100 * X_PERC / sum(X_PERC)) #ddply(cumulativeDispl, paste0("`", percSummaryGroups, "`"),transform,X_PERC=100*X_PERC/sum(X_PERC))
		names(cumulativeDispl)[names(cumulativeDispl) == "X_PERC"] = cumulativeGroupName
		
	}
	
	if(benchmark) startTime = benchMark("Aggregates...", startTime)
	if(verbose) cat("Computing h- and vlines...\n")
	y_lims = c(0, max(cumulativeDispl[[cumulativeGroupName]]))
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	
	if(benchmark) startTime = benchMark("Group expressions...", startTime)
	directionCatNumericGroup = nameToExpr(directionCatNumericGroupName)
	cumulativeGroup = nameToExpr(cumulativeGroupName)
	colorGroup = nameToExpr(colorGroupName); fillGroup = nameToExpr(fillGroupName)
	
	if(benchmark) startTime = benchMark("Generating plot...", startTime)
	if(verbose) cat("Generating plot...\n")
	breaks = seq(y_lims[1], y_lims[2], by = (y_lims[2] - y_lims[1]) / 4)
	breaksAngle = seq(0, 360-1, by = angleCatBy * 180 / pi)
	plot = ggplot() + 
		scale_x_continuous(limits = c(-15, 345), expand = c(0, 0), breaks = breaksAngle) +
		scale_y_continuous(limits = c(0, y_lims[2]), breaks = breaks)
	
	if(is.null(line.size)){
		line.size = y_lims[2] / 400
	}
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	
	colorGroupIndices = 1:length(levels(cumulativeDispl[[colorGroupName]]))
	#if(colorReverseOrder){colorGroupIndices = rev(colorGroupIndices)}
	
	if(is.dark){
		contrast.color = "white"
	}else{
		contrast.color = "black"
	}
	stat.text.color = contrast.color
	
	#for(i in colorGroupIndices){
		#thisCol = levels(cumulativeDispl[[colorGroupName]])[i]
	if(type == "polygon"){
		hgridlines = expand.grid(x=c(breaksAngle, breaksAngle[1]), y=breaks)
		plot = plot + geom_path(hgridlines, mapping = aes(x = x, y = y, group = y), size = line.size/4, color = contrast.color)
		vgridlines = expand.grid(x=breaksAngle, y=y_lims)
		plot = plot + geom_path(vgridlines, mapping = aes(x = x, y = y, group = x), size = line.size/4, color = contrast.color)
		plot = plot + geom_polygon(data = cumulativeDispl %>% 
								   	complete(DIRECTION_CARDINAL_NUMERIC = seq(0, 330, 30), 
								   			 TMX_GROUP_A = unique(cumulativeDispl$TMX_GROUP_A), 
								   			 fill = list(NUMBER_SPOTS = 0)), #cumulativeDispl[cumulativeDispl[[colorGroupName]] == thisCol, ], 
								   aes(x = !!directionCatNumericGroup, y = !!cumulativeGroup, color = !!colorGroup, 
								   	fill = !!fillGroup, group = !!colorGroup), size = line.size)# + 
			#geom_polygon()
	}else if(type == "bar"){
		plot = plot + geom_hline(yintercept = breaks, color = "grey90", size = line.size, color = contrast.color) +
			geom_vline(xintercept = breaksAngle, color = "grey90", size = line.size, color = contrast.color) + 
			geom_bar(data = cumulativeDispl, width = 30,
					 aes(x = !!directionCatNumericGroup, y = !!cumulativeGroup, color = !!colorGroup, 
					 	fill = !!fillGroup, group = !!colorGroup), 
					 size = line.size, stat = "identity", alpha = fillAlpha, position = "identity")
			
	}
	#}
	
	#return(list(plot = plot, stat = NULL, replicates = NULL, tracks = NULL))
	
	if(benchmark) startTime = benchMark("Facets, titles, colors...", startTime)
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	plot = facetPlot(plot, row = facet.row, col = facet.col, groupings = groupings, facet.wrap)
	
	plot = titlePlot(p = plot, title = title)
	if(is.null(subtitle)){
		firstPart = ""
		if(!is.null(replicateGroupName)){
			firstPart = paste0("n=", nrow(aggregate(reformulateT(replicateGroupName, "."), data = dataTracks, length, na.action = NULL)), " , ")
		}
		aggr = tryCatch(aggregateByGroups(data = dataTracks, groups = c(groups[groups != directionCatGroupName], replicateGroupName), length), 
				 error = retNULL)[[cumulativeGroupName]]
		#aggr = aggregate(reformulateT(c(groups[groups != directionCatGroupName], replicateGroupName), "."), data = dataTracks, length)[[cumulativeGroupName]]
		subtitle = paste0(firstPart, "# of tracks each group/replicate=", min(aggr), '-', max(aggr))
	}
	
	if(!is.na(subtitle)) plot = subtitlePlot(p = plot, subtitle = subtitle)
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	if(verbose) cat("Setting color scales...\n")
	
	plot = colorPlot(plot, dataTracks, groupings, colorGroupName, colorAlpha, is.dark)
	
	plot = fillPlot(plot, dataTracks, groupings, fillGroupName, fillAlpha, is.dark)
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	
	if(verbose) cat("Setting labels...\n")
	if(benchmark) startTime = benchMark("Labels...", startTime)
	plot = plot + xlab("") + ylab("")
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	
	if(!skip.degrees){
		#x.labels = seq(0 + 180, 330 + 180, by = 30) %% 360 - 180; x.labels = x.labels * -1 #x.labels[7] = abs(x.labels[7])
		#plot = plot + scale_x_discrete(breaks = c(levels[1:12]), drop = FALSE, labels = paste0(x.labels, "°"))
	}
	#Legend title
	if(!is.null(colorGroupName)){
		
	}
	if(!is.null(fillGroupName)){
		plot = plot + labs(fill = as.character(getGLab(groupings, fillGroupName)))
	}
	
	if(!skip.radar) {
		if(type == "polygon"){
			plot = plot + ggiraphExtra::coord_radar(start = start.angle - pi * 7/ 12, direction = -1)	
		}else if(type == "bar"){
			# pi * 7 / 12 = 105° required to keep 0 at x axis as norm
			plot = plot + coord_polar(theta = "x", start = start.angle - pi * 7/ 12, direction = -1)
		}
	}
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	if(benchmark) startTime = benchMark("Theme...", startTime)
	plot = setThemeBase(plot, is.dark, plot.subtitle.hjust, plot.subtitle.size, plot.subtitle.face, facet.label.fill.color, facet.text.face)
	
	if(verbose) cat("Setting theme...\n")
	plot = plot + theme(strip.text.x = element_text(size = rel(0.8)), axis.text.x = element_text(size = rel(0.8)), 
			  axis.line.x = element_blank(), panel.grid.major = element_blank())
	
	if(!show.y.axis) plot = plot + theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
										 axis.ticks.y = element_blank())
	dirPlot = plot
	# Histogram of each group to check data distribution
	directionGroupName = cardinalCols[[directionCatGroupName]]
	#browser()
	circDataModel = groupedCircDataModel(dataTracks, directionGroupName, directionCatGroupName, cumulativeGroupName,
									groupings, allGroupswRep, allGroupswoRep)#, summary.fun
									#colorGroup, fillGroup, colorGroupName, fillGroupName, 
									#facet.row, facet.col, facet.wrap, 
									#colorAlpha, fillAlpha, is.dark
									#)
	
	# QQ plots of each group to check data normality
	#qq = circQQ(dataTracks, x, directionCatGroupName, groupings, colorGroup, fillGroup, 
	#			 colorGroupName, fillGroupName, facet.row, facet.col, facet.wrap, colorAlpha, fillAlpha, is.dark)
	# Data shape
	#circularShape = groupedCircularShape(dataTracks, groupings, c(x, allGroupswoRep), y)
	
	
	summaryStats = data.frame()
	for(title in names(circDataModel)){
		circGroupData = circDataModel[[title]]
		firstMoment = trigonometric.moment(circGroupData, p = 1)
		secondMoment = trigonometric.moment(circGroupData, p = 2)
		secondMomentCentered = trigonometric.moment(circGroupData, p = 2, center = TRUE)
		circV = 1 - firstMoment$rho
		circSkewness = secondMomentCentered$sin / (circV**(3/2))
		circKurtosis = (secondMomentCentered$cos - firstMoment$rho**4)/(circV**2)
		summaryStat = data.frame(`Title` = title, `n` = firstMoment$n, 
								 `μ` = firstMoment$mu * 180/pi, 
								 `median` = median.circular(circGroupData) * 180/pi,
								 `R` = firstMoment$rho, # concentration parameter of the distribution. rho must be between 0 and
								 `V` = circV, # Sample circular variance
								 `σ` = sd.circular(circGroupData),
								 `δ` = (1-secondMoment$rho)/(2*(firstMoment$rho)**2), # The sample circular dispersion
								 `w` = range.circular(circGroupData), # Circular range
								 `Angular. Dev.` = angular.deviation(circGroupData),
								 `s` = circSkewness, 
								 `k` = circKurtosis)
		summaryStats = rbind(summaryStats, summaryStat)
	}
	
	uniformityTests = data.frame()
	uniformityOutput = ""
	for(title in names(circDataModel)){
		
		circGroupData = circDataModel[[title]]
		
		rayleigh = rayleigh.test(circGroupData)
		watson = watson.test(circGroupData)
		
		testOutput = capture.output(rayleigh)
		uniformityOutput = paste(c(uniformityOutput, "", title, testOutput[testOutput != ""]), collapse = "\n")
		testOutput = capture.output(watson)
		uniformityOutput = paste(c(uniformityOutput, "", title, testOutput[testOutput != ""]), collapse = "\n")
		
		uniformityTest = data.frame(`Title` = title,
				   `Rayleigh statistic` = rayleigh$statistic,
				   `Rayleigh p` = rayleigh$p
		)
		uniformityTests = rbind(uniformityTests, uniformityTest)
		
	}
	
	vonMisesGoodnessOfFit = ""
	for(title in names(circDataModel)){
		circGroupData = circDataModel[[title]]
		#browser()
		watson = watson.test(circGroupData, dist = "vonmises")
		testOutput = capture.output(watson)
		vonMisesGoodnessOfFit = paste(c(vonMisesGoodnessOfFit, "", title, testOutput[testOutput != ""]), collapse = "\n")
	}
	
	print("=========================")
	print(stat.measure)
	print(stat.method)
	print(stat.extras)
	print("=========================")
	
	statOut = NULL
	if(!is.null(stat.measure)){
		statMethod = match.fun(stat.method)
		statOut = tryCatch({statMethod(circDataModel)}, error = retNULL)
	}else{
		statOut = NULL
	}
	
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	return(list(plot = dirPlot, stat = statOut, replicates = aggrRepMergedNice, tracks = aggrNice, 
				circDataModel = circDataModel, summaryStats = summaryStats %>% tibble(), uniformityDF = uniformityTests, 
				uniformityText = uniformityOutput, vonMisesFit = vonMisesGoodnessOfFit))
	#return(plot)
}

#' Parses file format of chemotaxis tool.
#'
#' @param filePath File path
#' @param groupText Group identifier text
#' @param fileGroup Data frame of groups
#' @param recalculate Recalculation of features option
#' @param browse 
#' @param ... 
#'
#' @return Returns a list of data frames: list(trks = tracks, trajs = trajectories, feats = features, unts = units)
#' @export
#'
#' @examples
parseChemotaxisToolFile = function(filePath, groupText, fileGroup, recalculate = FALSE, browse = 0, ...){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	#browser()
	args = list(...)
	calibrationUnits = args$calibrationUnits
	unts = data.frame(spatialunits = "μm", timeunits = "sec")
	#filePath = "/home/stasciya/Desktop/Lab Work/for Julian/Analysis_pointVSline/Line_0nM_I_20180724_Col_PLB_fMLF-Titration_Results from P10 in µm per sec.txt" UTF!
	#filePath = "~/Desktop/Lab Work/for Mirka/20181030_2_5_combined.txt" ISO!
	
	trajs = data.frame()
	trajs = read.csv(file = filePath, sep = "\t", dec = ".")
	if(nrow(trajs) == 0){
		trajs = read.csv(file = filePath, sep = "\t", dec = ".", fileEncoding = "ISO-8859-1")
	}
	colnames(trajs) = c("spot_global_id", "TRACK_ID", "FRAME", "POSITION_X", "POSITION_Y", "DISPLACEMENT", "VELOCITY", 
					   "MEAN_INTENSITY")
	trajs["POSITION_Z"] = 0; trajs["POSITION_T"] = trajs["FRAME"]
	trajs$POSITION_X = trajs$POSITION_X * calibrationUnits$pixelWidth
	trajs$POSITION_Y = trajs$POSITION_Y * calibrationUnits$pixelHeight
	trajs$POSITION_Z = trajs$POSITION_Z * calibrationUnits$voxelThickness
	trajs$POSITION_T = trajs$POSITION_T * calibrationUnits$frameIntervall
	
	#trajs = trajs[c(1:5, 9:10, 6:8)]
	trajs[c("QUALITY", "RADIUS", "VISIBILITY", "MANUAL_COLOR", 
		   "MEDIAN_INTENSITY", "MIN_INTENSITY", "MAX_INTENSITY", "TOTAL_INTENSITY", 
		   "STANDARD_DEVIATION", "ESTIMATED_DIAMETER", "CONTRAST", "SNR")] = NA
	trajs = trajs %>% group_by(TRACK_ID) %>% 
		mutate(DISPLACEMENT = lead(DISPLACEMENT), VELOCITY = lead(VELOCITY),
					  SPOT_SOURCE_ID = c(spot_global_id[1:(n()-1)], NA), SPOT_TARGET_ID = c(spot_global_id[2:n()], NA), 
					  EDGE_TIME = (lead(POSITION_T) + POSITION_T) / 2, 
					  EDGE_X_LOCATION = (lead(POSITION_X) + POSITION_X)/2, 
					  EDGE_Y_LOCATION = (lead(POSITION_Y) + POSITION_Y)/2, 
					  EDGE_Z_LOCATION = (lead(POSITION_Z) + POSITION_Z)/2)
	trajs[c("LINK_COST", "RADIUS", "VISIBILITY", "MANUAL_COLOR", 
		   "MEDIAN_INTENSITY", "MIN_INTENSITY", "MAX_INTENSITY", "TOTAL_INTENSITY", 
		   "STANDARD_DEVIATION", "ESTIMATED_DIAMETER", "CONTRAST", "SNR")] = NA
	
	trks = data.frame(matrix(ncol = length(tracksColNames), nrow = 0))
	colnames(trks) = tracksColNames
	
	trks = trajs %>% group_by(TRACK_ID)
	#trajByTrackID %>% summarise(NUMBER_SPOTS = n(), TRACK_DISPLACEMENT = sqrt((last(POSITION_X) - first(POSITION_X)) ** 2 + (last(POSITION_Y) - first(POSITION_Y))  ** 2))
	trks = trks %>% summarise(NUMBER_SPOTS = n(),
						TRACK_DISPLACEMENT = sqrt((last(POSITION_X) - first(POSITION_X)) ** 2 + (last(POSITION_Y) - first(POSITION_Y))  ** 2), 
						TRACK_DURATION = last(FRAME) - first(FRAME), 
						TRACK_START = first(FRAME), TRACK_STOP = last(FRAME),
						TRACK_X_LOCATION = mean(POSITION_X), TRACK_Y_LOCATION = mean(POSITION_Y),
						TRACK_INDEX = first(TRACK_ID), 
						TRACK_MEAN_SPEED = mean(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + (lead(POSITION_Y) - POSITION_Y) ** 2)) / (lead(POSITION_T) - POSITION_T) , na.rm = TRUE),
						TRACK_MAX_SPEED = max(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + (lead(POSITION_Y) - POSITION_Y) ** 2)) / (lead(POSITION_T) - POSITION_T), na.rm = TRUE),
						TRACK_MIN_SPEED = min(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + (lead(POSITION_Y) - POSITION_Y) ** 2)) / (lead(POSITION_T) - POSITION_T), na.rm = TRUE),
						TRACK_MEDIAN_SPEED = median(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + (lead(POSITION_Y) - POSITION_Y) ** 2)) / (lead(POSITION_T) - POSITION_T), na.rm = TRUE),
						TRACK_STD_SPEED = sd(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + (lead(POSITION_Y) - POSITION_Y) ** 2)) / (lead(POSITION_T) - POSITION_T), na.rm = TRUE))
	trks[tracksColNames[! tracksColNames %in% colnames(trks)]] = NA
	
	feats = rbind(trajFeats, trackFeats)
	feats = rbind(feats, featsDir)
	feats = rbind(feats, featsPathLength)
	feats = rbind(feats, featsPersistence)
	feats = rbind(feats, featsVelocity)
	
	unts = cbind(unts, fileGroup)
	feats = cbind(feats, fileGroup)
	trks = cbind(trks, fileGroup)
	trajs = cbind(as.data.frame(trajs), fileGroup)
	
	feats$feature = as.character(feats$feature)
	
	return(list(trks = trks, trajs = trajs, feats = feats, unts = unts))
}

#' Parses imaris exported xlsx files
#'
#' @param filePath File path
#' @param groupText Group identifier text
#' @param fileGroup Data frame of groups
#' @param recalculate Recalculation of features option
#' @param browse 
#'
#' @return Returns a list of data frames: list(trks = tracks, trajs = trajectories, feats = features, unts = units)
#' @export
#'
#' @examples
parseImarisXLSXFile = function(filePath, groupText, fileGroup, recalculate = FALSE, browse = 0){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
#parseImarisFiles = function(files, groupings, groups, browse = 0, 
#							updateProgress = NULL, fileNames = NULL, closeProgress = NULL, initializeProgress = NULL) {
	#browser()
	sheets = excel_sheets(filePath)
	cat("\t");cat(paste("Parsing trajectory features - position for", groupText));cat("\n")
	essentialSheets = c("Position", "Time")
	
	if(!all(essentialSheets %in% sheets)){
		cat("\t");cat(paste("Essential sheets missing for", groupText));cat("\n")
		return(NULL)
	}
	
	positionsRaw = readxl::read_excel(path = filePath, sheet = "Position", skip = 1) %>% 
		select(c(imarisCoreNames, imarisPosNames)) %>% arrange(TRACK_ID, FRAME) %>% filter(!is.na(TRACK_ID))
	
	cat("\t");cat(paste("Parsing trajectory features - time for", groupText));cat("\n")
	timesRaw = read_excel(path = filePath, sheet = "Time", skip = 1) %>% rename(Time_ = Time...1, Time = Time...4) %>% 
		select(c(imarisCoreNames, imarisTimeNames, timeunits = "Unit")) %>% arrange(TRACK_ID, FRAME) %>% filter(!is.na(TRACK_ID))
	
	trajs = positionsRaw %>% left_join(timesRaw, by = c("SPOT_ID", "TRACK_ID", "FRAME"))
	trajs = trajs %>% group_by(TRACK_ID) %>% 
		mutate(SPOT_SOURCE_ID = c(SPOT_ID[1:(n()-1)], NA), SPOT_TARGET_ID = c(SPOT_ID[2:n()], NA), 
			   EDGE_TIME = (lead(POSITION_T) + POSITION_T) / 2,
			   EDGE_X_LOCATION = (lead(POSITION_X) + POSITION_X)/2, 
			   EDGE_Y_LOCATION = (lead(POSITION_Y) + POSITION_Y)/2, 
			   EDGE_Z_LOCATION = (lead(POSITION_Z) + POSITION_Z)/2)
	
	#browser()
	for(i in 1:length(imarisTrajectoryModel)){
		item = imarisTrajectoryModel[[i]]
		trajs = imarisImportTrajectories(trajs, filePath, item$sheetName, item$exprs, sheets, item$names,
										groupText, recalculate, imarisCoreNames, item$specNames, item$title)
	}
	
	trajs = trajs %>% group_by(TRACK_ID) %>% 
		mutate(EDGE_DIRECTION_PHI = atan2(-1 * get(displacementColumns[2]), get(displacementColumns[1])) %% (2 * pi), 
			   EDGE_DIRECTION_THETA = acos(get(displacementColumns[3]) / DISPLACEMENT) %% pi)
	trajs = trajs %>% group_by(TRACK_ID) %>% mutate(TURN_ANGLE = lead(EDGE_DIRECTION_PHI) - EDGE_DIRECTION_PHI)
	
	unts = trajs %>% ungroup() %>% select(spatialunits, timeunits) %>% summarise(spatialunits = first(spatialunits), 
																				   timeunits = first(timeunits))
	
	trajs = trajs %>% select(-spatialunits, -timeunits)
	trks = trajs %>% summarise(TRACK_ID = unique(TRACK_ID))
	# list(sheetName = "", specNames = , title = "",
	# 	 exprs = "")
	for(i in 1:length(imarisTrackModel)){
		item = imarisTrackModel[[i]]
		trks = imarisImportTracks(trks, trajs, filePath, item$sheetName, item$exprs, item$summarize, sheets, item$names, 
								  groupText, recalculate, imarisTrackCoreNames, item$specNames, item$title)
	}
	
	feats = rbind(trajFeats, trackFeats)
	feats = rbind(feats, featsDir)
	feats = rbind(feats, featsPathLength)
	feats = rbind(feats, featsPersistence)
	feats = rbind(feats, featsVelocity)
	
	unts = cbind(unts, fileGroup)
	feats = cbind(feats, fileGroup)
	trks = cbind(trks, fileGroup)
	trajs = cbind(trajs, fileGroup)
	
	feats$feature = as.character(feats$feature)
	
	return(list(trks = trks, trajs = trajs, feats = feats, unts = unts))
	#attr(data[[dims$feature[i]]], "unit") = unitstr
}

#' Checks for XML file errors such as missing features (attributes) in TrackMate files. If there are, these are complemented with missing values
#'
#' @param filePath File path
#' @param groupText Group identifier text
#' @param xmlAttrs XML attributes
#'
#' @return Returns complemented xml attributes
#' @export
#'
#' @examples
analyzeXMLError = function(filePath, groupText, xmlAttrs){
	cat(paste("Checking for errors", groupText)); cat("\n")
	prevxmlAttr = xmlAttrs[[1]]
	if(length(xmlAttrs) > 1){
		for(i in 2:length(xmlAttrs)){
			xmlAttr = xmlAttrs[[i]]
			if(length(prevxmlAttr) > length(xmlAttr)){
				cat(paste("\tMissing columns:")); cat("\n")
				cat(prevxmlAttr); cat("\n")
				cat("---------------------------------------"); cat("\n")
				cat(xmlAttr); cat("\n")
				newxmlAttr = prevxmlAttr
				for(name in names(newxmlAttr)){newxmlAttr[[name]] = "0"}
				for(name in names(xmlAttr)){newxmlAttr[[name]] = xmlAttr[[name]]}
				xmlAttrs[[i]] = newxmlAttr
				prevxmlAttr = newxmlAttr
			}
		}
	}
	return(xmlAttrs)
}

#' Parses a TrackMate file
#'
#' @param filePath File path
#' @param groupText Group identifier text
#' @param fileGroup Data frame of groups
#' @param recalculate Recalculation of features option
#' @param browse 
#'
#' @return
#' @export
#'
#' @examples
parseTMFile = function(filePath, groupText, fileGroup, recalculate = FALSE, browse = 0){
	if(browse == 1){browser()}
	
	xmlDoc = read_xml(filePath)
	xmlList = as_list(xmlDoc)
	
	cat("\t");cat(paste("Parsing features for", groupText));cat("\n")
	
	# Reading XML Model
	model = xml_find_first(xmlDoc, xpath = "/TrackMate/Model")
	model_attrs = as.list(xml_attrs(model))
	unts = bind_rows(list(model_attrs))
	row.names(unts) = NULL
	
	feats = data.frame()
	
	# Load each feature type
	for(featureType in featureTypes){
		typeFeaturesXML = xml_find_all(xmlDoc, xpath = paste0('/TrackMate/Model/FeatureDeclarations/', 
															  featureType, 'Features/Feature'))
		type_features_attrs = xml_attrs(typeFeaturesXML)
		type_features_attrs = lapply(type_features_attrs, as.list)
		typeFeatures = bind_rows(type_features_attrs)
		typeFeatures$type = featureType
		
		feats = rbind(feats, typeFeatures)
	}
	# Converting factors to better types
	feats$feature = as.character(feats$feature); feats$name = as.character(feats$name)
	feats$shortname = as.character(feats$shortname)
	feats$isint = as.logical.factor(as.factor(feats$isint))
	feats$type = as.factor(feats$type)
	feats$dimension = as.factor(feats$dimension)
	row.names(feats) = NULL
	
	cat("\t");cat(paste("Parsing tracks for", groupText));cat("\n")
	# Loading tracks
	#browser()
	xmlTrackAttrs = xml_attrs(xml_find_all(xmlDoc, xpath = '/TrackMate/Model/AllTracks//Track'))
	xmlTrackAttrs = analyzeXMLError(filePath = filePath, groupText = groupText, xmlAttrs = xmlTrackAttrs)
	xmlTrackAttrs = lapply(xmlTrackAttrs, as.list)
	trks = bind_rows(xmlTrackAttrs)
	#TODO check for NaN and Infinity values within trackmate files and report back to users!
	
	row.names(trks) = NULL
	# Getting track feature types to set tracks data frame variable types
	trks = setFeatureTypesinData(trks, feats, "Track")
	
	cat("\t");cat(paste("Parsing spots for", groupText));cat("\n")
	# Loading spots
	xmlSpotAttrs = xml_attrs(xml_find_all(xmlDoc, xpath = '/TrackMate/Model/AllSpots//Spot'))
	xmlSpotAttrs = analyzeXMLError(filePath = filePath, groupText = groupText, xmlAttrs = xmlSpotAttrs)
	xmlSpotAttrs = lapply(xmlSpotAttrs, as.list)
	spots = bind_rows(xmlSpotAttrs)
	
	row.names(spots) = NULL
	spots = setFeatureTypesinData(spots, feats, "Spot")
	
	# Sorting spots chronologically
	spots = spots %>% arrange(FRAME)
	
	cat("\t");cat(paste("Parsing edges for", groupText));cat("\n")
	# Loading edges with associated tracks IDs from parent nodes
	edge.nodes = xml_find_all(xmlDoc, ".//Edge")
	edge_attrs = xml_attrs(xml_find_all(xmlDoc, xpath = '/TrackMate/Model/AllTracks//Edge'))
	edge_attrs = lapply(edge_attrs, as.list)
	edges = bind_rows(edge_attrs)
	#edges = data.frame(t(data.frame(xml_attrs(xml_find_all(xmlDoc, xpath = '/TrackMate/Model/AllTracks//Edge')))))
	row.names(edges) = NULL
	edges = setFeatureTypesinData(edges, feats, "Edge")
	edges = cbind(data.frame(TRACK_ID = as.numeric(as.character(xml_find_first( edge.nodes, ".//ancestor::Track") %>% xml_attr("TRACK_ID")))), 
	              edges
	              )
	
	# Sorting spots chronologically and according to track ID
	edges = edges %>% dplyr::arrange(EDGE_TIME) %>% dplyr::arrange(TRACK_ID)
	
	# It is nicer if we can keep matching column (SPOT_SOURCE_ID) of the edges data frame
	edges = edges %>% mutate(SPOT_ID = SPOT_SOURCE_ID)
	
	cat("\t");cat(paste("Constructing trajectories for", groupText));cat("\n")
	# Trajectories are generated by merging spots and edges
	
	trajs = merge(spots, edges, by.x = "ID", by.y = "SPOT_ID", all.x = TRUE)
	
	#View(merge(spots, edges, by.x = "ID", by.y = "SPOT_ID", all.x = TRUE))
	
	# Track ID is missing for the last point in the data frame, to set it...
	# OLD simple but risky method: trajs[, ]$TRACK_ID = trajs[which(is.na(trajs$ID == trajs$SPOT_SOURCE_ID)) - 1, ]$TRACK_ID
	#cat(nrow(trajs))
	
	cat("\t");cat(paste("Removing trackless spots for", groupText));cat("\n")
	
	# There might be spots, which do not belong to any track. Since they are irrelevant for this purpose, they 
	# can be discarded
	# new trajs %>% mutate(TRACK_ID = if_else(is.na(TRACK_ID), if_else(ID %in% SPOT_TARGET_ID, TRACK_ID[match(ID, SPOT_TARGET_ID)], TRACK_ID), TRACK_ID))
	trajs$ID = as.numeric(as.character(trajs$ID))
	trajs$SPOT_SOURCE_ID = as.numeric(as.character(trajs$SPOT_SOURCE_ID))
	
	nSpots = nrow(trajs)
	trajs = trajs %>% mutate(TRACK_ID = if_else(is.na(TRACK_ID), if_else(ID %in% SPOT_TARGET_ID, TRACK_ID[match(ID, SPOT_TARGET_ID)], TRACK_ID), TRACK_ID))
	trajs = trajs %>% filter(!is.na(TRACK_ID))
	removeCounter = nSpots - nrow(trajs)
	# 
	# trajs$Remove = FALSE # Keep track of spots to be removed. DO NOT remove immediately since we are iterating!
	# removeCounter = 0
	# browser()
	# for(i in which(is.na(trajs$ID == trajs$SPOT_SOURCE_ID))){
	# 	# getting Track ID of an NA on the trajs side
	# 	# Track ID can be NA if it is the last spot (due to merging) or the spot has no track
	# 	# This line may return NAs and ID is actually SPOT_SOURCE_ID, so this will return at least one TRUE (with bunch of NAs and FALSEs), that TRUE will capture TRACK_ID
	# 	trackID = trajs$TRACK_ID[trajs$SPOT_TARGET_ID == trajs$ID[i]] 
	# 	trackID = trackID[!is.na(trackID)] # Here NAs are removed to leave numbers only
	# 	#cat(paste(i, trajs$TRACK_ID[i], trajs$SPOT_ID[i], trackID))
	# 	if(length(trackID) == 1){
	# 		trajs$TRACK_ID[i] = trackID # This is also to fix NA value at Track ID last spot!
	# 	}else{
	# 		browser()
	# 		#cat(paste("Removing",i,"..."))
	# 		trajs$Remove[i] = TRUE # Mark to remove
	# 		removeCounter = removeCounter + 1
	# 	}
	# }
	cat("\t");cat(paste("Removing trackless spots for", groupText, removeCounter, " were found."));cat("\n")
	# Finally removing, what needs to be removed
	# trajs = trajs[!trajs$Remove,]
	#cat(nrow(trajs))
	# We don't need this variable anymore
	# trajs$Remove = NULL
	
	cat("\t");cat(paste("Sorting spots for", groupText));cat("\n")
	
	# Sorting according to tracks ID and chronologically
	trajs = trajs[order(trajs$TRACK_ID, trajs$FRAME), ]
	
	# Adding grouping information and adding to final data frame
	#unts = cbind(unts, groups[r, -ncol(groups)])
	
	
	# Adding new calculated features to features table
	featsFix = feats[2:5, ]; 
	featsFix$feature = paste(featsFix$feature, "FIX", sep = "_")
	featsFix$name = paste(featsFix$name, " fixed at origin", sep = " ")
	featsFix$shortname = paste(featsFix$shortname, "fixed", sep = " ")
	feats = rbind(feats, featsFix)
	featsRot = feats[2:4, ]
	featsRot$feature = paste(featsFix$feature, "ROT", sep = "_")[1:3]
	featsRot$name = paste(featsFix$name, "& rotated towards target", sep = " ")[1:3]
	featsRot$shortname = paste(featsFix$shortname, "& rotated", sep = " ")[1:3]
	
	feats = rbind(feats, featsRot)
	feats = rbind(feats, featsDir)
	feats = rbind(feats, featsPathLength)
	feats = rbind(feats, featsPersistence)
	feats = rbind(feats, featsVelocity)
	feats = rbind(feats, featsStartEndPositions)
	#feats = rbind(feats, pointSourceFeats)
	
	# Adding imaris type velocity
	trajs = trajs %>% mutate(VELOCITY_AVG = avgSpotVelocity(POSITION_X, POSITION_Y, POSITION_Z, POSITION_T))
	feats = rbind(feats, trajFeats %>% filter(feature == "VELOCITY_AVG"))
	trks = trks %>% left_join(trajs %>% group_by(TRACK_ID) %>% 
							  	dplyr::summarise(TRACK_MEAN_SPEED_AVG = mean(VELOCITY_AVG, na.rm = TRUE)), 
							  by = "TRACK_ID")
	
	
	# Correct for missing pore size related values
	if(any(poreSizeColNames %in% colnames(trajs))){
		trajs = poreSizeCorrect(trajs)
	}
	
	unts = cbind(unts, fileGroup, row.names = NULL)
	feats = cbind(feats, fileGroup, row.names = NULL)
	trks = cbind(trks, fileGroup, row.names = NULL)
	trajs = cbind(trajs, fileGroup, row.names = NULL)
	
	
	return(list(trks = trks, trajs = trajs, feats = feats, unts = unts))
}

# List of tooltips for Shiny components
toolTips = list(
	title_In = "Main title of the plot",
	title_check_In = "Main title of the plot to be displayed on top",
	subtitle_In = "Subtitle appears below main title. If empty and a subtitle needs to be displayed, an automatic one will be generated.",
	subtitle_check_In = "Whether or not to display subtitle",
	download_PNG_In = "Download plot in PNG format. This format is for quick diplays. Use SVG for vector graphic format.",
	download_SVG_In = "Download track feature plot in SVG format. This format is more suitable for print and presentations. Also you can edit it easily in a vector graphic application.",
	subtitle_hjust_In = "Subtitle alignment (left/center/right)",
	subtitle_size_In = "Subtitle text size",
	subtitle_text_style_In = "Subtitle text style (italic/bold)",
	xlab_In = "Text to be displayed under the x axis, usually to indicate what grouping is used.",
	ylab_In = "Text to be displayed left of the y axis, usually to indicate what measure is used.",
	unit_In = "Unit conversion for the selected measure. You can choose a compatible unit to convert the values. e.g. μm/sec and μm/min are compatible but μm/min and μm aren\\'t.",
	facet_label_face_In = "Facet label text style (italic/bold)",
	facet_wrap_In = "If you only choose one facet grouping (e.g. drug treatment) but you have many groups (Blebbistatin, Latrunculin, Nocodazole, CK-666 etc.), You may want to wrap the plot to not have a very long plot.",
	facet_label_fill_color_In = "Background color for facet labels.",
	fill_alpha_In = "General transparency of fill colors.",
	color_alpha_In = "General transparency of line/contour colors.",
	dark_In = "This inverts all colors for a plot with black background.",
	stat_method_In = "Which test to use.",
	stat_label_In = "How should the results be displayed. Either p values or if the p value is significat (starts).",
	stat_hidens_In = "ns is displayed for p values \\> 0,05. These labels can be hidden here.",
	stat_pairwise_In = "Should all groups be compared to one another? This is NOT a pairwise test! Only to compared groups to eachother.",
	fill_In = "Fill coloring used for box plots, violin plots etc. Well when, there is something to fill. Colors of each group can be set in the Import/Grouping tab.",
	color_In = "This is used for line (also contours) and dot colors.",
	replicate_In = "Which grouping is the replicates? This is required for propert reporting and automatic subtitle.",
	stat_In = "Which grouping is used to compare groups. Use same grouping as main grouping (\"Group by\").",
	facet_row_In = "Facets divide plots into a grid (Rows in this case). This is useful for multidimensional data e.g. when you have genotpyes, drug treatments and temperature treatment. But you are more interested in the genotypes. So you can use facets to divide plots into sections for the treatments.",
	facet_col_In = "Facets divide plots into a grid (Columns in this case). This is useful for multidimensional data e.g. when you have genotpyes, drug treatments and temperature treatment. But you are more interested in the genotypes. So you can use facets to divide plots into sections for the treatments."
)


# cdat1 <- circular(fisherB10$set1*2*pi/360)
# cdat2 <- circular(fisherB10$set2*2*pi/360)
# cdat3 <- circular(fisherB10$set3*2*pi/360)
# Adapted from Circular statistics in R ISBN: 9780199671137
YgVal = function(cdats) {
	ndat = lengths(cdats)
	N = sum(ndat)
	tbar = rep(NA, length(cdats))
	delhat = rep(NA, length(cdats))
	
	for(i in 1:length(cdats)){
		tm1 = trigonometric.moment(cdats[[i]], p=1)
		tm2 = trigonometric.moment(cdats[[i]], p=2)
		Rbar1 = tm1$rho; Rbar2 = tm2$rho ; tbar[i] = tm1$mu
		delhat[i] = (1-Rbar2)/(2*Rbar1*Rbar1)
	}
	
	dhatmax = max(delhat) ; dhatmin = min(delhat)
	#print(paste(dhatmax, dhatmin, dhatmax/dhatmin))
	if(dhatmax/dhatmin <= 4){
		CP = 0 ; SP = 0 ; dhat0 = 0
		for(i in 1:length(cdats)){
			CP = CP + ndat[[i]]*cos(tbar[i])
			SP = SP + ndat[[i]]*sin(tbar[i])
			dhat0 = dhat0 + ndat[[i]]*delhat[i]
		}
		dhat0 = dhat0/N
		RP = sqrt(CP*CP+SP*SP)
		Yg = 2*(N-RP)/dhat0
		return(Yg)
	}else if(dhatmax/dhatmin > 4){
		CM = 0 ; SM = 0 ; Yg = 0
		for(i in 1:length(cdats)){
			CM = CM + (ndat[[i]]*cos(tbar[i])/delhat[i])
			SM = SM + (ndat[[i]]*sin(tbar[i])/delhat[i])
			Yg = Yg + (ndat[[i]]/delhat[i]) 
		}
		RM = sqrt(CM*CM+SM*SM)
		Yg = 2*(Yg-RM)
		return(Yg)
	}
}

watsons.large.sample.nonparametric.test = function(cdats){
	YgObs = YgVal(cdats)
	pVal = pchisq(YgObs, length(cdats)-1, lower.tail=F)
	return(list(`statistic` = YgObs, `p` = pVal))
}
# cdats23 = list(cdat2, cdat3)
# watson.williams.test.2(cdats = cdats23)

#' Helper function to use [watson.williams.test] in the [circular] package
#'
#' @param cdats is a list of measurements for each sample/group
#'
#' @return Returns statistic results such as F satistic and p-value
#' @seealso [watson.williams.test()]
#' @export
#'
#' @examples
watson.williams.test.2 = function(cdats){
	cdat = c()
	groupID = c()
	for(i in 1:length(cdats)){
		cdat = c(cdat, cdats[[i]])
		groupID = c(groupID, rep(i, length(cdats[[i]])))
	}
	return(watson.williams.test(cdat, groupID))
}

# Adapted from Circular statistics in R ISBN: 9780199671137
MinusPiPi = function(sample) {
	n = length(sample)
	for (j in 1:n) {
		if (sample[j] < -pi) {sample[j] = sample[j] + (2*pi)} else
			if (sample[j] > pi) {sample[j] = sample[j] - (2*pi)}
	}
	return(sample)
}

fisher.nonparametric.test = function(cdats){
	PgObs = PgVal(cdats)
	pVal = pchisq(PgObs, length(cdats) - 1, lower.tail = F)
	return(list(`statistic` = PgObs, p = pVal))
}

# Adapted from Circular statistics in R ISBN: 9780199671137
PgVal = function(cdats) {
	g = length(cdats)
	ndat = lengths(cdats) 
	N = sum(ndat)
	gmedian = median.circular(circular(unlist(cdats)))
	sumterms = 0 ; M = 0 
	for (i in 1:length(cdats)) {
		shiftdat = MinusPiPi(cdats[[i]]-gmedian) ; m = length(shiftdat[shiftdat<0]) ; M = M+m
		sumterms = sumterms + m*m/ndat[i]
	}
	term1 = ((N*N)/(M*(N-M))); term2 = (N*M)/(N-M) ; Pg = term1*sumterms-term2
	return(Pg)
}

# Adapted from Circular statistics in R ISBN: 9780199671137
PgRandTest = function(cdats, NR = 9999) {
	g = length(cdats)
	ndat = lengths(cdats)
	ndatcsum = cumsum(ndat)
	ndatranges = c(0, ndatcsum)
	PgObs = PgVal(cdats) ; nxtrm = 1
	v.cdats = circular(unlist(cdats))
	for(r in 1:NR){
		v.randsamp = sample(v.cdats)
		#browser()
		randsamp = as.list(rep(NA, length(cdats)))
		for(i in 1:length(cdats)){
			start = ndatranges[i] + 1; end = ndatranges[i+1]
			randsamp[[i]] = v.randsamp[start:end]
			#print(lengths(randsamp) == lengths(cdats))
		}
		PgRand = PgVal(randsamp)
		if(PgRand >= PgObs){nxtrm = nxtrm+1}
	}
	pval = nxtrm/(NR+1)
	return(list(`statistic` = PgObs, `p` = pval))
}

# Adapted from Circular statistics in R ISBN: 9780199671137
WallraffTest = function(cdats){
	g = length(cdats)
	ndat = lengths(cdats)
	ndatcsum = cumsum(ndat)
	N = length(cdat); ndatcsum = cumsum(ndat); tbar = circular(0); distdat = 0
	groupID = c()
	for(i in 1:length(cdats)){
		dist = 0 ; sample = circular(0)  
		
		tm1 = trigonometric.moment(cdats[[i]], p=1); tbar[i] = tm1$mu
		for(j in 1:ndat[i]){
			dist[j] = pi-abs(pi-abs(cdats[[i]][j]-tbar[i]))
		}
		distdat = c(distdat, dist)
		groupID = c(groupID, rep(i, length(cdats[[i]])))
	}
	distdat = distdat[-1]
	print(distdat)
	TestRes = kruskal.test(distdat, g=groupID)
	return(TestRes)
}
# 
# WallraffTest2 <- function(cdat, ndat, g) {
# 	N <- length(cdat) ; ndatcsum <- cumsum(ndat) ; tbar <- circular(0) ; distdat <- 0
# 	for (k in 1:g) {
# 		dist <- 0 ; sample <- circular(0)  
# 		if (k==1) {low <- 0} else
# 			if (k > 1) {low <- ndatcsum[k-1]}
# 		for (j in 1:ndat[k]) { sample[j] <- cdat[j+low] }
# 		tm1 <- trigonometric.moment(sample, p=1) ; tbar[k] <- tm1$mu
# 		for (j in 1:ndat[k]) { dist[j] <- pi-abs(pi-abs(sample[j]-tbar[k])) }
# 		distdat <- c(distdat, dist)
# 	}
# 	distdat <- distdat[-1]
# 	gID <- c(rep(1,n1), rep(2,n2), rep(3,n3))
# 	print(distdat)
# 	TestRes <- kruskal.test(distdat, g=gID)
# 	return(TestRes)
# } 


# PgVal = function(cdat, ndat, g) {
# 	N = length(cdat) 
# 	ndatcsum = cumsum(ndat) ; gmedian = median.circular(cdat) 
# 	sumterms = 0 ; M = 0 
# 	for (k in 1:g) {
# 		if (k==1) {low = 0} else
# 			if (k > 1) {low = ndatcsum[k-1]}
# 		sample = circular(0)
# 		for (j in 1:ndat[k]) { sample[j] = cdat[j+low] }
# 		shiftdat = MinusPiPi(sample-gmedian) ; m = length(shiftdat[shiftdat<0]) ; M = M+m
# 		sumterms = sumterms + m*m/ndat[k]
# 	}
# 	term1 = ((N*N)/(M*(N-M))); term2 = (N*M)/(N-M) ; Pg = term1*sumterms-term2
# 	return(Pg)
# }

# YgVal = function(cdat, ndat, g) {
# 	N = length(cdat) ; ndatcsum = cumsum(ndat) 
# 	delhat = 0 ; tbar = 0
# 	for (k in 1:g) {
# 		sample = circular(0)
# 		if (k==1) {low = 0} else
# 			if (k > 1) {low = ndatcsum[k-1]}
# 		for (j in 1:ndat[k]) { sample[j] = cdat[j+low] }
# 		tm1 = trigonometric.moment(sample, p=1)
# 		tm2 = trigonometric.moment(sample, p=2)
# 		Rbar1 = tm1$rho; Rbar2 = tm2$rho ; tbar[k] = tm1$mu
# 		delhat[k] = (1-Rbar2)/(2*Rbar1*Rbar1)
# 	}
# 	dhatmax = max(delhat) ; dhatmin = min(delhat)
# 	#print(paste(dhatmax, dhatmin, dhatmax/dhatmin))
# 	if (dhatmax/dhatmin <= 4) {
# 		CP = 0 ; SP = 0 ; dhat0 = 0
# 		for (k in 1:g) {
# 			CP = CP + ndat[k]*cos(tbar[k])
# 			SP = SP + ndat[k]*sin(tbar[k])
# 			dhat0 = dhat0 + ndat[k]*delhat[k] 
# 		}
# 		dhat0 = dhat0/N
# 		RP = sqrt(CP*CP+SP*SP)
# 		Yg = 2*(N-RP)/dhat0
# 		return(Yg) } 
# 	else if (dhatmax/dhatmin > 4) {
# 		CM = 0 ; SM = 0 ; Yg = 0
# 		for (k in 1:g) {
# 			CM = CM + (ndat[k]*cos(tbar[k])/delhat[k])
# 			SM = SM + (ndat[k]*sin(tbar[k])/delhat[k])
# 			Yg = Yg + (ndat[k]/delhat[k]) 
# 		}
# 		RM = sqrt(CM*CM+SM*SM)
# 		Yg = 2*(Yg-RM)
# 		return(Yg) }
# }
# 
# cdat <- c(cdat1, cdat2, cdat3) 
# n1 <- length(cdat1) ; n2 <- length(cdat2) ; n3 <- length(cdat3) 
# ndat <- c(n1, n2, n3) ; g <- 3
# YgObs <- YgVal(cdat, ndat, g)
# pchisq(YgObs, g-1, lower.tail=F)
# 
# cdats = list(cdat1, cdat2, cdat3)
# YgObs2 = YgVal(cdats)
# 
# cdat <- c(cdat1, cdat2, cdat3) ; n1 <- length(cdat1) ; n2 <- length(cdat2) ; n3 <- length(cdat3) ; ndat <- c(n1, n2, n3) ; g <- 3
# PgObs <- PgVal(cdat, ndat, g)
# PgObs
# pchisq(PgObs, g-1, lower.tail=F)
# 
# 
# PgObs2 = PgVal2(cdats)
# PgObs2
# pchisq(PgObs2, length(cdats)-1, lower.tail=F)