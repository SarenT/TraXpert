source("Black_Theme.R")

# For left skewed data
powerTransform = function(x, a = 1){return(x ^ a)}
powerTransformFormula = function(x, a = 1){formula = paste0("$\\left(", x, "\\right)^{", a, "}$"); return(formula)}
# For right skewed data
rootTransform = function(x, a = 1){return(x ^ (1/a))}
rootTransformFormula = 
	function(x, a = 1){formula = paste0("$\\left(", x, "\\right)^{", 1, "/", a, "}$"); return(formula)}
logTransform = function(x, a = 1){return(log(x, a))}
logTransformFormula = function(x, a = 1){formula = paste0("$\\log_", a, "\\left(", x, "\\right)$"); return(formula)}
invTransform = function(x, a = 1){x = 1/x; x[is.infinite(x)] = 0; return(x)}
invTransformFormula = 
	function(x, a = 1){formula = paste0("$\\left(", 1, "/", x, "\\right)^{", a, "}$"); return(formula)}
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
	#browser()
	oldFeatures = bind_rows(
		tibble(oldFeatures) %>% filter(!(feature %in% newFeatDF$feature & group_id %in% newFeatDF$group_id)), 
		newFeatDF)
	
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
					   closeProgress = NULL, recalculate = FALSE, browse = 0, benchmark = TRUE){
	if(browse == 1){ browse = browse - 1; browser() } else {browse = browse - 1}
	if(benchmark) startTime = benchMark()
	# Getting data frames
	tracks = dataList$tracks; trajectories = dataList$trajectories; files = dataList$files; features = dataList$features
	tracks$group_id = apply(tracks[, as.character(groupings$names), drop = F], 1, paste, collapse = "_")
	#tracks$group_part_id = apply(tracks[, c(as.character(groupings$names), "Part")], 1, paste, collapse = "_")
	tracks$track_global_id = paste0(tracks$group_id, "_", tracks$TRACK_ID)
	#tracks$track_global_id = paste0(tracks$group_part_id, "_", tracks$TRACK_ID)
	
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
	trajectories$track_global_id = paste0(trajectories$group_id, "_", trajectories$TRACK_ID)
	#trajectories$track_global_id = paste0(trajectories$group_part_id, "_", trajectories$TRACK_ID)
	
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
		
		if(benchmark) startTime = benchMark(
			"Process data - traj fixed position, edge displacement and edge directions", startTime)
		
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
		
		tracks$TRACK_DIRECTION_ROT = tracks$TRACK_DIRECTION; tracks$TRACK_DIRECTION_Z_ROT = tracks$TRACK_DIRECTION_Z
		tracks$DIRECTION_CARDINAL_ROT = tracks$DIRECTION_CARDINAL
		#; tracks$DIRECTION_CARDINAL_Z_ROT = tracks$DIRECTION_CARDINAL_Z
		
		
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
	
	trajectories$POSITION_X_FIX_ROT = r * cos(phi) * sin(theta)
	trajectories$POSITION_Y_FIX_ROT = r * sin(phi) * sin(theta)
	trajectories$POSITION_Z_FIX_ROT = r * cos(theta)
	
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
setThemeBase = function(plot, is.dark, subtitle.hjust, subtitle.size, subtitle.face, facet.label.fill.color, 
						facet.text.face){
	plot = plot + theme_classic() + 
		theme(plot.title = element_text(hjust = 0.5), 
			  plot.subtitle = element_text(hjust = subtitle.hjust, size = subtitle.size, face = subtitle.face), 
			  strip.background = element_rect(color = NA, fill=facet.label.fill.color), 
			  legend.justification = c(0, 1),
			  strip.text = element_text(face = facet.text.face))
	
	if(is.dark) plot = plot + theme_black()
	return(plot)
}


tabBGColorCSS = function(title, tabColor = "#cccccc"){
	return(paste0(".navbar-default .navbar-nav > li[class=active] > a[data-value='", title, "'] {background-color: ", tabColor, ";}
	.navbar-default .navbar-nav > li[class=active] > a[data-value='", title, "'] a:hover {background-color: ", tabColor, ";}
	.navbar-default .navbar-nav > li[class=active] > a[data-value='", title, "'] a:focus {background-color: ", tabColor, ";}
		   .navbar-default .navbar-nav > li > a[data-value='", title, "'] {background-color: ", tabColor, ";}"))
}

groupHistograms = function(data, x, y, default.y.Unit, y.unit, y.labDisp, groupings, colorGroup = NULL, 
						   fillGroup = NULL, colorGroupName = NULL, fillGroupName = NULL, 
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
		plot(x, cex=1.1, bin=bin, stack=stack, sep=sep,shrink=shrink, col="black", main = title, axes = FALSE, 
			 browse = browse)
		ticks = seq(0, 11*pi/6, pi/6)
		labels = paste0(seq(0, 11*pi/6, pi/6) * 180/pi, "°")
		axis.circular(at=circular(ticks), labels=labels, col="black", zero=zero, rotation='clock', cex=1.1)
		ticks.circular(circular(ticks), col="black", zero=zero, rotation='clock', tcl=0.075)
		rose.diag(x, bins = bins, col=col, cex=1.1, prop=prop, border="black", add=TRUE, axes = FALSE)
		for(i in 1:length(bw)){
			lines(density.circular(x, bw=bw[i]), 
				  lwd=lwd[((i-1) %% length(lwd)) + 1], 
				  lty=lty[((i-1) %% length(lwd)) + 1])
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
	
	plot.default(tqf, circdat, pch=16, xlim=c(0,2*pi), ylim=c(0,2*pi), xlab = "von Mises quantile function", 
				 ylab = "Empirical quantile function", main = title) 
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
groupedCircDataModel = function(data, xContinuous, x, y, groupings, allGroupswRep, allGroupswoRep){
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
groupQQ = function(data, x, y, default.y.Unit, y.unit, groupings, colorGroup = NULL, fillGroup = NULL, 
				   colorGroupName = NULL, fillGroupName = NULL, 
				   facet.row = NULL, facet.col = NULL, facet.wrap = FALSE, 
				   colorAlpha = 1.0, fillAlpha = 1.0,
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
	normalityTest = data %>% group_by_at(groups) %>% 
		summarise(W = shapiro.test(!!yVar)$statistic, p.value = shapiro.test(!!yVar)$p.value, 
				  Skewness = skewness(!!yVar, na.rm = TRUE, type = 1), 
				  Kurtosis = kurtosis(!!yVar, na.rm = TRUE, type = 1))
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
	
	trks = trks %>% summarise(NUMBER_SPOTS = n(),
						TRACK_DISPLACEMENT = sqrt((last(POSITION_X) - first(POSITION_X)) ** 2 + 
												  	(last(POSITION_Y) - first(POSITION_Y))  ** 2), 
						TRACK_DURATION = last(FRAME) - first(FRAME), 
						TRACK_START = first(FRAME), TRACK_STOP = last(FRAME),
						TRACK_X_LOCATION = mean(POSITION_X), TRACK_Y_LOCATION = mean(POSITION_Y),
						TRACK_INDEX = first(TRACK_ID), 
						TRACK_MEAN_SPEED = mean(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + 
													  	(lead(POSITION_Y) - POSITION_Y) ** 2)) / 
													(lead(POSITION_T) - POSITION_T) , na.rm = TRUE),
						TRACK_MAX_SPEED = max(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + 
														(lead(POSITION_Y) - POSITION_Y) ** 2)) / 
											  	(lead(POSITION_T) - POSITION_T), na.rm = TRUE),
						TRACK_MIN_SPEED = min(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + 
														(lead(POSITION_Y) - POSITION_Y) ** 2)) / 
											  	(lead(POSITION_T) - POSITION_T), na.rm = TRUE),
						TRACK_MEDIAN_SPEED = median(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + 
														  	(lead(POSITION_Y) - POSITION_Y) ** 2)) / 
														(lead(POSITION_T) - POSITION_T), na.rm = TRUE),
						TRACK_STD_SPEED = sd(sqrt(((lead(POSITION_X) - POSITION_X) ** 2 + 
												   	(lead(POSITION_Y) - POSITION_Y) ** 2)) / 
											 	(lead(POSITION_T) - POSITION_T), na.rm = TRUE))
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
		select(c(imarisCoreNames, imarisTimeNames, timeunits = "Unit")) %>% 
		arrange(TRACK_ID, FRAME) %>% 
		filter(!is.na(TRACK_ID))
	
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

#' Checks for XML file errors such as missing features (attributes) in TrackMate files. If there are, these are 
#' complemented with missing values
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
	edges = cbind(data.frame(TRACK_ID = as.numeric(as.character(xml_find_first( edge.nodes, ".//ancestor::Track") %>% 
																	xml_attr("TRACK_ID")))), edges)
	
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

icon_list = function(x){
	lapply(
		x,
		function(x) {
			tags$div(
				icon("arrows-alt-h"),
				tags$strong(x)
			)
		}
	)
}

#' Utility function to upgrade session data model
#'
#' @param dataSession 
#' @param version 
#'
#' @return
#' @export
#'
#' @examples
dataSessionVersionUpgrade = function(dataSession, version){
	if(version == 0){
		dataSession$version = 1
		version = 1
	}
	#Add new versions as separate if statements for stepwise adaptation for the newest version
	
	if(version == dataModelVersion){
		return(dataSession)
	}else{
		#TODO error reporting
		return(NULL)
	}
	
}


#' Generates a bucket list for the groupings to select from for pairwise comparisons
#'
#' @param ns namespace from Shiny session
#' @param choices Choices to pick from
#'
#' @return
#' @export
#'
#' @examples
generateBucketList = function(ns, choices){
	ui = tagList(fluidPage(fluidRow(
		column(6,
			   tags$div(class = "panel panel-default",	
			   		 tags$div(class = "panel-heading", icon("arrow-right"),	"Select groups"),
			   		 tags$div( class = "panel-body", id = ns("stat_comparison_select_In"), 
			   		 		  icon_list(choices))
			   ),
			   tags$div(class = "panel panel-default", 
			   		 tags$div(class = "panel-heading", icon("exchange"), "1."),
			   		 tags$div(class = "panel-body", id = ns("stat_comparison_select1_In"))
			   )),
		column(6,
			   tags$div(class = "panel panel-default",
			   		 tags$div(class = "panel-heading", icon("trash"), "Remove item"),
			   		 tags$div(class = "panel-body", id = ns("stat_comparison_selectBin_In"))
			   ),
			   tags$div(class = "panel panel-default",
			   		 tags$div(class = "panel-heading", icon("exchange"), "2."),
			   		 tags$div(class = "panel-body", id = ns("stat_comparison_select2_In"))
			   )
		)
	)),
	sortable_js(ns("stat_comparison_select_In"), options = sortable_options(group = list(
		pull = "clone",	name = ns("stat_comparison_select_Group"), put = FALSE), 
		onSort = sortable_js_capture_input("sort_vars"))),
	sortable_js(ns("stat_comparison_select1_In"), options = sortable_options(group = list(
		pull = TRUE, name = ns("stat_comparison_select_Group"), put = TRUE), 
		onSort = sortable_js_capture_input(ns("stat_comparison_select1_In")))),
	sortable_js(ns("stat_comparison_selectBin_In"), options = sortable_options(group = list(
		pull = TRUE, name = ns("stat_comparison_select_Group"), put = TRUE), 
		onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }"))),
	sortable_js(ns("stat_comparison_select2_In"), options = sortable_options(group = list(
		pull = "clone",	name = ns("stat_comparison_select_Group"), put = TRUE), 
		onSort = sortable_js_capture_input(ns("stat_comparison_select2_In"))))
	)
	return(ui)
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

tool_tip = function(id, message, placement = "bottom", trigger = "hover", options = NULL){
	bsTooltip(id, message, placement = placement, trigger = trigger, options = options)
}