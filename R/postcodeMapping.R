


fixGeoData <- function(data) {
	data <- data[,2:4]
	dataNames <- data.frame(lapply(data[4,], as.character), stringsAsFactors=FALSE)
	data <- tail(data, -4)
	names(data) <- dataNames
	return (data)	
}

loadPostcodeMappings <- function() {

	# To use, convert ABS Excel files to CSV first
	# sa4s = read.csv("data/1270055006_CG_POSTCODE_2011_SA4_2011.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))
	# sa3s = read.csv("data/1270055006_CG_POSTCODE_2011_SA3_2011.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))
	# sa2s = read.csv("data/1270055006_CG_POSTCODE_2011_SA2_2011.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))
	# sas = read.csv("data/1270055006_CG_POSTCODE_2011_SLA_2011.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))
	# lgas = read.csv("data/1270055006_CG_POSTCODE_2011_LGA_2011.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))

  	# Update on Windows to use Strawberry perl - http://strawberryperl.com/
  	# e.g. something like: 'D:/strawberry/perl/bin/perl.exe'
  	# 
  	perlExe = "perl"
	sa4s = read.xls("data/1270055006_CG_POSTCODE_2011_SA4_2011.xls", 4, verbose = FALSE, blank.lines.skip=TRUE, na.strings = c("", "NA","#DIV/0!"))
	sa3s = read.xls("data/1270055006_CG_POSTCODE_2011_SA3_2011.xls", 4, verbose = FALSE, blank.lines.skip=TRUE, na.strings = c("NA","#DIV/0!"))
	sa2s = read.xls("data/1270055006_CG_POSTCODE_2011_SA2_2011.xls", 4, verbose = FALSE, blank.lines.skip=TRUE, na.strings = c("NA","#DIV/0!"))
	sas = read.xls("data/1270055006_CG_POSTCODE_2011_SLA_2011.xls", 4, verbose = FALSE, blank.lines.skip=TRUE, na.strings = c("NA","#DIV/0!"))
	lgas = read.xls("data/1270055006_CG_POSTCODE_2011_LGA_2011.xls", 4, verbose = FALSE, blank.lines.skip=TRUE, na.strings = c("NA","#DIV/0!"))

	sa4s = fixGeoData(sa4s)
	sa3s = fixGeoData(sa3s)
	sa2s = fixGeoData(sa2s)
	sas = fixGeoData(sas)
	lgas = fixGeoData(lgas)
}


obtainGeoArea <- function(postcode, level) {
	value <- ""
	if (level == "SA4") {
	  	value <- as.character(sa4s[sa4s$POSTCODE == postcode,]$SA4_NAME_2011)
	}
	else if (level == "SA3") {
	  	value <- as.character(sa3s[sa3s$POSTCODE == postcode,]$SA3_NAME_2011)
	}
	else if (level == "SA2") {
	  	value <- as.character(sa2s[sa2s$POSTCODE == postcode,]$SA2_NAME_2011)
	}
	else if (level == "SLA") {
	  	value <- as.character(sas[sas$POSTCODE == postcode,]$SLA_NAME_2011)
	}
	else if (level == "LGA") {
	  	value <- as.character(lgas[lgas$POSTCODE == postcode,]$LGA_NAME_2011)
	}
	return (value)
}