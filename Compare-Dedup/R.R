.packages(TRUE)

# FUNCTIONEN: Durchsucht Beschreibung und Tablelenname, SPaltennamen nach wert
# wert kann mit so gesucht werden:
# wert
# w*t
# w?rt

# optionen: Regex, CaseSensitive

# Packet enhtält nur eine FUnktion
# packet enthält vignette

#such in:
# beschreibung des packets
# data(package = 'survival') -> text der da kommtn
# jede tabelle, die bei data(package= 'survival') kommt in den spaltennamen

#https://www.regular-expressions.info/rlanguage.html

data <- data(package = 'survival')
View(data$results)

desc <- packageDescription("survival")
View(desc) 

help <- help(package = "survival", help_type="text" )
View(help$info)

## Helper function that searches through the haystack and returns TRUE if something was found
find <- function (needle, haystack){
  return(length(grep(needle, haystack)) > 0)
}

findInPackageName <- function(needle, packagenames){
  return(apply(packagenames, 1, function(packagename) find(needle, packagename)))
}

findInPackageDescription <- function(needle, packagenames){
  return(apply(packagenames, 1, function(packagename) find(needle, packageDescription(packagename))))
}

findInPackageHelp <- function(needle, packagenames){
  return(apply(packagenames, 1, function(packagename) find(needle, help(package = eval(packagename), help_type="text"))))
}

findData <- function(
  needle,
  asRegex=FALSE,
  caseSensitive=FALSE,
  lookInPackagename=TRUE,
  lookInDescription=TRUE,
  lookInHelp=TRUE,
  lookInVignette=TRUE,
  lookInData=TRUE,
  lookInDataHeaders=TRUE,
  lookInDataValues=TRUE
){
  # Save all packages
  PACKAGES <- transform(.packages(TRUE))
  # create the return matrix
  collectedData = data.frame(PACKAGES)
  # give the column a name
  colnames(collectedData) <- "PackageName"
  
  
  if (lookInPackagename){
    collectedData <- cbind(collectedData, "Name" = findInPackageName(needle, PACKAGES))
  }
  
  if (lookInDescription){
    collectedData <- cbind(collectedData, "Description" = findInPackageDescription(needle, PACKAGES))
  }  
  
  if (lookInHelp){
    collectedData <- cbind(collectedData, "Help" = findInPackageHelp(needle, PACKAGES))
  }
  
  # Remove Packages that have false everywhere
  toKeep <- apply(collectedData[,2:length(collectedData)], 1,  sum) > 0
  collectedData <- collectedData[toKeep,]
  
  # sort
  
  # reset row numbering
  rownames(collectedData) <- seq(length=nrow(collectedData))
  
  # Return everything
  collectedData
}

a <- findData("ada")

