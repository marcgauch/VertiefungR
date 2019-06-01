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
find <- function (needle, haystack, ignorecase, ...){
  return(length(grep(pattern = needle, x = haystack, ignore.case = ignorecase, ...)) > 0)
}

findInPackageName <- function(needle, packages, ignore.case, ...){
  return(apply(packages, 1, function(packagename) find(needle, packagename, ignore.case, ...)))
}

findInPackageDescription <- function(needle, packages, ignore.case, ...){
  return(apply(packages, 1, function(packagename) find(needle, packageDescription(packagename), ignore.case, ...)))
}

findInPackageHelp <- function(needle, packages, ignore.case, ...){
  return(apply(packages, 1, function(packagename) find(needle, help(package = eval(packagename), help_type="text"), ignore.case,...)))
}

findInData <- function(needle, packages, ignore.case, ...){
    # create an empty list for the results
    returnList <- c()
    
    # foreach packagename
    for (package in packages[[1]]){
      
      # Ignore Package 'base' and 'stats' they have no data and throw a warning
      if (package == 'base' || package == 'stats') {
        returnList <- c(returnList, FALSE)
      } else {
        # get data information for certain package
        packageData <- data(package = eval(package))
        # only the results are important
        packageData <- packageData$results

        
        if (length(packageData) == 0) {
          # NO DATA HERE
          returnList <- c(returnList, FALSE)
        } else if (length(packageData) == 4){
          # exactly one package
          
          # find for the needle in the columns item and title
          res <- lapply(packageData[,3:4], function(x) find(needle, x, ignore.case, ...))
          # if one of them has the word in it return true
          returnList <- c(returnList, (res$Item || res$Title))
        } else {
          # more than one
          returnList <- c(returnList, sum(apply(packageData[,3:4], 2, function(x) find(needle, x, ignore.case, ...))) > 0)
        }
      }
    }
    
    return(returnList)
    
}


findInPackages <- function(
  needle,
  ...,
  ignore.case=TRUE,
  lookInPackagename=TRUE,
  lookInDescription=TRUE,
  lookInHelp=TRUE,
  lookInData=TRUE,
  resultFilteredByMatches=FALSE
){
  # Save all packages
  PACKAGES <- transform(.packages(TRUE))
  # create the return matrix
  collectedData = data.frame(PACKAGES)
  # give the column a name
  colnames(collectedData) <- "Package"
  
  
  if (lookInPackagename){
    collectedData <- cbind(collectedData, "Packagename" = findInPackageName(needle, PACKAGES, ignore.case, ...))
  }
  
  if (lookInDescription){
    collectedData <- cbind(collectedData, "Description" = findInPackageDescription(needle, PACKAGES, ignore.case, ...))
  }  
  
  if (lookInHelp){
    collectedData <- cbind(collectedData, "Help" = findInPackageHelp(needle, PACKAGES, ignore.case, ...))
  }
  
  if (lookInData){
    collectedData <- cbind(collectedData, "Data" = findInData(needle, PACKAGES, ignore.case, ...))
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

a <- findInPackages("Smok")
a
