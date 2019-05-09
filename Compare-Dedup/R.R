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

findInPackageName <- function(needle, packagename){
  return(length(grep(needle, packagename)) > 0)
}

findInPackageDescription <- function(needle, packagename){
  description <- packageDescription(packagename)
  return(length(grep(needle, description)) > 0)
}

findInPackageHelp <- function(needle, packagename){
  help <- help(package = print(packagename), help_type="text" )
  return(length(grep(needle, help)) > 0)
}



findData <- function(
  needle,
  asRegex=FALSE,
  caseSensitive=FALSE,
  lookInPackagename=TRUE,
  lookInDescription=TRUE,
  lookInHelp=FALSE,
){
  # Save all packages
  PACKAGES <- .packages(TRUE)
  # create the return matrix
  collectedData = data.frame(PACKAGES)
  # give the column a name
  colnames(collectedData) <- "PackageName"
  
  
  # Check packagename if requested
  if (lookInPackagename){
    # add new column to resulttable
    collectedData <- cbind(collectedData, FALSE)
    # name the newly generatred column
    colnames(collectedData)[length(collectedData)] <- "Name"
    # check for every element
    for (i in 1:nrow(collectedData)){
      # if its found
      if(findInPackageName(needle, collectedData[i, 1])) {
        # write TRUE in the column "Name"
        collectedData[i, length(collectedData)] <- TRUE
      }
    }
  }
  
  # Check packagename if requested
  if (lookInHelp){
    # add new column to resulttable
    collectedData <- cbind(collectedData, FALSE)
    # name the newly generatred column
    colnames(collectedData)[length(collectedData)] <- "Description"
    # check for every element
    for (i in 1:nrow(collectedData)){
      # if its found
      if(findInPackageDescription(needle, collectedData[i, 1])) {
        # write TRUE in the column
        collectedData[i, length(collectedData)] <- TRUE
      }
    }
  }  
  
  # Check packagename if requested
  if (lookInDescription){
    # add new column to resulttable
    collectedData <- cbind(collectedData, FALSE)
    # name the newly generatred column
    colnames(collectedData)[length(collectedData)] <- "Help"
    # check for every element
    for (i in 1:nrow(collectedData)){
      # if its found
      if(findInPackageHelp(needle, collectedData[i, 1])) {
        # write TRUE in the column
        collectedData[i, length(collectedData)] <- TRUE
      }
    }
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

a <- findData("marc")
a

