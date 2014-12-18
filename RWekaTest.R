# Description : Check if a package is installed
# Arguments :   mypkg : the name of a package
# Value :   TRUE  - If the package is installed
#           FALSE - If the package is not installed
is.installed <- function (mypkg) {
    is.element(mypkg,installed.packages()[,1])
}


# Description : Initialize required library for this module
# Arguments :   None
# Value :   None
init <- function() {
    # Check if RWeka is installed
    if (!is.installed("RWeka")) {
        print ("RWeka is not installed. Installing Now")
        install.packages("RWeka")
    }
    
    # Load RWeka
    library (RWeka)
}

classifier <- function (formula, data, methodList) {
    resultList <- list()
    id <- names(methodList)
    
    for (i in 1:length(methodList)) {
        resultList[[i]] <- methodList[[i]] (formula, data) 
    }
    
    error = vector("numeric", length(resultList))
    for (i in seq_along(resultList)) {
        error[i] <- summary(resultList[[i]])$details['meanAbsoluteError']
    }
    error  
}

irisClassifier <- function (fileName) {
    # Read input file
    irisData <- read.csv(fileName)
    
    # Create formula for iris
    irisFormula <- species ~ sepal_l + sepal_w + petal_l + petal_w
    
    # List of method to be tested
    methodList = list(SMO=SMO, JRip=JRip, J48=J48)
    
    # start classify
    irisResult <- classifier (irisFormula, irisData, methodList)
    
    # list of method names
    methodNames <- names(irisResult)
    
    print(irisResult)
    #barplot(irisResult, names.arg=methodNames)
    #par(pch=22, col="red")
    #par(mfrow=c(2,4), xrange=1:3)
    #plot(seq_along(irisResult), irisResult, type = "o")
    #lines(irisResult,names.arg=methodNames)
    
    ggplot(data.frame(seq_along(irisResult),irisResult),  aes(x=time, y=total_bill, group=1)) + geom_line() + geom_point()
}