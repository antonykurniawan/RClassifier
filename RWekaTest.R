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
    
    # Check if ggplot2 is installed
    if (!is.installed("ggplot2")) {
        print ("RWeka is not installed. Installing Now")
        install.packages("ggplot2")
    }
    
    # Load library
    library (RWeka)
    library (ggplot2)
}

NaiveBayes <- function (myFormula, myData) {
    NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
    NB(myFormula, myData)
}

classifier <- function (fileName, key, model=".", label="") {
    # Read input file
    myData <- read.csv(fileName)
    
    # Create formula for iris
    #myFormula <- species ~ sepal_l + sepal_w + petal_l + petal_w
    myFormula <- as.formula(paste(key, model, sep="~"))
    
    # List of method to be tested
    #methodList = list(SMO=SMO, JRip=JRip, NaiveBayes=NaiveBayes, Logistic=Logistic)
    methodList = list(SMO=SMO, JRip=JRip, NaiveBayes=NaiveBayes)
                      
    # run classifier model
    resultList <- list()
    for (i in 1:length(methodList)) {
        resultList[[i]] <- methodList[[i]] (myFormula, myData) 
        print(summary(resultList[[i]]))
    }
    
    # get mean absolut error from classifier
    errorList = vector("numeric", length(resultList))
    for (i in seq_along(resultList)) {
        errorList[i] <- summary(resultList[[i]])$details['meanAbsoluteError']
    }
    
    # list of method names
    methodNames <- names(methodList)

    #par(mfrow=c(2,2))
    barplot(errorList, names.arg=methodNames, 
            xlab=label, ylab="meanAbsoluteError",ylim=c(0,1))
    for (i in seq_along(errorList)) {
        text(i,errorList[i]+0.05, sprintf("%.3f",errorList[i]))
    }
}

runAll <- function () {
    init()
    par(mfrow=c(2,2))
    classifier("iris.csv","species", label = "Iris data")
    classifier("ionosphere.csv","class", label = "Ionosphere data")
    classifier("soybean_large.csv","class", label = "Soybean data")
}

testIris <- function () {
    init()
    par(mfrow=c(3,4))
    classifier("iris.csv","species","sepal_w + petal_l + petal_w", "no sepal_l")
    classifier("iris.csv","species","sepal_l + petal_l + petal_w", "no sepal_w")
    classifier("iris.csv","species","sepal_l + sepal_w + petal_w", "no petal_l")
    classifier("iris.csv","species","sepal_l + sepal_w + petal_l", "no petal_w")
    
    classifier("iris.csv","species","petal_l + petal_w", "petal only")
    classifier("iris.csv","species","sepal_l + sepal_w", "sepal only")
    classifier("iris.csv","species","sepal_l + petal_l", "length only")
    classifier("iris.csv","species","sepal_w + petal_w", "width only")
    
    classifier("iris.csv","species","sepal_l + sepal_w + petal_l + petal_w", "All")
    
}