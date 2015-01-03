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
        print ("ggplot2 is not installed. Installing Now")
        install.packages("ggplot2")
    }
    
    # check if xlxs is installed
    if (!is.installed("xlsx")) {
        print ("xlsx is not installed. Installing Now")
        install.packages("xlsx")
    }
    
    # Load library
    library (RWeka)
    library (ggplot2)
    library (xlsx)
}

NaiveBayes <- function (myFormula, myData) {
    NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
    NB(myFormula, myData)
}

classifier <- function (fileName, key, model=".") {
    # Read input file
    myData <- read.csv(fileName)
    
    # Create formula for iris
    #myFormula <- species ~ sepal_l + sepal_w + petal_l + petal_w
    myFormula <- as.formula(paste(key, model, sep="~"))
    
    # List of method to be tested
    #methodList = list(SMO=SMO, JRip=JRip, NaiveBayes=NaiveBayes, Logistic=Logistic)
    methodList = list(SMO=SMO, JRip=JRip, J48=J48, NaiveBayes=NaiveBayes)
                      
    # run classifier model
    resultList <- list()
    for (i in 1:length(methodList)) {
        resultList[[i]] <- methodList[[i]] (myFormula, myData) 
        #print(summary(resultList[[i]]))
    }
    
    # get mean absolut error from classifier
    errorList = vector("numeric", length(resultList))
    for (i in seq_along(resultList)) {
        errorList[i] <- summary(resultList[[i]])$details['meanAbsoluteError']
    }
    
    names(errorList) = names(methodList)
    errorList
    
}

barByData <- function (errorList, label="") {
    # list of method names
    methodNames <- names(errorList)
    vError = as.vector(errorList)
    
    #par(mfrow=c(2,2))
    barplot(vError, names.arg=methodNames, 
            xlab=label, ylab="meanAbsoluteError",ylim=c(0,0.5))
    for (i in seq_along(vError)) {
        text(i,vError[i]+0.05, sprintf("%.4f",errorList[i]))
    }
}

runAll <- function () {
    init()
    par(mfrow=c(2,2))
    barByData (classifier("iris.csv","species"), "iris")
    barByData (classifier("ionosphere.csv","class"), "ionosphere")
    barByData (classifier("soybean_large.csv","class"), "soybean")
}

testIris <- function () {
    init()
    
    result <- classifier("iris.csv","species","sepal_w + petal_l + petal_w")
    matrixRes <- matrix(result, ncol=length(result))
    row.names(matrixRes) = "no_sepal_l"
    matrixRes <- rbind (matrixRes, no_sepal_W = classifier("iris.csv","species","sepal_l + petal_l + petal_w"))
    matrixRes <- rbind (matrixRes, no_petal_l = classifier("iris.csv","species","sepal_l + sepal_w + petal_w"))
    matrixRes <- rbind (matrixRes, no_petal_w = classifier("iris.csv","species","sepal_l + sepal_w + petal_l"))
    
    matrixRes <- rbind (matrixRes, petal_only = classifier("iris.csv","species","petal_l + petal_w"))
    matrixRes <- rbind (matrixRes, sepal_only = classifier("iris.csv","species","sepal_l + sepal_w"))
    matrixRes <- rbind (matrixRes, length_only = classifier("iris.csv","species","sepal_l + petal_l"))
    matrixRes <- rbind (matrixRes, width_only = classifier("iris.csv","species","sepal_w + petal_w"))
    
    matrixRes <- rbind (matrixRes, all = classifier("iris.csv","species","sepal_l + sepal_w + petal_l + petal_w"))
    
    print(matrixRes)
    barplot (matrixRes, beside=T,col=sample(nearRcolor("red", dist=1),nrow(matrixRes)), legend.text=row.names(matrixRes))

}

combIris <- function() {
   init()
   
   myData <- read.csv("iris.csv")
   
   attr <- names(myData[1:4])
   for (i in 1:length(attr)) {
       combine <- combn(attr, i)
       for (j in 1:ncol(combine)) {
           formula <- paste(combine[,j], collapse='+')
           print(formula)
           
           if (j==1) {
               tempRes <- classifier("iris.csv", "species", formula)
               matrixRes <- matrix(tempRes, ncol=4)
               colnames(matrixRes) <- names(tempRes)
           } else {
               matrixRes <- rbind (matrixRes, classifier("iris.csv","species", formula))
           }
       }
       print(paste (i,matrixRes))
       
       if (i==1) {
           meanSummary <- matrix(apply(matrixRes,2,mean), ncol=4)
           minSummary <- matrix(apply(matrixRes,2,min), ncol=4)
           maxSummary <- matrix(apply(matrixRes,2,max), ncol=4)
       } else {
           meanSummary <- rbind(meanSummary, apply(matrixRes,2,mean))
           minSummary <- rbind(minSummary, apply(matrixRes,2,min))
           maxSummary <- rbind(maxSummary, apply(matrixRes,2,max))
       }
   }
   
 
   par(mfrow=c(3,1))
   barplot (meanSummary, beside=TRUE, col=c('red','green','cyan','blue'), names.arg=names(matrixRes))
   barplot (minSummary, beside=TRUE, col=c('red','green','cyan','blue'), names.arg=names(matrixRes))
   barplot (maxSummary, beside=TRUE, col=c('red','green','cyan','blue'), names.arg=names(matrixRes))
   write.csv (meanSummary,'mean.csv')
   write.csv (minSummary, file='minSummary.csv')
   write.csv (maxSummary, file='maxSummary.csv')
}

combIon <- function() {
    init()
    
    myData <- read.csv("ionosphere.csv")
    
    attr <- names(myData[1:34])
    combinationId <- c(30,31,32,33,34)
    for (i in combinationId) {
        combine <- combn(attr, i)
        for (j in 1:ncol(combine)) {
            formula <- paste(combine[,j], collapse='+')
            print(formula)
            
            if (j==1) {
                tempRes <- classifier("ionosphere.csv", "class", formula)
                matrixRes <- matrix(tempRes, ncol=4 )
                colnames (matrixRes) <- names(tempRes)
            } else {
                matrixRes <- rbind (matrixRes, classifier("ionosphere.csv","class", formula))
            }
        }


        if (i==combinationId[1]) {
            meanSummary <- matrix(apply(matrixRes,2,mean), ncol=4)
            minSummary <- matrix(apply(matrixRes,2,min), ncol=4)
            maxSummary <- matrix(apply(matrixRes,2,max), ncol=4)
            write.xlsx (matrixRes,'ionosphere.xlsx', sheetName=paste('matrixRes',i))
        } else {
            meanSummary <- rbind(meanSummary, apply(matrixRes,2,mean))
            minSummary <- rbind(minSummary, apply(matrixRes,2,min))
            maxSummary <- rbind(maxSummary, apply(matrixRes,2,max))
            write.xlsx (matrixRes,'ionosphere.xlsx', sheetName=paste('matrixRes',i), append=TRUE)
        }
    }

    par(mfrow=c(3,1))
    barplot (meanSummary, beside=TRUE, col=c('red','green'), names.arg=names(matrixRes))
    barplot (minSummary, beside=TRUE, col=c('red','green'), names.arg=names(matrixRes))
    barplot (maxSummary, beside=TRUE, col=c('red','green'), names.arg=names(matrixRes))
    write.xlsx (meanSummary,'ionosphere.xlsx', sheetName='meanSummary', append=TRUE)
    write.xlsx (minSummary,'ionosphere.xlsx', sheetName='minSummary', append=TRUE)
    write.xlsx (maxSummary,'ionosphere.xlsx', sheetName='maxSummary', append=TRUE)
    
}