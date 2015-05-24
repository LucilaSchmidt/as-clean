createFileCopies <- function(){
        createCopyOfVarFiles("X")
        createCopyOfVarFiles("y")
        createCopyOfVarFiles("subject")
}

createCopyOfVarFiles <- function(var){
        originalPath <- getPathTo(var, "train")
        newPath <- getCopyPathTo(var,"train")
        file.copy(originalPath, newPath,overwrite=TRUE)
        originalPath <- getPathTo(var, "test")
        newPath <- getCopyPathTo(var,"test")
        file.copy(originalPath, newPath)
        
}

readAndMergeData <- function(){        
        features <<- getFeatures()             
        X <<- getMergedVar("X", features)
        y <<- getMergedVar("y", c("activity"))
        subject <<- getMergedVar("subject", c("subject"))
        activities <<- getActivityLabels()

}

getFeatures <- function(){
        path <- paste(getPathToData(),"features.txt", sep="")
        c("emptyColumn", as.vector(read.delim(path, sep=" ", header=FALSE)[,2]))
        
}

getActivityLabels <- function(){
        path <- paste(getPathToData(),"activity_labels.txt", sep="")
        read.delim(path, sep=" ", header=FALSE, colClasses=c("character", "character"))
        
}

getMergedVar <- function(var, featureNames){
        trainVar <<- read.table(getCopyPathTo(var, "train"), sep=" ", header=FALSE)
        testVar <<- read.table(getCopyPathTo(var, "test"), sep=" ", header=FALSE)
        mergedVar <- rbind(trainVar, testVar)
        colnames(mergedVar) <- featureNames
        mergedVar
}

getPathTo <- function(var, type){
        directory <- paste(getPathToData(),type,"/", sep="")
        fileName <- paste(var,"_", type,".txt",sep="")
        
        paste(directory,fileName,sep="")
}

getCopyPathTo <- function(var, type){
        directory <- paste(getPathToData(),type,"/", sep="")
        fileName <- paste(var,"_", type,"2.txt",sep="")
        
        paste(directory,fileName,sep="")
}

getPathToData <- function(){
        "./"     
}

prepareFiles <- function(){
        files <- c(getCopyPathTo("X", "train"), getCopyPathTo("X","test"))
        for (f in files){
                x <- readLines(f)
                y <- sub( "  ", "", x)
                y <- gsub( "  ", " ", x)
                cat(y, file=f, sep="\n")
        }
}

extractRequiredColumns <- function(){
        requiredColumns <- vector()
        requiredFeatures <- vector()
        i <- 1
        for(j in 1:length(features)){
                feature <- features[j]
                if(grepl("mean()", feature, fixed=TRUE) || grepl("std()", feature, fixed=TRUE)){
                        requiredColumns[i] <- j
                        requiredFeatures[i] <- feature
                        i <- i + 1
                }   
        }
        requiredFeatures <<- requiredFeatures
        X <<- X[,requiredColumns]
}

renameActivities <- function(){
        y[,1] <<- factor(y[,1], levels=activities[,1], labels=activities[,2])
        
}

renameVariables <- function(){
        colnames(X) <<- requiredFeatures
        
}

createActivitySubjectEmptyDataFrame <- function(activity, subject){
 n <- length(levels(as.factor(subject)))
 m <- length(levels(activity))
 emptyMatrix <- numeric(n)
 for(i in 2:m){
         emptyMatrix <- cbind(emptyMatrix, numeric(n))
 }
 
 df <- as.data.frame(emptyMatrix)
 colnames(df) <- levels(activity)
 df
 
}

varNamesToMean <- function(dataSet){
        
        vars <- colnames(dataSet)
        vars[! vars[] %in% c("activity", "subject")]
        
}

createAnotherDataSet <- function(){
        dataSet <- data.frame(X, subject, y)
        dataSet$activity <- y
        s <- split(dataSet, dataSet[, activity])
        varNamesToMean <- getVarNamesToMean(dataSet)
        df <- createActivitySubjectEmptyDataFrame(dataSet$activity, dataSet$subject)
        for(i in 1:length(s)){
                activityName <- names(s)[i]
                activityData <- s[i]
                df[,activityName] <- sapply(activityData[,varNamesToMean], colMeans)
        }
        
        anotherDataSet <<- df
}

writeNewTable <- function(){
    write.table(anotherDataSet, "means.txt", row.name=FALSE) 

}


##############################################################


createFileCopies()
prepareFiles()
readAndMergeData()
extractRequiredColumns()
renameActivities()
renameVariables()
createAnotherDataSet()
writeNewTable()