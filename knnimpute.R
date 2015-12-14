#This script illustrates how to use the k-nn classification method to impute missing values in a data file. 
#In pattern recognition, the k-Nearest Neighbors algorithm (or k-NN for short) is a non-parametric method used for classification and regression.
#An object is classified by a majority vote of its neighbors, with the object being assigned to the class most common among its k nearest neighbors (k is a positive integer, typically small).

#Main function which expects a dataframe with 2 columns with NA values and argument K for number of nearest neighbours
#This function calls the helper function for each column that has to be imputed
kNNImpute <- function(inputdataframe,k){
  
  # convert to matrix for ease
  inputmatrix <- data.matrix(inputdataframe)
  toimpute <- inputmatrix
  imputedxvalues = imputeknnforcolumn(1,2,inputmatrix,toimpute[,1],k)
  colnames(imputedxvalues) = c(colnames(inputmatrix)[1], paste(colnames(inputmatrix)[1],"Imputed",sep = ""))
  imputedyvalues = imputeknnforcolumn(2,1,inputmatrix,toimpute[,2],k)
  colnames(imputedyvalues) = c(colnames(inputmatrix)[2], paste(colnames(inputmatrix)[2],"Imputed", sep = ""))
  imputed <- cbind(imputedxvalues,imputedyvalues)
  return(imputed)
  
}
#Helper function which takes in column number of matrix to be imputed, column number of matrix whose values should be used to impute,
#the input matrix and the column with missing values. This function does:
#1. Get complete cases without NA elements
#2. Find missing value rows for column to be imputed
#3. Find corresponding values for those rows in the other column
#4. Calculate K nearest neigbour elements for values in other column
#5. Calculate means of K nearest neighbours
#6. Insert these values into the column with missing values
imputeknnforcolumn <- function(columnnumbertoimpute, columnnumberwithcorrespvalues,inputmatrix, imputedcolumn,k ){
  #filter out NA elements
  completedata <- inputmatrix[complete.cases(inputmatrix),]
  #find rows of data that have missing values 
  missingrows <- inputmatrix[!complete.cases(inputmatrix),]
  #find missing values for column to impute
  missingcolumnvalues <- is.na(inputmatrix[,columnnumbertoimpute])
  #find corresponding values in other column
  correspvalues <- missingrows[,columnnumberwithcorrespvalues][!is.na(missingrows[,columnnumberwithcorrespvalues])]
  completecorrespvalues <- completedata[,columnnumberwithcorrespvalues]
  #calculate nearest neighbour elements for each corresponding complete value
  distances <- lapply(completecorrespvalues, function(x) (abs(x - correspvalues)))
  distancematrix <- do.call(rbind, distances)
  #find k nearest neighbours
  nearestneighbours <- apply(distancematrix, 2, function(x) (order(x)[1:k]))
  nearestneighbourvalues <- apply(nearestneighbours, 2, function(x) (completedata[x,columnnumbertoimpute]))
  #calculate average of K nearest values
  avgnearestneighbourvalues <- colMeans(nearestneighbourvalues)
  imputedcolumn[missingcolumnvalues] <- avgnearestneighbourvalues
  return(cbind(imputedcolumn,missingcolumnvalues))
}

#Example of how to use this function
heights <- read.table("heightmissing.txt",header = TRUE)
imputed = kNNImpute(heights,6)
imputed
plot(imputed[,"Male"],imputed[,"Female"])
plot(heights$Male,heights$Female)
