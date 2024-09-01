## Set the working directory to where the Excel file is
## Session > Set Working Directory > Choose Directory > Select the directory where the Excel file is

getwd()
dir()

## Principal Component Analysis
## Reference: http://www.statmethods.net/advstats/factor.html

input <- read.csv("Universities.csv",header=TRUE)
dim(input)

mydata <- input[1:25,3:8]
head(mydata)

pcaObj <- princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)

summary(pcaObj,loadings = TRUE)
plot(pcaObj)

loadings(pcaObj)
pcaObj$loadings  ## same as above

pc_scores <- pcaObj$scores
pc_scores

scale(mydata)
scale(mydata) %*% pcaObj$loadings ## this is same as pcaObj$scores

biplot(pcaObj)
biplot(pcaObj$scores[,1:2],pcaObj$loading[,1:2]) ## if you didn't want rescaling of the axis

## data compression
scaled_data <- scale(mydata)
rmse <- matrix(nrow=6,ncol=1)
for (i in 1:6) {
scaled_data_recovered <- pc_scores[,1:i] %*% t(pcaObj$loadings[,1:i]) ## if compressed using two components
scaled_data_recovered
rmse[i] <- sum((scaled_data_recovered - scaled_data)^2) ## errors
}
rmse

## Visualizing the data in two dimensions
pcaObj$scores
?plot
plot(pcaObj$scores[,1], pcaObj$scores[,2], col="blue", xlab="Component 1", ylab="Component 2")
?text
text(pcaObj$scores[,1:2], labels=input[,1],cex= 0.7, pos=3)

