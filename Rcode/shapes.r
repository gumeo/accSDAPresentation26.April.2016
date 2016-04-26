#########################################################################
# Use the accSDA package to differentiate between silhouettes
#########################################################################

library(accSDA)
library(R.matlab) # Need this to load the data

# This is a matlab file, load it to get a list with its contents
SilDat <- readMat('./Silhouettes.mat')

# Extract the coordinates of the silhouettes
coords <- SilDat$Xa

# Extract the labels, i.e. male or female
y <- rep(0,39)
y[SilDat$Male] <-1
y <- as.factor(y)

# Let's plot a single silhouette
# coords is an 39 by 130 matrix, each row is a sample
# and the first 65 variables are x coordinates and the latter are y coordinates.
plot(coords[1,1:65], coords[1,66:130], asp=1, type='l', 
     xlab = "", ylab = "", main = "One silhouette observation")
points(coords[1,1:65],coords[1,66:130])

# Visualize mean shape for male and female
maleMean <- colMeans(coords[SilDat$Male,])
femaleMean <- colMeans(coords[SilDat$Fem,])
plot(maleMean[1:65], maleMean[66:130], asp=1, type='l', 
     xlab = "", ylab = "", main = "Male (black) vs Female (red)")
lines(femaleMean[1:65], femaleMean[66:130],col='red')

# Plot all the data
matplot(t(coords[,1:65]),t(coords[,66:130]),type = 'l',asp=1,
        xlab = "", ylab="", main = "All the aligned data")

# Default run
resDef <- ASDA(coords,y,lam=0.002)

# Cross-validation parameters
lam <- seq(0.0001,0.002,len=30)
method <- "SDAAP"
control <- list(CV = TRUE,
                folds = 10,
                feat = 0.5,
                quiet = FALSE)

# Run the algorithm
res <- ASDA(Xt = coords,
            Yt = y,
            lam = lam,
            method = method,
            control = control)

# Interpretation plot
colFac <- rep(0,130)
colFac[res$beta!=0] <- 1
colFac <- colFac[1:65]+colFac[66:130]
colFac <- as.factor(colFac)
meanShape <- colMeans(coords)
plot(meanShape[1:65], meanShape[66:130], asp=1, type='l', 
     xlab = "", ylab = "", main = "Non-zero parameters")
points(meanShape[1:65], meanShape[66:130], col=colFac)