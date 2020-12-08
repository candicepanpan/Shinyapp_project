install.packages("readxl")
library(readxl)
getwd()
BAprogramfile <- read_xlsx("BA_program.xlsx", sheet=1, col_names=T)

d <- BAprogramfile
str(d)

hist(d$StudentAge, main="age", xlab="age", col="gold")
hist(d$StudentGPA, main="GPA", xlab="gpa", col="gold")


# % of rowing having missing values
dim(d[!complete.cases(d),])[[1]]/nrow(d)*100

# Make target variable first column in dataset
d <- d[,c(17,1:20)]
# Make target column name "y"
names(d)[1] <- "y"

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,3:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- d[,3:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)  


