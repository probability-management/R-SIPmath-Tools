#This library is desgined to provide the user with the basic tools to import,
#export, and create random numbers for simulation in a method consistent with
#SIPmath standard. The code below provides an example of how the functions included
#in thie library can work together:

data <- mtcars
data$ID <- rownames(mtcars)[1:4]
meta <- SIPMetaDF(data, "ID", rownames(mtcars)[1:4], colnames(mtcars)[1:4])

testdf <-data.frame(matrix(ncol=4,nrow=100))
colnames(testdf) <- colnames(meta[1:4])
for (i in 1:4) {
testdf[i] <- HDRUniformGen(100)
}

ExportSIPLib(testdf, testxml.xml, average = TRUE, median = TRUE, meta = meta)

#If you find any errors, please contact aaron@probabilitymanagement.org