x<-c(1:20)
y<-x+runif(20,10,20)#random uniform distribution
plot(x,y)

# list
x<-list(1:3,"a",c(TRUE,FALSE,TRUE),c(2.3,5.9))
str(x)
x<-list(list(list()))
str(x)
is.recursive(x)
c<-1:6
dim(c)<-c(3,2)
c

# naming conversion for matrix
a<-matrix(1:6, ncol=3, nrow=2)
length(a)
nrow(a)
ncol(a)
rownames(a)<-c("A","B")
colnames(a)<-c('a','b','c')
a

# naming conversion for array
b<-array(1:12,c(2,3,2))
dimnames(b)<-list(c('one','two'),c('a','b','c'),c('A','B'))
b

#checking the directory
getwd()
# using data frame
data<-read.table(file.choose(),header = TRUE,sep=";")
#data<-read.csv("Sight_Datafile.csv",header = TRUE,sep=";")
data
nrow(data)
