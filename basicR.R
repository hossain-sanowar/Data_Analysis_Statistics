1+1
print("this is rubish");library(help="datasets")
exp(1)
log(1)
x<-1
y<-2
plot(x,y,pch=3)
# Square Root solution
sqrt((20-1.5)^2+(20-1.5)^2)
years<-1966:2014
years
dbl_var<-c(1,2,4)
int_var<-c(1L,2L,4L)
ls()#display objects which currently stored in R
#rm(years)# to remove the objects
typeof(dbl_var)
length(dbl_var)
#attr(dbl_var)
a<-1:11
(average<-sum(a)/length(a))#close bracket to get the ouput 
#average
x<-c(10,20,NA,4,NA,2)
i<-is.na(x)
i;!i
(y<-x[!i])
mean(x,na.rm=TRUE)

# naming convension
fruit<-c(5,10,1,20)
names(fruit)<-c("orange","banana","apple","peach")
fruit[c('apple','orange')]
