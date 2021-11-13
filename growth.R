rm(list=ls())
getwd()
#----------------------------------------------------------
# get the data 
#----------------------------------------------------------
#growthData<-read.table(file.choose())
growthData<-read.csv("growth.csv")
str(growthData)

growthData
summary(growthData)

#=====================================
# Distribution of weight gain
# ==> Set up intervals (width 1.5) for histograms
bins <- seq(15,30, by=1.5)
growthHist <- hist(growthData$gain, 
                   breaks = bins,
                   main = "Histogram of Total Weight Gain",
                   xlab = "Weight Gain")
#growthHist
# draw the first histogram
wheatHist <- hist(growthData[,3][growthData[,2]=="wheat"], 
                  breaks=bins, 
                  col="cornflowerblue",
                  #density=12, angle=80, 
                  xlab="Gain", 
                  ylim = c(0, 8),
                  main="Distribution of Weight Gain")

oatsHist <- hist(growthData[,3][growthData[,2]=="oats"], 
                 col=rgb(red=0, green = 0, blue = 0, alpha=1),  ## <== transrancy through alpha 
                 density=8, angle=45, 
                 breaks=bins,  
                 add=TRUE) 

barleyHist <- hist(growthData[,3][growthData[,2]=="barley"], 
                   col=rgb(red=0.7, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                   #density=18, angle=145,                  
                   breaks=bins,  
                   add=TRUE)
# add a legend
legend(x=26,y=8, 
       fill = c("cornflowerblue", rgb(red=0, green = 0, blue = 0, alpha=1), rgb(red=0.7, green = 0, blue = 0, alpha=0.2)),
       density=c(NA, 8, NA),
       angle=c(FALSE, 45, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0,5, alpha=0.5),"cornflowerblue"), 
       legend=c("wheat", "oats", "barley"))
#=====================================
# Compare different supplements with 
# the contol group
#=====================================
parBackup <- par()
par(mfrow=c(3,1))

# draw the control histogram for the first comparison
controlHist <- hist(growthData[,3][growthData[,1]=="control"], 
                    col=rgb(red=0, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                    density=18, angle=145,                  
                    breaks=bins,
                    xlab="Gain", 
                    ylim = c(0, 4),
                    main="Distribution of Weight Gain")    
# add the second histogram to the existing plot
supergainHist <- hist(growthData[,3][growthData[,1]=="supergain"], 
                      breaks=bins, 
                      col=rgb(red=0, green = 0, blue = 0.7, alpha=0.2),  ## <== transrancy through alpha ,
                      #density=12, angle=80, 
                      add=TRUE)

# add a legend
legend(x=26,y=4, 
       fill = c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0, blue = 0.7, alpha=0.2)),
       density=c(18, NA),
       angle=c(145, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0, blue = 0.7, alpha=0.2)), 
       legend=c("control", "supergain")
)

#---------------------second: agrimore - control
# draw the contol histogram for the second comparison
controlHist <- hist(growthData[,3][growthData[,1]=="control"], 
                    col=rgb(red=0, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                    density=18, angle=145,                  
                    breaks=bins,
                    xlab="Gain", 
                    ylim = c(0, 4),
                    main="Distribution of Weight Gain") 


# add the second histogram to the existing plot
agrimoreHist <- hist(growthData[,3][growthData[,1]=="agrimore"], 
                     col=rgb(red=0.7, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                     #density=8, angle=45, 
                     breaks=bins,  
                     add=TRUE)    

# add a legend
legend(x=26,y=4, 
       fill = c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0.7, green = 0, blue = 0, alpha=0.2)),
       density=c(18, NA),
       angle=c(145, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0.7, green = 0, blue = 0, alpha=0.2)), 
       legend=c("control", "agrimore")
)


#---------------------third: supersupp - control
# draw the contol histogram for the third comparison
controlHist <- hist(growthData[,3][growthData[,1]=="control"], 
                    col=rgb(red=0, green = 0, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                    density=18, angle=145,                  
                    breaks=bins,
                    xlab="Gain", 
                    ylim = c(0, 4),
                    main="Distribution of Weight Gain") 


# add the second histogram to the existing plot
supersuppHist <- hist(growthData[,3][growthData[,1]=="supersupp"], 
                      col=rgb(red=0, green = 0.7, blue = 0, alpha=0.2),  ## <== transrancy through alpha 
                      #density=8, angle=45, 
                      breaks=bins,  
                      add=TRUE)    

# add a legend
legend(x=26,y=4, 
       fill = c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0.7, blue = 0, alpha=0.2)),
       density=c(18, NA),
       angle=c(145, FALSE),
       col=c(rgb(red=0, green = 0, blue = 0, alpha=0.2), rgb(red=0, green = 0.7, blue = 0, alpha=0.2)), 
       legend=c("control", "supersupp")
)

par(parBackup)

#=====================================
#=====================================
# Frequency Distributions - Dotcharts
#=====================================
#=====================================

#=====================================
# Dotplot: Sorted, Grouped by Diet and Colored by Supplement
#=====================================

# Sort by gain 
x <- growthData[order(growthData$gain),] 
str(x)
# prepare colors for the different supplements
x$color[x$supplement=="control"] <- "black"
x$color[x$supplement=="supergain"] <- "blue"
x$color[x$supplement=="supersupp"] <- "red"
x$color[x$supplement=="agrimore"] <- "darkgreen"

# create dotchart grouped by diet
dotchart(x$gain,cex=.7,xlim=c(15,25),
         main="Weightgain for supplements grouped by Diet",
         xlab="Weightgain", gcolor="black", color=x$color) 

# add a legend
legend(x=26,y=20, cex=.6,
       fill = c("black","blue","red","darkgreen"),
       col=c("black","blue","red","darkgreen"), 
       legend=c("control", "supergain", "supersupp", "agrimore")
)


