#----------------------------------------------------------
# Task 6
#----------------------------------------------------------

######################################################################
## Line Transect Sampling  and Point transect sampling 
## Estimation of population densities (number of individuals per area)
##
## Reference: Distance sampling; Len Thomas, Stephen T. Buckland, 
## Kenneth P. Burnham, David R. Anderson, Jeffrey L. Laake, 
## David L. Borchers & Samantha Strindberg; Volume 1, pp 544-552;
## Encyclopedia of Environmetrics (ISBN 0471 899976); Edited by
## Abdel H. El-Shaarawi and Walter W. Piegorsch;
## John Wiley & Sons, Ltd, Chichester, 2002
#######################################################################
# Distance sampling is a widely-used group of closely related methods 
# for estimating the density and/or abundance of biological populations. 
# The main methods are line transects and point transects (also called
# variable circular plots).

# Line Transect Sampling
# ======================
# In line-transect sampling a series of straight lines (tracklines) is traversed by an observer. 
# Any detected object of interest is recorded with its perpendicular distance x from the 
# trackline.

# Point-transect Sampling (also Variable Circular Plot Survey (VCPS))
# =======================
# In point-transect sampling, an observer visits a number (k) of points, the locations of which 
# are determined by some randomized design. Sampling is done from those points and detected 
# objects of interest are recorded with their distance r from that point. By recording from points, 
# the observer can concentrate on detecting the objects of interest, without having to navigate 
# along a line, and without having to negotiate a randomly positioned line through possibly 
# difficult terrain. 

# Random placement of the lines/plots ensures that the animals are uniformly distributed with respect to 
# their distance from the line, resp. the center of the plot. I.e. in a histogram of distance data 
# that had been sampled following line transect method we would expect a uniform distribution of the 
# class frequencies given all animals in the given area have been be detected. For data that has been 
# sampled following the point transect method it has to be considered, that the area of an incremental 
# annulus of radius r is dependent on r and increases linearly with increasing r. I.e. in a histogram 
# the distribution of the class frequencies would be expected to increase linearly with increasing r. 
# Scaling the class frequencies with their midpoints would reveal the underlying uniform distribution of
# the distance values again.

# The distance data are used to estimate the density of the observed animal species for a certain 
# study area. Given the number of observations (n) and the size of the area covered by the plots 
# (e.g. for point transect sampling: a = k * PI * w, with animals further than w not being recorded = 
# truncationdistance), the object density is estimated as follows: 
#
#                               D = n/(a*P)
#
# with a for Line transect sampling: a = 2 * w * L 
# (w: truncation distance, animals further than w are not being recorded)
# (L: total length of surveyed tracklines)
#
# with a for point transect sampling: a = k * PI * w
#
# P represents a detectability factor, i.e. the probability that a randomly chosen animal that is located 
# within the surveyed area is detected. ==> Before D  can be estimated, P has to be estimated.
#
# The estimation process of P is based on the detectability curve, resp. the detection function g(x) or g(r)  
# g(r). g(x) and g(r) are models that describe the probability that an object at distance x from the 
# line of a transect, resp. at distance r from the center of a plot is detected. A basic assumption of 
# the detectability model is that g(0)=1, that is, we are certain to detect any animal that is located 
# at the center of the plots. Furthermore the detection probability is expected to decline with 
# increasing distance (x or r).

# The frequency distribution of the recorded distance values can be considered a representation of the 
# distance dependent detectability. The shape of the histogram of the distance data can be considered 
# an estimate for the shape of g(x), resp. of g(r). For point transect data scaled class frequencies would 
# have to be used. That means the histogram of the distance data could be used to fit a detectability 
# model to the data. The results of such an approach, i.e. the quality of the resulting detectability 
# model then depends largely on the choice of bin origin and size. 

# P can be considered the proportion of the objects that have been detected among the total number of 
# objects in the given area. Since the area of the histogram of the survey data is a representation of 
# the number of objects that have been detected, P equals the proportion of the area within the expected 
# uniform distribution.

# P can also be considered the effective sampling area (, resp. nu), determined by an effective strip 
# distance , resp. radius rho. Within the the effective area as many objects are not detected as are 
# detected beyond , resp. rho. 

# A histogram that represents relative frequency densities covers an area that equals unity. Such a
# histogram can be used as a very basic estimator for the probability distribution (rather probability 
# density distribution or probability density function, PDF) of the given variable. 
# For fitting PDFs a lot of techniques have been developed. If we scale g(x), resp. g(r) accordingly, 
# we do get the respective PDFs (f(x), resp. f(r)). The estimated PDFs then can be used to estimate the
# object density D from the survey data:

# Line transect sampling: f(x) = g(x)/  ==>  f(0) = 1/    ==>  D = (n * f(0))/(2(sum of l))
# Point transect sampling: f(r) = 2*PI * r* g(r)/nu      ==>  D = (n * f'(0))/(2*PI*k)


# ===>
# Import the data from point transect sampling: 
# 84 observations of distances, recorded from 20 plots, each with a maximum radius w of 50 m. 

# Data import
distanceVec <- scan(sep=" ", text = "15 16 10 8 4 2 35 7 5 14 14 0 35 31 0 10 36 16 5 3 22 7 55 24 42 29 2 4 14 29 17 1 3 17 0 10 45 10 9 22 11 16 10 22 48 18 41 4 43 13 7 7 8 9 18 2 5 6 48 28 9 0 54 14 21 23 24 35 14 4 10 18 14 21 8 14 10 6 11 22 1 18 30 39")
str(distanceVec)
distanceVec

# (Relative) frequency density distribution of the distance data
hist1 <- hist(distanceVec,freq=FALSE) # hist returns an object that contains interval and frequency data

# In order to get a histigram that allows to model the detection function g(x), the linear dependency 
# of the area of an incremental annulus at radius r of the radius itself has to be considered by scaling
# the histogram frequencies: Frequency /Class Midpoints .
histAdjusted <- hist1 # copy the original hist object first and manipulate the copy 
histAdjusted$counts <- histAdjusted$counts/histAdjusted$mids
histAdjusted$xname <- "Distance Frequency/Class midpoint"
plot(histAdjusted, freq=TRUE, ylab = "Frequency/r") # ==> gives an idea of the shape of the detection model   
str(histAdjusted)

# Adjust the relative frequenciy densities as well 
(relFreq <- histAdjusted$counts/sum(histAdjusted$counts))
(classWidth <- 2*(histAdjusted$mids - histAdjusted$breaks[1:11]))
histAdjusted$density <- (histAdjusted$counts/sum(histAdjusted$counts))/classWidth
plot(histAdjusted, freq=FALSE)


# A histogram, constructed through discretization of the data, is piecewise constant 
# (hence not at all smooth) and can be extremely sensitive to the choice of bin origin and size. 
# ==> The following set of histograms illustrates the impact of the choice of bin origin! 
# Get the current graphics settings first, inorder to be able to set the configuration back to this
default_par <- par()
# devide the graphic window into two rows and 2 columns
par(mfrow = c(2, 2), cex=1/2)
breaksA <- seq(from=-2.5, to=57.5, by=10)
breaksB <- seq(from=-2, to=58, by=10)
breaksC <- seq(from=-1, to=59, by=10)
breaksD <- seq(from=0, to=60, by=10)
histA <- hist(distanceVec,breaks=breaksA, freq=FALSE)
histB <- hist(distanceVec,breaks=breaksB, freq=FALSE)
histC <- hist(distanceVec,breaks=breaksC, freq=FALSE)
histD <- hist(distanceVec,breaks=breaksD, freq=FALSE)
# setting the graphics settings back
par(default_par)

# Now create and display histograms with different amounts of bins.

# Get the current graphics settings before changing anything. 
# This allows you to set the configuration easily back to the current stat when the work is done.
default_par <- par()

# devide the graphic window into two rows and 2 columns
par(mfrow = c(3, 3),  
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)            # allow content to protrude into outer margin (and beyond)

# Histogram with 20 bins, suppress axis and legends in order to add afterwards what is needed
Hist_20 <- hist(distanceVec, breaks=20, xlab=NULL,  ylab=NULL, 
                xlim = c(0,60), ylim = c(0,0.065), main=NULL, 
                freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=seq(0,0.065,by=0.01))
axis(side = 1, at=seq(0,60,   by=10), labels=FALSE )
# Optionally, for comparison add a line that represents the kernel density estimate (see below)
# with a comparably moderate smoothing factor (low bandwidth parameter)
##lines(density(distanceVec, kernel="biweight", bw=1), main=NULL )
# title(main ="breaks=20", cex.main=0.8 )
legend(25,0.05,c('breaks=20'), bty = 'n', border = NA)

# Histogram with 19 bins
# Sometimes R overrules the number of bins set with the argument break, then alternatively the boundaries 
# for the desired interval should be defined.
# ==> Create a vector that will be used to define the boundaries for the bins
Hist_19 <- hist(distanceVec, breaks=seq(0,55,by=55/19), xlab=NULL,  ylab=NULL, 
                xlim = c(0,60), ylim = c(0,0.065), main=NULL, 
                freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=FALSE)
axis(side = 1, at=seq(0,60,   by=10), labels=FALSE )
# Optionally, for comparison add a line that represents the kernel density estimate (see below)
# with a comparably moderate smoothing factor (low bandwidth parameter)
##lines(density(distanceVec, kernel="biweight", bw=1), main=NULL )
# title(main ="breaks=20", cex.main=0.8 )
legend(25,0.05,c('breaks=19'), bty = 'n', border = NA)

# Histogram with 18 bins
Hist_18 <- hist(distanceVec, breaks=seq(0,55,by=55/18), xlab=NULL,  ylab=NULL, 
                xlim = c(0,60), ylim = c(0,0.065), main=NULL, 
                freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=FALSE)
axis(side = 1, at=seq(0,60,   by=10), labels=FALSE )
# Optionally, for comparison add a line that represents the kernel density estimate (see below)
# with a comparably moderate smoothing factor (low bandwidth parameter)
##lines(density(distanceVec, kernel="biweight", bw=1.5), main=NULL )
# title(main ="breaks=20", cex.main=0.8 )
legend(25,0.05,c('breaks=18'), bty = 'n', border = NA)


# Histogram with 11 bins
Hist_11 <- hist(distanceVec, breaks=seq(0,55,by=55/11), xlab=NULL, ylab=NULL, 
                xlim = c(0,60), ylim = c(0,0.065), main=NULL, freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=seq(0,0.065,by=0.01))
axis(side = 1, at=seq(0,60,   by=10), labels=FALSE )
# title(main ="breaks=11" )
legend(25,0.05,c('breaks=11'), bty = 'n', border = NA)

# Histogram with 10 bins
Hist_10 <- hist(distanceVec, breaks=seq(0,55,by=55/10), xlab=NULL, ylab=NULL, 
                xlim = c(0,60), ylim = c(0,0.065), main=NULL, freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=FALSE)
axis(side = 1, at=seq(0,60,   by=10), labels=FALSE )
# title(main ="breaks=10" )
legend(25,0.05,c('breaks=10'), bty = 'n', border = NA)

# Histogram with 9 bins
Hist_9 <- hist(distanceVec, breaks=seq(0,55,by=55/9), xlab=NULL, ylab=NULL, 
               xlim = c(0,60), ylim = c(0,0.065), main=NULL, freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=FALSE)
axis(side = 1, at=seq(0,60,   by=10), labels=FALSE )
# title(main ="breaks=9" )
legend(25,0.05,c('breaks=9'), bty = 'n', border = NA)

# Histogram with 8 bins
Hist_8 <- hist(distanceVec, breaks=seq(0,55,by=55/8), xlab=NULL, ylab=NULL, 
               xlim = c(0,60), ylim = c(0,0.065), main=NULL, freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=seq(0,0.065,by=0.01))
axis(side = 1, at=seq(0,60,   by=10),   labels=seq(0,60,   by=10) )
#title(main ="breaks=8" )
legend(25,0.05,c('breaks=8'), bty = 'n', border = NA)

# Histogram with 7 bins
Hist_7 <- hist(distanceVec, breaks=seq(0,55,by=55/7), xlab=NULL, ylab=NULL, 
               xlim = c(0,60), ylim = c(0,0.065), main=NULL, freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=FALSE)
axis(side = 1, at=seq(0,60,   by=10),   labels=seq(0,60,   by=10) )
#title(main ="breaks=8" )
legend(25,0.05,c('breaks=7'), bty = 'n', border = NA)

# histogram with 4 bins
bins2 <- seq(0,60,by=60/4)
Hist_4 <- hist(distanceVec, breaks=seq(0,55,by=55/7), xlab=NULL, ylab=NULL, 
               xlim = c(0,60), ylim = c(0,0.065), main=NULL, freq = FALSE, axes=FALSE)
axis(side = 2, at=seq(0,0.065,by=0.01), labels=FALSE)
axis(side = 1, at=seq(0,60,   by=10),   labels=seq(0,60,   by=10) )
# Optionally, for comparison add a line that represents the kernel density estimate (see below)
# with a comparably strong smoothing factor (high bandwidth parameter)
##lines(density(distanceVec, kernel="biweight", bw=5), main=NULL )
#title(main ="breaks=4" )
legend(25,0.05,c('breaks=4'), bty = 'n', border = NA)

title(xlab = "Distance", ylab = "Frequency Density", outer = TRUE, line = 2)

# setting the graphics settings back
par(default_par)

# ==> The number of breaks in our histogram largely influences the appearance of the histogram 
# (e.g. gaps, smoothness) and hence the resulting detectability model.  

# Beside histograms there are more sophisticated techniques for estimating densities 
# that produce smoother results, ... 

# ... e.g. the Average Shifted Histogram 
library(ash)
# The histogram R would produce without specifying the breaks.
hist1 <- hist(distanceVec,freq=FALSE)
# The density curve as a result of the Average Shifted Histogram approach
# with a moderate smoothing factor
f <- ash1(bin1(distanceVec,nbin=11), m=2)
lines(f$x,f$y,col="blue" )
# The density curve as a result of the Average Shifted Histogram approach
# with a stronger smoothing factor
f <- ash1(bin1(distanceVec,nbin=11), m=5)
lines(f$x,f$y,col="green" )


# ... or the Kernel Density Estimation
density(distanceVec, kernel="biweight", bw=3)
lines(density(distanceVec, kernel="biweight", bw=4), main=NULL )


#  