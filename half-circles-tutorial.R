source("lib/halfCircles.R")

disasters <- read.csv("data/billion-dollar-disasters-1980-2017.tsv", sep="\t", stringsAsFactors = FALSE)
disasters$year <- as.numeric(strftime(disasters$begin_date, format="%Y"))
disasters$dayofyear <- as.numeric(strftime(disasters$begin_date, format="%j"))


#
# Circles.
#

symbols(disasters$dayofyear, disasters$year, sqrt(disasters$cost_billions), inches=FALSE, xlab="", ylab="")


#
# Half circles
#

# One half circle, unfixed aspect ratio
plot(0, 0, type="n", xlim=c(0,2), ylim=c(1,2), xlab="", ylab="")
halfCircle(1, 1, 1)

# One half circle, fixed aspect ratio
plot(0, 0, type="n", xlim=c(0,2), ylim=c(1,2), asp=1, xlab="", ylab="")
halfCircle(1, 1, 1)

# Semicircle with fewer steps
par(mfrow=c(1,3))
nsteps <- c(100, 20, 5)
for (n in nsteps) {
    plot(0, 0, type="n", xlim=c(0,2), ylim=c(1,2), asp=1, xlab="", ylab="", main=paste("nsteps=", n, sep=""))
    halfCircle(1, 1, 1, nsteps=n)
}




# Multiple circles, fixed aspect ratio
x <- c(1, 2.75, 5, 8)
y <- c(0, 0, 0, 0)
r <- c(.5, .75, 1, 1.25)
plot(x, y, type="n", xlim=c(0, 10), ylim=c(0, 2), asp=1)   # Set asp to 1
for (i in 1:length(x)) {
    halfCircle(x[i], y[i], r[i])
}

# Multiple circles, use of apply()
plot(x, y, type="n", xlim=c(0, 10), ylim=c(0, 2), asp=1)   # Set asp to 1
vals <- cbind(x, y, r)
tmp <- apply(vals, 1, function(v) {
    halfCircle(v[1], v[2], v[3], col="black", border=NA)
})    


#
# Draw a quick half-circle plot.
#

halfCirclesBasic(disasters$dayofyear, disasters$year, sqrt(disasters$cost_billions), col="#00003380", main="Billion-Dollar Weather and Climate Disasters")



#
# Draw half-circle plot with scaling
#

halfCircles(disasters$dayofyear, disasters$year, 2*sqrt(disasters$cost_billions), col="#00003380", add=FALSE, withscaling=TRUE, main="Billion-Dollar Weather and Climate Disasters")



#
# Draw plot with customizations.
#

# Scaling
scale_factor <- (366-0) / (2017-1980)

par(las=1)
plot(0, 0, xlab="", ylab="", xlim=c(0, 366), ylim=c(1980, 2017)*scale_factor, type="n", asp=1, xaxt="n", yaxt="n", bty="n", main="Billion-Dollar Weather and Climate Disasters")

# Custom axes
axis(2, at = 1980:2017*scale_factor, labels = 1980:2017, lwd = 0, cex.axis=.7, line = -1)

axis_dates <- strftime(c("2017-01-01", "2017-04-01", "2017-08-01", "2017-12-01"), format="%j")
axis_date_labels <- c("January", "April", "August", "December")
axis(1, at = axis_dates, labels=axis_date_labels, cex.axis=.7, lwd=0, lwd.ticks=.5, padj=-1)

# Year lines
x0 <- rep(0, length(1980:2017))
x1 <- rep(366, length(1980:2017))
y0 <- rep(1980:2017*scale_factor, each=2)
y1 <- y0
segments(x0, y0, x1, y1, lwd=.5, col="#cccccc")

# Add half circles
halfCircles(disasters$dayofyear, disasters$year*scale_factor, 2*sqrt(disasters$cost_billions), col="#00003380", add=TRUE)


