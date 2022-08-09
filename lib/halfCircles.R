#
# Draw one half circle
#

halfCircle <- function(x, y, r, nsteps=100, ...) {
    rs <- seq(0, pi, len=nsteps) 
    xc <- x + r*cos(rs) 
    yc <- y + r*sin(rs) 
    polygon(xc, yc, ...) 
}



#
# Basic half circles more like symbols()
#

halfCirclesBasic <- function(x, y, r, col="#00000050", ...) {
    xlim <- range(x)
    ylim <- range(y)
    plot(0, 0, type="n", asp=1, xlab="", ylab="", xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", bty="n", ...)

    vals <- cbind(x, y, r)
    tmp <- apply(vals, 1, function(v) {
        halfCircle(v[1], v[2], v[3], col=col, border=NA)
    })    
}




#
# Half circles with scaling
#

halfCircles <- function(x, y, r, col="#00000050", add=FALSE, withscaling=FALSE, ...) {
    
    xlim <- range(x)
    ylim <- range(y)
    
    if (withscaling == TRUE) {
        x_range_diff <- max(x) - min(x)
        y_range_diff <- max(y) - min(y)

        if (x_range_diff >= y_range_diff) {
            scale_factor <- x_range_diff / y_range_diff
            xlim <- range(x)
            ylim <- range(y) * scale_factor
            y <- y * scale_factor
        } else {
            scale_factor <- y_range_diff / x_range_diff
            xlim <- range(x) * scale_factor
            ylim <- range(y)
            x <- x * scale_factor
        }
    }
    
    if (!add) {
        plot(0, 0, type="n", asp=1, xlab="", ylab="", xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", bty="n", ...)
    }
    
    vals <- cbind(x, y, r)
    tmp <- apply(vals, 1, function(v) {
        halfCircle(v[1], v[2], v[3], col=col, border=NA)
    })
}






