source("common.R")

find_peaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
       if(i + 2 > length(x)) return(numeric(0))

       z <- i - m + 1
       z <- ifelse(z > 0, z, 1)
       w <- i + m + 1
       w <- ifelse(w < length(x), w, length(x))
       if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
     pks <- unlist(pks)
     pks
}

# TODO expand upon the above w/ a n parameter to specify the number of significant peaks
# to return, sort the peaks list by their relative strengths before returning

quotes <- load_default_symbol("extrema")

peaks   <- find_peaks(coredata(Hi(quotes)), 5)
valleys <- find_peaks(-coredata(Lo(quotes)), 5)

pts <- index(quotes[peaks])
pvs <- Hi(quotes[pts])

vts <- index(quotes[valleys])
vvs <- Lo(quotes[vts])

# ... TODO detect high/low price ranges for sideways markets
# Detect when price ranges have failed

###

#print(chartSeries(quotes, TA=NULL))
#print(addTA(pvs, type='p', on=1, col=2, pch='+'))
#print(addTA(vvs, type='p', on=1, col=3, pch='-'))

write_out("extrema",  pvs, "peaks")
write_out("extrema",  vvs, "valleys")
