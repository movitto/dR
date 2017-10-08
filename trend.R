# We generate Trends via Moving Averages and ADX here

library(TTR)

source("common.R")

# ADX Trend Levels
ADX_LEVELS <- list(none   = 37.5, # below this = no trend
                   strong = 65)   # above this = strong trend

# MACD ranges
# ...TODO make this a collection of macd's to compute (?)
MACD_RANGES <- list(fast = 9, slow = 26, sig = 12)

# Averages to compute & check for crossovers
AVERAGES <- c(5, 10, 25, 50, 100, 200)

# Load symbol and calculate ADX
quotes     <- load_default_symbol("trend")
quotes.adx <- ADX(quotes, n=12)

# Remove Averages for which we don't have enough data for
NAVERAGES <- AVERAGES[AVERAGES < nrow(quotes)]
if(length(AVERAGES) != length(NAVERAGES)){
  warning("Insufficient data to compute averages: ")
  warning(paste0(setdiff(AVERAGES, NAVERAGES), sep=", "))
}

# Matrix of Pairs of Averages
#AVERAGE_PAIRS <- combn(NAVERAGES, 2)

# Infer trend levels from ADX
quotes.adx$trending       <- ifelse(quotes.adx$ADX < ADX_LEVELS[["none"]], FALSE, TRUE)
quotes.adx$strong_trend   <- ifelse(quotes.adx$trending,
                             ifelse(quotes.adx$ADX > ADX_LEVELS[["strong"]], TRUE, FALSE), FALSE)

# Compute MACD from Close Price (quotes[,4])
quotes.macd <- MACD(quotes[,4], nFast=MACD_RANGES[["fast"]],
                                nSlow=MACD_RANGES[["slow"]],
                                nSig=MACD_RANGES[["sig"]])

# Infer general trend dir from MACD/Signal
quotes.macd$trend_up   <- ifelse(quotes.macd$macd > quotes.macd$signal, TRUE, FALSE )
quotes.macd$trend_down <- ifelse(quotes.macd$macd < quotes.macd$signal, TRUE, FALSE )

# Compute MACD Zero & Signal Crossovers
# (All the times the MACD goes above/below the Signal)
quotes.macd$pos_zero_xover <- pos_crossovers(quotes.macd$macd, 0)
quotes.macd$neg_zero_xover <- neg_crossovers(quotes.macd$macd, 0)

quotes.macd$pos_signal_xover <- pos_crossovers(quotes.macd$macd, quotes.macd$signal)
quotes.macd$neg_signal_xover <- neg_crossovers(quotes.macd$macd, quotes.macd$signal)

# Compute few custom averages from Close Price
quotes.emas        <- lapply(NAVERAGES, function(x){ return(EMA(quotes[,4], x)) })
quotes.emas        <- do.call(merge.xts, quotes.emas)
names(quotes.emas) <- paste("EMA", NAVERAGES, sep="")

# Compute Closing Price / Average Crossovers
# (All the times the Closing Price goes above / blow the given average)
quotes.emas.pos_price_xovers <- lapply(quotes.emas, function(ema) { return(pos_crossovers(ema, quotes[,4])) })
quotes.emas.neg_price_xovers <- lapply(quotes.emas, function(ema) { return(neg_crossovers(ema, quotes[,4])) })

quotes.emas.pos_price_xovers <- do.call(merge.xts, quotes.emas.pos_price_xovers )
quotes.emas.neg_price_xovers <- do.call(merge.xts, quotes.emas.neg_price_xovers )

# ...Also Compute Crossovers between pairs of Averages (see AVERAGE_PAIRS above)
# All the times the first (smaller/faster) average crosses above/below the second (larger/slower)
# (use MACD?)

### data/plots
#print(quotes)
#print(quotes.macd)
# print(quotes.emas)

#print(chartSeries(quotes, TA=NULL))
#print(addTA(quotes.emas$EMA25, on=1, col=5))
#print(addTA(quotes.emas$EMA5, on=1, col=4))
#print(addTA(quotes.macd$macd))
#print(addTA(quotes.macd$signal, on=2, col=2))

#col1<-rgb(runif(5),runif(5),runif(5), 0.4)
#print(addTA(as.xts(as.logical(quotes.macd$trend_down), index(quotes.macd)), col=col1, on=1))
#
#print(addTA(quotes.adx$ADX))
#print(addTA(as.xts(as.logical(quotes.adx$trending), index(quotes.adx)), col=col1, on=3))

#ppx <- index(quotes.emas.pos_price_xovers$EMA10[quotes.emas.pos_price_xovers$EMA10])
#ppxc <-Cl(quotes[ppx])
#print(addTA(ppxc, type='p', on=1, col=3, pch='P'))
#
#psx <- index(quotes.macd$neg_signal_xover[as.logical(quotes.macd$neg_signal_xover)])
#psxc <-Cl(quotes[psx])
#print(addTA(psxc, type='p', on=1, col=2, pch='N'))

write_out("trend", quotes.macd, "macd")
write_out("trend",  quotes.adx, "adx")
