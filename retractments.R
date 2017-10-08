source("common.R")

# Statically set Trend
# TODO make this dynamic
TREND='down'

# Load symbol / default intervals
quotes <- load_default_symbol("retractments")

# Extrapolate quotes to higher timeframe, generate ATR
quotes.higher    <- extrapolate_quotes(quotes)
extrapolated_atr <- na.omit(ATR(quotes.higher)$atr)
extrapolated_atr <- coredata(extrapolated_atr[length(extrapolated_atr)])

# Generate list of previous quotes for each quote
quotes.prev <- complete_lag(Cl(quotes))

# Generate max/min extrema of previous quotes
#suppressMessages(function(){
  quotes.prev.max <- apply(quotes.prev, 1, max, na.rm=T)
  quotes.prev.min <- apply(quotes.prev, 1, min, na.rm=T)
#})

# Run through each data point and detect groups of
# periods imediately following whose close < current one
# (reverse for down trend).
# ... TODO also handle sideways (using moving average to detect trend, store reversals)
if(TREND == 'up'){
  quotes.ret <- ifelse(Cl(quotes) < quotes.prev.max, TRUE, FALSE)
  quotes.ret$level <- ifelse(quotes.ret, quotes.prev.max - Cl(quotes), NA)
}else{
  quotes.ret <- ifelse(Cl(quotes) > quotes.prev.min, TRUE, FALSE)
  quotes.ret$level <- ifelse(quotes.ret, Cl(quotes) - quotes.prev.min, NA)
}

# Group levels by retractment
quotes.rets <- split(quotes.ret$level, cumsum(is.na(quotes.ret$level)))

# Remove invalid retractments / data
quotes.rets <- quotes.rets[!is.na(lapply(quotes.rets, function(x){ return(x$level); }))]
quotes.rets <- lapply(quotes.rets, function(x){ return(x[!is.na(x$level)]) })

# Convert to xts
quotes.rets <- lapply(quotes.rets, as.xts)

#XXX adjust names
names(quotes.ret)[[1]] <- "In_Retractment"
names(quotes.rets)     <- index(quotes.rets)
quotes.rets            <- lapply(quotes.rets, function(x){ names(x) <- c("Retractment"); return(x) })

## For each retractment, generate max/mean levels
quotes.rets.max  <- lapply(quotes.rets, max)
quotes.rets.mean <- lapply(quotes.rets, mean)

# Generate max & current levels as a percentage of atr & abs price
quotes.rets.per_atr      <- lapply(quotes.rets,      function(x){ return(x/extrapolated_atr[[1]]) })
quotes.rets.max.per_atr  <- lapply(quotes.rets.max,  function(x){ return(x/extrapolated_atr[[1]]) })
quotes.rets.mean.per_atr <- lapply(quotes.rets.mean, function(x){ return(x/extrapolated_atr[[1]]) })

### data/plots
#barChart(quotes)
#addTA(as(quotes.ret.close, as.Date(index(x))), on=1, col=7)

write_list_out("retractments",  quotes.rets)
