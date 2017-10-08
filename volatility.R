# We use ATR here to calculate volatility

library(TTR)

source("common.R")

# Range over which to computer TR average
ATR_RANGE = 25

# TR/ATR ratios to indicate high/low volatility
HIGH_LEVEL = 1.5
LOW_LEVEL  = 0.2

# Order of polynomial used to generate ATR regression
POLY_ORDER = 3

# ATR regression slope below which sideways market it indicated (absolute value)
MAX_SIDEWAYS_DERIV = 0.001

# Load symbol
quotes     <- load_default_symbol("volatility")

if(nrow(quotes) <= ATR_RANGE){
  stop("Cannot compute ATR, insufficient range")
}

# Calc ATR
quotes.atr <- ATR(quotes, n=ATR_RANGE)

# Generate high/low volatility levels by comparing TR to ATR
quotes.atr$tr_atr_ratio <- quotes.atr$tr / quotes.atr$atr
quotes.atr$is_high      <- ifelse(quotes.atr$tr_atr_ratio > HIGH_LEVEL, TRUE, FALSE)
quotes.atr$is_low       <- ifelse(quotes.atr$tr_atr_ratio <  LOW_LEVEL, TRUE, FALSE)

# Also Generate ratio of atr to close price
quotes.atr$atr_close_ratio <- quotes.atr$atr / Cl(quotes)

# Generate rising, falling, sideways indicators by calculating slope of ATR regression line
atr_lm       <- list()
atr_lm$df    <- data.frame(quotes.atr$atr, Time = index(quotes.atr))
atr_lm$model <- lm(atr ~ poly(Time, POLY_ORDER), data = atr_lm$df) # polynomial linear model

atr_lm$fit   <- fitted(atr_lm$model)
atr_lm$diff  <- diff(atr_lm$fit)
atr_lm$diff  <- as.xts(atr_lm$diff)

quotes.atr$is_sideways <- ifelse(abs(atr_lm$diff) < MAX_SIDEWAYS_DERIV,         TRUE, FALSE)
quotes.atr$is_rising   <- ifelse(atr_lm$diff > 0, TRUE, FALSE)
quotes.atr$is_falling  <- ifelse(atr_lm$diff < 0, TRUE, FALSE)

# XXX replace NA values w/ FALSE
quotes.atr$is_high[is.na(quotes.atr$is_high)]         <- FALSE
quotes.atr$is_low[is.na(quotes.atr$is_low)]           <- FALSE
quotes.atr$is_sideways[is.na(quotes.atr$is_sideways)] <- FALSE
quotes.atr$is_rising[is.na(quotes.atr$is_rising)]     <- FALSE
quotes.atr$is_falling[is.na(quotes.atr$is_falling)]   <- FALSE

# Current ATR / Close Ratio
quotes.atr.abs_per <- median(quotes.atr$atr_close_ratio[!is.na(quotes.atr$atr_close_ratio)])

### data/plots
#print(quotes.atr)
#print(quotes.atr.abs_per)

# Core ATR/TR/LM plots
#print(chartSeries(quotes.atr$atr))
#print(addLines(predict(atr_lm$model)))
#print(addTA(quotes.atr$tr, type="h"))

# Plot Regions, recall they correspond to different source metrics
#col1<-rgb(runif(5),runif(5),runif(5), 0.4)
#print(addTA(as.xts(as.logical(quotes.atr$is_high), index(quotes.atr)), col=col1, on=1))

### output
write_out("volatility", quotes.atr)
