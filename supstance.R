# We use various algorithms here to calculate levels of interest

source("common.R")

# .. TODO Precise Decimal Level which to round pivot to
#ROUND_PIVOT_PRECISE <- 2

# ... TODO Multiple Level which to round pivot to (applied to precise decimal level above) 
# ROUND_PIVOT_MULTIPLE <- 5

# ...precise = 2 / multiple = 5 -> 42.00, 42.05, 42.10, etc.
# ...precise = 3 / multiple = 3 -> 42.000, 42.003, ..., 42.009, 42.010, 42.013

quotes     <- load_default_symbol("supstance")

# Computer Pivot Point Based Supstance
center <- xts(rowSums(HLC(quotes))/3, order.by=index(quotes)) # H+L+C/3
R1 <- (2*center)-Lo(quotes) # Resistance 1  = (2*P) - Low
S1 <- (2*center)-Hi(quotes) # Support 1     = (2*P) - High
R2 <- center + (R1 - S1)    # Resistance 2  = P + (R1-S1)
S2 <- center - (R1 - S1)    # Support 2     = P - (R1- S1)

# Combining all Calculations in adjacent columns
quotes.pivot_supstance <- cbind(center,R1,R2,S1,S2)
colnames(quotes.pivot_supstance) <- c('center','R1','R2','S1','S2')

# ... Calculate supstance based on local extrema

# Calculate supstance based on Close (quotes[,4]) Extrema
quotes.extrema <- ifelse(diff(sign(diff(EMA(quotes[,4])))) == -2, TRUE, FALSE)
quotes.extrema <- na.locf(ifelse(quotes.extrema, quotes[,4], NA))
quotes.extrema_level_lengths  <- sort(table(quotes.extrema), decreasing=TRUE) # Note: a level may not be contiguous, level will be tallied accross all extrema instances
extrema_level_values          <- sapply(names(quotes.extrema_level_lengths), as.double)

# For each extrema based level calculate % of Close prices that fall above / below level
per_under <- sapply(extrema_level_values,
                    function(x){ return(sum(quotes[,4] < x)) }) / nrow(quotes)

per_over  <- sapply(extrema_level_values,
                    function(x){ return(sum(quotes[,4] > x)) }) / nrow(quotes)

quotes.extrema_levels <- cbind(cbind(quotes.extrema_level_lengths, per_under), per_over)
colnames(quotes.extrema_levels)[1] <- "num" # XXX rename col to something more fiendly

# ... TODO calculate supstance based on volume levels

# data/plots
#print(quotes.pivot_supstance)
#print(quotes.extrema)
#print(quotes.extrema_levels)

#print(chartSeries(quotes, TA=NULL))
#print(addTA(EMA(quotes[,4]), on=1, col=6))
#print(addTA(quotes.extrema, on=1, col=3))
#print(abline(h=as.numeric(head(names(quotes.extrema_level_lengths), 2)), col=2))

write_out("supstance",  quotes.extrema_levels)
