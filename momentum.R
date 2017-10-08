# Generate Momentum Using ROC & Derivatives
# TODO also stochastic

source("common.R")

# Periods over which to compute the ROC
ROC_PERIODS = c(5, 10, 20)

# Stochastic Boundries
STOCH_OVERBOUGHT = 70
STOCH_MID        = 50
STOCH_OVERSOLD   = 30

# Load symbol
quotes <- load_default_symbol("momentum")

# Remove ROC Periods for which we don't have enough data
NPERIODS <- ROC_PERIODS[ROC_PERIODS < nrow(quotes)]
if(length(ROC_PERIODS) != length(NPERIODS)){
  warning("Insufficient data to compute ROC Periods: ")
  warning(paste0(setdiff(ROC_PERIODS, NPERIODS), sep=", "))
}

# Calculate ROCs from Close
quotes.rocs <- lapply(NPERIODS, function(x){ return(ROC(Cl(quotes), x)) })
names(quotes.rocs) <- paste("ROC", NPERIODS, sep="")

# ... TODO infer which ROC period aligns w/ the overall trend rate of change
#          also generate signals & stats based on ROC extrema & regressions (high/low/rising/falling)

# Calculate Stochastic from Close
quotes.sto <- stoch(Cl(quotes)) * 100

# Base Stochastic Signals & Crossovers
quotes.sto$overbought <- (quotes.sto$fastK > STOCH_OVERBOUGHT)
quotes.sto$oversold   <- (quotes.sto$fastK < STOCH_OVERSOLD)
quotes.sto$pos_mid_xover <- pos_crossovers(quotes.sto$fastK, STOCH_MID)
quotes.sto$neg_mid_xover <- neg_crossovers(quotes.sto$fastK, STOCH_MID)
quotes.sto$pos_sig_xover <- pos_crossovers(quotes.sto$fastK, quotes.sto$fastD)
quotes.sto$neg_sig_xover <- neg_crossovers(quotes.sto$fastK, quotes.sto$fastD)

### data/plots
#print(quotes)
#print(quotes.rocs)
#print(quotes.sto)

write_list_out("momentum", quotes.rocs)
write_out("momentum", quotes.sto, "sto")
