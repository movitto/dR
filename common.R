library(GetoptLong) # for qq
library(optparse)   # for OptionParser

library(zoo, quietly=TRUE, warn.conflicts=FALSE) # for xts
library(xts) # for xts and quantmod

suppressMessages(
  library(quantmod)   # for Lag/Next
)

##########################

# Config

options(width=500)

CACHE_DIR <- '../cache'

HIST_DIR <- '../hist'

DEFAULT_SOURCE <- 'cache' # cache, hist

DEFAULT_SYMBOL <- "AUDNZD"

DEFAULT_INTERVALS <- list(retractments = '5min',
                          volatility   = 'H',
                          trend        = 'H',
                          supstance    = 'H',
                          momentum     = "H",
                          extrema      = "H")

# Only apply to historical sources
DEFAULT_YEAR  <- 2011
DEFAULT_MONTH <- NA # 1-12
DEFAULT_DAY   <- NA # 1-31

HIGHER_PERIODS <- list(secs    = "minutes",
                       seconds = "minutes",
                       mins    = "hours",
                       minutes = "hours",
                       hours   = "days",
                       day     = "weeks",
                       days    = "weeks",
                       weeks   = "months",
                       months  = "years")

OUTPUT_DIR <- "../output"

###########################

# CLI Opts
cli_opts <- function(){
  option_list = list(
    make_option(c("--source"),
                action="store",
                default=NA,
                type='character',
                help="Source of Data to Scan"),

    make_option(c("-s", "--symbol"),
                action="store",
                default=NA,
                type='character',
                help="Symbol to Scan"),

    make_option(c("-i", "--interval"),
                action="store",
                default=NA,
                type='character',
                help="Interval to Load"),

    make_option(c("-e", "--extrapolate"),
                action="store",
                default=NA,
                type='character',
                help="Interval to Extrapolate To"),

    make_option(c("-y", "--year"),
                action="store",
                default=NA,
                type='character',
                help="Historical Year to load"),

    make_option(c("-m", "--month"),
                action="store",
                default=NA,
                type='character',
                help="Historical Month to load"),

    make_option(c("-d", "--day"),
                action="store",
                default=NA,
                type='character',
                help="Historical Day to load")
  )

  return(parse_args(OptionParser(option_list=option_list)))
}

parsed_cli = cli_opts()
#print("CLI Arguments:")
#print(parsed_cli)

specified_source <- function(){
  src = DEFAULT_SOURCE
  if(!is.na(parsed_cli$source)){
    src = parsed_cli$source
  }

  return(src)
}


specified_symbol <- function(){
  symbol = DEFAULT_SYMBOL
  if(!is.na(parsed_cli$symbol)){
    symbol = parsed_cli$symbol
  }

  return(symbol)
}

specified_interval <- function(categ){
  interval = DEFAULT_INTERVALS[[categ]]

  if(!is.na(parsed_cli$interval)){
    interval = parsed_cli$interval
  }

  return(interval)
}

specified_extrapolate <- function(categ){
  if(!is.na(parsed_cli$extrapolate)){
    return(parsed_cli$extrapolate);
  }

  interval = specified_interval(categ)
  if(!is.na(interval) && "list" %in% class(interval)){
    return(interval[["extrapolate"]])
  }

  return(NA)
}

specified_year <- function(){
  year = DEFAULT_YEAR
  if(!is.na(parsed_cli$year)){
    year = parsed_cli$year
  }

  return(year)
}

specified_month <- function(){
  month = DEFAULT_MONTH
  if(!is.na(parsed_cli$month)){
    month = parsed_cli$month
  }

  return(month)
}

specified_day <- function(){
  day = DEFAULT_DAY
  if(!is.na(parsed_cli$day)){
    day = parsed_cli$day
  }

  return(day)
}

specified_period <- function(){
  r <- specified_year()
  if(!is.na(specified_month())){
    m <- sprintf("%02d", specified_month())
    r <- paste(r, m, sep="")
  }
  if(!is.na(specified_day())){
    d <- sprintf("%02d", specified_day())
    r <- paste(r, d, sep="")
  }

  return(r)
}

specified_period_interval <- function(){
  if(!is.na(specified_day()) && !is.na(specified_month())){
    return("daily")
  }else if(!is.na(specified_month())){
    return("month")
  }else{
    return("year")
  }
}

###########################

# Common Helpers

# Return cache id for given symbol & interval.
# Cache ID is given by sha256 hash of symbol/interval,
# see generation in python module
cache_id <- function(symbol, interval){
  return(qq("@{symbol}-@{interval}"))
}

# Load symbol / interval from cache
load_cache_symbol <- function(symbol, interval) {
  # TODO detect/handle file not found err
  cid <- cache_id(symbol, interval)
  fle <- qq("@{CACHE_DIR}/@{cid}")

  tryCatch({
    quotes <- read.zoo(fle,
                       header=TRUE,
                       sep=",",
                       index.column=2,
                       format="%Y%m%d%H%M",
                       tz='')

  }, error = function(err){
    # TODO callback to fetch symbols...
    stop(qq("Could not load @{symbol}/@{interval})"))
  })

  # XXX
  quotes <- quotes[,-1] # remove symbol
  names(quotes) <- c('Open', 'High', 'Low', 'Close', 'Volume')
  storage.mode(quotes) <- "numeric"

  return(xts(quotes))
}

# Load symbol / interval from historical data
load_hist_symbol <- function(symbol, interval){
  period <- specified_period()
  period_interval <- specified_period_interval()
  fle    <- qq("@{HIST_DIR}/@{interval}/@{period_interval}/@{symbol}_@{period}.csv")
  tryCatch({
    quotes <- read.zoo(fle,
                       header=FALSE,
                       sep=",",
                       index.column=2,
                       format="%Y%m%d%H%M",
                       tz='')
  }, error = function(err){
    # TODO callback to fetch symbols...
    print(err)
    stop(qq("Could not load @{symbol}/@{interval}/@{period})"))
  })

  # XXX
  quotes <- quotes[,-1] # remove symbol
  names(quotes) <- c('Open', 'High', 'Low', 'Close', 'Volume')
  storage.mode(quotes) <- "numeric"

  # TODO extrapolate if option specified
  return(xts(quotes))
}

# Load symbol / interval from default source
load_symbol <- function(symbol, interval){
  src = specified_source()
  if(src == "cache"){
    return(load_cache_symbol(symbol, interval))

  }else if(src == "hist"){
    return(load_hist_symbol(symbol, interval))
  }
}

# Load default symbol/interval for category.
# If DEFAULT_INTERVALS is list for category, source and
# extrapolation intervals will be inferred
load_default_symbol <- function(categ){
  symbol   <- specified_symbol()
  interval <- specified_interval(categ)

  if(class(interval) == "list"){
    quotes <- load_symbol(symbol, interval[["source"]])

  }else{
    quotes <- load_symbol(symbol, interval)
  }

  if(!is.na(specified_extrapolate(categ))){
    quotes <- to.period(quotes, period=specified_extrapolate(categ))
  }

  return(quotes)
}


# Extrapolate quotes to less granular level n times
extrapolate_quotes <- function(quotes, times=1){
  p  <- unclass(periodicity(quotes))$units
  ex <- to.period(quotes, HIGHER_PERIODS[[p]])

  t  <- (times - 1)
  if(t == 0){
    return(ex)
  }else{
    return(extrapolate_quotes(ex, t))
  }
}

# Return Positive Crossover Vector
# Crossovers are calculated by computing the lag of the given vector and signal vector (if not a fixed numeric)
# If signs of rows / lag rows change, we have a crossover
pos_crossovers <- function(src, signal){
  if("numeric" %in% class(signal)){
    lag_signal = signal;
  }else{
    lag_signal = Lag(signal);
  }

  return(ifelse(src > signal & Lag(src) < lag_signal, TRUE, FALSE))
}

# Return Negative Crossover Vector
# See pos_crossovers_above
neg_crossovers <- function(src, signal){
  if("numeric" %in% class(signal)){
    lag_signal = signal;
  }else{
    lag_signal = Lag(signal);
  }

  return(ifelse(src < signal & Lag(src) > lag_signal, TRUE, FALSE))
}

# Return an obj w/ the complete Lag set for each row in obj
complete_lag <- function(obj){
  return(Lag(obj, c(1:nrow(obj))))
}

# Return an obj w/ the complete Next set for each row in obj
complete_next <- function(obj){
  return(Next(obj, c(1:nrow(obj))))
}

###

# Write output to dir
write_out <- function(categ, data, desc=NA){
  sym      <- specified_symbol()
  interval <- specified_interval(categ)
  extrap   <- specified_extrapolate(categ)

  if(specified_source() == "hist"){
    period <- paste("-", specified_period(), sep="")
  }else{
    period <- ""
  }

  if(!is.na(extrap)){
    interval <- extrap
  }

  if(!is.na(desc)){
    desc <- paste("-", desc, sep="")
  }else{
    desc <- ""
  }

  file     <- qq("@{OUTPUT_DIR}/@{sym}-@{interval}@{period}-@{categ}@{desc}")
  write.zoo(data, file=file, sep=",")
}

# Write list to output dir, desc will be generated from names
write_list_out <- function(categ, data){
  quiet <- lapply(names(data), function(d){ write_out(categ, data[[d]], d) })
}
