dR - Expiremental financial analysis and signals using quantmod.
---

Each script in this directory contains a market analysis target which can be invoked using R / RScript:

	$ Rscript volatility.R
				tr          atr trueHigh trueLow tr_atr_ratio is_high is_low atr_close_ratio is_sideways is_rising is_falling
	2017-07-26 06:40:00     NA           NA       NA      NA           NA       0      0              NA           0         0          0
	2017-07-26 08:30:00 0.0002           NA   1.0646  1.0644           NA       0      0              NA           0         0          0
	2017-07-26 08:35:00 0.0002           NA   1.0647  1.0645           NA       0      0              NA           0         0          0
	2017-07-26 08:40:00 0.0002           NA   1.0648  1.0646           NA       0      0              NA           0         0          0
	2017-07-26 08:45:00 0.0003 0.0002360000   1.0650  1.0647    1.2711864       0      0    0.0002217003           0         0          0
	2017-07-26 09:10:00 0.0000 0.0002036408   1.0644  1.0644    0.0000000       0      1    0.0001913198           1         0          1
	2017-07-26 09:15:00 0.0001 0.0001994951   1.0644  1.0643    0.5012654       0      0    0.0001874250           1         0          1
	2017-07-26 09:20:00 0.0001 0.0001955153   1.0644  1.0643    0.5114689       0      0    0.0001836859           1         0          1
	2017-07-26 09:25:00 0.0001 0.0001916947   1.0644  1.0643    0.5216628       0      0    0.0001800627           1         0          1
....


	$ R
	> source("momentum.R")

...


Default symbols / lookup intervals / data directories are specified in common.R though may be overridden
via the command line. See -h for more info:

	$ Rscript trend.R -h
	Usage: trend.R [options]

	Options:
		--source=SOURCE
			Source of Data to Scan

		-s SYMBOL, --symbol=SYMBOL
			Symbol to Scan

		-i INTERVAL, --interval=INTERVAL
			Interval to Load

		-e EXTRAPOLATE, --extrapolate=EXTRAPOLATE
			Interval to Extrapolate To

		-y YEAR, --year=YEAR
			Historical Year to load

		-m MONTH, --month=MONTH
			Historical Month to load

		-d DAY, --day=DAY
			Historical Day to load

		-h, --help
			Show this help message and exit

----

Copyright (C) 2017 - Mo Morsi <mo@morsi.org>

Licensed under the MIT License
