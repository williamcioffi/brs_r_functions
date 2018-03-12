# brs_r_functions
r functions for processing and manipulating wildlife computer tag output

## basic_tag_functions
+ cattag.r 
⋅⋅- concatenates wc portal downloaded data into single csv files for each data stream.
⋅⋅- still some bugs with some file formats in older tags

+ censor_dives.R
⋅⋅- removes shallow and or short dives to make a sensored behavior record
⋅⋅- needs more testing

+ findgaps.R
⋅⋅- looks for data gaps given a tolerance in seconds

+ load.R
⋅⋅- loads output of cattag into a list for the following data streams:
⋅⋅⋅⋅- behavior
⋅⋅⋅⋅- corrupt
⋅⋅⋅⋅- status
⋅⋅⋅⋅- all
⋅⋅⋅⋅- argos
⋅⋅⋅⋅- locations
⋅⋅⋅⋅- series (currently disabled)
⋅⋅- trys to reformat times to unambiguous YYYY-MM-DD HH:MM:SS

+ plot_dives
⋅⋅- trys to plot behavior records in a reasonable way with reasonable time labels

+ plot_status_corrupt.R
⋅⋅- plot_status
⋅⋅⋅⋅- plot a time series of status messages given a column
⋅⋅⋅⋅- needs updates
⋅⋅- plot_corrupt
⋅⋅⋅⋅- plots corrupt messages over time
⋅⋅⋅⋅- needs updates

## bsam
+ run_bsam.R
⋅⋅- template for running bsam model on splash data

## cleaning_error_det
+ clean_tags.r
⋅⋅- make some plots looking at status messages esp for depth
+ diagnostic_plits_20180311
⋅⋅- similar idea plus some other plots

## compare_dives
+ build_null_diver_vectorized_nonnormal.R
+ build_null_diver_vectorized.R
+ build_null_diver.R
⋅⋅- different versions of a function to make a random sequence of dives
⋅⋅- does not take temporal autocorrelation into account
⋅⋅- use with caution esp on non censored data
+ compare_dives_new.r
⋅⋅- look for synchronous dives and surfacing events
+ compare_dives.R
⋅⋅- old version, don't use, for backwards compatibility
+ experiments

## crawl

## douglas_filter
+ douglas.r
⋅⋅- douglas parameters
+ douglas_filter_test.r
+ douglasfunction.r
⋅⋅- does not currently reproduce the douglas filter


