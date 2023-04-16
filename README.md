# DaCODeBED R-Package
DaCODeBED (**D**ata **C**orrection using **O**utlier **De**tection and **B**ack-Distribution for **E**pidemiological **D**ata) is an R-Package that performs corrections on epidemiological time series data. It removes data dumps which occur during the reporting of outbreaks and back distributes the extra reported numbers over a given time frame to aid with infectious disease outbreak management efforts like disease projections and intervention and scenario planning using SEIR models.

## Functions
DaCODeBED has two usable functions – **outlier_detection()** and **death_distribution()**.

### outlier_detection()
This function takes two input arguments:

* _date_ - a column of dates
* _incidD_ - the number of reported data on that day.

Both the arguments should be of the same length. It returns a vector of 1’s and 0’s with length equal to the input arguments. Each element represents if the reported deaths on a given date is a data dump (1) or not (0).

### death_distribution()
This function takes three input arguments:

* _date_ - a column of dates
* _incidD_ - the number of reported data on that day
* _pois_out_ - a column of 1’s and 0’s indicating if each reported data is an outlier or not.

The function also takes optional arguments depending upon the method of back distribution to be used:

* _use_cases_ - a Boolean variable (default = FALSE) which indicates if additional data (for example cases if reported data is deaths) is provided to determine the back distribution ratio. If FALSE, the back distribution will take place using the original reported data (incidD).
* _incidI_ - the additional data which must be provided if use_cases is TRUE to determine the back distribution ratio.
* _cases_lag_ - this is the time period by which the supporting data is ahead of the target data in weeks. It defaults to 3 weeks.
* _support_ - a Boolean variable (default =FALSE) which indicates if additional data is provided to help with outlier identification, informing about the extra reported data (if present), and the date till which the extra deaths must be distributed (if present).
* _out_sup_ - if support is TRUE, it is a column of 1’s and 0’s indicating if each reported data is an outlier or not according to the supplementary data set provided.
* _diff_ - if support is TRUE, it is a column which indicates the number of extra numbers reported on that day according to the supplementary data set provided. If this is not provided but just an outlier is indicated by out_sup the extra reports are calculated on its own. For outlier points where extra reports are not mentioned and the non-outlier points the value of diff should be zero.
* _low_date_ - if support is TRUE, it is a column with dates which indicate the date till which the extra reports calculated using diff or by the function (if diff is not present for that given point) should be distributed to. If the dates are not mentioned the date to be back distributed will be calculated automatically. For outlier points where low_date is not present and non-outlier points the value should be NA.
