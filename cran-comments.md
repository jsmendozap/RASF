## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* asf_download: no visible binding for global variable ‘ID’
  Undefined global functions or variables:
    ID
    
I create ID column in data frame for use later in that function


Also, in rhub I got this note: 

* Namespace in Imports field not imported from: 'readr'
    All declared Imports should be used.
    
For the example in asf_search function, is necessary to load the package or an error will be presented when CMD check run
