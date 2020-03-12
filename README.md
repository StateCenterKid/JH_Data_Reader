# JH_Data_Reader
R Code for Reading and Correcting Johns Hopkins Covid Data

This is my attempt to read and clean the Johns Hopkins time series tables for the Covid-19 situation.

It pulls in the raw datafiles, cleans up the country/province names and gets rid of the duplicated data in the US.  I'm sure there are more problems i havent found yet.


-----------------------
R version 3.5.3 (2019-03-11)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     
