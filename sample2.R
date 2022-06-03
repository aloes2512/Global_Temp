url_CERES<-"https://climatedataguide.ucar.edu/climate-data/ceres-ebaf-clouds-and-earths-radiant-energy-systems-ceres-energy-balanced-and-filled?qt-climatedatasetmaintabs=4"#qt-climatedatasetmaintabs"
tmp_filename <- tempfile() 
download.file(url_CERES, tmp_filename) 


require(tidyverse)
tmp_filename%>%read.csv()
browseURL("https://ceres.larc.nasa.gov/documents/TSIdata/CERES_EBAF_Ed2.8_DailyTSI.txt")
