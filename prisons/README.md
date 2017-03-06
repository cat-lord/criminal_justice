### Prison Estate in England & Wales

These files derive from the National Offender Management Service's (NOMS) list of [Prisons and their resettlement providers](
https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/596255/Breakdown_List_of_the_Prison_Estate_and_CPAs.pdf) (27/02/2017). 

The prison addresses were batch geocoded using Google Maps with the [ggmap](https://cran.r-project.org/web/packages/ggmap/index.html) R package. For reference the R code used was:

```r
library(ggmap)
geo <- geocode(location = df$Postal.Address, output="latlon")
df$lon <- geo$lon
df$lat <- geo$lat
```






