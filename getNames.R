
countries <- read.csv2(file = "countries.csv", sep = ",", stringsAsFactors = FALSE)
commodities <- read.csv2(file = "commodities.csv", sep = ",", stringsAsFactors = FALSE)

getCountryName <- function(m49) {
    filter(countries,code==m49)$description_en
}

getCommodityName <- function(cpc) {
    filter(commodities,code==cpc)$description_en
}