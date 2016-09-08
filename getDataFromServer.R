
getDataFromServer <- function() {
    
    areaVar = "geographicAreaM49"
    yearVar = "timePointYears"
    itemVar = "measuredItemCPC"
    elementVar = "measuredElement"    
    
    key <- DatasetKey(
        domain = slot(swsContext.datasets[[1]],"domain"),
        dataset = slot(swsContext.datasets[[1]],"dataset"),
        dimensions = list(
            Dimension(name = areaVar, keys = country),
            Dimension(name = yearVar, keys = years),
            Dimension(name = itemVar, keys = commodity),
            Dimension(name = elementVar, keys = elements)
        )
    )
    
    productionPivot = c(
        Pivoting(code = areaVar, ascending = FALSE),
        Pivoting(code = itemVar, ascending = FALSE),
        Pivoting(code = yearVar, ascending = TRUE),
        Pivoting(code = elementVar, ascending = TRUE)
    )
    
    ## Query the data
    data = GetData(
        key = key,
        flags = TRUE,
        normalized = TRUE,
        pivoting = productionPivot
    )
    
    ## Convert time to numeric
    data[, timePointYears := as.numeric(timePointYears)]
    
    data
}
