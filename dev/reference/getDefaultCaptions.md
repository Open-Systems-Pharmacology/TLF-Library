# getDefaultCaptions

Creates default legend captions by concatenating the values of the
`data` and `metaData` of `variableList` variables from `data`.

## Usage

``` r
getDefaultCaptions(
  data,
  metaData = NULL,
  variableList = colnames(data),
  sep = "-"
)
```

## Arguments

- data:

  data.frame used for legend caption

- metaData:

  list of lists containing metaData on `data`

- variableList:

  ordered vector of variables used for specifying the caption

- sep:

  characters separating variables in caption

## Value

Factor levels corresponding to the legend captions

## Examples

``` r

data <- data.frame(
  Population = c("Caucasian", "Asian", "Caucasian", "Asian"),
  Gender = c("Male", "Male", "Female", "Female"),
  Dose = c(50, 100, 100, 50),
  Compound = c("Midazolam", "Midazolam", "Midazolam", "Midazolam")
)

metaData <- list(Dose = list(unit = "mg"))

# Get captions using every variable of data
getDefaultCaptions(data, metaData)
#> [1] "Caucasian-Male-50 [mg]-Midazolam"    "Asian-Male-100 [mg]-Midazolam"      
#> [3] "Caucasian-Female-100 [mg]-Midazolam" "Asian-Female-50 [mg]-Midazolam"     

# Get captions using specific variables of data
getDefaultCaptions(data, metaData, variableList = c("Gender", "Population"))
#> [1] "Male-Caucasian"   "Male-Asian"       "Female-Caucasian" "Female-Asian"    

# Get captions separating variables witha space (character " ")
getDefaultCaptions(data, metaData, sep = " ")
#> [1] "Caucasian Male 50 [mg] Midazolam"    "Asian Male 100 [mg] Midazolam"      
#> [3] "Caucasian Female 100 [mg] Midazolam" "Asian Female 50 [mg] Midazolam"     
```
