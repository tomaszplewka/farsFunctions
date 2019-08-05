# More Details

The package is able to summarize an accident data within a certain time range with `fars_summarize_years` function as well as create a map of accidents in a particular state with `fars_map_state` function. Additionally, the package consists of a few auxiliary functions.

The following code chunk presents the package perfomance using both aformentioned functions based on the datasets provided with the package:

```{r setup}
library(farsFunctions)
# prints out traffic accidents data from 2013 and 2014 in the form of tibble
fars_summarize_years(c(2013, 2014))

# creates a map of accidents in a particular state
fars_map_state(1, 2013)
```
