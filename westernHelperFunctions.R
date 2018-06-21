
#This function reads the raw data for both the antibody of interest (usually RpoS), and values to normalize by (usually RpoD or revert stain)
#normalizes, and also records other critical information to set up a tidy data table:

library(tidyverse)

normalizeSingleBlot <- function(normalizeValues, antibodyValues) {
  #order tables by lane
  
  #select normalize signal, keep two values, and rename one of them.
  
  normalizeData <- select(normalizeValues, Signal, Lane) %>%
    rename(normalize = Signal)  


    #select western signal
  
  blotData <- select(antibodyValues, -`Image Name`, -Channel, -Name, -Total, -Area, -Bkgnd., -Type,
                     -`Conc. Std.`, -Concentration) %>%
    rename(RpoS = Signal) %>%
    left_join(normalizeData, by="Lane") %>%
    mutate(RpoS/normalize) %>%
    rename(RpoSratio = `RpoS/normalize`)
  
  return(blotData)

}
