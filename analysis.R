
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

list.files("../")
dat = readRDS("../n2n_test_data.rds") %>% 
  as.data.frame() 

head(dat)

# ● Voter ID
# ● Canvasser ID
# ● Assignment ID
# ● Household ID
# ● Treatment (1 for treatment, 0 for control)
# ● Voted (1 for voted, 0 for did not vote)
# ● Canvass result (control, not attempted, not home, canvassed)
# ● Turnout score
# ● Partisanship score
# ● Age
# ● Race
# ● State
# ● Urbanicity (rural, suburban, urban)
