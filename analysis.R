
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

list.files("../")
dat = readRDS("../n2n_test_data.rds") %>% 
  as.data.frame() 

head(dat)

str(dat)

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

# want to calculate CACE: https://en.wikipedia.org/wiki/Local_average_treatment_effect

# are assignment ids completely unique?
dat %>% 
  count(assignment_id) %>% 
  count(n)

table(dat$treatment, dat$canvass_result)
# majority of treatment group coded as "NOT_VISITED"

dat %>% 
  filter(treatment == 0) %>% 
  pull(voted) %>% 
  mean() # 0.746

dat %>% 
  filter(treatment == 1,
         canvass_result == "NOT_VISITED") %>% 
  pull(voted) %>% 
  mean() # 0.749 

# ok, sanity check: same pct of "not visited" voted in treatment and control

dat %>% 
  filter(treatment == 1,
         canvass_result == "NOT_HOME") %>% 
  pull(voted) %>% 
  mean() # 0.716, slightly lower

dat %>% 
  filter(treatment == 1,
         canvass_result == "OPPOSITION_VOTER") %>% 
  pull(voted) %>% 
  mean() # 0.670, hah

dat %>% 
  filter(treatment == 1,
         canvass_result == "SPOKE_TO") %>% 
  pull(voted) %>% 
  mean() # 0.788

  