
# ================ load initial packages and data ==================

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

list.files("../")
dat = readRDS("../n2n_test_data.rds") %>% 
  as.data.frame() 

head(dat)

str(dat)

dat = 
  dat %>% 
  rename(state = stae)

sapply(dat, function(x) sum(is.na(x))) # 1400 NA ages, no other NAs, we'll come back to that

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



# ================ initial exploratory analysis =====================

# are assignment ids completely unique?
dat %>% 
  count(assignment_id) %>% 
  count(n) # nope!

# are voters represented only once in dataset?
dat %>% 
  count(voter_id) %>% 
  count(n) # yep!

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

# raw pct voting in each group
dat %>% 
  mutate(treat_canv_res = paste(treatment, canvass_result, sep = "_")) %>% 
  group_by(treat_canv_res) %>% 
  summarize(n = n(),
            pct_voting = mean(voted))

# ok, sanity check: same pct of "not visited" voted in treatment and control conditions

head(dat)

## ------------- effect of age ------------------

# what's up with NA aged individuals?
dat %>% filter(is.na(age)) %>% count(treatment, canvass_result, voted)
# seem to be voting at a lower rate than the overall sample, without more knowledge 
# of the sample I'll exclude

dat %>% 
  mutate(age = round(age)) %>% 
  group_by(age) %>% 
  summarize(n = n(),
            pct_vote = mean(voted)) %>% 
  ggplot(aes(x = age, y = pct_vote, size = n)) + 
  geom_point() + 
  scale_size_continuous()

sort(unique(round(dat$age))) 
# 16 and 17 year olds (who unsurprisingly have voting rates at or near 0)
# also some suspiciously old ages here, probably should truncate

dat %>% 
  filter(age < 18) %>% 
  group_by(round(age)) %>% 
  summarize(n = n(),
            pct_vote = mean(voted),
            n_voted = sum(voted), 
            n_treatment = sum(treatment),
            n_treatment_voted = sum(treatment*voted))

# ok 4 total voters under 18 (presumably coding error or 
# turned 18 between canvassing and election).  Simplest to just 
# exclude kids under 18  

dat %>% 
  mutate(age = round(age)) %>% 
  group_by(age) %>% 
  summarize(n = n(),
            pct_vote = mean(voted)) %>% 
  as.data.frame() %>% 
  tail(25)

# explore different thresholds for exclusion, but maybe use 90 as the cutoff for now?

# generally, though, this looks to be a quadratic relationship between age and pct voted, include in model

## ----------------- effect of race: -----------------------------

dat %>% 
  group_by(race) %>% 
  summarize(n = n(),
            pct_vote = mean(voted))
# very very few native voters in sample unfortunately
# hm, some NAs, change coding to UNK so NA isn't dropped in analysis


dat = 
  dat %>% 
  mutate(race = replace_na(race, "UNK"))

dat %>% filter(race == "NATIVE") %>% count(treatment)


## -------------- what's the relationship between partisan score and turnout score? --------

dat %>% 
  count(partisan_score < 60)

dat %>% 
  filter(partisan_score < 60) %>% 
  count(treatment, canvass_result) 

dat %>% 
  ggplot(aes(x = partisan_score, y = turnout_score, color = factor(treatment))) + 
  geom_hex(bins = 100) + 
  geom_vline(xintercept = 60)

dat %>% 
  ggplot(aes(x = partisan_score)) + 
  geom_bar(aes(fill = factor(partisan_score > 62))) + 
  scale_y_log10()

# 60 or so seems to be the cutoff for partisan scores here, there are only 122 voters with 
# partisan scores below 60

# check on this in analysis data (see below for analysis data creation):
adat %>% ggplot(aes(x = partisan_score, y = turnout_score, color = factor(treatment))) + geom_point()

adat %>% 
  filter(partisan_score < 60) %>% 
  count(treatment) 
# ok only 5 people in smaller sample with partisan score < 60 who were assigned to treatment and were spoken to
# so to help with model convergence and distribution of predictors we'll exclude tiny subsample of 
# voters who don't like dems.  

## ------------ does volunteer enthusiasm matter? -------------------

head(dat)

dat %>% 
  group_by(canvasser_id) %>% 
  summarize(n_assignment_ids = length(unique(assignment_id)),
            n_voters = n()) %>% 
  ggplot(aes(x = n_assignment_ids, y = n_voters)) + geom_point()

# nice linear relationship, ok

dat %>% 
  group_by(canvasser_id) %>% 
  mutate(n_assignment_ids = length(unique(assignment_id)),
            n_voters = n()) %>% 
  ungroup() %>% 
  filter(canvass_result == "SPOKE_TO") %>% 
  group_by(n_assignment_ids) %>% 
  summarize(pct_vote = mean(voted),
            n = n()) %>% 
  ggplot(aes(x = n_assignment_ids, y = pct_vote)) + 
  geom_point(aes(color = factor(n_assignment_ids == 1), size = n)) + scale_size_continuous() + 
  geom_smooth()
  
dat %>% 
  group_by(canvasser_id) %>% 
  mutate(mult_assign_ids = length(unique(assignment_id)) > 1) %>% 
  ungroup() %>% 
  filter(canvass_result == "SPOKE_TO") %>% 
  group_by(mult_assign_ids) %>% 
  summarize(pct_vote = mean(voted),
            n = n()) %>% 
  ggplot(aes(x = factor(mult_assign_ids), y = pct_vote, size = n)) + 
  geom_point()

# hm so the most enthusiastic volunteers may not be the most effective

## ------------ household effects? ---------------

dat %>% count(household_id) %>% count(n) # ok a bunch of households include more than one voter in the sample

dat %>% 
  group_by(household_id) %>% 
  summarize(household_size = n(),
         n_contacted = sum(canvass_result == "SPOKE_TO")) %>% 
  ungroup() %>% 
  filter(household_size > 1) %>% 
  count(household_size, n_contacted)

# ok, if anyone is contacted in the household, the whole household is considered "spoken_to"
# therefore, no need to add a term for "someone in household spoken to" for some that are coded otherwise


# ===================== analysis data creation ==================

# simplest analysis: just compare treatment == 1 and canvass_result == spoke_to
# to control households.  Then switch comparison group.

adat = 
  dat %>% 
  mutate(z_age = scale(age), # scaling these terms to help with model convergence
         z_turnout_score = scale(turnout_score),
         z_partisan_score = scale(partisan_score)) %>% 
  group_by(canvasser_id) %>% 
  mutate(n_assignment_ids = length(unique(assignment_id))) %>% 
  ungroup() %>% 
  mutate(mult_assign_ids = n_assignment_ids > 1) %>% 
  filter(age >= 18,
         age <= 91, # also will silently drop NA ages
         partisan_score > 60, # excluding small number of voters with low partisanship scores (only 5 were assigned to treatment)
         !(treatment == 1 & canvass_result == "NOT_VISITED"), # excluding this group for now
         canvass_result != "OPPOSITION_VOTER", # very few of these in sample
         !(treatment == 1 & canvass_result == "NOT_HOME")) %>%  # excluding for now, worth comparing 
  mutate(canvass_result2 = factor(canvass_result),
         race2 = factor(race),
         urbanicity2 = factor(urbanicity))

dim(adat)

sapply(adat, function(x) sum(is.na(x)))

# check for differences in distribution of predictors between groups: 

adat %>% ggplot(aes(x = factor(canvass_result), y = turnout_score)) + geom_violin(draw_quantiles = 0.5)
# turnout score looks decent

adat %>% ggplot(aes(x = factor(canvass_result), y = log(101 - partisan_score))) + geom_violin(draw_quantiles = 0.5)
# partisanship score looks decent

## ------------- initial model -----------------------------

library(lmerTest)

m1 = 
  glmer(voted ~ 
          # treatment
        canvass_result2 
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat
      , family = "binomial", na.action = na.fail)

summary(m1)

performance::check_collinearity(m1) # nice, no concern with multicollinearity
plot(DHARMa::simulateResiduals(m1)) # model assumptions look pretty good

## -------------- add canvasser enthusiasm? ---------------------------

m2 = 
  glmer(voted ~ 
          # treatment
          canvass_result2 
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + scale(n_assignment_ids) 
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat
        , family = "binomial", na.action = na.fail)

AICc(m1, m2) # fit is much improved

summary(m2)

performance::check_collinearity(m2) # nice, no concern with multicollinearity
plot(DHARMa::simulateResiduals(m2)) # model assumptions look pretty good

## -------------- interaction between canvass_result and race? -----------------

adat %>% group_by(race, canvass_result) %>% summarize(n = n(), pct_voted = mean(voted))

adat %>% 
  group_by(race, canvass_result) %>% 
  summarize(n = n(), 
            pct_voted = mean(voted)) %>% 
  ggplot(aes(x = factor(race), y = pct_voted, color = canvass_result, size = n)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10))
# only 4 native voters were successfully spoken to so we can't take anything away from that

# hm kinda weird that race == UNK voters voted at lower rates if they were successfully canvassed
# I'd have to understand the coding of race in the sample better to speculate why...

# also the effect of canvassing seems muted for AFAM voters.  Would be interesting to have 
# information on the race of canvassers to see if same race canvassers might have better 
# results...

# so does including this interaction improve model fit?

m3 = 
  glmer(voted ~ 
          # treatment
          canvass_result * race
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity
        + scale(n_assignment_ids)
        # + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat
        , family = "binomial", na.action = na.fail)

AICc(m2, m3) 
# essentially identical fit, hard to interpret results so sticking with more parsimonious model

## -------------- interaction between canvass_result and urbanicity? -----------------

adat %>% group_by(urbanicity, canvass_result) %>% summarize(n = n(), pct_voted = mean(voted))

adat %>% 
  group_by(urbanicity, canvass_result) %>% 
  summarize(n = n(), 
            pct_voted = mean(voted)) %>% 
  ggplot(aes(x = factor(urbanicity), y = pct_voted, color = canvass_result, size = n)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10))
# interesting, effect of canvassing strongest in rural areas

# so does including this interaction improve model fit?

m3 = 
  glmer(voted ~ 
          # treatment
          canvass_result * urbanicity
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        # + urbanicity
        + scale(n_assignment_ids)
        + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat
        , family = "binomial", na.action = na.fail)

AICc(m2, m3) 
# fit is worse, exclude

## --------------- compare this model to "null" model without canvass term ----------

m3 = 
  glmer(voted ~ 
          # treatment
          # canvass_result2 
        z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + n_assignment_ids 
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat
        , family = "binomial", na.action = na.fail)

AICc(m2, m3) # fit is considerably better if you include the canvass result term!

## --------------- plot predicted effects --------------------

library(sjPlot)

plot_model(m2)

plot_model(m2, type = "pred", terms = "canvass_result2")
# so we're seeing about a 2.5 percentage point increase in voting for 
# voters that were successfully contacted vs those that were in the control group

plot_model(m2, type = "pred", terms = "z_age [all]")
# 

plot_model(m2, type = "pred", terms = "race2") 
# hmmm, weird that model predicted voting rate for unk is so high, see below

plot_model(m2, type = "pred", terms = "n_assignment_ids [all]")
# weird, interesting that canvassers with more assignments were less successful!

plot_model(m2, type = "pred", terms = "z_turnout_score [all]")
plot_model(m2, type = "pred", terms = "z_partisan_score [all]")

plot_model(m2, type = "pred", terms = "urbanicity2")

# ================ second dive into data exploration =================

## ---------------- ok what's going on with race == "UNK"? -------------

# based on model m1, race == UNK voters are predicted to be voting at the highest
# rate of any race, all else equal.  But based on raw data, they vote at one 
# of the lowest rates.  What gives?

# possibility 1: could it be confounded by state (e.g. if race == unk voters are 
# overrepresented in certain states that happen to vote at low rates)?
adat %>% 
  group_by(state) %>% 
  summarize(n = n(),
            n_unk = sum(race == "UNK"),
            pct_unk = mean(race == "UNK"),
            pct_voted = mean(voted)) %>% 
  arrange(pct_unk) %>% 
  as.data.frame()

adat %>% 
  group_by(state) %>% 
  summarize(n = n(),
            n_unk = sum(race == "UNK"),
            pct_unk = mean(race == "UNK"),
            pct_voted = mean(voted)) %>% 
  arrange(pct_unk) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = pct_unk, y = pct_voted)) +
  geom_point(aes(size = n_unk)) + scale_size_continuous() + 
  geom_smooth()

adat %>% 
  group_by(state) %>% 
  summarize(n = n(),
            n_unk = sum(race == "UNK"),
            pct_unk = mean(race == "UNK"),
            pct_voted = mean(voted)) %>% 
  arrange(pct_unk) %>% 
  filter(n_unk > median(n_unk)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = pct_unk, y = pct_voted)) +
  geom_point(aes(size = n_unk)) + scale_size_continuous() + 
  geom_smooth(linewidth = 1)

# no relationship here between pct unk and pct voted, so this effect 
# not seemingly confounded by state

# option 2: what about age?
adat %>% 
  ggplot(aes(x = factor(race), y = age)) + 
  geom_violin(draw_quantiles = 0.5)

# so maybe because the race == UNK group skews younger (with the youngest voters especially 
# highly represented), models are predicting race == UNK to be voting at higher rates after 
# accounting for the lower rates of voting among the youngest voters?

adat %>% 
  mutate(age = round(age),
         age_3yr = ((age + 2) %/% 3)*3) %>% # binning into 3-year age bins
  filter(race != "NATIVE") %>% # excluding natives because we have so little data on that group
  group_by(age_3yr, race) %>% 
  summarize(n = n(),
            pct_vote = mean(voted)) %>% 
  filter(n > 10, # exclude especially small samples
         age_3yr < 81) %>% # exclude oldest voters for ease of viewing
  ggplot(aes(x = age_3yr, y = pct_vote, color = factor(race))) + 
  geom_point(aes(size = n), alpha = 0.5) + 
  geom_smooth(se = F) + 
  scale_size_continuous()

# hmmm so race == UNK skews disproportionately young, and young UNK race voters voted at high rates 
# relative to their age peers!  Interesting, and may account for the disparity between 
# the raw data and the model predictions

# ===================== repeat analysis with treatment == 1 and canvass_result == "NOT_VISITED" ===========

adat2 = 
  dat %>% 
  mutate(z_age = scale(age),
         z_turnout_score = scale(turnout_score),
         z_partisan_score = scale(partisan_score)) %>% 
  group_by(canvasser_id) %>% 
  mutate(n_assignment_ids = length(unique(assignment_id))) %>% 
  ungroup() %>% 
  mutate(mult_assign_ids = n_assignment_ids > 1) %>% 
  filter(age >= 18,
         partisan_score > 60, # excluding small number of voters with low partisanship scores (only 5 were assigned to treatment)
         !(treatment == 0 & canvass_result == "NOT_VISITED"), # excluding this group for now
         canvass_result != "OPPOSITION_VOTER", # very few of these in sample
         !(treatment == 1 & canvass_result == "NOT_HOME")) %>%  # excluding for now, worth comparing 
  mutate(canvass_result2 = factor(canvass_result),
         race2 = factor(race),
         urbanicity2 = factor(urbanicity))

dim(adat2)

sapply(adat2, function(x) sum(is.na(x)))

# check for differences in distribution of predictors between groups: 

adat2 %>% ggplot(aes(x = factor(canvass_result), y = turnout_score)) + geom_violin(draw_quantiles = 0.5)
# turnout score looks decent

adat2 %>% ggplot(aes(x = factor(canvass_result), y = log(101 - partisan_score))) + geom_violin(draw_quantiles = 0.5)
# partisanship score looks decent

## ------------- initial model -----------------------------


mm1 = 
  glmer(voted ~ 
          # treatment
          canvass_result 
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity
        + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat2
        , family = "binomial", na.action = na.fail)

summary(mm1)

performance::check_collinearity(mm1) # again no concern with multicollinearity
plot(DHARMa::simulateResiduals(mm1)) 
# Kolmogorov-Smirnov test significant but visual inspection suggests little cause for alarm

## -------------- add canvasser enthusiasm? ---------------------------

mm2 = 
  glmer(voted ~ 
          # treatment
          canvass_result2 
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + n_assignment_ids 
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat2
        , family = "binomial", na.action = na.fail)

AICc(mm1, mm2) # n_assignment ids still an important predictor!

summary(mm2)

performance::check_collinearity(mm2) # still no concern with multicollinearity
plot(DHARMa::simulateResiduals(mm2)) 
# as before, KS test significant but visual inspection suggests little cause for alarm

## -------------- interaction between canvass_result and race? -----------------

adat2 %>% group_by(race, canvass_result) %>% summarize(n = n(), pct_voted = mean(voted))

adat2 %>% 
  group_by(race, canvass_result) %>% 
  summarize(n = n(), 
            pct_voted = mean(voted)) %>% 
  ggplot(aes(x = factor(race), y = pct_voted, color = canvass_result, size = n)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10))
# still only 4 native voters successfully spoken to

# still odd that race == UNK voters voted at lower rates if they were successfully canvassed
# I'd have to understand the coding of race in the sample better to speculate why...

# as before, the effect of canvassing seems muted for AFAM voters.  Would be interesting to have 
# information on the race of canvassers to see if same race canvassers might have better 
# results...

# so does including this interaction improve model fit?

mm3 = 
  glmer(voted ~ 
          # treatment
          canvass_result * race
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity
        + n_assignment_ids
        # + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat2
        , family = "binomial", na.action = na.fail)

AICc(mm2, mm3) 
# essentially identical fit, hard to interpret results so sticking with more parsimonious model

## -------------- interaction between canvass_result and urbanicity? -----------------

adat2 %>% group_by(urbanicity, canvass_result) %>% summarize(n = n(), pct_voted = mean(voted))

adat2 %>% 
  group_by(urbanicity, canvass_result) %>% 
  summarize(n = n(), 
            pct_voted = mean(voted)) %>% 
  ggplot(aes(x = factor(urbanicity), y = pct_voted, color = canvass_result, size = n)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10))
# effect of canvassing again strongest in rural areas

# so does including this interaction improve model fit?

mm3 = 
  glmer(voted ~ 
          # treatment
          canvass_result * urbanicity
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        # + urbanicity
        + n_assignment_ids
        + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat2
        , family = "binomial", na.action = na.fail)

AICc(mm2, mm3) 
# fit is worse, exclude

## -------------- compare this model to "null" model without canvass term ----------

mm3 = 
  glmer(voted ~ 
          # treatment
          # canvass_result2 
        z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + n_assignment_ids 
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat2
        , family = "binomial", na.action = na.fail)

AICc(mm2, mm3) 
# ok fit is considerably better with the canvass_result2 term included!



## --------------- plot predicted effects -----------------------

library(sjPlot)

plot_model(mm2)

plot_model(mm2, type = "pred", terms = "canvass_result2")

plot_model(mm2, type = "pred", terms = "z_age [all]")

plot_model(mm2, type = "pred", terms = "race2") 
# same surprising UNK effect

plot_model(mm2, type = "pred", terms = "n_assignment_ids [all]")
# weird!

plot_model(mm2, type = "pred", terms = "z_turnout_score [all]")
plot_model(mm2, type = "pred", terms = "z_partisan_score [all]")

plot_model(mm2, type = "pred", terms = "urbanicity2")

# effects largely identical to original contrast group


# ===================== repeat analysis with treatment == 1 and canvass_result == "NOT_HOME" ===========

adat3 = 
  dat %>% 
  mutate(z_age = scale(age),
         z_turnout_score = scale(turnout_score),
         z_partisan_score = scale(partisan_score)) %>% 
  group_by(canvasser_id) %>% 
  mutate(n_assignment_ids = length(unique(assignment_id))) %>% 
  ungroup() %>% 
  mutate(mult_assign_ids = n_assignment_ids > 1) %>% 
  filter(age >= 18,
         partisan_score > 60, # excluding small number of voters with low partisanship scores (only 5 were assigned to treatment)
         canvass_result != "NOT_VISITED", # excluding this group for now
         canvass_result != "OPPOSITION_VOTER" # very few of these in sample
         ) %>%  # excluding for now, worth comparing 
  mutate(canvass_result2 = factor(canvass_result),
         race2 = factor(race),
         urbanicity2 = factor(urbanicity))

adat3 %>% count(treatment, canvass_result)

dim(adat3)

sapply(adat3, function(x) sum(is.na(x)))

# check for differences in distribution of predictors between groups: 

adat3 %>% ggplot(aes(x = factor(canvass_result), y = turnout_score)) + geom_violin(draw_quantiles = 0.5)
# turnout score slightly higher in spoke to group

adat3 %>% ggplot(aes(x = factor(canvass_result), y = log(101 - partisan_score))) + geom_violin(draw_quantiles = 0.5)
# partisanship score looks decent but the "not home" group slightly less favorably partisan

## ------------- initial model -----------------------------


mmm1 = 
  glmer(voted ~ 
          # treatment
          canvass_result 
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity
        + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat3
        , family = "binomial", na.action = na.fail)

summary(mmm1)

performance::check_collinearity(mmm1) # again no concern with multicollinearity
plot(DHARMa::simulateResiduals(mmm1)) 
# little cause for concern

## -------------- add canvasser enthusiasm? ---------------------------

mmm2 = 
  glmer(voted ~ 
          # treatment
          canvass_result2 
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + n_assignment_ids 
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat3
        , family = "binomial", na.action = na.fail)

AICc(mmm1, mmm2) # n_assignment_ids still an important predictor!

summary(mmm2)

performance::check_collinearity(mmm2) # still no concern with multicollinearity
plot(DHARMa::simulateResiduals(mmm2)) 
# as before, KS test significant but visual inspection suggests little cause for alarm

## -------------- interaction between canvass_result and race? -----------------

adat3 %>% group_by(race, canvass_result) %>% summarize(n = n(), pct_voted = mean(voted))

adat3 %>% 
  group_by(race, canvass_result) %>% 
  summarize(n = n(), 
            pct_voted = mean(voted)) %>% 
  ggplot(aes(x = factor(race), y = pct_voted, color = canvass_result, size = n)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10))
# still only 4 native voters successfully spoken to

# this time all but the Native group have better voting propensity if they were canvassed

# so does including this interaction improve model fit?

mmm3 = 
  glmer(voted ~ 
          # treatment
          canvass_result * race
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity
        + n_assignment_ids
        # + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat3
        , family = "binomial", na.action = na.fail)

AICc(mmm2, mmm3) 
# fit of mmm2 is better

## -------------- interaction between canvass_result and urbanicity? -----------------

adat3 %>% group_by(urbanicity, canvass_result) %>% summarize(n = n(), pct_voted = mean(voted))

adat3 %>% 
  group_by(urbanicity, canvass_result) %>% 
  summarize(n = n(), 
            pct_voted = mean(voted)) %>% 
  ggplot(aes(x = factor(urbanicity), y = pct_voted, color = canvass_result, size = n)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10))
# effect of canvassing again strongest in rural areas

# so does including this interaction improve model fit?

mmm3 = 
  glmer(voted ~ 
          # treatment
          canvass_result * urbanicity
        + z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        # + urbanicity
        + n_assignment_ids
        + race
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat3
        , family = "binomial", na.action = na.fail)

AICc(mmm2, mmm3) 
# fit is worse, exclude

## ---------------- compare this model to "null" model without canvass term ----------

mmm3 = 
  glmer(voted ~ 
          # treatment
          # canvass_result2 
        z_turnout_score
        + z_partisan_score
        + poly(z_age, 2)
        + urbanicity2
        + race2
        + n_assignment_ids 
        + (1|state)
        # + (1|urbanicity) # model fits better with urbanicity as main effect
        , data = adat3
        , family = "binomial", na.action = na.fail)

AICc(mmm2, mmm3) # again fit is considerably worse if you drop the canvass_result term



## ---------------- plot predicted effects ---------------------

library(sjPlot)

plot_model(mmm2)

plot_model(mmm2, type = "pred", terms = "canvass_result2")

plot_model(mmm2, type = "pred", terms = "z_age [all]")

plot_model(mmm2, type = "pred", terms = "race2") 
# same surprising UNK effect

plot_model(mmm2, type = "pred", terms = "n_assignment_ids [all]")
# weird!

plot_model(mmm2, type = "pred", terms = "z_turnout_score [all]")
plot_model(mmm2, type = "pred", terms = "z_partisan_score [all]")

plot_model(mmm2, type = "pred", terms = "urbanicity2")

# effects largely identical to original contrast group