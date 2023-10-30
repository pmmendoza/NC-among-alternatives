# Title: Study I
# Context: [Hostile Campaigning Project]
# Author: Philipp M.
# Date: Mon Sep 06 19:55:32 2021
# 0. Content ---------------------------------------------------------
# 1. Loading Packages & Data
# 2. Recoding
# 3. Analyses
# 
# [R] for Robustness checks
# [SC] for Sample Cuts
# [T] for Table output
# [G] for Graph output
#
# "FIXME", "TODO", "CHANGED", "IDEA", "HACK", "NOTE", "REVIEW", "BUG", "QUESTION", "COMBAK", "TEMP"
# NOTE differences to the Nai 2022 paper
# - expert judgements are adjusted with the ABSOLUTE, not the relative ideological
#   distance between parties and experts.
# TODO remove the harshness measure from the data file for the final code.
# TODO create adjusted negativity and incivility measures for the respective robustness checks.
# DEBUG Unknown columns: `p_neg_adj_close_combo`, `p_inciv_adj_close_combo`, `d_combodistwiki` 

# 1. Loading Packages & Data -----------------------------------------
# Global VARS
# MAC
# DATADIR <- "/Users/p.m.mendozauva.nl/OneDrive - UvA/07 Datasets/"
# projdir <- "/Users/p.m.mendozauva.nl/Library/CloudStorage/OneDrive-UvA/00 Work/01 ASCoR PhD/01 Study I/Git first paper/"

## 1.1 Packages and functions ---------------------------------------------
# Loading / Installing packages depending on whether it is already installed.
pacman::p_load(
  tidyverse, 
  tidylog,
  inops,
  glue,
  beepr, 
  clipr,
  inops
  # lme4,
  # lmerTest,
  # broom.mixed,
  # marginaleffects,
  # extrafont,
  # modelsummary,
  # kableExtra,
  )

# This Script loads the following functions
# pltmymodel - Quick model plotting function => enter summary object!
# pltmymodels - for >1 models
# getvariances - Save the variance at the respective levels as table
source("00_utils.R")


## 1.2 Loading Data -------------------------------------------------------

# NOTE This code:
# 1. Loads both datasets
# 2. Harmonises Country labels
# 3. Ads the EES party identifiers and labels to the EPEES dataset
#    (ees_partyname_engl, ees_partyname_orig, ees_partycode, ees_Q25_code, ees_Q7_code, ees_Q7_code_short)
#    via a previously created linkage file
# 4. Renames the EPEES partyname to enx_partyname
# 5. Creates unique party ids (EPEES) and respondent ids (EES) 
#    {COUNTRY-ABBREV}_{EES-PARTY-ID}
#    {COUNTRY-ABBREV}_{EES-RESPONDENT-ID}
# 6. Removes parties from EPEES that cannot be linked to the EES
source("02_pre-processing-data-cleaning.R")

# Create dictionaries for both datasets
codebees <- ees %>% labelled::generate_dictionary()
codebenx <- enx %>% labelled::generate_dictionary()

enx <- enx %>% 
  mutate(
    ees_partycode = ees_partycode %>% paste0("party_",.),
    regionbe = ifelse (is.na(regionbe), "other", regionbe)
         )


# 2. Recoding -------------------------------------------------------------
## 2.1 Matching potential -------------------------------------------------
#  2.1.1 Parties that I cannot match due to EES
# temp <- enx %>%
#   filter(is.na(ees_partycode)) %>%
#   select(cntry, contains("party_"))
# 
# # 12 unmatched parties
# # 183 parties should be matchable! => now 182
# write_csv(temp, "data/99 parties unmatched due to EES.csv")

#  2.1.2 Parties that I cannot match due to EPEES
temp <-
  ees %>% 
  select(
    cntry_short, starts_with("Q10_")
    ) %>% 
  mutate_at(vars(starts_with("Q10_")), ~replace(., which(.>10), NA)) %>% 
  pivot_longer(
    contains("Q10_"),
    names_to = "ees_partycode",
    values_to = "PTV",
  ) %>%
  na.omit() %>% 
  select(-PTV) %>% 
  distinct() %>% 
  mutate(
    ees_partycode = ees_partycode %>% str_replace("q10", "party")
    ) %>% 
  anti_join(
    enx %>% 
      select(cntry_short, ees_partycode)
    )


# 26 unmatched parties
# 182 matchable
write_csv(temp, "data/99 parties unmatched due to EPEES.csv")


# [SC] unmatched by EPEES ----------------------------------------------------
enx <- 
  enx %>% 
  mutate(
    ees_partycode = ifelse(ees_partycode %in~% "NA", NA, ees_partycode)
  ) %>% 
  filter(!is.na(ees_partycode))

##### ATTENTION: Maybe I should not yet kick them out if I'm going to do any average / country-wise calculations!
##### Next step here: check if these parties are in the EES manual


## 2.2 General Recodings --------------------------------------------------------
## 2.2.1 Harshness, negativity and incivility ----------------------------------------------------
enx <- 
  enx %>% 
    mutate(
      # Negativity from reversed Tonality of Campaign
      # original tone variable -10 negative, +10 positive
      # new negativity variable -10 positive, +10 negative
      negativity = -tone,
      # Incivility remains the same
      uncivil,
      harshness = ((negativity+10)/2 + uncivil)/2,
    )



## 2.3 Ideological Party Positions ----------------------------------------------------
### 2.3.1 Left-Right --------------------------------------------------------
# 1. Based on Wikipedia entry for 
# Party l-r Position from wikipedia: [0 = far left;12 = far right] > new [0;10]
enx <- 
  enx %>% 
  mutate(
    p_lrpos_wiki = p_position/13*10,
  )

# Check coding
# enx %>% select(p_lrpos_wiki, p_position) %>% table()

# TEMP delete all this
# # 2. Based on EES Respondent's perceived placement
# temp <- 
#   ees %>% 
#   select(
#     cntry,
#     regionbe,
#     starts_with("q13")
#   ) %>% 
#   # Set NA values
#   mutate_at(vars(starts_with("q13")), ~replace(., which(.>10), NA)) %>% 
#   group_by(cntry, regionbe) %>% 
#   summarise_all(mean, na.rm=T)
# 
# # Merging back to EPEES dataset
# enx1 <- 
#   temp %>% 
#   pivot_longer(
#     starts_with("q13_"), 
#     names_to = "ees_partycode", 
#     values_to = "p_lrpos_ees") %>% 
#   na.omit() %>% 
#   mutate(
#     ees_partycode = ees_partycode %>% str_replace("q13", "party")
#   ) %>% 
#   left_join(
#     enx, .
#   )


# 3. Based on CHES
# tbd.
# enx$CHESpid


### 2.2.1 EU position--------------------------------------------------------
# 1. From EPEES Expert Judgement
# Elite positions on EU - higher values, more in favour of EU!
enx <- 
  enx %>% 
  ungroup %>% 
  mutate(
    # I have to reverse one of the items
    eurefer_r = 10-eurefer
  ) 

# Reliability of scale
enx %>%
  select(euitegr, eufunct#, eurefer_r
         ) %>% #labelled::var_label()
  psych::alpha()
# Alpha [0.87 - 0.92] / [0.89 - 0.94]

# Calculate party-wise EU position via mean of the following variables
enx$p_eupos <- enx %>% select(euitegr, eufunct#, eurefer_r
                              ) %>% rowMeans(na.rm=T)

enx <- enx %>% filter(!is.na(p_eupos))

# [SC] Luxembourg Pirate Party is dropped ----------------------------------
# this one drops a warning that 1 value is Inf.
# because it has no p_eupos...
# enx %>% 
#   filter(p_eupos_mindist == Inf) %>% 
#   select(cntry_short, p_eupos, euitegr, eufunct, starts_with("p_eupos_")) %>% View



## 2.3 Ideological distance to closest competitor  ----------------------------------------------------
# This calculation can be expanded with other variables at will.
dimensions <- c("p_lrpos_wiki",# "p_lrpos_ees", 
                "p_eupos")

# For all forms at once:
for (ideol in dimensions){
  # Add the ideological positions (across all measurements) as columns to calculate distances afterwards
  enx <- enx %>%
    select(cntry, regionbe, ees_partycode, all_of(ideol)) %>% 
    mutate(
      ees_partycode = ees_partycode %>% str_replace("party", ideol)
    ) %>% 
    pivot_wider(names_from = "ees_partycode", values_from = all_of(ideol)) %>% 
    left_join(
      enx, .
    )
  
  # Calculate all distances
  for (j in 1:9){
    enx <-
      enx %>% 
      mutate(
        "{ideol}_dist_{j}" := ifelse(
          ees_partycode == glue("party_{j}"), NA, 
          abs(.data[[ideol]] - .data[[glue("{ideol}_{j}")]]))
      )
  }
  
  # Get min distance and index of closest competitor
  enx <- enx %>% 
    mutate(
      "{ideol}_mindist" := 
        enx %>% 
        select(starts_with(paste0(ideol, "_dist"))) %>% 
        apply(1, min, na.rm =T),
      "{ideol}_mindist_index" := 
        enx %>% 
        select(starts_with(paste0(ideol, "_dist"))) %>% 
        apply(1, which.min)
    )
}

  
### 2.3.2 Two-dimensional distance -----------------------------------------------
# combo <- c("p_lrpos_ees", "p_eupos")
combo <- c("p_lrpos_wiki", "p_eupos")

for (j in 1:9){
  enx <-
    enx %>% 
    rowwise() %>% 
    mutate(
      "p_combo_dist_{j}" := ifelse(
        ees_partycode == glue("party_{j}"), NA, 
        mean(
          c(abs(.data[[combo[1]]] - .data[[glue("{combo[1]}_{j}")]]),
          abs(.data[[combo[2]]] - .data[[glue("{combo[2]}_{j}")]])),
          na.rm = T)
        ),
      "p_combo_edist_{j}" := ifelse(
        ees_partycode == glue("party_{j}"), NA, 
          sqrt((.data[[combo[1]]] - .data[[glue("{combo[1]}_{j}")]])^2+
          (.data[[combo[2]]] - .data[[glue("{combo[2]}_{j}")]])^2)
        )
    ) %>% ungroup
}


# Get min distance and index of closest competitor
enx <- enx %>% 
  mutate(
    "p_combo_wiki_mindist" := 
      enx %>% 
      select(starts_with("p_combo_dist")) %>% 
      apply(1, min, na.rm =T),
    "p_combo_wiki_emindist" := 
      enx %>% 
      select(starts_with("p_combo_edist")) %>% 
      apply(1, min, na.rm =T),
    "p_combo_wiki_mindist_index" := 
      enx %>% 
      select(starts_with("p_combo_dist")) %>% 
      apply(1, which.min),
    "p_combo_wiki_emindist_index" := 
      enx %>% 
      select(starts_with("p_combo_edist")) %>% 
      apply(1, which.min)
  )

# # How many closest competitors do these dimensions return?
# for (i in c("p_combo", dimensions)){
#   sum(
#     enx %>% 
#       select(
#         starts_with(paste0(i, "_dist")), 
#         one_of(paste0(i,"_mindist"))) %>% 
#       apply(1, function(x){
#         sum(x[1:9] == x[paste0(i,"_mindist")], na.rm = T)
#       }) > 2
#   ) %>% 
#     paste0("Dimension ", i, " returns ", ., " times more than 1 closest competitor!") %>% 
#     print()
# }#  => use the combo one; the wiki LR alone is not sufficient!



# To get the degree to which a respondent is closer to the closest competition
# of a sponsor rather than to the sponsor itself, we need the ideological position
# of the closest competition of every party.
# Not currently in use thus here commented-out:
# enx1$eupos_next_closest <-
#   enx1 %>%
#   select(paste0("ees_partycode_", 1:9), eu_mindist_index) %>%
#   apply(1, function(x){x[x["eu_mindist_index"]]})

### 2.4.2 [R] Adjusted by expert ideology ---------------------------------------------
# NOTE adjusted negativity
# "inspired by a procedure described in Walter and Van der Eijk (2019). 
# The adjusted measure is obtained, for each party, by: 
# (a) regressing their value on the campaign harshness index 
#  on the difference between the average expert left-right 
# position and the party’s ideological profile (i.e. a measure 
# of how ‘ideologically distant’ the expert sample and the party 
# they evaluated are); and (b) store the regression residuals – that is, 
# the part of the dependent variable that is not explained by that 
# ideological distance – into a new variable. This adjusted measure of 
# campaign harshness is thus independent of the ideological proximity or 
# distance of the (average) expert from the ideological position of 
# the party. Models using this adjusted version of campaign harshness 
# show results are at times somewhat weaker, but generally robust.” (Nai et al. 2022, p. 16)

enx <-
  enx %>% 
  ungroup() %>% 
  mutate(
    # Create a L-R Expert-party distance variable
    # Nai et al. version (in varname DIFFIDEOL)
    # exp = exp_lrscale01,
    # p = p_positionr01,
    # expdist = abs(exp - p),
    
    # My version
    # Expert l-r Position original: [0 = left;10 = right]
    exp = (exp_lrscale)/10,
    # expdist_rel = exp - p_lrpos_wiki/10,
    expdist = abs(exp - p_lrpos_wiki/10),
    # expdist_rel = exp - p_lrpos_wiki/10,
    # expdist_rel = exp - p_lrpos_wiki/10, # this was ale's calculation presumably
    
    # Calculate residuals by expert ideology
    p_resid_neg = lm(data = enx, formula = negativity ~ expdist)$residuals,
    # p_resid_neg_rel = lm(data = enx, formula = negativity ~ DIFFIDEOL)$residuals,
    # p_resid_neg_rel = lm(data = enx, formula = negativity ~ expdist_rel)$residuals,
    p_resid_unciv = lm(data = enx, formula = uncivil ~ expdist)$residuals,
    # Calculate residuals by expert ideology
    p_resid_harsh = (p_resid_neg + p_resid_unciv)/2
  )

## 2.2.1 Average negativity of election ----------------------------------------------------
enx <-
  enx %>%
  group_by(cntry_short) %>%
  mutate(
    e_negativity = mean(negativity, na.rm=T),
    e_incivility = mean (uncivil, na.rm=T),
    e_adj_incivility = mean (p_resid_unciv, na.rm=T),
    e_adj_negativity = mean (p_resid_neg, na.rm=T),
    e_harshness = mean (harshness, na.rm=T),
  ) %>% ungroup

## 2.4 Negativity and Incivility of next closest competitor -----------------------------------------------

# Negativity of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry, regionbe, ees_partycode, negativity) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "negativity", names_glue = "neg_{ees_partycode}") %>%
  full_join(enx,.) # creates duplicates but these don't seem to matter

# Adjusted Negativity of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry, regionbe, ees_partycode, p_resid_neg) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_resid_neg", names_glue = "neg_adj_{ees_partycode}") %>%
  full_join(enx,.) # creates duplicates but these don't seem to matter

# Incivility of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry, regionbe, ees_partycode, uncivil) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "uncivil", names_glue = "inciv_{ees_partycode}") %>%
  full_join(enx,.)  # creates duplicates but these don't seem to matter

# Adjusted Incivility of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry, regionbe, ees_partycode, p_resid_unciv) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_resid_unciv", names_glue = "inciv_adj_{ees_partycode}") %>%
  full_join(enx,.)  # creates duplicates but these don't seem to matter

# TEMP Harshness of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry, regionbe, ees_partycode, harshness) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "harshness", names_glue = "harsh_{ees_partycode}") %>%
  full_join(enx,.)  # creates duplicates but these don't seem to matter

# TEMP Adjusted hashness
enx <- 
  enx %>% 
  # Harshness of all competitors as new columns
  select(cntry, regionbe, ees_partycode, p_resid_harsh) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_resid_harsh", names_glue = "harsh_adj_{ees_partycode}") %>%
  full_join(enx,.)  # creates duplicates but these don't seem to matter

enx <- 
  enx %>% 
  mutate(
    # Negativity of next closest competition
    # p_neg_close_lr_ees = enx %>% select(paste0("neg_party_", 1:9), p_lrpos_wiki_mindist_index) %>% apply(1, function(x){x[x["p_lrpos_wiki_mindist_index"]]}),
    p_neg_close_lr_wiki = enx %>% select(paste0("neg_party_", 1:9), p_lrpos_wiki_mindist_index) %>% apply(1, function(x){x[x["p_lrpos_ees_mindist_index"]]}),
    p_neg_close_eu = enx %>% select(paste0("neg_party_", 1:9), p_eupos_mindist_index) %>% apply(1, function(x){x[x["p_eupos_mindist_index"]]}),
    p_neg_close_combo = enx %>% select(paste0("neg_party_", 1:9), p_combo_wiki_mindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_mindist_index"]]}),
    p_neg_close_ecombo = enx %>% select(paste0("neg_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Adjusted
    p_neg_adj_close_combo = enx %>% select(paste0("neg_adj_party_", 1:9), p_combo_wiki_mindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_mindist_index"]]}),
    p_neg_adj_close_ecombo = enx %>% select(paste0("neg_adj_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Incivility of next closest competition
    # p_inciv_close_lr_resp = enx %>% select(paste0("inciv_party_", 1:9), p_lrpos_wiki_mindist_index) %>% apply(1, function(x){x[x["p_lrpos_wiki_mindist_index"]]}),
    p_inciv_close_lr_wiki = enx %>% select(paste0("inciv_party_", 1:9), p_lrpos_wiki_mindist_index) %>% apply(1, function(x){x[x["p_lrpos_ees_mindist_index"]]}),
    p_inciv_close_eu = enx %>% select(paste0("inciv_party_", 1:9), p_eupos_mindist_index) %>% apply(1, function(x){x[x["p_eupos_mindist_index"]]}),
    p_inciv_close_combo = enx %>% select(paste0("inciv_party_", 1:9), p_combo_wiki_mindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_mindist_index"]]}),
    p_inciv_close_ecombo = enx %>% select(paste0("inciv_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Adjusted
    p_inciv_adj_close_combo = enx %>% select(paste0("inciv_adj_party_", 1:9), p_combo_wiki_mindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_mindist_index"]]}),
    p_inciv_adj_close_ecombo = enx %>% select(paste0("inciv_adj_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Harshness
    p_harsh_close_combo = enx %>% select(paste0("harsh_party_", 1:9), p_combo_wiki_mindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_mindist_index"]]}),
    p_harsh_close_ecombo = enx %>% select(paste0("harsh_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    p_harsh_adj_close_combo = enx %>% select(paste0("harsh_adj_party_", 1:9), p_combo_wiki_mindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_mindist_index"]]}),
    p_harsh_adj_close_ecombo = enx %>% select(paste0("harsh_adj_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
  )

# TEMP Calculate difference in negativity and incivility
enx <- enx %>%
  mutate(
    # Rel incivility
    # p_inciv_dif_close_lr_resp = p_inciv_close_lr_resp - uncivil,
    p_inciv_dif_close_lr_wiki = p_inciv_close_lr_wiki - uncivil,
    p_inciv_dif_close_lr_eu = p_inciv_close_eu - uncivil,
    p_inciv_dif_close_lr_combo = p_inciv_close_combo - uncivil,
    
    # Rel negativity
    # p_neg_dif_close_lr_resp = p_neg_close_lr_ees - negativity,
    p_neg_dif_close_lr_wiki = p_neg_close_lr_wiki - negativity,
    p_neg_dif_close_lr_eu = p_neg_close_eu - negativity,
    p_neg_dif_close_lr_combo = p_neg_close_combo - negativity,
  )


# TEMP Some parties did not get the EUPOS distances or negativity of competitor...
# enx %>% filter(is.na(mindist)) %>% 
#   View()
# enx %>% 
#   filter(is.na(negativity_next_closest)) %>% 
#   select(ees_partycode, eu_mindist_index, starts_with("neg_"))
#   View
# => this is because they also have no PTVs
####

## 2.3 Ideological Distance Individual-Party ----------------------------------------------------
## 2.3.1 L-R position of respondent ----------------------------------------------------
ees <- 
  ees %>% 
  mutate(
    i_lrpos = ifelse(Q11>10, NA, Q11),
    i_extremity_lr = abs(i_lrpos-5),
  )

## 2.3.2 EU position of respondent ----------------------------------------------------
# Elite EU position is currently composed of 
# 1) evaluating EU integration
# 2) evaluating functioning of EU
# 3) considering referendum for EU Exit

# For respondents I use
# 1) evaluation of EU integration
# 2) satisfaction with EU
# 3) EU Exit vote preference
# 4) evaluation of EU membership
# Alpha for this scale is 0.81

# Calculate position per individual
ees$i_eupos <- ees %>% 
  select(
    Q23, # classical integration; Q24 would be the equivalent for placing other parties
    Q4, # satisfaction with EU
    Q15, # EU Exit
    Q22, # evaluation of membership in EU
    # Q11, # Conventional L-R scale
    # Q14_1, # ... other issue positions
    ) %>% 
  mutate(
    # Satisfaction with eu
    eu_sat = (4 - (as.numeric(Q4) %>% replace(Q4 > 5, NA)))/3,
    eu_int = (as.numeric(Q23) %>% replace(Q23 > 10, NA))/10,
    eu_exit = case_when(
      Q15 == 1 ~ 1,
      Q15 == 2 ~ 0,
      Q15 == 3 ~ 0.5,
      Q15 == 4 ~ 0.5,
      T ~ NA_real_
    ),
    eu_meval = case_when(
      Q22 == 1 ~ 1,
      Q22 == 2 ~ 0,
      Q22 == 3 ~ 0.5,
      T ~ NA_real_
    ),
  ) %>% 
  select(matches("eu_.*")) %>% 
  rowMeans(na.rm = T) * 10

# Eu pos extremity of individual
ees <- 
  ees %>% 
  mutate(
    i_extremity_eu = abs(i_eupos-5)
  )

## 2.3.3 Perceived Ideol. Distances to parties --------------------------------------------------------
ees <- ees %>% 
  mutate_at(vars(starts_with("q13")), ~replace(., which(.>10), NA)) %>% 
  mutate(
    i_dist2party_1 = abs(q13_1 - i_lrpos),
    i_dist2party_2 = abs(q13_2 - i_lrpos),
    i_dist2party_3 = abs(q13_3 - i_lrpos),
    i_dist2party_4 = abs(q13_4 - i_lrpos),
    i_dist2party_5 = abs(q13_5 - i_lrpos),
    i_dist2party_6 = abs(q13_6 - i_lrpos),
    i_dist2party_7 = abs(q13_7 - i_lrpos),
    i_dist2party_8 = abs(q13_8 - i_lrpos),
    i_dist2party_9 = abs(q13_9 - i_lrpos),
  )


## 2.3.4 Obj Ideol Distances to parties --------------------------------------------------------
# 1. Merge all parties' positions back to EES
ees <- enx %>% 
  select(
    cntry_short,
    one_of(paste0("p_lrpos_wiki_",1:9)),
    # one_of(paste0("p_lrpos_ees_",1:9)),
    one_of(paste0("p_eupos_",1:9)),
    ) %>% distinct() %>% 
  left_join(ees,.)

# Get Individual's distances for single dimensions
i_dimensions <- c("i_lrpos", "i_lrpos", "i_eupos")
base <- c("wiki_", #"ees_",
          "")

for (i in 1:length(dimensions)){
  # Add the ideological positions (across all measurements) as columns to calculate distances afterwards
  # Calculate all distances
  for (j in 1:9){
    ees <-
      ees %>% 
      mutate(
        "{i_dimensions[i]}_dist_{base[i]}{j}" := 
          abs(.data[[i_dimensions[i]]] - .data[[glue("{dimensions[i]}_{j}")]])
      )
  }
}

# Combo Distance
# this has to be
combo <- c("p_lrpos_wiki", "p_eupos")
base <- combo[1] %>% str_extract("wiki") #|ees
# Average distance between individual and all parties across both dimensions
# Calculate all distances
for (j in 1:9){
  ees <-
    ees %>% 
    rowwise() %>% 
    mutate(
      "i_combo_dist_e{base}_{j}" := 
        sqrt((i_lrpos - .data[[glue("{combo[1]}_{j}")]])^2+
        (i_eupos - .data[[glue("{combo[2]}_{j}")]])^2),
      "i_combo_dist_{base}_{j}" := 
        mean(
          c(abs(i_lrpos - .data[[glue("{combo[1]}_{j}")]]),
            abs(i_eupos - .data[[glue("{combo[2]}_{j}")]])),
          na.rm = T)
    ) %>% 
    ungroup()
}


## 2.4. Control vars --------------------------------------------------------
ees <- ees %>%
  mutate(
    i_pinterest = na_if(Q21, 99) %>% na_if(98) %>% sjlabelled::as_character(), # political interest
    i_yrbrn = D4_1, # year born
    # age_cat = na_if(hAge, ), # age
    i_edulvl = EDU %>% na_if(99) %>% na_if(97) %>% sjlabelled::as_character(),
    i_gender = D3 %>% sjlabelled::as_character(),
  ) 


### 2.4.1 [R] Following the election in the media --------------------------------------------------
# only focus on people following the media?
ees <- ees %>% 
  mutate(
    i_followelections = Q8 %>% as.numeric %>% replace(.>10, NA)
  )




# scatterandsmooth(enx, p_resid_neg, p_resid_neg_rel, p_lrpos_wiki)
# scatterandsmooth(enx, negativity, expdist, p_lrpos_wiki)
# scatterandsmooth(enx, negativity, expdist_rel, p_lrpos_wiki)


# compare to Ale's measures
# enx %>% 
#   select(
#     p_resid_neg,
#     )
# codebenx %>% View

### 2.4.3 [R] Extremity --------------------------------------------------
enx <- enx %>% 
  mutate(
    p_eupos_extreme = abs(p_eupos-5),
    p_lrpos_extreme_wiki = abs(p_lrpos_wiki-5),
    # p_lrpos_extreme_ees = abs(p_lrpos_ees-5),
  )

### 2.4.4 [R] Populism attitude of repsondents--------------------------------------------------
# #dislike negativity; Let’s only focus in on people who shouldn’t like negativity. 
# Populist attitudes are associated with liking negativity; Run analysis only with ppl. low on pop attitudes.

### 2.4.5 [R] N of experts --------------------------------------------------
# kick parties with less than 3 coders EPEES
enx <-
  enx %>% 
  filter(numresp>=3) #%>% 
  # select(cntry, enx_party) %>% pull(enx_party) %>% paste0(collapse = ", ") %>% write_clip
# =>  we lose luxembourg


# 3. [SC] Stacking data --------------------------------------------------------
# ALl control variables
cntrls <- c(#"extremity", "lrpos", 
            "i_gender",
            "i_yrbrn", 
            "i_edulvl", 
            "i_pinterest",
            "i_eupos",
            "i_lrpos",
            "i_extremity_eu",
            "i_extremity_lr",
            "p_lrpos_wiki",
            # "p_lrpos_ees",
            "p_eupos",
            "p_eupos_extreme",
            "p_neg_close_combo",
            "p_inciv_close_combo",
            "p_neg_adj_close_combo",
            "p_neg_adj_close_ecombo",
            "p_inciv_adj_close_combo",
            "p_inciv_adj_close_ecombo",
            # "p_lrpos_extreme_ees",
            "p_lrpos_extreme_wiki",
            "e_negativity",
            "e_incivility",
            "e_harshness",
            "e_adj_incivility",
            "e_adj_negativity"
            )

# Connecting EPEES to EES
enx <- 
  enx %>% 
  rename(
    p_harshness = harshness,
    p_negativity = negativity, 
    p_uncivil = uncivil, #negativity_difference_lr, uncivil_difference_lr,
    p_incumbency = p_NatGov19,
    p_numresp = numresp,
  )

temp <-
  ees %>% 
  select(
    i_unique, cntry_short,
    i_lrpos, 
    i_followelections,
    matches("Q10_"),
    matches("i_dist2party_"),
    all_of(cntrls %[out~% "p_|e_"), # all individual-level controls
    all_of(paste0("i_combo_dist_wiki_", 1:9)),
    all_of(paste0("i_combo_dist_ewiki_", 1:9)),
    all_of(paste0("i_lrpos_dist_wiki_", 1:9)),
    # all_of(paste0("i_lrpos_dist_ees_", 1:9)), 
    # all_of(paste0("i_eupos_dist_", 1:9)),
         ) %>% 
  # Convert main DV into numeric
  mutate_at(vars(matches("Q10_")), as.numeric, na.rm = T) %>% 
  pivot_longer(names_to = "ees_partycode", values_to = "ptv", cols = matches("Q10_")) %>% 
  mutate(
    ees_partycode = ees_partycode %>% str_replace("q10", "party"),
    p_uniqueid = str_c(cntry_short, "_", ees_partycode)
    ) %>% 
  # Remove DV NA rows
  filter(ptv <= 10)  %>% 
  
  # Remove parties not covered in EPEES
  filter(p_uniqueid %in% enx$p_uniqueid) %>% 
  
  # Connect to negex
  inner_join(
    enx # %>% filter(!is.na(enx_party), !is.na(ees_partycode))
             )

  temp %>% 
    summarise(
      countries = n_distinct(cntry_short),
      parties = n_distinct(p_uniqueid),
      respondents = n_distinct(i_unique),
      dyads = n()
    ) %>% print()
## VERIFICATION TESTS
# GET A SENSE OF WHICH ROWS WE'VE JUST LOST => nothing in addition to those 
# linkage %>% 
#   select(cntry_short, party_engl, p_uniqueid) %>% 
#   filter(p_uniqueid %in% enx$p_uniqueid) %>% 
#   filter(p_uniqueid %out% temp$p_uniqueid)


# # Inspect Duplicates
# temp %>% 
#   select(cntry, ees_partycode, ees_partyname, enx_party) %>% 
#   distinct() %>% 
#   group_by(ees_partycode, cntry) %>% 
#   filter(n()>1) %>% 
#   arrange(cntry, ees_partycode)

## 3.1. Dyadic measures --------
### 3.1.1 L-R distance 2 sponsor--------
# We already have the ideological distances to most parties; now we need to specify the sponsor
temp <-
  temp %>% 
    rowwise %>% 
  mutate(
    # L-R distance to sponsor
    d_lrdist_wiki = abs(i_lrpos - p_lrpos_wiki),
    # d_lrdist_ees = abs(i_lrpos - p_lrpos_ees),

    # two-dim distance to sponsor
    d_combodist_ewiki = sqrt(
        (i_lrpos - p_lrpos_wiki)^2 +
        (i_eupos - p_eupos)^2
      ),
    d_combodist_wiki = mean(
      c(
        abs(i_lrpos - p_lrpos_wiki),
        abs(i_eupos - p_eupos)
        )#, na.rm =T
      )
    # d_combodist_ees = abs(i_lrpos - p_lrpos_ees),
  )

# temp %>%
#   select(d_combodist_wiki, d_combodista_wiki, i_lrpos, p_lrpos_wiki, i_eupos, p_eupos) %>% View
# temp %>%
#   select(d_combodist_wiki, d_combodista_wiki) %>% 
#   cor(use = "complete.obs")

### 3.1.1 perceived L-R distance 2 sponsor--------
temp$d_percdist <- 
  temp %>% 
  mutate(
    ees_indicator = ees_partycode %>% str_remove("party_") %>% as.numeric
  ) %>% 
  select(starts_with("i_dist2party_"), ees_indicator) %>% 
  apply(1, function(x){x[x["ees_indicator"]]})


# COMBAK: Ideological position of closest competitor (for a deprecated IV) can be implemented lateron if necessary.


### 3.1.2 Number of interposing parties --------
# How many parties is an individual closer to than to the sponsor of negativity

# Create numeric ees partycode
temp <- 
  temp %>% 
  mutate(
  ees_partycode_num = ees_partycode %>% str_remove("party_") %>% as.numeric()
  ) 

# Set inter-party distance to sponsor to NA
for (i in 1:9){
  temp <- 
    temp %>% 
    mutate(
      # "i_lrpos_dist_ees_{i}" := ifelse(ees_partycode_num == i, NA, .data[[glue("i_lrpos_dist_ees_{i}")]]),
      "i_lrpos_dist_wiki_{i}" := ifelse(ees_partycode_num == i, NA, .data[[glue("i_lrpos_dist_wiki_{i}")]]),
      "i_combo_dist_wiki_{i}" := ifelse(ees_partycode_num == i, NA, .data[[glue("i_combo_dist_wiki_{i}")]]),
      "i_combo_dist_ewiki_{i}" := ifelse(ees_partycode_num == i, NA, .data[[glue("i_combo_dist_ewiki_{i}")]]),
      "i_dist2party_{i}" := ifelse(ees_partycode_num == i, NA, .data[[glue("i_dist2party_{i}")]]),
    )
}
# Verification of this step
# temp %>%
#   select(ees_partycode_num, contains("lrpos_dist_wiki"))
#   select(ees_partycode_num, contains("i_lrpos_dist_wiki_"))
#   select(ees_partycode_num, contains("i_lrpos_dist_ees_"))
#   select(ees_partycode_num, contains("i_dist2party_"))


# # based on parties' perceived LR position
# temp$d_ninterpos_ees <- 
#   temp %>% 
#   select(one_of(paste0("i_lrpos_dist_ees_",1:9)), d_lrdist_ees) %>%
#   apply(1, function(x){sum(x[1:9]<x["d_lrdist_ees"], na.rm = T)})
# temp$d_ninterpos_eq_ees <- 
#   temp %>% 
#   select(one_of(paste0("i_lrpos_dist_ees_",1:9)), d_lrdist_ees) %>%
#   apply(1, function(x){sum(x[1:9]<=x["d_lrdist_ees"], na.rm = T)})

# based on parties' wiki LR position
temp$d_ninterpos_wiki <- 
  temp %>% 
  select(one_of(paste0("i_lrpos_dist_wiki_",1:9)), d_lrdist_wiki) %>%
  apply(1, function(x){sum(x[1:9]<x["d_lrdist_wiki"], na.rm = T)})
temp$d_ninterpos_eq_wiki <- 
  temp %>% 
  select(one_of(paste0("i_lrpos_dist_wiki_",1:9)), d_lrdist_wiki) %>%
  apply(1, function(x){sum(x[1:9]<=x["d_lrdist_wiki"], na.rm = T)})

# based on eu and wiki based lr position
temp$d_ninterpos_combo <- 
  temp %>% 
  select(one_of(paste0("i_combo_dist_wiki_",1:9)), d_combodist_wiki) %>%
  apply(1, function(x){sum(x[1:9]<x["d_combodist_wiki"], na.rm = T)})
temp$d_ninterpos_eq_combo <- 
  temp %>% 
  select(one_of(paste0("i_combo_dist_wiki_",1:9)), d_combodist_wiki) %>%
  apply(1, function(x){sum(x[1:9]<=x["d_combodist_wiki"], na.rm = T)})

# based on eu and wiki based ideol position euclidean
temp$d_ninterpos_ecombo <- 
  temp %>% 
  select(one_of(paste0("i_combo_dist_ewiki_",1:9)), d_combodist_ewiki) %>%
  apply(1, function(x){sum(x[1:9]<x["d_combodist_ewiki"], na.rm = T)})
temp$d_ninterpos_eq_ecombo <- 
  temp %>% 
  select(one_of(paste0("i_combo_dist_ewiki_",1:9)), d_combodist_ewiki) %>%
  apply(1, function(x){sum(x[1:9]<=x["d_combodist_ewiki"], na.rm = T)})



# based on perceived distances
temp$d_ninterpos_perc <- 
  temp %>% 
  select(one_of(paste0("i_dist2party_",1:9)), d_percdist) %>%
  apply(1, function(x){sum(x[1:9]<x["d_percdist"], na.rm = T)})
temp$d_ninterpos_eq_perc <- 
  temp %>% 
  select(one_of(paste0("i_dist2party_",1:9)), d_percdist) %>%
  apply(1, function(x){sum(x[1:9]<=x["d_percdist"], na.rm = T)})

#wait did I select distance to the sponsor to zero na? 
# => not necessary if it's *smaller than* 
temp %>% select(contains("ninterpos")) %>% 
  cor(use = "complete.obs")


# cntrls <- c(cntrls, "d_combodist_wiki")
cntrls <- c(cntrls, "d_percdist", "d_combodist_wiki", "d_combodist_ewiki")

### 3.1.3 AVG dist. to interposing parties --------
# temp$avg_dist_interpos <- temp %>%
#   select(one_of(paste0("lrdist_",1:9)), i_lrdist) %>%
#   apply(1, function(x){mean(x[1:9]<x["i_lrdist"], na.rm = T)})
#   
# Issue: is zero for no interposing parties

# 4. last sample cuts --------------------------------------------------------
# Analysis dataset
tempdf <- 
  temp %>%
  mutate(e_region = region %>% sjlabelled::as_character()) %>% 
  # Select relevant variables for model
  select(
    # Control variables
    one_of(cntrls),
    # Nesting structure variables
    cntry_short, p_uniqueid, i_unique,
    ees_partyname_orig,
    # Dependent Variable
    d_ptv = ptv, 
    # p_prcEP19,
    
    # Independent Variables
    p_harshness,
    p_negativity, 
    p_incivility = p_uncivil, #negativity_difference_lr, uncivil_difference_lr,
    # p_resid_harsh,
    p_resid_neg,
    p_resid_unciv,
    
    # Moderation Variables
    e_ENP = ENP, 
    # p_combo_wiki_mindist, #p_lrpos_ees_mindist, p_lrpos_wiki_mindist, 
    p_combo_wiki_emindist, #p_lrpos_ees_mindist, p_lrpos_wiki_mindist, 
    d_ninterpos_perc = d_ninterpos_eq_perc,
    # d_ninterpos_combo,
    # d_ninterpos_ecombo,
    d_ninterpos_eq_combo,
    d_ninterpos_eq_ecombo,
    
    #d_ninterpos_ees, d_ninterpos_wiki, 
    
    # Controls
    ## party
    # p_harsh_close_combo,
    # p_harsh_adj_close_combo,
    # p_neg_close_combo, #p_neg_close_lr_ees, p_neg_close_lr_wiki, #p_neg_close_eu, 
    # p_inciv_close_combo, #p_inciv_close_lr_ees, p_inciv_close_lr_wiki, #p_inciv_close_eu, 
    p_inciv_close_ecombo, #p_inciv_close_lr_ees, p_inciv_close_lr_wiki, #p_inciv_close_eu, 
    p_neg_close_ecombo,
    # p_harsh_close_ecombo,
    
    p_lrpos_wiki,
    # p_lrpos_ees,
    p_eupos,
    
    ## individual
    i_lrpos,
    i_extremity_lr,
    
    ## dyadic
    # d_combodist_wiki, #control => in the controls vector
    # d_lrdist_wiki,
    # d_lrdist_ees,
    # d_combodist_ees,
    
    ## robustness
    i_followelections,
    
    # lrdist_rel_i,
    # lrpos_next_closest, # why do I need this one?
    # follownews,
    # controls
    
    e_region,
    p_incumbency, # Incumbency
    p_numresp,
    party_acro,
    starts_with("exp_"), 
    -exp_lrscale01 # double-coded
    )
  
# INSPECT MISSING CASES PATTERNS
# tempdf %>%
#   select(
#     -contains("ees"),
#     -contains("lr"),
#     -contains("dist"),
#     ) %>% 
#   mutate_all(function(x) ifelse(is.na(x),1,0)) %>%
#   as.data.frame() %>%
#   UpSetR::upset(
#     nsets = 20,
#     nintersects = 29,
#     order.by = "freq",
#     empty.intersections = T)


## NEXT STEP: value imputation

# tempdf %>% 
#   group_by(p_uniqueid) %>%
#   summarise(pctmis = sum(is.na(i_lrpos)) / n()
#             ) %>%
#   arrange(-pctmis)

# why 30% missing?
# on what should we not be kicking out data?
# on voteshares p_prcEP19

# Upset plot 
# tempdf %>% 
#   select(-i_edulvl) %>% 
#     mutate_all(function(x) ifelse(is.na(x),1,0)) %>%
#     as.data.frame() %>%
#     UpSetR::upset(
#       nsets = 10,
#       # nintersects = 29,
#       order.by = "freq",
#       empty.intersections = T)


# 5. Saving analysis dataset -------------------------------------------------
vroom::vroom_write(tempdf, glue("data/{Sys.Date()}_Analysisfile.csv"))
vroom::vroom_write(enx, glue("data/{Sys.Date()}_EPEES-file.csv"))


# feather::write_feather(tempdf, glue("data/{Sys.Date()}_Analysisfile.csv"))
