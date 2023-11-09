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


# 1. Loading Packages & Data -----------------------------------------
## 1.1 Packages and functions ---------------------------------------------
# loading / installing packages depending on whether it is already installed.
pacman::p_load(
  tidyverse, 
  tidylog,
  inops,
  glue,
  beepr, 
  clipr,
  inops
  )

# helper functions for this project
source("00_utils.R")


## 1.2 Loading Data -------------------------------------------------------
# TODO change this comment!, double check that all steps have been done!
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
# source("02_pre-processing-data-cleaning.R")


# EPEES_19 dataset
# TODO ideally move any sjlabelled operations to the data prep. script; this should only be the newly created enx datafile
enx <- read_csv("data/EPEES19-cleaned.csv")
enx_lab <- haven::read_dta("data/EP2019_data_parties_v2.dta")

# TEMP remove this
# enx_lab <- haven::read_dta("data/EPEES_19_datapart.dta")
# enx_lab2 <- haven::read_dta("data/EP2019_data_parties.dta")
# # TODO explain this merge with the newly found data
# enx <- full_join(
#   enx %>% select(-c(numresp, uncivil, feelgood, fear, sd_tone, sd_uncivil, sd_feelgood, sd_fear)), 
#   enx_lab2)
# enx_previously <- read_delim("/Users/p.m.mendozauva.nl/Library/CloudStorage/OneDrive-UvA/00 Work/01 ASCoR PhD/01 Study I/Git first paper/data/2022-04-25_EPEES-file.csv", delim = "\t")
# enx_old <- haven::read_dta("/Users/p.m.mendozauva.nl/Library/CloudStorage/OneDrive-UvA/07 Datasets/EPEES19/EP2019_data_parties_v2.dta") # ideally this is not needed bc. it's not available online

# Cleaned EES dataset
ees <- read_csv(file = "data/EES-cleaned.csv")
ees_lab <- haven::read_dta(glue("data/ZA7581_v2-0-1.dta"))
ees_parties <- read_csv(file = "data/EES_parties-cleaned.csv")

# the linkage file created in '01_party-matching-EPEES-EES.R'
linkage <- read_csv("data/00 2023-11-01_Linkage-matched-parties.csv")


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
    ees_partycode = ees_partycode %>% str_replace("q10", "party"),
    cntry_short = str_sub(cntry_short,1,2)
    ) %>% 
  anti_join(
    enx %>% 
      mutate(ees_partycode = as.character(ees_partycode)) %>% 
      select(cntry_short, ees_partycode)
    )
# 24 unmatched
# 184 matched

# CY_7 and SK_10 exist in the answers but not in the codebook
sc_noepees <- left_join(temp, ees_parties) %>% select(cntry_short, ees_party_id, name_engl, epees_id_num)
write_csv(sc_noepees, "data/99 parties unmatched due to EPEES.csv")


# [SC] unmatched by EPEES ----------------------------------------------------
enx <- filter(enx, !is.na(ees_party_id))
# dropping 12 parties

## 2.2 recoding of relevant variables --------------------------------------------------------
### 2.2.1 party-level --------------------------------------------------------
enx <- 
  enx %>% 
  mutate(
    # negativity is the reversed tonality; values: [-10 most positive; +10 most negative]
    p_negativity = -tone,
    
    # incivility remains as is
    p_incivility = uncivil,
    
    # for the ordering of labels in a visualisation
    p_harshness = ((p_negativity+10)/2 + p_incivility)/2, 

    
    # ideological position of parties
    # left-right position from wikipedia: [0 = far left;10 = far right]
    p_lrpos_wiki = p_position/13*10,
    
    # EU position from EPEES Expert Judgements
    p_eupos = select(., euitegr, eufunct) %>% rowMeans(na.rm = T),
    
    # [R] party ideological extremity
    p_eupos_extreme = abs(p_eupos-5),
    p_lrpos_extreme_wiki = abs(p_lrpos_wiki-5),
  )

# Reliability of EU position scale
enx %>%
  select(epees_id_num, euitegr, eufunct) %>% distinct() %>% 
  select(-epees_id_num) %>% 
  psych::alpha()
# Alpha = [0.96-0.98]

# [SC] Luxembourg Pirate Party is dropped
sc_p_eupos <- enx %>% filter(is.na(p_eupos)) %>% select(cntry_short, ees_party_id, partyname, epees_id_num)
enx <-  enx %>% filter(!is.na(p_eupos))


### 2.2.2 inter-party-level --------------------------------------------------------
# 1.) single-dimension
dimensions <- c("p_lrpos_wiki", "p_eupos") # can be expanded

for (ideol in dimensions){
  # add the ideological positions (across all measurements) as columns to calculate distances afterwards
  enx <- enx %>%
    select(cntry_short_be, ees_partycode, all_of(ideol)) %>%
    mutate(
      ees_partycode = ees_partycode %>% str_replace("party", ideol)
    ) %>%
    pivot_wider(names_from = "ees_partycode", values_from = all_of(ideol)) %>%
    left_join(
      enx, .
    )
}

# 2.) two-dimensional
# euclidean distance across two dimensions
for (j in 1:9){
  enx <-
    enx %>% 
    rowwise() %>% 
    mutate(
      "p_combo_edist_{j}" := ifelse(
        ees_partycode == glue("party_{j}"), NA, 
          sqrt((.data[[dimensions[1]]] - .data[[glue("{dimensions[1]}_{j}")]])^2+
          (.data[[dimensions[2]]] - .data[[glue("{dimensions[2]}_{j}")]])^2)
        )
    ) %>% ungroup
}

# Get min distance and index of closest competitor
enx <- enx %>% 
  mutate(
    "p_combo_wiki_emindist" := 
      select(enx, starts_with("p_combo_edist")) %>% apply(1, min, na.rm =T),
    "p_combo_wiki_emindist_index" := 
      select(enx, starts_with("p_combo_edist")) %>% apply(1, which.min),
  )


### 2.2.3 [R] Adjusted by expert ideology ---------------------------------------------
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
# show results are at times somewhat weaker, but generally robust.” 
# (Nai et al. 2022, p. 16)

enx <-
  enx %>% 
  mutate(
    exp = (exp_lrscale)/10, # expert l-r Position original: [0 = left;10 = right]
    expdist = abs(exp - p_lrpos_wiki/10), # ideological distance between expert and party
    
    # Calculate residuals by expert ideology
    p_resid_neg = lm(data = enx, formula = p_negativity ~ expdist)$residuals,
    p_resid_unciv = lm(data = enx, formula = p_incivility ~ expdist)$residuals,
  )


## 2.2.1 average negativity of election ----------------------------------------------------
enx <-
  enx %>%
  group_by(cntry_short) %>%
  mutate(
    e_negativity = mean(p_negativity, na.rm=T),
    e_incivility = mean (p_incivility, na.rm=T),
    e_adj_incivility = mean (p_resid_unciv, na.rm=T),
    e_adj_negativity = mean (p_resid_neg, na.rm=T),
  ) %>% ungroup


## 2.4 negativity and incivility of next closest competitor -----------------------------------------------
# negativity of all competitors as new columns
enx <-
  enx %>% 
  select(cntry_short_be, ees_partycode, p_negativity) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_negativity", names_glue = "neg_{ees_partycode}") %>%
  full_join(enx,.) # creates duplicates but these don't seem to matter

# adjusted negativity of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry_short_be, ees_partycode, p_resid_neg) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_resid_neg", names_glue = "neg_adj_{ees_partycode}") %>%
  full_join(enx,.) # creates duplicates but these don't seem to matter

# incivility of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry_short_be, ees_partycode, p_incivility) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_incivility", names_glue = "inciv_{ees_partycode}") %>%
  full_join(enx,.)  # creates duplicates but these don't seem to matter

# adjusted Incivility of all competitors as new columns
enx <- 
  enx %>% 
  select(cntry_short_be, ees_partycode, p_resid_unciv) %>% 
  pivot_wider(names_from = "ees_partycode", values_from = "p_resid_unciv", names_glue = "inciv_adj_{ees_partycode}") %>%
  full_join(enx,.)  # creates duplicates but these don't seem to matter

# extracting the negativity and incivility (adjusted) depending on the ideological distance measure
enx <-
  enx %>% 
  mutate(
    # Negativity of next closest competition
    p_neg_close_ecombo = enx %>% select(paste0("neg_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Adjusted
    p_neg_adj_close_ecombo = enx %>% select(paste0("neg_adj_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Incivility of next closest competition
    p_inciv_close_ecombo = enx %>% select(paste0("inciv_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
    
    # Adjusted
    p_inciv_adj_close_ecombo = enx %>% select(paste0("inciv_adj_party_", 1:9), p_combo_wiki_emindist_index) %>% apply(1, function(x){x[x["p_combo_wiki_emindist_index"]]}),
  )


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
  mutate(
    # satisfaction with EU
    eu_sat = (4 - (as.numeric(Q4) %>% replace(Q4 > 5, NA)))/3,
    # classical integration; Q24 would be the equivalent for placing other parties
    eu_int = (as.numeric(Q23) %>% replace(Q23 > 10, NA))/10,
    # EU Exit
    eu_exit = case_when(
      Q15 == 1 ~ 1,
      Q15 == 2 ~ 0,
      Q15 == 3 ~ 0.5,
      Q15 == 4 ~ 0.5,
      T ~ NA_real_
    ),
    # evaluation of membership in EU
    eu_meval = case_when(
      Q22 == 1 ~ 1,
      Q22 == 2 ~ 0,
      Q22 == 3 ~ 0.5,
      T ~ NA_real_
    ),
  ) %>% 
  select(matches("eu_.*")) %>% 
  rowMeans(na.rm = T) * 10

# EU pos extremity of individual
ees <- 
  ees %>% 
  mutate(
    i_extremity_eu = abs(i_eupos-5)
  )

## 2.3.3 Perceived Ideol. Distances to parties --------------------------------------------------------
ees <- ees %>% 
  mutate_at(vars(starts_with("q13")), ~replace(., which(.>10), NA)) %>% 
  mutate(across(starts_with("q13_"), ~abs(. - i_lrpos), .names = "i_dist2party_{.col}"))


## 2.3.4 Obj Ideol Distances to parties --------------------------------------------------------
# TODO simplify this => may actually be reduced to only perceived distance!

# 1. Merge all parties' positions back to EES
ees <- enx %>% 
  select(
    cntry_short,
    one_of(paste0("p_lrpos_wiki_",1:9)),
    one_of(paste0("p_eupos_",1:9)),
    ) %>% distinct() %>% 
  left_join(ees,.)

# Get Individual's distances for single dimensions
i_dimensions <- c("i_lrpos", "i_lrpos", "i_eupos")
base <- c("wiki_", "")

# 1.) single dimension
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
combo <- c("p_lrpos_wiki", "p_eupos")
base <- combo[1] %>% str_extract("wiki") #|ees


# 2.) two-dimensional
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
    i_edulvl = EDU %>% na_if(99) %>% na_if(97) %>% sjlabelled::as_character(),
    i_gender = D3 %>% sjlabelled::as_character(),
    
    # [R] Following the election in the media
    i_followelections = Q8 %>% as.numeric %>% replace(.>10, NA),
  ) 


### 2.4.5 [SC] N of experts --------------------------------------------------
# kick parties with less than 3 coders EPEES
enx <-
  enx %>% 
  filter(numresp>=3) #%>% 

enx$epees_id[enx$numresp<3]
# =>  we lose Luxembourg entirely
# 6 rows are kicked out

# 3. [SC] Stacking data --------------------------------------------------------
# ALl control variables
cntrls <- c("i_gender",
            "i_yrbrn", 
            "i_edulvl", 
            "i_pinterest",
            "i_eupos",
            "i_lrpos",
            "i_extremity_eu",
            "i_extremity_lr",
            "p_lrpos_wiki",
            "p_eupos",
            "p_eupos_extreme",
            "p_neg_close_ecombo",
            "p_inciv_close_ecombo",
            # "p_neg_close_combo",
            # "p_inciv_close_combo",
            # "p_neg_adj_close_combo",
            # "p_inciv_adj_close_combo",
            "p_neg_adj_close_ecombo",
            "p_inciv_adj_close_ecombo",
            "p_lrpos_extreme_wiki",
            "e_negativity",
            "e_incivility",
            "e_adj_incivility",
            "e_adj_negativity"
            )

# Connecting EPEES to EES
enx <- 
  enx %>% 
  rename(
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
    # TODO remove these?
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
  # FIXME enx has not p_uniqueid!
  filter(p_uniqueid %in% enx$p_uniqueid) %>% 
  
  # Connect to negex
  inner_join(
    enx
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
    # TODO bring this one back
    # ees_partyname_orig,
    # Dependent Variable
    d_ptv = ptv, 
    # p_prcEP19,
    
    # Independent Variables
    p_negativity, 
    p_incivility, #negativity_difference_lr, uncivil_difference_lr,
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
    )
  


# 5. Saving analysis dataset -------------------------------------------------
vroom::vroom_write(tempdf, glue("data/{Sys.Date()}_Analysisfile.csv"))
vroom::vroom_write(enx, glue("data/{Sys.Date()}_EPEES-file.csv"))


