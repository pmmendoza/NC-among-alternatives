# Title: NC-among-alternatives (PhD Study I)
# Component: step 2/4
# Context: [Hostile Campaigning Project]
# Author: Philipp M.
# Annotations:
#   [R] for Robustness checks
#   [SC] for Sample Cuts
# 
# 
# 1. setup -----------------------------------------
## 1.1 packages and functions ---------------------------------------------
# loading / installing packages depending on whether it is already installed.
pacman::p_load(
  tidyverse, 
  tidylog,
  inops,
  glue,
  inops
  )

# helper functions for this project
source("99_utils.R")


## 1.2 loading data -------------------------------------------------------
# EPEES_19 dataset
enx <- read_csv("data/01_EPEES19-cleaned.csv")

# Cleaned EES dataset
ees <- read_csv(file = "data/01_EES-cleaned.csv")
ees_parties <- read_csv(file = "data/01_EES_parties-cleaned.csv")

# the linkage file created in '01_party-matching-EPEES-EES.R'
linkage <- read_csv("data/01_Linkage-matched-parties.csv")


# 2. recoding -------------------------------------------------------------
## 2.1 matching potential -------------------------------------------------
# Parties that I cannot match due to EPEES
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
sc_noepees <- left_join(temp, ees_parties) %>% select(cntry_short, ees_party_id, ees_partyname_engl, epees_id_num)
write_csv(sc_noepees, "data/02_parties unmatched due to EPEES.csv")


# [SC] unmatched by EPEES ----------------------------------------------------
enx <- filter(enx, !is.na(ees_party_id))
# dropping 12 parties

## 2.2 party-level --------------------------------------------------------
### 2.2.1 party ideology --------------------------------------------------------
enx <- 
  enx %>% 
  mutate(
    # create a nice party label
    party_lab = glue("{party_acro} [{cntry_short}]"),
    
    # negativity is the reversed tonality; values: [0 = exclusively positive; 10 = exclusively negative]
    p_negativity = (-tone+10)/2,
    
    # incivility remains as is [0 = most civil; 10 = most uncivil]
    p_incivility = uncivil,
    
    # for the ordering of labels in a visualisation
    p_harshness = ((p_negativity+10)/2 + p_incivility)/2, 
    
    # ideological position of parties
    # left-right position from wikipedia: [0 = far left;10 = far right]
    p_lrpos_wiki = p_position/13*10,
    
    # EU position from EPEES Expert Judgements
    # 1) evaluating EU integration
    # 2) evaluating functioning of EU
    p_eupos = select(., euitegr, eufunct) %>% rowMeans(na.rm = T), # [0 = against EU; 10 = pro EU]
    
    # [R] party ideological extremity [0; 5]
    p_eupos_extreme = abs(p_eupos-5),
    p_lrpos_extreme_wiki = abs(p_lrpos_wiki-5),
    
    # Incumbency status
    p_incumbency = p_NatGov19,
    
    # number of respondents for this party
    p_numresp = numresp,
    
    # vote share at last election
    p_prcEP19 = p_prcEP19,
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


### 2.2.2 inter-party ideological distances --------------------------------------------------------
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


### 2.2.3 [R] neg. and inciv. adjusted by expert ideology ---------------------------------------------
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


### 2.2.4 neg. and inciv. of next closest competitor -----------------------------------------------
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


## 2.3 election-level ----------------------------------------------------
enx <-
  enx %>%
  group_by(cntry_short_be) %>%
  mutate(
    e_negativity = mean(p_negativity, na.rm=T),
    e_incivility = mean(p_incivility, na.rm=T),
    e_adj_incivility = mean(p_resid_unciv, na.rm=T),
    e_adj_negativity = mean(p_resid_neg, na.rm=T),
  ) %>% ungroup %>% 
  mutate(
    # Effective number of parties
    e_ENP = ENP,
    # Region in europe
    e_region = region,
  )


## 2.4 individual-level ----------------------------------------------------
## 2.4.1 L-R position of respondent ----------------------------------------------------
ees <- 
  ees %>% 
  mutate(
    # resp identifier
    i_unique = paste0(cntry_short, respid),
    
    # left-right position and extremity of voter 
    i_lrpos = ifelse(Q11>10, NA, Q11),
    i_extremity_lr = abs(i_lrpos-5),
    
    # control variables
    i_yrbrn = D4_1, # year born
    
    # [R] Following the election in the media
    i_followelections = Q8 %>% as.numeric %>% replace(.>10, NA),
  )


## 2.4.2 EU position of respondent ----------------------------------------------------
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


## 2.4.3 perceived ideol. distances to parties --------------------------------------------------------
ees <- ees %>% 
  mutate_at(vars(starts_with("q13")), ~replace(., which(.>10), NA)) %>% 
  mutate(across(starts_with("q13_"), ~abs(. - i_lrpos), .names = "i_dist2party_{str_remove(.col, 'q13_')}"))


## 2.4.4 obj ideol distances to parties --------------------------------------------------------
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
base <- combo[1] %>% str_extract("wiki")

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


## 2.5 [SC] N of experts --------------------------------------------------
# kick parties with less than 3 coders EPEES
enx$epees_id[enx$p_numresp<3]
# =>  we lose Luxembourg entirely
# 6 rows are kicked out
enx <-
  enx %>% 
  filter(p_numresp>=3) %>% 
  mutate(
    cntry_short = cntry_short_be,
    p_uniqueid = p_uniqueid_be
    )


## 2.6 [SC] stacking data --------------------------------------------------------
# ALl control variables
cntrls <- c("i_gender",
            "i_yrbrn", 
            "i_edulvl", 
            "i_pinterest",
            "i_eupos",
            "i_lrpos",
            "i_extremity_eu",
            "i_extremity_lr",
            "p_incumbency",
            "p_lrpos_wiki",
            "p_eupos",
            "p_eupos_extreme",
            "p_neg_close_ecombo",
            "p_inciv_close_ecombo",
            "p_neg_adj_close_ecombo",
            "p_inciv_adj_close_ecombo",
            "p_numresp",
            "p_lrpos_extreme_wiki",
            "e_negativity",
            "e_incivility",
            "e_adj_incivility",
            "e_adj_negativity",
            "e_region"
            )

# Connecting EPEES to EES
temp <-
  ees %>% 
  select(
    i_unique, cntry_short,
    i_lrpos, 
    i_followelections,
    matches("Q10_"),
    matches("i_dist2party_"),
    all_of(cntrls %[out~% "p_|e_"), # all individual-level controls
         ) %>% 
  # Convert main DV into numeric
  mutate_at(vars(matches("Q10_")), as.numeric, na.rm = T) %>% 
  pivot_longer(names_to = "ees_partycode", values_to = "ptv", cols = matches("Q10_")) %>% 
  mutate(
    ees_partycode = ees_partycode %>% str_replace("q10", "party"),
    p_uniqueid_be = str_c(cntry_short, "_", ees_partycode),
    ) %>% 
  # Remove DV NA rows
  filter(ptv <= 10)  %>%
  
  # Remove parties not covered in EPEES
  filter(p_uniqueid_be %in% enx$p_uniqueid_be) %>% 
  
  # Connect to EPEES
  inner_join(enx)

temp %>% 
    summarise(
      countries = n_distinct(cntry_short_be),
      parties = n_distinct(p_uniqueid_be),
      respondents = n_distinct(i_unique),
      dyads = n()
    ) %>% print()


## 2.7 dyadic measures --------
### 2.7.1 L-R distance 2 sponsor--------
temp <-
  temp %>% 
  rowwise %>% 
  mutate(
    # two-dim distance to sponsor
    d_combodist_ewiki = sqrt(
        (i_lrpos - p_lrpos_wiki)^2 +
        (i_eupos - p_eupos)^2
      ),
    
    # PTV for sponsor
    d_ptv = ptv,
  )


### 2.7.2 perceived L-R distance 2 sponsor --------
temp$d_percdist <- 
  temp %>% 
  mutate(
    ees_indicator = ees_partycode %>% str_remove("party_") %>% as.numeric
  ) %>% 
  select(starts_with("i_dist2party_"), ees_indicator) %>% 
  apply(1, function(x){x[x["ees_indicator"]]})


### 2.7.3 number of interposing parties --------
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
      "i_dist2party_{i}" := ifelse(ees_partycode_num == i, NA, .data[[glue("i_dist2party_{i}")]]),
    )
}

# based on perceived distances
temp$d_ninterpos_perc <-
  temp %>%
  select(one_of(paste0("i_dist2party_",1:9)), d_percdist) %>%
  apply(1, function(x){sum(x[1:9]<=x["d_percdist"], na.rm = T)})

# turn into a binary variable
temp <- temp %>% 
  mutate(
    # only valid for observations that have a perceived distance value!
    d_ninterpos_perc = ifelse(is.na(d_percdist), NA, d_ninterpos_perc),
    d_ninterpos_perc_bi_num =
      case_when(
        d_ninterpos_perc > 0 ~ 1,
        d_ninterpos_perc == 0 ~ 0,
        T ~ NA_real_
      ),
    d_ninterpos_perc_bi =
      case_when(
        d_ninterpos_perc > 0 ~ "Better alternatives available",
        d_ninterpos_perc == 0 ~ "No better alternatives",
        T ~ NA_character_
      ),
  )


# 3. last sample cuts --------------------------------------------------------
cntrls <- c(cntrls, "d_combodist_ewiki") %>% sort()

# Analysis dataset
tempdf <- 
  temp %>%
  select(
    # Expert variables
    starts_with("exp_"), 
    
    # Control variables
    one_of(cntrls),
    # Nesting structure variables
    cntry_short, 
    ees_partyname_orig,
    ees_partyname_engl,
    p_uniqueid = p_uniqueid_be, 
    i_unique,
    party_acro, party_lab,

    # Dependent Variable
    d_ptv, 
    p_prcEP19,
    
    # Independent Variables
    p_negativity, 
    p_incivility, 
    p_harshness,
    ## adjusted measures
    p_resid_neg,
    p_resid_unciv,
    
    # Moderation Variables
    e_ENP, 
    p_combo_wiki_emindist,
    d_ninterpos_perc_bi,
    d_ninterpos_perc_bi_num,
    i_followelections, # robustness
    )

# 4. saving analysis dataset -------------------------------------------------
vroom::vroom_write(tempdf, glue("data/02_Analysisfile.csv"))
vroom::vroom_write(enx, glue("data/02_EPEES-file.csv"))