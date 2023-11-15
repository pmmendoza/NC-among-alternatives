# Pre-processing Data Cleaning
# This code is called from within "02_data-wrangling.R"
# It does the following steps for the EPEES:
#   - converts labelled values into character
#   - harmonizes country labels
#   - corrects an EPEES country-code mistake
#   - creates an EPEES party id
#   - adds EES variables from the previously created linkage file to the dataset
# Following to the EES dataset:
#   - converts the country label to character
#   - splits belgium up in Flandern and Wallonia
#   - harmonizes country names


# 0. setup -------------------------------------------------------------------
# Global Variables
# TODO: this should be adjusted to be relative to the current wd
# DATADIR <- "/Users/p.m.mendozauva.nl/OneDrive - UvA/07 Datasets/"

# ├ loading data ------------------------------------------------------
# the linkage file created in '01_party-matching-EPEES-EES.R'
linkage <- read_csv("data/00 2023-10-18_Linkage-matched-parties.csv")

# EPEES_19 dataset
# TODO this should only be the newly created enx datafile
enx <- read_csv("data/EPEES19-cleaned.csv")
enx_lab <- haven::read_dta("data/EPEES_19_datapart.dta")
# enx <- haven::read_dta("data/EP2019_data_parties_v2.dta")

# Cleaned EES dataset
ees <- read_csv(file = "data/EES-cleaned.csv")
ees_lab <- haven::read_dta(glue("data/ZA7581_v2-0-1.dta"))

# 1. data harmonisation ---------------------------------------------------
# adjusting the country identifier



# TODO include the regional abbreviation in the country_short
# TODO move most of this into the party-matching file?

# Ad Linkage info to EPEES data




enx1 <-
# temp <- 
  enx %>%
  # because the updated version with the regional variable for belgium and a different name for the UK is coming from the linkage file
  select(-cntry_short) %>%  
  # filter(epees_id %in% linkage$epees_id) %>%
  left_join(
    linkage %>% select(
      cntry_short, epees_id, #epees_id_num, # match keys
      regionbe,
      ees_party_id, # EES variables
      # CHES_id, 
      ees14_id,
      enx_party = EPEES_name,
      ees_partycode = ees_party_id_num,
      ees_partyname_engl = EES_name,
      ees_partyname_orig = EES_name_orig,
      p_uniqueid,
      ),
    # by = c("epees_id", "cntry_short")
    ) %>% 
  mutate(ees_partycode = as.character(ees_partycode)) %>% 
  relocate(cntry_short, regionbe, enx_party_old = partyname)# %>% 
# joined by epees_id
# from 191 rows to 194; 
# 18 only in enx, 6 only in y





# double-check the duplicates and unmatched parties
enx1 %>% group_by(epees_id) %>% 
  identicals_df

# Dropped parties
enx %>%
  filter(partyname %out% enx1$partyname) %>%
  select(cntry_short, partyname) %>%
  mutate(partyname = str_c(partyname, " [",cntry_short ,"]")) %>%
  pull(partyname) %>% paste0(collapse = ", ") %>% write_clip


# 2. European Election Study (2019) - Voters ---------------------------------------
ees <- haven::read_dta(glue("{DATADIR}EES 2019/ZA7581_v2-0-1.dta"))


# Make a character variable                                     => DONE
ees$cntry <- ees$countrycode %>% sjlabelled::as_character()

# Changing some incongruent labels                              => DONE
ees$cntry[ees$cntry == "Czech Rep."] <- "Czech Republic"

# Create a short country label for the EES dataset                                     => DONE
temp <- enx %>% select(cntry, cntry_short) %>% unique
cntryabbrevs <- temp$cntry_short
names(cntryabbrevs) <- temp$cntry
ees$cntry_short <- cntryabbrevs[ees$cntry]

# Create a region variable to account for Belgium
ees <- 
  ees %>% 
  mutate(
    regionbe = case_when(
      meta_lang_be == 1 ~ "Flanders",
      meta_lang_be == 2 ~ "Wallonia",
      T ~ "other"
    ),
    cntry_short = case_when(
      regionbe == "Flanders" ~ "BEF",
      regionbe == "Wallonia" ~ "BEW",
      T ~ cntry_short
    ),
    i_unique = paste0(cntry_short, respid)
  ) 

enx <- enx1
rm(list = c("enx1", "temp", "linkage", "cntryabbrevs"))

