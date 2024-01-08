# Title: NC-among-alternatives (PhD Study I)
# Component: step 1/4
# Context: [Hostile Campaigning Project]
# Author: Philipp M.
# Description:
#   This code links and matches parties in the Voter Component of the European
#   Election Study (EES) to parties in the EPEES_2019, the
#   2019 European Parliament Election Expert Survey dataset.
#   The matching is done in two steps:
#   1. matching of parties via partyfacts.
#      The EPEES dataset has a CHES id. Via the partyfacts database (https://partyfacts.herokuapp.com/)
#      We can link this CHES id to the parties' ids for the previous edition of the EES14
#      Whenever a EES party's EES19 id is also contained in the EES14, we complete the match.
#   2. manual matching.
#      The remaining, unmatched parties in the EPEES were matched manually with EES19
#      party ids. These matches are available in the repository under 'data/00_manual-matches-EPEESxEES.csv'
# 
# 
# 1. setup -------------------------------------------------------------------
# Set working directory
Sys.setenv(WD = getwd()) # change the following environment variable to this script's project folder path.
setwd(Sys.getenv("WD"))

# loading packages
pacman::p_load(
  tidyverse,
  tidylog,
  inops,
  glue,
  stringdist
)

# load additional functions needed for this project
source("99_utils.R")


# ├ loading data ------------------------------------------------------
# create the necessary folder structure
if (!dir.exists("data")) {
  dir.create("data")
}

# downloading the EPEES_19 dataset
EPEESFILE <- "data/EPEES19_data.dta"
if (!file.exists(EPEESFILE)) {
  download.file(
    # TODO adjust this to my own repo!
    url = "https://osf.io/download/s9qjh/",
    destfile = EPEESFILE
  )
}

# loading the EPEES_19 dataset
enx <- haven::read_dta(EPEESFILE)

# check if the EES datasets exist already
if (!file.exists("data/ZA7581_v2-0-1.dta") | !file.exists("data/ZA7581_cp.csv")) {
  stop(paste0(
    "To be able to run this code you first need to manually download the EES datasets from the GESIS data archive.\n",
    "After logging in to the Gesis data archive you can access the data here: https://search.gesis.org/research_data/ZA7581\n",
    "Download the following two files and place them in the 'data' folder of your project directory.\n",
    "1) 'ZA7581_v2-0-1.dta' - The full dataset. You can find this under 'Downloads > Datasets\n'",
    "2) 'ZA7581_cp.csv' - The party dataset. You can find this under 'Downloads > Other documents'"
  ))
}

# load the EES Voter dataset
ees <- haven::read_dta(glue("data/ZA7581_v2-0-1.dta"))

# load the EES Parties dataset
parties <- read_csv(glue("data/ZA7581_cp.csv"), locale = locale(encoding = "CP1250"), name_repair = "universal")

# download partyfacts data
if (!file.exists("data/partyfacts-core-parties.csv")) {
  download.file(
    url = "https://partyfacts.herokuapp.com/download/core-parties-csv",
    destfile = "data/partyfacts-core-parties.csv"
  )
}
if (!file.exists("data/partyfacts-external-parties.csv")) {
  download.file(
    url = "https://partyfacts.herokuapp.com/download/external-parties-csv",
    destfile = "data/partyfacts-external-parties.csv"
  )
}

# read in both partyfacts data sets
partyfcore <- read_csv(glue("data/partyfacts-core-parties.csv"))
partyfext <- read_csv(glue("data/partyfacts-external-parties.csv"))


# 2. cleaning / harmonising data -----------------------------------------------------------
# EES - party dataset:
ees_parties <- parties %>%
  # change ids to character
  mutate_at(vars(contains("EES")), as.character) %>%
  # only focus on those parties that respondents were surveyed PTVs for
  filter(!is.na(Q10_PTV)) %>%
  # focus on relevant variables
  select(
    cntry_short = Coutnry_short,
    Region,
    Party_name_questionnaire,
    name_engl = English.name,
    ees_party_code_num = Unifed_party_code,
    ees_party_id = Q2_EES,
    ees_party_id_num = Unifed_party_code,
  ) %>%
  # correct some wrong codes in this dataset which emerged when comparing the provided dataset 
  # with the codebook.
  mutate(
    # regionbe = Region %>% case_when(name_engl %in~% "Green"),
    ees_party_id = case_when(
      Party_name_questionnaire %in~% "TOP" ~ "1203530", # this one was missing!
      Party_name_questionnaire %in~% "LSDP" ~ "1440320", # this one was mistakenly coded with the EES id of 1440620 which is of the TS-LKD party
      name_engl %in~% "Lithuanian Social Democratic" ~ "1440320",
      name_engl == "Social Democratic Labour Party" ~ "1752320", # this was 1752700, should be 1752320
      T ~ ees_party_id
    ),
  )

# EPEES dataset:
enx <-
  enx %>%
  mutate(
    cntry = country %>%
      sjlabelled::as_character() %>%
      str_remove_all("[0-9]*\\. "),

    # Specific incongruencies
    cntry = ifelse(cntry == "The Netherlands", "Netherlands", cntry),
    cntry = ifelse(cntry == "UK", "United Kingdom", cntry),

    # create a country acronym
    cntry_short = str_match(partycountry_acro, "\\((.*)\\)") %>% .[,2],
    cntry_short = case_when(
      cntry == "United Kingdom" ~ "UK", 
      # The Finnish Christian Democrats have the wrong country abbreviation (SE in stead of FI)
      cntry == "Finland" ~ "FI",
      # The Irish Sin Fein have the wrong country abbreviation (UK in stead of IE)
      cntry == "Ireland" ~ "IE",
      T ~ cntry_short
    ),
    
    # region indicator
    region = region %>% sjlabelled::as_character(),
    
    # CHES id
    CHES_id = as.character(CHESpid),

    # read out the party acronym
    party_acro = partycountry_acro %>% str_remove_all(" \\(.*\\)"),

    # Create an EPEES id
    epees_id = str_c(cntry_short, "_", party_acro),
    epees_id_num = str_c(cntry_short, "_", str_remove(partycode, "party")),
  )

# This Czech party seems to be missing some vote shares!
enx$p_prcEP19[enx$epees_id == "CZ_TOP09"] <- 11.65

cntry_dict <- enx %>% 
  select(cntry, cntry_short) %>% distinct %>% 
  with(setNames(object = cntry_short, nm = cntry))

# EES - voter dataset:
ees <-
  ees %>%
  mutate(
    # country names from labels to character
    cntry = countrycode %>% sjlabelled::as_character(),
    # Specific incongruencies
    cntry = ifelse(cntry == "Czech Rep.", "Czech Republic", cntry),
    
    # cntry_short
    cntry_short = cntry_dict[cntry],
    
    # Flandern and Wallonia differentiation
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
    i_unique = paste0(cntry_short, respid),
    
    # saving the labels of some individual-level variables
    i_pinterest = na_if(Q21, 99) %>% na_if(98) %>% sjlabelled::as_character(), 
    i_edulvl = EDU %>% na_if(99) %>% na_if(97) %>% sjlabelled::as_character(),
    i_gender = D3 %>% sjlabelled::as_character(),
  )


# Act I - connect EES to EPEES via EES14 > partyfacts > CHES ----------------------------------------------------------------------
# The aim of this code is to get an EES party id for every party in EPEES.
# The EPEES dataset has a CHES party id which I can connect to EES14 party ids
# via the partyfacts dataset.

# match the EES14 section with the CHES section
link1 <-
  inner_join(
    # EES 14
    partyfext %>%
      # get the EES14 section of tpartyfacts
      filter(dataset_key == "ees14") %>%
      select(
        country,
        ees14_id = dataset_party_id, # rename the ID variable
        name, name_english, partyfacts_id
      ),

    # CHES
    partyfext %>%
      # get the CHES section of partyfacts
      filter(dataset_key == "ches") %>%
      select(country, CHES_id = dataset_party_id, partyfacts_id) %>%
      # only the ones in EPEES19
      filter(CHES_id %in% enx$CHESpid)
  ) %>%
  select(
    ees14_id, CHES_id, partyfacts_id,
    px_country = country,
    px_name_english = name_english, px_name = name
  )
# 139 matches!

# NOTE: 2 duplicates due to the Belgium Worker's party and a coalition in Portugal.
# 1. We will split flandern and wallonia in the analysis but use the same expert
#    ratings for both components of the party.
# 2. For Portugal the members of the coalition PSD, CDS-PP are separately evaluated
#    in the EPEES but also come up as a coalition in the EES parties dataset.
#       - Centro Democrático Social/Partido Popular) (Coal. (PSD + CDS-PP))
#       - Partido Social Democrata (PSD)
#    What matters for the PTV is only the party,
#    => we can drop the coalition from our linkage file.
link1 <- link1 %>% filter(!ees14_id == 1620314)

# If the ees14 id for a specific party is also in the ees19 dataset, then use
# this id for the matching file
link1 <- link1 %>%
  mutate(
    ees_party_id = ifelse(ees14_id %in% ees_parties$ees_party_id, ees14_id, NA)
  )

# Add the relevant columns to the EPEES dataset: ees_party_id, partyfacts_id, px_country, px_name_english, px_name
enx1 <-
  # Matching by CHES_id!
  link1 %>% 
  select(CHES_id, ees_party_id, partyfacts_id, px_country, px_name_english, px_name) %>% 
  right_join(., enx) %>%
  mutate(
    regionbe = case_when(
      px_name %in~% "Arbeid" & cntry == "Belgium" ~ "Flanders",
      px_name %in~% "Travail de Belgique" ~ "Wallonia",
      T ~ NA_character_
    )
  )
# (0) 54 138 192 (only in x, only in y, matched, total)
# 54 are still without matches in the enx data! [documentation]
# enx1 %>% group_by(CHES_id, regionbe) %>% identicals_df()
# only 29 EPEES parties without CHES ids remain as duplicates at this point.

# also add the EPEES ids to the central linkage file
link2 <- link1 %>%
  left_join(
    enx1 %>% select(epees_id, epees_id_num, CHES_id, cntry_short, px_name#, regionbe
                    )
  )
# 0 54 138 138

# Add the linkage data (especially the epees_id_num) to the EES data set
ees_parties1 <-
  ees_parties %>%
  left_join(link2 %>% select(ees_party_id, epees_id_num) %>% filter(!is.na(ees_party_id)))
# 80 parties are left unmatched
# 128 parties are matched


# Act II - manual matching ------------------------------------------------
# Here I tried to manually find a match in the EPEES dataset for all unmatched
# parties in the EES voter study. Some parties remain unmatched.
manual_matches <- read_csv("data/00_manual-matches-EPEESxEES.csv")

# add manual matches to link
link3 <- 
  full_join(link2, 
          manual_matches %>% 
            mutate(ees_party_id = as.character(ees_party_id)) %>% 
            rename(epees_id_num = epees_id_num_new)
          )
# 135 80 3 218

# add link back to EES!
ees_parties2 <-
  link3 %>%
  # only take those entries which appear in both datasets
  filter(!is.na(ees_party_id)) %>% 
  filter(!is.na(epees_id_num)) %>% 
  # select the relevant columns
  select(cntry_short, ees_party_id, epees_id_num_new = epees_id_num, ees_party_name) %>%
  # add the matches to the ees party dataset!
  right_join(ees_parties1) %>%
  # where the epees id was missing, add the newly added one
  mutate(
    epees_id_num = ifelse(!is.na(epees_id_num_new), epees_id_num_new, epees_id_num),
    # introduce the cntry_short differentiation for Belgium!
    cntry_short = case_when(
      Region == "Flanders" ~ "BEF",
      Region == "Wallonia" ~ "BEW",
      T ~ cntry_short
    ),
    ees_partycode = ees_party_code_num %>% paste0("party_",.)
  ) %>% 
  rename(
    ees_partyname_orig = Party_name_questionnaire,
    ees_partyname_engl = name_engl
    ) %>% 
  select(-epees_id_num_new, -ees_party_name) %>% arrange(cntry_short)

# get the differentiation of belgium in here already
link3 <- left_join(
  link3,
  ees_parties2 %>%
    select(ees_party_id, cntry_short_be = cntry_short, ees_partycode, ees_partyname_orig, ees_partyname_engl) %>%
    filter(!is.na(ees_party_id))
) %>%
  mutate(cntry_short_be = ifelse(is.na(cntry_short_be), cntry_short, cntry_short_be))

sum(is.na(ees_parties2$epees_id_num))
# 26 EES parties remain unmatched by an EPEES party
# 12 (0) 206 218

# bind back to EPEES
enx2 <- link3 %>%
  # only take those entries which appear in both datasets
  filter(!is.na(ees_party_id)) %>%
  filter(!is.na(epees_id_num)) %>%
  select(cntry_short, cntry_short_be, ees_partycode,
         epees_id_num, ees_party_id, ees_partyname_orig,
         ees_partyname_engl,
         #ees_party_id_new = ees_party_id, 
         ees_party_name) %>% 
  right_join(enx1 %>% select(-ees_party_id)) %>% 
  # (0)
  # 12
  # 184
  # 196
  # here the workers party got unnecessarily duplicated.
  filter(!(regionbe == "Flanders" & ees_party_id == "1056335")|is.na(ees_party_id)) %>% 
  filter(!(regionbe == "Wallonia" & ees_party_id == "1056325")|is.na(ees_party_id)) %>% 
  relocate(cntry, cntry_short, epees_id_num, epees_id, regionbe, ees_party_id, partyname, partycountry_acro) %>% 
  arrange(epees_id_num) %>% 
  mutate(
    p_uniqueid = paste0(cntry_short, "_",ees_partycode),
    p_uniqueid_be = paste0(cntry_short_be, "_",ees_partycode)
    )

# ├ final linkage file ------------------------------------------------------
link_full <-
  full_join(
    enx2 %>%
      filter(!is.na(ees_party_id)) %>%
      select(
        # regionbe,
        cntry_short,
        cntry_short_be,
        epees_id_num, epees_id,
        epees_party = partyname, 
        epees_acro = party_acro,
        partyfacts_id,
        CHES_id,
        ees_party_id
      ),
    ees_parties2 %>%
      filter(!is.na(epees_id_num)) %>%
      select(
        cntry_short_be = cntry_short,
        regionbe=Region,
        ees_partyname_engl,
        ees_partyname_orig,
        ees_party_id,
        ees_partycode,
        epees_id_num,
        ees_party_code_num
      )
  ) %>%
  arrange(cntry_short, epees_id_num) %>%
  # re-arrange columns
  relocate(
    cntry_short,
    cntry_short_be,
    epees_id, epees_id_num,
    epees_party, epees_acro,
    ees_party_id,
    ees_partyname_engl,
    ees_partyname_orig
  ) %>%
  # calculate similartiy of party names
  mutate(
    # create unique party_ids for ees
    p_uniqueid = paste0(cntry_short, "_party_", ees_party_code_num),
    p_uniqueid_be = paste0(cntry_short_be, "_party_", ees_party_code_num),
  )
# 182 perfect matches.


# ├ final check for duplicates ------------------------------------------------------
# One duplicate remains in Bulgaria: United Patriots in EPEES are IMRO and ATAKA in EES
link_full %>%
  filter(!is.na(epees_id_num)) %>%
  group_by(cntry_short, epees_id_num, regionbe) %>%
  identicals_df()


# 3. save the matchfile and the harmonised files ------------------------------------------------------
# All parties
link_full %>%
  write_csv("data/01_Linkage-all-parties.csv")

# Only matched
link_full %>%
  filter(!is.na(ees_party_id), !is.na(epees_id_num)) %>%
  write_csv("data/01_Linkage-matched-parties.csv")


# ├ write the cleaned data sets back to disk ------------------------------------------------------
# ENX cannot have the new cntry_short because it doesn't differentiate parties enough in Belgium
write_csv(enx2 %>% select(-starts_with("px_")), file = "data/01_EPEES19-cleaned.csv")
write_csv(ees_parties2, file = "data/01_EES_parties-cleaned.csv")
write_csv(ees, file = "data/01_EES-cleaned.csv")
