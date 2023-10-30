# 0. file description -----------------------------------------------------
# This code links and matches parties in the Voter Component of the European
# Election Study (EES) to parties in the EPEES_2019, the
# 2019 European Parliament Election Expert Survey dataset.

# The matching is done in two steps.
# 1. matching of parties via partyfacts.
#    The EPEES dataset has a CHES id. Via the partyfacts database (https://partyfacts.herokuapp.com/)
#    We can link this CHES id to the parties' ids for the previous edition of the EES14
#    Whenever a EES party's EES19 id is also contained in the EES14, we complete the match.
# 2. manual matching.
#    The remaining, unmatched parties in the EPEES were matched manually with EES19
#    party ids. These matches are available in the repository under 'data/00_manual-matches-EPEESxEES.csv'

# 1. setup -------------------------------------------------------------------
# change the following environment variable to this script's project folder path.
Sys.setenv(WD = getwd())

# Set working directory
setwd(Sys.getenv("WD"))

# loading packages
# packages installed but not loaded:
# haven, pacman, sjlabelled;
pacman::p_load(
  tidyverse,
  tidylog,
  inops,
  glue,
  stringdist,
  beepr,
  clipr
)

# load additional functions needed for this project
source("00_utils.R")


# ├ loading data ------------------------------------------------------
# create the necessary folder structure
if (!dir.exists("data")) {
  dir.create("data")
}

# downloading the EPEES_19 dataset
if (!file.exists("data/EPEES_19_datapart.dta")) {
  download.file(
    url = "https://osf.io/download/bzm48/",
    destfile = "data/EPEES_19_datapart.dta"
  )
}

# loading the EPEES_19 dataset
enx <- haven::read_dta("data/EPEES_19_datapart.dta")

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
parties <- read_csv(glue("data/ZA7581_cp.csv"), local = locale(encoding = "CP1250"), name_repair = "universal")


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

# country iso look-up table
iso2 <- read_csv("data/eu_cntry_iso2.csv")
iso2dict <- setNames(iso2$Code, iso2$Country)

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
  )

# EES - voter dataset:
ees <-
  ees %>%
  mutate(
    # To character
    cntry = countrycode %>% sjlabelled::as_character(),
    # Specific incongruencies
    cntry = ifelse(cntry == "Czech Rep.", "Czech Republic", cntry),
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
    cntry_short = iso2dict[cntry],

    # CHES id
    CHES_id = as.character(CHESpid),

    # read out the party acronym
    party_acro = partycountry_acro %>% str_remove_all(" \\(.*\\)"),

    # Create an EPEES id
    epees_id = str_c(cntry_short, "_", party_acro),
    epees_id_num = str_c(cntry_short, "_", str_remove(partycode, "party")),
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

# Add the relevant columns to the EPEES dataset: ees14_id, partyfacts_id, px_country, px_name_english, px_name
# TODO: check if this still works

enx1 <-
  enx %>%
  left_join(link1) %>%
  mutate(
    regionbe = case_when(
      px_name %in~% "Travali de Belgique" ~ "W",
      px_name %in~% "van de Arbeid" ~ "F",
      T ~ "other"
    ),
    
    # make country codes unique
    cntry_short = case_when(
      regionbe == "Flanders" ~ "BEF",
      regionbe == "Wallonia" ~ "BEW",
      T ~ cntry_short)
  )
# 54 are still without matches in the enx data! [documentation]
# duplicates detection
# only EPEES parties without CHES ids remain as duplicates at this point.
# enx1 %>% group_by(CHES_id, regionbe) %>% identicals_df()

# also add these ids to the central linkage file
link2 <- link1 %>%
  left_join(
    enx %>% select(epees_id, epees_id_num, CHES_id)
  )


# Add the linkage data to the EES data set
ees_parties1 <-
  ees_parties %>%
  # mistake in this dataset
  mutate(
    ees_party_id = case_when(
      Party_name_questionnaire %in~% "TOP" ~ "1203530",
      Party_name_questionnaire %in~% "LSDP" ~ "1440320",
      name_engl %in~% "Lithuanian Social Democratic" ~ "1440320",
      name_engl == "Social Democratic Labour Party" ~ "1752320",
      # name_engl == "Lithuanian Social Democratic Party" ~ "1752320",
      T ~ ees_party_id
    ),
  ) %>%
  left_join(link1 %>% rename(ees_party_id = ees14_id))
# 128 parties are matched
# 80 parties are left unmatched

# If the ees14 id for a specific party is also in the ees19 dataset, then use
# this id for the matching file
link2 <- link1 %>%
  mutate(
    ees_party_id = ifelse(ees14_id %in% ees_parties1$ees_party_id, ees14_id, NA)
  )


# Act II - manual matching ------------------------------------------------
# Here I tried to manually find a match in the EPEES dataset for all unmatched
# parties in the EES voter study. Some parties remain unmatched.

manual_matches <- read_csv("data/00_manual-matches-EPEESxEES.csv")

# Assign the manual matches if possible.
ees_parties2 <-
  manual_matches %>%
  mutate(ees_party_id = as.character(ees_party_id)) %>%
  select(cntry_short, ees_party_id, epees_id_num_new, ees_party_name) %>%
  right_join(ees_parties1) %>%
  mutate(
    epees_id_num = ifelse(!is.na(epees_id_num_new), epees_id_num_new, epees_id_num)
  ) %>%
  select(-epees_id_num_new)
# 54 fewer NA

is.na(ees_parties2$epees_id_num) %>% sum()
# 26 remain unmatched


# ├ final linkage file ------------------------------------------------------
link_full <-
  full_join(
    enx1 %>%
      filter(!ees14_id == "1620314" | is.na(ees14_id)) %>%
      select(
        cntry_short,
        epees_id_num, epees_id,
        ees14_id,
        EPEES_name = partyname, EPEES_acro = party_acro,
        partyfacts_id,
        CHES_id
      ),
    ees_parties2 %>%
      # filter(!is.na(epees_id_num)) %>%
      select(
        cntry_short,
        Region,
        EES_name = name_engl,
        EES_name_orig = Party_name_questionnaire,
        ees_party_id,
        ees_party_id_num,
        epees_id_num,
      )
  ) %>%
  arrange(cntry_short, epees_id_num)

# adjust a wrong duplication
link_full <-
  link_full %>%
  # remove two mismatches
  filter(
    !((EES_name_orig %in~% "\\(PVDA/PTB\\)" & ees14_id == "1056335") |
      (EES_name_orig %in~% "\\(PTB\\)" & ees14_id == "1056325"))
  )

# final edits
link_full <-
  link_full %>%
  # re-arrange columns
  relocate(
    cntry_short,
    epees_id, epees_id_num,
    EPEES_name, EPEES_acro,
    ees_party_id, ees14_id,
    EES_name, EES_name_orig
  ) %>%
  # calculate similartiy of party names
  mutate(
    # create unique party_ids for ees
    p_uniqueid = paste0(cntry_short, "_party_", ees_party_id_num),
  )


# ├ final check for duplicates ------------------------------------------------------
# One duplicate remains in Bulgaria: United Patriots in EPEES are IMRO and ATAKA in EES
link_full %>%
  filter(!is.na(epees_id_num)) %>%
  group_by(cntry_short, epees_id_num, regionbe) %>%
  identicals_df()


# 3. save the matchfile and the harmonised files ------------------------------------------------------
# All parties
link_full %>%
  write_csv("data/00 2023-10-18_Linkage-all-parties.csv")

# Only matched
link_full %>%
  filter(!is.na(ees_party_id), !is.na(epees_id_num)) %>%
  write_csv("data/00 2023-10-18_Linkage-matched-parties.csv")


# ├ write the cleaned data sets back to disk ------------------------------------------------------
write_csv(enx1 %>% select(-starts_with("px_")), file = "data/EPEES19-cleaned.csv")
write_csv(ees, file = "data/EES-cleaned.csv")

# check diffs between enx and enx1
names(enx1) %[out% names(enx)
enx1 %>% select(cntry_short) %>% table()

