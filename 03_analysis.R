# Title: NC-among-alternatives (PhD Study I)
# Component: step 3/4
# Context: [Hostile Campaigning Project]
# Author: Philipp M.
# Annotations:
#   [T] Table output
#   [F] Figure output
#   [SC] Sample Cut
#   [R] Robustness Check
# Note: To create the tables for the respective regression models, also run 04_tables with the objects created from this code!
#
#
# 1. setup ---------------------------------------------------------------------
# Change the following environment variable to this script's project folder path.
Sys.setenv(WD = getwd())

# Set working directory
setwd(Sys.getenv("WD"))

# Load packages
pacman::p_load(
  # data wrangling
  tidyverse,
  tidylog,
  inops,
  glue,
  # modelling
  lme4,
  lmerTest,
  performance,
  # loading data
  vroom,
  # plotting
  gridtext,
  gridExtra,
  gtable,
  grid
)

# load additional functions needed for this project
source("99_utils.R")

# interplot function from the interplot package but adapted for a three-way interaction
source("99_interplot-3-way.R")


# ├ pre-sets for visualisations ------------------------------------------------
# define font for plots
FONT <- "EB Garamond" # for paper word paper version

# set standardised theme
mingara <- theme_minimal() +
  theme(
    text = element_text(
      family = FONT,
      color = "black"
    ),
    axis.text = element_text(
      family = FONT,
      color = "black"
    ),
  )
theme_set(mingara)
update_geom_defaults("text", list(colour = "black", family = theme_get()$text$family))


# ├ create the necessary folder structure --------------------------------------
folders <- c("plots", "tables", glue("plots/{FONT}"))
walk(folders, ~ {
  if (!dir.exists(.x)) {
    dir.create(.x)
  }
})


# ├ load data ------------------------------------------------------------------
# Check if the relevant files are already created, alternatively downlaod them 
# from OSF
DATA1 <- "data/02_Analysisfile.csv"
DATA2 <- "data/02_EPEES-file.csv"

# ANALYSIS DATA
if (!file.exists(DATA1)) {
  if(readline(glue("The dataset {DATA1} is not in the current data directory, should it be downloaded from the OSF repository?[T/F]"))){
    download.file(
      url = "https://osf.io/download/p6zd2/",
      destfile = DATA1
    )
  }
}

# EPEES
if (!file.exists(DATA2)) {
  if(readline(glue("The dataset {DATA2} is not in the current data directory, should it be downloaded from the OSF repository?[T/F]"))){
    download.file(
      url = "https://osf.io/download/5dtvp/",
      destfile = DATA2
    )
  }
}

# Load the analysis file
tempdf <- vroom(file = DATA1)

# Load the EPEES data set
enx <- vroom(file = DATA2)


# 2. rescaling -----------------------------------------------------------------
# For better fitting of models, all numeric covariates that do not require to
# be interpreted meaningfully are standardised.

# define variables that are numeric and don't need to be interpreted
stdsvars <-
  names(select_if(tempdf, is.numeric)) %[out~%
  c(
    "d_ptv",
    "p_prcEP19",
    "p_incivility",
    "p_negativity",
    "d_",
    "p_combo_wiki_emindist",
    "i_followelections",
    "e_ENP"
  )

# rescale them
tempdf_scale1 <-
  tempdf %>%
  select(-p_prcEP19, -party_acro) %>%
  mutate_at(vars(all_of(stdsvars)), scale2)


# ├ [SC, T] filter out outliers and incomplete cases ---------------------------
tempdf_scale <-
  tempdf_scale1 %>%
  # filter out any cases that lie outside of the 1.5 x interquartile range
  filter_if(
    is.numeric,
    any_vars(!outliers(., times = 1.5))
  ) %>%
  {
    . ->> sc1
  } %>%
  # filter out incomplete cases
  tidylog::filter(complete.cases(.)) %>%
  {
    . ->> sc2
  } %>%
  # filter out respondents disclosing less than 2 ptvs
  group_by(i_unique) %>%
  tidylog::filter(n() > 2) %>%
  ungroup()

# [Table 2] This table is manually created.
scs <- list(
  "orig" = tempdf_scale1,
  "sc1" = sc1,
  "sc2" = sc2,
  "analysis" = tempdf_scale
) %>%
  map(~ {
    .x %>% summarise(
      countries = n_distinct(cntry_short),
      parties = n_distinct(p_uniqueid),
      respondents = n_distinct(i_unique),
      dyads = n()
    )
  }) %>%
  enframe() %>%
  unnest_wider(value)
write_csv(scs, file = "tables/02_samplecuts.csv")


# ├ party-level df -------------------------------------------------------------
# Given that all interesting variation in the party- and system-level models
# is at the party- and system-level (but not at the individual-level), we
# aggregate the dataset here to the party-level.

party_df <-
  tempdf_scale %>%
  select(
    d_ptv, party_lab,
    cntry_short, p_uniqueid,
    ees_partyname_orig,
    p_harshness,
    p_negativity,
    p_incivility,
    p_resid_neg,
    p_resid_unciv,
    p_neg_close_ecombo,
    p_inciv_close_ecombo,
    p_neg_adj_close_ecombo,
    p_inciv_adj_close_ecombo,
    p_combo_wiki_emindist,
    p_lrpos_wiki, p_lrpos_extreme_wiki, p_eupos, p_eupos_extreme,
    p_incumbency,
    e_region, e_ENP,
    e_negativity, e_incivility,
    e_adj_negativity, e_adj_incivility,
    exp_lrscale, exp_perceptEU, exp_familiar, exp_easy, exp_citizen, exp_female,
  ) %>%
  group_by(across(c(-d_ptv))) %>%
  summarise(p_ptv = mean(d_ptv)) %>%
  left_join(
    enx %>% select(cntry_short, p_uniqueid, p_prcEP19)
  ) %>%
  mutate(
    p_prcEP19 = p_prcEP19 / 100,
  ) %>%
  ungroup() %>%
  # bind the party names back to the df
  left_join(enx %>% select(ees_partyname_orig, cntry_short, party_acro))


# 3. [T] descriptive stats table ---------------------------------------------------
# So that all the pictures of the mini plots are saved properly for knitting
descvars <- tempdf_scale %>%
  ungroup() %>%
  select(
    # Dependent Variable
    d_ptv,
    # Independent Variables
    p_negativity,
    p_incivility,
    # Moderation Variables
    e_ENP,
    p_combo_wiki_emindist,
    d_ninterpos_perc_bi_num,
  ) %>%
  {
    as.list(.) ->> varlists
  } %>%
  skimr::skim() %>%
  select(
    skim_variable,
    mean = numeric.mean,
    sd = numeric.sd,
    min = numeric.p0,
    max = numeric.p100
  ) %>%
  mutate(
    variable = c(
      "PTV",
      "negativity",
      "incivility",
      "ENP",
      "party ideol. dist.",
      "availability of altern."
    ),
    role = c(
      "dv",
      "iv",
      "iv",
      "int.",
      "int.",
      "int."
    ),
    level = c(
      "dyad", "party", "party",
      "election", "party", "dyad"
    ),
    boxplot = "",
    histogram = ""
  ) %>%
  select(-skim_variable) %>%
  relocate(variable, role, level)

# [Table 1]
# Knitting to a pdf document
descvars %>%
  mutate(
    boxplot = "", 
    histogram = "") %>% 
  mutate_all(kableExtra::linebreak) %>% 
  kableExtra::kbl(booktabs = T, 
                  digits = 2, 
                  caption = "Descriptive Statistics of main variables", 
                  escape = F) %>% 
  kableExtra::kable_paper(full_width = F, html_font = FONT) %>% 
  kableExtra::row_spec(0, extra_css = "border-bottom: 1px solid", bold = F) %>%
  kableExtra::row_spec(6, extra_css = "border-bottom: 1px solid", bold = F) %>%
  kableExtra::add_footnote("See the next table for the number of valid observations per level.") %>% 
  kableExtra::column_spec(8, image = kableExtra::spec_boxplot(varlists)) %>%
  kableExtra::column_spec(9, image = kableExtra::spec_hist(varlists)) %>%
  kableExtra::save_kable("tables/01_descriptives.pdf")


# 4. visualisation of both dimensions ------------------------------------------
p1 <- party_df %>%
  mutate(
    p_prcEP19 = p_prcEP19 * 100,
  ) %>%
  arrange(-p_harshness) %>%
  mutate(
    harshrank = seq_len(nrow(party_df)),
    party_lab = glue("{party_acro} [{cntry_short}]"),
    party_lab = case_when(
      harshrank < 6 ~ party_lab,
      harshrank > (nrow(party_df) - 5) ~ party_lab,
      T ~ NA_character_
    )
  ) %>%
  select(p_negativity, p_incivility, p_harshness, party_lab, p_prcEP19, p_ptv) %>%
  ggplot(aes(x = p_negativity, y = p_incivility)) +
  theme(legend.title = element_text(size = 9)) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_size_area(breaks = c(10, 30, 50)) +
  geom_point(aes(
    size = p_prcEP19
  ), alpha = 0.5) +
  ggrepel::geom_label_repel(aes(label = party_lab), family = FONT, size = 3) +
  guides(colour = guide_legend(title.hjust = 0.5)) +
  labs(
    x = "Negativity (10 = exclusively negative campaigning)", y = "Incivility (10 = very uncivil campaigning)", # colour = "Negativity Index",
    size = "% of votes\nEP 2019"
  )

# [Figure 1]
save_plot(plot = p1, file = glue("plots/{FONT}/01_dist_inciv-neg-harsh_{FONT}.png"))


# Exemplary visualisation of inter-party distances for Greece
p8 <- enx %>%
  filter(cntry_short == "GR") %>%
  select(party_acro, ees_partyname_engl, p_eupos, p_lrpos_wiki) %>%
  mutate(
    partyn =
      glue("{ees_partyname_engl} [{party_acro}]") %>%
        str_wrap(width = 25, indent = 0, exdent = 0)
  ) %>%
  ggplot(aes(x = p_lrpos_wiki, y = p_eupos, colour = partyn)) +
  geom_label(aes(label = party_acro)) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme(legend.position = "right", legend.spacing = unit(2, "cm")) +
  guides(colour = guide_legend(byrow = TRUE)) +
  labs(x = "L-R position", y = "EU position", colour = "Party")

# [Figure 8]
save_plot(plot = p8, file = glue("plots/{FONT}/08_mindist-for-greece.png"))



# 5. random slopes and variance decomposition ----------------------------------
# ├ [T] variance decomposition ----
# empty model
m0 <-
  lmer(
    formula = d_ptv ~ (1 | cntry_short) + (1 | p_uniqueid) + (1 | i_unique),
    verbose = 100,
    data = tempdf_scale,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
beepr::beep(10)

variances <-
  getvariances(
    model = summary(m0),
    lvlss = c("cntry_short", "p_uniqueid")
  ) %>%
  mutate(
    `Levels of aggregation` = c("Elections", "Parties", "Respondents", "Party-Respondent Dyads"),
    Variance = round(Variance, 2)
  )

# Knitting to PDF
# [Table 3]
variances %>%
  kableExtra::kbl() %>%
  kableExtra::kable_paper(full_width = F, html_font = FONT) %>%
  kableExtra::row_spec(0, extra_css = "border-bottom: 1px solid", bold = F) %>%
  kableExtra::row_spec(nrow(variances), extra_css = "border-bottom: 1px solid") %>%
  kableExtra::save_kable("tables/03_variances.pdf")


# ├ [F] random slopes with ptvs ------------------------------------------------
# Visualise random intercepts at country_level WITH PTV

# Running the random slope models by dimension
# negativity
m1_n <-
  lmer(
    formula = glue("d_ptv ~ p_negativity + (p_negativity | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Incivility
m1_i <-
  lmer(
    formula = glue("d_ptv ~ p_incivility + (p_incivility | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Adjusted
# negativity
m1_adj_n <-
  lmer(
    formula = glue("d_ptv ~ p_resid_neg + (p_resid_neg | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Incivility
m1_adj_i <-
  lmer(
    formula = glue("d_ptv ~ p_resid_unciv + (p_resid_unciv | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

## Robustness checks
# with all controls
# negativity
m1c_n <-
  lmer(
    formula = glue(
      "d_ptv ~
      i_gender + i_yrbrn + i_edulvl + i_pinterest +
      i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
      p_lrpos_wiki + p_lrpos_extreme_wiki +
      p_eupos + p_eupos_extreme +
      p_neg_close_ecombo +
      p_incumbency +
      exp_lrscale + exp_perceptEU +
      exp_familiar + exp_easy + exp_citizen + exp_female +
      e_region + e_negativity +
      p_negativity + (p_negativity | cntry_short) + (1 | i_unique)"
    ),
    verbose = 100,
    data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Incivility
m1c_i <-
  lmer(
    formula = glue(
      "d_ptv ~
      i_gender + i_yrbrn + i_edulvl + i_pinterest +
      i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
      p_lrpos_wiki + p_lrpos_extreme_wiki +
      p_eupos + p_eupos_extreme +
      p_inciv_close_ecombo +
      p_incumbency +
      exp_lrscale + exp_perceptEU +
      exp_familiar + exp_easy + exp_citizen + exp_female +
      e_region + e_incivility +
      p_incivility + (p_incivility | cntry_short) + (1 | i_unique)"
    ),
    verbose = 100,
    data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
# 


# with all controls except for Euroscepticism
# negativity
m1cnoe_n <-
  lmer(
    formula = glue("d_ptv ~
                     i_gender + i_yrbrn + i_edulvl + i_pinterest +
  i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
  p_lrpos_wiki + p_lrpos_extreme_wiki +
  p_neg_close_ecombo +
  p_incumbency +
  exp_lrscale + exp_perceptEU +
  exp_familiar + exp_easy + exp_citizen + exp_female +
  e_region + e_negativity +
                   p_negativity + (p_negativity | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Incivility
m1cnoe_i <-
  lmer(
    formula = glue("d_ptv ~
                  i_gender + i_yrbrn + i_edulvl + i_pinterest +
                  i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
                  p_lrpos_wiki + p_lrpos_extreme_wiki +
                  p_inciv_close_ecombo +
                  p_incumbency +
                  exp_lrscale + exp_perceptEU +
                  exp_familiar + exp_easy + exp_citizen + exp_female +
                  e_region + e_incivility +
                  p_incivility + (p_incivility | cntry_short) + (1 | i_unique)"),
    verbose = 100,
    data = tempdf_scale,
    control = lmerControl(optimizer = "bobyqa")
  )

ranplot_main <- get_ran_slopes(
  m1 = m1_n,
  m2 = m1_i,
  t1 = "p_negativity",
  t2 = "p_incivility",
  l1 = "Slopes of Negativity",
  l2 = "Slopes of Incivility"
)

ranplot_controls <- get_ran_slopes(
  m1 = m1c_n,
  m2 = m1c_i,
  t1 = "p_negativity",
  t2 = "p_incivility",
  l1 = "Slopes of Negativity",
  l2 = "Slopes of Incivility"
)

ranplot_controls_noEU <- get_ran_slopes(
  m1 = m1cnoe_n,
  m2 = m1cnoe_i,
  t1 = "p_negativity",
  t2 = "p_incivility",
  l1 = "Slopes of Negativity",
  l2 = "Slopes of Incivility"
)

ranplot_adj <- get_ran_slopes(
  m1 = m1_adj_n,
  m2 = m1_adj_i,
  t1 = "p_resid_neg",
  t2 = "p_resid_unciv",
  l1 = "Slopes of Adjusted Negativity",
  l2 = "Slopes of Adjusted Incivility"
)

# [Figure 2]
save_plot(
  ranplot_main,
  height = 150,
  glue(
    "plots/{FONT}/02_random-slopes-by-cntry_{FONT}.png"
  )
)
# [Figure 13]
save_plot(
  ranplot_controls,
  height = 150,
  glue(
    "plots/{FONT}/13_random-slopes-by-cntry_cntrls_{FONT}.png"
  )
)
# [Figure 14]
save_plot(
  ranplot_controls_noEU,
  height = 150,
  glue(
    "plots/{FONT}/14_random-slopes-by-cntry_cntrls_noEU_{FONT}.png"
  )
)
beepr::beep(10)


# 6. individual-level ----------------------------------------------------------
# ├ [F] main model -------------------------------------------------------------
# NOTE: General Model Notes
# * Our main dependent variable is at the dyad-level: a voter's ptv score
#   for a specific party.
# * Our main independent variable is at the party-level,
#   a party's negativity / incivility of their overall electoral campaign in this election.
# * At the voter-level we are interested in how the effect of negativity differs
#   depending on whether a voter has a better alternative available to the specific
#   party in question. Accordingly, this main moderator is at the voter-dyad level.
#
# We only add random slopes to the level at which I expect to *explain variations in
# slopes*. It does not make sense to estimate random slopes at the lowest level,
# level 1, because there will only ever be one observation per group and thus no
# slope by definition!
#
# Thus we cannot estimate a RS at the dyad-level and doing so at the voter-level
# would not be meaningful.
# In short: If my moderator variable is at the dyad-level, my level 1,
# then there is no point in adding a random slope to the individual level.
# => At the voter-level we don't have to estimate any random slopes.

# Define the formula
forml_ind_neg <-
  "d_ptv ~ p_negativity*d_ninterpos_perc_bi +
  i_gender + i_yrbrn + i_edulvl + i_pinterest +
  i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
  p_lrpos_wiki + p_lrpos_extreme_wiki +
  p_eupos + p_eupos_extreme +
  p_neg_close_ecombo +
  p_incumbency +
  exp_lrscale + exp_perceptEU +
  exp_familiar + exp_easy + exp_citizen + exp_female +
  e_region + e_negativity +
  (1|cntry_short) +
  (1|p_uniqueid) +
  (1|i_unique)"

# The model for incivility is identical, only the main predictor changes.
forml_ind_inc <-
  forml_ind_neg %>%
  str_replace_all("negativity", "incivility") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_close_ecombo")

# Modelling
m_ind_noideol_neg <-
  lmer(
    formula = forml_ind_neg,
    verbose = 100,
    data = tempdf_scale,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)
# summary(m_ind_noideol_neg)

m_ind_noideol_inc <-
  lmer(
    formula = forml_ind_inc,
    verbose = 100,
    data = tempdf_scale,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Combined plot visualisation
p_int_ind_comb <- plot_interplot_combo(
  model1 = m_ind_noideol_neg,
  model2 = m_ind_noideol_inc,
  var1a = "p_negativity",
  var1b = "p_incivility",
  dim1 = "Negativity",
  dim2 = "Incivility",
  var2 = "d_ninterpos_perc_bi",
  xlab = "Campaign dimension",
  ylab = "Average marginal effect of campaign\ndimension on ptv for sponsor",
  collab = "Availability of alternatives"
)


# [Figure 5]
save_plot(plot = p_int_ind_comb, file = glue("plots/{FONT}/05_AME_individual_{FONT}.png"))


# ├ [R][F] model with ideol. --------------------------------------------------------
# NOTE:
# * In addition to the above-mentioned, as a robustness test, we add the voter's ideological distance to
#   the party in question as a third interaction component.
forml_ind_ideol_neg <-
  "d_ptv ~ p_negativity*d_ninterpos_perc_bi_num*d_combodist_ewiki +
  i_gender + i_yrbrn + i_edulvl + i_pinterest +
  i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
  p_lrpos_wiki + p_lrpos_extreme_wiki +
  p_eupos + p_eupos_extreme +
  p_neg_close_ecombo +
  p_incumbency +
  exp_lrscale + exp_perceptEU +
  exp_familiar + exp_easy + exp_citizen + exp_female +
  e_region + e_negativity +
  (1|cntry_short) +
  (1|p_uniqueid) +
  (1|i_unique)"

forml_ind_ideol_inc <-
  forml_ind_ideol_neg %>%
  str_replace_all("negativity", "incivility") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_close_ecombo")

# Estimating the Models
m_indiv_int_ideol_neg <-
  lme4::lmer(
    formula = forml_ind_ideol_neg,
    verbose = 100,
    data = tempdf_scale,
    REML = T, control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

m_indiv_int_ideol_inc <-
  lme4::lmer(
    formula = forml_ind_ideol_inc,
    verbose = 100,
    data = tempdf_scale,
    REML = T, control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)


# Plotting - 1. getting the data
# Define facet levels of ideological distance moderators
modelvls <-
  summary(tempdf_scale$d_combodist_ewiki)[c(2, 3, 5)] %>% # Quantiles
  c(0.01, .) %>% # Minimum value
  round(2)

plotdata_neg <- interplot.lmerMod_3(
  m = m_indiv_int_ideol_neg,
  var1 = "p_negativity",
  var2 = "d_ninterpos_perc_bi_num",
  var3 = "d_combodist_ewiki",
  plot = F,
  var3_vals = modelvls
)
beepr::beep(10)

plotdata_inc <- interplot.lmerMod_3(
  m = m_indiv_int_ideol_inc,
  var1 = "p_incivility",
  var2 = "d_ninterpos_perc_bi_num",
  var3 = "d_combodist_ewiki",
  plot = F,
  var3_vals = modelvls
)
beepr::beep(10)

## this here works!!
# temp <- sjPlot::get_model_data(model = m_indiv_int_ideol_neg, type = "est")

# Plotting - 2. creating the plot
plot_data <-
  plotdata_neg %>%
  mutate(dim = "Negativity") %>%
  full_join(., plotdata_inc) %>%
  mutate(dim = ifelse(is.na(dim), "Incivility", dim)) %>%
  mutate(
    x = ifelse(
      d_ninterpos_perc_bi_num == 0,
      "Sponsor is best alternative",
      "Better alternative(s) available"
    ) %>% fct_relevel("Sponsor is best alternative"),
    distance = case_when(
      d_combodist_ewiki == modelvls[1] ~ glue("Minimal Distance [{modelvls[1]}]"),
      d_combodist_ewiki == modelvls[2] ~ glue("25% quantile [{modelvls[2]}]"),
      d_combodist_ewiki == modelvls[3] ~ glue("50% quantile [{modelvls[3]}]"),
      d_combodist_ewiki == modelvls[4] ~ glue("75% quantile [{modelvls[4]}]"),
      T ~ NA_character_
    ) %>% as.factor() %>% fct_reorder(d_combodist_ewiki),
    dim = dim %>% fct_relevel("Negativity")
  )

p_indiv_int_ideol_comb3 <-
  plot_data %>%
  ggplot(aes(
    x = dim,
    y = coef,
    colour = x
  )) +
  geom_hline(aes(yintercept = 0), colour = "darkgray") +
  geom_point(aes(shape = x), position = position_dodge(0.5)) +
  scale_shape_manual(values = c("square", "triangle")) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
    width = 0.3,
    position = position_dodge(0.5)
  ) +
  facet_wrap(~distance, nrow = 1) +
  theme(legend.position = "top") +
  labs(
    x = "Campaign dimension by distance to the ideological distance to the sponsor",
    y = "Average marginal effect of campaign\ndimension on ptv for sponsor",
    colour = "Availability of\nbetter alternatives",
    shape = "Availability of\nbetter alternatives"
  )

# [Figure 06]
save_plot(
  file = glue("plots/{FONT}/06_AME_individual_byideol_{FONT}.png"),
  plot = p_indiv_int_ideol_comb3
)


# ├ [R][F] adjusted measures -----
# Creating the adapted formulas

forml_ind_neg_adj <-
  forml_ind_neg %>%
  str_replace_all("p_negativity", "p_resid_neg") %>%
  str_replace_all("p_neg_close_ecombo", "p_neg_adj_close_ecombo") %>%
  str_replace_all("e_negativity", "e_adj_negativity")
forml_ind_inc_adj <-
  forml_ind_neg %>%
  str_replace_all("p_negativity", "p_resid_unciv") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_adj_close_ecombo") %>%
  str_replace_all("e_negativity", "e_adj_incivility")

# Estimating the models
m_ind_noideol_neg_adj <-
  lmer(
    formula = forml_ind_neg_adj,
    verbose = 100,
    data = tempdf_scale,
    REML = T, control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

m_ind_noideol_inc_adj <-
  lmer(
    formula = forml_ind_inc_adj,
    verbose = 100,
    data = tempdf_scale,
    REML = T, control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Plotting combined model
p_int_ind_comb_adj <- plot_interplot_combo(
  model1 = m_ind_noideol_neg_adj,
  model2 = m_ind_noideol_inc_adj,
  var1a = "p_resid_neg",
  var1b = "p_resid_unciv",
  dim1 = "Adjusted Negativity",
  dim2 = "Adjusted Incivility",
  var2 = "d_ninterpos_perc_bi",
  xlab = "Campaign dimension",
  ylab = "Average marginal effect of campaign\ndimension on ptv for sponsor",
  collab = "Availability of alternatives"
)
beepr::beep(10)

# [Figure 10]
save_plot(plot = p_int_ind_comb_adj, file = glue("plots/{FONT}/10_AME_individual_adj_{FONT}.png"))


# ├ [R][F] by following the election -----
# Creating the formula
forml_ind_follow_neg <- forml_ind_ideol_neg %>% str_replace_all("d_combodist_ewiki", "i_followelections")
forml_ind_follow_inc <- forml_ind_ideol_inc %>% str_replace_all("d_combodist_ewiki", "i_followelections")

# Estimating the models
m_indiv_int_follow_neg <-
  lme4::lmer(
    formula = forml_ind_follow_neg,
    verbose = 100,
    data = tempdf_scale,
    REML = T, control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

m_indiv_int_follow_inc <-
  lme4::lmer(
    formula = forml_ind_follow_inc,
    verbose = 100,
    data = tempdf_scale,
    REML = T, control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Define levels of the moderator at which to estimate the AMEs
modelvls <-
  summary(tempdf_scale$i_followelections)[c(2, 3, 5)] %>% # Quantiles
  c(0, ., 10) %>% # min & max values
  round(2)

# Creating the plot data
plotdata_neg_follow <- interplot.lmerMod_3(
  m = m_indiv_int_follow_neg,
  var1 = "p_negativity",
  var2 = "d_ninterpos_perc_bi_num",
  var3 = "i_followelections",
  plot = F,
  var3_vals = modelvls
)
beepr::beep(10)

plotdata_inc_follow <- interplot.lmerMod_3(
  m = m_indiv_int_follow_inc,
  var1 = "p_incivility",
  var2 = "d_ninterpos_perc_bi_num",
  var3 = "i_followelections",
  plot = F,
  var3_vals = modelvls
)
beepr::beep(10)

# plotting
plot_data_follow <-
  plotdata_neg_follow %>%
  mutate(dim = "Negativity") %>%
  full_join(., plotdata_inc_follow) %>%
  mutate(dim = ifelse(is.na(dim), "Incivility", dim)) %>%
  mutate(
    x = ifelse(
      d_ninterpos_perc_bi_num == 0,
      "Sponsor is best alternative",
      "Better alternative(s) available"
    ) %>% fct_relevel("Sponsor is best alternative"),
    distance = case_when(
      i_followelections == modelvls[1] ~ glue("Did not follow\nthe campaign [{modelvls[1]}]"),
      i_followelections == modelvls[2] ~ glue("25% quantile [{modelvls[2]}]"),
      i_followelections == modelvls[3] ~ glue("50% quantile [{modelvls[3]}]"),
      i_followelections == modelvls[4] ~ glue("75% quantile [{modelvls[4]}]"),
      i_followelections == modelvls[5] ~ glue("Max campaign\nattention [{modelvls[5]}]"),
      T ~ NA_character_
    ) %>% as.factor() %>% fct_reorder(i_followelections),
    dim = dim %>% fct_relevel("Negativity")
  )

p_indiv_int_follow_comb3 <-
  plot_data_follow %>%
  ggplot(aes(
    x = dim,
    y = coef,
    colour = x
  )) +
  geom_hline(aes(yintercept = 0), colour = "darkgray") +
  geom_point(aes(shape = x), position = position_dodge(0.5)) +
  scale_shape_manual(values = c("square", "triangle")) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
    width = 0.3,
    position = position_dodge(0.5)
  ) +
  facet_wrap(~distance, nrow = 1) +
  theme(legend.position = "top") +
  labs(
    colour = "Availability of\nbetter alternatives",
    shape = "Availability of\nbetter alternatives",
    y = "Average marginal effect of campaign\ndimension on ptv for sponsor",
    x = "Campaign dimension by degree of following the elections"
  )

# [Figure 07]
save_plot(
  file = glue("plots/{FONT}/07_AME_individual_byfollow_{FONT}.png"),
  plot = p_indiv_int_follow_comb3
)


# 7. party-level ---------------------------------------------------------
# ├ [F] main model ----
forml_p_neg <-
  paste0(
    "p_ptv ~ ",
    paste(
      "p_negativity*p_combo_wiki_emindist",
      "p_lrpos_wiki",
      "p_lrpos_extreme_wiki",
      "p_eupos",
      "p_eupos_extreme",
      "p_neg_close_ecombo",
      "p_incumbency",
      "exp_lrscale",
      "exp_perceptEU",
      "exp_familiar",
      "exp_easy",
      "exp_citizen",
      "exp_female",
      "e_region",
      "e_negativity",
      "(1|cntry_short)",
      sep = " + "
    )
  )

forml_p_inc <- forml_p_neg %>%
  str_replace_all("negativity", "incivility") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_close_ecombo")

# Modelling
m_party_neg <-
  lmer(
    formula = forml_p_neg,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

m_party_inc <-
  lmer(
    formula = forml_p_inc,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)


# plotting
p_part_combo <-
  plot_inter_combo_lin(
    m_a = m_party_neg,
    m_b = m_party_inc,
    var1a = "p_negativity",
    var1b = "p_incivility",
    var2 = "p_combo_wiki_emindist",
    ymin = -0.5,
    ymax = 0.5
  )

# [Figure 04]
save_plot(
  p_part_combo,
  file = glue("plots/{FONT}/04_AME_party_{FONT}.png")
)


# ├ [R][F] adjusted ------------------------------------------------------
forml_p_neg_adj <-
  forml_p_neg %>%
  str_replace_all("p_negativity", "p_resid_neg") %>%
  str_replace_all("p_neg_close_ecombo", "p_neg_adj_close_ecombo") %>%
  str_replace_all("e_negativity", "e_adj_negativity")
forml_p_inc_adj <-
  forml_p_neg %>%
  str_replace_all("p_negativity", "p_resid_unciv") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_adj_close_ecombo") %>%
  str_replace_all("e_negativity", "e_adj_incivility")


# Negativity
m_party_r_adj_neg <-
  lmer(
    formula = forml_p_neg_adj,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Incivility
m_party_r_adj_inc <-
  lmer(
    formula = forml_p_inc_adj,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)


# Plotting
p_part_adj_combo <-
  plot_inter_combo_lin(
    m_a = m_party_r_adj_neg,
    m_b = m_party_r_adj_inc,
    var1alab = "Adjusted Negativity",
    var1blab = "Adjusted Incivility",
    var1a = "p_resid_neg",
    var1b = "p_resid_unciv",
    var2 = "p_combo_wiki_emindist",
    ymin = -0.64,
    ymax = 0.5
  )

# [Figure 11]
save_plot(
  p_part_adj_combo,
  file = glue("plots/{FONT}/11_AME_party_adj_{FONT}.png")
)


# ├ [R][F] vote  ------------------------------------------------------
forml_p_neg_vote <-
  forml_p_neg %>%
  str_replace_all("p_ptv", "p_prcEP19")
forml_p_inc_vote <-
  forml_p_inc %>%
  str_replace_all("p_ptv", "p_prcEP19")

m_party_r_votes_neg <-
  lmer(
    formula = forml_p_neg_vote,
    verbose = 100,
    data = party_df %>% mutate(p_prcEP19 = p_prcEP19 * 10),
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

m_party_r_votes_inc <-
  lmer(
    formula = forml_p_inc_vote,
    verbose = 100,
    data = party_df %>% mutate(p_prcEP19 = p_prcEP19 * 10),
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

p_part_combo_vote <-
  plot_inter_combo_lin(
    m_a = m_party_r_votes_neg,
    m_b = m_party_r_votes_inc,
    var1a = "p_negativity",
    var1b = "p_incivility",
    var2 = "p_combo_wiki_emindist",
    ymin = -0.62,
    ymax = 0.5,
    ylab = "Average marginal effect of campaign\ndimensions on % of votes in EP elections"
  )

# [Figure 12]
save_plot(
  p_part_combo_vote,
  file = glue("plots/{FONT}/12_AME_party_adj_vote_{FONT}.png")
)


# 8. system-level ---------------------------------------------------------
forml_e_neg <-
  paste0(
    "p_ptv ~ ",
    paste(
      "p_negativity*e_ENP",
      "p_lrpos_wiki",
      "p_lrpos_extreme_wiki",
      "p_eupos",
      "p_eupos_extreme",
      "p_neg_close_ecombo",
      "p_incumbency",
      "exp_lrscale",
      "exp_perceptEU",
      "exp_familiar",
      "exp_easy",
      "exp_citizen",
      "exp_female",
      "e_region",
      "e_negativity",
      "(p_negativity|cntry_short)",
      sep = " + "
    )
  )

forml_e_inc <- forml_e_neg %>%
  str_replace_all("negativity", "incivility") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_close_ecombo")


# ├ [F] main model ----
m_e_neg <-
  lmer(
    formula = forml_e_neg,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

m_e_inc <-
  lmer(
    formula = forml_e_inc,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Plotting
p_e_combo <-
  plot_inter_combo_lin(
    m_a = m_e_neg,
    m_b = m_e_inc,
    var1a = "p_negativity",
    var1b = "p_incivility",
    var2 = "e_ENP",
    ymin = -0.5,
    ymax = 0.5,
    xlab = "Effective Number of Parties"
  )

# [Figure 3]
save_plot(
  p_e_combo,
  file = glue("plots/{FONT}/03_AME_system_{FONT}.png")
)


# ├ [R][F] adjusted  ------------------------------------------------------
forml_e_neg_adj <-
  forml_e_neg %>%
  str_replace_all("p_negativity", "p_resid_neg") %>%
  str_replace_all("p_neg_close_ecombo", "p_neg_adj_close_ecombo") %>%
  str_replace_all("e_negativity", "e_adj_negativity")
forml_e_inc_adj <-
  forml_e_neg %>%
  str_replace_all("p_negativity", "p_resid_unciv") %>%
  str_replace_all("p_neg_close_ecombo", "p_inciv_adj_close_ecombo") %>%
  str_replace_all("e_negativity", "e_adj_incivility")

m_e_neg_adj <-
  lmer(
    formula = forml_e_neg_adj,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)
m_e_inc_adj <-
  lmer(
    formula = forml_e_inc_adj,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Plotting
p_e_combo_adj <-
  plot_inter_combo_lin(
    m_a = m_e_neg_adj,
    m_b = m_e_inc_adj,
    var1alab = "Adjusted Negativity",
    var1blab = "Adjusted Incivility",
    var1a = "p_resid_neg",
    var1b = "p_resid_unciv",
    var2 = "e_ENP",
    ymin = -0.5,
    ymax = 0.5,
    xlab = "Effective Number of Parties"
  )

# [Figure 15]
save_plot(
  p_e_combo_adj,
  file = glue("plots/{FONT}/15_AME_system_adj_{FONT}.png")
)


# ├ [R][F] vote-shares ----
# Creating the furmla
forml_e_neg_vote <-
  forml_e_neg %>%
  str_replace_all("p_ptv", "p_prcEP19")

forml_e_inc_vote <-
  forml_e_inc %>%
  str_replace_all("p_ptv", "p_prcEP19")

# Modelling
m_system_neg_votes <-
  lmer(
    formula = forml_e_neg_vote,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )

beepr::beep(10)

m_system_inc_votes <-
  lmer(
    formula = forml_e_inc_vote,
    verbose = 100,
    data = party_df,
    REML = T,
    control = lmerControl(optimizer = "bobyqa")
  )
beepr::beep(10)

# Plotting
p_e_combo_vote <-
  plot_inter_combo_lin(
    m_a = m_system_neg_votes,
    m_b = m_system_inc_votes,
    var1a = "p_negativity",
    var1b = "p_incivility",
    var2 = "e_ENP",
    ymin = -0.5,
    ymax = 0.5,
    ylab = "Average marginal effect of campaig\ndimension on % of votes in the EP19 elections",
    xlab = "Effective Number of Parties"
  )

# [Figure 16]
save_plot(
  p_e_combo_vote,
  file = glue("plots/{FONT}/16_AME_system_vote_{FONT}.png")
)