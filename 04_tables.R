# Title: NC-among-alternatives (PhD Study I)
# Component: step 2/4
# Context: [Hostile Campaigning Project]
# Author: Philipp M.
# Annotations:
#   [T] Table output
# Note:  
#   This code only works after letting the analysis code run through once
#   as it requires the lmer mixed model items that should be displayed in the table
#
#
# 0. setup ----------------------------------------------------------------
# ├ packages ------------------------------------------------------
pacman::p_load(
  'officer',
  'officedown',
  'kableExtra',
  'modelsummary',
  'officer',
  'flextable'
)

# Change the following environment variable to this script's project folder path.
Sys.setenv(WD = getwd())

# Set working directory
setwd(Sys.getenv("WD"))


# ├ define template ----------------------------------------------
template <- "tables/00 Paper template.docx"


# 1. relevant regression models -------------------------------------------------------
# Here I create the regression tables for all models that are displayed in the
# main text. Regression tables for robustness checks are being left out.

# ├ all coefficients and their labels! ----
coef_map <- c(
  "(Intercept)" = "intercept",
  # IV
  "p_negativity" = "Negativity [P]",
  "p_incivility" = "Incivility [P]",
  "p_resid_neg" = "Negativity (adj) [P]",
  "p_resid_unciv" = "Incivility (adj) [P]",

  # Interaction
  ## System
  "p_negativity:e_ENP" = "Negativity * ENP [P]",
  "p_incivility:e_ENP" = "Incivility * ENP [P]",
  "p_resid_neg:e_ENP" = "Negativity (adj) * ENP [P]",
  "p_resid_unciv:e_ENP" = "Incivility (adj) * ENP [P]",

  ## Party
  "p_negativity:p_combo_wiki_emindist" = "Negativity * Ideol. dist. to closest competitor[P]",
  "p_incivility:p_combo_wiki_emindist" = "Incivility * Ideol. dist. to closest competitor[P]",
  "p_resid_neg:p_combo_wiki_emindist" = "Negativity (adj) * Ideol. dist. to closest competitor[P]",
  "p_resid_unciv:p_combo_wiki_emindist" = "Incivility (adj) * Ideol. dist. to closest competitor[P]",

  ## Voter
  "p_negativity:d_ninterpos_perc_biNo better alternatives" = "Negativity *\n no better alternatives [D]",
  "p_incivility:d_ninterpos_perc_biNo better alternatives" = "Incivility *\n no better alternatives [D]",
  "p_resid_neg:d_ninterpos_perc_biNo better alternatives" = "Negativity (adj) *\n no better alternatives [D]",
  "p_resid_unciv:d_ninterpos_perc_biNo better alternatives" = "Incivility (adj) *\n no better alternatives [D]",

  ## 3-way interaction
  # for the model with ideological distance and following the elections
  "p_negativity:d_ninterpos_perc_bi_num:d_combodist_ewiki" = "Negativity *\n better alternatives *\n ideol. distance to attacker[D]",
  "p_incivility:d_ninterpos_perc_bi_num:d_combodist_ewiki" = "Incivility *\n better alternatives *\n ideol. distance to attacker[D]",
  "p_negativity:d_ninterpos_perc_bi_num" = "Negativity *\n better alternatives [D]",
  "p_incivility:d_ninterpos_perc_bi_num" = "Incivility *\n better alternatives [D]",
  "p_negativity:d_combodist_ewiki" = "Negativity *\n ideol. distance to attacker [D]",
  "p_incivility:d_combodist_ewiki" = "Incivility *\n ideol. distance to attacker [D]",
  "d_ninterpos_perc_bi_num:d_combodist_ewiki" = "Better alternatives *\n ideol. distance to attacker[D]",

  # Moderators
  "d_ninterpos_perc_biNo better alternatives" = "No better alternatives [D]",
  "d_ninterpos_perc_bi_num" = "No better alternatives [D]",
  "p_combo_wiki_emindist" = "ideol. distance to closest competitor [P]",
  "d_combodist_ewiki" = "Ideological distance to attacker [D]",
  "e_ENP" = "ENP [E]",

  # Following the elections
  "p_negativity:d_ninterpos_perc_bi_num:i_followelections" = "Negativity *\nfollowing the elections *\nno better alternatives [D]",
  "p_incivility:d_ninterpos_perc_bi_num:i_followelections" = "Incivility *\nfollowing the elections *\nno better alternatives [D]",
  "p_negativity:i_followelections" = "Negativity * following the elections [I]",
  "p_incivility:i_followelections" = "Incivility * following the elections [I]",
  "d_ninterpos_perc_bi_num:i_followelections" = "following the elections *\nno better alternatives [D]",
  "i_followelections" = "following the elections [I]",

  # CONTROLS
  # Individual lvl
  "i_genderMale" = "gender (male, ref = female) [I]",
  "i_genderOther" = "gender (other, ref = female) [I]",
  "i_yrbrn" = "birthyear [I]",
  'i_edulvl16-19 years (="Medium")' = "medium education (ref = lower) [I]",
  'i_edulvl20 + (="High")' = "high education (ref = lower) [I]",
  "i_pinterestNot at all" = "political int. - lowest (ref. a little) [I]",
  "i_pinterestSomewhat" = "political int. - 2nd highest (ref. a little) [I]",
  "i_pinterestVery" = "political int. - highest (ref. a little) [I]",
  "i_lrpos" = "L-R position [I]",
  "i_extremity_lr" = "L-R extremity [I]",
  "i_extremity_eu" = "EU position extremity [I]",
  "i_eupos" = "EU position [I]",

  # party lvl
  "p_neg_close_ecombo" = "Negativity of closest competitor [P]",
  "p_inciv_close_ecombo" = "Incivility of closest competitor [P]",
  "p_neg_adj_close_ecombo" = "Negativity (adj) of closest competitor [P]",
  "p_inciv_adj_close_ecombo" = "Incivility (adj) of closest competitor [P]",
  "p_lrpos_wiki" = "L-R position[P]",
  "p_lrpos_extreme_wiki" = "L-R extremity [P]",
  "p_eupos" = "EU position [P]",
  "p_eupos_extreme" = "EU position extremity[P]",
  "p_incumbency" = "incumbency [P]",

  # System level
  "e_negativity" = "Negativity of election [E]",
  "e_incivility" = "Incivility of election [E]",
  "e_adj_negativity" = "Negativity (adj) of election [E]",
  "e_adj_incivility" = "Incivility (adj) of election [E]",
  "e_region2. North" = "region North (ref = East) [E]",
  "e_region3. South" = "region South (ref = East) [E]",
  "e_region4. West" = "region West (ref = East) [E]",

  # Expert controls
  "exp_lrscale" = "experts' L-R scale [E]",
  "exp_perceptEU" = "experts' perception of EU [E]",
  "exp_familiar" = "experts' familiarity [E]",
  "exp_easy" = "experts' easiness [E]",
  "exp_citizen" = "experts' citizen [E]",
  "exp_female" = "experts' % female [E]",

  # Other stuff
  "cor__(Intercept).p_negativity" = "correlation\n(ran. intercepts and neg. slopes)",
  "cor__(Intercept).p_incivility" = "correlation\n(ran. intercepts and inc. slopes)",
  "cor__(Intercept).p_resid_neg" = "correlation\n(ran. intercepts and neg. slopes)",
  "cor__(Intercept).p_resid_unciv" = "correlation\n(ran. intercepts and inc. slopes)",
  "sd__(Intercept)" = "variation of country-level random intercepts",
  # "sd__(Intercept)" = "variance of party-level random intercepts",
  # "sd__(Intercept)" = "variance of voter-level random intercepts",
  "sd__Observation" = "level 1 residual variation",
  "sd__p_negativity" = "variation of negativity slope (cntry)",
  "sd__p_incivility" = "variation of incivility slope (cntry)",
  "sd__p_resid_neg" = " variation negativity slope (cntry)",
  "sd__p_resid_unciv" = " variation incivility slope (cntry)",
  "Cor (Intercept~p_negativity cntry_short)" = "correlation\n(ran. intercepts and neg. slopes)",
  "Cor (Intercept~p_incivility cntry_short)" = "correlation\n(ran. intercepts and inc. slopes)",
  "Cor (Intercept~p_resid_neg cntry_short)" = "correlation\n(ran. intercepts and neg. slopes)",
  "Cor (Intercept~p_resid_unciv cntry_short)" = "correlation\n(ran. intercepts and inc. slopes)",
  "SD (Intercept cntry_short)" = "variation of country-level random intercepts",
  "SD (Intercept p_uniqueid)" = "variation of party-level random intercepts",
  "SD (Intercept i_unique)" = "variation of voter-level random intercepts",
  "SD (Observations)" = "variation of level 1 residual",
  "SD (p_negativity cntry_short)" = "variation of negativity slope (cntry)",
  "SD (p_incivility cntry_short)" = "variation of incivility slope (cntry)",
  "SD (p_resid_neg cntry_short)" = "variation of negativity slope (cntry)",
  "SD (p_resid_unciv cntry_short)" = "variation of incivility slope (cntry)"
)

# Models that are in the main text
models <-
  list(
    m_ind_noideol_neg,
    m_ind_noideol_inc,
    m_party_neg,
    m_party_inc,
    m_e_neg,
    m_e_inc,
    # additional analyses
    m_indiv_int_ideol_neg,
    m_indiv_int_ideol_inc,
    m_indiv_int_follow_inc,
    m_indiv_int_follow_neg,
    # votes as DV
    m_party_r_votes_neg,
    m_party_r_votes_inc,
    m_system_neg_votes,
    m_system_inc_votes,
    # Adjusted vars
    m_ind_noideol_neg_adj,
    m_ind_noideol_inc_adj,
    m_party_r_adj_neg,
    m_party_r_adj_inc,
    m_e_neg_adj,
    m_e_inc_adj
  )


# Creating the tables ----------------------------------------------------------
# Define relevant model parameters to ad to table
gm <- gof_map %>%
  select(raw, clean, fmt) %>%
  filter(raw %in% c(
    "nobs", "aic", "bic", "logLik", "r2.marginal" # , "df.residual", "icc"
  )) %>%
  mutate(
    clean = case_when(
      clean == "Num.Obs." ~ "N",
      T ~ clean,
    )
  )

# ├ [T] main models ----------------------------------------------------------------
# [Table 4]
nicelyformatted_table(
  models = list(
    "Voters (neg)" = m_ind_noideol_neg,
    "Voters (inc)" = m_ind_noideol_inc,
    "Parties (neg)" = m_party_neg,
    "Parties (inc)" = m_party_inc,
    "Systems (neg)" = m_e_neg,
    "Systems (inc)" = m_e_inc
  ),
  hlinepos1 = 42,
  hlinepos2 = 50,
  FONT = FONT,
  output = "tables/04_all_std_models.docx",
  coef_map = coef_map,
  group_map = c(
    "Fixed effects",
    "cntry_short" = "System-level",
    "p_uniqueid" = "Party-level",
    "i_unique" = "Respondent-level",
    "Residual" = "Dyad (residual)"
  )
)


# ├ [T] 3-way interactions ---------------------------------------------------------
# [Table 5]
nicelyformatted_table(
  hlinepos1 = 48,
  hlinepos2 = 52,
  models = list(
    "Voters by ideologigal dist. to sponsor (negativity)" = m_indiv_int_ideol_neg,
    "Voters by ideologigal dist. to sponsor (incivility)" = m_indiv_int_ideol_inc,
    "Voters by following the election (negativity)" = m_indiv_int_follow_neg,
    "Voters by following the election (incivility)" = m_indiv_int_follow_inc
  ),
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/05_models_ideol_follow.docx",
  group_map = c(
    "Fixed effects",
    "cntry_short" = "System-level",
    "p_uniqueid" = "Party-level",
    "i_unique" = "Respondent-level",
    "Residual" = "Dyad (residual)"
  )
)


# ├ [T] with Adjusted variables ----------------------------------------------------
# [Table 6]
nicelyformatted_table(
  models = list(
    "Voters (neg)" = m_ind_noideol_neg_adj,
    "Voters (inc)" = m_ind_noideol_inc_adj,
    "Parties (neg)" = m_party_r_adj_neg,
    "Parties (inc)" = m_party_r_adj_inc,
    "Systems (neg)" = m_e_neg_adj,
    "Systems (inc)" = m_e_inc_adj
  ),
  hlinepos1 = 42,
  hlinepos2 = 50,
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/06_all_std_models_adj.docx",
  group_map = c(
    "Fixed effects",
    "cntry_short" = "System-level",
    "p_uniqueid" = "Party-level",
    "i_unique" = "Respondent-level",
    "Residual" = "Dyad (residual)"
  )
)


# ├ [T] votes as DV ----------------------------------------------------------------
# [Table 7]
nicelyformatted_table(
  models = list(
    "Parties (neg)" = m_party_r_votes_neg,
    "Parties (inc)" = m_party_r_votes_inc,
    "Systems (neg)" = m_system_neg_votes,
    "Systems (inc)" = m_system_inc_votes
  ),
  hlinepos1 = 27,
  hlinepos2 = 33,
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/07_all_std_models_votes.docx",
  group_map = c(
    "Fixed effects",
    "cntry_short" = "System-level",
    "Residual" = "Party (residual)"
  )
)