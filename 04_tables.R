# readme ------------------------------------------------------------------
# This code only works after letting the analysis code run through once
# as it requires the lmer mixed model items that should be displayed in the table

# 0. setup ----------------------------------------------------------------
# ├ packages ----
pacman::p_load(
  officer,
  officedown,
  kableExtra,
  modelsummary,
  officer,
  flextable
)

# Define working directory
setwd("/Users/p.m.mendozauva.nl/Library/CloudStorage/OneDrive-UvA/00 Work/01 ASCoR PhD/01 Study I/Git first paper")


# ├ define template ----
template <- "/Users/p.m.mendozauva.nl/Library/CloudStorage/OneDrive-UvA/00 Work/01 ASCoR PhD/01 Study I/Git first paper/tables/00 Paper template.docx"


# 1. relevant regression models -------------------------------------------------------
# Here I create the regression tables for all models that are displayed in the
# main text. Regression tables for robustness checks are being left out.

# ├ all coefficients and their labels! ----
coef_map_dirty <- c(
  "(Intercept)" = "intercept",
  # IV
  "p_negativity" = "Negativity [P]",
  "p_incivility" = "Incivility [P]",
  "p_resid_neg" = "Negativity (adj) [P]",
  "p_resid_unciv" = "Incivility (adj) [P]",

  # Interaction
  ## System
  "p_negativitye_ENP" = "Negativity * ENP [P]",
  "p_incivilitye_ENP" = "Incivility * ENP [P]",
  "p_resid_neg:e_ENP" = "Negativity (adj) * ENP [P]",
  "p_resid_unciv:e_ENP" = "Incivility (adj) * ENP [P]",

  ## Party
  "p_negativityp_combo_wiki_emindist" = "Negativity * Ideol. dist. to closest competitor[P]",
  "p_incivilityp_combo_wiki_emindist" = "Incivility * Ideol. dist. to closest competitor[P]",
  "p_resid_neg:p_combo_wiki_emindist" = "Negativity (adj) * Ideol. dist. to closest competitor[P]",
  "p_resid_unciv:p_combo_wiki_emindist" = "Incivility (adj) * Ideol. dist. to closest competitor[P]",

  ## Voter
  "p_negativityd_ninterpos_perc_biNo better alternatives" = "Negativity *\n no better alternatives [D]",
  "p_incivilityd_ninterpos_perc_biNo better alternatives" = "Incivility *\n no better alternatives [D]",
  "p_resid_neg:d_ninterpos_perc_biNo better alternatives" = "Negativity (adj) *\n no better alternatives [D]",
  "p_resid_unciv:d_ninterpos_perc_biNo better alternatives" = "Incivility (adj) *\n no better alternatives [D]",

  ## 3-way interaction
  # for the model with ideological distance and following the elections
  "p_negativityd_ninterpos_perc_bi_numd_combodist_ewiki" =
    "Negativity *\n better alternatives *\n ideol. distance to attacker[D]",
  "p_incivilityd_ninterpos_perc_bi_numd_combodist_ewiki" =
    "Incivility *\n better alternatives *\n ideol. distance to attacker[D]",
  "p_negativityd_ninterpos_perc_bi_num" = "Negativity *\n better alternatives [D]",
  "p_incivilityd_ninterpos_perc_bi_num" = "Incivility *\n better alternatives [D]",
  "p_negativityd_combodist_ewiki" = "Negativity *\n ideol. distance to attacker [D]",
  "p_incivilityd_combodist_ewiki" = "Incivility *\n ideol. distance to attacker [D]",
  "d_ninterpos_perc_bi_numd_combodist_ewiki" = "Better alternatives *\n ideol. distance to attacker[D]",

  # Moderators
  "d_ninterpos_perc_biNo better alternatives" = "No better alternatives [D]",
  "d_ninterpos_perc_bi_num" = "No better alternatives [D]",
  "p_combo_wiki_emindist" = "ideol. distance to closest competitor [P]",
  "d_combodist_ewiki" = "Ideological distance to attacker [D]",
  "e_ENP" = "ENP [E]",

  # Following the elections
  "p_negativityd_ninterpos_perc_bi_numi_followelections" = "Negativity *\nfollowing the elections *\nno better alternatives [D]",
  "p_incivilityd_ninterpos_perc_bi_numi_followelections" = "Incivility *\nfollowing the elections *\nno better alternatives [D]",
  "p_negativityi_followelections" = "Negativity * following the elections [I]",
  "p_incivilityi_followelections" = "Incivility * following the elections [I]",
  "d_ninterpos_perc_bi_numi_followelections" = "following the elections *\nno better alternatives [D]",
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
  "i_eupos" = "Position on EU [I]",

  # party lvl
  "p_neg_close_ecombo" = "Negativity of closest competitor [P]",
  "p_inciv_close_ecombo" = "Incivility of closest competitor [P]",
  "p_neg_adj_close_ecombo" = "Negativity (adj) of closest competitor [P]",
  "p_inciv_adj_close_ecombo" = "Incivility (adj) of closest competitor [P]",
  "p_lrpos_wiki" = "L-R position[P]",
  "p_lrpos_extreme_wiki" = "L-R extremity [P]",
  "p_eupos" = "EU position [P]",
  "p_eupos_extreme" = "EU extremity[P]",
  "p_incumbency" = "incumbency [P]",

  # System level
  "e_negativity" = "Negativity of election [E]",
  "e_incivility" = "Negativity of election [E]",
  "e_adj_negativity" = "Negativity (adj) of election [E]",
  "e_adj_incivility" = "Negativity (adj) of election [E]",
  "e_region2. North" = "region North [E]",
  "e_region3. South" = "region South [E]",
  "e_region4. West" = "region West [E]",

  # Expert controls
  "exp_lrscale" = "L-R scale [E]",
  "exp_perceptEU" = "perception of EU [E]",
  "exp_familiar" = "familiarity [E]",
  "exp_easy" = "easyness [E]",
  "exp_citizen" = "citizen [E]",
  "exp_female" = "female [E]",

  # Other stuff
  "Cor (Intercept~p_negativity cntry_short)" = "correlation\n(ran. intercepts and neg. slopes)",
  "Cor (Intercept~p_incivility cntry_short)" = "correlation\n(ran. intercepts and inc. slopes)",
  "Cor (Intercept~.p_resid_neg cntry_short)" = "correlation\n(ran. intercepts and neg. slopes)",
  "Cor (Intercept~.p_resid_unciv cntry_short)" = "correlation\n(ran. intercepts and inc. slopes)",
  "SD (Intercept cntry_short)" = "variance of country-level random intercepts",
  "SD (Intercept p_uniqueid)" = "variance of party-level random intercepts",
  "SD (Intercept i_unique)" = "variance of voter-level random intercepts",
  "SD (Observations)" = "level 1 residual variation",
  "SD (p_negativity cntry_short)" = "negativity slope variation (cntry)",
  "SD (p_incivility cntry_short)" = "incivility slope variation (cntry)",
  "SD (p_resid_neg cntry_short)" = "negativity slope variation (cntry)",
  "SD (p_resid_unciv cntry_short)" = "incivility slope variation (cntry)"
)

coef_map <- coef_map_dirty
names(coef_map) <- names(coef_map_dirty) %>% str_remove_all(":")


# p_negativity:d_ninterpos_perc_biNo better alternatives
# p_incivility:d_ninterpos_perc_biNo better alternatives
# p_negativity:d_ninterpos_perc_bi_num
# p_negativity:d_combodist_ewiki
# d_ninterpos_perc_bi_num:d_combodist_ewiki
# p_negativity:d_ninterpos_perc_bi_num:d_combodist_ewiki
# p_incivility:d_ninterpos_perc_bi_num
# p_incivility:d_combodist_ewiki
# p_incivility:d_ninterpos_perc_bi_num:d_combodist_ewiki
# p_incivility:i_followelections
# d_ninterpos_perc_bi_num:i_followelections
# p_incivility:d_ninterpos_perc_bi_num:i_followelections
# p_negativity:i_followelections
# p_negativity:d_ninterpos_perc_bi_num:i_followelections
# p_negativity:p_combo_wiki_emindist
# p_incivility:p_combo_wiki_emindist
# p_negativity:e_ENP
# p_incivility:e_ENP
# sd__(Intercept)
# sd__Observation
# sd__p_negativity
# cor__(Intercept).p_negativity
# cor__(Intercept).p_incivility
# sd__p_incivility


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

# Check if all coefficients are already addressed:
models %>%
  purrr::map(~ (broom::tidy(.) %>% pull(term)) %[out% names(coef_map)) %>%
  unlist() %>%
  unique() # %>% clipr::write_clip()

names(coef_map) %[out% (models %>% purrr::map(~ (broom::tidy(.) %>% pull(term))) %>% unlist() %>% unique())


# Print to a document -----------------------------------------------------

if (F) {
  # Calculating the model parameters
  gof_list <- list(
    "Voters_neg" = get_gof(model = m_ind_noideol_neg),
    "Voters_inc" = get_gof(model = m_ind_noideol_inc),
    "Parties_neg" = get_gof(model = m_party_neg),
    "Parties_inc" = get_gof(model = m_party_inc),
    "Systems_neg" = get_gof(model = m_system_neg_votes),
    "Systems_inc" = get_gof(model = m_system_inc_votes)
  )
  gof_list <- list(
    "Voters_neg" = get_gof(model = m_ind_noideol_neg),
    "Voters_inc" = get_gof(model = m_ind_noideol_inc),
    "Parties_neg" = get_gof(model = m_party_neg),
    "Parties_inc" = get_gof(model = m_party_inc),
  )
  gof_df <- do.call(
    rbind,
    lapply(
      1:length(gof_list),
      function(i) {
        tibble(
          par_name = names(gof_list[[i]]),
          param = t(gof_list[[i]]),
          model = names(gof_list[i])
        )
      }
    )
  )
  gof_df %>%
    pivot_wider(values_from = "param", names_from = "model") %>%
    filter(complete.cases(.))
}

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


# Create the actual table
nicelyformatted_table <- function(hlinepos = 10,
                                  models,
                                  hlinepos1,
                                  hlinepos2,
                                  output,
                                  coef_map,
                                  FONT) {
  modelsummary(
    models = models,
    # group_map = c(
    #   "Fixed effects",
    #   "cntry_short" = "System-level",
    #   "p_uniqueid" = "Party-level",
    #   "i_unique" = "Respondent-level",
    #   "Residual" = "Dyad (residual)"
    # ),
    stars = T,
    fmt = function(x) format(round(x, 3), nsmall = 3),
    title = "Effect of NC on respondent's propensity to vote for the attacker",
    coef_map = coef_map,
    group = group + term ~ model,
    estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
    statistic = NULL,
    output = "flextable",
    gof_map = gm,
    # ); beepr::beep(10)
  ) %>%
    flextable::font(
      fontname = FONT,
      part = "all"
    ) %>%
    flextable::fontsize(9) %>%
    flextable::autofit() %>%
    hline(hlinepos1) %>%
    hline(hlinepos2) %>%
    fit_to_width(10) %>%
    flextable::save_as_docx(
      path = output,
      pr_section = prop_section(
        page_size = page_size(
          orient = "landscape",
          width = 8.3, height = 11.7
        ),
        type = "continuous",
        page_margins = page_mar()
      )
    )
  beepr::beep(10)
}



# Creating the tables -----------------------------------------------------

# ├ Main models -----------------------------------------------------
nicelyformatted_table(
  models = list(
    "Voters (neg)" = m_ind_noideol_neg,
    "Voters (inc)" = m_ind_noideol_inc,
    "Parties (neg)" = m_party_neg,
    "Parties (inc)" = m_party_inc,
    "Systems (neg)" = m_e_neg,
    "Systems (inc)" = m_e_inc
  ),
  hlinepos1 = 41,
  hlinepos2 = 49,
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/00_all_std_models.docx"
)
beepr::beep(10)

# ├ 3-way interactions -----------------------------------------------------
nicelyformatted_table(
  hlinepos1 = 47,
  hlinepos2 = 51,
  models = list(
    "Voters by ideologigal dist. to sponsor (negativity)" = m_indiv_int_ideol_neg,
    "Voters by ideologigal dist. to sponsor" = m_indiv_int_ideol_inc,
    "Voters by following the election (incivility)" = m_indiv_int_follow_inc,
    "Voters by following the election (negativity)" = m_indiv_int_follow_neg
  ),
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/00_models_ideol_follow.docx"
)
beepr::beep(10)

# ├ With Adjusted variables -----------------------------------------------------
nicelyformatted_table(
  models = list(
    "Voters (neg)" = m_ind_noideol_neg_adj,
    "Voters (inc)" = m_ind_noideol_inc_adj,
    "Parties (neg)" = m_party_r_adj_neg,
    "Parties (inc)" = m_party_r_adj_inc,
    "Systems (neg)" = m_e_neg_adj,
    "Systems (inc)" = m_e_inc_adj
  ),
  hlinepos1 = 41,
  hlinepos2 = 47,
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/00_all_std_models_adj.docx"
)
beepr::beep(10)

# ├ Votes as DV -----------------------------------------------------
nicelyformatted_table(
  models = list(
    "Parties (neg)" = m_party_r_votes_neg,
    "Parties (inc)" = m_party_r_votes_inc,
    "Systems (neg)" = m_system_neg_votes,
    "Systems (inc)" = m_system_inc_votes
  ),
  hlinepos1 = 26,
  hlinepos2 = 31,
  coef_map = coef_map,
  FONT = FONT,
  output = "tables/00_all_std_models_votes.docx"
)
beepr::beep(10)


