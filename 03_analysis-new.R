# 0. readme ----------------------------------------------------------------
# Annotation scheme of code
# [T] Table output
# [F] Figure output
# [SC] Sample Cut
# [R] Robustness Check

# This file is run after preparing the analysis dataset with
# "02_data-wrangling.R"
# run this when the data preparation script was updated;
# source("02_data-wrangling.R"); beepr::beep(10)
# rm(list = ls())

# 1. setup ---------------------------------------------------------
# Change the following environment variable to this script's project folder path.
Sys.setenv(WD = getwd())

# Set working directory
setwd(Sys.getenv("WD"))

# Load packages
pacman::p_load(
  tidyverse,
  tidylog,
  inops,
  glue,
  feather,
  lme4,
  lmerTest,
  performance,
  vroom,
  beepr
)

# load additional functions needed for this project
source("00_utils.R")

# interplot function from the interplot package but adapted for a three-way interaction
source("00_interplot-3-way.R")


# ├ pre-sets for visualisations ------------------------------------------------------
# Choose the font for your output (Plots and Tables)
# Define font for plots
FONT <- "EB Garamond" # for paper word paper version

# FIXME: remove the following
# FONT <- "CMU Serif" # for paper overleaf
# FONT <- "Helvetica" # for slides

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

# create the necessary folder structure for this project
folders <- c("plots", "tables", glue("plots/{FONT}"))
walk(folders, ~ {if (!dir.exists(.x)) {dir.create(.x)}})


# ├ load data ---------------------------------------------
# Get the date of the most recent Analysis file:
mostrecentdate <- (list.files("data/") %[in~% "Analysisfile") %>%
  str_remove("_.*") %>%
  as.Date() %>%
  max()

# Load the most recent analysis file
tempdf <- vroom(glue("data/{mostrecentdate}_Analysisfile.csv"))
print(glue("The most recent file is from {mostrecentdate};"))

# Get the most recent EPEES file
mostrecentdate <- (list.files("data/") %[in~% "EPEES-file") %>%
  str_remove("_.*") %>%
  as.Date() %>%
  max()
enx <- vroom(glue("data/{mostrecentdate}_EPEES-file.csv"))
print(glue("The most recent file is from {mostrecentdate};"))


# 2. rescaling ----------------------------------------------
# For better fitting of models, all numeric covariates that do not require to
# be interepreted meaningfully are standardised.

stdsvars <-
  names(select_if(tempdf, is.numeric)) %[out~%
  # Variables that should not be changed
  c(
    "d_ptv",
    "p_incivility",
    "p_negativity",
    "d_",
    "p_combo_wiki_emindist",
    "i_followelections",
    "e_ENP"
  )

# rescaling
tempdf_scale1 <-
  tempdf %>%
  select(-party_acro) %>%
  mutate_at(vars(all_of(stdsvars)), scale2) %>%
  mutate(
    # d_ninterpos_perc_bi = ifelse(d_ninterpos_perc > 0, 1, 0) %>%
    #   factor(levels = c(1, 0), labels = c("Better alternatives available", "No better alternatives")),
    # d_ninterpos_perc_bi_num = ifelse(d_ninterpos_perc > 0, 1, 0),
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
  ) %>%
  mutate(
    p_negativity = (p_negativity + 10) / 2
  )


# ├ [SC] filter out outliers and incomplete cases ---------------------------
# filter out any cases that lie outside of the 1.5 x interquartile range
# filter out incomplete cases
# filter out respondents disclosing less than 2 ptvs

tempdf_scale <-
  tempdf_scale1 %>%
  # kick out outliers
  filter_if(
    is.numeric,
    any_vars(!outliers(., times = 1.5))
  ) %>%
  # kick out incomplete cases
  tidylog::filter(complete.cases(.)) %>%
  # kick out respondents who don't mention more than 2PTV
  group_by(i_unique) %>%
  tidylog::filter(n() > 2) %>%
  ungroup()

# sample cut overview
tempdf %>%
  summarise(
    countries = n_distinct(cntry_short),
    parties = n_distinct(p_uniqueid),
    respondents = n_distinct(i_unique),
    dyads = n()
  ) %>%
  print()


# ├ party-level df ----------------------------------------------
# Given that all interesting variation in the party- and system-level models
# is at the party- and system-level (but not at the individual-level), we
# aggregate the dataset here to the party-level.

party_df <-
  tempdf_scale %>%
  select(
    d_ptv,
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


# 3. descriptive stats table ----------------------------------------------
# So that all the pictures of the mini plots are saved properly for knitting

tempdf_scale %>%
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
    . ->> tempdata
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
  relocate(variable, role, level) %>%
  write_csv("data/temp.csv")
write_csv(tempdata, "data/temp2.csv")

# To update the descriptives table on overleaf:
# 1) pull the edits from overleaf
# 2) knit the Descriptives-table.Rmd
# 3) stage & committ all edits (not MAIN.tex) (incl. the table file edits) to overleaf & push
# 3) only keep the tex file between begin and end document.

# rmarkdown::render("Descriptives-table.Rmd"
# , output_dir = "tables/"
# )

# Remove the document components so as to leave it as a tex doc;
# desctable <- readLines(con = "Descriptives-table.tex")
# desctable[which(desctable %in~% "begin\\{table\\}"):which(desctable %in~% "end\\{table\\}")] %>%
#   writeLines(con = "Descriptives-table_edited.tex")



# 4. visualisation of both dimensions -----------------------------------------------------
p1 <- party_df %>%
  mutate(
    p_prcEP19 = p_prcEP19 * 100,
  ) %>%
  arrange(-p_harshness) %>%
  mutate(
    harshrank = 1:nrow(party_df),
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

save_plot(plot = p1, file = glue("plots/{FONT}/00 dist_inciv-neg-harsh_{FONT}.png"))


# Exemplary visualisation of inter-party distances for Greece
p1 <- enx %>%
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
p1

save_plot(plot = p1, file = glue("plots/{FONT}/99_mindist-for-greece.png"))



# 5. random slopes and variance decomposition ---------------------------------------------------------
# ├ [T] variance decomposition ----
# empty model
m0 <-
  lmer(
    formula = d_ptv ~ (1 | cntry_short) + (1 | p_uniqueid) + (1 | i_unique),
    verbose = 100, data = tempdf,
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

# This table is created by uploading it to the Version Control with Overleaf / Git,
# knitting the online table and then downloading it straight from there.
variances %>%
  kableExtra::kbl(
    caption = "Variance decomposition at levels of aggregation",
    format = "latex", booktabs = T
  ) %>%
  kableExtra::kable_minimal(full_width = F, html_font = FONT) %>%
  kableExtra::save_kable("tables/variances.tex")


# ├ [F] random slopes with ptvs ---------------------------------------------------------
# Visualise random intercepts at country_level WITH PTV

# Running the random slope models by dimension
# negativity
m1_n <-
  lmer(
    formula = glue("d_ptv ~ p_negativity + (p_negativity | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
beepr::beep(10)

# Incivility
m1_i <-
  lmer(
    formula = glue("d_ptv ~ p_incivility + (p_incivility | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
beepr::beep(10)

# Adjusted
# negativity
m1_adj_n <-
  lmer(
    formula = glue("d_ptv ~ p_resid_neg + (p_resid_neg | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
beepr::beep(10)

# Incivility
m1_adj_i <-
  lmer(
    formula = glue("d_ptv ~ p_resid_unciv + (p_resid_unciv | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
beepr::beep(10)

## Robustness checks
# with all controls
# negativity
m1c_n <-
  lmer(
    formula = glue("d_ptv ~
                     i_gender + i_yrbrn + i_edulvl + i_pinterest +
  i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
  p_lrpos_wiki + p_lrpos_extreme_wiki +
  p_eupos + p_eupos_extreme +
  p_neg_close_ecombo +
  p_incumbency +
  exp_lrscale + exp_perceptEU +
  exp_familiar + exp_easy + exp_citizen + exp_female +
  e_region + e_negativity +
                   p_negativity + (p_negativity | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
beepr::beep(10)

# Incivility
m1c_i <-
  lmer(
    formula = glue("d_ptv ~
                  i_gender + i_yrbrn + i_edulvl + i_pinterest +
                  i_eupos + i_lrpos + i_extremity_eu + i_extremity_lr +
                  p_lrpos_wiki + p_lrpos_extreme_wiki +
                  p_eupos + p_eupos_extreme +
                  p_inciv_close_ecombo +
                  p_incumbency +
                  exp_lrscale + exp_perceptEU +
                  exp_familiar + exp_easy + exp_citizen + exp_female +
                  e_region + e_incivility +
                  p_incivility + (p_incivility | cntry_short) + (1 | i_unique)"),
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )

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
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
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
    verbose = 100, data = tempdf,
    control = lmerControl(optimizer = "Nelder_Mead")
  )


# Visualisation
get_ran_slopes <- function(
    m1, m2, # models 1 & 2
    t1, t2, # titles for 1 & 2
    l1, l2 # labels of slope for 1 & 2
    ) {
  # Getting the plotting data by dimension
  # First we need to estimate the random effects + CIs for both models
  ranefplot_n <- ranef(m1) %>% ggCaterpillar(QQ = FALSE)
  ranefplot_i <- ranef(m2) %>% ggCaterpillar(QQ = FALSE)

  temp <-
    as_tibble(ranefplot_n$cntry_short$data) %>%
    full_join(as_tibble(ranefplot_i$cntry_short$data)) %>%
    filter(!ind == "(Intercept)") %>%
    mutate(
      conf.low = y - ci,
      conf.high = y + ci,
    )

  # Ad fixed intercept and slope
  temp2 <-
    broom.mixed::tidy(m1, conf.int = T) %>%
    full_join(broom.mixed::tidy(m2, conf.int = T)) %>%
    filter(term %in% c(t1, t2)) %>%
    mutate(
      ID = "Fixed Effect" %>% as.factor(),
      y = estimate,
    ) %>%
    select(ID, conf.low, conf.high, ind = term, y, estimate) %>%
    full_join(temp) %>%
    mutate(
      ind = ifelse(ind == t1, l1, l2),
    )

  # order by slopes not intercepts
  # first negativity, then incivility
  temp2 <-
    temp2 %>%
    mutate(
      ID = as.character(ID),
      # this is a workaround so as to be able to truly let the countries be ordered separately by facet of the plot below
      indID = ifelse(as.character(ind) == l1, paste(ID, " "), ID),
      ind = ind %>% fct_relevel(l1),
    ) %>%
    mutate(
      indID = indID %>% fct_reorder(y),
      indID = indID %>% fct_relevel("Fixed Effect", "Fixed Effect  ", after = Inf)
    ) %>%
    arrange(indID)
  ranplot <-
    temp2 %>%
    mutate(
      ID = ID %>%
        fct_relevel(
          levels(temp2$ID) %[out~% "Fixed",
          "Fixed Effect"
        )
    ) %>%
    # remove FE intercept
    mutate(
      y = ifelse(ind %in~% "Intercept" & ID == "Fixed Effect", NA, y),
      estimate = ifelse(ind %in~% "Intercept" & ID == "Fixed Effect", NA, estimate),
      conf.low = ifelse(ind %in~% "Intercept" & ID == "Fixed Effect", NA, conf.low),
      conf.high = ifelse(ind %in~% "Intercept" & ID == "Fixed Effect", NA, conf.high),
    ) %>%
    ggplot(aes(x = indID, y = y)) +
    geom_hline(aes(yintercept = estimate), colour = "darkred", linetype = "dashed", alpha = 0.6) +
    geom_hline(aes(yintercept = 0)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
    geom_point(shape = "square", colour = "darkred") +
    coord_flip() +
    facet_wrap(~ind, scales = "free_y") +
    labs(x = "", y = "Random Intercepts")
  return(ranplot)
}

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
  l1 = "Slopes of adjusted Negativity",
  l2 = "Slopes of adjusted Incivility"
)

save_plot(ranplot_main,
  height = 150,
  glue("plots/{FONT}/00 RandomSlopesandIntercepts_cntry_{FONT}.png")
)
save_plot(ranplot_controls, height = 150, glue("plots/{FONT}/00 RandomSlopesandIntercepts_cntry_cntrls_{FONT}.png"))
save_plot(ranplot_controls_noEU, height = 150, glue("plots/{FONT}/00 RandomSlopesandIntercepts_cntry_cntrlsnoEU_{FONT}.png"))
save_plot(ranplot_adj, height = 150, glue("plots/{FONT}/00 RandomSlopesandIntercepts_cntry_adj_{FONT}.png"))
beepr::beep(10)


# 6. individual-level -----------------------------------------------------
# Some functions I will use again and again
plot_marg_pred <- function(model,
                           terms,
                           xlab = "Overall negativity of a party's campaign",
                           ylab = "Predicted PTV for sponsor",
                           collab = "Availability of alternatives") {
  marg_preds <-
    ggeffects::ggpredict(model,
      terms = terms
    )

  marg_preds %>%
    ggplot(aes(x = x, group = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.7,
      fill = "gray"
    ) +
    geom_line(aes(colour = group, y = predicted)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), ) +
    theme(legend.position = "top") +
    labs(
      x = xlab,
      y = ylab,
      colour = collab
    )
}

plot_interplot <- function(model, var1, var2, hist = F, xlab, ylab) {
  pind <- interplot::interplot(
    m = model,
    var1 = var1,
    var2 = var2,
    hist = hist
  )

  pindp <-
    pind$data %>%
    mutate(x = ifelse(
      fake == 0,
      "Better alternative(s) available",
      "Sponsor is best alternative"
    )) %>%
    ggplot(aes(x = x, y = coef1)) +
    geom_hline(aes(yintercept = 0), colour = "darkgray") +
    geom_point() +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1) +
    labs(
      x = xlab,
      y = ylab
    )
}

# Function for a combined model Interplot
plot_interplot_combo <- function(model1,
                                 model2,
                                 var1a,
                                 var1b,
                                 var2,
                                 dim1,
                                 dim2,
                                 histos = F,
                                 xlab,
                                 ylab,
                                 collab) {
  pind_a <- interplot::interplot(
    m = model1,
    var1 = var1a,
    var2 = var2,
    hist = histos
  )

  pind_b <- interplot::interplot(
    m = model2,
    var1 = var1b,
    var2 = var2,
    hist = histos
  )

  temp_df <- dplyr::bind_rows(
    pind_a$data %>% mutate(dim = dim1),
    pind_b$data %>% mutate(dim = dim2)
  )

  pindp <-
    temp_df %>%
    mutate(
      x = ifelse(
        fake == 0,
        "Better alternative(s) available",
        "Sponsor is best alternative"
      ) %>% factor(., levels = c(
        "Sponsor is best alternative",
        "Better alternative(s) available"
      )),
      dim = dim %>% fct_relevel(dim1)
    ) %>%
    ggplot(aes(
      x = dim,
      y = coef1,
      fill = x,
      colour = x
    )) +
    geom_hline(aes(yintercept = 0), colour = "darkgray") +
    geom_point(aes(shape = x), position = position_dodge(0.5)) +
    scale_shape_manual(values = c("square", "triangle")) +
    geom_errorbar(aes(ymin = lb, ymax = ub),
      width = 0.1,
      position = position_dodge(0.5)
    ) +
    labs(
      x = xlab,
      y = ylab,
      fill = collab,
      colour = collab,
      shape = collab
    )
}
plot_interplot_combo <- compiler::cmpfun(plot_interplot_combo)


# ├ [F] main model ------------------------------------
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

save_plot(plot = p_int_ind_comb, file = glue("plots/{FONT}/00 AME_individual_noideol_int_comb_{FONT}.png"))


# ├ [R][F] model with ideol. ----
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
  summary(tempdf_scale$d_combodist_wiki)[c(2, 3, 5)] %>% # Quantiles
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

save_plot(
  file = glue("plots/{FONT}/00 AME_individual_by_ideol_comb_t_{FONT}.png"),
  plot = p_indiv_int_ideol_comb3
)

# Currently no confidence intervals from this plot 🥲
# Visualising slopes
# p_indiv_int_ideol_neg <-
#   ggeffects::ggpredict(
#     m_indiv_int_ideol_neg,
#     terms = c("p_negativity", "d_ninterpos_perc_bi_num",  paste0("d_combodist_ewiki [", paste0(modelvls, collapse = ",") ,"]"))
#   ) %>% as_tibble %>%
#   mutate(
#     fctlvl = facet %>% as.character %>% as.numeric,
#     distance = case_when(
#       fctlvl == modelvls[1] ~ glue("Minimal Distance [{modelvls[1]}]"),
#       fctlvl == modelvls[2] ~ glue("25% quantile [{modelvls[2]}]"),
#       fctlvl == modelvls[3] ~ glue("50% quantile [{modelvls[3]}]"),
#       fctlvl == modelvls[4] ~ glue("75% quantile [{modelvls[4]}]"),
#       T ~ NA_character_
#     ) %>% as.factor() %>% fct_reorder(fctlvl)
#   ) %>%
#   ggplot(aes(x, predicted, group = group)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.7, fill = "gray") +
#   geom_line(aes(colour = group, linetype = group)) +
#   theme(legend.position = "top") +
#   facet_grid(. ~ distance) +
#   scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
#   labs(
#     x = "Overall negativity of a party's campaign",
#     y = "Predicted PTV for sponsor",
#     colour = "Availability of alternatives",
#     linetype = "Availability of alternatives",
#   ); p_indiv_int_ideol_neg; beepr::beep(10)
#
# save_plot(
#   file = glue("plots/{FONT}/00 AME_individual_by_ideol_{FONT}.png"),
#   plot = p_indiv_int_ideol_neg
# )

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

# saving the plot
save_plot(plot = p_int_ind_comb_adj, file = glue("plots/{FONT}/00 AME_individual_noideol_int_adj_comb_{FONT}.png"))


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

save_plot(
  file = glue("plots/{FONT}/00 AME_individual_by_follow_comb_t_{FONT}.png"),
  plot = p_indiv_int_follow_comb3
)


# 7. party-level ---------------------------------------------------------
# Neat interplots
# Function to set the y location of the historgram of interplot
library(gridtext)
library(gridExtra)
library(gtable)
library(grid)


# Change the position of the interplot histogram
set_interp_hist_to <- function(intp, newymin = 0, histo_height = 0.3) {
  rect_data <- layer_data(intp, 1)
  rect_data$ymax <- (rect_data$ymax - rect_data$ymin) / (max(rect_data$ymax) - min(rect_data$ymin)) * histo_height + newymin
  rect_data$ymin <- rect_data$ymin - rect_data$ymin + newymin
  intp$layers[[1]] <- NULL
  p <- intp +
    geom_rect(
      data = rect_data,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      alpha = 0,
      colour = "gray50"
    )
  p$layers <- c(geom_hline(yintercept = 0, colour = "gray50"), p$layers)
  return(p)
}

# function to create a combined interplot
plot_inter_combo_lin <- function(m_a,
                                 m_b,
                                 var1a,
                                 var1b,
                                 var2,
                                 var1alab = "Negativity",
                                 var1blab = "Incivility",
                                 ymin,
                                 ymax,
                                 histo_height = 0.3,
                                 ylab = "Average marginal effect of campaign\ndimension on ptv for sponsor",
                                 xlab = "Distance to closest competitor",
                                 newymin) {
  ma <- interplot::interplot(
    m = m_a,
    var1 = var1a,
    var2 = var2,
    hist = T
  )
  mb <- interplot::interplot(
    m = m_b,
    var1 = var1b,
    var2 = var2,
    hist = T,
  )

  gridExtra::grid.arrange(
    (ma +
      ylim(ymin, ymax) + labs(title = var1alab)) %>% set_interp_hist_to(ymin, histo_height = histo_height),
    (mb + ylim(ymin, ymax) + labs(title = var1blab)) %>% set_interp_hist_to(ymin, histo_height = histo_height),
    nrow = 1,
    left = textGrob(
      ylab,
      rot = 90,
      vjust = 1,
      gp = gpar(fontfamily = FONT, fontsize = 9)
    ),
    bottom = textGrob(
      xlab,
      gp = gpar(fontfamily = FONT)
    )
  )
}

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

save_plot(
  p_part_combo,
  file = glue("plots/{FONT}/00 AME_party_interp_combo_{FONT}.png")
)


# ├ [R][F] adjusted ----
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
    var1alab = "adjusted negativity",
    var1blab = "adjusted incivility",
    var1a = "p_resid_neg",
    var1b = "p_resid_unciv",
    var2 = "p_combo_wiki_emindist",
    ymin = -0.64,
    ymax = 0.5
  )
p_part_adj_combo

save_plot(
  p_part_adj_combo,
  file = glue("plots/{FONT}/00 AME_party_interp_combo_adj_{FONT}.png")
)



# ├ [R][F] vote ----
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
p_part_combo_vote

save_plot(
  p_part_combo_vote,
  file = glue("plots/{FONT}/00 AME_party_interp_combo_vote_{FONT}.png")
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
forml_e_neg
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

save_plot(
  p_e_combo,
  file = glue("plots/{FONT}/00 AME_system_interp_combo_{FONT}.png")
)


# ├ [R][F] adjusted ----

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
    var1alab = "adjusted negativity",
    var1blab = "adjusted incivility",
    var1a = "p_resid_neg",
    var1b = "p_resid_unciv",
    var2 = "e_ENP",
    ymin = -0.5,
    ymax = 0.5,
    xlab = "Effective Number of Parties"
  )
p_e_combo_adj

save_plot(
  p_e_combo_adj,
  file = glue("plots/{FONT}/00 AME_system_interp_combo_adj_{FONT}.png")
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
    formula = forml_e_neg,
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
p_e_combo_vote

save_plot(
  p_e_combo_vote,
  file = glue("plots/{FONT}/00 AME_system_interp_combo_vote_{FONT}.png")
)

# To create the tables for the respective regression models cf. 04_tables
