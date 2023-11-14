# Comparing new and old analysis file!
pacman::p_load(
  tidyverse,
  tidylog,
  inops,
  glue,
  # loading data
  lme4,
  lmerTest,
  performance,
  vroom,
  gridtext,
  gridExtra,
  gtable,
  grid
  # beepr,
  # feather,
)

source("00_utils.R")

identicals_acrossdfs <- function(df1, df2) {
  overl <- names(df1) %[in% names(df2)
  mat <- overl %>% map_vec(~ {
    identical(df1[[.x]], df2[[.x]])
  })
  return(
    list(
      "only_in_x" = names(df1) %[out% names(df2),
      "only_in_y" = names(df2) %[out% names(df1),
      "same_name" = overl,
      "identical_values" = overl[mat],
      "different_values" = overl[!mat]
    )
  )
}

identicals_col <- function(x) {
  all(lapply(x, identical, x[[1]]))
}


mostrecentdate <- (list.files("data/") %[in~% "Analysisfile") %>%
  str_remove("_.*") %>%
  as.Date() %>%
  max()
tempdf <- vroom(glue("data/{mostrecentdate}_Analysisfile.csv"))
enx <- vroom(glue("data/{mostrecentdate}_EPEES-file.csv")) %>% arrange(p_uniqueid)

tempdf_old <- vroom(glue("data/2023-03-13_Analysisfile.csv"))
enx_old <- vroom(glue("data/2023-03-13_EPEES-file.csv")) %>% arrange(p_uniqueid)



# Analysis file -----------------------------------------------------------
# Documentation of differences in Notion
tempdf$p_uniqueid %[out% tempdf_old$p_uniqueid
tempdf_old$p_uniqueid %[out% tempdf$p_uniqueid

# adjusting the new df to the old values
tempdf <-
  tempdf %>%
  mutate(
    p_neg_close_ecombo = -9.99999999999248 + 1.9999999999978 * p_neg_close_ecombo,
    p_negativity = -9.99999999999635 + 1.99999999999926 * p_negativity,
    e_negativity = -10.0000000000242 + 2.00000000000435 * e_negativity,
    p_resid_neg = 4.17108060947059e-15 + 1.99999999999951 * p_resid_neg,
    p_neg_adj_close_ecombo = -3.60566650530228e-14 + 1.99999999999934 * p_neg_adj_close_ecombo,
    p_harshness = -3.01782868487503 + 1.30942664975051 * p_harshness,
    e_adj_negativity = -3.07078246685102e-14 + 2.00000000000328 * e_adj_negativity,
  )

# checking of remaining incongruencies
ids <- identicals_acrossdfs(tempdf, tempdf_old)
ids$different_values
# => differences are always relateable to epees data!

# dyadic data!
temp <- full_join(
  tempdf %>% select(i_unique, p_uniqueid, one_of(ids$different_values)),
  tempdf_old %>% select(i_unique, p_uniqueid, one_of(ids$different_values)),
  by = c("i_unique", "p_uniqueid")
)

res <- enframe(ids$different_values)
pb <- pbn(length(res$value))
for (i in ids$different_values) {
  pb$tick()
  res$diff[res$value == i] <- temp %>%
    dplyr::select(matches(glue("{i}\\.[xy]"))) %>%
    split(1:nrow(.)) %>%
    map_lgl(~ {
      !identicals_col(.x)
    }) %>%
    which() %>%
    as.numeric() %>%
    list()
}

pb <- pbn(length(res$value))
# for all vars
for (j in 1:length(res$value)) {
  pb$tick()
  # maximum difference
  res$maxdiff[j] <-
    temp %>%
    dplyr::select(p_uniqueid, matches(glue("{res$value[j]}\\.[xy]"))) %>%
    dplyr::mutate(diff = .data[[paste0(res$value[j], ".x")]] - .data[[paste0(res$value[j], ".y")]]) %>%
    dplyr::slice(unlist(res$diff[j])) %>%
    # mutate_if(is.numeric, ~{format(.x, scientific = FALSE)}) %>%
    dplyr::pull(diff) %>%
    max(na.rm = T)

  # inspect as scatterplot
  # res$plot[[j]] <-
  temp %>%
    dplyr::select(matches(glue("{res$value[j]}\\.[xy]"))) %>%
    ggplot(aes(
      x = !!sym(glue("{res$value[j]}.x")),
      y = !!sym(glue("{res$value[j]}.y"))
    )) +
    geom_point() +
    theme_minimal() +
    labs(x = glue("{res$value[j]}.x"), y = glue("{res$value[j]}.y"))

  # is one a combination of the other?
  m <- lm(
    formula =
      glue("{res$value[j]}.y ~ {res$value[j]}.x"),
    data = temp
  )
  res$formula[j] <- glue("{m$coefficients[1]} + {m$coefficients[2]} * {res$value[j]}")
  res$rsq[j] <- summary(m)["r.squared"] %>% as.numeric()
}

# Get the formulas to adjust the new df to the old values
res %>%
  arrange(-maxdiff)

# write the new df to a file
tempdf %>% vroom::vroom_write(file = glue("data/{mostrecentdate}_Analysisfile_temp.csv"))

# why does p_harshness not have a perfect R²?

# EPEES DATA --------------------------------------------------------------
# Documentation of differences in Notion
enx$p_uniqueid %[out% enx_old$p_uniqueid
enx_old$p_uniqueid %[out% enx$p_uniqueid

# data fixes to make sure all the differences we are assuming are the only ones.

enx1 <-
  enx %>%
  mutate(
    # p_negativity = p_negativity * 2-10,
    p_harshness = p_harshness + p_negativity * 0.25 - 2.5,
    p_neg_close_ecombo = -10 + 2 * p_neg_close_ecombo,
    neg_party_1 = -9.99999999999998 + 2 * neg_party_1,
    neg_party_2 = -9.99999999999999 + 2 * neg_party_2,
    neg_party_3 = -10 + 2 * neg_party_3,
    neg_party_4 = -9.99999999999999 + 2 * neg_party_4,
    neg_party_5 = -10 + 1.99999999999999 * neg_party_5,
    neg_party_6 = -10 + 2 * neg_party_6,
    neg_party_7 = -10 + 2 * neg_party_7,
    neg_party_8 = -10 + 2 * neg_party_8,
    p_resid_neg = -4.5291627028895e-17 + 2 * p_resid_neg,
    p_neg_adj_close_ecombo = -1.78548792689856e-16 + 2 * p_neg_adj_close_ecombo,
    neg_adj_party_1 = -3.09132919980855e-15 + 2 * neg_adj_party_1,
    neg_adj_party_2 = 4.63876557119064e-16 + 2 * neg_adj_party_2,
    neg_adj_party_3 = 1.87288553073737e-15 + 2 * neg_adj_party_3,
    neg_adj_party_4 = -1.19232353065129e-16 + 2 * neg_adj_party_4,
    neg_adj_party_5 = 2.84444741192202e-16 + 2 * neg_adj_party_5,
    neg_adj_party_6 = -4.09326451138136e-16 + 2 * neg_adj_party_6,
    neg_adj_party_7 = 4.12439663164324e-16 + 2 * neg_adj_party_7,
    neg_adj_party_8 = 5.66888542145231e-16 + 2 * neg_adj_party_8,
    e_adj_negativity = -1.55237983443123e-16 + 2 * e_adj_negativity,
    e_negativity = e_negativity * 2 - 10,
  )


# fixes for old data
enx_old1 <-
  enx_old %>%
  mutate(
    p_negativity = (p_negativity + 10) / 2,
    # not an issue!
    p_prcEP19 = ifelse(p_uniqueid == "CZ_party_8", 11.65, p_prcEP19),
    # not an issue!
    region = case_when(
      region == 1 ~ "1. East",
      region == 2 ~ "2. North",
      region == 3 ~ "3. South",
      region == 4 ~ "4. West",
      T ~ NA_character_
    )
  )


# checking of remaining incongruencies
ids <- identicals_acrossdfs(enx1, enx_old1)
# ids$different_values %>% clipr::write_clip()

temp <- full_join(
  enx1 %>% select(p_uniqueid, one_of(ids$different_values)),
  enx_old1 %>% select(p_uniqueid, one_of(ids$different_values)),
  by = "p_uniqueid"
)

res <- enframe(ids$different_values)
for (i in ids$different_values) {
  res$diff[res$value == i] <- temp %>%
    dplyr::select(matches(glue("{i}\\.[xy]"))) %>%
    split(1:nrow(.)) %>%
    map_lgl(~ {
      !identicals_col(.x)
    }) %>%
    which() %>%
    as.numeric() %>%
    list()
  cat(glue("\n\nFor Column '{i}', the following observations have differing values:\n"))
  paste0("\n• ", res$diff[res$value == i]) %>% cat()
}

# for all vars
for (j in 1:length(res$value)) {
  # maximum difference
  res$maxdiff[j] <-
    temp %>%
    select(p_uniqueid, matches(glue("{res$value[j]}\\.[xy]"))) %>%
    mutate(diff = .data[[paste0(res$value[j], ".x")]] - .data[[paste0(res$value[j], ".y")]]) %>%
    slice(unlist(res$diff[j])) %>%
    # mutate_if(is.numeric, ~{format(.x, scientific = FALSE)}) %>%
    pull(diff) %>%
    max(na.rm = T)

  # inspect as scatterplot
  res$plot[[j]] <-
    temp %>%
    select(matches(glue("{res$value[j]}\\.[xy]"))) %>%
    ggplot(aes_string(x = glue("{res$value[j]}.x"), y = glue("{res$value[j]}.y"))) +
    geom_point() +
    theme_minimal() +
    labs(x = glue("{res$value[j]}.x"), y = glue("{res$value[j]}.y"))

  # is one a combination of the other?
  m <- lm(
    formula =
      glue("{res$value[j]}.y ~ {res$value[j]}.x"),
    data = temp
  )
  res$formula[j] <- glue("{m$coefficients[1]} + {m$coefficients[2]} * {res$value[j]}")
  res$rsq[j] <- summary(m)["r.squared"] %>% as.numeric()
}

# inspection
res %>% arrange(rsq, -maxdiff)

j <- 42
temp %>%
  select(p_uniqueid, matches(glue("{res$value[j]}\\.[xy]"))) %>%
  mutate(diff = .data[[paste0(res$value[j], ".x")]] - .data[[paste0(res$value[j], ".y")]]) %>%
  slice(unlist(res$diff[j])) %>%
  arrange(-abs(diff))

# make the final big differences disappear and then see what we'll be changing!


res %>%
  arrange(-maxdiff) %>%
  slice(5) %>%
  as.list()

j <- 44
temp %>%
  select(p_uniqueid, matches(glue("{res$value[j]}\\.[xy]"))) %>%
  mutate(diff = .data[[paste0(res$value[j], ".x")]] - .data[[paste0(res$value[j], ".y")]]) %>%
  slice(unlist(res$diff[j]))



# temporarily save adapted analysis file ----------------------------------
enx1 %>% vroom::vroom_write(file = glue("data/{mostrecentdate}_EPEES-file_temp.csv"))



# TODO do the same for the tempdf!
tempdf %>% vroom::vroom_write(file = glue("data/{mostrecentdate}_Analysisfile_temp.csv"))







# Investigate!
# region I may have included region as a continuous variable instead of a categorical

# Unproblematic
# regionbe => new epees has mostly NA only regionbe for the few relevant ones
# p_prcEP19 => CZ party 8 because we've added it's vote share, should not be problematic
# p_negativity => is currently already transformed the way I want it before I prepare the analysis data frame!
#   but a perfect correlation nevertheless; shouldn't change too much
#   (minimal difference at 16th decimal remain)
#   p_harshness => different due to negativity being different (minimal difference at 16th decimal remain)
#   p_resid_neg
#   neg_party_1
#   neg_party_2
#   neg_party_5
#   neg_party_4
#   neg_party_3
#   neg_party_6
#   neg_party_7
#   neg_party_8
#   neg_adj_party_1
#   neg_adj_party_2
#   neg_adj_party_5
#   neg_adj_party_4
#   neg_adj_party_3
#   neg_adj_party_6
#   neg_adj_party_7
#   neg_adj_party_8
#   e_adj_negativity
#   p_neg_close_ecombo
#   p_neg_adj_close_ecombo
#   e_negativity

# p_eupos => (minimal difference at 16th decimal remain)
#   p_eupos_extreme
#   p_eupos_7
#   p_combo_edist_1
#   p_combo_edist_2
#   p_combo_edist_3
#   p_combo_edist_4
#   p_combo_edist_5
#   p_combo_edist_7
#   p_combo_wiki_emindist
# exp => (minimal difference at 16th decimal remain)
#   expdist

# incivility?
# p_resid_unciv
# inciv_adj_party_1
# inciv_adj_party_2
# inciv_adj_party_5
# inciv_adj_party_4
# inciv_adj_party_3
# inciv_adj_party_6
# inciv_adj_party_7
# inciv_adj_party_8
# p_inciv_adj_close_ecombo
# e_incivility
# e_adj_incivility
