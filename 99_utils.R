# required packages -------------------------------------------------------
pacman::p_load(
  broom, 
  broom.mixed, 
  marginaleffects,
  tidyverse
)

# simplified progress bar function for verbose code.
pbn <- function(n){
  progress::progress_bar$new(
  format = paste0(" (:spin) [:bar] :percent | :current / :total | elapsed: :elapsed | eta: :eta"),
  total = n, clear = FALSE, width= 100)
  }


# data comparing functions ------------------------------------------------
# function to detect duplicates after joining / merging two data frames
identicals_df <- function(x) {
  identicals_col <- function(x) {
    all(lapply(x, identical, x[[1]]))
  }

  identicals_group <- function(x) {
    x %>% apply(2, identicals_col)
  }

  groupVARS <- groups(x) %>% as.character()

  x %>%
    dplyr::mutate(ndups = n()) %>%
    dplyr::filter(ndups > 1) %>%
    group_split() %>%
    map_dfr(~ {
      res <- identicals_group(.x)
      ids <-
        # dplyr::select(.x, one_of(groupVARS)) %>% dplyr::distinct() %>% paste0(collapse = " | ")
        dplyr::select(.x, one_of(groupVARS), ndups) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          ndups = ndups,
          col_diffvals = paste0(names(res)[res == F], collapse = ", ")
        )
    })
}

# data wrangling ----------------------------------------------------------

# define outliers
outliers <- function(x, times = 1.5){ # 1.5 is default
  # Inter quartile range
  IQR <- (quantile(x, 0.75, na.rm = T)[[1]] - quantile(x, 0.25, na.rm = T)[[1]])
  q50 <- quantile(x, 0.5, na.rm = T)[[1]]
  return(x < q50 - IQR * times | x > q50 + IQR * times)
}

# define function for z-standardising variables
scale2 <- function(x, center = T, scale = T, name = deparse(substitute(x))) {
  if (is.numeric(x)) {
    if (center) {
      x <- x - mean(x, na.rm = T)
    }
    if (scale) {
      x <- x / sd(x, na.rm = T)
    }
    return(x)
  } else {
    warning(paste0(name, " is not numeric and will be skipped."))
    return(x)
  }
}

# data visuailsations ---------------------------------------------------------
# Quickly plot the distribution of groups as a barplot
bars_by_group <- function(data = xdf, y = NA, group = NA, clr = NA, order = T, lab = T) {
  data %>%
    dplyr::group_by({{group}}) %>%
    dplyr::summarise(
      "{{y}}" := mean ({{y}}, na.rm = T)
    ) %>%
    dplyr::ungroup() -> x
  if(order) {
    x <- x %>%
      dplyr::mutate(
        "{{group}}" := {{group}} %>%
          as.character() %>%
          fct_reorder({{y}})
      )
  }
  temp <- x %>%
    dplyr::filter(complete.cases(.)) %>%
    ggplot(aes(x = {{group}}, y = {{y}}
    )) +
    coord_flip() + 
    theme_minimal(base_family = "CMU Serif")
  # ad own color scheme?
  if(missing(clr)) temp <- temp + geom_col(fill = rgb(25/255,40/255,100/255))
  else temp <- temp + geom_col(fill = clr) %>% return()
  
  # ad labels?
  if(lab) 
    temp <- 
    temp + 
    geom_text(
      aes(label = round({{y}},2)),
      stat = "unique", 
      family = "CMU Serif", size = 3,
      vjust = "bottom",
      position = position_nudge(y = max(x %>% select({{y}}))*0.05)
    )
  return(temp)
}
bars_by_group(iris, Sepal.Length, Species) + labs(title = "Example Plot")

# two-way scatterplot with a regression line plotted across the point cloud
scatterandsmooth <- function(data = xdf, x = NA, y = NA, colrs = NA){
  if(missing(colrs)){
    data %>%
      ggplot(aes(x = {{x}}, y = {{y}})) +
      theme_minimal(base_family = "CMU Serif") +
      geom_smooth(method = "lm") +
      geom_point() %>%
      return()
  } else {
    data %>% 
      ggplot(aes(x = {{x}}, y = {{y}}, colour = {{colrs}})) + 
      theme_minimal(base_family = "CMU Serif") +
      geom_smooth(method = "lm") +
      geom_point() %>% 
      return()
  }
}
scatterandsmooth(iris, Sepal.Length, Sepal.Width, Petal.Length) + labs(title = "Example Plot")

# Quick plot save - Save ggplot with standardised settings
save_plot <- function(plot, file, scale = 0.8, width = 225, height = 100){
  ggsave(filename = file, plot =  plot, device = "png",
         path = NULL, scale = scale, width = width, height = height, units = "mm",
         dpi = 300)
}

# Insert a ggplot layer at a certain location
insertLayer <- function(P, after=0, ...) {
  #  P     : Plot object
  # after  : Position where to insert new layers, relative to existing layers
  #  ...   : additional layers, separated by commas (,) instead of plus sign (+)
  
  if (after < 0)
    after <- after + length(P$layers)
  
  if (!length(P$layers))
    P$layers <- list(...)
  else 
    P$layers <- append(P$layers, list(...), after)
  
  return(P)
}

# ├ visualisations ------------------------------------------------------
## re = object of class ranef.mer
# https://github.com/jonkeane/mocapGrip/blob/master/R/ggCaterpillar.R
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(size = 2, colour="darkred", shape = "square") 
    p <- p + theme_minimal(base_family = "CMU Serif") 
    p <- p + theme(legend.position="none")
    return(p)
  }
  
  lapply(re, f)
}

# Quick model plotting function => enter summary object!
pltmymodel <- function(x){
  if(isS4(x)){
    x <- summary(x)
  }
  x$coefficients %>% {.->>x} %>% as_tibble %>% mutate(terms=rownames(x)) %>%
    filter(!terms=="(Intercept)") %>%
    ggplot(.,aes(y=Estimate, x=terms)) +
    geom_hline(aes(yintercept = 0),color="gray") +
    geom_point() + coord_flip() +
    labs(x="Coefficients") +
    theme_minimal()
}

# Combine several forest plots into one.
pltmymodels <- function(..., mnames=NULL,rm.int=T){
  x <- list(...)
  for (i in seq_along(x)){
    if(isS4(x[[i]])){
      x[[i]] <- summary(x[[i]])
      x[[i]] <-
        x[[i]]$coefficients %>%
        as_tibble %>%
        mutate(terms = rownames(x[[i]]$coefficients))
      if(is.null(mnames)) {x[[i]]$model <- glue("m_{i}")
      } else {x[[i]]$model <- mnames[i]}
    }
  }
  x <- data.table::rbindlist(x)
  if(rm.int){x <- x %>% filter(!terms=="(Intercept)")}
  x %>% as_tibble %>%
    rename(p.value=`Pr(>|t|)`) %>%
    ggplot(.,aes(y=Estimate, x=terms,  color = model)) +
    geom_hline(aes(yintercept = 0),color="gray") +
    geom_point(aes(alpha = p.value<0.01), shape=15, size = 3, position = position_dodge(1)) +
    coord_flip() +
    labs(x="Coefficients") +
    scale_alpha_manual(values=c(0.5,1)) +
    theme_minimal()
}

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
  # create interplots
  ma <- interplot::interplot(
    m = m_a,
    var1 = var1a,
    var2 = var2,
    hist = T,
  )
  
  mb <- interplot::interplot(
    m = m_b,
    var1 = var1b,
    var2 = var2,
    hist = T,
  )
  
  # compile the plots into one grid
  gridExtra::grid.arrange(
    (ma %>% set_interp_hist_to(ymin, histo_height = histo_height)) + ylim(ymin, ymax) + labs(title = var1alab),
    (mb %>% set_interp_hist_to(ymin, histo_height = histo_height)) + ylim(ymin, ymax) + labs(title = var1blab),
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

# ├ tables ------------------------------------------------------------------
# Save the variance at the respective levels as table
getvariances <- function(model, lvlss = NULL){
  #lvlss <- c("cntry", "cntry", "outparty_code", "inparty_code", "id_unique", "Residual")
  m0sum <- summary(model)
  variance <- m0sum$varcor %>% as_tibble() %>% rename(level=grp) %>%
    full_join(tibble(level = names(m0sum$ngrps), ngroups = m0sum$ngrps))
  variance$ngroups[variance$level=="Residual"] <-nobs(m0)
  variance <-
    variance %>%
    arrange(ngroups) %>%
    select(
      level, ngroups, vcov,
    ) %>%
    mutate(
      pct = paste0(format(round(vcov/sum(vcov)*100,2), nsmall=2), "%"),
    )
  
  if(exists("lvlss")) variance$level <- variance$level %>% fct_relevel(lvlss)
  variance %>% arrange(level) %>%
    mutate(
      level = recode(level,
                     cntr="Country",
                     inparty_code="In-party",
                     outparty_code="Out-party",
                     id_unique = "Individuals",
                     Residual = "Dyads"
      )
    ) %>%
    rename('Levels of aggregation' = level, 'Observations' = ngroups, 'Variance' = vcov, 'Proportion of overall variance' = pct)
}


# Function for Regression table output
nicelyformatted_table <- function(hlinepos = 10,
                                  models,
                                  hlinepos1,
                                  hlinepos2,
                                  output,
                                  FONT = "EB Garamond",
                                  ...) {
  modelsummary(
    models = models,
    stars = T,
    fmt = function(x) format(round(x, 3), nsmall = 3),
    # title = "Effect of NC on respondent's propensity to vote for the attacker",
    shape = group + term ~ model,
    estimate = "{estimate}{stars}{ifelse(conf.low %in~% 'NA','',paste0(' [',conf.low,',',conf.high,']'))}",
    statistic = NULL,
    output = "flextable",
    gof_map = gm,
    ...
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
    align(i = NULL, j = NULL, align = "center", part = "body") %>% 
    align(i = 1, j = NULL, align = "center", part = "header") %>% 
    align(i = NULL, j = 1:2, align = "left", part = "body") %>% 
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
