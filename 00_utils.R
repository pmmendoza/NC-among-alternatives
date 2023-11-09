# required packages -------------------------------------------------------
# FIXME: remove this if not needed
pacman::p_load(
  broom, 
  broom.mixed, 
  marginaleffects,
  tidyverse
)


# data wrangling ----------------------------------------------------------
# function to detect duplicates after merging
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
          col_overlap = paste0(names(res)[res == F], collapse = ", ")
        )
    })
}

# Defining outliers
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

# simple data viz ---------------------------------------------------------
# Quickly plot the distribution of groups as a barplot
bars_by_group <- function(data = xdf, y = NA, group = NA, clr = NA, order = T, lab = T) {
  data %>%
    group_by({{group}}) %>%
    summarise(
      "{{y}}" := mean ({{y}}, na.rm = T)
    ) %>%
    ungroup() -> x
  if(order) {
    x <- x %>%
      mutate(
        "{{group}}" := {{group}} %>%
          as.character() %>%
          fct_reorder({{y}})
      )
  }
  temp <- x %>%
    filter(complete.cases(.)) %>%
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
};scatterandsmooth(iris, Sepal.Length, Sepal.Width, Petal.Length) + labs(title = "Example Plot")

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

# modelling ---------------------------------------------------------------
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
