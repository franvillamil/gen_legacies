upr_lwr = function(df){

  if("std.error" %in% names(df)){df = df %>% rename(se = std.error)}
  if("estimate" %in% names(df)){df = df %>% rename(est = estimate)}
  df = df %>% mutate(
        upr = est + qnorm(0.975) * se,
        lwr = est - qnorm(0.975) * se,
        upr90 = est + qnorm(0.95) * se,
        lwr90 = est - qnorm(0.95) * se)
  if("p.value" %in% names(df)){df$sig95 = ifelse(df$p.value < 0.05, 1, 0)}
  return(df)
}


# Label models (for modelsummary)
modnames = function(l){
  names(l) = paste0("(", 1:length(l), ")")
  return(l)
}


# Function to make First Letter capital
capitalize = function(str){
  c = strsplit(str, " ")
  cu = lapply(c, function(x)
    paste(toupper(substring(x, 1,1)), substring(x, 2), sep="", collapse=" "))
  return(unlist(cu))
}


# Function to fix coordinates (Canary Islands)
# Borrowed from https://stackoverflow.com/questions/13757771
fix1 = function(object, params){
  r = params[1]
  scale = params[2]
  shift = params[3:4]
  object = elide(object, rotate = r)
  size = max(apply(bbox(object), 1, diff))/scale
  object = elide(object, scale = size)
  object = elide(object, shift = shift)
  object
}


# Simulations to dataframe
sim_to_df = function(simest, olab){
  df = data.frame(
    outcome = olab,
    mean = map_dbl(simest, mean),
    lwr = map_dbl(simest, quantile, 0.025),
    upr = map_dbl(simest, quantile, 0.975),
    lwr90 = map_dbl(simest, quantile, 0.05),
    upr90 = map_dbl(simest, quantile, 0.95))
  return(df)
}

# Inverse logit link function
invlink = function(x){return(1 / (1 + exp(-(x))))}

# Add covariates to simulated model
addcovs = function(df){
  df$female = 1
  df$ideology = mean(data$ideology, na.rm=T)
  df$age_l = mean(data$age_l, na.rm=T)
  df$cwrep_left_lp = mean(data$cwrep_left_lp, na.rm=T)
  df$educ_level = "primaria"
  df$ESTU = unique(data$ESTU)[1]
  return(df)
}

# Predictive probabilities (GLM)
predp = function(model, nd){
  nd$y = predict(model, newdata=nd, type="link")
  nd$se = predict(model, newdata=nd, type="link",se.fit=TRUE)$se.fit
  nd$upr = invlink(nd$y + qnorm(0.975) * nd$se)
  nd$lwr = invlink(nd$y - qnorm(0.975) * nd$se)
  nd$y = invlink(nd$y)
  return(nd)
}

# custom ggplot theme
my_theme = function() {
  theme_minimal() +#base_family = "Archivo Narrow") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}
