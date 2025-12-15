sim = function(model, int_values = NULL, n_sim = 1000){

  ## BASICS

  # Model components
  f = as.character(formula(model))
  interaction = ifelse(grepl(" \\* ", f[3]), TRUE, FALSE)
  terms = str_split(f[3], " ")[[1]]
  if(interaction){
    interaction_var = terms[which(terms == "*")+1]
  } else {interaction_var = NULL}
  coefs = model$coefficients
  outcome_var = f[2]
  cohort_var = terms[grepl("cohort", terms)]
  coh_var_values = paste0(cohort_var, model$xlevels[[cohort_var]])
  if(sum(terms == "*") > 1){stop("More than one interaction? Not supported")}
  terms = terms[!terms %in% c("*", "+")]
  if(any(grepl("factor\\(coh", names(coefs)))){
    stop("Cohort variable should not be included as 'factor(var)' - Just transform it before")}
  if(!any(grepl("^factor", names(coefs)))){
    stop("Please include at least some variable (FEs, etc) as 'factor(var)', otherwise change sim() code")}
  if(class(data[, cohort_var]) != "factor"){print("Cohort value must be a factor")}

  if(interaction & sum(grepl(paste0("^", interaction_var), names(coefs)))>1){
    int_factor = TRUE
    int_vars = names(coefs)[grepl(paste0("^", interaction_var), names(coefs))]
    interaction = FALSE
    print("Factor interaction (expanded dummies) used. Cohort variable must be a binary factor (i.e. only one coefficient).")
  } else {int_factor = FALSE}

  if(interaction & is.null(int_values)){stop("Int values must be supplied")}

  ## COVARIATE MATRIX

  # Matrix with cohort variables
  coh = names(coefs)[grepl("cohort", names(coefs)) & !grepl(":", names(coefs))]
  mat = matrix(0, ncol = length(coh), nrow = length(coh)+1,
    dimnames = list(NULL, coh))
  diag(mat) = 1
  matrix_rows = c(colnames(mat), coh_var_values[!coh_var_values %in% colnames(mat)])

  # If interaction, expand matrix
  if(interaction){
    mat = rep(1, length(int_values)) %x% mat
    colnames(mat) = coh
    matrix_rows = rep(matrix_rows, length(int_values))
    mat = cbind(mat, intvar = rep(int_values, each = nrow(mat)/length(int_values)))
    colnames(mat)[colnames(mat) == "intvar"] = interaction_var
    for(c in coh){
      mat = cbind(mat, new = mat[,c] * mat[, interaction_var])
      colnames(mat)[colnames(mat) == "new"] = paste0(c, ":", interaction_var)
    }
    matrix_rows = paste0(matrix_rows, "_", interaction_var, mat[,interaction_var])
  }

  # If factor interaction & expanded dummies, do it another way
  if(int_factor){

    mat_l = (length(coh)+1) * (length(int_vars)+1)
    mat = rep(1, length(int_vars) + 1) %x% mat
    colnames(mat) = coh

    int_mat_raw = matrix(0, ncol = length(int_vars), nrow = length(int_vars)+1)
    diag(int_mat_raw) = 1
    int_mat_raw = rep(1, length(coh)+1) %x% int_mat_raw
    ref_cat = model$xlevels[[interaction_var]][
      !model$xlevels[[interaction_var]] %in% gsub(interaction_var, "", int_vars)]
    matrix_rows = expand.grid(matrix_rows, c(int_vars, paste0(interaction_var, ref_cat)))
    matrix_rows = apply(matrix_rows, 1, paste, collapse = "_")

    colnames(int_mat_raw) = int_vars
    mat = cbind(mat, int_mat_raw)

    for(i in int_vars){
      mat = cbind(mat, new = mat[,i] * mat[, coh])
      colnames(mat)[colnames(mat) == "new"] = paste0(coh, ":", i)
    }

  }

  # Add further controls
  mat = cbind(mat,
    data.frame(
      female = 1,
      ideology = 5,
      age_l = 3.75,
      rural = 0,
      voted_lastelec = 1))
  # Add intercept to X matrix
  mat$Intercept = 1
  names(mat)[names(mat) == "Intercept"] = "(Intercept)"
  # Add fixed effects & factor variables (keep it at ref category)
  fe = names(coefs)[grepl("factor\\(", names(coefs))]
  mat = cbind(mat, matrix(0, ncol = length(fe), dimnames = list(NULL, fe)))

  # Check that all variables are there and order them
  if(!all(names(coefs) %in% names(mat))){stop("Missing variables in X matrix")}
  mat = mat[,match(names(coefs), names(mat))]

  ## SIMULATE

  # Check that model is glm
  if(!"glm" %in% class(model)){stop("Model is not GLM?")}

  # VCOV matrix
  covmat = vcov(model)
  # Random draws of coefficients
  betadraw = mvrnorm(n = n_sim, mu = coefs, Sigma = covmat)

  # Create list of point estimates and add them from sims
  estimates = vector("list", length(matrix_rows))
  names(estimates) = matrix_rows
  for(i in 1:length(matrix_rows)){
    name = matrix_rows[i]
    estimates[[name]] = 1 / (1 + exp(-(betadraw %*% as.numeric(mat[i,]))))
  }

  ## RETURN
  return(estimates)

}

### ================================================================================
### ================================================================================
### ================================================================================

sim_age = function(model, age_val, age_vars = c("age", "age2"), n_sim = 1000){

  ## BASICS

  # Model components
  f = as.character(formula(model))
  outcome_var = f[2]
  terms = str_split(f[3], " ")[[1]]
  terms = terms[!terms %in% c("*", "+")]
  coefs = model$coefficients

  ## COVARIATE MATRIX

  # Matrix with age variables
  mat = matrix(0, nrow = length(age_val), ncol = 2, dimnames = list(NULL, age_vars))
  mat[,age_vars[1]] = age_val
  mat[,age_vars[2]] = mat[,age_vars[1]] ^ 2

  # Add further controls
  mat = cbind(mat, data.frame(
    female = 1,
    ideology = 5,
    rural = 0,
    voted_lastelec = 1))
  # Add intercept to X matrix
  mat$Intercept = 1
  names(mat)[names(mat) == "Intercept"] = "(Intercept)"
  # Add fixed effects & factor variables (keep it at ref category)
  fe = names(coefs)[grepl("factor\\(", names(coefs))]
  mat = cbind(mat, matrix(0, ncol = length(fe), dimnames = list(NULL, fe)))

  # Check that all variables are there and order them
  if(!all(names(coefs) %in% names(mat))){stop("Missing variables in X matrix")}
  mat = mat[,match(names(coefs), names(mat))]

  ## SIMULATE

  # Check that model is glm
  if(!"glm" %in% class(model)){stop("Model is not GLM?")}

  # VCOV matrix
  covmat = vcov(model)
  # Random draws of coefficients
  betadraw = mvrnorm(n = n_sim, mu = coefs, Sigma = covmat)

  # Create list of point estimates and add them from sims
  estimates = vector("list", length(age_val))
  names(estimates) = paste0("age", age_val)
  for(i in 1:length(age_val)){
    name = paste0("age", age_val[i])
    estimates[[name]] = 1 / (1 + exp(-(betadraw %*% as.numeric(mat[i,]))))
  }

  ## RETURN
  return(estimates)


}
