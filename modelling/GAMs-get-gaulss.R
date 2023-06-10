# Setup
library(tidyverse)
library(mgcv)

# Old version used this, can delete once I'm sure.
# # Load all_data, output by `all_data.R`
# all_data <- read_rds("all_data_local.rds") # made on local using the csvs
# all_data_new <- read_rds("all_data_new.rds") # made on local using new rds files

# Load data (all predictors except prev_* ones; lag done in prepare_data)
fitdata        <-read_rds("surps_lms.RTs.rds")
fitdata_perword<-read_rds("surps_lms.RTs.perword.rds")


source("prepare_data.R") # functions for preparing ns SPRT surprisal data

fit_GAM_SPRT <- function(
    formula, modelname, data, data_perword,
    how_many_prevs = 1, perword = FALSE, .fit=bam, ...){
  #' fit a GAM of SPRT as a function of surprisal
  #' @param formula mcgv formula
  #' @param modelname name of model for surprisal
  if (perword) {
    cat(glue::glue("preparing perword data for model {modelname}\n"))
    data_prepared <- prepare_data_perword(
      data_perword, modelname, how_many_prevs = how_many_prevs)
  } else {
    cat(glue::glue("preparing data for model {modelname}\n"))
    data_prepared <- prepare_data_with_perword(
      data, data_perword,
      modelname, how_many_prevs = how_many_prevs, 
      drop_nas = FALSE)
  }
  .fit(formula, data = data_prepared, ...)
}

# just for testing, get a subset of subjects to test on
subset_data_byNworkers <- function(N, nItems_threshold = 500, data = all_data) {
  how_many_stories_per_worker <- data %>% group_by(WorkerId) %>%
    summarise(n_stories=n_distinct(story_num)) %>% arrange(desc(n_stories))
  how_many_items_per_worker <- data %>% group_by(WorkerId) %>%
    count() %>% arrange(desc(n)) %>%
    mutate(is_productive = n > nItems_threshold)
  selected_workers <- full_join(
    how_many_stories_per_worker, how_many_items_per_worker, by = "WorkerId") %>%
    arrange(desc(is_productive), desc(n_stories), (n-median(how_many_items_per_worker$n))^2) %>%
    head(N)
  # selected_workers <- how_many_items_per_worker %>%
  #   arrange((n-median(how_many_items_per_worker$n))^2) %>%
  #   head(N)
  data %>%
    filter(WorkerId %in% selected_workers$WorkerId)
}

lm_names_wsuffixes <- list(
  "boyce_ngram", "boyce_grnn", #"boyce_txl",
  "TXL",
  "TXL_80",
  "TXL_bysent",
  "GPT2",
  "GPT2_80",
  "GPT2_bysent",
  "GPT2-large",
  "GPT2-large_80",
  "GPT2-large_bysent",
  "GPT2-xl",
  "GPT2-xl_80",
  "GPT2-xl_bysent",
  "GPT-J",
  "GPT-J_80",
  "GPT-J_bysent",
  "GPT-Neo",
  "GPT-Neo_80",
  "GPT-Neo_bysent",
  "GPT3-ada",
  "GPT3-ada_bysent",
  "GPT3-curie",
  "GPT3-curie_bysent",
  "GPT3-davinci",
  "GPT3-davinci_bysent"
  ) %>% set_names(., .)

fitmodels <- function(
    modelnames, formula, 
    data,
    data_perword,
    saveprefix="COMPUTE-gams/", 
    how_many_prevs = 1,
    ...) {
  cat("Fitting model with\n- formula:\n")
  print(formula)
  cat("- modelname(s): \n")
  print(paste(names(modelnames), sep = "; "))
  mappl <- if (length(modelnames)>1) mcmapply else mapply
  fittedmodels <- mappl(
    FUN = function(
        formula, modelname, data, data_perword, saveprefix, ...) {
      filename <- paste0(saveprefix, modelname, ".rds")
      print(paste("Fitting surprisals from", modelname))
      m <- write_rds(
        fit_GAM_SPRT(
          formula, modelname, data, data_perword,
          how_many_prevs = how_many_prevs,
          ...),
        file = filename)
      print(paste("Saved to", filename))
      return(m)
    },
    modelname = modelnames,
    MoreArgs = list(formula = formula,
                    data = data,
                    data_perword = data_perword,
                    saveprefix = saveprefix,
                    ...),
    SIMPLIFY = FALSE)
  return(fittedmodels)
}

# Formulae for gaulss mean effect (no previous)
formula_basic <- response ~ 
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1) + te(freq, length)
formula_basic_fsk6 <- response ~ 
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1, k = 6) + te(freq, length)
formula.1_fsk6 <- ~ 
  s(surprisal, bs = "tp", k = 6) + 
  s(subject, surprisal, bs = "fs", m = 1, k = 6)

formula_basic_linear <- response ~
  surprisal + s(subject, bs = "re") + s(surprisal, subject, bs = "re") + 
  te(freq, length)
formula.1_basic_linear <- ~
  surprisal + s(subject, bs = "re") + s(surprisal, subject, bs = "re")

# Formula for gaussian mean effect (with previous, as models before)
formula_wprev <- response ~
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1) + te(freq, length) +
  s(prev_surprisal, bs = "tp") +
  s(subject, prev_surprisal, bs = "fs", m = 1) + te(prev_freq, prev_length)
# Formula for gaussian linear control mean effect (with previous, as models before)
formula_wprev_linear <- response ~
  surprisal +
  s(subject, bs = "re") +
  s(surprisal, subject, bs = "re") + te(freq, length) +
  prev_surprisal +
  s(prev_surprisal, subject, bs = "re") + te(prev_freq, prev_length)


# Formulae for gaussian mean effect (with previous) all restricted wiggliness
formula_wprevk6 <- response ~
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1, k = 6) + te(freq, length) +
  s(prev_surprisal, bs = "tp", k = 6) +
  s(subject, prev_surprisal, bs = "fs", m = 1, k = 6) + te(prev_freq, prev_length)

# As above, but with two previous words for predictors of mean response
formula_w2prevk6 <- response ~
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1, k = 6) + te(freq, length) +
  s(prev_surprisal, bs = "tp", k = 6) +
  s(subject, prev_surprisal, bs = "fs", m = 1, k = 6) + te(prev_freq, prev_length) + 
  s(prev2_surprisal, bs = "tp", k = 6) +
  s(subject, prev2_surprisal, bs = "fs", m = 1, k = 6) + te(prev2_freq, prev2_length)

# As above, but with three previous words for predictors of mean response
formula_w3prevk6 <- response ~
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1, k = 6) + te(freq, length) +
  s(prev_surprisal, bs = "tp", k = 6) +
  s(subject, prev_surprisal, bs = "fs", m = 1, k = 6) + te(prev_freq, prev_length) + 
  s(prev2_surprisal, bs = "tp", k = 6) +
  s(subject, prev2_surprisal, bs = "fs", m = 1, k = 6) + te(prev2_freq, prev2_length) + 
  s(prev3_surprisal, bs = "tp", k = 6) +
  s(subject, prev3_surprisal, bs = "fs", m = 1, k = 6) + te(prev3_freq, prev3_length)


fit_timed <- function(
    modelnames = lm_names_wsuffixes$`GPT3-davinci`,
    formula = formula_basic,
    saveprefix = "COMPUTE-gams-gaulsstest/simple_",
    data = fitdata, data_perword = fitdata_perword,
    how_many_prevs = 1,
    ...
    ){
  system.time(
    ms <- fitmodels(
      modelnames, formula, 
      data = data, 
      data_perword = data_perword, 
      saveprefix,
      how_many_prevs = how_many_prevs,
      ...)
  ) %>% print()
  return(ms)
}

fit_gaulss <- function(
    modelnames = lm_names_wsuffixes$`GPT3-davinci`,
    formula = list(formula_basic, ~s(surprisal)),
    saveprefix = "COMPUTE-gams-gaulsstest/simple_gaulss_",
    data = fitdata, data_perword = fitdata_perword,
    how_many_prevs = 1,
    ...
    ){
  system.time(
    ms <- fitmodels(
      modelnames, formula, 
      data = data, 
      data_perword = data_perword, 
      saveprefix,
      how_many_prevs = how_many_prevs,
      .fit = gam, 
      family = gaulss(), 
      ...)
  ) %>% print()
  return(ms)
}
