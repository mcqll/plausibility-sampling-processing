# Setup
library(tidyverse)
library(mgcv)
# library(parallel)
# cl <- makeCluster(detectCores())

# Load all_data, output by `all_data.R`
all_data <- read_rds("all_data_local.rds") # made on local using the csvs

source("prepare_data.R")

fit_GAM_SPRT <- function(formula, modelname, data, perword=FALSE, .fit=bam, ...){
  #' fit a GAM of SPRT as a function of surprisal
  #' @param formula mcgv formula
  #' @param modelname name of model for surprisal
  if (perword) {
    data_prepared <- prepare_data.perword(data, modelname)
  } else {
    data_prepared <- prepare_data(data, modelname)
  }
  .fit(formula, data = data_prepared, ...)
}

# just for testing, get a subset of subjects to test on
# see exploration in `EDA_surps_SPRT.Rmd` for justification of this subsetting
subset_data_byNworkers <- function(N, nItems_threshold=500, data=all_data) {
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

{# formulae I'm using
  formula_fsm1_k6 <- response ~
    s(surprisal, bs = "tp", k = 6) + s(subject, surprisal, bs = "fs", m = 1) + te(freq, length) + s(prev_surprisal, bs = "tp") + s(subject, prev_surprisal, bs = "fs", m = 1) + te(prev_freq, prev_length)
}
{
  # formulae I'm not using
  # formula_fsm1_k20 <- response ~
  #   s(surprisal, bs = "tp", k = 20) +
  #   s(subject, surprisal, bs = "fs", m = 1) + te(freq, length) +
  #   s(prev_surprisal, bs = "tp") +
  #   s(subject, prev_surprisal, bs = "fs", m = 1) + te(prev_freq, prev_length)
  # formula_fsm1_k12 <- response ~
  #   s(surprisal, bs = "tp", k = 12) +
  #   s(subject, surprisal, bs = "fs", m = 1) + te(freq, length) +
  #   s(prev_surprisal, bs = "tp") +
  #   s(subject, prev_surprisal, bs = "fs", m = 1) + te(prev_freq, prev_length)
  # formula_fsm1_k40 <- response ~
  #   s(surprisal, bs = "tp", k = 40) +
  #   s(subject, surprisal, bs = "fs", m = 1) + te(freq, length) +
  #   s(prev_surprisal, bs = "tp") +
  #   s(subject, prev_surprisal, bs = "fs", m = 1) + te(prev_freq, prev_length)
  #
  # formula_linear0 <- response ~
  #   surprisal +
  #   s(surprisal, subject, bs = "re") + te(freq, length) +
  #   prev_surprisal +
  #   s(prev_surprisal, subject, bs = "re") + te(prev_freq, prev_length)
  #
  # formula_linear1 <- response ~
  #   surprisal +
  #   s(subject, bs = "re") +
  #   s(surprisal, subject, bs = "re") + te(freq, length) +
  #   prev_surprisal +
  #   s(prev_surprisal, subject, bs = "re") + te(prev_freq, prev_length)
  #
  # formula_fsm1_k6.perword <- response ~
  #   s(surprisal, bs = "tp", k = 6) + te(freq, length) +
  #   s(prev_surprisal, bs = "tp") + te(prev_freq, prev_length)
  #
  # formula_linear.perword <- response ~
  #   surprisal + te(freq, length) +
  #   prev_surprisal + te(prev_freq, prev_length)
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
    modelnames, formula, data,
    saveprefix="COMPUTE-gams/", ...) {
  cat("Fitting...\n- formula:      ")
  print(formula)
  cat("- modelname(s): ")
  cat(names(modelnames), sep = "; ")
  mappl <- if (length(modelnames)>1) mcmapply else mapply
  fittedmodels <- mappl(
    FUN = function(formula, modelname, data, saveprefix, ...) {
      filename <- paste0(saveprefix, modelname, ".rds")
      print(paste("Fitting surprisals from", modelname))
      m <- write_rds(
        fit_GAM_SPRT(formula, modelname, data, ...),
        file = filename)
      print(paste("Saved to", filename))
      return(m)
    },
    modelname = modelnames,
    MoreArgs = list(formula = formula,
                    data = data,
                    saveprefix = saveprefix,
                    ...),
    SIMPLIFY = FALSE)
  return(fittedmodels)
}
#
# { # testing version (uses only subset of speakers)
#   all_data_subset <- subset_data_byNworkers(80)
#   system.time(
#     m.linear<-fitmodels(
#       lm_names_wsuffixes, formula_linear1,
#       all_data_subset,saveprefix = "COMPUTE-scratch/linear1_")) %>% print()
#   system.time(
#     m.fsm1_k6<-fitmodels(
#       lm_names_wsuffixes, formula_fsm1_k6,
#       all_data_subset,saveprefix = "COMPUTE-scratch/fsm1_k6_")) %>% print()
#   system.time(
#     m.fsm1_k12<-fitmodels(
#       lm_names_wsuffixes, formula_fsm1_k12,
#       all_data_subset,saveprefix = "COMPUTE-scratch/fsm1_k12_")) %>% print()
#   system.time(
#     m.fsm1_k20<-fitmodels(
#       lm_names_wsuffixes, formula_fsm1_k20,
#       all_data_subset,saveprefix = "COMPUTE-scratch/fsm1_k20_")) %>% print()
#   system.time(
#     m.fsm1_k40<-fitmodels(
#       lm_names_wsuffixes, formula_fsm1_k40,
#       all_data_subset,saveprefix = "COMPUTE-scratch/fsm1_k40_")) %>% print()
# }
#
# { # fit all (beware, of course, slow!)
#   system.time(
#     m.linear <- fitmodels(
#       lm_names_wsuffixes, formula_linear1,
#       all_data,saveprefix = "COMPUTE-gams/linear1_")) %>% print()
#   system.time(
#     m.fsm1_k6 <- fitmodels(
#       lm_names_wsuffixes, formula_fsm1_k6,
#       all_data,
#       saveprefix = "COMPUTE-gams/fsm1_k6_")) %>% print()
# }
#
# { # fit all perword
#     system.time(
#         m.linear <- fitmodels(lm_names_wsuffixes,
#             formula_linear.perword,
#             all_data.perword,
#             saveprefix = "COMPUTE-gams/perword_linear_",
#             perword = TRUE))
#     system.time(
#         m.fsm1_k6 <- fitmodels(lm_names_wsuffixes,
#             formula_fsm1_k6.perword,
#             all_data.perword,
#             saveprefix = "COMPUTE-gams/perword_fsm1_k6_",
#             perword = TRUE))
# }
#

#### Testing Gaulss

# fitdata <- subset_data_byNworkers(5)

formula_test <- response ~
  s(surprisal, bs = "tp", k = 6) +
  s(subject, surprisal, bs = "fs", m = 1) + te(freq, length)

fit_gauss<-function(
    modelnames = lm_names_wsuffixes$`GPT3-davinci`,
    formula = formula_test,
    saveprefix = "COMPUTE-gams-gaulsstest/simple_",
    data = fitdata
){
  system.time(
    ms <- fitmodels(modelnames,formula, data = data, saveprefix, .fit = gam)
  ) %>% print()
  return(ms)
}

fit_gaulss<-function(
    modelnames = lm_names_wsuffixes$`GPT3-davinci`,
    formula = list(formula_test, ~s(surprisal)),
    saveprefix = "COMPUTE-gams-gaulsstest/simple_gaulss_",
    data = fitdata, ...
){
  system.time(
    ms <- fitmodels(modelnames, formula, data = data, saveprefix, .fit = gam,
                    family = gaulss(), ...)
  ) %>% print()
  return(ms)
}
#
# r_20varbysubj <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/20varbysubjGPT3-davinci.rds")
# plot(r_20varbysubj, pages=1, main = "r_20varbysubj", scale=0)
#
# r_80varbysubj <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/80varbysubjGPT3-davinci.rds")
# plot(r_80varbysubj, pages=1, main = "r_80varbysubj", scale=0)
#
# r_20varbysubj_varfsk6 <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/20varbysubj_varfsk6GPT3-davinci.rds")
# plot(r_20varbysubj_varfsk6, pages=1, main = "r_20varbysubj_varfsk6", scale=0)
#
# r_80varbysubj_varfsk6 <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/80varbysubj_varfsk6GPT3-davinci.rds")
# plot(r_80varbysubj_varfsk6, pages=1, main = "r_80varbysubj_varfsk6", scale=0)
#
# r_80varbysubj_varfsk6_LSTM <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/80varbysubj_varfsk6_LSTMboyce_grnn.rds")
# plot(r_80varbysubj_varfsk6_LSTM, pages=1, main = "r_130varbysubj_varfsk6_LSTM", scale=0)
#
# # r_130varbysubj_varfsk6 <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/130varbysubj_varfsk6GPT3-davinci.rds")
# # plot(r_130varbysubj_varfsk6, pages=1, main = "r_130varbysubj_varfsk6", scale=0)
#
# r_130varbysubj_varfsk6_LSTM <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/130varbysubj_varfsk6_LSTMboyce_grnn.rds")
# plot(r_130varbysubj_varfsk6_LSTM, pages=1, main = "r_130varbysubj_varfsk6_LSTM", scale=0)
#
# r_130varbysubj_varfsk6_ngram <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/130varbysubj_varfsk6_ngramboyce_ngram.rds")
# plot(r_130varbysubj_varfsk6_ngram, pages=1, main = "r_130varbysubj_varfsk6_ngram", scale=0)
#
# r_20vark6 <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/20vark6GPT3-davinci.rds")
# plot(r_20vark6, pages=1, main = "r_20vark6", scale=0)
#
# r_80vark6 <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/80vark6GPT3-davinci.rds")
# plot(r_80vark6, pages=1, main = "r_80vark6", scale=0)
#
# r_nobysubjk6_GPT3d <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/allvark6GPT3-davinci.rds")
# plot(r_nobysubjk6_GPT3d, pages=1, main = "r_nobysubjk6_GPT3d", scale=0)
#
# ### !
# r_GPT3d_k20 <- read_rds("COMPUTE-gams-gaulsstest-remote/COMPUTE-gams-r/allvarbysubjGPT3-davinci.rds")
# plot(r_GPT3d_k20, pages=1, main = "r_GPT3d", scale=0)



plotsmooth <- function(
    gam_model, select=1, trans=I, ...) {
  p <- {
    pdf(NULL) # hack to get plot object without plotting
    res <- mgcv::plot.gam(gam_model, residuals=T, ...)
    invisible(dev.off())
    res
  }
  p <- p[[select]] # get just the selected plot
  smooth_df <- as_tibble(p[c("x","se","fit")])
  # data_df <- as.data.frame(p[c("raw", "p.resid")])

  smooth_df %>%
    ggplot(aes(x = x, y = trans(fit))) +
    # geom_rug(data = data_df, aes(x = raw, y = NULL), sides = "b") +
    # geom_point(data = data_df, aes(x = raw, y = p.resid)) +
    geom_ribbon(aes(ymin = trans(fit - se), ymax = trans(fit + se), y = NULL), alpha = 0.3) +
    geom_line() +
    labs(x = p$xlab, y = p$ylab)
}
plotsmooth(r_20varbysubj, select=4)

# same as:
plot(r_20varbysubj, select=4, scale=0, shade=T)


#################################
#moved to GAMs-plot-gaulss.R
###########################################


m_test_ll <- mgcv::gam(
  formula = list(response ~ s(surprisal), ~s(surprisal)),
  data = prepare_data(subset_data_byNworkers(10), "GPT3-davinci"),
  family = mgcv::gaulss(link=c("log", "logb")))
plot(m_test_ll, pages=1, shade=T, scale=0)


library(patchwork)

plotvar <- function(m, trans=I) {
  plotsmooth(m, select=4, trans=trans, yshift=coef(m)["(Intercept).1"]) + ylab("log(stdev in RT + 0.01)") + ylim(-.75,2)
}
wrap_plots(
plotvar(r_130varbysubj_varfsk6_ngram) + labs(title="ngram"),
plotvar(r_130varbysubj_varfsk6_LSTM) + labs(title="LSTM"),
plotvar(r_allvarbysubj_varfsk6) + labs(title="GPT3-davinci")
)

wrap_plots(
  plot_mean_and_var(r_130varbysubj_varfsk6_ngram) + labs(title="ngram"),
  plot_mean_and_var(r_130varbysubj_varfsk6_LSTM) + labs(title="LSTM"),
  plot_mean_and_var(r_allvarbysubj_varfsk6) + labs(title="GPT3-davinci")
)


# do.call(
#   wrap_plots,
#   mapply(function(lm) {
#     m <- mgcv::gam(
#       formula = list(response ~ s(surprisal), ~s(surprisal)),
#       data = prepare_data(subset_data_byNworkers(1), lm),
#       family = mgcv::gaulss())
#     plotsmooth(m, select=2) + labs(title=lm)},
#     lm_names_wsuffixes, SIMPLIFY = F
#   )
# ) + plot_annotation(title = "variance smooth (scaled as log(stdev + 0.01))")

