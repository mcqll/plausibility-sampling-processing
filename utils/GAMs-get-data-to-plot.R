c('tidyverse','mgcv') |>
vapply(library, logical(1), logical.return = TRUE, character.only = TRUE)

theme_set(theme_minimal())
theme_update(plot.title.position = "plot")

##### constants #####

plot_N <- 200 # how many x-values to use for getting plot data

context_len_labs <- c(
  max = "maximum",
  `80` = "80 words",
  bysent = "within sent."
)
# To only use two context lens (else all)
twocontexts <- TRUE
if (twocontexts) {
  context_lens <- c("max", "bysent")
  context_pref <- "2"
} else {
  context_lens <- c("max", "80", "bysent")
  context_pref <- ""
}
est_type_labs <- c(
  mean = "mean",
  var = "variance"
)
model_labs <- c(
  boyce_ngram = "n-gram",
  boyce_grnn = "LSTM",
  "TXL" = "TXL",
  "GPT2" = "GPT2",
  "GPT2-large" = "GPT2-large",
  "GPT2-xl" = "GPT2-xl",
  "GPT-Neo" = "GPT-Neo",
  "GPT-J" = "GPT-J",
  "GPT3-ada" = "GPT3-ada",
  "GPT3-curie" = "GPT3-curie",
  "GPT3-davinci" = "GPT3-davinci"
)
lm_names <- list(
  "boyce_ngram", "boyce_grnn",
  "TXL",
  "GPT2",
  "GPT2-large",
  "GPT2-xl",
  "GPT-Neo",
  "GPT-J",
  "GPT3-ada",
  "GPT3-curie",
  "GPT3-davinci"
) %>% set_names(., .)

#### utility functions ####

separate_modelnames <- function(df, modelnames_colname = model, names = lm_names) {
  df %>%
    separate({{modelnames_colname}},
             into = c("model", "by"), sep = "_", extra = "merge") %>%
    mutate(model = ifelse(model == "boyce", paste0("boyce_", by), model)) %>%
    mutate(context_len = ifelse(
      is.na(by), "max", ifelse(str_starts(model, "boyce"), "bysent", by))
    ) %>% select(-by) %>%
    mutate(context_len = fct_relevel(context_len, c("max")),
           model = fct_relevel(model, lm_names))
}
# for nicer plot labels
my_labeller <- labeller(
  context_len = context_len_labs,
  model = model_labs,
  est_type=c(mean="mean RT", var=glue::glue("log(sd RT)")))

evaluate_term <- function(
    gam_model, term="s.1(surprisal)", n=plot_N, type = "terms", shift = NULL) {
  # should act like `evaluate_parametric_term` from gratia
  # Note: `shift` should equal NULL (no shift) or a string "(Intercept)" or "(Intercept).1"
  # specifying the right intercept coef to shift by
  x <- gam_model$model$surprisal
  predict_at <- seq(min(x), max(x), length.out = n)
  preds <- predict(
    gam_model, newdata = data.frame(surprisal = predict_at, prev_surprisal = 0),
    newdata.guaranteed = TRUE, type = type, terms = term, se.fit = TRUE)
  df <- tibble(surprisal = predict_at,
         est = as.data.frame(preds$fit)[[term]],
         se = as.data.frame(preds$se.fit)[[term]])
  if (!is.null(shift)) {
    df <- df %>%  mutate(est_shifted = est + coef(gam_model)[shift])
  }
  return(df)
}

evaluate_sig2_constant <- function(
    gam_model, n=plot_N, shift = NULL) {
  # mimic evaluate_term for constant sig2 term, return transformed as log(sd).
  x <- gam_model$model$surprisal
  predict_at <- seq(min(x), max(x), length.out = n)
  df <- tibble(surprisal = predict_at,
         est = 0, # constant is zero without shift
         se = 0) # se is zero for this TODO: is it?
  if (!is.null(shift)) {
    df <- df %>%  mutate(est_shifted = est + log(sqrt(gam_model$sig2)))
  }
  return(df)
}

#### data-getting functions ####

get_plotdata_mean_or_var <- function(
    m, what="mean", fit_type="smooth", shifted=FALSE, n=plot_N) {
  # Note, if `shifted` = TRUE, will shift by the appropriate intercept coef.
  if (fit_type=="smooth") {
    if (what=="mean") {
      term<-"s(surprisal)"
      shift <- if (shifted) "(Intercept)" else NULL
    } else if (what=="var") {
      term <-"s.1(surprisal)"
      shift <- if (shifted) "(Intercept).1" else NULL
    }
    return(evaluate_term(m, term=term, n=n, shift=shift))
  }
  if (fit_type=="smooth0var") {
    if (what=="mean") {
      term<-"s(surprisal)"
      shift <- if (shifted) "(Intercept)" else NULL
      return(evaluate_term(m, term=term, n=n, shift=shift))
    } else if (what=="var") {
      term<-"surprisal"
      shift <- if (shifted) "anything" else NULL
      return(evaluate_sig2_constant(m, n=n, shift=shift))
    }
  }
  if (fit_type=="linear") {
    if (what=="mean") {
      term<-"surprisal"
      shift <- if (shifted) "(Intercept)" else NULL
    } else if (what=="var") {
      term <-"surprisal.1"
      shift <- if (shifted) "(Intercept).1" else NULL
    }
    return(evaluate_term(m, term=term, n=n, shift=shift))
  }
  if (fit_type=="lin0var") {
    if (what=="mean") {
      term<-"surprisal"
      shift <- if (shifted) "(Intercept)" else NULL
      return(evaluate_term(m, term=term, n=n, shift=shift))
    } else if (what=="var") {
      term<-"surprisal"
      shift <- if (shifted) "anything" else NULL
      return(evaluate_sig2_constant(m, n=n, shift=shift))
    }
  }
}

plot_mean_and_var <- function(
    m, use_shifted_est=TRUE, fit_type="smooth", n=plot_N, se.mult=1.96) {
  meandata <- get_plotdata_mean_or_var(m, what="mean", fit_type=fit_type, shifted=T, n=n)
  vardata <- get_plotdata_mean_or_var(m, what="var", fit_type=fit_type, shifted=T, n=n)
  data <- rbind(
    meandata %>% mutate(est_type="mean"),
    vardata %>% mutate(est_type="var"))
  if (use_shifted_est) data <- mutate(data, est = est_shifted)
  data %>%
    ggplot(aes(x = surprisal, y = est, ymin = est - se.mult*se, ymax = est + se.mult*se)) +
    geom_ribbon(aes(y=NULL), alpha = 0.3) + geom_line() +
    facet_grid(est_type~., scales="free")
}
# Example
# plot_mean_and_var(gam_GPT3d, use_shifted_est=F) + labs(title = "GP3-davinci")
# plot_mean_and_var(gam_lin0var_GPT3d, fit_type = "lin0var") + labs(title = "GPT3-davinci linear, constant variance")

get_data_from_rds_file <- function(
    f, 
    shifted=TRUE, fit_type="smooth", 
    n=plot_N, gc_after=T) {
  # shifted=TRUE will result in two output cols: raw `est`, and `est_shifted` = est + intercept
  message(c("Getting data (","fit_type=", fit_type, ") from file: ", f))
  m <- read_rds(f)
  meandata <- get_plotdata_mean_or_var(m, what="mean", fit_type=fit_type, shifted=shifted, n=n)
  vardata <- get_plotdata_mean_or_var(m, what="var", fit_type=fit_type, shifted=shifted, n=n)
  data <- rbind(
    meandata %>% mutate(est_type="mean"),
    vardata %>% mutate(est_type="var"))
  if (gc_after) gc()
  data
}

get_data_to_plot_from_rds_files <- function(gam_rds_files, ...) {
  dfs <- lapply(gam_rds_files, get_data_from_rds_file, ...)
  df <- bind_rows(dfs, .id = "model")
  df %>%
    separate_modelnames(modelnames_colname = model, names = lm_names)
}

save_plot.gam_all <- function(
    gam_rds_files, prefix,
    height=10, width=12, dir = "GAM-plots/plot.gam/",
    ...) {
  # applies plot.gam to all the files in the list, and outputs pdfs
  mapply(
    FUN = function(f, modelname) {
      m <- read_rds(f)
      formulastring <- strwrap(m$formula, width = 150, simplify = F) %>% lapply(\(.)paste0(.,collapse = "\n")) %>% paste0(collapse="\n")
      print(formulastring)
      title <- paste0(modelname)
      pdf(file = paste0(dir, prefix , modelname, ".pdf"),
          height = height, width = width)
      m %>% plot(pages = 1, scale = 0, ...)
      mtext(modelname, side = 3, line = 2, adj = 0, font = 2)
      mtext(formulastring, side=3, line = 1.6, adj = 0.3, cex = .6)
      dev.off()
      },
    f = gam_rds_files,
    modelname = names(gam_rds_files))
}
## Example:
# save_plot.gam_all(rds_files_smooth, dir = "GAM-plots/plot.gam/", prefix = "noprev_")


get_plotdata_and_save_csv <- function(rds_files, fit_type, id, save_plots = "GAM-plots/plot.gam/"){
    #' `rds_files` is list of rds files like `rds_files_var0_gauss_w3prev` (defined in GAMs-get-data-to-plot.R below)
    #' `fit_type` should be in c("smooth", "linear", "lin0var", "smooth0var")
    #' `id` is string to identify these plots, for example "var0_gauss_wprev" 
    #' `save_plots` string for dir to save gam.plot() in for each model in rds_files
    #'              or, set save_plots = NULL to disable saving plots.
    if (!is.null(save_plots)){
        save_plot.gam_all(rds_files, dir = save_plots, prefix = glue::glue("{id}_"))
    }
    data_to_plot <- get_data_to_plot_from_rds_files(rds_files, fit_type = fit_type)
    data_to_plot %>% write_csv(glue::glue("GAM-plots/data_to_plot_{id}_N{plot_N}.csv"), col_names = TRUE)
}

# Example
# get_plotdata_and_save_csv(rds_files=rds_files_var0_gauss_wprev, fit_type="smooth0var", id="var0_gauss_wprev")


#### get data to plot #####

######################################## NOPREV:
##### Smooth noprev ######
rds_files_smooth <- list(
  "boyce_ngram"         = "COMPUTE-smooth-noprev-various/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-smooth-noprev-various/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-smooth-noprev-various/TXL.rds",
  "GPT2"                = "COMPUTE-smooth-noprev-various/GPT2.rds",
  "GPT2-large"          = "COMPUTE-smooth-noprev-various/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-smooth-noprev-various/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-smooth-noprev-various/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-smooth-noprev-various/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-smooth-noprev-various/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-smooth-noprev-various/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-smooth-noprev-various/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-smooth-noprev-various/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-smooth-noprev-various/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-smooth-noprev-various/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-smooth-noprev-various/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-smooth-noprev-various/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-smooth-noprev-various/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-smooth-noprev-various/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-smooth-noprev-various/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-smooth-noprev-various/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-smooth-noprev-various/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-smooth-noprev-various/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-smooth-noprev-various/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-smooth-noprev-various/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-smooth-noprev-various/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-smooth-noprev-various/GPT3-davinci_bysent.rds"
)

##### Linear control constant variance, with family=gaussian() ####
rds_files_lin0var <- list(
  "boyce_ngram"         = "COMPUTE-lin0vargam6_096G_02H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-lin0vargam6_096G_02H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-lin0vargam6_096G_02H/TXL.rds",
  "GPT2"                = "COMPUTE-lin0vargam6_096G_02H/GPT2.rds",
  "GPT2-large"          = "COMPUTE-lin0vargam6_096G_02H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-lin0vargam6_096G_02H/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-lin0vargam6_096G_02H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-lin0vargam6_096G_02H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-lin0vargam6_096G_02H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-lin0vargam6_096G_02H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-lin0vargam6_096G_02H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-lin0vargam6_096G_02H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-lin0vargam6_096G_02H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-lin0vargam6_096G_02H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-lin0vargam6_096G_02H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-lin0vargam6_096G_02H/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-lin0vargam6_096G_02H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-lin0vargam6_096G_02H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-lin0vargam6_096G_02H/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-lin0vargam6_096G_02H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-lin0vargam6_096G_02H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-lin0vargam6_096G_02H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-lin0vargam6_096G_02H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-lin0vargam6_096G_02H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-lin0vargam6_096G_02H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-lin0vargam6_096G_02H/GPT3-davinci_bysent.rds"
)

##### Linear control with linear variance, with family=gaulss() ####
rds_files_linear <- list(  # was COMPUTE-linear6_384G_12H
  "boyce_ngram"         = "COMPUTE-linear2_128G_08H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-linear2_128G_08H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-linear2_128G_08H/TXL.rds",
  "GPT2"                = "COMPUTE-linear2_128G_08H/GPT2.rds",
  "GPT2-large"          = "COMPUTE-linear2_128G_08H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-linear2_128G_08H/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-linear2_128G_08H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-linear2_128G_08H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-linear2_128G_08H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-linear2_128G_08H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-linear2_128G_08H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-linear2_128G_08H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-linear2_128G_08H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-linear2_128G_08H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-linear2_128G_08H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-linear2_128G_08H/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-linear2_128G_08H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-linear2_128G_08H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-linear2_128G_08H/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-linear2_128G_08H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-linear2_128G_08H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-linear2_128G_08H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-linear2_128G_08H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-linear2_128G_08H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-linear2_128G_08H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-linear2_128G_08H/GPT3-davinci_bysent.rds"
)

######################################## PREV for mean
##### Smooth wprev only mean effect (no prev for variance effect) ######
rds_files_wprevm <- list(
  "boyce_ngram"         = "COMPUTE-wprevmk6_2_480G_96H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-wprevmk6_2_480G_96H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-wprevmk6_2_480G_96H/TXL.rds",
  "GPT2"                = "COMPUTE-wprevmk6_1_480G_168H/GPT2.rds", ## required >144H (147)
  "GPT2-large"          = "COMPUTE-wprevmk6_2_480G_96H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-wprevmk6_1_480G_168H/GPT2-xl.rds", ## required >144H (140 (?))
  "GPT-J"               = "COMPUTE-wprevmk6_2_480G_96H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-wprevmk6_2_480G_96H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-wprevmk6_2_480G_96H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-wprevmk6_2_480G_96H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-wprevmk6_2_480G_96H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-wprevmk6_2_480G_96H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-wprevmk6_2_480G_96H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-wprevmk6_2_480G_96H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-wprevmk6_2_480G_96H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-wprevmk6_1_480G_144H/GPT-J_80.rds", ## required >96H (~123)
  "GPT-Neo_80"          = "COMPUTE-wprevmk6_2_480G_96H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-wprevmk6_2_480G_96H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-wprevmk6_1_480G_144H/GPT2_bysent.rds", ## required >96H (~96 (?))
  "GPT2-large_bysent"   = "COMPUTE-wprevmk6_2_480G_96H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-wprevmk6_2_480G_96H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-wprevmk6_2_480G_96H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-wprevmk6_2_480G_96H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-wprevmk6_2_480G_96H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-wprevmk6_2_480G_96H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-wprevmk6_2_480G_96H/GPT3-davinci_bysent.rds"
)

##### Linear control WPREV constant variance, with family=gaussian() ####
rds_files_lin0var_wprevm <- list(
  "boyce_ngram"         = "COMPUTE-lin0var_wprev2_032G_02H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-lin0var_wprev2_032G_02H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-lin0var_wprev2_032G_02H/TXL.rds",
  "GPT2"                = "COMPUTE-lin0var_wprev2_032G_02H/GPT2.rds",
  "GPT2-large"          = "COMPUTE-lin0var_wprev2_032G_02H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-lin0var_wprev2_032G_02H/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-lin0var_wprev2_032G_02H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-lin0var_wprev2_032G_02H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-lin0var_wprev2_032G_02H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-lin0var_wprev2_032G_02H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-lin0var_wprev2_032G_02H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-lin0var_wprev2_032G_02H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-lin0var_wprev2_032G_02H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-lin0var_wprev2_032G_02H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-lin0var_wprev2_032G_02H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-lin0var_wprev2_032G_02H/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-lin0var_wprev2_032G_02H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-lin0var_wprev2_032G_02H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-lin0var_wprev2_032G_02H/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-lin0var_wprev2_032G_02H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-lin0var_wprev2_032G_02H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-lin0var_wprev2_032G_02H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-lin0var_wprev2_032G_02H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-lin0var_wprev2_032G_02H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-lin0var_wprev2_032G_02H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-lin0var_wprev2_032G_02H/GPT3-davinci_bysent.rds"
)

##### Linear control WPREV linear variance (no prev for variance), with family=gaussian() ####
rds_files_linear_wprevm <- list(
  "boyce_ngram"         = "COMPUTE-linear_wprevm2_230G_24H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-linear_wprevm2_230G_24H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-linear_wprevm2_230G_24H/TXL.rds",
  "GPT2"                = "COMPUTE-linear_wprevm2_230G_24H/GPT2.rds",
  "GPT2-large"          = "COMPUTE-linear_wprevm2_230G_24H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-linear_wprevm2_230G_24H/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-linear_wprevm2_230G_24H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-linear_wprevm2_230G_24H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-linear_wprevm2_230G_24H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-linear_wprevm2_230G_24H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-linear_wprevm2_230G_24H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-linear_wprevm2_230G_24H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-linear_wprevm2_230G_24H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-linear_wprevm2_230G_24H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-linear_wprevm2_230G_24H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-linear_wprevm2_230G_24H/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-linear_wprevm2_230G_24H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-linear_wprevm2_230G_24H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-linear_wprevm2_230G_24H/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-linear_wprevm2_230G_24H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-linear_wprevm2_230G_24H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-linear_wprevm2_230G_24H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-linear_wprevm2_230G_24H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-linear_wprevm2_230G_24H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-linear_wprevm2_230G_24H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-linear_wprevm2_230G_24H/GPT3-davinci_bysent.rds"
)


##### Linear control WPREV linear variance, with family=gaussian()  FORMULA WITH ALL TERMS IN VARIANCE (DECIDED NOT TO USE THIS ONE!!!!) ####
rds_files_linear_wprev_allterms <- list(
  "boyce_ngram"         = "COMPUTE-linear_wprev2_256G_24H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-linear_wprev2_256G_24H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-linear_wprev2_256G_24H/TXL.rds",
  "GPT2"                = "COMPUTE-linear_wprev2_256G_24H/GPT2.rds"#,
  # "GPT2-large"          = "COMPUTE-linear_wprev2_256G_24H/GPT2-large.rds",
  # "GPT2-xl"             = "COMPUTE-linear_wprev2_256G_24H/GPT2-xl.rds",
  # "GPT-J"               = "COMPUTE-linear_wprev2_256G_24H/GPT-J.rds",
  # "GPT-Neo"             = "COMPUTE-linear_wprev2_256G_24H/GPT-Neo.rds",
  # "GPT3-ada"            = "COMPUTE-linear_wprev2_256G_24H/GPT3-ada.rds",
  # "GPT3-curie"          = "COMPUTE-linear_wprev2_256G_24H/GPT3-curie.rds",
  # "GPT3-davinci"        = "COMPUTE-linear_wprev2_256G_24H/GPT3-davinci.rds",
  # "TXL_80"              = "COMPUTE-linear_wprev2_256G_24H/TXL_80.rds",
  # "GPT2_80"             = "COMPUTE-linear_wprev2_256G_24H/GPT2_80.rds",
  # "GPT2-large_80"       = "COMPUTE-linear_wprev2_256G_24H/GPT2-large_80.rds",
  # "GPT2-xl_80"          = "COMPUTE-linear_wprev2_256G_24H/GPT2-xl_80.rds",
  # "GPT-J_80"            = "COMPUTE-linear_wprev2_256G_24H/GPT-J_80.rds",
  # "GPT-Neo_80"          = "COMPUTE-linear_wprev2_256G_24H/GPT-Neo_80.rds",
  # "TXL_bysent"          = "COMPUTE-linear_wprev2_256G_24H/TXL_bysent.rds",
  # "GPT2_bysent"         = "COMPUTE-linear_wprev2_256G_24H/GPT2_bysent.rds",
  # "GPT2-large_bysent"   = "COMPUTE-linear_wprev2_256G_24H/GPT2-large_bysent.rds",
  # "GPT2-xl_bysent"      = "COMPUTE-linear_wprev2_256G_24H/GPT2-xl_bysent.rds",
  # "GPT-J_bysent"        = "COMPUTE-linear_wprev2_256G_24H/GPT-J_bysent.rds",
  # "GPT-Neo_bysent"      = "COMPUTE-linear_wprev2_256G_24H/GPT-Neo_bysent.rds",
  # "GPT3-ada_bysent"     = "COMPUTE-linear_wprev2_256G_24H/GPT3-ada_bysent.rds",
  # "GPT3-curie_bysent"   = "COMPUTE-linear_wprev2_256G_24H/GPT3-curie_bysent.rds",
  # "GPT3-davinci_bysent" = "COMPUTE-linear_wprev2_256G_24H/GPT3-davinci_bysent.rds"
)

######################################### Without high-surprisal items

##### Smooth wprev only mean effect (no prev for variance effect) ######
rds_files_wprevm_datasubsetless12 <- list(
  # "boyce_ngram"         = "",
  # "boyce_grnn"          = "",
  # "TXL"                 = "",
  # "GPT2"                = "",
  # "GPT2-large"          = "",
  # "GPT2-xl"             = "",
  # "GPT-J"               = "",
  # "GPT-Neo"             = "",
  # "GPT3-ada"            = "",
  # "GPT3-curie"          = "",
  "GPT3-davinci"        = "COMPUTE-datasubsetless12_wprevmk6_1_480G_96H/GPT3-davinci.rds"#,
  # "TXL_80"              = "",
  # "GPT2_80"             = "",
  # "GPT2-large_80"       = "",
  # "GPT2-xl_80"          = "",
  # "GPT-J_80"            = "",
  # "GPT-Neo_80"          = "",
  # "TXL_bysent"          = "",
  # "GPT2_bysent"         = "",
  # "GPT2-large_bysent"   = "",
  # "GPT2-xl_bysent"      = "",
  # "GPT-J_bysent"        = "",
  # "GPT-Neo_bysent"      = "",
  # "GPT3-ada_bysent"     = "",
  # "GPT3-curie_bysent"   = "",
  # "GPT3-davinci_bysent" = ""
)

##### Smooth wprev only mean effect (no prev for variance effect) ######
rds_files_wprevm_datasubsetless10 <- list(
  # "boyce_ngram"         = "",
  # "boyce_grnn"          = "",
  # "TXL"                 = "",
  # "GPT2"                = "",
  # "GPT2-large"          = "",
  # "GPT2-xl"             = "",
  # "GPT-J"               = "",
  # "GPT-Neo"             = "",
  # "GPT3-ada"            = "",
  # "GPT3-curie"          = "",
  "GPT3-davinci"        = "COMPUTE-datasubsetless10_wprevmk6_1_480G_96H/GPT3-davinci.rds"#,
  # "TXL_80"              = "",
  # "GPT2_80"             = "",
  # "GPT2-large_80"       = "",
  # "GPT2-xl_80"          = "",
  # "GPT-J_80"            = "",
  # "GPT-Neo_80"          = "",
  # "TXL_bysent"          = "",
  # "GPT2_bysent"         = "",
  # "GPT2-large_bysent"   = "",
  # "GPT2-xl_bysent"      = "",
  # "GPT-J_bysent"        = "",
  # "GPT-Neo_bysent"      = "",
  # "GPT3-ada_bysent"     = "",
  # "GPT3-curie_bysent"   = "",
  # "GPT3-davinci_bysent" = ""
)

##### Smooth wprev only mean effect (no prev for variance effect) ######
rds_files_wprevm_datasubsetless6 <- list(
  # "boyce_ngram"         = "",
  # "boyce_grnn"          = "",
  # "TXL"                 = "",
  # "GPT2"                = "",
  # "GPT2-large"          = "",
  # "GPT2-xl"             = "",
  # "GPT-J"               = "",
  # "GPT-Neo"             = "",
  # "GPT3-ada"            = "",
  # "GPT3-curie"          = "",
  "GPT3-davinci"        = "COMPUTE-datasubsetless6_wprevmk6_1_480G_96H/GPT3-davinci.rds"#,
  # "TXL_80"              = "",
  # "GPT2_80"             = "",
  # "GPT2-large_80"       = "",
  # "GPT2-xl_80"          = "",
  # "GPT-J_80"            = "",
  # "GPT-Neo_80"          = "",
  # "TXL_bysent"          = "",
  # "GPT2_bysent"         = "",
  # "GPT2-large_bysent"   = "",
  # "GPT2-xl_bysent"      = "",
  # "GPT-J_bysent"        = "",
  # "GPT-Neo_bysent"      = "",
  # "GPT3-ada_bysent"     = "",
  # "GPT3-curie_bysent"   = "",
  # "GPT3-davinci_bysent" = ""
)

# Version with constant variance (gaussian bam) with 1 previous word predictors

rds_files_var0_gauss_wprev <- list(
  "boyce_ngram"         = "COMPUTE-var0_gauss_wprev_1_40G_6H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-var0_gauss_wprev_1_40G_6H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-var0_gauss_wprev_1_40G_6H/TXL.rds",
  "GPT2"                = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2.rds",
  "GPT2-large"          = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-var0_gauss_wprev_1_40G_6H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-var0_gauss_wprev_1_40G_6H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-var0_gauss_wprev_1_40G_6H/GPT3-davinci_bysent.rds"
)


# Version with constant variance (gaussian bam) 
# but with 3 previous words for spillover.

rds_files_var0_gauss_w3prev <- list(
  "boyce_ngram"         = "COMPUTE-var0_gauss_w3prev_1_40G_6H/boyce_ngram.rds",
  "boyce_grnn"          = "COMPUTE-var0_gauss_w3prev_1_40G_6H/boyce_grnn.rds",
  "TXL"                 = "COMPUTE-var0_gauss_w3prev_1_40G_6H/TXL.rds",
  "GPT2"                = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2.rds",
  "GPT2-large"          = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2-large.rds",
  "GPT2-xl"             = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2-xl.rds",
  "GPT-J"               = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT-J.rds",
  "GPT-Neo"             = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT-Neo.rds",
  "GPT3-ada"            = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT3-ada.rds",
  "GPT3-curie"          = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT3-curie.rds",
  "GPT3-davinci"        = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT3-davinci.rds",
  "TXL_80"              = "COMPUTE-var0_gauss_w3prev_1_40G_6H/TXL_80.rds",
  "GPT2_80"             = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2_80.rds",
  "GPT2-large_80"       = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2-large_80.rds",
  "GPT2-xl_80"          = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2-xl_80.rds",
  "GPT-J_80"            = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT-J_80.rds",
  "GPT-Neo_80"          = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT-Neo_80.rds",
  "TXL_bysent"          = "COMPUTE-var0_gauss_w3prev_1_40G_6H/TXL_bysent.rds",
  "GPT2_bysent"         = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2_bysent.rds",
  "GPT2-large_bysent"   = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2-large_bysent.rds",
  "GPT2-xl_bysent"      = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT2-xl_bysent.rds",
  "GPT-J_bysent"        = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT-J_bysent.rds",
  "GPT-Neo_bysent"      = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT-Neo_bysent.rds",
  "GPT3-ada_bysent"     = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT3-ada_bysent.rds",
  "GPT3-curie_bysent"   = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT3-curie_bysent.rds",
  "GPT3-davinci_bysent" = "COMPUTE-var0_gauss_w3prev_1_40G_6H/GPT3-davinci_bysent.rds"
)