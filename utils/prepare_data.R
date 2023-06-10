# functions to prepare data for fitting GAMs
# input `data` should be a dataset either
# - `surps_lms.RTs_new`
# - `surps_lms.RTs.perword_new`
# which are output by `data_processing_surps.RTs.Rmd`

require(dplyr)
require(magrittr)
require(tidyr)

add_lags <- function(data, how_many_prevs) {
    #' Add lags to dataframe (in format outputted by all_data.R)
    #' This means sorting the data by a different column
    #' depending on the tokenization.
    data %>%
        mutate(across(
            c(contains("surp_"), freq, freq_c, length, length_c),
            .fns = list("prev" = function(x) { lag(x, 1L) },
                        "prev2" = function(x) { lag(x, 2L) },
                        "prev3" = function(x) { lag(x, 3L) }),
            .names = "{.fn}_{.col}"))
}

sort_data_for_tokenization_type <- function(data, modelname){
    # Sort data
    #if boyce_grnn or ngram
    if (modelname %>% stringr::str_starts("boy")) {
        data <- data %>%
            arrange_at(c("story_num", "word_num_in_story"))
    }
    #if TXL
    if (modelname == "TXL") {
        data <- data %>%
            tidyr::separate("txl_word_id",
                    into=c(
                        "txl_story_num",
                        "txl_sent_num",
                        "txl_word_num_in_sent"),
                    convert = T, remove = F) %>%
            arrange_at(c("story_num", "txl_sent_num", "txl_word_num_in_sent"))
    } else { # default is GPT tokenization ordering
        data <- data %>%
            arrange_at(c("story_num", "story_offset"))
    }
    return(data)
}

prepare_data.perword <- function(
        data, modelname, how_many_prevs = 1, leave_all_columns = FALSE) {

    # Sort data so lag is appropriate
    data <- data %>%
        sort_data_for_tokenization_type(modelname)

    data <- data %>% mutate(across(
        c(contains("surp_"), freq, length),
        .fns = list("c" = function(x) { x - mean(x, na.rm = T) }),
        .names = "{.col}_{.fn}")) %>%
        mutate(Word_ID = forcats::as_factor(stringr::str_c(
            story_num, word_num_in_story, sep = "_"))) %>%
        relocate(Word_ID, word)

    select_or_mutate <- if (leave_all_columns) mutate else select
    surp <- paste0("surp_", modelname)
    prev_surp <- paste0("prev_surp_", modelname)
    prev2_surp <- paste0("prev2_surp_", modelname)
    prev3_surp <- paste0("prev3_surp_", modelname)
    data_prepared <- data %>%
        group_by(story_num) %>% # to avoid lagging across story boundaries
        add_lags(how_many_prevs) %>% # Add lags
        ungroup() %>%
        select_or_mutate(response = meanItemRT, Word_ID, word,
               # This set should be a unique id:
               story_num, word_num_in_story, txl_word_id, gpt2_word_id,
               surprisal = !!rlang::sym(surp),
               prev_surprisal = !!rlang::sym(prev_surp),
               freq = freq_c, length = length_c,
               prev_freq = prev_freq_c, prev_length = prev_length_c,
               prev2_surprisal = !!rlang::sym(prev2_surp),
               prev2_freq = prev2_freq_c, prev2_length = prev2_length_c,
               prev3_surprisal = !!rlang::sym(prev3_surp),
               prev3_freq = prev3_freq_c, prev3_length = prev3_length_c) %>%
        mutate(model = modelname)
    #     %>%
    #     tidyr::drop_na(surprisal, freq, length,
    #             prev_surprisal, prev_freq, prev_length)
    # if (how_many_prevs > 1) {
    #     data_prepared <- data_prepared %>%
    #         tidyr::drop_na(prev2_surprisal, prev2_freq, prev2_length)  }
    # if (how_many_prevs == 3) {
    #     data_prepared <- data_prepared %>%
    #         tidyr::drop_na(prev3_surprisal, prev3_freq, prev3_length)  }
    data_prepared
}

prepare_data_with_perword <- function(
        data, data.perword, modelname,
        how_many_prevs = 1, drop_nas = T,
        leave_all_columns = FALSE) {

    data_prepared.perword <- data.perword %>%
        prepare_data.perword(
          modelname,
          how_many_prevs = how_many_prevs,
          leave_all_columns = leave_all_columns) %>%
        mutate(meanItemRT = response)

    data_prepared <- data %>% 
        select(
            -starts_with("surp_"), 
            orig_freq = freq, orig_length = length) %>%
        left_join(data_prepared.perword)
    if (leave_all_columns==FALSE) {
      data_prepared <- data_prepared %>%
        select(
          RT, WorkerId,
          Word_ID, word,
          story_num, word_num_in_story,
          txl_word_id, gpt2_word_id,
          contains("surprisal"), contains("prev"),
          contains("length"), contains("freq"))
    }
    if (drop_nas) {
        # how much to drop depends on the number of prevs being used
        data_prepared <- data_prepared %>%
            tidyr::drop_na(surprisal, freq, length,
                    prev_surprisal, prev_freq, prev_length)
        if (how_many_prevs > 1) {
            data_prepared <- data_prepared %>%
                tidyr::drop_na(prev2_surprisal, prev2_freq, prev2_length)
        }
        if (how_many_prevs == 3) {
            data_prepared <- data_prepared %>%
                tidyr::drop_na(prev3_surprisal, prev3_freq, prev3_length)
        }
    }
    data_prepared %>%
        mutate(response = RT, subject = factor(WorkerId))
}