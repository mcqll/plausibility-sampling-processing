# my_pacf_plot: A version of `itsadug::acf_plot`, but for *partial ACF* (PACF).

my_pacf_plot <- function (x, split_by = NULL, max_lag = NULL, plot = TRUE, fun = mean, 
          cond = NULL, return_all = FALSE, ...) 
{
  if (!is.vector(x)) {
    if (dim(x)[1] == 1) {
      x <- as.vector(x[1, ])
    }
    else if (dim(x)[2] == 1) {
      x <- as.vector(x[, 1])
    }
    else {
      stop(sprintf("%s is not a vector (dim: %d x %d).\n", 
                   deparse(substitute(x)), dim(x)[1], dim(x)[2]))
    }
  }
  xname <- deparse(substitute(x))
  plotci <- FALSE
  if (length(x) > 1) {
    n <- which(is.na(x))
    if (!is.null(split_by)) {
      for (i in 1:length(split_by)) {
        if (length(split_by[[i]]) != length(x)) {
          name_split_i <- ifelse(!is.null(names(split_by)[i]), 
                                 names(split_by)[i], sprintf("split_by[[%d]]", 
                                                             i))
          errormessage <- sprintf("Split factor %s is not of same length as %s: %s has %d elements, %s has %d elements.\n\n                    See help(acf_plot) for examples how to avoid this error.", 
                                  name_split_i, deparse(substitute(x)), deparse(substitute(x)), 
                                  length(x), name_split_i, length(split_by[[i]]))
          stop(errormessage)
        }
      }
    }
    else {
      split_by <- as.factor(rep("Factor", length(x)))
      plotci <- TRUE
    }
    if (!is.null(cond)) {
      if (!is.null(split_by)) {
        el <- 1:length(split_by[[1]])
        for (i in names(cond)) {
          if (i %in% names(split_by)) {
            el <- intersect(el, which(split_by[[i]] %in% 
                                        cond[[i]]))
          }
          else {
            warning(sprintf("Predictor %s not specified in split_by. cond will be ignored.", 
                            i))
          }
        }
        if (length(el) > 0) {
          x <- x[el]
          for (i in names(split_by)) {
            if (is.factor(split_by[[i]])) {
              split_by[[i]] <- droplevels(split_by[[i]][el])
            }
            else {
              split_by[[i]] <- split_by[[i]][el]
            }
          }
        }
        else {
          warning("Specified conditions not found in values of split_by. cond will be ignored.")
        }
      }
      else {
        warning("Split_by is empty, therefore cond will be ignored. Specify time series in cond for selecting specific time series.")
      }
    }
    splitdat <- split(x, f = split_by, drop = T)
    acfn <- lapply(splitdat, FUN = function(x) {
      x.rmna <- x[!is.na(x)]
      return(length(x.rmna))
    })
    splitacf <- lapply(splitdat, FUN = function(x) {
      x.rmna <- x[!is.na(x)]
      return(pacf(x.rmna, plot = F)$acf)
    })
    len <- max_lag
    if (is.null(len)) {
      len <- max(unlist(lapply(splitacf, FUN = length)), 
                 na.rm = T)
    }
    splitacf <- lapply(splitacf, FUN = function(x, max = len) {
      if (length(x) < max) {
        return(c(x, rep(NA, max - length(x))))
      }
      else if (length(x) > max) {
        return(x[1:max])
      }
      else {
        return(x)
      }
    })
    allacf <- as.data.frame(do.call("rbind", splitacf), stringsAsFactors = FALSE)
    names(allacf) <- (1:ncol(allacf))
    avgacf <- apply(allacf, 2, FUN = fun)
    if (plot) {
      plot_default <- list(main = sprintf("PACF of %s", 
                                          xname), xlab = "Lag", ylab = ifelse(plotci == 
                                                                                TRUE, "ACF function (per time series)", "ACF"), 
                           ylim = c(min(min(avgacf, na.rm = TRUE), 0), max(max(avgacf, 
                                                                               na.rm = TRUE), 1)), col = "black", type = "h")
      plot_args <- list(...)
      plot_args_def <- c()
      for (pa in names(plot_default)[!names(plot_default) %in% 
                                     names(plot_args)]) {
        value <- plot_default[[pa]]
        if (length(value) > 1) {
          value <- sprintf("c(%s)", paste(value, collapse = ","))
          plot_args_def <- c(plot_args_def, paste(pa, 
                                                  value, sep = "="))
        }
        else {
          plot_args_def <- c(plot_args_def, paste(pa, 
                                                  ifelse(is.character(value), sprintf("'%s'", 
                                                                                      value), value), sep = "="))
        }
      }
      if (length(plot_args_def) > 0) {
        eval(parse(text = paste("plot(1:len, avgacf, ", 
                                paste(plot_args_def, collapse = ","), ", ...)")))
      }
      else {
        plot(1:len, avgacf, ...)
      }
      if (plotci) {
        ci <- -(1/acfn[[1]]) + 2/sqrt(acfn[[1]])
        abline(h = c(-1, 1) * ci, lty = 2, col = "blue")
      }
      abline(h = 0)
    }
    acf_out <- avgacf
    if (return_all) {
      dfracf <- do.call("rbind", mapply(function(x, y, 
                                                 z) {
        data.frame(acf = x, lag = 1:length(x), 
                   n = rep(y, length(x)), event = rep(z, length(x)), 
                   stringsAsFactors = FALSE)
      }, splitacf, acfn, names(splitacf), SIMPLIFY = FALSE, 
      USE.NAMES = FALSE))
      dfracf$ci <- -(1/dfracf$n) + 2/sqrt(dfracf$n)
      events <- as.data.frame(split_by, stringsAsFactors = FALSE)
      events$event <- apply(events, 1, function(x) {
        gsub(" ", "", paste(x, collapse = "."), fixed = TRUE)
      })
      events <- events[!duplicated(events), ]
      dfracf <- merge(dfracf, events, by = "event", all.x = TRUE, 
                      all.y = FALSE)
      acfn <- do.call("rbind", lapply(names(acfn), function(x) {
        data.frame(n = acfn[[x]], event = x, stringsAsFactors = FALSE)
      }))
      acf_out <- list(acf = avgacf, acftable = allacf, 
                      dataframe = dfracf, n = acfn, series = deparse(substitute(x)), 
                      FUN = fun)
    }
    invisible(acf_out)
  }
  else {
    stop(sprintf("Not sufficient data to plot ACF: %s has %d elements.\n", 
                 deparse(substitute(x)), length(x)))
  }
}