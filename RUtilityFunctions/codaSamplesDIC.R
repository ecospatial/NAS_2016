library(rjags)


coda.samples.dic <- function (model, variable.names = NULL, n.iter, thin = 1, ...)
{
  load.module('dic') # necessary for pD and deviance monitor
  
  start <- model$iter() + thin
  varnames=c(variable.names, c('deviance', 'pD'))
  out <- jags.samples(model, varnames, n.iter, thin,
                      type = "trace", ...)
  deviance <- out$deviance
  pD <- out$pD
  out$deviance <- NULL
  out$pD <- NULL
  ans <- vector("list", model$nchain())
  for (ch in 1:model$nchain()) {
    ans.ch <- vector("list", length(out))
    vnames.ch <- NULL
    for (i in seq(along = out)) {
      varname <- names(out)[[i]]
      d <- dim(out[[i]])
      if (length(d) < 3) {
        stop("Invalid dimensions for sampled output")
      }
      vardim <- d[1:(length(d) - 2)]
      nvar <- prod(vardim)
      niter <- d[length(d) - 1]
      nchain <- d[length(d)]
      values <- as.vector(out[[i]])
      var.i <- matrix(NA, nrow = niter, ncol = nvar)
      for (j in 1:nvar) {
        var.i[, j] <- values[j + (0:(niter - 1)) * nvar +
                               (ch - 1) * niter * nvar]
      }
      vnames.ch <- c(vnames.ch, rjags:::coda.names(varname, vardim))
      ans.ch[[i]] <- var.i
    }
    ans.ch <- do.call("cbind", ans.ch)
    colnames(ans.ch) <- vnames.ch
    ans[[ch]] <- mcmc(ans.ch, start = start, thin = thin)
  }
  
  # if (isTRUE(na.rm)) {
  #   all.missing <- sapply(ans, function(x) {
  #     apply(is.na(x), 2, any)
  #   })
  #   drop.vars <- if (is.matrix(all.missing)) {
  #     apply(all.missing, 1, any)
  #   }
  #   else {
  #     any(all.missing)
  #   }
  #   ans <- lapply(ans, function(x) return(x[, !drop.vars, 
  #                                           drop = FALSE]))
  # }
  
  dic <- list(deviance = mean(as.vector(deviance)), penalty = mean(as.vector(pD)), type = 'pD')
  class(dic) <- "dic"
  return(list(samples=mcmc.list(ans), dic=dic))
}
