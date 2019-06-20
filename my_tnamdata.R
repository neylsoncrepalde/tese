# My tnamdata
my_tnamdata = function(formula, center.y = FALSE) 
{
  if (class(formula) != "formula") {
    stop("'formula' must be a formula object.")
  }
  lhs <- deparse(formula[[2]])
  lhs <- eval(parse(text = lhs))
  rhs <- paste0(deparse(formula[[3]]), collapse = "")
  rhs <- gsub("\\s+", " ", rhs)
  rhs <- strsplit(rhs, " \\+ ")[[1]]
  time <- numeric()
  node <- character()
  response <- numeric()
  if (class(lhs) == "list") {
    for (i in 1:length(lhs)) {
      if (!is.numeric(lhs[[i]])) {
        stop(paste("The response variable should be numeric or a list of", 
                   "numerics or a data frame with one time point per column."))
      }
      if (is.null(names(lhs[[i]])) || length(names(lhs[[i]])) != 
          length(lhs[[i]])) {
        stop(paste("The outcome variable must have node labels if multiple", 
                   "time points are present."))
      }
      node <- c(node, names(lhs[[i]]))
      time <- c(time, rep(i, length(lhs[[i]])))
      if (center.y == TRUE) {
        lhs[[i]] <- lhs[[i]] - mean(lhs[[i]], na.rm = TRUE)
      }
      response <- c(response, lhs[[i]])
    }
  }
  else if (class(lhs) == "data.frame") {
    for (i in 1:ncol(lhs)) {
      if (!is.numeric(lhs[, i])) {
        stop(paste("The response variable should be numeric or a list of", 
                   "numerics or a data frame with one time point per column."))
      }
      if (is.null(rownames(lhs)) || length(rownames(lhs)) != 
          nrow(lhs)) {
        stop(paste("The outcome variable must have node labels if multiple", 
                   "time points are present."))
      }
      node <- c(node, rownames(lhs))
      time <- c(time, rep(i, nrow(lhs)))
      if (center.y == TRUE) {
        lhs[, i] <- lhs[, i] - mean(lhs[, i], na.rm = TRUE)
      }
      response <- c(response, lhs[, i])
    }
  }
  else if (!is.numeric(lhs)) {
    stop("Data type of the response variable could not be recognized.")
  }
  else {
    response <- lhs
    if (center.y == TRUE) {
      response <- response - mean(response, na.rm = TRUE)
    }
    time <- rep(1, length(lhs))
    node <- as.character(1:length(lhs))
  }
  dat <- data.frame(response = response, time = time, node = node)
  resultlist <- list()
  for (i in 1:length(rhs)) {
    result <- eval(parse(text = rhs[i]))
    resultlist[[i]] <- result
  }
  for (i in 1:length(resultlist)) {
    for (j in 1:length(resultlist)) {
      itime <- length(unique(resultlist[[i]]$time))
      jtime <- length(unique(resultlist[[j]]$time))
      if ((itime > 1 || jtime > 1) && i < j) {
        inters <- length(intersect(resultlist[[i]]$node, 
                                   resultlist[[j]]$node))
        if (inters == 0) {
          stop(paste("Model terms", i, "and", j, "do not have any", 
                     "intersecting node labels. Please attach names, row names, or", 
                     "vertex names to the 'y' or 'networks' argument."))
        }
      }
    }
  }
  for (i in 1:length(resultlist)) {
    lag.i <- attributes(resultlist[[i]])$lag
    if (is.null(lag.i) || length(lag.i) == 0) {
      lag.i <- 0
    }
    resultlist[[i]]$time <- resultlist[[i]]$time + lag.i
  }
  for (i in 1:length(resultlist)) {
    dat <- merge(dat, resultlist[[i]], by = c("time", "node"), 
                 all.x = TRUE, all.y = FALSE)
    colnames(dat)[3] <- "response"
    dat$node <- as.character(dat$node)
    if (ncol(resultlist[[i]]) == 4) {
      dat <- dat[, -ncol(dat)]
    }
  }
  dat <- dat[, c(3, 1, 2, 4:ncol(dat))]
  return(dat)
}
