#' Make a data.frame of diversity measures
#' @param df data frame object
#' @return a data frame with the calculated Shannon diversity index, species richness and evenness
makeDiversityDF <- function(df){
  div.df <- data.frame(
    H = vegan::diversity(df, "shannon"),
    s = vegan::specnumber(df)
  )
  div.df$E <- exp(div.df$H) / div.df$s
  return(div.df)
}

#' Make a data.frame with the results of
#' @param data data frame object
#' @param x    vector to use as the x variable in the lme formula
#' @param r    vector to use as the random variable in the lme formula
#' @return a data frame with the p- and F- value for each column of the data
makeANOVAresultsDF <- function(data, x, r){
  res <- data.frame(stat = c("valeur-p", "valeur-F"))
  res <- cbind(res, `names<-`(rep(NA, length(data)), colnames(data)))
  for (c in colnames(data)) {
    y <- unlist(data[c])
    x <- unlist(x)
    r <- unlist(r)
    a <- stats::anova(nlme::lme(y ~ x, random = ~ 1 | r))
    res[c] <- c(
      sprintf("%.4f", a$`p-value`[2]),
      sprintf("%.4f", a$`F-value`[2])
    )
  }
  return(res)
}
