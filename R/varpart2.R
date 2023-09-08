#' Partition the Variation of Community Matrix by 2 or more Explanatory Models
#'
#' @param Y Data frame or matrix containing the response data table or dissimilarity structure inheriting from dist. In community ecology, that table is often a site-by-species table or a dissimilarity object.
#' @param X An explanatory model defined by a one-sided formula (~ X).
#' @param ...  One or more other explanatory models defined by a one-sided formula.
#' @param data The data frame with the variables used in the formulae in X.
#' @param chisquare Partition Chi-square or the inertia of Correspondence Analysis (cca).
#' @param transfo Transformation for Y (community data) using decostand. All alternatives in decostand can be used, and those preserving Euclidean metric include "hellinger", "chi.square", "total", "norm". Ignored if Y are dissimilarities.
#' @param scale Should the columns of Y be standardized to unit variance. Ignored if Y are dissimilarities.
#' @param add Add a constant to the non-diagonal values to euclidify dissimilarities (see wcmdscale for details). Choice "lingoes" (or TRUE) use the recommended method of Legendre & Anderson (1999: “method 1”) and "cailliez" uses their “method 2”. The argument has an effect only when Y are dissimilarities.
#' @param sqrt.dist Take square root of dissimilarities. This often euclidifies dissimilarities. NB., the argument name cannot be abbreviated. The argument has an effect only when Y are dissimilarities.
#' @param permutations If chisquare = TRUE, the adjusted R^2 is estimated by permutations, and this paramater can be a list of control values for the permutations as returned by the function how, or the number of permutations required, or a permutation matrix where each row gives the permuted indices. (Default is 1000L)
#' @note
#' Based on the [vegan::varpart()] function and modified to allow more explanatory models.
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @return A list
#'
#' @examples \dontrun{
#' mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
#' mod
#' summary(mod)
#' }
#' @export
varpart2 <- function(Y,
                     X,
                     ...,
                     data = parent.frame(),
                     chisquare = FALSE,
                     transfo = NULL,
                     scale = FALSE,
                     add = FALSE,
                     sqrt.dist = FALSE,
                     permutations = 1000L){
  lifecycle::signal_stage("experimental", "varpart2()")
  pkg_found <- vapply(c("data.table", "vegan", "stringi"), \(pkg) {
    as.character(tryCatch(packageVersion(pkg), error = \(e) ""))
  }, "")
  if (any(!nzchar(pkg_found)))
    stop("This function requires the ", salixUtils::toStr(pkg_found[!nzchar(pkg_found)]), if (sum(!nzchar(pkg_found)) > 1) " packages." else " package.")
  require(data.table, quietly = TRUE)
  #-- arguments' checks
  if (missing(X)) stop("argument 'X' is missing with no default")
  if (missing(Y)) stop("argument 'Y' is missing with no default")
  X <- list(X, ...)
  X <- X[lengths(X) > 0]
  if (length(X) < 2) stop("Please provide more than one explanatory tables.")
  if (length(X) > 10) warning("Wow, that's a lot of matrices, good luck.")
  if (length(X) > 26) warning("This isn't supported yet as this function uses alphabet letters.")
  Y.class <- .assert(Y, c("dist", "matrix", "data.frame"), name = "Y", which = TRUE)
  for (i in seq_along(X)) {
    .assert(X[[i]],
            what = c("matrix", "data.frame", "formula"),
            name = paste0("X[[", i, "]]"))
  }
  if (!missing(chisquare))
    .assert(chisquare, what = "logical", name = "chisquare", check.length = 1)
  if (!is.null(transfo))
    .assert(transfo, what = "character", name = "transfo", check.length = 1)
  if (!missing(scale))
    .assert(scale, what = "logical", name = "scale")
  if (!missing(add)) {
    .assert(add, what = c("logical", "character"), name = "add", check.length = 1)
    if (isTRUE(add)) {
      add <- "lingoes"
    } else if (is.character(add)) {
      add <- match.arg(add, c("lingoes", "cailliez"))
    }
  }
  if (!missing(sqrt.dist))
    .assert(sqrt.dist, what = "logical", name = "sqrt.dist")
  if (!missing(permutations))
    .assert(permutations, what = c("numeric", "integer"), name = "permutations")
  #--
  if (Y.class == "data.frame") {
    Y <- as.matrix(Y)
    Y.class <- "matrix"
  }
  if (Y.class == "matrix" && isSymmetric(unname(Y))) {
    Y <- as.dist(Y)
    Y.class <- "dist"
  }
  #--
  out <- .prep_y(list(Y = Y,
                      Y.class = Y.class,
                      permat = permutations,
                      chisquare = chisquare,
                      scale = scale,
                      transfo = transfo,
                      sqrt.dist = sqrt.dist
  ))
  out$XX <- .make_sets(X = X, data = data)
  out <- .get_fract(out)
  out$part$indfract <- .get_indfract(out$part$fract)
  out$part$nSets = length(X)^2 - 1
  out$call <- match.call()[-(1:2)]
  out$tables <- vapply(out$call, deparse, width.cutoff = 500, "")
  out$data <- list(XX = out$XX, Y = out$Y)
  out[c("part", "inert", "RDA", "call", "tables", "data")]
}

#=== steps functions ===
#--- .make_sets (X1,X2,... matrices)
.make_sets <- function(X, data) {
  Sets <-  lapply(X, \(x){
    x <- switch(
      class(x)[1],
      data.frame = {
        model.matrix( ~ ., x)[, -1, drop = FALSE]
      },
      factor = {
        model.matrix( ~ ., as.data.frame(x))[, -1, drop = FALSE]
      },
      formula = {
        mf <- model.frame(x,
                          data,
                          na.action = na.fail,
                          drop.unused.levels = TRUE)
        mf <- model.matrix(attr(mf, "terms"), mf)
        mf[, colnames(mf) != "(Intercept)", drop = FALSE]
      },
      {
        as.matrix(x)
      }
    )
    scale(x, center = TRUE, scale = TRUE)
  })
  combs <- .combn2(seq_along(Sets))
  Sets <- lapply(combs, \(rng) do.call(cbind, Sets[rng]))
  names(Sets) <- lapply(combs, \(rng) paste0("X", rng, collapse = "+"))
  Sets
}
#--- .prep_y
.prep_y <- function(x) {
  has.scale <- !is.null(x$scale) && !isFALSE(x$scale)
  has.transfo <- !is.null(x$transfo) && !isFALSE(x$transfo)
  if (x$Y.class == "dist") {
    if (has.transfo || has.scale)
      message("arguments 'x$transfo' and 'x$scale' are ignored with distances")
    inert <- attr(x$Y, "method")
    if (is.null(inert)) {
      inert <- "Unknown user-supplied distance"
    } else {
      inert <- gsub("^\\s*([[:lower:]])", "\\U\\1", inert, perl = TRUE)
      inert <- paste(inert, if (x$sqrt.dist) "squared distance" else "distance")
    }
    if (x$sqrt.dist) x$Y <- sqrt(x$Y)
    # else inert <- paste("squared", inert) # ??? not the oposite?
    if (is.character(x$add)) {
      ac <- switch(x$add, lingoes = vegan:::x$addLingoes(x$Y), cailliez = vegan:::x$addCailliez(x$Y))
      x$Y <- switch(x$add, lingoes = sqrt(x$Y^2 + 2 * ac), cailliez =  x$Y + ac)
      if (ac > sqrt(.Machine$double.eps))
        inert <- paste(gsub("^\\s*([[:lower:]])", "\\U\\1", x$add, perl = TRUE), "adjusted", inert)
    }
    RDA <- "dbRDA"
  } else if (x$chisquare) {
    inert <- "Chi-square"
    RDA <- "CCA"
    x$permat <- vegan:::getPermuteMatrix(x$permat, nrow(x$Y))
  } else {
    inert <- "variance"
    RDA <- "RDA"
    if (has.transfo) {
      x$Y <- vegan::decostand(x$Y, x$transfo)
      x$transfo <- attr(x$Y, "decostand")
    }
    if (has.transfo && (is.null(dim(x$Y)) || ncol(x$Y) == 1))
      warning("transformations are probably meaningless with a single variable")
    if (has.scale && has.transfo)
      warning("Y should not be both transformed and scaled (standardized)")
    x$Y <- scale(x$Y, center = TRUE, scale = x$scale)
  }
  if (RDA != "RDA" && (has.scale || has.transfo)) {
    message("arguments 'scale' and 'transfo' ignored: valid only in RDA")
    x$scale <- FALSE
    x$transfo <- NULL
  }
  x$RDA <- RDA
  x$inert <- inert
  x$Y.class <- NULL
  x
}
#--- .get_fract (fraction of each circle)
.get_fract <- function(x){
  collinwarn <- function(case, mm, m){
    warning(gettextf("collinearity detected in %s: mm = %d, m = %d",
                     case, mm, m), call. = FALSE)
  }
  if (inherits(x$Y, "dist")) {
    x$Y <- vegan:::GowerDblcen(as.matrix(x$Y^2), na.rm = FALSE)
    x$Y <- -x$Y/2
    SS.Y <- sum(diag(x$Y))
    simpleRDA2 <- vegan:::simpleDBRDA
  } else {
    x$Y <- as.matrix(x$Y)
    if (x$chisquare) {
      SS.Y <- sum(vegan:::initCA(x$Y)^2)
      simpleRDA2 <- vegan:::simpleCCA
    }
    else {
      simpleRDA2 <- vegan:::simpleRDA2
      x$Y <- scale(x$Y, center = TRUE, scale = FALSE)
      SS.Y <- sum(x$Y * x$Y)
    }
  }
  n <- nrow(x$Y)
  nn <- vapply(x$XX, nrow, 0)
  mm <- vapply(x$XX, ncol, 0)
  if (any(nn != n))
    stop("Y and ", paste0("X", which(nn != n), collapse = ", ")," do not have the same number of rows")
  x$XX <- lapply(x$XX, scale, center = TRUE, scale = FALSE) #isn't this redundant?
  x$part <- list(
    fract = lapply(x$XX, \(xx){
      o <- simpleRDA2(x$Y, xx, SS.Y, x$permat)
      list(
        Df = o$m,
        Rsquare = o$Rsquare,
        RsquareAdj = o$RsquareAdj
      )
    }) |>
      data.table::rbindlist(idcol = TRUE),
    SS.Y = SS.Y,
    n = n,
    ordination = tolower(x$RDA)
  )
  if (any(x$part$fract$Df != mm)) {
    for (i in which(x$part$fract$Df != mm)) {
      collinwarn(x$part$fract$.id[i], mm = mm[i], m = x$part$fract$Df[i])
    }
  }
  x
}
#--- .get_indfract (fraction of each section of each circle)
.get_indfract <- function(fract) {
  fract <- as.list(`names<-`(fract[["RsquareAdj"]], fract[[".id"]]))
  n <- max(stringi::stri_count_fixed(names(fract), "+")) + 1
  bin_tbl <- .make_bin_table(n)
  allVars <- colnames(bin_tbl)
  rn_nchar <- nchar(rownames(bin_tbl))
  indfract <- apply(bin_tbl[rn_nchar == 1,,drop = FALSE], 1,
                    \(x) {
                      var <- allVars[x]
                      var2rm <- allVars[!x]
                      ls <- .paste(sort(c(var, var2rm)))
                      rs <- .paste(var2rm)
                      data.table::data.table(
                        formula = paste(ls, "-", rs),
                        RsquareAdj = eval(
                          str2lang(.abc2X(formula, n = n)), envir = fract
                        )
                      )
                    }) |>
    data.table::rbindlist(idcol = "id")
  for (i in seq_len(n)[-1L]) {
    out <- apply(bin_tbl[rn_nchar == i,,drop = FALSE], 1, \(x) {
        var <- allVars[x]
        var2rm <- allVars[!x]
        if (length(var2rm)) {
          area2rm <- grepl(paste(tolower(var), collapse = "|"), indfract$id) & !grepl(paste(tolower(allVars[!x]), collapse = "|"), indfract$id)
          area2rm <- indfract[area2rm,]
          formula <- paste0(.paste(c(var, var2rm)), " - ", .paste(var2rm))
          data.table::data.table(
            formula = paste0(formula, " - (",
                             paste(area2rm$id, collapse = " + "),
                             ")"),
            RsquareAdj = eval(
              str2lang(.abc2X(formula, n = n)), envir = fract
            ) - sum(area2rm$RsquareAdj)
          )
        } else {
          tmp <- eval(str2lang(.abc2X(.paste(var), n = n)), envir = fract)
          data.table::data.table(formula = "",
                                 RsquareAdj = tmp - sum(indfract$RsquareAdj))
        }
      }, simplify = FALSE)
    indfract <- rbind(indfract, data.table::rbindlist(out, idcol = "id"))
  }
  out <- data.table::data.table(id = "Residual", formula = "", RsquareAdj = 1 - sum(indfract$RsquareAdj))
  rbind(indfract, out)
}
#=== utils functions ===
#--- .combn2
.combn2 <- \(x){
  out <- lapply(x, \(i) combn(x, i, simplify = FALSE)) |>
    unlist(recursive = FALSE, use.names = FALSE)
  out[order(lengths(out))]
}
#--- .abc2X
.abc2X <- function(x, n){
  for (i in seq_len(n)){
    x <- stringi::stri_replace_all_regex(x, LETTERS[i], paste0("X", i))
  }
  stringi::stri_replace_all_regex(x, "([0-9])(X)", "$1+$2")
}
#--- .paste
.paste <- \(x, y = NULL, z = NULL){
  if (!is.null(z) && !is.null(y)) {
    z <- .charsplit(z, simplify = FALSE)
    if (length(z) > 1) z <- do.call(intersect, args = z)
    else z <- z[[1]]
    z <- .outersect(z, y)
    x <- x[!x %in% z]
  }
  paste0("`", paste(sort(c(x, y)), collapse = ""), "`")
}
#--- .make_bin_table
.make_bin_table <- function(n){
  comb <- .combn2(seq_len(n))
  area <- vapply(comb, \(x) letters[x] |> paste(collapse = ""), "")
  bin_tbl <- t(vapply(comb, \(x) {
    out <- logical(length = n)
    out[x] <- TRUE
    out
  }, FUN.VALUE = logical(n)))
  dimnames(bin_tbl) <- list(area, LETTERS[1:n])
  bin_tbl
}
#--- .charsplit
.charsplit <- function(x, simplify = TRUE){
  c(stringi::stri_split_regex(x, "(?<=.)(?=.)", simplify = simplify))
}
#--- .outersect
.outersect <- \(x, y){
  c(setdiff(x, y), setdiff(y, x))
}
#--- .assert
.assert <- function(x, what, name = NULL, check.length = NULL, which = FALSE) {
  if (missing(name)) {
    name <- deparse(substitute(x))
  }
  out <- inherits(x = x, what = what, which = which)
  if (!sum(out)) {
    stop("'", name, "' must be of class ", salixUtils::toStr(x = what, join_word = "or"), call. = FALSE)
  } else {
    if (isTRUE(check.length) && !length(x)) {
      stop("'", name, "' can't be empty", call. = FALSE)
    } else if (inherits(check.length, c("numeric", "integer")) &&
               check.length > 0 && length(x) != check.length) {
      stop("'", name, "' must be length ", check.length, call. = FALSE)
    }
    if (isTRUE(which)) {
      what[as.logical(out)]
    } else {
      if ("logical" %in% what && length(out) == 1L) {
        if (is.na(out)) out <- FALSE
      }
      out
    }
  }
}
