#############################################################################
# Adapted functions for working with lme4 merMod objects
# v 0.1
#
# Jason Grafmiller
# Aug 04, 2015
#############################################################################

colldiag.mer <- function (fit, scale = TRUE, center = FALSE, add.intercept = TRUE) {
	## adapted from perturb::colldiag, method in Belsley, Kuh, and
	## Welsch (1980). Useful for diagnosing multicollinearity
	## look for a high condition index (> 30) with
	## more than one high variance propotion.  see ?colldiag for more
	## tips.
	result = NULL
	if (center) add.intercept = FALSE
	if (is.matrix(fit) || is.data.frame(fit)) {
		X = as.matrix(fit)
		nms = colnames(fit)
	} else if (class(fit)[1] %in% c("glmerMod", "lmerMod")) {
		nms = names(fixef(fit))
		X = getME(fit, "X")
		if (any(grepl("(Intercept)", nms))) {
			add.intercept = FALSE
		}}
	X = X[!is.na(apply(X, 1, all)), ]

	if (add.intercept) {
		X = cbind(1, X)
		colnames(X)[1] = "(Intercept)"
	}
	X = scale(X, scale = scale, center = center)

	svdX = svd(X)
	svdX$d
	condindx = max(svdX$d)/svdX$d
	dim(condindx) = c(length(condindx), 1)

	Phi = svdX$v %*% diag(1/svdX$d)
	Phi = t(Phi^2)
	pi = prop.table(Phi, 2)
	colnames(condindx) = "cond.index"
	if (!is.null(nms)) {
		rownames(condindx) = nms
		colnames(pi) = nms
		rownames(pi) = nms
	} else {
		rownames(condindx) = 1:length(condindx)
		colnames(pi) = 1:ncol(pi)
		rownames(pi) = 1:nrow(pi)}
	result = data.frame(cbind(condindx, pi))
	zapsmall(result)
	}}


collin.fnc.mer <- function(fit){
	# adaption of languageR::collin.fnc() for compatibility with current version
	# of lme4
	require(languageR, quietly = T)
	if (class(fit) == "lmerMod" || class(fit) == "glmerMod"){
		return (collin.fnc(getME(fit, "X")[,-1]))
	} else stop("model not a merMod object")
}


kappa.mer <- function (fit, scale = TRUE, center = FALSE, add.intercept = TRUE, exact = FALSE) {
	# adapted version of base R's kappa()
	X = getME(fit,"X")
	nam = names(fixef(fit))
	## exclude intercepts
	nrp = sum(1 * (nam == "(Intercept)"))
	if (nrp > 0) {
		X = X[, -(1:nrp), drop = FALSE]
		nam = nam[-(1:nrp)]	}
	if (add.intercept) {
		X = cbind(rep(1), scale(X, scale = scale, center = center))
		kappa(X, exact = exact)
	} else {
		kappa(scale(X, scale = scale, center = center), exact = exact)
	}
}


maxcorr.mer <- function (fit, exclude.intercept = TRUE) {
	so = summary(fit)
	corF = so$vcov@factors$correlation
	nam = names(fixef(fit))

	## exclude intercepts
	ns = sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
	if (ns > 0 & exclude.intercept) {
		corF = corF[-(1:ns), -(1:ns), drop = FALSE]
		nam = nam[-(1:ns)]}
	corF[!lower.tri(corF)] = 0
	maxCor = max(corF)
	minCor = min(corF)
	if (abs(maxCor) > abs(minCor)) {
		zapsmall(maxCor)
	} else zapsmall(minCor)
}


overdisp.mer <- function(model) {
	## Diagnose overdispersion in a model's response variable
	## number of variance parameters in
	##   an n-by-n variance-covariance matrix
	# from http://glmm.wikidot.com/faq
	vpars <- function(m) {
		nrow(m)*(nrow(m) + 1)/2}
	model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
	rdf <- nrow(model.frame(model)) - model.df
	rp <- residuals(model, type = "pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
	c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}


permute.varimp <- function(fit, data, verbose = F){
	# Function for computing permutation based measures of predictor importance in regression models
	# similar in spirit to method used by party::varimp() and party::varimpAUC().
	# Loops through each predictor in the model (including random effects groups) and randomly 
	# permutes its values. Then the model is refitted to the dataframe with the permuted predictor and
	# various model statistics---C, AICc, and predictive accuracy---and these are compared to those
	# of the original model. The output dataframe represents the difference between the original model
	# and the model with the permuted predictor. Larger differences reflect greater importance.
	# I prefer this method to the "drop-one" method because the model structures (and dfs) are exactly
	# the same, and we can include both fixed and random effects. 
  require(lme4, quietly = T)
  require(Hmisc, quietly = T)
  require(MuMIn, quietly = T)
  # Want this to work with glm and glmer models...
  if (class(fit) == "lmerMod" || class(fit) == "glmerMod"){
    random <- names(ranef(fit))
    vars <- colnames(attr(attr(fit@frame, "terms"), "factors"))
    y <- fit@resp$y
    # only fixed effs
    # vars <- strsplit(toString(attr(attr(fit@frame, "terms"), "predvars.fixed")), ', ')[[1]][-c(1:2)]
  } else if (class(fit)[1] == "lrm"){
    vars <- colnames(attr(f.lrm$terms, "factors"))
    y <- as.numeric(fit$y) - 1
  } else {
    vars <- colnames(attr(attr(fit$model, "terms"), "factors"))
    y <- fit$y
  }
  # remove interaction terms
  vars <- vars[grep(":", vars, invert = T)]

  # get predictions from full model
  full.probs <- predict(fit, type = "response")
  full.C <- somers2(full.probs, y)[[1]]
  full.acc <- mean(round(full.probs) == y)
  full.AICc <- MuMIn::AICc(fit)

  varimp_mat <- matrix(nrow = length(vars), ncol = 3)
  if (verbose) cat("variables run:\n")
  # loop through (fixed effects) predictors
  for (i in seq(1, length(vars))){
    # find main effect and any interactions
    d <- data
    d[, vars[i]] <- sample(data[, vars[i]])
    new_fit <- update(fit, data = d)
    new.probs <- predict(new_fit, type = "response")
    if (class(fit) == "lmerMod" || class(fit) == "glmerMod"){
      new_y <- new_fit@resp$y
    } else {
      new_y <- new_fit$y
    }
    new.C <- somers2(new.probs, new_y)[[1]]
    C.diff <- full.C - new.C
    new.acc <- mean(round(new.probs) == new_y)
    Acc.diff <- full.acc - new.acc
    new.AICc <- MuMIn::AICc(new_fit)
    AICc.diff <- new.AICc - full.AICc
    varimp_mat[i, ] <- c(C.diff, Acc.diff, AICc.diff)
    if (verbose) cat(vars[i], "... ", sep = "")
  }
  rownames(varimp_mat) <- vars
  colnames(varimp_mat) <- c("C", "accuracy", "AICc")
  return(as.data.frame(varimp_mat))
}


ranef.disp <- function(fit){
	require(gridExtra)
	r <- ranef(fit)
	plist <- list()
	for(i in 1:length(r)){
		lab <- names(r)[i]
		d <- r[[i]]
		cols <- gsub("(\\(|\\))", "", names(d), perl = T)
		names(d) <- cols
		print(cols)
		if (length(cols) > 1){
			for (j in 1:length(cols)){
				p <- ggQQ.plot(d, cols[j]) +
					ggtitle(paste0(lab, ":", cols[j]))
				plist <- append(plist, list(p))
			}
		} else {
			p <- ggQQ.plot(d, cols) + ggtitle(lab)
			plist <- append(plist, list(p))
			print(i)
		}}
	do.call("grid.arrange", c(plist, ncol = 2))
}


scores.mer <- function(fit, R2 = F, digits = 3){
	# Calculates a number of evaluative statistics for binomial mixed effects regression models fit with glmer().
	# - c, Dxy, AICc, Log likelihood, kappa, log loss (cross entropy loss), accuracy and average per class 
	# accuracy. Optionally calculates marginal and conditional (pseudo) R squared
	require(dplyr)
	require(magrittr)
	require(Hmisc)
	require(MuMIn)
	d <- fit@frame # dataframe
 	probs <- predict(fit, type = "response")
  	preds <- ifelse(probs > .5, 1, 0)
  	y <- getME(fit, "y")
  	d2 <- cbind(d, data.frame(probs = probs, preds = preds, y = y))
  	# AUC and Dxy
  	C <- Hmisc::somers2(probs, y)[1] %>% as.vector
  	Dxy <- Hmisc::somers2(probs, y)[2] %>% as.vector
 	AICc <- MuMIn::AICc(fit)
  	logL <- logLik(fit)[1]
  	N <- nrow(d)
  	kappa <- collin.fnc.mer(fit)$cnumber # measure of multicollinearity
  	# log-loss (- average log-likelihood)
  	LL <- - mean(y*log(probs) + (1 - y)*log(1 - probs))
  	# predictive accuracies
  	acc <- length(which(preds == y))/nrow(d)
  	base <- 1 - mean(y)
  	resp <- levels(d[, 1])
  	resp_col <- names(d)[1]
  	d.1 <- dplyr::filter_(d2, paste0(resp_col," == '", resp[1],"'")) %>% droplevels
  	d.2 <- dplyr::filter_(d2, paste0(resp_col," == '", resp[2],"'")) %>% droplevels
		class1.acc <- length(which(d.1$preds == d.1$y))/nrow(d.1)
		class2.acc <- length(which(d.2$preds == d.2$y))/nrow(d.2)
		out <- c(N = N, C = C, Dxy = Dxy, AICc = AICc, kappa = kappa,
			predicted.corr = acc,
			baseline = base,
			Log.Loss = LL,
			Class.1.acc = class1.acc,
			Class.2.acc = class2.acc,
			Avg.Per.Class = mean(c(class1.acc, class2.acc)))
	if (R2){
		R2 <- MuMIn::r.squaredGLMM(fit)
		out <- c(out, R2.m = R2[[1]], R2.c = R2[[2]])
	}
	if (is.null(digits)){
		return (out)
	} else {
		return (round(out, digits))
	}
}


somers.mer <- function(fit, ...){
	# C and somers Dxy for glmer using somers2()
	# from the Hmisc package
	require(Hmisc, quietly = TRUE)
	if (class(fit) == "glmerMod"){
		y = getME(fit, "y") # vector of responses
		return (somers2(fitted(fit), as.numeric(y), ...))
	}
	else stop("object not of class glmerMod")
}


VIF.mer <- function (fit) {
	## adapted from rms::vif
	v <- vcov(fit)
	nam <- names(fixef(fit))
	## exclude intercepts
	ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
	if (ns > 0) {
		v <- v[-(1:ns), -(1:ns), drop = FALSE]
		nam <- nam[-(1:ns)]
	}
	d <- diag(v)^0.5
	v2 <- diag(solve(v/(d %o% d)))
	names(v2) <- nam
	v2 <- sort(v2, decreasing = TRUE)
	return(v2)
}

