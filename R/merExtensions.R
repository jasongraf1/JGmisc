#############################################################################
#
# Adapted functions for working with lme4 merMod objects
# v 0.1
#
# Jason Grafmiller
# Aug 04, 2015
#############################################################################

aic.table <- function(fit){
	# function for calculating the differences in AIC with predictors removed
	require(MuMIn, quietly = T) # add MuMIn for calculating conditional AIC
	if (class(fit) == "lmerMod" || class(fit) == "glmerMod"){
		vars <- colnames(attr(attr(fit@frame, "terms"), "factors"))
		# only fixed effs
		# vars <- strsplit(toString(attr(attr(fit@frame, "terms"),
		#															 "predvars.fixed")), ', ')[[1]][-c(1:2)]
	}
	else vars <- colnames(attr(attr(fit$model, "terms"), "factors"))

	full.aic <- AICc(fit)
	mod.aic <- c()

	print("variables run:")

	# loop through (fixed effects) predictors
	for (i in seq(1, length(vars))){
		# find main effect and any interactions
		cur_vars = paste(vars[which(grepl(vars[i], vars))], collapse = ' - ')
		new.formula = reformulate(termlabels = paste(". -", cur_vars, ""),
															response = ".")
		mod.aic <- c(mod.aic,
								 AICc(update(fit, new.formula)))
		cat(cur_vars, ", ", sep = "")
	}

	# add AIC values to dataframe
	aic.vals <- c(full.aic, mod.aic)
	aic.diffs <- aic.vals - full.aic
	aic.data <- data.frame(Predictor = c("Full model", vars),
												 AIC = aic.vals,
												 AICdiff = aic.diffs
	)
	aic.data <- droplevels(subset(aic.data, AICdiff != 0))
	# sort the dataframe
	aic.data <- aic.data[order(aic.data$AICdiff, decreasing = T),]
	# order the factor levels (for ggplot2)
	aic.data$Predictor <- factor(aic.data$Predictor,
															 levels = rev(aic.data$Predictor))
	return (aic.data)
}


colldiag.mer <- function (fit, scale = TRUE, center = FALSE, add.intercept = TRUE) {
	## adapted from perturb::colldiag, method in Belsley, Kuh, and
	## Welsch (1980). Useful for diagnosing multicollinearity
	## look for a high condition index (> 30) with
	## more than one high variance propotion.  see ?colldiag for more
	## tips.
	result = NULL
	if (center)
		add.intercept = FALSE
	if (is.matrix(fit) || is.data.frame(fit)) {
		X = as.matrix(fit)
		nms = colnames(fit)
	}
	else if (class(fit)[1] %in% c("glmerMod", "lmerMod")) {
		nms = names(fixef(fit))
		X = getME(fit, "X")
		if (any(grepl("(Intercept)", nms))) {
			add.intercept = FALSE
		}
	}
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
		rownames(pi) = 1:nrow(pi)
	}

	result = data.frame(cbind(condindx, pi))
	zapsmall(result)
}


collin.fnc.mer <- function(fit){
	# adaption of Baayen's collin.fnc() for compatibility with current version
	# of lme4
	require(languageR, quietly = T)
	if (class(fit) == "lmerMod" || class(fit) == "glmerMod"){
		return (collin.fnc(getME(fit, "X")[,-1]))
	}
	else stop("model not a merMod object")
}


kappa.mer <- function (fit,
											 scale = TRUE, center = FALSE,
											 add.intercept = TRUE,
											 exact = FALSE) {
	# adapted version of base R's kappa()
	X = getME(fit,"X")
	nam = names(fixef(fit))
	## exclude intercepts
	nrp = sum(1 * (nam == "(Intercept)"))
	if (nrp > 0) {
		X = X[, -(1:nrp), drop = FALSE]
		nam = nam[-(1:nrp)]
	}
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
		nam = nam[-(1:ns)]
	}
	corF[!lower.tri(corF)] = 0
	maxCor = max(corF)
	minCor = min(corF)
	if (abs(maxCor) > abs(minCor)) {
		zapsmall(maxCor)
	} else {
		zapsmall(minCor)
	}
}


overdisp.mer <- function(model) {
	## Diagnose overdispersion in a model's response variable
	## number of variance parameters in
	##   an n-by-n variance-covariance matrix
	# from http://glmm.wikidot.com/faq
	vpars <- function(m) {
		nrow(m)*(nrow(m) + 1)/2
	}
	model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
	rdf <- nrow(model.frame(model)) - model.df
	rp <- residuals(model, type = "pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
	c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
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


sum.stats.mer <- function(fit, y = NULL, rnd = T, R2 = T){
	# function for creating a list object with summary statistics for
	# mixed-effects models.
	require(Hmisc, quietly = TRUE)
	require(MuMIn, quietly = TRUE)
	response <- getME(fit,"y")
	z <- attr(fit@frame, "terms")
	depvar <- names(attr(terms(z), "dataClasses")[attr(terms(z),"response")])
	outcomes <- levels(fit@frame[,1])
	probs <- fitted(fit)
	DF <- attr(logLik(fit),"df")
	N <- length(fit@resp$n)
	logL <- logLik(fit)[1]
	AIC <- AIC(fit)
	AIC.c <- AIC(fit) + ((DF+1)*2*DF)/(N-DF-1)
	C <- somers2(probs, response)[1]
	Dxy <- somers2(probs, response)[2]
	percent.corr <- sum(ifelse(probs>.5,1,0)==response)/N
	baseline.acc <- max(table(response))/N
	k <- kappa(cbind(1, scale(getME(fit,"X")[,-1],center=F)), exact=T)
	R2 <- r.squaredGLMM(fit)
	# computing R2 takes some time, so leave it as optional
	if(R2){
		R2.m <- R2[[1]]
		R2.c <- R2[[2]]
		stats = list(N = as.integer(N), df = as.integer(DF),
								 logLik = logL,
								 AIC = AIC,
								 AICc = AIC.c,
								 C = C,
								 Dxy = Dxy,
								 percent.corr = (100*percent.corr),
								 baseline.acc = (100*baseline.acc),
								 kappa = k,
								 R2.m = R2.m,
								 R2.c = R2.c)
	}
	else{
		stats = list(N = as.integer(N), df = as.integer(DF),
								 logLik = logL,
								 AIC = AIC,
								 AICc = AIC.c,
								 C = C,
								 Dxy = Dxy,
								 percent.corr = (100*percent.corr),
								 baseline.acc = (100*baseline.acc),
								 kappa = k)
	}
	return(stats)
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

