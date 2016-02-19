#############################################################################
#
# Adapted functions for working with MCMCglmm objects
# v 0.2
#
# Jason Grafmiller
# Aug 04, 2015
#############################################################################


# prior setting ---------------------------------------------------------

# function for fixed effects priors from Jarrod Hadfield
# http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/8608

# It returns a covariance matrix that can be passed to prior$B$V.  If the predictors
# had been scaled according to the procedure outlined in Gelman et al.
# (2008) then this would induce an identity prior covariance matrix on
# the new regression coefficients (i.e. they're iid a priori). Gelman
# recommends a scaled Cauchy or scaled t prior on these new regression
# ceofficients. I don't think this is possible in MCMCglmm* but a normal
# with variance equal to the sum of the variance components + 1 (probit)
# pi^2/3 (logit) I think is not too unreasonable - but it does put
# less weight on very extreme probabilities than the Cauchy and t. To
# achieve this just multiply the output of prior.scale by the variance
# you want to use.
#
# Bear in mind that the inputs are scaled, not the columns of the design
# matrix - this means that interactions are penalised more than main
# effects. In my field just-so stories involving 'significant'
# interactions  are often used to resuscitate a `failed' experiment, so
# penalising them by default may be no bad thing.

# Gelman et al. A WEAKLY INFORMATIVE DEFAULT PRIOR DISTRIBUTION FOR
# LOGISTIC AND OTHER REGRESSION MODELS The Annals of Applied Statistics
# 2008, Vol. 2, No. 4, 1360–1383

prior.scale <- function(formula, data){
	X1 <- model.matrix(formula, data)
	X2 <- get_all_vars(formula, data)
	X2 <- as.data.frame(lapply(X2, 
														 function(x){if(is.numeric(x)){scale(x,
																																	 scale=sd(x)*2)} else{x}}))
	X2 <- model.matrix(formula, data = X2)
	X2[, -1] <- apply(X2[, -1], 2,
										function(x){if(any(!x%in%c(0,1))){x} else{scale(x,
								 											 center = sum(x)/length(x), scale = 1)}})
	crossprod(t(solve(t(X1) %*% X1, t(X1) %*% X2)))
}


# model checking funcitons ----------------------------------------------

# these were taken from:
# https://github.com/aufrank/R-hacks/blob/master/MCMCglmm-utils.R

vif.MCMCglmm <- function (fit, intercept.columns = c(1)) {
		nF <- fit$Fixed$nfl
		v <- cov(as.matrix(fit$X[,1:nF]))
		nam <- colnames(fit$Sol[,1:nF])

		v <- v[-intercept.columns, -intercept.columns, drop = FALSE]
		nam <- nam[-intercept.columns]

		d <- diag(v)^0.5
		v <- diag(solve(v/(d %o% d)))
		names(v) <- nam
		v
	}


kappa.MCMCglmm <- function (fit, add.intercept = TRUE,
														scale = TRUE, intercept.columns = c(1)) {
	nF <- fit$Fixed$nfl
	X <- fit$X[,1:nF]
	if (add.intercept & scale) {
		kappa(cBind(rep(1), scale(X[, -intercept.columns])))
	} else if (add.intercept & !scale) {
		kappa(X)
	} else if (!add.intercept & scale) {
		kappa(scale(X[,-intercept.columns]))
	} else {
		kappa(X[,-intercept.columns])
	}
}


# model predictions -----------------------------------------------------

#' Define a generic prediction function
#'
#' This defintes a generic \code{predict2} function which
#' is similar to the usual \code{predict} but can use different
#' methods. In particular, the \code{MCMCglmm} method has features
#' not available in the regular \code{predict} method for \code{MCMCglmm}
#' objects.
#'


MCMCglmm.predict <- function(object, data, newdata = NULL){

	fix.f <- object$Fixed$formula
	response <- all.vars(fix.f)[1]

	nf <- nrow(summary(object)$solutions)
	means <- apply(object$Sol, 2, mean)
	fixed <- means[1:nf]

	# get the number of levels for each random effect
	cur.n <<- nf
	# get random effects names
	random.effs <- all.vars(object$Random$formula)
	rf.list <- list()
	for (i in 1:length(random.effs)){
		cur.n <<- cur.n + 1
		r <- random.effs[i]
		n <- length(levels(data[, r])) - 1

		rf.list[[r]] <- means[cur.n:(cur.n + n)]
		cur.n <<- cur.n + n
	}

	# create dataframe from model matrix
	modMat <- as.data.frame(as.matrix(object$X))
	if (is.null(newdata)){
		predMat <- modMat # dataframe for predictions
		for (i in 1:length(random.effs)){
			predMat[, random.effs[i]] <- 0
		}
	}
	else predMat <- newdata

	for (i in 1:nrow(modMat)){
		predMat[i, 1:nf] <- as.vector(predMat[i, 1:nf] * fixed)
		for (j in 1:length(random.effs)){
			r <- random.effs[j]
			r2 <- paste(r, data[i, r], sep = '.')
			predMat[i, r] <- as.vector(rf.list[[r]][r2])
		}
	}

	s2 <- rowSums(predMat)

	resp.levs <- levels(data[, response])

	predicted <- ifelse(s2 > 0, resp.levs[2], resp.levs[1])
	observed <- factor(data[, response],
										 levels = sort(levels(data[, response])))

	out <- list(
		logodds = s2,
		probs = exp(s2)/(1 + exp(s2)),
		perc.corr = sum(predicted == observed)/nrow(data),
		pred.tab = table(predicted, observed),
		baseline = max(prop.table(table(data[, response])))
	)

	return (out)

}


# Variable selection and importance -------------------------------------

# create a function for representing variable importance using DIC similar to
# AIC rankings in standard glmm models.

dic.table <- function(object, data, prior, family,
											burnin = 3000, nitt = 13000,
											thin = 10, pr = F, pl = F, verbose = F,
											rcov = ~units) {
	fixed.f <- object$Fixed$formula
	random.f <- object$Random$formula

	fullDIC <- object$DIC

	# get all the models
	vars <- all.vars(fixed.f)[-1]
	fchars <- as.character(fixed.f)

	dic.df <- data.frame(Predictor = c("full", vars),
											 DIC = c(fullDIC, rep(0, length(vars))),
											 DICdiff = rep(0, 1+length(vars))
	)

	print("Variables run:")

	for (v in vars){
		#print(v)
		new.vars <- gsub(v, '', fchars[3], fixed = T)
		new.vars <- gsub("\\+ *\\)", ")", new.vars, perl = T)
		new.vars <- gsub("\\+ *\\*", "+", new.vars, perl = T)
		new.vars <- gsub("^ *\\*", "", new.vars, perl = T)
		new.vars <- gsub("\\( *\\+", "(", new.vars, perl = T)
		new.vars <- gsub("\\+ *\\+", "+", new.vars, perl = T)
		new.vars <- gsub("\\+ *$", "", new.vars, perl = T)
		new.f <- as.formula(paste(fchars[2], "~", new.vars))

		new.mod <- MCMCglmm(new.f, data = data, prior = prior, rcov = rcov,
												random = random.f, family = family,
												burnin = burnin, nitt = nitt, thin = thin,
												pr = pr, pl = pl, verbose = verbose)

		curDIC <- new.mod$DIC
		curDiff <- curDIC - fullDIC
		dic.df[dic.df$Predictor == v, 2:3] <- c(curDIC, curDiff)
		cat(v, ", ", sep = "")
	}
	dic.df <- dic.df[order(dic.df$DICdiff, decreasing = T),]
	dic.df$Predictor <- factor(dic.df$Predictor,
														 levels = rev(dic.df$Predictor))

	return (dic.df)
}



# Displying model output ------------------------------------------------

MCMCglmm.table <- function(data, betas = TRUE){
	
	# functions
	lower <- function(x) quantile(x, probs = .025)
	upper <- function(x) quantile(x, probs = .975)
	above0 <- function(x) {length(which(x > 0))/length(x)}
	below0 <- function(x) {length(which(x < 0))/length(x)}
	
	df <- data.frame(mean = apply(data, 2, "mean"))
	df <- cbind(df,
						 data.frame(lower = apply(data, 2, lower)))
	df <- cbind(df,
						 data.frame(upper = apply(data, 2, upper)))
	if(betas){
		df <- cbind(df,
								data.frame(below = apply(data, 2, below0)))
		df <- cbind(df,
								data.frame(above = apply(data, 2, above0)))
		colnames(df) <- c("posterior mean", "l-95% HPDI", "u-95% HPDI", "P(&beta; < 0)", "P(&beta; > 0)")
	}
	else colnames(df) <- c("posterior mean", "l-95% HPDI", "u-95% HPDI")
	return (df)
}



