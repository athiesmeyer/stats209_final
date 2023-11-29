library('tidyverse')
library('DOS2')
library('optmatch')
library('RItools')
source('../../scripts/utility.R')

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

orig.df <- read.csv("data/proc_data.csv")

orig.df$CONTROL <- as.factor(orig.df$CONTROL)
orig.df$REGION <- as.factor(orig.df$REGION)

lr.model <- glm(Z ~ . - UNITID - INSTNM - ADMCON7_2 - RET_FT4_PCT_CHANGE,
                family="binomial",
                data=orig.df)
prop.scores <- lr.model$fitted.values
orig.df$PROP <- prop.scores

plot(xBalance(Z ~ . - UNITID - INSTNM - ADMCON7_2 - RET_FT4_PCT_CHANGE - 1,
              data=orig.df), ggplot=TRUE)

h.0 <- hist(prop.scores[orig.df$Z == 0], plot=FALSE)
h.1 <- hist(prop.scores[orig.df$Z == 1], plot=FALSE)
plot(h.0, col=c1, main='Propensity Score of Control and Treatment',
     xlab='Propensity Score')
plot(h.1, col=c2, add=TRUE)
legend(0.15, 600, legend=c('Control', 'Treatment'),
       fill=c(c1, c2))

mat.1 <- match_on(Z ~ . - UNITID - INSTNM - ADMCON7_2 - RET_FT4_PCT_CHANGE - PROP,
                   data=orig.df)

mat.2 <- addalmostexact(mat.1, z=orig.df$Z, f=orig.df$CONTROL)
mat.3 <- addalmostexact(mat.2, z=orig.df$Z, f=orig.df$REGION)

mat.4 <- addcaliper(mat.3, z=orig.df$Z, p=orig.df$RET_FT4)
mat.5 <- addcaliper(mat.4, z=orig.df$Z, p=orig.df$PROP)

ms <- pairmatch(mat.5, data=orig.df)

plot(xBalance(Z ~ . - UNITID - INSTNM - ADMCON7_2 - RET_FT4_PCT_CHANGE - 1,
              strata=list(unstrat=NULL, ms=~ms),
              data=orig.df), ggplot=TRUE)

mdf <- summarize.match(orig.df, ms)

###### ANALYSIS ######
tau.f <- mean(mdf$RET_FT4_PCT_CHANGE.1 - mdf$RET_FT4_PCT_CHANGE.0)

taus <- c()
for (i in 1:1000) {
  random.swap <- sample(c(TRUE, FALSE), dim(mdf)[1], replace=TRUE)
  treated.y <- mdf$RET_FT4_PCT_CHANGE.1
  control.y <- mdf$RET_FT4_PCT_CHANGE.0
  treated.y[random.swap] <- mdf$RET_FT4_PCT_CHANGE.0[random.swap]
  control.y[random.swap] <- mdf$RET_FT4_PCT_CHANGE.1[random.swap]
  
  tau <- mean(treated.y - control.y)
  taus <- append(taus, tau)
}

mean(taus <= tau.f)

n.pairs <- dim(mdf)[1]

tau.m <- mean(mdf$RET_FT4_PCT_CHANGE.1 - mdf$RET_FT4_PCT_CHANGE.0)

irr.cols <- c("UNITID", "PROP", "INSTNM", "ADMCON7_2", "Z")

mu.0 <- lm(RET_FT4_PCT_CHANGE ~ .,
           data=orig.df[orig.df$Z == 0, !(names(orig.df) %in% irr.cols)])
mu.1 <- lm(RET_FT4_PCT_CHANGE ~ .,
           data=orig.df[orig.df$Z == 1, !(names(orig.df) %in% irr.cols)])

ignore.cols.0 <- c("UNITID.0", "PROP.0", "INSTNM.0", "ADMCON7_2.0", "Z.0",
                   "RET_FT4_PCT_CHANGE.0")
ignore.cols.1 <- c("UNITID.1", "PROP.1", "INSTNM.1", "ADMCON7_2.1", "Z.1",
                   "RET_FT4_PCT_CHANGE.1")

mask.0 <- c()
mask.1 <- c()
for (i in 1:dim(mdf)[2]) {
  if (grepl(".0", names(mdf)[i], fixed=TRUE) & (!(names(mdf)[i] %in% ignore.cols.0))) {
    mask.0 <- c(mask.0, TRUE)
    mask.1 <- c(mask.1, FALSE)
  } else if (grepl(".1", names(mdf)[i], fixed=TRUE) & (!(names(mdf)[i] %in% ignore.cols.1))) {
    mask.0 <- c(mask.0, FALSE)
    mask.1 <- c(mask.1, TRUE)
  } else {
    mask.0 <- c(mask.0, FALSE)
    mask.1 <- c(mask.1, FALSE)
  }
}

mdf.0 <- mdf[, mask.0]
new.names <- c()
for (i in 1:dim(mdf.0)[2]) {
  col <- names(mdf.0)[i]
  new.names <- c(new.names, substring(col, 1, nchar(col)-2))
}
names(mdf.0) <- new.names

mdf.1 <- mdf[, mask.1]
names(mdf.1) <- new.names

mu.1.preds.treated <- predict(mu.1, mdf.1)
mu.1.preds.control <- predict(mu.1, mdf.0)
mu.0.preds.treated <- predict(mu.0, mdf.1)
mu.0.preds.control <- predict(mu.0, mdf.0)

b <- mean(mu.0.preds.treated - mu.0.preds.control)
tau <- tau.m - b

# ks <- c()
# for (i in 1:n.pairs) {
#   # Using afqtct as proxy for id as its unique
#   # per individual
#   id <- mdf[i, "UNITID.0"]
#   ks <- c(ks, sum(id == mdf$UNITID.0))
# }

var <- (1 / n.pairs^2) * (sum((mdf$RET_FT4_PCT_CHANGE.1 - mu.1.preds.treated)^2) +
                            sum((mdf$RET_FT4_PCT_CHANGE.0 - mu.0.preds.control)^2))

tau + 1.96 * sqrt(var)
tau - 1.96 * sqrt(var)

pnorm(tau, mean=0, sd=sqrt(var))

