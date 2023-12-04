library(data.table)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(mltools)

orig.df <- read.csv("data/proc_data.csv")

orig.df$CONTROL <- as.factor(orig.df$CONTROL)
orig.df$REGION <- as.factor(orig.df$REGION)

orig.dt <- setDT(orig.df)
orig.dt <- one_hot(orig.dt)

ml_dt <- DoubleMLData$new(
  orig.dt,
  y_col="RET_FT4_PCT_CHANGE",
  d_cols="Z",
  x_cols=c(
    "UGDS_WHITE",
    "CONTROL_1",
    "CONTROL_2",
    "REGION_1",
    "REGION_2",
    "REGION_3",
    "REGION_4",
    "REGION_5",
    "REGION_6",
    "REGION_7",
    "REGION_8",
    "ADM_RATE",
    "ACTCMMID",
    "SAT_AVG",
    "UGDS",
    "COSTT4_A",
    "TUITIONFEE_IN",
    "TUITIONFEE_OUT",
    "C150_4",
    "PCTFLOAN",
    "DEBT_MDN",
    "AGE_ENTRY",
    "FEMALE",
    "DEPENDENT",
    "FIRST_GEN",
    "MD_FAMINC",
    "D_PCTPELL_PCTFLOAN",
    "C150_4_PELL",
    "FTFTPCTPELL",
    "FTFTPCTFLOAN",
    "UG12MN",
    "ROOMBOARD_ON",
    "ENDOWBEGIN",
    "STUFACR",
    "IRPS_WHITE",
    "IRPS_WOMEN",
    "RET_FT4"
  ),
  use_other_treat_as_covariate=FALSE
)

ml_m <- lrn("classif.lda")
ml_g <- lrn("regr.xgboost")

model <- DoubleMLIRM$new(ml_dt, ml_g, ml_m, n_folds=2)
model$fit()
print(model)



