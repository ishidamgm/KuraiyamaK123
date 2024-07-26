# TrunkVolume_cah_model.R

library(caret)

#' Estimate parameters for function of tree trunk volume with crown area and tree height
#'
#' TrunkVolume = a * CrownArea^b * TreeHeight^c
#'
#' @param d data.frame
#'
#' @return TV list of nls results (TV$all, TV$conifer, TV$broadleaved)
#' @export
#'
#' @examples
#' # data set from  k123_field ####
#' d <- k123_field
#' sp <- d$sp ; dbh <- d$dbh ;  h <- d$h ;  ca <- d$ca
#' conif <- d$conif==1 ; crown <- d$crown==1
#' v <-TrunkVolume_roman(sp,dbh,h)
#' d <- data.frame(sp,dbh,h,ca,v,conif,crown)
#' d <- na.omit(d)
#' head(d)
#' # all tree species ####
#'
#' all <- TrunkVolume_cah_model(d)
#' summary(all)
#'
#' # coniferous species ####
#' conifer <- TrunkVolume_cah_model(subset(d,conif))
#' summary(conifer)
#'
#' # broadleaved tree species ####
#' broadleaved <- TrunkVolume_cah_model(subset(d,!conif),a=0.01136,b=0.65086,c=0.78120)
#' summary(broadleaved)
#' TV <- list(all=all,conifer=conifer,broadleaved=broadleaved)
#' TV
#' # save(TV,file="TrunkVolume_cah_TV.RData")
#' # load("TrunkVolume_cah_TV.RData")
#' l <- lapply(TV,nls_model_summary)
#'
#' df<-data.frame(n=rbind(l$all$n,l$conifer$n,l$broadleaved$n),row.names=c("all","conifer","bloadleaved"))
#' df<-data.frame(df,rbind(l$all$coef,l$conifer$coef,l$broadleaved$coef))
#' df<-data.frame(df,rbind(l$all$ast,l$conifer$ast,l$broadleaved$ast),rbind(l$all$P,l$conifer$P,l$broadleaved$P))
#' df<-data.frame(df,ResidualStandardError=rbind(l$all$se,l$conifer$se,l$broadleaved$se))
#' df<-data.frame(df,Adjusted_R_squared=rbind(l$all$adjR2,l$conifer$adjR2,l$broadleaved$adjR2))
#' df
TrunkVolume_cah_model　<-function(d,a = 4.147e-05, b = 0.3017, c = 2.958){
  # caret用のカスタムトレーニング関数の定義
  nlsFit <- function(x, y, wts, param, lev, last, classProbs, ...) {
    df <- data.frame(v = y, ca = x[,1], h = x[,2])
    mod <- nls(v ~ a * ca^b * h^c, data = df, start = list(a = param$a, b = param$b, c = param$c))
    mod
  }

  # 予測関数の定義
  nlsPredict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata = data.frame(ca = newdata[,1], h = newdata[,2]))
  }

  # モデルの格納
  nlsModel <- list(
    type = "Regression",
    library = NULL,
    loop = NULL,
    parameters = data.frame(parameter = c("a", "b", "c"),
                            class = rep("numeric", 3),
                            label = c("a", "b", "c")),
    grid = function(x, y, len = NULL, search = "grid") {},
    fit = nlsFit,
    predict = nlsPredict,
    prob = NULL,
    varImp = NULL
  )

  # 2 ####
  # データの準備
  set.seed(123)

  # トレーニングデータとテストデータに分割
  trainIndex <- createDataPartition(d$v, p = .8, list = FALSE)
  trainData <- d[trainIndex,]
  testData  <- d[-trainIndex,]

  # k分割交差検証の設定
  set.seed(123)
  train_control <- trainControl(method = "cv", number = 10)

  # モデルのトレーニング
  tunedParams <- expand.grid(a = a, b = b, c = c)
  nls_fit <- train(
    x = trainData[, c("ca", "h")],
    y = trainData$v,
    method = nlsModel,
    trControl = train_control,
    tuneGrid = tunedParams
  )

  # モデルの結果を表示
  #print(nls_fit)


  # 3 ####
  # クロスバリデーションで得られた最適なパラメータを使用して、全データで最終モデルをフィッティング
  final_model <- nls(v ~ a * ca^b * h^c, data = d, start = list(a = a, b = b, c = c))

  # 最終モデルの要約を表示
  #summary(final_model)
  return(final_model)
}

#' summarize of nls(Nonlinear Least Squares) result
#'
#' @param nls_res nls(Nonlinear Least Squares) result
#'
#' @return data frame of summarized nls result
#'
#' @export
#'
#' @examples
#' nls_model_summary(TV$all)
#' nls_model_summary(TV$conifer)
#' nls_model_summary(TV$broadleaved)
nls_model_summary <- function(nls_res=TV$all){
  # パラメーターの推定値と標準誤差の抽出
  model_summary <- summary(nls_res)



  # パラメーターの推定値
  params <- coef(model_summary)
  # 標準誤差
  stderr <- coef(model_summary, se = TRUE)[, "Std. Error"]

  # t値
  tvalues <- params / stderr

  # p値の計算
  pvalues <- 2 * pt(abs(tvalues), df.residual(nls_res), lower.tail = FALSE)

  # 結果をデータフレームにまとめる
  result <- data.frame(
    #Parameter = names(params),
    Estimate = params,
    Std.Error = stderr,
    t.value = tvalues,
    p.value = pvalues
  )
  result <- data.frame(
    #Parameter = names(params),
    params,
    stderr,
    tvalues,
    pvalues
  )

  # return(result)

  residual_standard_error <- model_summary$sigma
  # 調整済み決定係数の計算
  residuals <- resid(nls_res)
  sse <- sum(residuals^2)  # 残差平方和 (Sum of Squared Errors)
  sst <- sum((d$v - mean(d$v))^2)  # 全平方和 (Total Sum of Squares)
  r_squared <- 1 - (sse / sst)  # 決定係数 (R-squared)
  n <- length(residuals)  # 観測数
  p <- length(coef(nls_res))  # パラメーター数
  adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))  # 調整済み決定係数

  # summary list
  coef_summary <- model_summary$coefficients
  n <- length(predict(nls_res))
  coef <- coef_summary[,"Estimate"]
  P <- coef_summary[,"Pr(>|t|)"]
  ast <- sapply(P,p_value_to_asterisk )
  se <- residual_standard_error
  adjR2 <- adjusted_r_squared
  l <- list(n=n,coef=coef,P=P,ast=ast,se=se,adjR2=adjR2)

  return(l)

}


# p値をアスタリスクに変換する関数 ####
#
#' p_value_to_asterisk
#'
#' @param a P values
#'
#' @return strings P<0.05 "*", 0.01 "**", 0.001 "***"
#' @export
#'
#' @examples
#' p<-c(5.588200e-03,3.021976e-26,1.196543e-97,0.2,0.05,0.01 )
#' sapply(p,p_value_to_asterisk )

p_value_to_asterisk <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}







# 以下、参考　>>>>>>> 2024/7/25 #####

# 生　####

# caret用のカスタムトレーニング関数の定義
nlsFit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  df <- data.frame(y = y, ca = x[,1], h = x[,2])
  mod <- nls(y ~ a * ca^b * h^c, data = df, start = list(a = param$a, b = param$b, c = param$c))
  mod
}

# 予測関数の定義
nlsPredict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata = data.frame(ca = newdata[,1], h = newdata[,2]))
}

# モデルの格納
nlsModel <- list(
  type = "Regression",
  library = NULL,
  loop = NULL,
  parameters = data.frame(parameter = c("a", "b", "c"),
                          class = rep("numeric", 3),
                          label = c("a", "b", "c")),
  grid = function(x, y, len = NULL, search = "grid") {},
  fit = nlsFit,
  predict = nlsPredict,
  prob = NULL,
  varImp = NULL
)

# 2 ####
# データの準備
set.seed(123)

# トレーニングデータとテストデータに分割
trainIndex <- createDataPartition(d$v, p = .8, list = FALSE)
trainData <- d[trainIndex,]
testData  <- d[-trainIndex,]

# k分割交差検証の設定
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# モデルのトレーニング
tunedParams <- expand.grid(a = 4.147e-05, b = 0.3017, c = 2.958)
nls_fit <- train(
  x = trainData[, c("ca", "h")],
  y = trainData$v,
  method = nlsModel,
  trControl = train_control,
  tuneGrid = tunedParams
)

# モデルの結果を表示
print(nls_fit)


# 3 ####
# クロスバリデーションで得られた最適なパラメータを使用して、全データで最終モデルをフィッティング
final_model <- nls(v ~ a * ca^b * h^c, data = d, start = list(a = 4.147e-05, b = 0.3017, c = 2.958))

# 最終モデルの要約を表示
summary(final_model)


names(k123_field)

# data set from  k123_field ####
v <- k123_field$Vrin
ca <- k123_field$ca
h <- k123_field$h
conif<-k123_field$conif==1
d <- data.frame(ca,h,v,conif)
d <- na.omit(d)

# nls ####
model <- nls(v ~ a * ca^b * h^c, data = d, start = list(a = 4.147e-05 , b = 0.3017 , c = 2.958))
summary(model)

# 観測値と予測値の残差をプロット
predicted <- predict(model)
residuals <- d$v - predicted

# 残差のプロット
plot(predicted, residuals)
abline(h = 0, col = "red")

# R^2の計算
SSE <- sum(residuals^2)
SST <- sum((d$v - mean(d$v))^2)
R2 <- 1 - SSE / SST
R2

# AICの計算
AIC(model)

# RMSEの計算
RMSE <- sqrt(mean(residuals^2))
RMSE

# クロスバリデーション　caret ####
library(caret)
## caret用のカスタムトレーニング関数の定義 ####
nlsFit <- function(d, wts, param, lev, last, classProbs, ...) {
  mod <- nls(v ~ a * ca^b * h^c, d = data.frame(v = d$v, ca = d$ca, h = d$h),
             start = list(a = param$a, b = param$b, c = param$c))
  mod
}

## 予測関数の定義 ####
nlsPredict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata = newdata)
}

## モデルの格納 ####
nlsModel <- list(
  type = "Regression",
  library = NULL,
  loop = NULL,
  parameters = data.frame(parameter = c("a", "b", "c"),
                          class = rep("numeric", 3),
                          label = c("a", "b", "c")),
  grid = function(x, y, len = NULL, search = "grid") {},
  fit = nlsFit,
  predict = nlsPredict,
  prob = NULL,
  varImp = NULL
)

## データをトレーニングセットとテストセットに分割 ####
set.seed(123)
trainIndex <- createDataPartition(d$v, p = .8,
                                  list = FALSE,
                                  times = 1)
trainData <- d[ trainIndex,]
testData  <- d[-trainIndex,]

## nlsモデルのフィッティング ####
model_cv <- nls(v ~ a * ca^b * h^c, data = trainData, start = list(a = 4.147e-05 , b = 0.3017 , c = 2.958))

## テストデータでの予測 ####
predictions <- predict(model_cv, newdata = testData)

## RMSEの計算 ####
test_residuals <- testData$v - predictions
test_RMSE <- sqrt(mean(test_residuals^2))
test_RMSE

model_cv

# print(model_cv$bestTune) >>> NULL !!!

## k分割交差検証の実施　####

# k分割交差検証の設定
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# モデルのトレーニング
tunedParams <- expand.grid(a = 4.147e-05, b = 0.3017, c = 2.958)
nls_fit <- train(
  v = trainData[, c("ca", "h")],
  v = trainData$v,
  method = nlsModel,
  trControl = train_control,
  tuneGrid = tunedParams
)

# モデルの結果を表示
print(nls_fit)



