## Calculate the corr coefficients

## showing with a bar chart
a=cor(nyse[,5:30])
b=a[,1]
b
par(cex=0.6)
barplot(b, main="Stock Prices Correlation", xlab="Companies", ylab="Coefficients",col=3)

## Regression using the 5 variables with highest correlation
attach(nyse)
nyse_lm0 <- lm(GOOGL ~ CDE + AMZN + SAM + MSFT + GIL, data=nyse)
summary(nyse_lm0)

## Variable selection, forward method
nyse1 <- lm(GOOGL ~ 1, data = nyse)
add1(nyse1, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse2 <- lm(GOOGL ~ CDE, data = nyse)
add1(nyse2, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse3 <- lm(GOOGL ~ CDE + KO, data = nyse)
add1(nyse3, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse4 <- lm(GOOGL ~ CDE + KO + WMT, data = nyse)
add1(nyse4, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse5 <- lm(GOOGL ~ CDE + KO + WMT + MSFT, data = nyse)
add1(nyse5, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse6 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL, data = nyse)
add1(nyse6, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse7 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN, data = nyse)
add1(nyse7, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse8 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS, data = nyse)
add1(nyse8, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse9 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C, data = nyse)
add1(nyse9, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse10 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN, data = nyse)
add1(nyse10, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse11 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ, data = nyse)
add1(nyse11, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse12 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V, data = nyse)
add1(nyse12, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse13 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K, data = nyse)
add1(nyse13, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse14 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL, data = nyse)
add1(nyse14, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse15 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR, data = nyse)
add1(nyse15, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse16 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM, data = nyse)
add1(nyse16, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse17 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP, data = nyse)
add1(nyse17, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse18 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG, data = nyse)
add1(nyse18, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse19 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T, data = nyse)
add1(nyse19, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse20 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M, data = nyse)
add1(nyse20, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

nyse21 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)
add1(nyse21, test = "F",
     scope = ~ AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN)

## I stop here, no more significant variables for alpha=0.01

## Backwards method

nyse21 <- lm(GOOGL ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)
drop1(nyse21, test = "F",
      scope = ~ CDE + KO + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI)

nyse22 <- lm(GOOGL ~ CDE + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)
drop1(nyse22, test = "F",
      scope = ~ CDE + WMT + MSFT + DAL + XIN + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI)

nyse23 <- lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)
drop1(nyse23, test = "F",
      scope = ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI)

nyse24 <- lm(GOOGL ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)
drop1(nyse24, test = "F",
      scope = ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI)

nyse25 <- lm(GOOGL ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + M + SPGI, data = nyse)
drop1(nyse24, test = "F",
      scope = ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + M + SPGI)

nyse26 <- lm(GOOGL ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + SPGI, data = nyse)

## all values are relevant stop the algorithm at nyse24, so the method suggests a model with 17 independent variables

nyse_lm1 <- lm(GOOGL ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)

summary(nyse_lm1)
par(mfrow=c(2,2))
plot(nyse_lm1)

## Validation

data_for_fitting <- nyse[1:1000,]
data_for_predicting <- nyse[1001:nrow(nyse),]

## Prediction function for nyse_lm1

predict_google <- function(nyse, newdata){
  nyse_lm1 <- lm(GOOGL ~ WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI, data = nyse)
  predictions <- predict(nyse_lm1, newdata = newdata)
  return(predictions)
}

y_hat <- predict_google(nyse = data_for_fitting, newdata = data_for_predicting)
LMSE <- (mean(log((data_for_predicting$GOOGL - y_hat)^2)))
LMSE


## Stepwise method including Time variables

nyseStep <- lm(GOOGL ~ 1, data = nyse)
step(nyseStep,  scope = ~ Year + Month + Date + AAPL + AMZN + AZN + BP + C + CDE + DAL + DPZ + F + GIL+ JPM + K + KO + M + MSFT + NOK
     + PG + RBS + SAM + SPGI + T + V + WMT + WHR +XIN, direction = "both")

predict_google2 <- function(nyse, newdata){
  
  nyse_lm2 <- lm(formula = GOOGL ~ Date + NOK + AAPL + SAM + AMZN + DPZ 
               + V + K + MSFT + M + T + AZN + RBS + C + KO + GIL + BP 
               + DAL + F + CDE + XIN + WHR + WMT + PG, data = nyse)
  predictions2 <- predict(nyse_lm2, newdata = newdata)
  return(predictions2)
}

y_hat2 <- predict_google2(nyse = data_for_fitting, newdata = data_for_predicting)
LMSE2 <- (mean(log((data_for_predicting$GOOGL - y_hat2)^2)))
LMSE2

summary(nyse_lm2)
plot(nyse_lm2)

## Attempt of transformations

plot(GOOGL ~ AAPL, data=nyse)
plot(GOOGL ~ AMZN, data=nyse)
plot(GOOGL ~ AZN, data=nyse)
plot(GOOGL ~ BP, data=nyse)
plot(GOOGL ~ C, data=nyse)
plot(GOOGL ~ CDE, data=nyse)
plot(GOOGL ~ DAL, data=nyse)
plot(GOOGL ~ DPZ, data=nyse)
plot(GOOGL ~ F, data=nyse)
plot(GOOGL ~ GIL, data=nyse)
plot(GOOGL ~ JMP, data=nyse)
plot(GOOGL ~ K, data=nyse)
plot(GOOGL ~ KO, data=nyse)
plot(GOOGL ~ M, data=nyse)
plot(GOOGL ~ MSFT, data=nyse)
plot(GOOGL ~ NOK, data=nyse)
plot(GOOGL ~ PG, data=nyse)
plot(GOOGL ~ RBS, data=nyse)
plot(GOOGL ~ SAM, data=nyse)
plot(GOOGL ~ SPGI, data=nyse)
plot(GOOGL ~ T, data=nyse)
plot(GOOGL ~ V, data=nyse)
plot(GOOGL ~ WMT, data=nyse)
plot(GOOGL ~ WHR, data=nyse)
plot(GOOGL ~ XIN, data=nyse)

data_for_fitting <- nyse[1:1000,]
data_for_predicting <- nyse[1001:nrow(nyse),]

predict_google25T <- function(nyse, newdata){
  
  nyse$GIL.ex      <- (nyse$GIL+1)^(0.0001)  
  nyse$SAM.ex      <- (nyse$SAM+1.6)^(0.00001)
  nyse$PG.ex      <- (nyse$PG+1.5)^(0.001)
  nyse$M.ex      <- (nyse$M+2.6)^(1.1)
  
  newdata$GIL.ex      <- (newdata$GIL+1)^(0.00001)  
  newdata$SAM.ex      <- (newdata$SAM+1.6)^(0.00001)
  newdata$PG.ex      <- (newdata$PG+1.5)^(0.0001)
  newdata$M.ex      <- (newdata$M+2.6)^(1.1)
  
  nyse_lm25T <- lm(formula = GOOGL ~ Date + NOK + AAPL + SAM.ex + AMZN + DPZ 
                 + V + K + MSFT + M.ex + T + AZN + RBS + C + KO + GIL.ex + BP 
                 + DAL + F + CDE + XIN + WHR + WMT + PG.ex, data = nyse)
  predictions25T <- predict(nyse_lm25T, newdata = newdata)
  return(predictions25T)
}


y_hat25T <- predict_google25T(nyse = data_for_fitting, newdata = data_for_predicting)
LMSE25T <- (mean(log((data_for_predicting$GOOGL - y_hat25T)^2)))
LMSE25T

## NO IMPROVEMENTS in the real LMSE

## Exploring other methods: leaps and bounds

best_subset <- leaps(x = nyse[, 6:30], y = nyse[, 5],
                     nbest = 2, method = "adjr2",
                     names = colnames(nyse[,1:6:30]))

data.frame(Size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)


predict_google25L <- function(nyse, newdata){
  nyse25L <- lm(GOOGL ~ Year + Month + Date + AZN + BP + C + DAL + DPZ + GIL + K + M + MSFT + PG + RBS + SAM + SPGI + T + V + WMT + WHR, data = nyse)
  predictions25L <- predict(nyse25L, newdata = newdata)
  return(predictions25L)
}

y_hat25L <- predict_google25L(nyse = data_for_fitting, newdata = data_for_predicting)
lMSE25L <- log(mean((data_for_predicting$GOOGL - y_hat25L)^2))
lMSE25L

## NO IMPROVEMENTS