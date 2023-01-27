#' @title regressMarkups
#' @description regress markups
#' @param plot plot the regression lines
#' @import tidyverse
#' @importFrom minpack.lm nlsLM
#' @import Metrics
#' @import ggplot2
#' @import ggrepel
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen

regressMarkups <- function(plot = TRUE){


  FAOmarkuppT <- calcMarkup(plot = TRUE)

#######  with  catering@#!!!!!!
## below is no catering

FAOmarkuppT$FAOmarkup_perT <- FAOmarkuppT$FAOmarkupwCater_perT

FAOmarkuppT2 <- mutate(FAOmarkuppT,
                       loggdp = log(gdp, base = 10))
# #Remove HUN fruit and ISL overall?
# FAOmarkuppT2 <- filter(FAOmarkuppT2,
#                        !(BHName == "Fruit" & iso3c == "HUN"))
# FAOmarkuppT2 <- filter(FAOmarkuppT2,
#                        !(iso3c == "ISL"))
# FAOmarkuppT2 <- filter(FAOmarkuppT2,
#                        !(iso3c == "LUX"))
# FAOmarkuppT2 <- filter(FAOmarkuppT2, FAOmarkup_perT < 10000)

#linear model
lm_m <- function(df, y){
  lm(FAOmarkup_perT ~ loggdp, weight=pop,
     data = df)
}

#exp model  simple
exp_ms <- function(df){
  minpack.lm::nlsLM(FAOmarkup_perT ~ a*(b^loggdp),
                    weight = pop,
                    alg = "plinear",
                    data = df,
                    start = list(a = 0.005, b = 10 ))
}
exp_ms <- possibly(exp_ms, otherwise = "Error, possibly singular")

#exp model
exp_m <- function(df){
  nls(FAOmarkup_perT ~ a*(b^loggdp) + c,
      weight = pop,
      data = df,
      start = list(a = 0.07, b = 10, c=0))
}
exp_m <- possibly(exp_m, otherwise = "Error, possibly singular")

R2_func <- function(mod){
  RSS.p <- sum(residuals(mod)^2)  # Residual sum of squares
  (TSS <- sum((augment(mod)$FAOmarkup_perT - mean(augment(mod)$FAOmarkup_perT))^2))  # Total sum of squares
  R2 <- 1 - (RSS.p/TSS)
  return(R2)}
R2_func <- possibly(R2_func, otherwise = "no model output")

efrons_pseudo_r2<-function(model){
  pred <- predict(model)
  n <- length(pred)
  res <- resid(model)
  w <- weights(model)
  if (is.null(w)) w <- rep(1, n)
  rss <- sum(w * res ^ 2)
  resp <- pred + res
  center <- weighted.mean(resp, w)
  r.df <- summary(model)$df[2]
  int.df <- 1
  tss <- sum(w * (resp - center)^2)
  r.sq <- 1 - rss/tss
  adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
  out <- list(pseudo.R.squared = r.sq,
              adj.R.squared = adj.r.sq)
  return(out)
}


rmse_func <- function(df, model) {
  actual <- df$FAOmarkup_perT
  predicted <- predict(model)
  return(rmse(actual = actual, predicted = predicted))
}
rmse_func <- possibly(rmse_func, otherwise = "Error, no model")

prods <- unique(FAOmarkuppT2$BHName)
mods <- tibble(prod = prods)
data <- list()
for(i in mods$prod){
  data[[i]] <- filter(FAOmarkuppT2, BHName == i)
}
mods$data <- data

modsNoLogwCater <-  mods %>%
  mutate(lm = map(data, lm_m),
         lmR2 = map(lm, R2_func),
         lmRMSE = map2(data, lm, rmse_func),
         lmBIC =  map(lm, BIC),
         lmR2w = map(lm, efrons_pseudo_r2),
         exp = map(data, exp_m),
         expR2 =  map(exp, R2_func),
         expR2w = map(exp, possibly(efrons_pseudo_r2, otherwise = "no model")),
         expRMSE = map2(data, exp, rmse_func),
         expBIC =  map(exp, possibly(BIC, otherwise = "nomodel")),
         exps = map(data, exp_ms),
         expsR2 = map(exps, R2_func),
         expsR2w = map(exps, possibly(efrons_pseudo_r2, otherwise = "no model")),
         expsRMSE = map2(data, exps, possibly(rmse_func, otherwise = "nomodel")),
         expsBIC =  map(exps,  possibly(BIC, otherwise = "no model")),
         # MMb = map(data, MM_mb),
         lm_coef = map(lm, coef),
         exp_coef = map(exp, possibly(coef, otherwise = "no convergence")),
         exps_coef = map(exps, possibly(coef, otherwise = "no convergence")))

### NO CATERING

FAOmarkuppT2$FAOmarkup_perT <- FAOmarkuppT2$FAOmarkupNoCater_perT
prods <- unique(FAOmarkuppT2$BHName)
mods <- tibble(prod = prods)
data <- list()
for(i in mods$prod){
  data[[i]] <- filter(FAOmarkuppT2, BHName == i)
}
mods$data <- data

modsNoLogNoCater <-  mods %>%
  mutate(lm = map(data, lm_m),
         lmR2 = map(lm, R2_func),
         lmRMSE = map2(data, lm, rmse_func),
         lmBIC =  map(lm, BIC),
         lmR2w = map(lm, efrons_pseudo_r2),
         exp = map(data, exp_m),
         expR2 =  map(exp, R2_func),
         expR2w = map(exp, possibly(efrons_pseudo_r2, otherwise = "no model")),
         expRMSE = map2(data, exp, rmse_func),
         expBIC =  map(exp, possibly(BIC, otherwise = "nomodel")),
         exps = map(data, exp_ms),
         expsR2 = map(exps, R2_func),
         expsR2w = map(exps, possibly(efrons_pseudo_r2, otherwise = "no model")),
         expsRMSE = map2(data, exps, possibly(rmse_func, otherwise = "nomodel")),
         expsBIC =  map(exps,  possibly(BIC, otherwise = "no model")),
         # MMb = map(data, MM_mb),
         lm_coef = map(lm, coef),
         exp_coef = map(exp, possibly(coef, otherwise = "no convergence")),
         exps_coef = map(exps, possibly(coef, otherwise = "no convergence")))

coefsCater <- unlist(modsNoLogwCater$exps_coef)
coefsNoCater <-  unlist(modsNoLogNoCater$exps_coef)

coefsCater <- stack(coefsCater)  %>%
  separate(ind, sep = "[.]", into = c("prod", "value"))  %>%
  pivot_wider(names_from = value, values_from = values)
coefsCater$cater <- "cater"

coefsNoCater <- stack(coefsNoCater)  %>%
  separate(ind, sep = "[.]", into = c("prod", "value"))  %>%
  pivot_wider(names_from = value, values_from = values)
coefsNoCater$cater <- "noCater"

coefs <- rbind(coefsCater, coefsNoCater)

if(plot){

   ### plotting models
   t <- modsNoLogNoCater$exps[[1]]
   proc <- filter(FAOmarkuppT2, BHName == prods[1])
   proc$pred <- predict(t)
   t <- modsNoLogNoCater$exps[[2]]
   fr <- filter(FAOmarkuppT2, BHName == prods[2])
fr$pred <- predict(t)
t <- modsNoLogNoCater$exps[[3]]
ve <- filter(FAOmarkuppT2, BHName == prods[3])
ve$pred <- predict(t)
t <- modsNoLogNoCater$exps[[4]]
milk <- filter(FAOmarkuppT2, BHName == prods[4])
milk$pred <- predict(t)
t <- modsNoLogNoCater$exps[[5]]
br <- filter(FAOmarkuppT2, BHName == prods[5])
br$pred <- predict(t)
t <- modsNoLogNoCater$exps[[6]]
mt <- filter(FAOmarkuppT2, BHName == prods[6])
mt$pred <- predict(t)
t <- modsNoLogNoCater$exps[[7]]
egg <- filter(FAOmarkuppT2, BHName == prods[7])
egg$pred <- predict(t)

t <- modsNoLogwCater$exps[[1]]
procC <- filter(FAOmarkuppT2, BHName == prods[1])
procC$pred <- predict(t)
t <- modsNoLogwCater$exps[[2]]
frC <- filter(FAOmarkuppT2, BHName == prods[2])
frC$pred <- predict(t)
t <- modsNoLogwCater$exps[[3]]
veC <- filter(FAOmarkuppT2, BHName == prods[3])
veC$pred <- predict(t)
t <- modsNoLogwCater$exps[[4]]
milkC <- filter(FAOmarkuppT2, BHName == prods[4])
milkC$pred <- predict(t)
t <- modsNoLogwCater$exps[[5]]
brC <- filter(FAOmarkuppT2, BHName == prods[5])
brC$pred <- predict(t)
t <- modsNoLogwCater$exps[[6]]
mtC <- filter(FAOmarkuppT2, BHName == prods[6])
mtC$pred <- predict(t)
t <- modsNoLogwCater$exps[[7]]
eggC <- filter(FAOmarkuppT2, BHName == prods[7])
eggC$pred <- predict(t)


regPredsNoCater <-  rbind(br, mt, fr, ve, milk, egg, proc)
regPredswCater <-  rbind(brC, mtC, frC, veC, milkC, eggC, procC)


regPredsNoC <- mutate(regPredsNoCater,
                      label = case_when(
                        pop > 100 ~ iso3c),
                      BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                         "Meat",
                                                         "Milk products", "Eggs", "Processed")))

a <- ggplot(regPredsNoC, aes(y=pred, x= loggdp)) +
  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
  geom_line(aes(size = 2), color = "darkgreen") +
  ggtitle("a) Consumer Price Markups Food-At-Home") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$17 / tWM)") + xlab("log10(GDPpc)")+
  facet_wrap(~ BHName, scales = "free", nrow = 2) +
 ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 22)# facet_wrap(~Bhagg, scales = "free")
#label larger countries only


regPredswC <- mutate(regPredswCater,
                     label = case_when(
                       pop > 100 ~ iso3c),
                     BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                        "Meat",
                                                        "Milk products", "Eggs", "Processed")))
 b <- ggplot(regPredswC, aes(y=pred, x= loggdp)) +
  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
  geom_line(aes(size = 2), color = "darkgreen") +
  ggtitle("b) Consumer Price Markups Food-Away-from-Home") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$05 / tWM)") + xlab("log10(GDPpc)")+
  facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 22)# facet_wrap(~Bhagg, scales = "free")

#library(gridExtra)
#t <- grid.arrange(a, b)
#ggsave(t, height =  24, width = 16, file = "/p/projects/landuse/users/davidch/ICPdata/Fig1.pdf")
#ggsave(t, height =  24, width = 16, file = "/p/projects/landuse/users/davidch/ICPdata/Fig1.png")

#label larger countries only


 }

return(coefs)


}

