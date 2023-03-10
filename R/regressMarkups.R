#' @title regressMarkups
#' @description regress markups
#' @param plot plot the regression lines
#' @import tidyverse
#' @importFrom minpack.lm nlsLM
#' @import Metrics
#' @import ggplot2
#' @import ggrepel
#' @import brms
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen

regressMarkups <- function(plot = TRUE){

library(brms)
library(mrmarkup)
setConfig(forcecache=T)

FAOmarkuppT <- calcMarkup(plot = TRUE)

## make weight same scale as 
FAOmarkuppT <- FAOmarkuppT  %>% group_by(BHName)  %>% 
             add_count()  %>% 
             group_by(year, BHName)  %>% 
             mutate(totPop = sum(pop),
             weight = pop/totPop * n)
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
  lm(FAOmarkup_perT ~ loggdp, weight=weight,
     data = df)
}

#exp model  simple
exp_ms <- function(df){
  minpack.lm::nlsLM(FAOmarkup_perT ~ a*(b^loggdp),
                    weight = weight,
                    alg = "plinear",
                    data = df,
                    start = list(a = 0.005, b = 10 ))
}
exp_ms <- possibly(exp_ms, otherwise = "Error, possibly singular")

#exp model
exp_m <- function(df){
  nls(FAOmarkup_perT ~ a*(b^loggdp) + c,
      weight = weight,
      data = df,
      start = list(a = 0.07, b = 10, c=0))
}
exp_m <- possibly(exp_m, otherwise = "Error, possibly singular")

options(mc.cores = 4)

exp_sbr <-function(df){ brm(
  bf(FAOmarkup_perT|weights(weight)~  a*(b^loggdp),
     a ~ 1, b ~ 1,
     nl = TRUE),
  data = df, family = gaussian(),
  prior = c( prior(normal(1, 500), nlpar = "a"),
             prior(normal(10, 80), nlpar = "b")),
  control = list(adapt_delta = 0.9 ))
}


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



markupCater  <- FAOmarkuppT   %>% 
        mutate(loggdp = log(gdp, base = 10)) %>% 
           select(iso3c, year, BHName, FAOmarkupNoCater_perT, FAOmarkupwCater_perT,
                    loggdp, pop)  %>% 
           rename("noCater" = FAOmarkupNoCater_perT, "Cater" = FAOmarkupwCater_perT)  %>% 
           pivot_longer(cols = c(noCater, Cater), names_to = "Cater") %>% 
         ungroup()  %>% 
             add_count()  %>% 
             mutate(totPop = sum(pop),
             weight = pop/totPop * n) 

markupPrior <- c(
  set_prior("normal(0.0001, 0.1)", nlpar = "a", lb=0),
  set_prior("normal(15, 20)", nlpar = "b", lb=1),
  set_prior("normal(10, 10)", class="sd", nlpar="a"),
  set_prior("normal(0, 20)", class="sd", nlpar="b"))

markupPriorTest <- c(
    set_prior("normal(5,10)", class = "alpha", lb=0),
  set_prior("normal(0.1, 0.05)", nlpar = "a", lb=0),
  set_prior("normal(15, 20)", nlpar = "b", lb=0),
  set_prior("normal(0, 10)", class="sd", nlpar="a"),
  set_prior("normal(0, 200)", class="sd", nlpar="b"))



markupPriorTight  <- c(
  set_prior("normal(0.1, 0.05)", nlpar = "a", lb=0),
  set_prior("normal(10, 5)", nlpar = "b", lb=1),
  set_prior("normal(0.1, 0.05)", class="sd", nlpar="a"),
  set_prior("normal(1, 0.5)", class="sd", nlpar="b"))

markupPrior

brmMarkup <- brm(
  bf(value|weights(weight)~  a*(b^loggdp),
     a ~ (1|BHName) + (1|Cater), b ~  (1|BHName),
     nl = TRUE),
     prior = markupPrior, seed = 2323,
  data = markupCater, family = gaussian(),
  control = list(adapt_delta = 0.99, max_treedepth= 15)) 

brmMarkup2 <- brm(
  bf(value|weights(weight)~  a*(b^loggdp),
     a ~ (1|BHName) + (1|Cater), b ~  (1|BHName),
     nl = TRUE),
     prior = markupPrior, seed = 2323, init = 0, 
  data = markupCater, family = gaussian(),
  control = list(adapt_delta = 0.99, max_treedepth= 15)) 


#Prior predictive check with sample_prior="only"
fitPrior <-  brm(
  bf(value|weights(weight)~  a*(b^loggdp),
     a ~ (1|BHName) + (1|Cater), b ~  (1|BHName),
     nl = TRUE),
     prior = markupPriorTight, 
  data = markupCater, init = 0,
           sample_prior = "only", family = gaussian(),
  control = list(adapt_delta = 0.99, max_treedepth= 15)) 

pp_check(fitPrior)

get_prior(bf(value|weights(weight)~  a*(b^loggdp),
     a ~ (1|BHName|Cater), b ~  (1|BHName),
     nl = TRUE),
     prior = markupPrior, 
  data = markupCater, family = "skew_normal",
  control = list(adapt_delta = 0.99 )) 


#Plots the prior predictive check
pp_check(brmMarkup)


save(brmMarkup, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/brmshiermodel.Rda")

load("/p/projects/magpie/users/davidch/ICPdata_cluster/brms4hiermodel.Rda")
summary(t, prior = T)

prior_summary(brmMarkup4)

fitted <- fitted(brmMarkup4)
z <- cbind(markupCater, fitted)


pred <- predict(brmMarkup4)
pred <- pred  %>%  as.data.frame()  %>% 
       rename("pred25" = `Q2.5`,
              "pred975" = `Q97.5`)  %>% 
              select("pred25", "pred975")
z1 <- cbind(z, pred)
z1 <- inner_join(select(markupCater, iso3c, loggdp),
              z, by = "loggdp")

head(z)
head(markupCater)

t <- cbind(brmSmooth1$data, pred)

z2 <- cbind(z1, t)

y <- mutate(z,          label = case_when(
                        weight > 100 ~ iso3c),
                      BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                         "Meat",
                                                         "Milk products", "Eggs", "Processed")))



ggplot(z1, aes(x = loggdp, y = Estimate)) +
geom_line()+ 
geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "lightgreen")+
geom_point(aes(y=value)) +
facet_grid(cols = vars(Cater),rows=vars(BHName),
           scales = "free_y")


brmsplot <- ggplot(y, aes(y=Estimate, x= loggdp)) +
 # geom_ribbon(aes(ymin = pred25, ymax = pred975), fill =  "#ADDEB9", alpha = 0.5 ) +
  geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
  geom_point(aes(y=value, size = weight)) +
  geom_line(aes(size = 1), color = "darkgreen") +
  ggtitle("Consumer Price Markups by Price and Consumption") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$05 / tWM)") + xlab("log10(GDPpc)")+
facet_grid(rows = vars(Cater), cols = vars(BHName),
           scales = "free_y") +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 22) + 
  theme(legend.position = "none")# facet_wrap(~Bhagg, scales = "free")
brmsplot
ggsave(brmsplot, height =  18, width = 30, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Fig1brm.pdf")
ggsave(brmsplot, height =  18, width = 30, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Fig1brm.png")






filter(z, iso3c == "COG" )
plot(conditional_effects(t), points = T)
plot(t)
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
         expsbr = map(data, exp_sbr),
        expsbrR2 = map(expsbr, R2_func),
        expsbrR2w = map(expsbr, possibly(efrons_pseudo_r2, otherwise = "no model")),
        expsbrRMSE = map2(data, expsbr, possibly(rmse_func, otherwise = "nomodel")),
        expsbrBIC =  map(expsbr,  possibly(BIC, otherwise = "no model")),
        # MMb = map(data, MM_mb),
         lm_coef = map(lm, coef),
         exp_coef = map(exp, possibly(coef, otherwise = "no convergence")),
         exps_coef = map(exps, possibly(coef, otherwise = "no convergence")),
         expsbr_coef = map(expsbr, possibly(fixef, otherwise = "no converge")))  
        
        
 modsNoLogwCater  <-   modsNoLogwCater  %>%     mutate(expsbr_coef = map(expsbr, possibly(fixef, otherwise = "no convergence") ))

modsNoLogwCater$expsbr_co
modsNoLogwCater$exps_coef

modsNoLogwCater$expsbr
modsNoLogwCater$exps[[1]]
plot(conditional_effects(modsNoLogwCater$expsbr[[3]]), points = TRUE)
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
         expsbr = map(data, exp_sbr),
         expsbrR2 = map(expsbr, R2_func),
        expsbrR2w = map(expsbr, possibly(efrons_pseudo_r2, otherwise = "no model")),
         expsbrRMSE = map2(data, expsbr, possibly(rmse_func, otherwise = "nomodel")),
         expsbrBIC =  map(expsbr,  possibly(BIC, otherwise = "no model")),
         exps = map(data, exp_ms),
         expsR2 = map(exps, R2_func),
         expsR2w = map(exps, possibly(efrons_pseudo_r2, otherwise = "no model")),
         expsRMSE = map2(data, exps, possibly(rmse_func, otherwise = "nomodel")),
         expsBIC =  map(exps,  possibly(BIC, otherwise = "no model")),
         # MMb = map(data, MM_mb),
         lm_coef = map(lm, coef),
         exp_coef = map(exp, possibly(coef, otherwise = "no convergence")),
         exps_coef = map(exps, possibly(coef, otherwise = "no convergence")),
         expsbr_coef = map(expsbr, possibly(fixef, otherwise = "no converge")))  

coefsCater1 <- do.call(rbind, modsNoLogwCater$expsbr_coef)  %>%
               as_tibble(rownames = NA)  %>%
               rownames_to_column() 
coefsCater1$prod <- rep(names(modsNoLogwCater$expsbr_coef), each = 2)
coefs1cater <- coefsCater1  %>% mutate(rowname = gsub("_Intercept", "", rowname))  %>%  
              ## here get uncertainty
              select(rowname, prod, Estimate)  %>% 
               pivot_wider(values_from = Estimate, names_from = rowname)  %>% 
               mutate(cater = "cater")

coefsNoCater1 <- do.call(rbind, modsNoLogNoCater$expsbr_coef)  %>%
               as_tibble(rownames = NA)  %>%
               rownames_to_column() 
coefsNoCater1$prod <- rep(names(modsNoLogNoCater$expsbr_coef), each = 2)
coefs1Nocater <- coefsNoCater1  %>% mutate(rowname = gsub("_Intercept", "", rowname))  %>%  
              ## here get uncertainty
              select(rowname, prod, Estimate)  %>% 
               pivot_wider(values_from = Estimate, names_from = rowname)  %>% 
               mutate(cater = "noCater")

coefsbr <- rbind(coefs1cater, coefs1Nocater )



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
      b <- modsNoLogNoCater$expsbr[[1]]
   fr <- filter(FAOmarkuppT2, BHName == prods[1])
   fr <- cbind(fr, fitted(b))


   t <- modsNoLogNoCater$exps[[2]]
   b <- modsNoLogNoCater$expsbr[[2]]
   proc <- filter(FAOmarkuppT2, BHName == prods[2])
   proc <- cbind(proc, fitted(b))


t <- modsNoLogNoCater$exps[[3]]
b <- modsNoLogNoCater$expsbr[[3]]
mt <- filter(FAOmarkuppT2, BHName == prods[3])
mt <- cbind(mt, fitted(b))

  t <- modsNoLogNoCater$exps[[4]]
  b <- modsNoLogNoCater$expsbr[[4]]
  ve <- filter(FAOmarkuppT2, BHName == prods[4])
 ve <- cbind(ve, fitted(b))

t <- modsNoLogNoCater$exps[[5]]
b <- modsNoLogNoCater$expsbr[[5]]
milk <- filter(FAOmarkuppT2, BHName == prods[5])
milk <- cbind(milk, fitted(b))

t <- modsNoLogNoCater$exps[[6]]
b <- modsNoLogNoCater$expsbr[[6]]
br <- filter(FAOmarkuppT2, BHName == prods[6])
br <-  cbind(br, fitted(b))

t <- modsNoLogNoCater$exps[[7]]
b <- modsNoLogNoCater$expsbr[[7]]
egg <- filter(FAOmarkuppT2, BHName == prods[7])
egg <- cbind(egg, fitted(b))



t <- modsNoLogwCater$exps[[1]]
b <- modsNoLogwCater$expsbr[[1]]
frC <- filter(FAOmarkuppT2, BHName == prods[1])
frC <- cbind(frC, fitted(b))

t <- modsNoLogwCater$exps[[2]]
b <- modsNoLogwCater$expsbr[[2]]
procC <- filter(FAOmarkuppT2, BHName == prods[2])
procC <- cbind(procC, fitted(b))

t <- modsNoLogwCater$exps[[3]]
b <- modsNoLogwCater$expsbr[[3]]
mtC <- filter(FAOmarkuppT2, BHName == prods[3])
mtC <- cbind(mtC, fitted(b))

t <- modsNoLogwCater$exps[[4]]
b <- modsNoLogwCater$expsbr[[4]]
veC <- filter(FAOmarkuppT2, BHName == prods[4])
veC <- cbind(veC, fitted(b))

t <- modsNoLogwCater$exps[[5]]
b <- modsNoLogwCater$expsbr[[5]]
milkC <- filter(FAOmarkuppT2, BHName == prods[5])
milkC <- cbind(milkC, fitted(b))

t <- modsNoLogwCater$exps[[6]]
b <- modsNoLogwCater$expsbr[[6]]
brC <- filter(FAOmarkuppT2, BHName == prods[6])
brC <- cbind(brC, fitted(b))

t <- modsNoLogwCater$exps[[7]]
b <- modsNoLogwCater$expsbr[[7]]
eggC <- filter(FAOmarkuppT2, BHName == prods[7])
eggC <- cbind(eggC, fitted(b))


regPredsNoCater <-  rbind(br, mt, fr, ve, milk, egg, proc)
regPredswCater <-  rbind(brC, mtC, frC, veC, milkC, eggC, procC)


regPredsNoC <- mutate(regPredsNoCater,
                      label = case_when(
                        pop > 100 ~ iso3c),
                      BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                         "Meat",
                                                         "Milk products", "Eggs", "Processed")))

a <- ggplot(regPredsNoC, aes(y=Estimate, x= loggdp)) +
  geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
  geom_line(aes(size = 2), color = "darkgreen") +
  ggtitle("a) Consumer Price Markups Food-At-Home") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$17 / tWM)") + xlab("log10(GDPpc)")+
  facet_wrap(~ BHName, scales = "free", nrow = 2) +
 ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 22) + 
  theme(legend.position = "none")# facet_wrap(~Bhagg, scales = "free")
#label larger countries only
a

regPredswC <- mutate(regPredswCater,
                     label = case_when(
                       pop > 100 ~ iso3c),
                     BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                        "Meat",
                                                        "Milk products", "Eggs", "Processed")))
 b <- ggplot(regPredswC, aes(y=Estimate, x= loggdp)) +
   geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
  geom_line(aes(size = 2), color = "darkgreen") +
  ggtitle("b) Consumer Price Markups Food-Away-from-Home") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$05 / tWM)") + xlab("log10(GDPpc)")+
  facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 22) + 
  theme(legend.position = "none")# facet_wrap(~Bhagg, scales = "free")
b
#library(gridExtra)
#t <- grid.arrange(a, b)
#ggsave(t, height =  24, width = 16, file = "/p/projects/landuse/users/davidch/ICPdata/Fig1.pdf")
#ggsave(t, height =  24, width = 16, file = "/p/projects/landuse/users/davidch/ICPdata/Fig1.png")

#label larger countries only


 }

return(coefs)


}


#ggplot(filter(regPredswC, BHName == "Vegetables"), aes(y=Estimate, x= loggdp)) +
#   geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
#  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
#  geom_line(aes(size = 2), color = "darkgreen") +
#  ggtitle("b) Consumer Price Markups Food-Away-from-Home") +
#  geom_hline(yintercept=0, linetype="dashed") +
#  ylab("Markup (USD$05 / tWM)") + xlab("log10(GDPpc)")+
#  facet_wrap(~ BHName, scales = "free", nrow = 2) +
#  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
#  theme_bw(base_size = 22) + 
#  theme(legend.position = "none")
