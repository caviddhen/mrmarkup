#' @title calcFAOExpenditures
#' @description read gets FAO food expenditures
#' @import tidyr dplyr mrcommons
#' @importFrom magpiesets findset
#' @export
#'
#' @return dataframe of FAO FBS
#' @author David M Chen

calcFAOExpenditures <- function() {

  magMapping <-  read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                                      package = "mrmarkup"))

  consK <- readFBSnew()
  prprk <- readFAOPrices(fill = T)

citems <- intersect(getNames(consK, dim =1), getNames(prprk, dim = 1))
expK <- consK[,,citems] * prprk[getRegions(consK), getYears(consK), citems]

#remove sugar since it's actually a processed item
expK <- expK[,,"sugar", inv = T]

expLECagg <- toolAggregate(collapseNames(expK[,,"food"]), rel = magMapping, from = "k", to = "prod",
                           partrel = TRUE, dim = 3)
expLECagg_proc <- dimSums(expK[,,"processed"][,,"cottn_pro", inv = T], dim = 3)
expLECagg_proc <- add_dimension(expLECagg_proc, dim = 3.1, nm = "Processed")

expLECagg <- mbind(expLECagg, expLECagg_proc)

# what to do with Fruit others mapping still

expLEC <- as.data.frame(expLECagg, rev = 2)  %>%
  rename("value" = .value,
         "iso3c" = Area,
         "year" = Year)  %>%
  group_by(iso3c, prod)  %>%
  filter(!is.na(value))   %>%
  mutate(value = value/1e6) #get to millions


#split expenditures AH and AFH
AFHpred <- regressFAFH(weight = TRUE, plot = TRUE, predict = "SSP2", threshold = 0.85, calibrate = TRUE)

expLEC <- inner_join(expLEC, AFHpred)  %>%
  mutate(FAOexpAFH = value * AFHshr,
         FAOexpAH = value*(1-AFHshr)) #add regressFAFH coefficients

return(expLEC)
#save(expLEC, file = "/p/projects/landuse/users/davidch/ICPdata/expLECconConv_newProc_caterSplit.Rds")

}
