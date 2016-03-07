library(doMC)
library(dplyr)

registerDoMC(cores=4)


load("pressure.RData")

orig_date = ymd("1985-01-01")
day = 1
date = orig_date + days(day)

date_map = data.frame(index= 1:dim(phi)[3], date = get_date(1:dim(phi)[3])) %>%
  mutate(year = year(date))

get_date = function(day) orig_date + days(day)

components = foreach(year = 1985:2015, .combine="rbind") %dopar% {
  
  indexes = unlist(date_map %>%  dplyr::filter(year==year) %>% dplyr::select(index))
  phi_df = foreach(i=indexes, .combine='rbind') %do% as.vector(phi[,,i])
  
  pca_calc = prcomp(phi_df,center = TRUE,scale. = TRUE)$rotation
  
  df = matrix(ncol= 144*15, nrow= 2, data= NA)
  df[1,] = pca_calc[,1]
  df[2,] = pca_calc[,2]
  
  df = data.frame(df)
  df$component = c(1,2)
  df$year = year
  
  df

}

save(components,file = "computed_component.Rdata")

