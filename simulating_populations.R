library(doParallel)
library(magrittr)

simulate_corr_data<-function(nobs = 1000, rval, exact = FALSE){
  mu<-rep(0,2)
  sigma<-matrix(rval,
                nrow = 2,
                ncol = 2) + diag(2)*(1-rval)
  
  MASS::mvrnorm(n = nobs, mu = mu, Sigma = sigma, empirical = exact)
}

true_r_vals<-seq(from = -.95,
                 to = .95,
                 by = .05)

clust<-makeCluster(spec = detectCores() - 1)
registerDoParallel(cl = clust)
clusterExport(cl = clust,
              varlist = c('simulate_corr_data'),
              envir = .GlobalEnv)

sim_pops<-parLapply(cl = clust,
                   true_r_vals,
                   function(.){
                     
                     cbind(data.frame(simulate_corr_data(nobs = 10000,
                                                         rval = .,
                                                         exact = TRUE)
                                      ),
                           data.frame('true_r' = rep(., 10000))
                     )
                   }
)

sim_pops<-parLapply(cl = clust,
                    sim_pops,
                    function(.){
                      
                    data.table::setnames(.,
                                         old = names(.),
                                         new = c('x','y','true_r'))
                    }
)

names(sim_pops)<-round(true_r_vals,2)

str(sim_pops$`0.9`)


final_sim_data<-plyr::ldply(sim_pops) %>%
  mutate(true_r = round(true_r, 2))

library(dplyr)
group_by(final_sim_data,
         true_r) %>%
  summarise(obs_corr = cor(x,y))

stopCluster(clust)

write.csv(final_sim_data,
          file = 'simulated_corrd_data.csv',
          row.names = FALSE)
