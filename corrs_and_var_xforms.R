library(dplyr)
library(ggplot2)

#####
#Functions
#####

generate_data<-function(nobs = 1000, nox = 2, rval, exact = FALSE){
  mu<-rep(0,nox)
  sigma<-matrix(rval,
                nrow = nox,
                ncol = nox) + diag(nox)*(1-rval)
  
  MASS::mvrnorm(n = nobs, mu = mu, Sigma = sigma, empirical = exact)
}

generate_corr_data<-function(nobs){
  
  purrr::map(seq(.1,.9,.05), function(.){
    sim_dat<-generate_data(nobs = nobs,
                           rval = .)
    
    as.data.frame(cbind(sim_dat, rep(., nrow(sim_dat))))
    
  }) %>%
    purrr::map(function(.){
      
      data.table::setnames(., old = colnames(.), new = c("x","y", "true_r"))
      
      mutate(.,
             x_cat = as.numeric(cut(x, 7)),
             y_cat = as.numeric(cut(y, 7)),
             x_bin = ifelse(x_cat >= 6, 1, 0),
             y_bin = ifelse(y_cat >= 6, 1, 0)
      )
    }) %>%
    plyr::ldply() %>%
    group_by(true_r) %>%
    summarize(cont_corr = cor(x,y),
              bin_corr = cor(x_bin, y_bin),
              cat_corr = cor(x_cat, y_cat)
    )
}


#####
#Getting data
#####
test_df<-plyr::rdply(10, 
                     generate_corr_data(nobs = 10))

final_df<-tidyr::gather(test_df, key = type, value = corr, -true_r, -.n)


#####
#Plots
#####

ggthemr::ggthemr("flat")
#ggthemr::ggthemr_reset()

#note to self: factor transformation are important
ggplot(final_df, 
       aes(x = as.factor(true_r), 
           y = corr,
           fill = factor(type, labels = c("Top2 Box", "Discrete", "Continuous")))
       ) +
  geom_boxplot() +
  ggtitle("Correlation Coefficient Estimates by Variable Treatment",
        subtitle = "Drawn from 1000 Random Samples of 1000 Observations") +
  labs(x = "Known Population Correlation Coefficient",
       y = "Estimated Sample Correlation Coefficient",
       fill = "Variable Treatment") +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_y_continuous(breaks = seq(.1,.9,.05))

