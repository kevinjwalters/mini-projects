### Based on https://www.itl.nist.gov/div898/handbook/apr/section4/apr451.r
### from https://www.itl.nist.gov/div898/handbook/apr/section4/apr451.htm


library('ggplot2')
##('grid')
##library('gridExtra')
library('ggforce')


### 1) The NIST refence example
  
  ## The qchisq function requires left tail probability inputs. 
  ## LOWER = T*2 / qchisq(1-alpha/2, df=2*(r+1))
  ## UPPER = T*2 / qchisq(alpha/2, df=2*r)
  
  ## Example.
  LOWER=1600/ qchisq(0.95, df=6)
  LOWER
  
  ##> [1] 127.0690
  
  UPPER=1600 / qchisq(.05,df=4)
  UPPER
  
  ##> [1] 2251.229
  
  
### 60%
  LOWER=1600/ qchisq(0.70, df=6)
  LOWER
  
  ##> [1] 127.0690
  
  UPPER=1600 / qchisq(.30,df=4)
  UPPER
  
  

### 2) Experimenting with code a bit
  
### Originally 5000 miscellaneous things
### occasionally failing for around a month
  
# target_mtbf <- 5
target_mtbf <- 1
target_afr <- 1 / target_mtbf
# things <- 5000
things <- 365  ### to get one failure on day 1
duration_days <- 30 
duration_years <- duration_days / 365.25
total_runtime_years <- things * duration_years
actual_failures <- 17
target_failures <- total_runtime_years * (1 / target_mtbf)

estimated_mtbf <- total_runtime_years / actual_failures
  
### 90% two sided
confidence_level <- 90 / 100
confidence_one_side <- (1 - confidence_level) / 2
lower_mtbf <- 2 * total_runtime_years / qchisq(1 - confidence_one_side, df=2 * (actual_failures + 1))
upper_mtbf <- 2 * total_runtime_years / qchisq(confidence_one_side, df=2 * actual_failures)
actual_lower <- 1 / lower_mtbf * things * duration_years
actual_upper <- 1 / upper_mtbf * things * duration_years

writeLines(sprintf("%s\n%s\n%s\n%s\n", target_mtbf, estimated_mtbf, lower_mtbf, upper_mtbf))

writeLines(sprintf("%s\n%s\n%s\n%s\n", target_failures, actual_failures, actual_lower, actual_upper))



### 3) Time to draw some charts

lower_upper_mtbf <- function(total_runtime, observed_failures, confidence_level) {
  confidence_one_side <- (1 - confidence_level) / 2
  if (observed_failures > 0) {
    obs_mtbf <- total_runtime / observed_failures
    lower_mtbf <- 2 * total_runtime / qchisq(1 - confidence_one_side,
                                             df=2 * (observed_failures + 1))
    upper_mtbf <- 2 * total_runtime / qchisq(confidence_one_side,
                                             df=2 * observed_failures)
  } else {
    obs_mtbf <- Inf
    lower_mtbf <- total_runtime / (0 - log(confidence_one_side))
    upper_mtbf <- Inf
  }
  return(c(lower_mtbf, obs_mtbf, upper_mtbf))
}


filebase <- "crr-hpp-approximation"

for (c_level in c(0.60, 0.80, 0.90, 0.95, 0.98, 0.99)) {
  x_days <- seq(1, 1000, by=0.25)
  typical_failures <- round(x_days / 365.25 * things * target_afr)
  mtbfs <- lapply(1:length(x_days),
                  function(idx) { lower_upper_mtbf(things * x_days[idx] / 365.25,
                                                   typical_failures[idx],
                                                   c_level)  })
  
  data <- data.frame(days=x_days,
                     failures=typical_failures,
                     est_mtbf=sapply(mtbfs, function(e) { e[2] } ),
                     est_mtbf_lower=sapply(mtbfs, function(e) { e[1] } ),
                     est_mtbf_upper=sapply(mtbfs, function(e) { e[3] } ),
                     est_afr=1 / sapply(mtbfs, function(e) { e[2] } ),
                     est_afr_lower=1 / sapply(mtbfs, function(e) { e[3] } ),
                     est_afr_upper=1 / sapply(mtbfs, function(e) { e[1] } )
                     )
  
  ###
  model1 <- lm(est_afr_upper ~ poly(failures, 1), data=data)
  model2 <- lm(est_afr_upper ~ poly(failures, 2), data=data)
  model3 <- nls(est_afr_upper~b / failures + a, data=data, start=list(a=1, b=1))
  model4 <- nls(est_afr_upper~b * log(failures) + a, data=data, start=list(a=1, b=1))
  model5 <- nls(est_afr_upper~1 / failures * b * log(failures) + a, data=data, start=list(a=1, b=1))
  model6 <- nls(est_afr_upper~b / failures * log(failures + c) + a, data=data, start=list(a=1, b=1, c=0))
  
  data$est_afr_upper_approx <- predict(model6, data)
  
  g1 <- ggplot(data,
               aes(x=failures)) +
        ggtitle("Approximation of dchisq for MTBF/AFR confidence values",
                subtitle=paste0("confidence value ", c_level * 100, "% (two-sided)")) +
    theme_light(base_size=28) +
    theme(plot.title=element_text(hjust=0.5, size=38),
          plot.subtitle=element_text(hjust=0.5, size=28),
          legend.position="top",
          legend.title=element_blank()) +
    geom_line(aes(y=est_afr, color='est_afr'), linewidth=1) + 
         geom_line(aes(y=est_afr_upper, color='est_afr_upper'), linewidth=1) +
         geom_line(aes(y=est_afr_upper_approx, color='eau_approx'), linewidth=1.5) +
         scale_colour_manual("", values=c("est_afr"="black",
                                          "est_afr_upper"="red",
                                          "eau_approx"="purple")) +
    labs(y="annualized failure rate") +
    # HANGS with geom_label but ok with geom_text !?!?!?!
    # geom_label(
    #  label=paste(paste("model6", format(formula(model6)), collapse=""),
    #              paste(paste0(paste0(names(coef(model6)),"="),
    #                           signif(coef(model6), digits=6)), collapse=" "),
    #              sep="\n"),
    #  x=800,
    #  y=2,
    #  hjust=1,
    #  vjust=0.5,
    #  label.padding = unit(0.4, "lines"),
    #  label.size = 0.5
    # ) + 
    # geom_text(label="test2",x=Inf,y=Inf, hjust=1, vjust=1, size=8) +
    geom_text(
      label=paste(paste("model6", format(formula(model6)), collapse=""),
                 paste(paste0(paste0(names(coef(model6)),"="),
                              signif(coef(model6), digits=6)), collapse=" "),
                 sep="\n"),
     x=Inf,
     y=Inf,
     hjust=1,
     vjust=1,
     size=8
    )
    
    g1a <- g1 + coord_cartesian(ylim=c(0, 6)) + facet_zoom(xlim=c(0, 20), zoom.size=1)
    g1b <- g1 + coord_cartesian(ylim=c(1.0, 1.2)) + facet_zoom(xlim=c(900, 1000), zoom.size=1)
    
  filename <- sprintf("-v2-%.2f-z1", c_level * 100)
  ggsave(paste(filebase, filename, ".png", sep=""),
         g1a,
         dpi=100, height=24, width=24, units="in", limitsize = FALSE)
  filename <- sprintf("-v2-%.2f-z2", c_level * 100)
  ggsave(paste(filebase, filename, ".png", sep=""),
         g1b,
         dpi=100, height=24, width=24, units="in", limitsize = FALSE)
}
