############################################################
#
# DATA ANALYSIS TEXTBOOK
# INFERENCE
# ILLUSTRATION STUDY
# S&P500 index 
# data covers 11 years starting with August 25 2006 and ending with August 26 2016. It includes 2,519 days.
#
############################################################  
# WHAT THIS CODES DOES:

# Loads the csv file 
# Clean the dataset
# Generate new variables
# Generate samples by resampling to derive confidence intervals
# Generate samples by bootstraping to derive confidence intervals


# CLEAR MEMORY
rm(list=ls())

library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(DataCombine)
library(broom)



# set the path
dir <- "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
#dir <- "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

# Location folders
data_in <- paste0(dir,"da_data_repo/sandp500/clean/")
data_out <- paste0(dir,"da_case_studies/ch05-stock-market-loss-generalize/")
output <- paste0(dir,"da_case_studies/ch05-stock-market-loss-generalize/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))

# LOAD  DATA
sp500 <- read.csv(paste(data_in,"SP500_2006_16_data.csv",sep=""), na.strings = "#N/A")
sp500 <- subset(sp500, VALUE != "NA")


# CREATE PERCENT RETURN; DataCombine::change
sp500 <- change(sp500, Var = 'VALUE', slideBy = -1,  type = "percent", NewVar = 'pct_return')

# CREATE DATE VARIABLE
sp500$date <- as.Date(sp500$DATE, format = "%Y-%m-%d") 
sp500$year <- format(sp500$date, "%Y")
sp500$month <- format(sp500$date, "%m")
sp500$year <- as.numeric(sp500$year)
sp500$month <- as.numeric(sp500$month)
sp500$yearmonth <- sp500$year*100 + sp500$month

# Distribution 

# Figure 5.1
returns_histogram <-ggplot(sp500,aes(pct_return))+
  geom_histogram(binwidth = 0.25, fill = color[1], color = color[4], alpha = 0.8)+
  geom_vline(xintercept = -5, size = 2, color=color[2])+
  labs(x = "Percent daily return", y = "Frequency") +
  coord_cartesian(xlim = c(-10, 10), ylim = c(0, 400)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bg() 
returns_histogram
ggsave(paste0(output, "returns_histogram_R.png"), width = mywidth_large, height = myheight_large, 
       units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "returns_histogram_R.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 20,
         fallback_resolution = 1200)
print(returns_histogram)
dev.off()


# Figure 5.2 prep

# Create 10 000 samples, with 450 and 900 observations in each sample, taken from sp500$pct_return

# in every sample: for each observation, check if it is a loss of 5% or more. Then calculate the percentage of observations out of 450 or 900
# where the loss exceeds 5%. E.g. for the 233rd sample, 9 out of 900 obs had larger than 5% losses. 

# remove first row as it has NA in pct_return
pct_return <- sp500$pct_return[!is.na(sp500$pct_return)]



# write function that for a specified number of samples: draws a specified number of observations from a vector, calculates the percentage of obs with greater than 5% losses 
# 3 inputs: 'vector' is a vector of the source data, in this case pct_return. 'n_samples' is the number of samples we want to use. 
# 'n_obs' is the number of observations in each sample
# output is a vector
create_samples <- function(vector, n_samples, n_obs) {
  samples_pcloss <- c()
  for (i in 1:n_samples){
    
    single_sample <- sample(vector,n_obs, replace = FALSE)
    samples_pcloss[i] <- sum(single_sample < -5)/n_obs*100 
   
  }
  samples_pcloss
}

set.seed(123)

# Figure 5.2, 5.3, 5.4 input
nobs_900 <- create_samples(pct_return, 10000, 900)
nobs_450 <- create_samples(pct_return, 10000, 450)
nobs_df <- as.data.frame(cbind(nobs_450, nobs_900))

error <- qnorm(0.975)*sd(nobs_df$nobs_900)/sqrt(length(nobs_df$nobs_900))
left <- mean(nobs_df$nobs_900)-error
right <- mean(nobs_df$nobs_900)+error


# Figure 5.2

# TODO
# Check: Warning messages:
#1: Removed 4 rows containing non-finite values (stat_bin). 
#2: Removed 2 rows containing missing values (geom_bar). 

resample900<-    ggplot(nobs_df,aes(nobs_900)) +
    geom_histogram(binwidth = 0.11, fill = color[1], color = color[4], alpha = 0.8) +
    labs(x = "Daily return with losses of 5% or more", y = "Frequency") +
      scale_x_continuous(limits = c(0,1.2), breaks= c(seq(0,1.2, by = 0.2))) +
      geom_vline(aes(xintercept = mean(nobs_900)), color =color[2], linetype = 'dashed',size = 2) +
      scale_y_continuous(expand = c(0, 50)) +
      geom_segment(aes(x = 0.8, y = 2000, xend = 0.53, yend = 2000), arrow = arrow(length = unit(0.2, "cm")))+
      annotate("text", x = 0.85, y = 2000, label = "Mean")+
    theme_bg()
    resample900

    ggsave(paste0(output, "resample900_R.png"), width = mywidth_large, height = myheight_large, units = "cm", dpi = 1200)
    cairo_ps(filename = paste0(output, "resample900_R.eps"),
             width = mywidth_large, height = myheight_large, pointsize = 20,
             fallback_resolution = 1200)
    print(resample900)
    dev.off()
    
    
    # Figure 5.3
    # TODO
    # legend box not needed, but have larger fonts on axes, maybe annotations
    
    resample_densities<-ggplot(nobs_df,aes(nobs_900, color = 'n900'))+
        #geom_density(bw=0.4, colour="darkgreen",linetype="dotted")+
        geom_density( bw = 0.45,size = 1.5)+
        geom_density(aes(nobs_450, color = "n450"), bw=0.45,linetype="dashed", size = 1.5)+
        labs(x="Percent of days with losses over 5%", y="Density")+
        geom_hline(yintercept=0, colour="white", size=2) +
        geom_vline(xintercept = 0,colour="white", size = 1.5)+
        geom_vline(xintercept = max(nobs_450), colour="white", size = 1.5)+
        geom_vline(xintercept = 0.5,colour="black", size = 1)+
        scale_color_manual(name = "Observations per sample", values = c(n900= color[1], n450 = color[2]), labels = c("n = 450","n = 900"))+
        theme_bg()+
      theme(legend.position=c(0.8,0.5), legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
        geom_segment(aes(x = 0.9, y = 0.8, xend = 0.6, yend = 0.8), arrow = arrow(length = unit(0.2, "cm")))+
      annotate("text", x = 1.1, y = 0.8, label = "Larger sample")+
    geom_segment(aes(x = 0.9, y = 0.7, xend = 0.7, yend = 0.7), arrow = arrow(length = unit(0.2, "cm")))+
      annotate("text", x = 1.1, y = 0.7, label = "Smaller sample")+      
        resample_densities
        
        ggsave(paste0(output, "resample_densities_R.png"), width = mywidth_large, height = myheight_large, 
               units = "cm", dpi = 1200)
        cairo_ps(filename = paste0(output, "resample_densities_R.eps"),
                 width = mywidth_large, height = myheight_large, pointsize = 20,
                 fallback_resolution = 1200)
        print(resample_densities)
        dev.off()
        






####################################
#BOOTSRTAP SAMPLES
set.seed(573164)
M <- 10000

Results <- matrix(rep(0,(M*10)),nrow=M,ncol=10)

for (i in 1:M){
  bsample <- sample(sp500$pct_return,size=dim(sp500)[1], replace = TRUE)
  
  for (j in 1:10){
    loss <- as.numeric(bsample<(-j))*100
    Results[i,j] <- mean(loss, na.rm=T)
  }
}

Results <- as.data.frame(Results)
colnames(Results) <- c("loss1","loss2","loss3","loss4","loss5","loss6",
                       "loss7","loss8","loss9","loss10") 
  

# Figure 5.5
bootstrap<- ggplot(Results,aes(loss5))+
    geom_histogram(binwidth = 0.04, fill = color[1], color = color[4], alpha = 0.8)+
    scale_y_continuous(limits = c(0,1200), breaks = seq(0,1200,200)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
    labs(x = "Percent of days with losses of 5% or more", y = "Frequency")+
  theme_bg()
bootstrap
  ggsave(paste0(output, "bootstrap_R.png"), width = mywidth_large, height = myheight_large, units = "cm", dpi = 1200)
  cairo_ps(filename = paste0(output, "bootstrap_R.eps"),
           width = mywidth_large, height = myheight_large, pointsize = 20,
           fallback_resolution = 1200)
  print(bootstrap)
  dev.off()
  
  # TODO
  # check warning
  # : Removed 13 rows containing non-finite values (stat_bin). 
  # 2: Removed 2 rows containing missing values (geom_bar)

