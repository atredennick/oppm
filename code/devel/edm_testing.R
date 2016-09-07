##  MANIFOLD RECONSTRUNCTION TEST

rm(list=ls(all.names = TRUE))

library(plyr)
library(reshape2)
library(scatterplot3d)
library(rEDM)

make_lags <- function(df, col_id = "x"){
  tmp1 <- df
  tmp1$year <- tmp1$year+1
  colnames(tmp1)[2] <- paste0(col_id, "_t-1")
  tmp2 <- df
  tmp2$year <- tmp2$year+2
  colnames(tmp2)[2] <- paste0(col_id, "_t-2")
  out <- merge(df, tmp1, all = TRUE)
  out <- merge(out, tmp2, all = TRUE)
  return(out)
}

dat <- read.csv("../../data/NewMexico/speciesData/SPFL/quadratCover_full.csv")
cover_dat <- ddply(dat, .(year), summarise,
                   mean_cover.t = mean(totCover)/10000)
tmp1 <- cover_dat
tmp1$year <- tmp1$year+1
colnames(tmp1)[2] <- "mean_cover.tminus1"
tmp2 <- cover_dat
tmp2$year <- tmp2$year+2
colnames(tmp2)[2] <- "mean_cover.tminus2"

cover_dat <- merge(cover_dat, tmp1, all = TRUE)
cover_dat_all <- merge(cover_dat, tmp2, all = TRUE)

# Manifold plot
cover_dat <- cover_dat_all
scatterplot3d(cover_dat$mean_cover.t, cover_dat$mean_cover.tminus2,  cover_dat$mean_cover.tminus1, type="l", lwd=2, color="blue", col.grid = "white")
scatterplot3d(cover_dat$mean_cover.t, cover_dat$mean_cover.tminus2, 
              cover_dat$mean_cover.tminus1, pch=19, lwd=2, color="royalblue", angle = 45)


# Simplex projection -- univariate
ts_dat <- ddply(dat, .(year), summarise,
            mean_cover = mean(totCover)/10000)
ts <- ts_dat$mean_cover
simplex_output <- simplex(ts)
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")


simplex_output_tp <- simplex(ts, tp=1:10, E=4)
plot(simplex_output_tp$tp, simplex_output_tp$rho, type = "l", xlab = "Time to Prediction (tp)", 
     ylab = "Forecast Skill (rho)")


# Simplex projection -- multivariate (lags)
spfl <- read.csv("../../data/NewMexico/speciesData/SPFL/quadratCover_full.csv")
spfl_cover <- ddply(spfl, .(year), summarise,
                    mean_cover.t = mean(totCover)/10000)
spfl <- make_lags(spfl_cover)
colnames(spfl)[2] <- "x_t"

boer <- read.csv("../../data/NewMexico/speciesData/BOER/quadratCover_full.csv")
boer_cover <- ddply(boer, .(year), summarise,
                    mean_cover.t = mean(totCover)/10000)
boer <- make_lags(boer_cover, col_id = "y")
colnames(boer)[2] <- "y_t"

ts_block <- cbind(spfl, boer[2:ncol(boer)])

block_lnlp_output <- block_lnlp(ts_block, columns = c(1,2, 5), 
                                target_column = 1, stats_only = FALSE, 
                                first_column_time = TRUE)
observed <- block_lnlp_output[[1]]$model_output$obs
predicted <- block_lnlp_output[[1]]$model_output$pred


plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed", 
     ylab = "Predicted", main=paste("rho =", block_lnlp_output[[1]]$stats["rho"]))
abline(a = 0, b = 1, lty = 2, col = "blue")

