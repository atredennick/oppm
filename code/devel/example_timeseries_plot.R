##  Script to plot example climate and size time series
##  just to give people idea of the data and the conceptual
##  problem -- linking demography to weather variation

library(ggplot2)
library(ggthemes)
library(plyr)
library(gridExtra)

recr_data <- readRDS("../../data/fitting_dataframes/apriori_climate/recr_apriori_climate_Arizona.RDS")[["BORO"]]

g1 <- ggplot(recr_data, aes(x=year, y=pptLag))+
  geom_line()+
  geom_point(size=3)+
  ylab("Previous year\nprecipitation (mm)")+
  xlab("")+
  scale_x_continuous(breaks = unique(recr_data$year))+
  ggtitle("A")+
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "white", linetype = "dotted"),
        panel.background = element_rect("white"),
        axis.line.x = element_line("grey25"),
        axis.line.y = element_line("grey25"),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust = 1),
        plot.title = element_text(hjust = 0, size=16))

recr_data_mean <- ddply(recr_data, .(year), summarise,
                        mean_num_recruits = mean(R),
                        min_num_recruits = min(R),
                        max_num_recruits = max(R))
g2 <- ggplot(recr_data_mean, aes(x=year, y=mean_num_recruits))+
  geom_line()+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=min_num_recruits, ymax=max_num_recruits), width=0.2)+
  ylab("Average number\nof recruits")+
  xlab("Observation year")+
  scale_x_continuous(breaks = unique(recr_data$year))+
  ggtitle("B")+
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "white", linetype = "dotted"),
        panel.background = element_rect("white"),
        axis.line.x = element_line("grey25"),
        axis.line.y = element_line("grey25"),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust = 1),
        plot.title = element_text(hjust = 0, size=16))


png("../../figures/example_time_series_AZBORO.png", width = 5, height = 5, units = "in", res = 72)
gA <- ggplotGrob(g1)
gB <- ggplotGrob(g2)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
dev.off()

