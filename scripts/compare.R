library(ggthemes)
library(ggplot2)
library(tidyquant)

setwd("#setyourpathtofiles")
china <- "china.csv"
#fullpath <- file.path(filename)
dat <- read.csv(china)

canada <- "canada.csv"
#fullpath <- file.path(filename)
dat2 <- read.csv(canada)

dat$Date <- as.Date(dat$Date)
dat2$Date <- as.Date(dat2$Date)
colors <- c("China" = "red", "Canada" = "blue")

ggplot(dat, aes(x = dat$Date)) +
  geom_point(aes(y = Diff, color = "China"),size = 0.7) + geom_line(aes(y = Diff, color = "China"), size = 0.5) +
  geom_point(aes(y = Diff, color = "Canada"),size = 0.7) + geom_line(aes(y = Diff, color = "Canada"), size = 0.5) +
  labs(x = "Date",
       y = "New Cases Per Day",
       color = "Legend") +
  scale_color_manual(values = colors)+
  ggtitle("COVID19: Canada vs. China")+
  theme_tq()+
  theme(legend.position = c(0.8, 0.87))



ggplot(dat, aes(x = dat$Date)) +
  geom_point(aes(y = dat$Total, color = "China"),size = 0.7) + geom_line(aes(y = dat$Total, color = "China"), size = 0.5) +
  geom_point(aes(y = dat2$Total, color = "Canada"),size = 0.7) + geom_line(aes(y = dat2$Total, color = "Canada"), size = 0.5) +
  labs(x = "Date",
       y = "Total # Cases",
       color = "Legend") +
  scale_color_manual(values = colors)+
  ggtitle("COVID19: Canada vs. China")+
  theme_tq()+
  theme(legend.position = c(0.8, 0.8))

colors2 <- c("BC" = "#87B38D", "AB" = "#AA9597", "SK" = "#CC76A1", "MB" = "#DD9296", "ON" = "#F2B7C6", "QC" = "#F2D0A9", "NB" = "#F1E3D3", "NS" = "#99c1B9", "PE" = "#8E7DBE", "NL" = "#87F1FF") #"ship" = "#AFD2E9")
ggplot(dat2, aes(x = dat2$Date)) +
  geom_point(aes(y = dat2$British.Columbia, color = "BC"),size = 0.7) + geom_line(aes(y = dat2$British.Columbia, color = "BC"), size = 0.5) +
  geom_point(aes(y = dat2$Alberta, color = "AB"),size = 0.7) + geom_line(aes(y = dat2$Alberta, color = "AB"), size = 0.5) +
  geom_point(aes(y = dat2$Saskatchewan, color = "SK"),size = 0.7) + geom_line(aes(y = dat2$Saskatchewan, color = "SK"), size = 0.5) +
  geom_point(aes(y = dat2$Manitoba, color = "MB"),size = 0.7) + geom_line(aes(y = dat2$Manitoba, color = "MB"), size = 0.5) +
  geom_point(aes(y = dat2$Ontario, color = "ON"),size = 0.7) + geom_line(aes(y = dat2$Ontario, color = "ON"), size = 0.5) +
  geom_point(aes(y = dat2$Quebec, color = "QC"),size = 0.7) + geom_line(aes(y = dat2$Quebec, color = "QC"), size = 0.5) +
  geom_point(aes(y = dat2$New.Brunswick, color = "NB"),size = 0.7) + geom_line(aes(y = dat2$New.Brunswick, color = "NB"), size = 0.5) +
  geom_point(aes(y = dat2$Nova.Scotia, color = "NS"),size = 0.7) + geom_line(aes(y = dat2$Nova.Scotia, color = "NS"), size = 0.5) +
  geom_point(aes(y = dat2$Prince.Edward.Island, color = "PE"),size = 0.7) + geom_line(aes(y = dat2$Prince.Edward.Island, color = "PE"), size = 0.5) +
  geom_point(aes(y = dat2$Newfoundland.and.Labrador, color = "NL"),size = 0.7) + geom_line(aes(y = dat2$Newfoundland.and.Labrador, color = "NL"), size = 0.5) +
  #geom_point(aes(y = dat2$Grand.Princess, color = "ship"),size = 0.7) + geom_line(aes(y = dat2$Grand.Princess, color = "ship"), size = 0.5) +
  labs(x = "Date",
       y = "Total # Cases",
       color = "Legend") +
  scale_color_manual(values = colors2)+
  ggtitle("COVID19: Canada by Province")+
  theme_tq()+
  theme(legend.position = c(0.1, 0.7))
