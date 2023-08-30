###################NHL HEIGHTS###################################

###Install required packages
library(ggplot2)
install.packages("dplyr")
install.packages("circlize")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("cowplot")
install.packages("ggjoy")
install.packages("viridis")
install.packages("extrafont")

###Load packages
Packages <- c("dplyr", "ggplot2", "ggthemes", "cowplot", "ggjoy", "viridis", "extrafont")
lapply(Packages, library, character.only = TRUE)

###Import data
heights <- read.delim(file.choose(), header = T)

#Change year format into yyyy-yyyy 
heights$Year <- paste(substr(heights$Year, 0, 4), "-", 
                      as.numeric(substr(heights$Year,0,4)) + 1)

###Change year format from yyyy-yyyy to yyyy only
heights$Year <- paste(substr(heights$Year, 0, 4))

#################SCATTER PLOTS#########################################

###Make a variable for year intervals
heights$Year_Int <- as.numeric(as.character(heights$Year))

###Mean height per year
mean_height_df <- heights %>% group_by(Year_Int) %>%
  summarize(Ht = mean(Ht))


###Set color palettes
####this is a great website for choosing color palettes:http://earlglynn.github.io/RNotes/package/RColorBrewer/index.html
red <- RColorBrewer::brewer.pal(11, "RdYlBu")
blues <- RColorBrewer::brewer.pal(11, "RdBu")
black <- RColorBrewer::brewer.pal(11, "RdGy")


###Mean height basic plot
meanheight <- ggplot(mean_height_df, aes(x = Year_Int, y = Ht)) +
  geom_point(size = 4, color = red[9], alpha = 1) +
  stat_smooth(method = "lm", size = 1, color = red[11]) +
  ylab("Height, cm") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1917, 2018, 20), 
                     labels = seq(1917, 2018, 20)) +
  theme_minimal()

###View plot
meanheight

###better plot?
better <- ggplot(mean_height_df, aes(x = Year_Int, y = Ht)) +
  geom_point(size = 2, color = red[9], alpha = 0.75) +
  stat_smooth(method = "lm", size = 1, color = red[11]) +
  ylab("Height, cm") +
  xlab("Year") +
  labs(title = "NHL Player Mean Height", 
       subtitle = "Data: hockey-reference") +
  scale_x_continuous(breaks = seq(1917, 2018, 20), 
                     labels = seq(1917, 2018, 20)) +
  theme_grey()

###View plot
better

###Both mean height graphs to compare
cowplot::plot_grid(meanheight, better)

##all players, draft plot
ap <- ggplot(heights, aes(x = Year_Int, y = Ht))+
  geom_jitter(size = 3, color = red[2], alpha = .25, width = 0.25)+
  stat_smooth(method = "lm", size = 1, se = F, color = black[11])+
  labs(title = "NHL Player Height since 1917",
       subtitle = "Data: hockey-reference") +
  scale_x_continuous(breaks = seq(1918, 2018, 20), labels = seq(1918, 2018, 20))+
  theme_fivethirtyeight() 

ap

##all players, better plot
apb <- ggplot(heights, aes(x = Year_Int, y = Ht))+
  geom_jitter(size = 1, color = red[2], alpha = .08, width = .05)+
  stat_smooth(method = "lm", size = 1, se = F, color = black[11])+
  labs(title = "NHL Player Height since 1917",
       subtitle = "Data: hockey-reference") +
  scale_x_continuous(breaks = seq(1918, 2018, 20), labels = seq(1918, 2018, 20))+
  theme_fivethirtyeight() 

apb

###Both all-player graphs to compare
cowplot::plot_grid(ap, apb)


####Goalies--------------------------------------------

###Filter goalies
goalies <- filter(heights, Pos2 == "G")
goalies <- tbl_df(goalies)

###Plot goalies
g <- ggplot(goalies, aes(x = Year_Int, y = Ht)) +
  geom_jitter(size = 1, color = red[2], alpha = .15, width = .05) +
  geom_smooth(method = "lm", color = black[11], size = 1) +
  labs(title = "Goalies") +
  scale_x_continuous(breaks = seq(1918, 2018, 20), labels = seq(1918, 2018, 20)) +
  theme_fivethirtyeight()

g

####Forwards-------------------------------------------

###Filter forwards
forwards <- filter(heights, Pos2 == "F")
forwards <- tbl_df(forwards)

###Plot forwards
f <- ggplot(forwards, aes(x = Year_Int, y = Ht)) +
  geom_jitter(size = 1, color = red[2], alpha = .15, width = .05) +
  geom_smooth(method = "lm", color = black[11], size = 1) +
  labs(title = "Forwards") +
  scale_x_continuous(breaks = seq(1918, 2018, 20), labels = seq(1918, 2018, 20)) +
  theme_fivethirtyeight()

f

####Defensemen-------------------------------------------

###Filter forwards
defensemen <- filter(heights, Pos2 == "D")
defensemen <- tbl_df(defensemen)

###Plot
d <- ggplot(defensemen, aes(x = Year_Int, y = Ht)) +
  geom_jitter(size = 1, color = red[2], alpha = .15, width = .05) +
  geom_smooth(method = "lm", color = black[11], size = 1) +
  labs(title = "Defensemen") +
  scale_x_continuous(breaks = seq(1918, 2018, 20), labels = seq(1918, 2018, 20)) +
  theme_fivethirtyeight()

d


#####Combine plots 

alltogethernow <- cowplot::plot_grid(apb, f, d, g)
alltogethernow

##################JOY PLOT########################################################

###Mean height 20-year period
period_df <- mean_height_df %>%
  mutate(Year_Bin = cut(as.numeric(Year_Int), seq(0, 2017, 20))) %>%
  group_by(Year_Bin) %>%
  summarize(Ht = mean(Ht))

###plot joyplot
joyplot <- ggplot(heights,aes(x=Ht,y=cut(as.numeric(Year_Int), c(seq(0, 2017, 20), Inf)), fill=..x..)) +
  geom_joy_gradient(scale = 1.2, rel_min_height = 0.01, gradient_lwd = 1.) +    
  scale_fill_viridis(name = "in cm", option = "plasma") +
  scale_y_discrete(breaks=c("(1.9e+03,1.92e+03]","(1.92e+03,1.94e+03]", "(1.94e+03,1.96e+03]", "(1.96e+03,1.98e+03]","(1.98e+03,2e+03]",
                            "(2e+03,Inf]"), labels=c("1917-1923","1923-1943","1943-1963","1963-1983","1983-2003","2003-2017")) +
  labs(x="height (cm)",y="Period", title="NHL Player heights over the years",
       subtitle="Data: hockey-reference") +
  theme_fivethirtyeight() 

###View joyplot
joyplot

###Remove joyplot's scale
joyplot + theme(legend.position = "none") 

####################################################################################################################



