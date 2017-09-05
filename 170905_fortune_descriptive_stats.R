#Analysing the fortune 1000 data
#Chance to apply the tiydr principles
#If there's chance, analyse the 2015 and 2014 list
#key variables

#libraries
library(ggplot2)
library(tidyr)
library(dplyr)

#set working directory
setwd("C:/Users/HUANGJ/Desktop/items/Mike APAC solutions")

#read in data set
fortune = read.csv("14_fortune_scrape_dat.csv",stringsAsFactors = FALSE)
fortune$count = 1

#Revenue size
qplot(fortune$REVENUE_PAST_FOUR_QUARTERS._M)
summary(fortune$REVENUE_PAST_FOUR_QUARTERS._M)

#Sectors
# Draw plot
#Order the sectors
# fortune$sector = ordered(fortune$sector, levels = c("Financials","Industrials","Technology","Health Care","Business Services","Retailing", "Other"))

ggplot(fortune, aes(x=sector, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="dark green") + 
  theme(axis.text=element_text(size=12,face = "bold",color = "black"),axis.title=element_text(size=20,face="bold")) 

sec = as.data.frame(table(fortune$sector))

#Countries
comma = str_locate_all(fortune$loc,",")
first = function(x){
  start_pos = x[1]
  return(start_pos)
}

fortune$country_states = substring(fortune$loc,first = sapply(comma,first)+2, last = nchar(fortune$loc))
fortune$country_states = ifelse(fortune$country_states == "Bermuda","United Kingdom",fortune$country_states) 
ctry_states = as.data.frame(table(fortune$country_states))

fortune$country = ifelse(fortune$country_states != "China" & 
                         fortune$country_states != "Ireland"&
                         fortune$country_states != "Israel" & 
                         fortune$country_states != "Switzerland" &
                         fortune$country_states != "United Kingdom","US",fortune$country_states)

ctry = as.data.frame(table(fortune$country))

#Plotting financials by sector
#Revenue
fortune_secall = fortune;fortune_secall$sector = "All Sectors"
fortune_secall$sector = ordered(fortune_secall$sector,levels = c("Other","Retailing","Business Services","Health Care","Technology","Industrials","Financials","All Sectors"))

ggplot(data = rbind(fortune_secall,subset(fortune,fortune$sector == "Financials"|fortune$sector == "Industrials "|fortune$sector == "Technology"|fortune$sector == "Health Care")), 
       mapping = aes(x = sector, y = rev_past4qtr)) + geom_boxplot(fill = "orange") + 
  ylim(0,5000) + 
  theme(axis.text=element_text(size=12,face = "bold",color = "black"),axis.title=element_text(size=20,face="bold")) + coord_flip()

fortune%>%
  summarize(avg = mean(rev_past4qtr, na.rm = TRUE),
            med = median(rev_past4qtr, na.rm = TRUE))


ggplot(data = rbind(fortune_secall,subset(fortune,fortune$sector == "Financials"|fortune$sector == "Industrials "|fortune$sector == "Technology"|fortune$sector == "Health Care")), 
       mapping = aes(x = sector, y = netY_past4qtr)) + geom_boxplot(fill = "dark green") + 
  ylim(0,750) + 
  theme(axis.text=element_text(size=12,face = "bold",color = "black"),axis.title=element_text(size=20,face="bold")) + coord_flip()

fortune%>%
  summarize(avg = mean(netY_past4qtr, na.rm = TRUE),
            med = median(netY_past4qtr, na.rm = TRUE))


ggplot(data = rbind(fortune_secall,subset(fortune,fortune$sector == "Financials"|fortune$sector == "Industrials "|fortune$sector == "Technology"|fortune$sector == "Health Care")), 
       mapping = aes(x = sector, y = netY_past4qtr)) + geom_boxplot(fill = "dark green") + 
  ylim(0,750) + 
  theme(axis.text=element_text(size=12,face = "bold",color = "black"),axis.title=element_text(size=20,face="bold")) + coord_flip()


#Mean and median of revenue growth rate, eps and total return
rev_growth_avg = fortune%>%
  group_by(sector) %>% 
  summarize(avg_revgrowthrate = mean(rev3yrgrowthrate, na.rm = TRUE))

# fortune%>%
#   summarize(avg_revgrowthrate = mean(rev3yrgrowthrate, na.rm = TRUE))
mean(fortune$rev3yrgrowthrate,na.rm = TRUE)

eps_growth_avg = fortune%>%
  group_by(sector) %>% 
  summarize(avg_epsgrowthrate = mean(eps3yrgrowthrate, na.rm = TRUE))
mean(fortune$eps3yrgrowthrate,na.rm = TRUE)

ret_rate_avg = fortune%>%
  group_by(sector) %>% 
  summarize(avg_totretgrowthrate = mean(totret3yrrate, na.rm = TRUE))
mean(fortune$totret3yrrate,na.rm = TRUE)

list = fortune%>%
  group_by(sector) %>% 
  summarize(avg_curstreak = mean(cur_streak, na.rm = TRUE),
            avg_onlist = mean(yr_on_list, na.rm = TRUE))

fortune%>%
  summarize(avg_curstreak = mean(cur_streak, na.rm = TRUE),
            avg_onlist = mean(yr_on_list, na.rm = TRUE))


# fortune%>%
#   group_by(sector) %>% 
#   summarize(avg_totretgrowthrate = mean(totret3yrrate, na.rm = TRUE),
#             med_totretgrowthrate = median(totret3yrrate, na.rm = TRUE))



