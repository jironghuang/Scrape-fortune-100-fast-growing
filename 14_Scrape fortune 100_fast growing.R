#Use spell-checker algortihm to match the company in 2000 to Fortune
library("stringr")
library(XML)  #for parsing
library(rvest)

setwd("C:/Users/HUANGJ/Desktop/items/Mike APAC solutions")

###################################Scraping direclty from website########################
#functions in the main function
locate_start = function(x,pat){
  end = str_locate_all(pattern = pat,x)[[1]][1,1]
  return (end)
}

locate_end = function(x,pat){
  end = str_locate_all(pattern = pat,x)[[1]][1,2]
  return (end)
}

 
fortune100 = read.csv("13_Fortune 100 Fast Growing companies.csv",stringsAsFactors = FALSE)

# fortune100$link = paste
fortune100$lastchar = substring(fortune100$COMPANY, first = nchar(fortune100$COMPANY), last = nchar(fortune100$COMPANY))  
fortune100$COMPANY = ifelse(fortune100$lastchar == "-",substring(fortune100$COMPANY, first = 1, last = nchar(fortune100$COMPANY)-1)  ,
                             fortune100$COMPANY)

# http://fortune.com/fortune500/exxon-mobil/
fortune100$link = paste("http://fortune.com/100-fastest-growing-companies/",fortune100$COMPANY,"/",sep = "")

fortune100$descr = ""
fortune100$loc = ""
fortune100$cur_streak = ""
fortune100$yr_on_list = ""
fortune100$ceo = ""
fortune100$website = ""
fortune100$industry = ""
fortune100$sector = ""
fortune100$rev_past4qtr = ""
fortune100$netY_past4qtr = ""
fortune100$rev3yrgrowthrank = ""
fortune100$eps3yrgrowthrate = ""
fortune100$eps3yrgrowthrank = ""
fortune100$totret3yrrate = ""
fortune100$totret3yrrank = ""

for(i in 1:nrow(fortune100)){
  print(i);print(fortune100$COMPANY[i])
  html = readLines(fortune100$link[i]) 
  if(length(html)>1){
    a = as.data.frame(html); a$html = as.character(a$html)

    descr = locate_end(a$html[grep('<div class="columns small-12 company-info-card-desc"',a$html)],'<div class="columns small-12 company-info-card-desc"')
    loc = locate_end(a$html[grep('company-info-card-Location.1.0">',a$html)],'company-info-card-Location.1.0">')
    cur_streak = locate_end(a$html[grep('company-info-card-Current Streak.1.0">',a$html)],'company-info-card-Current Streak.1.0">')
    yr_list = locate_end(a$html[grep('company-info-card-Years on List.1.0">',a$html)],'company-info-card-Years on List.1.0">')
    ceo = locate_end(a$html[grep('company-info-card-CEO.1.0">',a$html)],'company-info-card-CEO.1.0">')
    web = locate_end(a$html[grep('company-info-card-Website.1.0">',a$html)],'company-info-card-Website.1.0">')
    ind = locate_end(a$html[grep('company-info-card-Industry.1.0">',a$html)],'company-info-card-Industry.1.0">')
    sec = locate_end(a$html[grep('company-info-card-Sector.1.0">',a$html)],'company-info-card-Sector.1.0">')  
    
    
    fortune100$descr[i] = substring(a$html[grep('<div class="columns small-12 company-info-card-desc"',a$html)], first = descr + 1, last = descr+300)  
    fortune100$descr[i] = substring(fortune100$descr[i], first = str_locate(fortune100$descr[i],"<p>")+3, last = -1 + str_locate(fortune100$descr[i],"</p>")) 
    
    fortune100$loc[i] = substring(a$html[grep('company-info-card-Location.1.0">',a$html)], first = loc + 1, last = loc+30)  
    fortune100$loc[i] = substring(fortune100$loc[i], first = 1, last = -1 + str_locate(fortune100$loc[i],"<")) 
    
    fortune100$cur_streak[i] = substring(a$html[grep('company-info-card-Current Streak.1.0">',a$html)], first = cur_streak + 1, last = cur_streak+30)  
    fortune100$cur_streak[i] = substring(fortune100$cur_streak[i], first = 1, last = -1 + str_locate(fortune100$cur_streak[i],"<")) 
    
    fortune100$yr_on_list[i] = substring(a$html[grep('company-info-card-Years on List.1.0">',a$html)], first = yr_list + 1, last = yr_list+30)  
    fortune100$yr_on_list[i] = substring(fortune100$yr_on_list[i], first = 1, last = -1 + str_locate(fortune100$yr_on_list[i],"<")) 
    
    fortune100$ceo[i] = substring(a$html[grep('company-info-card-CEO.1.0">',a$html)], first = ceo + 1, last = ceo+30)  
    fortune100$ceo[i] = substring(fortune100$ceo[i], first = 1, last = -1 + str_locate(fortune100$ceo[i],"<")) 
    
    fortune100$website[i] = substring(a$html[grep('company-info-card-Website.1.0">',a$html)], first = web + 1, last = web +70)  
    fortune100$website[i] = substring(fortune100$website[i], first = 1, last = -1 + str_locate(fortune100$website[i],"<")) 
    
    fortune100$industry[i] = substring(a$html[grep('company-info-card-Industry.1.0">',a$html)], first = ind + 1, last = ind+30)  
    fortune100$industry[i] = substring(fortune100$industry[i], first = 1, last = -1 + str_locate(fortune100$industry[i],"<"))  
    
    fortune100$sector[i] = substring(a$html[grep('company-info-card-Sector.1.0">',a$html)], first = sec + 1, last = sec+30)  
    fortune100$sector[i] = substring(fortune100$sector[i], first = 1, last = -1 + str_locate(fortune100$sector[i],"<"))  

#Financial data
    webpage = read_html(fortune100$link[i])
    tbls <- html_nodes(webpage, "table")
    tbls_ls <- webpage %>%
      html_nodes("table") %>%
      .[1:2] %>%
      html_table(fill = TRUE)
    
    fin = tbls_ls[[1]]; fin = fin[,1:2] ; names(fin) = c("indicator","num")
    fin_long = tbls_ls[[2]]; fin_long = fin_long[,-2]; names(fin_long) = c("indicator","num")
    fin = rbind(fin,fin_long)
    
    fortune100[i,which(names(fortune100) == "rev_past4qtr"):which(names(fortune100) == "totret3yrrank")] = fin$num[1:nrow(fin)]
  }
}

write.csv(fortune100,"14_fortune_scrape_dat.csv",row.names = FALSE)

#Summary of data

