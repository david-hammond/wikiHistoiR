library(rvest)
library(magrittr)
library(tidyverse)
library(pbapply)
library(padr)
#read HTML code from the website
webpage <- read_html("https://en.wikipedia.org/wiki/Christianity")
links <- data.frame(links = webpage %>% html_nodes("a") %>% html_attr("href")) %>%
  filter(grepl("/wiki/", links)) %>% mutate(links = gsub("/wiki/", "", links)) %>%
  distinct()
links = links$links
links = split(links, as.factor(links))

get_people = function(wikipage){
  vcard = NULL
  print(wikipage)
  url = paste0("https://en.wikipedia.org/wiki/", wikipage)
  webpage <- try(read_html(url))
  if(class(webpage) != "try-error"){
    table <- webpage %>%
      html_nodes(xpath='//*[@class="infobox biography vcard"]') %>%
      html_table(header = F, fill = TRUE)
    if(length(table)>0){
      vcard <- table[[1]][,1:2]
      names(vcard) = c("descr", "info")
    }
  }
  return(vcard)
}
people = pblapply(links, get_people)
test = people[!sapply(people, is.null, simplify = T)]
test = bind_rows(test, .id = "person") %>%
  filter(X1 %in% c("Born", "Died")) %>% select(1:3)
names(test) = c("person", "descr", "info")


standard_date = function(date_string){
  dates = "\\d+-\\d+-\\d+"
  as.Date(str_extract(date_string, dates), "%Y-%m-%d")
}

test$date = standard_date(test$info)

character_date = function(date_string){
  tmp = date_string #not quite right yet
  all = NULL
  for (i in month.name){
    pos = grep(i, date_string)
    date_string[pos] =  gsub('[[:punct:] ]+',' ', date_string[pos])
    dates = paste( "\\d+", i, "\\d+")
    tmp = data.frame(pos = pos, date = as.Date(str_extract(date_string[pos] , dates), format = "%d %B %Y")) %>%
      filter(complete.cases(date))
    pos = setdiff(grep(i, date_string), tmp$pos) 
    dates = paste(i,  "\\d+", "\\d+")
    tmp = rbind(tmp, data.frame(pos = pos, date = as.Date(str_extract(date_string[pos], dates) , format = "%B %d %Y"))) %>%
      filter(complete.cases(date))
    all = rbind(all, tmp)
  }
  tmp = data.frame(date_string = date_string[all$pos], date = all$date, stringsAsFactors = F)
  tmp = data.frame(date_string = date_string) %>% left_join(tmp)
  return(tmp$date)
}
pos = is.na(test$date)
test$date[pos] = character_date(test$info[pos])

anno_domini_date = function(date_string){
  dates = c("AD \\d+")
  tmp = gsub("AD ", "", str_extract(date_string, dates))
  pos = is.na(tmp)
  dates = c("\\d+ AD")
  tmp[pos] = gsub(" AD", "", str_extract(date_string[pos], dates))
  pos = nchar(tmp) == 1 & !is.na(tmp)
  tmp[pos] = paste0("01-01-000", tmp[pos])
  pos = nchar(tmp) == 2 & !is.na(tmp)
  tmp[pos] = paste0("01-01-00", tmp[pos])
  pos = nchar(tmp) == 3 & !is.na(tmp)
  tmp[pos] = paste0("01-01-0", tmp[pos])
  tmp = as.Date(tmp, format = "%d-%m-%Y")
  return(tmp)
}
pos = is.na(test$date)
test$date[pos] = anno_domini_date(test$info[pos])

before_christ_date = function(date_string){
  dates = c("\\d+ BC")
  tmp = as.numeric(gsub(" BC", "", str_extract(date_string, dates)))
  tmp = lubridate::ymd("0000-01-01") - lubridate::years(tmp)
  tmp
}

pos = is.na(test$date)
test$date[pos] = before_christ_date(test$info[pos])

circa_date = function(date_string){
  dates = "c\\. \\d+" #not picking up everything
  tmp = str_extract(date_string,dates )
  tmp = gsub("c\\. ", "", tmp)
  pos = nchar(tmp) == 1 & !is.na(tmp)
  tmp[pos] = paste0("01-01-000", tmp[pos])
  pos = nchar(tmp) == 2 & !is.na(tmp)
  tmp[pos] = paste0("01-01-00", tmp[pos])
  pos = nchar(tmp) == 3 & !is.na(tmp)
  tmp[pos] = paste0("01-01-0", tmp[pos])
  pos = nchar(tmp) == 4 & !is.na(tmp)
  tmp[pos] = paste0("01-01-", tmp[pos])
  tmp = as.Date(tmp, format = "%d-%m-%Y")
  return(tmp)
}
pos = is.na(test$date)
test$date[pos] = circa_date(test$info[pos])
test = test %>% filter(complete.cases(date))
test$date[test$descr == "Died" ] = lubridate::today()
pos = is.na(test$date) & test$descr == "Born"
test = test %>% filter(!person %in% test$person[pos])
test2 = test %>% select(-info) %>% spread(descr, date) group_by(person) %>% pad() %>% ungroup()
min(test$date, na.rm=T)
max(test$date, na.rm=T)
