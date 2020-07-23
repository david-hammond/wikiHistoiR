standard_date = function(date_string){
  dates = "\\d+-\\d+-\\d+"
  as.Date(str_extract(date_string, dates), "%Y-%m-%d")
}

character_date = function(date_string){
  tmp = date_string #not quite right yet
  all = NULL
  for (i in month.name){
    pos = grep(i, date_string)
    date_string[pos] =  gsub('[[:punct:] ]+',' ', date_string[pos])
    dates = paste( "\\d+", i, "\\d{4}")
    tmp = data.frame(pos = pos, date = as.Date(str_extract(date_string[pos] , dates), format = "%d %B %Y")) %>%
      filter(complete.cases(date))
    pos = setdiff(grep(i, date_string), tmp$pos) 
    dates = paste(i,  "\\d+", "\\d{4}")
    tmp = rbind(tmp, data.frame(pos = pos, date = as.Date(str_extract(date_string[pos], dates) , format = "%B %d %Y"))) %>%
      filter(complete.cases(date))
    all = rbind(all, tmp)
  }
  duplicates = all %>% filter(pos %in% all$pos[which(duplicated(all$pos))])
  duplicates = duplicates %>% group_by(pos) %>% summarise(date = mean(date)) %>% ungroup()
  all = all %>% filter(!(pos %in% duplicates$pos)) %>% rbind(duplicates)
  all = data.frame(date_string = date_string[all$pos], date = all$date, stringsAsFactors = F)
  tmp = data.frame(date_string = date_string) %>% left_join(all)
  return(tmp$date)
}

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

before_christ_date = function(date_string){
  dates = c("\\d+ BC")
  tmp = as.numeric(gsub(" BC", "", str_extract(date_string, dates)))
  tmp = lubridate::ymd("0000-01-01") - lubridate::years(tmp)
  tmp
}

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

#First you need to get the links from url
# so need a function that does that

get_links = function(url){
  webpage <- read_html(url)
  links <- data.frame(links = webpage %>% html_nodes("a") %>% html_attr("href")) %>%
    filter(grepl("/wiki/", links)) %>% mutate(links = gsub("/wiki/", "", links)) %>%
    distinct()
  links = links$links
  links = split(links, as.factor(links))
  return(links)
}

# Then you need to get people

get_people = function(wikipage){
  vcard = NULL
  url = paste0("https://en.wikipedia.org/wiki/", wikipage)
  #print(url)
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



format_dates = function(formatted_people){
  formatted_people$date = standard_date(formatted_people$info)
  pos = is.na(formatted_people$date)
  formatted_people$date[pos] = character_date(formatted_people$info[pos])
  pos = is.na(formatted_people$date)
  formatted_people$date[pos] = anno_domini_date(formatted_people$info[pos])
  pos = is.na(formatted_people$date)
  formatted_people$date[pos] = before_christ_date(formatted_people$info[pos])
  pos = is.na(formatted_people$date)
  formatted_people$date[pos] = circa_date(formatted_people$info[pos])
  formatted_people = formatted_people %>% filter(!is.na(date)) %>% select(-info) %>% ungroup()
  return(formatted_people)
}

quality_assurance = function(formatted_people){
  formatted_people = formatted_people %>% tidyr::spread(descr, date)
  formatted_people$Died[is.na(formatted_people$Died)] = lubridate::today()
  formatted_people$Born[is.na(formatted_people$Born)] = formatted_people$Died[is.na(formatted_people$Born)] - lubridate::years(50)
  formatted_people$age = round((formatted_people$Died - formatted_people$Born)/365, 0)
  formatted_people = formatted_people %>% filter(age < 150) %>% select(-age) %>% gather(descr, date, -person)
  formatted_people$newyear = as.Date(paste0(lubridate::year(formatted_people$date), "-01-01"))
  formatted_people = formatted_people %>% select(-date) %>% group_by(person) %>% pad(interval = "year") %>% ungroup() 
  return(formatted_people)
}
