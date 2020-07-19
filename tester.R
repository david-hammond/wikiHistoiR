library(rvest)
library(magrittr)
library(tidyverse)
library(pbapply)
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
  WikipediaR::links(wikipage)
  if(class(webpage) != "try-error"){
    table <- webpage %>%
      html_nodes(xpath='//*[@class="infobox biography vcard"]') %>%
      html_table(header=F)
    if(length(table)>0){
      vcard <- table[[1]]
    }
  }
  return(vcard)
}

people = pblapply(links, get_people)

