library(rvest)
library(magrittr)
library(tidyverse)
library(pbapply)
library(padr)
source("libs.R")
get_bios = function(url){
  links = get_links(url)
  people = lapply(links[1:50], get_people)
  formatted_people = people[!sapply(people, is.null, simplify = T)]
  formatted_people = bind_rows(formatted_people, .id = "person") %>%
    filter(descr %in% c("Born", "Died")) %>% select(1:3)
  formatted_people = format_dates(formatted_people)
  formatted_people = quality_assurance(formatted_people)
  subject = gsub("https://en.wikipedia.org/wiki/", "", url)
  formatted_people = formatted_people %>% mutate(subject = subject) %>%
    select(subject, person, newyear)
  return(formatted_people)
}

url = c("https://en.wikipedia.org/wiki/Islam", "https://en.wikipedia.org/wiki/Judaism",  "https://en.wikipedia.org/wiki/Christianity")
people = pblapply(url, get_bios)
saveRDS(people, "people.rds")
people = bind_rows(people)
timeline = people %>% group_by(subject, newyear) %>% summarise(num = n()) %>%
  ungroup()
ggplot(timeline, aes(x = newyear, y = num, fill = subject)) + geom_area(stat = "smooth")
