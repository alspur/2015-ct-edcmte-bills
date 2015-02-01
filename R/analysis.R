library(stringr)
library(dplyr)
library(tidyr)

# scrape table from ed commitee website on cga.gov 
url <- "http://www.cga.ct.gov/2015/eddata/cbr/ED.htm"
htmlCode = readLines(url)

# convert to dataframe
htmlCode <- data.frame(lines = htmlCode, stringsAsFactors = FALSE)

# extract bill numbers
bill.num <- htmlCode %>%
  filter(str_detect(lines, "PSB") | str_detect(lines, "PHB")) %>%
  separate(lines, sep = ">", into = c("a", "b", "c", "num"), extra = "merge")%>%
  separate(num, sep = "<", into = c("num", "drop"), extra = "merge") %>%
  select(num)

# extract bill titles and sponsors
bills <- htmlCode %>%
  filter(str_detect(lines, "<FONT FACE='ARIAL' SIZE='2'><B>AA")) %>%
  separate(lines, 
           into = c("html1", "title"), 
           sep = "<B>") %>%
  separate(title, 
           into = c("title", "sponsors"), 
           sep = "\\.</B><BR><BR>") %>%
  separate(sponsors, sep = "<BR>", 
           into = c("sponsor1", "sponsor2", "sponsor3"), 
           extra = "merge") %>%
  separate(sponsor3, sep = "<BR><BR>", 
           into = c("sponsor3", "et.al"), 
           extra = "drop") %>%
  separate(et.al, sep = "<", 
           into = c("et.al", "drop2"),
           extra = "merge") %>%
  mutate(num = bill.num$num,
         et.al = str_detect(et.al, "et al")) %>%
  select(num, title, sponsor1, sponsor2, sponsor3, et.al)

# convert bills from wide to long
long.bills <- bills %>%
  gather(type, sponsor, -num, -title)%>%
  separate(num, into = c("house.sen", "num"), sep = "\\-")%>%
  arrange(num) %>%
  filter(!is.na(sponsor))


# idenitfy bills with > 3 sponsors
many.sponsors <- long.bills %>%
  filter(type == "et.al")