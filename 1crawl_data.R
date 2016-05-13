library(scholar)
library(rvest)
library(ggplot2)
library(data.table)
library(XML)

# General info data
Areas <- list()
Colleagues <- list()
Articles <- list()
data_profiles <- NULL
data_network <- NULL
data_articles <- NULL

test <- function(x){
  try({
    print(log("a"))
  })
  return("a")
}

time <- proc.time()

#page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=cVeVZ1YAAAAJ") # Erdos 



crawl_network <- function(mid){
  print(paste("     Getting network info for id", mid))
  page <- read_html(paste("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=",mid,sep=''))
  
  parse_node <- function(node) {
    name <- node %>% html_nodes(css=".gsc_1usr_name a") %>% html_text()
    if(length(names) == 0){names <- c(NA)}
    cited <- node %>% html_nodes(css = ".gsc_1usr_cby") %>% html_text()
    if(length(cited) == 0){cited <- c(NA)}
    affil <- node %>% html_nodes(css = ".gsc_1usr_aff") %>% html_text()
    if(length(affil) == 0){affil <- c(NA)}else{affil <- as.character(affil)[1]}
    areas <- node %>% html_nodes(css = ".gsc_co_int") %>% html_text()
    areas <- as.character(areas)
    if(length(areas) == 0){areas <- NA}
    links <- node %>% html_nodes("a") %>% html_attr("href")
    links <- as.character(links)
    #links <- unique(links)
    links <- subset(links, substr(links, 1, 16) == "/citations?user=")
    id <- as.character(gsub("&hl","", strsplit(links[1], '=', fixed = T)[[1]][2])) # Unique author id
    #id <- gsub("/citations?user=", "", gsub("&amp;hl=en", "", links[1])) 
    Areas[[id]] <<- c(areas)
    Colleagues[[mid]] <<- c(Colleagues[[mid]], id)
    
    return(list(id = id,
                #links = links,
                name=name[1],
                cited=cited[1],
                affil = affil[1]))
  }
  data <- page %>% html_nodes(css = ".gsc_1usr_text") %>% lapply(parse_node) %>% rbindlist
  return(data)
}

crawl_authors <- function(N = 3, seed = "cVeVZ1YAAAAJ"){
  nine_years <- function(mid){
    page <- read_html(paste("http://scholar.google.com/citations?hl=en&user=",mid,"&pagesize=100&view_op=list_works",sep=''))
    years <- page %>% html_nodes(xpath = "//*/span[@class='gsc_g_t']") %>% html_text() %>% as.numeric()
    if(length(years) > 0){
      vals <- page %>% html_nodes(xpath = "//*/span[@class='gsc_g_al']") %>% html_text() %>% as.numeric()
      ind <- page %>% html_nodes(xpath = "//*/a[@class='gsc_g_a']") %>% xml_attr("style")
      ind <- as.numeric(sapply(ind, function(x){return(substr(x, nchar(x),nchar(x)))})) # Get indexes
      y <- data.frame(year = years, cites = 0)
      for(i in ind){
        y[i,2] <- vals[i]
      }
      z <- y[,2]
      names(z) <- as.character(y[,1])
      return(z)
    }else{
      y <- data.frame(year = c(2008:2016), cites = 0)
      z <- y[,2]
      names(z) <- as.character(y[,1])
      return(z)
    }
    
  }
  
  todo <- c(seed)
  done <- c()
  count <- 1
  
  while( (length(done) < N) | (length(todo) > 0) ){
    try({
      if((length(done) + length(todo)) < N){print(paste("Beginning author number",count))}
      else{print(paste("Beginning author number",count,"/",as.character(length(done) + length(todo))))}
      mid <- head(todo, n = 1)
      
      # We get details
      print(paste("     Getting profile info for id", mid))
      x <- get_profile(mid)
      Sys.sleep(rnorm(9,11))
      print(paste("     Getting publications info for id", mid))
      papers <- get_publications(mid)
      Sys.sleep(rnorm(9,11))
      t <- nine_years(mid)
      Sys.sleep(rnorm(9,11))
      temp <- (data.frame(id = c(mid),
                          name = c(x$name),
                          field = c(x$affiliation),
                          h_index = c(x$h_index),
                          i10_index = c(x$i10_index),
                          nb_articles = nrow(papers),
                          nb_journals = length(unique(papers$journal)),
                          y2008 = max(c(0, t["2008"]), na.rm = T),
                          y2009 = max(c(0, t["2009"]), na.rm = T),
                          y2010 = max(c(0, t["2010"]), na.rm = T),
                          y2011 = max(c(0, t["2011"]), na.rm = T),
                          y2012 = max(c(0, t["2012"]), na.rm = T),
                          y2013 = max(c(0, t["2013"]), na.rm = T),
                          y2014 = max(c(0, t["2014"]), na.rm = T),
                          y2015 = max(c(0, t["2015"]), na.rm = T),
                          y2016 = max(c(0, t["2016"]), na.rm = T),
                          oldest = min(papers$year, na.rm = TRUE),
                          newest = max(papers$year, na.rm = TRUE)))
      if(is.null(data_profiles)){
        data_profiles <<- temp
      }else{
        data_profiles <<- unique(rbind(data_profiles, temp))
      }
      
      temp <- unique(papers[,c("pubid","cid", "year","cites","title", "author","journal")])
      temp$id <- mid
      # temp2 <- sapply(as.character(temp$cid), function(x){return(update_articles(x, mid))})
      if(is.null(data_articles)){
        data_articles <<- temp
      }else{
        data_articles <<- unique(rbind(data_articles, temp))
      }
      
      if((length(done) + length(todo)) < N){
        # We get details that we see when crawling the network
        temp <- crawl_network(mid)
        Sys.sleep(rnorm(9,11))
        if(is.null(data_network)){
          data_network <<- temp
        }else{
          data_network <<- unique(rbind(data_network, temp))
        }
        todo <- unique(c(todo, temp$id))
      }
    })
    
    todo <- tail(todo, n = -1)
    todo <- unique(todo)
    
    done <- c(done, mid)
    count <- count + 1
  }
  return("All done.")
}


#erdos <- crawl_authors("9nD5N4AAAAAJ", N = 3)
erdos <- crawl_authors("cVeVZ1YAAAAJ", N = 5000)
# data <- data.frame(Coauthors, citations, affilation, links)

print(proc.time() - time)
rm(time)

write.csv2(data_articles, file = "C:/Users/Bouxtehouve/Desktop/network/data_articles.csv", row.names = F)
write.csv2(data_profiles, file = "C:/Users/Bouxtehouve/Desktop/network/data_profiles.csv", row.names = F)
write.csv2(data_network, file = "C:/Users/Bouxtehouve/Desktop/network/data_network.csv", row.names = F)

save(Areas, file = "C:/Users/Bouxtehouve/Desktop/network/Areas.Rdata")
save(Colleagues, file = "C:/Users/Bouxtehouve/Desktop/network/Colleagues.Rdata")

save.image(file = "C:/Users/Bouxtehouve/Desktop/network/info.Rdata")
