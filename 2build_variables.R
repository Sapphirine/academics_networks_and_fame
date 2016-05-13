library(igraph)
library(gender)

data_articles2 <- NULL
data_network2 <- NULL
data_profiles2 <- NULL

if(is.null(data_articles2)){data_articles2 <- data_articles}else{data_articles <- data_articles2}
if(is.null(data_network2)){data_network2 <- data_network}else{data_network <- data_network2}
if(is.null(data_profiles2)){data_profiles2 <- data_profiles}else{data_profiles <- data_profiles2}


# Cleaning the data we got
data_profiles$id <- as.character(data_profiles$id)
data_profiles$name <- as.character(data_profiles$name)
data_profiles$field <- as.character(data_profiles$field)

data_profiles <- data_profiles[!duplicated(data_profiles$id),]

data_network <- data_network[!duplicated(data_network$id),]
data_network$cited <- sapply(data_network$cited, function(x){
  if(!grepl("Cited by ",x)){ # Some researchers have no Cited by data, looking at the cases they're mostly PhD and MsC students, can assume they have 0 cites
    return(0)
  }else{
    return(as.numeric(gsub("Cited by ","",x)))
  }})

data <- merge(data_profiles, data_network, all.x = F, all.y = F, sort = F) # 2250 persons
data2 <- data

write.csv2(data, file = "C:/Users/Bouxtehouve/Desktop/network/data.csv", row.names = F)
data <- read.csv2(file = "C:/Users/Bouxtehouve/Desktop/network/data.csv", header = T, stringsAsFactors = F,  encoding ="UTF-8")

data$firstname <- gsub("\\ .*","",data$name)
#data$gender <- (gender(data$firstname, method = "genderize"))$gender # We have to call an API, takes a few seconds
first <- read.csv2(file = "C:/Users/Bouxtehouve/Desktop/network/first.csv", header = T, stringsAsFactors = F)
middle <- read.csv2(file = "C:/Users/Bouxtehouve/Desktop/network/middle.csv", header = T, stringsAsFactors = F)
last <- read.csv2(file = "C:/Users/Bouxtehouve/Desktop/network/last.csv", header = T, stringsAsFactors = F)
# 
data$gender <- c(as.character(first$gender),as.character(middle$gender),as.character(last$gender))
rm(first, middle, last)

data$age <- data$newest - data$oldest
data$age[(data$oldest < 1900) | (data$newest < 1950)] <- NA

Articles <- list()
struct_data_articles <- function(){ # This function is not run because we do not use articles data in the end. Roughly 5 hours of my life wasted
  # Now for the articles
  # There is no unique article id for articles in Google Scholar, we create our own using a combination of title and authors ( should be good enough )
  # Also no equivalence relation for articles of coauthors: you can find an article written by A and B on A's page but not on B's
  data_articles$pubid <- as.character(data_articles$pubid)
  data_articles$cid <- as.character(data_articles$cid)
  data_articles$title <- as.character(data_articles$title)
  data_articles$author <- as.character(data_articles$author)
  data_articles$journal <- as.character(data_articles$journal)
  time <- proc.time()
  data_articles$uid <- factor(paste(data_articles$author, data_articles$title)) # 350k unique, to be taken carefully
  data_articles$uid <- as.numeric(data_articles$uid)
  N <- nrow(data_articles)
  q <- N %/% 16
  r <- N %% 16
  #Y We have to loop over the entire dataframe. R behaves very badly with loops so we have to divide the big loop
  for(i in 1:(q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (q +1):(2*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (2*q +1):(3*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (3*q +1):(4*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (4*q +1):(5*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (5*q +1):(6*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (6*q +1):(7*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (7*q +1):(8*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (8*q +1):(9*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (9*q +1):(10*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (10*q +1):(11*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (11*q +1):(12*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (12*q +1):(13*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (13*q +1):(14*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (14*q +1):(15*q)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }
  for(i in (15*q +1):(16*q + r)){
    u <- as.character(data_articles[i,"id"])
    Articles[[u]] <<- c(Articles[[u]], data_articles[i,"uid"])
  }# Takes about 45 sec
  print(proc.time() - time) # Takes 5 mins
  rm(time)
}

# Create the graph of connected researchers
base <- names(Colleagues)
noms <- data$id
edges <- c()
for(i in base){
  for(j in Colleagues[[i]]){
    if(all(c(i,j) %in% noms)){edges <- c(edges,i,j)}
  }
}
erdos <- make_undirected_graph(edges)

# plot(erdos, vertex.label = NA, vertex.size = 1) # Entire graph

D <- distances(erdos)
g_dist_erdos <- D["cVeVZ1YAAAAJ",] # Erdos number
g_betw <- betweenness(erdos, directed = F) # Betweenness
g_deg <- degree(erdos, loops = F)
g_close <- closeness(erdos)
g_eigen <- eigen_centrality(erdos)$vector
g_page <- page_rank(erdos, damping = 0.9)$vector
g_n2 <- ego_size(erdos, 2)
g_bona <- bonpow(erdos)

d_dist_erdos <- data.frame(id = names(g_dist_erdos), dist_erdos = as.numeric(g_dist_erdos))
d_betw <- data.frame(id = names(g_betw), betw = as.numeric(g_betw))
d_deg <- data.frame(id = names(g_deg), deg = as.numeric(g_deg))
d_close <- data.frame(id = names(g_close), close = as.numeric(g_close))
d_eigen <- data.frame(id = names(g_eigen), eigen = as.numeric(g_eigen))
d_page <- data.frame(id = names(g_page), page = as.numeric(g_page))
d_n2 <- data.frame(id = names(g_page), n2 = as.numeric(g_n2))
d_bona <- data.frame(id = names(g_bona), bona = as.numeric(g_bona))

data <- merge(data, d_dist_erdos, all = F, sort = F)
data <- merge(data, d_betw, all = F, sort = F)
data <- merge(data, d_deg, all = F, sort = F)
data <- merge(data, d_close, all = F, sort = F)
data <- merge(data, d_eigen, all = F, sort = F)
data <- merge(data, d_page, all = F, sort = F)
data <- merge(data, d_n2, all = F, sort = F)
data <- merge(data, d_bona, all = F, sort = F)

temp <- make_ego_graph(erdos, 1)
g_max_d1 <- c()
t <- names(g_page)
for(i in 1:length(temp)){
  z <- V(temp[[i]])$name
  x <- setdiff(z,c(t[i]))
  z <- subset(data, id %in% x)
  g_max_d1[i] <- max(z$h_index)
}
d_max_d1 <- data.frame(id = as.character(t), max_d1 = as.numeric(g_max_d1))
rm(x,z,temp)

temp <- make_ego_graph(erdos, 2)
g_max_d2 <- c()
for(i in 1:length(temp)){
  z <- V(temp[[i]])$name
  x <- setdiff(z,c(t[i]))
  z <- subset(data, id %in% x)
  g_max_d2[i] <- max(z$h_index)
}
d_max_d2 <- data.frame(id = as.character(t), max_d2 = as.numeric(g_max_d2))
rm(x,z,t,temp)

data <- merge(data, d_max_d1, all = F, sort = F)
data <- merge(data, d_max_d2, all = F, sort = F)

rm(g_dist_erdos, d_dist_erdos,
   g_betw, d_betw,
   g_deg, d_deg,
   g_close, d_close,
   g_eigen, d_eigen,
   g_page, d_page,
   g_n2, d_n2,
   g_bona, d_bona,
   g_max_d1, d_max_d1,
   g_max_d2, d_max_d2,
   D)

g_louvain <- cluster_louvain(erdos)
g_walk <- cluster_walktrap(erdos, steps = 4)
g_label <- cluster_label_prop(erdos)
g_spin <- cluster_spinglass(erdos, spins = 20)

m_louvain <- modularity(erdos, membership(g_louvain))
m_walk <- modularity(erdos, membership(g_walk))
m_label <- modularity(erdos, membership(g_label))
m_spin <- modularity(erdos, membership(g_spin))

d_louvain <- membership(g_louvain)
d_walk <- membership(g_walk)
d_label <- membership(g_label)
d_spin <- membership(g_spin)

d_louvain <- data.frame(id = names(d_louvain), c_louvain = as.numeric(d_louvain))
d_walk <- data.frame(id = names(d_walk), c_walk = as.numeric(d_walk))
d_label <- data.frame(id = names(d_label), c_label = as.numeric(d_label))
d_spin <- data.frame(id = names(d_spin), c_spin = as.numeric(d_spin))

#data <- subset(data, id %in% names(d_louvain))

data <- merge(data, d_louvain, all = F, sort = F)
data <- merge(data, d_walk, all = F, sort = F)
data <- merge(data, d_label, all = F, sort = F)
data <- merge(data, d_spin, all = F, sort = F)

rm(g_louvain, m_louvain, d_louvain,
   g_walk, m_walk, d_walk,
   g_label, m_label, d_label,
   g_spin, m_spin, d_spin)


