
######
### Create network from COW alliance data
#####

### Start with a single year

# Load additional libraries

library(cshapes)
library(readstata13)

# Load data

alliance<-read.csv("alliance_v4.1_by_dyad_yearly.csv")
head(alliance)

mids<-read.dta13("dyadic mid 3.1_may 2018.dta")
head(mids)

# Pick year (1955) and clean data

dat<-alliance[alliance$year==1955,]
names(dat)
dat<-dat[,c(2:5)]
head(dat)

# Create edge list

edge_list<-dat[,c(1,3)]
head(edge_list)

# Create Node list
a<-dat[,c(1,2)]
names(a)<-c("ccode","name")
b<-dat[,c(3,4)]
names(b)<-c("ccode","name")

node_list<-rbind(a,b)
node_list<-node_list[!duplicated(node_list),]
node_list

# Create network object

alliance_network<-graph_from_data_frame(d = edge_list, vertices = node_list, directed = F)

# Plot

ggraph(alliance_network,layout = "kk") +  geom_node_point(size=0.3) +
  geom_edge_link2(alpha=0.15,aes(edge_colour="blue"),show.legend = F)  + geom_node_text(label=node_list$name,size=2,repel = T)  + 
  ggtitle("Alliance Network in 1955")
ggsave("alliance_1955.pdf",scale=1.8)

### Loop for select years (to make a gif)

# Load additional packages

library(png)
library(magick)

# Set years

years<-c(1816,1850,1870,1895,1913,1922,1938,1955,1980,1991,2001,2011)

# Create loop for image

setwd("C:/Users/krosi/OneDrive/Documents/Thrive Work Stuff/Other Projects/Network Analysis/alliance images")
z<-c()
for (i in seq_along(1:NROW(years))){
  
  dat<-alliance[alliance$year==years[i],]
  dat<-dat[,c(2:5)]
  
  # Create edge list
  
  edge_list<-dat[,c(1,3)]

  # Create Node list
  a<-dat[,c(1,2)]
  names(a)<-c("ccode","name")
  b<-dat[,c(3,4)]
  names(b)<-c("ccode","name")
  
  node_list<-rbind(a,b)
  node_list<-node_list[!duplicated(node_list),]
  
  # Create network object
  
  alliance_network<-graph_from_data_frame(d = edge_list, vertices = node_list, directed = F)
  
  # Plot and save
  
  ggraph(alliance_network,layout = "kk") +  geom_node_point(size=0.3) +
    geom_edge_link(alpha=0.15,aes(edge_colour="blue"),show.legend = F)  + geom_node_text(label=node_list$name,size=2,repel = T)  +
    ggtitle(paste("Alliance Network in",years[i],sep=" "))
  ggsave(paste("alliance",years[i],".png",sep=""),scale=1.25)
  
  z[i]<-paste("alliance",years[i],".png",sep="")
}
print(z)

GIF.convert <- function(x, fps, output){
  image_read(x) %>%
    image_animate(fps = fps) %>%
    image_write(output)
}
GIF.convert(z,.25,"alliance.gif")
