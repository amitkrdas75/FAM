FAM <- function(filepath, filename, alpha)
{
  library(igraph)
  library(dplyr)

  filename1 <- paste0(filepath, filename, ".csv")
  mydata <- read.table(filename1, header=TRUE, sep=",")

  cord<-abs(cor(mydata[-ncol(mydata)]))
  for (i in 1:ncol(cord)) {cord[i,i]=0}

  cord1 <- apply(cord,c(1,2),myfunc<- function(x){return(ifelse(x>=alpha,1,0))})
  s<-colSums(cord1)
  z<-which(s==0)
  z1<-which(s>=3)
  cordadj<-cord1
  if(length(z)>0) {cordadj<-cord1[-z,-z]}
  g1 <- graph.adjacency(cordadj,mode="undirected", weighted = TRUE, diag = FALSE)

  alpha<- 0.9
  cord2 <- apply(cord,c(1,2),myfunc<- function(x){return(ifelse(x>=alpha,1,0))})
  s<-colSums(cord2)
  z<-which(s==0)
  cordadj<-cord2
  if(length(z)>0) {cordadj<-cord2[-z,-z]}

  g2 <- graph.adjacency(cordadj,mode="undirected", weighted = TRUE, diag = FALSE)


  mainDir <- getwd()

  if (!file.exists("graphs/")){
    dir.create(file.path(mainDir, "graphs/"))
  }

  filename1 <- paste0("graphs/", filename, ".png")
  V(g1)$color <- ifelse(V(g1)$name %in% V(g2)$name, ifelse(V(g1)$name %in% names(z1), "red", "darkgreen"), ifelse(V(g1)$name %in% names(z1), "red", "blue"))
  E(g1)$color <- "red"
  png(filename=filename1)

  plot(g1, vertex.label.color="black")
  dev.off()
}
