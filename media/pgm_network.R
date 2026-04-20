library(visNetwork)

hc_filter <- function(don = hc,
                      who = "who",
                      when = "when",
                      where1 = "where1",
                      where2 = "where2",
                      wgt = "tags",
                      self = FALSE,
                      when_start = NA,
                      when_end = NA,
                      who_exc = NA,
                      who_inc = NA,
                      where1_exc = NA,
                      where1_inc = NA,
                      where2_exc = NA,
                      where2_inc = NA)

{                          
  
  df<-data.table(who = don[[who]],
                 when = don[[when]],
                 where1 = don[[where1]],
                 where2 = don[[where2]],
                 wgt = don[[wgt]])
  
  # Select time period
  if (is.na(when_start)==FALSE) { 
    df <- df[when >= as.Date(when_start), ]}
  if (is.na(when_end)==FALSE) { 
    df <- df[when <= as.Date(when_end), ]}
  # Select who
  if (is.na(who_exc)==FALSE) { 
    df <- df[!(who %in% who_exc), ]}
  if (is.na(who_inc)==FALSE) { 
    df <- df[(who %in% who_inc), ]}
  # Select where1
  if (is.na(where1_exc)==FALSE) { 
    df <- df[!(where1 %in% where1_exc), ]}
  if (is.na(where1_inc)==FALSE) { 
    df <- df[(where1 %in% where1_inc), ]}
  # Select where2
  if (is.na(where2_exc)==FALSE) { 
    df <- df[!(where2 %in% where2_exc), ]}
  if (is.na(where2_inc)==FALSE) { 
    df <- df[(where2 %in% where2_inc), ]}
  # eliminate internal links
  if (self==FALSE) { 
    df <- df[(where1 != where2), ]}
  return(df)
  
}

build_int <- function(don = don,       # a dataframe with columns i, j , Fij
                      i = "where1",
                      j = "where2",
                      Fij = "wgt",
                      s1 = 10,
                      s2 = 10,
                      n1 = 2,
                      n2 = 2,
                      k = 1)
  
{  
  df<-data.table(i=don[[i]],j=don[[j]],Fij=don[[Fij]])
  int <-df[,.(Fij=sum(Fij)),.(i,j)]
  int<-dcast(int,formula = i~j,fill = 0)
  mat<-as.matrix(int[,-1])
  row.names(mat)<-int$i
  mat<-mat[apply(mat,1,sum)>=s1,apply(mat,2,sum)>=s2 ]
  m0<-mat
  m0[m0<k]<-0
  m0[m0>=k]<-1
  mat<-mat[apply(m0,1,sum)>=n1,apply(m0,2,sum)>=n2 ]
  int<-reshape2::melt(mat)
  names(int) <-c("i","j","Fij")
  return(int)
}



rand_int <- function(int = int, # A table with columns i, j Fij
                     maxsize = 100000,
                     diag    = FALSE,
                     resid   = FALSE) {
  # Eliminate diagonal ?
  if (diag==FALSE) { 
    int <- int[as.character(int$i) != as.character(int$j), ]}
  
  # Compute model if size not too large
  if (dim(int)[1] < maxsize) {
    # Proceed to poisson regression model
    mod <- glm( formula = Fij ~ i + j,family = "poisson", data = int)
    
    # Add residuals if requested
    if(resid == TRUE)   { 
      # Add estimates
      int$Eij <- mod$fitted.values
      
      # Add absolute residuals
      int$Rabs_ij <- int$Fij-int$Eij
      
      # Add relative residuals
      int$Rrel_ij <- int$Fij/int$Eij
      
      # Add chi-square residuals
      int$Rchi_ij <-  (int$Rabs_ij)**2 / int$Eij
      int$Rchi_ij[int$Rabs_ij<0]<- -int$Rchi_ij[int$Rabs_ij<0]
    }
    
  } else { paste ("Table > 100000 -  \n 
                     modify maxsize =  parameter \n
                     if you are sure that your computer can do it !")}
  # Export results
  int$i<-as.character(int$i)
  int$j<-as.character(int$j)
  return(int)
  
}


geo_network<- function(don = don,
                       from = "i",
                       to = "j", 
                       size = "Fij",
                       minsize = 1,
                       maxsize = NA,
                       test = "Fij",
                       mintest = 1,
                       loops  = FALSE, 
                       title = "Network")
  
{
  int<-data.frame(i = as.character(don[,from]),
                  j = as.character(don[,to]),
                  size = don[,size],
                  test = don[,test]
  )
  if (is.na(minsize)==FALSE) {int =int[int$size >= minsize,]} 
  if (is.na(maxsize)==FALSE) {int =int[int$size <= maxsize,]} 
  if (is.na(mintest)==FALSE) {int =int[int$test >= mintest,]}
  
  nodes<-data.frame(code = unique(c(int$i,int$j)))
  nodes$code<-as.character(nodes$code)
  nodes$id<-1:length(nodes$code)
  nodes$label<-nodes$code
  nodes$color <-"gray"
  nodes$color[nodes$code %in% int$j]<-"red"
  
  
  # Adjust edge codes
  edges <- int %>% mutate(width = 5+20*size / max(size)) %>%
    left_join(nodes %>% select(i=code, from = id)) %>%  
    left_join(nodes %>% select(j=code, to = id )) 
  
  # compute nodesize
  toti<-int %>% group_by(i) %>% summarize(size =sum(size)) %>% select (code=i,size)
  totj<-int %>% group_by(j) %>% summarize(size =sum(size)) %>% select (code=j,size)
  tot<-rbind(toti,totj)
  tot<-unique(tot)
  tot$code<-as.factor(tot$code)
  nodes <- left_join(nodes,tot) %>% mutate(value = 1 +5*sqrt(size/max(size)))
  
  
  #sel_nodes <-nodes %>% filter(code %in% unique(c(sel_edges$i,sel_edges$j)))
  
  # eliminate loops
  
  if(loops == FALSE) {edges <- edges[edges$from < edges$to,]}
  
  net<- visNetwork(nodes, 
                   edges, 
                   main = title,
                   height = "1000px", 
                   width = "100%")   %>%   
    visNodes(scaling =list(min =20, max=60, 
                           label=list(min=20,max=80, 
                                      maxVisible = 20)))%>%
    visEdges(scaling = list(min=20,max=60))%>%
    visOptions(highlightNearest = TRUE,
               #               selectedBy = "group", 
               #               manipulation = TRUE,
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>% 
    visLegend() %>%
    visIgraphLayout(layout ="layout.fruchterman.reingold",smooth = TRUE) 
  
  net
  return(net)
} 