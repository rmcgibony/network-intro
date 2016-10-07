library(data.table)
library(igraph)
library(visNetwork)

setwd('C:/Users/rmcgibony/Downloads/fec/')

# Read data
bike <- fread('C:/Users/rmcgibony/Downloads/2016-Q1-Trips-History-Data.csv', header=TRUE, verbose=TRUE)
setnames(bike, c('duration', 'start_date', 'end_date', 'start_station', 'start_name', 'end_station', 'end_name', 'bike_id', 'member_type'))
bike[, start_date2:=as.POSIXct(start_date, "%m")]

trips <- bike[, .(start_station, end_station, bike_id, duration)]

stations <- data.table()

# Read committee data
comm <- fread('cm.txt', sep='|', header=FALSE, verbose=FALSE,
            select=c('V1', 'V2'))
setnames(comm, c('comm_id', 'comm_name'))
setkey(comm, comm_id)


# Read candidate data
cand <- fread('cn.txt', sep='|', header=FALSE, verbose=TRUE,
              select=c('V1', 'V2', 'V3', 'V5', 'V6', 'V8'))
setnames(cand, c('cand_id', 'cand_name', 'party', 'state', 'office', 'status'))
setkey(cand, cand_id)

# Limit to Senate candidates
cand <- cand[office=='S',]

# Collapse all third-party candidates into a single group
cand[!party %in% c('DEM','REP'), party:='OTH']

# Read contribution data
# contr_header <- read.csv('pas2_header_file.csv', header=TRUE)
# contr_explore <- fread('itpas2.txt', sep='|', header=FALSE)
# setnames(contr_explore, names(contr_header))

contr_all <- fread('itpas2.txt', sep='|', header=FALSE, 
                select=c('V1', 'V17', 'V15'), na.strings="")
setnames(contr_all, c('comm_id', 'cand_id', 'amount'))

# Aggregate contributions to distinct committee-candidate pairs
contr <- contr_all[, .(amount=sum(amount), count=.N), by=.(comm_id, cand_id)]

# Ignore cases where donations/amendments totaled zero or less
contr[amount<=0, .N]
contr <- contr[amount>0,]

# Ignore cases with no candidate specified
contr[is.na(cand_id), .N]
contr <- contr[!is.na(cand_id),]

# Limit contributions to our defined set of candidates
contr <- contr[cand_id %in% cand[, cand_id]]

committee_counts <- contr[, .(count=.N), by=comm_id]
cand_totals <- contr[, .(total=sum(amount)), by=cand_id]
setkey(cand_totals, cand_id)


# Exclude committees that gave to at least X candidates, to reduce total relationships?
contr <- contr[comm_id %in% committee_counts[count<=10, comm_id]]

# Add candidate count 

# Add total contribution totals to candidate attributes
cand <- cand_totals[cand]
cand <- cand[!is.na(total),]

# Include only candidates who received at least $100K in contributions
# contr <- contr[cand_id %in% cand_totals[total>=1e5, cand_id]]


# Get list of committees and candidates that appear in our contribution set
contr_list <- c(contr[, unique(cand_id)], contr[, unique(comm_id)])

# Combine candidates and committees for our node list
nodes <- rbindlist(list(cand[, .(id=cand_id, type=TRUE, cand_name, party, state, status, total)],
                        comm[, .(id=comm_id, type=FALSE)]),
                   fill=TRUE)
nodes <- nodes[id %in% contr_list]

ig <- graph.data.frame(contr, vertices=nodes, directed=TRUE)
summary(ig)

write.graph(ig, file='contributions.GraphML', format="graphml")

no.clusters(ig)

cand_net <- bipartite.projection(ig, which='true')

write.graph(cand_net, file='candidates.GraphML', format="graphml")

# cl <- clusters(ig)

# ig2 <- bipartite.projection(ig)

cl <- clusters(ig)[[1]]

cl <- data.table(id=names(cl),cl)
cl[, cl:=as.numeric(cl)]
setkey(cl,cl)
cl[, size:=.N, by=cl]


main_cl <- cl[size==max(size), unique(cl)]

main_cl <- induced_subgraph(ig, vids=cl[size==max(size),id])

cand_net <- bipartite.projection(main_cl, which='true')

quantile(V(cand_net)$total, na.rm=TRUE)

# Limit to candidates with at least $100K in contributions
# cand_net <- delete.vertices(cand_net, V(cand_net)[total<5e6])

V(cand_net)$label <- V(cand_net)$cand_name
V(cand_net)$color <- V(cand_net)$party # assign the party attribute as the vertex color
V(cand_net)$color <- gsub("DEM","blue", V(cand_net)$color) 
V(cand_net)$color <- gsub("REP","red", V(cand_net)$color) 
V(cand_net)$color <- gsub("OTH","gray", V(cand_net)$color) 

V(cand_net)$size <- log(V(cand_net)$total)

plot(cand_net, vertex.shape="circle", vertex.label=NA, edge.arrow.size=0, layout=layout.fruchterman.reingold(cand_net) )

# Plot network
visnet <- toVisNetworkData(cand_net)
visNetwork(nodes=visnet$nodes, edges=visnet$edges) %>% visPhysics(solver="barnesHut", timestep=0.2) %>% 
  visNodes(color = visnet$color, size=visnet$size, label=visnet$cand_name) %>% visEdges(color = "#C3C3C3", width=1)



library("networkD3")


# Remove all nodes with fewer than 50 edges
# deg <- degree(gs, mode = "out")
# idx <- names(which(deg > 50))
# gn <- induced.subgraph(gs, idx)

# Extract into data frame and plot
gd <- get.data.frame(cand_net, what = "edges")
simpleNetwork(gd, fontSize = 12)



# Get node attributes
cand.degrees <- degree(cand_net, v=V(cand_net), mode="all")
cand.betweenness <- betweenness(cand_net, v=V(cand_net))
cand.pagerank <- page.rank(cand_net, v=V(cand_net))$vector

temp <- data.frame(cand_id=names(cand.degrees), degree=cand.degrees, row.names=NULL)
temp <- data.table(temp)

temp2 <- data.frame(cand_id=names(cand.betweenness), btw=cand.betweenness, row.names=NULL)
temp2 <- data.table(temp2)

temp3 <- data.frame(cand_id=names(cand.pagerank), pr=cand.pagerank, row.names=NULL)
temp3 <- data.table(temp3)

setkey(temp, cand_id)
setkey(temp2, cand_id)
setkey(temp3, cand_id)

cand_attr <- temp[temp2[temp3]]
cand_attr <- cand[cand_attr]
