library(data.table)
library(igraph)
library(visNetwork)

setwd('C:/Files/code/graph-fraud-detection')

# Data links are from http://www.fec.gov/finance/disclosure/ftpdet.shtml

# Download and unzip FEC data
download.file("ftp://ftp.fec.gov/FEC/2014/cm14.zip", destfile="data/cm14.zip")
download.file("ftp://ftp.fec.gov/FEC/2014/cn14.zip", destfile="data/cn14.zip")
download.file("ftp://ftp.fec.gov/FEC/2014/pas214.zip", destfile="data/pas214.zip")

unzip('data/cm14.zip', exdir='data')
unzip('data/cn14.zip', exdir='data')
unzip('data/pas214.zip', exdir='data')

# Read committee data
comm <- fread('data/cm.txt', sep='|', header=FALSE, verbose=FALSE,
            select=c('V1', 'V2'))
setnames(comm, c('comm_id', 'comm_name'))
setkey(comm, comm_id)
comm

# Read candidate data
cand_full <- fread('data/cn.txt', sep='|', header=FALSE, verbose=FALSE,
              select=c('V1', 'V2', 'V3', 'V5', 'V6', 'V8'))
setnames(cand_full, c('cand_id', 'cand_name', 'party', 'state', 'office', 'status'))
setkey(cand_full, cand_id)
cand_full

cand_full[, table(office)]

# Limit to Senate candidates
cand <- cand_full[office=='S',]

mytable <- cand[, table(party)]
sort(mytable, decreasing=TRUE)

# Collapse all third-party candidates into a single group
cand[!party %in% c('DEM','REP'), party:='OTH']

# Read contribution data
# contr_header <- read.csv('pas2_header_file.csv', header=TRUE)
# contr_explore <- fread('itpas2.txt', sep='|', header=FALSE)
# setnames(contr_explore, names(contr_header))

contr_all <- fread('data/itpas2.txt', sep='|', header=FALSE, 
                select=c('V1', 'V15', 'V17'), na.strings="")
setnames(contr_all, c('comm_id', 'amount', 'cand_id'))
setcolorder(contr_all, c('comm_id', 'cand_id', 'amount'))
contr_all

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

committee_counts <- contr[, .(num_candidates=.N), by=comm_id]
cand_totals <- contr[, .(total=sum(amount)), by=cand_id]
setkey(cand_totals, cand_id)


# Exclude committees that gave to more than 5 candidates, to reduce total relationships
contr <- contr[comm_id %in% committee_counts[num_candidates <= 5, comm_id]]

# Add total contribution totals to candidate attributes
cand <- cand_totals[cand, nomatch=0]

# Include only candidates who received at least $100K in contributions
# contr <- contr[cand_id %in% cand_totals[total>=1e5, cand_id]]


# Get list of committees and candidates that appear in our contribution set
contr_list <- c(contr[, unique(cand_id)], contr[, unique(comm_id)])

# Combine candidates and committees for our node list
nodes <- rbindlist(list(cand[, .(id=cand_id, type=TRUE, cand_name, party, state, status, total)],
                        comm[, .(id=comm_id, type=FALSE)]),
                   fill=TRUE)
nodes <- nodes[id %in% contr_list]

contr_net <- graph_from_data_frame(contr, vertices=nodes, directed=TRUE)
summary(contr_net)

# See how many connected components are in the network
no.clusters(contr_net)

cand_net <- bipartite.projection(contr_net, which='true')

write.graph(cand_net, file='candidates.GraphML', format="graphml")

comps <- components(contr_net)[[1]]

comps <- data.table(id=names(comps), comp_no=comps)
comps[, comp_no:=as.numeric(comp_no)]
setkey(comps, comp_no)
comps[, size:=.N, by=comp_no]


main_cl <- comps[size==max(size), unique(comp_no)]

main_cl <- induced_subgraph(contr_net, vids=comps[size==max(size), id])
other_cl <- induced_subgraph(contr_net, vids=comps[size < max(size),id])


cand_net <- bipartite.projection(main_cl, which='true')

quantile(V(cand_net)$total, na.rm=TRUE)

# Limit to candidates with at least $500K in contributions
g <- induced_subgraph(cand_net, vids=V(cand_net)[which(V(cand_net)$total > 5e6)])

V(g)$color <- V(g)$party # assign the party attribute as the vertex color
V(g)$color <- gsub("DEM","blue", V(g)$color) 
V(g)$color <- gsub("REP","red", V(g)$color) 
V(g)$color <- gsub("OTH","gray", V(g)$color) 

V(g)$size <- log(V(g)$total)/2

plot(g, vertex.shape="circle", vertex.label=V(g)$cand_name, edge.arrow.size=0, layout=layout.fruchterman.reingold(g) )

# Plot network using visNetwork
visnet <- toVisNetworkData(g)
visnet$nodes$label <- visnet$nodes$cand_name
visNetwork(nodes=visnet$nodes, edges=visnet$edges) %>% visPhysics(solver="barnesHut", timestep=0.2) %>% 
  visEdges(color = "#C3C3C3", width=1)


# Extract into data frame and plot
# gd <- get.data.frame(cand_net, what = "edges")


# Get node attributes
cand_attributes <- data.table(
  cand_id = V(cand_net)$name,
  cand_name = V(cand_net)$cand_name,
  degree = degree(cand_net, v=V(cand_net), mode="all"),
  btw = betweenness(cand_net, v=V(cand_net)),
  pagerank = page.rank(cand_net, v=V(cand_net))$vector)

cand_attributes[order(-degree)]
cand_attributes[order(-btw)]
cand_attributes[order(-pagerank)]


