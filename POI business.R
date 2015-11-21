########### Large data.frames

POI.business1 <- POI.business[1:1000,]
POI.business2 <- POI.business[1001:2000,]
POI.business3 <- POI.business[2001:3000,]
POI.business4 <- POI.business[3001:4000,]
POI.business5 <- POI.business[4001:5000,]
POI.business6 <- POI.business[5001:6000,]
POI.business7 <- POI.business[6001:7000,]
POI.business8 <- POI.business[7001:8000,]
POI.business9 <- POI.business[8001:9000,]
POI.business10 <- POI.business[9001:10000,]
POI.business11<- POI.business[10001:11000,]
POI.business12 <- POI.business[11001:12000,]
POI.business13 <- POI.business[12001:13000,]
POI.business14 <- POI.business[13001:14000,]
POI.business15 <- POI.business[14001:15000,]
POI.business16 <- POI.business[15001:16000,]
POI.business17 <- POI.business[16001:17000,]
POI.business18 <- POI.business[17001:18000,]
POI.business19 <- POI.business[18001:19000,]
POI.business20 <- POI.business[19001:20000,]
POI.business21 <- POI.business[20001:21000,]
POI.business22 <- POI.business[21001:22000,]
POI.business23 <- POI.business[22001:23000,]
POI.business24 <- POI.business[23001:24000,]
POI.business25 <- POI.business[24001:25000,]
POI.business26 <- POI.business[25001:26000,]
POI.business27 <- POI.business[26001:27000,]
POI.business28 <- POI.business[27001:28000,]
POI.business29 <- POI.business[28001:29000,]
POI.business30 <- POI.business[30001:31000,]
POI.business31 <- POI.business[31001:32000,]
POI.business32 <- POI.business[32001:33000,]
POI.business33 <- POI.business[33001:34000,]
POI.business34 <- POI.business[34001:35000,]
POI.business35 <- POI.business[35001:36000,]
POI.business36 <- POI.business[36001:37000,]
POI.business37 <- POI.business[37001:38000,]
POI.business38 <- POI.business[38001:39000,]
POI.business39 <- POI.business[39001:40000,]
POI.business40 <- POI.business[40001:41000,]
POI.business41 <- POI.business[41001:42666,]

p2 <- matrix(c(POI.business41$X,POI.business41$Y),nrow=length(POI.business41$X),ncol=2)

dists <- apply(p2, 1, function(x) distHaversine(p1, x))

dists <- as.data.frame(dists)

dists$min1 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min2 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min3 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min4 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min5 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min6 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min7 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min8 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min9 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min10 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min11 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min12 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min13 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min14 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min15 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min16 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min17 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min18 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min19 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min20 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min21 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min22 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min23 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min24 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min25 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min26 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min27 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min28 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min29 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min30 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min31 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min32 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min33 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min34 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min35 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min36 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min37 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min38 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min39 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min40 <- do.call(pmin, dists[,1:ncol(dists)])
dists$min41 <- do.call(pmin, dists[,1:ncol(dists)])

dists$min <- do.call(pmin, dists[,1:ncol(dists)])



