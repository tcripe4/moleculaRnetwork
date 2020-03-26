#Somewhat streamlined for efficiency

#For keeping track what this simulation data is
#ELEMENT = -IUM


# ------- !!! ------
# Your FILE FORMAT should be xyz
# Coordinates should start on line 2
# If they do not adjust accordingly
# ------------------



#ADJUST ME: how many steps did you run total?
numbersteps<-3999

#ADJUST ME: where's the trajectory data located?
all.steps<-paste("/MyDocuments/acerc/2011-projects/La-forcefit-desktop/polygon-analyses-216-waters/La-DFT-spc-gas-min0_05/100ps/step",c(rep("0000",9),rep("000",90),rep("00",900),rep("0",9000),rep("",30001)),1:numbersteps,".xyz",sep="")

#ADJUST ME: and where are you going to save the analyses?
main.dir<-"/MyDocuments/acerc/2011-projects/La-forcefit-desktop/polygon-analyses-216-waters/La-DFT-spc-gas-min0_05/100ps/analysis"


# ------------ BOX DIMENSIONS ----------------

#ADJUST ME: non-cubic boxes are okay, just be sure you match the dimensions, what are they?
xbside<-18.774
ybside<-xbside
zbside<-xbside

# ------------ ION-SPECIFIC INFO ---------------

#this is how close a water oxygen must be to the central ion in order for it to be considered coordinated - 1st minimum of RDF
#it can be determined by the radial distribution function, by the literature, or backward-revised after you do the analysis and plot the distances and find a whole bunch in a cluster a certain radius out

#ADJUST ME: RDF MINIMUM or MINIMAL CROSSING DISTANCE (second pass)
coordination.cutoff<-3.225

#ADJUST ME: sqrt(2)*(r_RDF.goes.to.1)
polygon.cutoff<-3.642

#ADJUST ME: the maximum number of waters you can consider "coordinated"?
maxco<-10

#ADJUST ME: the minimum?
minco<-8

coors.to.check<-c(minco:maxco)


#LEAVE BE: define functions

andsum<-function(x){
x<-x^2
sum(x)
}


androot<-function(x){
sqrt(andsum(x))
}

#LEAVE BE: load packages
#if you do not have these go to package installer, get list, and choose them...check "install dependencies" box

library(Matrix)
library(igraph)

#set parameters




# ------------ NUMERIC CONSTANTS -------------

#ADJUST ME: water ATOMS in the system
wateratoms<-648

#ADJUST ME: that number squared
wateratomsquared<-648^2

#ADJUST ME: waters in the system
numberwaters<-216

#ADJUST ME: total atoms including the ion
plusion<-649


#indices of water oxygens. note that it goes to "wateratoms" --it will stop at the last water oxygen position 
ow.pos<-seq(from=1,to=wateratoms,by=3)





# ------------ LOOP BEGINS HERE ----------------

setwd(main.dir)
for(iall in 1:1000){

# --^--^--^--^--^--^--^--^--^--^--^--^--^--^--^--^--^--^--

#read in coordinates one frame at a time
#BEWARE THE SKIP -- if you have blank lines at the top of your file, or lines saying the step number/number atoms,
#then you need the option skip=numbersuchlines.  if your trajectories start on line 1, delete skip

ref.ion<-read.table(all.steps[iall],skip=2,fill=TRUE)

ref.ion<-ref.ion[,2:4]

ref.ion<-as.matrix(ref.ion)


#NEW!

insh.adja.dex.2.precursor.x<-sort(ref.ion[,1][ow.pos],index.return=TRUE)$ix

insh.adja.dex.2.precursor.x<-c(insh.adja.dex.2.precursor.x[(numberwaters-49):numberwaters],insh.adja.dex.2.precursor.x,insh.adja.dex.2.precursor.x[1:50])

insh.adja.dex.2.column2.dex<-rep(c(0:(numberwaters-1)),each=101)+rep(c(1:101),times=numberwaters)

insh.adja.dex.2.column1.dex<-50+rep(c(1:numberwaters),each=101)

insh.adja.dex.2.precursor.x.column1<-insh.adja.dex.2.precursor.x[insh.adja.dex.2.column1.dex]

insh.adja.dex.2.precursor.x.column2<-insh.adja.dex.2.precursor.x[insh.adja.dex.2.column2.dex]

precursor.dump<-which(insh.adja.dex.2.precursor.x.column2-insh.adja.dex.2.precursor.x.column1<1)

insh.adja.dex.2.precursor.x.column1<-insh.adja.dex.2.precursor.x.column1[-precursor.dump]

insh.adja.dex.2.precursor.x.column2<-insh.adja.dex.2.precursor.x.column2[-precursor.dump]

insh.adja.dex.2.precursor.y<-sort(ref.ion[,2][ow.pos],index.return=TRUE)$ix

insh.adja.dex.2.precursor.y<-c(insh.adja.dex.2.precursor.y[(numberwaters-49):numberwaters],insh.adja.dex.2.precursor.y,insh.adja.dex.2.precursor.y[1:50])

insh.adja.dex.2.precursor.y.column1<-insh.adja.dex.2.precursor.y[insh.adja.dex.2.column1.dex]

insh.adja.dex.2.precursor.y.column2<-insh.adja.dex.2.precursor.y[insh.adja.dex.2.column2.dex]

precursor.dump<-which(insh.adja.dex.2.precursor.y.column2-insh.adja.dex.2.precursor.y.column1<1)

insh.adja.dex.2.precursor.y.column1<-insh.adja.dex.2.precursor.y.column1[-precursor.dump]

insh.adja.dex.2.precursor.y.column2<-insh.adja.dex.2.precursor.y.column2[-precursor.dump]

insh.adja.dex.2.precursor.z<-sort(ref.ion[,3][ow.pos],index.return=TRUE)$ix

insh.adja.dex.2.precursor.z<-c(insh.adja.dex.2.precursor.z[(numberwaters-49):numberwaters],insh.adja.dex.2.precursor.z,insh.adja.dex.2.precursor.z[1:50])

insh.adja.dex.2.precursor.z.column1<-insh.adja.dex.2.precursor.z[insh.adja.dex.2.column1.dex]

insh.adja.dex.2.precursor.z.column2<-insh.adja.dex.2.precursor.z[insh.adja.dex.2.column2.dex]

precursor.dump<-which(insh.adja.dex.2.precursor.z.column2-insh.adja.dex.2.precursor.z.column1<1)

insh.adja.dex.2.precursor.z.column1<-insh.adja.dex.2.precursor.z.column1[-precursor.dump]

insh.adja.dex.2.precursor.z.column2<-insh.adja.dex.2.precursor.z.column2[-precursor.dump]

insh.adja.x.y<-cbind(c(insh.adja.dex.2.precursor.x.column1,insh.adja.dex.2.precursor.y.column1),c(insh.adja.dex.2.precursor.x.column2,insh.adja.dex.2.precursor.y.column2))

insh.adja.x.y<-insh.adja.x.y[duplicated(insh.adja.x.y),]

insh.adja.x.y.z<-cbind(c(insh.adja.x.y[,1],insh.adja.dex.2.precursor.z.column1),c(insh.adja.x.y[,2],insh.adja.dex.2.precursor.z.column2))

insh.adja.x.y.z<-insh.adja.x.y.z[duplicated(insh.adja.x.y.z),]



distance.to.hydrogens.1.H1.modme<-ref.ion[ow.pos,][insh.adja.x.y.z[,1],]-ref.ion[ow.pos+1,][insh.adja.x.y.z[,2],]

distance.to.hydrogens.1.H2.modme<-ref.ion[ow.pos,][insh.adja.x.y.z[,1],]-ref.ion[ow.pos+2,][insh.adja.x.y.z[,2],]

distance.to.hydrogens.2.H1.modme<-(-1)*(ref.ion[ow.pos+1,][insh.adja.x.y.z[,1],]-ref.ion[ow.pos,][insh.adja.x.y.z[,2],])

distance.to.hydrogens.2.H2.modme<-(-1)*(ref.ion[ow.pos+2,][insh.adja.x.y.z[,1],]-ref.ion[ow.pos,][insh.adja.x.y.z[,2],])

distance.to.hydrogens.1.H1.modme[which(distance.to.hydrogens.1.H1.modme>10)]<-distance.to.hydrogens.1.H1.modme[which(distance.to.hydrogens.1.H1.modme>10)]-xbside

distance.to.hydrogens.1.H2.modme[which(distance.to.hydrogens.1.H2.modme>10)]<-distance.to.hydrogens.1.H2.modme[which(distance.to.hydrogens.1.H2.modme>10)]-xbside

distance.to.hydrogens.2.H1.modme[which(distance.to.hydrogens.2.H1.modme>10)]<-distance.to.hydrogens.2.H1.modme[which(distance.to.hydrogens.2.H1.modme>10)]-xbside

distance.to.hydrogens.2.H2.modme[which(distance.to.hydrogens.2.H2.modme>10)]<-distance.to.hydrogens.2.H2.modme[which(distance.to.hydrogens.2.H2.modme>10)]-xbside

distance.to.hydrogens.1.H1.modme[which(distance.to.hydrogens.1.H1.modme<(-10))]<-distance.to.hydrogens.1.H1.modme[which(distance.to.hydrogens.1.H1.modme<(-10))]+xbside

distance.to.hydrogens.1.H2.modme[which(distance.to.hydrogens.1.H2.modme<(-10))]<-distance.to.hydrogens.1.H2.modme[which(distance.to.hydrogens.1.H2.modme<(-10))]+xbside

distance.to.hydrogens.2.H1.modme[which(distance.to.hydrogens.2.H1.modme<(-10))]<-distance.to.hydrogens.2.H1.modme[which(distance.to.hydrogens.2.H1.modme<(-10))]+xbside

distance.to.hydrogens.2.H2.modme[which(distance.to.hydrogens.2.H2.modme<(-10))]<-distance.to.hydrogens.2.H2.modme[which(distance.to.hydrogens.2.H2.modme<(-10))]+xbside

distance.to.hydrogens.1.H1<-apply(distance.to.hydrogens.1.H1.modme,1,androot)

distance.to.hydrogens.1.H2<-apply(distance.to.hydrogens.1.H2.modme,1,androot)

distance.to.hydrogens.2.H1<-apply(distance.to.hydrogens.2.H1.modme,1,androot)

distance.to.hydrogens.2.H2<-apply(distance.to.hydrogens.2.H2.modme,1,androot)

distance.to.hydrogens.1<-distance.to.hydrogens.1.H1

distance.to.hydrogens.1[which(distance.to.hydrogens.1.H2<distance.to.hydrogens.1.H1)]<-distance.to.hydrogens.1.H2[which(distance.to.hydrogens.1.H2<distance.to.hydrogens.1.H1)]

distance.to.hydrogens.2<-distance.to.hydrogens.2.H1

distance.to.hydrogens.2[which(distance.to.hydrogens.2.H2<distance.to.hydrogens.2.H1)]<-distance.to.hydrogens.2.H2[which(distance.to.hydrogens.2.H2<distance.to.hydrogens.2.H1)]

summary(distance.to.hydrogens.1)
summary(distance.to.hydrogens.2)

length(which(distance.to.hydrogens.1<=2.5))
length(which(distance.to.hydrogens.2<=2.5))

distance.to.own.hydrogens.1.H1<-ref.ion[ow.pos,][insh.adja.x.y.z[,2],]-ref.ion[ow.pos+1,][insh.adja.x.y.z[,2],]

distance.to.own.hydrogens.1.H2<-ref.ion[ow.pos,][insh.adja.x.y.z[,2],]-ref.ion[ow.pos+2,][insh.adja.x.y.z[,2],]

distance.to.own.hydrogens.2.H1<-ref.ion[ow.pos,][insh.adja.x.y.z[,1],]-ref.ion[ow.pos+1,][insh.adja.x.y.z[,1],]

distance.to.own.hydrogens.2.H2<-ref.ion[ow.pos,][insh.adja.x.y.z[,1],]-ref.ion[ow.pos+2,][insh.adja.x.y.z[,1],]

angles.sums.1.H1<-apply(distance.to.hydrogens.1.H1.modme*distance.to.own.hydrogens.1.H1,1,sum)

angles.norms.1.H1.a<-distance.to.hydrogens.1.H1

angles.norms.1.H1.b<-apply(distance.to.own.hydrogens.1.H1,1,androot)

angles.cosines.1.H1<-acos(angles.sums.1.H1/(angles.norms.1.H1.a*angles.norms.1.H1.b))

angles.sums.1.H2<-apply(distance.to.hydrogens.1.H2.modme*distance.to.own.hydrogens.1.H2,1,sum)

angles.norms.1.H2.a<-distance.to.hydrogens.1.H2

angles.norms.1.H2.b<-apply(distance.to.own.hydrogens.1.H2,1,androot)

angles.cosines.1.H2<-acos(angles.sums.1.H2/(angles.norms.1.H2.a*angles.norms.1.H2.b))

angles.sums.2.H1<-apply(distance.to.hydrogens.2.H1.modme*distance.to.own.hydrogens.2.H1,1,sum)

angles.norms.2.H1.a<-distance.to.hydrogens.2.H1

angles.norms.2.H1.b<-apply(distance.to.own.hydrogens.2.H1,1,androot)

angles.cosines.2.H1<-acos(angles.sums.2.H1/(angles.norms.2.H1.a*angles.norms.2.H1.b))

angles.sums.2.H2<-apply(distance.to.hydrogens.2.H2.modme*distance.to.own.hydrogens.2.H2,1,sum)

angles.norms.2.H2.a<-distance.to.hydrogens.2.H2

angles.norms.2.H2.b<-apply(distance.to.own.hydrogens.2.H2,1,androot)

angles.cosines.2.H2<-acos(angles.sums.2.H2/(angles.norms.2.H2.a*angles.norms.2.H2.b))


h.bonds.list.1<-insh.adja.x.y.z[c(intersect(which(angles.cosines.1.H1*(180/pi)>150),which(distance.to.hydrogens.1.H1<2.5)),intersect(which(angles.cosines.1.H2*(180/pi)>150),which(distance.to.hydrogens.1.H2<2.5))),]

h.bonds.list.2<-insh.adja.x.y.z[c(intersect(which(angles.cosines.2.H1*(180/pi)>150),which(distance.to.hydrogens.2.H1<2.5)),intersect(which(angles.cosines.2.H2*(180/pi)>150),which(distance.to.hydrogens.2.H2<2.5))),]

h.bonds.list<-rbind(h.bonds.list.1,h.bonds.list.2)

h.bonds.edge.list<-c(t(h.bonds.list))

#YEAH! FASTER! Needs parameterization, 101 won't do for the big box -- larger required? May have to break in two or three depending on dimensions

#This isn't weighted anymore. Needs fix

noion.net<-graph(edges=h.bonds.edge.list,n=216,directed=FALSE)

#removes duplicate edges
noion.net<-simplify(noion.net)


#then copy it and set all weights to 1

noion.unweighted.net<-noion.net

E(noion.unweighted.net)$weight<-1


#find all the distances to the ion

ionnei.precursor<-sqrt((rep(ref.ion[,1][plusion],numberwaters)-ref.ion[,1][ow.pos])^2+(rep(ref.ion[,2][plusion],numberwaters)-ref.ion[,2][ow.pos])^2+(rep(ref.ion[,3][plusion],numberwaters)-ref.ion[,3][ow.pos])^2)

write(ionnei.precursor,file="distance.to.ion",ncolumns=numberwaters,append=TRUE)
write(order(ionnei.precursor),file="ion.dista.dex",ncolumns=numberwaters,append=TRUE)


#now we want those which are close enough to be coordinated to it
ionnei.dex<-which(ionnei.precursor<coordination.cutoff)

write(length(ionnei.dex),file="ion.coordination.number",ncolumns=1,append=TRUE)



#create graphs WITH the ion in them


ion.net<-add.vertices(noion.net,1)

#you need to add edges between the ion and its coordinated waters
#the way that this adds edges is if you have a vector, say
#c(1,2,2,3,3,5)
#it will add an edge between 1 and 2, 2 and 3, and 3 and 5.
#so we have an index that will, accordingly, alternate the water vertex id with the ion vertex id.

new.elist.dex<-c(seq(from=2,to=length(ionnei.dex)*2,by=2),seq(from=1,to=length(ionnei.dex)*2,by=2))

new.elist<-c(rep(numberwaters,length(ionnei.dex)),ionnei.dex-1)
new.elist<-new.elist[order(new.elist.dex)]

start.new.edges<-ecount(ion.net)+1

end.new.edges<-start.new.edges+length(ionnei.dex)-1

ion.net<-add.edges(ion.net,new.elist)

E(ion.net)$weight[start.new.edges:end.new.edges]<-ionnei.precursor[ionnei.dex]



#THE GRAPH WE REALLY WANT

ion.unweighted.net<-add.vertices(noion.unweighted.net,1)

ion.unweighted.net<-add.edges(ion.unweighted.net,new.elist)

E(ion.unweighted.net)$weight[start.new.edges:end.new.edges]<-1



# -------------- GRAPH ANALYSIS: FULL NETWORKS --------------------

#clustering
clustas<-clusters(ion.unweighted.net)


#saving a vector giving which cluster the vertex/water belongs to. so if there are three clusters
#and one is all of the waters but three, another is two waters that are only H-bonded to each other,
#and the last one is a singleton,
#you could have a row of ... 0 0 0 0 0 1 1 0 0 0 0 0 0 2 0 0 ... for example
#whatever cluster the first one belongs to is cluster zero, i believe.

write(clustas$membership,file="cluster.membership",ncol=(numberwaters+1),append=TRUE)

#just to be sure, this writes a vector that gives the sizes of the clusters in order
#its use is that it can quickly tell you how many singletons you have.
#i pad it with zeros so that it is a constant length, which makes it easier for R to read it in again.

write(c(clustas$size,rep(0,20-length(clustas$size))),file="cluster.size",ncol=20,append=TRUE)


#pathlengths -- shorter version
#pathologcl<-path.length.hist(ion.net)

#write and pad to constant length
#write(c(pathologcl$res,rep(0,40-length(pathologcl$res))),file="path.lengths.with.ion",ncolumns=40,append=TRUE)

#noion.pathologcl<-path.length.hist(noion.net)

#write(c(noion.pathologcl$res,rep(0,40-length(noion.pathologcl$res))),file="path.lengths.without",ncolumns=40,append=TRUE)



#degrees

#this is the number of hydrogen bonds each water has, as well as the coordination of the ion, histogrammed
degredist<-degree.distribution(ion.net)

write(c(degredist*(numberwaters+1),rep(0,15-length(degredist))),file="degree.distribution",ncolumns=15,append=TRUE)

#and this is the degree for each water, as well as the ion, so you can plot as a function of distance
net.degs<-degree(ion.net)

no.net.degs<-degree(noion.net)

write(net.degs,file="all.degrees",ncolumns=(numberwaters+1),append=TRUE)

write(no.net.degs,file="no.ion.all.degrees",ncolumns=(numberwaters+1),append=TRUE)


#H-bond network page ranks

#weights and without weights, with the standard 0.85 damping factor, and the cranked up .99 damping factor that is more sensitive to the global structure of the network

#ion.prs.general<-page.rank(ion.net)$vec

ion.unw.prs.general<-page.rank(ion.unweighted.net)$vec

#ion.prs.global<-page.rank(ion.net,damping=.99)$vec

ion.unw.prs.global<-page.rank(ion.unweighted.net,damping=.99)$vec

#write(ion.prs.general,file="weighted.page.ranks.85",ncolumns=(numberwaters+1),append=TRUE)

write(ion.unw.prs.general,file="unweighted.page.ranks.85",ncolumns=(numberwaters+1),append=TRUE)

#write(ion.prs.global,file="weighted.page.ranks.99",ncolumns=(numberwaters+1),append=TRUE)

write(ion.unw.prs.global,file="unweighted.page.ranks.99",ncolumns=(numberwaters+1),append=TRUE)






# --------- POLYGONS --------------

# ----- you need to specify which of these you want, at the beginning, with maxco and minco -------

#10s
if(10%in%coors.to.check){
	
	ion.shell.10<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:10]],plusion),]


#this is a copy of code from before, which is fast code, but which needed updating to account for the fact that we aren't always after 8, 9, and 10-coordinated ions


#"androot" IS A HOUSE MADE EUCLIDEAN DISTANCE FINDER.  HERE WE ARE GETTING ALL THE PAIR DISTANCES
	distapre<-apply(ion.shell.10[rep(c(1:11),each=11),]-ion.shell.10[rep(c(1:11),11),],1,androot)


#GEOMETRICALLY YOU WANT TO SCRAP "EDGES" THAT AREN'T -- SAY, INTERNALS, WHATEVER.  YOU ONLY WANT THE POLYHEDRON.  A DISTANCE CRITERIA SOLVES THIS ISSUE.  I SUGGEST LOOKING AT YOUR DISTANCES AND SEEING IF THEY COME IN TWO CHUNKS: SMALLER THAN X AND BIGGER THAN X, AND CHOOSING X BASED ON THAT.
	distapre[which(distapre>polygon.cutoff)]=0

#MAKE A MATRIX W/ CAPITAL M FOR PROCESSING
	distamat<-Matrix(data=distapre,nrow=11,ncol=11,sparse=TRUE)

#MAKE GRAPH OF ALL 10 PLUS ION
	ten.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

#pitcha.maka.10<-paste("right.snapgraph10-",ime,".png",sep="")

#rgl.open()
#par3d(windowRect=c(85,120,744,705))
#rglplot(ten.star,vertex.color=c(rep("red",10),"blue"),edge.width=5,layout=as.matrix(ion.shell.10))
#rgl.viewpoint(zoom=.5)
#rgl.snapshot(pitcha.maka.10,fmt="png",top=TRUE)
#rgl.close()



#PAGE RANK: THIS IS HOW WE IDENTIFY THE POLYHEDRON
#simplify is required to eliminate duplicate edges, which add to the rank of their associated vertex
#damping is the damping factor d in the page rank equation.  the large value used here tells us that the ranks are more sensitive to changes in overall structure

	ten.star.pr<-page.rank(simplify(ten.star),damping=.99)$vector


#WRITE FILES
	write(ten.star.pr,file="right.weighted.tenstar",ncolumns=11,append=TRUE)

#REDO AND PAGE RANK WITHOUT WEIGHTS!!! IT'S ACTUALLY THESE THAT MATCH OUR IDEALIZED POLYHEDRA
	ten.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)

#REMOVE DUPLICATED EDGES
	ten.star.unpr<-page.rank(simplify(ten.star.un),damping=.99)$vector

	write(ten.star.unpr,file="right.unweighted.tenstar",ncolumns=11,append=TRUE)

	
	}

#9s
if(9%in%coors.to.check){
	ion.shell.9<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:9]],plusion),]


	distapre<-apply(ion.shell.9[rep(c(1:10),each=10),]-ion.shell.9[rep(c(1:10),10),],1,androot)



	distapre[which(distapre>polygon.cutoff)]=0


	distamat<-Matrix(data=distapre,nrow=10,ncol=10,sparse=TRUE)


	nine.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

	#pitcha.maka.9<-paste("right.snapgraph9-",ime,".png",sep="")

	#rgl.open()
	#par3d(windowRect=c(85,120,744,705))
	#rglplot(nine.star,vertex.color=c(rep("red",9),"blue"),edge.width=5,layout=as.matrix(ion.shell.9))
	#rgl.viewpoint(zoom=.5)
	#rgl.snapshot(pitcha.maka.9,fmt="png",top=TRUE)
	#rgl.close()



	nine.star.pr<-page.rank(simplify(nine.star),damping=.99)$vector


	write(nine.star.pr,file="right.weighted.ninestar",ncolumns=10,append=TRUE)


	nine.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)


	nine.star.unpr<-page.rank(simplify(nine.star.un),damping=.99)$vector

	write(nine.star.unpr,file="right.unweighted.ninestar",ncolumns=10,append=TRUE)

	
	
	}

#8s
if(8%in%coors.to.check){
	ion.shell.8<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:8]],plusion),]


	distapre<-apply(ion.shell.8[rep(c(1:9),each=9),]-ion.shell.8[rep(c(1:9),9),],1,androot)



	distapre[which(distapre>polygon.cutoff)]=0


	distamat<-Matrix(data=distapre,nrow=9,ncol=9,sparse=TRUE)


	eight.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

	#pitcha.maka.8<-paste("FIXED.right.snapgraph8-",ime,".png",sep="")

	#rgl.open()
	#par3d(windowRect=c(85,120,744,705))
	#rglplot(eight.star,vertex.color=c(rep("red",8),"blue"),edge.width=5,layout=as.matrix(ion.shell.8))
	#rgl.viewpoint(zoom=.5)
	#rgl.snapshot(pitcha.maka.8,fmt="png",top=TRUE)
	#rgl.close()



	eight.star.pr<-page.rank(simplify(eight.star),damping=.99)$vector


	write(eight.star.pr,file="FIXED.right.weighted.eightstar",ncolumns=9,append=TRUE)


	eight.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)


	eight.star.unpr<-page.rank(simplify(eight.star.un),damping=.99)$vector

	write(eight.star.unpr,file="FIXED.right.unweighted.eightstar",ncolumns=9,append=TRUE)

	
	}

#smaller systems:
#7s
if(7%in%coors.to.check){
	ion.shell.7<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:7]],plusion),]


	distapre<-apply(ion.shell.7[rep(c(1:8),each=8),]-ion.shell.7[rep(c(1:8),8),],1,androot)



	distapre[which(distapre>polygon.cutoff)]=0


	distamat<-Matrix(data=distapre,nrow=8,ncol=8,sparse=TRUE)


	seven.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

	#pitcha.maka.7<-paste("FIXED.right.snapgraph7-",ime,".png",sep="")

	#rgl.open()
	#par3d(windowRect=c(85,120,744,705))
	#rglplot(seven.star,vertex.color=c(rep("red",7),"blue"),edge.width=5,layout=as.matrix(ion.shell.7))
	#rgl.viewpoint(zoom=.5)
	#rgl.snapshot(pitcha.maka.7,fmt="png",top=TRUE)
	#rgl.close()



	seven.star.pr<-page.rank(simplify(seven.star),damping=.99)$vector


	write(seven.star.pr,file="FIXED.right.weighted.sevenstar",ncolumns=8,append=TRUE)


	seven.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)


	seven.star.unpr<-page.rank(simplify(seven.star.un),damping=.99)$vector

	write(seven.star.unpr,file="FIXED.right.unweighted.sevenstar",ncolumns=8,append=TRUE)

	
	}

#6s
if(6%in%coors.to.check){
	ion.shell.6<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:6]],plusion),]


	distapre<-apply(ion.shell.6[rep(c(1:7),each=7),]-ion.shell.6[rep(c(1:7),7),],1,androot)



	distapre[which(distapre>polygon.cutoff)]=0


	distamat<-Matrix(data=distapre,nrow=7,ncol=7,sparse=TRUE)


	six.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

	#pitcha.maka.6<-paste("FIXED.right.snapgraph6-",ime,".png",sep="")

	#rgl.open()
	#par3d(windowRect=c(85,120,744,705))
	#rglplot(six.star,vertex.color=c(rep("red",6),"blue"),edge.width=5,layout=as.matrix(ion.shell.6))
	#rgl.viewpoint(zoom=.5)
	#rgl.snapshot(pitcha.maka.6,fmt="png",top=TRUE)
	#rgl.close()



	six.star.pr<-page.rank(simplify(six.star),damping=.99)$vector


	write(six.star.pr,file="FIXED.right.weighted.sixstar",ncolumns=7,append=TRUE)


	six.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)


	six.star.unpr<-page.rank(simplify(six.star.un),damping=.99)$vector

	write(six.star.unpr,file="FIXED.right.unweighted.sixstar",ncolumns=7,append=TRUE)
	
	}

#5s
if(5%in%coors.to.check){
	ion.shell.5<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:5]],plusion),]


	distapre<-apply(ion.shell.5[rep(c(1:6),each=6),]-ion.shell.5[rep(c(1:6),6),],1,androot)



	distapre[which(distapre>polygon.cutoff)]=0


	distamat<-Matrix(data=distapre,nrow=6,ncol=6,sparse=TRUE)


	five.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

	#pitcha.maka.5<-paste("FIXED.right.snapgraph6-",ime,".png",sep="")

	#rgl.open()
	#par3d(windowRect=c(85,120,744,705))
	#rglplot(five.star,vertex.color=c(rep("red",5),"blue"),edge.width=5,layout=as.matrix(ion.shell.5))
	#rgl.viewpoint(zoom=.5)
	#rgl.snapshot(pitcha.maka.5,fmt="png",top=TRUE)
	#rgl.close()



	five.star.pr<-page.rank(simplify(five.star),damping=.99)$vector


	write(five.star.pr,file="FIXED.right.weighted.fivestar",ncolumns=6,append=TRUE)


	five.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)


	five.star.unpr<-page.rank(simplify(five.star.un),damping=.99)$vector

	write(five.star.unpr,file="FIXED.right.unweighted.fivestar",ncolumns=6,append=TRUE)
	
	}



#4s
if(4%in%coors.to.check){
	ion.shell.4<-ref.ion[c(ow.pos[order(ionnei.precursor)[1:4]],plusion),]


	distapre<-apply(ion.shell.4[rep(c(1:5),each=5),]-ion.shell.4[rep(c(1:5),5),],1,androot)



	distapre[which(distapre>polygon.cutoff)]=0


	distamat<-Matrix(data=distapre,nrow=5,ncol=5,sparse=TRUE)


	four.star<-graph.adjacency(distamat,mode="undirected",weighted=TRUE)

#--UNCOMMENT HERE IF YOU WANT SNAPSHOTS MADE!

	#pitcha.maka.4<-paste("FIXED.right.snapgraph4-",ime,".png",sep="")

	#rgl.open()
	#par3d(windowRect=c(85,120,744,705))
	#rglplot(four.star,vertex.color=c(rep("red",4),"blue"),edge.width=5,layout=as.matrix(ion.shell.4))
	#rgl.viewpoint(zoom=.5)
	#rgl.snapshot(pitcha.maka.4,fmt="png",top=TRUE)
	#rgl.close()



	four.star.pr<-page.rank(simplify(four.star),damping=.99)$vector


	write(four.star.pr,file="FIXED.right.weighted.fourstar",ncolumns=5,append=TRUE)


	four.star.un<-graph.adjacency(distamat,mode="undirected",weighted=NULL)


	four.star.unpr<-page.rank(simplify(four.star.un),damping=.99)$vector

	write(four.star.unpr,file="FIXED.right.unweighted.fourstar",ncolumns=5,append=TRUE)
	
	}
	
	
# DIPOLE, TILT. Haven't seen these in a while and I thought I'd give them to you!  We can look for waters that aren't "pointing" right, as well as think about the "landing/launching trajectory" of entering/exiting waters


dipole.vectors<-(-2)*ref.ion[ow.pos,]+ref.ion[ow.pos+1,]+ref.ion[ow.pos+2,]

dipole.cosines<-(ref.ion[ow.pos,]-ref.ion[rep(nrow(ref.ion),numberwaters),])*dipole.vectors/(apply((ref.ion[ow.pos,]-ref.ion[rep(nrow(ref.ion),numberwaters),]),1,androot)*apply(dipole.vectors,1,androot))

dipole.cosines<-rowSums(dipole.cosines)

#WRITE TO FILE

write(dipole.cosines,file="dipole.cosines",ncol=numberwaters,append=TRUE)


tilt.vectors<-cbind((ref.ion[,2][ow.pos]-ref.ion[,2][ow.pos+1])*(ref.ion[,3][ow.pos]-ref.ion[,3][ow.pos+2])-(ref.ion[,3][ow.pos]-ref.ion[,3][ow.pos+1])*(ref.ion[,2][ow.pos]-ref.ion[,2][ow.pos+2]),(ref.ion[,3][ow.pos]-ref.ion[,3][ow.pos+1])*(ref.ion[,1][ow.pos]-ref.ion[,1][ow.pos+2])-(ref.ion[,1][ow.pos]-ref.ion[,1][ow.pos+1])*(ref.ion[,3][ow.pos]-ref.ion[,3][ow.pos+2]),(ref.ion[,1][ow.pos]-ref.ion[,1][ow.pos+1])*(ref.ion[,2][ow.pos]-ref.ion[,2][ow.pos+2])-(ref.ion[,2][ow.pos]-ref.ion[,2][ow.pos+1])*(ref.ion[,1][ow.pos]-ref.ion[,1][ow.pos+2]))

# Since the "top side" of the water is arbitrarily defined, some tilts point toward the ion while others point away from it.

# To account for this, positive and negative cosines will be treated the same.  Only absolute value is of interest here.

tilt.cosines<-(ref.ion[ow.pos,]-ref.ion[rep(nrow(ref.ion),numberwaters),])*tilt.vectors/(apply((ref.ion[ow.pos,]-ref.ion[rep(nrow(ref.ion),numberwaters),]),1,androot)*apply(tilt.vectors,1,androot))

tilt.cosines<-rowSums(tilt.cosines)

write(tilt.cosines,file="tilt.cosines",ncol=numberwaters,append=TRUE)

}	
