#first things first: lanthanum "distance to ion" for all waters.


#adjust directory path as needed


#---!---!---!---!---

#TO RUN CODE:
#highlight all text you want to run
#command-return
#or, place the cursor anywhere in a line, and command-return to run the line.

#---!---!---!---!---

#ADJUST PATH
lanthanum.distances<-read.table("/MyDocuments/acerc/2011-projects/La-forcefit-desktop/barbara/distance.to.ion")
lanthanum.distances<-as.matrix(lanthanum.distances)


#---!---!---!---!---

#LESSON
#data types: default is to read in the file as a "data frame"
#this kind of data structure will give you results you don't expect
#you are best off working with vectors (1-dimensional) and matrices (2-dimensional)
#converting to the matrix form is kind of memory heavy
#so always "gc()" afterwards

#---!---!---!---!---


gc()

#this gets rid of extra junk on the "heap", basically homeless calls, variable values no longer attached to an object, etc -- it frees memory so I do it a lot.


lanthanum.dista.dex<-apply(lanthanum.distances,1,order)


#---!---!---!---!---

#LESSON
#apply(): takes a function that works on a vector and applies it over rows (1) or columns (2) of a matrix.  The function here is "order" which produces a vector of indices, the first value of which is the index of the smallest value in the vector it's applied to.

#---!---!---!---!---


lanthanum.dista.dex<-t(lanthanum.dista.dex)


#---!---!---!---!---

#LESSON
#t() : matrix transpose. applying a function to rows, for some reason, produces a result in columns!

#---!---!---!---!---


#OPTIONAL
#assess the minimal crossing distance for the shell boundary
#you can skip this step if you prefer to use RDF cutoff values
#or if you know it already ;)


#HOW THIS CODE WORKS:

#ALGORITHM
#two nested loops!
#outer loop: a loop over a range of possible shell boundaries
# (try not to cover the RDF peaks, choose a starting value after the first and before the second)
# pick a value, do inner loop
	# inner loop: it's a loop over all snapshots!
	# preprocess: identify which are in the shell in the first step, store this
	# 1. identify which waters are in the shell
	# 2. record how many there were
	# 3. compare current step to previous step
	# 4. record how many went in
	# 5. record how many went out
	# repeat loop
# when done, store the values, repeat loop on the next possible radius.
#at the end, sum the columns of the .all matrices to find the radius with the fewest changes.  You can also take column means on the .size.all matrix to integrate the RDF.


#---!---!---!---!---


#Ready? Go!

first.shell.lanthanum.water.in.all<-NULL

first.shell.lanthanum.water.out.all<-NULL

first.shell.lanthanum.size.all<-NULL


#---!---!---!---!---

#LESSON
#NULL is a special object, think of this as "initializing" the variables

#---!---!---!---!---


#PARAMETER
#ADJUST minimum and maximum radii, spanning 1st minimum of RDF
#defaults to 2.5 and 4.5 

min.1sh.radius<-2.5
max.1sh.radius<-4.5

#---!---!---!---!--- OUTER LOOP

for(j in seq(from=min.1sh.radius,to=max.1sh.radius,by=.05)){


#---!---!---!---!---

#LESSON
#seq(): sequences. from=value1, to=value2, by=interval between values, don't worry it will stop <=value2.
#try:
#seq(from=1,to=10,by=2)
#seq(from=1,to=10,by=3)
#seq(from=1,to=10,by=100)

#---!---!---!---!---


#initialize inshells lists: start with the first step

prior.first.shell.membership<-which(lanthanum.distances[1,]<=j)


#---!---!---!---!---

#LESSON
#which() is the most useful function.
#it returns a vector of indices telling you which values of a vector satisfy a certain criterion
#try this
#c(1,3,6,4,7,3,2)>2
#which(c(1,3,6,4,7,3,2)>2)

#---!---!---!---!---


#initialize change vectors, ADJUST IF YOU HAVE LESS THAN 40K SNAPSHOTS

first.shell.lanthanum.water.in<-rep(0,40000)

first.shell.lanthanum.water.out<-rep(0,40000)

first.shell.lanthanum.size<-rep(0,40000)


#---!---!---!---!--- INNER LOOP

#ADJUST IF YOU HAVE LESS THAN 40K SNAPSHOTS
for(i in 1:40000){	
	first.shell.membership<-which(lanthanum.distances[i,]<=j)
	

#---!---!---!---!---
	
#LESSON
#indexing [x,] means row x
#[c(1:x),] rows 1, 2, ..., x
#[,x] column x
#[x] value x (counts down columns first)
#you might see [[x]] meaning "list element x", returns ONE value only

#---!---!---!---!---
	
	
	first.shell.lanthanum.size[i]<-length(first.shell.membership)
	
	first.shell.lanthanum.water.in[i]<-length(setdiff(first.shell.membership,prior.first.shell.membership))
	
	first.shell.lanthanum.water.out[i]<-(-1)*length(setdiff(prior.first.shell.membership,first.shell.membership))


#---!---!---!---!---

#LESSON
#setdiff(x,y): values in the first set (x) that aren't found in the second set (y)  order matters!!

#---!---!---!---!---

	
	prior.first.shell.membership<-first.shell.membership
	}


#---!---!---!---!--- CLOSE OF INNER LOOP
	
	
first.shell.lanthanum.water.in.all<-cbind(first.shell.lanthanum.water.in.all,first.shell.lanthanum.water.in,deparse.level=0)

first.shell.lanthanum.water.out.all<-cbind(first.shell.lanthanum.water.out.all,first.shell.lanthanum.water.out,deparse.level=0)

first.shell.lanthanum.size.all<-cbind(first.shell.lanthanum.size.all,first.shell.lanthanum.size,deparse.level=0)
}


#---!---!---!---!--- CLOSE OF OUTER LOOP


#Plotting / visualization!

#OPEN GRAPHICS WINDOW
#visual inspection of crossing curves

plot(x=seq(from=min.1sh.radius,to=max.1sh.radius,by=.05),y=colSums(first.shell.lanthanum.water.out.all),ylim=c(min(colSums(first.shell.lanthanum.water.out.all)),max(colSums(first.shell.lanthanum.water.in.all))),col="red",pch=21,main="Crossing Frequency by Radius")

#look first, then run

points(x=seq(from=min.1sh.radius,to=max.1sh.radius,by=.05),y=colSums(first.shell.lanthanum.water.in.all),col="blue",pch=22)

#points() adds points to a plot
#plot() starts a new plot


#finding the MCD automatically using which() -- here, which.max() , a special case function

lanthanum.minimal.crossing.distance<-seq(from=min.1sh.radius,to=max.1sh.radius,by=.05)[which.max(colSums(first.shell.lanthanum.water.out.all))]

first.shell.lanthanum.size<-first.shell.lanthanum.size.all[,which.max(colSums(first.shell.lanthanum.water.out.all))]

#PRINT OUT LANTHANUM MINIMUM CROSSING DISTANCE AND RECORD
#PRINT OUT FIRST SHELL SIZE AND RECORD

#OPTION:set MCD yourself

#lanthanum.minimal.crossing.distance<-

#these go along with the distance you set yourself

#length.which.under<-function(x,cutoff){length(which(x<=cutoff))}


#LESSON
#FIRST INSTANCE OF HOUSE MADE FUNCTION...


#first.shell.lanthanum.size<-apply(lanthanum.distances,1,length.which.under,cutoff=lanthanum.minimal.crossing.distance)



#---!---!---!---!---



#SECOND SHELL

#SECOND VERSE SAME AS THE FIRST
#same algorithm

second.shell.lanthanum.water.in.all<-NULL

second.shell.lanthanum.water.out.all<-NULL

second.shell.lanthanum.size.all<-NULL

#PARAMETERS
#ADJUST spanning second minimum of RDF
#defaults to 4.5 and 6.5
min.2sh.radius<-4.5
max.2sh.radius<-6.5


#---!---!---!---!--- OUTER LOOP


for(j in seq(from=min.2sh.radius,to=max.2sh.radius,by=.05)){

#initialize inshells lists
prior.second.shell.membership<-intersect(which(lanthanum.distances[1,]<=j),which(lanthanum.distances[1,]>lanthanum.minimal.crossing.distance))

#initialize change vectors ADJUST IF LESS THAN 40K SNAPSHOTS
second.shell.lanthanum.water.in<-rep(0,40000)

second.shell.lanthanum.water.out<-rep(0,40000)

second.shell.lanthanum.size<-rep(0,40000)


#---!---!---!---!--- INNER LOOP

#ADJUST IF LESS THAN 40K SNAPSHOTS
for(i in 1:40000){
	second.shell.membership<-intersect(which(lanthanum.distances[i,]<=j),which(lanthanum.distances[i,]>lanthanum.minimal.crossing.distance))
	
	second.shell.lanthanum.size[i]<-length(second.shell.membership)
	
	second.shell.lanthanum.water.in[i]<-length(setdiff(second.shell.membership,prior.second.shell.membership))
	
	second.shell.lanthanum.water.out[i]<-(-1)*length(setdiff(prior.second.shell.membership,second.shell.membership))
	
	prior.second.shell.membership<-second.shell.membership
	}


#---!---!---!---!--- CLOSE OF INNER LOOP

	
second.shell.lanthanum.water.in.all<-cbind(second.shell.lanthanum.water.in.all,second.shell.lanthanum.water.in,deparse.level=0)

second.shell.lanthanum.water.out.all<-cbind(second.shell.lanthanum.water.out.all,second.shell.lanthanum.water.out,deparse.level=0)

second.shell.lanthanum.size.all<-cbind(second.shell.lanthanum.size.all,second.shell.lanthanum.size,deparse.level=0)
}


#---!---!---!---!--- CLOSE OF OUTER LOOP


lanthanum.second.shell.minimal.crossing.distance<-seq(from=min.2sh.radius,to=max.2sh.radius,by=.05)[which.max(colSums(second.shell.lanthanum.water.out.all))]

second.shell.lanthanum.size<-second.shell.lanthanum.size.all[,which.max(colSums(second.shell.lanthanum.water.out.all))]

#PRINT LANTHANUM 2ND SHELL DISTANCE AND 2ND SHELL SIZE AND RECORD

#OPEN GRAPHICS WINDOW -- this will plot both 1 and 2 shell crossings

plot(x=c(seq(from=min.1sh.radius,to=max.1sh.radius,by=.05),seq(from=min.2sh.radius,to=max.2sh.radius,by=.05)),y=c(colSums(first.shell.lanthanum.water.out.all),colSums(second.shell.lanthanum.water.out.all)),ylim=c(min(c(colSums(first.shell.lanthanum.water.out.all),colSums(second.shell.lanthanum.water.out.all))),max(c(colSums(first.shell.lanthanum.water.in.all),colSums(second.shell.lanthanum.water.in.all)))),col="red",pch=21,main="Both Shells Crossings")

points(x=c(seq(from=min.1sh.radius,to=max.1sh.radius,by=.05),seq(from=min.2sh.radius,to=max.2sh.radius,by=.05)),y=c(colSums(first.shell.lanthanum.water.in.all),colSums(second.shell.lanthanum.water.in.all)),col="blue",pch=21)




#---!---!---!---!---


#SUPREME IMPORTANCE

first.shell.lanthanum.size.rle<-rle(first.shell.lanthanum.size)


#---!---!---!---!---

#LESSON
#rle() is run length encoding
#it returns a two-component named list, "lengths" for the run lengths, and "values" for the number that is running

#Query it to learn about the run behavior
max(first.shell.lanthanum.size.rle$lengths)  #longest run
length(which(first.shell.lanthanum.size.rle$lengths>20))  #number of runs greater than 0.5 ps
length(which(first.shell.lanthanum.size.rle$lengths>80))  #number of runs greater than 2 ps
length(which(first.shell.lanthanum.size.rle$lengths>100))  #number of runs greater than 2.5 ps
length(which(first.shell.lanthanum.size.rle$values==9))  #how many runs of 9-coordination
length(which(first.shell.lanthanum.size.rle$values==10))  #how many runs of 10-coordination

first.shell.lanthanum.size.rle$lengths[which(first.shell.lanthanum.size.rle$values==9)]
first.shell.lanthanum.size.rle$lengths[which(first.shell.lanthanum.size.rle$values==10)]

#LESSON
# the $ sign gives you named components of a list
# just be sure to spell the name right!
# unsure? use names(object)

#---!---!---!---!---



#CNS
#this is the total time it is a specific CN
length(which(first.shell.lanthanum.size==9))
length(which(first.shell.lanthanum.size==10))

mean(first.shell.lanthanum.size)
sd(first.shell.lanthanum.size)

#PRINT RECORD THE  CN AND CN TOTAL TIME (AS PERCENT OF TOTAL RUN) IN COLUMNS 1-2
#---!---!---!---!---



#MRT

#PARAMETER
#desired t* value * (1 ps / x ps/snapshot)
#defaults to 0.5, 25 fs/shot
tstar<-0.5*(1/.025)

lanthanum.MRT<-mean(first.shell.lanthanum.size)*1000/length(which(first.shell.lanthanum.size.rle$lengths>tstar))

lanthanum.MRT.sd<-sd(first.shell.lanthanum.size)*1000/length(which(first.shell.lanthanum.size.rle$lengths>tstar))

#PRINT AND RECORD THE MRT AND PUT IN THE TABLE
#---!---!---!---!---



#MRT version 2
#this one works a little differently.

#ALGORITHM
# matrix the same size as the distance matrix
# 0 if not in the shell, 1 if it is
# ignore any particles that are never in the shell
# rle() on the columns --
# say you have a run of 20
# this counts as a run of 20, 2 runs of 19, 3 runs of 18, 4 runs of 17...and so on
# house-made functions tabulate these up and add together
# plot the tallies vs run length, and the log of the tallies (plus one, to avoid log(0)), and perform linear regression on the linear portion of the log(tallies+1) vs run length plot


lanthanum.inshell.kdelta<-matrix(data=0,nrow=40000,ncol=216)
lanthanum.inshell.kdelta[which(lanthanum.distances<lanthanum.minimal.crossing.distance)]<-1

lanthanum.inshell.kdelta<-lanthanum.inshell.kdelta[,(which(colSums(lanthanum.inshell.kdelta)>1))]

lanthanum.length.param<-max(colSums(lanthanum.inshell.kdelta))

vector.gen.from.rle<-function(x){c(rev(seq(1:x)),rep(0,lanthanum.length.param-x))}

expo.fitter.from.rle<-function(x){a<-rle(x);b<-rep(0,lanthanum.length.param);for(i in 1:length(which(a$values==1))){b<-b+vector.gen.from.rle(a$lengths[which(a$values==1)[i]])};b}

lanthanum.fitting<-c();for(i in 1:ncol(lanthanum.inshell.kdelta)){lanthanum.fitting<-cbind(lanthanum.fitting,expo.fitter.from.rle(lanthanum.inshell.kdelta[,i]))}
lanthanum.fitting<-rowSums(lanthanum.fitting)

#OPEN GRAPHICS WINDOW
par(mfrow=c(1,2))
plot(x=c(1:lanthanum.length.param),y=lanthanum.fitting,col="hotpink",pch=21,main="Frequencies vs run length")
plot(x=c(1:lanthanum.length.param),y=log(lanthanum.fitting+1),col="hotpink",pch=21,main="Log(frequencies+1) vs run length")

#fit at will: change 15000 to however many points you want to use in the fitting
#PARAMETER
max.fit.lanthanum.mrt2<-15000

lanthanum.lm<-lm(log(lanthanum.fitting+1)[1:max.fit.lanthanum.mrt2]~c(1:max.fit.lanthanum.mrt2))
summary(lanthanum.lm)


#---!---!---!---!---

#LESSON
#lm(y~x) is linear regression
# this produces a named list with a summary method
#you'll want the second (slope) coefficient, the first is intercept

#---!---!---!---!---


#MRT
lanthanum.MRT.2<-.025*(-1)*log(2)/lanthanum.lm$coefficients[2]


#comparison
lanthanum.MRT
lanthanum.MRT.2


#---!---!---!---!---



#MOVING ON TO DISTRIBUTIONS
#FIRST, ESTABLISH HOW TO SPLIT THE SNAPSHOTS UP
#we can already do by coordination number, since we have a vector that we can index: which(first.shell.lanthanum.size==desired.CN)
#now we would like to know which are exchanges, and which are "stable" runs

#Exchange vs Stable

lanthanum.exch.frames<-rep(0,length(first.shell.lanthanum.size.rle$values))

lanthanum.exch.frames[which(first.shell.lanthanum.size.rle$lengths<=20)]=1


#find distinct exchanges

keep.lanthanum.unique<-which(c(0,diff(lanthanum.exch.frames))>0)


#---!---!---!---!---

#LESSON
#diff(x) : takes the differences between values 2 and 1, 3 and 2, 4 and 3, etc in a vector

#---!---!---!---!---


#all short run snapshot index

lanthanum.exch.frames<-rep(lanthanum.exch.frames,c(first.shell.lanthanum.size.rle$lengths))


lanthanum.unique.exch.frames<-rep(0,length(first.shell.lanthanum.size.rle$values))

lanthanum.unique.exch.frames[which(first.shell.lanthanum.size.rle$lengths<=20)[keep.lanthanum.unique]]=1


#short run snapshot index of unique events (see paper)

lanthanum.unique.exch.frames<-rep(lanthanum.unique.exch.frames,c(first.shell.lanthanum.size.rle$lengths))




#---!---!---!---!---



# ----!@#$%  ____ !!!!!POLYHEDRA!!!!! ____  %$#@!----


#OPTIONS

#1. use a workspace with polyhedra loaded

#OR

#2. get data files, useful for matching ONLY

#UNCOMMENT TO LOAD
#nines.vector<-read.table("~/january-11/ninesvector_short",colClasses="numeric")
#tens.vector<-read.table("~/january-11/tensvector_short",colClasses="numeric")
#matchme.9s<-read.table("~/january-11/matchme9s_short",sep=",")
#matchme.10s<-read.table("~/january-11/matchme10s_short",sep=",")



#LOAD POLYHEDRA DATA


lanthanum.pageranks.9s<-read.table("/MyDocuments/acerc/2011-projects/La-forcefit-desktop/barbara/right.unweighted.ninestar")

lanthanum.centralmatch.9s<-lanthanum.pageranks.9s[,10]


#this is the list of polyhedra: named .polygons for backward compatibility with older scripts

lanthanum.nines.polygons<-matchme.9s[match(lanthanum.centralmatch.9s,signif(nines.vector,digits=7))]


#how good was our performance?

lanthanum.nines.match.percentage<-100-(100*length(which(is.na(lanthanum.nines.polygons)))/length(lanthanum.nines.polygons))


lanthanum.pageranks.10s<-read.table("/MyDocuments/acerc/2011-projects/La-forcefit-desktop/barbara/right.unweighted.tenstar")

lanthanum.centralmatch.10s<-lanthanum.pageranks.10s[,11]


#second list of polyhedra

lanthanum.tens.polygons<-matchme.10s[match(lanthanum.centralmatch.10s,signif(tens.vector,digits=7))]


#performance

lanthanum.tens.match.percentage<-100-(100*length(which(is.na(lanthanum.tens.polygons)))/length(lanthanum.tens.polygons))




#the rest is just statistics




#OVERALL DISTRIBUTIONS

noquote(cbind(levels(as.factor(lanthanum.nines.polygons)),tabulate(as.factor(lanthanum.nines.polygons)),paste(100*tabulate(as.factor(lanthanum.nines.polygons))/length(lanthanum.nines.polygons),"%")))


#---!---!---!---!---

#LESSON
#formatting is tricky
#check out the help files: ?format , ?pretty , and ?noquote , for example

#---!---!---!---!---


noquote(cbind(levels(as.factor(lanthanum.tens.polygons)),tabulate(as.factor(lanthanum.tens.polygons)),paste(100*tabulate(as.factor(lanthanum.tens.polygons))/length(lanthanum.tens.polygons),"%")))



#WHEN 9-COORDINATED

noquote(cbind(levels(as.factor(lanthanum.nines.polygons[which(first.shell.lanthanum.size==9)])),tabulate(as.factor(lanthanum.nines.polygons[which(first.shell.lanthanum.size==9)])),paste(100*tabulate(as.factor(lanthanum.nines.polygons[which(first.shell.lanthanum.size==9)]))/length(lanthanum.nines.polygons[which(first.shell.lanthanum.size==9)]),"%")))

noquote(cbind(levels(as.factor(lanthanum.tens.polygons[which(first.shell.lanthanum.size==9)])),tabulate(as.factor(lanthanum.tens.polygons[which(first.shell.lanthanum.size==9)])),paste(100*tabulate(as.factor(lanthanum.tens.polygons[which(first.shell.lanthanum.size==9)]))/length(lanthanum.tens.polygons[which(first.shell.lanthanum.size==9)]),"%")))

#PRINT AND RECORD % FOR POLYGONS FOR THE CN9 POLYGONS (THE FIRST noquote)

#WHEN 10-COORDINATED

#9s when 10
noquote(cbind(levels(as.factor(lanthanum.nines.polygons[which(first.shell.lanthanum.size==10)])),tabulate(as.factor(lanthanum.nines.polygons[which(first.shell.lanthanum.size==10)])),paste(100*tabulate(as.factor(lanthanum.nines.polygons[which(first.shell.lanthanum.size==10)]))/length(lanthanum.nines.polygons[which(first.shell.lanthanum.size==10)]),"%")))

#10s when 10
noquote(cbind(levels(as.factor(lanthanum.tens.polygons[which(first.shell.lanthanum.size==10)])),tabulate(as.factor(lanthanum.tens.polygons[which(first.shell.lanthanum.size==10)])),paste(100*tabulate(as.factor(lanthanum.tens.polygons[which(first.shell.lanthanum.size==10)]))/length(lanthanum.tens.polygons[which(first.shell.lanthanum.size==10)]),"%")))

#PRINT AND RECORD % FOR POLYGONS FOR THE CN10 POLYGONS

#EXCHANGE ONLY: Meaningless (?) for lanthanum, but potentially useful for other ions with a more frequent exchange rate.

#9s when 9 and exchange (10 - 9 - 10 dissociative, the polyhedra of the 9 intermediate)
noquote(cbind(levels(as.factor(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))])),tabulate(as.factor(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))])),paste(100*tabulate(as.factor(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))]))/length(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))]),"%")))

#10s when 10 and exchange (9 - 10 - 9 , so associative, the polyhedra of the 10 intermediate)
noquote(cbind(levels(as.factor(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))])),tabulate(as.factor(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))])),paste(100*tabulate(as.factor(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))]))/length(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==9),which(lanthanum.exch.frames==1))]),"%")))

#9s when 10 and exchange ( 9 - 10 - 9, underlying 9 the underlying 9  polyhedra of the 10 intermediate)
noquote(cbind(levels(as.factor(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))])),tabulate(as.factor(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))])),paste(100*tabulate(as.factor(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))]))/length(lanthanum.nines.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))]),"%")))


#---!---!---!---!---

#LESSON
#intersect(x,y) gives elements in both x and y
#VERY USEFUL but takes two arguments only
#if you want to know more, nest
# intersect(x,intersect(y,z))
# intersect(intersect(x,y),z)
# these are equivalent

#---!---!---!---!---

#10s when 9 and exchange (10 - 9 - 10 and the overlying of the 9 intermediate)
noquote(cbind(levels(as.factor(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))])),tabulate(as.factor(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))])),paste(100*tabulate(as.factor(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))]))/length(lanthanum.tens.polygons[intersect(which(first.shell.lanthanum.size==10),which(lanthanum.exch.frames==1))]),"%")))




#---!---!---!---!---




#POLYHEDRON LIFETIMES

#ALGORITHM
#how to calculate the lifetime of a polyhedron
# find the indices of CN change points from the size.rle
# find which polyhedra you care to look at
# do a little overhead work -- find how many runs longer than 100 there are, and the longest possible polyhedra run (so this won't fail)
# initialize a tally of run lengths for each polyhedron
# loop: for each run over 100 snapshots in length,
# 1. rle the polyhedra during that run
# 2. tabulate the run lengths for each polyhedra type
# 3. add on to the tally and repeat
# when done plot log(tally) vs length, compute slope of linear region


first.shell.lanthanum.size.rle.endpt<-cumsum(first.shell.lanthanum.size.rle$lengths)


#---!---!---!---!---

#LESSON
#cumsum() is cumulative sum
#try
#cumsum(c(1,2,3,17))
#cumsum(1,2,3,17) fails

#---!---!---!---!---


first.shell.lanthanum.size.rle.startpt<-c(1,cumsum(first.shell.lanthanum.size.rle$lengths)+1)

first.shell.lanthanum.size.rle.startpt<-first.shell.lanthanum.size.rle.startpt[-length(first.shell.lanthanum.size.rle.startpt)]

lanthanum.coordination.number.change.count<-length(which(first.shell.lanthanum.size.rle$lengths>100))

lanthanum.cn.nines.blocks<-intersect(which(first.shell.lanthanum.size.rle$lengths>100),which(first.shell.lanthanum.size.rle$values==9))

lanthanum.cn.tens.blocks<-intersect(which(first.shell.lanthanum.size.rle$lengths>100),which(first.shell.lanthanum.size.rle$values==10))


#PARAMETER
#maximum run length on polyhedra, or set your own
max.poly.run.lanthanum<-max(rle(lanthanum.nines.polygons)$lengths)


lanthanum.monosquanti.runs<-rep(0,max.poly.run.lanthanum)
lanthanum.triaug.runs<-rep(0,max.poly.run.lanthanum)
#lanthanum.other.runs<-rep(0,max.poly.run.lanthanum)


#---!---!---!---!--- LOOP

for(i in 1:length(lanthanum.cn.nines.blocks)){
	
	lanthanum.nines.polygon.temp.rle<-rle(lanthanum.nines.polygons[first.shell.lanthanum.size.rle.startpt[lanthanum.cn.nines.blocks[i]]:first.shell.lanthanum.size.rle.endpt[lanthanum.cn.nines.blocks[i]]])
	
	lanthanum.monosquanti.temp.tally<-tabulate(lanthanum.nines.polygon.temp.rle$lengths[which(lanthanum.nines.polygon.temp.rle$values=="monocapped square antiprism")])
	
	lanthanum.triaug.temp.tally<-tabulate(lanthanum.nines.polygon.temp.rle$lengths[which(lanthanum.nines.polygon.temp.rle$values=="triaugmented triangular prism")])
	
	#lanthanum.other.temp.tally<-tabulate(lanthanum.nines.polygon.temp.rle$lengths[which(lanthanum.nines.polygon.temp.rle$values=="other")])
	
	lanthanum.monosquanti.runs<-lanthanum.monosquanti.runs+c(lanthanum.monosquanti.temp.tally,rep(0,max.poly.run.lanthanum-length(lanthanum.monosquanti.temp.tally)))
	
	lanthanum.triaug.runs<-lanthanum.triaug.runs+c(lanthanum.triaug.temp.tally,rep(0,max.poly.run.lanthanum-length(lanthanum.triaug.temp.tally)))
	
	#lanthanum.other.runs<-#lanthanum.other.runs+c(#lanthanum.other.temp.tally,rep(0,max.poly.run.lanthanum-length(#lanthanum.other.temp.tally)))
	
	}
		
#---!---!---!---!--- END LOOP


#OPEN GRAPHICS WINDOW
par(mfrow=c(2,2))
plot(x=1:max.poly.run.lanthanum,y=lanthanum.monosquanti.runs,col="hotpink",pch=21,main="9 co, monosquanti")
plot(x=1:max.poly.run.lanthanum,y=lanthanum.triaug.runs,col="hotpink",pch=21,main="9 co, triaug")
#plot(x=1:max.poly.run.lanthanum,y=lanthanum.other.runs,col="hotpink",pch=21,main="9 co, other")

plot(x=1:max.poly.run.lanthanum,y=log(lanthanum.monosquanti.runs+1),col="hotpink",pch=21,main="9 co, logmonosquanti")
plot(x=1:max.poly.run.lanthanum,y=log(lanthanum.triaug.runs+1),col="hotpink",pch=21,main="9 co, logtriaug")
#plot(x=1:max.poly.run.lanthanum,y=log(lanthanum.other.runs+1),col="hotpink",pch=21,main="9 co, logother")

#PARAMETER
#CURRENTLY VISUAL INSPECTION REQUIRED
#how many data points to count for your linear regression? defaults to 10
lanthanum.monosquanti.lm.stop<-10
	
lanthanum.monosquanti.runs.loglm<-lm(log(lanthanum.monosquanti.runs+1)[1:lanthanum.monosquanti.lm.stop]~c(1:lanthanum.monosquanti.lm.stop))

lanthanum.monosquanti.halflife<-(-25)*log(2)/lanthanum.monosquanti.runs.loglm$coefficients[2]

#PARAMETER
#CURRENTLY VISUAL INSPECTION REQUIRED
#how many data points to count for your linear regression? defaults to 40
lanthanum.triaug.lm.stop<-40

lanthanum.triaug.runs.loglm<-lm(log(lanthanum.triaug.runs+1)[1:lanthanum.triaug.lm.stop]~c(1:lanthanum.triaug.lm.stop))

lanthanum.triaug.halflife<-(-25)*log(2)/lanthanum.triaug.runs.loglm$coefficients[2]



lanthanum.bisquanti.runs<-rep(0,max.poly.run.lanthanum)
lanthanum.tetraaug.runs<-rep(0,max.poly.run.lanthanum)
#lanthanum.other.runs<-rep(0,max.poly.run.lanthanum)


#---!---!---!---!--- LOOP

for(i in 1:length(lanthanum.cn.tens.blocks)){
	
	lanthanum.tens.polygon.temp.rle<-rle(lanthanum.tens.polygons[first.shell.lanthanum.size.rle.startpt[lanthanum.cn.tens.blocks[i]]:first.shell.lanthanum.size.rle.endpt[lanthanum.cn.tens.blocks[i]]])
	
	lanthanum.bisquanti.temp.tally<-tabulate(lanthanum.tens.polygon.temp.rle$lengths[which(lanthanum.tens.polygon.temp.rle$values=="bicapped square antiprism")])
	
	lanthanum.tetraaug.temp.tally<-tabulate(lanthanum.tens.polygon.temp.rle$lengths[which(lanthanum.tens.polygon.temp.rle$values=="tetraaugmented v4")])
	
	#lanthanum.other.temp.tally<-tabulate(lanthanum.tens.polygon.temp.rle$lengths[which(lanthanum.tens.polygon.temp.rle$values=="other")])
	
	lanthanum.bisquanti.runs<-lanthanum.bisquanti.runs+c(lanthanum.bisquanti.temp.tally,rep(0,max.poly.run.lanthanum-length(lanthanum.bisquanti.temp.tally)))
	
	lanthanum.tetraaug.runs<-lanthanum.tetraaug.runs+c(lanthanum.tetraaug.temp.tally,rep(0,max.poly.run.lanthanum-length(lanthanum.tetraaug.temp.tally)))
	
	#lanthanum.other.runs<-#lanthanum.other.runs+c(#lanthanum.other.temp.tally,rep(0,max.poly.run.lanthanum-length(#lanthanum.other.temp.tally)))
	
	}

#---!---!---!---!--- END LOOP


#OPEN GRAPHICS WINDOW

par(mfrow=c(2,2))
plot(x=1:max.poly.run.lanthanum,y=lanthanum.bisquanti.runs,col="hotpink",pch=21,main="10 co, bisquanti")
plot(x=1:max.poly.run.lanthanum,y=lanthanum.tetraaug.runs,col="hotpink",pch=21,main="10 co, tetraaug")
#plot(x=1:max.poly.run.lanthanum,y=lanthanum.other.runs,col="hotpink",pch=21,main="10 co, other")

plot(x=1:max.poly.run.lanthanum,y=log(lanthanum.bisquanti.runs+1),col="hotpink",pch=21,main="10 co, logbisquanti")
plot(x=1:max.poly.run.lanthanum,y=log(lanthanum.tetraaug.runs+1),col="hotpink",pch=21,main="10 co, logtetraaug")


#PARAMETER
#CURRENTLY VISUAL INSPECTION REQUIRED
#how many data points to count for your linear regression? defaults to 18
lanthanum.bisquanti.lm.stop<-18
	
lanthanum.bisquanti.runs.loglm<-lm(log(lanthanum.bisquanti.runs+1)[1:lanthanum.bisquanti.lm.stop]~c(1:lanthanum.bisquanti.lm.stop))

lanthanum.bisquanti.halflife<-(-25)*log(2)/lanthanum.bisquanti.runs.loglm$coefficients[2]


#PARAMETER
#CURRENTLY VISUAL INSPECTION REQUIRED
#how many data points to count for your linear regression? defaults to 2
lanthanum.tetraaug.lm.stop<-2

lanthanum.tetraaug.runs.loglm<-lm(log(lanthanum.tetraaug.runs+1)[1:lanthanum.tetraaug.lm.stop]~c(1:lanthanum.tetraaug.lm.stop))

lanthanum.tetraaug.halflife<-(-25)*log(2)/lanthanum.tetraaug.runs.loglm$coefficients[2]


#PRINT OUT ALL HALFLIFE INFO AND RECORD IN TABLE


#---!---!---!---!---




#DEGREES AND DIPOLES

relevant.lanthanum.distances<-NULL;for(i in 1:40){relevant.lanthanum.distances<-cbind(relevant.lanthanum.distances,c(lanthanum.distances[c((lanthanum.dista.dex[,i]-1)*40000+c(1:40000))]),deparse.level=0)}

#average distance in angstroms of the closest 40 waters
lanthanum.relative.mean.distances<-apply(relevant.lanthanum.distances,2,mean)

#for absolute distances
lanthanum.distance.vector<-seq(from=(floor(10*min(relevant.lanthanum.distances))/10),to=quantile(x=relevant.lanthanum.distances,probs=.95),by=.05)


#---!---!---!---!---

#LESSON
#automatically choosing a range for the exploration of degree and dipole data by distance
#quantile() is used to get a 95% probability value, so the tails of our plots are good and not based on few data points (thus subject to scatter)
#floor() is used to round down to the nearest .1 A (so we don't start at, say, 2.03)

#---!---!---!---!---


lanthanum.degrees<-read.table("/MyDocuments/acerc/2011-projects/La-forcefit-desktop/barbara/all.degrees")
lanthanum.degrees<-as.matrix(lanthanum.degrees)

relevant.lanthanum.degrees<-NULL;for(i in 1:40){relevant.lanthanum.degrees<-cbind(relevant.lanthanum.degrees,c(lanthanum.degrees[c((lanthanum.dista.dex[,i]-1)*40000+c(1:40000))]),deparse.level=0)}

lanthanum.relative.mean.degrees<-apply(relevant.lanthanum.degrees,2,mean)

lanthanum.degrees.by.distance<-rep(0,(length(lanthanum.distance.vector)-1))

for(i in 1:length(lanthanum.degrees.by.distance)){lanthanum.degrees.by.distance[i]<-mean(relevant.lanthanum.degrees[intersect(which(relevant.lanthanum.distances>lanthanum.distance.vector[i]),which(relevant.lanthanum.distances<=lanthanum.distance.vector[i+1]))])}


#OPEN GRAPHICS WINDOW
plot(x=lanthanum.distance.vector[2:length(lanthanum.distance.vector)],y=lanthanum.degrees.by.distance,pch=21,col="hotpink",main="lanthanum Degrees By Distance")
points(x=lanthanum.relative.mean.distances,y=lanthanum.relative.mean.degrees,pch=22,col="blue")

#WANT CSV FILE FOR DEGREE AS A FUNCTION OF DISTANCE
	
#---!---!---!---!---


lanthanum.dipoles<-read.table("/MyDocuments/acerc/2011-projects/La-forcefit-desktop/barbara/dipole.cosines")
lanthanum.dipoles<-as.matrix(lanthanum.dipoles)

relevant.lanthanum.dipoles<-NULL;for(i in 1:40){relevant.lanthanum.dipoles<-cbind(relevant.lanthanum.dipoles,c(lanthanum.dipoles[c((lanthanum.dista.dex[,i]-1)*40000+c(1:40000))]),deparse.level=0)}

lanthanum.relative.mean.dipoles<-apply(acos(relevant.lanthanum.dipoles),2,mean)*(180/pi)

lanthanum.dipoles.by.distance<-rep(0,(length(lanthanum.distance.vector)-1))

for(i in 1:length(lanthanum.dipoles.by.distance)){lanthanum.dipoles.by.distance[i]<-mean(acos(relevant.lanthanum.dipoles[intersect(which(relevant.lanthanum.distances>lanthanum.distance.vector[i]),which(relevant.lanthanum.distances<=lanthanum.distance.vector[i+1]))]))}

lanthanum.dipoles.by.distance<-lanthanum.dipoles.by.distance*(180/pi)

#OPEN GRAPHICS WINDOW
plot(x=lanthanum.distance.vector[2:length(lanthanum.distance.vector)],y=lanthanum.dipoles.by.distance,pch=21,col="hotpink",main="lanthanum Dipoles By Distance")
points(x=lanthanum.relative.mean.distances,y=lanthanum.relative.mean.dipoles,pch=22,col="blue")

3WANT CSV FILE FOR DIPOLE AS A FUNCTION OF DISTANCE


#---!---!---!---!---




#T-TESTS
#the t test compares two distributions to see if they have the same mean.
#the hypothesis that the two distributions do have the same mean is the "null hypothesis"
#the p value is the probability that we are wrong if we reject the null hypothesis
#most people are satisfied with p < .05 , that is, a 5% chance of being wrong.
#we may be tighter if we so choose.

#ALGORITHM
#do t tests on water 1 vs all waters in the "relevant" matrix further out (water 2, water 3, ...)
#repeat for water 2 (water 3, water 4, ....)
#repeat until there aren't any more
#store as a matrix
#make that matrix into a graph: edges are those pairs that failed the t test, with p > .05
#use cluster analysis to find the statistically similar waters.

library(igraph)

lanthanum.shellsize.9.degree.ttests<-matrix(data=0,nrow=20,ncol=20)
for(i in 1:19){for(j in (i+1):20){
		lanthanum.shellsize.9.degree.ttests[i,][j]=t.test(x=relevant.lanthanum.degrees[,i][which(first.shell.lanthanum.size==9)],y=relevant.lanthanum.degrees[,j][which(first.shell.lanthanum.size==9)])$p.value
		}}
clusters(graph(edges=c(cbind(which(lanthanum.shellsize.9.degree.ttests>.05)%%20,(which(lanthanum.shellsize.9.degree.ttests>.05)%/%20)+1))-1,n=20))


lanthanum.shellsize.10.degree.ttests<-matrix(data=0,nrow=20,ncol=20)
for(i in 1:19){for(j in (i+1):20){
		lanthanum.shellsize.10.degree.ttests[i,][j]=t.test(x=relevant.lanthanum.degrees[,i][which(first.shell.lanthanum.size==10)],y=relevant.lanthanum.degrees[,j][which(first.shell.lanthanum.size==10)])$p.value
		}}
clusters(graph(edges=c(cbind(which(lanthanum.shellsize.10.degree.ttests>.05)%%20,(which(lanthanum.shellsize.10.degree.ttests>.05)%/%20)+1))-1,n=20))


lanthanum.shellsize.9v10.degree.ttests<-rep(0,20)
for(i in 1:20){
		lanthanum.shellsize.9v10.degree.ttests[i]=t.test(x=relevant.lanthanum.degrees[,i][which(first.shell.lanthanum.size==10)],y=relevant.lanthanum.degrees[,i][which(first.shell.lanthanum.size==9)])$p.value
		}
		
		

#here, we just compare each water (1st to 1st, 2nd to 2nd, 3rd to 3rd...) in the cases of different polyhedra.


lanthanum.degree.9.polyhedra.ttests<-rep(0,20);for(i in 1:20){lanthanum.degree.9.polyhedra.ttests[i]<-t.test(x=relevant.lanthanum.degrees[,i][intersect(which(lanthanum.exch.frames==0),intersect(which(first.shell.lanthanum.size==9),which(lanthanum.nines.polygons=="monocapped square antiprism")))],y=relevant.lanthanum.degrees[,i][intersect(which(lanthanum.exch.frames==0),intersect(which(first.shell.lanthanum.size==9),which(lanthanum.nines.polygons=="triaugmented triangular prism")))])$p.value}

lanthanum.degree.10.polyhedra.ttests<-rep(0,20);for(i in 1:20){lanthanum.degree.10.polyhedra.ttests[i]<-t.test(x=relevant.lanthanum.degrees[,i][intersect(which(lanthanum.exch.frames==0),intersect(which(first.shell.lanthanum.size==10),which(lanthanum.tens.polygons=="bicapped square antiprism")))],y=relevant.lanthanum.degrees[,i][intersect(which(lanthanum.exch.frames==0),intersect(which(first.shell.lanthanum.size==10),which(lanthanum.tens.polygons=="tetraaugmented v4")))])$p.value}

