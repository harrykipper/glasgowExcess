### Creates agents matching totals in Census with accurate age distribution
create_agents<-function(x,s){
  val<-ifelse(s=="M",3,4)
  age<-round(runif(x[val],x[1],x[2]))
  age<-as.data.frame(age)
  age$sex<-s
  # age$class<-NULL
  # age$occupation<-NULL
  rbind(agents,age)
}

lpl_pop<-read.csv("../liverpool/liverpool-age_classes_1951.csv")
gla_pop<-read.csv("../glasgow/glasgow-age_classes_1951.csv")

agents<-data.frame()
lpl_agents<-data.frame()
gla_agents<-data.frame()
fava<-data.frame()

## Create Liverpool ================
maschi<-apply(lpl_pop,1,create_agents,s="M")
femine<-apply(lpl_pop,1,create_agents,s="F")
for(i in 1:length(maschi)){
  lpl_agents<-rbind(lpl_agents,as.data.frame(maschi[i]))
}
for(i in 1:length(femine)){
  lpl_agents<-rbind(lpl_agents,as.data.frame(femine[i]))
}

## Create Glasgow ==========================
maschi<-apply(gla_pop,1,create_agents,s="M")
femine<-apply(gla_pop,1,create_agents,s="F")

for(i in 1:length(maschi)){
  gla_agents<-rbind(gla_agents,as.data.frame(maschi[i]))
}
for(i in 1:length(femine)){
  gla_agents<-rbind(gla_agents,as.data.frame(femine[i]))
}


# ========| Assign an occupation to people. Liverpool |==============================
work_lpl<-read.csv("../liverpool/liverpool-occupation_1951_clean.csv")

## Students and retired 
students<-work_lpl[93,3:4]
retired<-work_lpl[94,3:4]

lpl_agents$idx<-row.names(lpl_agents)
retM<-lpl_agents[lpl_agents$age>66 & lpl_agents$sex=="M",][1:as.numeric(retired[1]),]
lpl_agents<-lpl_agents[!lpl_agents$idx %in% retM$idx,]
retF<-lpl_agents[lpl_agents$age>66 & lpl_agents$sex=="F",][1:as.numeric(retired[2]),]
lpl_agents<-lpl_agents[!lpl_agents$idx %in% retF$idx,]
stuM<-lpl_agents[lpl_agents$age>14 & lpl_agents$age<19 & lpl_agents$sex=="M",][1:as.numeric(students[1]),]
lpl_agents<-lpl_agents[!lpl_agents$idx %in% stuM$idx,]
stuF<-lpl_agents[lpl_agents$age>14 & lpl_agents$age<19 & lpl_agents$sex=="F",][1:as.numeric(students[2]),]
lpl_agents<-lpl_agents[!lpl_agents$idx %in% stuF$idx,]

retM$occupation<-"Retired"
retF$occupation<-"Retired"
stuM$occupation<-"Student"
stuF$occupation<-"Student"


## We assume that ppl with age > 66 who are not retired fall 
## in the "No gainful occupation" category
elderUnocc<-lpl_agents[lpl_agents$age>66,]
elderUnocc$occupation<-work_lpl[95,2]
lpl_agents<-lpl_agents[!lpl_agents$idx %in% elderUnocc$idx,]
work_lpl[95,]$M<-work_lpl[95,]$M - nrow(elderUnocc[elderUnocc$sex=="M",])
work_lpl[95,]$F<-work_lpl[95,]$F - nrow(elderUnocc[elderUnocc$sex=="F",])

# We subset the remaining working age ppl, of 15 and above
lpl_occ<-lpl_agents[lpl_agents$age>14,]
# Lpl_agents now contains children, we'll decide later who goes to school and who doesn't
lpl_agents<-lpl_agents[!lpl_agents$idx %in% lpl_occ$idx,] 
lpl_agents$occupation<-"CHILD"
# We shuffle the dataset
lpl_occ=lpl_occ[sample(1:nrow(lpl_occ)),]

lpl_agents_occupations<-data.frame()

# This assigns a random job to working age people
# producing the totals specified in work_lpl
assign_occupation<-function(x){
  this<-data.frame()
  m<-as.numeric(x[3])
  f<-as.numeric(x[4])
  t<-x[2]
  occM<-lpl_occ[lpl_occ$sex=="M",][1:m,]
  occF<-lpl_occ[lpl_occ$sex=="F",][1:f,]
  this<-rbind(occM,occF)
  this$occupation<-t
  rbind(lpl_agents_occupations,this)
}
lpl_agents_occupations<-apply(work_lpl,1,assign_occupation)

liverpool_workers<-data.frame()
for(i in 1:length(lpl_agents_occupations)){
  liverpool_workers<-rbind(liverpool_workers,as.data.frame(lpl_agents_occupations[i]))
}

# Now we put everything together
lpl_agents<-rbind(lpl_agents,retM,retF,stuM,stuF,liverpool_workers)
write.csv(lpl_agents,"liverpool_agents.csv",row.names = FALSE)

######################### End of ##########################################