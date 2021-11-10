### Glasgow wards
glasgow<-read.csv("../glasgow_wards.csv")
glasgow$houses<-round(glasgow$pop1951/glasgow$ppr/glasgow$roomsPerHouse)
glasgow$pph<-glasgow$pop1951/glasgow$houses
write.csv(glasgow,"../glasgow/glasgow_wards_elaborated.csv")
