library(ggplot2)

# Initialise Varible ---------------------------------------------------------------

path <- "./Data/"

cfileCED <-read.csv(paste(path,"completedCED.csv", sep =""))
pfileCED <-read.csv(paste(path,"progressCED.csv", sep = ""))
afileCED <-read.csv(paste(path,"allcollectionCED.csv", sep = ""))
ufileCED <-read.csv(paste(path,"usercollectionCED.csv", sep = ""))

cfileEEA <-read.csv(paste(path,"completedEEA.csv", sep = ""))
pfileEEA <-read.csv(paste(path,"progressEEA.csv", sep = ""))
afileEEA <-read.csv(paste(path,"allcollectionEEA.csv", sep = ""))
ufileEEA <-read.csv(paste(path,"usercollectionEEA.csv", sep = ""))

cfileEEC <-read.csv(paste(path,"completedEEC.csv", sep = ""))
pfileEEC <-read.csv(paste(path,"progressEEC.csv", sep = ""))
afileEEC <-read.csv(paste(path,"allcollectionEEC.csv", sep = ""))
ufileEEC <-read.csv(paste(path,"usercollectionEEC.csv", sep = ""))

cfileMD <- read.csv(paste(path,"completedMD.csv", sep = ""))
pfileMD <-read.csv(paste(path,"progressMD.csv", sep = ""))
afileMD <-read.csv(paste(path,"allcollectionMD.csv", sep = ""))
ufileMD <-read.csv(paste(path,"usercollectionMD.csv", sep = ""))

cfileFLE <-read.csv(paste(path,"completedFLE.csv", sep = ""))
pfileFLE <-read.csv(paste(path,"progressFLE.csv", sep = ""))
afileFLE <-read.csv(paste(path,"allcollectionFLE.csv", sep = ""))
ufileFLE <-read.csv(paste(path,"usercollectionFLE.csv", sep = ""))

cfileNQ <-read.csv(paste(path,"completedNQ.csv", sep = ""))
pfileNQ <-read.csv(paste(path,"progressNQ.csv", sep = ""))
afileNQ <-read.csv(paste(path,"allcollectionNQ.csv", sep = ""))
ufileNQ <-read.csv(paste(path,"usercollectionNQ.csv", sep = ""))

cfileOAT <-read.csv(paste(path,"completedOAT.csv", sep = ""))
pfileOAT <-read.csv(paste(path,"progressOAT.csv", sep = ""))
afileOAT <-read.csv(paste(path,"allcollectionOAT.csv", sep = ""))
ufileOAT <-read.csv(paste(path,"usercollectionOAT.csv", sep = ""))

cfileREI <-read.csv(paste(path,"completedREI.csv", sep = ""))
pfileREI <-read.csv(paste(path,"progressREI.csv", sep = ""))
afileREI <-read.csv(paste(path,"allcollectionREI.csv", sep = ""))
ufileREI <-read.csv(paste(path,"usercollectionREI.csv", sep = ""))

cfileRL <-read.csv(paste(path,"completedRL.csv", sep = ""))
pfileRL <-read.csv(paste(path,"progressRL.csv", sep = ""))
afileRL <-read.csv(paste(path,"allcollectionRL.csv", sep = ""))
ufileRL <-read.csv(paste(path,"usercollectionRL.csv", sep = ""))

CED <- merge(x = cfileCED, y = pfileCED, all = TRUE)
EEA <- merge(x = cfileEEA, y = pfileEEA, all = TRUE)
EEC <- merge(x = cfileEEC, y = pfileEEC, all = TRUE)
MD <- merge(x = cfileMD, y = pfileMD, all = TRUE)
FLE <- merge(x = cfileFLE, y = pfileFLE, all = TRUE)
NQ <- merge(x = cfileNQ, y = pfileNQ, all = TRUE)
OAT <- merge(x = cfileOAT, y = pfileOAT, all = TRUE)
REI <- merge(x = cfileREI, y = pfileREI, all = TRUE)
RL <- merge(x = cfileRL, y = pfileRL, all = TRUE)

test <- Reduce(function(x, y) merge(x, y, all=TRUE), list(CED, EEA, EEC, MD, FLE, NQ, OAT, REI, RL))


ggplot(data = CED) +
	geom_smooth(mapping = aes(x = average_score, y = count, color = status))

