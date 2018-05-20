library("ggplot2")

# Initialise Varible ---------------------------------------------------------------

path <- "./Data/"

cfileCED <-read.csv(paste(path,"completedCED.csv"))
pfileCED <-read.csv(paste(path,"progressCED.csv"))
afileCED <-read.csv(paste(path,"allcollectionCED.csv"))
ufileCED <-read.csv(paste(path,"usercollectionCED.csv"))

cfileEEA <-read.csv(paste(path,"completedEEA.csv"))
pfileEEA <-read.csv(paste(path,"progressEEA.csv"))
afileEEA <-read.csv(paste(path,"allcollectionEEA.csv"))
ufileEEA <-read.csv(paste(path,"usercollectionEEA.csv"))

cfileEEC <-read.csv(paste(path,"completedEEC.csv"))
pfileEEC <-read.csv(paste(path,"progressEEC.csv"))
afileEEC <-read.csv(paste(path,"allcollectionEEC.csv"))
ufileEEC <-read.csv(paste(path,"usercollectionEEC.csv"))

cfileMD <- read.csv(paste(path,"completedMD.csv"))
pfileMD <-read.csv(paste(path,"progressMD.csv"))
afileMD <-read.csv(paste(path,"allcollectionMD.csv"))
ufileMD <-read.csv(paste(path,"usercollectionMD.csv"))

cfileFLE <-read.csv(paste(path,"completedFLE.csv"))
pfileFLE <-read.csv(paste(path,"progressFLE.csv"))
afileFLE <-read.csv(paste(path,"allcollectionFLE.csv"))
ufileFLE <-read.csv(paste(path,"usercollectionFLE.csv"))

cfileNQ <-read.csv(paste(path,"completedNQ.csv"))
pfileNQ <-read.csv(paste(path,"progressNQ.csv"))
afileNQ <-read.csv(paste(path,"allcollectionNQ.csv"))
ufileNQ <-read.csv(paste(path,"usercollectionNQ.csv"))

cfileOAT <-read.csv(paste(path,"completedOAT.csv"))
pfileOAT <-read.csv(paste(path,"progressOAT.csv"))
afileOAT <-read.csv(paste(path,"allcollectionOAT.csv"))
ufileOAT <-read.csv(paste(path,"usercollectionOAT.csv"))

cfileREI <-read.csv(paste(path,"completedREI.csv"))
pfileREI <-read.csv(paste(path,"progressREI.csv"))
afileREI <-read.csv(paste(path,"allcollectionREI.csv"))
ufileREI <-read.csv(paste(path,"usercollectionREI.csv"))

cfileRL <-read.csv(paste(path,"completedRL.csv"))
pfileRL <-read.csv(paste(path,"progressRL.csv"))
afileRL <-read.csv(paste(path,"allcollectionRL.csv"))
ufileRL <-read.csv(paste(path,"usercollectionRL.csv"))

test1 <- read.csv("./Data/completedMD.csv")

ggplot(data = cfileMD) +
  geom_point(mapping = aes(x = average_score, y = total_score, color = competency_code))
