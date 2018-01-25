odi = read.csv("C://Users//Administrator//Desktop//odi-batting11.csv")


colSums(is.na(odi))

View(odi[is.na(odi$Runs),])


library(dplyr)
avg_mark = odi %>% filter(Player == 'Mark E Waugh')

avg_mark %>% summarise(avg_runs = mean(Runs,na.rm = T))

odi[odi$Player == 'Mark E Waugh' & is.na(odi$Runs),]$Runs = 36

odi[odi$Player == 'Mark E Waugh' & odi$Runs == 36,]


odi[odi$Player == 'Mark E Waugh' & is.na(odi$Runs),]
