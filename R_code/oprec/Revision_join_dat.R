##############################################################################
###Estudio estadistico OPREC###########################################
######################Autor: Juan Pablo Cely#################################
######################19/02/2020#################################

#https://dplyr.tidyverse.org/reference/join.html
r1 <- read_excel(paste("rev_join.xlsx",sep=""),1)
r2 <- read_excel(paste("rev_join.xlsx",sep=""),2)


verifi1<-r1 %>% inner_join(r2, by="a")
verifi2<-r1 %>% left_join(r2, by="a")
verifi3<-r1 %>% right_join(r2, by="a")
verifi4<-r1 %>% full_join(r2, by="a")
verifi5<-r1 %>% semi_join(r2, by="a")
verifi6<-r1 %>% anti_join(r2, by="a")
verifi7<-r1 %>% nest_join(r2, by="a")

verifi1[duplicated(verifi1$a), ]
verifi5[duplicated(verifi5$a), ]

nrow(verifi1[duplicated(verifi1$a), ])
nrow(verifi5[duplicated(verifi5$a), ])


aa<-distinct(verifi5, a)

data2232<-group_by(verifi5, a)%>%summarise(b=b[1])

##################Borrar duplicados y dejar solo 1
bor_dup5<-verifi5 %>%
  group_by(a) %>%
  slice(1)


bor_dup1<-verifi1 %>%
  group_by(a) %>%
  slice(1)



######Completar variables



r3 <- read_excel(paste("rev_join.xlsx",sep=""),3)



r3$rr5<-paste(r3$a,r3$d[r3$d!=r3$a],sep="",recycle0 = FALSE)


r3$rr3<-paste(r3$d,r3$a[r3$a!=r3$d],sep="")

r3$rr4<-paste(r3$d,r3$a[r3$a==r3$d],sep="")




r3$a %>% complete(a, nesting(a,d), fill = list(a=d))



dat$a1_t[dat$a1=="A"] <- paste((dat$a1[dat$a1=="A"]),(dat$a1_c[dat$a1=="A"]),sep="_")
paste



dat %>% complete(c(id, c, d), choice) 

a<-r3 %>%
  fill(a,d)



r3$ee <- r3[complete.cases(r3$a,r3$d), ]







r3$a.mean<- ifelse(is.na(r3$a), r3$a)

r3$a.mean<- ifelse(is.na(r3$a), mean(r3$a, na.rm=TRUE),r3$d)

r3$a.mean<- ifelse(is.na(r3$a), all.equal(r3$a, r3$d, na.rm=FALSE),r3$a)

r3$a.mean2<- ifelse(is.na(r3$d), all.equal.environment(r3$d, r3$a.mean, na.rm=TRUE),r3$d)

r3$a.b %>% complete(a, nesting(a,d), fill = list(a=d))


df <- tibble(r3$a, r3$d
)
names(df)
df %>% complete(`r3$a`, nesting(item_id, item_name), fill = list(value1 = 0))

df$`r3$a`

