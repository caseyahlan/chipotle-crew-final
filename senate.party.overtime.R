independents <- data.frame("party" = "I", "n" = "0")

cmd <- 'curl "https://api.propublica.org/congress/v1/115/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd <- straighten(cmd)
str(parsed_cmd)
actual_function <- make_req(parsed_cmd)[[1]]
request.body.list <- content(actual_function())
members.list <- request.body.list$results[[1]]$members
names(members.list) <- NULL
members.json <- toJSON(members.list)
senate.members.115 <- flatten(fromJSON(members.json, flatten = TRUE)) %>% select(party)
senate.members.115$party <- as.factor(unlist(senate.members.115$party))
senate.115 <- tally(group_by(senate.members.115, party)) %>% select(n)
colnames(senate.115)[1] <- "115"

cmd1 <- 'curl "https://api.propublica.org/congress/v1/114/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd1 <- straighten(cmd1)
str(parsed_cmd1)
actual_function1 <- make_req(parsed_cmd1)[[1]]
request.body.list1 <- content(actual_function1())
members.list1 <- request.body.list1$results[[1]]$members
names(members.list1) <- NULL
members.json1 <- toJSON(members.list1)
senate.members.114 <- flatten(fromJSON(members.json1, flatten = TRUE)) %>% select(party)
senate.members.114$party <- as.factor(unlist(senate.members.114$party))
senate.114 <- tally(group_by(senate.members.114, party)) %>% select(n)
colnames(senate.114)[1] <- "114"


cmd2 <- 'curl "https://api.propublica.org/congress/v1/113/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd2 <- straighten(cmd2)
str(parsed_cmd2)
actual_function2 <- make_req(parsed_cmd2)[[1]]
request.body.list2 <- content(actual_function2())
members.list2 <- request.body.list2$results[[1]]$members
names(members.list2) <- NULL
members.json2 <- toJSON(members.list2)
senate.members.113 <- flatten(fromJSON(members.json2, flatten = TRUE)) %>% select(party)
senate.members.113$party <- as.factor(unlist(senate.members.113$party))
senate.113 <- tally(group_by(senate.members.113, party)) %>% select(n)
colnames(senate.113)[1] <- "113"

cmd3 <- 'curl "https://api.propublica.org/congress/v1/112/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd3 <- straighten(cmd3)
str(parsed_cmd3)
actual_function3 <- make_req(parsed_cmd3)[[1]]
request.body.list3 <- content(actual_function3())
members.list3 <- request.body.list3$results[[1]]$members
names(members.list3) <- NULL
members.json3 <- toJSON(members.list3)
senate.members.112 <- flatten(fromJSON(members.json3, flatten = TRUE)) %>% select(party)
senate.members.112$party <- as.factor(unlist(senate.members.112$party))
senate.112 <- tally(group_by(senate.members.112, party)) %>% select(n)
colnames(senate.112)[1] <- "112"


cmd4 <- 'curl "https://api.propublica.org/congress/v1/111/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd4 <- straighten(cmd4)
str(parsed_cmd4)
actual_function4 <- make_req(parsed_cmd4)[[1]]
request.body.list4 <- content(actual_function4())
members.list4 <- request.body.list4$results[[1]]$members
names(members.list4) <- NULL
members.json4 <- toJSON(members.list4)
senate.members.111 <- flatten(fromJSON(members.json4, flatten = TRUE)) %>% select(party) 
senate.members.111$party <- as.factor(unlist(senate.members.111$party))
senate.111a <- tally(group_by(senate.members.111, party))
independents.2 <- data.frame("party" = "I", "n" = "2")
senate.111b <- rbind(senate.111a, independents.2) %>% select(n)
colnames(senate.111b)[1] <- "111"
senate.111 <- senate.111b[c(1,5,4),]

cmd5 <- 'curl "https://api.propublica.org/congress/v1/110/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd5 <- straighten(cmd5)
str(parsed_cmd5)
actual_function5 <- make_req(parsed_cmd5)[[1]]
request.body.list5 <- content(actual_function5())
members.list5 <- request.body.list5$results[[1]]$members
names(members.list5) <- NULL
members.json5 <- toJSON(members.list5)
senate.members.110 <- flatten(fromJSON(members.json5, flatten = TRUE)) %>% select(party) %>% filter(party != "ID") %>% 
  filter(party != "I")
senate.members.110$party <- as.factor(unlist(senate.members.110$party))
senate.110a <- tally(group_by(senate.members.110, party)) 
independents.2 <- data.frame("party" = "I", "n" = "2")
senate.110a <- rbind(senate.110a, independents.2) %>% select(n)
colnames(senate.110a)[1] <- "110"
senate.110 <- senate.110a[c(1,3,2),]

cmd6 <- 'curl "https://api.propublica.org/congress/v1/109/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd6 <- straighten(cmd6)
str(parsed_cmd6)
actual_function6 <- make_req(parsed_cmd6)[[1]]
request.body.list6 <- content(actual_function6())
members.list6 <- request.body.list6$results[[1]]$members
names(members.list6) <- NULL
members.json6 <- toJSON(members.list6)
senate.members.109 <- flatten(fromJSON(members.json6, flatten = TRUE)) %>% select(party)
senate.members.109$party <- as.factor(unlist(senate.members.109$party))
senate.109 <- tally(group_by(senate.members.109, party)) %>% select(n)
colnames(senate.109)[1] <- "109"

cmd7 <- 'curl "https://api.propublica.org/congress/v1/108/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd7 <- straighten(cmd7)
str(parsed_cmd7)
actual_function7 <- make_req(parsed_cmd7)[[1]]
request.body.list7 <- content(actual_function7())
members.list7 <- request.body.list7$results[[1]]$members
names(members.list7) <- NULL
members.json7 <- toJSON(members.list7)
senate.members.108 <- flatten(fromJSON(members.json7, flatten = TRUE)) %>% select(party)
senate.members.108$party <- as.factor(unlist(senate.members.108$party))
senate.108 <- tally(group_by(senate.members.108, party)) %>% select(n)
colnames(senate.108)[1] <- "108"

cmd8 <- 'curl "https://api.propublica.org/congress/v1/107/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd8 <- straighten(cmd8)
str(parsed_cmd8)
actual_function8 <- make_req(parsed_cmd8)[[1]]
request.body.list8 <- content(actual_function8())
members.list8 <- request.body.list8$results[[1]]$members
names(members.list8) <- NULL
members.json8 <- toJSON(members.list8)
senate.members.107 <- flatten(fromJSON(members.json8, flatten = TRUE)) %>% select(party)
senate.members.107$party <- as.factor(unlist(senate.members.107$party))
senate.107 <- tally(group_by(senate.members.107, party)) %>% select(n)
colnames(senate.107)[1] <- "107"

cmd9 <- 'curl "https://api.propublica.org/congress/v1/106/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd9 <- straighten(cmd9)
str(parsed_cmd9)
actual_function9 <- make_req(parsed_cmd9)[[1]]
request.body.list9 <- content(actual_function9())
members.list9 <- request.body.list9$results[[1]]$members
names(members.list9) <- NULL
members.json9 <- toJSON(members.list9)
senate.members.106 <- flatten(fromJSON(members.json9, flatten = TRUE)) %>% select(party)
senate.members.106$party <- as.factor(unlist(senate.members.106$party))
senate.106a <- tally(group_by(senate.members.106, party))
senate.106b <- rbind(senate.106a, independents)
senate.106c <- senate.106b[c(1,3,2),]
senate.106 <- senate.106c %>% select(n)
colnames(senate.106)[1] <- "106"
print(senate.106)

cmd10 <- 'curl "https://api.propublica.org/congress/v1/105/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd10 <- straighten(cmd10)
str(parsed_cmd10)
actual_function10 <- make_req(parsed_cmd10)[[1]]
request.body.list10 <- content(actual_function10())
members.list10 <- request.body.list10$results[[1]]$members
names(members.list10) <- NULL
members.json10 <- toJSON(members.list10)
senate.members.105 <- flatten(fromJSON(members.json10, flatten = TRUE)) %>% select(party)
senate.members.105$party <- as.factor(unlist(senate.members.105$party))
senate.105a <- tally(group_by(senate.members.105, party))
senate.105b <- rbind(senate.105a, independents)
senate.105c <- senate.105b[c(1,3,2),]
senate.105 <- senate.105c %>% select(n)
colnames(senate.105)[1] <- "105"

cmd11 <- 'curl "https://api.propublica.org/congress/v1/104/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd11 <- straighten(cmd11)
str(parsed_cmd11)
actual_function11 <- make_req(parsed_cmd11)[[1]]
request.body.list11 <- content(actual_function11())
members.list11 <- request.body.list11$results[[1]]$members
names(members.list11) <- NULL
members.json11 <- toJSON(members.list11)
senate.members.104 <- flatten(fromJSON(members.json11, flatten = TRUE)) %>% select(party)
senate.members.104$party <- as.factor(unlist(senate.members.104$party))
senate.104a <- tally(group_by(senate.members.104, party))
senate.104b <- rbind(senate.104a, independents)
senate.104c <- senate.104b[c(1,3,2),]
senate.104 <- senate.104c %>% select(n)
colnames(senate.104)[1] <- "104"

cmd12 <- 'curl "https://api.propublica.org/congress/v1/103/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd12 <- straighten(cmd12)
str(parsed_cmd12)
actual_function12 <- make_req(parsed_cmd12)[[1]]
request.body.list12 <- content(actual_function12())
members.list12 <- request.body.list12$results[[1]]$members
names(members.list12) <- NULL
members.json12 <- toJSON(members.list12)
senate.members.103 <- flatten(fromJSON(members.json12, flatten = TRUE)) %>% select(party)
senate.members.103$party <- as.factor(unlist(senate.members.103$party))
senate.103a <- tally(group_by(senate.members.103, party))
senate.103b <- rbind(senate.103a, independents)
senate.103c <- senate.103b[c(1,3,2),]
senate.103 <- senate.103c %>% select(n)
colnames(senate.103)[1] <- "103"

cmd13 <- 'curl "https://api.propublica.org/congress/v1/102/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd13 <- straighten(cmd13)
str(parsed_cmd13)
actual_function13 <- make_req(parsed_cmd13)[[1]]
request.body.list13 <- content(actual_function13())
members.list13 <- request.body.list13$results[[1]]$members
names(members.list13) <- NULL
members.json13 <- toJSON(members.list13)
senate.members.102 <- flatten(fromJSON(members.json13, flatten = TRUE))
senate.members.102$party <- as.factor(unlist(senate.members.102$party))
senate.102a <- tally(group_by(senate.members.102, party))
senate.102b <- rbind(senate.102a, independents)
senate.102c <- senate.102b[c(1,3,2),]
senate.102 <- senate.102c %>% select(n)
colnames(senate.102)[1] <- "102"

cmd14 <- 'curl "https://api.propublica.org/congress/v1/101/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd14 <- straighten(cmd14)
str(parsed_cmd14)
actual_function14 <- make_req(parsed_cmd14)[[1]]
request.body.list14 <- content(actual_function14())
members.list14 <- request.body.list14$results[[1]]$members
names(members.list14) <- NULL
members.json14 <- toJSON(members.list14)
senate.members.101 <- flatten(fromJSON(members.json14, flatten = TRUE))
senate.members.101$party <- as.factor(unlist(senate.members.101$party))
senate.101a <- tally(group_by(senate.members.101, party))
senate.101b <- rbind(senate.101a, independents)
senate.101c <- senate.101b[c(1,3,2),]
senate.101 <- senate.101c %>% select(n)
colnames(senate.101)[1] <- "101"

cmd15 <- 'curl "https://api.propublica.org/congress/v1/100/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd15 <- straighten(cmd15)
str(parsed_cmd15)
actual_function15 <- make_req(parsed_cmd15)[[1]]
request.body.list15 <- content(actual_function15())
members.list15 <- request.body.list15$results[[1]]$members
names(members.list15) <- NULL
members.json15 <- toJSON(members.list15)
senate.members.100 <- flatten(fromJSON(members.json15, flatten = TRUE))
senate.members.100$party <- as.factor(unlist(senate.members.100$party))
senate.100a <- tally(group_by(senate.members.100, party))
senate.100b <- rbind(senate.100a, independents)
senate.100c <- senate.100b[c(1,3,2),]
senate.100 <- senate.100c %>% select(n)
colnames(senate.100)[1] <- "100"

cmd16 <- 'curl "https://api.propublica.org/congress/v1/99/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd16 <- straighten(cmd16)
str(parsed_cmd16)
actual_function16 <- make_req(parsed_cmd16)[[1]]
request.body.list16 <- content(actual_function16())
members.list16 <- request.body.list16$results[[1]]$members
names(members.list16) <- NULL
members.json16 <- toJSON(members.list16)
senate.members.99 <- flatten(fromJSON(members.json16, flatten = TRUE))
senate.members.99$party <- as.factor(unlist(senate.members.99$party))
senate.99a <- tally(group_by(senate.members.99, party))
senate.99b <- rbind(senate.99a, independents)
senate.99c <- senate.99b[c(1,3,2),]
senate.99 <- senate.99c %>% select(n)
colnames(senate.99)[1] <- "99"

cmd17 <- 'curl "https://api.propublica.org/congress/v1/98/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd17 <- straighten(cmd17)
str(parsed_cmd17)
actual_function17 <- make_req(parsed_cmd17)[[1]]
request.body.list17 <- content(actual_function17())
members.list17 <- request.body.list17$results[[1]]$members
names(members.list17) <- NULL
members.json17 <- toJSON(members.list17)
senate.members.98 <- flatten(fromJSON(members.json17, flatten = TRUE))
senate.members.98$party <- as.factor(unlist(senate.members.98$party))
senate.98a <- tally(group_by(senate.members.98, party))
senate.98b <- rbind(senate.98a, independents)
senate.98c <- senate.98b[c(1,3,2),]
senate.98 <- senate.98c %>% select(n)
colnames(senate.98)[1] <- "98"

cmd18 <- 'curl "https://api.propublica.org/congress/v1/97/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd18 <- straighten(cmd18)
str(parsed_cmd18)
actual_function18 <- make_req(parsed_cmd18)[[1]]
request.body.list18 <- content(actual_function18())
members.list18 <- request.body.list18$results[[1]]$members
names(members.list18) <- NULL
members.json18 <- toJSON(members.list18)
senate.members.97 <- flatten(fromJSON(members.json18, flatten = TRUE))
senate.members.97$party <- as.factor(unlist(senate.members.97$party))
senate.97 <- tally(group_by(senate.members.97, party)) %>% select(n)
colnames(senate.97)[1] <- "97"

cmd19 <- 'curl "https://api.propublica.org/congress/v1/96/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd19 <- straighten(cmd19)
str(parsed_cmd19)
actual_function19 <- make_req(parsed_cmd19)[[1]]
request.body.list19 <- content(actual_function19())
members.list19 <- request.body.list19$results[[1]]$members
names(members.list19) <- NULL
members.json19 <- toJSON(members.list19)
senate.members.96 <- flatten(fromJSON(members.json19, flatten = TRUE))
senate.members.96$party <- as.factor(unlist(senate.members.96$party))
senate.96 <- tally(group_by(senate.members.96, party)) %>% select(n)
colnames(senate.96)[1] <- "96"

cmd20 <- 'curl "https://api.propublica.org/congress/v1/95/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd20 <- straighten(cmd20)
str(parsed_cmd20)
actual_function20 <- make_req(parsed_cmd20)[[1]]
request.body.list20 <- content(actual_function20())
members.list20 <- request.body.list20$results[[1]]$members
names(members.list20) <- NULL
members.json20 <- toJSON(members.list20)
senate.members.95 <- flatten(fromJSON(members.json20, flatten = TRUE))
senate.members.95$party <- as.factor(unlist(senate.members.95$party))
senate.95 <- tally(group_by(senate.members.95, party)) %>% select(n)
colnames(senate.95)[1] <- "95"

cmd21 <- 'curl "https://api.propublica.org/congress/v1/94/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd21 <- straighten(cmd21)
str(parsed_cmd21)
actual_function21 <- make_req(parsed_cmd21)[[1]]
request.body.list21 <- content(actual_function21())
members.list21 <- request.body.list21$results[[1]]$members
names(members.list21) <- NULL
members.json21 <- toJSON(members.list21)
senate.members.94 <- flatten(fromJSON(members.json21, flatten = TRUE))
senate.members.94$party <- as.factor(unlist(senate.members.94$party))
senate.94 <- tally(group_by(senate.members.94, party)) %>% select(n)
colnames(senate.94)[1] <- "94"

cmd22 <- 'curl "https://api.propublica.org/congress/v1/93/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd22 <- straighten(cmd22)
str(parsed_cmd22)
actual_function22 <- make_req(parsed_cmd22)[[1]]
request.body.list22 <- content(actual_function22())
members.list22 <- request.body.list22$results[[1]]$members
names(members.list22) <- NULL
members.json22 <- toJSON(members.list22)
senate.members.93 <- flatten(fromJSON(members.json22, flatten = TRUE))
senate.members.93$party <- as.factor(unlist(senate.members.93$party))
senate.93 <- tally(group_by(senate.members.93, party)) %>% select(n)
colnames(senate.93)[1] <- "93"

cmd23 <- 'curl "https://api.propublica.org/congress/v1/92/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd23 <- straighten(cmd23)
str(parsed_cmd23)
actual_function23 <- make_req(parsed_cmd23)[[1]]
request.body.list23 <- content(actual_function23())
members.list23 <- request.body.list23$results[[1]]$members
names(members.list23) <- NULL
members.json23 <- toJSON(members.list23)
senate.members.92 <- flatten(fromJSON(members.json23, flatten = TRUE))
senate.members.92$party <- as.factor(unlist(senate.members.92$party))
senate.92 <- tally(group_by(senate.members.92, party)) %>% select(n)
colnames(senate.92)[1] <- "92"

cmd24 <- 'curl "https://api.propublica.org/congress/v1/91/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd24 <- straighten(cmd24)
str(parsed_cmd24)
actual_function24 <- make_req(parsed_cmd24)[[1]]
request.body.list24 <- content(actual_function24())
members.list24 <- request.body.list24$results[[1]]$members
names(members.list24) <- NULL
members.json24 <- toJSON(members.list24)
senate.members.91 <- flatten(fromJSON(members.json24, flatten = TRUE))
senate.members.91$party <- as.factor(unlist(senate.members.91$party))
senate.91 <- tally(group_by(senate.members.91, party)) %>% select(n)
colnames(senate.91)[1] <- "91"

cmd25 <- 'curl "https://api.propublica.org/congress/v1/90/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd25 <- straighten(cmd25)
str(parsed_cmd25)
actual_function25 <- make_req(parsed_cmd25)[[1]]
request.body.list25 <- content(actual_function25())
members.list25 <- request.body.list25$results[[1]]$members
names(members.list25) <- NULL
members.json25 <- toJSON(members.list25)
senate.members.90 <- flatten(fromJSON(members.json25, flatten = TRUE))
senate.members.90$party <- as.factor(unlist(senate.members.90$party))
senate.90 <- tally(group_by(senate.members.90, party)) %>% select(n)
colnames(senate.90)[1] <- "90"

cmd26 <- 'curl "https://api.propublica.org/congress/v1/89/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd26 <- straighten(cmd26)
str(parsed_cmd26)
actual_function26 <- make_req(parsed_cmd26)[[1]]
request.body.list26 <- content(actual_function26())
members.list26 <- request.body.list26$results[[1]]$members
names(members.list26) <- NULL
members.json26 <- toJSON(members.list26)
senate.members.89 <- flatten(fromJSON(members.json26, flatten = TRUE))
senate.members.89$party <- as.factor(unlist(senate.members.89$party))
senate.89 <- tally(group_by(senate.members.89, party)) %>% select(n)
colnames(senate.89)[1] <- "89"

cmd27 <- 'curl "https://api.propublica.org/congress/v1/88/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd27 <- straighten(cmd27)
str(parsed_cmd27)
actual_function27 <- make_req(parsed_cmd27)[[1]]
request.body.list27 <- content(actual_function27())
members.list27 <- request.body.list27$results[[1]]$members
names(members.list27) <- NULL
members.json27 <- toJSON(members.list27)
senate.members.88 <- flatten(fromJSON(members.json27, flatten = TRUE))
senate.members.88$party <- as.factor(unlist(senate.members.88$party))
senate.88 <- tally(group_by(senate.members.88, party)) %>% select(n)
colnames(senate.88)[1] <- "88"

cmd28 <- 'curl "https://api.propublica.org/congress/v1/87/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd28 <- straighten(cmd28)
str(parsed_cmd28)
actual_function28 <- make_req(parsed_cmd28)[[1]]
request.body.list28 <- content(actual_function28())
members.list28 <- request.body.list28$results[[1]]$members
names(members.list28) <- NULL
members.json28 <- toJSON(members.list28)
senate.members.87 <- flatten(fromJSON(members.json28, flatten = TRUE))
senate.members.87$party <- as.factor(unlist(senate.members.87$party))
senate.87 <- tally(group_by(senate.members.87, party)) %>% select(n)
colnames(senate.87)[1] <- "87"

cmd29 <- 'curl "https://api.propublica.org/congress/v1/86/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd29 <- straighten(cmd29)
str(parsed_cmd29)
actual_function29 <- make_req(parsed_cmd29)[[1]]
request.body.list29 <- content(actual_function29())
members.list29 <- request.body.list29$results[[1]]$members
names(members.list29) <- NULL
members.json29 <- toJSON(members.list29)
senate.members.86 <- flatten(fromJSON(members.json29, flatten = TRUE))
senate.members.86$party <- as.factor(unlist(senate.members.86$party))
senate.86 <- tally(group_by(senate.members.86, party)) %>% select(n)
colnames(senate.86)[1] <- "86"

cmd30 <- 'curl "https://api.propublica.org/congress/v1/85/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd30 <- straighten(cmd30)
str(parsed_cmd30)
actual_function30 <- make_req(parsed_cmd30)[[1]]
request.body.list30 <- content(actual_function30())
members.list30 <- request.body.list30$results[[1]]$members
names(members.list30) <- NULL
members.json30 <- toJSON(members.list30)
senate.members.85 <- flatten(fromJSON(members.json30, flatten = TRUE))
senate.members.85$party <- as.factor(unlist(senate.members.85$party))
senate.85 <- tally(group_by(senate.members.85, party)) %>% select(n)
colnames(senate.85)[1] <- "85"

cmd31 <- 'curl "https://api.propublica.org/congress/v1/84/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd31 <- straighten(cmd31)
str(parsed_cmd31)
actual_function31 <- make_req(parsed_cmd31)[[1]]
request.body.list31 <- content(actual_function31())
members.list31 <- request.body.list31$results[[1]]$members
names(members.list31) <- NULL
members.json31 <- toJSON(members.list31)
senate.members.84 <- flatten(fromJSON(members.json31, flatten = TRUE))
senate.members.84$party <- as.factor(unlist(senate.members.84$party))
senate.84 <- tally(group_by(senate.members.84, party)) %>% select(n)
colnames(senate.84)[1] <- "84"

cmd32 <- 'curl "https://api.propublica.org/congress/v1/83/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd32 <- straighten(cmd32)
str(parsed_cmd32)
actual_function32 <- make_req(parsed_cmd32)[[1]]
request.body.list32 <- content(actual_function32())
members.list32 <- request.body.list32$results[[1]]$members
names(members.list32) <- NULL
members.json32 <- toJSON(members.list32)
senate.members.83 <- flatten(fromJSON(members.json32, flatten = TRUE))
senate.members.83$party <- as.factor(unlist(senate.members.83$party))
senate.83 <- tally(group_by(senate.members.83, party)) %>% select(n)
colnames(senate.83)[1] <- "83"

cmd33 <- 'curl "https://api.propublica.org/congress/v1/82/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd33 <- straighten(cmd33)
str(parsed_cmd33)
actual_function33 <- make_req(parsed_cmd33)[[1]]
request.body.list33 <- content(actual_function33())
members.list33 <- request.body.list33$results[[1]]$members
names(members.list33) <- NULL
members.json33 <- toJSON(members.list33)
senate.members.82 <- flatten(fromJSON(members.json33, flatten = TRUE))
senate.members.82$party <- as.factor(unlist(senate.members.82$party))
senate.82 <- tally(group_by(senate.members.82, party)) %>% select(n)
colnames(senate.82)[1] <- "82"

cmd34 <- 'curl "https://api.propublica.org/congress/v1/81/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd34 <- straighten(cmd34)
str(parsed_cmd34)
actual_function34 <- make_req(parsed_cmd34)[[1]]
request.body.list34 <- content(actual_function34())
members.list34 <- request.body.list34$results[[1]]$members
names(members.list34) <- NULL
members.json34 <- toJSON(members.list34)
senate.members.81 <- flatten(fromJSON(members.json34, flatten = TRUE))
senate.members.81$party <- as.factor(unlist(senate.members.81$party))
senate.81 <- tally(group_by(senate.members.81, party)) %>% select(n)
colnames(senate.81)[1] <- "81"

cmd35 <- 'curl "https://api.propublica.org/congress/v1/80/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd35 <- straighten(cmd35)
str(parsed_cmd35)
actual_function35 <- make_req(parsed_cmd35)[[1]]
request.body.list35 <- content(actual_function35())
members.list35 <- request.body.list35$results[[1]]$members
names(members.list35) <- NULL
members.json35 <- toJSON(members.list35)
senate.members.80 <- flatten(fromJSON(members.json35, flatten = TRUE))
senate.members.80$party <- as.factor(unlist(senate.members.80$party))
senate.80a <- tally(group_by(senate.members.80, party))
senate.80b <- rbind(senate.80a, independents)
senate.80c <- senate.80b[c(1,3,2),]
senate.80 <- senate.80c
colnames(senate.80)[2] <- "80"


senate.party <- cbind(senate.80, senate.81, senate.82, senate.83, senate.84, senate.85, senate.86, senate.87, senate.88, senate.89,
      senate.90, senate.91, senate.92, senate.93, senate.94, senate.95, senate.96, senate.97, senate.98, senate.99,
      senate.100, senate.101, senate.102, senate.103, senate.104, senate.105, senate.106, senate.107, senate.108, 
      senate.109, senate.110, senate.111, senate.112, senate.113, senate.114, senate.115)

write.csv(senate.party, "senate.makeup")
