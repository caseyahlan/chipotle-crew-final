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
senate.115 <- tally(group_by(senate.members.115, party)) %>% 
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
senate.111b <- rbind(senate.111a, independents.2)
colnames(senate.111b)[1] <- "111"
senate.111 <- senate.111b[c(1,3,2),]
print(senate.111)


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
senate.110a <- rbind(senate.110a, independents.2)
colnames(senate.110a)[1] <- "110"
senate.110 <- senate.110a[c(1,3,2),]
print(senate.110)

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


cbind(senate.102, senate.103, senate.104, senate.105, senate.106, senate.107, senate.108, senate.109, senate.110, senate.111, 
                     senate.112, senate.113, senate.114, senate.115)

