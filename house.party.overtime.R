cmd <- 'curl "https://api.propublica.org/congress/v1/115/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd <- straighten(cmd)
str(parsed_cmd)
actual_function <- make_req(parsed_cmd)[[1]]
request.body.list <- content(actual_function())
members.list <- request.body.list$results[[1]]$members
names(members.list) <- NULL
members.json <- toJSON(members.list)
house.members.115 <- flatten(fromJSON(members.json, flatten = TRUE))
house.members.115$party <- as.factor(unlist(house.members.115$party))
house.115 <- tally(group_by(house.members.115, party)) %>% select(n)
colnames(house.115)[1] <- "115"


cmd1 <- 'curl "https://api.propublica.org/congress/v1/114/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd1 <- straighten(cmd1)
str(parsed_cmd1)
actual_function1 <- make_req(parsed_cmd1)[[1]]
request.body.list1 <- content(actual_function1())
members.list1 <- request.body.list1$results[[1]]$members
names(members.list1) <- NULL
members.json1 <- toJSON(members.list1)
house.members.114 <- flatten(fromJSON(members.json1, flatten = TRUE)) %>% select(party)
house.members.114$party <- as.factor(unlist(house.members.114$party))
house.114 <- tally(group_by(house.members.114, party)) %>% select(n)
colnames(house.114)[1] <- "114"


cmd2 <- 'curl "https://api.propublica.org/congress/v1/113/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd2 <- straighten(cmd2)
str(parsed_cmd2)
actual_function2 <- make_req(parsed_cmd2)[[1]]
request.body.list2 <- content(actual_function2())
members.list2 <- request.body.list2$results[[1]]$members
names(members.list2) <- NULL
members.json2 <- toJSON(members.list2)
house.members.113 <- flatten(fromJSON(members.json2, flatten = TRUE)) %>% select(party)
house.members.113$party <- as.factor(unlist(house.members.113$party))
house.113 <- tally(group_by(house.members.113, party)) %>% select(n)
colnames(house.113)[1] <- "113"

cmd3 <- 'curl "https://api.propublica.org/congress/v1/112/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd3 <- straighten(cmd3)
str(parsed_cmd3)
actual_function3 <- make_req(parsed_cmd3)[[1]]
request.body.list3 <- content(actual_function3())
members.list3 <- request.body.list3$results[[1]]$members
names(members.list3) <- NULL
members.json3 <- toJSON(members.list3)
house.members.112 <- flatten(fromJSON(members.json3, flatten = TRUE)) %>% select(party)
house.members.112$party <- as.factor(unlist(house.members.112$party))
house.112 <- tally(group_by(house.members.112, party)) %>% select(n)
colnames(house.112)[1] <- "112"


cmd4 <- 'curl "https://api.propublica.org/congress/v1/111/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd4 <- straighten(cmd4)
str(parsed_cmd4)
actual_function4 <- make_req(parsed_cmd4)[[1]]
request.body.list4 <- content(actual_function4())
members.list4 <- request.body.list4$results[[1]]$members
names(members.list4) <- NULL
members.json4 <- toJSON(members.list4)
house.members.111 <- flatten(fromJSON(members.json4, flatten = TRUE)) %>% select(party)
house.members.111$party <- as.factor(unlist(house.members.111$party))
house.111 <- tally(group_by(house.members.111, party)) %>% select(n)
colnames(house.111)[1] <- "111"


cmd5 <- 'curl "https://api.propublica.org/congress/v1/110/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd5 <- straighten(cmd5)
str(parsed_cmd5)
actual_function5 <- make_req(parsed_cmd5)[[1]]
request.body.list5 <- content(actual_function5())
members.list5 <- request.body.list5$results[[1]]$members
names(members.list5) <- NULL
members.json5 <- toJSON(members.list5)
house.members.110 <- flatten(fromJSON(members.json5, flatten = TRUE)) %>% select(party)
house.members.110$party <- as.factor(unlist(house.members.110$party))
house.110.members <- tally(group_by(house.members.110, party))
independents <- data.frame("party" = "I", "n" = "0")
house.110 <- rbind(house.110.members, independents)
house.110  <- house.110 %>% select(n)
colnames(house.110)[1] <- "110"

cmd6 <- 'curl "https://api.propublica.org/congress/v1/109/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd6 <- straighten(cmd6)
str(parsed_cmd6)
actual_function6 <- make_req(parsed_cmd6)[[1]]
request.body.list6 <- content(actual_function6())
members.list6 <- request.body.list6$results[[1]]$members
names(members.list6) <- NULL
members.json6 <- toJSON(members.list6)
house.members.109 <- flatten(fromJSON(members.json6, flatten = TRUE)) %>% select(party)
house.members.109$party <- as.factor(unlist(house.members.109$party))
house.109 <- tally(group_by(house.members.109, party)) %>% select(n)
colnames(house.109)[1] <- "109"

cmd7 <- 'curl "https://api.propublica.org/congress/v1/108/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd7 <- straighten(cmd7)
str(parsed_cmd7)
actual_function7 <- make_req(parsed_cmd7)[[1]]
request.body.list7 <- content(actual_function7())
members.list7 <- request.body.list7$results[[1]]$members
names(members.list7) <- NULL
members.json7 <- toJSON(members.list7)
house.members.108 <- flatten(fromJSON(members.json7, flatten = TRUE)) %>% select(party)
house.members.108$party <- as.factor(unlist(house.members.108$party))
house.108 <- tally(group_by(house.members.108, party)) %>% select(n)
colnames(house.108)[1] <- "108"

cmd8 <- 'curl "https://api.propublica.org/congress/v1/107/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd8 <- straighten(cmd8)
str(parsed_cmd8)
actual_function8 <- make_req(parsed_cmd8)[[1]]
request.body.list8 <- content(actual_function8())
members.list8 <- request.body.list8$results[[1]]$members
names(members.list8) <- NULL
members.json8 <- toJSON(members.list8)
house.members.107 <- flatten(fromJSON(members.json8, flatten = TRUE)) %>% select(party)
house.members.107$party <- as.factor(unlist(house.members.107$party))
house.107 <- tally(group_by(house.members.107, party)) %>% select(n)
colnames(house.107)[1] <- "107"

cmd9 <- 'curl "https://api.propublica.org/congress/v1/106/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd9 <- straighten(cmd9)
str(parsed_cmd9)
actual_function9 <- make_req(parsed_cmd9)[[1]]
request.body.list9 <- content(actual_function9())
members.list9 <- request.body.list9$results[[1]]$members
names(members.list9) <- NULL
members.json9 <- toJSON(members.list9)
house.members.106 <- flatten(fromJSON(members.json9, flatten = TRUE)) %>% select(party)
house.members.106$party <- as.factor(unlist(house.members.106$party))
house.106 <- tally(group_by(house.members.106, party)) %>% select(n)
colnames(house.106)[1] <- "106"

cmd10 <- 'curl "https://api.propublica.org/congress/v1/105/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd10 <- straighten(cmd10)
str(parsed_cmd10)
actual_function10 <- make_req(parsed_cmd10)[[1]]
request.body.list10 <- content(actual_function10())
members.list10 <- request.body.list10$results[[1]]$members
names(members.list10) <- NULL
members.json10 <- toJSON(members.list10)
house.members.105 <- flatten(fromJSON(members.json10, flatten = TRUE)) %>% select(party)
house.members.105$party <- as.factor(unlist(house.members.105$party))
house.105 <- tally(group_by(house.members.105, party)) %>% select(n)
colnames(house.105)[1] <- "105"

cmd11 <- 'curl "https://api.propublica.org/congress/v1/104/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd11 <- straighten(cmd11)
str(parsed_cmd11)
actual_function11 <- make_req(parsed_cmd11)[[1]]
request.body.list11 <- content(actual_function11())
members.list11 <- request.body.list11$results[[1]]$members
names(members.list11) <- NULL
members.json11 <- toJSON(members.list11)
house.members.104 <- flatten(fromJSON(members.json11, flatten = TRUE)) %>% select(party)
house.members.104$party <- as.factor(unlist(house.members.104$party))
house.104 <- tally(group_by(house.members.104, party)) %>% select(n)
colnames(house.104)[1] <- "104"

cmd12 <- 'curl "https://api.propublica.org/congress/v1/103/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd12 <- straighten(cmd12)
str(parsed_cmd12)
actual_function12 <- make_req(parsed_cmd12)[[1]]
request.body.list12 <- content(actual_function12())
members.list12 <- request.body.list12$results[[1]]$members
names(members.list12) <- NULL
members.json12 <- toJSON(members.list12)
house.members.103 <- flatten(fromJSON(members.json12, flatten = TRUE)) %>% select(party)
house.members.103$party <- as.factor(unlist(house.members.103$party))
house.103 <- tally(group_by(house.members.103, party)) %>% select(n)
colnames(house.103)[1] <- "103"

cmd13 <- 'curl "https://api.propublica.org/congress/v1/102/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
parsed_cmd13 <- straighten(cmd13)
str(parsed_cmd13)
actual_function13 <- make_req(parsed_cmd13)[[1]]
request.body.list13 <- content(actual_function13())
members.list13 <- request.body.list13$results[[1]]$members
names(members.list13) <- NULL
members.json13 <- toJSON(members.list13)
house.members.102 <- flatten(fromJSON(members.json13, flatten = TRUE))
house.members.102$party <- as.factor(unlist(house.members.102$party))
house.102 <- tally(group_by(house.members.102, party))
colnames(house.102)[2] <- "102"
colnames(house.102)[1] <- "party"

house.party <- cbind(house.102, house.103, house.104, house.105, house.106, house.107, house.108, house.109, house.110, house.111, 
      house.112, house.113, house.114, house.115)

write.csv(house.party, "house.makeup")
