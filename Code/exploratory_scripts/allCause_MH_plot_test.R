
## MH data aggregated to county, year
dat00 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2000.csv")
dat01 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2001.csv")
dat02 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2002.csv")
dat03 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2003.csv")
dat04 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2004.csv")
dat05 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2005.csv")
dat06 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2006.csv")
dat07 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2007.csv")
dat08 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2008.csv")
dat09 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2009.csv")
dat10 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2010.csv")
dat11 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2011.csv")

plot <- data.frame(years = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011),
                   all_cause = c(sum(dat00$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat01$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat02$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat03$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat04$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat05$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat06$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat07$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat08$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat09$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat10$all_cause_hospitalizations, na.rm = TRUE),
                                 sum(dat11$all_cause_hospitalizations, na.rm = TRUE)),
                   mh = c(sum(dat00$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat01$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat02$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat03$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat04$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat05$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat06$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat07$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat08$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat09$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat10$mental_health_hospitalizations, na.rm = TRUE),
                          sum(dat11$mental_health_hospitalizations, na.rm = TRUE)),
                   anxiety = c(sum(dat00$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat01$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat02$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat03$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat04$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat05$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat06$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat07$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat08$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat09$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat10$anxiety_disorders_hospitalizations, na.rm = TRUE),
                          sum(dat11$anxiety_disorders_hospitalizations, na.rm = TRUE)),
                   mood = c(sum(dat00$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat01$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat02$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat03$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat04$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat05$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat06$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat07$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat08$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat09$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat10$mood_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat11$mood_disorders_hospitalizations, na.rm = TRUE)),
                   attention = c(sum(dat00$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat01$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat02$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat03$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat04$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat05$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat06$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat07$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat08$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat09$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat10$attention_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat11$attention_disorders_hospitalizations, na.rm = TRUE)),
                   adjustment = c(sum(dat00$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat01$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat02$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat03$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat04$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat05$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat06$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat07$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat08$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat09$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat10$adjustment_reaction_hospitalizations, na.rm = TRUE),
                                 sum(dat11$adjustment_reaction_hospitalizations, na.rm = TRUE)),
                   developmental = c(sum(dat00$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat01$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat02$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat03$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat04$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat05$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat06$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat07$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat08$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat09$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat10$developmental_disorders_hospitalizations, na.rm = TRUE),
                                  sum(dat11$developmental_disorders_hospitalizations, na.rm = TRUE)),
                   schizophrenia_psychotic = c(sum(dat00$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat01$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat02$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat03$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat04$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat05$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat06$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat07$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat08$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat09$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat10$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                            sum(dat11$schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE)),
                   substance = c(sum(dat00$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat01$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat02$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat03$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat04$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat05$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat06$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat07$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat08$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat09$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat10$substance_disorders_hospitalizations, na.rm = TRUE),
                               sum(dat11$substance_disorders_hospitalizations, na.rm = TRUE)),
                   alcohol = c(sum(dat00$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat01$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat02$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat03$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat04$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat05$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat06$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat07$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat08$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat09$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat10$alcohol_disorders_hospitalizations, na.rm = TRUE),
                                 sum(dat11$alcohol_disorders_hospitalizations, na.rm = TRUE)))

                                 


p1 <- ggplot(plot, aes(years, all_cause)) + geom_line() + ggtitle("All cause")
p2 <- ggplot(plot, aes(years, mh)) + geom_line() + ggtitle("All MH")
p3 <- ggplot(plot, aes(years, anxiety)) + geom_line() + ggtitle("Anxiety")
p4 <- ggplot(plot, aes(years, mood)) + geom_line() + ggtitle("Mood")
p5 <- ggplot(plot, aes(years, alcohol)) + geom_line() + ggtitle("Alcohol")
p6 <- ggplot(plot, aes(years, substance)) + geom_line() + ggtitle("Substance")



multiplot(p1, p2, cols = 2)


alc_icd9 <- c("2910", "2911", "2912", "2913", "2914", "2915", "2918", "29181", "29182", "29189", "2919", "30300", "30301", "30302", "30303", "30390", "30391", "30392", "30393", "30500", "30501", "30502", "30503", "3575", "4255", "5353", "53530", "53531", "5710", "5711", "5712", "5713", "76071", "9800")
alc_icd10 <- c("F1010", "F1011", "F10120", "F10121", "F10129", "F1014", "F10150", "F10151", "F10159", "F10180", "F10181", "F10182", "F10188", "F1019", "F1020", "F1021", "F10220", "F10221", "F10229", "F10230", "F10231", "F10232", "F10239", "F1024", "F10250", "F10251", "F10259", "F1026", "F1027", "F10280", "F10281", "F10282", "F10288", "F1029", "F10920", "F10921", "F10929", "F1094", "F10950", "F10951", "F10959", "F1096", "F1097", "F10980", "F10981", "F10982", "F10988", "F1099", "G621", "I426", "K2920", "K2921", "K700", "K7010", "K7011", "K702", "K7030", "K7031", "K7040", "K709", "O99310", "O99311", "O99312", "O99313", "O99314", "O99315", "P043", "Q860")


sub_icd9 <- c("2920", "29211", "29212", "2922", "29281", "29282", "29283", "29284", "29285", "29289", "2929", "30400", "30401", "30402", "30403", "30410", "30411", "30412", "30413", "30420", "30421", "30422", "30423", "30430", "30431", "30432", "30433", "30440", "30441", "30442", "30443", "30450", "30451", "30452", "30453", "30460", "30461", "30462", "30463", "30470", "30471", "30472", "30473", "30480", "30481", "30482", "30483", "30490", "30491", "30492", "30493", "30520", "30521", "30522", "30523", "30530", "30531", "30532", "30533", "30540", "30541", "30542", "30543", "30550", "30551", "30552", "30553", "30560", "30561", "30562", "30563", "30570", "30571", "30572", "30573", "30580", "30581", "30582", "30583", "30590", "30591", "30592", "30593", "64830", "64831", "64832", "64833", "64834", "65550", "65551", "65553", "76072", "76073", "76075", "7795", "96500", "96501", "96502", "96509", "V6542")
sub_icd10 <- c("F1110", "F1111", "F11120", "F11121", "F11122", 
               "F11129", "F1114", "F11150", "F11151", "F11159", 
               "F11181", "F11182", "F11188", "F1119", "F1120", "F1121", "F11220", "F11221", 
               "F11222", "F11229", "F1123", "F1124", "F11250", "F11251", "F11259", "F11281", "F11282", "F11288", "F1129", "F1190", "F11920", 
               "F11921", "F11922", "F11929", "F1193", "F1194", "F11950", "F11951", "F11959", "F11981", "F11982", "F11988", "F1199", "F1210", 
               "F1211", "F12120", "F12121", "F12122", "F12129", "F12150", "F12151", "F12159", "F12180", "F12188", "F1219", "F1220", "F1221", 
               "F12220", "F12221", "F12222", "F12229", "F1223", "F12250", "F12251", "F12259", "F12280", "F12288", "F1229", "F1290", "F12920",
               "F12921", "F12922", "F12929", "F1293", "F12950", "F12951", "F12959", "F12980", "F12988", "F1299", "F1310", "F1311", "F13120", 
               "F13121", "F13129", "F1314", "F13150", "F13151", "F13159", "F13180", "F13181", "F13182", "F13188", "F1319", "F1320", "F1321", "
               F13220", "F13221", "F13229", "F13230", "F13231", "F13232", "F13239", "F1324", "F13250", "F13251", "F13259", "F1326", "F1327", 
               "F13280", "F13281", "F13282", "F13288", "F1329", "F1390", "F13920", "F13921", "F13929", "F13930", "F13931", "F13932", "F13939", 
               "F1394", "F13950", "F13951", "F13959", "F1396", "F1397", "F13980", "F13981", "F13982", "F13988", "F1399", "F1410", "F1411", "F14120", 
               "F14121", "F14122", "F14129", "F1414", "F14150", "F14151", "F14159", "F14180", "F14181", "F14182", "F14188", "F1419", "F1420", "F1421", 
               "F14220", "F14221", "F14222", "F14229", "F1423", "F1424", "F14250", "F14251", "F14259", "F14280", "F14281", "F14282", "F14288", "F1429", 
               "F1490", "F14920", "F14921", "F14929", "F1494", "F14950", "F14951", "F14959", "F14980", "F14981", "F14982", "F14988", "F1499", "F1510",
               "F1511", "F15120", "F15121", "F15122", "F15129", "F1514", "F15150", "F15151", "F15159", "F15180", "F15181", "F15182", "F15188", "F1519", 
               "F1520", "F1521", "F15220", "F15221", "F15222", "F15229", "F1523", "F1524", "F15250", "F15251", "F15259", "F15280", "F15281", "F15282", 
               "F15288", "F1529", "F1590", "F15920", "F15921", "F15929", "F1593", "F1594", "F15950", "F15951", "F15959", "F15980", "F15981", "F15982", 
               "F15988", "F1599", "F1610", "F1611", "F16120", "F16121", "F16122", "F16129", "F1614", "F16150", "F16151", "F16159", "F16180", "F16183", 
               "F16188", "F1619", "F1620", "F1621", "F16220", "F16221", "F16229", "F1624", "F16250", "F16251", "F16259", "F16280", "F16283", "F16288", 
               "F1629", "F1690", "F16920", "F16921", "F16929", "F1694", "F16950", "F16951", "F16959", "F16980", "F16983", "F16988", "F1699", "F17200", 
               "F17201", "F17203", "F17208", "F17209", "F17210", "F17211", "F17213", "F17218", "F17219", "F17220", "F17221", "F17223", "F17228", "F17229", 
               "F17290", "F17291", "F17293", "F17298", "F17299", "F1810", "F1811", "F18120", "F18121", "F18129", "F1814", "F18150", "F18151", "F18159", 
               "F1817", "F18180", "F18188", "F1819", "F1820", "F1821", "F18220", "F18221", "F18229", "F1824", "F18250", "F18251", "F18259", "F1827", 
               "F18280", "F18288", "F1829", "F1890", "F18920", "F18921", "F18929", "F1894", "F18950", "F18951", "F18959", "F1897", "F18980", "F18988", 
               "F1899", "F1910", "F1911", "F19120", "F19121", "F19122", "F19129", "F1914", "F19150", "F19151", "F19159", "F1916", "F1917", "F19180", 
               "F19181", "F19182", "F19188", "F1919", "F1920", "F1921", "F19220", "F19221", "F19222", "F19229", "F19230", "F19231", "F19232", "F19239", 
               "F1924", "F19250", "F19251", "F19259", "F1926", "F1927", "F19280", "F19281", "F19282", "F19288", "F1929", "F1990", "F19920", "F19921",
               "F19922", "F19929", "F19930", "F19931", "F19932", "F19939", "F1994", "F19950", "F19951", "F19959", "F1996", "F1997", "F19980", "F19981", 
               "F19982", "F19988", "F1999", "F550", "F551", "F552", "F553", "F554", "F558", "O355XX0", "O355XX1", "O355XX2", "O355XX3", "O355XX4",
               "O355XX5", "O355XX9", "O99320", "O99321", "O99322", "O99323", "O99324", "O99325", "P0441", "P0449", "P961", "P962", "T400X1A",
               "T400X1D", "T400X1S", "T400X3A", "T400X3D", "T400X3S", "T400X4A", "T400X4D", "T400X4S", "T400X5A", "T400X5D", "T400X5S", 
               "T400X6A", "T400X6D", "T400X6S", "T401X1A", "T401X1D", "T401X1S", "T401X3A", "T401X3D", "T401X3S", "T401X4A", "T401X4D", 
               "T401X4S", "T401X5A", "T401X5D", "T401X5S", "T405X1A", "T405X1D", "T405X1S", "T405X3A", "T405X3D", "T405X3S", "T405X4A", 
               "T405X4D", "T405X4S", "T405X5A", "T405X5D", "T405X5S", "T405X6A", "T405X6D", "T405X6S", "T407X1A", "T407X1D", "T407X1S", 
               "T407X3A", "T407X3D", "T407X3S", "T407X4A", "T407X4D", "T407X4S", "T407X5A", "T407X5D", "T407X5S", "T408X1A", "T408X1D", 
               "T408X1S", "T408X3A", "T408X3D", "T408X3S", "T408X4A", "T408X4D", "T408X4S", "T408X5A", "T408X5D", "T408X5S", "T40901A", 
               "T40901D", "T40901S", "T40903A", "T40903D", "T40903S", "T40904A", "T40904D", 
               "T40904S", "T40905A", "T40905D", "T40905S", "T40906A", "T40906D", "T40906S", "T40991A", "T40991D", "T40991S", "T40994A", "T40994D", "T40994S", "T40995A", "T40995D", "T40995S", "T40996A", "T40996D", "T40996S")