#1.1
Baseball = read.csv("Baseball.csv")
nrow(Baseball)
#1.2
table(Baseball$Year)
length(table(Baseball$Year))
#1.3
Baseball = subset(Baseball, Playoffs == 1)
nrow(Baseball)
#1.4
table(Baseball$Year)

#2.1
PlayoffTable = table(Baseball$Year)
str(names(PlayoffTable))
#2.2
PlayoffTable[c("1990", "2001")]
#2.3
Baseball$NumCompetitors = PlayoffTable[as.character(Baseball$Year)]
#2.4
Baseball$NumCompetitors = PlayoffTable[as.character(Baseball$Year)]
nrow( Baseball$NumCompetitors )
table(Baseball$NumCompetitors)

#3.1
Baseball$WorldSeries = as.numeric(Baseball$RankPlayoffs == 1)
table(Baseball$WorldSeries)
#3.2
summary(glm(WorldSeries~Year, data=Baseball, family="binomial"))

#4.1
LogMODEL = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=Baseball, family=binomial)
#4.2
cor(Baseball$Year, Baseball$RA)
cor(Baseball[c(“Year”, “RA”, “RankSeason”, “NumCompetitors”)])
#4.3
MODEL1 = glm(WorldSeries ~ Year + RA, data=Baseball, family=binomial)
MODEL2 = glm(WorldSeries ~ Year + RankSeason, data=Baseball, family=binomial)
MODEL3 = glm(WorldSeries ~ Year + NumCompetitors, data=Baseball, family=binomial)
MODEL4 = glm(WorldSeries ~ RA + RankSeason, data=Baseball, family=binomial)
MODEL5 = glm(WorldSeries ~ RA + NumCompetitors, data=Baseball, family=binomial)
MODEL6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=Baseball, family=binomial)

