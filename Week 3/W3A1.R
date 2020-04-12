#1.1
songs = read.csv("songs.csv")
table(songs$year)
#1.2
ml = subset(songs, artistname == "Michael Jackson")
nrow(ml)
#1.3
ml$songtitle
ml$Top10[1]
ml$Top10[4]
#1.4
table(songs$timesignature)
#1.5
which.max(songs$tempo)
songs$songtitle[6206]

#2.1
SongsTrain = subset(songs, year<=2009)
SongsTest = subset(songs, year==2010)
str(SongsTrain)
#2.2, 2.3, 2.4, 2.5
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
LOG1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(LOG1)

#3.1
cor(SongsTrain$loudness,SongsTrain$energy)
#3.2
LOG2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
#3.3
LOG3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

#4.1
testpredict = predict(LOG3, newdata=SongsTest, type="response")
table(SongsTest$Top10, testpredict>=0.45)
#4.2, 4.3
table(SongsTest$Top10)
#4.4, 4.5
table(SongsTest$Top10, testpredict>=0.45)

