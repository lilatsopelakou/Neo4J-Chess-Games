#File Paths
setwd("C:/Users/meropi.argyropoulou/Documents/Master 3rd trimester/BIG DATA SYSTEMS/2ND ASSIGNMENT")

filepath="C:/Users/meropi.argyropoulou/Documents/Master 3rd trimester/BIG DATA SYSTEMS/2ND ASSIGNMENT/chessData.txt"


processFile = function(filepath) {
  con = file(filepath, "r")
  fileConn<-file("output.txt","w")
  counter<-0
  while ( TRUE ) {
    line = readLines(con, n = 1)
    counter<-counter+1
    if ( length(line) == 0 ) {
      break
    }
    if (line=="=========================== Game ======================================================"){
      counter2<-counter+16
    }
    if (counter<counter2 & line!="=========================== Game ======================================================" )
    {print(line)
    write(line,fileConn, append=TRUE)
     #writeLines(line, fileConn)
    }
    }
  
  close(con)
  close(fileConn)
}

processFile(filepath)

game<-read.table("output.txt", sep=":", quote="")
#install.packages("tidyr")
library("tidyr")
library("purrr")
list1 <- game %>% 
  split(.$V1) %>% 
  map(~ as.character(.$V2))
data.frame(list1)
#Trim null character from beginning
games<-data.frame(list1)
games$White<-sub('.', '', games$White)
games$Black<-sub('.', '', games$Black)
games$Date<-sub('.', '', games$Date)
games$HalfMoves <-sub('.', '', games$HalfMoves)
games$Moves<-sub('.', '', games$Moves)
games$Result<-sub('.', '', games$Result)
games$WhiteElo<-sub('.', '', games$WhiteElo)
games$BlackElo<-sub('.', '', games$BlackElo)
games$GameNumber<-sub('.', '', games$GameNumber)
games$Event<-sub('.', '', games$Event)
games$Site<-sub('.', '', games$Site)
games$EventDate<-sub('.', '', games$EventDate)
games$Round<-sub('.', '', games$Round)
games$ECO<-sub('.', '', games$ECO)
games$Opening<-sub('.', '', games$Opening)

games$BlackElo<-as.numeric(games$BlackElo)
games$GameNumber<-as.numeric(games$GameNumber)
games$HalfMoves<-as.numeric(games$HalfMoves)
games$Moves<-as.numeric(games$Moves)
games$Round<-as.numeric(games$Round)
games$WhiteElo<-as.numeric(games$WhiteElo)


processFile2 = function(filepath) {
  b<-FALSE
  con = file(filepath, "r")
  fileConn<-file("outputmoves.txt","w")
  counter<-0
  while ( TRUE ) {
    line = readLines(con, n = 1)
    counter<-counter+1
    if ( length(line) == 0 ) {
      break
    }
    if (line=="--------------------------------------------------------- Game Moves ---------------------------------------------------------------------"){
      b<-TRUE
    }
    else if (line=="======================================================================================"){
      b<-FALSE
    }
    if (b==TRUE & line!="--------------------------------------------------------- Game Moves ---------------------------------------------------------------------")
    {print(line)
     write(line,fileConn, append=TRUE)
    }
  }
  
  
  close(con)
  close(fileConn)
}

processFile2(filepath)

moves<-read.table("outputmoves.txt", sep=",", comment.char = "")
colnames(moves)<-c("MoveNumber", "Side", "Move", "FEN", "GameNumber")
moves$MoveNumber <-sub('MoveNumber: ', '', moves$MoveNumber)
moves$Side<-sub('Side:', '', moves$Side)
moves$Move<-sub('Move: ', '', moves$Move)
moves$FEN<-sub('FEN:', '', moves$FEN)
moves$GameNumber<-sub('GameNumber: ', '', moves$GameNumber)

moves$Side<-sub('...', '', moves$Side)
moves$Move<-sub('..', '', moves$Move)
moves$FEN<-sub('...', '', moves$FEN)


moves$MoveNumber<-as.numeric(moves$MoveNumber)
moves$GameNumber<-as.numeric(moves$GameNumber)

games<-games[,c(14,1,3,8,9,11,15,2,7,5,13,6,12,4,10)]

final<- merge(games,moves,by="GameNumber")

final<-final[,-c(4,5,6,8,9,11,12,13,14)]

#write.csv(final, file = "final.csv", row.names=F)

#Node game
game<-games[,c(5, 6, 9, 15)]
write.csv(game, file = "game.csv", row.names=F)

#Node event
event<-data.frame(unique(games$Event))
colnames(event)<-"Event"
write.csv(event, file = "event.csv", row.names=F)

#Relationships Event-Game csv
egame<-games[,c(9,10)]
write.csv(egame, file = "reg.csv", row.names=F)

  
#Relationships Player-Game csv
rgames<-games
rgames<-rgames[,c(1,2,9)]
rgames$color<-"White"
rgames$colorB<-"Black"
df1<-rgames[,c(1,3,4)]
df2<-rgames[,c(2,3,5)]
colnames(df1)<-c("Player", "GameNumber", "Color")
colnames(df2)<-c("Player", "GameNumber", "Color")
rpg<-rbind(df1,df2)
write.csv(rpg, file = "rpg.csv", row.names=F)

#Node player
player<-data.frame(unique(rpg$Player))
colnames(player)<-"Player"
write.csv(player, file = "player.csv", row.names=F)

#Node Position
rmoves<-moves
position<-data.frame(unique(rmoves$FEN))
colnames(position)<-"Position"
write.csv(position, file = "position.csv", row.names=F)

#Relationships Game-Position csv-When move=1
movesone<-moves[moves$MoveNumber==1,]
write.csv(movesone, file = "rgpos.csv", row.names=F)

#Relationships Position-Position csv
mmoves<-moves
mmoves2<-mmoves[c(2:nrow(mmoves)),]
mmoves2<-rbind(mmoves2, c(1,2,3,4,5))
rpp<-cbind(mmoves, mmoves2)


colnames(rpp)<-c("MoveNumberBefore","SideBefore", "MoveBefore","FENBefore","GameNumberBefore","MoveNumberAfter","SideAfter","MoveAfter", "FENAfter", "GameNumberAfter")
#omg<-rpp[rpp$GameNumberBefore!=rpp$GameNumberAfter,]
rpp<-rpp[!(rpp$GameNumberBefore!=rpp$GameNumberAfter),]
nrow(rpp)
write.csv(rpp, file = "rpospos.csv", row.names=F)