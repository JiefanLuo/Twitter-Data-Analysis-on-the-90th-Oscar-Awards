
#Jiefan Luo
library(ggplot2)
library(lubridate)
library(scales)
library(twitteR)
library("rio")
setup_twitter_oauth("FSj24SZjFsgfIRbwFjt81Kz03", "fIqaiQqZJYBRi41rvHEq7K0FA8iLuYnwWlbn9aVA5YBaeE8nkr",
                    "953064193003057155-2zMYLTKknYeDS08cSDHjRijk3DlwFuQ","YNHnYfErU52lZoAC87e5rCkQhZ0iONg50ocx0yYHmlSUd")
forest_traindata <- read.csv(file="/Users/Luojiefan/Desktop/Oscars Award Categories/BestActor.csv",head=TRUE,sep=",")



##Use hasstage and award categories as search criteria
BestPicture  <-searchTwitter("#Oscars ‘Best Picture’" , n=7000, lang = "en")
BestPicture  <-twListToDF(BestPicture)
export(BestPicture, "/Users/Luojiefan/Desktop/BestPicture.csv")


BestDirector  <-searchTwitter("#Oscars ‘Best Director’" , n=7000, lang = "en")
BestDirector  <-twListToDF(BestDirector)
export(BestPictures, "/Users/Luojiefan/Desktop/BestDirector.csv")


BestActor  <-searchTwitter("#Oscars ‘Best Actor’" , n=7000, lang = "en")
BestActor  <-twListToDF(BestActor)
export(BestPictures, "/Users/Luojiefan/Desktop/BestActor.csv")


BestOriginalScreenplay  <-searchTwitter("#Oscars ‘Best Original Screenplay’" , n=7000, lang = "en")
BestOriginalScreenplay  <-twListToDF(BestOriginalScreenplay)
export(BestOriginalScreenplay, "/Users/Luojiefan/Desktop/BestOriginalScreenplay.csv")


BestAnimatedFeatureFilm  <-searchTwitter("#Oscars ‘Best Animated Film’" , n=7000, lang = "en")
BestAnimatedFeatureFilm  <-twListToDF(BestAnimatedFeatureFilm)
export(BestAnimatedFeatureFilm, "/Users/Luojiefan/Desktop/BestAnimatedFeatureFilm.csv")


BestOriginalScore  <-searchTwitter("#Oscars ‘Best Original Score’" , n=7000, lang = "en")
BestOriginalScore  <-twListToDF(BestOriginalScore)
export(BestAnimatedFeatureFilm, "/Users/Luojiefan/Desktop/BestOriginalScore.csv")


BestActress  <-searchTwitter("#Oscars ‘Best Actress’" , n=7000, lang = "en")
BestActress  <-twListToDF(BestActress)
export(BestActress, "/Users/Luojiefan/Desktop/BestActress.csv")


#Use hasstage and names as search criteria
hOTheShapeofWater <-searchTwitter("#Oscars 'The Shape of Water'", n=7000, lang = "en")
hOTheShapeofWater <-twListToDF(hOTheShapeofWater)
export(hOTheShapeofWater, "/Users/Luojiefan/Desktop/hOTheShapeofWater.csv")

hOThreeBillboards <-searchTwitter("#Oscars 'Three Billboards'", n=7000, lang = "en")
hOThreeBillboards <-twListToDF(hOThreeBillboards)
export(hOThreeBillboards, "/Users/Luojiefan/Desktop/hOThreeBillboards.csv")


hOGuillermodelToro<-searchTwitter("#Oscars 'Guillermo del Toro'", n=7000, lang = "en")
hOGuillermodelToro <-twListToDF(hOGuillermodelToro)
export(hOGuillermodelToro, "/Users/Luojiefan/Desktop/hOGuillermodelToro.csv")


hOJordanPeele<-searchTwitter("#Oscars 'Jordan Peele'", n=7000, lang = "en")
hOJordanPeele <-twListToDF(hOJordanPeele)
export(hOJordanPeele, "/Users/Luojiefan/Desktop/hOJordanPeele.csv")


hOCallMebyYourName <-searchTwitter("#Oscars 'Call Me by Your Name'", n=7000, lang = "en")
hOCallMebyYourName <-twListToDF(hOCallMebyYourName)
export(hOCallMebyYourName, "/Users/Luojiefan/Desktop/hOCallMebyYourName.csv")


hOGaryOldman <-searchTwitter("#Oscars 'Gary Oldman'", n=7000, lang = "en")
hOGaryOldman <-twListToDF(hOGaryOldman)
export(hOGaryOldman, "/Users/Luojiefan/Desktop/hOGaryOldman.csv")


hODanielKaluuya <-searchTwitter("#Oscars 'Daniel Kaluuya'", n=7000, lang = "en")
hODanielKaluuya <-twListToDF(hODanielKaluuya)
export(hODanielKaluuya, "/Users/Luojiefan/Desktop/hODanielKaluuya.csv")



hOFrancesMcDormand <-searchTwitter("#Oscars 'Frances McDormand'", n=7000, lang = "en")
hOFrancesMcDormand <-twListToDF(hOFrancesMcDormand)
export(hOFrancesMcDormand, "/Users/Luojiefan/Desktop/hOFrancesMcDormand.csv")


hOMerylStreeps  <-searchTwitter("#Oscars 'Meryl Streep'", n=7000, lang = "en")
hOMerylStreeps  <-twListToDF(hOMerylStreeps)
export(hOMerylStreeps, "/Users/Luojiefan/Desktop/hOMerylStreeps.csv")


hOGetOut  <-searchTwitter("#Oscars 'Get Out'", n=7000, lang = "en")
hOGetOut  <-twListToDF(hOGetOut)
export(hOGetOut, "/Users/Luojiefan/Desktop/hOGetOut.csv")


hOCoco  <-searchTwitter("#Oscars 'Coco'", n=7000, lang = "en")
hOCoco  <-twListToDF(hOCoco)
export(hOCoco, "/Users/Luojiefan/Desktop/hOCoco.csv")



############################
#Just use name as search criteria
garyoldman_tweets <-searchTwitter("Actor Gary Oldman", n=7000, lang = "en")
garyoldman_tweetsDF<-twListToDF(garyoldman_tweets)
export(garyoldman_tweetsDF, "/Users/Luojiefan/Desktop/Actor Gary Oldman.csv")

danieldaylewis_tweets <-searchTwitter("Actor Daniel Kaluuya", n=7000, lang = "en")
danieldaylewis_tweetsDF<-twListToDF(danieldaylewis_tweets)
export(danieldaylewis_tweetsDF, "/Users/Luojiefan/Desktop/Actor Daniel Kaluuya.csv")


GuillermodelToro_tweets <-searchTwitter("Director Guillermo del Toro", n=7000, lang = "en")
GuillermodelToro_tweetsDF<-twListToDF(GuillermodelToro_tweets)
export(GuillermodelToro_tweetsDF, "/Users/Luojiefan/Desktop/Director_GuillermodelToro.csv")

ChristopherNolan_tweets <-searchTwitter("Director Christopher Nolan", n=7000, lang = "en")
ChristopherNolan_tweetsDF<-twListToDF(ChristopherNolan_tweets)
export(ChristopherNolan_tweetsDF, "/Users/Luojiefan/Desktop/Director_Christopher Nolan.csv")


BestAnimatedCoco_tweets <-searchTwitter("Animated Coco", n=7000, lang = "en")
BestAnimatedCoco_tweetsDF<-twListToDF(BestAnimatedCoco_tweets)
export(BestAnimatedCoco_tweetsDF, "/Users/Luojiefan/Desktop/Animated Coco.csv")


BestAnimatedTheBossBaby_tweets <-searchTwitter("The Boss Baby", n=7000, lang = "en")
BestAnimatedTheBossBaby_tweetsDF<-twListToDF(BestAnimatedTheBossBaby_tweets)
export(BestAnimatedTheBossBaby_tweetsDF, "/Users/Luojiefan/Desktop/The Boss Baby.csv")

BestOriginalScreenplayGetOut_tweets <-searchTwitter("Screenplay Get Out", n=7000, lang = "en")
BestOriginalScreenplayGetOut_tweetsDF<-twListToDF(BestOriginalScreenplayGetOut_tweets)
export(BestOriginalScreenplayGetOut_tweetsDF, "/Users/Luojiefan/Desktop/Screenplay Get Out.csv")

BestOriginalScreenplayTheBigSick_tweets <-searchTwitter("Screenplay The Shape of Water", n=7000, lang = "en")
BestOriginalScreenplayTheBigSick_tweetsDF<-twListToDF(BestOriginalScreenplayTheBigSick_tweets)
export(BestOriginalScreenplayTheBigSick_tweetsDF, "/Users/Luojiefan/Desktop/Screenplay The Shape of Water.csv")


BestOriginalScreenplayTheBigSick_tweets <-searchTwitter("Screenplay The Shape of Water", n=7000, lang = "en")
BestOriginalScreenplayTheBigSick_tweetsDF<-twListToDF(BestOriginalScreenplayTheBigSick_tweets)
export(BestOriginalScreenplayTheBigSick_tweetsDF, "/Users/Luojiefan/Desktop/Screenplay The Shape of Water.csv")


###Search for each catergories use keyword
BestPicture90Oscar <-searchTwitter("Best Picture Oscars", n=7000, lang = "en")
BestPicture90Oscar_tweetsDF<-twListToDF(BestPicture90Oscar)
export(BestPicture90Oscar_tweetsDF, "/Users/Luojiefan/Desktop/Best Picture Oscars.csv")


BestDirector90Oscars <-searchTwitter("Best Director Oscars", n=7000, lang = "en")
BestDirector90Oscars<-twListToDF(BestDirector90Oscars)
export(BestDirector90Oscars, "/Users/Luojiefan/Desktop/Best Director Oscars.csv")


BestActor90Oscars <-searchTwitter("Best Actor Oscars", n=7000, lang = "en")
BestActor90Oscars<-twListToDF(BestActor90Oscars)
export(BestActor90Oscars, "/Users/Luojiefan/Desktop/Best Actor Oscars.csv")


BestoriginalscreenplayOscars <-searchTwitter("Best original screenplay Oscars", n=7000, lang = "en")
BestoriginalscreenplayOscars<-twListToDF(BestoriginalscreenplayOscars)
export(BestoriginalscreenplayOscars, "/Users/Luojiefan/Desktop/Best original screenplay Oscars.csv")


BestAnimatedOscars <-searchTwitter("Best Animated Oscars", n=7000, lang = "en")
BestAnimatedOscars<-twListToDF(BestAnimatedOscars)
export(BestAnimatedOscars, "/Users/Luojiefan/Desktop/Best Animated Oscars.csv")



#Search by movie name
TheShapeofWater <-searchTwitter("The Shape of Water", n=7000, lang = "en")
TheShapeofWater<-twListToDF(TheShapeofWater)
export(TheShapeofWater, "/Users/Luojiefan/Desktop/The Shape of Water.csv")

ThreeBillboards <-searchTwitter("Three Billboards", n=7000, lang = "en")
ThreeBillboards<-twListToDF(ThreeBillboards)
export(ThreeBillboards, "/Users/Luojiefan/Desktop/Three Billboards.csv")

GaryOldman <-searchTwitter("Gary Oldman", n=7000, lang = "en")
GaryOldman<-twListToDF(GaryOldman)
export(GaryOldman, "/Users/Luojiefan/Desktop/Gary Oldman.csv")

DanielKaluuya <-searchTwitter("Daniel Kaluuya", n=7000, lang = "en")
DanielKaluuya<-twListToDF(DanielKaluuya)
export(DanielKaluuya, "/Users/Luojiefan/Desktop/Daniel Kaluuya.csv")

GuillermodelToro <-searchTwitter("Guillermo del Toro", n=7000, lang = "en")
GuillermodelToro <-twListToDF(GuillermodelToro)
export(GuillermodelToro, "/Users/Luojiefan/Desktop/Guillermo del Toro.csv")


ChristopherNolan <-searchTwitter("Christopher Nolan", n=7000, lang = "en")
ChristopherNolan <-twListToDF(ChristopherNolan)
export(ChristopherNolan, "/Users/Luojiefan/Desktop/Christopher Nolan.csv")

GetOutMovie <-searchTwitter("Get Out Movie", n=7000, lang = "en")
GetOutMovie <-twListToDF(GetOutMovie)
export(GetOutMovie, "/Users/Luojiefan/Desktop/Get Out Movie.csv")


GetOutScreenplay <-searchTwitter("Get Out Screenplay", n=7000, lang = "en")
GetOutScreenplay <-twListToDF(GetOutScreenplay)
export(GetOutScreenplay, "/Users/Luojiefan/Desktop/Get Out Screenplay.csv")


TheBigSick <-searchTwitter("The Big Sick", n=7000, lang = "en")
TheBigSick <-twListToDF(TheBigSick)
export(TheBigSick, "/Users/Luojiefan/Desktop/The Big Sick.csv")


CocoMovie <-searchTwitter("Coco Movie", n=7000, lang = "en")
CocoMovie <-twListToDF(CocoMovie)
export(CocoMovie, "/Users/Luojiefan/Desktop/Coco Movie.csv")


TheBossBaby  <-searchTwitter("The Boss Baby", n=7000, lang = "en")
TheBossBaby  <-twListToDF(TheBossBaby)
export(TheBossBaby, "/Users/Luojiefan/Desktop/The Boss Baby.csv")



########################
#Search for recently released movie topics
BlackPanther <-searchTwitter("#BlackPanther", n=7000, lang = "en")
BlackPanther<-twListToDF(BlackPanther)
export(BlackPanther, "/Users/Luojiefan/Desktop/BlackPanther.csv")


TombRaiderMovie <-searchTwitter("#TombRaiderMovie", n=7000, lang = "en")
TombRaiderMovie<-twListToDF(TombRaiderMovie)
export(TombRaiderMovie, "/Users/Luojiefan/Desktop/TombRaiderMovie.csv")


AWrinkleInTime <-searchTwitter("#AWrinkleInTime", n=7000, lang = "en")
AWrinkleInTime <-twListToDF(AWrinkleInTime)
export(AWrinkleInTime, "/Users/Luojiefan/Desktop/AWrinkleInTime.csv")


Annihilation <-searchTwitter("#Annihilation", n=7000, lang = "en")
Annihilation <-twListToDF(Annihilation)
export(Annihilation, "/Users/Luojiefan/Desktop/Annihilation.csv")


PeterRabbit <-searchTwitter("#PeterRabbit", n=7000, lang = "en")
PeterRabbit <-twListToDF(PeterRabbit)
export(PeterRabbit, "/Users/Luojiefan/Desktop/PeterRabbit.csv")
