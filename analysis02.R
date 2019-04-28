#csv file has to be in folder that project is saved in
dataframe=read.csv("tanzania02.csv")
hist(dataframe$NEAR_FID)
dataframe$treated=0
dataframe[dataframe$NEAR_DIST<50000,]["treated"]=1
#create no null dataframe with values that we want to be complete for both years to allow comparison
dataframe_nonull=dataframe[complete.cases(dataframe[c("mergedata_air_pollution_o3_1_130", "mergedata_esa_landcover_v207_146", "mergedata_ltdr_avhrr_yearly__207")]),]
dataframe_nonull=dataframe_nonull[c("mergedata_air_pollution_o3_1_130", "mergedata_esa_landcover_v207_146", "mergedata_ltdr_avhrr_yearly__207", "treated")]
library("MatchIt")
matchingmodel=matchit(treated~mergedata_air_pollution_o3_1_130+mergedata_esa_landcover_v207_146+mergedata_ltdr_avhrr_yearly__207,
                      data=dataframe_nonull,
                      caliper=0.25)
matchingmodel$model
matcheddataframe=match.data(matchingmodel)
matchingmodel
#wanting to look at Value_diff (change in assets) with variables
finalmodelanalysis=lm(Value_diff~treated+mergedata_air_pollution_o3_1_130+mergedata_esa_landcover_v207_146+mergedata_ltdr_avhrr_yearly__207,
                     data=matcheddataframe)
names(dataframe)
finalmodelanalysis=lm(Value_diff~treated+mergedata_air_pollution_o3_1_130+mergedata_esa_landcover_v207_146+mergedata_ltdr_avhrr_yearly__207,
                      data=matcheddataframe)
