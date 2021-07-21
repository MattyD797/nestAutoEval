#defining a function for building the matrices that will work with our question


## To Do ## 
# 1. Need function to identify the first 1 or 4 in the sequence
# 2. Is there a way to note attempts too? not just animals?
# this may look like: if there is a gap of 0's followed by 1s again, count it as another attempt
# 3.
# 4.
## -- ##

#RF_prediction is a dataframe output from the $predb command from the m2b workflow

# behavior_signal is a numeric of the coded behavior in the RF. In our examples, '1' corresponds to nesting while '4' corresponds to chick-tending.

# season.begin and season.end must be "MM-DD" (e.g., March 25 is "05-25"). Dates corresponding to the earliest and latest known dates that the species breeds

#period_length should be the number of days that a given behavior needs to cover before considered successful.

# 


## for testing ##
RF_prediction = readRDS("C:/Users/14064/Dropbox/BTGO Movmement Study/nestAutoEval/predicted_behaviors_oldRF.rds")
season.begin = "03-25"
season.end = "08-20"
period_length = 27
behavior_signal = "1"
## -- ##


### start function ###

#build_matrices <- function(RF_prediction, season.begin, season.end, period_length, behavior_signal){ 
  

#extract Julian day
Julian <- as.numeric(format(RF_prediction$t, "%j"))
RF_prediction <- cbind(RF_prediction, Julian)


#define bounds of the 
#Nesting - between 70 and 213
#chick tending - between 80 and 244

#fate
beh <- RF_prediction %>% group_by(id, Julian) %>% count(b)

#define fate
beh <- beh[beh$b==as.numeric(behavior_signal),]

#create empty fate matrix
mat_beh <- matrix(NA, nrow = length(unique(beh$id)), ncol = 365)

#loop through individuals
for(i in 1:length(beh$id)){
  mat_beh[match(beh[i,1], pull(unique(beh[,1]))),
          as.numeric(beh[i, 2])] <- as.numeric(beh[i,4])
}

#fate matrix
mat_beh[is.na(mat_beh)] <- 0

for(i in 1:nrow(mat_beh)){ tmp <-mat_beh[i,max(which(mat_beh[i,] > 2))];  print(as.data.frame(tmp))}

apply(mat_beh>0,2,which.max)

max.col(t(mat_beh >0), "last")

#GPS fixes

fixes <- RF_prediction %>% group_by(id) %>% count(Julian)

#create blank matrix to fill
mat_fix <- matrix(NA, nrow = length(unique(fixes$id)), ncol = 365)


for(i in 1:length(fixes$id)){
  mat_fix[match(fixes[i,1], pull(unique(fixes[,1]))),
          as.numeric(fixes[i, 2])] <- as.numeric(fixes[i,3])
}

#GPS_fix_matrix
mat_fix[is.na(mat_fix)] <- 0




# gps.fixes.df <- as.data.frame(gps.fixes)
# head(gps.fixes.df)


#


colnames(mat_fix) <- colnames(mat_beh) <- NULL
 
rownames(mat_fix) <- rownames(mat_beh) <- c(unique(beh$id))

str(mat_beh); str(mat_fix)  

  #convert to POSIXct
  season.begin_fmt <- as.POSIXct(season.begin, format = "%m-%d")
  season.end_fmt <- as.POSIXct(season.end, format = "%m-%d")
  
  
  ## subset matrices to season lenght ##

 mat_beh <- mat_beh[,as.numeric(format(season.begin_fmt, "%j")):as.numeric(format(season.end_fmt, "%j"))]
 
 mat_fix <- mat_fix[,as.numeric(format(season.begin_fmt, "%j")):as.numeric(format(season.end_fmt, "%j"))]
  

 #identify first non-zero value in each row
 tmp_start <- as.vector(apply(mat_beh, 1, function(x) which(x!=0, arr.ind=T)))
 
 #convert to list, then vector
 lst <- list(NA,nrow(mat_beh))
 
 for(i in 1:nrow(mat_beh)){
   lst[i] <- min(tmp_start[[i]])
 }
 
 tmp_start <- as.vector(do.call(rbind, lst))
 
 tmp_end <- tmp_start + period_length
 
 #use each to subset the matrices
 for(i in 1:nrow(mat_beh)){
   
   lst[[i]] <- mat_beh[i,c(mat_beh[i,tmp[i]]:mat_beh[i,tmp[i]+period_length])]
   
   }
  
 mat_beh_final <- rbind(lst)
 
  ## identify first date in which each bird exhibited behavior ##
  
  list_start <- as.data.frame(as.numeric(colnames(nest.beh.final)[apply(nest.beh.final==behavior_signal,1, which.max)]))
 
  list_end <- list_start + (period_length)
  
  for(i in 1:nrow(nest.beh.final)){
  nest.beh.final[i,] <- nest.beh.final[i,c(list_start[i], list_end[i])]}
  
  for(i in 1:nrow(gps.fixes.final)){
    gps.fixes.final[i,] <- gps.fixes.final[i,c(list_start[i], list_end[i])]}
  
  

  
  
# }

#build_matrices()

build_matrices(RF_prediction = predicted_behaviors_oldRF, season.begin = "03-25", season.end = "08-20", period_length = 27, behavior_signal = "1")



#check
str(mat_fix); str(mat_beh)










### first attempt below, built after the nestR example











build_matrices <- function(RF_prediction, season.begin, season.end, period_length, behavior){
  
  #create unique animal identifier
  ind <- as.character(unique(RF_prediction$burst)) # here we need something that can handle repeat nesting (they will renest if nest fails first time)
  
   ## Initialize final, blank matrices ##
  
  # Observation process
  mat_fix <- matrix(NA, nrow = nrow(ind), ncol = period_length)
  
  # Survival process
  mat_beh <- matrix(NA, nrow = nrow(ind), ncol = period_length)
 
  #set rownames
  rownames(mat_fix) <- rownames(mat_beh) <- 1:nrow(mat_fix)
  
  ## create year matrices ##
  
  #extract julian day
  Julian <- as.numeric(format(RF_prediction$t, "%j"))
  predicted_full <- cbind(RF_prediction, Julian)
  
  
  #fate
  nest.beh <- predicted_full %>% group_by(id, Julian) %>% count(b)
  nest.beh <- nest.beh[nest.beh$b==behavior,]
  
  nest.beh.final <- matrix(nrow = length(unique(nest.beh$id)), ncol = 365)
  rownames(nest.beh.final) <- c(unique(nest.beh$id))
  
  for(i in 1:length(nest.beh$id)){
    nest.beh.final[match(nest.beh[i,1], pull(unique(nest.beh[,1]))),
                   as.numeric(nest.beh[i, 2])] <- as.numeric(nest.beh[i,4])
  }
 
   #create fate matrix
  nest.beh.final[is.na(nest.beh.final)] <- 0
  
  for(i in 1:nrow(nest.beh.final)){ tmp <- nest.beh.final[i,max(which(nest.beh.final[i,] > 2))];  print(as.data.frame(tmp))}
  
  apply(nest.beh.final>0,2,which.max)
  
  max.col(t(nest.beh.final >0), "last")
  
  #GPS fixes
  gps.fixes <- RF_prediction %>% group_by(id) %>% count(Julian)
  
  gps.fixes.final <- matrix(nrow = length(unique(gps.fixes$id)), ncol = 365)
  rownames(gps.fixes.final) <- c(unique(gps.fixes$id))
  
  
  
  for(i in 1:length(gps.fixes$id)){
    gps.fixes.final[match(gps.fixes[i,1], pull(unique(gps.fixes[,1]))),
                    as.numeric(gps.fixes[i, 2])] <- as.numeric(gps.fixes[i,3])
  }
  
  #GPS_fix_matrix
  gps.fixes.final[is.na(gps.fixes.final)] <- 0
  
  
  ## account for multiple attempts ##
  
  # e.g. if a bird has a string of 0s followed by a restart, count it as a seperate attempt
  
  #loop over breeding attempts
  for(i in 1:nrow(ind)){
    
    #select current nest
    trk <- ind[i,]
    
    ## Here is where we need to specify to count past first type ##
    
    #call it first_beh
    foo1 <- function(x){which(x == signal)[1]}
    apply(trk, 2,foo1)
    
    start <- min(which(trk[i]) == signal)
    
    #data on behavior 
    beh_tmp <- RF_prediction %>% 
      dplyr::filter(burst == i) %>%
    #cut between start and end of cycle
    dplyr::filter(datetime >= as.POSIXlt(season.begin),
                  datetime <= as.POSIXlt(season.begin + period_length))
    
    # Count daily fixes within attempt
    fix <- beh_tmp %>%
      dplyr::group_by(datetime = lubridate::as_date(date)) %>%
      dplyr::tally()
 
    #count daily times behavior recorded within attempt
    beh <- beh_tmp %>%
      dplyr::filter(loc_id == trk$loc) %>%
      dplyr::group_by(date = lubridate::as_date(date)) %>%
      dplyr::tally()
    
    #initialize history
    history <- data.frame(date = lubridate::as_date(
        first_beh:(first_beh + nest_cycle - 1)))
    
    #join n of fix and beh
    history <- dplyr::left_join(history, fix, by = "date")
    history <- dplyr::left_join(history, beh, by = "date")
    names(history) <- c("date", "fix", "beh")
    
    #replace NAs with zeros
    history <- history %>%
      dplyr::mutate(fix = case_when(
        is.na(fix) ~ as.integer(0),
        TRUE ~ fix
      )) %>%
      dplyr::mutate(beh = case_when(
        is.na(beh) ~ as.integer(0),
        TRUE ~ beh
      ))
    
    #plug values into matrices
    mat_fix[i,] <- history$fix
    mat_beh[i,] <- history$vis
    
    # Name rows with attempt ID
    rownames(mat_fix)[i] <- trk
    rownames(mat_beh)[i] <- trk
 
    }
   
   mats <- list(mat_fix, mat_beh)
   names(mats) <- c("fixes", "behavior")
   
   return(mats)
  
}


