#Read in data
#First row should contain variable name
#Second row should contain question text, this is removed
#To-do: Write unit test/check to confirm this structure when loading in data
#Removing question text and other participant identification info, first variable is strategic clarity

x <- 'OASIS_Connections_Drive_Performance_in_Energy_Companies 10-25.csv'
y <- 'Oasis Panel Draft v2.xls'

tmt_process <- function(x,y)
{
  
QUALTRICS_DATA <- read_csv(x, 
                      na = c(''), 
                      col_names = T,
                      progress = T)[-1,-c(1,2,6,7,8,9)]

colnames(QUALTRICS_DATA)[1] <- "Name" 

#Read In Paneel 
PANEL_DATA <- read_excel(y,
                         col_names = T)
PANEL_DATA$Name <- paste(PANEL_DATA$`Last Name`,
                         ', ',
                         PANEL_DATA$`First Name`,sep = "")

FNLN_PANELS <- PANEL_DATA[grep(pattern = 'FT[0-9]$',x = colnames(PANEL_DATA))]

FNLN_PANELS <- FNLN_PANELS[c(1:length(FNLN_PANELS[, colSums(is.na(FNLN_PANELS)) != nrow(FNLN_PANELS)]))] %>% data.frame

name_rearrange <- function(x){
  apply(X = do.call(rbind,str_split(string = x, pattern = " "))[,c(2,1)],MARGIN = 1,FUN = paste,sep=' ',collapse=', ')
}

FNLN_PANELS <- apply(X = FNLN_PANELS, 2, name_rearrange) %>% gsub(pattern = "NA, NA",replacement = NA,.)

PANEL_DATA[grep(pattern = 'FT[0-9]$',x = colnames(PANEL_DATA))][c(1:ncol(FNLN_PANELS))] <- FNLN_PANELS 

MERGED_DATA <- merge(x = PANEL_DATA, y = QUALTRICS_DATA, by = "Name", all.x = TRUE) %>% unique

#First step makes a mean composite for each variable in Section 1
#Uses the composite function from the multicon package
#Refer to the CCL v. Oasis Data Comparison file for a list of variable names
#The goal is to keep this general (DRY principles)

#Step by step
#Creates a matrix of variable names from section 1 of the Oasis survey
#Ideally, new variables can be added / taken away from this with future survey iterations
#Subsets the oasisData object for those variables
#Applies the multicon::composite() function to this subset
#Returns a nPerson row x 1 vector of composite scores
#cbinds these vectors together into composite dataframe objects called sec1_composites
#Some rows have NAs due to missing values from the survey data
#Arguments that go into the composite function were taken from previous code (e.g. tr=0, nomiss=0.1, etc.)

#Join on Qualtrics panel data

INDV_VARS <- c("stratclar",'stratcom','competitors','pillar','agility','perf','PI','PJ','DJ')

#Returns individual-level data
sec1_composites <- data.frame(
  apply(
    X = matrix(INDV_VARS),
    MARGIN = 1,
    FUN = function(x){
      composite(
        apply(
          X = MERGED_DATA[grep(pattern = x,colnames(MERGED_DATA),value = T)], 1, as.numeric) %>% t, R = NULL, Zitems = FALSE, maxScore = NULL, rel=FALSE, nomiss = 0.1, tr = 0)
    }
  )
)

colnames(sec1_composites) <- c("stratclar",'stratcom','competitors','pillar','agility','perf','PI','PJ','DJ')

sec1_composites <- cbind(MERGED_DATA$Name,MERGED_DATA$FTname, MERGED_DATA$Role,MERGED_DATA$FTrole, sec1_composites)

#Checking to ensure the dimensions of the section 1 composite scores is equivalent to the rows of the survey data and the number of variables in section 1

#Change format of this potentially

dim(sec1_composites) == c(nrow(MERGED_DATA),length(c("stratclar",'stratcom','competitors','pillar','agility','perf','PI','PJ','DJ')))

#Section 2 is network data
#Variables are: know, sc(x2), lead(x2), integ, comp
#This section creates 7 graph objects using igraph for each of these network variables.
#The OASIS data had 29 responses but had each person rate 60 people on various interpersonal variables. My assumption is that there will be the number of respondents will rate an equal number of their peers, creating a nPerson x nPerson adjacency matrix. To accomodate any misalignment between the number of people being rated and the number of people completing the survey, I have code attached to rbind NA's to the rater x ratee matrix making it easier to create an adjacency matrix for network statistics / visualizations

#This function adds a 0 to the diagonal of a NxN matrix to make it a N+1 x N+1 matrix
#Given a person can't evaluate themselves on network questions, it makes the self-evaluation (diagnoal) 0.
# adj_matrix <- function(mat){
#   matrix(apply(X = matrix(1:ncol(mat)),
#                MARGIN = 1,
#                FUN = function(x,y)
#                {
#                  c(y[1:x-1,x],
#                    0,
#                    y[x:ncol(y),x])},
#                y=mat
#   ), nrow=nrow(mat)+1, ncol=ncol(mat)+1)
# }

#This function subsets the network variables from the survey, adds NAs to ensure the matrix is square, uses the previous function to make an adjacency matrix, and then makes a graph object using igraph
# return_adj_matrix <- function(x){
#   
#   subset_matrix <- oasisData[,grep(pattern = x, x = colnames(oasisData))]
#   
#   na_matrix <- matrix(NA, nrow = ncol(subset_matrix) - nrow(subset_matrix), ncol = ncol(subset_matrix)) %>% data.frame
#   
#   colnames(na_matrix) <- colnames(subset_matrix)
#   
#   return(list(x,adj_matrix(rbind(subset_matrix,na_matrix)) %>% apply(., 2, as.numeric) %>% graph_from_adjacency_matrix))
# }

#This section applies the previous function over the network variables
#Creates a single list of igraph objects that can be plotted or used to compute network scores
adj_matrix_list <- apply(X = matrix(c("know_",
                                      "comp_",
                                      "integ_",
                                      "^sc_.*\\_1$",
                                      "^sc_.*\\_2$",
                                      "^lead_.*\\_1$",
                                      "^lead_.*\\_2$")),1,FUN = function(x){apply(X = matrix(unlist(matrix(MERGED_DATA[grep(pattern = x,x = colnames(MERGED_DATA))])),ncol = nrow(MERGED_DATA)),2,as.numeric) %>% graph_from_adjacency_matrix
                                        }
                         )

adj_matrix_list <- setNames(object = adj_matrix_list, nm = c("know_",
                                          "comp_",
                                          "integ_",
                                          "^sc_.*\\_1$",
                                          "^sc_.*\\_2$",
                                          "^lead_.*\\_1$",
                                          "^lead_.*\\_2$"))

######

#Sections 3 and in progress and could use clarification

#Skip A_task and I_task, need help

#FTrole
#Subset each team
#Replace colnames of role variable with the people they have on their team
#Order stays consistent
#Create adjacency matrix object for each team
#

#This section
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


rq <- MERGED_DATA[,c('Name','FTname','FTsize',
                     paste("FT",c(1:8),sep = ''),
                     grep(pattern = 'FTrq',x = colnames(MERGED_DATA),value = T))]

rq<-rq[order(rq$FTname),]

rq <- apply(X = matrix(unique(rq$FTname)),MARGIN = 1,FUN = function(x){return(
  rq[which(rq$FTname==x),]
)})

rq <- rq[lapply(rq,nrow) > 0]

rq_adj_matrix <- apply(X = matrix(c(1:length(rq))), MARGIN = 1, FUN = function(x){

for(i in 1:length(rq)){  
colnames(rq[[i]])[grep(pattern = 'FTrq',colnames(rq[[i]]))] <- na.omit(do.call(rbind,rm_between(colnames(rq[[i]]), 'FTrq', '_', extract=TRUE))) %>% matrix
}
  
  rq_subset <- rq[[x]][c(
  1,2,3,
  grep(pattern = 'FT[0-9]',x = colnames(rq[[x]])),
  which(colnames(rq[[x]]) %in% rq[[x]]$FTsize))]

colnames(rq_subset)[(ncol(rq_subset)-rq_subset$FTsize[1]+1):ncol(rq_subset)] <- unique(rq[[x]][grep(pattern = 'FT[0-9]',x = colnames(rq[[x]]))])[1:rq[[x]]$FTsize[1]]

return(rq_subset[,-grep(pattern = "FT",x = colnames(rq_subset))])
})

###Section 4###
#FTPA for heatmap

'%!in%' <- function(x,y)!('%in%'(x,y))

ftpa <- cbind(MERGED_DATA$Name,MERGED_DATA$FTname,MERGED_DATA[grep(pattern = 'FTPA_',x = colnames(MERGED_DATA),value = T)])

ftpa <- ftpa[order(ftpa$`MERGED_DATA$FTname`),]

ftpa <- ftpa[ftpa$`MERGED_DATA$FTname` %!in% c('Investor Relations Team',NA),]

ftpa[,-c(1,2)] <- data.frame(apply(X = ftpa[,-c(1,2)], 2, as.numeric))

ftpa <- cbind(matrix(unique(ftpa$`MERGED_DATA$FTname`)), do.call(rbind,lapply(X = apply(X = matrix(unique(ftpa$`MERGED_DATA$FTname`)),MARGIN = 1,function(x){ftpa[ftpa$`MERGED_DATA$FTname`==x,-c(1,2)]}),colMeans,na.rm=T))) %>% data.frame

#AW
AW <- data.frame(MERGED_DATA$Name, MERGED_DATA$FTname, MERGED_DATA[grep(pattern = 'FTAW',x = colnames(MERGED_DATA))])

#AI
AI <- data.frame(MERGED_DATA$Name, MERGED_DATA$FTname, MERGED_DATA[grep(pattern = 'FTAI',x = colnames(MERGED_DATA))])

#AC
AC <- data.frame(MERGED_DATA$Name, MERGED_DATA$FTname, MERGED_DATA[grep(pattern = 'FTAC',x = colnames(MERGED_DATA))])

#instrumental_leadership
instrumental_leadership <- data.frame(instrumental_leadership = 
  apply(
    X = matrix('flead'),
    MARGIN = 1,
    FUN = function(x){
      composite(
        apply(
          X = MERGED_DATA[grep(pattern = x,colnames(MERGED_DATA),value = T),c(1,3,4,5,6,7,8,17)], 1, as.numeric) %>% t, R = NULL, Zitems = FALSE, maxScore = NULL, rel=FALSE, nomiss = 0.1, tr = 0)
    }
  )
)


#participative
participative_leadership <- data.frame(participative_leadership = 
                                        apply(
                                          X = matrix('flead'),
                                          MARGIN = 1,
                                          FUN = function(x){
                                            composite(
                                              apply(
                                                X = MERGED_DATA[grep(pattern = x,colnames(MERGED_DATA),value = T),c(9,11,12,13,14,18)], 1, as.numeric) %>% t, R = NULL, Zitems = FALSE, maxScore = NULL, rel=FALSE, nomiss = 0.1, tr = 0)
                                          }
                                        )
)

#Fairness
fairness <- data.frame(fairness = 
                                         apply(
                                           X = matrix('flead'),
                                           MARGIN = 1,
                                           FUN = function(x){
                                             composite(
                                               apply(
                                                 X = MERGED_DATA[grep(pattern = x,colnames(MERGED_DATA),value = T),c(2,10)], 1, as.numeric) %>% t, R = NULL, Zitems = FALSE, maxScore = NULL, rel=FALSE, nomiss = 0.1, tr = 0)
                                           }
                                         )
)


#c('JDM', 'CB', 'relcoh', 'DAC')

INDV_VARS <- c('FTjdm', 'FTcb', 'FTrelcoh', 'FTDAC')

#Returns individual-level data
sec4_composites <- data.frame(
  apply(
    X = matrix(INDV_VARS),
    MARGIN = 1,
    FUN = function(x){
      composite(
        apply(
          X = MERGED_DATA[grep(pattern = x,colnames(MERGED_DATA),value = T)], 1, as.numeric) %>% t, R = NULL, Zitems = FALSE, maxScore = NULL, rel=FALSE, nomiss = 0.1, tr = 0)
    }
  )
)

colnames(sec4_composites) <- c('jdm', 'CB', 'relcoh', 'DAC')

sec4_composites <- cbind(MERGED_DATA$Name,MERGED_DATA$FTname, MERGED_DATA$Role,MERGED_DATA$FTrole, sec4_composites)

# #Renames columns to team members names who are being rated
# #Ignore the warnigns
# rq_teamlist <- apply(X = matrix(c(1:length(rq_teamlist))),MARGIN = 1,
#       function(x){
# 
# for(i in c(1:length(rq_teamlist))){
#   if(length(colnames(rq_teamlist[[i]])[grep(pattern = 'FTrq',x = colnames(rq_teamlist[[i]]))]) > 0 ){
#     colnames(rq_teamlist[[i]])[grep(pattern = 'FTrq',x = colnames(rq_teamlist[[i]]))] <- rq_teamlist[[i]][grep(pattern = 'FT[0-9]',x = colnames(rq_teamlist[[i]]))] %>% unique
# 
#   }}
# 
# return(rq_teamlist[[x]][,-c(2,grep(pattern = 'FT[0-9]',colnames(rq_teamlist[[x]])))])
# })


#This code is the same as section one and is currently applied over the LSS and environ variables. I have a few questions on what to do with other items and would like to follow up if possible.
sec5_composites <- data.frame(
  apply(
    X = matrix(c('enviro_','LSS_')),
    MARGIN = 1,
    FUN = function(x){
      composite(
        apply(
          X = oasisData[grep(pattern = x,colnames(oasisData),value = T)], 1, as.numeric) %>% t, R = NULL, Zitems = FALSE, maxScore = NULL, rel=FALSE, nomiss = 0.1, tr = 0)
    }
  )
)

colnames(sec5_composites) <- c('enviro','LSS')

return(
  list(
    adj_matrix_list,
    AC,
    AI,
    AW,
    fairness,
    ftpa,
    instrumental_leadership,
    participative_leadership,
    rq_adj_matrix,
    sec1_composites,
    sec4_composites,
    sec5_composites
    )
  )
}

#This code should create 6 objects in the global environment

#CSS is only completed by CEO, who are the top 5 connected people are?
#How well do they know they network?

# Add panel data information
# Join on indiviual-level info
# 'External data reference'
#

# Curtis thoughts
# Make a Github repo
# TMT networks

######
#Section 3 would have mean scores by person (actual vs ideal)
#Maybe aggregated by team

#FTrq8 presents questions based on size of team
#Within-team networks
#If they're a 4 person they only see 4
#Number after rq is team size
#The _1 matches the person in the panel
#Depending on the ID of the person, they will have different people in their FT questions

#Section 3
#JDM, relcoh, DAC, lead (no leader in this), perf (only leader in this), 

#Section 4
#Teams are listed in the order their teams are listed in survey
#network or heatmap















