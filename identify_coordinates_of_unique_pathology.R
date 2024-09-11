#This code goes through each connected component (lesion) in the sum of all brain lesion segmentation images for a given subject and checks to see if the lesion of interest exists in each individual segmentation. If the lesion is "disappearing" (ie., does not exist for a pathological reason or was not segmented) the visit where the disappearing lesion occurred and the index of a voxel where this lesion exists in the sum image (and individual images since they're registered to the same space as the sum) is recorded in a df to be checked visually for errors or actual cases of pathologically "disappearing lesions"

#Image processing packages
library(neurobase)
library(ANTsR)
library(extrantsr)
library(fslr)

#Used to run functionin parallel
library(parallel)

#Set working directory
setwd("/path/to/manual/segmentations")


#list file names of all manual segmentation images that have connected components labelled
scans = list.files(path = "./registered_FLAIRS_and_FLAIR_segmentations",
                   pattern="_lesion_binary_mask_registered.nii.gz",
                   full.names = TRUE)

#subject ID list
subject = c("01-001","01-002","01-003","02-001","02-002","02-003","03-001","03-002","04-001","04-002","04-003")

do.lesion = function(i){
  #Extract ID and Subj for print statements
  label = gsub("_FLAIR_SAG_VFL_lesion_binary_mask_registered.nii.gz","",strsplit(scans[i],"/")[[1]][3])
  subj = strsplit(label,"_")[[1]][1]
  Site_Visit = strsplit(label,"_")[[1]][2]
  
  #list all of the "sum" files (sum of all manual segmentations for given subject)
  sum_path = paste0("./sum_registered_lesion_masks/cc_sum_lesion_masks/",subj,"_lesion_masks_sum_CC.nii.gz")
  
  #read in segmentation sum and individual segmentation 'i'
  sum = readnii(sum_path)
  img = readnii(scans[i])

  #get total number of connnected components in sum mask (proxy for lesion number)
  number_of_CC = 1:max(sum)

  #initialize empty list to store result (non-unique lesion/connected component label)
  not_unique = list() 
  
  #loop through each connected component (lesion)
   for (l  in number_of_CC){

  #subset individual lesion of interest by CC label  
  lesion = which(sum==l, arr.ind=TRUE)

  #loop through each voxel index within individual lesion 
  for (j in 1:nrow(lesion)){

  index = lesion[j,]
 
  #lesion indices 
  x = (index[1])
  y = (index[2])
  z = (index[3])
  
  #print messages to display if condition is met
  #unique = paste0("Lesion ", l, " is unique")
  not_unique = paste0("Lesion ", l, " from ",subj, " is not unique")
   
  #if any of the voxel indices in the connected component lesion sum mask in the lesion of interest correspond to an intensity of 1 in the individual segmentation masks then lesion is present/has been segmented in the individual image, so stop current iteration and move to next lesion/connected component and repeat process
  if (img[x,y,z]==1){
      print(not_unique)
    
    #Keep track of non-unique lesions in not_unique list
    not_unique = c(not_unique,l)
    not_unique_list=as.numeric(unlist(not_unique))
      stop=TRUE
       break}
  }
  
  #Lesion numbers that are not present in the non-unique list are potential unique/disappearing lesions
  unique_list = number_of_CC[!(number_of_CC %in% not_unique)]
  unique_lesion_df = data.frame()
  
  #Create a dataframe of indices for a single voxel index associared with each "Unique" connnected component
  #Resulting data frame is used to input indices into image viewer to quickly identify ROI and visually confirm pathology rather than doing this all by eye/hand
  for (z in 1:length(unique_list)){
    lesion_label = unique_list[z]
    lesion = which(sum==unique_list[z], arr.ind=TRUE)
    index = lesion[1,]
    x = (index[1])
    y = (index[2])
    z = (index[3])
    ok = cbind(subj,lesion_label,x,y,z)
    unique_lesion_df = rbind(unique_lesion_df,ok)
     unique_lesion_df[!duplicated(unique_lesion_df[c(1,2)]),]
  }
            
  
   }
  rownames(unique_lesion_df)=NULL
  # write.csv(unique_lesion_df,paste0("./",label,"_unique_lesion_df.csv"))
            
          
}

i = 1:length(scans)
#utilize 11 cores at a time (1 for each subject)
mclapply(i, do.lesion,mc.cores=11)


