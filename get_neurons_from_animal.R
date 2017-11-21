animal.folder<-'/Volumes/Tom HD1/runtry/A0015A732'
#SET UP THESE
inspected<-c(15, 23, 36, 51)
smp<-0.1
smp.coord<-c(1.1, 0.1, -1.0, -2.75) #brain coordinates based on bregma from openbrainmap.org


#coord[[1]]<-map.to.atlas(image.number=c(15, 23, 36, 51), coordinate=smp.coord, sampling.period=smp) # brain A0015A732 - HET
#coord[[2]]<-map.to.atlas(image.number=c(19, 28, 42, 61), coordinate=smp.coord, sampling.period=smp) # brain A0015A734 - HET
#coord[[3]]<-map.to.atlas(image.number=c(15, 25, 37, 54), coordinate=smp.coord, sampling.period=smp) # brain A0015A738 - WT
#coord[[4]]<-map.to.atlas(image.number=c(13, 20, 32, 45), coordinate=smp.coord, sampling.period=smp) # brain A0015A846 - WT


animal.folder<-'/Users/lab/Desktop/A0015/'

####################

brainoutlineFilter<-structure(list(alim = c(0, 1000), threshold.range = c(0L, 65536L
), eccentricity = 1000L, Max = 2252, Min = 0, brain.threshold = 563L, 
resize = 0.04, blur = 4L, downsample = 0.25), .Names = c("alim", 
                                                         "threshold.range", "eccentricity", "Max", "Min", "brain.threshold", 
                                                         "resize", "blur", "downsample"))


d3_filter<-structure(list(alim = c(8, 100), threshold.range = c(10, 30), eccentricity = 500L, Max = 20, Min = 0, brain.threshold = 563L,
                          resize = 0.0444, blur = 15L, downsample = 1), .Names = c("alim",
                                                                                   "threshold.range", "eccentricity", "Max", "Min", "brain.threshold",
                                                                                   "resize", "blur", "downsample"))



setwd(animal.folder)

images<-get.images('./stitched_A0015A732')
images<-images[seq(2,length(images),by=2)]#only FITC

d2_images<-get.images('./filtered/stitched_d3')


coord<-map.to.atlas(image.number=inspected, coordinate=smp.coord, sampling.period=smp, number.of.sections=length(images)) # brain A0015A846 - WT

files.not.processed<-character()

for(i in seq_along(images)){
  #ERROR HANDLING
  possibleError <- tryCatch({
    
    seg1<-segment(images[i], filter=brainoutlineFilter, display=FALSE)
    seg2<-segment(d2_images[i], filter=d3_filter, display=FALSE)
    
    
    regi<-registration(images[i], coordinate=coord[i], filter=seg1$filter)
    dev.off()
    setwd(animal.folder)
    create.output.directory("output")
    setwd(paste0(animal.folder,"/output"))
    dataset<-inspect.registration(registration = regi, segmentation = seg2, forward.warp=TRUE)
    dev.off()
    #get the pixel intensity and SNR
    SNR<-get.pixel.intensity(images[i], seg2$soma$x, seg2$soma$y, roi=5, background=20)
    #only slect cells that are in the brain and have an SNR higher than 1.3
    index<-which( (SNR$intensity>SNR.parameter) & !(is.na(dataset$acronym)) )
    dataset<-dataset[index,]
    #set name to animal
    dataset$animal<-basename(animal.folder)
    fluorescent<-get.pixel.intensity(images[i], dataset$x, dataset$y, type='intensity')
    dataset$intensity<-fluorescent$intensity
    save(seg1, seg2, regi, dataset, file=paste0(dataset$image[1],".RData") )
    makewebmap(images[i], registration = regi, dataset=dataset, fluorophore = 'cfos')
  }
  ,
  error=function(e) {
    e
    print(paste0("Oops! --> Error in Loop ",i))
    files.not.processed<-append(files.not.processed, basename(images[i]) )
  }
  )
  
  if(inherits(possibleError, "error")) next
  
  print(paste0("  End Loop ",i))
  
}



# LOAD ALL DATA SECTION SO YOU CAN LOOK AT THEM
setwd(paste0(animal.folder,"/output"))


data.to.be.loaded<-which(tools::file_ext(dir())=='RData')
data.to.be.loaded<-dir()[data.to.be.loaded]
load(data.to.be.loaded[1])

data.all.slices<-dataset

for(i in 2:length(data.to.be.loaded)){
  load(data.to.be.loaded[i])
  
  data.all.slices<-rbind(data.all.slices, dataset)
  
}
dataset<-data.all.slices
