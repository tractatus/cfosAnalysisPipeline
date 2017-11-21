library(profvis)
profvis({
  data(diamonds, package = "ggplot2")
  
  plot(price ~ carat, data = diamonds)
  m <- lm(price ~ carat, data = diamonds)
  abline(m, col = "red")
})







animal.folder<-'/Volumes/Tom HD1/runtry/A0015A732'
channel.names=c('FITC')
setwd(animal.folder)
create.output.directory('filtered')



filtered<-paste0(animal.folder, '/filtered/d3')


files<-list.files(animal.folder, recursive=TRUE, full.names=TRUE)
#check only for tif images
subset.tiff<-which( file_ext(files) == 'tif')
files<-files[subset.tiff]
#decide which folder to run in
run.folder<-dirname(files)
run.folder<-unique(run.folder)

#allocate variables before going into the for loop
run.image.index<-numeric()
run.plate<-character()
plate.col<-numeric()
plate.row<-numeric()
plate.order<-numeric()
brain.sections<-numeric()
tiles<-character()
stitched.filename<-character()
output.folder<-character()
memory.used<-numeric()
q=0;
for(j in run.folder){
  run.image.index<-which(dirname(files)==j)
  run.plate<-files[run.image.index]
  #get row IDs
  plate.row<-substr(basename(run.plate),1,1)
  plate.row<-as.numeric(as.factor(plate.row))
  #get column ID
  plate.col<-as.numeric(substr(basename(run.plate),5,6) )
  #plate order
  plate.order<-order(plate.col, plate.row)
  run.plate<-run.plate[plate.order]
  #get unique brain section
  brain.sections<-paste(plate.col, plate.row)
  brain.sections <-lapply(1:length(unique(brain.sections)), function(x){ rep(x, sum(brain.sections==unique(brain.sections)[x]) ) })
  brain.sections <-unlist(brain.sections)
  
  #check if you have more channels
  if(!is.null(channel.names)){
    channel.ID<-character()
    for(i in channel.names){
      search.term<-paste(i,'+', sep='')
      channel.ID[grep(search.term, basename(run.plate), perl=TRUE, value=FALSE)]<-i
    }
    
  }
  
  
  for(i in unique(brain.sections)){
    
    if(is.null(channel.names)){
      tiles<-run.plate[brain.sections==i]
      stitched.filename<-paste(basename(animal.folder),'_', formatC(i+q, digits=2, flag='0'),'.tif', sep='' )
      stitched.filename<-paste(basename(animal.folder),'_', formatC(i+q, digits=2, flag='0'), '.tif', sep='' )
      cat(paste('Stitching:', stitched.filename),'\r')
      output.folder<-paste(dirname(animal.folder), paste('stitched', basename(animal.folder), sep='_'), sep='/' )
      create.output.directory(basename(output.folder),mainDir=dirname(output.folder), verbose=FALSE)
      stitch(tiles, stitched.image.name= stitched.filename, type='row.by.row', overlap = 0.05,order='right.&.down', verbose='FALSE', output.folder =output.folder)        
    }else{
      k=length(channel.names)
      while(k!=0){
        tiles<-na.omit(run.plate[(brain.sections==i)&(channel.ID==channel.names[k])])
        stitched.filename<-paste(basename(animal.folder),'_', formatC(i+q, digits=2, flag='0'), '_', channel.names[k],'.tif', sep='' )
        cat(paste('Stitching:', stitched.filename),'\r')
        output.folder<-paste(dirname(animal.folder), paste('stitched', basename(animal.folder), sep='_'), sep='/' )
        create.output.directory(basename(output.folder),mainDir=dirname(output.folder), verbose=FALSE)
        stitch(tiles, stitched.image.name= stitched.filename, type='row.by.row', overlap = 0.05,order='right.&.down', verbose='FALSE', output.folder =output.folder)        
        stitched.file<-paste(dirname(output.folder),basename(output.folder), stitched.filename , sep='/')
        img.range<-get.range(stitched.file)
        setwd(filtered)
        makewebmap(stitched.file)
        for(tile in tiles){
         memory.used<-append(memory.used , pryr::mem_used() )
          mrd(tile, energy.trace=TRUE,  min = img.range$min, max = img.range$max, output= tools::file_path_sans_ext(basename(tile))  )
        }
        if(k>1){
        k<-k-1
        }else{
          k<-0
        }
      } 
    }
    
  }
  q=q+i
  
}

#STITCHING DONE

files<-list.files(filtered, recursive=TRUE, full.names=TRUE)
#check only for tif images
subset.tiff<-which( file_ext(files) == 'tif')
files<-files[subset.tiff]
#decide which folder to run in
run.folder<-dirname(files)
run.folder<-unique(run.folder)

#allocate variables before going into the for loop
run.image.index<-numeric()
run.plate<-character()
plate.col<-numeric()
plate.row<-numeric()
plate.order<-numeric()
brain.sections<-numeric()
tiles<-character()
stitched.filename<-character()
output.folder<-character()

q=0;
for(j in run.folder){
  run.image.index<-which(dirname(files)==j)
  run.plate<-files[run.image.index]
  #get row IDs
  plate.row<-substr(basename(run.plate),4,4)
  plate.row<-as.numeric(as.factor(plate.row))
  #get column ID
  plate.col<-as.numeric(substr(basename(run.plate),8,9) )
  #plate order
  plate.order<-order(plate.col, plate.row)
  run.plate<-run.plate[plate.order]
  #get unique brain section
  brain.sections<-paste(plate.col, plate.row)
  brain.sections <-lapply(1:length(unique(brain.sections)), function(x){ rep(x, sum(brain.sections==unique(brain.sections)[x]) ) })
  brain.sections <-unlist(brain.sections)
  
  #check if you have more channels
  if(!is.null(channel.names)){
    channel.ID<-character()
    for(i in channel.names){
      search.term<-paste(i,'+', sep='')
      channel.ID[grep(search.term, basename(run.plate), perl=TRUE, value=FALSE)]<-i
    }
    
  }
  
  for(i in unique(brain.sections)){
    
    if(is.null(channel.names)){
      tiles<-run.plate[brain.sections==i]
      stitched.filename<-paste(basename(filtered),'_', formatC(i+q, digits=2, flag='0'),'.tif', sep='' )
      stitched.filename<-paste(basename(filtered),'_', formatC(i+q, digits=2, flag='0'), '.tif', sep='' )
      cat(paste('Stitching:', stitched.filename),'\r')
      output.folder<-paste(dirname(filtered), paste('stitched', basename(filtered), sep='_'), sep='/' )
      create.output.directory(basename(output.folder),mainDir=dirname(output.folder), verbose=FALSE)
      stitch(tiles, stitched.image.name= stitched.filename, type='row.by.row', overlap = 0.05,order='right.&.down', verbose='FALSE', output.folder =output.folder)        
    }else{
      k=length(channel.names)
      while(k!=0){
        tiles<-na.omit(run.plate[(brain.sections==i)&(channel.ID==channel.names[k])])
        
        stitched.filename<-paste(basename(filtered),'_', formatC(i+q, digits=2, flag='0'), '_', channel.names[k],'.tif', sep='' )
        cat(paste('Stitching:', stitched.filename),'\r')
        output.folder<-paste(dirname(filtered), paste('stitched', basename(filtered), sep='_'), sep='/' )
        create.output.directory(basename(output.folder),mainDir=dirname(output.folder), verbose=FALSE)
        stitch(tiles, stitched.image.name= stitched.filename, type='row.by.row', overlap = 0.05,order='right.&.down', verbose='FALSE', output.folder =output.folder)        
        stitched.file<-paste(dirname(output.folder),basename(output.folder), stitched.filename , sep='/')
        img.range<-get.range(stitched.file)
        setwd(filtered)
        makewebmap(stitched.file)
        if(k>1){
          k<-k-1
        }else{
          k<-0
        }
      } 
    }
    
  }
  q=q+i
  
}

