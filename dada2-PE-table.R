args = commandArgs(trailingOnly=TRUE)

library(dada2);packageVersion("dada2")
require(HelpersMG)

getwd()
path <- getwd() # CHANGE ME to the directory containing your demultiplexed fastq files
filtpath <- file.path(path, "filtered") # Filtered files go into the filtered/ subdirectory

# declare i that is
i <- as.vector(args)
outlist=list()
outlist2=list()
run.id <- c('6007','5997')
for (run.id in list.files(path, pattern = run.id)){
  mxEEfv=1
  mxEErv=2
  fns <- list.files(path, pattern = "_1.subsample.fastq.gz")
  rvs <- list.files(path, pattern = "_2.subsample.fastq.gz")
  reads.in = c(100,100)
  reads.out=c(10,10)
  out <- as.data.frame(cbind(reads.in,reads.out))
  mean(out$reads.out)/mean(out$reads.in)
  pass_prop <- mean(out$reads.out)/mean(out$reads.in)
  while (pass_prop < 0.4){
    out<-filterAndTrim(fwd=file.path(path, fns), filt=file.path(filtpath, fns),
                       rev=file.path(path, rvs), filt.rev=file.path(filtpath, rvs),
                       maxN=0, maxEE=c(mxEEfv,mxEErv), truncQ=2, trimLeft = c(20,18),rm.phix=TRUE,verbose = T,compress=TRUE, multithread=TRUE)
    out <- as.data.frame(out)
    
    pass_prop <- mean(out$reads.out)/mean(out$reads.in)
    mxEErv= mxEErv+1
  }
    
  
  ####
  filtpath <- "filtered/" # Change to the directory containing your filtered fastq files
  filtF <- list.files(filtpath, pattern = "_1.subsample.fastq.gz", full.names=T)
  filtR <- list.files(filtpath, pattern = "_2.subsample.fastq.gz", full.names=T)
  sample.names <- basename(filtF) # Takes advantage of Illumina-produced sequence names
  names(filtF) <- sample.names
  names(filtR) <- sample.names
  set.seed(100)
  errF <- learnErrors(filtF, multithread=TRUE, randomize=TRUE, nbases = 1e3)
  errR <- learnErrors(filtR, multithread=TRUE, randomize=TRUE, nbases = 1e3)
  
  mergers <- vector("list", length(sample.names))
  names(mergers) <- sample.names
  for(sam in sample.names) {
    cat("Processing:", sam, "\n")
    derepF <- derepFastq(filtF[[sam]])
    ddF <- dada(derepF, err=errF, multithread=TRUE)
    derepR <- derepFastq(filtR[[sam]])
    ddR <- dada(derepR, err=errR, multithread=TRUE)
    merger <- mergePairs(ddF, derepF, ddR, derepR)
    mergers[[sam]] <- merger
  }
  seqtab <- makeSequenceTable(mergers)
  outlist[[i]] <- seqtab
  outlist2[[i]] <- out
}

st.all <- mergeSequenceTables(tables = outlist)
seqtab <- removeBimeraDenovo(st.all, method="consensus", multithread=TRUE)
### the ABOVE should work
#produce the tracking read table

track <- cbind(sapply(outlist2, function(x){as.numeric(x[1])}),
               sapply(outlist2, function(x){as.numeric(x[2])}),
               rowSums(seqtab))
colnames(track) <- c("input", "filtered", "nonchim")
head(track)
write.csv(track, "track.csv")
