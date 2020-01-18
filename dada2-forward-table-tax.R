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

for (i in list.files(path, pattern = "_1.subsample.fastq.gz")){
  fns <- list.files(path, pattern = i)
  out<-filterAndTrim(fwd=file.path(path, fns), filt=file.path(filtpath, fns),
                     maxN=0, maxEE=1, truncQ=2, trimLeft = 20, rm.phix=TRUE, verbose = T,
                     compress=TRUE, multithread=TRUE)
  
  ####
  filtpath <- "filtered/" # Change to the directory containing your filtered fastq files
  filts <- list.files(filtpath, pattern= i, full.names=T) # CHANGE if different file extensions
  sample.names <- basename(filts) # Takes advantage of Illumina-produced sequence names
  names(filts) <- sample.names
  set.seed(100)
  err <- learnErrors(filts, multithread=TRUE, randomize=TRUE, nbases = 1e3)
  dds <- vector("list", length(sample.names))
  names(dds) <- sample.names
  
  for(sam in sample.names) {
    
    cat("Processing:", sam, "\n")
    derep <- derepFastq(filts[[sam]])
    dds[[sam]] <- dada(derep, err=err, multithread=TRUE)
    
  }
  seqtab <- makeSequenceTable(dds)
  outlist[[i]] <- seqtab
  outlist2[[i]] <- out
}

st.all <- mergeSequenceTables(tables = outlist)
seqtab <- removeBimeraDenovo(st.all, method="consensus", multithread=TRUE)

#produce the tracking read table

track <- cbind(sapply(outlist2, function(x){as.numeric(x[1])}),
               sapply(outlist2, function(x){as.numeric(x[2])}),
               rowSums(seqtab))
colnames(track) <- c("input", "filtered", "nonchim")
head(track)
write.csv(track, "track.csv")

# assign taxonomy as per greengenes database v 13.8 NB! It needs a download
tax <- assignTaxonomy(seqtab, 
                      "gg_13_8_train_set_97.fa.gz?download=1", 
                      multithread=TRUE, outputBootstraps = T)
# save goodies
saveRDS(seqtab, "seqtab.nochim.rds")
saveRDS(tax, "tax.rds")
