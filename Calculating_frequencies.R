# install/ load packages
if (!require(plyr, quietly=TRUE)) {
  install.packages("plyr")
  library(plyr)
}
if (!require(tidyverse, quietly=TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(tidyr, quietly=TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(dplyr, quietly=TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(purrr, quietly=TRUE)) {
  install.packages("purrr")
  library(purrr)
}
if (!require(stringr, quietly=TRUE)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(openxlsx, quietly=TRUE)) {
    install.packages("openxlsx")
    library(openxlsx)
}
  
if (!require(readxl, quietly=TRUE)) {
      install.packages("readxl")
      library(readxl)
}

####################
## Start Analysis ##
####################

# set work directory
dir <- getwd()
setwd(dir = dir)
# get date
now <- as.Date(date(), format = "%a %b %d %H:%M:%S %Y")

# create excel workbook
wb <- openxlsx::createWorkbook()


# save input files into vector
in_files <- list.files(dir, pattern = "[0-9].txt")

for (inFile in in_files){
  sheetName <- str_remove(inFile, "\\.txt")
  print(sheetName)

  cd14 <- read_delim(paste(dir,inFile, sep = "/"), delim = "\t", 
                   col_types = cols(.default = "c")) 

  # import annotation data
  groupData <- read_delim(paste(dir,"Groups_data.txt", sep = "/"), delim = "\t", col_names = F,
                        col_types = cols(.default = "c"))
  colnames(groupData) <- c("Sample", "Group")



  test <- column_to_rownames(cd14, var = "Chr:Pos")

  test <- test %>% rownames_to_column %>% 
    gather(Position,value, -rowname) %>% 
    spread(rowname, value)

  #test$Position
  test <- test[order(match(test$Position,names(cd14))),]

  # replace ":" in names with "."
  colnames(test) <- str_replace_all(colnames(test), ":", ".")
  colnames(test) <- str_pad(colnames(test), side = "left", pad = "x", width = 12)
  #test <- test %>% mutate_all(as.character)

  test2 <- dplyr::filter(test, xxxxPosition != c("Rsid", "Ref", "Alt", "X45"))
  # remove SHP013-OS sample
  test2 <- filter(test2, xxxxPosition != "SHP013-OS")


  full_data <- full_join(groupData, test2, by = c("Sample" = "xxxxPosition"))
  full_data <- na.omit(full_data)

  positions <- names(test2)
  positions <- positions[positions != "xxxxPosition"]

  # get genotypic and allelic frequencies
  cmb_genotype_freq <- list()
  cmb_allele_freq <- list()

  # genotype fishers test
  contigency_table <- data.frame()
  # allele fishers test
  cont_tab <- data.frame()
  allele_df <- data.frame()
  hets_df <- data.frame()

  # lists to store allelic and genotypic fishers test results
  #genotypic_results_list <- list()
  hets_homo_results_list <- list()
  homo_mutatnt_results_list <- list()
  allelic_results_list <- list()

  allele_list <- list()
  #pos <- "x9.117705818"
  for (pos in positions){
    print(pos)
    #}
    # Genotypic frequencies
    total_num <- plyr::count(test2[,pos])
    names(total_num) <- c("Genotype", "Frequency")
    total_num$Ratio <- round(total_num$Frequency/sum(total_num$Frequency), 3)
    total_num$Percent <- round(total_num$Ratio * 100, 1)
    total_num$Position <- rep(pos, nrow(total_num))
    
    # allelic frequencies
    wildtype <- test[, pos][2] #test$`5:140631730`[2]
    mutant <- test[, pos][3] #test$`5:140631730`[3]
    
    # alleles lengths
    #length(total_num$Genotype)
    ref_homo <- paste0(wildtype,wildtype)
    ref_het <- paste0(wildtype,mutant)
    mut_homo <- paste0(mutant,mutant)
    
    for (gen in seq_along(total_num$Genotype)){
      print(total_num$Genotype[gen]) 
      
      if (str_detect(total_num$Genotype[gen], paste0("^",ref_homo,"$"))){
        total_num$GenotypeGroup[gen] <- "ref_homo"
      } else if (str_detect(total_num$Genotype[gen], paste0("^",ref_het,"$"))) {
        total_num$GenotypeGroup[gen] <- "ref_het"
      } else {
        total_num$GenotypeGroup[gen] <- "mut_homo"
      }
    }
    
    total_num <- total_num %>% select(Position, GenotypeGroup, 1:4)
    
    ref_len <- str_length(wildtype)
    alt_len <- str_length(mutant)
    
    wildtype_freq <- as.numeric()
    mutant_freq <- as.numeric()
    
    test3 <- vector()
    
    if (ref_len > 1){
      test3 <- str_replace_all(test2[, pos], wildtype, "Z")
      ref <- "Z"
      wildtype_freq <- sum(str_count(test3, ref))
    } else {
      test3 <- test2[, pos]
      wildtype_freq <- sum(str_count(test3, wildtype))
    }
    
    
    if (alt_len > 1){
      test3 <- str_replace_all(test2[, pos], mutant, "Z")
      alt <- "Z"
      mutant_freq <- sum(str_count(test3, alt))
    } else {
      test3 <- test2[, pos]
      mutant_freq <- sum(str_count(test3, mutant))
    }

    allelic <- data.frame(Allele = c(wildtype, mutant), A.Frequency = c(wildtype_freq, mutant_freq))
    allelic$Ratio <- round(allelic$A.Frequency/(2 * sum(total_num$Frequency)), 3)
    allelic$Percent <- round(allelic$Ratio * 100, 1)
    
    allelic$Position <- rep(pos, nrow(allelic))
    allelic$AlleleType <- names(c(ref=wildtype, mut=mutant))
    allelic <- allelic %>% select(Position, AlleleType, 1:4)
    #sum(allelic$A.Frequency)
    # get all the genotypic freqs in a list
    cmb_genotype_freq[[pos]] <- total_num
    # get all the allelic freqs for each position in a list
    cmb_allele_freq[[pos]] <- allelic

    #}
    
    # Calculate Fishers test for alleles and genotypes for group comparisons
    
    #pos <- "x5.140631730"
    
    fisher_run <- plyr::count(full_data, c("Group", pos)) # "x5.140631730"
    fisher_run2 <- spread(fisher_run, pos, freq)
    rownames(fisher_run2) <- fisher_run2$Group
    fisher_run2$Group <- NULL
    
    fisher_alleles <- full_data[,c("Group", pos)] %>% separate(pos, c("ref", "mut"), ref_len) %>% 
      gather(key = "type", value = "alleles", -Group) %>%
      select(-type) %>% plyr::count(c("Group", "alleles")) %>%
      spread(alleles, freq)
    
    rownames(fisher_alleles) <- fisher_alleles$Group
    fisher_alleles$Group <- NULL
    
    # reorder alleles, ref, mut
    fisher_alleles_cols <- c(wildtype, mutant)
    fisher_alleles <- fisher_alleles[,fisher_alleles_cols]
    
    # select the groups to compare
    
    nam <- rownames(fisher_run2)
    nam2 <- rownames(fisher_run2)

    for (nm in 1:length(nam)){
      first_var <- nam[nm]
      
      for (cmp in 1:length(nam2)){
        if(identical(first_var, nam2[cmp])){
          drop_value <- nam2[cmp]
          #nam2 <- nam2[! nam2 %in% drop_value]
          next
        }
        
        # get names of groups being compared
        sam_comp <- paste(first_var, nam2[cmp], sep="_vs_")
        
        # allele analysis
        #nam2[cmp] <- "EC"
        cont_tab <- fisher_alleles[rownames(fisher_run2) %in% c(first_var, nam2[cmp]),]
        allele_mat <- data.matrix(cont_tab, rownames.force = NA)
        allele_mat[is.na(allele_mat)] <- 0
        
        allele_mat2 <- matrix(allele_mat, nrow = 2, dimnames = list(Group = rownames(allele_mat),
                                                                       Allele = colnames(allele_mat)))
        # change order of alleles to begin with mutant
        allele_mat2 <- allele_mat2[,c(mutant, wildtype)]
        
        
        allele_mat2[is.na(allele_mat2)] <- 0
        allele_fisher_test <- fisher.test(allele_mat2, alternative = "two.sided")
        
        allele_df <- data.frame(Position = pos, Comparison = sam_comp,
                              P_value = round(allele_fisher_test$p.value, 4), 
                              Odds_ratio = round(unname(allele_fisher_test$estimate), 4),
                              CI_min = round(allele_fisher_test$conf.int[1],4), 
                              CI_max = round(allele_fisher_test$conf.int[2],4))
        
        # add allelic_df to list
        pos_comp <- paste(pos, sam_comp, sep = "_")
        allelic_results_list[[pos_comp]] <- allele_df
        #cat(first_var, nam2[cmp], "\n")
        #nam2 <- nam2[! nam2 %in% drop_value]
        
        # select the groups to compare
        # genotype analysis
        contigency_table <- fisher_run2[rownames(fisher_run2) %in% c(first_var, nam2[cmp]),]
        mat <- data.matrix(contigency_table, rownames.force = NA)
        mat[is.na(mat)] <- 0
        
        # Compare individuals with heterozygous genotypes to individuals with homo wildtype
        if (ncol(fisher_run2) > 1) {
        mat1 <- mat[,1:2]
        mat1 <- cbind(mat1[,2, drop=F], mat1[,1, drop=F])
        mat2 <- cbind(mat1, hets_homo = rowSums(mat1))
       
        hets_homo_wildtype <- matrix(mat1, nrow = 2, dimnames = list(Group = rownames(mat1),
                                                                     Genotype = colnames(mat1)))
        hets_homo_wildtype[is.na(hets_homo_wildtype)] <- 0
        hets_homo_W <- fisher.test(hets_homo_wildtype, alternative = "two.sided")
        
        hets_df <- data.frame(Position = pos, Comparison = sam_comp, Data_name = hets_homo_W$data.name,
                              P_value = round(hets_homo_W$p.value,4), 
                              Odds_ratio = round(unname(hets_homo_W$estimate),4),
                              CI_min = round(hets_homo_W$conf.int[1],4),
                              CI_max = round(hets_homo_W$conf.int[2],4))
        # add hets_df to the list
        hets_homo_results_list[[pos_comp]] <- hets_df
        }
        # Compare individuals with "homozygous mutant" to individuals with "heterozygous and homozygous wildtype"
        if (length(mat) == 6){
          mat3 <- cbind(mat[,3,drop=FALSE],mat2[,3,drop=FALSE])
          homo_mutant <- matrix(mat3, nrow = 2, dimnames = list(Group = rownames(mat3),
                                                                Genotype = colnames(mat3)))
          homo_mutant[is.na(homo_mutant)] <- 0
          homo_m <- fisher.test(homo_mutant, alternative = "two.sided")
          
          homo_df <- data.frame(Position = pos, Comparison = sam_comp, Data_name = homo_m$data.name,
                                P_value = round(homo_m$p.value,4),
                                Odds_ratio = round(unname(homo_m$estimate),4),
                                CI_min = round(homo_m$conf.int[1],4),
                                CI_max = round(homo_m$conf.int[2],4))
          
          # add homo_df to the list
          homo_mutatnt_results_list[[pos_comp]] <- homo_df
          
        }
    }
  
  nam2 <- nam2[! nam2 %in% drop_value]

  }
  }

  #cmb_genotype_freq
  genotypic_cmb <- do.call(rbind, cmb_genotype_freq)
  genotypic_cmb$Position <- str_replace(genotypic_cmb$Position, "x","")
  genotypic_cmb$Position <- str_replace(genotypic_cmb$Position, "\\.",":")
  rownames(genotypic_cmb) <- NULL
  
  genotypic_cmb <- dplyr::arrange(genotypic_cmb, desc(GenotypeGroup))
  
  #cmb_allele_freq
  #do.call(rbind, cmb_allele_freq)
  allelic_cmb <- dplyr::bind_rows(cmb_allele_freq)
  allelic_cmb$Position <- str_replace(allelic_cmb$Position, "x","")
  allelic_cmb$Position <- str_replace(allelic_cmb$Position, "\\.",":")
  allelic_cmb <- dplyr::arrange(allelic_cmb, desc(AlleleType))
  
  #rownames(allelic_cmb) <- NULL
  #genotypic_cmb
  #allelic_results_list
  allelic_fisher <- dplyr::bind_rows(allelic_results_list)
  allelic_fisher$Position <- str_replace(allelic_fisher$Position, "x","")
  allelic_fisher$Position <- str_replace(allelic_fisher$Position, "\\.",":")
  allelic_fisher <- dplyr::arrange(allelic_fisher, Comparison)
  #genotypic_results_lists
  # hets_homo_wildtype
  genotypic_fisher <- dplyr::bind_rows(hets_homo_results_list)
  genotypic_fisher$Position <- str_replace(genotypic_fisher$Position, "x","")
  genotypic_fisher$Position <- str_replace(genotypic_fisher$Position, "\\.",":")
  genotypic_fisher <- dplyr::arrange(genotypic_fisher, Comparison)
  
  #homo_mutatnt_results_list
  genotypic_homo_fisher <- dplyr::bind_rows(homo_mutatnt_results_list)
  genotypic_homo_fisher$Position <- str_replace(genotypic_homo_fisher$Position, "x","")
  genotypic_homo_fisher$Position <- str_replace(genotypic_homo_fisher$Position, "\\.",":")
  # arrange rows by Comparison column
  genotypic_homo_fisher <- dplyr::arrange(genotypic_homo_fisher, Comparison)
  
  # write data to xlsx file
  df_list <- list(Allelic_freq=allelic_cmb, Genotypic_freq=genotypic_cmb,
                  Allelic_fisher_test=allelic_fisher, Genotypic_fisher_test_A=genotypic_fisher,
                  Genotypic_fisher_test_B=genotypic_homo_fisher)
  
  openxlsx::addWorksheet(wb, sheetName)
  
  current_row <- 1
  for (i in seq_along(df_list)){
    openxlsx::writeData(wb, sheetName, names(df_list)[i], startCol = 1, startRow = current_row)
    openxlsx::writeData(wb, sheetName, df_list[[i]], startCol = 1, startRow = current_row+1)
    current_row <- current_row + nrow(df_list[[i]]) + 3
  }

}
saveWorkbook(wb, paste("Gene_frequencies", now, "xlsx", sep = "."), overwrite = T)


##################################################################################################

