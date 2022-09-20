# ************************************************
#  MANM354 MACHINE LEARNING & VISULISATION
#Supplementary Sheet with functions
#
# Vartan Zahorodnykov
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
# (Original author Prof N. Ryman-Tubb)
# 10 June 2019
#
# v1.00


# 1
NreadDataset<-function(csvFilename){
  dataset<-read.csv(csvFilename,encoding="UCI-G.csv",stringsAsFactors = FALSE)
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
} #Ryman-Tubb 2019

# 2
NPREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    if (is.numeric(dataset[,field]))
      field_types[field]<-"NUMERIC"
    else
      field_types[field]<-"SYMBOLIC"
  }
  return(field_types)
}  #create a preprocessing function for determining field categories

# 3
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){
  
  rescale0to1<-function(x){
    minv<-min(x)
    maxv<-max(x)
    return((x-minv)/(maxv-minv))
  }
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields marked NUMERIC
    if (field_types[field]=="NUMERIC") {
      
      #Scale the whole field (column) to between 0 and 1
      
      
      scaled_column<-rescale0to1(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of their numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      bins<-(bins/length(scaled_column))*100.0 #percentage
      
      #Here the 10 bins will have the counts in them
      #Bar charts help visulisation
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-"DISCREET"
      else
        field_types[field]<-"ORDINAL"
      
      barplot(bins, main=field_types[field],xlab=names(dataset[field]),names.arg = 1:10)
    }
  }
  return(field_types)
}


# 4
NPREPROCESSING_categorical<-function(dataset,field_types){
  
  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    
    #Only for fields marked NUMERIC
    if ((field_types[field]=="SYMBOLIC")||(field_types[field]=="DISCREET")) {
      
      #Create a list of unique values in the field (each is a literal)
      literals<-as.vector(unique(dataset[,field]))
      numberLiterals<-length(literals)
      
      #if there are just two literals in the field we can convert to 0 and 1
      if (numberLiterals==2){
        transformed<-ifelse (dataset[,field]==literals[1],0.0,1.0)
        catagorical<-cbind(catagorical,transformed)
        colnames(catagorical)[ncol(catagorical)]<-colnames(dataset)[field]
        
      } else
      {
        #We have now to one-hot encoding FOR SMALL NUMBER of literals
        if (numberLiterals<20){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)
            catagorical<-cbind(catagorical,hotEncoding)
            nameOfLiteral<-gsub(" ","_",nameOfLiteral) #301119 - replace spaces with _
            nameOfLiteral<-gsub("-","_",nameOfLiteral) #301119 - replace - with _
            nameOfLiteral<-gsub("\\(","",nameOfLiteral) #301119 - replace - with _
            nameOfLiteral<-gsub("\\)","",nameOfLiteral) #301119 - replace - with _
            colnames(catagorical)[ncol(catagorical)]<-paste(colnames(dataset)[field],nameOfLiteral,sep="_")
          }
        } else {
          stop(paste("Error - too many literals",names(dataset)[field]))
        }
        
      }
    }
  }
  
  return(catagorical[,-1]) #Remove that first column that was full of NA due to R
  
}

# 5 Correlagram
NPLOT_correlagram<-function(cr){
  
  library(corrplot)
  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))
  
  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)
  
  corrplot(cr,method="square",order="FPC",cl.ratio=0.2, cl.align="r",tl.cex = 0.6,cl.cex = 0.6,cl.lim = c(0, 1),mar=c(1,1,1,1))
}


# 6 Correlation
NPREPROCESSING_redundantFields<-function(dataset,cutoff){
  
  print(paste("Before redundancy check Fields=",ncol(dataset)))
  
  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1
  
  if (length(xx)>0L)
    dataset<-dataset[,-xx]
  
  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  NPLOT_correlagram(cr)
  
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]
  
  if (length(list_fields_correlated)>0){
    
    print("Following fields are correlated")
    print(list_fields_correlated)
    
    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])
    
    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}

#7 To check the status of the field included
myModelFormula<-function(training_data,fieldNameOutput){
  inputs<-paste(names(training_data)[which(names(training_data)!=fieldNameOutput)],collapse = "+")
  output<-paste(fieldNameOutput,"~")
  formular=as.formula(paste(output,inputs))
  return(formular)
} #endof myModelFormula()

#8
myEvaluateClassifier<-function(LDAmodel,testing_data,threshold) {
  evaluated<-predict(LDAmodel,testing_data)
  predictedClass<-ifelse(evaluated$posterior[,1]>threshold,0,1)
  confusion<-table(factor(predictedClass,levels=0:1),testing_data$Churn)
  results<-NcalcConfusion(predictedClass,testing_data$Churn)
  results<-append(results,list(probabilityClass=evaluated$posterior[,1]))
  return(results)
}


#9

NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}
NcalcMCC<-function(TP,FP,TN,FN){return( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))}

# ***************************************************
# NcalcConfusion() : Calculate a confusion matrix for 2-class classifier
# INPUT: vector - output from table()
#
# OUTPUT: A list with the following entries:
#        TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#        accuracy - float - accuracy measure
#        pgood - float - precision for "good" (values are 1) measure
#        pbad - float - precision for "bad" (values are 1) measure
#        FPR - float - FPR measure
#        TPR - float - FPR measure
# ***************************************************

# 10
NcalcMeasures<-function(TP,FN,FP,TN){
  
  NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
  NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
  NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
  NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
  NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}
  NcalcMCC<-function(TP,FP,TN,FN){return( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))}
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=NcalcAccuracy(TP,FP,TN,FN),
                  "pgood"=NcalcPgood(TP,FP,TN,FN),
                  "pbad"=NcalcPbad(TP,FP,TN,FN),
                  "FPR"=NcalcFPR(TP,FP,TN,FN),
                  "TPR"=NcalcTPR(TP,FP,TN,FN),
                  "MCC"=NcalcMCC(TP,FP,TN,FN)
  )
  return(retList)
}

#11
NcalcConfusion<-function(expected,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expected,levels=0:1))
  
  TP<-confusion[1,1]
  FN<-confusion[2,1]
  FP<-confusion[1,2]
  TN<-confusion[2,2]
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
  
}

