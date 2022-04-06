#' mydescriptive
#'
#' It gives the general summary statistics:  no. of observations, average, standard deviation, median and interquartile range with 1st quartile and 3rd quartile.
#'
#' @param x input numeric variable
#'
#' @return character vector
#' @export
#'
#'
#' @examples x=c(1:10)
#'   mydescriptive(x)
mydescriptive=function(x){c(n=length(na.omit(x)),Total=round(sum(x,na.rm=T),2),mean=round(mean(x,na.rm=T),2),sd=round(sd(x,na.rm=T),2),median=median(x,na.rm=T),iqr=paste(round(IQR(x,na.rm=T),2),"(",round(summary(x)[2],2),",",round(summary(x)[5],2),")",sep=""))}

#' mycat
#'
#' It gives the designed table heading.
#'
#' @param text Table's heading
#'
#' @return
#' @export
#'
#' @examples mycat("This is my table")
mycat=function(text){cat("\n",rep("=",60),"\n",text,"\n",rep("=",60),sep="")}

#' mycenter
#'
#' it helps to fix the table's heading into the center.
#'
#' @param text Table's heading
#' @param lsp  no. of spaces/blanks spaces in the left side of the text.
#' @param usp  no. of spaces/blanks spaces in the right side of the text.
#'
#' @return character
#' @export
#'
#' @examples mycat(mycenter("This is my Table"),20,0)
mycenter=function(text,lsp,usp){paste(paste(rep(" ",lsp),collapse=""),text,paste(rep(" ",usp),collapse = ""),sep=" ")}


#' mytab_indp_K_Populations
#'
#' It gives a output table for testing the significant difference between the  means of k number of independent populations.
#'
#' @param x dependent numeric variable
#' @param group independent categorical variable
#' @param xname name of the dependent variable
#'
#' @return Returns a data frame containing the descriptive stat along with the 1 way ANOVA F test stat., 1 way ANOVA P Value & Non parametric(Kruskal Wallis test) P Value.
#' @export
#'
#' @examples
mytab_indp_K_Populations = function(x,group,xname=deparse(substitute(x))){
  tab=do.call(rbind,tapply(x,group,mydescriptive))
  tab=data.frame(var=c(xname,rep("",nlevels(group)-1)),
                 group=levels(group),
                 tab,
                 Test_Stat=paste("F-test:",summary(aov(x~group))[[1]][1,4],sep=" "), ##-- 1 way ANOVA F value
                 parametric_Pval=summary(aov(x~group))[[1]][1,5],                    ##-- 1 way ANOVA P value
                 Nonparametric_Pval=kruskal.test(x~group)$p.value)                   ##-- Kruskal Wallis test
  rownames(tab)=NULL
  tab
}



#' freq_table
#'
#' It gives a frequency table with the percentage.
#'
#' @param x  input numeric variable
#' @param xname name of the variable
#'
#' @return a data frame of the frequency table
#' @export
#'
#' @examples
freq_table=function(x,xname=deparse(substitute(x))){
  tabl=data.frame(var=c(xname,rep("",nlevels(x)-1)),table(x),Per=round(100*prop.table(table(x)),2))[,-4]
  names(tabl)=c("Var","Group","Freq","Per(%)")
  return(tabl)
}


#' mytab_indep_two_Populations
#'
#' It gives a output table for testing the significant difference between the  means of two independent populations.
#'
#' @param x input dependent numeric variable.
#' @param group independent character vector for the names of the two populations.
#' @param xname name of the input dependent numeric variable.
#'
#' @return Returns a data frame containing the descriptive stat along with the indep. t-test stat., indep. t-test P Value & Non parametric(Mann Whitney U test) P Value.
#' @export
#'
#' @examples
mytab_indep_two_Populations = function(x,group,xname=deparse(substitute(x))){
  tab=do.call(rbind,tapply(x,group,mydescriptive))
  tab=data.frame(var=c(xname,rep("",nlevels(group)-1)),
                 group=levels(group),
                 tab,
                 Test_Stat=c(paste("t-stat:",round(t.test(x~group,mu=0,var.eq=F,paired=F)$statistic,3),sep=" "),rep("",nlevels(group)-1)), #----- t- test test statistic
                 Conf_Int= c(paste("(",round(t.test(x~group,mu=0,var.eq=F,paired=F)$conf.int[1],3),",",round(t.test(x~group,mu=0,var.eq=F,paired=F)$conf.int[2],3),")",sep = " "),rep("",nlevels(group)-1)),  #----- Confidence Interval
                 parametric_Pval=c(sprintf("%0.3f",t.test(x~group,mu=0,var.eq=F,paired=F)$p.value),rep("",nlevels(group)-1)),              #----- t- test P-value
                 Nonparametric_Pval=c(sprintf("%0.3f",wilcox.test(x~group)$p.value),rep("",nlevels(group)-1)))                             #----- Mann Whitney U test P-Value
  rownames(tab)=NULL
  tab
}



#' mytab_paired_one_Populations
#'
#' It gives a output table for testing the significant difference between the pre and post means of a independent populations.
#'
#' @param x.before numeric vector for the pre score.
#' @param x.after numeric vector for the post score.
#' @param var name of the variable/ population.
#'
#' @return Returns a data frame containing the descriptive stat along with the Paired.t- test test statistic, Confidence Interval, Paired.t- test P-value & Non parametric(Wilcoxon Signed Rank test) P Value.
#' @export
#'
#' @examples
mytab_paired_one_Populations = function(x.before,x.after,var){
  tab=rbind(mydescriptive(x.before),mydescriptive(x.after))
  tab=data.frame(Var=c(var,""),
                 Period=c("Before","After"),
                 tab,
                 Test_Stat=c(round(t.test(x.before,x.after,paired=T)$statistic,3),""), #----- Paired.t- test test statistic
                 Conf_Int= c(paste("(",round(t.test(x.before,x.after,paired=T)$conf.int[1],3),",",round(t.test(x.before,x.after,paired=T)$conf.int[2],3),")",sep = ""),""),  #----- Confidence Interval
                 Parametric.P_val=c(sprintf("%0.3f",t.test(x.before,x.after,paired=T)$p.value),""),              #----- Paired.t- test P-value
                 Nonparametric.P_val=c(sprintf("%0.3f",wilcox.test(x.before,x.after,paired = T)$p.value),""))                             #----- Wilcoxon Signed Rank test
  rownames(tab)=NULL
  tab
}




#' mytab_two_two_Contingency
#'
#' It gives an output table for testing the significant difference between the probabilities of two independent populations,i.e., for 2x2 contingency table.
#'
#' @param x indep. character vector.
#' @param y dep. character vector.
#' @param xlev levels of x.
#'
#' @return Returns a data frame containing the probs., percentages along with the Chi-square Test P-Value & Fisher's Exact Test P-Value for Non-Parametric.
#' @export
#'
#' @examples
mytab_two_two_Contingency=function(x,y,xlev=deparse(substitute(x))){
  tab=table(x,y)                         ##--- simple Contingency table
  tab_p=round(prop.table(tab,1)*100,2)   ##--- Contingency table with Percentage
  tab_out=data.frame(var=c(xlev,rep("",nlevels(x)-1)),level=levels(x),matrix(paste(tab,"(",tab_p,")",sep = ""),nrow(tab),ncol(tab)))
  names(tab_out)[-c(1,2)]=colnames(tab)
  tab_out=data.frame(tab_out,Pval.chisq=c(sprintf("%0.3f",chisq.test(x,y)$p.value),rep("",nlevels(x)-1)), ##---- Chi-square Test P-Value
                     PValue.Fisher=c(sprintf("%0.3f",fisher.test(x,y)$p.value),rep("",nlevels(x)-1)))     ##---- Fisher's Exact Test P-Value for Non-Parametric
  return(tab_out)
}




#' calcRelativeRisk
#'
#' Calculates the Relative Risk and CI.
#'
#' @param table Contingency table.
#' @param alpha critical value
#' @param referencerow fix a indep. group as a base/reference.
#' @param roundoff round-off digit.
#'
#' @return character
#' @export
#'
#' @examples
calcRelativeRisk = function(table,alpha=0.05,referencerow=2,roundoff=3)
{
  #----here, "table" is the contingency table----#

  numrow = nrow(table)
  myrownames = rownames(table)
  for (i in 1:numrow)
  {
    rowname = myrownames[i]
    DiseaseUnexposed = table[referencerow,1]
    ControlUnexposed = table[referencerow,2]
    if (i != referencerow)
    {
      DiseaseExposed = table[i,1]
      ControlExposed = table[i,2]
      totExposed = DiseaseExposed + ControlExposed
      totUnexposed = DiseaseUnexposed + ControlUnexposed
      probDiseaseGivenExposed = DiseaseExposed/totExposed
      probDiseaseGivenUnexposed = DiseaseUnexposed/totUnexposed

      #--- calculate the relative risk ---#
      relativeRisk = probDiseaseGivenExposed/probDiseaseGivenUnexposed
      print(paste("category =", rowname, ", relative risk = ",round(relativeRisk,roundoff)))

      #--- calculate a confidence interval ---#
      confidenceLevel = (1 - alpha)*100
      sigma = sqrt((1/DiseaseExposed) - (1/totExposed) +
                     (1/DiseaseUnexposed) - (1/totUnexposed))
      #--- sigma is the standard error of estimate of log of relative risk ---#
      z = qnorm(1-(alpha/2))
      lowervalue = relativeRisk * exp(-z * sigma)
      uppervalue = relativeRisk * exp( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",round(lowervalue,roundoff),",",round(uppervalue,roundoff),"]"))
    }
  }
}



#' calcOddsRatio
#'
#' Calculates odds Ratio and CI.
#'
#' @param table Contingency table.
#' @param alpha critical value
#' @param referencerow fix a indep. group as a base/reference.
#' @param quiet If there are just two treatments (exposed/nonexposed). Default value is FALSE.
#'
#' @return
#' @export
#'
#' @examples
calcOddsRatio = function(table,alpha=0.05,referencerow=2,quiet=FALSE)
{
  #--- here, "table" is the contingency table ---#

  numrow = nrow(table)
  myrownames = rownames(table)

  for (i in 1:numrow)
  {
    rowname = myrownames[i]
    DiseaseUnexposed = table[referencerow,1]
    ControlUnexposed = table[referencerow,2]
    if (i != referencerow)
    {
      DiseaseExposed = table[i,1]
      ControlExposed = table[i,2]

      totExposed = DiseaseExposed + ControlExposed
      totUnexposed = DiseaseUnexposed + ControlUnexposed

      probDiseaseGivenExposed = DiseaseExposed/totExposed
      probDiseaseGivenUnexposed = DiseaseUnexposed/totUnexposed
      probControlGivenExposed = ControlExposed/totExposed
      probControlGivenUnexposed = ControlUnexposed/totUnexposed

      #--- calculate the odds ratio ---#
      oddsRatio = (probDiseaseGivenExposed*probControlGivenUnexposed)/
        (probControlGivenExposed*probDiseaseGivenUnexposed)
      if (quiet == FALSE)
      {
        print(paste("category =", rowname, ", odds ratio = ",oddsRatio))
      }

      #--- calculate a confidence interval ---#
      confidenceLevel = (1 - alpha)*100
      sigma = sqrt((1/DiseaseExposed)+(1/ControlExposed)+
                     (1/DiseaseUnexposed)+(1/ControlUnexposed))
      #--- sigma is the standard error of our estimate of the log of the odds ratio ---#
      z <- qnorm(1-(alpha/2))
      lowervalue = oddsRatio * exp(-z * sigma)
      uppervalue = oddsRatio * exp( z * sigma)
      if (quiet == FALSE)
      {
        print(paste("category =", rowname, ", ", confidenceLevel,
                    "% confidence interval = [",lowervalue,",",uppervalue,"]"))
      }
    }
  }
  if (quiet == TRUE && numrow == 2) #--- If there are just two treatments (exposed/nonexposed) ---#
  {
    return(oddsRatio)
  }
}


#' count
#'
#' count how many values in a vector by removing all NAs.
#'
#' @param x numeric vector.
#'
#' @return number.
#' @export
#'
#' @examples
count=function(x){
  length(na.omit(x))
}
