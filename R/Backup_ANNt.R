#'Backup_ANNt
#'Generate backup of the role
#'@param () No require parameters
#'@param Investment 'No' is default; 'Yes' if command Investment_Horizon was executed
#'@examples
#'Backup_ANNt()
#'@export
Backup_ANNt <- function(Investment='No'){
  library(stringr)
  library(writexl)

  Backup = 'Backup'
  Readme_ANNt = as.data.frame(matrix(nrow=12,ncol=1500))
  nomes=c('Inputs','Values')
  colnames(Readme_ANNt[1:length(nomes)])=nomes
  Inputs = c('Tickers',
             'RM',
             'Initial_Date',
             'Final_Date',
             'Initial_Date_Training',
             'Final_Date_Training',
             'Initial_Date_Testing',
             'Final_Date_Testing',
             'Hidden',
             'Stepmax',
             'Rf',
             'Until_Date',
             'N_Assets',
             'Total_N_Assets',
             'Total_length_series',
             'Total_training_length',
             'Total_testing_length',
             'Relation_Row_Col_Testing',
             'Relation_Row_Col_Training',
             'Relation_length_Training_Testing')

  Readme_ANNt[1:length(Inputs),1]=Inputs
  load('~/Tickers.rda')
  load('~/tickers.rda')
  load('~/RM.rda')
  load('~/Initial_Date.rda')
  load('~/Final_Date.rda')
  #load('~/x0.rda')
  load('~/Initial_Date_Training.rda')
  load('~/Final_Date_Training.rda')
  load('~/Initial_Date_Testing.rda')
  load('~/Final_Date_Testing.rda')
  load('~/Hidden.rda')
  load('~/Stepmax.rda')
  load('~/Rf.rda')
  load('~/Until_Date.rda')
  load('~/N_Assets.rda')
  load("~/Signal_Sharpe.rda")
  load("~/scenario.set.rda")
  tickers=colnames(scenario.set)
  if(Signal_Sharpe==1){
    RM = "SHARPE"
  }
  if(Final_Date==''){
    load('~/scenario.set.rda')
    Final_Date = rownames(as.data.frame(scenario.set)[nrow(scenario.set),])
  }

  Total_N_Assets=ncol(scenario.set)-1
  Total_length_series=nrow(scenario.set)
  Total_training_length=which(rownames(as.data.frame(scenario.set))==Final_Date_Training)
  Total_testing_length=(which(rownames(as.data.frame(scenario.set))==Final_Date_Testing)
                         -Total_training_length)
  Relation_Row_Col_Testing= round(Total_testing_length/Total_N_Assets,1)
  Relation_Row_Col_Training=round(Total_training_length/Total_N_Assets,1)
  Relation_length_Training_Testing=round(Total_training_length/Total_testing_length,1)

### Matrix generation
  Values=c(tickers)
  for(k in (2:length((Values)))){
    Readme_ANNt[1,k]=Values[k]
  }
  Values_inputs=c(RM,
                  Initial_Date,
                  Final_Date,
                  Initial_Date_Training,
                  Final_Date_Training,
                  Initial_Date_Testing,
                  Final_Date_Testing,
                  Hidden,
                  Stepmax,
                  Rf,
                  Until_Date,
                  N_Assets,
                  Total_N_Assets,
                  Total_length_series,
                  Total_training_length,
                  Total_testing_length,
                  Relation_Row_Col_Testing,
                  Relation_Row_Col_Training,
                  Relation_length_Training_Testing)

    for(i in (1:length((Values_inputs)))){
    Readme_ANNt[i+1,2]=Values_inputs[i]
    }


View(Readme_ANNt)

  Data = Sys.time()

if(Investment=='No'){
  nome_dir= str_replace(Data,"-","_")
  nome_dir= str_replace(nome_dir,"-","_")
  nome_dir= str_replace(nome_dir,":","h")
  nome_dir= str_replace(nome_dir,":","m")
  nome_dir= str_replace(nome_dir,"-","_")
} else{
  if(Investment=='Yes'){
    load('~/RM_Nome_Backup.rda')
    nome_dir= str_replace(Data,"-","_")
    nome_dir= str_replace(nome_dir,"-","_")
    nome_dir= str_replace(nome_dir,":","h")
    nome_dir= str_replace(nome_dir,":","m")
    nome_dir= str_replace(nome_dir,"-","_")
    nome_dir=paste(RM_Nome_Backup,nome_dir, sep="_")
  }
}
  nome_readme=paste("Readme_ANNt_", nome_dir, sep="")
  save(Readme_ANNt, file='~/Readme_ANNt.rda')


  nome_dir_backup=paste("~/Backup_ANNt_",nome_dir, sep="")
  dir.create(nome_dir_backup)

  Readme_ANNt_wrt = paste(nome_dir_backup,"/",nome_readme,".xlsx", sep="")
  write_xlsx(Readme_ANNt, Readme_ANNt_wrt)

  Current_Work_Space=paste(nome_dir_backup,"/","Work_Space",nome_dir,".RData", sep="")
  save.image(Current_Work_Space)



  files = dir('~/')[1:length(dir('~/'))]
  caminho = '~/'
  files_from = str_c(caminho,files)
  files_to = str_c(nome_dir_backup,'/',files)
  file.copy(files_from, files_to)




}
