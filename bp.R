library(tidyverse)
library(dplyr)

#Get the JNC7 Parameters
bp.centile <- read.csv(file='datasets/JNC7_parameters.csv')

#5/13/2019 revision

#Get percentile parameter from JNC7
get_bp <- function() {
          return( bp.centile )
}


#Mean systolic blood pressure calculation.
get_bp_regression <- function( alpha, betas, gammas, age.month, Zht ){     
     #Convert Age:month to year
     y <- age.month/12;    
     term1 <- 0;
     term2 <- 0;
     #Mean systolic blood pressure calaculation
     for(i in seq(1,4)){
     term1 <- term1 + betas[i] * (y-10)^i;
		 term2 <- term2 + gammas[i] * (Zht)^i;
	 } 
     #Get Mean systolic blood pressure
     mBP <- alpha + term1 + term2;
     return( mBP );
}


#Fetch bp according to the parameters provided by CDC and JNC7
bloodpressure <- function( Zht, bp.centile, sex_of_child, age_of_child_months ) {
	#Get parameters for computing "Systolic BP" by age and height
	if( sex_of_child == "male" ){
		  subDataset <- bp.centile %>% filter(Gender == "Male")
	} else {
		  subDataset <- bp.centile %>% filter(Gender == "Female")
	}
      params.Sys <- subDataset %>% filter(BPvar == "Sys")
      params.Dia <- subDataset %>% filter(BPvar == "Dia")  
    
      
      betas  <- c(params.Sys$B1, params.Sys$B2, params.Sys$B3, params.Sys$B4);
      gammas <- c(params.Sys$G1, params.Sys$G2, params.Sys$G3, params.Sys$G4);
      alpha  <- params.Sys$alpha;
      stdDev <- params.Sys$SD;
      SBP    <- get_bp_regression( alpha, betas, gammas, age_of_child_months, Zht );
     
    
     #Get parameters for compute "Diastolic BP" by age and height
      betas  <- c(params.Dia$B1, params.Dia$B2, params.Dia$B3, params.Dia$B4);
      gammas <- c(params.Dia$G1, params.Dia$G2, params.Dia$G3, params.Dia$G4);
      alpha  <- params.Dia$alpha;
      stdDev <- params.Dia$SD;
      DBP    <- get_bp_regression( alpha, betas, gammas, age_of_child_months, Zht );
      return( list( e.SBP = SBP, e.DBP = DBP ))
     
}


#Get z score and centiles
ZBP <- function( Zht, bp.centile, sex_of_child, age_of_child_months, recorded_SBP, recorded_DBP ) {
     
     #get stand dev params for sex 
     #fetch params
     if( sex_of_child == "male" ){
	      subDataset <- bp.centile %>% filter(Gender == "Male")
      } else 	{
	    subDataset <- bp.centile %>% filter(Gender == "Female")
	}
      params.Sys <- subDataset %>% filter(BPvar == "Sys")
      params.Dia <- subDataset %>% filter(BPvar == "Dia") 
      
      SD.Sys <- params.Sys$SD;
      SD.Dia <- params.Dia$SD;
      e.BP <- bloodpressure( Zht, bp.centile, sex_of_child, age_of_child_months );
      e.Sys <- e.BP$e.SBP;
      e.Dia <- e.BP$e.DBP;
    
      Z.Sys <- (recorded_SBP - e.Sys)/SD.Sys;
      Z.Dia <- (recorded_DBP - e.Dia)/SD.Dia;
     
    #area under normal curve * 100 for centile.
      centile.Sys <- pnorm( Z.Sys )*100;
      centile.Dia <- pnorm( Z.Dia )*100;
      return( list( c.Sys = centile.Sys, c.Dia = centile.Dia  ) );
     
}


generateBP <- function( sex_of_child, Zht, bp.centile, centile ) {
    #Get SD by sex & Systolic or Diastolic Bp
    if( sex_of_child == "male" ) {
      subDataset <- bp.centile %>% filter(Gender == "Male")
      } else {
	    subDataset <- bp.centile %>% filter(Gender == "Female")
}
     SD.sys <- subDataset[ which(subDataset$BPvar == "Sys" ), ]$SD
     SD.dia <- subDataset[ which(subDataset$BPvar == "Dia" ), ]$SD    
     
     ageV <- seq(from = 12, to = (17 * 12), by = 1)
     sysMatrix <- matrix(0, length( ageV ), length( centile ) )
     diaMatrix <- matrix(0, length( ageV ), length( centile ) )
     
     colnames(sysMatrix) <- paste( "C", centile, sep = "")
     colnames(diaMatrix) <- paste( "C", centile, sep = "")
     for(i in seq(1,length( centile ))) {
          sysMatrix[,i] <- qnorm( centile[i] / 100 ) * SD.sys + bloodpressure( Zht, bp.centile, sex_of_child, ageV )$e.SBP;
          diaMatrix[,i] <- qnorm( centile[i] / 100 ) * SD.dia + bloodpressure( Zht, bp.centile, sex_of_child, ageV )$e.DBP;
}
     
     #For given sex, Zht, return two dataframes of age x, which is required centiles systolic and diastolic
     return( list( sysTab = data.frame( abscissa = ageV, sysMatrix),
                   diaTab = data.frame( abscissa = ageV, diaMatrix) ) )
}

