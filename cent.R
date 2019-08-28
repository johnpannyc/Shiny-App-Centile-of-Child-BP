library(reshape)
library(ggplot2)
library(grid)
library(dplyr)
library(tidyverse)

#5/13/2019 revision

#Get L,M,S parameter from CDC database by Gender difference.
d <- read.csv(file='datasets/CDC_parameters.csv')

m <- d %>% filter(Sex==1)
f <- d %>% filter(Sex==2)

LMS_boy <-  m 
LMS_girl <- f

stat_boy <- data.frame(months = LMS_boy$Agemos,L = LMS_boy$L,M = LMS_boy$M,S = LMS_boy$S );
stat_girl <- data.frame(months = LMS_girl$Agemos,L = LMS_girl$L,M = LMS_girl$M,S = LMS_girl$S );

rm( d )


seek_LMS_stat <- function(months, sex) {
	#get gender based data
	if(sex == "male")
	{
		d <- LMS_boy;
	}
	else
	{
		d <- LMS_girl;
	}
	if((months > head(tail(d$Agemos, n=2),n=1)) | (months < head(d$Agemos,n=1))) 
	{
		lst = list();
		lst["L"] <- -1;
		lst["M"] <- -1;
		lst["S"] <- -1;
		return(lst);
	} 
	else 
	{
		val <- max(which(d$Agemos <= months));
		val <- val + 1;
		lst = list();
		lst["L"] <- d$L[ val ];
		lst["M"] <- d$M[ val ];
		lst["S"] <- d$S[ val ];
		return(lst);
	}
}

timeInMonths <- function(selected_date, sDate) 
{
     end_date <- as.POSIXlt(selected_date,tryFormats = c("%Y-%m-%d"))
     start_date <- as.POSIXlt(sDate,tryFormats = c("%Y-%m-%d"))  
     12 * (end_date$year - start_date$year) + (end_date$mon - start_date$mon)
}

#formula:using LMS to generate Z score and percentiles are from CDC database:
generalZ <- function( L, M, S, X ) 
{
	val <- 0;
	if(L!=0)
	{
		val <- ((((X/M)^L)-1)/(L * S));
	}
	else
	{
		val <- (log(X/M)/S );
	}
	return (val);
}

generalX <- function( L, M, S, Z ) {
     #given a centile, compute the corresponding anthropomoprhic value X
     val <- 0;
     if(L!=0)
     {
		 val <- (M*(1+L*S*Z)^(1/L));
	 }
	 else
	 {
		 val <- (M*exp(S*Z));
	 }
     return (val);
     
}

statsZ <- function( months, sex, height) 
{
	lms_stat <- seek_LMS_stat(months, sex);
    L <- lms_stat$L;
    M <- lms_stat$M;
    S <- lms_stat$S;
    general_z <- generalZ(L,M,S,height);
    return(general_z)         
}


centileFromZ <- function( Z ) 
{
	p_norm <- pnorm( Z ) * 100;
	return(p_norm)
}


#Using list of LMS parameters to generate a ggplot compatible data frame for percentiles.
get_cent_df <- function(LMS, centile) 
{
     number_of_rows <- dim(LMS)[1];
     number_of_columns <- length(centile) + 1;
     mat_table <- matrix( rep( 0, len=number_of_rows*number_of_columns), nrow = number_of_rows);
     cents <- qnorm(centile/100 );
     cat(number_of_columns);
     for(i in seq(1,number_of_rows))
     {
		 #for each month in LMS
		 L <- LMS$L[i];
		 M <- LMS$M[i];
		 S <- LMS$S[i];
         mat_table[i,1] <- LMS$months[i];
         #for each required centile 
         for(j in seq(2,number_of_columns))
         {
			 mat_table[i,j] <- generalX(L,M,S,cents[j-1]);
		 }  
     }
     dataFrame <- data.frame(mat_table);
     colnames(dataFrame) <- c("abscissa", paste( "C", centile, sep = ""));
     return(dataFrame)     
}

     
#Generate plots for Systolic Bp, Diastolic Bp, and Height.     
plotCentile <- function( D, xVal, yVal, sex, pointTxt, xLabel, yLabel, plotTitle ) 
{
     centList <- names( D )[2:length(names(D))];
     nCents <- (length( names(D) ) - 2)/2;
     
     minAbs <- min( D$abscissa );
     maxAbs <- max( D$abscissa );

     minOrd <- min( D[,2:dim(D)[2]] );
     maxOrd <- max( D[,2:dim(D)[2]] );
     
     alphaLevels <- rep( 0.08, length.out = nCents );
     alphaLevels[ nCents ] <- 0.15
     
     colCents <- dim( D )[2] + 1;
     
     if( sex == "female" ) 
     {
          plotColour <- "pink"
     } 
     else 
     {
		 plotColour <- "light blue";
     }
     
     PP <- ggplot(D, aes(x=abscissa, y = C50 ))
     PP <- PP + theme(panel.background = element_rect(fill="white"))
     for(i in seq(1,nCents))
     {
		 PP <- PP + geom_ribbon( ymin=D[ , i+1 ], ymax=D[ , colCents - i ], colour = "black",
                                  fill=plotColour, alpha = alphaLevels[i] )  
     }
     PP <- PP + geom_line(size = 1.25)  #plot 50th / median centile line
     
     
     if( xVal >= minAbs & xVal <= maxAbs ) 
     {
          subLoc <- data.frame( xX = xVal, yY = yVal );
          PP <- PP + geom_hline(yintercept=yVal) + geom_vline(xintercept=xVal) 
          PP <- PP + geom_point( data = subLoc, aes( x = xX, y = yY), colour = "red", size = 4.0 )
     
          if( pointTxt != "" ) 
          {
               
               my.grob = grobTree(textGrob(pointTxt, x=0.70,  y=0.2, hjust=0,
                                           gp=gpar(col="black", fontsize=15 )))
     
               PP <- PP + annotation_custom(my.grob)
          }
     }

     #add legend
     legLocX <- round( minAbs + (maxAbs - minAbs)/2 );
     legX    <- D$abscissa[ max( which( D$abscissa <= legLocX ) ) ];
     legLocY <- D[ max( which( D$abscissa <= legLocX ) ), 2:dim(D)[2] ];
     
     for(i in seq(1,length(centList)))
     {
          PP <- PP + annotate("text", x = legX, y = legLocY[1,i], label = centList[i], colour = "black", size = 3 )
     }
     PP <- PP + xlab(xLabel) + ylab(yLabel) + ggtitle( plotTitle )
     return( PP )
}

