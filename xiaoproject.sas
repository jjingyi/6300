proc import datafile="C:\Users\xiaoxiao\Desktop\data.csv" out=A dbms=csv replace;
getnames=yes; 
run;
DATA A; SET A;
KEEP fixed volatile residual chlorides free total density pH  sulphates alcohol quality;
RUN;
/* data displaying check S and T */
DATA B; SET A;
 KEEP fixed volatile residual chlorides free total density pH  sulphates alcohol;
PROC IML;
 RESET AUTONAME;
  USE B;
  TITLE 'PROJECT DATA';
  READ ALL INTO Y;
  N = NROW(Y);
  YBAR = 1/N*Y`*J(N,1);
  S = 1/(N-1)*Y`*(I(N)-1/N*J(N))*Y;
  ZBAR=A*YBAR;
  SZ=A*S*A`;
  DETS=DET(S);
  TRAS=TRACE(S);
  D=DIAG(S);
  D=SQRT(D);
  DINV=INV(D);
  R=DINV*S*DINV;
  DETR=DET(R);
  TRAR=TRACE(R);
  EIGS=EIGVAL(S);
  EIGR=EIGVAL(R);
  PRINT YBAR, S, Z1BAR1, Z1BAR2, SZ1, A, ZBAR, SZ;
  PRINT DETS, TRAS, R, DETR, TRAR, EIGS, EIGR;
RUN;

/*check normality assumption*/
/*UNIVARIATE*/
PROC RANK DATA=B OUT=C NORMAL=BLOM;
 VAR fixed volatile residual chlorides free total density pH sulphates alcohol; RANKS ZSC1 ZSC2 ZSC3 ZSC4 ZSC5 ZSC6 ZSC7 ZSC8 ZSC9 ZSC10;
RUN;

PROC GPLOT DATA=C;
 PLOT fixed*ZSC1=1;
 PLOT volatile*ZSC2=1;;
 PLOT residual*ZSC3=1;
 PLOT chlorides*ZSC4=1;
 PLOT free*ZSC5=1;
 PLOT total*ZSC6=1;
 PLOT density*ZSC7=1;
 PLOT pH*ZSC8=1;
 PLOT sulphates*ZSC9=1;
 PLOT alcohol*ZSC10=1;
 SYMBOL1 C=RED V=STAR I=RL;
RUN;

PROC UNIVARIATE DATA=C NORMAL PLOT;
 VAR fixed volatile residual chlorides free total density pH sulphates alcohol;
RUN;
/*MULTIVARIATE*/
DATA Z; SET C;
 KEEP fixed volatile residual chlorides free total density pH sulphates alcohol;
PROC IML;
 USE Z;
 READ ALL INTO Y;
 N=NROW(Y);       					*Finds number of observations;
 YBAR=1/N*Y`*J(N,1);  				*Computes yabr vector - Px1;
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y;   	*Computes var-cov matrix of Y - PxP;
 SINV=INV(S);   					*Computes S inverse matrix - PxP;
 YB=YBAR*J(1,N,1);  				*Computes PxN matrix where all elements of Ith row are the value of 
                     				YBAR for the Ith variable;
 YD=Y-YB`;   						*Subtracts YBAR of variable from each observed value of the variable - NxP;
 DSQ=YD*SINV*YD`;  					*Diagonal elements are the standardized squared distance from Ith
                    				Y vector to YBAR vector - NxN;
 D=DIAG(DSQ);  						*Creates NxN diagonal matrix with Di-sq's on diagonal;
 D2=D*J(N,1,1); 					*Creates Nx1 vector of Di-sq's;
 CREATE SGD FROM D2[COLNAME='D2'];  *Creates data set SGD from the vector D2 to import back
                                     to regular SAS;  
 APPEND from D2;
print D2;
RUN;
DATA SGD; SET SGD;
PROC SORT; BY D2;
PROC PRINT DATA=SGD;
RUN;
DATA SGD; SET SGD;
 N=1599; P=11;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B);
RUN;
PROC PRINT DATA=SGD;
 VAR U V UU PR;
PROC GPLOT DATA=SGD;
 PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=SGD;
 VAR U V;
RUN;


/*one way manova analysis*/
PROC GLM DATA=A; CLASS quality;                                                            
  MODEL fixed volatile residual chlorides free total density pH  sulphates alcohol = quality;                                              
  MANOVA H=_ALL_/PRINTH SHORT;                                                  
  MEANS quality/TUKEY;
  LSMEANS quality/PDIFF ADJUST=TUKEY;
RUN;

PROC GLM DATA=A; CLASS quality;                                                            
  MODEL fixed volatile residual chlorides free total density pH  sulphates alcohol = quality;                                                   
  MANOVA H=_ALL_/MSTAT=EXACT;
  ODS SELECT MULTSTAT;
 RUN;

 /*Principal component analysis */
PROC CORR DATA=A;
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;with quality;
RUN;
PROC PRINCOMP COV OUT=D DATA=A;
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;
RUN;
PROC GPLOT DATA=D;
 PLOT Prin2*Prin1='*'/VREF=0 HREF=0;
 SYMBOL1 C=RED V=STAR;
 SYMBOL2 C=BLUE V=PLUS;
RUN;

 /*Principal component regression*/
PROC REG DATA=A;
 MODEL quality=fixed volatile residual chlorides free total density pH  sulphates alcohol/VIF;
RUN;

PROC REG DATA=A;
 MODEL quality=fixed volatile residual chlorides free total density pH  sulphates alcohol/SELECTION=RSQUARE ADJRSQ CP BEST=4 AIC MSE;
RUN;

PROC REG DATA=A;
MODEL quality=volatile alcohol/VIF;
MODEL quality=volatile sulphates alcohol/VIF;
MODEL quality=volatile total sulphates alcohol/VIF;
MODEL quality=volatile total pH sulphates alcohol/VIF;
MODEL quality=volatile chlorides total pH sulphates alcohol/VIF;
MODEL quality=volatile chlorides free total pH sulphates alcohol/VIF;
MODEL quality=volatile chlorides free total density pH sulphates alcohol/VIF;
MODEL quality=volatile residual chlorides free total density pH sulphates alcohol/VIF;
RUN;



PROC MEANS DATA=A NOPRINT;
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol quality;
 OUTPUT OUT=B STD=Sfixed Svolatile Sresidual Schlorides Sfree Stotal Sdensity SpH  Ssulphates Salcohol Squality;
RUN;
DATA B; SET B;
 DO I=1 TO _FREQ_;
 OUTPUT; END;
RUN;
DATA AB (DROP=I); MERGE A B;
 ARRAY AA{11} fixed volatile residual chlorides free total density pH  sulphates alcohol quality;
 ARRAY BB{11} Sfixed Svolatile Sresidual Schlorides Sfree Stotal Sdensity SpH  Ssulphates Salcohol Squality;
 ARRAY CC{11} Nfixed Nvolatile Nresidual Nchlorides Nfree Ntotal Ndensity NpH  Nsulphates Nalcohol Nquality;
 DO I=1 TO 11;
 CC{I} = AA{I}/BB{I}; END;
RUN;

TITLE2 'NEW ANALYSIS WITH STANDARDIZED VARIABLES';
PROC PRINCOMP DATA=AB COV OUT=E;
 VAR Nfixed Nvolatile Nresidual Nchlorides Nfree Ntotal Ndensity NpH  Nsulphates Nalcohol Nquality; 
RUN;
PROC GPLOT DATA=E;
 PLOT PRIN2*PRIN1=1/VREF=0 HREF=0;
RUN;

/*factorial analysis*/
DATA A; SET A;                                                                  
LABEL                                                                           
 fixed='fixed'                                                              
 volatile='volatile'                                                              
 residual='residual'                                                             
 chlorides='chlorides'                                                             
 free='free'                                                             
 total='total'                                                            
 density='density'                                                                 
 pH='pH'                                                                 
 sulphates='sulphates'                                                                
 alcohol='alcohol';                                                               
RUN; 
PROC PRINT DATA=A;  
RUN; 
PROC CORR DATA=A NOPROB NOSIMPLE;
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;
RUN; 
PROC FACTOR DATA=A R=V SCREE;                                                          
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;                                                                    
RUN; 


PROC CORR DATA=A NOMISS ALPHA NOSIMPLE NOCORR;                                         
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;                                                                     
RUN;
PROC FACTOR DATA=A R=HK ;                                                               
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;                                                                    
RUN;                                                                            
PROC FACTOR DATA=A R=V SCREE n=10;                                                          
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;                                                                    
RUN; 

PROC FACTOR DATA=A R=V SCREE n=6 OUT=BB;                                                          
 VAR fixed volatile residual chlorides free total density pH  sulphates alcohol;                                                                    
RUN; 


PROC CORR DATA=BB;
 VAR FACTOR1-FACTOR6;
RUN;
PROC MEANS DATA=BB; CLASS quality;
 VAR FACTOR1-FACTOR6;
RUN;
PROC GLM DATA=BB; CLASS quality;
 MODEL FACTOR1-FACTOR6=quality;
 MANOVA H=quality;
 MEANS quality/TUKEY;
RUN;


PROC REG DATA=BB;
 MODEL quality= fixed volatile residual chlorides free total density pH  sulphates alcohol/SELECTION=STEPWISE;
 MODEL quality= fixed volatile residual chlorides free total density pH  sulphates alcohol/SELECTION=BACKWARD;
 MODEL quality= fixed volatile residual chlorides free total density pH  sulphates alcohol/SELECTION=RSQUARE CP ADJRSQ AIC MSE BEST=3;
RUN;

PROC REG DATA=BB;
 MODEL quality=FACTOR1-FACTOR6/SELECTION=RSQUARE CP ADJRSQ AIC MSE BEST=3;
RUN;
