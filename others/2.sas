LIBNAME LIBRARY "E:\6300\project";
OPTIONS PAGENO=1;                                                            
TITLE1 'STAT 6300 PROJECT JM&SZ';                                                         
TITLE2 'UNBALANCE MULTIVARIATE TWO-WAY MANOVA'; 
DATA ONE ; SET LIBRARY.INTEREST;
DATA A; SET ONE;
KEEP age socdom sociabty stress worry impulsve thrillsk;run;
data SA; set A;
if age<30;
proc print data=SA;run;
data NA; set SA;
keep socdom sociabty stress worry impulsve thrillsk;
PROC IML;
 USE NA;
 READ ALL INTO Y;
 N=NROW(Y);
 YBAR=1/N*Y`*J(N,1);  				
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y;   	
 SINV=INV(S);   					
 YB=YBAR*J(1,N,1);  				
 YD=Y-YB`;   						
 DSQ=YD*SINV*YD`;  					
  D=DIAG(DSQ);  						
 D2=D*J(N,1,1); 					
 CREATE NFLD FROM D2[COLNAME='D2'];  
 APPEND from D2;
DATA NFLD;SET NFLD;
PROC SORT; BY D2;
PROC PRINT DATA=NFLD; RUN;
DATA NFLD; SET NFLD;
 N=37; P=6;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B);
RUN;
PROC GPLOT DATA=NFLD;
 PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=NFLD;
 VAR U V;
RUN;
* N2;
data SB; set A;
if 30<age<40 or age=30;
proc print data=SB;run;
data NB; set SB;
keep socdom sociabty stress worry impulsve thrillsk;
PROC IML;
 USE NB;
 READ ALL INTO Y;
 N=NROW(Y);
 YBAR=1/N*Y`*J(N,1);  				
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y;   	
 SINV=INV(S);   					
 YB=YBAR*J(1,N,1);  				
 YD=Y-YB`;   						
 DSQ=YD*SINV*YD`;  					
  D=DIAG(DSQ);  						
 D2=D*J(N,1,1); 					
 CREATE NFLD FROM D2[COLNAME='D2'];  
 APPEND from D2;
DATA NFLD;SET NFLD;
PROC SORT; BY D2;
PROC PRINT DATA=NFLD; RUN;
DATA NFLD; SET NFLD;
 N=87; P=6;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B);
RUN;
PROC GPLOT DATA=NFLD;
 PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=NFLD;
 VAR U V;
RUN;
* N3;
data SC; set A;
if 40<age<50 or age=40;
proc print data=SC;run;
data NC; set SC;
keep socdom sociabty stress worry impulsve thrillsk;
PROC IML;
 USE NC;
 READ ALL INTO Y;
 N=NROW(Y);
 YBAR=1/N*Y`*J(N,1);  				
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y;   	
 SINV=INV(S);   					
 YB=YBAR*J(1,N,1);  				
 YD=Y-YB`;   						
 DSQ=YD*SINV*YD`;  					
  D=DIAG(DSQ);  						
 D2=D*J(N,1,1); 					
 CREATE NFLD FROM D2[COLNAME='D2'];  
 APPEND from D2;
DATA NFLD;SET NFLD;
PROC SORT; BY D2;
PROC PRINT DATA=NFLD; RUN;
DATA NFLD; SET NFLD;
 N=90; P=6;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B);
RUN;
PROC GPLOT DATA=NFLD;
 PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=NFLD;
 VAR U V;
RUN;
* N4;
data SD; set A;
if age>50 or age=50;
proc print data=SD;run;
data ND; set SD;
keep socdom sociabty stress worry impulsve thrillsk;
PROC IML;
 USE ND;
 READ ALL INTO Y;
 N=NROW(Y);
 YBAR=1/N*Y`*J(N,1);  				
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y;   	
 SINV=INV(S);   					
 YB=YBAR*J(1,N,1);  				
 YD=Y-YB`;   						
 DSQ=YD*SINV*YD`;  					
  D=DIAG(DSQ);  						
 D2=D*J(N,1,1); 					
 CREATE NFLD FROM D2[COLNAME='D2'];  
 APPEND from D2;
DATA NFLD;SET NFLD;
PROC SORT; BY D2;
PROC PRINT DATA=NFLD; RUN;
DATA NFLD; SET NFLD;
 N=36; P=6;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B);
RUN;
PROC GPLOT DATA=NFLD;
 PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=NFLD;
 VAR U V;
RUN;
