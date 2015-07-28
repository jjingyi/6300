**6300 poject;
OPTIONS PAGENO=1;                                                            
TITLE1 'STAT 6300 PROJECT ';                                                         

data iris;
input order Y1 Y2 Y3 Y4 class;
datalines;
1       5.1     3.5     1.4    0.2    A
2       4.9     3.0     1.4    0.2    A
3       4.7     3.2     1.3    0.2    A
4       4.6     3.1     1.5    0.2    A
5       5.0     3.6     1.4    0.2    A
6       5.4     3.9     1.7    0.4    A
7       4.6     3.4     1.4    0.3    A
8       5.0     3.4     1.5    0.2    A
9       4.4     2.9     1.4    0.2    A
10      4.9     3.1     1.5    0.1    A
11      5.4     3.7     1.5    0.2    A
12      4.8     3.4     1.6    0.2    A
13      4.8     3.0     1.4    0.1    A
14      4.3     3.0     1.1    0.1    A
15      5.8     4.0     1.2    0.2    A
16      5.7     4.4     1.5    0.4    A
17      5.4     3.9     1.3    0.4    A
18      5.1     3.5     1.4    0.3    A
19      5.7     3.8     1.7    0.3    A
20      5.1     3.8     1.5    0.3    A
21      5.4     3.4     1.7    0.2    A
22      5.1     3.7     1.5    0.4    A
23      4.6     3.6     1.0    0.2    A
24      5.1     3.3     1.7    0.5    A
25      4.8     3.4     1.9    0.2    A
26      5.0     3.0     1.6    0.2    A
27      5.0     3.4     1.6    0.4    A
28      5.2     3.5     1.5    0.2    A
29      5.2     3.4     1.4    0.2    A
30      4.7     3.2     1.6    0.2    A
31      4.8     3.1     1.6    0.2    A
32      5.4     3.4     1.5    0.4    A
33      5.2     4.1     1.5    0.1    A
34      5.5     4.2     1.4    0.2    A
35      4.9     3.1     1.5    0.2    A
36      5.0     3.2     1.2    0.2    A
37      5.5     3.5     1.3    0.2    A
38      4.9     3.6     1.4    0.1    A
39      4.4     3.0     1.3    0.2    A
40      5.1     3.4     1.5    0.2    A
41      5.0     3.5     1.3    0.3    A
42      4.5     2.3     1.3    0.3    A
43      4.4     3.2     1.3    0.2    A
44      5.0     3.5     1.6    0.6    A
45      5.1     3.8     1.9    0.4    A
46      4.8     3.0     1.4    0.3    A
47      5.1     3.8     1.6    0.2    A
48      4.6     3.2     1.4    0.2    A
49      5.3     3.7     1.5    0.2    A
50      5.0     3.3     1.4    0.2    A
51      7.0     3.2     4.7    1.4    B
52      6.4     3.2     4.5    1.5    B
53      6.9     3.1     4.9    1.5    B
54      5.5     2.3     4.0    1.3    B
55      6.5     2.8     4.6    1.5    B
56      5.7     2.8     4.5    1.3    B
57      6.3     3.3     4.7    1.6    B
58      4.9     2.4     3.3    1.0    B
59      6.6     2.9     4.6    1.3    B
60      5.2     2.7     3.9    1.4    B
61      5.0     2.0     3.5    1.0    B
62      5.9     3.0     4.2    1.5    B
63      6.0     2.2     4.0    1.0    B
64      6.1     2.9     4.7    1.4    B
65      5.6     2.9     3.6    1.3    B
66      6.7     3.1     4.4    1.4    B
67      5.6     3.0     4.5    1.5    B
68      5.8     2.7     4.1    1.0    B
69      6.2     2.2     4.5    1.5    B
70      5.6     2.5     3.9    1.1    B
71      5.9     3.2     4.8    1.8    B
72      6.1     2.8     4.0    1.3    B
73      6.3     2.5     4.9    1.5    B
74      6.1     2.8     4.7    1.2    B
75      6.4     2.9     4.3    1.3    B
76      6.6     3.0     4.4    1.4    B
77      6.8     2.8     4.8    1.4    B
78      6.7     3.0     5.0    1.7    B
79      6.0     2.9     4.5    1.5    B
80      5.7     2.6     3.5    1.0    B
81      5.5     2.4     3.8    1.1    B
82      5.5     2.4     3.7    1.0    B
83      5.8     2.7     3.9    1.2    B
84      6.0     2.7     5.1    1.6    B
85      5.4     3.0     4.5    1.5    B
86      6.0     3.4     4.5    1.6    B
87      6.7     3.1     4.7    1.5    B
88      6.3     2.3     4.4    1.3    B
89      5.6     3.0     4.1    1.3    B
90      5.5     2.5     4.0    1.3    B
91      5.5     2.6     4.4    1.2    B
92      6.1     3.0     4.6    1.4    B
93      5.8     2.6     4.0    1.2    B
94      5.0     2.3     3.3    1.0    B
95      5.6     2.7     4.2    1.3    B
96      5.7     3.0     4.2    1.2    B
97      5.7     2.9     4.2    1.3    B
98      6.2     2.9     4.3    1.3    B
99      5.1     2.5     3.0    1.1    B
100     5.7     2.8     4.1    1.3    B
101     6.3     3.3     6.0    2.5    C
102     5.8     2.7     5.1    1.9    C
103     7.1     3.0     5.9    2.1    C
104     6.3     2.9     5.6    1.8    C
105     6.5     3.0     5.8    2.2    C
106     7.6     3.0     6.6    2.1    C
107     4.9     2.5     4.5    1.7    C
108     7.3     2.9     6.3    1.8    C
109     6.7     2.5     5.8    1.8    C
110     7.2     3.6     6.1    2.5    C
111     6.5     3.2     5.1    2.0    C
112     6.4     2.7     5.3    1.9    C
113     6.8     3.0     5.5    2.1    C
114     5.7     2.5     5.0    2.0    C
115     5.8     2.8     5.1    2.4    C
116     6.4     3.2     5.3    2.3    C
117     6.5     3.0     5.5    1.8    C
118     7.7     3.8     6.7    2.2    C
119     7.7     2.6     6.9    2.3    C
120     6.0     2.2     5.0    1.5    C
121     6.9     3.2     5.7    2.3    C
122     5.6     2.8     4.9    2.0    C
123     7.7     2.8     6.7    2.0    C
124     6.3     2.7     4.9    1.8    C
125     6.7     3.3     5.7    2.1    C
126     7.2     3.2     6.0    1.8    C
127     6.2     2.8     4.8    1.8    C
128     6.1     3.0     4.9    1.8    C
129     6.4     2.8     5.6    2.1    C
130     7.2     3.0     5.8    1.6    C
131     7.4     2.8     6.1    1.9    C
132     7.9     3.8     6.4    2.0    C
133     6.4     2.8     5.6    2.2    C
134     6.3     2.8     5.1    1.5    C
135     6.1     2.6     5.6    1.4    C
136     7.7     3.0     6.1    2.3    C
137     6.3     3.4     5.6    2.4    C
138     6.4     3.1     5.5    1.8    C
139     6.0     3.0     4.8    1.8    C
140     6.9     3.1     5.4    2.1    C
141     6.7     3.1     5.6    2.4    C
142     6.9     3.1     5.1    2.3    C
143     5.8     2.7     5.1    1.9    C
144     6.8     3.2     5.9    2.3    C
145     6.7     3.3     5.7    2.5    C
146     6.7     3.0     5.2    2.3    C
147     6.3     2.5     5.0    1.9    C
148     6.5     3.0     5.2    2.0    C
149     6.2     3.4     5.4    2.3    C
150     5.9     3.0     5.1    1.8    C
; 

proc print data=iris;
run; 
*KEEP VARIABLE Y1; 
DATA AA; SET IRIS;
 IF ORDER<51  THEN GRP=1;
 IF 50<ORDER<101 THEN GRP=2;
 ELSE IF 100<ORDER<151 THEN GRP=3;
 DROP ORDER CLASS;
RUN; 
PROC SORT DATA=AA; BY GRP;     
RUN; 
PROC PRINT DATA=AA;;
RUN;
DATA A; SET  AA;
IF GRP=1;
RUN;
DATA A1;SET A;
DROP GRP;
PROC PRINT DATA=A1;
RUN;
*** CHECK A TYPE OF IRIS' NORMAILITY;

PROC IML;
 USE A1;
 READ ALL INTO Y;
 N=NROW(Y);           *FINDS NUMBER OF OBSERVATIONS;
 YBAR=1/N*Y`*J(N,1);   *COMPUTES YBAR VECTOR;
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y; *COMPUTES VAR-COV MATRIX OF Y;
 SINV=INV(S);       *COMPUTES INVERSE OF S;
 YB=YBAR*J(1,N,1);  *COMPUTES MATRIX WITH P ROWS AND N COLUMNS -
                     ALL ELEMENTS IN Ith ROW ARE YBARS OF ITH VARIABLE;
 YD=Y-YB`;   *SUBTRACTS YBAR OF VARIABLE FROM EACH OBSERVED VALUE OF 
              VARIABLE - NxP;
 DSQ=YD*SINV*YD`;  *DIAGONAL ELEMENTS ARE STANDARDIZED SQUARED DISTANCE FROM Ith
                    Y VECTOR TO YBAR VECTOR - NxN; 
 D=DIAG(DSQ);  *CREATES NxN DIAGONAL MATRIX WITH Di-SQ'S ON DIAGONALS;  
 D2=D*J(N,1,1); *CREATES Nx1 VECTOR WITH THE Di-SQ'S;
 CREATE SGD FROM D2[COLNAME='D2']; *Creates data set SGD from the vector D2 to import back
                                     to regular SAS;  
APPEND FROM D2; 
print D2;

DATA SGD; SET SGD;
PROC SORT; BY D2;

PROC PRINT DATA=SGD2;

DATA SGD; SET SGD;
 N=50; P=4;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B); 
 UU=V; 

PROC PRINT DATA=SGD;
 VAR U V ;
RUN;

PROC GPLOT DATA=SGD;
  PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=SGD;;
 VAR U V;
RUN;
* *******CHECK FOR TYPE 2 NORMAILITY;

proc print data=iris;
run;   
DATA AA; SET IRIS;
 IF ORDER<51  THEN GRP=1;
 IF 50<ORDER<101 THEN GRP=2;
 ELSE IF 100<ORDER<151 THEN GRP=3;
 DROP ORDER CLASS;
RUN; 
PROC SORT DATA=AA; BY GRP;     
RUN; 
PROC PRINT DATA=AA;;
RUN;
DATA A; SET  AA;
IF GRP=2;
RUN;
DATA A2;SET A;
DROP GRP;
PROC PRINT DATA=A2;
RUN;
*** CHECK A TYPE OF IRIS' NORMAILITY;

PROC IML;
 USE A2;
 READ ALL INTO Y;
 N=NROW(Y);           *FINDS NUMBER OF OBSERVATIONS;
 YBAR=1/N*Y`*J(N,1);   *COMPUTES YBAR VECTOR;
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y; *COMPUTES VAR-COV MATRIX OF Y;
 SINV=INV(S);       *COMPUTES INVERSE OF S;
 YB=YBAR*J(1,N,1);  *COMPUTES MATRIX WITH P ROWS AND N COLUMNS -
                     ALL ELEMENTS IN Ith ROW ARE YBARS OF ITH VARIABLE;
 YD=Y-YB`;   *SUBTRACTS YBAR OF VARIABLE FROM EACH OBSERVED VALUE OF 
              VARIABLE - NxP;
 DSQ=YD*SINV*YD`;  *DIAGONAL ELEMENTS ARE STANDARDIZED SQUARED DISTANCE FROM Ith
                    Y VECTOR TO YBAR VECTOR - NxN; 
 D=DIAG(DSQ);  *CREATES NxN DIAGONAL MATRIX WITH Di-SQ'S ON DIAGONALS;  
 D2=D*J(N,1,1); *CREATES Nx1 VECTOR WITH THE Di-SQ'S;
 CREATE SGD FROM D2[COLNAME='D2']; *Creates data set SGD from the vector D2 to import back
                                     to regular SAS;  
APPEND FROM D2; 
print D2;

DATA SGD; SET SGD;
PROC SORT; BY D2;

PROC PRINT DATA=SGD2;

DATA SGD; SET SGD;
 N=50; P=4;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B); 
 UU=V; 

PROC PRINT DATA=SGD;
 VAR U V ;
RUN;

PROC GPLOT DATA=SGD;
  PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=SGD;;
 VAR U V;
RUN;

*********CHECK TYPE 3 OF IRIS;


DATA AA; SET IRIS;
 IF ORDER<51  THEN GRP=1;
 IF 50<ORDER<101 THEN GRP=2;
 ELSE IF 100<ORDER<151 THEN GRP=3;
 DROP ORDER CLASS;
RUN; 
PROC SORT DATA=AA; BY GRP;     
RUN; 
PROC PRINT DATA=AA;;
RUN;
DATA A; SET  AA;
IF GRP=3;
RUN;
DATA A3;SET A;
DROP GRP;
PROC PRINT DATA=A3;
RUN;
*** CHECK A TYPE OF IRIS' NORMAILITY;

PROC IML;
 USE A3;
 READ ALL INTO Y;
 N=NROW(Y);           *FINDS NUMBER OF OBSERVATIONS;
 YBAR=1/N*Y`*J(N,1);   *COMPUTES YBAR VECTOR;
 S=1/(N-1)*Y`*(I(N)-1/N*J(N))*Y; *COMPUTES VAR-COV MATRIX OF Y;
 SINV=INV(S);       *COMPUTES INVERSE OF S;
 YB=YBAR*J(1,N,1);  *COMPUTES MATRIX WITH P ROWS AND N COLUMNS -
                     ALL ELEMENTS IN Ith ROW ARE YBARS OF ITH VARIABLE;
 YD=Y-YB`;   *SUBTRACTS YBAR OF VARIABLE FROM EACH OBSERVED VALUE OF 
              VARIABLE - NxP;
 DSQ=YD*SINV*YD`;  *DIAGONAL ELEMENTS ARE STANDARDIZED SQUARED DISTANCE FROM Ith
                    Y VECTOR TO YBAR VECTOR - NxN; 
 D=DIAG(DSQ);  *CREATES NxN DIAGONAL MATRIX WITH Di-SQ'S ON DIAGONALS;  
 D2=D*J(N,1,1); *CREATES Nx1 VECTOR WITH THE Di-SQ'S;
 CREATE SGD FROM D2[COLNAME='D2']; *Creates data set SGD from the vector D2 to import back
                                     to regular SAS;  
APPEND FROM D2; 
print D2;

DATA SGD; SET SGD;
PROC SORT; BY D2;

PROC PRINT DATA=SGD2;

DATA SGD; SET SGD;
 N=50; P=4;
 U=N*D2/(N-1)**2;
 A=P/2;
 B=(N-P-1)/2;
 ALPHA=(P-2)/(2*P);
 BETA=(N-P-3)/(2*(N-P-1));
 PR=(_N_-ALPHA)/(N-ALPHA-BETA+1);
 V=BETAINV(PR,A,B); 
 UU=V; 

PROC PRINT DATA=SGD;
 VAR U V ;
RUN;

PROC GPLOT DATA=SGD;
  PLOT U*V=1;
 SYMBOL1 C=RED V=STAR I=RL;
PROC CORR DATA=SGD;;
 VAR U V;
RUN;
**********MEAN FOR DIFFERENT TYPES;
DATA AA; SET IRIS;
 IF ORDER<51  THEN GRP=1;
 IF 50<ORDER<101 THEN GRP=2;
 ELSE IF 100<ORDER<151 THEN GRP=3;
 DROP ORDER CLASS;
RUN; 
PROC SORT DATA=AA; BY GRP;     
RUN; 
* MEAN OF 4 VARIABLES BY GROUP;
PROC SORT DATA=AA; BY GRP;                                                             
PROC MEANS DATA=AA; BY GRP;                                                            
 VAR Y1 Y2 Y3 Y4;             
RUN;
proc print data=AA;
run;
*****one-way -MNOVA;
PROC GLM DATA=AA; CLASS  GRP;                                                     
  MODEL Y1 Y2 Y3 Y4 =  GRP;                                             
  MANOVA H=_ALL_/PRINTH PRINTE SHORT;                                           
  MEANS  GRP /TUKEY LINES;                                                 
 * LSMEANS  GRP/PDIFF; 
  LSMEANS GRP/PDIFF=ALL ADJUST=TUKEY;
RUN; 



****Manova for 3 types -exact P-value;
PROC GLM DATA=AA; CLASS GRP;                                                          
  MODEL  Y1 Y2 Y3 Y4=GRP;                                                  
  MANOVA H=_ALL_/MSTAT=EXACT;                                           
  ODS SELECT MULTSTAT; 
RUN; 
/*Principal component analysis */
PROC CORR DATA=AA;
 VAR Y1 Y2 Y3 Y4;with GRP;
RUN;
PROC PRINCOMP COV OUT=D DATA=AA;
 VAR Y1 Y2 Y3 Y4;
RUN;
PROC GPLOT DATA=D;
 PLOT Prin2*Prin1='*'/VREF=0 HREF=0;
 SYMBOL1 C=RED V=STAR;
 SYMBOL2 C=BLUE V=PLUS;
RUN;

 /*Principal component regression*/
PROC REG DATA=AA;
 MODEL GRP=Y1 Y2 Y3 Y4/VIF;
RUN;

PROC REG DATA=AA;
 MODEL GRP=Y1 Y2 Y3 Y4/SELECTION=RSQUARE ADJRSQ CP BEST=4 AIC MSE;
RUN;

PROC REG DATA=AA;
MODEL GRP=Y3  Y4/VIF;
MODEL GRP=Y2 Y3 Y4/VIF;
MODEL GRP=Y1 Y3 Y4 /VIF;
MODEL GRP=Y1 Y4/VIF;
MODEL GRP=Y2 Y4/VIF;
RUN;



**Factor ANALYSIS;

PROC FACTOR DATA=AA R=V N=2 out=BB;                                                          
 VAR Y1 Y2 Y3 Y4;                                                                    
RUN; 

proc print data=BB;
run;
proc corr data=BB;
var factor1-factor2;
run;
proc means data=BB; class GPR;
var factor1-factor2;
run;
proc glm data=BB; class GPR;
model factor1-factor2=GPR;
manova H=GRP;
means GRP/tukey;
run;

PROC corr data=AA nomiss alpha nosimple nocorr;
var ;
run;

PROC GLM DATA=BB; CLASS GRP;
 MODEL FACTOR1=GRP;
 MANOVA H=GRP;
 MEANS GRP/TUKEY;
RUN;

PROC GLM DATA=BB; CLASS GRP;
 MODEL FACTOR2=GRP;
 MANOVA H=GRP;
 MEANS GRP/TUKEY;
RUN;
