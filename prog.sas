
*libname for teacher;
libname c 'C:\Users\stefano.balla\Desktop\clamda2021';
*preprocessing;
*scaling;
*pca;
*clustering;
*ttest/pvalue;
*chisquare test;

data c.luxurycopy; set c.Luxury;
if (d11 < 3) THEN delete ;
run;


data c.lux; set c.luxurycopy;
if (d1= 6 OR d1=38 OR d1= 42 OR d1=97 OR d1=58 OR d1=80) then delete;
run;

proc freq data= c.Luxury; /* distribuzione delle risposte prima e dopo il cleaning*/
run;

proc freq data= c.lux;
run;

proc means data=c.lux;
run;

proc corr data=c.lux;  /* correlazione fra variabili*/
var d11-d29;
run;
/****** principal component analysis****/

data c.lux_adj; set c.lux;
avgi=mean(of d11-d29);
mini=min(of d11-d29);
maxi=max(of d11-d29);
run;


proc princomp data=c.lux_adj out=c.coord;
var d11-d29;
run;

proc corr data=c.coord;
var prin1 avgi;
run;


data c.lux_adj_1; set c.lux_adj;
array x d11-d29;
array x_new new11-new29;
do over x_new;
x_new=.;
if x>avgi then x_new=(x-avgi)/(maxi-avgi);
if x<avgi then x_new=(x-avgi)/(avgi-mini);
if x=avgi then x_new=0;
if x=. then x_new=0;
end;
label new11='Livello di Interesse';
label new12='Negozio';
label new13='Negozio2';
label new14='Negozio3';
label new15='Negozio4';
label new16='%acquisti';
label new17='%acquisiti2';
label new18='Quanto Daccordo';
label new19='Negozio5';
label new20='Quanto Daccordo2';
label new21='Quanto Daccordo 3';
label new22='Quanto Daccordo 4';
label new23='Quanto Daccordo 5';
label new24='Quanto Daccordo 6';
label new25='Quanto Daccordo 7';
label new26='Quanto Daccordo 8';
label new27='Quanto Daccordo 9';
label new28='Quanto Daccordo 10';
label new29='Quanto Daccordo 11';
run;

proc means data=c.lux_adj_1;
var new:;
run;



proc princomp data=c.lux_adj_1 out=c.coord_adj;
var new:;
run;
/*********** FAMO CLUSTERING*****/


proc cluster data=c.coord_adj method=ward outtree=c.tree_adj;
var prin1-prin10;
id d1;
run;

proc tree; run;  /* abbiam fatto il dendogram*/


**qui;
proc tree data=c.tree_adj;
id d1;
run;

proc tree data=c.tree_adj out=c.cluster_adj noprint nclusters=4; /* selezioniamo 4 cluster */
id d1;
run;

proc freq data=c.cluster_adj;
table cluster;
run;

proc sort data=c.lux_adj_1; by d1; run;
proc sort data=c.cluster_adj; by d1; run;

data c.lux_adj_2; merge c.lux_adj_1 c.cluster_adj;
by d1 ;
run;

*aggiungo il cluster fake 5;
data c.lux_adj_fake; set c.lux_adj_2;
cluster=5;
run;

*data preparation of final dset for proc ttest, faccio join doppiando il dataset aggiungendo la copia con tag cluster=5;
data c.lux_adj_app;
set c.lux_adj_2 c.lux_adj_fake;
run;




*macro to produce 5 analysis of our 5 clusters;
%macro do_k_cluster;
%do k=1 %to 4;
proc ttest data=c.lux_adj_app;
where cluster=&k or cluster=5;
class cluster;
var new:;
ods output ttests=c.cl_ttest_&k (where=( method='Satterthwaite') 
rename=(tvalue=tvalue_&k) rename=(probt=prob_&k));
run;
%end;
%mend do_k_cluster;
%do_k_cluster;








data c.ttest_all; merge c.cl_ttest_1 c.cl_ttest_2 c.cl_ttest_3 c.cl_ttest_4;
by variable;
run;

proc contents data=c.lux_adj_app out=c.content noprint;
run;

data c.content_1; set c.content;
keep name label;
rename name=variable;
run;

proc sort data=c.ttest_all; by variable; run;
proc sort data=c.content_1; by variable; run;


data c.ttest_all_1; 
merge c.ttest_all (in=a) c.content_1(in=b);
by variable;
if a;
run;

proc princomp data=c.ttest_all_1 noprint
out=c.coord_ttest outstat=c.coord_ttest_cluster;
var tvalue:;
run;

proc transpose data=c.coord_ttest_cluster out=c.coord_ttest_cluster_1; run;

proc sgplot data=c.coord_ttest_cluster_1;
scatter x=prin1 y=prin2 / datalabel=_name_;
run;

proc sgplot data=c.coord_ttest;
scatter x=prin1 y=prin2 / datalabel=label;
run;

************* scartare fino prossimo commento************;
*macro to produce 5 analysis of our 5 clusters using supplementary quant variables;
%macro do_k_cluster;
%do k=1 %to 5;
proc ttest data=c.unico_adj_app;
where cluster=&k or cluster=6;
class cluster;
var red pound--satisf;
ods output ttests=c.cl_ttest_sup_&k (where=( method='Satterthwaite') 
rename=(tvalue=tvalue_&k) rename=(probt=prob_&k));
run;
%end;
%mend do_k_cluster;
%do_k_cluster;

proc sort data=c.cl_ttest_sup_1; by variable; run;
proc sort data=c.cl_ttest_sup_2; by variable; run;
proc sort data=c.cl_ttest_sup_3; by variable; run;
proc sort data=c.cl_ttest_sup_4; by variable; run;
proc sort data=c.cl_ttest_sup_5; by variable; run;
data c.ttest_sup_all; 
merge 
c.cl_ttest_sup_1 
c.cl_ttest_sup_2 
c.cl_ttest_sup_3 
c.cl_ttest_sup_4 
c.cl_ttest_sup_5;
by variable;
run;

*****fine commento****

*descrizione di tutti i cluster using cat-variable gendre;
proc freq data=c.lux_adj_2;
table d7*cluster;
run;

proc freq data=c.lux_adj_2;
table d7*cluster / expected chisq;
run;

*test GENERE-D7 cluster1;
data c.lux_adj_3; set c.lux_adj_2;
cluster1=.;
if cluster=1 then cluster1=1;
else cluster1=2;
run;

proc freq data=c.lux_adj_3;
table cluster1*d7 / expected chisq;
run;

*test GENERE cluster2;
data c.lux_adj_4; set c.lux_adj_3;
cluster2=.;
if cluster=2 then cluster2=1;
else cluster2=2;
run;

proc freq data=c.lux_adj_4;
table cluster2*d7 / expected chisq;
run;

*test GENERE cluster3;   ******* prendere grafico;
data c.lux_adj_5; set c.lux_adj_4;
cluster3=.;
if cluster=3 then cluster3=1;
else cluster3=2;
run;

proc freq data=c.lux_adj_5;
table cluster3*d7 / expected chisq;
run;

*test GENERE cluster4;
data c.lux_adj_6; set c.lux_adj_5;
cluster4=.;
if cluster=4 then cluster4=1;
else cluster4=2;
run;

proc freq data=c.lux_adj_6;
table cluster4*d7 / expected chisq;
run;


************************************************ETA********************;

*descrizione di tutti i cluster using cat-variable età binned;
proc freq data=c.lux_adj_2;
table d8*cluster;
run;

proc freq data=c.lux_adj_2;
table d8*cluster / expected chisq;
run;

*test ETA-D8 cluster1;
data c.lux_adj_3; set c.lux_adj_2;
etacluster1=.;
if cluster=1 then etacluster1=1;
else etacluster1=2;
run;

proc freq data=c.lux_adj_3;
table etacluster1*d8 / expected chisq;
run;

*test ETA cluster2;
data c.lux_adj_4; set c.lux_adj_3;
etacluster2=.;
if cluster=2 then etacluster2=1;
else etacluster2=2;
run;

proc freq data=c.lux_adj_4;
table etacluster2*d8 / expected chisq;
run;

*test ETA cluster3;   
data c.lux_adj_5; set c.lux_adj_4;
etacluster3=.;
if cluster=3 then etacluster3=1;
else etacluster3=2;
run;

proc freq data=c.lux_adj_5;
table etacluster3*d8 / expected chisq;
run;

*test ETA cluster4;
data c.lux_adj_6; set c.lux_adj_5;
etacluster4=.;
if cluster=4 then etacluster4=1;
else etacluster4=2;
run;

proc freq data=c.lux_adj_6;
table etacluster4*d8 / expected chisq;
run;


*************PROFESSIONE**********

*descrizione di tutti i cluster using cat-variable età binned;
proc freq data=c.lux_adj_2;
table d9*cluster;
run;

proc freq data=c.lux_adj_2;
table d9*cluster / expected chisq;
run;

*test PROFESSIONI-D9 cluster1;
data c.lux_adj_3; set c.lux_adj_2;
procluster1=.;
if cluster=1 then procluster1=1;
else procluster1=2;
run;

proc freq data=c.lux_adj_3;
table procluster1*d9 / expected chisq;
run;

*test PRO cluster2;
data c.lux_adj_4; set c.lux_adj_3;
procluster2=.;
if cluster=2 then procluster2=1;
else procluster2=2;
run;

proc freq data=c.lux_adj_4;
table procluster2*d9 / expected chisq;
run;

*test PRO cluster3;   
data c.lux_adj_5; set c.lux_adj_4;
procluster3=.;
if cluster=3 then procluster3=1;
else procluster3=2;
run;

proc freq data=c.lux_adj_5;
table procluster3*d9 / expected chisq;
run;

*test PRO cluster4;
data c.lux_adj_6; set c.lux_adj_5;
procluster4=.;
if cluster=4 then procluster4=1;
else procluster4=2;
run;

proc freq data=c.lux_adj_6;
table procluster4*d9 / expected chisq;
run;


****************************** PROVENIENZA**************;

*descrizione di tutti i cluster using cat-variable PROVENIENZA;
proc freq data=c.lux_adj_2;
table d10*cluster;
run;

proc freq data=c.lux_adj_2;
table d10*cluster / expected chisq;
run;

*test LOC-D10 cluster1;    ************** DA CONSIDERARE;
data c.lux_adj_3; set c.lux_adj_2;
LOCcluster1=.;
if cluster=1 then LOCcluster1=1;
else LOCcluster1=2;
run;

proc freq data=c.lux_adj_3;
table LOCcluster1*d10 / expected chisq;
run;

*test LOCATION cluster2;
data c.lux_adj_4; set c.lux_adj_3;
LOCcluster2=.;
if cluster=2 then LOCcluster2=1;
else LOCcluster2=2;
run;

proc freq data=c.lux_adj_4;
table LOCcluster2*d10 / expected chisq;
run;

*test LOCATION cluster3;   
data c.lux_adj_5; set c.lux_adj_4;
LOCcluster3=.;
if cluster=3 then LOCcluster3=1;
else LOCcluster3=2;
run;

proc freq data=c.lux_adj_5;
table LOCcluster3*d10 / expected chisq;
run;

*test LOC cluster4;
data c.lux_adj_6; set c.lux_adj_5;
LOCcluster4=.;
if cluster=4 then LOCcluster4=1;
else LOCcluster4=2;
run;

proc freq data=c.lux_adj_6;
table LOCcluster4*d10 / expected chisq;
run;





