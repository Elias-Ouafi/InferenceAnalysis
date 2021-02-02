/*Traitement des donn�es*/
proc import datafile="path\Life_expectancy_2014.csv" 
    out=Infe.Life_expectancy_2014
    dbms=csv                
    replace;              
    delimiter=',';          
    getnames=yes;       
run; 

proc import datafile="path\Life_expectancy.csv" 
    out=Infe.Life_expectancy
    dbms=csv                
    replace;              
    delimiter=',';          
    getnames=yes;       
run;

/*Drop les colonnes avec beaucoup trop d'informations manquantes*/
data infe.Life_expectancy_2014;
	set infe.Life_expectancy_2014 (drop = Annee mort_adult mort_enfant Hepatite_B 
rougeole moins5_morts Polio Diphtherie SIDA Population maigre1 maigre5 depenses_sante depenses_indiv taux_depenses);
run;

data infe.Life_expectancy;
	set infe.Life_expectancy (drop = mort_adult mort_enfant Hepatite_B 
rougeole moins5_morts Polio Diphterie SIDA Population maigre1 maigre5 depenses_sante depenses_indiv taux_depenses);
run;


/*----------------------------------------------------------------------------
QUESTION 1 : analyses descriptives
distribution Y*/
Proc freq data=infe.Life_expectancy_2014;
	Tables esperance;
Run ;

/*Analyses var cat�gorielles*/
Proc freq data=infe.Life_expectancy_2014;
	Tables statut;
Run ;

/*Analyses var continues*/
Proc means data=infe.Life_expectancy_2014;
	Var esperance alcool IMC PIB educ;
Run ;

/*histrogrammes*/
proc sgplot data=infe.Life_expectancy_2014;
histogram esperance ;
run;

proc sgplot data=infe.Life_expectancy_2014;
histogram PIB ;
run;

proc sgplot data=infe.Life_expectancy_2014;
histogram alcool;
run;

proc sgplot data=infe.Life_expectancy_2014;
histogram IMC;
run;




/*------------------------------------------------------
analyses bivari�es
QUESTION 2 : test T*/
PROC TTEST data=Infe.Life_expectancy_2014; 
Class statut;
Var esperance;
Run ;

/*QUESTION 3 : R�gression Lin�aire simple
m�thode informelle*/
proc sgplot data=Infe.Life_expectancy_2014 ;
scatter y=esperance x=educ;
reg y=esperance x=educ ;
yaxis label="esp�rance de vie";	
xaxis label="Education";	
run;
/*m�thode formelle*/
Proc corr data =Infe.Life_expectancy_2014;
Var esperance educ;
Run ;
/*ajuster le mod�le de r�gression lin�aire*/
proc glm data=Infe.Life_expectancy_2014;
model esperance=educ  / ss3 solution clparm ;
run;
/*v�rifier des r�sidus
Obtenir les RJS :*/
proc glm data=Infe.Life_expectancy_2014;
class Statut(ref="1");
model esperance=educ /ss3 solution;
output out=residuals predicted=pred rstudent=dsresid;
run;
/*Histogramme et qqplot :*/
proc sgplot data= residuals;
histogram dsresid;
density dsresid;
density dsresid / type=kernel;
keylegend / location=inside position=topright;
run;
proc univariate data= residuals noprint;
qqplot dsresid / normal(mu=est sigma=est color=red l=2) square;
run;
/*H�t�rosc�dasticit� :*/
proc sgscatter data=residuals;         
plot dsresid*pred / reg=(degree=1);
run;
proc loess data=residuals plots(only)=(fit);
model dsresid=esperance / clm;
run;

proc loess data=residuals plots(only)=(fit);
model dsresid=educ / clm;
run;



/*------------------------------------------------------------ 
Analsyes multivari�es
QUESTION 5 R�gression lin�aire multiple
step 1: v�rifier la corr�lation
m�thode informelle*/
Proc sgscatter data=Infe.Life_expectancy_2014;
Plot esperance*IMC / reg=(degree=1);
Run;

Proc sgscatter data=Infe.Life_expectancy_2014;
Plot esperance*educ / reg=(degree=1);
Run;

Proc sgscatter data=Infe.Life_expectancy_2014;
Plot esperance*PIB / reg=(degree=1);
Run;

/*m�thode formelle*/
Proc corr data=Infe.Life_expectancy_2014;
Var esperance IMC educ PIB;
Run;

/*ajustement du mod�le de r�gression lin�aire multiple*/
Proc glm data=Infe.Life_expectancy_2014;
class Statut(ref="1");
Model esperance=IMC educ PIB/ ss3 solution clparm;
Run ;

/*v�rif des r�sidus
Obtenir les RJS :*/
proc glm data=Infe.Life_expectancy_2014;
class Statut(ref="1");
model esperance=IMC educ PIB/ss3 solution;
output out=residuals predicted=pred rstudent=dsresid;
run;
/*Histogramme et qqplot :*/
proc sgplot data= residuals;
histogram dsresid;
density dsresid;
density dsresid / type=kernel;
keylegend / location=inside position=topright;
run;
proc univariate data= residuals noprint;
qqplot dsresid / normal(mu=est sigma=est color=red l=2) square;
run;
/*H�t�rosc�dasticit� :*/
proc sgscatter data=residuals;         
plot dsresid*pred / reg=(degree=1);
run;
proc loess data=residuals plots(only)=(fit);
model dsresid=esperance / clm;
run;



/*QUESTION 6 : Analyse Longitudinales
Pour obtenir un graphe spaghetti de l'esp�rance de vie dans le temps pour chaque pays*/
Proc sgplot data=infe.life_expectancy;
series x=annee y=esperance /group=pays LineAttrs= (pattern=1);
run;


*Pour cr�er une copie de la variable temps;
data temp;
set infe.life_expectancy;
anneecat=annee;
run;

*Pour ajuster une mod�le avec structure de covariance compound symmetry-- ou 
on souppose les observations et erreurs sont corrol�s;
proc mixed data=temp method=reml covtest;
class pays anneecat;
model esperance= imc statut educ alcool annee /solution cl;
repeated anneecat / subject=pays type=cs r=2 rcorr=2;
run;
