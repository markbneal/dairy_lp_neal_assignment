$Title An LP of a dairy farm (Neal, 1999)

***OPTIONS***

option limrow = 4;
option limcol = 4;
option lp = conopt;

***SETS***

Set s Seasons / Summer, Autumn, Winter, Spring /;
Set p Pastures / Kikuyu, Lucerne, Rye /;
Set g Grain / Maize /;
Set m OtherMilkPrices / ManMilkPrice /;

***DATA***
*** Pastures and Fertiliser

Scalar  TotalArea        Total area of land available (ha)   / 150 /;


* Here is an example of reading data from an excel file saved in csv
Table PG(s,p)  Pasture growth (ME units?)
$ondelim
$include PG.csv
$offdelim

*                   Kikuyu        Lucerne           Rye
*         Summer      9000          11025          9000
*         Autumn      4500           7200          4500
*         Winter      4500           7200         12375
*         Spring      9750          11025          7875;

Table LP(s,p)  Labour use for pasture (hrs per ha)

                   Kikuyu        Lucerne           Rye
         Summer        0.             0.            0.
         Autumn        0.             5             5
         Winter        0.             0.            0.
         Spring        0.             0.            0.;

Table CP(s,p)  Cash use for pasture (dol per ha)

                   Kikuyu        Lucerne           Rye
         Summer        0.             0.            0.
         Autumn        0.           250            250
         Winter        0.             0.            0.
         Spring        0.             0.            0.;

Table F(s,p)  Fert Response (ME per kg)

                   Kikuyu        Lucerne           Rye
         Summer      17.5           21.4          17.5
         Autumn       8.7           13.9           8.7
         Winter       8.7           13.9          24.0
         Spring      18.9           21.4          15.3;

Table LF(s,p)  Labour use for fert (hrs per kg)

                   Kikuyu        Lucerne           Rye
         Summer     0.005          0.005           0.
         Autumn     0.             0.              0.
         Winter     0.             0.            0.005
         Spring     0.005          0.005         0.005;

Table CF(s,p)  Cash use for fert (dol per kg)

                   Kikuyu        Lucerne           Rye
         Summer       0.2            0.2           0.
         Autumn       0.             0.            0.
         Winter       0.             0.            0.2
         Spring       0.2            0.2           0.2;

*** Grain and hay

Table CG(s,g)  Cash use for grain (dol per kg)

                    Maize
         Summer       195
         Autumn       200
         Winter       205
         Spring       210;

Scalar MaxFert     Maximum amount of Fertiliser (kg per ha)     / 750 /;

*** Milk and ME

Scalar MEPerL      Met Energy per Litre         (ME per L)     / 4.76 /;

Scalar MktMilk     Market milk price            (Dol per L)    / 0.47 /;

Table OtherMilkPrices(s,m)  Other (inc Manufacturing) milk prices  (Dol per L)

                 ManMilkPrice
         Summer          0.20
         Autumn          0.27
         Winter          0.31
         Spring          0.26;

Scalar MEMaint      Met Energy for Maint         (ME per cow)  / 13440 /;

Scalar MaxGrain     Maximum amount of grain      (kg per cow)    / 720 /;

Scalar MaxLPerCow   Maximum litres per cow       (L per cow)    / 3240 /;

Scalar LC           Labour per cow               (hours per cow) / 6.6 /;

Scalar CC           Cash costs per cow           (dol per cow) / 111.6 /;


***Costs, constraints and RHS

Scalar LabCost      Cost of hired labour         (dol per hour)   / 15 /;

Scalar OwnLab       Owner labour       (hours per season)       / 1440 /;

Scalar FixCosts     Fixed farm costs   (dol per season)        / 12000 /;

Scalar OpCash       Opening cash                 (dol)         / 40000 /;

Scalar MaxOD        Maximum overdraft            (dol)         / 60000 /;

Scalar Quota        Quota per season   (L per season)         / 200000 /;

Scalar MaxCows      Maximum number of cows       (cows)          / 450 /;

Scalar ODInt        Overdraft interest (1 plus (i per season))  / 1.03 /;

Scalar kgT          Kilograms in a tonne         (1000)         / 1000 /;

Scalar MET          ME in a tonne of grain       (ME per t)     / 8000 /;

***VARIABLES***

Variables        PastureArea(p)        PastureArea planted to pasture(p)   (ha)
                 Fertiliser(p)         Fertiliser on pasture(p)            (kg?)
                 PastMEToAvailME(s,p)  Transfer pasture ME to available ME (ME)
                 Grain(s)              Grain in season(s)                  (t)
                 AvailMEToMilk(s)      Avalaible ME transfered to milk     (ME)
                 MilkToMkt(s)          Milk transferred to market          (L)
                 MilkToMan(s)          Milk transferred to manufacturing   (L)
                 NumberCows(s)         Number of cows in season(s)         (cows)
                 FixedCosts            Fixed costs                         (dol)
                 HireLab(s)            Hire of labour in season(s)         (hours)
                 CashTransfer(s)       Transfer of cash between seasons    (dol)
                 Overdraft(s)          Overdraft and transfers (s)         (dol)
                 Objective             Value of final cash balance         ($)
;

Positive Variables
                 PastureArea(p)
                 Fertiliser(p)
                 PastMEToAvailME(s,p)
                 Grain(s)
                 AvailMEToMilk(s)
                 MilkToMkt(s)
                 MilkToMan(s)
                 NumberCows(s)
                 FixedCosts
                 HireLab(s)
                 CashTransfer(s)
                 Overdraft(s)
;

***EQUATIONS***

Equations  ObjFunction(s) ObjectiveFunction -> max final cash balance
           AreaSum       PastureArea must be less than TotalArea
           PastUse(s,p)  Kikuyu production and use
           ME(s)         ME balance
           MilkLitres(s) Milk Balance
           FertLimit(p)  Fertiliser Balance
           GrainLimit(s) Grain Balance
           QuotaLimit(s) Quota Balance
           MaxLitres(s)  Limit on production per cow
           LabourLimit(s) Labour Balance
           CashLimit(s)  Cash Balance
           OverdraftLimit(s) Overdraft limit
           CowsEqual(s)  Cows equal in all seasons
           CowsLimit(s)     Not more than a certain number of cows
           FixedCostsLimit Fixed costs equal one
;

ObjFunction(s)..
  CashTransfer(s)$(ord(s)=card(s)) - ODInt*Overdraft(s)$(ord(s)=card(s))
                                     =e= Objective$(ord(s)=card(s));
AreaSum..
  sum(p, PastureArea(p))                                  =l= TotalArea;
PastUse(s,p)..
  - PastureArea(p)  * PG(s,p)
  - Fertiliser(p)   * F(s,p)
  + PastMEToAvailME(s,p)                                 =l= 0;
ME(s)..
  - sum(p,PastMEToAvailME(s,p)) - Grain(s) * MET
  + AvailMEToMilk(s) * MEPerL  + NumberCows(s)* MEMaint   =l= 0;
MilkLitres(s)..
  - AvailMEToMilk(s) + MilkToMkt(s) + MilkToMan(s)        =l= 0;
FertLimit(p)..
  - PastureArea(p)  * MaxFert + Fertiliser(p)             =l= 0;
GrainLimit(s)..
  Grain(s) * kgT    - NumberCows(s) * MaxGrain            =l= 0;
QuotaLimit(s)..
  MilkToMkt(s)                                            =l= Quota;
MaxLitres(s)..
  AvailMEToMilk(s)   - NumberCows(s) * MaxLPerCow         =l= 0;

LabourLimit(s)..
  Sum(p, PastureArea(p)  * LP(s,p))
  + Sum(p, Fertiliser(p)   * LF(s,p))
  + NumberCows(s) * LC  - HireLab(s)                      =l= OwnLab;

CashLimit(s)..
  Sum(p, PastureArea(p)  * CP(s,p))
  + Sum(p, Fertiliser(p)   * CF(s,p))
  + sum(g, Grain(s) * CG(s, g)) + NumberCows(s) * CC
  - MilkToMkt(s) * MktMilk
  - sum(m, MilkToMan(s) * OtherMilkPrices(s, m)) + FixedCosts * Fixcosts
  + HireLab(s) * LabCost
  + CashTransfer(s)        - CashTransfer(s-1)$(ord(s) > 1)
  - Overdraft(s)           + Overdraft(s-1)$(ord(s) > 1)*ODInt
                                            =l= (ord(s) = 1)*OpCash;
OverdraftLimit(s)..
  Overdraft(s)                                            =l= MaxOD;
CowsEqual(s)..
  NumberCows(s) - NumberCows(s--1)                        =e= 0;
CowsLimit(s)..
  NumberCows(s)$(ord(s)=card(s))                          =l= MaxCows;
FixedCostsLimit..
  FixedCosts                                              =e= 1;

***MODEL AND SOLVE***

Model DairyFarm  / All / ;

Option SAVEPOINT=1;

Solve DairyFarm using lp maximizing Objective;
                     
Display PastureArea.l;
