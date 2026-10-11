[ prob ]
One-compartmental model + 0 and 1st order abs and 1st order elim
Allometric scaling on CL and V patrameters
Covariate-effects (SEX, AGE, ALB) on CL
Proportional error model
This model requires mrgsolve >= 1.0.3

[ cmt ] depot cent

[ input ] 
WT  = 70
SEX = 1
AGE = 35
ALB = 4.5

[ theta ]
1.5    //  1 CL (L/hr) - 3.5
5.0    //  2 V  (L) - 60
0.5    //  3 KA (1/hr) - 1.5
0.8    //  4 D1 (/hr) - 1.5
0.7    //  5 SEX~CL (fractional multiplier)
0.5    //  6 AGE~CL (exponent)
0.2    //  7 ALB~CL (exponent)
0.75   //  8 WT~CL (exponent)
1.0    //  9 WT~CL (exponent)

[ omega ] @correlation
0.15             // ETA(CL)
0.50 0.15        // ETA(V)
0.10 0.20 0.1    // ETA(D1)

[ sigma ]
0.15              // Prop

[ pk ]
// Typical values of PK parameters
double TVCL   = THETA(1);
double TVV    = THETA(2);
double TVKA   = THETA(3);
double TVD1   = THETA(4);

// PK covariates
double CL_SEX = pow(THETA(5)  , SEX-1   );
double CL_AGE = pow((AGE/35.0), THETA(6));
double CL_ALB = pow((ALB/4.5) , THETA(7));
double CL_WT  = pow((WT/70)   , THETA(8));
double V_WT   = pow((WT/70)   , THETA(9));

double CLCOV = CL_AGE*CL_WT;
double VCOV  = V_WT;

// PK parameters
double CL = TVCL*CLCOV*exp(ETA(1));
double V  = TVV*VCOV*exp(ETA(2)); 
double KA = TVKA; 
double D1 = TVD1*exp(ETA(3));

double S2 = V;

D_depot = D1; 

[ ode ]
dxdt_depot = -KA*depot;
dxdt_cent = KA*depot - (CL/V)*cent; 

[ error ] 
double IPRED = cent/S2;
double Y = IPRED * (1+EPS(1));

[ capture ] CL V IPRED Y
