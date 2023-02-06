      BLOCK DATA 
C
C     * MAR 13/09 - D.VERSEGHY. REPLACE SURFCON COMMON BLOCK WITH
C     *                         CLASSD2; NEW VARIABLE ANGMAX
C     * FEB 06/07 - D.VERSEGHY. NEW VALUE FOR ALVSO AND ALIRI.
C     * NOV 04/04 - D.VERSEGHY. NEW VALUES FOR ALVSI AND ALIRI.
C     * DEC 02/03 - D.VERSEGHY. HARMONIZE VALUES OF HCPICE,
C     *                         RHOICE AND SPHICE.
C     * JUL 18/03 - D.VERSEGHY. SPLIT OFF DATA STATEMENTS FROM
C     *                         OLD "CLASSD" ROUTINE TO COMPLY
C     *                         WITH FORTRAN 90 CONVENTION.
C     * AUG 06/02 - D.VERSEGHY. DEFINE PHYSICAL CONSTANTS PASSED
C     *                         THROUGH CLASS COMMON BLOCKS.  
C     *                         (BASED ON ROUTINE "HYDCON".)
C
C     * THE FOLLOWING COMMON BLOCK PARAMETERS ARE DEFINED WITHIN
C     * THE GCM.
C
      COMMON /PARAMS/ X1,    X2,    X3,    X4,   G,GAS,   X5,
     1                X6,    CPRES, GASV,  X7
      COMMON /PARAM1/ CPI,   X8,    CELZRO,X9,    X10,    X11
      COMMON /PARAM3/ X12,   X13,   X14,   X15,   SIGMA,  X16
      COMMON  /TIMES/ DELTIM,K1,    K2,    K3,    K4,     K5,
     1                K6,    K7,    K8,    K9,    K10,    K11

C     * COMMON BLOCKS DEFINED SPECIFICALLY FOR USE IN CLASS.

      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS5/ THPORG(3),THRORG(3),THMORG(3),BORG(3),
     1                PSISORG(3),GRKSORG(3)
      COMMON /CLASS6/ PI,GROWYR(18,4,2),ZOLNG,ZOLNS,ZOLNI,ZORAT(4),
     1                ZORATG
      COMMON /CLASS7/ CANEXT(4),XLEAF(4)
      COMMON /CLASS8/ ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
C
      DATA      VKC,        CT,         VMIN
     1       /  0.40,       1.15E-3,    0.1     /

      DATA      TCW,        TCICE,      TCSAND,     TCCLAY,     TCOM
     1       /  0.57,       2.24,       2.5,        2.5,        0.25   /
C     1       /  0.57,       2.24,       8.0,        2.5,        0.25   /

      DATA      TCDRYS,     RHOSOL,     RHOOM
     1       /  0.275,      2.65E3,     1.30E3  /    

      DATA      HCPW,       HCPICE,     HCPSOL,     HCPOM
     1       /  4.187E6,    1.9257E6,   2.25E6,     2.50E6 /

      DATA      HCPSND,     HCPCLY,     SPHW,       SPHICE,     SPHVEG
     1       /  2.13E6,     2.38E6,     4.186E3,    2.10E3,     2.70E3 /

      DATA      RHOW,       RHOICE,     TCGLAC,     CLHMLT,     CLHVAP
     1       /  1.0E3,      0.917E3,    2.24,       0.334E6,    2.501E6/

      DATA      ZOLNG,      ZOLNS,      ZOLNI,      ZORATG
     1       /  -4.605,     -6.908,     -6.215,     3.0         /

      DATA      ALVSI,      ALIRI,      ALVSO,      ALIRO,      ALBRCK
     1       /  0.95,       0.73,       0.05,       0.30,       0.27  /

      DATA GROWYR   /213.,213.,213.,213.,213.,213.,0.,0.,0.,
     1               0.,0.,0., 75.,106.,136.,167.,167.,167.,
     2               273.,273.,273.,273.,273.,273.,0.,0.,0.,
     3               0.,0.,0.,135.,166.,196.,196.,196.,196.,
     4               121.,121.,121.,121.,121.,121.,0.,0.,0.,
     5               0.,0.,0.,275.,244.,214.,214.,214.,214.,
     6               151.,151.,151.,151.,151.,151.,0.,0.,0.,
     7               0.,0.,0.,305.,274.,244.,244.,244.,244.,
     8               213.,213.,213.,213.,213.,213.,0.,0.,0.,
     9               0.,0., 75.,106.,136.,167.,167.,167.,167.,
     A               273.,273.,273.,273.,273.,273.,0.,0.,0.,
     B               0.,0.,135.,166.,196.,196.,196.,196.,196.,
     C               121.,121.,121.,121.,121.,121.,0.,0.,0.,
     D               0.,0.,275.,244.,214.,214.,214.,214.,214.,
     E               151.,151.,151.,151.,151.,151.,0.,0.,0.,
     F               0.,0.,305.,274.,244.,244.,244.,244.,244. /
C
      DATA ZORAT    /1.0,1.0,1.0,1.0/
      DATA CANEXT   /-0.5,-1.5,-0.8,-0.8/
      DATA XLEAF    /0.0247,0.0204,0.0456,0.0456/
      DATA THPORG   /0.93,0.88,0.83/
      DATA THRORG   /0.275,0.620,0.705/
      DATA THMORG   /0.04,0.15,0.22/
      DATA BORG     /2.7,6.1,12.0/
      DATA PSISORG  /0.0103,0.0102,0.0101/
      DATA GRKSORG  /2.8E-4,2.0E-6,1.0E-7/
C
C     * ASSIGN VALUES NORMALLY SPECIFIED WITHIN THE GCM.
C
      DATA      G,          GAS,        CPRES,      GASV
     1/         9.80616,    287.04,     1.00464E3,  461.50   /

      DATA      CELZRO,     SIGMA,      DELTIM
C     1/         273.16,     5.66796E-8, 900.0   /
     1/         273.16,     5.66796E-8, 1800.0   /

      DATA      CPI 
     1       /  3.1415926535898    /
C
C     * ADDITIONAL VALUES FOR RPN AND GCM COMMON BLOCKS.
C
      DATA      DELTA,      AS,         ASX,        ANGMAX
     1       /  0.608,      12.0,       4.7,        0.85   /

      DATA      CI,         BS,         BETA,       FACTN,      HMIN 
     1       /  40.0,       1.0,        1.0,        1.2,        40.   /

      DATA      X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8
     1       /  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0  /

      DATA      X9,  X10, X11, X12, X13, X14, X15, X16
     1       /  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0  /

      DATA      K1,  K2,  K3,  K4,  K5,  K6,  K7,  K8
     1       /  0,   0,   0,   0,   0,   0,   0,   0    /

      DATA      K9,  K10, K11
     1       /  0,   0,   0    /
C
      END
