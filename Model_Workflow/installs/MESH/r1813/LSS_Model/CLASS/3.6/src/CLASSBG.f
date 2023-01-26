      subroutine CLASSBG(THPOR, THLRET, THLMIN, BI, PSISAT, GRKSAT,
     &                   THLRAT, HCPS, TCS, THFC, PSIWLT,
     &                   DELZW, ZBOTW, ALGWET, ALGDRY,
     &                   SAND, CLAY, ORGM, DELZ, ZBOT, SDEPTH,
     &                   ISAND, IGDR, NML, IL1, IL2, IG, ICTEMMOD,
     &                   WC_THPOR, WC_THLRET, WC_THLMIN, WC_BI,
     &                   WC_PSISAT, WC_GRKSAT, WC_HCPS, WC_TCS,
     &                   NL, NM, ILG, ILMOS, JLMOS)
C
C     * NOV 11/11 - M.LAZARE.   - IMPLEMENT CTEM CHOICE OF
C     *                           ALGDRY DETERMINED BY ADDED
C     *                           PASSED SWITCH "ICTEMMOD".
C     * OCT 18/11 - M.LAZARE.   - REMOVE UNUSED "IORG".
C     *                         - CHANGE "THSAND", "THORG"
C     *                           AND "THFINE" FROM ARRAYS
C     *                           (INTERNAL ONLY) TO SCALAR.
C     *                         - IGDR NOW PASSED OUT TO BE
C     *                           USED IN GRINFL/GRDRAN/WEND.
C     *                         - PASS IN IL1 AND IL2 TO
C     *                           DEFINE LOOPS.
C     * OCT 08/11 - M.LAZARE.   ALGDRY CHANGED BACK TO FORMULA
C     *                         USED IN GCM15I (.0056->.0046).
C     * SEP 27/11 - D.VERSEGHY. CONSTRAIN DELZW TO BE >= 5 CMS
C     *                         TO AVOID PROBLEMATIC UNDERSHOOTS.
C     * AUG 25/11 - D.VERSEGHY. USE THFC FORMULATION FOR BOTTOM
C     *                         LAYER AT BOTTOM OF SOIL PERMEABLE
C     *                         DEPTH.
C     * DEC 23/09 - V.FORTIN.   REVISE CALCULATION OF THFC FOR
C     *                         BOTTOM LAYER IN MINERAL SOILS
C     *                         ACCORDING TO SOULIS ET AL. (2009).
C     * JAN 06/09 - D.VERSEGHY. REVERSE ORDER OF 200 AND 300 LOOPS.
C     * DEC 11/07 - D.VERSEGHY. CHANGE CALCULATION OF TCS FROM
C     *                         GEOMETRIC MEAN TO LINEAR MEAN.
C     * FEB 07/07 - D.VERSEGHY. SET THFC TO THLRET FOR ORGANIC SOILS;
C     *                         STREAMLINE SOME CALCULATIONS.
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 300 LOOP.
C     * APR 06/05 - D.VERSEGHY. MOVE CALCULATION OF GRKTLD
C     *                         INTO GRINFL; REVISED CALCULATION
C     *                         OF ALGDRY (WITH M.LAZARE).
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 04/03 - D.VERSEGHY. PERMEABLE THICKNESS OF SOIL
C     *                         LAYERS CONSTRAINED TO >= 1 MM;
C     *                         PROTECT SENSITIVE CALCULATIONS
C     *                         AGAINST ROUNDOFF ERROR.
C     * JUN 28/02 - D.VERSEGHY. ASSIGN SOIL HYDROLOGICAL AND
C     *                         THERMAL PROPERTIES BASED ON
C     *                         SAND, CLAY AND ORGANIC MATTER
C     *                         CONTENT.
C
      use FLAGS
      implicit none
C
C     * INTEGER CONSTANTS.
C
      integer NML, IL1, IL2, IG, ICTEMMOD, NL, NM, ILG,
     &        k, m, j
C
C     * OUTPUT ARRAYS.
C
      real THPOR(NML, IG), THLRET(NML, IG), THLMIN(NML, IG),
     &     BI(NML, IG), PSISAT(NML, IG), GRKSAT(NML, IG),
     &     THLRAT(NML, IG), HCPS(NML, IG),
     &     TCS(NML, IG), THFC(NML, IG), PSIWLT (NML, IG),
     &     DELZW(NML, IG), ZBOTW(NML, IG),
     &     ALGWET(NML), ALGDRY(NML)
C
      integer ISAND(NML, IG), IGDR(NML)
C
C     * INPUT ARRAYS.
C
      real SAND(NML, IG), CLAY(NML, IG), ORGM(NML, IG),
     &     DELZ(IG), ZBOT(IG), SDEPTH (NML)
C
      real THPORG(3), THRORG(3), THMORG(3),
     &     BORG(3), PSISORG(3), GRKSORG(3)
C
C     * TEMPORARY VARIABLES.
C
      real VSAND, VORG, VFINE, VTOT, AEXP, ABC, THSAND, THFINE, THORG

C     * VARIABLES FOR SOIL.INI FILE
      integer ILMOS(ILG), JLMOS(ILG)
      real WC_THPOR(NM, IG), WC_THLRET(NM, IG),
     &     WC_THLMIN(NM, IG), WC_BI(NM, IG),
     &     WC_PSISAT(NM, IG), WC_GRKSAT(NM, IG),
     &     WC_HCPS(NM, IG), WC_TCS(NM, IG)
C
C     * COMMON BLOCK PARAMETERS.
C
      real TCW, TCICE, TCSAND, TCFINE, TCOM, TCDRYS, RHOSOL, RHOOM,
     &     HCPW, HCPICE, HCPSOL, HCPOM, HCPSND, HCPFIN, SPHW, SPHICE,
     &     SPHVEG, SPHAIR, RHOW, RHOICE, TCGLAC, CLHMLT, CLHVAP
C
      common /CLASS3/ TCW, TCICE, TCSAND, TCFINE, TCOM, TCDRYS,
     &                RHOSOL, RHOOM
      common /CLASS4/ HCPW, HCPICE, HCPSOL, HCPOM, HCPSND, HCPFIN,
     &                SPHW, SPHICE, SPHVEG, SPHAIR, RHOW, RHOICE,
     &                TCGLAC, CLHMLT, CLHVAP
      common /CLASS5/ THPORG, THRORG, THMORG, BORG, PSISORG, GRKSORG
C
C-----------------------------------------------------------------------
C
      do 50 k = IL1, IL2
        IGDR(k) = 1
C
50    continue
C
      do 100 j = 1, IG
        do 100 k = IL1, IL2
          ISAND(k, j) = NINT(SAND(k, j))
          if (ISAND(k, j) > -3) IGDR(k) = j
C
100   continue
C
      do 200 k = IL1, IL2
        do 150 j = 1, IG
          if (ISAND(k, 1) == -4) then
            DELZW(k, j) = DELZ(j)
            ISAND(k, j) = -4
          else if (ISAND(k, j) == -3) then
            DELZW(k, j) = 0.0
          else if (SDEPTH(k) >= ZBOT(j)) then
            DELZW(k, j) = DELZ(j)
          else if (SDEPTH(k) < (ZBOT(j) - DELZ(j) + 0.025)) then
            DELZW(k, j) = 0.0
            ISAND(k, j) = -3
          else
            DELZW(k, j) = max(0.05, (SDEPTH(k) - (ZBOT(j) - DELZ(j))))
          end if
          ZBOTW(k, j) = max(0.0, ZBOT(j) - DELZ(j)) + DELZW(k,j)
C
150     continue
C
        if (SAND(k, 1) >= 0.0) then
          ALGWET(k) = 0.08 + 0.0006*SAND(k, 1)
          if (ICTEMMOD == 0) then
            ALGDRY(k) = min(0.14 + 0.0046*SAND(k, 1), 0.45) ! FOR GLC2000
          else
            ALGDRY(k) = min(0.14 + 0.0027*SAND(k, 1), 0.41) ! FOR CTEM
          end if
        else
          ALGWET(k)=0.0
          ALGDRY(k)=0.0
        end if
C
200   continue
C
      do 300 j = 1, IG
        do 300 k = IL1, IL2
          m = JLMOS(k)
          if (ISAND(k, j) == -4) then
            THPOR(k, j) = 0.0
            THLRET(k, j) = 0.0
            THLMIN(k, j) = 0.0
            BI(k, j) = 0.0
            PSISAT(k, j) = 0.0
            GRKSAT(k, j) = 0.0
            THLRAT(k, j) = 0.0
            HCPS(k, j) = HCPICE
            TCS(k, j) = TCICE
            THFC(k, j) = 0.0
            PSIWLT(k, j) = 0.0
          else if (ISAND(k, j) == -3) then
            THPOR(k, j) = 0.0
            THLRET(k, j) = 0.0
            THLMIN(k, j) = 0.0
            BI(k, j) = 0.0
            PSISAT(k, j) = 0.0
            GRKSAT(k, j) = 0.0
            THLRAT(k, j) = 0.0
            HCPS(k, j) = HCPSND
            TCS(k, j) = TCSAND
            THFC(k, j) = 0.0
            PSIWLT(k, j) = 0.0
          else if (ISAND(k, j) == -2) then
            THPOR(k, j) = THPORG(max(1,min(nint(ORGM(k, j)), 3)))
            THLRET(k, j) = THRORG(max(1,min(nint(ORGM(k, j)), 3)))
            THLMIN(k, j) = THMORG(max(1,min(nint(ORGM(k, j)), 3)))
            BI(k, j) = BORG(max(1,min(nint(ORGM(k, j)), 3)))
            PSISAT(k, j) = PSISORG(max(1,min(nint(ORGM(k, j)), 3)))
            GRKSAT(k, j) = GRKSORG(max(1,min(nint(ORGM(k, j)), 3)))
            THLRAT(k, j) = 0.5**(1.0/(2.0*BI(k, j) + 3.0))
            HCPS(k, j) = HCPOM
            TCS(k, j) = TCOM
            THFC(k, j) = THLRET(k, j)
            PSIWLT(k, j) = PSISAT(k, j)*(THLMIN(k, j)/
     &                     THPOR(k, j))**(-BI(k, j))
          else if (SAND(k, j) >= 0.0) THEN
            if (SOILINIFLAG == 5) then
              THPOR(k, j) = WC_THPOR(m, j)
              THLRET(k, j) = WC_THLRET(m, j)
              THLMIN(k, j) = WC_THLMIN(m, j)
              BI(k, j) = WC_BI(m, j)
              PSISAT(k, j) = WC_PSISAT(m, j)
              GRKSAT(k, j) = WC_GRKSAT(m, j)
              HCPS(k, j) = WC_HCPS(m, j)
              TCS(k, j) = WC_TCS(m, j)
            else
              THPOR(k, j) = (-0.126*SAND(k, j) + 48.9)/100.0
              THLRET(k, j) = 0.04
              THLMIN(k, j) = 0.04
              BI(k, j) = 0.159*CLAY(k, j) + 2.91
              PSISAT(k, j) = 0.01*exp(-0.0302*SAND(k, j) + 4.33)
              GRKSAT(k, j) = 7.0556E-6*(exp(0.0352*SAND(k, j) - 2.035))
              VSAND = SAND(k, j)/(RHOSOL*100.0)
              VORG = ORGM(k, j)/(RHOOM*100.0)
              VFINE = (100.0 - SAND(k, j) - ORGM(k, j))/(RHOSOL*100.0)
              VTOT = VSAND + VFINE + VORG
              THSAND = (1.0 - THPOR(k, j))*VSAND/VTOT
              THORG = (1.0 - THPOR(k, j))*VORG/VTOT
              THFINE = 1.0 - THPOR(k, j) - THSAND - THORG
              HCPS(k, j) = (HCPSND*THSAND + HCPFIN*THFINE +
     &                      HCPOM*THORG)/(1.0 - THPOR(k, j))
              TCS(k, j) = (TCSAND*THSAND + TCOM*THORG +
     &                     TCFINE*THFINE)/(1.0 - THPOR(k, j))
            end if
            THLRAT(k, j) = 0.5**(1.0/(2.0*BI(k, j) + 3.0))
            if (j /= IGDR(k)) then
              THFC(k, j) = THPOR(k, j)*(1.157E-9/GRKSAT(k, j))**
     &                     (1.0/(2.0*BI(k, j) + 3.0))
            else
              AEXP = (BI(k, j) - 1.0)/BI(k, j)
              ABC = (3.0*BI(k, j) + 2.0)**AEXP -
     &              (2.0*BI(k, j) + 2.0)**AEXP
              THFC(k, j) = (ABC*THPOR(k, j)/(BI(k, j) - 1.0))*
     &                     (PSISAT(k, j)*BI(k, j)/SDEPTH(k))**
     &                     (1.0/BI(k, j))
            end if
            PSIWLT(k, j) = PSISAT(k, j)*(max(0.5*THFC(k, j),
     &                     THLMIN(k, j))/THPOR(k, j))**(-BI(k, j))
          end if
C
300   continue
C
      return
C
      end
