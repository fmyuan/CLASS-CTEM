      SUBROUTINE SPWCON7 (FVORT,PI) 
C 
C     * DEC 04/90 - M.LAZARE. - T2S NOW -40C INSTEAD OF -15C. 
C     * JAN 30/89 - M.LAZARE. - INCLUDE PARAMETERS USED IN ICE-PHASE AND MOISTURE CONSERVATION CALCULATIONS. 
C     * MAR 15/88 - R.LAPRISE.  ORIGINAL VERSION SPWCONH FOR GCM3H. 
  
C     * SETS CONSTANTS FOR THE GCM7.
C     * ILEVM = JUST ILEV-1.
C     *  LEVS = THE NUMBER OF PROGNOSTIC MOISTURE LEVELS. 
C     *    SH = SIGMA VALUE AT MID POINT OF THERMO. LAYERS. 
C     *   SHB = SIGMA VALUE AT BASE OF THERMO. LAYERS.
C     *    WW = EARTH ROTATION RATE (1/SEC).
C     *     A = EARTH RADIUS (M). 
C     *  GRAV = GRAVITY ACCELERATION (M/SEC**2).
C     *  RGAS = DRY AIR GAS CONSTANT (JOULE/(KG*DEG)).
C     * RGOCP = RGAS/(DRY AIR SPECIFIC HEAT)
C     * FVORT = VORTICITY OF EARTH ROTATION.
C 
C     * PARAMETERS USED IN THERMODYNAMICS AND DYNAMICS. 
      COMMON /PARAMS/ WW,TW,RAYON,ASQ,GRAV,RGAS,RGOCP,RGOASQ,CPRES
      COMMON /PARAMS/ RGASV,CPRESV
C 
C     * PARAMETERS USED BY  FUNCTION  HTVOCP. 
      COMMON /HTCP/   T1S,T2S,AI,BI,AW,BW,SLP 
C 
C     * PARAMETERS USED BY  FUNCTIONS  DEWPNT,  SPCHUM,  DELTAQ.
      COMMON /EPS/    A,B,EPS1,EPS2 
C 
C     * PARAMETERS USED BY  FUNCTION  GAMSAT. 
      COMMON /GAMS/   EPSS,CAPA 
C 
C     * PARAMETERS USED IN MOIST CONVECTIVE ADJUSTMENT. 
      COMMON /ADJPCP/ HC,HF,HM,AA,DEPTH,LHEAT,MOIADJ,MOIFLX 
C 
C     * PARAMETERS USED IN ICE PHASE PHYSICS CALCULATIONS.
      COMMON /EPSICE/ AICE,BICE,TICE,QMIN 
C-----------------------------------------------------------------------
      WW     =7.292E-5
      TW     =WW+WW 
      RAYON  =6.37122E06
      ASQ    =RAYON*RAYON 
      GRAV   =9.80616 
      RGAS   =287.04
      RGOCP  =2./7. 
      RGOASQ =RGAS/ASQ
      CPRES  =RGAS/RGOCP
      CPRESV =1870. 
      RGASV  =461.50
      PI     =3.1415926535897 
      FVORT  =TW*SQRT(2./3.)
C 
      RAUW   =1.E+3 
      GRAV   =9.80616 
      DEPTH  = 1./(RAUW*GRAV) 
      CP     =1004.5
      CAPA   =RGOCP 
      T1S    =273.16
      T2S    =233.16
      AW     =3.15213E+6/CP 
      BW     =2.38E+3/CP
      AI     =2.88053E+6/CP 
      BI     =0.167E+3/CP 
      A      =21.656
      B      =5418. 
      AICE   =24.292
      BICE   =6141. 
      TICE   =233.16
      SLP    =1./(T1S-T2S)
      QMIN   =1.E-20
      EPS1   =0.622 
      EPS2   =0.378 
      EPSS   =EPS1
C 
      HC     =.95 
      HF     =.95 
      HM     =.95 
      LHEAT  =1 
      MOIADJ =1 
      MOIFLX =1 
      AA     =0.0 
      IF(HM.LT.1.)  AA=1./(6.*(1.-HM)**2) 
C 
      RETURN
C-----------------------------------------------------------------------
      END
