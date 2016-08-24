      SUBROUTINE DRCOEF (CDM,CDH,RIB,CFLUX,
     1                   ZOMIN,ZOHIN,CRIB,TVIRTG,TVIRTA,VA,FCT,ITER,
     2                   ZOM,ZOH,ILG,IL1,IL2)
C
C     * JUL 17/97 - D.VERSEGHY. BUG FIX - CONVERT ZOM AND ZOH TO
C     *                         INTERNAL WORK ARRAYS.
C     * MAR 10/97 - E. CHAN.    LOCAL VERSION FOR LAND SURFACE
C     *                         PROCESSES NODE: BYPASS ARTIFICIAL
C     *                         DAMPING OF TURBULENT FLUXES
C     *                         USING CDHMOD.
C     * MAY 21/96 - K. ABDELLA. MODIFICATION FOR FREE-CONVECTIVE
C     *                         LIMIT ON UNSTABLE SIDE ADDED.
C     * JAN 10/96 - K. ABDELLA. CORRECT ERROR IN AU1 (UNSTABLE
C     *                         SIDE) AND PUT IN PRANDTL NUMBER
C     *                         RANGE (0.74->1)/
C     *                         "CDHMOD" USED ON BOTH STABLE AND
C     *                         UNSTABLE SIDE, TO LIMIT FLUXES
C     *                         OVER LONG TIMESTEP.     
C     * M. LAZARE - FEB 14/95.  USE VARIABLES "ZOLN" AND "ZMLN"
C     *                         ON UNSTABLE SIDE, FOR OPTIMIZATION.
C     *                         THIS IS PREVIOUS VERSION "DRCOEFX".
C     * K. ABDELLA/M. LAZARE. - NOV 30/94.

C     * CALCULATES DRAG COEFFICIENTS AND RELATED VARIABLES FOR CLASS.

C     * OUTPUT FIELDS ARE:
C     *    CDM    : STABILITY-DEPENDENT DRAG COEFFICIENT FOR MOMENTUM.
C     *    CDH    : STABILITY-DEPENDENT DRAG COEFFICIENT FOR HEAT.
C     *    RIB    : BULK RICHARDSON NUMBER.
C     *    CFLUX  : CD * MOD(V), BOUNDED BY FREE-CONVECTIVE LIMIT.

C     * INPUT FIELDS ARE:
C     *    ZOM/ZOH: ROUGHNESS HEIGHTS FOR MOMENTUM/HEAT NORMALIZED BY
C     *             REFERENCE HEIGHT.
C     *    CRIB   : -RGAS*SLTHKEF/(VA**2), WHERE
C     *             SLTHKEF=-LOG(MAX(SGJ(ILEV),SHJ(ILEV)))
C     *    TVIRTG : "SURFACE" VIRTUAL TEMPERATURE.
C     *    TVIRTA : LOWEST LEVEL VIRTUAL TEMPERATURE.
C     *    VA     : AMPLITUDE OF LOWEST LEVEL WIND.
C     *    FCT    : FRACTION OF SURFACE TYPE BEING STUDIED.
C     *    ITER   : INDEX ARRAY INDICATING IF POINT IS UNDERGOING
C     *             FURTHER ITTERATION OR NOT. 

C     * OUTPUT ARRAYS.

      REAL CDM    (ILG),   CDH    (ILG),   RIB    (ILG),   CFLUX  (ILG) 

C     * INPUT ARRAYS.

      REAL ZOMIN  (ILG),   ZOHIN  (ILG),   CRIB   (ILG),   TVIRTG (ILG)
      REAL TVIRTA (ILG),   VA     (ILG),   FCT    (ILG)

      INTEGER ITER(ILG)
 
      REAL ZOM    (ILG),   ZOH    (ILG)

      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,SLTHICK,BEEM,ALFAH,              
     1                FAC,GAMRH,GAMRM,VMIN                                        
C-------------------------------------------------------------
      AA=9.5285714
      AA1=14.285714
      BETA=1.2
      PR = 1.
C      
      DO 100 I=IL1,IL2
        IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                         THEN
          RIB(I)=CRIB(I)*(TVIRTG(I)-TVIRTA(I))
          IF(RIB(I).GE.0.0) THEN
            ZLEV=-CRIB(I)*TVIRTA(I)*(VA(I)**2)/GRAV  
            ZS=MAX(10.,5.*MAX(ZOMIN(I)*ZLEV, ZOHIN(I)*ZLEV))
            ZS=ZLEV*(1.+RIB(I))/(1.+(ZLEV/ZS)*RIB(I))
            ZOM(I)=ZOMIN(I)*ZLEV/ZS
            ZOH(I)=ZOHIN(I)*ZLEV/ZS
            RIB(I)=RIB(I)*ZS/ZLEV
          ELSE
            ZOM(I)=ZOMIN(I)
            ZOH(I)=ZOHIN(I)
          ENDIF
          ZOLN=LOG(ZOH(I))
          ZMLN=LOG(ZOM(I))
          IF(RIB(I).LT.0.0) THEN
            CPR=MAX(ZOLN/ZMLN,0.74)
            CPR=MIN(CPR,1.0)
            ZI=1000.0
            OLSF=BETA**3*ZI*VKC**2/ZMLN**3
            OLFACT=1.7*(LOG(1.+ZOM(I)/ZOH(I)))**0.5+0.9
            OLSF=OLSF*OLFACT
            ZL = -CRIB(I)*TVIRTA(I)*(VA(I)**2)/GRAV
            ZMOL=ZOM(I)*ZL/OLSF
            ZHOL=ZOH(I)*ZL/OLSF
            XM=(1.00-15.0*ZMOL)**(0.250)
            XH=(1.00-9.0*ZHOL)**0.25
            BH1=-LOG(-2.41*ZMOL)+LOG(((1.+XM)/2.)**2*(1.+XM**2)/2.)
            BH1=BH1-2.*ATAN(XM)+ATAN(1.)*2.
            BH1=BH1**1.5
            BH2=-LOG(-0.25*ZHOL)+2.*LOG(((1.00+XH**2)/2.00))
            BH=VKC**3.*BETA**1.5/(BH1*(BH2)**1.5)
            WB=SQRT(GRAV*(TVIRTG(I)-TVIRTA(I))*ZI/TVIRTG(I))
            WSTAR=BH**(0.333333)*WB
            RIB0=RIB(I)

            WSPEED=SQRT(VA(I)**2+(BETA*WSTAR)**2)
            RIB(I)=RIB0*VA(I)**2/WSPEED**2
            AU1=1.+5.0*(ZOLN-ZMLN)*RIB(I)*(ZOH(I)/ZOM(I))**0.25
            OLS=-RIB(I)*ZOLN**2/(CPR*ZMLN)*(1.0+AU1/
     1           (1.0-RIB(I)/(ZOM(I)*ZOH(I))**0.25))
            PSIM1=LOG(((1.00+(1.00-15.0*OLS)**0.250)/2.00)**2*
     1            (1.0+(1.00-15.0*OLS)**0.5)/2.0)-2.0*ATAN(
     2            (1.00-15.0*OLS)**0.250)+ATAN(1.00)*2.00
            PSIM0=LOG(((1.00+(1.00-15.0*OLS*ZOM(I))**0.250)/2.00)**2
     1            *(1.0+(1.00-15.0*OLS*ZOM(I))**0.5)/2.0)-2.0*
     2            ATAN((1.00-15.0*OLS*ZOM(I))**0.250)+ATAN(1.00)*2.0
            PSIH1=LOG(((1.00+(1.00-9.0*OLS)**0.50)/2.00)**2)
            PSIH0=LOG(((1.00+(1.00-9.0*OLS*ZOH(I))**0.50)/2.00)**2)

            USTAR=VKC/(-ZMLN-PSIM1+PSIM0)
            TSTAR=VKC/(-ZOLN-PSIH1+PSIH0)
            CDH(I)=USTAR*TSTAR/PR
            WTS=CDH(I)*WSPEED*(TVIRTG(I)-TVIRTA(I))
            if (WTS .le. -1.0) WTS = 0.1                 !HSuo Apr 2013
            WSTAR=(GRAV*ZI/TVIRTG(I)*WTS)**(0.333333)

            WSPEED=SQRT(VA(I)**2+(BETA*WSTAR)**2)
            RIB(I)=RIB0*VA(I)**2/WSPEED**2
            AU1=1.+5.0*(ZOLN-ZMLN)*RIB(I)*(ZOH(I)/ZOM(I))**0.25
            OLS=-RIB(I)*ZOLN**2/(CPR*ZMLN)*(1.0+AU1/
     1           (1.0-RIB(I)/(ZOM(I)*ZOH(I))**0.25))
            PSIM1=LOG(((1.00+(1.00-15.0*OLS)**0.250)/2.00)**2*
     1            (1.0+(1.00-15.0*OLS)**0.5)/2.0)-2.0*ATAN(
     2            (1.00-15.0*OLS)**0.250)+ATAN(1.00)*2.00
            PSIM0=LOG(((1.00+(1.00-15.0*OLS*ZOM(I))**0.250)/2.00)**2
     1            *(1.0+(1.00-15.0*OLS*ZOM(I))**0.5)/2.0)-2.0*
     2            ATAN((1.00-15.0*OLS*ZOM(I))**0.250)+ATAN(1.00)*2.0
            PSIH1=LOG(((1.00+(1.00-9.0*OLS)**0.50)/2.00)**2)
            PSIH0=LOG(((1.00+(1.00-9.0*OLS*ZOH(I))**0.50)/2.00)**2)
 
          ELSE

            WSPEED=VA(I)
            AS1=10.0*ZMLN*(ZOM(I)-1.0)
                AS2=5.00/(2.0-8.53*RIB(I)*EXP(-3.35*RIB(I))+0.05*RIB(I)**2)
            AS2=AS2*PR*SQRT(-ZMLN)/2.
            AS3=27./(8.*PR*PR)

            OLS=RIB(I)*(ZMLN**2+AS3*AS1*(RIB(I)**2+AS2*RIB(I)))
     1          /(AS1*RIB(I)-PR*ZOLN)
            PSIM1=-0.667*(OLS-AA1)*EXP(-0.35*OLS)-AA-OLS
            PSIM0=-0.667*(OLS*ZOM(I)-AA1)*EXP(-0.35*OLS*ZOM(I))
     1            -AA-OLS*ZOM(I)
            PSIH1=-(1.0+2.0*OLS/3.0)**1.5-0.667*(OLS-AA1)
     1            *EXP(-0.35*OLS)-AA+1.0
            PSIH0=-(1.0+2.0*OLS*ZOH(I)/3.0)**1.5-0.667*(OLS*ZOH(I)-AA1)
     1            *EXP(-0.35*OLS*ZOH(I))-AA+1.0

          ENDIF  
      
          USTAR=VKC/(-ZMLN-PSIM1+PSIM0)
          TSTAR=VKC/(-ZOLN-PSIH1+PSIH0)

          CDM(I)=USTAR**2.0
          CDH(I)=USTAR*TSTAR/PR    
          
          ZTIK=-CRIB(I)*TVIRTA(I)*(WSPEED**2)/GRAV
          CDFAC=2.0*DELT*ABS(WSPEED/ZTIK)

CEC       CDHMOD=(1.0-EXP(-CDFAC*CDH(I)))/CDFAC
CEC       CDMMOD=(1.0-EXP(-CDFAC*CDM(I)))/CDFAC
C
C         * USE IMPLICIT FORMULATION FOR HEAT UNDER STABLE CONDITIONS,
C         * TO CONTROL "OVERSHOOT". ALSO USED ON UNSTABLE SIDE TO
C         * LIMIT EXCESSIVE FLUXES OVER LONG TIMESTEP.
C 
CEC       IF(CDHMOD.GT.0.) CDH(I)=CDHMOD
C
C         * CALCULATE CD*MOD(V) UNDER FREE-CONVECTIVE LIMIT.
C
          IF(TVIRTG(I).GT.TVIRTA(I))    THEN
            CLIMIT=1.9E-3*(TVIRTG(I)-TVIRTA(I))**0.333333 
          ELSE
            CLIMIT=0.          
          ENDIF             
          CFLUX(I)=MAX(CDH(I)*WSPEED,CLIMIT)
        ENDIF
 100  CONTINUE  
C-------------------------------------------------------------
      RETURN
      END                                                        
