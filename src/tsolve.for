      SUBROUTINE TSOLVE(ISNOW,FCT,
     1                  TZERO,QZERO,GZERO,QMELT,CDH,CDM,QSWNET,QLWOUT,
     2                  QTRANS,QSENS,QEVAP,EVAP,RIB,
     3                  CRIB,CEVAP,TADP,CPHCH,TVIRTA,EPS,QLWIN,TA,QA,VA,
     4                  CDOH,CDOM,ZOH,ZOM,PADRY,RHOAIR,GCONST,GCOEFF,
     5                  PRESSG,TSTART,TRSNOW,PSIZRO,QSWINV,QSWINI,
     6                  ALVISG,ALNIRG,IWATER,IEVAP,ILW,
     7                  ILG,IL1,IL2,JL,  
     8                  TSTEP,TVIRTS,RHZERO,RESID,RESIDL,RESIDO,TZEROL,
     9                  TZEROO,TRTOP,A,B,CFLUX,ZOMWRK,ZOHWRK,ITER,NITER)
C
C     * JUL 24/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         REPLACE BISECTION METHOD IN SURFACE 
C     *                         TEMPERATURE ITERATION SCHEME WITH 
C     *                         SECANT METHOD FOR FIRST TEN ITERATIONS.
C     * JUN 20/97 - D.VERSEGHY. PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.  ALSO, PASS SWITCH "ILW"
C     *                         THROUGH SUBROUTINE CALL, SPECIFYING 
C     *                         WHETHER QLWIN REPRESENTS INCOMING
C     *                         (ILW=1) OR NET (ILW=2) LONGWAVE
C     *                         RADIATION ABOVE THE GROUND.
C     * NOV 30/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         NEW DRAG COEFFICIENT AND RELATED FIELDS,
C     *                         NOW DETERMINED IN ROUTINE "DRCOEF"
C     *                         "CFLUX" NOW WORK FIELD INSTEAD OF "CLIMIT".
C     * OCT 04/94 - D.VERSEGHY. CHANGE "CALL ABORT" TO "CALL XIT" TO
C     *                         ENABLE RUNNING ON PCS.
C     * JAN 24/94 - M.LAZARE.   UNFORMATTED I/O COMMENTED OUT IN LOOP 200.
C     * JUL 29/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REMOVE RE-DEFINITION OF QMELT NEAR END
C     *                         (SINCE DONE ELSEWHERE ALREADY) AND
C     *                         REDEFINE QSWNET FOR DIAGNOSTIC PURPOSES
C     *                         TO INCLUDE TRANSMISSION THROUGH 
C     *                         SNOWPACK.
C     * OCT 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.                  
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ITERATIVE SURFACE TEMPERATURE 
C     *                         CALCULATIONS FOR SNOW/SOIL.
C
C     * OUTPUT ARRAYS.
C
      REAL TZERO (ILG),    QZERO (ILG),    GZERO (ILG),    QMELT (ILG),
     1     CDH   (ILG),    CDM   (ILG),    QSWNET(ILG),    QLWOUT(ILG),
     2     QTRANS(ILG),    QSENS (ILG),    QEVAP (ILG),    EVAP  (ILG),
     3     RIB   (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FCT   (ILG),    CRIB  (ILG),    CEVAP (ILG),    TADP  (ILG),
     1     CPHCH (ILG),    TVIRTA(ILG),    EPS   (ILG),
     2     QLWIN (ILG),    TA    (ILG),    QA    (ILG),    VA    (ILG),
     3     CDOH  (ILG),    CDOM  (ILG),    ZOH   (ILG),    ZOM   (ILG),
     4     PADRY (ILG),    RHOAIR(ILG),    GCONST(ILG),    GCOEFF(ILG),
     5     PRESSG(ILG),    TSTART(ILG),    TRSNOW(ILG),    PSIZRO(ILG),
     6     QSWINV(ILG),    QSWINI(ILG),    ALVISG(ILG),    ALNIRG(ILG)  

C
      INTEGER              IWATER(ILG),    IEVAP (ILG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL TSTEP (ILG),    TVIRTS(ILG),    RHZERO(ILG),    
     1     RESID (ILG),    RESIDL(ILG),    RESIDO(ILG),    TZEROL(ILG),    
     2     TZEROO(ILG),    TRTOP (ILG),    A     (ILG),    B     (ILG),
     3     CFLUX (ILG),    ZOMWRK(ILG),    ZOHWRK(ILG)
C
      INTEGER              ITER  (ILG),    NITER (ILG)
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,SLTHICK,BEEM,ALFAH,              
     1                FAC,GAMRH,GAMRM,VMIN                                        
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * INITIALIZATION AND PRE-ITERATION SEQUENCE.
C
      DO 50 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              IF(ISNOW.EQ.0)                      THEN
                  TRTOP(I)=0.
              ELSE
                  TRTOP(I)=TRSNOW(I)
              ENDIF
              QSWNV=QSWINV(I)*(1.0-ALVISG(I))                                                   
              QSWNI=QSWINI(I)*(1.0-ALNIRG(I))                                                   
              QSWNET(I)=QSWNV+QSWNI                                                          
              QTRANS(I)=QSWNET(I)*TRTOP(I)   
              QSWNET(I)=QSWNET(I)-QTRANS(I) 
              QMELT(I)=0.0                                                                   
              TZERO(I)=TSTART(I)                                                                
              TSTEP(I)=5.0
              IF(IWATER(I).GT.0 .OR. ISNOW.EQ.1)                    THEN
                  RHZERO(I)=1.0                                                          
              ELSE                                                                    
                  RHZERO(I)=0.0                                                      
              ENDIF                                                               
              RESID(I)=999999.
              RESIDL(I)=999999.
              TZEROL(I)=999999.                                                               
              ITER(I)=1
              NITER(I)=1
          ENDIF
   50 CONTINUE
C
C     * ITERATION SECTION.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS 
C     * ON THE CURRENT LATITUDE CIRCLE(S). 
C  
  100 CONTINUE
C
      NUMIT=0
      NIT=0
      DO 150 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(IEVAP(I).GT.0.AND.IWATER(I).LE.0.AND.ISNOW.NE.1) THEN
                  RHZERO(I)=EXP(CEVAP(I)/TZERO(I))                                         
              ENDIF
              IF(TZERO(I).GE.TFREZ)                        THEN
                  A(I)=17.269                                                                
                  B(I)=35.86                                                                 
              ELSE                                                                        
                  A(I)=21.874                                                                
                  B(I)=7.66                                                                  
              ENDIF                                                                       
            IF (TZERO(I) .le. 200.0) TZERO(I) = 230.0   !HSuo Feb2013
              WZERO=0.622*RHZERO(I)*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/
     1              (TZERO(I)-B(I)))/PADRY(I)           
              QZERO(I)=WZERO/(1.0+WZERO)                                                 
              TVIRTS(I)=TZERO(I)*(1.0+0.61*QZERO(I))
              NIT=NIT+1
          ENDIF
  150 CONTINUE      
C
      IF(NIT.GT.0)                                                  THEN
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C     * OTHER RELATED QUANTITIES.
C
          CALL DRCOEF (CDM,CDH,RIB,CFLUX,
     1                 ZOM,ZOH,CRIB,TVIRTS,TVIRTA,VA,FCT,ITER,
     2                 ZOMWRK,ZOHWRK,ILG,IL1,IL2)
C
C     * REMAINING CALCULATIONS.
C
        DO 175 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              QSENS(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TZERO(I)-TA(I))
              EVAP(I)=RHOAIR(I)*CFLUX(I)*(QZERO(I)-QA(I)) 
              IF(EVAP(I).LT.0. .AND. TZERO(I).GE.TADP(I)) EVAP(I)=0.0
              QEVAP(I)=CPHCH(I)*EVAP(I)                                                        
              IF(ILW.EQ.2) THEN
                  QLWOUT(I)=0.0
              ELSE
                  QLWOUT(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              ENDIF
              RESID(I)=QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1                 GZERO(I)                             
              IF(ABS(RESID(I)).LT.1.0)                       ITER(I)=0
              IF(ABS(TSTEP(I)).LT. 1.0E-4 .AND. ABS(RESID(I)).LT.
     1          (ABS(RESIDL(I))-0.01))                       ITER(I)=0
              IF(FCT(I).GT.0. .AND. NITER(I).EQ.50)          ITER(I)=-1
          ENDIF
C
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(NITER(I).EQ.1) THEN
                  TZEROO(I)=TZERO(I)
                  RESIDO(I)=RESID(I)
                  IF(RESID(I).GT.0.0) THEN
                      TZERO(I)=TZERO(I)+5.0
                  ELSE
                      TZERO(I)=TZERO(I)-5.0
                  ENDIF
              ELSEIF(NITER(I).LE.10) THEN
                  TZEROL(I)=TZERO(I)
                  RESIDL(I)=RESID(I)
                  TZERO(I)=(TZEROL(I)*RESIDO(I)-TZEROO(I)*RESIDL(I))/
     1               (RESIDO(I)-RESIDL(I)+0.00001)  !HSuo Mar2013
                  IF(TZERO(I).LT.0.0) TZERO(I)=TZEROL(I)-1.0
                  IF(TZERO(I).GT.373.16) TZERO(I)=TZEROL(I)+1.0
                  TSTEP(I)=TZERO(I)-TZEROL(I)
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))    THEN 
                      TSTEP(I)=-TSTEP(I)/2.0                                                    
                  ENDIF
                  TZERO(I)=TZERO(I)+TSTEP(I)
                  RESIDL(I)=RESID(I)
              ENDIF
          ENDIF
C
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  175   CONTINUE
      ENDIF
C
      IBAD=0
      DO 200 I=IL1,IL2
C         IF(FCT(I).GT.0. .AND. ITER(I).EQ.-1)                      THEN 
C             WRITE(6,6250) I,JL,RESID(I),TZERO(I),RIB(I)
C6250         FORMAT('0GROUND ITERATION LIMIT',3X,2I3,2(F8.2,E12.4))            
C         ENDIF                              
          IF(FCT(I).GT.0. .AND. (TZERO(I).LT.0..OR.TZERO(I).GT.373.16))
     1                                                              THEN              
              IBAD=I
          ENDIF  
  200 CONTINUE
C
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6275) IBAD,JL,TZERO(IBAD),NITER(IBAD),ISNOW
 6275     FORMAT('0BAD ITERATION TEMPERATURE',3X,2I3,F16.2,2I4)
          WRITE(6,6280) QSWNET(IBAD),QLWIN(IBAD),QSENS(IBAD),
     1        QEVAP(IBAD),GZERO(IBAD),CFLUX(IBAD)
 6280     FORMAT(2X,8F12.4)
          CALL XIT('TSOLVE',-1)
      ENDIF 
C
      IF(NUMIT.GT.0)                                    GO TO 100
C
C     * POST-ITERATION CLEAN-UP. 
C
      NIT=0
      DO 300 I=IL1,IL2
          IF(((IWATER(I).EQ.1 .AND. TZERO(I).LT.TFREZ) .OR. 
     1        (IWATER(I).EQ.2 .AND. TZERO(I).GT.TFREZ)) .AND. 
     2        FCT(I).GT.0.)                                         THEN
              TZERO(I)=TFREZ                                                             
              WZERO=0.622*611.0/PADRY(I)
              QZERO(I)=WZERO/(1.0+WZERO)                                                 
              TVIRTS(I)=TZERO(I)*(1.0+0.61*QZERO(I))
              ITER(I)=1
              NIT=NIT+1 
          ELSE
              ITER(I)=0
          ENDIF  
  300 CONTINUE
C
      IF(NIT.GT.0)                                                  THEN 
C
C       * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C       * OTHER RELATED QUANTITIES.
C
        CALL DRCOEF (CDM,CDH,RIB,CFLUX,
     1               ZOM,ZOH,CRIB,TVIRTS,TVIRTA,VA,FCT,ITER,
     2               ZOMWRK,ZOHWRK,ILG,IL1,IL2)
      ENDIF
C
C     * REMAINING CALCULATIONS.
C
      DO 350 I=IL1,IL2 
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              QSENS(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TZERO(I)-TA(I))
              EVAP(I)=RHOAIR(I)*CFLUX(I)*(QZERO(I)-QA(I)) 
              IF(EVAP(I).LT.0. .AND. TZERO(I).GE.TADP(I)) EVAP(I)=0.0
              QEVAP(I)=CPHCH(I)*EVAP(I)                                                        
              IF(ILW.EQ.2) THEN
                  QLWOUT(I)=0.0
              ELSE
                  QLWOUT(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              ENDIF
              QMELT(I)=QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1                 GZERO(I)                             
              RESID(I)=0.0
          ENDIF                                                                       
          IF(FCT(I).GT.0.)                                 THEN
              GZERO(I)=GZERO(I)+RESID(I)
              QSWNET(I)=QSWNET(I)+QTRANS(I)
              EVAP(I)=EVAP(I)/RHOW
          ENDIF
  350 CONTINUE
C
C     DO 400 I=IL1,IL2
C         IF(FCT(I).GT.0.)                                          THEN
C             WRITE(6,60) I,JL,TZERO(I),QSWNET(I),QLWOUT(I),QSENS(I),
C    1                    QEVAP(I),GZERO(I),QMELT(I)                
C60           FORMAT('0',I4,I4,F8.2,6F9.3)
C          ENDIF
C 400 CONTINUE
C
      RETURN                                                                      
      END  
