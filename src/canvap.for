      SUBROUTINE CANVAP(THLIQ,EVAP,SUBL,RAICAN,SNOCAN,TCAN,ZSNOW,
     1                  WLOST,CHCAP,QFCF,QFCL,QFN,QFC,HTCC,HTCS,HTC,
     2                  TBAR,CMASS,FROOT,TSNOW,RHOSNO,HCPSNO,THPOR,
     3                  FCT,DELZW,IG,ILG,IL1,IL2,JL,
     4                  EVLOST,RLOST,IROOT,N,TRANS  )   !HSuo Feb 2013, output THTRAN
C
C     * JUL 07/09 - H.SUO.      ADD TRAN FOR TRANSPIRATION                                                                            
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE SOIL 
C     *                         PERMEABLE DEPTH.
C     * DEC 30/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUGFIXES IN CALCULATION OF QFN AND 
C     *                         QFC.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE CALCULATION OF WLOST;
C     *                         REFINE CALCULATION OF QFCL.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3. 
C     *                         ADDITIONAL DIAGNOSTIC CALCULATIONS -
C     *                         HTCC AND HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C                                        NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATE ACTUAL EVAPORATION, 
C     *                         SUBLIMATION AND TRANSPIRATION FROM
C     *                         VEGETATION CANOPY.
C                                                                
C     * INPUT/OUTPUT ARRAYS.
C
      REAL THLIQ (ILG,IG), TBAR  (ILG,IG), QFC   (ILG,IG),
     1     HTC   (ILG,IG)
C
      REAL EVAP  (ILG),    SUBL  (ILG),    RAICAN(ILG),    SNOCAN(ILG),
     1     TCAN  (ILG),    ZSNOW (ILG),    WLOST (ILG),    CHCAP (ILG),    
     2     HTCC  (ILG),    HTCS  (ILG),    QFCF  (ILG),    QFCL  (ILG),
     3     QFN   (ILG),    TSNOW (ILG),    HCPSNO(ILG),    TRANS (ILG)   !HSuo Feb 2013
C
C     * INPUT ARRAYS.
C
      REAL FROOT (ILG,IG), THPOR(ILG,IG),  DELZW (ILG,IG)
C
      REAL RHOSNO(ILG),    CMASS (ILG),    FCT   (ILG)
C
C     * WORK ARRAYS.
C
      REAL EVLOST(ILG),    RLOST (ILG)
C
      INTEGER              IROOT (ILG)
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * INITIALIZE ARRAYS.
C
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              RLOST (I)=0.0
              EVLOST(I)=0.0 
              IROOT (I)=0
              HTCC  (I)=HTCC(I)-FCT(I)*TCAN(I)*CHCAP(I)/DELT
              HTCS(I)=HTCS(I)-FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              HTC (I,1)=HTC(I,1)-FCT(I)*(TBAR(I,1)+TFREZ)*THLIQ(I,1)*
     1            HCPW*DELZW(I,1)/DELT
              HTC (I,2)=HTC(I,2)-FCT(I)*(TBAR(I,2)+TFREZ)*THLIQ(I,2)*
     1            HCPW*DELZW(I,2)/DELT
              HTC (I,3)=HTC(I,3)-FCT(I)*(TBAR(I,3)+TFREZ)*THLIQ(I,3)*
     1            HCPW*DELZW(I,3)/DELT
          ENDIF
  100 CONTINUE
C
C     * SUBLIMATION CASE.  IF SNOW ON CANOPY IS INSUFFICIENT TO SUPPLY
C     * DEMAND, RESIDUAL IS TAKEN FIRST FROM SNOW UNDERLYING CANOPY AND
C     * THEN FROM LIQUID WATER ON CANOPY.
C
      DO 200 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. SUBL(I).GT.0.)                      THEN 
              SLOST=SUBL(I)*DELT*RHOW                                                    
              IF(SLOST.LE.SNOCAN(I))                          THEN  
                  SNOCAN(I)=SNOCAN(I)-SLOST                                                 
                  SUBL(I)=0.0                                                            
              ELSE                                                                    
                  SLOST=SLOST-SNOCAN(I)                                                  
                  QFCF(I)=QFCF(I)-FCT(I)*SLOST/DELT
                  SNOCAN(I)=0.0                                                          
                  IF(SLOST.LE.ZSNOW(I)*RHOSNO(I))           THEN                                      
                      ZSNOW(I)=ZSNOW(I)-SLOST/RHOSNO(I)                                        
                      SUBL(I)=0.0                                                        
                      QFN(I)=QFN(I)+FCT(I)*SLOST/DELT
                  ELSE                                                                
                      SLOST=SLOST-ZSNOW(I)*RHOSNO(I)                                        
                      QFN(I)=QFN(I)+FCT(I)*ZSNOW(I)*RHOSNO(I)/DELT
                      ZSNOW(I)=0.0                                                       
                      WLOST(I)=WLOST(I)-SLOST*CLHMLT/CLHVAP                                     
                      EVAP(I)=EVAP(I)+SLOST*(CLHMLT+CLHVAP)/
     1                        (CLHVAP*DELT*RHOW)              
                      QFCL(I)=QFCL(I)+FCT(I)*SLOST*(CLHMLT+CLHVAP)/
     1                        (CLHVAP*DELT)
                  ENDIF                                                               
              ENDIF                                                                   
          ENDIF
  200 CONTINUE
C
C     * EVAPORATION.  IF WATER ON CANOPY IS INSUFFICIENT TO SUPPLY
C     * DEMAND, ASSIGN RESIDUAL TO TRANSPIRATION.
C     * (THE WORK ARRAY "IROOT" INDICATES SOIL LAYERS WHERE ROOTS 
C     * EXIST.)
C
      DO 300 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. EVAP(I).GT.0.)                      THEN
              RLOST(I)=EVAP(I)*RHOW*DELT
              IF(RLOST(I).LE.RAICAN(I))                         THEN 
                  RAICAN(I)=RAICAN(I)-RLOST(I)
                  RLOST (I)=0.
              ELSE                                                                    
                  RLOST(I)=RLOST(I)-RAICAN(I)                                                  
                  QFCL(I)=QFCL(I)-FCT(I)*RLOST(I)/DELT
                  IF(MAX(FROOT(I,1),FROOT(I,2),FROOT(I,3)).GT.0.0)
     1                                                       THEN
                      IROOT(I)=1
                  ELSE
                      EVLOST(I)=RLOST(I)
                  ENDIF           
                  RAICAN(I)=0.
              ENDIF
          ENDIF
  300 CONTINUE
C
C     * TRANSPIRATION.
C
      DO 400 J=1,IG
      DO 401 I=IL1,IL2 
          IF(FCT(I).GT.0. .AND. IROOT(I).GT.0)                     THEN
              IF(DELZW(I,J).GT.0.0) THEN
                  THTRAN=RLOST(I)*FROOT(I,J)/(RHOW*DELZW(I,J))                      
              ELSE
                  THTRAN=0.0
              ENDIF
              IF(THPOR(I,J).LT.THLMIN)                THEN
                  THLLIM=THPOR(I,J)
              ELSE
                  THLLIM=THLMIN
              ENDIF
              IF(THTRAN.LE.(THLIQ(I,J)-THLLIM))                 THEN                        
                  QFC  (I,J)=QFC(I,J)+FCT(I)*RLOST(I)*FROOT(I,J)/DELT
                  THLIQ(I,J)=THLIQ(I,J)-THTRAN                                
              ELSE                                                        
                  QFC  (I,J)=QFC(I,J)+FCT(I)*(THLIQ(I,J)-THLLIM)*RHOW*
     1                       DELZW(I,J)/DELT
                  EVLOST (I)=EVLOST(I)+(THTRAN+THLLIM-THLIQ(I,J))*RHOW*            
     1                       DELZW(I,J)   
                  THLIQ(I,J)=THLLIM
              ENDIF                                                       
          ENDIF
  401 CONTINUE
  400 CONTINUE                                                        
C
C     * CLEANUP.
C
      DO 500 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              CHCAP(I)=RAICAN(I)*SPHW+SNOCAN(I)*SPHICE+CMASS(I)*SPHVEG                                
              WLOST(I)=WLOST(I)+EVLOST(I)  
              TRANS(I)=QFC(I,1)+QFC(I,2)+QFC(I,3)   !HSuo Feb 2013, transpiration
              HTCC  (I)=HTCC(I)+FCT(I)*TCAN(I)*CHCAP(I)/DELT
              HTCS(I)=HTCS(I)+FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              HTC (I,1)=HTC(I,1)+FCT(I)*(TBAR(I,1)+TFREZ)*THLIQ(I,1)*
     1            HCPW*DELZW(I,1)/DELT
              HTC (I,2)=HTC(I,2)+FCT(I)*(TBAR(I,2)+TFREZ)*THLIQ(I,2)*
     1            HCPW*DELZW(I,2)/DELT
              HTC (I,3)=HTC(I,3)+FCT(I)*(TBAR(I,3)+TFREZ)*THLIQ(I,3)*
     1            HCPW*DELZW(I,3)/DELT
          ENDIF
  500 CONTINUE
C                                                                        
      RETURN                                                                      
      END                                                                                 
