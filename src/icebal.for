      SUBROUTINE ICEBAL(TBAR,TPOND,ZPOND,RUNOFF,OVRFLW,QMELT,TSNOW,
     1                  RHOSNO,ZSNOW,HCPSNO,HMFG,HTCS,HTC,WTRS,WTRG,
     2                  HCP,FCT,EVAP,R,TR,GZERO,G12,G23,
     3                  ISAND,DELZ,
     4                  IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                  ZMAT,TMOVE,WMOVE,ZRMDR,TADD,ZMOVE,TBOT,ICONT   )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY 
C     *                         BETWEEN SOIL LAYERS IN MAIN CODE.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. STEP AHEAD "SOIL" LAYER TEMPERATURES
C     *                         OVER CONTINENTAL ICE SHEETS; ASSIGN
C     *                         PONDED WATER TO RUNOFF; ADJUST LAYER
C     *                         DEPTHS FOR ACCUMULATION/ABLATION.
C
C     * INPUT/OUTPUT FIELDS.
C                                                                                 
      REAL TBAR  (ILG,IG), HMFG  (ILG,IG), HTC   (ILG,IG)
C
      REAL TPOND (ILG),    ZPOND (ILG),    RUNOFF(ILG),    OVRFLW(ILG),
     1     QMELT (ILG),    TSNOW (ILG),    RHOSNO(ILG),    ZSNOW (ILG),
     2     HCPSNO(ILG),    HTCS  (ILG),    WTRS  (ILG),    WTRG  (ILG)
C
C     * INPUT FIELDS.
C
      REAL HCP   (ILG,IG)
C
      REAL FCT   (ILG),    EVAP  (ILG),    R     (ILG),    TR    (ILG), 
     1     GZERO (ILG),    G12   (ILG),    G23   (ILG)    
C
      INTEGER              ISAND (ILG,IG)
C
      REAL DELZ  (IG)
C
C     * WORK FIELDS.
C
      REAL ZMAT  (ILG,IGP2,IGP1),          TMOVE (ILG,IGP2),
     1     WMOVE (ILG,IGP2),               ZRMDR (ILG,IGP1)
C
      REAL TADD  (ILG),    ZMOVE (ILG),    TBOT  (ILG) 
C
      INTEGER              ICONT (ILG)
C                                                                                  
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C
C     * STEP AHEAD LAYER TEMPERATURES AND ADD HEATING EFFECTS OF
C     * RAINFALL.
C    
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              TZERO=TBAR(I,1)+DELZ(1)*(GZERO(I)+0.5*G12(I))/(3.0*TCGLAC)                        
              TBOT(I)=TZERO-(G23(I)*(DELZ(3)+DELZ(2))+G12(I)*
     1                (DELZ(1)+DELZ(2))+GZERO(I)*DELZ(1))/(2.0*TCGLAC)
              TBAR(I,1 )=TBAR(I,1 )+(GZERO(I)-G12(I))*DELT/
     1                   (HCP(I,1) *DELZ(1))                       
              TBAR(I,2 )=TBAR(I,2 )+(G12  (I)-G23(I))*DELT/
     1                   (HCP(I,2) *DELZ(2))                         
              TBAR(I,IG)=TBAR(I,IG)+ G23(I)          *DELT/
     1                   (HCP(I,IG)*DELZ(IG))                           
C                                                                                  
              IF(R(I).GT.0.)                                  THEN 
                 RADD=R(I)*DELT                                                             
                 TPOND(I)=(TPOND(I)*ZPOND(I)+TR(I)*RADD)/(ZPOND(I)+RADD)                                
                 ZPOND(I)=ZPOND(I)+RADD                                                        
              ENDIF                                                                       
              IF(ZPOND(I).GT.0.)                              THEN  
                 HTC (I,1)=HTC(I,1)-FCT(I)*(TBAR(I,1)+TFREZ)*HCPICE*
     1                     DELZ(1)/DELT
                 TBAR(I,1)=(TBAR(I,1)*HCPICE*DELZ(1)+                   
     1                      TPOND(I)*HCPW*ZPOND(I))/(HCPICE*DELZ(1))
                 HTC (I,1)=HTC(I,1)+FCT(I)*(TBAR(I,1)+TFREZ)*HCPICE*
     1                     DELZ(1)/DELT
                 RUNOFF(I)=RUNOFF(I)+ZPOND(I)                                                     
                 OVRFLW(I)=OVRFLW(I)+FCT(I)*ZPOND(I)                                                     
                 ZPOND (I)=0.0                                                               
                 TPOND (I)=0.0
              ENDIF
          ENDIF
  100 CONTINUE
C
C     * IF LAYER TEMPERATURES OVERSHOOT ZERO, ADD EXCESS HEAT TO
C     * HEAT OF MELTING.
C
      DO 150 J=1,IG
      DO 150 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              IF(TBAR(I,J).GT.0.)                            THEN  
                  QADD=TBAR(I,J)*HCPICE*DELZ(J)/DELT  
                  QMELT(I)=QMELT(I)+QADD
                  HTC(I,J)=HTC(I,J)-FCT(I)*QADD
                  HTC(I,1)=HTC(I,1)+FCT(I)*QADD
                  TBAR(I,J)=0.0                                                       
              ENDIF
              HTC(I,J)=HTC(I,J)-FCT(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                 DELZ(J)/DELT
          ENDIF
  150 CONTINUE
C
C     * APPLY CALCULATED HEAT OF MELTING TO UPPER ICE LAYER; ADD MELTED
C     * WATER TO TOTAL RUNOFF; CALCULATE DEPTH OF ICE REMOVED BY MELTING
C     * AND SUBLIMATION; RECALCULATE ICE LAYER TEMPERATURES.
C     
      DO 200 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN                         
              IF(QMELT(I).GT.0. .OR. EVAP(I).GT.0.)           THEN                                        
                  TMOVE(I,1)=TBAR(I,2)                                                      
                  TMOVE(I,2)=TBAR(I,3)                                                      
                  TMOVE(I,3)=TBOT(I)                                                           
                  ZMELT=QMELT(I)*DELT/((0.0-TBAR(I,1))*HCPICE+
     1                  CLHMLT*RHOICE)                 
                  RUNOFF(I)=RUNOFF(I)+ZMELT*RHOICE/RHOW                                         
                  HMFG(I,1)=HMFG(I,1)+FCT(I)*CLHMLT*RHOICE*ZMELT/DELT
                  HTC (I,1)=HTC(I,1)-FCT(I)*(QMELT(I)-CLHMLT*RHOICE*
     1                     ZMELT/DELT)
                  ZMOVE (I)=ZMELT+EVAP(I)*DELT*RHOW/RHOICE
                  WTRG  (I)=WTRG(I)+FCT(I)*ZMOVE(I)*RHOICE/DELT
              ENDIF
          ENDIF                                                                       
  200 CONTINUE                                       
C
      DO 250 J=1,IG
      DO 250 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       (QMELT(I).GT.0. .OR. EVAP(I).GT.0.))                   THEN 
              TBAR(I,J)=(TBAR(I,J)*(DELZ(J)-ZMOVE(I))+TMOVE(I,J)*
     1                   ZMOVE(I))/DELZ(J)
          ENDIF                                                                       
  250 CONTINUE                                                                
C
C     * IF SNOW PACK EXCEEDS 100 KG M-2 OR SNOW DENSITY EXCEEDS 
C     * 900 KG M-3, CONVERT EXCESS TO ICE AND MOVE THE LOCATIONS
C     * OF THE ICE LAYERS ACCORDINGLY.
C     * IF SUBLIMATION (DEPOSITION) IS OCCURRING, MOVE THE ICE
C     * LAYERS AS WELL.
C
      DO 300 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              ICONT(I)=0
              HTCS(I)=HTCS(I)-FCT(I)*(TSNOW(I)+TFREZ)*HCPSNO(I)*
     1                ZSNOW(I)/DELT
              IF((RHOSNO(I)*ZSNOW(I)).GT.100.)                THEN                                        
                  WMOVE(I,1)=(RHOSNO(I)*ZSNOW(I)-100.)/RHOICE                                
                  TMOVE(I,1)=TSNOW(I)                                                      
                  WTRS(I)=WTRS(I)-FCT(I)*WMOVE(I,1)*RHOICE/DELT
                  ZSNOW(I)=ZSNOW(I)-WMOVE(I,1)                                                
                  RHOSNO(I)=100.0/ZSNOW(I)                                                  
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                  ICONT(I)=1
              ELSE IF(RHOSNO(I).GE.900.)                      THEN
                  WMOVE(I,1)=ZSNOW(I)*RHOSNO(I)/RHOICE                                        
                  TMOVE(I,1)=TSNOW(I)                                                      
                  WTRS(I)=WTRS(I)-FCT(I)*WMOVE(I,1)*RHOICE/DELT
                  ZSNOW(I)=0.0                                                           
                  RHOSNO(I)=0.0                                                          
                  HCPSNO(I)=0.0                                                          
                  TSNOW(I)=0.0
                  ICONT(I)=1
              ENDIF                     
              HTCS(I)=HTCS(I)+FCT(I)*(TSNOW(I)+TFREZ)*HCPSNO(I)*
     1                ZSNOW(I)/DELT
          ENDIF
  300 CONTINUE
C               
      DO 400 J=1,IG
      DO 400 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              ZRMDR(I,J)=DELZ(J)                                                    
          ENDIF                                                  
  400 CONTINUE                                                            
C               
      DO 450 J=1,IG
      DO 450 K=1,IG+1
      DO 450 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              ZMAT(I,K,J)=0.0 
          ENDIF
  450 CONTINUE
C
      DO 500 J=2,IG+1
      DO 500 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              WMOVE(I,J)=DELZ(J-1)                                                  
              TMOVE(I,J)=TBAR(I,J-1)                                                
          ENDIF
  500 CONTINUE
C
      DO 550 K=1,IG+1
      DO 550 J=1,IG
      DO 550 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              IF(ZRMDR(I,J).GT.0. .AND. WMOVE(I,K).GT.0.)      THEN                    
                  ZMAT(I,K,J)=WMOVE(I,K)                                          
                  IF(ZMAT(I,K,J).GE.ZRMDR(I,J))           THEN                              
                      ZMAT(I,K,J)=ZRMDR(I,J)                                      
                      WMOVE(I,K)=WMOVE(I,K)-ZRMDR(I,J)                              
                      ZRMDR(I,J)=0.0                                            
                  ELSE                                                        
                      ZRMDR(I,J)=ZRMDR(I,J)-ZMAT(I,K,J)                             
                      WMOVE(I,K)=0.0                                            
                  ENDIF                                                       
              ENDIF                                                           
          ENDIF
  550 CONTINUE
C
      DO 650 J=1,IG
          DO 600 I=IL1,IL2
              IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)               THEN
                  TADD(I)=0.
              ENDIF
  600     CONTINUE
C
          DO 620 K=1,IG+1
          DO 620 I=IL1,IL2
              IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1           ICONT(I).EQ.1)                                     THEN
                  TADD(I)=TADD(I)+TMOVE(I,K)*ZMAT(I,K,J)
              ENDIF                                    
  620     CONTINUE                                                            
C
          DO 640 I=IL1,IL2
              IF(FCT(I).GT.0. .AND. ISAND(I,1).EQ.-4)               THEN
                  TADD(I)=TADD(I)+TBAR(I,J)*ZRMDR(I,J)                                        
                  TBAR(I,J)=TADD(I)/DELZ(J)
                  HTC(I,J)=HTC(I,J)+FCT(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                     DELZ(J)/DELT
              ENDIF                                              
  640     CONTINUE
  650 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
