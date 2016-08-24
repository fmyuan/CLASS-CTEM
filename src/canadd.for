      SUBROUTINE CANADD(R,TR,S,TS,RAICAN,SNOCAN,TCAN,CHCAP,HTCC,
     1                  RDRIP,SDRIP,FSVF,CWCAP,CMASS,FCT,
     2                  ILG,IL1,IL2,JL)
C                                                                                 
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATE CANOPY INTERCEPTION; ADD
C     *                         THROUGHFALL AND CANOPY DRIP TO
C     *                         PRECIPITATION REACHING GROUND.
C     *                         ADJUST CANOPY TEMPERATURE AND HEAT
C     *                         CAPACITY.
C                                                                
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG),    TR    (ILG),    S     (ILG),    TS    (ILG),
     1     RAICAN(ILG),    SNOCAN(ILG),    TCAN  (ILG),    CHCAP (ILG),
     2     HTCC  (ILG),    RDRIP (ILG),    SDRIP (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FSVF  (ILG),    CWCAP (ILG),    CMASS (ILG),    FCT   (ILG)
C 
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          RDRIP(I)=0.0
          SDRIP(I)=0.0
C    -------------------- CTEM MODIFICATIONS -------------------------\          
C         ADD CMASS(I) TO THIS IF STATEMENT AS WELL
          IF(FCT(I).GT.0. .AND. CMASS(I).GT.0. .AND.
C    -------------------- CTEM MODIFICATIONS -------------------------/          
     1        (R(I).GT.0. .OR. S(I).GT.0.))THEN
              RTHRU=R(I)*FSVF(I)                                                                
              STHRU=S(I)*FSVF(I)                                                                
              RINT=(R(I)-RTHRU)*DELT*RHOW                                                    
              SINT=(S(I)-STHRU)*DELT*RHOSNI                                                  
              IF((RAICAN(I)+RINT).GT.0.)                 THEN
                  TRCAN=(RAICAN(I)*TCAN(I)+RINT*TR(I))/(RAICAN(I)+RINT)                               
              ELSE                                                                        
                  TRCAN=0.0                                                               
              ENDIF                                                                       
              IF((SNOCAN(I)+SINT).GT.0.)                 THEN 
                  TSCAN=(SNOCAN(I)*TCAN(I)+SINT*TS(I))/(SNOCAN(I)+SINT)                               
              ELSE                                                                        
                  TSCAN=0.0                                                               
              ENDIF                                                                       
              WEXCES=RINT+RAICAN(I)+SINT+SNOCAN(I)-CWCAP(I)
C                                        
              IF(WEXCES.GT.0.)                           THEN 
                  RDRIP(I)=WEXCES*(RINT+RAICAN(I))/((RINT+RAICAN(I)+
     1                  SINT+SNOCAN(I))*DELT*RHOW)                                                          
                  IF((RDRIP(I)+RTHRU).GT.0.)       THEN                                           
                      TR(I)=(RDRIP(I)*TRCAN+RTHRU*TR(I))/
     1                      (RDRIP(I)+RTHRU)                             
                  ELSE                                                                    
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
                  SDRIP(I)=WEXCES*(SINT+SNOCAN(I))/((RINT+RAICAN(I)+
     1                  SINT+SNOCAN(I))*DELT*RHOSNI)
                  IF((SDRIP(I)+STHRU).GT.0.)       THEN                                           
                      TS(I)=(SDRIP(I)*TSCAN+STHRU*TS(I))/
     1                      (SDRIP(I)+STHRU)                             
                  ELSE                                                                    
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
                  R(I)=RDRIP(I)+RTHRU                                                           
                  RAICAN(I)=RAICAN(I)+RINT-RDRIP(I)*DELT*RHOW                                      
                  S(I)=SDRIP(I)+STHRU                                                           
                  SNOCAN(I)=SNOCAN(I)+SINT-SDRIP(I)*DELT*RHOSNI                                    
              ELSE                                                                        
                  R(I)=RTHRU                                                                 
                  RAICAN(I)=RAICAN(I)+RINT                                                      
                  S(I)=STHRU                                                                 
                  SNOCAN(I)=SNOCAN(I)+SINT                                                      
              ENDIF
C
              CHCAPI  =CHCAP(I)
              TCANI   =TCAN(I)
              CHCAP(I)=RAICAN(I)*SPHW+SNOCAN(I)*SPHICE+CMASS(I)*SPHVEG                                
              TCAN (I)=(RAICAN(I)*SPHW*TRCAN+SNOCAN(I)*SPHICE*TSCAN+
     1                 CMASS(I)*SPHVEG*TCAN(I))/CHCAP(I)
              HTCC (I)=HTCC(I)+FCT(I)*(CHCAP(I)*TCAN(I)-CHCAPI*TCANI)/
     1                 DELT
              TR(I)=TR(I)-TFREZ                                                                 
              TS(I)=TS(I)-TFREZ                                                                 
          ENDIF
  100 CONTINUE                                                                        
C
      RETURN                                                                      
      END
