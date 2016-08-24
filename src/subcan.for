      SUBROUTINE SUBCAN(IWATER,FCT,R,TR,S,TS,EVAPG,QFN,QFG,PCPN,PCPG,
     1                  ILG,IL1,IL2,JL)
C
C     * NOV 09/00 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS INTO
C     *                         WPREP.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         INCORPORATE DIAGNOSTICS.
C     * APR 21/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. PERFORM "WPREP" CALCULATIONS UNDER 
C     *                         CANOPY: LUMP DOWNWARD WATER VAPOUR 
C     *                         FLUXES TOGETHER WITH PRECIPITATION 
C     *                         REACHING GROUND.
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG),    TR    (ILG),    S     (ILG),    TS    (ILG),
     1     EVAPG (ILG),    QFN   (ILG),    QFG   (ILG),    PCPN  (ILG),
     2     PCPG  (ILG)
C
C     * INPUT ARRAY.
C
      REAL FCT   (ILG)
C                                                                                 
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              IF(S(I).GT.0. .OR.
     1           (IWATER.EQ.2 .AND. EVAPG(I).LT.0.))          THEN                         
                  SADD=S(I)-EVAPG(I)*RHOW/RHOSNI                                                
                  IF(SADD.GT.0.)                        THEN
                      S(I)=SADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-SADD*RHOSNI/RHOW                                             
                      S(I)=0.0                                                               
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  S(I)=0.0                                                                   
                  TS(I)=0.0                                                                  
              ENDIF
C
              IF(R(I).GT.0. .OR.
     1           (IWATER.EQ.1 .AND. EVAPG(I).LT.0.))          THEN                         
                  RADD=R(I)-EVAPG(I)                                                            
                  IF(RADD.GT.0.)                     THEN 
                      R(I)=RADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-RADD                                                         
                      R(I)=0.0                                                               
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  R(I)=0.0                                                                   
                  TR(I)=0.0   
              ENDIF                                                               
          ENDIF                                                                       
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END       
