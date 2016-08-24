      SUBROUTINE SNOVAP(RHOSNO,ZSNOW,HCPSNO,TSNOW,EVAP,QFN,QFG,HTCS,
     1                  WLOST,FCT,R,S,ILG,IL1,IL2,JL)
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 16/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         INCORPORATE DIAGNOSTIC ARRAY "WLOST". 
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         ADDITIONAL DIAGNOSTIC CALCULATION -
C     *                         UPDATE HTCS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. SUBLIMATION FROM SNOWPACK.
C                                          
C     * INPUT/OUTPUT ARRAYS.
C
      REAL RHOSNO(ILG),   ZSNOW (ILG),   HCPSNO(ILG),   TSNOW (ILG), 
     1     EVAP  (ILG),   QFN   (ILG),   QFG   (ILG),   HTCS  (ILG),
     2     WLOST (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FCT   (ILG),   R     (ILG),   S     (ILG)   
C                                       
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. (S(I).LE.0. .OR. R(I).LE.0.))      THEN
              HTCS(I)=HTCS(I)-FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              IF(EVAP(I).LT.0.)                             THEN 
                  ZADD=-EVAP(I)*DELT*RHOW/RHOSNI                                             
                  RHOSNO(I)=(ZSNOW(I)*RHOSNO(I)+ZADD*RHOSNI)/
     1                      (ZSNOW(I)+ZADD)                          
                  ZSNOW (I)=ZSNOW(I)+ZADD                                                        
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE                                             
                  EVAP  (I)=0.0                                                                
              ELSE                                                                        
                  ZLOST=EVAP(I)*DELT*RHOW/RHOSNO(I)                                             
                  IF(ZLOST.LE.ZSNOW(I))                     THEN 
                      ZSNOW(I)=ZSNOW(I)-ZLOST                                                   
                      EVAP (I)=0.0                                                            
                  ELSE                                                                    
                      ZREM=(ZLOST-ZSNOW(I))*RHOSNO(I)/RHOW
                      ZSNOW(I)=0.0                                                           
                      HCPSNO(I)=0.0
                      TSNOW(I)=0.0 
                      EVAP(I)=ZREM*(CLHMLT+CLHVAP)/(CLHVAP*DELT)
                      WLOST(I)=WLOST(I)-ZREM*RHOW*CLHMLT/CLHVAP
                      QFN(I)=QFN(I)-FCT(I)*ZREM*RHOW/DELT
                      QFG(I)=QFG(I)+FCT(I)*EVAP(I)*RHOW
                  ENDIF                                                                   
              ENDIF 
              HTCS(I)=HTCS(I)+FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF                                                                      
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END        
