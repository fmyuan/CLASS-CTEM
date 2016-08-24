      SUBROUTINE SNOADD(ALBSNO,TSNOW,RHOSNO,ZSNOW,HCPSNO,HTCS,
     1                  FCT,S,TS,ILG,IL1,IL2,JL)
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ACCUMULATION OF SNOW ON GROUND.
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL ALBSNO(ILG),   TSNOW (ILG),   RHOSNO(ILG),   ZSNOW (ILG),
     1     HCPSNO(ILG),   HTCS  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FCT   (ILG),   S     (ILG),   TS    (ILG)
C                                                                                 
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. S(I).GT.0.)                         THEN
              HTCS  (I)=HTCS(I)-FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                  ZSNOW(I)/DELT
              SNOFAL=S(I)*DELT                      
              IF(S(I).GE.1.4E-6)                            THEN 
                  ALBSNO(I)=0.84                                                             
              ELSE IF(.NOT.(ZSNOW(I).GT.0.))                THEN
                  ALBSNO(I)=0.50                                                         
              ENDIF                                                                   
              HCPSNP=HCPICE*RHOSNI/RHOICE
              TSNOW (I)=((TSNOW(I)+TFREZ)*ZSNOW(I)*HCPSNO(I) +
     1                   (TS   (I)+TFREZ)*SNOFAL  *HCPSNP)/
     2                  (ZSNOW(I)*HCPSNO(I) + SNOFAL*HCPSNP) -
     3                   TFREZ
              RHOSNO(I)=(ZSNOW(I)*RHOSNO(I) + SNOFAL*RHOSNI)/
     1                  (ZSNOW(I)+SNOFAL)                          
              ZSNOW (I)=ZSNOW(I)+SNOFAL                                                          
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
              HTCS  (I)=HTCS(I)+FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                  ZSNOW(I)/DELT
          ENDIF                                                 
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END    
