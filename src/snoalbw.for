      SUBROUTINE SNOALBW(ALBSNO,RHOSNO,ZSNOW,HCPSNO,XSNOW,
     1                   FCT,ISAND,S,RMELT,                        
     2                   ILG,IG,IL1,IL2,JL)       
C
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         SPECIFY LOCATION OF ICE SHEETS
C     *                         BY SOIL TEXTURE ARRAY RATHER
C     *                         THAN BY SOIL COLOUR INDEX.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * MAR 13/92 - M.LAZARE.   CLASS - VERSION 2.1.
C     *                         CODE FOR MODEL VERSION GCM7 -
C     *                         DIVIDE PREVIOUS SUBROUTINE 
C     *                         "SNOALB" INTO "SNOALBA" AND
C     *                         "SNOALBW" AND VECTORIZE.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. CALCULATE DECREASE IN SNOW ALBEDO
C     *                         AND INCREASE IN DENSITY DUE TO
C     *                         AGING. (ASSIGN DIFFERENT LOWER
C     *                         SNOW ALBEDO LIMITS FOR DRY AND
C     *                         MELTING SNOW.)
C                                                                                 
C     * OUTPUT ARRAYS.                                                            
C                                                                                 
      REAL ALBSNO(ILG),   RHOSNO(ILG),   ZSNOW (ILG),   HCPSNO(ILG),
     1     XSNOW(ILG)
C                                                                                 
C     * INPUT ARRAYS.                                                             
C                                                                                 
      REAL FCT   (ILG),   S     (ILG),   RMELT (ILG)              
C 
      INTEGER             ISAND (ILG,IG)
C                                                                                 
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C----------------------------------------------------------------------               
      IPTBAD=0                                                                    
      DO 100 I=IL1,IL2  
          IF(ZSNOW(I).GT.0. .AND. ISAND(I,1).NE.-4 .AND.
     1       FCT  (I).GT.0. .AND. S(I).LT.1.4E-6)                  THEN
              IF(RMELT(I).GT.0. .AND. ALBSNO(I).GT.0.50)      THEN
                  TIMFAC=EXP(LOG((ALBSNO(I)-0.50)/0.34)-                    
     1                       2.778E-6*DELT)                                      
                  ALBSNO(I)=0.34*TIMFAC+0.50                                 
              ELSE IF(RMELT(I).LE.0. .AND. ALBSNO(I).GT.0.70) THEN
                  TIMFAC=EXP(LOG((ALBSNO(I)-0.70)/0.14)-                    
     1                       2.778E-6*DELT)                                      
                  ALBSNO(I)=0.14*TIMFAC+0.70                                 
              ENDIF                                                           
          ENDIF
C                                                       
          IF(FCT(I).GT.0. .AND. ZSNOW(I).GT.0. .AND. 
     1       RHOSNO(I).LT.300.)                                    THEN
              RHOOLD=RHOSNO(I)                                                       
              TIMFAC=EXP(LOG((300.0-RHOSNO(I))/200.0)-2.778E-6*DELT)                
              RHOSNO(I)=300.0-200.0*TIMFAC                                           
              ZSNOW(I)=ZSNOW(I)*RHOOLD/RHOSNO(I)                                           
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
          ENDIF
          IF((ALBSNO(I).LT.0.50 .OR. ALBSNO(I).GT.1.0) .AND. 
     1       ZSNOW (I).GT.0. .AND. FCT(I).GT.0.)               IPTBAD=I
C
          IF(ZSNOW(I).GT.0. .AND. FCT(I).GT.0.)            XSNOW(I)=1.0      
  100 CONTINUE                                                                    
C
      IF(IPTBAD.NE.0) THEN                                                        
         WRITE(6,6100) IPTBAD,JL,ALBSNO(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALBSNO = ',F10.5)            
         CALL XIT('SNOALBW',-1)                                                               
      ENDIF                                                                       
C                                                                                 
      RETURN                                                                      
      END        
