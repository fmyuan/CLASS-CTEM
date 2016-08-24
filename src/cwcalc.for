      SUBROUTINE CWCALC(TCAN,RAICAN,SNOCAN,CHCAP,HMFC,HTCC,CMASS,
     1                  FCT,ILG,IL1,IL2)                           
C                                                                 
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * MAR 17/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 13/91 - D.VERSEGHY. ADJUST CANOPY TEMPERATURE AND
C     *                         INTERCEPTED LIQUID/FROZEN 
C     *                         MOISTURE STORES FOR FREEZING/
C     *                         THAWING.
C                                       
C     * I/O ARRAYS.
C
      REAL TCAN  (ILG),    RAICAN(ILG),    SNOCAN(ILG),
     1     CHCAP (ILG),    HMFC  (ILG),    HTCC  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL CMASS (ILG),    FCT   (ILG)
C                                          
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C---------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0.)                                        THEN
              HTCC  (I)=HTCC(I)-FCT(I)*TCAN(I)*CHCAP(I)/DELT
              IF(RAICAN(I).GT.0. .AND. TCAN(I).LT.TFREZ)      THEN                                    
                  HFREZ=CHCAP(I)*(TFREZ-TCAN(I))                                                
                  HCONV=RAICAN(I)*CLHMLT                                                     
                  IF(HFREZ.LT.HCONV)                       THEN 
                     RCONV=HFREZ/CLHMLT                                                  
                     SNOCAN(I)=SNOCAN(I)+RCONV                                                 
                     RAICAN(I)=RAICAN(I)-RCONV                                                 
                     TCAN  (I)=TFREZ                                                          
                     HMFC  (I)=HMFC(I)-FCT(I)*CLHMLT*RCONV/DELT
                     HTCC  (I)=HTCC(I)-FCT(I)*CLHMLT*RCONV/DELT
                  ELSE                                                                    
                     HCOOL=HFREZ-HCONV                                                   
                     SNOCAN(I)=SNOCAN(I)+RAICAN(I)                                                
                     TCAN  (I)=-HCOOL/(SPHVEG*CMASS(I)+SPHICE*
     1                         SNOCAN(I))+TFREZ  
                     HMFC  (I)=HMFC(I)-FCT(I)*CLHMLT*RAICAN(I)/DELT
                     HTCC  (I)=HTCC(I)-FCT(I)*CLHMLT*RAICAN(I)/DELT
                     RAICAN(I)=0.0                                                          
                  ENDIF                                                                   
              ENDIF                                                                       
              IF(SNOCAN(I).GT.0. .AND. TCAN(I).GT.TFREZ)        THEN                                    
                  HMELT=CHCAP(I)*(TCAN(I)-TFREZ)                                                
                  HCONV=SNOCAN(I)*CLHMLT                                                     
                  IF(HMELT.LT.HCONV)                       THEN 
                     SCONV=HMELT/CLHMLT                                                  
                     SNOCAN(I)=SNOCAN(I)-SCONV                                                 
                     RAICAN(I)=RAICAN(I)+SCONV                                                 
                     TCAN(I)=TFREZ                                                          
                     HMFC  (I)=HMFC(I)+FCT(I)*CLHMLT*SCONV/DELT
                     HTCC  (I)=HTCC(I)+FCT(I)*CLHMLT*SCONV/DELT
                  ELSE                                                                    
                     HWARM=HMELT-HCONV                                                   
                     RAICAN(I)=RAICAN(I)+SNOCAN(I)                                                
                     TCAN(I)=HWARM/(SPHVEG*CMASS(I)+SPHW*RAICAN(I))+
     1                       TFREZ                         
                     HMFC  (I)=HMFC(I)+FCT(I)*CLHMLT*SNOCAN(I)/DELT
                     HTCC  (I)=HTCC(I)+FCT(I)*CLHMLT*SNOCAN(I)/DELT
                     SNOCAN(I)=0.0                                                          
                  ENDIF                                                                   
              ENDIF                                                                       
              CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)+SPHICE*SNOCAN(I)
              HTCC (I)=HTCC(I)+FCT(I)*TCAN(I)*CHCAP(I)/DELT
          ENDIF                                
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END 
