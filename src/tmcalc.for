      SUBROUTINE TMCALC(TBAR,THLIQ,THICE,HCP,TPOND,ZPOND,RUNOFF,
     1                  OVRFLW,ZSNOW,TSNOW,ALBSNO,RHOSNO,HCPSNO,TBASE,
     2                  HMFG,HTC,HTCS,WTRS,WTRG,TBARW,THPOR,GZERO,G12,
     3                  G23,FCT,TA,HCPS,ZPLIM,ISAND,DELZW,DELZZ,DELZ,
     4                  IG,ILG,IL1,IL2,JL)
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION
C     *                         OF OVERLAND FLOW.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * AUG 16/95 - D.VERSEGHY. TWO NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATIONS OF TBAR AND HTC;
C     *                         ALLOW SPECIFICATION OF LIMITING POND 
C     *                         DEPTH "PNDLIM" (PARALLEL CHANGES
C     *                         MADE SIMULTANEOUSLY IN CLASSW).
C     * NOV 01/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISED VERSION WITH IN-LINED CODE
C     *                         FROM TWCALC AND TFREEZ TO PERMIT 
C     *                         FREEZING AND THAWING OF SOIL LAYERS
C     *                         AT THE END OF EACH TIME STEP.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. STORE PONDED WATER INTO FIRST 
C     *                         SOIL LAYER LIQUID WATER; STEP
C     *                         AHEAD SOIL LAYER TEMPERATURES 
C     *                         USING CONDUCTION HEAT FLUX
C     *                         CALCULATED AT TOP AND BOTTOM
C     *                         OF EACH LAYER.
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TBAR  (ILG,IG),    THLIQ (ILG,IG),    THICE (ILG,IG),
     1     HCP   (ILG,IG),    HMFG  (ILG,IG),    HTC   (ILG,IG)
C
      REAL TPOND (ILG),       ZPOND (ILG),       RUNOFF(ILG),
     1     ZSNOW (ILG),       TSNOW (ILG),       ALBSNO(ILG),
     2     RHOSNO(ILG),       HCPSNO(ILG),       WTRS  (ILG),
     3     WTRG  (ILG),       HTCS  (ILG),       OVRFLW(ILG),
     4     TBASE (ILG)
C
C     * INPUT ARRAYS.
C
      REAL TBARW (ILG,IG), THPOR (ILG,IG),       HCPS  (ILG,IG),
     1     DELZW (ILG,IG), DELZZ (ILG,IG)
C
      REAL GZERO (ILG),       G12   (ILG),       G23   (ILG),
     1     FCT   (ILG),       TA    (ILG),       ZPLIM (ILG)
C
      INTEGER                 ISAND (ILG,IG)   
C
      REAL DELZ  (IG)
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C
C     * CALCULATE SUBSURFACE AND OVERLAND RUNOFF TERMS; ADJUST
C     * SURFACE PONDING DEPTH.
C
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)  THEN
              RUNOFF(I)=RUNOFF(I)+MAX(ZPOND(I)-ZPLIM(I),0.)
              OVRFLW(I)=OVRFLW(I)+FCT(I)*MAX(ZPOND(I)-ZPLIM(I),0.)
              ZPOND(I)=MIN(ZPOND(I),ZPLIM(I))
          ENDIF
  100 CONTINUE
C
C     * UPDATE SOIL TEMPERATURES AFTER GROUND WATER MOVEMENT.
C
      DO 200 J=1,IG
      DO 200 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)  THEN
              HTC(I,J)=HTC(I,J)+FCT(I)*((TBARW(I,J)+TFREZ)*
     1                 HCPW*THLIQ(I,J)+(TBAR(I,J)+TFREZ)*
     2                 HCPICE*THICE(I,J))*DELZW(I,J)/DELT
              HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                 HCPS(I,J)*(1.-THPOR(I,J))                                                   
              IF(DELZZ(I,J).GT.0.0)              THEN
                  TBAR(I,J)=((TBARW(I,J)+TFREZ)*HCPW*THLIQ(I,J)*
     1                      DELZW(I,J)+(TBAR(I,J)+TFREZ)*((HCPICE*
     2                      THICE(I,J)+HCPS(I,J)*(1.-THPOR(I,J)))*
     3                      DELZW(I,J)+HCPSND*(DELZZ(I,J)-DELZW(I,J))))/
     4                      (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     5                      DELZW(I,J)))-TFREZ
              ENDIF
          ENDIF
  200 CONTINUE
C
C     * STEP AHEAD POND TEMPERATURE; CHECK FOR FREEZING, AND ADD
C     * FROZEN WATER TO SNOW PACK.
C
      DO 300 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. ZPOND(I).GT.0.0)
     1                                                             THEN
              HTC(I,1)=HTC(I,1)+FCT(I)*HCPW*(TPOND(I)+TFREZ)*
     1            ZPOND(I)/DELT
              GP1=ZPOND(I)*(G12(I)-GZERO(I))/(ZPOND(I)+DELZZ(I,1))+
     1            GZERO(I)
              TPOND(I) =TPOND(I)+(GZERO(I)-GP1)*DELT/(HCPW*ZPOND(I))
              GZERO(I)=GP1
          ENDIF
  300 CONTINUE
C
      DO 400 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ZPOND(I).GT.0. .AND. TPOND(I).LT.0.)
     1                                                              THEN
             HTCS(I)=HTCS(I)-FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1               ZSNOW(I)/DELT
             ZFREZ=0.0
             HADD=-TPOND(I)*HCPW*ZPOND(I)                                              
             TPOND(I)=0.0                                                               
             HCONV=CLHMLT*RHOW*ZPOND(I)                                               
             HTC(I,1)=HTC(I,1)-FCT(I)*HCPW*(TPOND(I)+TFREZ)*
     1                ZPOND(I)/DELT
             IF(HADD.LE.HCONV)             THEN                                                      
                ZFREZ=HADD/(CLHMLT*RHOW)                                                
                ZPOND(I)=ZPOND(I)-ZFREZ                                                       
                ZFREZ=ZFREZ*RHOW/RHOICE                                                 
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                TSNOW(I)=TSNOW(I)*HCPSNO(I)*ZSNOW(I)/(HCPSNO(I)*ZSNOW(I)
     1                  +HCPICE*ZFREZ)                    
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)
     1                   +ZFREZ)                        
                HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE                                             
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                TPOND(I)=0.0                                                               
             ELSE                                                                        
                HADD=HADD-HCONV                                                         
                ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                TTEST=-HADD/(HCPICE*ZFREZ)                                              
                IF(ZSNOW(I).GT.0.0) THEN
                    TLIM=MIN(TSNOW(I),TBAR(I,1))
                ELSE
                    TLIM=MIN(TA(I)-TFREZ,TBAR(I,1))
                ENDIF
                IF(TTEST.LT.TLIM)       THEN                                    
                   HEXCES=HADD+TLIM*HCPICE*ZFREZ                         
                   GZERO(I)=GZERO(I)-HEXCES/DELT                                             
                   HTC(I,1)=HTC(I,1)+FCT(I)*(HADD-HEXCES)/DELT
                   TSNOW(I)=(TSNOW(I)*HCPSNO(I)*ZSNOW(I)+
     1                      TLIM*HCPICE*ZFREZ)          
     2                      /(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)                                    
                ELSE                                                                    
                   TSNOW(I)=(TSNOW(I)*HCPSNO(I)*ZSNOW(I)+TTEST*HCPICE*
     1                       ZFREZ)/(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)                                    
                   HTC(I,1)=HTC(I,1)+FCT(I)*HADD/DELT
                ENDIF                                                                   
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)+
     1                     ZFREZ)                        
                HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE                                             
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                ZPOND(I)=0.0                                                               
                TPOND(I)=0.0                                                               
             ENDIF                                                                       
             HTC (I,1)=HTC (I,1)+FCT(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
             HMFG(I,1)=HMFG(I,1)-FCT(I)*CLHMLT*RHOICE*ZFREZ/DELT
             WTRS(I)=WTRS(I)+FCT(I)*ZFREZ*RHOICE/DELT
             WTRG(I)=WTRG(I)-FCT(I)*ZFREZ*RHOICE/DELT
             HTCS(I)=HTCS(I)+FCT(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
  400 CONTINUE
C
C     * STEP AHEAD SOIL LAYER TEMPERATURES; CHECK FOR FREEZING OR
C     * THAWING.
C
      DO 500 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)               THEN
              TBAR(I,1)=TBAR(I,1)+(GZERO(I)-G12(I))*DELT/
     1                  (HCP(I,1)*DELZW(I,1)+HCPSND*(DELZZ(I,1)-
     2                  DELZW(I,1)))
              TBAR(I,2)=TBAR(I,2)+(G12  (I)-G23(I))*DELT/
     1                  (HCP(I,2)*DELZW(I,2)+HCPSND*(DELZZ(I,2)-
     2                  DELZW(I,2)))                         
              IF(DELZZ(I,IG).GT.0.0)                     THEN
                  TBAR(I,IG)=TBAR(I,IG)+G23(I)*(1.0-(DELZ(IG)-
     1                  DELZZ(I,IG))/DELZ(IG))*DELT/(HCP(I,IG)*
     2                  DELZZ(I,IG))
              ENDIF
              IF(DELZZ(I,IG).LT.DELZ(IG)) THEN
                  TBASE(I)=TBASE(I)+FCT(I)*G23(I)*((DELZ(IG)-
     1                  DELZZ(I,IG))/DELZ(IG))*DELT/(HCPSND*(DELZ(IG)-
     2                  DELZZ(I,IG)))
              ENDIF
          ENDIF
  500 CONTINUE
C     
      DO 600 J=1,IG                                                               
      DO 600 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)  THEN
              HTC(I,J)=HTC(I,J)-FCT(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
              IF(TBAR(I,J).LT.0. .AND. THLIQ(I,J).GT.THLMIN)    THEN                        
                  THFREZ=-(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOW)                              
                  IF(THFREZ.LE.(THLIQ(I,J)-THLMIN)) THEN                         
                      HMFG(I,J)=HMFG(I,J)-FCT(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FCT(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      THLIQ(I,J)=THLIQ(I,J)-THFREZ                                        
                      THICE(I,J)=THICE(I,J)+THFREZ*RHOW/RHOICE                            
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                      TBAR (I,J)=-HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)-FCT(I)*(THLIQ(I,J)-THLMIN)*
     1                          CLHMLT*RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FCT(I)*(THLIQ(I,J)-THLMIN)*
     1                          CLHMLT*RHOW*DELZW(I,J)/DELT
                      HADD=(THFREZ-(THLIQ(I,J)-THLMIN))*CLHMLT*
     1                     RHOW              
                      THICE(I,J)=THICE(I,J)+(THLIQ(I,J)-THLMIN)*
     1                           RHOW/RHOICE          
                      THLIQ(I,J)=THLMIN
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=-HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
C                                                                   
              IF(TBAR(I,J).GT.0. .AND. THICE(I,J).GT.0.)        THEN                           
                  THMELT=(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOICE)                             
                  IF(THMELT.LE.THICE(I,J))                 THEN 
                      HMFG(I,J)=HMFG(I,J)+FCT(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FCT(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      THICE(I,J)=THICE(I,J)-THMELT                                        
                      THLIQ(I,J)=THLIQ(I,J)+THMELT*RHOICE/RHOW                            
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)+FCT(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FCT(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HADD=(THMELT-THICE(I,J))*CLHMLT*RHOICE                            
                      THLIQ(I,J)=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                          
                      THICE(I,J)=0.0                                                    
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
              HTC(I,J)=HTC(I,J)+FCT(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
          ENDIF
  600 CONTINUE
C                                                                           
C     * ADD PONDED WATER BACK INTO FIRST SOIL LAYER LIQUID MOISTURE
C     * STORE.
C
      DO 700 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4
     1             .AND.  ZPOND(I).GT.0. .AND. DELZW(I,1).GT.0.) THEN
              HCP  (I,1)=HCPW*(THLIQ(I,1)+ZPOND(I)/DELZW(I,1)) +
     1                   HCPICE*THICE(I,1) + HCPS(I,1)*(1.-THPOR(I,1)) 
              TBAR (I,1)=((TPOND(I)+TFREZ)*HCPW*ZPOND(I) + 
     1                    (TBAR(I,1)+TFREZ)*((HCPW*THLIQ(I,1) +          
     2                    HCPICE*THICE(I,1) + 
     3                    HCPS(I,1)*(1.-THPOR(I,1)))*DELZW(I,1)+
     4                    HCPSND*(DELZZ(I,1)-DELZW(I,1))))/
     5                    (HCP(I,1)*DELZW(I,1)+HCPSND*(DELZZ(I,1)-
     6                    DELZW(I,1)))-TFREZ  
              THLIQ(I,1)=THLIQ(I,1)+ZPOND(I)/DELZW(I,1)                                     
          ENDIF
  700 CONTINUE
C                                                                                  
      RETURN                                                                      
      END 
