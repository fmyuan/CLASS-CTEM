      SUBROUTINE TWCALC(TBAR,THLIQ,THICE,HCP,TBARW,HMFG,HTC,
     1                  THPOR,EVAP,FCT,HCPS,ISAND,DELZW,DELZZ,
     2                  IG,ILG,IL1,IL2,JL)
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT. 
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U - 
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ADJUST SOIL LAYER TEMPERATURES
C     *                         AND LIQUID/FROZEN MOISTURE CONTENTS
C     *                         FOR FREEZING/THAWING.
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), THLIQ (ILG,IG), THICE (ILG,IG), 
     1     HCP   (ILG,IG), TBARW (ILG,IG), HMFG  (ILG,IG),
     2     HTC   (ILG,IG)
C
C     * INPUT ARRAYS.
C
      REAL THPOR (ILG,IG), HCPS  (ILG,IG), DELZW (ILG,IG),
     1     DELZZ (ILG,IG)
C
      REAL EVAP  (ILG),    FCT   (ILG)
C
      INTEGER              ISAND (ILG,IG)   
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
      DO 100 J=1,IG                                                               
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)               THEN
              HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                   HCPS(I,J)*(1.-THPOR(I,J))                                                   
              HTC  (I,J)=HTC(I,J)-FCT(I)*(HCP(I,J)*DELZW(I,J)+
     1                   HCPSND*(DELZZ(I,J)-DELZW(I,J)))*
     2                   (TBAR(I,J)+TFREZ)/DELT
              IF(TBAR(I,J).LT.0. .AND. THLIQ(I,J).GT.THLMIN)    THEN                        
                  THFREZ=-(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOW)                              
                  IF(J.EQ.1)                               THEN 
                      THEVAP=EVAP(I)*DELT/DELZW(I,J)                                          
                  ELSE                                                                
                      THEVAP=0.0                                                      
                  ENDIF                                                               
                  IF((THLIQ(I,J)-THLMIN-THEVAP).GT.0.0)        THEN 
                    IF(THFREZ.LE.(THLIQ(I,J)-THLMIN-THEVAP)) THEN                         
                      HMFG(I,J)=HMFG(I,J)-FCT(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FCT(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      THLIQ(I,J)=THLIQ(I,J)-THFREZ                                        
                      THICE(I,J)=THICE(I,J)+THFREZ*RHOW/RHOICE                            
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                    ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)-FCT(I)*(THLIQ(I,J)-THLMIN-
     1                          THEVAP)*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FCT(I)*(THLIQ(I,J)-THLMIN-
     1                          THEVAP)*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HADD=(THFREZ-(THLIQ(I,J)-THLMIN-THEVAP))*CLHMLT*
     1                     RHOW              
                      THICE(I,J)=THICE(I,J)+(THLIQ(I,J)-THLMIN-THEVAP)*
     1                           RHOW/RHOICE          
                      THLIQ(I,J)=THLMIN+THEVAP                                          
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J)) 
                      TBAR (I,J)=-HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                    ENDIF                                                               
                  ENDIF                                                               
              ENDIF
C                                                                   
              IF(TBAR(I,J).GT.0. .AND. THICE(I,J).GT.0.)        THEN                           
                  THMELT=(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                   DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOICE)                             
                  IF(THMELT.LE.THICE(I,J))                 THEN 
                      HMFG(I,J)=HMFG(I,J)+FCT(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FCT(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      THICE(I,J)=THICE(I,J)-THMELT                                        
                      THLIQ(I,J)=THLIQ(I,J)+THMELT*RHOICE/RHOW                            
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
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
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
              HTC  (I,J)=HTC(I,J)+FCT(I)*(HCP(I,J)*DELZW(I,J)+
     1                   HCPSND*(DELZZ(I,J)-DELZW(I,J)))*
     2                   (TBAR(I,J)+TFREZ)/DELT
              TBARW(I,J)=TBAR(I,J)
              HTC(I,J)=HTC(I,J)-FCT(I)*(TBAR(I,J)+TFREZ)*
     1                 (HCPW*THLIQ(I,J)+HCPICE*THICE(I,J))*
     2                 DELZW(I,J)/DELT
          ENDIF                                                      
  100 CONTINUE                                                                    
C                                                                                  
      RETURN                                                                      
      END 
