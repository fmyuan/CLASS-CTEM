      SUBROUTINE GRDRAN(IVEG,THLIQ,THICE,TBARW,THLMAX,THTEST,
     1                  FDT,TFDT,RUNOFF,QFG,WLOST,
     2                  FCT,DT,BI,XDRAIN,EVAP,R,ZPOND,THPORI,
     3                  PSISTI,GRKSTI,DELZW,ISAND,IGRNF,
     4                  IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                  WEXCES,IGRD                              )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * DEC 30/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUGFIX IN CALCULATION OF QFG.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         ADDITIONAL DIAGNOSTIC CALCULATIONS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. UPDATE SOIL LAYER TEMPERATURES AND
C     *                         LIQUID MOISTURE CONTENTS FOR
C     *                         NON-INFILTRATING CONDITIONS (I.E.
C     *                         NO PONDED WATER AND NO RAINFALL 
C     *                         OCCURRING WITHIN CURRENT TIMESTEP).
C
C     * INPUT/OUTPUT FIELDS.
C
      REAL THLIQ (ILG,IG), THICE (ILG,IG), TBARW (ILG,IG),  
     1     THLMAX(ILG,IG), THTEST(ILG,IG), FDT   (ILG,IGP1),          
     2     TFDT  (ILG,IGP1)                                                    
C                        
      REAL RUNOFF(ILG),    QFG   (ILG),    WLOST (ILG)
C
C     * INPUT FIELDS.
C
      REAL FCT   (ILG),    DT    (ILG),    BI    (ILG,IG),
     1     XDRAIN(ILG),    EVAP  (ILG),    R     (ILG),    ZPOND (ILG),
     2     THPORI(ILG,IG), PSISTI(ILG,IG), GRKSTI(ILG,IG),
     3     DELZW (ILG,IG)
C  
      INTEGER              ISAND (ILG,IG), IGRNF(ILG)
C
C     * WORK FIELDS.
C
      REAL WEXCES(ILG)
C
      INTEGER              IGRD  (ILG)
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * DETERMINE POINTS WHICH SATISFY CONDITIONS FOR THESE CALCULATIONS
C     * AND STORE THEM AS HAVING NON-ZERO VALUES FOR WORK ARRAY "IGRD".
C     * NOTE THAT POINTS WHICH GO THROUGH THE ROUTINE "GRINFL" SHOULD
C     * NOT GO THROUGH THIS ROUTINE WHEN IT IS CALLED FROM CLASSW.
C     * THE INPUT ARRAY "IGRNF" HANDLES THIS CONDITION (PASSED AS
C     * "IZERO" ARRAY WHEN CALLED FROM "WEND" OR THE END OF "GRINFL"). 
C
      DO 50 I=IL1,IL2
          IF(FCT (I).GT.0. .AND. 
     1       ISAND(I,1).GT.-4 .AND.DT(I).GT.0. .AND.IGRNF(I).EQ.0 .AND.
     2       (R(I).EQ.0. .AND. ZPOND(I).EQ.0.))                     THEN
              IGRD(I)=1
          ELSE
              IGRD(I)=0
          ENDIF
   50 CONTINUE
C
C     * CALCULATE MAXIMUM LIQUID WATER CONTENT OF EACH SOIL LAYER.
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          IF(IGRD(I).GT.0)                                          THEN
             IF(THICE(I,J).GT.0.)                             THEN
                THLMAX(I,J)=MAX((THPORI(I,J)-THICE(I,J)*RHOICE/RHOW),
     1              THLMIN)                
             ELSE                                                                    
                THLMAX(I,J)=THPORI(I,J)
             ENDIF                                                                   
          ENDIF
  100 CONTINUE
C
C     * CALCULATE THEORETICAL FLOW RATES AT BOTTOM OF SOIL COLUMN AND
C     * BETWEEN SOIL LAYERS.

      DO 150 I=IL1,IL2
          IF(IGRD(I).GT.0)                                          THEN
             FDT(I,1)=-EVAP(I)*DT(I)                                                           
             IF(THLIQ(I,IG).GT.THLMIN)              THEN 
                FDT(I,IG+1)=GRKSTI(I,3)*DT(I)*XDRAIN(I)*
     1                      ((THLIQ(I,3)/THPORI(I,3))**(2.*BI(I,3)+3.))
             ELSE                                                                        
                FDT(I,IG+1)=0.0                                                           
             ENDIF                                                                       
          ENDIF
  150 CONTINUE
C
      DO 200 J=1,IG-1                                                             
      DO 200 I=IL1,IL2
          IF(IGRD(I).GT.0)                                          THEN
              IF(THPORI(I,J).GT.0.0.AND.THPORI(I,J+1).GT.0.0.AND.
     1                  ISAND(I,J+1).GT.-3) THEN
                  IF(DELZW(I,J+1).GT.DELZW(I,J)) THEN
                      THPBND=(THPORI(I,J)+THPORI(I,J+1))/2.0
                      THLBND=(THLIQ(I,J)+THLIQ(I,J+1))/2.0                                        
                      DTHLDZ=(THLIQ(I,J+1)-THLBND)/DELZW(I,J+1)+
     1                       (THLBND-THLIQ(I,J))/DELZW(I,J)
                  ELSE
                      DTHLDZ=2.0*(THLIQ(I,J+1)-THLIQ(I,J))/
     1                       (DELZW(I,J+1)+DELZW(I,J))
                      THLBND=THLIQ(I,J)+0.5*DTHLDZ*DELZW(I,J)
                      DTHPDZ=2.0*(THPORI(I,J+1)-THPORI(I,J))/
     1                       (DELZW(I,J+1)+DELZW(I,J))
                      THPBND=THPORI(I,J)+0.5*DTHPDZ*DELZW(I,J)
                  ENDIF
                  BBND=(BI(I,J)+BI(I,J+1))/2.0
                  GRSBND=(GRKSTI(I,J)+GRKSTI(I,J+1))/2.0
                  PSSBND=(PSISTI(I,J)+PSISTI(I,J+1))/2.0
                  GRK=MIN(GRSBND*(THLBND/THPBND)**(2.*BBND+3.),
     1                   GRSBND)                     
                  PSI=MIN(PSSBND*(THLBND/THPBND)**(-BBND),PSSBND)                          
                  FDT(I,J+1)=GRK*DT(I)*((-BBND*PSI*DTHLDZ/THLBND)+1.)
              ELSE
                  FDT(I,J+1)=0.0
              ENDIF
          ENDIF
  200 CONTINUE 
C                               
C     * CHECK FOR SUSTAINABLE EVAPORATION RATE FROM TOP SOIL LAYER; IF
C     * LIQUID WATER SUPPLY INSUFFICIENT, REMOVE WATER FROM FROZEN SOIL 
C     * MOISTURE.
C
      IPTBAD=0                                        
      DO 250 J=1,IG                                                               
      DO 250 I=IL1,IL2
          IF(IGRD(I).GT.0 .AND. J.EQ.1 .AND. FDT(I,J).LT.0.)    THEN 
              THTEST(I,J)=THLIQ(I,J)+(FDT(I,J)-FDT(I,J+1))/DELZW(I,J)                            
              IF(THTEST(I,J).LT.THLMIN)             THEN
                  FDT(I,J)=FDT(I,J)+(THLIQ(I,J)-THLMIN)*DELZW(I,J)
                  THLIQ(I,J)=THLMIN
                  WEXCES(I)=-FDT(I,J)                                                  
                  FDT(I,J)=0.0                                                      
                  THSUBL=WEXCES(I)*RHOW/(RHOICE*DELZW(I,J))                             
                  IF(THLIQ(I,J).GT.0.0)                 THEN
                      TBARW(I,J)=TBARW(I,J)-(CLHMLT*RHOICE*THSUBL)/                       
     1                           (HCPW*THLIQ(I,J))                                             
                  ENDIF
                  IF(THSUBL.LE.THICE(I,J))              THEN
                      THICE(I,J)=THICE(I,J)-THSUBL                                        
                  ELSE
                      THSUBL=THSUBL-THICE(I,J)
                      THICE(I,J)=0.0
                      QFG(I)=QFG(I)-FCT(I)*THSUBL*RHOICE*DELZW(I,J)/
     1                       DELT
                      WLOST(I)=WLOST(I)+THSUBL*RHOICE*DELZW(I,J)
                  ENDIF
              ENDIF
              IF(THICE(I,J).LT.0.) IPTBAD=I
          ENDIF
C                                                    
C     * ENSURE THAT CALCULATED WATER FLOWS BETWEEN SOIL LAYERS DO NOT
C     * CAUSE LIQUID MOISTURE CONTENT OF ANY LAYER TO FALL BELOW THE
C     * RESIDUAL VALUE OR TO EXCEED THE CALCULATED MAXIMUM.
C
          IF(IGRD(I).GT.0 .AND. THLIQ(I,J).LE.THLMIN)               THEN 
              IF(FDT(I,J).LE.0. .AND. FDT(I,J+1).GE.0.)      THEN                          
                  FDT(I,J)=0.0    
                  FDT(I,J+1)=0.0   
              ELSE IF(FDT(I,J).GE.0. .AND. FDT(I,J+1).GT.0.) THEN                      
                  FDT(I,J+1)=0.0 
              ELSE IF(FDT(I,J).LT.0. .AND. FDT(I,J+1).LE.0.) THEN                      
                  FDT(I,J)=0.0    
              ENDIF
          ENDIF   
  250 CONTINUE    
C
      IF(IPTBAD.NE.0)                                           THEN
          WRITE(6,6500) IPTBAD,JL,IVEG,THICE(IPTBAD,1)
 6500     FORMAT('0AT (I,J)=(',I3,',',I3,'), IVEG=',I2,' THICE(1)= ',
     1            E13.5)
          CALL XIT('GRDRAN',-1)
      ENDIF
C
      DO 300 J=IG,1,-1                                                            
      DO 300 I=IL1,IL2
          IF(IGRD(I).GT.0 .AND. THLIQ(I,J).GE.THLMAX(I,J))          THEN
              IF(FDT(I,J).GE.0. .AND. FDT(I,J+1).LE.0.)      THEN                          
                  FDT(I,J)=0.0                                                      
                  FDT(I,J+1)=0.0                                                    
              ELSE IF(FDT(I,J).GT.0. .AND. FDT(I,J+1).GE.0.) THEN                      
                  IF(FDT(I,J).GT.FDT(I,J+1)) FDT(I,J)=FDT(I,J+1)                          
              ELSE IF(FDT(I,J).LE.0. .AND. FDT(I,J+1).LT.0.) THEN                      
                  IF(FDT(I,J+1).LT.FDT(I,J)) FDT(I,J+1)=FDT(I,J)                          
              ENDIF                                                               
          ENDIF                                                                   
  300 CONTINUE
C
C     * (FOR FOLLOWING LOOPS VECTORIZATION AND OPTIMIZATION ARE ENHANCED
C     * BY UNROLLING THE INNER-MOST LOOP. THIS EXPLICITLY ASSUMES THAT
C     * IG=3 AND THE ROUTINE IS ABORTED IF SUCH IS NOT THE CASE.)
C
      IF(IG.NE.3)                      CALL XIT('GRDRAN',-2)
C
      DO 400 J=1,IG                                                               
      DO 400 I=IL1,IL2
          IF(IGRD(I).GT.0.AND.ISAND(I,J).NE.-3)                     THEN
              THTEST(I,J)=THLIQ(I,J)+(FDT(I,J)-FDT(I,J+1))/DELZW(I,J)                            
              IF(THTEST(I,J).LT.THLMIN)                           THEN
                  IF(FDT(I,J).LE.0. .AND. FDT(I,J+1).GE.0.)      THEN                          
                      FDTBOT=(THLIQ(I,J)-THLMIN)*DELZW(I,J)*FDT(I,J+1)/
     1                    (FDT(I,J+1)-FDT(I,J)) 
                      FDT(I,J)=FDTBOT-(THLIQ(I,J)-THLMIN)*DELZW(I,J) 
                      FDT(I,J+1)=FDTBOT
                  ELSE IF(FDT(I,J).GE.0. .AND. FDT(I,J+1).GT.0.) THEN                      
                      FDT(I,J+1)=FDT(I,J)+(THLIQ(I,J)-THLMIN)*DELZW(I,J)  
                  ELSE IF(FDT(I,J).LT.0. .AND. FDT(I,J+1).LE.0.) THEN                      
                      FDT(I,J)=FDT(I,J+1)-(THLIQ(I,J)-THLMIN)*DELZW(I,J)  
                  ENDIF
                  THTEST(I,J)=THLMIN
                  IF(J.LT.IG) THEN
                      IF(DELZW(I,J+1).GT.0.0) THTEST(I,J+1)=THLIQ(I,J+1)
     1                    +(FDT(I,J+1)-FDT(I,J+2))/DELZW(I,J+1)
                  ENDIF
                  IF(J.GT.1) THEN
                      IF(DELZW(I,J-1).GT.0.0) THTEST(I,J-1)=THLIQ(I,J-1)
     1                    +(FDT(I,J-1)-FDT(I,J))/DELZW(I,J-1)
                  ENDIF
              ENDIF                                                                   
          ELSE
              THTEST(I,J)=0.0
          ENDIF
  400 CONTINUE               
C
      DO 500 J=IG,1,-1                                                            
      DO 500 I=IL1,IL2
          IF(IGRD(I).GT.0 .AND. THTEST(I,J).GT.THLMAX(I,J))         THEN
             WLIMIT=MAX((THLMAX(I,J)-THLIQ(I,J)),0.0)*DELZW(I,J)                      
             WEXCES(I)=(THTEST(I,J)-THLMAX(I,J))*DELZW(I,J)                                
             IF(FDT(I,J).GE.0. .AND. FDT(I,J+1).LE.0.)        THEN                          
                IF(FDT(I,J).GE.WLIMIT)             THEN                                       
                   FDT(I,J)=WLIMIT                                               
                   FDT(I,J+1)=0.0                                                
                ELSE                                                            
                   FDT(I,J+1)=FDT(I,J)-WLIMIT                                      
                ENDIF                                                           
             ELSE IF(FDT(I,J).GT.0. .AND. FDT(I,J+1).GE.0.)   THEN                      
                FDT(I,J)=FDT(I,J)-WEXCES(I)                                            
             ELSE IF(FDT(I,J).LE.0. .AND. FDT(I,J+1).LT.0.)   THEN                      
                FDT(I,J+1)=FDT(I,J+1)+WEXCES(I)                                        
             ENDIF                                                               
             IF(DELZW(I,1).GT.0.0)                            THEN
                 THTEST(I,1)=THLIQ(I,1)+(FDT(I,1)-FDT(I,2))/DELZW(I,1)                    
             ENDIF
             IF(DELZW(I,2).GT.0.0)                            THEN
                 THTEST(I,2)=THLIQ(I,2)+(FDT(I,2)-FDT(I,3))/DELZW(I,2)
             ENDIF
             IF(DELZW(I,3).GT.0.0)                            THEN
                 THTEST(I,3)=THLIQ(I,3)+(FDT(I,3)-FDT(I,4))/DELZW(I,3)
             ENDIF
         ENDIF                                                                   
  500 CONTINUE
C
      IPTBAD=0
      DO 600 I=IL1,IL2
          IF(IGRD(I).GT.0)                                        THEN
              IF(FDT(I,IG+1).LT.0.)                         THEN
                  WEXCES(I)=-FDT(I,IG+1)
                  FDT(I,1)=FDT(I,1)+WEXCES(I)
                  FDT(I,2)=FDT(I,2)+WEXCES(I)
                  FDT(I,3)=FDT(I,3)+WEXCES(I)
                  FDT(I,4)=FDT(I,4)+WEXCES(I)                                                
                  THSUBL=WEXCES(I)*RHOW/(RHOICE*DELZW(I,1))                                     
                  IF(THLIQ(I,J).GT.0.0)               THEN
                      TBARW(I,1)=TBARW(I,1)-(CLHMLT*RHOICE*THSUBL)/
     1                           (HCPW*THLIQ(I,1))                
                  ENDIF
                  IF(THSUBL.LE.THICE(I,1))            THEN
                      THICE(I,1)=THICE(I,1)-THSUBL                                        
                  ELSE
                      THSUBL=THSUBL-THICE(I,1)
                      THICE(I,1)=0.0
                      QFG(I)=QFG(I)-FCT(I)*THSUBL*RHOICE*DELZW(I,1)/
     1                       DELT
                      WLOST(I)=WLOST(I)+THSUBL*RHOICE*DELZW(I,1)
                  ENDIF
                  IF(THICE(I,1).LT.0.0) IPTBAD=I
              ENDIF                                                                       
C
C     * CALCULATE DRAINAGE FROM BOTTOM OF SOIL COLUMN AND RE-EVALUATE
C     * SOIL LAYER TEMPERATURES AND LIQUID MOISTURE CONTENTS AFTER
C     * WATER MOVEMENT.
C
              TFDT(I,1)=TBARW(I,1)                                                            
              TFDT(I,IG+1)=TBARW(I,IG)
              RUNOFF(I)=RUNOFF(I)+FDT(I,IG+1)
          ENDIF
  600 CONTINUE
C
      IF(IPTBAD.NE.0)                                           THEN
          WRITE(6,6500) IPTBAD,JL,IVEG,THICE(IPTBAD,1)
          CALL XIT('GRDRAN',-3)
      ENDIF
C                                                      
      DO 700 J=1,IG
      DO 700 I=IL1,IL2
          IF(IGRD(I).GT.0)                                          THEN
             IF(J.LT.IG)                               THEN
                IF(FDT(I,J+1).GT.0.)          THEN                                                
                   TFDT(I,J+1)=TBARW(I,J)                                                  
                ELSE                                                                    
                   TFDT(I,J+1)=TBARW(I,J+1)                                                
                ENDIF                                                                   
             ENDIF
             IF(THTEST(I,J).GT.0.0)           THEN
                 TBARW(I,J)=(THLIQ(I,J)*TBARW(I,J)+(FDT(I,J)*TFDT(I,J)-
     1                      FDT(I,J+1)*TFDT(I,J+1))/DELZW(I,J))/
     2                      THTEST(I,J)                                       
             ENDIF
             THLIQ(I,J)=THTEST(I,J)
          ENDIF                                                      
  700 CONTINUE                                                                    
C                                                                                  
      RETURN                                                                      
      END        
