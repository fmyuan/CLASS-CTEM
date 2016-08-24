      SUBROUTINE GRINFL(IVEG,THLIQ,THICE,TBARW,RUNOFF,QFG,WLOST,
     1                  IGRN,FCT,DT,BI,XDRAIN,EVAP,R,TR,TPOND,ZPOND,
     3                  THPORI,PSISTI,GRKSTI,GRKTLI,THLRTI,ZERO,
     4                  DELZW,ZBOTW,ISAND,IZERO,
     5                  IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     6                  ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     7                  DELZX,ZBOTX,PSIF,THLINF,GRKINF,FDT,TFDT,
     8                  FDUMMY,TDUMMY,ZRMDR,THLMAX,THTEST,THLDUM,
     9                  THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     A                  WEXCES,FDTBND,WADD,TADD,WADJ,TPONDW,DZF,
     B                  DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,
     C                  WABS,IGRD,IFILL,LZF,
     D                  NINF,IFIND,ITER,NEND,ISIMP              )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * APR 17/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         BUG FIX: INITIALIZE FDT AND TFDT
C     *                         TO ZERO.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).            
C     * APR 11/89 - D.VERSEGHY. UPDATE SOIL LAYER TEMPERATURES AND 
C     *                         LIQUID MOISTURE CONTENTS FOR 
C     *                         INFILTRATING CONDITIONS (I.E.
C     *                         PONDED WATER OR RAINFALL OCCURRING
C     *                         WITHIN CURRENT TIMESTEP).
C
C     * INPUT/OUTPUT FIELDS.
C
      REAL THLIQ (ILG,IG), THICE (ILG,IG), TBARW (ILG,IG)
C                        
      REAL RUNOFF(ILG),    QFG   (ILG),    WLOST (ILG)
C
      INTEGER              IGRN  (ILG)  
C
C     * INPUT FIELDS.
C
      REAL FCT   (ILG),    DT    (ILG),    BI    (ILG,IG), XDRAIN(ILG),
     1     EVAP  (ILG),    R     (ILG),    TR    (ILG),    TPOND (ILG),
     2     ZPOND (ILG),    THPORI(ILG,IG), PSISTI(ILG,IG),
     3     GRKSTI(ILG,IG), GRKTLI(ILG,IG), THLRTI(ILG,IG), ZERO  (ILG),
     4     DELZW (ILG,IG), ZBOTW (ILG,IG)
C  
      INTEGER              ISAND (ILG,IG), IZERO (ILG)
C
C     * WORK FIELDS (FOR ALL CALLED ROUTINES AS WELL).
C
      REAL ZMAT  (ILG,IGP2,IGP1)
C
      REAL WMOVE (ILG,IGP2),   TMOVE (ILG,IGP2)
C
      REAL THLIQX(ILG,IGP1),   THICEX(ILG,IGP1),   TBARWX(ILG,IGP1),
     1     DELZX (ILG,IGP1),   ZBOTX (ILG,IGP1),   PSIF  (ILG,IGP1),
     2     THLINF(ILG,IGP1),   GRKINF(ILG,IGP1),   FDT   (ILG,IGP1),
     3     TFDT  (ILG,IGP1),   FDUMMY(ILG,IGP1),   TDUMMY(ILG,IGP1),
     4     ZRMDR (ILG,IGP1)
C
      REAL THLMAX(ILG,IG),     THTEST(ILG,IG),     THLDUM(ILG,IG),
     1     THIDUM(ILG,IG),     TDUMW (ILG,IG)
C
      REAL TRMDR (ILG),    ZF    (ILG),    FMAX  (ILG),    TUSED (ILG),
     1     RDUMMY(ILG),    WEXCES(ILG),    FDTBND(ILG),    WADD  (ILG),
     2     TADD  (ILG),    WADJ  (ILG),    TPONDW(ILG),    DZF   (ILG),
     3     DTFLOW(ILG),    THLNLZ(ILG),    THLQLZ(ILG),    DZDISP(ILG),
     4     WDISP (ILG),    WABS  (ILG)  
C
      INTEGER              IGRD  (ILG),    IFILL (ILG),    LZF   (ILG),
     1                     NINF  (ILG),    IFIND (ILG),    ITER  (ILG),
     2                     NEND  (ILG),    ISIMP (ILG)    
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * DETERMINE POINTS WHICH SATISFY CONDITIONS FOR THESE CALCULATIONS
C     * AND STORE THEM AS HAVING NON-ZERO VALUES FOR WORK ARRAY "IGRN".
C
      DO 50 I=IL1,IL2
          IF(FCT (I).GT.0. .AND. 
     1       ISAND(I,1).GT.-4 .AND. DT(I).GT.0. .AND.
     2       (R(I).GT.0. .OR. ZPOND(I).GT.0.))                     THEN
              IGRN(I)=1
              IFILL(I)=0
              RDUMMY(I)=0.
          ELSE
              IGRN(I)=0
              IFILL(I)=0
          ENDIF
   50 CONTINUE
C
C     * INITIALIZATION; DETERMINATION OF SOIL HYDRAULIC CONDUCTIVITIES
C     * AND SOIL MOISTURE SUCTION ACROSS WETTING FRONT.
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              THLPOT=THPORI(I,J)-THICE(I,J)*RHOICE/RHOW                                  
              THLTLD=THLRTI(I,J)*THPORI(I,J)                                                
              THLIQX(I,J)=THLIQ(I,J)                                                      
              THICEX(I,J)=THICE(I,J)                                                      
              TBARWX(I,J)=TBARW(I,J)                                                      
              DELZX(I,J)=DELZW(I,J)                                                        
              ZBOTX(I,J)=ZBOTW(I,J)                                                        
              FDT (I,J)=0.0
              TFDT(I,J)=0.0
              IF(THLIQ(I,J).GT.MIN(THLTLD,THLPOT))        THEN                               
                  THLINF(I,J)=MAX(THLIQ(I,J),THLMIN)                                    
                  GRKINF(I,J)=MIN(GRKSTI(I,J)*(THLINF(I,J)/THPORI(I,J))
     1                        **(2.*BI(I,J)+3.), GRKSTI(I,J))
              ELSE                                                                    
                  IF(THICE(I,J).GT.0.)              THEN                                            
                     THLINF(I,J)=MIN(THLTLD,MAX(THLPOT,THLMIN))                    
                     GRKINF(I,J)=MIN(GRKSTI(I,J)*(THLINF(I,J)/
     1                     THPORI(I,J))**(2.*BI(I,J)+3.), GRKTLI(I,J))
                  ELSE                                                                
                     THLINF(I,J)=THLTLD                                                
                     GRKINF(I,J)=GRKTLI(I,J)  
                  ENDIF                                                               
              ENDIF
          ENDIF
  100 CONTINUE
C
      DO 150 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              THLIQX(I,IG+1)=THLIQX(I,IG)                                                     
              THICEX(I,IG+1)=THICEX(I,IG)                                                     
              TBARWX(I,IG+1)=TBARWX(I,IG)                                                     
              IF(XDRAIN(I).GT.0.0) THEN
                  DELZX(I,IG+1)=99999.9                                                        
              ELSE
                  DELZX(I,IG+1)=0.0
              ENDIF
              ZBOTX (I,IG+1)=999999.                                                         
              FDT   (I,IG+1)=0.0
              TFDT  (I,IG+1)=0.0
              THLINF(I,IG+1)=THLINF(I,IG)                                                     
              GRKINF(I,IG+1)=GRKINF(I,IG)*XDRAIN(I)
          ENDIF
  150 CONTINUE
C                                                 
      DO 200 J=1,IG
      DO 200 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
             IF(THPORI(I,J).GT.0.)                      THEN
                 PSIINF=MAX(PSISTI(I,J)*(THLINF(I,J)/THPORI(I,J))**
     1                        (-BI(I,J)),PSISTI(I,J))                    
                 GRK=MIN(GRKSTI(I,J)*(THLIQ(I,J)/THPORI(I,J))**
     1                     (2.*BI(I,J)+3.),GRKSTI(I,J))                   
                 PSI=MAX(PSISTI(I,J)*(THLIQ(I,J)/THPORI(I,J))**
     1                     (-BI(I,J)),PSISTI(I,J))
             ELSE
                 PSIINF=PSISTI(I,J)
                 GRK=GRKSTI(I,J)
                 PSI=PSISTI(I,J)
             ENDIF
             IF(THLINF(I,J).GT.THLIQ(I,J))                  THEN 
                PSIF(I,J)=MAX(BI(I,J)*(GRKINF(I,J)*PSIINF-GRK*PSI)/
     1                    (GRKSTI(I,J)*(BI(I,J)+3.)), 0.0) 
             ELSE                                                                    
                PSIF(I,J)=0.0                                                         
             ENDIF                                                                   
          ENDIF
  200 CONTINUE
C
      DO 250 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
             PSIF(I,IG+1)=PSIF(I,IG)      
             TRMDR(I)=DELT
          ELSE
             TRMDR(I)=0. 
          ENDIF
  250 CONTINUE
C    
      DO 300 J=1,IGP2
      DO 300 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN 
             WMOVE(I,J)=0.0                 
             TMOVE(I,J)=0.0
          ENDIF
  300 CONTINUE
C
C     * DETERMINE STARTING POSITION OF WETTING FRONT; INITIALIZATION
C     * FOR SATURATED INFILTRATION.
C     * (FOR FOLLOWING LOOPS VECTORIZATION AND OPTIMIZATION ARE ENHANCED
C     * BY UNROLLING THE INNER-MOST LOOP. THIS EXPLICITLY ASSUMES THAT
C     * IG=3 AND THE ROUTINE IS ABORTED IF SUCH IS NOT THE CASE.)
C
      IF(IG.NE.3)                      CALL XIT('GRINFL',-1)
      DO 400 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN 
             IF(THLIQ(I,1).GE.THLINF(I,1) .AND.
     1          THLIQ(I,2).GE.THLINF(I,2) .AND.                     
     2          THLIQ(I,3).GE.THLINF(I,3))                   THEN                                             
                ZF(I)=ZBOTW(I,3)                                                              
                LZF(I)=4                                                                   
                NINF(I)=5                                                                  
                WMOVE(I,2)=THLIQ(I,1)*DELZW(I,1)                                         
                TMOVE(I,2)=TBARW(I,1)                                                 
                WMOVE(I,3)=THLIQ(I,2)*DELZW(I,2)                                         
                TMOVE(I,3)=TBARW(I,2)
                WMOVE(I,4)=THLIQ(I,3)*DELZW(I,3)                                         
                TMOVE(I,4)=TBARW(I,3)
                TMOVE(I,NINF(I))=TBARWX(I,LZF(I))                                                 
                FMAX(I)=MIN(GRKINF(I,1),GRKINF(I,2),GRKINF(I,3))                               
             ELSE IF(THLIQ(I,1).GE.THLINF(I,1) .AND.
     1               THLIQ(I,2).GE.THLINF(I,2))              THEN                
                ZF(I)=ZBOTW(I,2) 
                LZF(I)=3                                                                   
                NINF(I)=4                                                                  
                WMOVE(I,2)=THLIQ(I,1)*DELZW(I,1)                                         
                TMOVE(I,2)=TBARW(I,1)                                                 
                WMOVE(I,3)=THLIQ(I,2)*DELZW(I,2)                                         
                TMOVE(I,3)=TBARW(I,2)
                TMOVE(I,NINF(I))=TBARWX(I,LZF(I))                                                 
                FMAX(I)=MIN(GRKINF(I,1),GRKINF(I,2))                                         
             ELSE IF(THLIQ(I,1).GE.THLINF(I,1))              THEN 
                ZF(I)=ZBOTW(I,1)  
                LZF(I)=2                                                                   
                NINF(I)=3                                                                  
                WMOVE(I,2)=THLIQ(I,1)*DELZW(I,1)                                               
                TMOVE(I,2)=TBARW(I,1)                                                       
                TMOVE(I,NINF(I))=TBARWX(I,LZF(I))                                                 
                FMAX(I)=GRKINF(I,1)                                                          
             ELSE IF(ZPOND(I).GT.0. .OR. GRKINF(I,1).LE.0.)  THEN                               
                ZF(I)=0.0                                                                  
                LZF(I)=1                                                                   
                NINF(I)=2                                                                  
                TMOVE(I,NINF(I))=TBARWX(I,LZF(I))                                                 
                FMAX(I)=999999.
             ELSE
                IFILL(I)=1
             ENDIF
          ENDIF
  400 CONTINUE
C
C     * IF SATURATED INFILTRATION CONDITIONS ARE NOT PRESENT AT ONCE
C     * (IFILL=1), CALL "WFILL" TO DO PROCESSING FOR PERIOD OF
C     * UNSATURATED INFILTRATION.
C                                                            
      CALL WFILL(WMOVE,TMOVE,ZF,TRMDR,LZF,NINF,
     1           PSIF,GRKINF,THLINF,THLIQX,TBARWX,DELZX,ZBOTX,
     2           R,TR,IFILL,
     3           IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     4           WADJ,WADD,TPONDW,DZF,IFIND                       )
C
      DO 500 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN 
              FMAX(I)=999999. 
          ENDIF                                                           
  500 CONTINUE
C
      DO 600 J=1,IGP1
      DO 600 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. LZF(I).GT.1 .AND. J.LT.LZF(I))     THEN   
              FMAX(I)=MIN(GRKINF(I,J),FMAX(I))                                      
          ENDIF                                                                   
  600 CONTINUE
C
C     * CALL "WFLOW" TO DO PROCESSING FOR PERIOD OF SATURATED
C     * INFILTRATION.
C
      CALL WFLOW(WMOVE,TMOVE,ZF,TRMDR,TPOND,ZPOND,FMAX,LZF,NINF,
     1           PSIF,GRKINF,THLINF,THLIQX,TBARWX,DELZX,ZBOTX,
     2           R,TR,EVAP,IGRN,
     3           IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     4           DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,
     5           WDISP,WABS,ITER,NEND,ISIMP       )
C
C     * RECALCULATE TEMPERATURES AND LIQUID MOISTURE CONTENTS OF
C     * SOIL LAYERS FOLLOWING INFILTRATION.
C
      CALL WEND(ZMAT,WMOVE,ZRMDR,THLIQX,TBARWX,FDT,TFDT,
     1          THLMAX,THTEST,THLDUM,THIDUM,TDUMW,RUNOFF,NINF,
     2          TMOVE,THICEX,THLINF,DELZX,FCT,TRMDR,BI,XDRAIN,
     3          THPORI,PSISTI,GRKSTI,ZERO,DELZW,ISAND,IGRN,LZF,
     4          IZERO,IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5          TUSED,RDUMMY,WEXCES,FDTBND,WADD,TADD,IGRD        )
C
      DO 800 J=1,IG
      DO 800 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              THLIQ(I,J)=THLIQX(I,J)                                                      
              THICE(I,J)=THICEX(I,J)                                                      
              TBARW(I,J)=TBARWX(I,J)      
          ENDIF                                                
  800 CONTINUE
C
C     * IF TIME REMAINS IN THE CURRENT MODEL STEP AFTER INFILTRATION
C     * HAS CEASED (TRMDR>0), CALL "GRDRAN" TO CALCULATE WATER FLOWS
C     * BETWEEN LAYERS FOR THE REMAINDER OF THE TIME STEP.
C
      CALL GRDRAN(IVEG,THLIQ,THICE,TBARW,THLMAX,THTEST,
     1            FDUMMY,TDUMMY,RUNOFF,QFG,WLOST,
     2            FCT,TRMDR,BI,XDRAIN,EVAP,ZERO,ZERO,THPORI,
     3            PSISTI,GRKSTI,DELZW,ISAND,IZERO,
     4            IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5            WEXCES,IGRD                                          )
C
      RETURN                                                                      
      END       
