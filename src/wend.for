      SUBROUTINE WEND(ZMAT,WMOVE,ZRMDR,THLIQX,TBARWX,FDT,TFDT,
     1                THLMAX,THTEST,THLDUM,THIDUM,TDUMW,RUNOFF,NINF,
     2                TMOVE,THICEX,THLINF,DELZX,FCT,TRMDR,BI,XDRAIN,
     3                THPORI,PSISTI,GRKSTI,ZERO,DELZW,
     4                ISAND,IGRN,LZF,IZERO,
     5                IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     6                TUSED,RDUMMY,WEXCES,FDTBND,WADD,TADD,IGRD)
C                                                                                 
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * APR 24/92 - D.VERSEGHY,M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. RECALCULATE LIQUID MOISTURE CONTENT
C     *                         OF SOIL LAYERS AFTER INFILTRATION
C     *                         AND EVALUATE FLOW ("RUNOFF") FROM
C     *                         BOTTOM OF SOIL COLUMN.
C
C     * INPUT/OUTPUT FIELDS.
C
      REAL ZMAT  (ILG,IGP2,IGP1), WMOVE (ILG,IGP2), ZRMDR (ILG,IGP1),
     1     THLIQX(ILG,IGP1),      TBARWX(ILG,IGP1), FDT   (ILG,IGP1),
     2     TFDT  (ILG,IGP1)
C
      REAL THLMAX(ILG,IG), THTEST(ILG,IG), THLDUM(ILG,IG),
     1     THIDUM(ILG,IG), TDUMW (ILG,IG)          
C
      REAL RUNOFF(ILG)
C
      INTEGER              NINF  (ILG)
C
C     * INPUT FIELDS.
C
      REAL TMOVE (ILG,IGP2),      THICEX(ILG,IGP1), THLINF(ILG,IGP1),
     1     DELZX (ILG,IGP1)
C 
      REAL FCT   (ILG),    TRMDR (ILG),    BI    (ILG,IG), XDRAIN(ILG),
     1     THPORI(ILG,IG), PSISTI(ILG,IG), GRKSTI(ILG,IG), ZERO  (ILG),
     2     DELZW (ILG,IG)
C  
      INTEGER              ISAND (ILG,IG), IGRN  (ILG),    LZF   (ILG),
     1                     IZERO (ILG) 
C
C     * WORK FIELDS (INCLUDING THOSE FOR GRDRAN CALLED HERE).
C
      REAL TUSED (ILG),    RDUMMY(ILG),    WEXCES(ILG),    FDTBND(ILG),
     1     WADD  (ILG),    TADD  (ILG) 
C
      INTEGER              IGRD  (ILG)
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C
C     * INITIALIZATION.
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. LZF(I).LE.IG)                       THEN
              THLDUM(I,J)=THLIQX(I,J)                                                 
              THIDUM(I,J)=THICEX(I,J)                                                 
              TDUMW (I,J)=TBARWX(I,J)                  
          ENDIF
  100 CONTINUE 
C
C     * DETERMINE AMOUNT OF TIME OUT OF CURRENT MODEL STEP DURING WHICH 
C     * INFILTRATION WAS OCCURRING.
C     * SET WORK ARRAY "TUSED" TO ZERO FOR POINTS WHERE WETTING FRONT
C     * IS BELOW BOTTOM OF LOWEST SOIL LAYER TO SUPPRESS CALCULATIONS
C     * DONE IN "GRDRAN".
C
      DO 125 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. LZF(I).LE.IG)                       THEN
              TUSED(I)=DELT-TRMDR(I)
          ELSE
              TUSED(I)=0.
          ENDIF
  125 CONTINUE
C
C     * CALL "GRDRAN" WITH COPIES OF CURRENT LIQUID AND FROZEN SOIL
C     * MOISTURE CONTENTS AND LAYER TEMPERATURES TO DETERMINE MOISTURE
C     * FLOW BETWEEN LAYERS BELOW THE WETTING FRONT.
C
      CALL GRDRAN(IVEG,THLDUM,THIDUM,TDUMW,THLMAX,THTEST,
     1            FDT,TFDT,RDUMMY,RDUMMY,RDUMMY,
     2            FCT,TUSED,BI,XDRAIN,ZERO,ZERO,ZERO,THPORI,
     3            PSISTI,GRKSTI,DELZW,ISAND,IZERO,
     4            IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5            WEXCES,IGRD                              )
C
C     * CONSISTENCY CHECKS ON FLOWS INTO AND OUT OF SOIL LAYER 
C     * CONTAINING WETTING FRONT.
C
      DO 150 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. LZF(I).LE.IG)                       THEN
              FDT(I,LZF(I)+1)=MAX(FDT(I,LZF(I)+1),0.0)
              FDTBND(I)=FDT(I,LZF(I))                                                             
              IF(LZF(I).GT.1)                              THEN 
                  IF(FDTBND(I).GT.0. .AND. THLIQX(I,LZF(I)-1).LE.THLMIN)
     1               FDTBND(I)=0.0                
              ENDIF
          ENDIF
C
C     * INITIALIZATION OF ARRAYS IN PREPARATION FOR RE-ALLOCATION OF
C     * MOISTURE STORES WITHIN SOIL LAYERS.
C
          IF(IGRN(I).GT.0)                                       THEN
               NINF(I)=MIN(NINF(I),IGP1)                                                        
          ENDIF                                                                       
  150 CONTINUE
C
      DO 200 J=IGP1,1,-1                                                           
      DO 200 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              ZRMDR(I,J)=DELZX(I,J)
              IF(J.LE.LZF(I))                  THEN
                  FDT(I,J)=0.0
              ENDIF
          ENDIF
  200 CONTINUE
C
      DO 300 J=1,IGP1                                                             
      DO 300 K=1,IGP1
      DO 300 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                       THEN
              ZMAT(I,K,J)=0.0                                                       
          ENDIF
  300 CONTINUE                        
C
C     * ASSIGN VALUES IN MATRIX "ZMAT": DETERMINE DEPTH OUT OF EACH
C     * SOIL LAYER J WHICH IS FILLED BY WATER FROM RESERVOIR K
C     * IN "WMOVE"; FIND THE DEPTH "ZRMDR" LEFT OVER WITHIN EACH
C     * SOIL LAYER.
C
      DO 400 K=1,IGP1
      DO 400 J=1,IGP1
      DO 400 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                       THEN
              IF(ZRMDR(I,J).GT.0. .AND. WMOVE(I,K).GT.0.) THEN                        
                  ZMAT(I,K,J)=WMOVE(I,K)/THLINF(I,J)                                    
                  IF(ZMAT(I,K,J).GE.ZRMDR(I,J)) THEN                                  
                      ZMAT(I,K,J)=ZRMDR(I,J)                                          
                      WMOVE(I,K)=WMOVE(I,K)-ZRMDR(I,J)*THLINF(I,J)                        
                      ZRMDR(I,J)=0.0                                                
                  ELSE                                                            
                      ZRMDR(I,J)=ZRMDR(I,J)-ZMAT(I,K,J)                                 
                      WMOVE(I,K)=0.0                                                
                  ENDIF                                                           
              ENDIF                                                               
          ENDIF
  400 CONTINUE
C
C     * ADD WATER CONTENT AND TEMPERATURE CHANGES DUE TO INFILTRATION
C     * (WADD, TADD) AND DRAINAGE (WDRA, TDRA) TO WATER REMAINING IN
C     * EACH SOIL LAYER AFTER THESE PROCESSES (WREM, TREM).
C
      DO 600 J=IG,1,-1
          DO 500 I=IL1,IL2
              IF(IGRN(I).GT.0)                                      THEN
                  WADD(I)=0.
                  TADD(I)=0.
              ENDIF
  500     CONTINUE
C  
          DO 525 K=1,IGP1
          DO 525 I=IL1,IL2
              IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                   THEN
                  WADD(I)=WADD(I)+THLINF(I,J)*ZMAT(I,K,J)                                       
                  TADD(I)=TADD(I)+TMOVE(I,K)*THLINF(I,J)*ZMAT(I,K,J)
              ENDIF
  525     CONTINUE
C
          DO 550 I=IL1,IL2
              IF(IGRN(I).GT.0)                                  THEN
                 IF(ZRMDR(I,J).GT.0.)                 THEN 
                    WREM=THLIQX(I,J)*ZRMDR(I,J)                                             
                    TREM=TBARWX(I,J)*THLIQX(I,J)*ZRMDR(I,J)                                   
                 ELSE                                                                    
                    WREM=0.0                                                            
                    TREM=0.0                                                            
                 ENDIF                                                                   
                 IF(J.EQ.LZF(I))                      THEN    
                    THDRAN=THLIQX(I,J)+(FDTBND(I)-FDT(I,J+1))/
     1                     DELZW(I,J)                          
                    THINFL=(WADD(I)+WREM-FDT(I,J+1))/DELZW(I,J)                                 
                    IF(THINFL.LT.THDRAN)   THEN                                           
                       FDT(I,J)=THDRAN*DELZW(I,J)-WADD(I)-WREM+
     1                          FDT(I,J+1)                        
                    ENDIF                                                               
                 ENDIF                                                                   
                 WDRA=FDT(I,J)-FDT(I,J+1)                                                    
                 TDRA=FDT(I,J)*TFDT(I,J)-FDT(I,J+1)*TFDT(I,J+1)                                  
                 IF(DELZW(I,J).GT.0.0) THEN
                     THLIQX(I,J)=(WADD(I)+WREM+WDRA)/DELZW(I,J)
                 ENDIF
                 IF(THPORI(I,J).GE.THLMIN) THEN
                     THLIQX(I,J)=MAX(THLIQX(I,J),THLMIN)
                 ENDIF
                 IF(THLIQX(I,J).GT.0.0) THEN
                     TBARWX(I,J)=(TADD(I)+TREM+TDRA)/(THLIQX(I,J)*
     1                           DELZW(I,J))
                 ENDIF
              ENDIF                          
  550     CONTINUE
  600 CONTINUE
C
C     * CALCULATE FLOW OUT OF BOTTOM OF SOIL COLUMN DUE TO INFILTRATION
C     * AND GRAVITY DRAINAGE AND ADD TO "RUNOFF".
C
      DO 700 K=1,IGP1
      DO 700 I=IL1,IL2
          IF(IGRN(I).GT.0)                                      THEN
              IF(LZF(I).EQ.IGP1 .AND. K.LE.NINF(I))  THEN 
                  RUNOFF(I)=RUNOFF(I)+THLINF(I,IGP1)*ZMAT(I,K,IGP1)
              ELSE IF(K.EQ.IGP1)                     THEN
                  RUNOFF(I)=RUNOFF(I)+FDT(I,K)
              ENDIF                              
          ENDIF
  700 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
