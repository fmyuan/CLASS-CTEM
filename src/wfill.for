      SUBROUTINE WFILL(WMOVE,TMOVE,ZF,TRMDR,LZF,NINF,
     1                 PSIF,GRKINF,THLINF,THLIQX,TBARWX,DELZX,ZBOTX,
     2                 R,TR,IFILL,
     3                 IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     4                 WADJ,WADD,TPOND,DZF,IFIND                       )

C     * NOV 30/94 - M.LAZARE. BRACKET TERMS IN "WADJ" CALCULATION IN
C     *                       LOOP 200 TO AVOID OPTIMIZATION LEADING
C     *                       TO RE-ORDERING OF CALCULATION AND
C     *                       RARE VERY LARGE ITTERATION LIMITS.
C     * AUG 16/93 - D.VERSEGHY/M.LAZARE. ADD MISSING OUTER LOOP ON "J"
C     *                                  IN 200 LOOP.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. UNSATURATED FLOW OF WATER INTO SOIL.            
C
C     * INPUT/OUTPUT FIELDS.
C                      
      REAL WMOVE (ILG,IGP2),   TMOVE (ILG,IGP2)
C
      REAL ZF    (ILG),        TRMDR (ILG)
C
      INTEGER                  LZF   (ILG),        NINF  (ILG) 
C
C     * INPUT FIELDS.
C
      REAL PSIF  (ILG,IGP1),   GRKINF(ILG,IGP1),   THLINF(ILG,IGP1),
     1     THLIQX(ILG,IGP1),   TBARWX(ILG,IGP1),   DELZX (ILG,IGP1),
     2     ZBOTX (ILG,IGP1) 
C
      REAL R     (ILG),        TR    (ILG)
C
      INTEGER                  IFILL (ILG)
C
C     * INTERNAL WORK FIELDS.
C
      REAL WADJ  (ILG),        WADD  (ILG),        TPOND (ILG),
     1     DZF   (ILG)
C
      INTEGER                  IFIND (ILG) 
C-----------------------------------------------------------------------
C     * INITIALIZATION.
C
      DO 50 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
             IFIND(I)=0 
             WADJ(I)=0.
          ENDIF
   50 CONTINUE
C                                                                    
C     * TEST SUCCESSIVE SOIL LAYERS TO FIND DEPTH OF WETTING FRONT 
C     * AT THE TIME PONDING BEGINS, I.E. AT THE TIME THE DECREASING
C     * INFILTRATION RATE EQUALS THE RAINFALL RATE.
C
      DO 100 J=1,IGP1
      DO 100 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. IFIND(I).EQ.0)                     THEN                                                             
              IF(GRKINF(I,J).GT.0. .AND. GRKINF(I,J).LT.R(I))  THEN                                           
                  ZF(I)=PSIF(I,J)/(R(I)/GRKINF(I,J)-1)                                  
                  IF(ZF(I).LT.(ZBOTX(I,J)-DELZX(I,J))) THEN                          
                      ZF(I)=ZBOTX(I,J-1)                                           
                      LZF(I)=J                                                   
                      IFIND(I)=1                                                 
                  ELSE IF(ZF(I).LT.ZBOTX(I,J))         THEN                                 
                      LZF(I)=J                                                   
                      IFIND(I)=1                                                 
                  ENDIF                                                       
              ELSE IF (GRKINF(I,J).GT.0. .AND. GRKINF(I,J).GE.R(I)) THEN
                  ZF(I)=ZBOTX(I,J)                                                 
                  LZF(I)=MIN(J+1,IGP1)                                          
              ELSE IF (GRKINF(I,J).LE.0.)                      THEN
                  ZF(I)=ZBOTX(I,J-1)                                                   
                  LZF(I)=J                                                           
                  IFIND(I)=1                                                         
              ENDIF                                                               
          ENDIF                                                                   
  100 CONTINUE
C
C     * FIND THE VOLUME OF WATER NEEDED TO CORRECT FOR THE DIFFERENCE 
C     * (IF ANY) BETWEEN THE LIQUID MOISTURE CONTENTS OF THE LAYERS 
C     * OVERLYING THE WETTING FRONT AND THAT OF THE LAYER CONTAINING 
C     * THE WETTING FRONT.
C
      DO 200 J=1,IGP1
      DO 200 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. LZF(I).GT.1 .AND. J.LT.LZF(I))     THEN
              WADJ(I)=WADJ(I)+DELZX(I,J)*( (THLINF(I,J)-THLIQX(I,J)) -
     1                (THLINF(I,LZF(I))-THLIQX(I,LZF(I))) )
          ENDIF                                                    
  200 CONTINUE                                                                
C
C     * CALCULATE THE TIME TO PONDING, GIVEN THE DEPTH REACHED BY THE 
C     * WETTING FRONT AT THAT TIME.

      DO 250 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              TPOND(I)=(ZF(I)*(THLINF(I,LZF(I))-THLIQX(I,LZF(I)))+
     1                 WADJ(I))/R(I)                                 
              IF(ZF(I).GT.999990.) TPOND(I)=999999.                                             
C
C     * IN THE CASE WHERE THE TIME TO PONDING EXCEEDS OR EQUALS THE
C     * TIME REMAINING IN THE CURRENT MODEL STEP, RECALCULATE THE 
C     * ACTUAL DEPTH ATTAINED BY THE WETTING FRONT OVER THE CURRENT
C     * MODEL STEP; ASSIGN VALUES IN THE WATER MOVEMENT MATRIX.
C      
              IF(TPOND(I).GE.TRMDR(I))                 THEN 
                  TMOVE(I,1)=TR(I)                                                             
                  WMOVE(I,1)=R(I)*TRMDR(I)                                                        
                  WADD(I)=WMOVE(I,1)
              ENDIF
          ENDIF    
  250 CONTINUE
C
      DO 300 J=1,IGP1
      DO 300 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. TPOND(I).GE.TRMDR(I) .AND.
     1        WADD(I).GT.0.)                                        THEN
              THLADD=MAX(THLINF(I,J)-THLIQX(I,J),0.0)                           
              IF(THLADD.GT.0.)                      THEN                                          
                  DZF(I)=WADD(I)/THLADD                                             
              ELSE                                                            
                  DZF(I)=9999.                                                   
              ENDIF                                                           
              IF(DZF(I).GE.DELZX(I,J))              THEN                                        
                  WADD(I)=WADD(I)-THLADD*DELZX(I,J)                                   
              ELSE                                                            
                  LZF(I)=J                                                       
                  IF(J.EQ.1)                 THEN                                             
                      ZF(I)=DZF(I)                                                  
                  ELSE                                                        
                      ZF(I)=ZBOTX(I,J-1)+DZF(I)                                       
                  ENDIF                                                       
                  WADD(I)=0.                                                    
              ENDIF                                                           
          ENDIF                                                               
  300 CONTINUE
C                                                                
      DO 400 J=1,IGP1
      DO 400 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. TPOND(I).GE.TRMDR(I) .AND.
     1        J.LE.LZF(I))                                          THEN
              TMOVE(I,J+1)=TBARWX(I,J)                                                
              IF(J.EQ.LZF(I))                       THEN                                                   
                  WMOVE(I,J+1)=THLIQX(I,J)*DZF(I)                                        
              ELSE                                                                
                  WMOVE(I,J+1)=THLIQX(I,J)*DELZX(I,J)                                   
              ENDIF                                 
          ENDIF                              
  400 CONTINUE 
C
C     * IN THE CASE WHERE THE TIME TO PONDING IS LESS THAN THE TIME
C     * REMAINING IN THE CURRENT MODEL STEP, ACCEPT THE DEPTH OF THE
C     * WETTING FRONT FROM LOOP 100; ASSIGN VALUES IN THE WATER
C     * MOVEMENT MATRIX.
C
      DO 450 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. TPOND(I).LT.TRMDR(I))              THEN
              TMOVE(I,1)=TR(I)                                                             
              WMOVE(I,1)=R(I)*TPOND(I)                                                        
              IF(LZF(I).EQ.1)                             THEN 
                  DZF(I)=ZF(I)                                                              
              ELSE                                                                    
                  DZF(I)=ZF(I)-ZBOTX(I,LZF(I)-1)                                                 
              ENDIF
          ENDIF
  450 CONTINUE
C
      DO 500 J=1,IGP1
      DO 500 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. TPOND(I).LT.TRMDR(I) .AND.
     1        J.LE.LZF(I))                                          THEN
              TMOVE(I,J+1)=TBARWX(I,J)                                                
              IF(J.EQ.LZF(I))                      THEN                                                   
                  WMOVE(I,J+1)=THLIQX(I,J)*DZF(I)                                        
              ELSE                                                                
                  WMOVE(I,J+1)=THLIQX(I,J)*DELZX(I,J)                                   
              ENDIF                                 
          ENDIF                              
  500 CONTINUE                                                                
C
C     * CALCULATE TIME REMAINING IN CURRENT MODEL STEP AFTER
C     * UNSATURATED FLOW.
C
      DO 600 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              IF(TPOND(I).GE.TRMDR(I))              THEN    
                  TRMDR(I)=0.
              ELSE
                  TRMDR(I)=TRMDR(I)-TPOND(I)
              ENDIF
              NINF(I)=LZF(I)+1
          ENDIF               
  600 CONTINUE                                                 
C                                                                                  
      RETURN                                                                      
      END        
