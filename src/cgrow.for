      SUBROUTINE CGROW(GROWTH,TBAR,TA,ILG,IL1,IL2,JL,IG)

C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * APR 11/89 - D.VERSEGHY. INCREMENT/DECREMENT GROWTH INDEX FOR
C     *                         VEGETATION TYPES 1 AND 2 (NEEDLELEAF
C     *                         AND BROADLEAF TREES).
C                                      
C     * OUTPUT ARRAY.
C
      REAL GROWTH(ILG)
C
C     * INPUT ARRAYS.
C                                           
      REAL TBAR  (ILG,IG)
C
      REAL TA    (ILG)
C                                                                                  
      COMMON /CLASS1/ DELT,TFREZ        !DELT 1800; TFREZ 273.16                                          
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(GROWTH(I).LT.0.)                                       THEN                                                      
              GROWTH(I)=MIN(0.0,(GROWTH(I)+DELT/5.184E6))                                 
          ELSE                                                                        
              IF(TA(I).GT.TFREZ .AND. TBAR(I,1).GT.TFREZ)     THEN                             
                  GROWTH(I)=MIN(1.0,(GROWTH(I)+DELT/5.184E6))                             
              ELSE IF(TA(I).LE.TFREZ .OR. TBAR(I,1).LE.TFREZ) THEN                          
                  IF(GROWTH(I).GE.0.10)     THEN                                             
                      GROWTH(I)=-GROWTH(I)                                                  
                  ELSE                                                                
                      GROWTH(I)=0.0                                                      
                  ENDIF                                                               
              ENDIF                                                                   
          ENDIF
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END        
