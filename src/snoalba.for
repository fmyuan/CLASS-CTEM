      SUBROUTINE SNOALBA(ALVSSN,ALIRSN,
     1                   ALBSNO,TRSNOW,ZSNOW,FSNOW,SAND,
     2                   ALVSI,ALIRI,ILG,IG,IL1,IL2,JL)
C
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         SPECIFY LOCATION OF ICE SHEETS
C     *                         BY SOIL TEXTURE ARRAY RATHER
C     *                         THAN BY SOIL COLOUR INDEX.
C     * NOV 29/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         CALL ABORT CHANGED TO CALL XIT TO 
C     *                         ENABLE RUNNING ON PC'S.
C     * MAR 13/92 - M.LAZARE.   CODE FOR MODEL VERSION GCM7 -
C     *                         DIVIDE PREVIOUS SUBROUTINE 
C     *                         "SNOALB" INTO "SNOALBA" AND
C     *                         "SNOALBW" AND VECTORIZE.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. DISAGGREGATE SNOW ALBEDO INTO
C     *                         VISIBLE AND NEAR-IR PORTIONS;
C     *                         CALCULATE TRANSMISSIVITY TO
C     *                         SHORTWAVE RADIATION.
C    
C     * OUTPUT ARRAYS.
C
      REAL ALVSSN(ILG),ALIRSN(ILG)
C
C     * INPUT ARRAYS.
C
      REAL ALBSNO(ILG),TRSNOW(ILG),ZSNOW(ILG),FSNOW(ILG),SAND(ILG,IG)
C------------------------------------------------------------------
      IPTBAD=0
      DO 100 I=IL1,IL2                                           
         IF(ALBSNO(I).LT.0.50.AND.ALBSNO(I).GT.0.499) ALBSNO(I)=0.50                      
         IF(FSNOW(I).GT.0. .AND. NINT(SAND(I,1)).NE.-4) THEN  
            IF(ALBSNO(I).GT.0.70) THEN                                             
               TIMFAC=(ALBSNO(I)-0.70)/0.14                                       
               ALVSSN(I)=0.11*TIMFAC+0.84                                         
               ALIRSN(I)=0.16*TIMFAC+0.56                                         
            ELSE                                                                
               TIMFAC=(ALBSNO(I)-0.50)/0.34                                       
               ALVSSN(I)=0.34*TIMFAC+0.61                                         
               ALIRSN(I)=0.34*TIMFAC+0.38                                         
            ENDIF
            TRSNOW(I)=EXP(-25.0*ZSNOW(I))                                                 
         ELSE IF(FSNOW(I).GT.0.) THEN  
            ALVSSN(I)=ALVSI                                                        
            ALIRSN(I)=ALIRI
            TRSNOW(I)=EXP(-25.0*ZSNOW(I))                                                 
         ENDIF                                                                   
         IF(ALVSSN(I).GT.1.0.OR.ALVSSN(I).LT.0.0) IPTBAD=I
         IF(ALIRSN(I).GT.1.0.OR.ALIRSN(I).LT.0.0) IPTBAD=I
  100 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSSN(IPTBAD),ALIRSN(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSSN,ALIRSN = ',2F10.5)
         CALL XIT('SNOALBA',-1)
      ENDIF
C
      RETURN
      END
