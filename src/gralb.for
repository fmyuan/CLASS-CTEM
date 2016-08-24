      SUBROUTINE GRALB(ALVSG,ALIRG,
     1                 SAND,ALVSU,ALIRU,THLIQ,FSNOW,FCMXU,
     2                 ALVSI,ALIRI,DELZW,ILG,IG,IL1,IL2,JL)
C
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CALCULATE SOIL ALBEDO FROM PERCENT
C     *                         SAND CONTENT RATHER THAN FROM COLOUR 
C     *                         INDEX.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         FIX BUG TO CALCULATE GROUND ALBEDO
C     *                         UNDER CANOPIES AS WELL AS OVER BARE
C     *                         SOIL.
C     * NOV 29/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         "CALL ABORT" CHANGED TO "CALL XIT",
C     *                         TO ENABLE RUNNING RUN ON PC'S.
C     * FEB 12/93 - D.VERSEGHY/M.LAZARE. INCREASE ALBDRY TO 0.45 FROM
C     *                                  0.35. 
C     * MAR 13/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CLASS - VERSION 2.0.
C     *                         CODE FOR MODEL VERSION GCM7U (WITH
C     *                         CANOPY). 
C     * APR 11/89 - D.VERSEGHY. CALCULATE VISIBLE AND NEAR-IR SOIL
C     *                         ALBEDOS BASED ON TEXTURE AND SURFACE
C     *                         WETNESS. (SET TO ICE ALBEDOS OVER
C     *                         CONTINENTAL ICE SHEETS.)
C                
C     * OUTPUT ARRAYS.
C
      REAL ALVSG (ILG),   ALIRG (ILG)
C
C     * INPUT ARRAYS.
C
      REAL ALVSU (ILG),   ALIRU (ILG),   FSNOW (ILG),   FCMXU (ILG),
     1     SAND  (ILG,IG),THLIQ (ILG,IG),DELZW (ILG,IG)
C--------------------------------------------------------------------
      IPTBAD=0
      DO 100 I=IL1,IL2
         IF(FSNOW(I).LT.1. .AND. NINT(SAND(I,1)).NE.-4)       THEN
            ALBWET=0.08+0.0022*SAND(I,1)
            FURB=FCMXU(I)*(1.-FSNOW(I))                                    
            ALBDRY=0.14+0.0036*SAND(I,1)
            IF(DELZW(I,1).LE.0.10) THEN
                THLZRO=THLIQ(I,1)
            ELSE
                THLZRO=MAX(0.0,0.5*(3.0*THLIQ(I,1)-THLIQ(I,2)))                                   
            ENDIF
            IF(THLZRO.GE.0.20) THEN                                                     
               ALBSOL=ALBWET                                                           
            ELSEIF(THLZRO.LE.0.04) THEN                                                 
               ALBSOL=ALBDRY                                                           
            ELSE                                                                        
               ALBSOL=THLZRO*(ALBWET-ALBDRY)/0.16+ALBDRY-0.25*(ALBWET-                 
     1                ALBDRY)                                                             
            ENDIF                                                                       
            ALVSG(I)=2.0*ALBSOL/3.0                                                        
            ALIRG(I)=2.0*ALVSG(I)                                                             
            ALVSG(I)=((1.0-FSNOW(I)-FURB)*ALVSG(I)+FURB*ALVSU(I))/
     1          (1.0-FSNOW(I))
            ALIRG(I)=((1.0-FSNOW(I)-FURB)*ALIRG(I)+FURB*ALIRU(I))/
     1          (1.0-FSNOW(I))
            IF(ALVSG(I).GT.1.0.OR.ALVSG(I).LT.0.0) IPTBAD=I
            IF(ALIRG(I).GT.1.0.OR.ALIRG(I).LT.0.0) IPTBAD=I
         ELSE IF(FSNOW(I).LT.1. .AND. NINT(SAND(I,1)).EQ.-4)    THEN
            ALVSG(I)=ALVSI
            ALIRG(I)=ALIRI
         ENDIF     
  100 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSG(IPTBAD),ALIRG(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSG,ALIRG = ',2F10.5)
         CALL XIT('GRALB',-1)
      ENDIF

      RETURN                                                                      
      END
