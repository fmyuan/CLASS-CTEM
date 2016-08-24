      SUBROUTINE CHKWAT(IVEG,THLIQ,THICE,THLIQT,THICET,THPOR,
     1                  EVAP,FCS,FGS,FCT,RAICAN,SNOCAN,RUNOFF,
     2                  PCPR,RAIN,SNOW,WLOST,ZSNOW,RHOSNO,SNO,
     3                  ZPOND,ZPONDI,ISAND,DELZW,BAL,DELT,
     4                  IG,ILG,IL1,IL2,JL                   )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE USE OF WLOST.
C     *                         ALSO INTRODUCE NEW VALUE OF ACCLMT
C     *                         CORRESPONDING TO 3 MM/YR AS USED
C     *                         BY THE PILPS COMMUNITY.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * JAN 31/94 - D.VERSEGHY. LOCAL VERSION FOR CLIMATE RESEARCH
C     *                         NETWORK: CHECK RAICAN AND SNOCAN
C     *                         AGAINST -ACCLMT INSTEAD OF AGAINST 0.
C     * AUG 16/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         RETURN WATER BALANCE CHECK TO ROUTINE
C     *                         USE (COMMENTED OUT IN PREVIOUS VERSION)
C     *                         AND RENAME SUBROUTINE FROM "CHKVAL"
C     *                         TO "CHKWAT".
C     * MAY 15/92 - M.LAZARE.   CLASS - VERSION 2.1.
C     *                         MOISTURE BALANCE CHECKS EXTRACTED FROM
C     *                         "CLASSW" AND VECTORIZED.
C     * APR 11/89 - D.VERSEGHY. THE FOLLOWING MOISTURE BALANCE CHECKS 
C     *                         ARE CARRIED OUT: INTERCEPTED MOISTURE
C     *                         STORES AND LOCAL RUNOFF MUST BE .GE.0;
C     *                         LIQUID SOIL LAYER MOISTURE STORES MUST
C     *                         BE LESS THAN THE PORE VOLUME AND GREATER
C     *                         THAN THE LIMITING VALUE "THLMIN"; FROZEN
C     *                         SOIL LAYER MOISTURE STORES MUST BE LESS
C     *                         THAN THE MAXIMUM AVAILABLE VOLUME (THE
C     *                         PORE VOLUME - THLMIN) AND GE.0; AND THE
C     *                         MOISTURE BALANCE OF THE TOTAL CANOPY/
C     *                         SNOW/SOIL COLUMN MUST BE WITHIN A 
C     *                         SPECIFIED TOLERANCE.  THE TOLERANCE
C     *                         LEVEL ADOPTED IS DESIGNATED BY "ACCLMT".
C
C     * INPUT FIELDS.
C
      REAL THLIQ (ILG,IG), THICE (ILG,IG), THLIQT(ILG,IG),
     1     THICET(ILG,IG), THPOR (ILG,IG), DELZW (ILG,IG)

      REAL EVAP  (ILG),    FCS   (ILG),    FGS   (ILG),    FCT   (ILG),
     1     RAICAN(ILG),    SNOCAN(ILG),    RUNOFF(ILG),    PCPR  (ILG),
     2     RAIN  (ILG),    SNOW  (ILG),    WLOST (ILG),    ZSNOW (ILG),
     3     RHOSNO(ILG),    SNO   (ILG),    ZPOND (ILG),    ZPONDI(ILG)
C
      INTEGER              ISAND (ILG,IG)   
C
C     * WORK ARRAYS.
C
      REAL BAL   (ILG)  
C
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C
C      ACCLMT=3.0*DELT/3.1536E7
      ACCLMT=3.0*DELT/3.1536E3      
C-----------------------------------------------------------------------
      IF(IVEG.EQ.1 .OR. IVEG.EQ.3)                                  THEN      
          IPTBAD=0
          JPTBAD=0
      ENDIF
      KPTBAD=0
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)                   THEN
              IF(IVEG.EQ.1 .OR. IVEG.EQ.3)                   THEN      
                  IF(RAICAN(I).LT.(-1.0*ACCLMT)) IPTBAD=I
                  IF(SNOCAN(I).LT.(-1.0*ACCLMT)) JPTBAD=I
              ENDIF
              IF(RUNOFF(I).LT.0.0) KPTBAD=I
          ENDIF
  100 CONTINUE
C
      IF(IVEG.EQ.1 .OR. IVEG.EQ.3)                                  THEN      
          IF(IPTBAD.NE.0)                                    THEN
             WRITE(6,6100) IPTBAD,JL,IVEG,RAICAN(IPTBAD)
 6100        FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' RAICAN = ',
     1               E13.5)
             CALL XIT('CHKWAT',-1)
          ENDIF
          IF(JPTBAD.NE.0)                                           THEN
             WRITE(6,6150) JPTBAD,JL,IVEG,SNOCAN(JPTBAD)
 6150        FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' SNOCAN = ',
     1               E13.5)
             CALL XIT('CHKWAT',-2)
          ENDIF
      ENDIF
      IF(KPTBAD.NE.0)                                           THEN
         WRITE(6,6200) KPTBAD,JL,IVEG,RUNOFF(KPTBAD)
 6200    FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' RUNOFF = ',
     1           E13.5)
         CALL XIT('CHKWAT',-3)
      ENDIF
C
      IPTBDI=0
      JPTBDI=0
      KPTBDI=0
      LPTBDI=0
      DO 150 J=1,IG
      DO 150 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)                   THEN
              IF(J.GT.1) THEN
                  IF((THLIQ(I,J)-THPOR(I,J)).GT.ACCLMT)  THEN
                      IPTBDI=I
                      IPTBDJ=J
                  ENDIF
              ELSE
                  IF((THLIQ(I,J)-THPOR(I,J)-ZPONDI(I)/DELZW(I,J))
     1                          .GT.ACCLMT)  THEN
                      IPTBDI=I
                      IPTBDJ=J
                  ENDIF
              ENDIF
              IF(THLIQ(I,J).LT.(THLMIN-ACCLMT) .AND. ISAND(I,J).NE.-3)
     1                                               THEN
                  JPTBDI=I
                  JPTBDJ=J
              ENDIF
              IF((THICE(I,J)*RHOICE/RHOW-THPOR(I,J)+THLMIN).GT.ACCLMT
     1                      .AND.ISAND(I,J).NE.-3)   THEN 
                  KPTBDI=I
                  KPTBDJ=J
              ENDIF
              IF(THICE(I,J).LT.-1.*ACCLMT)           THEN
                  LPTBDI=I
                  LPTBDJ=J
              ENDIF
          ENDIF
  150 CONTINUE
C
      IF(IPTBDI.NE.0)                                               THEN
          WRITE(6,6250) IPTBDI,JL,IVEG,THLIQ(IPTBDI,IPTBDJ),
     1                  THPOR(IPTBDI,IPTBDJ),IPTBDJ
 6250     FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' THLIQ = ',
     1            E13.5,' THPOR = ',E13.5,' FOR J=',I2)
          CALL XIT('CHKWAT',-4)
      ENDIF
      IF(JPTBDI.NE.0)                                               THEN
          WRITE(6,6300) JPTBDI,JL,IVEG,THLIQ(JPTBDI,JPTBDJ),JPTBDJ
 6300     FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' THLIQ = ',
     1            E13.5,' FOR J=',I2)
          CALL XIT('CHKWAT',-5)
      ENDIF
      IF(KPTBDI.NE.0)                                               THEN
          WRITE(6,6350) KPTBDI,JL,IVEG,THICE(KPTBDI,KPTBDJ),
     1                  THPOR(KPTBDI,KPTBDJ),KPTBDJ
 6350     FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' THICE = ',
     1            E13.5,' THPOR = ',E13.5,' FOR J=',I2)
          CALL XIT('CHKWAT',-6)
      ENDIF
      IF(LPTBDI.NE.0)                                               THEN
          WRITE(6,6400) LPTBDI,JL,IVEG,THICE(LPTBDI,LPTBDJ),LPTBDJ
 6400     FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' THICE = ',
     1            E13.5,' FOR J=',I2)
          CALL XIT('CHKWAT',-7)
      ENDIF
C
      IPTBAD=0
      IF(IVEG.EQ.1 .OR. IVEG.EQ.3)                                  THEN
          CANFAC=1.0
      ELSE
          CANFAC=0.0
      ENDIF
 
      DO 275 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ISAND(I,1).GT.-4)                   THEN
             IF(IVEG.EQ.1 .OR. IVEG.EQ.2)                 THEN
                SNOFAC=1.0/(FCS(I)+FGS(I))
             ELSE
                SNOFAC=0.0
             ENDIF
             BAL(I)=PCPR(I)*DELT-                                               
     1                 EVAP(I)*RHOW*DELT-RUNOFF(I)*RHOW+ZPOND(I)*RHOW+                   
     2                 WLOST(I)+CANFAC*
     3                 (RAIN(I)-RAICAN(I)+SNOW(I)-SNOCAN(I))-                   
     4                 (THLIQ(I,1)-THLIQT(I,1))*RHOW*DELZW(I,1)-                       
     5                 (THLIQ(I,2)-THLIQT(I,2))*RHOW*DELZW(I,2)-                       
     6                 (THLIQ(I,3)-THLIQT(I,3))*RHOW*DELZW(I,3)-                       
     7                 (THICE(I,1)-THICET(I,1))*RHOICE*DELZW(I,1)-                     
     8                 (THICE(I,2)-THICET(I,2))*RHOICE*DELZW(I,2)-                     
     9                 (THICE(I,3)-THICET(I,3))*RHOICE*DELZW(I,3)-                     
     A                 ZSNOW(I)*RHOSNO(I)+SNOFAC*SNO(I)
             IF(ABS(BAL(I)).GT.ACCLMT)                           THEN
                 IPTBAD=I
             ENDIF  
          ENDIF
  275 CONTINUE

      IF(IPTBAD.NE.0)                                            THEN
          WRITE(6,6450) IPTBAD,JL,IVEG,BAL(IPTBAD)
          WRITE(6,6460) PCPR(IPTBAD)*DELT,EVAP(IPTBAD)*RHOW*DELT,
     1        RUNOFF(IPTBAD)*RHOW,ZPOND(IPTBAD)*RHOW,WLOST(IPTBAD),
     2        RAIN(IPTBAD)-RAICAN(IPTBAD),SNOW(IPTBAD)-SNOCAN(IPTBAD)
          WRITE(6,6460) 
     1        (THLIQ(IPTBAD,1)-THLIQT(IPTBAD,1))*RHOW*DELZW(IPTBAD,1),
     2        (THLIQ(IPTBAD,2)-THLIQT(IPTBAD,2))*RHOW*DELZW(IPTBAD,2),
     3        (THLIQ(IPTBAD,3)-THLIQT(IPTBAD,3))*RHOW*DELZW(IPTBAD,3),
     4        (THICE(IPTBAD,1)-THICET(IPTBAD,1))*RHOICE*DELZW(IPTBAD,1),
     5        (THICE(IPTBAD,2)-THICET(IPTBAD,2))*RHOICE*DELZW(IPTBAD,2),
     6        (THICE(IPTBAD,3)-THICET(IPTBAD,3))*RHOICE*DELZW(IPTBAD,3)
          WRITE(6,6460) ZSNOW(IPTBAD)*RHOSNO(IPTBAD),SNO(IPTBAD)
          WRITE(6,6470) FCS(IPTBAD),FGS(IPTBAD)
6450      FORMAT('0AT (I,JL)=(',I3,',',I3,'), IVEG=',I2,' BAL = ',
     1        E13.5)
6460      FORMAT(2X,7F15.8)
6470      FORMAT(2X,4E20.6)
          CALL XIT('CHKWAT',-8)
      ENDIF

      RETURN
      END  
