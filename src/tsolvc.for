      SUBROUTINE TSOLVC(ISNOW,FCT,
     1                  TZERO,QCAN,GZERO,QMELTG,CDH,CDM,QSWNET,QLWOUT,
     2                  QTRANS,QSENS,QEVAP,EVAPC,EVAPG,RIB,
     3                  CRIB,CEVAP,TADP,CPHCHC,CPHCHG,TVIRTA,EPS,
     4                  QLWIN,TCAN,CONST,CMASS,TA,QA,VA,
     5                  CDOH,CDOM,ZOH,ZOM,PADRY,RHOAIR,GCONST,GCOEFF,
     6                  PRESSG,TGND,TRSNOW,PSIZRO,RC,FSVF,
     7                  FSNOWC,FRAINC,RAICAN,SNOCAN,CHCAP,QSWINV,QSWINI,
     8                  ALVISC,ALNIRC,ALVISG,ALNIRG,TRVISC,TRNIRC,
     9                  IWATER,IEVAP,ILW,
     A                  ILG,IL1,IL2,JL,  
     B                  TSTEP,TVIRTC,TVIRTG,QMELTC,RHZERO,RESID,RESIDL,
     C                  RESIDO,TZEROL,TZEROO,TCANL,
     D                  RAICNI,SNOCNI,CHCAPI,QLWOC,QLWOG,QSENSC,QSENSG,
     E                  QSWNC,QSWNG,QEVAPC,QEVAPG,TCANO,TRTOP,A,B,
     F                  CLIMIT,QZERO,RA,CFLUX,QSTOR,XEVAP,TCDP,
     G                  ZOMWRK,ZOHWRK,ITER,NITER,
C    -------------------- CTEM MODIFICATIONS ---------------------------\
     H                  AILCG,  FCANC,   RH, CO2CONC,  RMATCTEM,
     I                  THLIQ,   SAND, CLAY,      IG,     COSZS, 
     J                XDIFFUS,    ICC,   IC,   CO2I1,     CO2I2,
     K                  CTEM1,  CTEM2, SLAI, FCANCMX,     L2MAX,
     L               NOL2PFTS, CFLUXV,
C    --------------- CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW --------|
     M                  ANVEG, RMLVEG,                                   !)
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
     N                 CTEMN,XNUP_VEG, NRUB0, VCMAX0, !)
C     ----------------------------------------------------HSuo testing BELOW 
     O  		          GPP_VEG,JE_VEG,JC_VEG,JS_VEG, !NEP_VEG,RE_VEG,
     P                  IPAR_HS,FPAR_VEG,SM_HS,QSWV_HS,VMAXC_VEG,
     Q             VM_VEG,VMUNS_VEG,VMUNS1_VEG,VMUNS2_VEG,VMUNS3_VEG,
     R             CO2I_VEG,TGA_HS,KC_HS,KO_HS,TCAN_HS,FSEAS_HS,
     S QSWINV_HS,ALVISC_HS,QSWNVG_HS  )
C     * AILCG    - GREEN LAI FOR CARBON PURPOSES
C     * FCANC    - FRACTIONAL COVERAGE OF 9 CARBON PFTs
C     * RH       - RELATIVE HUMIDITY
C     * CO2CONC  - ATMOS. CO2 CONC. IN PPM
C     * RMATCTEM - FRACTION OF ROOTS IN EACH SOIL LAYER FOR EACH OF THE 8 PFTs
C                  FOR CARBON RELATED PURPOSES. 
C     * RCPHTSYN - STOMATAL RESISTANCE ESTIMATED BY THE PHTSYN SUBROUTINE, S/M
C     * COSZS    - COS OF SUN'S ZENITH ANGLE
C     * XDIFFUS  - FRACTION OF DIFFUSED RADIATION
C     * CO2I1    - INTERCELLULAR CO2 CONC.(PA) FOR THE SINGLE/SUNLIT LEAF 
C     * CO2I2    - INTERCELLULAR CO2 CONC.(PA) FOR THE SHADED LEAF
C     * CTEM1    - LOGICAL BOOLEAN FOR USING CTEM's STOMATAL RESISTANCE
C                  OR NOT
C     * CTEM2    - LOGICAL BOOLEAN FOR USING CTEM's STRUCTURAL ATTRIBUTES
C                  OR NOT
C     * SLAI     - STORAGE LAI. SEE PHTSYN SUBROUTINE FOR MORE DETAILS.
C     * FCANCMX  - MAX. FRACTIONAL COVERAGE OF CTEM PFTs
C     * L2MAX    - MAX. NUMBER OF LEVEL 2 CTEM PFTs
C     * NOL2PFTS - NUMBER OF LEVEL 2 CTEM PFTs
C     * ANVEG    - NET PHTOSYNTHETIC RATE, u-MOL/M^2/S, FOR CTEM's 8 PFTs
C     * RMLVEG   - LEAF MAINTENANCE RESP. RATE, u-MOL/M^2/S, FOR CTEM's 8 PFTs
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
C     * CTEMN    - SWITCH FOR VCMAX0 MODULATED BY RUBISCO-N CONTENT (INPUT)  
C     * VCMAX0   - RUBISCO-N AND CANOPY TEMPERATURE MODULATED (INPUT)  
C     * XNUP_VEG - EMPIRICAL REDUCTION FACTOR TO ROOT ION UPTAKE 
C                 BY LIGHT-LIMITED CANOPY (OUTPUT)
C
C    -------------------- CTEM MODIFICATIONS ---------------------------/
C
C     * MAR 01/03 - V.ARORA.    MAKE MODIFICATIONS TO CALL PHTSYN SUBROUTINE
C     *                         FROM WITHIN THIS SUBROUTINE SO THAT STOMATAL 
C     *                         RESISTANCE ESTIMATED BY PHTSYN CAN BE USED, 
C     *                         AND PHTOSYNTHETIC CO2 UPTAKE CAN BE ESTIMATED. 
C     * JUL 24/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         REPLACE BISECTION METHOD IN SURFACE 
C     *                         TEMPERATURE ITERATION SCHEME WITH 
C     *                         SECANT METHOD FOR FIRST TEN ITERATIONS.
C     * JUN 20/97 - D.VERSEGHY. PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.  ALSO, PASS SWITCH "ILW"
C     *                         THROUGH SUBROUTINE CALL, SPECIFYING 
C     *                         WHETHER QLWIN REPRESENTS INCOMING
C     *                         (ILW=1) OR NET (ILW=2) LONGWAVE
C     *                         RADIATION ABOVE THE GROUND.
C     * NOV 30/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         NEW DRAG COEFFICIENT AND RELATED FIELDS,
C     *                         NOW DETERMINED IN ROUTINE "DRCOEF".
C     * OCT 04/94 - D.VERSEGHY. CHANGE "CALL ABORT" TO "CALL XIT" TO
C     *                         ENABLE RUNNING ON PCS.
C     * JAN 24/94 - M.LAZARE.   UNFORMATTED I/O COMMENTED OUT IN LOOPS
C     *                         200 AND 600. 
C     * JUL 29/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         SUPPRESS DEW WHEN GROUND TEMPERATURE
C     *                         EXCEEDS DEWPOINT TEMPERATURE OF AIR IN
C     *                         CANOPY SPACE (REQUIRES PASSED INPUT
C     *                         ARRAY "CONST" AND INTERNAL WORK ARRAY
C     *                         "TCDP").
C     *                         ADD TRANSMISSION THROUGH SNOWPACK TO
C     *                         "QSWNET" FOR DIAGNOSTIC PURPOSES. 
C     * OCT 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. ITERATIVE TEMPERATURE CALCULATIONS 
C     *                         FOR VEGETATION CANOPY AND UNDERLYING
C     *                         SURFACE.
C
C     * OUTPUT ARRAYS.
C
      REAL TZERO (ILG),    QCAN  (ILG),    GZERO (ILG),    QMELTG(ILG),
     1     CDH   (ILG),    CDM   (ILG),    QSWNET(ILG),    QLWOUT(ILG),
     2     QTRANS(ILG),    QSENS (ILG),    QEVAP (ILG),    EVAPC (ILG),
     3     EVAPG (ILG),    RIB   (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FCT   (ILG),    CRIB  (ILG),    CEVAP (ILG),    TADP  (ILG),
     1     CPHCHC(ILG),    CPHCHG(ILG),    TVIRTA(ILG),    EPS   (ILG),
     2     QLWIN (ILG),    TCAN  (ILG),    CONST (ILG),
     3     CMASS (ILG),    TA    (ILG),    QA    (ILG),    VA    (ILG),
     4     CDOH  (ILG),    CDOM  (ILG),    ZOH   (ILG),    ZOM   (ILG),
     5     PADRY (ILG),    RHOAIR(ILG),    GCONST(ILG),    GCOEFF(ILG),
     6     PRESSG(ILG),    TGND  (ILG),    TRSNOW(ILG),    PSIZRO(ILG),
     7     RC    (ILG),    FSVF  (ILG),    FSNOWC(ILG),    FRAINC(ILG),
     8     RAICAN(ILG),    SNOCAN(ILG),    CHCAP (ILG),    QSWINV(ILG),
     9     QSWINI(ILG),    ALVISC(ILG),    ALNIRC(ILG),    ALVISG(ILG),
     A     ALNIRG(ILG),    TRVISC(ILG),    TRNIRC(ILG),
C    -------------------- CTEM MODIFICATIONS ---------------------------\
     B     QSWNVC(ILG)   
C    -------------------- CTEM MODIFICATIONS ---------------------------/
C
      INTEGER              IWATER(ILG),    IEVAP (ILG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL TSTEP (ILG),    TVIRTC(ILG),    TVIRTG(ILG),    QMELTC(ILG),
     1     RHZERO(ILG),    RESID (ILG),    RESIDL(ILG),    RESIDO(ILG),    
     2     TZEROL(ILG),    TZEROO(ILG),    TCANL (ILG),    RAICNI(ILG),
     3     SNOCNI(ILG),    CHCAPI(ILG),    QLWOC (ILG),    QLWOG (ILG),
     4     QSENSC(ILG),    QSENSG(ILG),    QSWNC (ILG),    QSWNG (ILG),
     5     QEVAPC(ILG),    QEVAPG(ILG),    TCANO (ILG),    TRTOP (ILG),
     6     A     (ILG),    B     (ILG),    CLIMIT(ILG),
     7     QZERO (ILG),    RA    (ILG),    CFLUX (ILG),    QSTOR (ILG),
     8     XEVAP (ILG),    TCDP  (ILG),    ZOMWRK(ILG),    ZOHWRK(ILG)
C
      INTEGER              ITER  (ILG),    NITER (ILG)
C
C    -------------------- CTEM MODIFICATIONS ---------------------------\
      REAL  AILCG(ILG,ICC),  FCANC(ILG,ICC),   CO2CONC(ILG),    RH(ILG),
     1       THLIQ(ILG,IG),    SAND(ILG,IG),   CLAY(ILG,IG), COSZS(ILG),    
     2        XDIFFUS(ILG),   RCPHTSYN(ILG), ANVEG(ILG,ICC),
     3      CO2I1(ILG,ICC),  CO2I2(ILG,ICC),       RMATCTEM(ILG,ICC,IG),
     4    FCANCMX(ILG,ICC), RMLVEG(ILG,ICC),   SLAI(ILG,ICC) 
C
      REAL    CFLUXV(ILG)

      INTEGER IC, IG, ICC, L2MAX, NOL2PFTS(IC)

      LOGICAL CTEM1, CTEM2 

C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
C
      LOGICAL CTEMN  
      REAL XNUP_VEG(ILG,ICC), NRUB0(ILG,ICC), VCMAX0(ILG,ICC)
C
C    -------------------- CTEM MODIFICATIONS ---------------------------/
C------------------------------------------------------------HSuo testing\
      REAL JE_VEG(ILG,ICC),JC_VEG(ILG,ICC),JS_VEG(ILG,ICC),
	1     IPAR_HS(ILG),FPAR_VEG(ILG,ICC),QSWV_HS(ILG),
	2     GPP_VEG(ILG,ICC),FSEAS_HS(ILG),  !NEP_VEG(ILG,ICC),RE_VEG(ILG,ICC),
     3     SM_HS(ILG,ICC),VM_VEG(ILG,ICC),VMUNS_VEG(ILG,ICC),
     4     VMUNS1_VEG(ILG,ICC),VMUNS2_VEG(ILG,ICC),VMUNS3_VEG(ILG,ICC),
	5     TGA_HS(ILG),CO2I_VEG(ILG,ICC),KO(ILG),KC(ILG),TCAN_HS(ILG),
	6   QSWINV_HS(ILG),ALVISC_HS(ILG),QSWNVG_HS(ILG),fPAR_HS(ILG) 

C------------------------------------------------------------HSuo testing/

      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,SLTHICK,BEEM,ALFAH,              
     1                FAC,GAMRH,GAMRM,VMIN                                        
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C
C
C-----------------------------------------------------------------------
C     * INITIALIZATION AND PRE-ITERATION SEQUENCE.
C
      DO 50 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              IF(ISNOW.EQ.0)                      THEN
                  TRTOP(I)=0.
              ELSE
                  TRTOP(I)=TRSNOW(I)
              ENDIF
              QSWNVG=QSWINV(I)*TRVISC(I)*(1.0-ALVISG(I)) 
              QSWNIG=QSWINI(I)*TRNIRC(I)*(1.0-ALNIRG(I))
C    -------------------- CTEM MODIFICATIONS ---------------------------\
C             MAKE QSWNVC AN ARRAY OF SIZE ILG
              QSWNVC(I)=QSWINV(I)*(1.0-ALVISC(I))-QSWNVG                                           
C    -------------------- CTEM MODIFICATIONS ---------------------------/
              QSWNIC=QSWINI(I)*(1.0-ALNIRC(I))-QSWNIG                                           
              QSWNG(I)=QSWNVG+QSWNIG                                                         
              QSWNC(I)=QSWNVC(I)+QSWNIC
              QTRANS(I)=QSWNG(I)*TRTOP(I)   
              QSWNG(I)=QSWNG(I)-QTRANS(I)  
              QMELTC(I)=0.0                                                                  
              QMELTG(I)=0.0                                                                  
              TCANO(I)=TCAN(I)
              !TCANO(I)=TA(I)    !HSuo, Apr.2012
              IF(ABS(TCAN(I)-TFREZ).LT.1.0E-8)             TCAN(I)=TFREZ

              TZERO(I)=TGND(I)
              TSTEP(I)=5.0
C                                                                                 
              IF(IWATER(I).GT.0 .OR. ISNOW.EQ.1)           THEN
                  RHZERO(I)=1.0                                                          
              ELSE                                                                    
                  RHZERO(I)=0.0                                                      
              ENDIF
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              IF(TCAN(I).GE.TFREZ) THEN                                             
                  CA=17.269                                                       
                  CB=35.86                                                        
              ELSE                                                                
                  CA=21.874                                                       
                  CB=7.66                                                         
              ENDIF  
              TCDP(I)=(CB*CONST(I)-CA*TFREZ)/(CONST(I)-CA)        
              RESID(I)=999999.
              RESIDL(I)=99999999.
              TZEROL(I)=999999.
              ITER(I)=1
              NITER(I)=1
          ENDIF
   50 CONTINUE
C
C    -------------------- CTEM MODIFICATIONS ---------------------------\
C    CALL PHOTOSYNTHESIS SUBROUTINE HERE TO GET A NEW ESTIMATE OF
C    RC BASED ON PHOTOSYNTHESIS
C
      IF(CTEM1)                                                    THEN
C
        CALL PHTSYN (  AILCG, FCANC,     TCAN, CO2CONC,  PRESSG,   FCT,     
     1                CFLUXV,    RH,   QSWNVC,      IC,   THLIQ,  SAND,
     2                    TA,  CLAY, RMATCTEM,   COSZS, XDIFFUS,   ILG,
     3                   IL1,   IL2,       IG,     ICC,   ISNOW,  SLAI, 
     4               FCANCMX,  CTEM1,   CTEM2,   L2MAX, NOL2PFTS,
     5              RCPHTSYN, CO2I1,    CO2I2,   ANVEG,  RMLVEG,          !)  
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
     6              CTEMN, XNUP_VEG,	  NRUB0,  VCMAX0, !)
C     ----------------------------------------------------HSuo testing BELOW 
     7  		 GPP_VEG,JE_VEG,JC_VEG,JS_VEG,TCAN_HS, !NEP_VEG,RE_VEG,
     8           IPAR_HS,fPAR_HS,SM_HS,TGA_HS,CO2I_VEG,KO_HS,KC_HS,
     9             VM_VEG,VMUNS_VEG,VMUNS1_VEG,VMUNS2_VEG,VMUNS3_VEG,
     A          VMAXC_VEG,FSEAS_HS)
	  

        DO 70 I =IL1,IL2
          RC(I)=RCPHTSYN(I)
!---------------------------------------------------HSuo testing        
          QSWV_HS(I) = QSWNVC(I)                  
          QSWINV_HS(I) = QSWINV(I)
		ALVISC_HS(I) = ALVISC(I)
		QSWNVG_HS(I) = QSWNVG  
!---------------------------------------------------HSuo testing        
70      CONTINUE 
C
      ENDIF
C
C    -------------------- CTEM MODIFICATIONS ---------------------------/
C
C     * ITERATION FOR SURFACE TEMPERATURE OF GROUND UNDER CANOPY.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS
C     * ON THE CURRENT LATITUDE CIRCLE(S).
C  
  100 CONTINUE  
C
      NUMIT=0
      DO 150 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(IEVAP(I).GT.0.AND.IWATER(I).LE.0.AND.ISNOW.NE.1) THEN
                  RHZERO(I)=EXP(CEVAP(I)/TZERO(I))                                         
              ENDIF
              IF(TZERO(I).GE.TFREZ)                        THEN
                  A(I)=17.269                                                                
                  B(I)=35.86                                                                 
              ELSE                                                                        
                  A(I)=21.874                                                                
                  B(I)=7.66                                                                  
              ENDIF                                                                       
            if (TZERO(I) .le. 200.0) TZERO(I) = 230.0
              WZERO=0.622*RHZERO(I)*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/ 
     1              (TZERO(I)-B(I)))/PADRY(I)           
              QZERO(I)=WZERO/(1.0+WZERO)                                                 
              QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              TVIRTG(I)=TZERO(I)*(1.0+0.61*QZERO(I))                                           
              TVIRTC(I)=TCAN(I)*(1.0+0.61*QA(I))                                               
              IF(TVIRTG(I).GT.TVIRTC(I))                   THEN 
                  QSENSG(I)=RHOAIR(I)*SPHAIR*1.9E-3*(TZERO(I)-TCAN(I))*
     1                      (TVIRTG(I)-TVIRTC(I))**0.333333
                  EVAPG (I)=RHOAIR(I)*1.9E-3*(QZERO(I)-QA(I))*
     1                      (TVIRTG(I)-TVIRTC(I))**0.333333            
                  IF(EVAPG(I).LT.0. .AND. TZERO(I).GE.TCDP(I))
     1            EVAPG(I)=0. 
              ELSE                                                                    
                  QSENSG(I)=0.0                                                          
                  EVAPG (I)=0.0                                                           
              ENDIF                                                                   
              QEVAPG(I)=CPHCHG(I)*EVAPG(I)                                                     
              IF(ILW.EQ.2) THEN
                  RESID(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                (QLWOC(I)-QLWOG(I))-QSENSG(I)-QEVAPG(I)-GZERO(I)
              ELSE
                  RESID(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
              ENDIF
              IF(ABS(RESID(I)).LT.1.0)                     ITER(I)=0
              IF(ABS(TSTEP(I)).LT.1.0E-4 .AND. ABS(RESID(I)).LT.
     1          (ABS(RESIDL(I))-0.01))                     ITER(I)=0
              IF(FCT(I).GT.0. .AND. NITER(I).EQ.50)        ITER(I)=-1
          ENDIF
C
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(NITER(I).EQ.1) THEN
                  TZEROO(I)=TZERO(I)
                  RESIDO(I)=RESID(I)
                  IF(RESID(I).GT.0.0) THEN
                      TZERO(I)=TZERO(I)+5.0
                  ELSE
                      TZERO(I)=TZERO(I)-5.0
                  ENDIF
              ELSEIF(NITER(I).LE.10) THEN
                  TZEROL(I)=TZERO(I)
                  RESIDL(I)=RESID(I)
                  TZERO(I)=(TZEROL(I)*RESIDO(I)-TZEROO(I)*RESIDL(I))/
     1                (RESIDO(I)-RESIDL(I)+0.000001)
C    -------------------- CTEM MODIFICATIONS ---------------------------\
                  IF(TZERO(I).LT.173.16) TZERO(I)=TZEROL(I)-1.0
C    -------------------- CTEM MODIFICATIONS ---------------------------/
                  IF(TZERO(I).GT.373.16) TZERO(I)=TZEROL(I)+1.0
                  TSTEP(I)=TZERO(I)-TZEROL(I)
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))   THEN 
                      TSTEP(I)=-TSTEP(I)/2.0                                                    
                  ENDIF
                  IF(TSTEP(I).GT.10.0) TSTEP(I)=10.0
                  IF(TSTEP(I).LT.-10.0) TSTEP(I)=-10.0
                  TZERO(I)=TZERO(I)+TSTEP(I)
                  RESIDL(I)=RESID(I)
              ENDIF
          ENDIF
C
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  150 CONTINUE
C
      IBAD=0
C
      DO 200 I=IL1,IL2
C          IF(FCT(I).GT.0. .AND. ITER(I).EQ.-1)                      THEN 
C              WRITE(6,6250) I,JL,RESID(I),TZERO(I),RIB(I),NITER(I)
C6250          FORMAT('0SUBCAN ITERATION LIMIT',3X,2I3,2F8.2,E12.4,I6)            
C              WRITE(6,6280) QSWNG(I),FSVF(I),QLWIN(I),QLWOC(I),QLWOG(I),
C     1            QSENSG(I),QEVAPG(I),GZERO(I),TCAN(I),TVIRTC(I),
C     2            TVIRTG(I)
C6280          FORMAT(2X,10F8.2)
C          ENDIF                                            
C    -------------------- CTEM MODIFICATIONS ---------------------------\
           IF(FCT(I).GT.0. .AND.(TZERO(I).LT.173.16  
     1      .OR.TZERO(I).GT.373.16)) THEN
C    -------------------- CTEM MODIFICATIONS ---------------------------/
              IBAD=I
           ENDIF
 200  CONTINUE
C
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6370) IBAD,JL,TZERO(IBAD),NITER(IBAD),ISNOW
 6370     FORMAT('0BAD GROUND ITERATION TEMPERATURE',3X,2I3,F16.2,2I4)
          CALL XIT('TSOLVC',-1)
      ENDIF
C
      IF(NUMIT.GT.0)                                    GO TO 100
C
C     * POST-ITERATION CLEAN-UP.
C
      DO 300 I=IL1,IL2
          GZERO(I)=GZERO(I)+RESID(I)
          IF(((IWATER(I).EQ.1 .AND. TZERO(I).LT.TFREZ) .OR. 
     1        (IWATER(I).EQ.2 .AND. TZERO(I).GT.TFREZ)) .AND. 
     2        FCT(I).GT.0.)                                         THEN
              TZERO(I)=TFREZ                                                             
              WZERO=0.622*611.0/PADRY(I)
              QZERO(I)=WZERO/(1.0+WZERO)
              QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              TVIRTG(I)=TZERO(I)*(1.0+0.61*QZERO(I))                                           
              TVIRTC(I)=TCAN(I)*(1.0+0.61*QA(I))                                               
              IF(TVIRTG(I).GT.TVIRTC(I))                   THEN 
                  QSENSG(I)=RHOAIR(I)*SPHAIR*1.9E-3*(TZERO(I)-TCAN(I))*
     1                      (TVIRTG(I)-TVIRTC(I))**0.333333
                  EVAPG (I)=RHOAIR(I)*1.9E-3*(QZERO(I)-QA(I))*
     1                      (TVIRTG(I)-TVIRTC(I))**0.333333            
              ELSE                                                                    
                  QSENSG(I)=0.0                                                          
                  EVAPG (I)=0.0                                                           
              ENDIF                                                                   
              QEVAPG(I)=CPHCHG(I)*EVAPG(I)                                                     
              IF(ILW.EQ.2) THEN
                  QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                 (QLWOC(I)-QLWOG(I))-QSENSG(I)-QEVAPG(I)-GZERO(I)
              ELSE
                  QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                 QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
              ENDIF
              RESID(I)=0.0
          ENDIF                                                                   
  300 CONTINUE
C
C     * PRE-ITERATION SEQUENCE FOR VEGETATION CANOPY.
C
      DO 400 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              ITER(I)=1
              NITER(I)=1                                                                      
              TSTEP(I)=5.0
              RESID(I)=999999.
              RESIDL(I)=999999.                                                              
              TCANL(I)=999999.
          ENDIF
  400 CONTINUE
C
C     * ITERATION FOR CANOPY TEMPERATURE.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS
C     * ON THE CURRENT LATITUDE CIRCLE(S).
C  
  500 CONTINUE
C
      NUMIT=0
      NIT=0
      DO 550 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)              THEN    
              IF(TCAN(I).GE.TFREZ)      THEN
                  A(I)=17.269                                                                
                  B(I)=35.86                                                                 
              ELSE                                                                        
                  A(I)=21.874                                                                
                  B(I)=7.66                                                                  
              ENDIF                                                                       
              WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1             (TCAN(I)-B(I)))/PADRY(I)           
              QCAN(I)=WCAN/(1.0+WCAN)                                                 
              TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
              NIT=NIT+1
          ENDIF
  550 CONTINUE      
C
      IF(NIT.GT.0)                                                  THEN 
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C     * OTHER RELATED QUANTITIES.
C
        CALL DRCOEF (CDM,CDH,RIB,CFLUX,
     1               ZOM,ZOH,CRIB,TVIRTC,TVIRTA,VA,FCT,ITER,
     2               ZOMWRK,ZOHWRK,ILG,IL1,IL2)
C
C     * REMAINING CALCULATIONS.
C
        if (CFLUX(1) .eq. 0.0) CFLUX(1) =0.00001 
        DO 560 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              RA(I)=1.0/CFLUX(I)                                                            
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              QSENSC(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TCAN(I)-TA(I)) 
              IF(QA(I).GT.QCAN(I))                         THEN
                  XEVAP(I)=1.0/RA(I)                                                        
              ELSE                                                                    
                  IF(FSNOWC(I).GT.0.0)               THEN 
                      XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RA(I)                                        
                  ELSE                                                                
                      XEVAP(I)=FRAINC(I)/RA(I)+(1.0-FRAINC(I))/
     1                         (RA(I)+RC(I))                            
                  ENDIF                                                               
              ENDIF                                                                   
              IF(FRAINC(I).GT.0. .OR. FSNOWC(I).GT.0. .OR. 
     1           RC(I).LT.10000. .OR. QA(I).GT.QCAN(I))       THEN 
                  EVAPC(I)=RHOAIR(I)*XEVAP(I)*(QCAN(I)-QA(I)) 
              ELSE                                                                    
                  EVAPC(I)=0.0                                                           
              ENDIF                                                                   
              IF(EVAPC(I).LT.0. .AND. TCAN(I).GE.TADP(I)) EVAPC(I)=0.0
              QEVAPC(I)=CPHCHC(I)*EVAPC(I)                                                     
              QSTOR (I)=CHCAP(I)*(TCAN(I)-TCANO(I))/DELT
              IF(ILW.EQ.2) THEN
                  RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-QLWOC(I))*
     1                 (1.0-FSVF(I))+QSENSG(I)-QSENSC(I)-QEVAPC(I)-
     2                 QSTOR(I)-QMELTC(I)
              ELSE
                  RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1                 (1.0-FSVF(I))+QSENSG(I)-QSENSC(I)-QEVAPC(I)-
     2                 QSTOR(I)-QMELTC(I)
              ENDIF
              IF(ABS(RESID(I)).LT.1.0)                       ITER(I)=0
              IF(ABS(TSTEP(I)).LT. 1.0E-4 .AND. ABS(RESID(I)).LT.
     1          (ABS(RESIDL(I))-0.01))                       ITER(I)=0
              IF(FCT(I).GT.0. .AND. NITER(I).EQ.50)          ITER(I)=-1
          ENDIF
  560   CONTINUE     
C
        DO 575 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(NITER(I).EQ.1) THEN
                  RESIDO(I)=RESID(I)
                  IF(RESID(I).GT.0.0) THEN
                      TCAN(I)=TCAN(I)+5.0
                  ELSE
                      TCAN(I)=TCAN(I)-5.0
                  ENDIF
              ELSEIF(NITER(I).LE.10) THEN
                  TCANL(I)=TCAN(I)
                  RESIDL(I)=RESID(I)
                  TCAN(I)=(TCANL(I)*RESIDO(I)-TCANO(I)*RESIDL(I))/
     1                (RESIDO(I)-RESIDL(I))
C    -------------------- CTEM MODIFICATIONS ---------------------------\
C                 CHANGE TCAN ITERATION BOTTOM RANGE TO 173.16 K
                  IF(TCAN(I).LT.173.16) TCAN(I)=TCANL(I)-1.0
C    -------------------- CTEM MODIFICATIONS ---------------------------/
                  IF(TCAN(I).GT.373.16) TCAN(I)=TCANL(I)+1.0
                  TSTEP(I)=TCAN(I)-TCANL(I)
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))    THEN 
                      TSTEP(I)=-TSTEP(I)/2.0                                                    
                  ENDIF
                  IF(TSTEP(I).GT.10.0) TSTEP(I)=10.0
                  IF(TSTEP(I).LT.-10.0) TSTEP(I)=-10.0
                  TCAN(I)=TCAN(I)+TSTEP(I)
                  RESIDL(I)=RESID(I)
              ENDIF
          ENDIF
C
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(ABS(TCAN(I)-TFREZ).LT.1.0E-6)             TCAN(I)=TFREZ
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  575   CONTINUE
      ENDIF
C
      IBAD=0
!      DO 600 I=IL1,IL2
C         IF(FCT(I).GT.0. .AND. ITER(I).EQ.-1)                      THEN 
C             WRITE(6,6350) I,JL,RESID(I),TCAN(I),RIB(I),NITER(I)
C6350         FORMAT('0CANOPY ITERATION LIMIT',3X,2I3,2F8.2,E12.4,I6)            
C         ENDIF                                            
C    -------------------- CTEM MODIFICATIONS ---------------------------\
!          IF(FCT(I).GT.0. .AND.(TCAN(I).LT.173.16 
!     1    .OR. TCAN(I).GT.373.16)) THEN
C    -------------------- CTEM MODIFICATIONS ---------------------------/
!              IBAD=I
!          ENDIF  
!  600 CONTINUE
C
!      IF(IBAD.NE.0)                                                 THEN
!          WRITE(6,6375) IBAD,JL,TCAN(IBAD),TSTEP(I),NITER(IBAD),ISNOW
! 6375     FORMAT('0BAD CANOPY ITERATION TEMPERATURE',3X,2I3,2F12.2,2I4)
!          WRITE(6,6380) QSWNC(IBAD),QLWIN(IBAD),QLWOG(IBAD),
!     1                  QLWOC(IBAD),QSENSG(IBAD),QSENSC(IBAD),
!     2                  QEVAPC(IBAD),QSTOR(IBAD),QMELTC(IBAD),TA(IBAD)
! 6380     FORMAT(2X,10F10.2)
!          CALL XIT('TSOLVC',-2)
!      ENDIF
C  
      IF(NUMIT.GT.0)                                    GO TO 500
C
C     * POST-ITERATION CLEAN-UP.
C
      NIT=0
      DO 650 I=IL1,IL2
          IF(FCT(I).GT.0.) THEN
              IF(RAICAN(I).GT.0. .AND. TCAN(I).LT.TFREZ)      THEN 
                  QSTOR(I)=-CHCAP(I)*TCANO(I)/DELT
                  HFREZ=CHCAP(I)*(TFREZ-TCAN(I))                                                
                  HCONV=RAICAN(I)*CLHMLT                                                     
                  IF(HFREZ.LT.HCONV)                       THEN 
                     RCONV=HFREZ/CLHMLT                                                  
                     SNOCAN(I)=SNOCAN(I)+RCONV                                                 
                     RAICAN(I)=RAICAN(I)-RCONV                                                 
                     TCAN  (I)=TFREZ                                                          
                     QMELTC(I)=-CLHMLT*RCONV/DELT
                     CHCAP(I)=SPHVEG*CMASS(I)+SPHICE*SNOCAN(I)+
     1                         SPHW*RAICAN(I)
                     QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
                     WCAN=0.622*611.0/PADRY(I)  
                     QCAN(I)=WCAN/(1.0+WCAN)                                                 
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                     ITER(I)=1
                     NIT=NIT+1
                  ELSE                                                                    
                     HCOOL=HFREZ-HCONV                                                   
                     SNOCAN(I)=SNOCAN(I)+RAICAN(I)                                                
                     TCAN  (I)=-HCOOL/(SPHVEG*CMASS(I)+SPHICE*
     1                         SNOCAN(I))+TFREZ  
                     QMELTC(I)=-CLHMLT*RAICAN(I)/DELT
                     RAICAN(I)=0.0                                                          
                     CHCAP(I)=SPHVEG*CMASS(I)+SPHICE*SNOCAN(I)
                     QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
                     A(I)=21.874                                                                
                     B(I)=7.66                                                                  
                     WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                    (TCAN(I)-B(I)))/PADRY(I)           
                     QCAN(I)=WCAN/(1.0+WCAN)                                                 
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                     ITER(I)=1
                     NIT=NIT+1
                  ENDIF                                                                   
              ELSEIF(SNOCAN(I).GT.0. .AND. TCAN(I).GT.TFREZ)    THEN
                  QSTOR(I)=-CHCAP(I)*TCANO(I)/DELT
                  HMELT=CHCAP(I)*(TCAN(I)-TFREZ)                                                
                  HCONV=SNOCAN(I)*CLHMLT                                                     
                  IF(HMELT.LT.HCONV)                       THEN 
                     SCONV=HMELT/CLHMLT                                                  
                     SNOCAN(I)=SNOCAN(I)-SCONV                                                 
                     RAICAN(I)=RAICAN(I)+SCONV                                                 
                     TCAN  (I)=TFREZ                                                          
                     QMELTC(I)=CLHMLT*SCONV/DELT
                     CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)+
     1                        SPHICE*SNOCAN(I)
                     QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
                     WCAN=0.622*611.0/PADRY(I)  
                     QCAN(I)=WCAN/(1.0+WCAN)                                                 
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                     ITER(I)=1
                     NIT=NIT+1
                  ELSE                                                                    
                     HWARM=HMELT-HCONV                                                   
                     RAICAN(I)=RAICAN(I)+SNOCAN(I)                                                
                     TCAN  (I)=HWARM/(SPHVEG*CMASS(I)+SPHW*
     1                         RAICAN(I))+TFREZ                         
                     QMELTC(I)=CLHMLT*SNOCAN(I)/DELT
                     SNOCAN(I)=0.0                                                          
                     CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)
                     QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
                     A(I)=17.269                                                                
                     B(I)=35.86                                                                 
                     WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                    (TCAN(I)-B(I)))/PADRY(I)           
                     QCAN(I)=WCAN/(1.0+WCAN)                                                 
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                     ITER(I)=1
                     NIT=NIT+1
                  ENDIF                                                                   
              ELSE
                  ITER(I)=0
              ENDIF                       
          ENDIF
  650 CONTINUE      
C
      IF(NIT.GT.0)                                                  THEN 
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C     * OTHER RELATED QUANTITIES.
C
        CALL DRCOEF (CDM,CDH,RIB,CFLUX,
     1               ZOM,ZOH,CRIB,TVIRTC,TVIRTA,VA,FCT,ITER,
     2               ZOMWRK,ZOHWRK,ILG,IL1,IL2)
      ENDIF
C
C     * REMAINING CALCULATIONS.
C
C
      DO 700 I=IL1,IL2
          IF(FCT(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              RA(I)=1.0/CFLUX(I)                                                            
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              QSENSC(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TCAN(I)-TA(I)) 
              IF(QA(I).GT.QCAN(I))                         THEN
                  XEVAP(I)=1.0/RA(I)                                                        
              ELSE                                                                    
                  IF(FSNOWC(I).GT.0.0)               THEN 
                      XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RA(I)                                        
                  ELSE                                                                
                      XEVAP(I)=FRAINC(I)/RA(I)+(1.0-FRAINC(I))/
     1                         (RA(I)+RC(I))                            
                  ENDIF                                                               
              ENDIF                                                                   
              IF(FRAINC(I).GT.0. .OR. FSNOWC(I).GT.0. .OR. 
     1           RC(I).LT.10000. .OR. QA(I).GT.QCAN(I))       THEN 
                  EVAPC(I)=RHOAIR(I)*XEVAP(I)*(QCAN(I)-QA(I)) 
              ELSE                                                                    
                  EVAPC(I)=0.0                                                           
              ENDIF                                                                   
              IF(EVAPC(I).LT.0. .AND. TCAN(I).GE.TADP(I)) EVAPC(I)=0.0
              QEVAPC(I)=CPHCHC(I)*EVAPC(I)                                                     
              IF(ILW.EQ.2) THEN
                  RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-QLWOC(I))*
     1                 (1.0-FSVF(I))+QSENSG(I)-QSENSC(I)-QEVAPC(I)-
     2                 QSTOR(I)-QMELTC(I)
              ELSE
                  RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1                 (1.0-FSVF(I))+QSENSG(I)-QSENSC(I)-QEVAPC(I)-
     2                 QSTOR(I)-QMELTC(I)
              ENDIF
          ENDIF
          IF(FCT(I).GT.0.)                                          THEN 
              QSENSC(I)=QSENSC(I)+RESID(I)
              IF(TCAN(I).GE.TFREZ) THEN                                             
                  CA=17.269                                                       
                  CB=35.86                                                        
              ELSE                                                                
                  CA=21.874                                                       
                  CB=7.66                                                         
              ENDIF  
              TCDP(I)=(CB*CONST(I)-CA*TFREZ)/(CONST(I)-CA)        
              IF(EVAPG(I).LT.0. .AND. TZERO(I).GE.TCDP(I)) EVAPG(I)=0.
              QEVAPG(I)=CPHCHG(I)*EVAPG(I)
              IF(ABS(TZERO(I)-TFREZ).LT.1.0E-8) THEN
                  IF(ILW.EQ.2) THEN
                      QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                    (QLWOC(I)-QLWOG(I))-QSENSG(I)-QEVAPG(I)-
     2                    GZERO(I)
                      QLWOUT(I)=0.0
                  ELSE
                      QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                    QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
                      QLWOUT(I)=FSVF(I)*QLWOG(I)+(1.0-FSVF(I))*QLWOC(I)
                  ENDIF
              ELSE
                  IF(ILW.EQ.2) THEN
                      GZERO(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                    (QLWOC(I)-QLWOG(I))-QSENSG(I)-QEVAPG(I)
                      QLWOUT(I)=0.0
                  ELSE
                      GZERO(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                    QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)
                      QLWOUT(I)=FSVF(I)*QLWOG(I)+(1.0-FSVF(I))*QLWOC(I)
                  ENDIF
              ENDIF
              QSWNET(I)=QSWNG(I)+QSWNC(I)+QTRANS(I)
              QSENS(I)=QSENSC(I)                                                                
              QEVAP(I)=QEVAPC(I)+QEVAPG(I)                                                         
              EVAPC(I)=EVAPC(I)/RHOW                                                            
              EVAPG(I)=EVAPG(I)/RHOW
          ENDIF
  700 CONTINUE
C                                                                                  
C    -------------------- CTEM MODIFICATIONS ---------------------------\
C
C     STORE AERODYNAMIC CONDUCTANCE FOR USE IN NEXT TIME STEP
C
      DO 710 I = IL1, IL2
       CFLUXV(I) = CFLUX(I)
710   CONTINUE
C
C    -------------------- CTEM MODIFICATIONS ---------------------------/
C
      RETURN                                                                      
      END
C