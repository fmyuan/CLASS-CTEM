      SUBROUTINE CLASST (TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     1   THICEC, THICEG, HCPC,   HCPG,   FROOT,  QSENS,  TFLUX,  QEVAP,  
     2   EVAP,   QFLUX,  EVAPB,  CDH,    CDM,    
     3   GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,   G12CS,  G12GS,  
     4   G23C,   G23G,   G23CS,  G23GS,  QFREZC, QFREZG, QMELTC, QMELTG, 
     5   EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, TCANO,  TCANS,  
     6   ZPOND,  TPONDC, TPONDG, TPNDCS, TPNDGS, TSNOCS, TSNOGS, 
     7   GT,     QG,     ST,     SU,     SV,     SQ,     
     8   FSGV,   FSGS,   FSGG,   FLGV,   FLGS,   FLGG,   HFSC,   HFSS,   
     9   HFSG,   HEVC,   HEVS,   HEVG,   HMFC,   HTCC,   HTCS,   HTC,    
     A   TBAR,   THLIQ,  THICE,  TBASE,  SAND,   CLAY,   ORGM,   DRAG,   
     B   QSWINV, QSWINI, QLWIN,  UGCM,   VGCM,   TA,     QA,     PRESSG, 
     C   TH,     SGJ,    SHJ,    ENV,    FC,     FG,     FCS,    FGS,
     D   FSVF,   FSVFS,  ALVSCN, ALIRCN, ALVSG,  ALIRG,  ALVSCS, ALIRCS, 
     E   ALVSSN, ALIRSN, TRVSCN, TRIRCN, TRVSCS, TRIRCS, RCMIN,  RCMINS, 
     F   FRAINC, FSNOWC, RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP,  CHCAPS,
     G   CMASSC, CMASCS, DISP,   DISPS,  ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, 
     H   ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, TCAN,   TSNOW,  ZSNOW,  TRSNOW,
     I   RHOSNO, DELZ,   DELZW,  ZREF,   ILW,    ILG,    IL1,    IL2,
     J   JL,     IC,     IG,     NLANDCS,NLANDGS,NLANDC, NLANDG, ITERCT,
C
C    ---------------------- CTEM MODIFICATIONS -------------------------\
     K    AILCG,   AILCGS,    FCANC,   FCANCS,   CO2CONC,   CO2I1CG, 
     L  CO2I1CS,  CO2I2CG,  CO2I2CS,    COSZS,   XDIFFUS,      SLAI,
     M      ICC,    CTEM1,    CTEM2, RMATCTEM,   FCANCMX,     L2MAX,
     N NOL2PFTS,  CFLUXCG,  CFLUXCS,
C    ------------ CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW -----------|
     O  ANCSVEG,  ANCGVEG, RMLCSVEG, RMLCGVEG,    CANRES,  
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
     P    CTEMN,      ETP,    XMINF,       BI,	NRUB0,		VCMAX0, !)
C     ----------------------------------------------------HSuo testing BELOW 
     Q  GPP_GVEG,GPP_SVEG, 
     R JE_SVEG,JE_GVEG,JC_GVEG,JC_SVEG,JS_GVEG,JS_SVEG, 
     S IPAR_HS,FPAR_SVEG,FPAR_GVEG,QSWV_HS,SM_HS,VMAXC_GVEG,VMAXC_SVEG,
     T VM_GVEG,VMUNS_GVEG,VMUNS1_GVEG,VMUNS2_GVEG,VMUNS3_GVEG,
     U VM_SVEG,VMUNS_SVEG,VMUNS1_SVEG,VMUNS2_SVEG,VMUNS3_SVEG,
     V CO2I_GVEG,CO2I_SVEG,TGA_HS,KC_HS,KO_HS,TCAN_HS,FSEAS_HS,
     W QSWINV_HS,ALVISC_HS,QSWNVG_HS  )

  
C    AILCG    - GREEN LAI FOR USE WITH PHOTOSYNTHESIS SUBTROUTINE FOR
C               CANOPY OVER GROUND SUBAREA
C    AILCGS   - GREEN LAI FOR USE WITH PHOTOSYNTHESIS SUBTROUTINE FOR
C               CANOPY OVER SNOW SUBAREA
C    FCANC    - FRACTIONAL COVERAGE OF 8 CARBON PFTs, CANOPY OVER GROUND
C    FCANCS   - FRACTIONAL COVERAGE OF 8 CARBON PFTs, CANOPY OVER SNOW
C    CO2CONC  - ATMOS. CO2 CONC. IN PPM
C    CO2I1CG  - INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER GROUND
C               SUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
C    CO2I2CG  - SAME AS ABOVE BUT FOR SHADED LEAF
C    CO2I1CS  - INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER SNOW
C               SUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
C    CO2I2CS  - SAME AS ABOVE BUT FOR SHADED LEAF
C    COSZS    - COSINE OF SUN'S ZENITH ANGLE
C    XDIFFUS  - FRACTION OF DIFFUSED RADIATION
C    SLAI     - STORAGE LAI. SEE PHTSYN SUBROUTINE FOR MORE DETAILS.
C    ICC      - 8 (CTEM's PLANT FUNCTIONAL TYPES) 
C    CTEM1    - LOGICAL BOOLEAN FOR USING CTEM's STOMATAL RESISTANCE
C               OR NOT
C    CTEM2    - LOGICAL BOOLEAN FOR USING CTEM's STRUCTURAL ATTRIBUTES
C               OR NOT
C    RMATCTEM - FRACTION OF ROOTS IN EACH SOIL LAYER FOR EACH OF CTEM's
C               8 PFTs
C    FCANCMX  - MAX. FRACTIONAL COVERAGE OF CTEM PFTs
C    ANCSVEG  - NET PHOTOSYNTHETIC RATE FOR CTEM's 8 PFTs FOR CANOPY
C               OVER SNOW SUBAREA
C    ANCGVEG  - NET PHOTOSYNTHETIC RATE FOR CTEM's 8 PFTs FOR CANOPY
C               OVER GROUND SUBAREA
C    RMLCSVEG - LEAF RESPIRATION RATE FOR CTEM's 8 PFTs FOR CANOPY
C               OVER SNOW SUBAREA
C    RMLCGVEG - LEAF RESPIRATION RATE FOR CTEM's 8 PFTs FOR CANOPY
C               OVER GROUND SUBAREA
C    CANRES   - WEIGHTED AVERAGE OF CANOPY RESISTANCE FOR CANOPY OVER
C               SNOW (RCS) AND CANOPY OVER GROUND (RC) SUBAREAS. THIS 
C               IS TO BE USED AS A DIAGNOSTIC FOR COMPARING CANOPY 
C               RESISTANCE FROM CLASS AND CTEM's PHTSYN SUBROUTINE.
C    ---------------------- CTEM MODIFICATIONS --------------------------/
C
C     * MAR 12/03 - V.ARORA.    MAKE MODIFICATIONS TO PASS ADDITIONAL 
C     *                         VARIABLES TO TSOLVC FOR USE BY PHTSYN
C     *                         SUBROUTINE (SEE TSOLVC FOR DETAILS). 
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * OCT 11/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         REVISE CALCULATION OF SLTHKEF AND 
C     *                         DEFINITION OF ZLEV FOR CONSISTENCY
C     *                         WITH ZREF.
C     * SEP 27/96 - D.VERSEGHY. FIX BUG IN CALCULATION OF FLUXES
C     *                         BETWEEN SOIL LAYERS (PRESENT SINCE 
C     *                         RELEASE OF CLASS VERSION 2.5).
C     * MAY 21/96 - K.ABDELLA.  CORRECT EXPRESSION FOR ZOSCLH (4 PLACES).
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS; ALSO, PASS IN ZREF AND
C     *                         ILW THROUGH SUBROUTINE CALL.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL 
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         ADD THREE NEW DIAGNOSTIC FIELDS;
C     *                         REVISE CALCULATION OF HTCS, HTC.
C     * DEC 06/94 - M.LAZARE. - PASS "CFLUX" TO TSOLVE INSTEAD OF
C     *                         "CLIMIT" IN CONJUNCTION WITH CHANGES
C     *                         TO THAT ROUTINE.
C     *                       - REVISE CALCULATION OF "ZLEV" TO INCLUDE
C     *                         VIRTUAL TEMPERATURE EFFECTS.
C     *                       - REVISE CALCULATION OF "SLTHKEF".
C     * NOV 28/94 - M.LAZARE.   FORM DRAG "CDOM" MODIFICATION REMOVED.
C     * NOV 18/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         LOCAL VERSION WITH INTERNAL WORK ARRAYS
C     *                         HARD-CODED FOR USE ON PCS.
C     * NOV 05/93 - M.LAZARE.   ADD NEW DIAGNOSTIC OUTPUT FIELD: DRAG.
C     * JUL 27/93 - D.VERSEGHY/M.LAZARE. PREVIOUS VERSION CLASSTO.
C
C     * OUTPUT FIELDS.
C                                                                              
      REAL TBARC (ILG,IG),TBARG (ILG,IG),TBARCS(ILG,IG),TBARGS(ILG,IG),
     1     THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),
     2     HCPC  (ILG,IG),HCPG  (ILG,IG),FROOT (ILG,IG),HTC   (ILG,IG)             
C   
      REAL QSENS (ILG),   TFLUX (ILG),   QEVAP (ILG),   EVAP  (ILG),                
     1     QFLUX (ILG),   EVAPB (ILG),   CDH   (ILG),   CDM   (ILG),                 
     2     GZEROC(ILG),   GZEROG(ILG),   GZROCS(ILG),   GZROGS(ILG),              
     3     G12C  (ILG),   G12G  (ILG),   G12CS (ILG),   G12GS (ILG),               
     4     G23C  (ILG),   G23G  (ILG),   G23CS (ILG),   G23GS (ILG),               
     5     QFREZC(ILG),   QFREZG(ILG),   QMELTC(ILG),   QMELTG(ILG),              
     6     EVAPC (ILG),   EVAPCG(ILG),   EVAPG (ILG),   EVAPCS(ILG),
     7     EVPCSG(ILG),   EVAPGS(ILG),   TCANO (ILG),   TCANS (ILG), 
     8     ZPOND (ILG),   TPONDC(ILG),   TPONDG(ILG),   TPNDCS(ILG),              
     C     TPNDGS(ILG),   TSNOCS(ILG),   TSNOGS(ILG),   GT    (ILG),                  
     D     QG    (ILG),   
     E     ST    (ILG),   SU    (ILG),   SV    (ILG),   SQ    (ILG),   
     F     FSGV  (ILG),   FSGS  (ILG),   FSGG  (ILG),   FLGV  (ILG),
     G     FLGS  (ILG),   FLGG  (ILG),   HFSC  (ILG),   HFSS  (ILG),
     H     HFSG  (ILG),   HEVC  (ILG),   HEVS  (ILG),   HEVG  (ILG),  
     I     HMFC  (ILG),   HTCC  (ILG),   HTCS  (ILG),   DRAG  (ILG)  
C
C     * INPUT FIELDS.
C
      REAL TBAR  (ILG,IG),THLIQ (ILG,IG),THICE (ILG,IG),
     1     SAND  (ILG,IG),CLAY  (ILG,IG),ORGM  (ILG,IG)
C
      INTEGER    ITERCT (6,50)
C
      REAL QSWINV(ILG),   QSWINI(ILG),   QLWIN (ILG),   UGCM  (ILG), 
     1     VGCM  (ILG),   TA    (ILG),   QA    (ILG),   PRESSG(ILG),              
     2     TH    (ILG),   SGJ   (ILG),   SHJ   (ILG),   ENV   (ILG),                 
     3     FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),                  
     4     FSVF  (ILG),   FSVFS (ILG),   ALVSCN(ILG),   ALIRCN(ILG),              
     5     ALVSG (ILG),   ALIRG (ILG),   ALVSCS(ILG),   ALIRCS(ILG),              
     6     ALVSSN(ILG),   ALIRSN(ILG),   TRVSCN(ILG),   TRIRCN(ILG),              
     7     TRVSCS(ILG),   TRIRCS(ILG),   RCMIN (ILG),   RCMINS(ILG),               
     8     FRAINC(ILG),   FSNOWC(ILG),   RAICAN(ILG),   SNOCAN(ILG),              
     9     RAICNS(ILG),   SNOCNS(ILG),   CHCAP (ILG),   CHCAPS(ILG),              
     A     CMASSC(ILG),   CMASCS(ILG),   DISP  (ILG),   DISPS (ILG),              
     B     ZOMLNC(ILG),   ZOELNC(ILG),   ZOMLNG(ILG),   ZOELNG(ILG),              
     C     ZOMLCS(ILG),   ZOELCS(ILG),   ZOMLNS(ILG),   ZOELNS(ILG),              
     D     TCAN  (ILG),   TSNOW (ILG),   ZSNOW (ILG),   TRSNOW(ILG),
     E     RHOSNO(ILG),   TBASE (ILG)
C     
C     * SOIL PROPERTY ARRAYS.
C
      REAL DELZ(IG),      DELZW(ILG,IG) 
C
C     * INTERNAL WORK ARRAYS FOR THIS ROUTINE.
C     * FOR 2-D ARRAYS, THE SECOND INDEX REFERS TO THE GRID CELL 
C     * SUBAREAS: CANOPY-SNOW, SNOW-GROUND, CANOPY-GROUND AND
C     * BARE GROUND (VALUES 1 THROUGH 4, RESPECTIVELY). 
C
      REAL TCTOP (  3, 3),TCBOT (  3, 3)
C
      REAL STT   (  3,4), SUT   (  3,4), SVT   (  3,4), SQT   (  3,4),
     1     CDHT  (  3,4), CDMT  (  3,4), RIBT  (  3,4), TSURT (  3,4),
     2     QSURT (  3,4), QSWT  (  3,4), QLWT  (  3,4), QSENT (  3,4),
     3     QEVAT (  3,4)
C
      REAL EPS   (  3),   PSIZRO(  3),   RC    (  3),   HCPSNO(  3),
     1     TCSNOW(  3),   VA    (  3),   VPD   (  3),   PADRY (  3),
     2     RHOAIR(  3),   ZLEV  (  3),   CDOM  (  3),   CDOH  (  3),
     3     ZOSCLM(  3),   ZOSCLH(  3),   EVAPBC(  3),   EVAPBS(  3),
     4     GSNOWC(  3),   GSNOWG(  3),   TVIRTA(  3),   TADP  (  3),
     5     CRIB  (  3),   CEVAP (  3),   A1    (  3),
     6     A2    (  3),   A3    (  3),   B1    (  3),   B2    (  3),
     7     B3    (  3),   C2    (  3),   C3    (  3),   D3    (  3),
     8     GDENOM(  3),   GCOEFF(  3),   GCONST(  3),   TSTART(  3),
     9     CPHCHC(  3),   CPHCHG(  3),   QTRANS(  3),
     A     RCS   (  3),   CONST (  3)     
C
      INTEGER             ISAND (  3,3), 
     1                    IEVAP (  3),   IWATER(  3)
C
C     * INTERNAL WORK ARRAYS FOR TPREP (POINTERS AFTER).
C
      REAL THPORI(  3,3), PSISTI(  3,3), BI    (  3,3), 
     1     FVEG  (  3),   PSIGND(  3),   FRTOT (  3),   TCSAT (  3),
     2     THSAND(  3,3), THORG (  3,3)
C
C     * INTERNAL WORK ARRAYS FOR TSOLVC/TSOLVE.
C   
      REAL TSTEP (  3),    TVIRTC(  3),    TVIRTG(  3),    QMELT (  3),
     1     RHZERO(  3),    RESID (  3),    RESIDO(  3),    RAICNI(  3),
     2     SNOCNI(  3),    CHCAPI(  3),    QLWOC (  3),    QLWOG (  3),
     3     QSENSC(  3),    QSENSG(  3),    QSWNC (  3),    QSWNG (  3),
     4     QEVAPC(  3),    QEVAPG(  3),    TCANP (  3),    TRTOP (  3),
     5     TVIRTS(  3),    AC    (  3),    BC    (  3),    CLIMIT(  3),
     6     QZERO (  3),    RA    (  3),    CFLUX (  3),    QSTOR (  3),
     7     XEVAP (  3),    TCDP  (  3),    ZOMWRK(  3),    ZOHWRK(  3),
     8     RESIDL(  3),    TZEROL(  3),    TZEROO(  3),    TCANL (  3)
C
      INTEGER             ITER  (  3),   NITER (  3)
C
C    ---------------------- CTEM MODIFICATIONS --------------------------\
C
      REAL        RH(ILG),       AILCG(ILG,ICC),      FCANCS(ILG,ICC), 
     1     FCANC(ILG,ICC),         CO2CONC(ILG),           COSZS(ILG),      
     2       XDIFFUS(ILG),     CO2I1CS(ILG,ICC),           
     4   CO2I1CG(ILG,ICC),      AILCGS(ILG,ICC),     CO2I2CS(ILG,ICC),
     5   CO2I2CG(ILG,ICC), RMATCTEM(ILG,ICC,IG),     FCANCMX(ILG,ICC)
     
      REAL ANCGVEG(ILG,ICC),    ANCSVEG(ILG,ICC),   RMLCSVEG(ILG,ICC),
     1    RMLCGVEG(ILG,ICC),       SLAI(ILG,ICC),         CANRES(ILG)
C
      REAL     CFLUXCG(ILG),        CFLUXCS(ILG)
C
      INTEGER ICC, L2MAX, NOL2PFTS(IC)
C
      LOGICAL CTEM1, CTEM2 
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
      LOGICAL CTEMN
      REAL ETP(ILG),             XMINF(ILG,ICC),     NRUB0(ILG,ICC),
	1     VCMAX0(ILG,ICC)
      REAL XUPCSVEG(ILG,ICC),    XUPCGVEG(ILG,ICC)
C
C    ---------------------- CTEM MODIFICATIONS --------------------------/

C     ----------------------------------------------------HSuo testing BELOW 
      REAL  GPP_GVEG(ILG,ICC),  GPP_SVEG(ILG,ICC), !NEP_VEG,RE_VEG,
     1      JE_GVEG(ILG,ICC),   JE_SVEG(ILG,ICC), 
     2      JC_GVEG(ILG,ICC),   JC_SVEG(ILG,ICC),     
     3      JS_GVEG(ILG,ICC),   JS_SVEG(ILG,ICC), 
C    4     FPAR_GVEG(ILG,ICC), FPAR_SVEG(ILG,ICC), !HSuo     
     4      FPAR_GVEG(ILG), FPAR_SVEG(ILG),      
     5      IPAR_HS(ILG),       FSEAS_HS(ILG),      
     6      QSWV_HS(ILG),       SM_HS(ILG),
     7      VM_GVEG(ILG,ICC),    VM_SVEG(ILG,ICC),
     8      VMUNS_GVEG(ILG,ICC), VMUNS_SVEG(ILG,ICC),
     9      VMUNS1_GVEG(ILG,ICC),VMUNS1_SVEG(ILG,ICC),      
     A      VMUNS2_GVEG(ILG,ICC),VMUNS2_SVEG(ILG,ICC),
     B	  VMUNS3_GVEG(ILG,ICC),VMUNS3_SVEG(ILG,ICC),
     C      CO2I_GVEG(ILG,ICC),  CO2I_SVEG(ILG,ICC),
     D	  TGA_HS(ILG),KC_HS(ILG),KO_HS(ILG),TCAN_HS(ILG),
     E      VMAXC_GVEG(ILG,ICC),VMAXC_SVEG(ILG,ICC),
     F   QSWINV_HS(ILG),ALVISC_HS(ILG),QSWNVG_HS(ILG)  


      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,SLTHICK,BEEM,ALFAH,              
     1                FAC,GAMRH,GAMRM,VMIN                                        
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C                                                                                  
      ZREFLN=LOG(ZREF) 
C----------------------------------------------------------------------
C
C     * ASSIGN SOIL BACKGROUND PARAMETERS.
C
      DO 40 J=1,IG
      DO 40 I=IL1,IL2
          ISAND (I,J)=NINT(SAND(I,J))                                               
          IF(SAND(I,J).GE.0.0) THEN
              THPORI(I,J)=(-0.126*SAND(I,J)+48.9)/100.0
          ELSE
              THPORI(I,J)=0.0
          ENDIF
   40 CONTINUE
C
C     * CALCULATION OF ATMOSPHERIC INPUT FIELDS REQUIRED BY CLASS FROM VARIABLES SUPPLIED BY GCM.
C
      DO 50 I=IL1,IL2                                                            
          VA(I)=MAX(VMIN,SQRT(UGCM(I)*UGCM(I)+VGCM(I)*VGCM(I)))                
          EA=QA(I)*PRESSG(I)/(0.622+0.378*QA(I))                              
          IF(TA(I).GE.TFREZ) THEN                                             
              CA=17.269                                                       
              CB=35.86                                                        
          ELSE                                                                
              CA=21.874                                                       
              CB=7.66                                                         
          ENDIF                                                               
          EASAT=611.0*EXP(CA*(TA(I)-TFREZ)/(TA(I)-CB))                        
C
C    ---------------------- CTEM MODIFICATIONS --------------------------\
          RH(I)=EA/EASAT
          RH(I)=MIN(RH(I),1.0)
C    ---------------------- CTEM MODIFICATIONS --------------------------/
C
          VPD(I)=MAX(0.0,(EASAT-EA)/100.0)                                     
          PADRY(I)=PRESSG(I)-EA                                                  
          RHOAIR(I)=PADRY(I)/(RGAS*TA(I))+EA/(RGASV*TA(I))                          
          TVIRTA(I)=TA(I)*(1.0+0.61*QA(I))
          WA=QA(I)/(1.0-QA(I))
          CONST(I)=LOG(WA*PADRY(I)/(0.622*611.0)+0.000001)                                          
          TADP(I)=(CB*CONST(I)-CA*TFREZ)/(CONST(I)-CA)
          EPS(I)=0.
C
          ZLEV(I)=ZREF
          SLTHKEF=GRAV*ZREF/(RGAS*TH(I)*(1.0+0.61*QA(I)))
          CRIB(I)=-RGAS*SLTHKEF/(VA(I)**2)                                                  
C
C     * LATENT HEAT OF VAPORIZATION FROM CANOPY.
C
          IF(FSNOWC(I).GT.0. .OR. FRAINC(I).GT.0.)                  THEN
              CPHCHC(I)=(FSNOWC(I)*(CLHVAP+CLHMLT)+FRAINC(I)*CLHVAP)/
     1                  (FSNOWC(I)+FRAINC(I))           
          ELSE                                                                        
              CPHCHC(I)=CLHVAP                                                           
          ENDIF                                                                       
C
C     * CHECK LIQUID AND FROZEN SOIL MOISTURE CONTENTS FOR SMALL
C     * ABERRATIONS CAUSED BY PACKING/UNPACKING.
C
          IF(ISAND(I,1).GT.-4)                                   THEN     
              IF(ISAND(I,1).NE.-3)                         THEN
                  IF(THLIQ(I,1).LT.THLMIN) THLIQ(I,1)=THLMIN                      
                  THLIM1=(THPORI(I,1)-THLMIN)*RHOW/RHOICE                        
              ELSE
                  IF(THLIQ(I,1).LT.0.0) THLIQ(I,1)=0.0
                  THLIM1=THPORI(I,1)*RHOW/RHOICE                        
              ENDIF
              IF(THICE(I,1).GT.THLIM1) THICE(I,1)=THLIM1                      
              IF(THICE(I,1).LT.0.0) THICE(I,1)=0.0                            
          ENDIF
   50 CONTINUE
C
      DO 60 J=2,IG
      DO 60 I=IL1,IL2
          IF(ISAND(I,1).GT.-4)                                   THEN
              IF(ISAND(I,J).NE.-3)                         THEN
                  IF(THLIQ(I,J).LT.THLMIN) THLIQ(I,J)=THLMIN                  
                  IF(THICE(I,J).LT.0.0) THICE(I,J)=0.0                        
                  THTOT=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                     
                  IF(THTOT.GT.THPORI(I,J))           THEN                              
                      THLIQ(I,J)=MAX(THLIQ(I,J)*THPORI(I,J)/               
     1                           THTOT,THLMIN)                                       
                      THICE(I,J)=(THPORI(I,J)-THLIQ(I,J))*
     1                               RHOW/RHOICE
                  ENDIF
              ELSE
                  IF(THLIQ(I,J).LT.0.0) THLIQ(I,J)=0.0
                  IF(THICE(I,J).LT.0.0) THICE(I,J)=0.0                        
                  THTOT=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                     
                  IF(THTOT.GT.THPORI(I,J))           THEN                              
                      THLIQ(I,J)=MAX(THLIQ(I,J)*THPORI(I,J)/               
     1                           THTOT,0.0)                                       
                      THICE(I,J)=(THPORI(I,J)-THLIQ(I,J))*
     1                               RHOW/RHOICE
                  ENDIF
              ENDIF                                                       
          ENDIF
   60 CONTINUE                                                        
C
C    ---------------------- CTEM MODIFICATIONS --------------------------\
C     INITIALIZE VARIABLES ESTIMATED BY THE PHOTOSYNTHESIS SUBROUTINE
C     CALLED FROM WITHIN TSOLVC
C    
      DO 71 J = 1, ICC
        DO 72 I=IL1,IL2     
          ANCSVEG(I,J)=0.0
          ANCGVEG(I,J)=0.0
          RMLCSVEG(I,J)=0.0
          RMLCGVEG(I,J)=0.0
   72   CONTINUE
   71 CONTINUE
C    ---------------------- CTEM MODIFICATIONS --------------------------/
C
C     * INITIALIZE OUTPUT DIAGNOSTIC DRAG FIELD TO BE ACCUMULATED
C     * OVER THE FOUR VEGETATION TYPES.
C
      DO 80 I=IL1,IL2     
          DRAG(I)=0.
   80 CONTINUE
C
C     * PREPARATION.
C
      CALL  TPREP     (THLIQC, THLIQG, THICEC, THICEG, TBARC,  TBARG,
     1                 TBARCS, TBARGS, HCPC,   HCPG,   TCTOP,  TCBOT,
     2                 ZPOND,  PSIZRO, RC,     HCPSNO, TCSNOW, TSNOGS,
     3                 TSNOCS, TCANO,  TCANS,  CEVAP,  RCS,    IEVAP,
     4                 STT,    SUT,    SVT,    SQT,    CDHT,   CDMT,
     5                 RIBT,   TSURT,  QSURT,  QSWT,   QLWT,   QSENT,
     6                 QEVAT,  
     7                 EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS,
     8                 EVAPBC, EVAPBS, GSNOWC, GSNOWG, GZEROC, GZEROG,
     9                 QMELTC, QMELTG, FSGV,   FSGS,   FSGG,   FLGV,   
     A                 FLGS,   FLGG,   HFSC,   HFSS,   HFSG,   HEVC,
     B                 HEVS,   HEVG,   HMFC,   THLIQ,  THICE,  FROOT,
     C                 QSWINV, QSWINI, RCMIN,  TCAN,   RHOSNO, TSNOW,
     D                 VPD,    FC,     FCS,    TA,     ZSNOW,
     E                 SAND,   CLAY,   ORGM,   THPORI, DELZ,   DELZW,  
     F                 ISAND,  ILG,    IL1,    IL2,    JL,     IG,
     G                 FVEG,   PSISTI, BI,     THSAND, THORG,
     H                 PSIGND, FRTOT,  TCSAT                          )     

C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C                                                                                  
      IF(NLANDCS.GT.0)                                              THEN
          DO 100 I=IL1,IL2                                    
              IF(FCS(I).GT.0.)                                      THEN
                  if ((ZREF-DISPS(I)) .LE. 0.0) ZREF = ZREF + 10.0 !HSuo Feb2013
                  RTCDOM=VKC/(LOG(ZREF-DISPS(I))-ZOMLCS(I))
                  CDOM(I)=RTCDOM*RTCDOM                                              
                  CDOH(I)=RTCDOM*VKC/(LOG(ZREF-DISPS(I))-ZOELCS(I))                 
                  ZOSCLM(I)=EXP(-VKC/SQRT(CDOM(I)))                                     
                  ZOSCLH(I)=EXP(-VKC*SQRT(CDOM(I))/CDOH(I))
                  DRAG(I)=DRAG(I)+FCS(I)*CDOM(I)
              ENDIF
  100     CONTINUE
C                                     
          CALL CWCALC(TCANS,RAICNS,SNOCNS,CHCAPS,HMFC,HTCC,CMASCS,
     1                FCS,ILG,IL1,IL2)
          CALL TSPREP(A1,A2,A3,B1,B2,B3,C2,C3,D3,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,TSTART,IWATER, 
     2                TBAR,TCTOP,TCBOT,FCS,ZPOND,ZSNOW,TSNOW,TCSNOW,
     3                DELZ,ILG,IL1,IL2,JL,IG                        )
          ISNOW=1
          CALL TSOLVC(ISNOW,FCS,
     1                TSURT(1,1),QSURT(1,1),GSNOWC,QMELTC,CDHT(1,1),
     2                CDMT(1,1),QSWT(1,1),QLWT(1,1),QTRANS,
     3                QSENT(1,1),QEVAT(1,1),EVAPCS,EVPCSG,RIBT(1,1),
     4                CRIB,CEVAP,TADP,CPHCHC,CPHCHG,TVIRTA,EPS,
     5                QLWIN,TCANS,CONST,CMASCS,TA,QA,VA,
     6                CDOH,CDOM,ZOSCLH,ZOSCLM,PADRY,RHOAIR,GCONST,
     7                GCOEFF,PRESSG,TSTART,TRSNOW,PSIZRO,RCS,FSVFS,
     8                FSNOWC,FRAINC,RAICNS,SNOCNS,CHCAPS,QSWINV,QSWINI,
     9                ALVSCS,ALIRCS,ALVSSN,ALIRSN,TRVSCS,TRIRCS,
     A                IWATER,IEVAP,ILW,   
     B                ILG,IL1,IL2,JL,  
     C                TSTEP,TVIRTC,TVIRTG,QMELT,RHZERO,RESID,RESIDL,
     D                RESIDO,TZEROL,TZEROO,TCANL,
     E                RAICNI,SNOCNI,CHCAPI,QLWOC,QLWOG,QSENSC,QSENSG,
     F                QSWNC,QSWNG,QEVAPC,QEVAPG,TCANP,TRTOP,AC,BC,
     G                CLIMIT,QZERO,RA,CFLUX,QSTOR,XEVAP,TCDP,
     H                ZOMWRK,ZOHWRK,ITER,NITER,
C    ---------------------- CTEM MODIFICATIONS --------------------------\
     I                AILCGS,   FCANCS,   RH, CO2CONC,  RMATCTEM,
     J                THLIQC,     SAND, CLAY,      IG,     COSZS, 
     K               XDIFFUS,      ICC,   IC, CO2I1CS,   CO2I2CS,
     L                 CTEM1,    CTEM2, SLAI, FCANCMX,     L2MAX,
     M              NOL2PFTS,  CFLUXCS,
     N               ANCSVEG, RMLCSVEG,     !)
C    ---------------------- CTEM MODIFICATIONS --------------------------/
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
     O                CTEMN,  XUPCSVEG, NRUB0, VCMAX0, !)
C     ----------------------------------------------------HSuo testing BELOW 
     P  		          GPP_SVEG,JE_SVEG,JC_SVEG,JS_SVEG, !NEP_VEG,RE_VEG,
     Q                  IPAR_HS,FPAR_SVEG,SM_HS,QSWV_HS,VMAXC_SVEG,
     R          VM_SVEG,VMUNS_SVEG,VMUNS1_SVEG,VMUNS2_SVEG,VMUNS3_SVEG,
     S          CO2I_SVEG,TGA_HS,KC_HS,KO_HS,TCAN_HS,FSEAS_HS,
     T QSWINV_HS,ALVISC_HS,QSWNVG_HS  )


          CALL TSPOST(TBARCS,GZROCS,G12CS,G23CS,TPNDCS,GSNOWC,TSNOCS,
     1                QMELTC,GCONST,GCOEFF,
     2                TBAR,TCTOP,TCBOT,HCPC,ZPOND,ZSNOW,TSURT(1,1),
     3                TBASE,HCPSNO,QTRANS,A1,A2,A3,B1,B2,B3,C2,C3,D3,
     4                FCS,DELZ,DELZW,ILG,IL1,IL2,JL,IG           )
C
C     * DIAGNOSTICS.
C
          DO 150 I=IL1,IL2
              IF(FCS(I).GT.0.)                                      THEN
                  EVAPBS(I)=1.0/(1.0+RC(I)*CDHT(I,1)*VA(I))                                    
                  ZRUF=ZOSCLH(I)*ZLEV(I)                                                
                  ZSCRN=MAX(ZRUF,2.0)                                           
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                                      
                  RATFC1=RATFC*SQRT(CDMT(I,1))                                        
                  IF(RIBT(I,1).GE.0.)                           THEN                                          
                      RATIO=RATFC1                                                 
                  ELSE                                                            
                      RATIO=RATFC1*CDHT(I,1)/CDMT(I,1)
                      RATIO=MIN(RATIO,(ZSCRN/ZLEV(I))**(1./3.))                     
                  ENDIF                                                           
                  STT(I,1)=TCANS(I)-(MIN(RATIO,1.))*(TCANS(I)-TA(I))                            
                  SUT(I,1)=RATFC1*UGCM(I)                                             
                  SVT(I,1)=RATFC1*VGCM(I)                                             
                  SQT(I,1)=QA(I)+(QSURT(I,1)-QA(I))*MIN(RATIO,1.)
                  FSGV(I) =FSGV(I)+FCS(I)*QSWNC(I)
                  FSGS(I) =FSGS(I)+FCS(I)*QSWNG(I)
                  FSGG(I) =FSGG(I)+FCS(I)*QTRANS(I)
                  FLGV(I) =FLGV(I)+FCS(I)*(QLWIN(I)+QLWOG(I)-2.0*
     1                     QLWOC(I))*(1.0-FSVFS(I))
                  FLGS(I) =FLGS(I)+FCS(I)*(QLWOC(I)*(1.0-FSVFS(I))+
     1                     QLWIN(I)*FSVFS(I)-QLWOG(I))
                  HFSC(I) =HFSC(I)+FCS(I)*(QSENSC(I)-QSENSG(I))
                  HFSS(I) =HFSS(I)+FCS(I)*QSENSG(I)
                  HEVC(I) =HEVC(I)+FCS(I)*QEVAPC(I)
                  HEVS(I) =HEVS(I)+FCS(I)*QEVAPG(I)
                  HMFC(I) =HMFC(I)+FCS(I)*QMELT(I)
                  HTCS(I) =HTCS(I)+FCS(I)*(-GZROCS(I)+
     1                     QTRANS(I))
                  HTC(I,1)=HTC(I,1)+FCS(I)*(GZROCS(I)-QTRANS(I)-
     1                     G12CS(I))
                  HTC(I,2)=HTC(I,2)+FCS(I)*(G12CS(I)-G23CS(I))
                  HTC(I,3)=HTC(I,3)+FCS(I)*G23CS(I)
              ENDIF
  150     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
          DO 200 I=IL1,IL2                                    
              IF(FGS(I).GT.0.)                                      THEN
                  RTCDOM=VKC/(ZREFLN-ZOMLNS(I))
                  CDOM(I)=RTCDOM*RTCDOM                                              
                  CDOH(I)=RTCDOM*VKC/(ZREFLN-ZOELNS(I))                 
                  ZOSCLM(I)=EXP(-VKC/SQRT(CDOM(I)))                                     
                  ZOSCLH(I)=EXP(-VKC*SQRT(CDOM(I))/CDOH(I))
                  DRAG(I)=DRAG(I)+FGS(I)*CDOM(I)
              ENDIF
  200     CONTINUE
C
          CALL TSPREP(A1,A2,A3,B1,B2,B3,C2,C3,D3,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,TSTART,IWATER, 
     2                TBAR,TCTOP,TCBOT,FGS,ZPOND,ZSNOW,TSNOW,TCSNOW,
     3                DELZ,ILG,IL1,IL2,JL,IG                        )
          ISNOW=1 
          CALL TSOLVE(ISNOW,FGS,
     1                TSURT(1,2),QSURT(1,2),GSNOWG,QMELTG,CDHT(1,2),
     2                CDMT(1,2),QSWT(1,2),QLWT(1,2),
     3                QTRANS,QSENT(1,2),QEVAT(1,2),EVAPGS,RIBT(1,2),
     4                CRIB,CEVAP,TADP,CPHCHG,TVIRTA,EPS,QLWIN,TA,QA,VA,
     5                CDOH,CDOM,ZOSCLH,ZOSCLM,PADRY,RHOAIR,GCONST,
     6                GCOEFF,PRESSG,TSTART,TRSNOW,PSIZRO,QSWINV,QSWINI, 
     7                ALVSSN,ALIRSN,IWATER,IEVAP,ILW,
     8                ILG,IL1,IL2,JL,  
     9                TSTEP,TVIRTS,RHZERO,RESID,RESIDL,RESIDO,TZEROL,
     A                TZEROO,TRTOP,AC,BC,CFLUX,ZOMWRK,ZOHWRK,ITER,NITER)
          CALL TSPOST(TBARGS,GZROGS,G12GS,G23GS,TPNDGS,GSNOWG,TSNOGS,
     1                QMELTG,GCONST,GCOEFF,
     2                TBAR,TCTOP,TCBOT,HCPG,ZPOND,ZSNOW,TSURT(1,2),
     3                TBASE,HCPSNO,QTRANS,A1,A2,A3,B1,B2,B3,C2,C3,D3,
     4                FGS,DELZ,DELZW,ILG,IL1,IL2,JL,IG           )
C
C     * DIAGNOSTICS.
C
          DO 250 I=IL1,IL2
              IF(FGS(I).GT.0.)                                      THEN
                  ZRUF=ZOSCLH(I)*ZLEV(I)                                                
                  ZSCRN=MAX(ZRUF,2.0)                                           
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                                      
                  RATFC1=RATFC*SQRT(CDMT(I,2))                                        
                  IF(RIBT(I,2).GE.0.)                           THEN                                          
                      RATIO=RATFC1                                                 
                  ELSE                                                            
                      RATIO=RATFC1*CDHT(I,2)/CDMT(I,2)
                      RATIO=MIN(RATIO,(ZSCRN/ZLEV(I))**(1./3.))                     
                  ENDIF                                                           
                  STT(I,2)=TSURT(I,2)-(MIN(RATIO,1.))*
     1                    (TSURT(I,2)-TA(I))                            
                  SUT(I,2)=RATFC1*UGCM(I)                                             
                  SVT(I,2)=RATFC1*VGCM(I)                                             
                  SQT(I,2)=QA(I)+(QSURT(I,2)-QA(I))*MIN(RATIO,1.)
                  FSGS(I) =FSGS(I)+FGS(I)*(QSWT(I,2)-QTRANS(I))
                  FSGG(I) =FSGG(I)+FGS(I)*QTRANS(I)
                  FLGS(I) =FLGS(I)+FGS(I)*(QLWIN(I)-QLWT(I,2))
                  HFSS(I) =HFSS(I)+FGS(I)*QSENT(I,2)
                  HEVS(I) =HEVS(I)+FGS(I)*QEVAT(I,2)
                  HTCS(I) =HTCS(I)+FGS(I)*(-GZROGS(I)+
     1                     QTRANS(I))
                  HTC(I,1)=HTC(I,1)+FGS(I)*(GZROGS(I)-QTRANS(I)-
     1                     G12GS(I))
                  HTC(I,2)=HTC(I,2)+FGS(I)*(G12GS(I)-G23GS(I))
                  HTC(I,3)=HTC(I,3)+FGS(I)*G23GS(I)
              ENDIF
  250     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C                                                                                  
      IF(NLANDC.GT.0)                                               THEN
          DO 300 I=IL1,IL2                                    
              IF(FC(I).GT.0.)                                       THEN
                  RTCDOM=VKC/(LOG(ZREF-DISP(I))-ZOMLNC(I))                      
                  CDOM(I)=RTCDOM*RTCDOM                                              
                  CDOH(I)=RTCDOM*VKC/(LOG(ZREF-DISP(I))-ZOELNC(I))                 
                  ZOSCLM(I)=EXP(-VKC/SQRT(CDOM(I)))                                     
                  ZOSCLH(I)=EXP(-VKC*SQRT(CDOM(I))/CDOH(I))
                  DRAG(I)=DRAG(I)+FC(I)*CDOM(I)
              ENDIF
  300     CONTINUE
C
          CALL CWCALC(TCANO,RAICAN,SNOCAN,CHCAP,HMFC,HTCC,CMASSC,
     1                FC,ILG,IL1,IL2)
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,TSTART,IWATER, 
     2                TBAR,TCTOP,TCBOT,FC,ZPOND,DELZ,ISAND,
     3                ILG,IL1,IL2,JL,IG                            )
          ISNOW=0
          CALL TSOLVC(ISNOW,FC,
     1                TSURT(1,3),QSURT(1,3),GZEROC,QFREZC,CDHT(1,3),
     2                CDMT(1,3),QSWT(1,3),QLWT(1,3),QTRANS,
     3                QSENT(1,3),QEVAT(1,3),EVAPC,EVAPCG,RIBT(1,3),
     4                CRIB,CEVAP,TADP,CPHCHC,CPHCHG,TVIRTA,EPS,
     5                QLWIN,TCANO,CONST,CMASSC,TA,QA,VA,
     6                CDOH,CDOM,ZOSCLH,ZOSCLM,PADRY,RHOAIR,GCONST,
     7                GCOEFF,PRESSG,TSTART,TRSNOW,PSIZRO,RC,FSVF,
     8                FSNOWC,FRAINC,RAICAN,SNOCAN,CHCAP,QSWINV,QSWINI,
     9                ALVSCN,ALIRCN,ALVSG,ALIRG,TRVSCN,TRIRCN,
     A                IWATER,IEVAP,ILW,   
     B                ILG,IL1,IL2,JL,  
     C                TSTEP,TVIRTC,TVIRTG,QMELT,RHZERO,RESID,RESIDL,
     D                RESIDO,TZEROL,TZEROO,TCANL,
     E                RAICNI,SNOCNI,CHCAPI,QLWOC,QLWOG,QSENSC,QSENSG,
     F                QSWNC,QSWNG,QEVAPC,QEVAPG,TCANP,TRTOP,AC,BC,
     G                CLIMIT,QZERO,RA,CFLUX,QSTOR,XEVAP,TCDP,
     H                ZOMWRK,ZOHWRK,ITER,NITER,
C    ---------------------- CTEM MODIFICATIONS --------------------------\
     I                AILCG,     FCANC,   RH, CO2CONC,  RMATCTEM,
     J               THLIQC,      SAND, CLAY,      IG,     COSZS, 
     K               XDIFFUS,      ICC,   IC, CO2I1CG,   CO2I2CG,
     L                 CTEM1,    CTEM2, SLAI, FCANCMX,     L2MAX,
     M              NOL2PFTS,  CFLUXCG,
     N               ANCGVEG, RMLCGVEG,  !)
C    ---------------------- CTEM MODIFICATIONS --------------------------/
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
     O                 CTEMN, XUPCGVEG, NRUB0, VCMAX0,  !)
C     ----------------------------------------------------HSuo testing BELOW 
     P  		          GPP_GVEG,JE_GVEG,JC_GVEG,JS_GVEG, !NEP_VEG,RE_VEG,
     Q          IPAR_HS,FPAR_GVEG,SM_HS,QSWV_HS,VMAXC_GVEG,
     R          VM_GVEG,VMUNS_GVEG,VMUNS1_GVEG,VMUNS2_GVEG,VMUNS3_GVEG,
     S          CO2I_GVEG,TGA_HS,KC_HS,KO_HS,TCAN_HS,FSEAS_HS,
     T QSWINV_HS,ALVISC_HS,QSWNVG_HS  )


          CALL TNPOST(TBARC,G12C,G23C,TPONDC,GZEROC,QFREZC,GCONST,
     1                GCOEFF,TBAR,TCTOP,TCBOT,HCPC,ZPOND,TSURT(1,3),
     2                TBASE,A1,A2,B1,B2,C2,FC,IWATER,DELZ,DELZW,
     3                ILG,IL1,IL2,JL,IG                          )
C
C     * DIAGNOSTICS.
C
          DO 350 I=IL1,IL2
              IF(FC(I).GT.0.)                                       THEN
                  EVAPBC(I)=1.0/(1.0+RC(I)*CDHT(I,3)*VA(I))                                    
                  ZRUF=ZOSCLH(I)*ZLEV(I)                                                
                  ZSCRN=MAX(ZRUF,2.0)                                           
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                                      
                  RATFC1=RATFC*SQRT(CDMT(I,3))                                        
                  IF(RIBT(I,3).GE.0.)                           THEN                                          
                      RATIO=RATFC1                                                 
                  ELSE                                                            
                      RATIO=RATFC1*CDHT(I,3)/CDMT(I,3)
                      RATIO=MIN(RATIO,(ZSCRN/ZLEV(I))**(1./3.))                     
                  ENDIF                                                           
                  STT(I,3)=TCANO(I)-(MIN(RATIO,1.))*(TCANO(I)-TA(I))                            
                  SUT(I,3)=RATFC1*UGCM(I)                                             
                  SVT(I,3)=RATFC1*VGCM(I)                                             
                  SQT(I,3)=QA(I)+(QSURT(I,3)-QA(I))*MIN(RATIO,1.)
                  FSGV(I) =FSGV(I)+FC(I)*QSWNC(I)
                  FSGG(I) =FSGG(I)+FC(I)*QSWNG(I)
                  FLGV(I) =FLGV(I)+FC(I)*(QLWIN(I)+QLWOG(I)-2.0*
     1                     QLWOC(I))*(1.0-FSVF(I))
                  FLGG(I) =FLGG(I)+FC(I)*(FSVF(I)*QLWIN(I)+
     1                     (1.0-FSVF(I))*QLWOC(I)-QLWOG(I))
                  HFSC(I) =HFSC(I)+FC(I)*(QSENSC(I)-QSENSG(I))
                  HFSG(I) =HFSG(I)+FC(I)*QSENSG(I)
                  HEVC(I) =HEVC(I)+FC(I)*QEVAPC(I)
                  HEVG(I) =HEVG(I)+FC(I)*QEVAPG(I)
                  HMFC(I) =HMFC(I)+FC(I)*QMELT(I)
                  HTC(I,1)=HTC(I,1)+FC(I)*(-G12C(I))
                  HTC(I,2)=HTC(I,2)+FC(I)*(G12C(I)-G23C(I))
                  HTC(I,3)=HTC(I,3)+FC(I)*G23C(I)
              ENDIF
  350     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR BARE GROUND.
C                                                                                  
      IF(NLANDG.GT.0)                                               THEN
          DO 400 I=IL1,IL2                                    
              IF(FG(I).GT.0.)                                       THEN
                  RTCDOM=VKC/(ZREFLN-ZOMLNG(I))                      
                  CDOM(I)=RTCDOM*RTCDOM                                              
                  CDOH(I)=RTCDOM*VKC/(ZREFLN-ZOELNG(I))                 
                  ZOSCLM(I)=EXP(-VKC/SQRT(CDOM(I)))                                     
                  ZOSCLH(I)=EXP(-VKC*SQRT(CDOM(I))/CDOH(I))
                  DRAG(I)=DRAG(I)+FG(I)*CDOM(I)
              ENDIF
  400     CONTINUE
C
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,TSTART,IWATER, 
     2                TBAR,TCTOP,TCBOT,FG,ZPOND,DELZ,ISAND,
     3                ILG,IL1,IL2,JL,IG                            )
          ISNOW=0
          CALL TSOLVE(ISNOW,FG,
     1                TSURT(1,4),QSURT(1,4),GZEROG,QFREZG,CDHT(1,4),
     2                CDMT(1,4),QSWT(1,4),QLWT(1,4),
     3                QTRANS,QSENT(1,4),QEVAT(1,4),EVAPG,RIBT(1,4),
     4                CRIB,CEVAP,TADP,CPHCHG,TVIRTA,EPS,QLWIN,TA,QA,VA,
     5                CDOH,CDOM,ZOSCLH,ZOSCLM,PADRY,RHOAIR,GCONST,
     6                GCOEFF,PRESSG,TSTART,TRSNOW,PSIZRO,QSWINV,QSWINI,
     7                ALVSG,ALIRG,IWATER,IEVAP,ILW,
     8                ILG,IL1,IL2,JL,  
     9                TSTEP,TVIRTS,RHZERO,RESID,RESIDL,RESIDO,TZEROL,
     A                TZEROO,TRTOP,AC,BC,CFLUX,ZOMWRK,ZOHWRK,ITER,NITER)
          CALL TNPOST(TBARG,G12G,G23G,TPONDG,GZEROG,QFREZG,GCONST,
     1                GCOEFF,TBAR,TCTOP,TCBOT,HCPG,ZPOND,TSURT(1,4),
     2                TBASE,A1,A2,B1,B2,C2,FG,IWATER,DELZ,DELZW,
     3                ILG,IL1,IL2,JL,IG                          )
C
C     * DIAGNOSTICS.
C
          DO 450 I=IL1,IL2
              IF(FG(I).GT.0.)                                       THEN
                  ZRUF=ZOSCLH(I)*ZLEV(I)                                                
                  ZSCRN=MAX(ZRUF,2.0)                                           
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                                      
                  RATFC1=RATFC*SQRT(CDMT(I,4))                                        
                  IF(RIBT(I,4).GE.0.)                           THEN                                          
                      RATIO=RATFC1                                                 
                  ELSE                                                            
                      RATIO=RATFC1*CDHT(I,4)/CDMT(I,4)
                      RATIO=MIN(RATIO,(ZSCRN/ZLEV(I))**(1./3.))                     
                  ENDIF                                                           
                  STT(I,4)=TSURT(I,4)-(MIN(RATIO,1.))*
     1                    (TSURT(I,4)-TA(I))                            
                  SUT(I,4)=RATFC1*UGCM(I)                                             
                  SVT(I,4)=RATFC1*VGCM(I)                                             
                  SQT(I,4)=QA(I)+(QSURT(I,4)-QA(I))*MIN(RATIO,1.)
                  FSGG(I) =FSGG(I)+FG(I)*QSWT(I,4)
                  FLGG(I) =FLGG(I)+FG(I)*(QLWIN(I)-QLWT(I,4))
                  HFSG(I) =HFSG(I)+FG(I)*QSENT(I,4)
                  HEVG(I) =HEVG(I)+FG(I)*QEVAT(I,4)
                  HTC(I,1)=HTC(I,1)+FG(I)*(-G12G(I))
                  HTC(I,2)=HTC(I,2)+FG(I)*(G12G(I)-G23G(I))
                  HTC(I,3)=HTC(I,3)+FG(I)*G23G(I)
              ENDIF
  450     CONTINUE
      ENDIF                                                               
C
C     * AVERAGE FLUXES AND DIAGNOSTIC VARIABLES OVER FOUR GRID CELL
C     * SUBAREAS.
C
      DO 500 I=IL1,IL2
          QLWOUT=FCS(I)*QLWT(I,1)+FGS(I)*QLWT(I,2)+FC(I)*QLWT(I,3)+
     1           FG (I)*QLWT(I,4)          
          CDM(I)=FCS(I)*CDMT(I,1)+FGS(I)*CDMT(I,2)+FC(I)*CDMT(I,3)+
     1           FG (I)*CDMT(I,4)              
          CDH(I)=FCS(I)*CDHT(I,1)+FGS(I)*CDHT(I,2)+FC(I)*CDHT(I,3)+
     1           FG (I)*CDHT(I,4)
          IF(CDH(I).GT.0.)                               THEN                                              
              EVAPB(I)=(FCS(I)*CDHT(I,1)*EVAPBS(I) + FGS(I)*CDHT(I,2) +
     1                  FC (I)*CDHT(I,3)*EVAPBC(I) + FG (I)*CDHT(I,4))/
     2                  CDH(I)                                   
              QG(I)=(FCS(I)*QSURT(I,1)*CDHT(I,1)*EVAPBS(I) +
     1               FGS(I)*QSURT(I,2)*CDHT(I,2) +
     2               FC (I)*QSURT(I,3)*CDHT(I,3)*EVAPBC(I) +
     3               FG (I)*QSURT(I,4)*CDHT(I,4))/(CDH(I)*EVAPB(I))                                                  
          ELSE                                                                
              EVAPB(I)=FCS(I)*EVAPBS(I)+FGS(I)+FC(I)*EVAPBC(I)+FG(I)                
              IF(EVAPB(I).GT.0.)                        THEN                                        
                  QG(I)=(FCS(I)*QSURT(I,1)*EVAPBS(I)+FGS(I)*QSURT(I,2)+                   
     1                   FC (I)*QSURT(I,3)*EVAPBC(I)+FG (I)*QSURT(I,4))/
     2                   EVAPB(I)               
              ELSE                                                            
                  QG(I)=FCS(I)*QSURT(I,1) + FGS(I)*QSURT(I,2) + 
     1                  FC (I)*QSURT(I,3) + FG (I)*QSURT(I,4)
              ENDIF                                                           
          ENDIF                                                               
          QSENS(I)=FCS(I)*QSENT(I,1)+FGS(I)*QSENT(I,2)+FC(I)*QSENT(I,3)+                  
     1             FG (I)*QSENT(I,4)                                                    
          TFLUX(I)=-QSENS(I)/(RHOAIR(I)*SPHAIR)                                  
          GT(I)=(QLWOUT/SBC)**0.25                                            
          QEVAP(I)=FCS(I)*QEVAT(I,1)+FGS(I)*QEVAT(I,2)+FC(I)*QEVAT(I,3)+
     1             FG (I)*QEVAT(I,4)
          EVAP(I)=FCS(I)*(EVAPCS(I)+EVPCSG(I)) + FGS(I)*EVAPGS(I) +              
     1            FC (I)*(EVAPC (I)+EVAPCG(I)) + FG (I)*EVAPG(I)                       
          EVAP(I)=EVAP(I)*RHOW                                                
          QFLUX(I)=-EVAP(I)/RHOAIR(I)                                            
          ST(I)=FCS(I)*STT(I,1) + FGS(I)*STT(I,2) + FC (I)*STT(I,3)+
     1          FG (I)*STT(I,4)                   
          SU(I)=FCS(I)*SUT(I,1) + FGS(I)*SUT(I,2) + FC (I)*SUT(I,3)+
     1          FG (I)*SUT(I,4)
          SV(I)=FCS(I)*SVT(I,1) + FGS(I)*SVT(I,2) + FC (I)*SVT(I,3)+
     1          FG (I)*SVT(I,4)
          SQ(I)=FCS(I)*SQT(I,1) + FGS(I)*SQT(I,2) + FC (I)*SQT(I,3)+
     1          FG (I)*SQT(I,4)     
  500 CONTINUE
C                                                                                  
C    ---------------------- CTEM MODIFICATIONS -------------------------\
C     CALCULATE WEIGHTED AVERAGE OF CANOPY RESISTANCE FOR CANOPY OVER SNOW
C     (RCS) AND CANOPY OVER GROUND (RC) SUBAREAS. 
C
      DO 550 I = IL1, IL2
        IF( (FC(I)+FCS(I)).GT. 1.0E-12) THEN
          CANRES(I)=( (FC(I)*RC(I)) + (FCS(I)*RCS(I)) )/ (FC(I)+FCS(I))
        ELSE
          CANRES(I)=0.0
        ENDIF
550   CONTINUE
C
C    ---------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
      DO 600 I = IL1, IL2
        IF( (FC(I)+FCS(I)).GT. 1.0E-12) THEN
          ETP(I)=FCS(I)*EVAPCS(I)+FC (I)*EVAPC (I)                       
          ETP(I)=ETP(I)*RHOW                                                
        ELSE
          ETP(I)=0.0
        ENDIF
600   CONTINUE
C
      DO 610 J = 1, ICC
          DO 620 I = IL1, IL2
              IF( (FC(I)+FCS(I)).GT. 1.0E-12) THEN
                  XMINF(I,J)=FCANCS(I,J)*XUPCSVEG(I,J)
     1                      +FCANC(I,J)*XUPCGVEG(I,J)                       
              ELSE
                  XMINF(I,J)=0.0
              ENDIF
620       CONTINUE
610   CONTINUE
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
      RETURN                                                                      
      END        

