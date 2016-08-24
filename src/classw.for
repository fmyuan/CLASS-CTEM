       SUBROUTINE CLASSW(THLIQ,  THICE,  TBAR,   TCAN,   RCAN,   SCAN,
     1                  RUNOFF, SNO,    TSNOW,  RHOSNO, ALBSNO, GROWTH,
     2                  PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     3                  QFN,    QFG,    QFC,    HMFC,   HMFG,   HMFN,
     4                  HTCC,   HTCS,   HTC,    ROFC,   ROFN,   OVRFLW,
     5                  WTRS,   WTRG,   WL,     WF,     WLA,    WFA,
     6                  WLAS,   WFAS,   TBASE,  TBARC,  TBARG,  TBARCS, 
     7                  TBARGS, THLIQC, THLIQG, THICEC, THICEG, HCPC,   
     8                  HCPG,   FC,FG,  FCS,    FGS,    TPONDC, TPONDG,
     9                  TPNDCS, TPNDGS, EVAPC,  EVAPCG, EVAPG,  EVAPCS,
     A                  EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG,
     B                  RAICAN, SNOCAN, RAICNS, SNOCNS, EVAP,   FROOT,
     C                  FSVF,   FSVFS,  CWCAP,  CWCAPS, TCANO,  TCANS,
     D                  CHCAP,  CHCAPS, CMASSC, CMASCS, ZSNOW,  ZPOND, 
     E                  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,
     F                  G12CS,  G12GS,  G23C,   G23G,   G23CS,  G23GS,
     G                  TSNOCS, TSNOGS, ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS,
     H                  SAND,   CLAY,   ORGM,   DRN,    PCPR,TA,DELZ,
     I                  DELZW,  ZBOTW,  ILG,IL1,IL2,JL,IC,IG,IGP1,IGP2,
     J                  NLANDCS,NLANDGS,NLANDC, NLANDG, TRANS          )  !HSuo Feb2013
C                                                                        
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION OF
C     *                         OVERLAND FLOW.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4. 
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 24/95 - D.VERSEGHY. UPDATE ARRAY "EVAP" TO TAKE INTO 
C     *                         ACCOUNT "WLOST"; RATIONALIZE 
C     *                         CALCULATION OF THE LATTER.
C     *                         COMPLETION OF WATER BUDGET DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL 
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         CHANGES TO SUBROUTINE CALLS ASSOCIATED
C     *                         WITH REVISIONS TO DIAGNOSTICS.
C     *                         ALLOW SPECIFICATION OF LIMITING POND
C     *                         DEPTH "PNDLIM" (PARALLEL CHANGES MADE
C     *                         SIMULTANEOUSLY IN TMCALC).
C     * DEC 16/94 - D.VERSEGHY. TWO NEW DIAGNOSTIC FIELDS.
C     * NOV 18/93 - D.VERSEGHY. LOCAL VERSION WITH INTERNAL WORK ARRAYS
C     *                         HARD-CODED FOR USE ON PCS.
C     * NOV 01/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISIONS ASSOCIATED WITH NEW VERSION
C     *                         OF TMCALC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NUMEROUS NEW DIAGNOSTIC FIELDS.
C     * MAY 06/93 - D.VERSEGHY/M.LAZARE. CORRECT BUG IN CALL TO TMCALC
C     *                                  FOR CANOPY-SNOW CASE, WHERE
C     *                                  SHOULD BE PASSING "HCPCS"
C     *                                  INSTEAD OF "HCPGS". 
C     * MAY 15/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C                               CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. LAND SURFACE WATER BUDGET CALCULATIONS.
C                                                                                 
C     * MAIN OUTPUT FIELDS.
C                                                                                  
      REAL THLIQ (ILG,IG), THICE (ILG,IG), TBAR  (ILG,IG),
     1     QFC   (ILG,IG), HMFG  (ILG,IG), HTC   (ILG,IG)
C
      REAL TCAN  (ILG),    RCAN  (ILG),    SCAN  (ILG),    RUNOFF(ILG),
     1     SNO   (ILG),    TSNOW (ILG),    RHOSNO(ILG),    ALBSNO(ILG),
     2     GROWTH(ILG),    PCFC  (ILG),    PCLC  (ILG),    PCPN  (ILG),
     3     PCPG  (ILG),    QFCF  (ILG),    QFCL  (ILG),    QFN   (ILG),
     4     QFG   (ILG),    HMFC  (ILG),    HMFN  (ILG),    HTCC  (ILG),
     5     HTCS  (ILG),    ROFC  (ILG),    ROFN  (ILG),    WTRS  (ILG),
     6     WTRG  (ILG),    OVRFLW(ILG),    WL    (ILG),    WF    (ILG),
     7     WLA   (ILG),    WFA   (ILG),    WLAS  (ILG),    WFAS  (ILG),
     8     TBASE (ILG)
C
C     * I/O FIELDS PASSED THROUGH CLASS.
C
      REAL TBARC(ILG,IG), TBARG(ILG,IG), TBARCS(ILG,IG),TBARGS(ILG,IG),                      
     1     THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),           
     2     HCPC  (ILG,IG),HCPG  (ILG,IG),FROOT(ILG,IG)
C
      REAL FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),
     1     TPONDC(ILG),   TPONDG(ILG),   TPNDCS(ILG),   TPNDGS(ILG),              
     2     EVAPC (ILG),   EVAPCG(ILG),   EVAPG (ILG),   EVAPCS(ILG),               
     3     EVPCSG(ILG),   EVAPGS(ILG),   QFREZC(ILG),   QFREZG(ILG),           
     4     QMELTC(ILG),   QMELTG(ILG),   EVAP  (ILG),
     5     RAICAN(ILG),   SNOCAN(ILG),   RAICNS(ILG),   SNOCNS(ILG),              
     6     FSVF  (ILG),   FSVFS (ILG),   CWCAP (ILG),   CWCAPS(ILG),               
     7     TCANO (ILG),   TCANS (ILG),   CHCAP (ILG),   CHCAPS(ILG),           
     8     CMASSC(ILG),   CMASCS(ILG),   ZSNOW (ILG),   ZPOND (ILG),
     9     GZEROC(ILG),   GZEROG(ILG),   GZROCS(ILG),   GZROGS(ILG),              
     A     G12C  (ILG),   G12G  (ILG),   G12CS (ILG),   G12GS (ILG),               
     B     G23C  (ILG),   G23G  (ILG),   G23CS (ILG),   G23GS (ILG),
     C     TSNOCS(ILG),   TSNOGS(ILG),   ZPLIMC(ILG),   ZPLIMG(ILG),
     D     ZPLMCS(ILG),   ZPLMGS(ILG)
C
C     * MAIN LONGITUDE ARRAYS FROM PHYSIC7.
C
      REAL PCPR  (ILG),   TA    (ILG)
C
C     * SOIL PROPERTY INPUT ARRAYS.
C
      REAL SAND  (ILG,IG),CLAY  (ILG,IG),ORGM  (ILG,IG),DRN   (ILG),
     1     DELZW (ILG,IG),ZBOTW (ILG,IG)
C
      REAL DELZ(IG)
C
C     * INTERNAL WORK ARRAYS USED THROUGHOUT CLASSW.
C     * FOR 2-D ARRAYS, THE SECOND INDEX REFERS TO THE GRID CELL
C     * SUBAREAS: CANOPY-SNOW, SNOW-GROUND, CANOPY-GROUND AND 
C     * BARE GROUND (VALUES 1 THROUGH 4, RESPECTIVELY).
C
      REAL TBARWC(  3, 3),TBARWG(  3, 3),TBRWCS(  3, 3),TBRWGS(  3, 3),
     1     THLQCO(  3, 3),THLQGO(  3, 3),THLQCS(  3, 3),THLQGS(  3, 3),               
     2     THICCO(  3, 3),THICGO(  3, 3),THICCS(  3, 3),THICGS(  3, 3),               
     3     HCPCO (  3, 3),HCPGO (  3, 3),HCPCS (  3, 3),HCPGS (  3, 3),
     4     BI    (  3, 3),THPORI(  3, 3),GRKSTI(  3, 3),PSISTI(  3, 3),
     5     GRKTLI(  3, 3),THLRTI(  3, 3),HCPS  (  3, 3),DELZZ (  3, 3)
C
      REAL ST    (  3,4), TST   (  3,4), RT    (  3,4), TRT   (  3,4),
     1     ALBST (  3,4), RHOST (  3,4), ZPONDT(  3,4), ZSNOWT(  3,4),
     2     EVT   (  3,4), HCPST (  3,4), RUNOFT(  3,4), XSNOWT(  3,4)
C
      REAL SUBLC (  3),   SUBLCS(  3),   WLOSTC(  3),   WLOSTG(  3),
     1     WLSTCS(  3),   WLSTGS(  3),   RAC   (  3),   RACS  (  3),
     2     SNC   (  3),   SNCS  (  3),   TSNOWC(  3),   TSNOWG(  3), 
     3     DT    (  3),   ZERO  (  3),
     4     RALB  (  3)
C
      INTEGER             ISAND (  3,3), IZERO (  3),   IGRN  (  3)
C
C     * INTERNAL WORK FIELDS FOR GRINFL/GRDRAN/ICEBAL (AND THEIR CALLED
C     * ROUTINES (I.E. WFILL,WFLOW,WEND).
C
      REAL ZMAT  (  3,   5,  4)
C
      REAL WMOVE (  3,   5),   TMOVE (  3,   5)
C
      REAL THLIQX(  3,  4),   THICEX(  3,  4),   TBARWX(  3,  4),
     1     DELZX (  3,  4),   ZBOTX (  3,  4),   PSIF  (  3,  4),
     2     THLINF(  3,  4),   GRKINF(  3,  4),   FDT   (  3,  4),
     3     TFDT  (  3,  4),   FDUMMY(  3,  4),   TDUMMY(  3,  4),
     4     ZRMDR (  3,  4)
C
      REAL THLMAX(  3, 3),     THTEST(  3, 3),     THLDUM(  3, 3),
     1     THIDUM(  3, 3),     TDUMW (  3, 3)
C
      REAL TRMDR (  3),    ZF    (  3),    FMAX  (  3),    TUSED (  3),
     1     RDUMMY(  3),    WEXCES(  3),    FDTBND(  3),    WADD  (  3),
     2     TADD  (  3),    WADJ  (  3),    TPONDW(  3),    DZF   (  3),
     3     DTFLOW(  3),    THLNLZ(  3),    THLQLZ(  3),    DZDISP(  3),
     4     WDISP (  3),    WABS  (  3),    ZMOVE (  3),    TBOT  (  3)  
C
      INTEGER              ICONT (  3),    IGRD  (  3),    IFILL (  3),
     1                     LZF   (  3),    NINF  (  3),    IFIND (  3),
     2                     ITER  (  3),    NEND  (  3),    ISIMP (  3)
C
C     * INTERNAL WORK ARRAYS FOR WPREP.
C
      REAL R     (  3),    S     (  3),    TR    (  3),    TS    (  3),
     1     RADD  (  3),    SADD  (  3)
C
C     * INTERNAL WORK ARRAYS FOR CANVAP.
C
      REAL EVLOST(  3),    RLOST (  3), TRANS(3) !HSuo Feb2013
C
      INTEGER              IROOT (  3)
C
C     * INTERNAL WORK ARRAYS FOR CHKWAT.
C
      REAL BAL   (  3)
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * INITIALIZATION OF WORK FIELDS AND SPECIFICATION OF SOIL
C     * CHARACTERISTICS.
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          ISAND (I,J)=NINT(SAND(I,J))                                               
          IF(SAND(I,J).GE.0.0) THEN
              THPORI(I,J)=(-0.126*SAND(I,J)+48.9)/100.0
              BI    (I,J)=0.159*CLAY(I,J)+2.91
              GRKSTI(I,J)=(10.0**(0.0153*SAND(I,J)-0.884))*7.0556E-6
              GRKTLI(I,J)=GRKSTI(I,J)/2.0
              PSISTI(I,J)=(10.0**(-0.0131*SAND(I,J)+1.88))/100.0
              THLRTI(I,J)=0.5**(1.0/(2.0*BI(I,J)+3.0))
          ELSE
              THPORI(I,J)=0.0
              BI    (I,J)=0.0
              GRKSTI(I,J)=0.0
              GRKTLI(I,J)=0.0
              PSISTI(I,J)=0.0
              THLRTI(I,J)=0.0
          ENDIF
  100 CONTINUE
C
      NLANDGL=0                                                                                  
      DO 150 I=IL1,IL2
          IF(ISAND(I,1).EQ.-4)           NLANDGL=NLANDGL+1
          DT    (I)=DELT
          ZERO  (I)=0.
          IZERO (I)=0 
          DELZZ (I,1)=DELZ(1)
          DELZZ (I,2)=DELZ(2)
          DELZZ (I,3)=DELZW(I,3)
  150 CONTINUE
C
C     * PREPARATION.
C
      CALL WPREP(THLQCO, THLQGO, THLQCS, THLQGS, THICCO, THICGO,
     1           THICCS, THICGS, HCPCO,  HCPGO,  HCPCS,  HCPGS,
     2           ST,     TST,    RT,     TRT,    ALBST,  RHOST,
     3           ZPONDT, ZSNOWT, EVT,    HCPST,  RUNOFT, XSNOWT,
     4           SUBLC,  SUBLCS, WLOSTC, WLOSTG, WLSTCS, WLSTGS,
     5           RAC,    RACS,   SNC,    SNCS,   TSNOWC, TSNOWG,
     6           PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     7           QFN,    QFG,    QFC,    HMFN,   HMFG,   
     8           ROFC,   ROFN,   OVRFLW, HCPS,   FSVF,   FSVFS,
     9           THLIQC, THLIQG, THICEC, THICEG, HCPC,   HCPG,
     A           FC,     FG,     FCS,    FGS,    PCPR,   TA,
     B           ZPOND,  ZSNOW,  ALBSNO, RHOSNO, EVAPC,  EVAPCG,
     C           EVAPG,  EVAPCS, EVPCSG, EVAPGS, RAICAN, SNOCAN,
     D           RAICNS, SNOCNS, THPORI, SAND,   ORGM,   ISAND,  
     E           ILG,    IL1,    IL2,    JL,     IG,                        
     F           NLANDCS,NLANDGS,NLANDC, NLANDG,
     G           R,      S,      TR,     TS,     RADD,   SADD  )
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C
      IF(NLANDCS.GT.0)                                              THEN
          CALL CANVAP(THLQCS,EVAPCS,SUBLCS,RAICNS,SNOCNS,TCANS,
     1                ZSNOWT(1,1),WLSTCS,CHCAPS,QFCF,QFCL,QFN,QFC,
     2                HTCC,HTCS,HTC,TBARCS,CMASCS,FROOT,TSNOCS,
     3                RHOST(1,1),HCPST(1,1),THPORI,FCS,DELZW,
     4                IG,ILG,IL1,IL2,JL,EVLOST,RLOST,IROOT,N, TRANS ) !HSuo Feb2013
          CALL CANADD(RT(1,1),TRT(1,1),ST(1,1),TST(1,1),RAICNS,
     1                SNOCNS,TCANS,CHCAPS,HTCC,RADD,SADD,FSVFS,CWCAPS,
     2                CMASCS,FCS,ILG,IL1,IL2,JL)
          CALL CWCALC(TCANS,RAICNS,SNOCNS,CHCAPS,HMFC,HTCC,CMASCS,
     1                FCS,ILG,IL1,IL2)
C
          DO 200 I=IL1,IL2
              ROFC(I)=ROFC(I)+FCS(I)*(RADD(I)*RHOW+SADD(I)*RHOSNI)
              PCPN(I)=PCPN(I)+FCS(I)*(RADD(I)*RHOW+SADD(I)*RHOSNI)
              QFN(I) =QFN(I)+FCS(I)*EVPCSG(I)*RHOW
  200     CONTINUE
C
          CALL SUBCAN(2,FCS,RT(1,1),TRT(1,1),ST(1,1),TST(1,1),EVPCSG,
     1                QFN,QFG,PCPN,PCPG,ILG,IL1,IL2,JL)
          CALL TWCALC(TBARCS,THLQCS,THICCS,HCPCS,TBRWCS,HMFG,HTC,
     1                THPORI,ZERO,FCS,HCPS,ISAND,DELZW,DELZZ,
     2                IG,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDT(1,1),TPNDCS,ZSNOWT(1,1),TSNOCS,ALBST(1,1),
     1                RHOST(1,1),HCPST(1,1),GZROCS,HMFG,HTCS,HTC,WTRS,
     2                WTRG,TBARCS(1,1),TA,ZERO,FCS,ISAND,
     3                IG,ILG,IL1,IL2,JL) 
          CALL SNOVAP(RHOST(1,1),ZSNOWT(1,1),HCPST(1,1),TSNOCS,EVPCSG,
     1                QFN,QFG,HTCS,WLSTCS,FCS,RT(1,1),ST(1,1),ILG,
     2                IL1,IL2,JL)
          CALL TMELT(ZSNOWT(1,1),TSNOCS,QMELTC,RT(1,1),TRT(1,1),
     1               GZROCS,RALB,HMFN,HTCS,HTC,FCS,HCPST(1,1),
     2               RHOST(1,1),ISAND,IG,ILG,IL1,IL2)
          CALL SNOADD(ALBST(1,1),TSNOCS,RHOST(1,1),ZSNOWT(1,1),
     1                HCPST(1,1),HTCS,FCS,ST(1,1),TST(1,1),
     2                ILG,IL1,IL2,JL)
          CALL SNINFL(RT(1,1),TRT(1,1),ZSNOWT(1,1),TSNOCS,RHOST(1,1),
     1                HCPST(1,1),HTCS,HMFN,PCPG,ROFN,FCS,ILG,IL1,IL2,JL)
          CALL GRINFL(1,THLQCS,THICCS,TBRWCS,RUNOFT(1,1),QFG,WLSTCS,
     1                IGRN,FCS,DT,BI,DRN,EVPCSG,RT(1,1),TRT(1,1),
     2                TPNDCS,ZPONDT(1,1),THPORI,PSISTI,GRKSTI,GRKTLI,
     3                THLRTI,ZERO,DELZW,ZBOTW,ISAND,IZERO,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     6                DELZX,ZBOTX,PSIF,THLINF,GRKINF,FDT,TFDT,
     7                FDUMMY,TDUMMY,ZRMDR,THLMAX,THTEST,THLDUM,
     8                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     9                WEXCES,FDTBND,WADD,TADD,WADJ,TPONDW,DZF,
     A                DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,
     B                WABS,IGRD,IFILL,LZF,
     C                NINF,IFIND,ITER,NEND,ISIMP         )
          CALL GRDRAN(1,THLQCS,THICCS,TBRWCS,THLMAX,THTEST,
     1                FDUMMY,TDUMMY,RUNOFT(1,1),QFG,WLSTCS,
     2                FCS,DT,BI,DRN,EVPCSG,RT(1,1),ZPONDT(1,1),
     3                THPORI,PSISTI,GRKSTI,DELZW,ISAND,IGRN,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                WEXCES,IGRD                        )
          CALL TMCALC(TBARCS,THLQCS,THICCS,HCPCS,TPNDCS,ZPONDT(1,1),
     1                RUNOFT(1,1),OVRFLW,ZSNOWT(1,1),TSNOCS,ALBST(1,1),
     2                RHOST(1,1),HCPST(1,1),TBASE,HMFG,HTC,HTCS,WTRS,
     3                WTRG,TBRWCS,THPORI,GZROCS,G12CS,G23CS,FCS,TA,
     4                HCPS,ZPLMCS,ISAND,DELZW,DELZZ,DELZ,IG,ILG,
     5                IL1,IL2,JL)
          CALL CHKWAT(1,THLQCS,THICCS,THLIQC,THICEC,THPORI,
     1                EVT(1,1),FCS,FGS,FCS,RAICNS,SNOCNS,RUNOFT(1,1),
     2                PCPR,RACS,SNCS,WLSTCS,ZSNOWT(1,1),RHOST(1,1),SNO,
     3                ZPOND,ZPONDT(1,1),ISAND,DELZW,BAL,DELT,
     4                IG,ILG,IL1,IL2,JL                        ) 
          CALL SNOALBW(ALBST(1,1),RHOST(1,1),ZSNOWT(1,1),HCPST(1,1),
     1                 XSNOWT(1,1),FCS,ISAND,ST(1,1),RALB,
     2                 ILG,IG,IL1,IL2,JL)       
      ENDIF                                                               
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
          CALL TWCALC(TBARGS,THLQGS,THICGS,HCPGS,TBRWGS,HMFG,HTC,
     1                THPORI,ZERO,FGS,HCPS,ISAND,DELZW,DELZZ,
     2                IG,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDT(1,2),TPNDGS,ZSNOWT(1,2),TSNOGS,ALBST(1,2),
     1                RHOST(1,2),HCPST(1,2),GZROGS,HMFG,HTCS,HTC,WTRS,
     2                WTRG,TBARGS(1,1),TA,ZERO,FGS,ISAND,
     3                IG,ILG,IL1,IL2,JL) 
          CALL SNOVAP(RHOST(1,2),ZSNOWT(1,2),HCPST(1,2),TSNOGS,EVAPGS,
     1                QFN,QFG,HTCS,WLSTGS,FGS,RT(1,2),ST(1,2),ILG,
     2                IL1,IL2,JL)  
          CALL TMELT(ZSNOWT(1,2),TSNOGS,QMELTG,RT(1,2),TRT(1,2),
     1               GZROGS,RALB,HMFN,HTCS,HTC,FGS,HCPST(1,2),
     2               RHOST(1,2),ISAND,IG,ILG,IL1,IL2)
          CALL SNOADD(ALBST(1,2),TSNOGS,RHOST(1,2),ZSNOWT(1,2),
     1                HCPST(1,2),HTCS,FGS,ST(1,2),TST(1,2),
     2                ILG,IL1,IL2,JL)
          CALL SNINFL(RT(1,2),TRT(1,2),ZSNOWT(1,2),TSNOGS,RHOST(1,2),
     1                HCPST(1,2),HTCS,HMFN,PCPG,ROFN,FGS,ILG,IL1,IL2,JL)
          IF(NLANDGL.NE.0)                                       THEN
              CALL ICEBAL(TBARGS,TPNDGS,ZPONDT(1,2),RUNOFT(1,2),OVRFLW,
     1                    QMELTG,TSNOGS,RHOST(1,2),ZSNOWT(1,2),
     2                    HCPST(1,2),HMFG,HTCS,HTC,WTRS,WTRG,HCPGS,FGS,
     3                    EVAPGS,RT(1,2),TRT(1,2),GZROGS,G12GS,G23GS,
     4                    ISAND,DELZ,IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                    ZMAT,TMOVE,WMOVE,ZRMDR,TADD,ZMOVE,TBOT,ICONT )
          ENDIF
          CALL GRINFL(2,THLQGS,THICGS,TBRWGS,RUNOFT(1,2),QFG,WLSTGS,
     1                IGRN,FGS,DT,BI,DRN,EVAPGS,RT(1,2),TRT(1,2),
     2                TPNDGS,ZPONDT(1,2),THPORI,PSISTI,GRKSTI,GRKTLI,
     3                THLRTI,ZERO,DELZW,ZBOTW,ISAND,IZERO,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     6                DELZX,ZBOTX,PSIF,THLINF,GRKINF,FDT,TFDT,
     7                FDUMMY,TDUMMY,ZRMDR,THLMAX,THTEST,THLDUM,
     8                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     9                WEXCES,FDTBND,WADD,TADD,WADJ,TPONDW,DZF,
     A                DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,
     B                WABS,IGRD,IFILL,LZF,
     C                NINF,IFIND,ITER,NEND,ISIMP         )
          CALL GRDRAN(2,THLQGS,THICGS,TBRWGS,THLMAX,THTEST,
     1                FDUMMY,TDUMMY,RUNOFT(1,2),QFG,WLSTGS,
     2                FGS,DT,BI,DRN,EVAPGS,RT(1,2),ZPONDT(1,2),
     3                THPORI,PSISTI,GRKSTI,DELZW,ISAND,IGRN,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                WEXCES,IGRD                        )
          CALL TMCALC(TBARGS,THLQGS,THICGS,HCPGS,TPNDGS,ZPONDT(1,2),
     1                RUNOFT(1,2),OVRFLW,ZSNOWT(1,2),TSNOGS,ALBST(1,2),
     2                RHOST(1,2),HCPST(1,2),TBASE,HMFG,HTC,HTCS,WTRS,
     3                WTRG,TBRWGS,THPORI,GZROGS,G12GS,G23GS,FGS,TA,
     4                HCPS,ZPLMGS,ISAND,DELZW,DELZZ,DELZ,IG,ILG,
     5                IL1,IL2,JL)
          CALL CHKWAT(2,THLQGS,THICGS,THLIQG,THICEG,THPORI,
     1                EVT(1,2),FCS,FGS,FGS,RAICNS,SNOCNS,RUNOFT(1,2),
     2                PCPR,RACS,SNCS,WLSTGS,ZSNOWT(1,2),RHOST(1,2),SNO,
     3                ZPOND,ZPONDT(1,2),ISAND,DELZW,BAL,DELT,
     4                IG,ILG,IL1,IL2,JL                        ) 
          CALL SNOALBW(ALBST(1,2),RHOST(1,2),ZSNOWT(1,2),HCPST(1,2),
     1                 XSNOWT(1,2),FGS,ISAND,ST(1,2),RALB,
     2                 ILG,IG,IL1,IL2,JL)       
      ENDIF                                                               
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C
      IF(NLANDC.GT.0)                                               THEN
          CALL CANVAP(THLQCO,EVAPC,SUBLC,RAICAN,SNOCAN,TCANO,
     1                ZSNOWT(1,3),WLOSTC,CHCAP,QFCF,QFCL,QFN,QFC,
     2                HTCC,HTCS,HTC,TBARC,CMASSC,FROOT,TSNOWC,
     3                RHOST(1,3),HCPST(1,3),THPORI,FC,DELZW,
     4                IG,ILG,IL1,IL2,JL,EVLOST,RLOST,IROOT,N, TRANS ) !HSuo Feb 2013
          CALL CANADD(RT(1,3),TRT(1,3),ST(1,3),TST(1,3),RAICAN,SNOCAN,
     1                TCANO,CHCAP,HTCC,RADD,SADD,FSVF,CWCAP,CMASSC,FC,
     2                ILG,IL1,IL2,JL)     
          CALL CWCALC(TCANO,RAICAN,SNOCAN,CHCAP,HMFC,HTCC,CMASSC,
     1                FC,ILG,IL1,IL2)
C
          DO 300 I=IL1,IL2
              ROFC(I)=ROFC(I)+FC(I)*(RADD(I)*RHOW+SADD(I)*RHOSNI)
              PCPN(I)=PCPN(I)+FC(I)*SADD(I)*RHOSNI
              PCPG(I)=PCPG(I)+FC(I)*RADD(I)*RHOW
              QFG(I) =QFG(I)+FC(I)*EVAPCG(I)*RHOW
  300     CONTINUE
C
          CALL SUBCAN(1,FC,RT(1,3),TRT(1,3),ST(1,3),TST(1,3),EVAPCG,
     1                QFN,QFG,PCPN,PCPG,ILG,IL1,IL2,JL)
          CALL TWCALC(TBARC,THLQCO,THICCO,HCPCO,TBARWC,HMFG,HTC,
     1                THPORI,EVAPCG,FC,HCPS,ISAND,DELZW,DELZZ,
     2                IG,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDT(1,3),TPONDC,ZSNOWT(1,3),TSNOWC,ALBST(1,3),
     1                RHOST(1,3),HCPST(1,3),GZEROC,HMFG,HTCS,HTC,WTRS,
     2                WTRG,TBARC(1,1),TA,QFREZC,FC,ISAND,
     3                IG,ILG,IL1,IL2,JL) 
          CALL SNOADD(ALBST(1,3),TSNOWC,RHOST(1,3),ZSNOWT(1,3),
     1                HCPST(1,3),HTCS,FC,ST(1,3),TST(1,3),
     2                ILG,IL1,IL2,JL)
          CALL GRINFL(3,THLQCO,THICCO,TBARWC,RUNOFT(1,3),QFG,WLOSTC,
     1                IGRN,FC,DT,BI,DRN,EVAPCG,RT(1,3),TRT(1,3),
     2                TPONDC,ZPONDT(1,3),THPORI,PSISTI,GRKSTI,GRKTLI,
     3                THLRTI,ZERO,DELZW,ZBOTW,ISAND,IZERO,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     6                DELZX,ZBOTX,PSIF,THLINF,GRKINF,FDT,TFDT,
     7                FDUMMY,TDUMMY,ZRMDR,THLMAX,THTEST,THLDUM,
     8                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     9                WEXCES,FDTBND,WADD,TADD,WADJ,TPONDW,DZF,
     A                DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,
     B                WABS,IGRD,IFILL,LZF,
     C                NINF,IFIND,ITER,NEND,ISIMP         )
          CALL GRDRAN(3,THLQCO,THICCO,TBARWC,THLMAX,THTEST,
     1                FDUMMY,TDUMMY,RUNOFT(1,3),QFG,WLOSTC,
     2                FC,DT,BI,DRN,EVAPCG,RT(1,3),ZPONDT(1,3),
     3                THPORI,PSISTI,GRKSTI,DELZW,ISAND,IGRN,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                WEXCES,IGRD                        )
          CALL TMCALC(TBARC,THLQCO,THICCO,HCPCO,TPONDC,ZPONDT(1,3),
     1                RUNOFT(1,3),OVRFLW,ZSNOWT(1,3),TSNOWC,ALBST(1,3),
     2                RHOST(1,3),HCPST(1,3),TBASE,HMFG,HTC,HTCS,WTRS,
     3                WTRG,TBARWC,THPORI,GZEROC,G12C,G23C,FC,TA,
     4                HCPS,ZPLIMC,ISAND,DELZW,DELZZ,DELZ,IG,ILG,
     5                IL1,IL2,JL)
          CALL CHKWAT(3,THLQCO,THICCO,THLIQC,THICEC,THPORI,
     1                EVT(1,3),FC,FG,FC,RAICAN,SNOCAN,RUNOFT(1,3),
     2                PCPR,RAC,SNC,WLOSTC,ZSNOWT(1,3),RHOST(1,3),SNO,
     3                ZPOND,ZPONDT(1,3),ISAND,DELZW,BAL,DELT,
     4                IG,ILG,IL1,IL2,JL                        ) 
C
          DO 400 I=IL1,IL2
              IF(ZSNOWT(I,3).GT.0.0) XSNOWT(I,3)=1.0
  400     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR BARE GROUND.
C
      IF(NLANDG.GT.0)                                               THEN
          CALL TWCALC(TBARG,THLQGO,THICGO,HCPGO,TBARWG,HMFG,HTC,
     1                THPORI,EVAPG,FG,HCPS,ISAND,DELZW,DELZZ,
     2                IG,ILG,IL1,IL2,JL)             
          CALL TFREEZ(ZPONDT(1,4),TPONDG,ZSNOWT(1,4),TSNOWG,ALBST(1,4),
     1                RHOST(1,4),HCPST(1,4),GZEROG,HMFG,HTCS,HTC,WTRS,
     2                WTRG,TBARG(1,1),TA,QFREZG,FG,ISAND,
     3                IG,ILG,IL1,IL2,JL) 
          CALL SNOADD(ALBST(1,4),TSNOWG,RHOST(1,4),ZSNOWT(1,4),
     1                HCPST(1,4),HTCS,FG,ST(1,4),TST(1,4),
     2                ILG,IL1,IL2,JL)
          IF(NLANDGL.NE.0)                                       THEN
              CALL ICEBAL(TBARG,TPONDG,ZPONDT(1,4),RUNOFT(1,4),OVRFLW,
     1                    QFREZG,TSNOWG,RHOST(1,4),ZSNOWT(1,4),
     2                    HCPST(1,4),HMFG,HTCS,HTC,WTRS,WTRG,HCPGO,FG,
     3                    EVAPG,RT(1,4),TRT(1,4),GZEROG,G12G,G23G,
     4                    ISAND,DELZ,IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                    ZMAT,TMOVE,WMOVE,ZRMDR,TADD,ZMOVE,TBOT,ICONT )
          ENDIF
          CALL GRINFL(4,THLQGO,THICGO,TBARWG,RUNOFT(1,4),QFG,WLOSTG,
     1                IGRN,FG,DT,BI,DRN,EVAPG,RT(1,4),TRT(1,4),
     2                TPONDG,ZPONDT(1,4),THPORI,PSISTI,GRKSTI,GRKTLI,
     3                THLRTI,ZERO,DELZW,ZBOTW,ISAND,IZERO,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     6                DELZX,ZBOTX,PSIF,THLINF,GRKINF,FDT,TFDT,
     7                FDUMMY,TDUMMY,ZRMDR,THLMAX,THTEST,THLDUM,
     8                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     9                WEXCES,FDTBND,WADD,TADD,WADJ,TPONDW,DZF,
     A                DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,
     B                WABS,IGRD,IFILL,LZF,
     C                NINF,IFIND,ITER,NEND,ISIMP         )
          CALL GRDRAN(4,THLQGO,THICGO,TBARWG,THLMAX,THTEST,
     1                FDUMMY,TDUMMY,RUNOFT(1,4),QFG,WLOSTG,
     2                FG,DT,BI,DRN,EVAPG,RT(1,4),ZPONDT(1,4),
     3                THPORI,PSISTI,GRKSTI,DELZW,ISAND,IGRN,
     4                IG,IGP1,IGP2,ILG,IL1,IL2,JL,
     5                WEXCES,IGRD                        )
          CALL TMCALC(TBARG,THLQGO,THICGO,HCPGO,TPONDG,ZPONDT(1,4),
     1                RUNOFT(1,4),OVRFLW,ZSNOWT(1,4),TSNOWG,ALBST(1,4),
     2                RHOST(1,4),HCPST(1,4),TBASE,HMFG,HTC,HTCS,WTRS,
     3                WTRG,TBARWG,THPORI,GZEROG,G12G,G23G,FG,TA,
     4                HCPS,ZPLIMG,ISAND,DELZW,DELZZ,DELZ,IG,ILG,
     5                IL1,IL2,JL)
          CALL CHKWAT(4,THLQGO,THICGO,THLIQG,THICEG,THPORI,
     1                EVT(1,4),FC,FG,FG,RAICAN,SNOCAN,RUNOFT(1,4),
     2                PCPR,RAC,SNC,WLOSTG,ZSNOWT(1,4),RHOST(1,4),SNO,
     3                ZPOND,ZPONDT(1,4),ISAND,DELZW,BAL,DELT,
     4                IG,ILG,IL1,IL2,JL                        ) 
C
          DO 500 I=IL1,IL2
              IF(ZSNOWT(I,4).GT.0.0) XSNOWT(I,4)=1.0
  500     CONTINUE
      ENDIF
C
C     * AVERAGE RUNOFF AND PROGNOSTIC VARIABLES OVER FOUR GRID CELL
C     * SUBAREAS.
C
      IPTBAD=0
      DO 600 J=1,IG
      DO 600 I=IL1,IL2
          IF(J.LT.IG) THEN
              TBAR(I,J)=(FCS(I)*(TBARCS(I,J)+TFREZ)*(DELZW(I,J)*
     1                   HCPCS(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+
     2                   FGS(I)*(TBARGS(I,J)+TFREZ)*(DELZW(I,J)*
     3                   HCPGS(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+
     4                   FC (I)*(TBARC (I,J)+TFREZ)*(DELZW(I,J)*
     5                   HCPCO(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+             
     6                   FG (I)*(TBARG (I,J)+TFREZ)*(DELZW(I,J)*
     7                   HCPGO(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND))/
     8                  (FCS(I)*(DELZW(I,J)*HCPCS(I,J)+
     9                   (DELZ(J)-DELZW(I,J))*HCPSND) + 
     A                   FGS(I)*(DELZW(I,J)*HCPGS(I,J)+
     B                   (DELZ(J)-DELZW(I,J))*HCPSND) +
     C                   FC (I)*(DELZW(I,J)*HCPCO(I,J)+
     D                   (DELZ(J)-DELZW(I,J))*HCPSND) + 
     E                   FG (I)*(DELZW(I,J)*HCPGO(I,J)+
     F                   (DELZ(J)-DELZW(I,J))*HCPSND))              
          ELSE
              TBAR(I,J)=((FCS(I)*(TBARCS(I,J)+TFREZ)*HCPCS(I,J) +
     1                   FGS(I)*(TBARGS(I,J)+TFREZ)*HCPGS(I,J) +
     2                   FC (I)*(TBARC (I,J)+TFREZ)*HCPCO(I,J) +             
     3                   FG (I)*(TBARG (I,J)+TFREZ)*HCPGO(I,J))*
     4                   DELZW(I,J)+TBASE(I)*HCPSND*
     5                   (DELZ(J)-DELZW(I,J)))/
     4                  ((FCS(I)*HCPCS(I,J) + FGS(I)*HCPGS(I,J) +
     5                   FC (I)*HCPCO(I,J) + FG (I)*HCPGO(I,J))*
     8                   DELZW(I,J)+HCPSND*(DELZ(J)-DELZW(I,J)))             
          ENDIF
          THLIQ(I,J)=FCS(I)*THLQCS(I,J)+FGS(I)*THLQGS(I,J)+
     1               FC (I)*THLQCO(I,J)+FG (I)*THLQGO(I,J)                                   
          THICE(I,J)=FCS(I)*THICCS(I,J)+FGS(I)*THICGS(I,J)+
     1               FC (I)*THICCO(I,J)+FG (I)*THICGO(I,J)
C          IF(TBAR(I,1).LT.0. .OR. TBAR(I,1).GT.373.16) IPTBAD=I
  600 CONTINUE                                                            
C
      IF(IPTBAD.NE.0)                                               THEN
          WRITE(6,6600) IPTBAD,JL,TBAR(IPTBAD,1)
 6600     FORMAT('0AT (I,J)= (',I3,',',I3,'), TBAR(1) = ',F10.5)
          CALL XIT('CLASSW',-1)
      ENDIF
C
      JPTBAD=0
      KPTBAD=0
      LPTBAD=0
      DO 625 I=IL1,IL2 
          IF((FC(I)+FCS(I)).GT.0.)                                  THEN
              TCAN(I)=(FCS(I)*TCANS(I)*CHCAPS(I)+FC(I)*TCANO(I)*              
     1                CHCAP(I))/(FCS(I)*CHCAPS(I)+FC(I)*CHCAP(I))                 
          ELSE                                                                
              TCAN(I)=0.0                                                     
          ENDIF                                                               
          IF(TCAN(I).LT.0. .OR. TCAN(I).GT.373.16) JPTBAD=I
          RCAN  (I)=FCS(I)*RAICNS(I) + FC (I)*RAICAN(I)                            
          SCAN  (I)=FCS(I)*SNOCNS(I) + FC (I)*SNOCAN(I)                            
          RUNOFF(I)=FCS(I)*RUNOFT(I,1) + FGS(I)*RUNOFT(I,2) +
     1              FC (I)*RUNOFT(I,3) + FG (I)*RUNOFT(I,4) 
          RUNOFF(I)=RUNOFF(I)*RHOW/DELT                                       
          OVRFLW(I)=OVRFLW(I)*RHOW/DELT
          EVAP  (I)=EVAP(I)-(FCS(I)*WLSTCS(I)+FGS(I)*WLSTGS(I)+
     1              FC(I)*WLOSTC(I)+FG(I)*WLOSTG(I))/DELT
  625 CONTINUE
C
C     * THESE 2 LOOPS MIGHT BE COMBINED ON THE SX-3 (WOULDN'T WORK
C     * ON THE CRAY).
C
      DO 650 I=IL1,IL2     
          IF(ZSNOWT(I,3).GT.0. .OR. ZSNOWT(I,4).GT.0. .OR.
     1       ZSNOWT(I,1).GT.0. .OR. ZSNOWT(I,2).GT.0.)              THEN                                             
              IF(ZSNOWT(I,1).GT.0. .OR. ZSNOWT(I,2).GT.0.)    THEN                         
                  ALBSNO(I)=(FCS(I)*ALBST(I,1)*XSNOWT(I,1) +
     1                       FGS(I)*ALBST(I,2)*XSNOWT(I,2))/
     2                      (FCS(I)*XSNOWT(I,1)+FGS(I)*XSNOWT(I,2))                   
              ELSE                                                            
                  ALBSNO(I)=(FC (I)*ALBST(I,3)*XSNOWT(I,3) +
     1                       FG (I)*ALBST(I,4)*XSNOWT(I,4))/
     2                      (FC (I)*XSNOWT(I,3)+FG (I)*XSNOWT(I,4))                     
              ENDIF                                                           
              TSNOW(I)=(FCS(I)*(TSNOCS(I)+TFREZ)*HCPST(I,1)*
     1                  ZSNOWT(I,1)*XSNOWT(I,1) +                
     2                  FGS(I)*(TSNOGS(I)+TFREZ)*HCPST(I,2)*
     3                  ZSNOWT(I,2)*XSNOWT(I,2) +                      
     4                  FC (I)*(TSNOWC(I)+TFREZ)*HCPST(I,3)*
     5                  ZSNOWT(I,3)*XSNOWT(I,3) +                          
     6                  FG (I)*(TSNOWG(I)+TFREZ)*HCPST(I,4)*
     7                  ZSNOWT(I,4)*XSNOWT(I,4))/                         
     8                 (FCS(I)*HCPST(I,1)*ZSNOWT(I,1)*XSNOWT(I,1) +                               
     9                  FGS(I)*HCPST(I,2)*ZSNOWT(I,2)*XSNOWT(I,2) +                                
     A                  FC (I)*HCPST(I,3)*ZSNOWT(I,3)*XSNOWT(I,3) +                                 
     B                  FG (I)*HCPST(I,4)*ZSNOWT(I,4)*XSNOWT(I,4))
              RHOSNO(I)=(FCS(I)*RHOST(I,1)*ZSNOWT(I,1)*XSNOWT(I,1) +                         
     1                   FGS(I)*RHOST(I,2)*ZSNOWT(I,2)*XSNOWT(I,2) +                                
     2                   FC (I)*RHOST(I,3)*ZSNOWT(I,3)*XSNOWT(I,3) +                                 
     3                   FG (I)*RHOST(I,4)*ZSNOWT(I,4)*XSNOWT(I,4))/                                
     4                  (FCS(I)*ZSNOWT(I,1)*XSNOWT(I,1) +
     5                   FGS(I)*ZSNOWT(I,2)*XSNOWT(I,2) +                 
     6                   FC (I)*ZSNOWT(I,3)*XSNOWT(I,3) +
     7                   FG (I)*ZSNOWT(I,4)*XSNOWT(I,4))                    
              ZSNOW(I)=FCS(I)*ZSNOWT(I,1) + FGS(I)*ZSNOWT(I,2) +
     1                 FC (I)*ZSNOWT(I,3) + FG (I)*ZSNOWT(I,4)
              SNO(I)=ZSNOW(I)*RHOSNO(I)                                       
              IF(SNO(I).LT.1.0E-9) THEN
                  SNO(I)=0.0                            
              ENDIF
          ELSE                                                                
              TSNOW(I)=0.0                                                    
              RHOSNO(I)=0.0                                                   
              SNO(I)=0.0                                                      
          ENDIF
C
C          IF(TSNOW(I).LT.0.0) KPTBAD=I
          IF((TSNOW(I)-TFREZ).GT.1.0E-3) LPTBAD=I
  650 CONTINUE
C
      DO 700 I=IL1,IL2     
          IF(DELZW(I,1).GT.0.0) THEN
              WL(I)=(DELZW(I,1)*THLIQ(I,1)/MAX(THPORI(I,1),1.0E-4)+
     1               DELZW(I,2)*THLIQ(I,2)/MAX(THPORI(I,2),1.0E-4)+
     2               DELZW(I,3)*THLIQ(I,3)/MAX(THPORI(I,3),1.0E-4))/
     3              (DELZW(I,1)+DELZW(I,2)+DELZW(I,3))
              WF(I)=(DELZW(I,1)*THICE(I,1)/MAX(THPORI(I,1),1.0E-8)+
     1               DELZW(I,2)*THICE(I,2)/MAX(THPORI(I,2),1.0E-4)+
     2               DELZW(I,3)*THICE(I,3)/MAX(THPORI(I,3),1.0E-4))/
     3              (DELZW(I,1)+DELZW(I,2)+DELZW(I,3))
              WLA(I)=RHOW*(DELZW(I,1)*THLIQ(I,1)+DELZW(I,2)*THLIQ(I,2)+
     1               DELZW(I,3)*THLIQ(I,3))
              WFA(I)=RHOICE*(DELZW(I,1)*THICE(I,1)+DELZW(I,2)*
     1               THICE(I,2)+DELZW(I,3)*THICE(I,3))
              WLAS(I)=RHOW*DELZW(I,1)*THLIQ(I,1)
              WFAS(I)=RHOICE*DELZW(I,1)*THICE(I,1)
          ELSE
              WL(I)=0.0
              WF(I)=0.0
              WLA(I)=0.0
              WFA(I)=0.0
              WLAS(I)=0.0
              WFAS(I)=0.0
          ENDIF
700   CONTINUE
C
      IF(JPTBAD.NE.0)                                               THEN
          WRITE(6,6625) JPTBAD,JL,TCAN(JPTBAD)
 6625     FORMAT('0AT (I,J)= (',I3,',',I3,'), TCAN = ',F10.5)
          CALL XIT('CLASSW',-2)
      ENDIF
C
      IF(KPTBAD.NE.0)                                               THEN
          WRITE(6,6626) KPTBAD,JL,TSNOW(KPTBAD)
 6626     FORMAT('0AT (I,J)= (',I3,',',I3,'), TSNOW = ',F10.5)
          CALL XIT('CLASSW',-3)
      ENDIF
C
      IF(LPTBAD.NE.0)                                               THEN
          WRITE(6,6626) LPTBAD,JL,TSNOW(LPTBAD)
          CALL XIT('CLASSW',-4)
      ENDIF
C
      CALL CGROW(GROWTH,TBAR,TA,ILG,IL1,IL2,JL,IG)
C                                                                                  
      RETURN                                                                      
      END        
