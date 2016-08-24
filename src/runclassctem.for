C
      PROGRAM RUNCLASSCTEM
C=====================FIRST SET OF DEFINITIONS:==================================================
C     * BACKGROUND VARIABLES BY THE GCM (SUFFIX "GAT"). THESE VARIABLES UNDERGO A "GATHER-SCATTER" 
C     * PROCEDURE WITHIN THE GCM AS PART OF THE VECTORIZATION REQUIREMENTS.)

      REAL          FCANGAT(  3,5),  LNZ0GAT(  3,5),
     1              ALVCGAT(  3,5),  ALICGAT(  3,5),
     2              LAMXGAT(  3,4),  LAMNGAT(  3,4),
     3              CMASGAT(  3,4),  ROOTGAT(  3,4),
     4              SANDGAT(  3,3),  CLAYGAT(  3,3),
     5              ORGMGAT(  3,3),  DRNGAT (    3),
     6              SOILGAT(  3,3,3),SBNDGAT(  3,2)
C
      REAL          TBARGAT(  3,3),  THLQGAT(  3,3),
     1              THICGAT(  3,3),  HTCGAT (  3,3),
     2              QFCGAT (  3,3),  HMFGGAT(  3,3),
     3              SGLGAT (  3), SHLGAT (  3), 
     4              HFSGAT (  3), QFSGAT (  3), TBASGAT(  3),
     5              ROFGAT (  3), THLGAT (  3),
     6              ULGAT  (  3), VLGAT  (  3), UVGAT  (  3),
     7              QSGAT  (  3), PREGAT (  3), TSGAT  (  3),
     8              ENVGAT (  3), CMAIGAT(  3), FSVGAT (  3),
     9              FSIGAT (  3), FSFGAT (  3), FDLGAT (  3),
     A              FSVHGAT(  3), FSIHGAT(  3), CSZGAT (  3),
     B              PRESGAT(  3), GCGAT  (  3)
C
      REAL          DRGAT  (  3), GTGAT  (  3), SNOGAT (  3),
     1              ALBSGAT(  3), RHOSGAT(  3), TSNOGAT(  3),
     2              TTGAT  (  3), TCANGAT(  3), WCANGAT(  3),
     3              SCANGAT(  3), QEVPGAT(  3), CDHGAT (  3),
     4              CDMGAT (  3), QGGAT  (  3), TFXGAT (  3),
     5              QFXGAT (  3), SFCTGAT(  3), SFCQGAT(  3),
     6              SFCUGAT(  3), SFCVGAT(  3), EFGAT  (  3),
     7              WLGAT  (  3), WFGAT  (  3), ALSWGAT(  3),
     8              ALLWGAT(  3), FSGVGAT(  3), FSGSGAT(  3),
     9              FSGGGAT(  3), FLGVGAT(  3), FLGSGAT(  3), 
     A              FLGGGAT(  3), HFSCGAT(  3), HFSSGAT(  3),
     B              HFSGGAT(  3), HEVCGAT(  3), HEVSGAT(  3),
     C              HEVGGAT(  3), HMFCGAT(  3), HTCSGAT(  3),
     D              PCFCGAT(  3), PCLCGAT(  3), PCPNGAT(  3),
     E              PCPGGAT(  3), QFGGAT (  3), QFNGAT (  3),
     F              QFCLGAT(  3), QFCFGAT(  3), HMFNGAT(  3),
     G              HTCCGAT(  3), XDIFFUS(  3), ROFOGAT(  3),
     H              ROFCGAT(  3), ROFNGAT(  3), WTRCGAT(  3),
     I              WTRSGAT(  3), WTRGGAT(  3), WLAGAT (  3),
     J              WFAGAT (  3), WLASGAT(  3), WFASGAT(  3)

      REAL*8        RADJ   (  3)
      INTEGER       ILAND  (  3), ITERCT (3,6,50),
     1              NSNO   (  3), ILSL   (  3)
      INTEGER       ITERSM (  6), ITER10 (  6), ITER25 (  6),
     1              ITER49 (  6), ITER50 (  6), ITER5  (  6)

C     * ARRAYS DEFINED TO PASS INFORMATION BETWEEN "CLASSA", "CLASST" AND "CLASSW".

      REAL          TBARC  (  3,3), TBARG  (  3,3),
     1              TBARCS (  3,3), TBARGS (  3,3),
     2              THLIQC (  3,3), THLIQG (  3,3),
     3              THICEC (  3,3), THICEG (  3,3),
     4              HCPC   (  3,3), HCPG   (  3,3),
     5              FROOT  (  3,3)

      REAL  FC     (  3),FG     (  3),FCS    (  3),FGS    (  3),
     1      ALVSCN (  3),ALIRCN (  3),ALVSG  (  3),ALIRG  (  3),
     2      ALVSCS (  3),ALIRCS (  3),ALVSSN (  3),ALIRSN (  3),
     3      TRVSCN (  3),TRIRCN (  3),TRVSCS (  3),TRIRCS (  3),
     4      FSVF   (  3),FSVFS  (  3),FRAINC (  3),FSNOWC (  3),
     5      RAICAN (  3),SNOCAN (  3),RAICNS (  3),SNOCNS (  3),
     6      CWCAP  (  3),CWCAPS (  3),DISP   (  3),DISPS  (  3),
     7      ZOMLNC (  3),ZOELNC (  3),ZOMLNG (  3),ZOELNG (  3),
     8      ZOMLCS (  3),ZOELCS (  3),ZOMLNS (  3),ZOELNS (  3),
     9      RCMIN  (  3),RCMINS (  3),CMASSC (  3),CMASCS (  3),
     A      CHCAP  (  3),CHCAPS (  3),TCANO  (  3),TCANS  (  3),
     B      ZPOND  (  3),TPONDC (  3),TPONDG (  3),
     C      TPNDCS (  3),TPNDGS (  3),ZSNOW  (  3),TRSNOW (  3),
     D      TSNOCS (  3),TSNOGS (  3),QFREZC (  3),QFREZG (  3),
     E      QMELTC (  3),QMELTG (  3),GZEROC (  3),GZEROG (  3),
     F      GZROCS (  3),GZROGS (  3),G12C   (  3),G12G   (  3),
     G      G12CS  (  3),G12GS  (  3),G23C   (  3),G23G   (  3),
     H      G23CS  (  3),G23GS  (  3),EVAPC  (  3),EVAPCG (  3),
     I      EVAPG  (  3),EVAPCS (  3),EVPCSG (  3),EVAPGS (  3),
     J      ZPLMCS (  3),ZPLMGS (  3),ZPLIMC (  3),ZPLIMG (  3)

C     * INTERNAL ARRAYS USED IN CLASSA.
C
      REAL RMAT  (  3,4,3), H     (  3,4),   HS    (  3,4),
     1     AIL   (  3,4),   AILS  (  3,4),
     2     FCAN  (  3,4),   FCANS (  3,4), 
     3     CWCPAV(  3),     AILCAN(  3),     AILCNS(  3),   
     4     FSNOW (  3),     FCLOUD(  3),     GROWA (  3),
     5     GROWN (  3),     GROWB (  3),
     6     RRESID(  3),     SRESID(  3),
     7     FORG  (  3,3),   THPOR (  3,3),   HCPS  (  3,3)

      INTEGER               ISAND (  3,3),   ICLAY (  3,3) 

C     * CANOPY AND SOIL INFORMATION ARRAYS.SOIL LAYERS (3); BROAD VEGETATION CATEGORIES (4)

      REAL DELZ(3), ZBOT(3), DELZW(3,3), ZBOTW(3,3), ZORAT(4),
     1     THPORA(3), HCPGND(3)

C     * OUTPUT AND DISPLAY ARRAYS."ACC" REFERS TO ACCUMULATOR ARRAYS USED IN CALCULATING TIME AVERAGES

      REAL          DAYTOT (12)
      INTEGER       MONEND (12)
      CHARACTER     TITLE1*4,     TITLE2*4,     TITLE3*4,
     1              TITLE4*4,     TITLE5*4,     TITLE6*4
      CHARACTER     NAME1*4,      NAME2*4,      NAME3*4,
     1              NAME4*4,      NAME5*4,      NAME6*4
      CHARACTER     PLACE1*4,     PLACE2*4,     PLACE3*4,
     1              PLACE4*4,     PLACE5*4,     PLACE6*4

      REAL          PREACC (  3), GTACC  (  3), QEVPACC(  3),
     1              HFSACC (  3), ROFACC (  3), SNOACC (  3),
     2              ALSWACC(  3), ALLWACC(  3), FSINACC(  3),
     3              FLINACC(  3), TSACC  (  3), UVACC  (  3),
     4              PRESACC(  3), QSACC  (  3),
     5              EVAPACC(  3), FLUTACC(  3), OVRACC (  3),
     6              HMFNACC(  3),
     7              RHOSACC(  3), TSNOACC(  3), TCANACC(  3),
     8              WCANACC(  3), SCANACC(  3), GROACC (  3)

      REAL          TBARACC(  3,3), THLQACC(  3,3),
     1              THICACC(  3,3), THALACC(  3,3)
C
C     * INTERNAL WORK ARRAYS FOR CLASST. 
C     * (3,4)The first:GCM layers;THE SECOND 4 INDEX REFERS TO CANOPY-SNOW,GROUND-SNOW,BARE CANOPY AND BARE-GROUND, RESPECTIVELY. 
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
     2     RHOAIR(  3),   ZLREF (  3),   CDOM  (  3),   CDOH  (  3),
     3     ZOSCLM(  3),   ZOSCLH(  3),   EVAPBC(  3),   EVAPBS(  3),
     4     GSNOWC(  3),   GSNOWG(  3),   TVIRTA(  3),   TADP  (  3),
     5     CRIB  (  3),   CEVAP (  3),   A1    (  3),
     6     A2    (  3),   A3    (  3),   B1    (  3),   B2    (  3),
     7     B3    (  3),   C2    (  3),   C3    (  3),   D3    (  3),
     8     GDENOM(  3),   GCOEFF(  3),   GCONST(  3),   TSTART(  3),
     9     CPHCHC(  3),   CPHCHG(  3),   QTRANS(  3),
     A     RCS   (  3),   CONST (  3),   ZOMS  (  3),   ZOHS  (  3)
C
      INTEGER             IEVAP (  3),   IWATER(  3),
     1                    KF    (  3),   KF1   (  3),   KF2   (  3)
C
C     * INTERNAL WORK ARRAYS FOR TPREP (POINTERS AFTER).
C
      REAL PSISAT(  3,3), BI    (  3,3), 
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
     7     XEVAP (  3),    TCDP  (  3), 
     8     RESIDL(  3),    TZEROL(  3),    TZEROO(  3),    TCANL (  3)
C
      INTEGER             ITER  (  3),   NITER (  3)
C
C     * INTERNAL WORK ARRAYS USED THROUGHOUT CLASSW.
C
      REAL TBARWC(  3, 3),TBARWG(  3, 3),TBRWCS(  3, 3),TBRWGS(  3, 3),
     1     THLQCO(  3, 3),THLQGO(  3, 3),THLQCS(  3, 3),THLQGS(  3, 3),               
     2     THICCO(  3, 3),THICGO(  3, 3),THICCS(  3, 3),THICGS(  3, 3),               
     3     HCPCO (  3, 3),HCPGO (  3, 3),HCPCS (  3, 3),HCPGS (  3, 3),
     4     GRKSAT(  3, 3),
     5     GRKTLD(  3, 3),THLRAT(  3, 3),DELZZ (  3, 3)
C
      REAL ST    (  3,4), TST   (  3,4), RT    (  3,4), TRT   (  3,4),
     1     ALBST (  3,4), RHOST (  3,4), ZPONDT(  3,4), ZSNOWT(  3,4),
     2     EVT   (  3,4), HCPST (  3,4), RUNOFT(  3,4), XSNOWT(  3,4)
C
      REAL SUBLC (  3),   SUBLCS(  3),   WLOSTC(  3),   WLOSTG(  3),
     1     WLSTCS(  3),   WLSTGS(  3),   RAC   (  3),   RACS  (  3),
     2     SNC   (  3),   SNCS  (  3),   TSNOWC(  3),   TSNOWG(  3), 
     3     XDRAIN(  3),   DT    (  3),   ZERO  (  3),
     4     RALB  (  3)
C
      INTEGER             IZERO (  3),   IGRN  (  3)
C
C     * INTERNAL WORK FIELDS FOR GRINFL/GRDRAN/ICEBAL (AND THEIR CALLED ROUTINES (I.E. WFILL,WFLOW,WEND).
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
     2                     NEND  (  3),    ISIMP (  3)
C
C     * INTERNAL WORK ARRAYS FOR WPREP.
C
      REAL R     (  3),    S     (  3),    TR    (  3),    TS    (  3),
     1     RADD  (  3),    SADD  (  3)
C
C     * INTERNAL WORK ARRAYS FOR CANVAP.
C
      REAL EVLOST(  3),    RLOST (  3)
C
      INTEGER              IROOT (  3)
C
C     * INTERNAL WORK ARRAYS FOR CHKWAT.
C
      REAL BAL   (  3)
C
C     -------------------- CTEM MODIFICATIONS -------------------------\
C
C     DECLARE ALL CTEM RELATED NEW VARIABLES. EXPLANATIONS IN SUBROUTINES.
C
      INTEGER      ICC,        STDALN(3),  PANDAYS(3,9), LFSTATUS(3,9),  
     1               M,                N, COLDDAYS(3,2),      CTEMLOOP,
     2        LOPCOUNT,             ISUM,   NOL2PFTS(4),         L2MAX,
     3              K1,               K2,     ICOUNT(3)
C
      LOGICAL    CTEM1,            CTEM2,       LANDUSE 
C
      CHARACTER   TITLEC*80
C
      REAL FCANCMX(3,9),    RMATC(3,4,3),    ZOLNC(3,4),     AILC(3,4),
     1       AILCG(3,9),     AILCGS(3,9),   FCANCS(3,9),    FCANC(3,9),
     2       CO2CONC(3),    CO2I1CG(3,9),  CO2I1CS(3,9),  CO2I2CG(3,9),
     3     CO2I2CS(3,9),    ANCSVEG(3,9),  ANCGVEG(3,9), RMLCSVEG(3,9),
     4    RMLCGVEG(3,9), RMATCTEM(3,9,3),     SLAI(3,9),   TCANOACC(3),
     5      TCANSACC(3),   TBARCACC(3,3),TBARCSACC(3,3), TBARGACC(3,3),
     6   TBARGSACC(3,3),   ANCSVGAC(3,9), ANCGVGAC(3,9), RMLCSVGA(3,9),
     7    RMLCGVGA(3,9),          DELTAT,      VVACC(3),    LIGHTNG(3),  
     8      PRBFRHUC(3),     EXTNPROB(3), STEMMASS(3,9), ROOTMASS(3,9),  
     9   LITRMASS(3,10),   GLEAFMAS(3,9), BLEAFMAS(3,9),SOILCMAS(3,10),
     A       AILCB(3,9),   FLHRLOSS(3,9),    GAVGLAI(3),
     B    GRWTHEFF(3,9),   LYSTMMAS(3,9), LYROTMAS(3,9), TYMAXLAI(3,9),
     C      VGBIOMAS(3),     GAVGLTMS(3),   GAVGSCMS(3), STMHRLOS(3,9),
     D           NPP(3),          NEP(3),   HETRORES(3),    AUTORES(3),
     E      SOILRESP(3),           RM(3),         RG(3),        NBP(3),
     F        LITRES(3),       SOCRES(3),        GPP(3),   DSTCEMLS(3),
     G      LITRFALL(3),     HUMIFTRS(3),  DVDFCAN(3,9),  BMASVEG(3,9),
     H    CMASVEGC(3,4),         ABSZERO,     CANRES(3),  VEGHGHT(3,9),
     I    ROOTDPTH(3,9),          RML(3),        RMS(3),        RMR(3),
     J    TLTRLEAF(3,9),   TLTRSTEM(3,9), TLTRROOT(3,9), LEAFLITR(3,9),
     K    ROOTTEMP(3,9),    AFRLEAF(3,9),  AFRSTEM(3,9),  AFRROOT(3,9),
     L    WTSTATUS(3,9),   LTSTATUS(3,9),  LAIMAXG(3,9),   AVGYRNPP(3),
     M      AVGYRGPP(3),      AVGYRLE(3),   AVGYRNEP(3),  AILCMIN(3,9),
     N     AILCMAX(3,9),   ROTHRLOS(3,9),  MODELPFT(12), PFCANCMX(3,9),
     O      LUCEMCOM(3),     BURNAREA(3),   PROBFIRE(3),   LUCLTRIN(3),
     P      LUCSOCIN(3),   NFCANCMX(3,9),  ALVSCTM(3,4),  ALIRCTM(3,4),
     Q    ANNPPVEG(3,9),     NPPVEG(3,9),    CFLUXCG(3),    CFLUXCS(3)

C
      REAL  FSNOWACC(3),  THLIQCACC(3,3), THLIQGACC(3,3),  AILACC(3,4),
     1   THICECACC(3,3),     GRCLAREA(3) 
C
      REAL   ANVEG(3,9),     RMLVEG(3,9), MLIGHTNG(3,12),    CSUM(3,4)
C
C
C     -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
      LOGICAL CTEMN,          CALSOL(3)
C
      INTEGER NDAYTIME(3)
C
      REAL RNLEAF(3, 9),      RNLFB(3, 9),  RNSTEM(3, 9),  RNROOT(3, 9)
      REAL RNLITR(3, 10),     RNSOM(3, 10) 
      REAL SNH4(3, 10),       SNO3(3,10)
      REAL NRUB(3, 9),        NRUB0(3, 9)
C
      REAL VCMX0(3, 9),       BTDPTH(3)														  
C
      REAL DNDEPGRD(3),       DNFERGRD(3),        DNPLTRGRD(3),          &
     1     DNDISGRD(3),       DNLSOMGRD(3),       DNMINGRD(3),           &
     2     DNNITGRD(3),       DNPUPGRD(3),        DNDNITGRD(3),          & 
     3     DNLEAGRD(3),       DNVOLGRD(3),        DNSORGRD(3),           &
     4     DNLOSGRD(3),       
C    ------------------------------------------------------------!HSuo
     5 N2OTOTGRD(3),N2TOTGRD(3),DNBFIXGRD(3),DNPLOSSGRD(3),
     6 DNSLOSSGRD(3),NLITR_HS(3,10),NSOM_HS(3,10)
C    ------------------------------------------------------------!HSuo
C
      REAL ET0,           JMAX,       KL,         KM,     KMIN0,         &
     1     RTMASS0,       SOLNH4
      REAL RNLF0,         RNSM0,      RNRT0
      REAL CONREAL
      REAL LAI0,			KRUBN,      KN
      REAL KNI0,          KDN0,       KV0
      REAL NBFIX0,        NDEP0,      NFER0,      NFERO0
C
      REAL ETP(3),        XMINF(3,9)
      REAL ETPACC(3),     XMINFBAR(3,9)
      REAL GZEROGAT(3),   G0ACC(3)
C
      REAL YRNDEP(3),     YRNFER(3),  YRNPLTR(3), YRNDIS(3),             &
     1     YRNLSOM(3),    YRNMIN(3),  YRNNIT(3),  YRNPUP(3),             &
     2     YRNDNIT(3),    YRNLEA(3),  YRNVOL(3),  YRNSRCE(3),            &
     3     YRNLOSS(3),    
C-----------------------------------------------------------!HSuo
     4  YRNN2O(3),  YRNN2(3),YRNBFX(3),YRNPLOSS(3),YRNSLOSS(3)  
C-----------------------------------------------------------!HSuo

C	INITIAL C STOCKS ARE OBSERVED
      LOGICAL CSTOCKINI
      REAL    AILCMIN0(3,9)    
      REAL    AILCMAX0(3,9)    
      REAL    GLEAFMAS0(3,9)
      REAL    BLEAFMAS0(3,9)       
      REAL    STEMMASS0(3,9)       
      REAL    ROOTMASS0(3,9)
      REAL    LITRMASS0(3,10)       
      REAL    SOILCMAS0(3,10)       
C
      COMMON /NITROGEN/ ET0,  JMAX,   KL,     KM,     KMIN0,  RTMASS0,      &
     1                SOLNH4, RNLF0,  RNSM0,  RNRT0,  CONREAL,              &
     2                LAI0,   KRUBN,  KN,     KNI0,   KDN0,   KV0,          &
     3                NBFIX0, NDEP0,  NFER0,  NFERO0      
C-------------------------------------------------------------HSuo N add



C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C
C-------------------------------------------------------------HSuo testing\
      REAL  GPPVEG(3,9),GPP_GVEG(3,9),GPP_SVEG(3,9), 
C 	5	  NPPVEG(3,9),REVEG(3,9),
C 	5	  NPP_GVEG(3,9),NPP_SVEG(3,9),
C 	5	  RE_GVEG(3,9),RE_SVEG(3,9),
	1      JEVEG(3,9),JE_GVEG(3,9),JE_SVEG(3,9),
	2	  JCVEG(3,9),JC_GVEG(3,9),JC_SVEG(3,9),
	3	  JSVEG(3,9),JS_GVEG(3,9),JS_SVEG(3,9),
	4      IPAR_HS(3), !IPAR_GVEG(3),IPAR_SVEG(3),
	5	  FPARVEG(3,9),FPAR_GVEG(3,9),FPAR_SVEG(3,9),                         
	6      QSWV_HS(3), FCLOUD_HS(3),SM_HS(3)  ,
     7      VMVEG(3,9),VM_GVEG(3,9),VM_SVEG(3,9),
     8      VMUNSVEG(3,9), VMUNS_GVEG(3,9), VMUNS_SVEG(3,9),
     9      VMUNS1VEG(3,9),VMUNS1_GVEG(3,9),VMUNS1_SVEG(3,9),      
     A      VMUNS2VEG(3,9),VMUNS2_GVEG(3,9),VMUNS2_SVEG(3,9),
     B	  VMUNS3VEG(3,9),VMUNS3_GVEG(3,9),VMUNS3_SVEG(3,9),
     C      CO2IVEG(3,9),CO2I_GVEG(3,9), CO2I_SVEG(3,9),
     D      TGA_HS(3),KC_HS(3),KO_HS(3),TCAN_HS(3),FSEAS_HS(3),
     E      VMAXCVEG(3,9),VMAXC_GVEG(3,9),VMAXC_SVEG(3,9),
     F      DAY_HS,DECL_HS,HOUR_HS,COSZ_HS,QSWINV_HS(3),ALVISC_HS(3),
     G      QSWNVG_HS(3)

C************************PHYSICAL CONSTANTS.********************************
C
C     * THE COMMON BLOCK PARAMS REPRODUCES A BLOCK USED WITHIN THE GCM.
      COMMON /PARAMS/ WW,     TW,    A,     ASQ,  GRAV, RGAS,  RGOCP,
     1                RGOASQ, CPRES,  RGASV,  CPRESV

C     * THE FOLLOWING COMMON BLOCKS ARE DEFINED SPECIFICALLY FOR USE IN CLASS.
      COMMON /CLASS1/ DELTIM,CELZRO
      COMMON /CLASS2/ GAS,GASV,G,SIGMA,VKC,CT,SLTHICK,BEEM,ALFAH,
     1                FAC,GAMRH,GAMRM,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
      COMMON /CLASS6/ CPI,GROWYR(18,4,2),ZOLNG,ZOLNS,ZOLNI,ZORATG
      COMMON /CLASS7/ CANEXT(4)
C
      DATA    RVORD      ,TFREZ 
     1/       0.622      ,273.16  /
      DATA    SBC 
     1/       5.66796E-8 /
      DATA      VKC,        CT,         SLTHICK,    BEEM,       ALFAH
     1       /  0.40,       0.90E-3,    0.060,      10.0,       1.0    /
      DATA      FAC,        GAMRH,      GAMRM,      VMIN,       TCGLAC
     1       /  32.5,       6.00,       6.00,       0.1,        2.24   /
      DATA      TCW,        TCICE,      TCSAND,     TCCLAY,     TCOM
     1       /  0.57,       2.24,       8.0,        2.5,        0.25   /
      DATA      TCDRYS,     TCDRYP,     TCSAPW,     TCSAPI,     THLMIN
     1       /  0.275,      0.06,       0.48,       1.44,       0.04   /
      DATA      RHOSOL,     RHOOM,      HCPSND,     HCPCLY
     1       /  2.65E3,     1.30E3,     2.13E6,     2.38E6      /
      DATA      HCPW,       HCPICE,     HCPSOL,     HCPOM,      HCPSNI
     1       /  4.187E6,    1.96E6,     2.25E6,     2.50E6,     0.20E6 /
      DATA      SPHW,       SPHICE,     SPHVEG
     1       /  4.186E3,    2.10E3,     2.70E3      /
      DATA      RHOW,       RHOICE,     RHOSNI,     CLHMLT,     CLHVAP
     1       /  1.0E3,      0.917E3,    0.10E3,     0.334E6,    2.501E6/
      DATA      ZOLNG,      ZOLNS,      ZOLNI,      ZORATG
     1       /  -4.605,     -6.908,     -6.215,     3.0         /
      DATA      ILG,        ICAN,       IGND
     1       /  3,          4,          3      / 
      DATA      LONSL
     1       /  360    /
      DATA  PI      /3.1415926535898 /
      DATA  DELZ    /0.10,0.25,3.75/
      DATA  ZBOT    /0.10,0.35,4.10/
      DATA GROWYR   /213.,213.,213.,213.,213.,213.,0.,0.,0.,
     1               0.,0.,0., 75.,106.,136.,167.,167.,167.,
     2               273.,273.,273.,273.,273.,273.,0.,0.,0.,
     3               0.,0.,0.,135.,166.,196.,196.,196.,196.,
     4               121.,121.,121.,121.,121.,121.,0.,0.,0.,
     5               0.,0.,0.,275.,244.,214.,214.,214.,214.,
     6               151.,151.,151.,151.,151.,151.,0.,0.,0.,
     7               0.,0.,0.,305.,274.,244.,244.,244.,244.,
     8               213.,213.,213.,213.,213.,213.,0.,0.,0.,
     9               0.,0., 75.,106.,136.,167.,167.,167.,167.,
     A               273.,273.,273.,273.,273.,273.,0.,0.,0.,
     B               0.,0.,135.,166.,196.,196.,196.,196.,196.,
     C               121.,121.,121.,121.,121.,121.,0.,0.,0.,
     D               0.,0.,275.,244.,214.,214.,214.,214.,214.,
     E               151.,151.,151.,151.,151.,151.,0.,0.,0.,
     F               0.,0.,305.,274.,244.,244.,244.,244.,244. /
      DATA CANEXT   /-0.5,-1.5,-0.8,-0.8/
      DATA ZORAT    /2.0,2.0,7.0,12.0/
      DATA ILAND    /1,2,3 /
      DATA DAYTOT   /31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,
     1               30.,31./
      DATA MONEND   /31, 59, 90,120,151,181,212,243,273,304,
     1               334,365/

C     -------------------- CTEM MODIFICATIONS -------------------------\
C
C     NO. OF CTEM PLANT FUNCTIONAL TYPES
      DATA ICC/9/    
C
C     SEPARATION OF THESE PFTs INTO LEVEL 1 (FOR CLASS) AND LEVEL 2 (FOR CTEM) PFTs. 
      DATA MODELPFT/1,     1,     0,    ! CLASS PFT 1 NDL
C                  EVG    DCD
     &              1,     1,     1,    ! CLASS PFT 2 BDL
C                  EVG  DCD-CLD DCD-DRY ! NOTE 2 TYPES OF BDL DCD - COLD & DRY
     &              1,     1,     0,    ! CLASS PFT 3 CROP
C                  C3      C4
     &              1,     1,     0/    ! CLASS PFT 4 GRASS
C                  C3      C4
C
C     MAXIMUM NO. OF LEVEL 2 PFTs
      DATA L2MAX/3/
C
C     CTEM's TIME STEP IN DAYS
      DATA DELTAT/1.0/
C
      DATA ABSZERO/1E-12/
C
C     SET LOGICAL SWITCHES CTEM1 AND CTEM2 TO TRUE OR FALSE
C
      CTEM1=.TRUE.   ! SET THIS TO TRUE FOR USING STOMATAL CONDUCTANCE
C                    ! CALCULATED BY PHTSYN SUBROUTINE, ELSE THE STANDARD
C                    ! JARVIS TYPE FORMULATION OF CLASS 2.7 IS USED. WITH
C                    ! ONLY THIS SWITCH ON CLASS' LAI IS USED.

      CTEM2=.TRUE.   ! SET THIS TO TRUE FOR USING CTEM SIMULATED DYNAMIC
C                    ! LAI AND CANOPY MASS, ELSE CLASS SIMULATED SPECIFIED 
C                    ! LAI AND CANOPY MASS ARE USED. WITH THIS SWITCH ON, 
C                    ! ALL CTEM SUBROUTINES ARE RUN. 
C
      LANDUSE=.FALSE.! SET THIS TO TRUE IF LAND USE CHANGE IS TO BE
C                    ! IMPLIMENTED BY READING IN THE FRACTIONS OF 9 CTEM
C                    ! PFTs FROM A FILE.
C
      CTEMLOOP = 2   ! NO. OF TIMES THE .MET FILE IS TO BE READ. THIS
C                    ! OPTION IS USEFUL TO SEE HOW CTEM's C POOLS 
C                    ! EQUILIBRATE WHEN DRIVEN WITH SAME CLIMATE DATA
C                    ! OVER AND OVER AGAIN.
C
      JK=1           ! CHOOSE CTEM PFT FOR WHICH WE WANT TO WRITE SOME SELECTED RESULTS (YUAN(OCT 21 2006): CHANGED)  
C
      LOPCOUNT = 1   ! INITIALIZE LOOP COUNT TO 1.
C
C     -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
!      CTEMN  =.FALSE. ! SET THIS TO TRUE IF CTEM ACTIVIATE THE SOIL-PLANT N CYCLING MODULE DEVELOPED AT McMASTER UNIVERSITY
      CTEMN  =.TRUE.  ! SET THIS TO TRUE IF CTEM ACTIVIATE THE SOIL-PLANT N CYCLING MODULE DEVELOPED AT McMASTER UNIVERSITY
      CSTOCKINI =.FALSE. ! SET THIS TO TRUE IF CTEM DOESN'T UPDATE ECOSYSTEM C POOLS DURING MODEL SPIN-UP
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C     *****************************************************************
C
C
C
C     =================================================================

      NLAND=1
      IL1=1
      IL2=NLAND
      DELT=1800.
      ITINCR=NINT(DELT)/60
      ZC=0.10
      DELZW(1,1)=0.10
      DELZW(1,2)=0.25
      DELZW(1,3)=3.75
      ZBOTW(1,1)=0.10
      ZBOTW(1,2)=0.35
      ZBOTW(1,3)=4.10

      CALL SPWCON7(RVORD,PI)

      IDELT=DELT
      DELTIM=DELT
      CELZRO=TFREZ
      GAS=RGAS
      GASV=RGASV
      G=GRAV
      SIGMA=SBC
      SPHAIR=CPRES
      CPI=PI
      IDISP=1
C
C     * OPEN FILES FOR READING AND WRITING.
      OPEN(UNIT=50,FILE='CA-TP4_0207.INI',STATUS='OLD')
      OPEN(UNIT=51,FILE='CA-TP4_0207.MET',STATUS='OLD')
      OPEN(UNIT=61,FILE='CA-TP4_0207_OF1')
      OPEN(UNIT=62,FILE='CA-TP4_0207_OF2')
      OPEN(UNIT=63,FILE='CA-TP4_0207_OF3')
      OPEN(UNIT=64,FILE='CA-TP4_0207_OF4')
      OPEN(UNIT=65,FILE='CA-TP4_0207_OF5')
      OPEN(UNIT=66,FILE='CA-TP4_0207_OF6')
      OPEN(UNIT=67,FILE='CA-TP4_0207_OF7')
C     -------------------- CTEM MODIFICATIONS -------------------------\
      OPEN(UNIT=68,FILE='CA-TP4_0207_OF8')   ! WRITE LAI USED BY CLASS TO THIS FILE
      OPEN(UNIT=100,FILE='CA-TP4_0207_HS')   ! HSuo testing HH output FILE
C
      IF (CTEM2) THEN
        IF(.NOT.CTEM1) THEN
          WRITE(6,*)'YOU CANNOT SET CTEM2 TO TRUE, AND CTEM1 TO FALSE.'
          WRITE(6,*)'SET BOTH THESE SWITCHES TO .TRUE. IF YOU WANT TO'
          WRITE(6,*)'USE CTEM COMPLETELY COUPLED WITH CLASS.'
          CALL XIT('RUNCLASS', -1)
        ENDIF
      ENDIF

      IF (CTEMN) THEN
        IF(.NOT.CTEM1) THEN
          WRITE(6,*)'YOU CANNOT SET CTEMN TO TRUE, AND CTEM1 TO FALSE.'
          WRITE(6,*)'SET BOTH THESE SWITCHES TO .TRUE. IF YOU WANT TO'
          WRITE(6,*)'USE CTEM COMPLETELY COUPLED WITH CLASS.'
          CALL XIT('RUNCLASS', -1)
        ENDIF
      ENDIF

C
      IF (CTEM1) THEN
        OPEN(UNIT=70,FILE='CA-TP4_0207.CTM',STATUS='OLD') !INI FILE FOR CTEM
        OPEN(UNIT=71,FILE='CA-TP4_0207_CT1')              !HALF-HOURLY OUTPUT FROM CTEM
      ENDIF

      IF (CTEM2) THEN                      ! DAILY OUTPUT FROM CTEM
        OPEN(UNIT=72,FILE='CA-TP4_0207_CT2')
        OPEN(UNIT=73,FILE='CA-TP4_0207_CT3')
        OPEN(UNIT=74,FILE='CA-TP4_0207_CT4')
        OPEN(UNIT=75,FILE='CA-TP4_0207_CT5')
        OPEN(UNIT=76,FILE='CA-TP4_0207_CT6')
        OPEN(UNIT=77,FILE='CA-TP4_0207_CT7') ! YEARLY OUTPUT FILE FOR JK PFT
        OPEN(UNIT=78,FILE='CA-TP4_0207_CT8')
        OPEN(UNIT=79,FILE='CA-TP4_0207_CT9') ! YEARLY OUTPUT FILE FOR ALL PFTs
      ENDIF

      IF (LANDUSE) THEN
        OPEN(UNIT=90,FILE='CA-TP4_0207.LUC')
      ENDIF
C     -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
      IF (CTEMN) THEN
        OPEN(UNIT=80,FILE='CA-TP4_0207.CTN',STATUS='OLD') ! INI FILE FOR CTEM-N COMPONENT
        OPEN(UNIT=81,FILE='CA-TP4_0207_CN1')              ! DAILY OUTPUT FROM CTEM N COMPONENT
        OPEN(UNIT=82,FILE='CA-TP4_0207_CN2')              ! DAILY OUTPUT FROM CTEM N COMPONENT
        OPEN(UNIT=83,FILE='CA-TP4_0207_CN3')              ! YEARLY OUTPUT FROM CTEM N COMPONENT
      ENDIF
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C     * READ AND PROCESS INITIALIZATION AND BACKGROUND INFORMATION.

      READ (50,5010) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      READ (50,5010) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      READ (50,5010) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(61,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6           !HSuo--080714    
C       WRITE(61,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(61,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(62,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C       WRITE(62,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(62,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(63,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C       WRITE(63,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(63,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(64,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C       WRITE(64,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(64,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(65,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C       WRITE(65,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(65,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(66,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C       WRITE(66,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(66,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C C
C C     -------------------- CTEM MODIFICATIONS -------------------------\
C       WRITE(68,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C       WRITE(68,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C       WRITE(68,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C       WRITE(68,*) 'LAI FOR CLASS 4 PFTS'
C
      IF (CTEM1) THEN
        READ (70,7010) TITLEC
        READ (70,7010) TITLEC
        READ (70,7010) TITLEC
C
C         WRITE(71,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6       !hs--080714
C         WRITE(71,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(71,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(71,7015) 
C         WRITE(71,7030) 
      ENDIF
C
C       IF (CTEM2) THEN
C         WRITE(72,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(72,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(72,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(72,7020) 
C         WRITE(72,7040) 
C C
C         WRITE(73,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(73,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(73,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(73,7020) 
C         WRITE(73,7050) 
C C
C         WRITE(74,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(74,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(74,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(74,7020) 
C         WRITE(74,7060) 
C C
C         WRITE(75,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(75,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(75,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(75,7020) 
C         WRITE(75,7070) 
C C
C         WRITE(76,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(76,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(76,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(76,7020) 
C         WRITE(76,7080) 
C C
C         WRITE(77,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(77,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(77,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(77,7090) 
C         WRITE(77,7100) 
C         WRITE(77,7101) 
C C
C         WRITE(78,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(78,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(78,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(78,7020) 
C         WRITE(78,7110) 
C         WRITE(78,7111) 
C C
C         WRITE(79,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(79,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(79,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(79,7090)
C         WRITE(79,7112)
C         WRITE(79,7113)
C       ENDIF
7010  FORMAT(A80)
7015  FORMAT('CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) HALF-HOURLY 
     &RESULTS')    
7020  FORMAT('CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) DAILY RESULTS'
     &)    
7090  FORMAT('CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) YEARLY RESULTS
     &')    
7030  FORMAT('HOUR MIN  DAY, An FOR 9 PFTs, RmL FOR 9 PFTs, 
     &AIL FOR 9 PFTs')
7040  FORMAT('  DAY YEAR       GPP       NPP       NEP       NBP   AUTOR
     &ES  HETRORES    LITRES    SOCRES  DSTCEMLS  LITRFALL  HUMIFTRS')
7050  FORMAT('  DAY YEAR       RML       RMS       RMR        RG  LEAFLI
     &TR  TLTRLEAF  TLTRSTEM  TLTRROOT')
7060  FORMAT('  DAY YEAR  VGBIOMAS   GAVGLAI  GAVGLTMS  GAVGSCMS  GLEAFM
     &AS  BLEAFMAS  STEMMASS  ROOTMASS  LITRMASS  SOILCMAS')
7070  FORMAT('  DAY YEAR     AILCG     AILCB    RMATCTEM LAYER 1, 2, & 3
     &     VEGHGHT  ROOTDPTH  ROOTTEMP      SLAI')
7080  FORMAT('  DAY YEAR   AFRLEAF   AFRSTEM   AFRROOT  TCANOACC  LFSTAT
     &US') 
7100  FORMAT('  YEAR   LAIMAXG  STEMMASS  ROOTMASS  LITRMASS  SOILCMAS A
     &NNUALNPP ANNUALGPP ANNUALNEP  AVGYRLE')  
7101  FORMAT('          m2/m2    Kg C/m2  Kg C/m2    Kg C/m2   Kg C/m2
     &gC/m2.yr  gC/m2.yr  gC/m2.yr    W/m2  ')
7110  FORMAT('  DAY YEAR   BURNAREA   PROBFIRE   LUCEMCOM   LUCLTRIN    
     &LUCSOCIN  GRCLAREA') 
7111  FORMAT('               KM^2        -    uMOL-CO2/M2.S KgC/M2.DAY  
     &KgC/M2.DAY    KM^2')
7112  FORMAT(' YEAR END POOLS, AVERAGE ANNUAL NPP, & MAX. ANNUAL LAI FOR
     & 9 PFTs')
7113  FORMAT(' POOLS (Kg C/M2), NPP (gC/M2), LAI (M2/M2), LFSTATUS (-),
     & +ve PHYSYN DAYS (DAY)')
C
C     -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
C       IF (CTEMN) THEN
C         WRITE(81,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(81,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(81,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(81,8010) 
C         WRITE(81,8020) 
C C
C         WRITE(82,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(82,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(82,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(82,8010) 
C         WRITE(82,8030) 
C C
C         WRITE(83,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
C         WRITE(83,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C         WRITE(83,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C         WRITE(83,8050) 
C         WRITE(83,8030) 
C C
C       ENDIF
8010  FORMAT('N COMPONENT FOR CTEM 1.0/CLASS2.7 DAILY RESULTS')
8050  FORMAT('N COMPONENT FOR CTEM 1.0/CLASS2.7 YEARLY RESULTS')
8020  FORMAT('  DAY YEAR N/C(leaf) N/C(stem) N/C(root) N/C(litr)  N/C(so
     1m)    NH4+-N    NO3--N  Nrubisco Nrub(top) Vcmax(top)')
8030  FORMAT('  YEAR  Ndepos.     Nfertil     Nlitring   Ndist-emis  
     1  Nhumif      Nmineral     Nnitrif     Nuptake  Ndenitri  Nleach
     2  Nvolat      Nsource      Nloss')
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
      READ(50,5020) DEGLAT,DEGLON,ZREF,ILW
      JLAT=NINT(DEGLAT)
      RADL=DEGLAT*PI/180.
      JLON=NINT(DEGLON)

      DO 50 I=1,NLAND 
          READ(50,5040) (FCANGAT(I,J),J=1,ICAN+1),(LAMXGAT(I,J),
     1                  J=1,ICAN)
          READ(50,5040) (LNZ0GAT(I,J),J=1,ICAN+1),(LAMNGAT(I,J),
     1                  J=1,ICAN)
          READ(50,5040) (ALVCGAT(I,J),J=1,ICAN+1),(CMASGAT(I,J),
     1                  J=1,ICAN)
          READ(50,5040) (ALICGAT(I,J),J=1,ICAN+1),(ROOTGAT(I,J),
     1                  J=1,ICAN)
          READ(50,5040) (SBNDGAT(I,J),J=1,2),SGLGAT(I),SHLGAT(I),
     1                  ENVGAT(I)
          READ(50,5050) (SOILGAT(I,J,1),J=1,3)   !5050  FORMAT(5F10.2)
          READ(50,5050) (SOILGAT(I,J,2),J=1,3)
          READ(50,5050) (SOILGAT(I,J,3),J=1,3)
          READ(50,5050) (TBARGAT(I,J),J=1,IGND),TCANGAT(I),TSNOGAT(I)
          READ(50,5060) (THLQGAT(I,J),J=1,IGND),(THICGAT(I,J),J=1,IGND)  !5060  FORMAT(6F10.3)
          READ(50,5070) WCANGAT(I),SCANGAT(I),SNOGAT(I),ALBSGAT(I), !5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3)
     1                  RHOSGAT(I),TTGAT(I)
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
		DO 51 J=2,ICAN
		  IF (FCANGAT(I,J).GT. FCANGAT(I,J-1)) JK=J
51      CONTINUE
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
50    CONTINUE
C
C     -------------------- CTEM MODIFICATIONS -------------------------\
C
C     READ FROM CTEM INITIALIZATION FILE 
C
      IF (CTEM1) THEN
        DO 60 I=1,NLAND 
          READ(70,*) (AILCMIN(I,J),J=1,ICC)    !ICC=9
          READ(70,*) (AILCMAX(I,J),J=1,ICC)    
          READ(70,*) (DVDFCAN(I,J),J=1,ICC)    
          READ(70,*) (GLEAFMAS(I,J),J=1,ICC)       
          READ(70,*) (BLEAFMAS(I,J),J=1,ICC)       
          READ(70,*) (STEMMASS(I,J),J=1,ICC)       
          READ(70,*) (ROOTMASS(I,J),J=1,ICC)       
          READ(70,*) (LITRMASS(I,J),J=1,ICC+1)       
          READ(70,*) (SOILCMAS(I,J),J=1,ICC+1)       
          READ(70,*) (LFSTATUS(I,J),J=1,ICC)       
          READ(70,*) (PANDAYS(I,J),J=1,ICC)       
          READ(70,*) (MLIGHTNG(I,J),J=1,6)  !MEAN MONTHLY LIGHTNING FREQUENCY
          READ(70,*) (MLIGHTNG(I,J),J=7,12) !FLASHES/KM2.YEAR      
          READ(70,*) EXTNPROB(I)
          READ(70,*) PRBFRHUC(I)
          READ(70,*) STDALN(I)
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
        IF (CSTOCKINI) THEN      ! INITIAL C STOCKS DOESN'T UPDATE DURING SPIN-UP, control factor, logical
			DO 55 J = 1, ICC
				AILCMIN0(I,J)   = AILCMIN(I,J)   
				AILCMAX0(I,J)   = AILCMAX(I,J)   
				GLEAFMAS0(I,J)  = GLEAFMAS(I,J)
				BLEAFMAS0(I,J)  = BLEAFMAS(I,J)       
				STEMMASS0(I,J)  = STEMMASS(I,J)       
				ROOTMASS0(I,J)  = ROOTMASS(I,J)       
				LITRMASS0(I,J)  = LITRMASS(I,J)       
				SOILCMAS0(I,J)  = SOILCMAS(I,J)       
55			CONTINUE
				LITRMASS0(I,ICC+1)  = LITRMASS(I,ICC+1)       
				SOILCMAS0(I,ICC+1)  = SOILCMAS(I,ICC+1)       
			
		ENDIF
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
60      CONTINUE
      ENDIF
      CLOSE(70)
C     -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
C
      IF (CTEMN) THEN
          DO 85 I=1,NLAND
              READ(80,7010) TITLEC
              READ(80,7010) TITLEC
              READ(80,7010) TITLEC
              READ(80,*) (RNLEAF(I,J),J=1,ICC)       
              READ(80,*) (RNSTEM(I,J),J=1,ICC)       
              READ(80,*) (RNROOT(I,J),J=1,ICC)       
              READ(80,*) (RNLITR(I,J),J=1,ICC+1)       
              READ(80,*) (RNSOM(I,J),J=1,ICC+1)       
              READ(80,*) (SNH4(I,J),J=1,ICC+1)       
              READ(80,*) (SNO3(I,J),J=1,ICC+1)       
              READ(80,7010) TITLEC
              READ(80,*) et0      ! characteristic ET for N uptake [mm/s]
              READ(80,*) jmax     ! High affinity maximum rate of ion uptake [-]
              READ(80,*) kl       ! low affinity root ion uptake [-]
              READ(80,*) km       ! Mihalis Menten factor for 50% of maximum ion uptake rate [-]
              READ(80,*) kmin0    ! upper limit to kmin for adequate roots and ET [1/s]
              READ(80,*) rtmass0  ! reference root mass - now a constant [gC/m2]
              READ(80,*) solNH4   ! soluability of ammonium ion [-]
              READ(80,*) rnlf0    ! ideal structual N to C ratio for new leaves [-]
              READ(80,*) rnsm0    ! ideal N to C ratio in new stem tissue [-]
              READ(80,*) rnrt0    ! ideal N to C ratio in new root tissue [-]
              READ(80,*) conreal  ! reallocation of nitrogen coefficient [-]
              READ(80,*) LAI0     ! top LAI assuming same RubiscoN content [m2/m2]
              READ(80,*) krubn    ! canopy rubisco-nitrogen decay coefficient [-]
              READ(80,*) kn       ! canopy nitrogen decay coefficient [-]
              READ(80,*) kni0     ! max. NH4 nitrifiction rate [1/s]
              READ(80,*) kdn0     ! max. NO3 denitrifiction rate [1/s]
              READ(80,*) kv0      ! max. NH4 volitization rate [1/s]
              READ(80,*) nbfix0   ! N source by biofixation at reference conditions [gN/m2/s)
              READ(80,*) ndep0    ! N source by deposition [gN/m2/yr]
              READ(80,*) nfer0    ! N source by inorganic fertilization [gN/m2/yr]
              READ(80,*) nfero0   ! N source by organic fertilization [gN/m2/yr]
85        CONTINUE
      ENDIF
      CLOSE(80)
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C      
      DO 100 I=1,NLAND
          ILSL(I)=JLON
          RADJ(I)=RADL
          GCGAT(I)=-1.
          CMAIGAT(I)=0.
          PREACC(I)=0.
          GTACC(I)=0.
          QEVPACC(I)=0.
          EVAPACC(I)=0.
          HFSACC(I)=0.
          HMFNACC(I)=0.
          ROFACC(I)=0.
          OVRACC(I)=0.
          DO 30 J=1,IGND
              TBARGAT(I,J)=TBARGAT(I,J)+TFREZ
              TBARACC(I,J)=0.
              THLQACC(I,J)=0.
              THICACC(I,J)=0.
              THALACC(I,J)=0.
30        CONTINUE
          ALSWACC(I)=0.
          ALLWACC(I)=0.
          RHOSACC(I)=0.
          SNOACC(I)=0.
          NSNO(I)=0
          TSNOACC(I)=0.
          TSNOGAT(I)=TSNOGAT(I)+TFREZ
          TCANACC(I)=0.
          TCANGAT(I)=TCANGAT(I)+TFREZ
          WCANACC(I)=0.
          SCANACC(I)=0.
          GROACC(I)=0.
          FSINACC(I)=0.
          FLINACC(I)=0.
          FLUTACC(I)=0.
          TSACC(I)=0.
          UVACC(I)=0.
          PRESACC(I)=0.
          QSACC(I)=0.
C-------------------------------------------------------
          G0ACC(I) = 0.                 ! YUAN: G0 OUTPUT
C-------------------------------------------------------
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
          ETPACC(I)    = 0.0
          DO 35 J = 1, ICAN
              XMINFBAR(I,J)  = 0.0
              IF (FCANGAT(I,J) .LT. FCANGAT(I,J+1)) JK=MIN(J+1,ICAN)
35        CONTINUE
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C     -------------------- CTEM MODIFICATIONS -------------------------\
          DO 31 J = 1, ICAN
              AILACC(I,J)=0.0
31        CONTINUE
C     -------------------- CTEM MODIFICATIONS -------------------------/
100   CONTINUE
C
C     -------------------- CTEM MODIFICATIONS -------------------------\
C     CTEM INITIALIZATIONS.
      IF (CTEM1) THEN
      DO 101 J = 1, ICAN                   !CALCULATE NUMBER OF LEVEL 2 PFTs USING MODELPFT
        ISUM = 0
        K1 = (J-1)*L2MAX + 1               !3 lines: 1~3, 4~6, 7~9
        K2 = K1 + (L2MAX - 1)
        DO N = K1, K2
          IF(MODELPFT(N).EQ.1) ISUM = ISUM + 1  !find each line
        ENDDO
        NOL2PFTS(J)=ISUM  ! NUMBER OF LEVEL 2 PFTs (0~9)
101   CONTINUE

      DO 110 I=1,NLAND
        CO2CONC(I)=360.00      !SPECIFY CO2 CONCENTRATION FOR PHTSYN
        DO 111 J = 1, ICC
          CO2I1CS(I,J)=0.0     !INTERCELLULAR CO2 CONCENTRATIONS
          CO2I1CG(I,J)=0.0
          CO2I2CS(I,J)=0.0
          CO2I2CG(I,J)=0.0
          ANCSVGAC(I,J)=0.0    !DAILY ACCU. NET PHOTOSYN. FOR CANOPY OVER SNOW SUBAREA
          ANCGVGAC(I,J)=0.0    !DAILY ACCU. NET PHOTOSYN. FOR CANOPY OVER GROUND SUBAREA
          RMLCSVGA(I,J)=0.0    !DAILY ACCU. LEAF RESPIRATION FOR CANOPY OVER SNOW SUBAREA
          RMLCGVGA(I,J)=0.0    !DAILY ACCU. LEAF RESPIRATION FOR CANOPY OVER GROUND SUBAREA
          SLAI(I,J)=0.0        !IF BIO2STR IS NOT CALLED WE NEED TO INITIALIZE THIS TO ZERO
111     CONTINUE

        FSNOWACC(I)=0.0         !DAILY ACCU. FRACTION OF SNOW
        TCANOACC(I)=0.0         !DAILY ACCU. CANOPY TEMPERATURE
        TCANSACC(I)=0.0         !DAILY ACCU. CANOPY TEMP. OVER SNOW
        VVACC(I) = 0.0          !DAILY ACCU. V WIND SPEED
        DO 112 J = 1,IGND       !SOIL TEMPERATURE AND MOISTURE OVER DIFFERENT SUBAREAS
          TBARCACC (I,J)=0.0
          TBARCSACC(I,J)=0.0
          TBARGACC (I,J)=0.0
          TBARGSACC(I,J)=0.0
          THLIQCACC(I,J)=0.0
          THLIQGACC(I,J)=0.0
          THICECACC(I,J)=0.0
112     CONTINUE
110   CONTINUE
C
C     FIND FCANCMX WITH CLASS' FCANMXs AND DVDFCANs READ FROM CTEM's 
C     INITIALIZATION FILE. THIS IS TO DIVIDE NEEDLE LEAF AND BROAD LEAF 
C     INTO DCD AND EVG, AND CROPS AND GRASSES INTO C3 AND C4.
C
      DO 113 J = 1, ICAN
        CSUM(I,J) = 0.0
        ICOUNT(I) = 0

        DO 114 I = IL1, IL2  !1,1
          K1 = (J-1)*L2MAX + 1
          K2 = K1 + (L2MAX - 1)
          DO N = K1, K2
            IF(MODELPFT(N).EQ.1)THEN
              ICOUNT(I) = ICOUNT(I) + 1
              CSUM(I,J) = CSUM(I,J) + DVDFCAN(I,ICOUNT(I))
              FCANCMX(I,ICOUNT(I))=FCANGAT(I,J)*DVDFCAN(I,ICOUNT(I))
            ENDIF
          ENDDO

          IF( ABS(CSUM(I,J)-1.0).GT.ABSZERO ) THEN
            WRITE(6,1130)I,J
1130        FORMAT('DVDFCANs FOR (',I1,',',I1,') MUST ADD TO 1.0')
            CALL XIT('RUNCLASS', -2)
          ENDIF
114     CONTINUE

113   CONTINUE


C     IF LAND USE CHANGE SWITCH IS ON THEN READ THE FRACTIONAL COVERAGES 
C     OF CTEM's 9 PFTs FOR THE FIRST YEAR AND SET THEM TO PREVIOUS YEAR's 
C     COVERAGES
C
      IF (LANDUSE) THEN
        READ (90,7010) TITLEC
        READ (90,7010) TITLEC
        READ (90,7010) TITLEC
        DO I = 1, NLAND
          READ (90,*) USELESS,(PFCANCMX(I,J),J=1,ICC)
        ENDDO
C
C       GET FCANMXs FOR USE BY CLASS USING THE PFCANCMXs JUST READ 
C
        DO J = 1, ICAN
          DO I = 1, NLAND
            FCANGAT(I,J)=0.0
          ENDDO     
        ENDDO     
C
        K1=0
        DO 170 J = 1, ICAN
          IF(J.EQ.1) THEN
            K1 = K1 + 1
          ELSE
            K1 = K1 + NOL2PFTS(J-1)
          ENDIF
          K2 = K1 + NOL2PFTS(J) - 1
          DO 171 M = K1, K2
            DO 172 I = 1, NLAND
              FCANGAT(I,J)=FCANGAT(I,J)+PFCANCMX(I,M)
172         CONTINUE
171       CONTINUE
170     CONTINUE
C
        CLOSE(90)
        OPEN(UNIT=90,FILE='CA-TP4_0207.LUC')
        READ (90,7010) TITLEC
        READ (90,7010) TITLEC
        READ (90,7010) TITLEC
C
        DO J = 1, ICC
          DO I = 1, NLAND
            FCANCMX(I,J)=PFCANCMX(I,J)
          ENDDO     
        ENDDO     
      ENDIF
C
C     WITH FCANCMX CALCULATED ABOVE AND INITIALIZED VALUES OF ALL CTEM POOLS,
C     FIND GRID AVERAGE VEGETATION BIOMASS, LITTER MASS, AND SOIL C MASS. 
C     ALSO INITIALIZE ADDITIONAL VARIABLES WHICH ARE USED BY CTEM.
C 
      IF (CTEM2) THEN
        DO 115 I = IL1, IL2
          VGBIOMAS(I)=0.0       !GRID AVE. VEG. BIOMASS
          GAVGLAI (I)=0.0       !GRID AVE. GREEN LAI
          GAVGLTMS(I)=0.0       !GRID AVE. LITTER MASS
          GAVGSCMS(I)=0.0       !GRID AVE. SOIL C MASS
          LUCEMCOM(I)=0.0       !LAND USE CHANGE COMBUSTION EMISSION LOSSES
          LUCLTRIN(I)=0.0       !LAND USE CHANGE INPUTS TO LITTER POOL
          LUCSOCIN(I)=0.0       !LAND USE CHANGE INPUTS TO SOIL C POOL
          COLDDAYS(I,1)=0       !COLD DAYS COUNTER FOR NDL DCD
          COLDDAYS(I,2)=0       !COLD DAYS COUNTER FOR CROPS
          AVGYRNPP(I)=0.0       !ANNUAL AVERAGED NPP
          AVGYRGPP(I)=0.0       !ANNUAL AVERAGED GPP
          AVGYRNEP(I)=0.0       !ANNUAL AVERAGED NEP
          AVGYRLE(I)=0.0        !ANNUAL AVERAGED LATENT HEAT FLUX
          DO 116 J = 1, ICC
            VGBIOMAS(I)=VGBIOMAS(I)+FCANCMX(I,J)*(GLEAFMAS(I,J)+
     &       STEMMASS(I,J)+ROOTMASS(I,J)+BLEAFMAS(I,J))
            GAVGLTMS(I)=GAVGLTMS(I)+FCANCMX(I,J)*LITRMASS(I,J)
            GAVGSCMS(I)=GAVGSCMS(I)+FCANCMX(I,J)*SOILCMAS(I,J)
            GRWTHEFF(I,J)=100.0   !SET GROWTH EFFICIENCY TO SOME LARGE NUMBER 
C                                 !SO THAT NO GROWTH RELATED MORTALITY OCCURS IN FIRST YEAR
            FLHRLOSS(I,J)=0.0     !FALL/HARVEST LOSS
            STMHRLOS(I,J)=0.0     !STEM HARVEST LOSS FOR CROPS
            ROTHRLOS(I,J)=0.0     !ROOT DEATH FOR CROPS
            LYSTMMAS(I,J)=STEMMASS(I,J)
            LYROTMAS(I,J)=ROOTMASS(I,J)
            TYMAXLAI(I,J)=0.0
            LAIMAXG(I,J)=0.0
            ANNPPVEG(I,J)=0.0
116      CONTINUE
115    CONTINUE
C
        GAVGLTMS(I)=GAVGLTMS(I)+ (1.0-FCANGAT(I,1)-FCANGAT(I,2)-
     &   FCANGAT(I,3)-FCANGAT(I,4))*LITRMASS(I,ICC+1)
        GAVGSCMS(I)=GAVGSCMS(I)+ (1.0-FCANGAT(I,1)-FCANGAT(I,2)-
     &   FCANGAT(I,3)-FCANGAT(I,4))*SOILCMAS(I,ICC+1)

C     CALCULATE VEGETATION STRUCTURAL ATTRIBUTES FROM LEAF, STEM, AND ROOT BIOMASS FOR USE BY CLASS.
C
          CALL      BIO2STR( GLEAFMAS, BLEAFMAS, STEMMASS, ROOTMASS,
     1                            ICC,      ILG,      IL1,      IL2,
     2                           IGND,     ICAN,  FCANCMX,    ZBOTW,
     3                          DELZW, NOL2PFTS,    L2MAX,
     4                          AILCG,    AILCB,     AILC,    ZOLNC,
     5                          RMATC, RMATCTEM,     SLAI,  BMASVEG,
     6                       CMASVEGC,  VEGHGHT, ROOTDPTH,  ALVSCTM,
     8                        ALIRCTM)
C
      ENDIF   ! IF (CTEM2) THEN
C      
	ENDIF   ! IF (CTEM1) THEN
C
C     -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
C      * INITIALIZATION FOR NITROGEN COMPONENTS
      IF (CTEMN) THEN
          DO 95 I=1,NLAND
              BTDPTH(I)  = MAX(ROOTGAT(I,1),ROOTGAT(I,2),ROOTGAT(I,3))    ! BOTTOM DRAINAGE DEPTH
              CALSOL(I)  = .TRUE.                    ! DEFINE SOIL TYPE AS CALCIOUS
              YRNDEP(I)  = 0.0                       ! YEARLY N DEPOSITION (gN/m2)
              YRNFER(I)  = 0.0                       ! YEARLY N FERTILIZATION (gN/m2)
              YRNPLTR(I) = 0.0                       ! YEARLY N LITTER-FALLING (gN/m2)
              YRNDIS(I)  = 0.0                       ! YEARLY N DISTURBANCE LOSS (gN/m2)
              YRNLSOM(I) = 0.0                       ! YEARLY N HUMIFICATION (gN/m2)
              YRNMIN(I)  = 0.0                       ! YEARLY N MINERALIZATION (gN/m2)
              YRNNIT(I)  = 0.0                       ! YEARLY N NITRIFICATION (gN/m2)
              YRNPUP(I)  = 0.0                       ! YEARLY N PLANT UPTAKE (gN/m2)
              YRNDNIT(I) = 0.0                       ! YEARLY N DENITRIFICATION (gN/m2)
              YRNLEA(I)  = 0.0                       ! YEARLY NO3 LEACHING (gN/m2)
              YRNVOL(I)  = 0.0                       ! YEARLY NH4 VOLATILIZATION (gN/m2)
              YRNSRCE(I) = 0.0                       ! YEARLY N SOURCES (gN/m2)
              YRNLOSS(I) = 0.0                       ! YEARLY N LOSSES (gN/m2)
	        YRNN2O(I)  = 0.0                       ! YEARLY N2O LOSSES (gN/m2) HSuo
	        YRNN2(I)   = 0.0                       ! YEARLY N2 LOSSES (gN/m2) HSuo

C      INITIAL VCMAX0 (umol/m2/s)
              DO 105 J=1,ICC          
                  VCMX0(I,J) = 60.0E-6			   ! DEFAULT VALUE
105	        CONTINUE
95        CONTINUE
      ENDIF
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C
      DO 120 I=1,6
          ITERSM(I)=0
          ITER5(I)=0
          ITER10(I)=0
          ITER25(I)=0
          ITER49(I)=0
          ITER50(I)=0
          DO 121 J=1,50
              ITERCT(1,I,J)=0
121       CONTINUE
120   CONTINUE

      DO 130 I=1,NLAND
		TBASGAT(I)=TBARGAT(I,3)
		DRNGAT(I) =SBNDGAT(I,2)
		IF (SOILGAT(I,1,1).GT.0) SANDGAT(I,1)=SOILGAT(I,1,1)*5.0+17.0
        IF (SOILGAT(I,1,2).GT.0) SANDGAT(I,2)=SOILGAT(I,2,1)*5.0+17.0
        IF (SOILGAT(I,1,3).GT.0) SANDGAT(I,3)=SOILGAT(I,3,1)*5.0+17.0
		CLAYGAT(I,1)=SOILGAT(I,1,2)*5.0-2.0
		CLAYGAT(I,2)=SOILGAT(I,2,2)*5.0-2.0
		CLAYGAT(I,3)=SOILGAT(I,3,2)*5.0-2.0
		ORGMGAT(I,1)=SOILGAT(I,1,3)
		ORGMGAT(I,2)=SOILGAT(I,2,3)
		ORGMGAT(I,3)=SOILGAT(I,3,3)
130   CONTINUE

5010  FORMAT(2X,6A4)
5020  FORMAT(3F10.2,I5)
5040  FORMAT(9F8.3)
5050  FORMAT(5F10.2)
5060  FORMAT(6F10.3)
5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3)
5200  FORMAT(4I10)
C5300  FORMAT(12X,2F8.2,E12.3,2F8.2,F10.2,E12.3)
5300  FORMAT(2F10.2,E12.3,F10.2,F8.2,F10.2,E12.3)
6001  FORMAT('CLASS TEST RUN:     ',6A4)
6002  FORMAT('RESEARCHER:         ',6A4)
6003  FORMAT('INSTITUTION:        ',6A4)
C
C=======================================================================================================
C
C
C
C     * LAUNCH RUN.

      READ(50,5200) IYEAR,IDAY,IHOUR,IMIN
      READ(50,5200) JOUT1,JOUT2,JAV1,JAV2 
      READ(50,5200) KOUT1,KOUT2,KAV1,KAV2 
      NCOUNT=IHOUR*2+IMIN/30+1
      NSUM=1
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
      IYEAR0 = IYEAR
      IDAY0  = IDAY
      IHOUR0 = IHOUR
      IMIN0  = IMIN
      DO I=1,NLAND
          NDAYTIME(I) = 0
	ENDDO
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
150   CONTINUE
C
C     -------------------- CTEM MODIFICATIONS -------------------------\
C     WRITE(6,*)'DAY=',IDAY,' HOUR=',IHOUR,' MIN=',IMIN
C     -------------------- CTEM MODIFICATIONS -------------------------/

C                                               !CTEM has not considered leap year
      IF(IDAY.GE.  1.AND.IDAY.LE. 31) MONTH= 1
      IF(IDAY.GE. 32.AND.IDAY.LE. 59) MONTH= 2
      IF(IDAY.GE. 60.AND.IDAY.LE. 90) MONTH= 3
      IF(IDAY.GE. 91.AND.IDAY.LE.120) MONTH= 4
      IF(IDAY.GE.121.AND.IDAY.LE.151) MONTH= 5
      IF(IDAY.GE.152.AND.IDAY.LE.181) MONTH= 6
      IF(IDAY.GE.182.AND.IDAY.LE.212) MONTH= 7
      IF(IDAY.GE.213.AND.IDAY.LE.243) MONTH= 8
      IF(IDAY.GE.244.AND.IDAY.LE.273) MONTH= 9
      IF(IDAY.GE.274.AND.IDAY.LE.304) MONTH=10
      IF(IDAY.GE.305.AND.IDAY.LE.334) MONTH=11
      IF(IDAY.GE.335.AND.IDAY.LE.365) MONTH=12

C 
      DO 200 I=1,NLAND
          READ(51,*,END=900) FSDOWN,FDLGAT(I),PREGAT(I),
     1         TSGAT(I),UVGAT(I),PRESGAT(I),QSGAT(I)
C         MET file UNIT:     W/m2   W/m2   kg/m2/s(mm/s)   K        m/s      Pa        kg/kg    !HSuo
          FSVHGAT(I)=0.5*FSDOWN  
          FSIHGAT(I)=0.5*FSDOWN
         !TSGAT(I)=TSGAT(I)+TFREZ 
		ULGAT(I)=UVGAT(I)  !U
          VLGAT(I)=0.0       !Vertical
          UVGAT(I)=MAX(VMIN,UVGAT(I))    !VMIN=0.1
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
C           PREGAT(I)= PREGAT(I)/DELT      ! YUAN (07/15/06): CONVERTION OF UNIT: mm/delt to kg/m2/s
C           PRESGAT(I)=PRESGAT(I)*1000.0   ! YUAN (07/15/06): CONVERTION OF UNIT: KPa to Pa
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
200   CONTINUE

C========================================================================
C     * CALCULATE SOLAR ZENITH ANGLE AND COMPONENTS OF INCOMING SHORT-WAVE RADIATION FLUX
C 
      DAY=REAL(IDAY)+(REAL(IHOUR)+REAL(IMIN)/60.)/24.
      DECL=SIN(2.*PI*(284.+DAY)/365.)*23.45*PI/180.  !declination ��γ
      HOUR=(REAL(IHOUR)+REAL(IMIN)/60.)*PI/12.-PI
      COSZ=SIN(RADL)*SIN(DECL)+COS(RADL)*COS(DECL)*COS(HOUR) !solar zenith angle Z
C 
C------------------------------------------------------------------------HSuo testing
      HOUR_diff=(REAL(IHOUR-2)+REAL(IMIN)/60.)*PI/12.-PI
      COSZ_diff=SIN(RADL)*SIN(DECL)+COS(RADL)*COS(DECL)*COS(HOUR_diff) !HSuo testing

      DAY_HS=DAY
      DECL_HS=DECL
      HOUR_HS=HOUR
      COSZ_HS=COSZ
C-------------------------------------------------------------------------
C 
      DO 250 IL=IL1,IL2
          CSZGAT(IL)=COSZ
          IF(ABS(CSZGAT(IL)).LT.ZC) CSZGAT(IL)=ZC  !ZC = 0.1
          THLGAT(IL)=TSGAT(IL)
          IF(PREGAT(IL).GT.0.) THEN
              XDIFFUS(IL)=1.0                   !FRACTION OF DIFFUSED RADIATION
          ELSE
C               XDIFFUS(IL)=MIN(1.0-0.9*COSZ,1.)
	        XDIFFUS(IL)=MIN(1.0-0.9*COSZ_diff,1.)                      !HSuo testing: using shifted
          ENDIF               !FSVHGAT=FSDOWN*0.5:ˮƽ�����µĿɼ�����(W m-2)
          FSVGAT(IL)=MAX(FSVHGAT(IL)/CSZGAT(IL),0.)  !-->QSWV in CLASSA
          FSIGAT(IL)=MAX(FSIHGAT(IL)/CSZGAT(IL),0.) !�������� -->QSWI in CLASSA
          FSFGAT(IL)=(FSVGAT(IL)+FSIGAT(IL))*XDIFFUS(IL) !FSFGAT-->QSWD in CLASSA
250   CONTINUE
C
C=======================================================================
C     ************ CLASS - CANADIAN LAND SURFACE SCHEME ****************
C
C     * ALBEDO AND TRANSMISSIVITY CALCULATIONS; GENERAL VEGETATION CHARACTERISTICS.
C
          CALL CLASSA  (ALSWGAT,ALLWGAT,WCANGAT,SCANGAT,FC,     FG,
     1                  FCS,    FGS,    RAICAN, RAICNS, SNOCAN, SNOCNS,
     2                  DISP,   DISPS,  CHCAP,  CHCAPS, ZOMLNC, ZOMLCS,
     3                  ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS,
     4                  RCMIN,  RCMINS, CMASSC, CMASCS, FSVF,   FSVFS,
     5                  CWCAP,  CWCAPS, FRAINC, FSNOWC, ALVSCN, ALIRCN,
     6                  ALVSG,  ALIRG,  ALVSCS, ALIRCS, ALVSSN, ALIRSN,
     7                  TRVSCN, TRIRCN, TRVSCS, TRIRCS, TRSNOW, ZSNOW,
     8                  FROOT,  CMAIGAT,HTCCGAT,HTCSGAT,HTCGAT, WTRCGAT,
     9                  WTRSGAT,WTRGGAT,ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS,
     A                  FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,LAMXGAT,LAMNGAT,
     B                  CMASGAT,ROOTGAT,THLQGAT,THICGAT,CSZGAT, FSFGAT,
     C                  FSIGAT, FSVGAT, SANDGAT,CLAYGAT,ORGMGAT,SNOGAT, 
     D                  RHOSGAT,ALBSGAT,TTGAT,  TCANGAT,TSNOGAT,TBARGAT,
     E                  TSGAT,  RADJ,   ILAND,  ILSL,   LONSL,  ZORAT,  
     F                  DELZ,   DELZW,  ZBOTW,  FSNOW,  
     G                  IDAY,ILG,1,NLAND,JLAT,ICAN,ICAN+1,IGND,IDISP,  
     H                  RMAT,   H,HS,   AIL,    AILS,   FCAN,   FCANS, 
     I                  CWCPAV, AILCAN, AILCNS, FCLOUD, GROWA,  GROWN,  
     J                  GROWB,  RRESID, SRESID, THPOR,  HCPS,   ISAND, 
C    -------------------- CTEM MODIFICATIONS -------------------------\
     K               FCANCMX,   ICC,  CTEM1,   CTEM2,    RMATC, ZOLNC,
     L                 AILC, AILCG, CMASVEGC, AILCMIN, AILCMAX, L2MAX,
     M             NOL2PFTS, ALVSCTM, ALIRCTM,
C    ------------- CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW --------|
     N              AILCGS, FCANCS, FCANC)
C     
      IF ( CTEM1 .AND. (.NOT.CTEM2) ) THEN   !IF ONLY CTEM1 IS ON THEN USE FRACTION OF ROOT IN EACH SOIL LAYER FOR EACH OF CTEM PFT FROM CLASS.
        DO 300 K = 1, IGND
          K1=0
          DO 301 J = 1, ICAN
            IF(J.EQ.1) THEN
              K1 = K1 + 1
            ELSE
              K1 = K1 + NOL2PFTS(J-1)
            ENDIF
            K2 = K1 + NOL2PFTS(J) - 1
            DO 302 M = K1, K2
              DO 303 I = IL1, IL2
                RMATCTEM(I,M,K)=RMAT(I,J,K)
303           CONTINUE
302         CONTINUE
301       CONTINUE
300     CONTINUE
      ENDIF
C    -------------------- CTEM MODIFICATIONS -------------------------/
C
C
C     * DEFINE NUMBER OF POINTS IN EACH LAND SURFACE SUBAREA (CANOPY-
C     * COVERED, CANOPY-AND-SNOW-COVERED, BARE SOIL, AND SNOW OVER
C     * BARE SOIL) FOR CALCULATIONS IN CLASST/CLASSW.

      NLANDC =0
      NLANDCS=0
      NLANDG =0
      NLANDGS=0

      DO 350 I=1,NLAND
          IF(FC (I).NE.0.)            NLANDC =NLANDC +1
          IF(FCS(I).NE.0.)            NLANDCS=NLANDCS+1
          IF(FG (I).NE.0.)            NLANDG =NLANDG +1
          IF(FGS(I).NE.0.)            NLANDGS=NLANDGS+1
350   CONTINUE
C-----------------------------------------------------------------------
C          * SURFACE TEMPERATURE AND FLUX CALCULATIONS.
C
      CALL   CLASST    (TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     1  THICEC, THICEG, HCPC,   HCPG,   FROOT,  HFSGAT, TFXGAT, QEVPGAT,
     2  QFSGAT, QFXGAT, EFGAT,  CDHGAT, CDMGAT, 
     3  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,   G12CS,  G12GS,  
     4  G23C,   G23G,   G23CS,  G23GS,  QFREZC, QFREZG, QMELTC, QMELTG, 
     5  EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, TCANO,  TCANS,  
     6  ZPOND,  TPONDC, TPONDG, TPNDCS, TPNDGS, TSNOCS, TSNOGS, 
     7  GTGAT,  QGGAT,  SFCTGAT,SFCUGAT,SFCVGAT,SFCQGAT,
     8  FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT,FLGGGAT,HFSCGAT,HFSSGAT,
     9  HFSGGAT,HEVCGAT,HEVSGAT,HEVGGAT,HMFCGAT,HTCCGAT,HTCSGAT,HTCGAT, 
     A  TBARGAT,THLQGAT,THICGAT,TBASGAT,SANDGAT,CLAYGAT,ORGMGAT,DRGAT,  
     B  FSVHGAT,FSIHGAT,FDLGAT, ULGAT,  VLGAT,  TSGAT,  QSGAT,  PRESGAT,
     C  THLGAT, SGLGAT, SHLGAT, ENVGAT, FC,     FG,     FCS,    FGS,
     D  FSVF,   FSVFS,  ALVSCN, ALIRCN, ALVSG,  ALIRG,  ALVSCS, ALIRCS, 
     E  ALVSSN, ALIRSN, TRVSCN, TRIRCN, TRVSCS, TRIRCS, RCMIN,  RCMINS, 
     F  FRAINC, FSNOWC, RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP,  CHCAPS,
     G  CMASSC, CMASCS, DISP,   DISPS,  ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, 
     H  ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, TCANGAT,TSNOGAT,ZSNOW,  TRSNOW,
     I  RHOSGAT,DELZ,   DELZW,  ZREF,   ILW,    ILG,    1,      NLAND,
     J  JLAT,   ICAN,   IGND,   NLANDCS,NLANDGS,NLANDC, NLANDG, ITERCT,
C    ---------------------- CTEM MODIFICATIONS -------------------------\
     K    AILCG,   AILCGS,    FCANC,   FCANCS,   CO2CONC,   CO2I1CG,
     L  CO2I1CS,  CO2I2CG,  CO2I2CS,   CSZGAT,   XDIFFUS,      SLAI,
     M      ICC,    CTEM1,    CTEM2, RMATCTEM,   FCANCMX,     L2MAX,
     N NOL2PFTS,  CFLUXCG,  CFLUXCS,
C    ------------ CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW -----------|
     O  ANCSVEG,  ANCGVEG, RMLCSVEG, RMLCGVEG,    CANRES,  !)
C    ---------------------- CTEM MODIFICATIONS -------------------------/
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++
     P    CTEMN,      ETP,    XMINF,       BI,	NRUB0,		VCMX0, !)
C     ----------------------------------------------------HSuo testing BELOW 
     Q  GPP_GVEG,GPP_SVEG, !NEP_VEG,RE_VEG,
     R JE_SVEG,JE_GVEG,JC_GVEG,JC_SVEG,JS_GVEG,JS_SVEG, 
     S IPAR_HS,FPAR_SVEG,FPAR_GVEG,QSWV_HS,SM_HS,VMAXC_GVEG,VMAXC_SVEG,
     T VM_GVEG,VMUNS_GVEG,VMUNS1_GVEG,VMUNS2_GVEG,VMUNS3_GVEG,
     U VM_SVEG,VMUNS_SVEG,VMUNS1_SVEG,VMUNS2_SVEG,VMUNS3_SVEG,
     V CO2I_GVEG,CO2I_SVEG,TGA_HS,KC_HS,KO_HS,TCAN_HS,FSEAS_HS,
     W QSWINV_HS,ALVISC_HS,QSWNVG_HS  )
C-----------------------------------------------------------------------
C          * WATER BUDGET CALCULATIONS.
C
          CALL CLASSW  (THLQGAT,THICGAT,TBARGAT,TCANGAT,WCANGAT,SCANGAT,
     1                  ROFGAT, SNOGAT, TSNOGAT,RHOSGAT,ALBSGAT,TTGAT,  
     2                  PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT,QFCFGAT,QFCLGAT,
     3                  QFNGAT, QFGGAT, QFCGAT, HMFCGAT,HMFGGAT,HMFNGAT,
     4                  HTCCGAT,HTCSGAT,HTCGAT, ROFCGAT,ROFNGAT,ROFOGAT,
     5                  WTRSGAT,WTRGGAT,WLGAT,  WFGAT,  WLAGAT, WFAGAT,
     6                  WLASGAT,WFASGAT,TBASGAT,TBARC,  TBARG,  TBARCS, 
     7                  TBARGS, THLIQC, THLIQG, THICEC, THICEG, HCPC,   
     8                  HCPG,   FC,FG,  FCS,    FGS,    TPONDC, TPONDG,
     9                  TPNDCS, TPNDGS, EVAPC,  EVAPCG, EVAPG,  EVAPCS,
     A                  EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG,
     B                  RAICAN, SNOCAN, RAICNS, SNOCNS, QFSGAT, FROOT,
     C                  FSVF,   FSVFS,  CWCAP,  CWCAPS, TCANO,  TCANS,
     D                  CHCAP,  CHCAPS, CMASSC, CMASCS, ZSNOW,  ZPOND,
     E                  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,
     F                  G12CS,  G12GS,  G23C,   G23G,   G23CS,  G23GS,
     G                  TSNOCS, TSNOGS, ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS,
     H                  SANDGAT,CLAYGAT,ORGMGAT,DRNGAT, PREGAT, TSGAT,
     I                  DELZ,DELZW,ZBOTW,ILG,1,NLAND,JLAT,ICAN,IGND,
     J                  IGND+1,IGND+2,NLANDCS,NLANDGS,NLANDC,NLANDG )
C
C
C    ---------------------- CTEM MODIFICATIONS -------------------------\
C
C     USE NET PHOTOSYNTHETIC RATES FROM CANOPY OVER SNOW AND CANOPY OVER 
C     GROUND SUB-AREAS TO FIND AVERAGE NET PHOTOSYNTHETIC RATE FOR EACH
C     PFT. AND SIMILARLY FOR LEAF RESPIRATION.
C
      IF (CTEM1) THEN
        DO 600 J = 1, ICC
          DO 610 I = 1, NLAND
            IF ( (FCANC(I,J)+FCANCS(I,J)).GT.ABSZERO) THEN
              ANVEG(I,J)=(FCANC(I,J)*ANCGVEG(I,J) + 
     &                    FCANCS(I,J)*ANCSVEG(I,J)) / 
     &                   (FCANC(I,J)+FCANCS(I,J))   
              RMLVEG(I,J)=(FCANC(I,J)*RMLCGVEG(I,J) + 
     &                    FCANCS(I,J)*RMLCSVEG(I,J)) / 
     &                    (FCANC(I,J)+FCANCS(I,J))   
C     -----------------------------------------------------\HSuo testing     
	       GPPVEG(I,J)=(FCANC(I,J)*GPP_GVEG(I,J)+
     &	                 FCANCS(I,J)*GPP_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
             JEVEG(I,J)=(FCANC(I,J)*JE_GVEG(I,J)+
     &	                 FCANCS(I,J)*JE_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
             JCVEG(I,J)=(FCANC(I,J)*JC_GVEG(I,J)+
     &	                 FCANCS(I,J)*JC_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
             JSVEG(I,J)=(FCANC(I,J)*JS_GVEG(I,J)+
     &	                 FCANCS(I,J)*JS_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
             FPARVEG(I,J)=(FCANC(I,J)*FPAR_GVEG(I,J)+
     &	                 FCANCS(I,J)*FPAR_SVEG(I,J))/
     &                     (FCANC(I,J)+FCANCS(I,J))  
	       FCLOUD_HS(I)= FCLOUD(I)
	       VMVEG(I,J)=(FCANC(I,J)*VM_GVEG(I,J)+
     &	                 FCANCS(I,J)*VM_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
	       VMUNSVEG(I,J)=(FCANC(I,J)*VMUNS_GVEG(I,J)+
     &	                 FCANCS(I,J)*VMUNS_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
	       VMUNS1VEG(I,J)=(FCANC(I,J)*VMUNS1_GVEG(I,J)+
     &	                 FCANCS(I,J)*VMUNS1_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
	       VMUNS2VEG(I,J)=(FCANC(I,J)*VMUNS2_GVEG(I,J)+
     &	                 FCANCS(I,J)*VMUNS2_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
	       VMUNS3VEG(I,J)=(FCANC(I,J)*VMUNS3_GVEG(I,J)+
     &	                 FCANCS(I,J)*VMUNS3_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
	       CO2IVEG(I,J)=(FCANC(I,J)*CO2I_GVEG(I,J)+
     &	                 FCANCS(I,J)*CO2I_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   
		   VMAXCVEG(I,J)=(FCANC(I,J)*VMAXC_GVEG(I,J)+
     &	                 FCANCS(I,J)*VMAXC_SVEG(I,J))/
     &                    (FCANC(I,J)+FCANCS(I,J))   

C     -----------------------------------------------------/HSuo testing     
            ELSE
              ANVEG(I,J)=0.0
              RMLVEG(I,J)=0.0
C     -----------------------------------------------------\HSuo testing     
C 	       GPPVEG(I,J)=0.0
C              JEVEG(I,J)=0.0
C 	       JCVEG(I,J)=0.0
C              JSVEG(I,J)=0.0
C              FPARVEG(I,J)=0.0
C 	       VMVEG(I,J)=0.0
C 	       VMUNSVEG(I,J)=0.0
C 	       VMUNS1VEG(I,J)=0.0
C 	       VMUNS2VEG(I,J)=0.0
C 	       VMUNS3VEG(I,J)=0.0
C              CO2IVEG(I,J)=0.0
C     -----------------------------------------------------/HSuo testing     
            ENDIF
610       CONTINUE
600     CONTINUE
      ENDIF 
C
C    ---------------------- CTEM MODIFICATIONS -------------------------/
C-------------------------------------------------------
      DO 615 I = 1, NLAND
          GZEROGAT(I) = (FC(I)*GZEROC(I)+FG(I)*GZEROG(I)                 ! YUAN: G0 OUTPUT
     1                  +FCS(I)*GZROCS(I)+FGS(I)*GZROGS(I)) 
615   CONTINUE
C-------------------------------------------------------
C
C
C=======================================================================
C     * ACCUMULATE OUTPUT DATA.

      DO 650 I=1,NLAND
          PREACC(I)=PREACC(I)+PREGAT(I)*DELT
          GTACC(I)=GTACC(I)+GTGAT(I)
          QEVPACC(I)=QEVPACC(I)+QEVPGAT(I)
          EVAPACC(I)=EVAPACC(I)+QFSGAT(I)*DELT
C-------------------------------------------------------
          G0ACC(I) = G0ACC(I)+GZEROGAT(I)                 ! YUAN: G0 OUTPUT
C-------------------------------------------------------
          HFSACC(I)=HFSACC(I)+HFSGAT(I)
          HMFNACC(I)=HMFNACC(I)+HMFNGAT(I)
          ROFACC(I)=ROFACC(I)+ROFGAT(I)*DELT
          OVRACC(I)=OVRACC(I)+ROFOGAT(I)*DELT
          DO 660 J=1,IGND
              TBARACC(I,J)=TBARACC(I,J)+TBARGAT(I,J)
              THLQACC(I,J)=THLQACC(I,J)+THLQGAT(I,J)
              THICACC(I,J)=THICACC(I,J)+THICGAT(I,J)
              THALACC(I,J)=THALACC(I,J)+THLQGAT(I,J)+THICGAT(I,J)
660       CONTINUE
          ALSWACC(I)=ALSWACC(I)+ALSWGAT(I)*FSVHGAT(I)
          ALLWACC(I)=ALLWACC(I)+ALLWGAT(I)*FSIHGAT(I)
          IF(RHOSGAT(I).GT.0.0) THEN
              RHOSACC(I)=RHOSACC(I)+SNOGAT(I)/RHOSGAT(I)
          ELSE
              RHOSACC(I)=RHOSACC(I)
          ENDIF
          IF(SNOGAT(I).GT.0.0) THEN
              SNOACC(I)=SNOACC(I)+SNOGAT(I)
              TSNOACC(I)=TSNOACC(I)+TSNOGAT(I)
              NSNO(I)=NSNO(I)+1
          ENDIF
          TCANACC(I)=TCANACC(I)+TCANGAT(I)
          WCANACC(I)=WCANACC(I)+WCANGAT(I)
          SCANACC(I)=SCANACC(I)+SCANGAT(I)
          GROACC(I)=GROACC(I)+TTGAT(I)
          FSINACC(I)=FSINACC(I)+FSDOWN
          FLINACC(I)=FLINACC(I)+FDLGAT(I)
          FLUTACC(I)=FLUTACC(I)+SBC*GTGAT(I)**4
          TSACC(I)=TSACC(I)+TSGAT(I)
          UVACC(I)=UVACC(I)+UVGAT(I)
          PRESACC(I)=PRESACC(I)+PRESGAT(I)
          QSACC(I)=QSACC(I)+QSGAT(I)
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
          ETPACC(I)    = ETPACC(I)+ETP(I)
          IF(CSZGAT(I).GT.0.0) NDAYTIME(I) = NDAYTIME(I) + 1
          DO J = 1, ICAN
              IF(CSZGAT(I).GT.0.0)THEN
                  XMINFBAR(I,J)  = XMINFBAR(I,J)+ XMINF(I,J)
              ENDIF
          ENDDO
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C    ---------------------- CTEM MODIFICATIONS -------------------------\
          DO J = 1, ICAN
            AILACC(I,J)=AILACC(I,J)+AIL(I,J)
          ENDDO
C    ---------------------- CTEM MODIFICATIONS -------------------------/
650   CONTINUE
C
C
C    ---------------------- CTEM MODIFICATIONS -------------------------\
C
C     ACCUMULATE VARIABLES NOT ALREADY ACCUMULATED BUT WHICH ARE REQUIRED BY
C     CTEM.
C
      IF (CTEM2) THEN
        DO 700 I = 1, NLAND
          FSNOWACC(I)=FSNOWACC(I)+FSNOW(I)
          TCANOACC(I)=TCANOACC(I)+TCANO(I)
          TCANSACC(I)=TCANSACC(I)+TCANS(I)
          VVACC(I)=VVACC(I)+ VLGAT(I)
C
          DO 710 J=1,IGND
             TBARCACC(I,J)=TBARCACC(I,J)+TBARC(I,J)
             TBARCSACC(I,J)=TBARCSACC(I,J)+TBARCS(I,J)
             TBARGACC(I,J)=TBARGACC(I,J)+TBARG(I,J)
             TBARGSACC(I,J)=TBARGSACC(I,J)+TBARGS(I,J)
             THLIQCACC(I,J)=THLIQCACC(I,J)+THLIQC(I,J)
             THLIQGACC(I,J)=THLIQGACC(I,J)+THLIQG(I,J)
             THICECACC(I,J)=THICECACC(I,J)+THICEC(I,J)
710       CONTINUE
C
          DO 711 J = 1, ICC
            ANCSVGAC(I,J)=ANCSVGAC(I,J)+ANCSVEG(I,J)
            ANCGVGAC(I,J)=ANCGVGAC(I,J)+ANCGVEG(I,J)
            RMLCSVGA(I,J)=RMLCSVGA(I,J)+RMLCSVEG(I,J)
            RMLCGVGA(I,J)=RMLCGVGA(I,J)+RMLCGVEG(I,J)
711       CONTINUE
C
700     CONTINUE
      ENDIF
C
C    ---------------------- CTEM MODIFICATIONS -------------------------/
C
C=======================================================================
C     * WRITE RESULTS TO OUTPUT FILES.

      IF(((KOUT1.EQ.KOUT2).AND.(IDAY.GE.JOUT1.AND.IDAY.LE.JOUT2)).OR.
     1    ((KOUT1.NE.KOUT2).AND.((IYEAR.EQ.KOUT1.AND.IDAY.GE.JOUT1).OR.
     2    (IYEAR.GT.KOUT1.AND.IYEAR.LT.KOUT2).OR.
     3    (IYEAR.EQ.KOUT2.AND.IDAY.LE.JOUT2)))) THEN

      DO 770 I=1,NLAND
          FSSTAR=FSGVGAT(I)+FSGSGAT(I)+FSGGGAT(I)
          IF(FSDOWN.GT.0.0) THEN
              ALTOT=(FSDOWN-FSSTAR)/FSDOWN   !all-wave total albedo for crops and grass
          ELSE
              ALTOT=0.0
          ENDIF
          FLSTAR=FDLGAT(I)-SBC*GTGAT(I)**4
          QH=HFSGAT(I)
          QE=QEVPGAT(I)
          BEG=FSSTAR+FLSTAR-QH-QE
C-------------------------------------------------------
          GZERO = GZEROGAT(I)                 ! YUAN: G0 OUTPUT
C-------------------------------------------------------
          DRAIN=ROFGAT(I)-ROFOGAT(I)
          SNOMLT=HMFNGAT(I)
          IF(RHOSGAT(I).GT.0.0) THEN
              ZSN=SNOGAT(I)/RHOSGAT(I)
          ELSE
              ZSN=0.0
          ENDIF
          IF(TCANGAT(I).GT.0.01) THEN
              TCN=TCANGAT(I)-TFREZ
          ELSE
              TCN=0.0
          ENDIF
          IF(TSNOGAT(I).GT.0.01) THEN
              TSN=TSNOGAT(I)-TFREZ
          ELSE
              TSN=0.0
          ENDIF
          IF(ILW.EQ.1) THEN
              GTOUT=GTGAT(I)-TFREZ
          ELSE
              GTOUT=0.0
          ENDIF

          IF(LOPCOUNT.GE.CTEMLOOP)THEN
          WRITE(64,6400) IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,
     1                   SNOMLT,BEG,GTOUT,SNOGAT(I),RHOSGAT(I),
     2                   ALTOT,ROFGAT(I),
C    -------------------- CTEM MODIFICATIONS -------------------------\
     3                   CANRES(I),GZERO
C    -------------------- CTEM MODIFICATIONS -------------------------/
          WRITE(65,6500) IHOUR,IMIN,IDAY,IYEAR,(TBARGAT(I,J)-TFREZ,
     1                   THLQGAT(I,J),THICGAT(I,J),J=1,3),TCN,
     2                   WCANGAT(I),SCANGAT(I),TSN
          WRITE(66,6600) IHOUR,IMIN,IDAY,IYEAR,FSDOWN,FDLGAT(I),
     1                   TSGAT(I)-TFREZ,UVGAT(I),PRESGAT(I),QSGAT(I),
     2                   PREGAT(I),QFSGAT(I),ZSN
          WRITE(67,6700) IHOUR,IMIN,IDAY,IYEAR,FSGGGAT(I),FLGGGAT(I),
     1                   HFSGGAT(I),HEVGGAT(I),GZEROC(I),FSGVGAT(I),
     2                   FLGVGAT(I),HFSCGAT(I),HEVCGAT(I)
          ENDIF
6400  FORMAT(1X,I2,1X,I2,2I5,13E12.3)
6500  FORMAT(1X,I2,1X,I2,2I5,3(F8.2,2F6.3),F8.2,2F7.4,F8.2)
6600  FORMAT(1X,I2,1X,I2,2I5,3F9.2,F8.2,F10.2,3E12.3,F12.3)
6700  FORMAT(1X,I2,1X,I2,2I5,9F8.2)
C
C    ---------------------- CTEM MODIFICATIONS -------------------------\
C
C         WRITE HALF-HOURLY CTEM RESULTS TO FILE *.CT1
C
C         NET PHOTOSYNTHETIC RATES AND LEAF MAINTENANCE RESPIRATION FOR
C         EACH PFT. HOWEVER, IF CTEM2 OPTION IS ON THEN PHYSYN SUBROUTINE
C         IS USING STORAGE LAI WHILE ACTUAL LAI IS ZERO. IF ACTUAL LAI IS
C         ZERO THEN WE MAKE ANVEG AND RMLVEG ZERO AS WELL BECAUSE THESE
C         ARE IMAGINARY JUST LIKE STORAGE LAI. NOTE THAT ANVEG AND RMLVEG
C         ARE NOT PASSED TO CTEM. RATHER ANCSVEG, ANCGVEG, RMLCSVEG, AND
C         RMLCGVEG ARE PASSED.

          IF (CTEM1) THEN
            DO 760 J = 1,ICC
              IF(AILCG(I,J).EQ.0.0)THEN
                ANVEG(I,J)=0.0
                RMLVEG(I,J)=0.0
C     -----------------------------------------------------HSuo testing     
C 	       GPPVEG(I,J)=0.0
C              JEVEG(I,J)=0.0
C 	       JCVEG(I,J)=0.0
C              JSVEG(I,J)=0.0
C C              IPARVEG(I)=0.0
C              FPARVEG(I,J)=0.0
C 	       VMVEG(I,J)=0.0
C 	       VMUNSVEG(I,J)=0.0
C 	       VMUNS1VEG(I,J)=0.0
C 	       VMUNS2VEG(I,J)=0.0
C 	       VMUNS3VEG(I,J)=0.0

C  C     -----------------------------------------------------HSuo testing     
              ENDIF
760         CONTINUE
C  
C  
C  
C             
!------------------------------------------------------------------HSuo testing HH output            
            IF(LOPCOUNT.GE.CTEMLOOP)THEN
C 		  WRITE(71,7200)IHOUR,IMIN,IDAY,IYEAR,  
C      1      IPAR_HS(I),QSWV_HS(I),FCLOUD_HS(I),XDIFFUS(I),SM_HS(I),
C      2      (ANVEG(I,J),  J=1,ICC),(RMLVEG(I,J),J=1,ICC),
C      3      (GPPVEG(I,J),J=1,ICC),  !GPP=ANVEG+RMLVEG
C      4  (JEVEG(I,J),J=1,ICC),(JCVEG(I,J),J=1,ICC),(JSVEG(I,J),J=1,ICC),
C      4     (AILCG(I,J), J=1,ICC),(FPARVEG(I,J),J=1,ICC),
C      4      (VMVEG(I,J),J=1,ICC),(VMUNSVEG(I,J),J=1,ICC),
C      4      (VMUNS1VEG(I,J),J=1,ICC),(VMUNS2VEG(I,J),J=1,ICC),              
C      4	  (VMUNS3VEG(I,J),J=1,ICC),(CO2IVEG(I,J),J=1,ICC),
C 	4      TGA_HS(I),KO_HS(I),KC_HS(I),TCAN_HS(I),FSEAS_HS(I),
C 	4      (VMAXCVEG(I,J),J=1,ICC)
		  WRITE(71,7200)IHOUR,IMIN,IDAY,IYEAR,IPAR_HS(I),                !<---change here
     1	  VMAXCVEG(I,1),VMVEG(I,1),VMUNSVEG(I,1),VMUNS1VEG(I,1),  
     2      QSWV_HS(I),FCLOUD_HS(I),XDIFFUS(I),FPARVEG(I,1),
     3      ANVEG(I,1),RMLVEG(I,1),GPPVEG(I,1),
     4      JEVEG(I,1),JCVEG(I,1),JSVEG(I,1),
     5      VMUNS2VEG(I,1),VMUNS3VEG(I,1),
     6      AILCG(I,1),SM_HS(I),CO2IVEG(I,1),
     7      TGA_HS(I),KO_HS(I),KC_HS(I),TCAN_HS(I),FSEAS_HS(I),
	8      DAY_HS,DECL_HS,HOUR_HS,COSZ_HS

	      
            ENDIF
          ENDIF
C 7200      FORMAT(1X,I2,1X,I2,2I5,9F11.3,9F11.3, 9F11.3)  !Oiginal
C 7200   FORMAT(1X,I2,1X,I2,2I5,1E11.3,4F11.3,9F11.3,9F11.3,9F11.3,   !<---change here
C      1        9F11.3,9F11.3,9F11.3,9F11.3,9F11.3,9E11.3,9E11.3,9E11.3,
C      2        9F11.3,9F11.3,9F11.3,5F11.3,9E11.3)                         
7200   FORMAT(1X,I2,1X,I2,2I5,5E11.3,19F11.3,E11.3,4F11.3)           

	 WRITE(100,10000)  QSWINV_HS(I),ALVISC_HS(I),QSWNVG_HS(I)
10000	 FORMAT(3E11.3)   
!------------------------------------------------------------------HSuo testing HH output  
C 
C 
C 
C           
C    ---------------------- CTEM MODIFICATIONS -------------------------/
C
770   CONTINUE

      ENDIF
 
      IF(NCOUNT.EQ.48) THEN
C
      DO 850 I=1,NLAND
          PREACC(I)=PREACC(I)
          GTACC(I)=GTACC(I)/REAL(NSUM)
          QEVPACC(I)=QEVPACC(I)/REAL(NSUM)
          EVAPACC(I)=EVAPACC(I)
          HFSACC(I)=HFSACC(I)/REAL(NSUM)
          HMFNACC(I)=HMFNACC(I)/REAL(NSUM)
          ROFACC(I)=ROFACC(I)
          OVRACC(I)=OVRACC(I)
          DO 820 J=1,IGND
              TBARACC(I,J)=TBARACC(I,J)/REAL(NSUM)
              THLQACC(I,J)=THLQACC(I,J)/REAL(NSUM)
              THICACC(I,J)=THICACC(I,J)/REAL(NSUM)
              THALACC(I,J)=THALACC(I,J)/REAL(NSUM)
820       CONTINUE
          IF(FSINACC(I).GT.0.0) THEN
              ALSWACC(I)=ALSWACC(I)/(FSINACC(I)*0.5)
              ALLWACC(I)=ALLWACC(I)/(FSINACC(I)*0.5)
          ELSE
              ALSWACC(I)=0.0
              ALLWACC(I)=0.0
          ENDIF
          IF(RHOSACC(I).GT.0.0) THEN
              RHOSACC(I)=SNOACC(I)/RHOSACC(I)
          ELSE
              RHOSACC(I)=0.0
          ENDIF
          IF(NSNO(I).GT.0) THEN
              SNOACC(I)=SNOACC(I)/REAL(NSNO(I))
              TSNOACC(I)=TSNOACC(I)/REAL(NSNO(I))
          ENDIF
          TCANACC(I)=TCANACC(I)/REAL(NSUM)
          WCANACC(I)=WCANACC(I)/REAL(NSUM)
          SCANACC(I)=SCANACC(I)/REAL(NSUM)
          GROACC(I)=GROACC(I)/REAL(NSUM)
          FSINACC(I)=FSINACC(I)/REAL(NSUM)
          FLINACC(I)=FLINACC(I)/REAL(NSUM)
          FLUTACC(I)=FLUTACC(I)/REAL(NSUM)
          TSACC(I)=TSACC(I)/REAL(NSUM)
          UVACC(I)=UVACC(I)/REAL(NSUM)
          PRESACC(I)=PRESACC(I)/REAL(NSUM)
          QSACC(I)=QSACC(I)/REAL(NSUM)
C
C-------------------------------------------------------
          G0ACC(I) = G0ACC(I)                 ! YUAN: G0 OUTPUT
C-------------------------------------------------------
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
          ETPACC(I) = ETPACC(I)
          DO J = 1, ICAN
              XMINFBAR(I,J)  = XMINFBAR(I,J)/REAL(NDAYTIME(I))
          ENDDO
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C    -------------------- CTEM MODIFICATIONS -------------------------\
C
          DO J = 1, ICAN
            AILACC(I,J)=AILACC(I,J)/REAL(NSUM)
          ENDDO
C
C         DAILY AVERAGES OF ACCUMULATED VARIABLES FOR CTEM
C
          IF (CTEM2) THEN
            FSNOWACC(I)=FSNOWACC(I)/REAL(NSUM)
            TCANOACC(I)=TCANOACC(I)/REAL(NSUM)
            TCANSACC(I)=TCANSACC(I)/REAL(NSUM)
C
            DO 831 J=1,IGND
              TBARCACC(I,J)=TBARCACC(I,J)/REAL(NSUM)
              TBARCSACC(I,J)=TBARCSACC(I,J)/REAL(NSUM)
              TBARGACC(I,J)=TBARGACC(I,J)/REAL(NSUM)
              TBARGSACC(I,J)=TBARGSACC(I,J)/REAL(NSUM)
C
C             CONVERT SOIL TEMPERATURES INTO KELVIN
              TBARCACC(I,J)=TBARCACC(I,J)+TFREZ
              TBARCSACC(I,J)=TBARCSACC(I,J)+TFREZ
              TBARGACC(I,J)=TBARGACC(I,J)+TFREZ
              TBARGSACC(I,J)=TBARGSACC(I,J)+TFREZ
C
              THLIQCACC(I,J)=THLIQCACC(I,J)/REAL(NSUM)
              THLIQGACC(I,J)=THLIQGACC(I,J)/REAL(NSUM)
              THICECACC(I,J)=THICECACC(I,J)/REAL(NSUM)
831         CONTINUE       
C
            DO 832 J = 1, ICC
              ANCSVGAC(I,J)=ANCSVGAC(I,J)/REAL(NSUM)
              ANCGVGAC(I,J)=ANCGVGAC(I,J)/REAL(NSUM)
              RMLCSVGA(I,J)=RMLCSVGA(I,J)/REAL(NSUM)
              RMLCGVGA(I,J)=RMLCGVGA(I,J)/REAL(NSUM)
832         CONTINUE
C
C           PASS ON MEAN MONTHLY LIGHTNING FOR THE CURRENT MONTH TO CTEM
C           LIGHTNG(I)=MLIGHTNG(I,MONTH)
C
C           IN A VERY SIMPLE WAY TRY TO INTERPOLATE MONTHLY LIGHTNING TO
C           DAILY LIGHTNING
C
            IF(IDAY.GE.15.AND.IDAY.LE.45)THEN ! Mid Jan - Mid Feb
              MONTH1=1
              MONTH2=2
              XDAY=IDAY-15
            ELSE IF(IDAY.GE.46.AND.IDAY.LE.74)THEN ! Mid Feb - Mid Mar
              MONTH1=2
              MONTH2=3
              XDAY=IDAY-46
            ELSE IF(IDAY.GE.75.AND.IDAY.LE.105)THEN ! Mid Mar - Mid Apr
              MONTH1=3
              MONTH2=4
              XDAY=IDAY-75
            ELSE IF(IDAY.GE.106.AND.IDAY.LE.135)THEN ! Mid Apr - Mid May
              MONTH1=4
              MONTH2=5
              XDAY=IDAY-106
            ELSE IF(IDAY.GE.136.AND.IDAY.LE.165)THEN ! Mid May - Mid June
              MONTH1=5
              MONTH2=6
              XDAY=IDAY-136
            ELSE IF(IDAY.GE.166.AND.IDAY.LE.196)THEN ! Mid June - Mid July
              MONTH1=6
              MONTH2=7
              XDAY=IDAY-166
            ELSE IF(IDAY.GE.197.AND.IDAY.LE.227)THEN ! Mid July - Mid Aug
              MONTH1=7
              MONTH2=8
              XDAY=IDAY-197
            ELSE IF(IDAY.GE.228.AND.IDAY.LE.258)THEN ! Mid Aug - Mid Sep
              MONTH1=8
              MONTH2=9
              XDAY=IDAY-228
            ELSE IF(IDAY.GE.259.AND.IDAY.LE.288)THEN ! Mid Sep - Mid Oct
              MONTH1=9
              MONTH2=10
              XDAY=IDAY-259
            ELSE IF(IDAY.GE.289.AND.IDAY.LE.319)THEN ! Mid Oct - Mid Nov
              MONTH1=10
              MONTH2=11
              XDAY=IDAY-289
            ELSE IF(IDAY.GE.320.AND.IDAY.LE.349)THEN ! Mid Nov - Mid Dec
              MONTH1=11
              MONTH2=12
              XDAY=IDAY-320
            ELSE IF(IDAY.GE.350.OR.IDAY.LT.14)THEN ! Mid Nov - Mid Dec
              MONTH1=12
              MONTH2=1
              XDAY=IDAY-350
              IF(XDAY.LT.0)XDAY=IDAY
            ENDIF
C
            LIGHTNG(I)=MLIGHTNG(I,MONTH1)+(XDAY/30)*
     &                 (MLIGHTNG(I,MONTH2)-MLIGHTNG(I,MONTH1))
C
          ENDIF
C
850   CONTINUE        
C
C     CALL CANADIAN TERRESTRIAL ECOSYSTEM MODEL WHICH OPERATES AT A
C     DAILY TIME STEP, AND USES DAILY ACCUMULATED VALUES OF VARIABLES
C     SIMULATED BY CLASS.
C
      IF (CTEM2) THEN

         IF ((LANDUSE).AND.(IDAY.EQ.1)) THEN 
           DO I = 1, NLAND
             READ (90,*) USELESS,(NFCANCMX(I,J),J=1,ICC)
           ENDDO
         ENDIF

         CALL      CTEM ( FCANCMX,  FSNOWACC,   SANDGAT,   CLAYGAT,   
     2                       ICAN,       ILG,         1,     NLAND,
     3                       IGND,       ICC,      IDAY,      RADJ,
     4                   TCANOACC,  TCANSACC,  TBARCACC, TBARCSACC,
     5                   TBARGACC, TBARGSACC,     TSACC,     DELZW,
     6                   ANCSVGAC,  ANCGVGAC,  RMLCSVGA,  RMLCGVGA,
     7                      ZBOTW, THLIQCACC, THLIQGACC,    DELTAT,
     8                      UVACC,     VVACC,   LIGHTNG,  PRBFRHUC,
     9                   EXTNPROB,    STDALN,   TBARACC,     L2MAX,
     A                   NOL2PFTS,  PFCANCMX,  NFCANCMX,  LANDUSE,
     B                  THICECACC,                  
C    -------------- INPUTS USED BY CTEM ARE ABOVE THIS LINE ---------
     C                   STEMMASS,  ROOTMASS,  LITRMASS,  GLEAFMAS,
     D                   BLEAFMAS,  SOILCMAS,     AILCG,      AILC,
     E                      ZOLNC,  RMATCTEM,     RMATC,     AILCB,
     F                   FLHRLOSS,   PANDAYS,  LFSTATUS,  GRWTHEFF,
     G                   LYSTMMAS,  LYROTMAS,  TYMAXLAI,  VGBIOMAS,
     H                   GAVGLTMS,  GAVGSCMS,  STMHRLOS,      SLAI,
     I                    BMASVEG,  CMASVEGC,  COLDDAYS,  ROTHRLOS,
     J                    FCANGAT,   ALVSCTM,   ALIRCTM,   GAVGLAI,
C    -------------- INPUTS UPDATED BY CTEM ARE ABOVE THIS LINE ------
     K                        NPP,       NEP,  HETRORES,   AUTORES,
     L                   SOILRESP,        RM,        RG,       NBP,
     M                     LITRES,    SOCRES,       GPP,  DSTCEMLS,
     N                   LITRFALL,  HUMIFTRS,   VEGHGHT,  ROOTDPTH,
     O                        RML,       RMS,       RMR,  TLTRLEAF,
     P                   TLTRSTEM,  TLTRROOT,  LEAFLITR,  ROOTTEMP,
     Q                    AFRLEAF,   AFRSTEM,   AFRROOT,  WTSTATUS,
     R                   LTSTATUS,  BURNAREA,  PROBFIRE,  LUCEMCOM,
     S                   LUCLTRIN,  LUCSOCIN,    NPPVEG,  GRCLAREA,     
C      1                   GPPVEG,       RGVEG,     RMVEG,  HETRSVEG,    ! HSuo add HH output
C    ---------------- OUTPUTS ARE LISTED ABOVE THIS LINE ------------
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
C  -- INPUTS FROM CLASS
     T                    CTEMN,      CALSOIL,    THPOR,      BTDPTH,   &
     U                    ETPACC,      OVRACC,    ROFACC,     XMINFBAR, &
     V                    BI,                                           &
C  -- N RATIOS & CONTENTS IN C STOCKS OR SOIL BULK (UPDATED)
     V                    RNLEAF,     RNSTEM,     RNROOT,     RNLITR,   &
     W                    RNSOM,      SNH4,       SNO3,       NRUB,     &
     X                    NRUB0,                                        &
C  -- N BUDGTS (OUTPUT)
     Y                    DNDEPGRD,   DNFERGRD,   DNPLTRGRD,  DNDISGRD, &
     Z                    DNLSOMGRD,  DNMINGRD,   DNNITGRD,   DNPUPGRD, & 
     1                    DNDNITGRD,  DNLEAGRD,   DNVOLGRD,             &
     3                    DNSORGRD,   DNLOSGRD,   
C 	------------------------------------------------------------------------!HSuo
     4 N2OTOTGRD,N2TOTGRD,NLITR_HS,NSOM_HS,DNBFIXGRD,DNPLOSSGRD,
     5 DNSLOSSGRD ) 
C 	------------------------------------------------------------------------!HSuo
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C    ---------------- OUTPUTS ARE LISTED ABOVE THIS LINE ------------
      ENDIF
C
C    -------------------- CTEM MODIFICATIONS -------------------------/
C

c  ----------------------------------------------------------------------------------    start here

C
      DO 851 I=1,NLAND
C
          IF(((KAV1.EQ.KAV2).AND.(IDAY.GE.JAV1.AND.IDAY.LE.JAV2)).OR.
     1        ((KAV1.NE.KAV2).AND.((IYEAR.EQ.KAV1.AND.IDAY.GE.JAV1).OR.
     2        (IYEAR.GT.KAV1.AND.IYEAR.LT.KAV2).OR.
     3        (IYEAR.EQ.KAV2.AND.IDAY.LE.JAV2)))) THEN

              ALTOT=(ALSWACC(I)+ALLWACC(I))/2.0
              FSSTAR=FSINACC(I)*(1.-ALTOT)
              FLSTAR=FLINACC(I)-FLUTACC(I)
              QH=HFSACC(I)
              QE=QEVPACC(I)
C-------------------------------------------------------
              GZERO=G0ACC(I)                 ! YUAN: G0 OUTPUT
C-------------------------------------------------------
              BEG=FSSTAR+FLSTAR-QH-QE
              DRAIN=ROFACC(I)-OVRACC(I)
              SNOMLT=HMFNACC(I)
              IF(RHOSACC(I).GT.0.0) THEN
                  ZSN=SNOACC(I)/RHOSACC(I)
              ELSE
                  ZSN=0.0
              ENDIF
              IF(TCANACC(I).GT.0.01) THEN
                  TCN=TCANACC(I)-TFREZ
              ELSE
                  TCN=0.0
              ENDIF
              IF(TSNOACC(I).GT.0.01) THEN
                  TSN=TSNOACC(I)-TFREZ
              ELSE
                  TSN=0.0
              ENDIF
              IF(ILW.EQ.1) THEN
                  GTOUT=GTACC(I)-TFREZ
              ELSE
                  GTOUT=0.0
              ENDIF

              IF(LOPCOUNT.GE.CTEMLOOP)THEN
              WRITE(61,6100) IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,
     1                       BEG,GTOUT,SNOACC(I),RHOSACC(I),
     2                       ALTOT,ROFACC(I),GZERO
              WRITE(62,6200) IDAY,IYEAR,(TBARACC(I,J)-TFREZ,
     1                       THLQACC(I,J),THICACC(I,J),J=1,3),TCN,                        
     2                       WCANACC(I),SCANACC(I),TSN
              WRITE(63,6300) IDAY,IYEAR,FSINACC(I),FLINACC(I),
     1                       TSACC(I)-TFREZ,UVACC(I),PRESACC(I),
     2                       QSACC(I),PREACC(I),EVAPACC(I),ZSN
C 6100  FORMAT(1X,I4,I5,8F8.2,F10.4,F9.3,F11.3,F11.3)
6100  FORMAT(1X,I4,I5,8F10.2,F10.4,F9.3,F11.3,F11.3)    !hs------OF1 format problem
6200  FORMAT(1X,I4,I5,3(F8.2,2F6.3),F8.2,2F7.4,F8.2)
6300  FORMAT(1X,I4,I5,3F9.2,F8.2,F10.2,E12.3,2F12.3,F8.3)
C
C    -------------------- CTEM MODIFICATIONS -------------------------\
C
C
              WRITE(68,6800) IDAY,IYEAR,AILACC(I,1),AILACC(I,2),
     1                       AILACC(I,3),AILACC(I,4)
C
C             WRITE DAILY CTEM RESULTS
C
                IF (CTEM2) THEN
C
C               WRITE GRID-AVERAGED FLUXES OF BASIC QUANTITIES TO 
C               FILE *.CT2
C
                WRITE(72,8200)IDAY,IYEAR,GPP(I),NPP(I),NEP(I),NBP(I),
     1                        AUTORES(I),HETRORES(I),LITRES(I),
     2                        SOCRES(I),DSTCEMLS(I),LITRFALL(I),
     3                        HUMIFTRS(I)
C
C               WRITE BREAKDOWN OF SOME OF BASIC FLUXES TO FILE *.CT3 
C               AND SELECTED LITTER FLUXES FOR SELECTED PFT
C
                WRITE(73,8300)IDAY,IYEAR,RML(I),RMS(I),RMR(I),RG(I),
     1          LEAFLITR(I,JK),TLTRLEAF(I,JK),TLTRSTEM(I,JK),
     2          TLTRROOT(I,JK) 
C
C               WRITE GRID-AVERAGED POOL SIZES AND COMPONENT SIZES FOR
C               SELECED CTEM PFT TO FILE *.CT4
C
                WRITE(74,8400)IDAY,IYEAR,VGBIOMAS(I),GAVGLAI(I),
     1                        GAVGLTMS(I),GAVGSCMS(I),GLEAFMAS(I,JK),
     2                        BLEAFMAS(I,JK), STEMMASS(I,JK),
     3                        ROOTMASS(I,JK), LITRMASS(I,JK), 
     4                        SOILCMAS(I,JK)
C
C               WRITE LAI, RMATCTEM, & STRUCTURAL ATTRIBUTES FOR SELECTED 
C               PFT TO FILE *.CT5
C
                WRITE(75,8500)IDAY,IYEAR, AILCG(I,JK), AILCB(I,JK),
     1                        (RMATCTEM(I,JK,K),K=1,3), VEGHGHT(I,JK),
     2                        ROOTDPTH(I,JK),ROOTTEMP(I,JK),SLAI(I,JK) 
C
C               WRITE ALLOCATION FRACTIONS FOR SELECTED PFT TO FILE *.CT6
C
                WRITE(76,8600)IDAY,IYEAR, AFRLEAF(I,JK), AFRSTEM(I,JK),
     1                        AFRROOT(I,JK), TCANOACC(I), LFSTATUS(I,JK)
C
C               WRITE FIRE AND LUC RESULTS TO FILE *.CT8
C
                WRITE(78,8800)IDAY,IYEAR, BURNAREA(I), PROBFIRE(I),
     1                        LUCEMCOM(I), LUCLTRIN(I), LUCSOCIN(I),
     2                        GRCLAREA(I)
C
                ENDIF 
6800            FORMAT(1X,I4,I5,4F10.5)
8200            FORMAT(1X,I4,I5,11F10.5)
8300            FORMAT(1X,I4,I5,8F10.5)
8400            FORMAT(1X,I4,I5,10F10.5)
8500            FORMAT(1X,I4,I5,9F10.5)
8600            FORMAT(1X,I4,I5,4F10.5,I8)
8800            FORMAT(1X,I4,I5,5F11.4,2X,F9.2)
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
                IF (CTEMN) THEN
C
C  WRITE original SELECTED POOL N/C RATIO and GRID-AVERAGED N DAILY CHANGES (FLUXES) to CN1
       WRITE(81,9100)IDAY,IYEAR,RNLEAF(I,JK),RNSTEM(I,JK),RNROOT(I,JK),
     1   RNLITR(I,JK), RNSOM(I,JK), VCMX0(I,JK)              
C
C  WRITE HSuo selected TO FILE *.CN2 
       WRITE(82,9200)IDAY,IYEAR,SNH4(I,JK),SNO3(I,JK), NLITR_HS(I,JK),
     1  NSOM_HS(I,JK),DNLSOMGRD(I),DNNITGRD(I),DNDNITGRD(I),DNMINGRD(I), 
     2  DNPUPGRD(I),DNPLTRGRD(I),NRUB0(I,JK),NRUB(I,JK),           
     3  DNSORGRD(I),DNBFIXGRD(I),DNDEPGRD(I),DNFERGRD(I),     
     4  DNLOSGRD(I),N2OTOTGRD(I),N2TOTGRD(I),DNVOLGRD(I), DNLEAGRD(I),         
     5  DNDISGRD(I),DNPLOSSGRD(I),DNSLOSSGRD(I)                         
C
                ENDIF 
9100            FORMAT(1X,I4,I5,6E12.5)
9200            FORMAT(1X,I4,I5,24E13.4)      !HSuo
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
              ENDIF  ! DAILY FILE WRITE CONTROL
C    -------------------- CTEM MODIFICATIONS -------------------------/
C
          ENDIF
C
          PREACC(I)=0.
          GTACC(I)=0.
          QEVPACC(I)=0.
          HFSACC(I)=0.
          HMFNACC(I)=0.
          ROFACC(I)=0.
          SNOACC(I)=0.
          NSNO(I)=0
          OVRACC(I)=0.
          DO 830 J=1,IGND
              TBARACC(I,J)=0.
              THLQACC(I,J)=0.
              THICACC(I,J)=0.
              THALACC(I,J)=0.
830       CONTINUE
          ALSWACC(I)=0.
          ALLWACC(I)=0.
          RHOSACC(I)=0.
          TSNOACC(I)=0.
          TCANACC(I)=0.
          WCANACC(I)=0.
          SCANACC(I)=0.
          GROACC(I)=0.
          FSINACC(I)=0.
          FLINACC(I)=0.
          TSACC(I)=0.
          UVACC(I)=0.
          PRESACC(I)=0.
          QSACC(I)=0.
          EVAPACC(I)=0.
          FLUTACC(I)=0.
C-------------------------------------------------------
          G0ACC(I) = 0.                 ! YUAN: G0 OUTPUT
C-------------------------------------------------------
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
          ETPACC(I)    = 0.0
          NDAYTIME(I)    = 0
          DO J = 1, ICAN
              XMINFBAR(I,J)  = 0.0
          ENDDO
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
C    -------------------- CTEM MODIFICATIONS -------------------------\
C
          DO J = 1, ICAN
            AILACC(I,J)=0.0
          ENDDO
C
C         SET ACCUMULATED VARIABLES USED BY CTEM TO ZERO FOR THE NEXT DAY
C
          FSNOWACC(I)=0.0
          TCANOACC(I)=0.0
          TCANSACC(I)=0.0
C
          DO 835 J=1,IGND
             TBARCACC(I,J)=0.0
             TBARCSACC(I,J)=0.0
             TBARGACC(I,J)=0.0
             TBARGSACC(I,J)=0.0
             THLIQCACC(I,J)=0.0
             THLIQGACC(I,J)=0.0
835       CONTINUE
C
          DO 836 J = 1, ICC
            ANCSVGAC(I,J)=0.0
            ANCGVGAC(I,J)=0.0
            RMLCSVGA(I,J)=0.0
            RMLCGVGA(I,J)=0.0
836       CONTINUE
C
C         WRITE/PREPARE YEARLY CTEM RESULTS FOR FILE *.CT7
C
          IF(IDAY.LE.365) THEN
            AVGYRNPP(I)=AVGYRNPP(I)+NPP(I)
            AVGYRGPP(I)=AVGYRGPP(I)+GPP(I)
            AVGYRNEP(I)=AVGYRNEP(I)+NEP(I)
            AVGYRLE(I)=AVGYRLE(I)+QE
            DO 930 J = 1, ICC
              ANNPPVEG(I,J)=ANNPPVEG(I,J)+NPPVEG(I,J)
              IF(AILCG(I,J).GT.LAIMAXG(I,J)) THEN
                LAIMAXG(I,J)=AILCG(I,J)
              ENDIF
930         CONTINUE
          ENDIF
C
C         WRITE MAX GREEN LAI, AND STEM, ROOT, LITTER, AND SOIL C AT THE 
C         END OF EVERY YEAR, FOR SELECTED PFT, AND GRID AVERAGED ANNUAL
C         NPP, TO THE YEARLY RESULTS FILE.
C
          IF(IDAY.EQ.365) THEN
            AVGYRNPP(I)=AVGYRNPP(I)*1.0368 ! CONVERT TO gC/M2.YEAR
            AVGYRGPP(I)=AVGYRGPP(I)*1.0368 ! CONVERT TO gC/M2.YEAR
            AVGYRNEP(I)=AVGYRNEP(I)*1.0368 ! CONVERT TO gC/M2.YEAR
            DO J = 1, ICC
              ANNPPVEG(I,J)=ANNPPVEG(I,J)*1.0368 ! CONVERT TO gC/M2,YEAR
            ENDDO
            AVGYRLE(I)=AVGYRLE(I)/365.0
            IF (CTEM2) THEN
 !             IF(LOPCOUNT.GE.CTEMLOOP)THEN
              WRITE(77,8700)IYEAR, LAIMAXG(I,JK), STEMMASS(I,JK),
     &                 ROOTMASS(I,JK), LITRMASS(I,JK),SOILCMAS(I,JK),
     &                 AVGYRNPP(I),AVGYRGPP(I),AVGYRNEP(I),AVGYRLE(I)  
C               WRITE(79,8710)'GLEAFMAS ',IYEAR, (GLEAFMAS(I,J),J=1,ICC)
C               WRITE(79,8710)'BLEAFMAS ',IYEAR, (BLEAFMAS(I,J),J=1,ICC)
C               WRITE(79,8710)'STEMMASS ',IYEAR, (STEMMASS(I,J),J=1,ICC)
C               WRITE(79,8710)'ROOTMASS ',IYEAR, (ROOTMASS(I,J),J=1,ICC)
C               WRITE(79,8710)'LITRMASS ',IYEAR, (LITRMASS(I,J),J=1,ICC)
C               WRITE(79,8710)'SOILCMAS ',IYEAR, (SOILCMAS(I,J),J=1,ICC)
C               WRITE(79,8710)'LAIMAXG  ',IYEAR, (LAIMAXG(I,J),J=1,ICC)
C               WRITE(79,8710)'ANNPPVEG ',IYEAR, (ANNPPVEG(I,J),J=1,ICC)
C               WRITE(79,8720)'LFSTATUS ',IYEAR, (LFSTATUS(I,J),J=1,ICC)
C               WRITE(79,8720)'PANDAYS  ',IYEAR, (PANDAYS(I,J),J=1,ICC)
C 8710        FORMAT(A9,I5,9F10.3)
C 8720        FORMAT(A9,I5,9I10)
              WRITE(79,8710) (GLEAFMAS(I,J),J=1,ICC)
              WRITE(79,8710) (BLEAFMAS(I,J),J=1,ICC)
              WRITE(79,8710) (STEMMASS(I,J),J=1,ICC)
              WRITE(79,8710) (ROOTMASS(I,J),J=1,ICC)
              WRITE(79,8710) (LITRMASS(I,J),J=1,ICC)
              WRITE(79,8710) (SOILCMAS(I,J),J=1,ICC)
              WRITE(79,8710) (LAIMAXG(I,J),J=1,ICC)
              WRITE(79,8710) (ANNPPVEG(I,J),J=1,ICC)
              WRITE(79,8720) (LFSTATUS(I,J),J=1,ICC)
              WRITE(79,8720) (PANDAYS(I,J),J=1,ICC)
8710        FORMAT(9F10.3)
8720        FORMAT(9I10)
!              ENDIF
            ENDIF
            AVGYRNPP(I)=0.0
            AVGYRGPP(I)=0.0
            AVGYRNEP(I)=0.0
            AVGYRLE(I)=0.0
            DO 950 J = 1, ICC
              LAIMAXG(I,J)=0.0
              ANNPPVEG(I,J)=0.0
950         CONTINUE
8700        FORMAT(1X,I5,9F10.3)
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
		  IF ((CSTOCKINI) .AND. (LOPCOUNT.LT.CTEMLOOP)) THEN	! DON'T UPDATE C POOL
			DO 960 J = 1, ICC
			    GLEAFMAS(I,J)  = GLEAFMAS0(I,J)
			    BLEAFMAS(I,J)  = BLEAFMAS0(I,J)       
				STEMMASS(I,J)  = STEMMASS0(I,J)       
				ROOTMASS(I,J)  = ROOTMASS0(I,J)       
				LITRMASS(I,J)  = LITRMASS0(I,J)       
				SOILCMAS(I,J)  = SOILCMAS0(I,J)       
960           CONTINUE
				LITRMASS(I,ICC+1)  = LITRMASS0(I,ICC+1)       
				SOILCMAS(I,ICC+1)  = SOILCMAS0(I,ICC+1)       
		  ENDIF
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\


          ENDIF
C
C    -------------------- CTEM MODIFICATIONS -------------------------/
C
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++\
C         * WRITE/PREPARE YEARLY N RESULTS FOR FILE *.CN3
C
          IF (CTEMN) THEN
              IF(IDAY.LE.365) THEN
                  YRNDEP(I)  = YRNDEP(I)+DNDEPGRD(I)
                  YRNFER(I)  = YRNFER(I)+DNFERGRD(I)
                  YRNPLTR(I) = YRNPLTR(I)+DNPLTRGRD(I)
                  YRNDIS(I)  = YRNDIS(I)+DNDISGRD(I)
                  YRNLSOM(I) = YRNLSOM(I)+DNLSOMGRD(I)
                  YRNMIN(I)  = YRNMIN(I)+DNMINGRD(I)
                  YRNNIT(I)  = YRNNIT(I)+DNNITGRD(I)
                  YRNPUP(I)  = YRNPUP(I)+DNPUPGRD(I)
                  YRNDNIT(I) = YRNDNIT(I)+DNDNITGRD(I)
                  YRNLEA(I)  = YRNLEA(I)+DNLEAGRD(I)
                  YRNVOL(I)  = YRNVOL(I)+DNVOLGRD(I)
                  YRNSRCE(I) = YRNSRCE(I)+DNSORGRD(I)
                  YRNLOSS(I) = YRNLOSS(I)+DNLOSGRD(I)
C    ---------------------------------------------------!HSuo
                  YRNN2O(I)  = YRNN2O(I)+N2OTOTGRD(I)  
                  YRNN2(I)   = YRNN2(I)+N2TOTGRD(I)  
	            YRNBFX(I)  = YRNBFX(I)+DNBFIXGRD(I)
                  YRNPLOSS(I)= YRNPLOSS(I)+DNPLOSSGRD(I)
	            YRNSLOSS(I)= YRNSLOSS(I)+DNSLOSSGRD(I) 
C    ---------------------------------------------------!HSuo
              ENDIF
C
              IF(IDAY.EQ.365) THEN
                  IF(LOPCOUNT.GE.CTEMLOOP)THEN
                  WRITE(83,9300) IYEAR,    YRNDEP(I),  YRNFER(I),
     1               YRNPLTR(I), YRNDIS(I),YRNLSOM(I), YRNMIN(I), 
     2               YRNNIT(I),  YRNPUP(I),YRNDNIT(I), YRNLEA(I),
     3               YRNVOL(I),  YRNSRCE(I), YRNLOSS(I), 
C    ------------------------------------------------------!HSuo
     4 YRNN2O(I),YRNN2(I),YRNBFX(I),YRNPLOSS(I),YRNSLOSS(I)  
C    ------------------------------------------------------!HSuo
                  ENDIF

                  YRNDEP(I)  = 0.0
                  YRNFER(I)  = 0.0
                  YRNPLTR(I) = 0.0
                  YRNDIS(I)  = 0.0
                  YRNLSOM(I) = 0.0
                  YRNMIN(I)  = 0.0
                  YRNNIT(I)  = 0.0
                  YRNPUP(I)  = 0.0
                  YRNDNIT(I) = 0.0
                  YRNLEA(I)  = 0.0
                  YRNVOL(I)  = 0.0
                  YRNSRCE(I) = 0.0
                  YRNLOSS(I) = 0.0
	            YRNN2O(I)  = 0.0   !HSuo
	            YRNN2(I)   = 0.0
              ENDIF
9300        FORMAT(1X,I5,18F10.3)  !HSuo
          ENDIF
C     +++++++++++++++++ NITROGEN COMPONENTS FOR CTEM ++++++++++++++++++/
C
851   CONTINUE
C
      ENDIF                   ! IF NCOUNT.EQ.48 LOOP
C
      NCOUNT=NCOUNT+1
      NSUM=NSUM+1
      IF(NCOUNT.GT.48) THEN
          NCOUNT=1
          NSUM=1
      ENDIF

      IDAY=IDAY+(IHOUR+(IMIN+ITINCR)/60)/24
      IHOUR=IHOUR+(IMIN+ITINCR)/60
      IF(IHOUR.EQ.24) IHOUR=0
      IMIN=IMIN+ITINCR
      IF(IMIN.EQ.60) IMIN=0
      IF(IDAY.GT.365) THEN
          IDAY=1
          IYEAR=IYEAR+1
          WRITE(*,*) 'LOOP', LOPCOUNT, '    YEAR',IYEAR-1,' DONE'     ! CTEM MODIFICATION
      ENDIF

      GO TO 150
900   CONTINUE
C
C    -------------------- CTEM MODIFICATIONS -------------------------\
      LOPCOUNT = LOPCOUNT+1
      IF(LOPCOUNT.LE.CTEMLOOP)THEN
C
C      REWINDING THE MET FILE TO THE FIRST LINE
C
	  REWIND (UNIT=51)             
        IYEAR = IYEAR0
        IDAY  = IDAY0
        IHOUR = IHOUR0
        IMIN  = IMIN0

        GO TO 150 
      ENDIF
C    -------------------- CTEM MODIFICATIONS -------------------------/
C      

      STOP
      END
C
C      CLASS' SUBROUTINES
C
C      INCLUDE 'APREP.FOR'
C      INCLUDE 'CANADD.FOR'
C      INCLUDE 'CANALB.FOR'
C      INCLUDE 'CANVAP.FOR'
C      INCLUDE 'CGROW.FOR'
C      INCLUDE 'CHKWAT.FOR'
C      INCLUDE 'CLASSA.FOR'
C      INCLUDE 'CLASST.FOR'
C      INCLUDE 'CLASSW.FOR'
C      INCLUDE 'CWCALC.FOR'
C      INCLUDE 'DRCOEF.FOR'
C      INCLUDE 'GRALB.FOR'
C      INCLUDE 'GRDRAN.FOR'
C      INCLUDE 'GRINFL.FOR'
C      INCLUDE 'ICEBAL.FOR'
C      INCLUDE 'SNINFL.FOR'
C      INCLUDE 'SNOADD.FOR'
C      INCLUDE 'SNOALBA.FOR'
C      INCLUDE 'SNOALBW.FOR'
C      INCLUDE 'SNOVAP.FOR'
C      INCLUDE 'SPWCON7.FOR'
C      INCLUDE 'SUBCAN.FOR'
C      INCLUDE 'TFREEZ.FOR'
C      INCLUDE 'TMCALC.FOR'
C      INCLUDE 'TMELT.FOR'
C      INCLUDE 'TNPOST.FOR'
C      INCLUDE 'TNPREP.FOR'
C      INCLUDE 'TPREP.FOR'
C      INCLUDE 'TSOLVC.FOR'
C      INCLUDE 'TSOLVE.FOR'
C      INCLUDE 'TSPOST.FOR'
C      INCLUDE 'TSPREP.FOR'
C      INCLUDE 'TWCALC.FOR'
C      INCLUDE 'WEND.FOR'
C      INCLUDE 'WFILL.FOR'
C      INCLUDE 'WFLOW.FOR'
C      INCLUDE 'WPREP.FOR'
C      INCLUDE 'XIT.FOR'
C
C      CTEM'S SUBROUTINES
C
C      INCLUDE 'ALLOCATE.F'
C      INCLUDE 'BALCAR.F'
C      INCLUDE 'BIO2STR.F'
C      INCLUDE 'CTEM.F'
C      INCLUDE 'DISTURB.F'
C      INCLUDE 'HETRESG.F'
C      INCLUDE 'HETRESV.F'
C      INCLUDE 'MAINRES.F'
C      INCLUDE 'MORTALTY.F'
C      INCLUDE 'PHENOLGY.F'
C      INCLUDE 'PHTSYN.F'
C      INCLUDE 'TURNOVER.F'
