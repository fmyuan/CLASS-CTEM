      SUBROUTINE CLASSA(ALVS,   ALIR,   RCAN,   SCAN,   FC,     FG,
     1                  FCS,    FGS,    RAICAN, RAICNS, SNOCAN, SNOCNS,
     2                  DISP,   DISPS,  CHCAP,  CHCAPS, ZOMLNC, ZOMLCS,           
     3                  ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS,           
     4                  RCMIN,  RCMINS, CMASSC, CMASCS, FSVF,   FSVFS,               
     5                  CWCAP,  CWCAPS, FRAINC, FSNOWC, ALVSCN, ALIRCN,               
     6                  ALVSG,  ALIRG,  ALVSCS, ALIRCS, ALVSSN, ALIRSN,           
     7                  TRVSCN, TRIRCN, TRVSCS, TRIRCS, TRSNOW, ZSNOW,         
     8                  FROOT,  CMAI,   HTCC,   HTCS,   HTC,    WTRC,   
     9                  WTRS,   WTRG,   ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS,
     A                  FCANMX, ZOLN,   ALVSC,  ALIRC,  AILMAX, AILMIN,           
     B                  CWGTMX, ZRTMAX, THLIQ,  THICE,  COSZS,  QSWD,           
     C                  QSWI,   QSWV,   SAND,   CLAY,   ORGM,   SNO,    
     D                  RHOSNO, ALBSNO, GROWTH, TCAN,   TSNOW,  TBAR,   
     E                  TA,     RADJ,   ILAND,  ILSL,   LONSL,  ZORAT,  
     F                  DELZ,   DELZW,  ZBOTW,  FSNOW,  
     G                  IDAY,ILG,IL1,IL2,JL,IC,ICP1,IG,IDISP,  
     H                  RMAT,   H,HS,   AIL,    AILS,   FCAN,   FCANS,
     I                  CWCPAV, AILCAN, AILCNS, FCLOUD, GROWA,  GROWN,
     J                  GROWB,  RRESID, SRESID, THPORI, HCPS,   ISAND,
C
C    -------------------- CTEM MODIFICATIONS -------------------------\
     K            FCANCMX,    ICC,    CTEM1,   CTEM2,    RMATC, ZOLNC, 
     L               AILC,  AILCG, CMASVEGC, AILCMIN,  AILCMAX, L2MAX,
     M           NOL2PFTS, ALVSCTM, ALIRCTM,
C    ------------- CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW --------|
     N             AILCGS,  FCANCS, FCANC)
C
C     * CTEM1  - LOGICAL BOOLEAN FOR USING CTEM's STOMATAL RESISTANCE OR NOT
C     * CTEM2  - LOGICAL BOOLEAN FOR USING CTEM's STRUCTUTAL ATTRIBUTES OR NOT
C     * AILCG  - GREEN LAI FOR USE IN PHOTOSYNTHESIS
C     * AILCGS - GREEN LAI FOR CANOPY OVER SNOW SUB-AREA
C     * AILCMIN- MIN. LAI FOR CTEM PFTs
C     * AILCMAX- MAX. LAI FOR CTEM PFTs
C     * L2MAX  - MAXIMUM NUMBER OF LEVEL 2 CTEM PFTs
C     * NOL2PFTS - NUMBER OF LEVEL 2 CTEM PFTs
C     * FCANC  - FRACTION OF CANOPY OVER GROUND FOR CTEM's 9 PFTs
C     * FCANCS - FRACTION OF CANOPY OVER SNOW FOR CTEM's 9 PFTs
C     * SEE BIO2STR SUBROUTINE FOR DEFINITION OF OTHER CTEM VARIABLES
C
C    -------------------- CTEM MODIFICATIONS -------------------------/
C
C     * MAR 20/03 - V.ARORA.    MADE MODIFICATIONS TO COUPLE CLASS 2.7  
C     *                         WITH CTEM V1.0. ADDITIONAL VARIABLES 
C     *                         ARE PASSED TO CLASSA AND APREP. 
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         FIX BUG TO CALCULATE GROUND ALBEDO
C     *                         UNDER CANOPIES AS WELL AS OVER BARE
C     *                         SOIL.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     *                         ALSO, PASS IDISP TO SUBROUTINE APREP.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 16/95 - D.VERSEGHY. THREE NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * OCT 14/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF FCLOUD TO
C     *                         HANDLE CASES WHERE INCOMING SOLAR
C     *                         RADIATION IS ZERO AT LOW SUN ANGLES.
C     * NOV 24/92 - M.LAZARE.   CLASS - VERSION 2.1.
C     *                         MODIFIED FOR MULTIPLE LATITUDES.
C     * OCT 13/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. VISIBLE AND NEAR-IR ALBEDOS AND 
C     *                         TRANSMISSIVITIES FOR COMPONENTS OF
C     *                         LAND SURFACE.
C
C     * OUTPUT ARRAYS USED ELSEWHERE IN CLASS.
C
      REAL ALVS  (ILG),     ALIR  (ILG),     RCAN  (ILG),   SCAN  (ILG),
     1     FC    (ILG),     FG    (ILG),     FCS   (ILG),   FGS   (ILG),    
     2     RAICAN(ILG),     RAICNS(ILG),     SNOCAN(ILG),   SNOCNS(ILG),
     3     DISP  (ILG),     DISPS (ILG),     CHCAP (ILG),   CHCAPS(ILG), 
     4     ZOMLNC(ILG),     ZOMLCS(ILG),     ZOELNC(ILG),   ZOELCS(ILG),
     5     ZOMLNG(ILG),     ZOMLNS(ILG),     ZOELNG(ILG),   ZOELNS(ILG),
     6     RCMIN (ILG),     RCMINS(ILG),     CMASSC(ILG),   CMASCS(ILG),
     7     FSVF  (ILG),     FSVFS (ILG),     CWCAP (ILG),   CWCAPS(ILG), 
     8     FRAINC(ILG),     FSNOWC(ILG),     ALVSCN(ILG),   ALIRCN(ILG),              
     9     ALVSG (ILG),     ALIRG (ILG),     ALVSCS(ILG),   ALIRCS(ILG),              
     A     ALVSSN(ILG),     ALIRSN(ILG),     TRVSCN(ILG),   TRIRCN(ILG),              
     B     TRVSCS(ILG),     TRIRCS(ILG),     TRSNOW(ILG),   ZSNOW (ILG),
     C     HTCC  (ILG),     HTCS  (ILG),     WTRC  (ILG),   WTRS  (ILG),     
     D     WTRG  (ILG),     ZPLIMC(ILG),     ZPLIMG(ILG),   ZPLMCS(ILG),
     E     ZPLMGS(ILG),     CMAI  (ILG),     FSNOW (ILG)
C
      REAL FROOT (ILG,IG),  HTC   (ILG,IG)
C
C     * INPUT ARRAYS DEPENDENT ON LONGITUDE.
C  
      REAL FCANMX(ILG,ICP1),                 ZOLN  (ILG,ICP1),
     1     ALVSC (ILG,ICP1),                 ALIRC (ILG,ICP1),
     2     AILMAX(ILG,IC),  AILMIN(ILG,IC),  CWGTMX(ILG,IC),                     
     3     ZRTMAX(ILG,IC),  THLIQ (ILG,IG),  THICE (ILG,IG),
     4     TBAR  (ILG,IG),  SAND  (ILG,IG),  CLAY  (ILG,IG),
     5     ORGM  (ILG,IG)
C
      REAL COSZS (ILG),     QSWD  (ILG),     QSWI  (ILG),   QSWV  (ILG),
     1     SNO   (ILG),     RHOSNO(ILG),     ALBSNO(ILG),
     2     GROWTH(ILG),     TCAN  (ILG),     TSNOW (ILG),   TA    (ILG)

      REAL*8 RADJ(ILG) 
C
      INTEGER ILAND (ILG),  ILSL  (ILG)
C
C     * BACKGROUND ARRAYS - INDEPENDENT OF LONGITUDE.
C
      REAL GROWYR(18,4,2),  ZORAT (IC),      DELZ  (IG),
     1     DELZW (ILG,IG),  ZBOTW (ILG,IG),  CANEXT(4)
C                                                                                 
C     * INTERNAL ARRAYS NOT USED ELSEWHERE IN CLASS.
C
      REAL RMAT (ILG,IC,IG),H     (ILG,IC),  HS    (ILG,IC),
     1     AIL   (ILG,IC),  AILS  (ILG,IC),
     2     FCAN  (ILG,IC),  FCANS (ILG,IC), 
     3     CWCPAV(ILG),     AILCAN(ILG),     AILCNS(ILG),   
     4     FCLOUD(ILG),     
     5     GROWA (ILG),     GROWN (ILG),     GROWB (ILG),
     6     RRESID(ILG),     SRESID(ILG), 
     7     THPORI(ILG,IG),  HCPS  (ILG,IG)
C
C    ----------------- CTEM MODIFICATIONS ------------------------\
C
      REAL     AILCG(ILG,ICC),       AILC(ILG,IC),  AILCGS(ILG,ICC),
     1       RMATC(ILG,IC,IG),   FCANCMX(ILG,ICC),   FCANC(ILG,ICC), 
     2        FCANCS(ILG,ICC),      ZOLNC(ILG,IC), CMASVEGC(ILG,IC),
     3       AILCMIN(ILG,ICC),   AILCMAX(ILG,ICC),  ALVSCTM(ILG,IC),
     4        ALIRCTM(ILG,IC)
C
      INTEGER ICC, L2MAX, NOL2PFTS(IC)
C
      LOGICAL CTEM1, CTEM2
C    ----------------- CTEM MODIFICATIONS ------------------------/
C
      INTEGER               ISAND(ILG,IG)
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
      COMMON /CLASS6/ PI,GROWYR,ZOLNG,ZOLNS,ZOLNI,ZORATG                   
      COMMON /CLASS7/ CANEXT
                                                                                  
      DATA ALVSI,ALIRI/0.90,0.70/                                                 
      DATA SNOLIM/0.10/                                                           
C------------------------------------------------------------------
C     * CALCULATION OF SNOW DEPTH ZSNOW AND FRACTIONAL SNOW COVER FSNOW; ESTIMATION OF FRACTIONAL CLOUD COVER FCLOUD.
C                                                                                  
      DO 100 I=IL1,IL2                                                            
          IF(SNO(I).GT.0.0) THEN                                              
              ZSNOW(I)=SNO(I)/RHOSNO(I)                                       
              IF(ZSNOW(I).GT.SNOLIM) THEN                                     
                  FSNOW(I)=1.0                                                   
              ELSE                                                            
                  FSNOW(I)=ZSNOW(I)/SNOLIM                                       
                  ZSNOW(I)=SNOLIM                                             
              ENDIF                                                           
          ELSE                                                                
              ZSNOW(I)=0.0                                                    
              FSNOW(I)=0.0                                                       
          ENDIF
          IF(COSZS(I).GT.0.0.AND.(QSWV(I)+QSWI(I)).GT.0.0) THEN  
              FCLOUD(I)=MAX(0.0,MIN(1.0,QSWD(I)/(QSWV(I)+QSWI(I))))
          ELSE
              FCLOUD(I)=0.
          ENDIF
          ALVSCN(I)=0.0                                                   
          ALIRCN(I)=0.0                                                   
          ALVSCS(I)=0.0  
          ALIRCS(I)=0.0    
          TRVSCN(I)=0.0                                                   
          TRIRCN(I)=0.0                                                   
          TRVSCS(I)=0.0                                                   
          TRIRCS(I)=0.0
          ALVSSN(I)=0.0                                                   
          ALIRSN(I)=0.0                                                   
          ALVSG (I)=0.0
          ALIRG (I)=0.0
          TRSNOW(I)=0.0                                                       
  100 CONTINUE
C
C    ----------------- CTEM MODIFICATIONS -------------------------\
C     IF USING DYNAMIC VEGETATION COMPONENT OF CTEM, REPLACE ALBEDOS THAT ARE BASED ON CTEM.
C
      IF(CTEM2)THEN
        DO J = 1, IC
          DO I = IL1, IL2
            ALVSC(I,J)=ALVSCTM(I,J) !ALVISC calculated in bio2str
            ALIRC(I,J)=ALIRCTM(I,J) !ALNIRC calculated in bio2str
          ENDDO
        ENDDO
      ENDIF
C
C    ----------------- CTEM MODIFICATIONS -------------------------/
C
C     * PREPARATION.
C
      CALL APREP(FROOT,FC,FG,FCS,FGS,FRAINC,FSNOWC,FSVF,FSVFS,
     1           RAICAN,RAICNS,SNOCAN,SNOCNS,DISP,DISPS,
     2           CHCAP,CHCAPS,ZOMLNC,ZOMLCS,ZOELNC,ZOELCS,ZOMLNG,
     3           ZOMLNS,ZOELNG,ZOELNS,RCMIN,RCMINS,CMASSC,CMASCS,
     4           CMAI,CWCAP,CWCAPS,ZPLIMC,ZPLIMG,ZPLMCS,ZPLMGS,
     5           HTCC,HTCS,HTC,WTRC,WTRS,WTRG,AIL,AILS,FCAN,FCANS,
     6           FCANMX,ZOLN,AILMAX,AILMIN,CWGTMX,ZRTMAX,THLIQ,THICE,
     7           GROWTH,SAND,CLAY,ORGM,FSNOW,ZSNOW,RHOSNO,SNO,
     8           RCAN,SCAN,TCAN,TSNOW,TBAR,TA,RADJ,ILAND,ILSL,LONSL,
     9           ZORAT,DELZ,DELZW,ZBOTW,
     A           IC,ICP1,IG,ILG,IL1,IL2,IDAY,IDISP,
     B           RMAT,H,HS,CWCPAV,AILCAN,AILCNS,GROWA,GROWN,GROWB,
     C           RRESID,SRESID,ISAND,THPORI,HCPS,N,   
C
C    ----------------- CTEM MODIFICATIONS -------------------------\
     D            FCANCMX,   ICC,   CTEM1,    CTEM2,   RMATC, ZOLNC, 
     E               AILC, AILCG, CMASVEGC, AILCMIN, AILCMAX, L2MAX,
     F           NOL2PFTS,
C    ------------- CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW -----|
     G             AILCGS,  FCANCS, FCANC)
C    ----------------- CTEM MODIFICATIONS -------------------------/
C
C     * CANOPY ALBEDOS AND TRANSMISSIVITIES.
C
      CALL CANALB(ALVSCN,ALIRCN,ALVSCS,ALIRCS,TRVSCN,TRIRCN,
     1            TRVSCS,TRIRCS,
     2            FCAN,FCANS,ALVSC,ALIRC,AIL,AILS,AILMAX,AILMIN,
     3            FC,FCS,FSNOW,FSNOWC,FCLOUD,COSZS,
     4            IC,ICP1,JL,ILG,IL1,IL2)                                                 
C
C     * SNOW ALBEDOS AND TRANSMISSIVITY.
C 
      CALL SNOALBA(ALVSSN,ALIRSN,
     1             ALBSNO,TRSNOW,ZSNOW,FSNOW,SAND,
     2             ALVSI,ALIRI,ILG,IG,IL1,IL2,JL)
C
C     * BARE SOIL ALBEDOS.
C
       CALL GRALB(ALVSG,ALIRG,
     1            SAND,ALVSC(1,5),ALIRC(1,5),
     2            THLIQ,FSNOW,FCANMX(1,5),
     3            ALVSI,ALIRI,DELZW,ILG,IG,IL1,IL2,JL) 
C
C     * EFFECTIVE WHOLE-SURFACE VISIBLE AND NEAR-IR ALBEDOS.
C
      DO 500 I=IL1,IL2
          ALVS(I)=FC(I)*ALVSCN(I)+FG(I)*ALVSG(I)+FCS(I)*ALVSCS(I)+
     1            FGS(I)*ALVSSN(I)                                                
          ALIR(I)=FC(I)*ALIRCN(I)+FG(I)*ALIRG(I)+FCS(I)*ALIRCS(I)+            
     1            FGS(I)*ALIRSN(I)                                                
  500 CONTINUE
C


      RETURN                                                                      
      END
