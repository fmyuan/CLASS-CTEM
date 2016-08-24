      SUBROUTINE    BIO2STR( GLEAFMAS, BLEAFMAS, STEMMASS, ROOTMASS,                        
     1                            ICC,      ILG,      IL1,      IL2,
     2                             IG,       IC,  FCANCMX,    ZBOTW,
     3                          DELZW, NOL2PFTS,    L2MAX,
C    4--------------- INPUTS ABOVE THIS LINE, OUTPUTS BELOW --------
     5                          AILCG,    AILCB,     AILC,    ZOLNC,
     6                          RMATC, RMATCTEM,     SLAI,  BMASVEG,
     7                       CMASVEGC,  VEGHGHT, ROOTDPTH,   ALVISC,
     8                         ALNIRC)
C
C     ----------------------------------------------------------------
C
C           CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) V1.0
C        BIOMASS TO STRUCTURAL ATTRIBUTES CONVERSION SUBROUTINE 
C
C     14  MAR. 2003 - THIS SUBROUTINE CONVERTS LEAF, STEM, AND ROOT BIOMASS 
C     V. ARORA        INTO LAI, VEGETATION HEIGHT, AND FRACTION OF ROOTS
C                     IN EACH SOIL LAYER. STORAGE LAI IS ALSO CALCULATED.
C                     
C                     NOTE THAT WHILE CTEM KEEPS TRACK OF 9 PFTs, CLASS 2.7
C                     KEEPS TRACK OF 4 PFTs, SO ALL THESE VEGETATION 
C                     STRUCTURAL ATTRIBUTES ARE CALCULATED FOR 9 PFTs
C                     SEPATARELY BUT THEN LUMPED INTO 4 PFTs FOR USE IN
C                     ENERGY & WATER BALANCE CALCULATIONS BY CLASS
C               
C                     ALSO, THIS SUBROUTINE DOES NOT ESTIMATE ZOLNC(I,5)
C                     THE LOG OF ROUGHNESS LENGTH OVER THE BARE FRACTION
C                     OF THE GRID CELL. ONLY ROUGHNESS LENGTHS OVER THE
C                     VEGETATED FRACTION ARE UPDATED
C 
C              CTEM 1.0                 CLASS 2.7
C         -----------------            -------------
C        1. NEEDLE LEAF EVG
C        2. NEEDLE LEAF DCD           1. NEEDLE LEAF
C        3. BROAD LEAF EVG
C        4. BROAD LEAF COLD DCD       2. BROAD LEAF
C        5. BROAD LEAF DRY DCD            
C        6. C3 CROP
C        7. C4 CROP                   3. CROP
C        8. C3 GRASS
C        9. C4 GRASS                  4. GRASS
C
C     ----------------------------------------------------------------
C     INPUTS
C
C     GLEAFMAS  - GREEN OR LIVE LEAF MASS IN KG C/M2, FOR THE 9 PFTs
C     BLEAFMAS  - BROWN OR DEAD LEAF MASS IN KG C/M2, FOR THE 9 PFTs
C     STEMMASS  - STEM BIOMASS IN KG C/M2, FOR THE 9 PFTs
C     ROOTMASS  - ROOT BIOMASS IN KG C/M2, FOR THE 9 PFTs
C     FCANCMX   - MAX. FRACTIONAL COVERAGES OF CTEM's 9 PFTs. THIS IS
C                 DIFFERENT FROM FCANC AND FCANCS (WHICH MAY VARY WITH
C                 SNOW DEPTH). FCANCMX DOESN'T CHANGE, UNLESS OF COURSE
C                 ITS CHANGED BY LAND USE CHANGE OR DYNAMIC VEGETATION.
C     DELZW     - THICKNESSES OF THE 3 SOIL LAYERS
C     ZBOTW     - BOTTOM OF SOIL LAYERS
C     ICC       - NO OF PFTs FOR USE BY CTEM, CURRENTLY 9
C     IC        - NO OF PFTs FOR USE BY CLASS, CURRENTLY 4
C     IG        - NO. OF SOIL LAYERS, 3
C     ILG       - NO. OF GRID CELLS IN LATITUDE CIRCLE
C     IL1, IL2  - IL1=1, IL2=ILG
C     NOL2PFTS  - NUMBER OF LEVEL 2 PFTs
C     L2MAX     - MAXIMUM NUMBER OF LEVEL 2 PFTs
C    
C     OUTPUTS
C
C     AILCG     - GREEN LAI FOR CTEM's 9 PFTs
C     AILCB     - BROWN LAI FOR CTEM's 9 PFTs. FOR NOW WE ASSUME ONLY
C                 GRASSES CAN HAVE BROWN LEAVES.
C     AILC      - LAI TO BE USED BY CLASS
C     ZOLNC     - LOG OF ROUGHNESS LENGTH TO BE USED BY CLASS
C     RMATCTEM  - FRACTION OF LIVE ROOTS IN EACH SOIL LAYER FOR EACH OF CTEM's 9 PFTs
C     RMATC     - FRACTION OF LIVE ROOTS IN EACH SOIL LAYER FOR EACH OF THE CLASS' 4 PFTs
C     SLAI      - STORAGE OR IMAGINARY LAI FOR PHENOLOGY PURPOSES
C     BMASVEG   - TOTAL (GLEAF + STEM + ROOT) BIOMASS FOR EACH CTEM PFT, Kg C/M2
C     CMASVEGC  - TOTAL CANOPY MASS FOR EACH OF THE 4 CLASS PFTs. RECALL THAT 
C                 CLASS REQUIRES CANOPY MASS AS AN INPUT, AND THIS IS NOW 
C                 PROVIDED BY CTEM. KG/M2.
C     VEGHGHT   - VEGETATION HEIGHT (METERS)
C     ROOTDPTH  - 99% SOIL ROOTING DEPTH (METERS)
C                 BOTH VEGHGHT & ROOTDPTH CAN BE USED AS DIAGNOSTICS TO SEE
C                 HOW VEGETATION GROWS ABOVE AND BELOW GROUND, RESPECTIVELY.
C     ALVISC    - ALBEDO FOR 4 CLASS PFTs SIMULATED BY CTEM, VISIBLE 
C     ALNIRC    - ALBEDO FOR 4 CLASS PFTs SIMULATED BY CTEM, NEAR IR
C     ----------------------------------------------------------------    
C
      IMPLICIT NONE

      INTEGER IC,  ICC,  ILG, IL1, IL2, IG, I, J, K, M, N, KK, K1, K2
      INTEGER NOL2PFTS(IC), L2MAX, ICOUNT, SORT(ICC)  
      
      PARAMETER (KK=12) ! PRODUCT OF L2MAX AND CLASS' PFTs
  
      REAL  GLEAFMAS(ILG,ICC),  BLEAFMAS(ILG,ICC),  STEMMASS(ILG,ICC),
     1      ROOTMASS(ILG,ICC),     AILCG(ILG,ICC),     AILCB(ILG,ICC),   
     2           AILC(ILG,IC),      ZOLNC(ILG,IC),  
     3       RMATC(ILG,IC,IG),   FCANCMX(ILG,ICC),      DELZW(ILG,IG),
     4          ZBOTW(ILG,IG),                   RMATCTEM(ILG,ICC,IG),
     5          SLAI(ILG,ICC),   BMASVEG(ILG,ICC),   CMASVEGC(ILG,IC),
     6           SAI(ILG,ICC),       SAIC(ILG,IC),   SFCANCMX(ILG,IC),
     7         ALVISC(ILG,IC),     ALNIRC(ILG,IC)

      REAL       LFESPANY(KK),           SLA(ICC),   VEGHGHT(ILG,ICC),
     1      LNRGHLTH(ILG,ICC),   AVEROUGH(ILG,IC),           ABAR(KK),
     2           AVERTMAS(KK),          ALPHA(KK),             B(ICC),
     3      ROOTDPTH(ILG,ICC),  USEALPHA(ILG,ICC),         A(ILG,ICC),
     4          USEB(ILG,ICC),              ZROOT,      SOILDPTH(ILG),
     5           PRCNSLAI(KK),            ETA(KK),          KAPPA(KK),
     6            MINSLAI(KK),        SPECSLA(KK),             KN(KK),
     7           MXRTDPTH(KK),         ALBVIS(KK),         ALBNIR(KK)

      REAL             FCOEFF,               ZERO,           FRACBOFG
C
C
      COMMON /CTEM1/ ETA, KAPPA, KN
      COMMON /CTEM2/ LFESPANY, FRACBOFG, SPECSLA
C     ---------------------------------------------------------------
C     PARAMETERS USED. ALSO NOTE THE STRUCTURE OF PARAMETER VECTORS
C     WHICH CLEARLY SHOWS THE CLASS PFTs (ALONG ROWS) AND CTEM SUB-PFTs
C     (ALONG COLUMNS) 
C
C     NEEDLE LEAF |  EVG       DCD       ---    
C     BROAD LEAF  |  EVG   DCD-CLD   DCD-DRY   
C     CROPS       |   C3        C4       ---  
C     GRASSES     |   C3        C4       --- 
C
C     PARAMETER DETERMINING AVERAGE ROOT PROFILE
      DATA ABAR/4.70, 5.86, 0.00,
     &          3.87, 3.46, 3.97,
     &          3.97, 3.97, 0.00,
     &          5.86, 4.92, 0.00/
C
C     AVERAGE ROOT BIOMASS (KG C/M2) FOR CTEM's 8 PFTs USED FOR
C     ESTIMATING ROOTING PROFILE
      DATA AVERTMAS/1.85, 1.45, 0.00,
     &              2.45, 2.10, 2.10,
     &              0.10, 0.10, 0.00,
     &              0.70, 0.70, 0.00/
C
C     ALPHA, PARAMETER DETERMINING HOW THE ROOTS GROW
      DATA ALPHA/0.80, 0.80, 0.00,
     &           0.80, 0.80, 0.80,
     &           0.80, 0.80, 0.00,
     &           0.80, 0.80, 0.00/
C
C     PRCNSLAI - STORAGE/IMAGINARY LAI IS THIS PERCENTAGE OF MAXIMUM 
C     LEAF AREA INDEX THAT A GIVEN ROOT+STEM BIOMASS CAN SUPOORT 
      DATA PRCNSLAI/7.5, 7.5, 0.0,
     &              7.5, 7.5, 7.5, 
     &              7.5, 7.5, 0.0,
     &              2.5, 2.5, 0.0/
C
C     MINSLAI - MINIMUM STORAGE LAI. THIS IS WHAT THE MODEL USES AS
C     LAI WHEN GROWING VEGETATION FOR SCRATCH. CONSIDER THESE AS MODEL
C     SEEDS.
      DATA MINSLAI/0.3, 0.3, 0.0,
     &             0.3, 0.3, 0.3,
     &             0.2, 0.2, 0.0,
     &             0.2, 0.2, 0.0/
C
C      MAXIMUM ROOTING DEPTH. THIS IS USED SO THAT THE ROOTING DEPTHS
C      SIMULATED BY CTEM's VARIABLE ROOTING DEPTH PARAMETERZATION ARE
C      CONSTRAINED TO REALISTIC VALUES
C
C       DATA MXRTDPTH/3.00, 3.00, 0.00,
C     &               3.75, 3.25, 3.00,
C     &               2.00, 2.00, 0.00,
C     &               0.60, 0.60, 0.00/
      DATA MXRTDPTH/3.00, 3.00, 0.00,  !Vivek modified
     &              5.00, 5.00, 3.00,
     &              2.00, 2.00, 0.00,
     &              1.00, 1.00, 0.00/
C
C     VISIBLE AND NEAR IR ALBEDOS OF THE 9 CTEM PFTs
C
       DATA ALBVIS/3.00, 3.00, 0.00,
     &             3.00, 5.00, 5.00,
     &             5.50, 5.50, 0.00,
     &             5.00, 6.00, 0.00/
C
       DATA ALBNIR/19.0, 19.0, 0.00,
     &             23.0, 29.0, 29.0,
     &             34.0, 34.0, 0.00,
     &             30.0, 34.0, 0.00/
C
      DATA ZERO/1.0E-20/
C
C     ---------------------------------------------------------------
C
      IF(ICC.NE.9)                            CALL XIT('BIO2STR',-1)  
      IF(IC.NE.4)                             CALL XIT('BIO2STR',-2)  
C
C     INITIALIZATION
C
      DO 30 J = 1,ICC
        DO 40 K = 1,IG
          DO 50 I = IL1,IL2
            RMATCTEM(I,J,K)=0.0
50        CONTINUE
40      CONTINUE
30    CONTINUE
C
      DO 31 J = 1,IC
        DO 41 K = 1,IG
          DO 51 I = IL1,IL2
            RMATC(I,J,K)=0.0
51        CONTINUE
41      CONTINUE
31    CONTINUE
C
      ICOUNT=0
      DO 52 J = 1, IC
        DO 53 M = 1, NOL2PFTS(J)
          N = (J-1)*L2MAX + M
          ICOUNT = ICOUNT + 1
          SORT(ICOUNT)=N
53      CONTINUE
52    CONTINUE
C
      DO 60 J = 1,IC
        DO 70 I =  IL1, IL2
          AILC(I,J)= 0.0001  !0.0 HSuo, Jan, 2013	  
          SAIC(I,J)=0.0
          ZOLNC(I,J)=0.0
          AVEROUGH(I,J)=0.0
          ALVISC(I,J)=0.0
          ALNIRC(I,J)=0.0
          CMASVEGC(I,J)=0.0
          SFCANCMX(I,J)=0.0    ! SUM OF FCANCMXs
70      CONTINUE
60    CONTINUE
C
      DO 80 J = 1,ICC
        SLA(J)=0.0
        DO 90 I =  IL1, IL2
          USEALPHA(I,J)=ALPHA(SORT(J))
          USEB(I,J)=0.0
          AILCG(I,J)=0.0001  !0.0 HSuo, Jan, 2013
          AILCB(I,J)=0.0
          VEGHGHT(I,J)=0.0
          LNRGHLTH(I,J)=0.0
          A(I,J)=0.0
          SLAI(I,J)=0.0
          SAI(I,J)=0.0
          BMASVEG(I,J)=0.0
90      CONTINUE
80    CONTINUE
C
C
C     ------ 1. CONVERSION OF LEAF BIOMASS INTO LEAF AREA INDEX -------
C
C     FIND SPECIFIC LEAF AREA (SLA, M2/KG) USING LEAF LIFE SPAN
C
      ICOUNT=0
      DO 100 J = 1, IC
        DO 101 M = 1, NOL2PFTS(J)
          N = (J-1)*L2MAX + M
          ICOUNT = ICOUNT + 1
          SLA(ICOUNT) = 25.0*(LFESPANY(N)**(-0.50))
          IF(SPECSLA(N).GT.ZERO) SLA(ICOUNT)=SPECSLA(N)  
101     CONTINUE
100   CONTINUE
C
C     CONVERT LEAF BIOMASS INTO LAI. BROWN LEAVES COULD HAVE LESS
C     LAI THAN THE GREEN LEAVES FOR THE SAME LEAF MASS. FOR NOW WE
C     ASSUME SLA OF BROWN LEAVES IS FRACBOFG TIMES THAT OF GREEN LEAVES. 
C
C     ALSO FIND STEM AREA INDEX AS A FUNCTION OF STEM BIOMASS
C
      DO 150 J = 1,ICC
        DO 160 I = IL1,IL2
          AILCG(I,J)=SLA(J)*GLEAFMAS(I,J)
          if (AILCG(I,J) .LE. 0.0) AILCG = 0.0001 !HSuo, Dec, 2012
          AILCB(I,J)=SLA(J)*BLEAFMAS(I,J)*FRACBOFG
C         SAI(I,J)=0.55*(1.0-EXP(-0.175*STEMMASS(I,J))) !STEM AREA INDEX
160     CONTINUE
150   CONTINUE
C
C     GET FCANCMX WEIGHTED LEAF AREA INDEX FOR USE BY CLASS 
C       NEEDLE LEAF EVG + DCD = TOTAL NEEDLE LEAF    
C       BROAD LEAF EVG + DCD CLD + DCD DRY = TOTAL BROAD LEAF    
C       CROP C3 + C4 = TOTAL CROP
C       GRASS C3 + C4 = TOTAL GRASS
C     ALSO ADD BROWN LAI. NOTE THAT ALTHOUGH GREEN + BROWN
C     LAI IS TO BE USED BY CLASS FOR ENERGY AND WATER BALANCE
C     CALCULATIONS, STOMATAL CONDUCTANCE ESTIMATED BY THE 
C     PHOTOSYNTHESIS SUBROUTINE IS ONLY BASED ON GREEN LAI.
C     THAT IS ALTHOUGH BOTH GREEN+BROWN LEAVES INTERCEPT
C     WATER AND LIGHT, ONLY THE GREEN PORTION PHOTOSYNTHESIZES.
C
      K1=0
      DO 200 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 210 M = K1, K2
          DO 220 I = IL1, IL2
            SFCANCMX(I,J)=SFCANCMX(I,J)+FCANCMX(I,M)
            AILC(I,J)=AILC(I,J)+(FCANCMX(I,M)*(AILCG(I,M)+AILCB(I,M)))
            SAIC(I,J)=SAIC(I,J)+(FCANCMX(I,M)*SAI(I,M))
220       CONTINUE
210     CONTINUE
200   CONTINUE
C
      DO 230 J = 1, IC
        DO 240 I = IL1, IL2
C
          IF(SFCANCMX(I,J).GT.ZERO)THEN
             AILC(I,J)=AILC(I,J)/SFCANCMX(I,J)
             SAIC(I,J)=SAIC(I,J)/SFCANCMX(I,J)
C            COMMENT THE FOLLOWING LINE FOR NOW. IT APPEARS LEAVING THE
C            STEM AREA INDEX HANGING AROUND IN WINTER CAUSES SOIL
C            TEMPERATURES TO RISE SO MUCH THAT LEAF ONSET IS NO LONGER
C            CONTROLLED BY SOIL TEMPERATURE FOR BDL DCD COLD PFT.
C            AILC(I,J)=MAX(AILC(I,J),SAIC(I,J))
          ELSE
            AILC(I,J)=0.0
            SAIC(I,J)=0.0
          ENDIF
C         FOR CROPS AND GRASSES SET THE MINIMUM LAI TO A SMALL NUMBER, OTHER
C         WISE CLASS WILL NEVER RUN TSOLVC AND THUS PHTSYN AND CTEM WILL NOT
C         BE ABLE TO GROW CROPS OR GRASSES.
          IF(J.EQ.3.OR.J.EQ.4) AILC(I,J)=MAX(AILC(I,J),0.1)
C
240     CONTINUE
230   CONTINUE
C
C     ------ 2. CONVERSION OF STEM BIOMASS INTO ROUGHNESS LENGTH -------
C 
C     CLASS USES LOG OF ROUGHNESS LENGTH (ZOLN) AS AN INPUT PARAMETER. WHEN 
C     VEGETATION GROWS AND DIES AS PER CTEM, THEN ZOLN IS PROVIDED BY CTEM.
C
C     1. CONVERT STEM BIOMASS INTO VEGETATION HEIGHT FOR TREES AND CROPS,
C        AND CONVERT LEAF BIOMASS INTO VEGETATION HEIGHT FOR GRASS
C     2. CONVERT VEGETATION HEIGHT INTO ROUGHNESS LENGTH & TAKE ITS LOG
C     3. LUMP THIS FOR CTEM's 9 PFTs INTO CLASS' 4 PFTs
C
      K1=0
      DO 250 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 260 M = K1, K2
          DO 270 I = IL1, IL2
C          
          IF (J.LE.2) THEN                            ! TREES
           VEGHGHT(I,M)=10.0*STEMMASS(I,M)**0.385
           VEGHGHT(I,M)=MIN(45.0, VEGHGHT(I,M))
          ELSE IF (J.EQ.3) THEN                       ! CROPS
           VEGHGHT(I,M)=1.0*(STEMMASS(I,M)+GLEAFMAS(I,M))**0.385
          ELSE IF (J.EQ.4) THEN                       ! GRASSES
           VEGHGHT(I,M)=3.5*(GLEAFMAS(I,M)+FRACBOFG*BLEAFMAS(I,M))**0.50   
          ENDIF
          LNRGHLTH(I,M)= LOG(0.10 * MAX(VEGHGHT(I,M),0.10))
C
270       CONTINUE
260     CONTINUE
250   CONTINUE
C
      K1=0
      DO 300 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 310 M = K1, K2
          DO 320 I = IL1, IL2
            AVEROUGH(I,J)=AVEROUGH(I,J)+(FCANCMX(I,M)*LNRGHLTH(I,M))
320       CONTINUE
310     CONTINUE
300   CONTINUE
C
      DO 330 J = 1, IC
        DO 340 I = IL1, IL2
C
          IF(SFCANCMX(I,J).GT.ZERO)THEN
             AVEROUGH(I,J)=AVEROUGH(I,J)/SFCANCMX(I,J)
          ELSE
            AVEROUGH(I,J)=-4.605
          ENDIF
          ZOLNC(I,J)=AVEROUGH(I,J)
C
340     CONTINUE
330   CONTINUE
C
C
C     ------ 3. ESTIMATING FRACTION OF ROOTS IN EACH SOIL LAYER FOR -----
C     ------      CTEM's EACH VEGETATION TYPE, USING ROOT BIOMASS   -----
C
C     
C     ESTIMATE PARAMETER B OF VARIABLE ROOT PROFILE PARAMETERIZATION
C
      ICOUNT=0
      DO 350 J = 1, IC
        DO 360 M = 1, NOL2PFTS(J)
          N = (J-1)*L2MAX + M
          ICOUNT = ICOUNT + 1
          B(ICOUNT) = ABAR(N) * (AVERTMAS(N)**ALPHA(N))
360     CONTINUE
350   CONTINUE
C
C     USE B TO ESTIMATE 99% ROOTING DEPTH
C
      K1=0
      DO 370 J = 1,IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 380 M = K1, K2
          DO 390 I = IL1, IL2
C
            USEB(I,M)=B(M)
            USEALPHA(I,M)=ALPHA(SORT(M))
            ROOTDPTH(I,M) = (4.605*(ROOTMASS(I,M)**ALPHA(SORT(M))))/B(M)
C
C           IF ESTIMATED ROOTING DEPTH IS GREATER THAN SOIL DEPTH, OR
C           THE MAXIMUM ROOTING DEPTH THEN ADJUST ROOTING DEPTH AND
C           PARAMETER ALPHA

            SOILDPTH(I)=DELZW(I,1)+DELZW(I,2)+DELZW(I,3)
            IF(ROOTDPTH(I,M).GT.MIN(SOILDPTH(I),MXRTDPTH(SORT(M))))THEN
              ROOTDPTH(I,M) = MIN(SOILDPTH(I),MXRTDPTH(SORT(M)))
              A(I,M)=4.605/ROOTDPTH(I,M)
            ELSE

C             USEALPHA(I,M) = LOG( ROOTDPTH(I,M)*ABAR(SORT(M))/4.605 )
C             USEALPHA(I,M) = USEALPHA(I,M)/
C    &                        LOG( ROOTMASS(I,M)/AVERTMAS(SORT(M)) )    
C             USEB(I,M)=ABAR(SORT(M))*(AVERTMAS(SORT(M))**USEALPHA(I,M))
              IF(ROOTMASS(I,M).EQ.0.0)THEN
                A(I,M)=100.0
              ELSE
                A(I,M)=USEB(I,M)/(ROOTMASS(I,M)**USEALPHA(I,M))
              ENDIF

            ENDIF
C
390       CONTINUE
380     CONTINUE
370   CONTINUE
C
C
C     FIND A (PARAMETER DETERMINING ROOT PROFILE). THIS IS THE A WHICH DEPENDS
C     ON TIME VARYING ROOT BIOMASS 
C
      DO 400 J = 1,ICC
        DO 410 I = IL1, IL2
C
C         USING PARAMETER A WE CAN FIND FRACTION OF ROOTS IN EACH SOIL LAYER
C
          ZROOT=ROOTDPTH(I,J)
          IF(ZROOT.LE.ZBOTW(I,1)) THEN
              RMATCTEM(I,J,1)=1.0
              RMATCTEM(I,J,2)=0.0
              RMATCTEM(I,J,3)=0.0
          ELSE
              FCOEFF=EXP(-A(I,J)*ZROOT)
              RMATCTEM(I,J,1)=
     &          1.0-(EXP(-A(I,J)*ZBOTW(I,1))-FCOEFF)/(1.0-FCOEFF)
              IF(ZROOT.LE.ZBOTW(I,2)) THEN
                  RMATCTEM(I,J,2)=1.0-RMATCTEM(I,J,1)
                  RMATCTEM(I,J,3)=0.0
              ELSE
                  RMATCTEM(I,J,3)=
     &              (EXP(-A(I,J)*ZBOTW(I,2))-FCOEFF)/(1.0-FCOEFF)
                  RMATCTEM(I,J,2)=1.0-RMATCTEM(I,J,1)-RMATCTEM(I,J,3)
              ENDIF
          ENDIF
C
410     CONTINUE
400   CONTINUE
C
C     LUMP RMATCTEM(I,9,3)  INTO RMATC(I,4,3) FOR USE BY CLASS
C

      K1=0
      DO 420 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 430 M = K1, K2
          DO 440 I = IL1, IL2
            RMATC(I,J,1)=RMATC(I,J,1)+(FCANCMX(I,M)*RMATCTEM(I,M,1))
            RMATC(I,J,2)=RMATC(I,J,2)+(FCANCMX(I,M)*RMATCTEM(I,M,2))
            RMATC(I,J,3)=RMATC(I,J,3)+(FCANCMX(I,M)*RMATCTEM(I,M,3))
440       CONTINUE
430     CONTINUE
420   CONTINUE
C
      DO 450 J = 1, IC
        DO 460 I = IL1, IL2
C
          IF(SFCANCMX(I,J).GT.ZERO)THEN
             RMATC(I,J,1)=RMATC(I,J,1)/SFCANCMX(I,J)
             RMATC(I,J,2)=RMATC(I,J,2)/SFCANCMX(I,J)
             RMATC(I,J,3)=RMATC(I,J,3)/SFCANCMX(I,J)
          ELSE
            RMATC(I,J,1)=1.0
            RMATC(I,J,2)=0.0
            RMATC(I,J,3)=0.0
          ENDIF
C
460     CONTINUE
450   CONTINUE
C
C
C     -------------------  4. CALCULATE STORAGE LAI  --------------------
C
      DO 500 J = 1, ICC
        DO 510 I = IL1, IL2
          SLAI(I,J)=((STEMMASS(I,J)+ROOTMASS(I,J))/ETA(SORT(J)))
     &     **(1/KAPPA(SORT(J)))
          SLAI(I,J)=(PRCNSLAI(SORT(J))/100.0)*SLA(J)*SLAI(I,J)
C
C         NEED A MINIMUM SLAI TO BE ABLE TO GROW FROM SCRATCH. CONSIDER THIS AS MODEL SEEDS.
          SLAI(I,J)=MAX(SLAI(I,J),MINSLAI(SORT(J)))
510     CONTINUE
500   CONTINUE
C
C
C     --- 5. CALCULATE TOTAL VEGETATION BIOMASS FOR EACH CTEM PFT, AND --
C     ---------------- CANOPY MASS FOR EACH CLASS PFT ------------------
C
      DO 550 J = 1, ICC
        DO 560 I = IL1, IL2
          BMASVEG(I,J)=GLEAFMAS(I,J)+STEMMASS(I,J)+ROOTMASS(I,J)
560     CONTINUE
550   CONTINUE
C
C     SINCE CLASS USES CANOPY MASS AND NOT TOTAL VEGETATION BIOMASS AS AN
C     INPUT, WE FIND CANOPY MASS AS A SUM OF STEM AND LEAF MASS, FOR EACH
C     CLASS PFT, I.E. ONLY ABOVE GROUND BIOMASS. 
C
      K1=0
      DO 600 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 610 M = K1, K2
          DO 620 I = IL1, IL2
            CMASVEGC(I,J)= CMASVEGC(I,J) +
     &      (FCANCMX(I,M)*(BLEAFMAS(I,M)+GLEAFMAS(I,M)+STEMMASS(I,M)))  
620       CONTINUE
610     CONTINUE
600   CONTINUE
C
      DO 630 J = 1, IC
        DO 640 I = IL1, IL2
C
          IF(SFCANCMX(I,J).GT.ZERO)THEN
            CMASVEGC(I,J)=CMASVEGC(I,J)/SFCANCMX(I,J)
            CMASVEGC(I,J)=CMASVEGC(I,J)*(1.0/0.50) !ASSUMING BIOMASS IS 50% C
          ELSE
            CMASVEGC(I,J)=0.0
          ENDIF
C       
C         IF THERE IS NO VEGETATION CANOPY MASS WILL BE ZERO. THIS SHOULD 
C         ESSENTIALLY MEAN MORE BARE GROUND, BUT SINCE WE ARE NOT CHANGING
C         FRACTIONAL COVERAGES AT PRESENT, WE PASS A MINIMUM CANOPY MASS
C         TO CLASS SO THAT IT DOESN'T RUN INTO NUMERICAL PROBLEMS.
C
          CMASVEGC(I,J)=MAX(CMASVEGC(I,J),3.0)
C
640     CONTINUE
630   CONTINUE
C
C     --- 6. CALCULATE ALBEDO FOR CLASS' 4 PFTs BASED ON SPECIFIED ----
C     ------ ALBEDOS OF CTEM 9 PFTs AND THEIR FRACTIONAL COVERAES -----
C
      K1=0
      DO 700 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 710 M = K1, K2
          DO 720 I = IL1, IL2
            ALVISC(I,J)= ALVISC(I,J) + (FCANCMX(I,M)*ALBVIS(SORT(M)))  
            ALNIRC(I,J)= ALNIRC(I,J) + (FCANCMX(I,M)*ALBNIR(SORT(M)))  
720       CONTINUE
710     CONTINUE
700   CONTINUE
C
      DO 730 J = 1, IC
        DO 740 I = IL1, IL2

          IF(SFCANCMX(I,J).GT.ZERO)THEN
            ALVISC(I,J)=(ALVISC(I,J)/SFCANCMX(I,J))/100.0
            ALNIRC(I,J)=(ALNIRC(I,J)/SFCANCMX(I,J))/100.0
          ELSE
            ALVISC(I,J)=0.0
            ALNIRC(I,J)=0.0
          ENDIF

740     CONTINUE
730   CONTINUE

      RETURN
      END


