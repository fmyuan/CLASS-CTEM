      SUBROUTINE TURNOVER (STEMMASS, ROOTMASS,  LFSTATUS,    AILCG,
     1                          ICC,      ILG,       IL1,      IL2,   
     2                         SORT, NOL2PFTS,        IC,
C    3 ------------------ INPUTS ABOVE THIS LINE ----------------------   
     4                     STMHRLOS, ROTHRLOS,
C    5 ----------- INPUTS WHICH ARE UPDATED ABOVE THIS LINE -----------
     6                     STEMLITR, ROOTLITR)
C    7 ------------------OUTPUTS ABOVE THIS LINE ----------------------
C
C               CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) V1.0
C                       STEM AND ROOT TURONOVER SUBROUTINE
C
C     07  MAY 2003  - THIS SUBROUTINE CALCULATES THE LITTER GENERATED
C     V. ARORA        FROM STEM AND ROOT TURNOVER
C
C     INPUTS 
C
C     STEMMASS  - STEM MASS FOR EACH OF THE 9 CTEM PFTs, Kg C/M2
C     ROOTMASS  - ROOT MASS FOR EACH OF THE 9 CTEM PFTs, Kg C/M2
C     LFSTATUS  - LEAF STATUS. AN INTEGER INDICATING IF LEAVES ARE  
C                 IN "MAX. GROWTH", "NORMAL GROWTH", "FALL/HARVEST", 
C                 OR "NO LEAVES" MODE. SEE PHENOLGY SUBROUTINE FOR 
C                 MORE DETAILS.
C     AILCG     - GREEN OR LIVE LAI
C     ICC       - NO. OF CTEM PLANT FUNCTION TYPES, CURRENTLY 9
C     ILG       - NO. OF GRID CELLS IN LATITUDE CIRCLE
C     IL1,IL2   - IL1=1, IL2=ILG
C     SORT      - INDEX FOR CORRESPONDENCE BETWEEN 9 CTEM PFTs AND
C                 SIZE 12 OF PARAMETER VECTORS
C     NOL2PFTS  - NUMBER OF LEVEL 2 CTEM PFTs
C     IC        - NUMBER OF CLASS PFTs
C
C     UPDATES
C
C     STMHRLOS  - STEM HARVEST LOSS FOR CROPS. WHEN IN "HARVEST" 
C                 MODE FOR CROPS, STEM IS ALSO ASSUMED TO BE
C                 HARVESTED AND THIS GENERATES LITTER.
C     ROTHRLOS  - ROOT DEATH FOR CROPS. WHEN IN "HARVEST" 
C                 MODE FOR CROPS, ROOT IS ASSUMED TO DIE IN A
C                 SIMILAR WAY AS STEM IS HARVESTED.
C
C     OUTPUTS
C
C     STEMLITR  - STEM LITTER (Kg C/M2)
C     ROOTLITR  - ROOT LITTER (Kg C/M2)
C
      IMPLICIT NONE
C
      INTEGER ILG, ICC, IL1, IL2, I, J, K, LFSTATUS(ILG,ICC), KK, N,
     1          M,  K1,  K2,  IC
C
      PARAMETER (KK=12)  ! PRODUCT OF CLASS PFTs AND L2MAX
C
      INTEGER       SORT(ICC),      NOL2PFTS(IC)
C
      REAL  STEMMASS(ILG,ICC), ROOTMASS(ILG,ICC),    AILCG(ILG,ICC)
C
      REAL  STEMLITR(ILG,ICC), ROOTLITR(ILG,ICC), NRMLSMLR(ILG,ICC),
     1      NRMLRTLR(ILG,ICC), ROTHRLOS(ILG,ICC), STMHRLOS(ILG,ICC)
C
      REAL       STEMLIFE(KK),      ROOTLIFE(KK),             ZERO,
     1               STMHRSPN
C
C     ------------------------------------------------------------------
C
C     PARAMETERS USED. ALSO NOTE THE STRUCTURE OF PARAMETER VECTORS
C     WHICH CLEARLY SHOWS THE CLASS PFTs (ALONG ROWS) AND CTEM
C     SUB-PFTs (ALONG COLUMNS)
C
C     NEEDLE LEAF |  EVG       DCD       ---
C     BROAD LEAF  |  EVG   DCD-CLD   DCD-DRY
C     CROPS       |   C3        C4       ---
C     GRASSES     |   C3        C4       ---
C
C     STEMLIFE, TURNOVER TIME SCALE FOR STEM FOR DIFFERENT PFTs
      DATA  STEMLIFE/60.0, 50.0, 0.00,
C      DATA  STEMLIFE/20.0, 50.0, 0.00,		   ! F.Yuan: TP89
     &               35.0, 35.0, 35.0,
     &               20.0, 20.0, 0.00,
     &               0.00, 0.00, 0.00/
C
C     ROOTLIFE, TURNOVER TIME SCALE FOR ROOT FOR DIFFERENT PFTs
      DATA  ROOTLIFE/8.5, 7.5, 0.0,
C      DATA  ROOTLIFE/5.0, 7.5, 0.0,              ! F. Yuan: TP89
     &               4.5, 4.5, 4.5,
     &               3.0, 3.0, 0.0,
     &               2.5, 2.5, 0.0/
C
C     STEM HARVEST SPAN. SAME AS CROP HARVEST SPAN. PERIOD IN DAYS
C     OVER WHICH CROPS ARE HARVESTED. 
      DATA STMHRSPN/17.0/
C
C     ZERO
      DATA ZERO/1E-20/
C
C     ---------------------------------------------------------------
C
      IF(ICC.NE.9)                            CALL XIT('TURNOVER',-1)
C
C     INITIALIZE REQUIRED ARRAYS TO ZERO
C
      DO 140 J = 1,ICC
        DO 150 I = IL1, IL2
          STEMLITR(I,J)=0.0          ! TOTAL STEM LITTER
          ROOTLITR(I,J)=0.0          ! TOTAL ROOT LITTER
          NRMLSMLR(I,J)=0.0          ! STEM LITTER FROM NORMAL TURNOVER
          NRMLRTLR(I,J)=0.0          ! ROOT LITTER FROM NORMAL TURNOVER
150     CONTINUE                  
140   CONTINUE
C
C     INITIALIZATION ENDS    
C
C     ------------------------------------------------------------------
C
C     CALCULATE NORMAL STEM AND ROOT LITTER USING THE AMOUNT OF STEM AND
C     ROOT BIOMASS AND THEIR TURNOVER TIME SCALES.
C
      DO 200 J = 1, ICC
       N = SORT(J)
       DO 210 I = IL1, IL2
        IF(STEMLIFE(N).GT.ZERO)THEN
          NRMLSMLR(I,J)=STEMMASS(I,J)*(1.0-EXP(-1.0/(365*STEMLIFE(N))))  
        ENDIF
        IF(ROOTLIFE(N).GT.ZERO)THEN
          NRMLRTLR(I,J)=ROOTMASS(I,J)*(1.0-EXP(-1.0/(365*ROOTLIFE(N))))  
        ENDIF
210    CONTINUE
200   CONTINUE
C
C     IF CROPS ARE IN HARVEST MODE THEN WE START HARVESTING STEM AS WELL.
C     IF STEM HAS ALREADY BEEN HARVESTED THEN WE SET THE STEM HARVEST
C     LOSS EQUAL TO ZERO. THE ROOTS OF THE CROP DIE IN A SIMILAR WAY.
C     
      K1=0
      DO 250 J = 1, IC
       IF(J.EQ.1) THEN
         K1 = K1 + 1
       ELSE
         K1 = K1 + NOL2PFTS(J-1)
       ENDIF
       K2 = K1 + NOL2PFTS(J) - 1
       DO 255 M = K1, K2
        DO 260 I = IL1, IL2
          IF(J.EQ.3)THEN     !STEM/ROOT HARVEST/DEATH FOR CROPS
C
            IF(LFSTATUS(I,M).EQ.3.AND.STMHRLOS(I,M).LE.ZERO.AND.
     &      STEMMASS(I,M).GT.ZERO)THEN          
              STMHRLOS(I,M)=STEMMASS(I,M)*(1.0/STMHRSPN)
            ENDIF
C
            IF(LFSTATUS(I,M).EQ.3.AND.ROTHRLOS(I,M).LE.ZERO.AND.
     &      ROOTMASS(I,M).GT.ZERO)THEN          
              ROTHRLOS(I,M)=ROOTMASS(I,M)*(1.0/STMHRSPN)   
            ENDIF
C
            IF(STEMMASS(I,M).LE.ZERO.OR.LFSTATUS(I,M).EQ.1.OR.
     &      LFSTATUS(I,M).EQ.2)THEN
              STMHRLOS(I,M)=0.0
            ENDIF
C
            IF(ROOTMASS(I,M).LE.ZERO.OR.LFSTATUS(I,M).EQ.1.OR.
     &      LFSTATUS(I,M).EQ.2)THEN
              ROTHRLOS(I,M)=0.0
            ENDIF
C
          ELSE
            STMHRLOS(I,M)=0.0
            ROTHRLOS(I,M)=0.0
          ENDIF
260     CONTINUE
255    CONTINUE   
250   CONTINUE   
C
C     ADD STEM AND ROOT LITTER FROM ALL SOURCES
C
      DO 350 J = 1, ICC
        DO 360 I = IL1, IL2
          STEMLITR(I,J)=NRMLSMLR(I,J)+STMHRLOS(I,J)
          ROOTLITR(I,J)=NRMLRTLR(I,J)+ROTHRLOS(I,J)
360     CONTINUE
350   CONTINUE
C
      RETURN
      END

