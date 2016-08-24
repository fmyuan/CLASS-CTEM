      SUBROUTINE MAINRES (  FCAN,      FCT,     STEMMASS,   ROOTMASS,    
     1                       ICC,       IG,          ILG,        IL1,
     2                       IL2,     TCAN,         TBAR,   RMATCTEM,
     3                      SORT, NOL2PFTS,           IC,
C    -------------- INPUTS ABOVE THIS LINE, OUTPUTS BELOW ----------
     4                      RMSVEG, RMRVEG,     ROOTTEMP, !)
C    ---------------LEAF MAINTENANCE RESP.(f.m. YUAN) --------------
     5                      AILVEG,   NRUB,        RMLVEG)
C
C
C               CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) V1.0
C                    MAINTENANCE RESPIRATION SUBTOUTINE
C
C     20  SEP. 2001 - THIS SUBROUTINE CALCULATES MAINTENANCE RESPIRATION,
C     V. ARORA        OVER A GIVEN SUB-AREA, FOR STEM AND ROOT COMPONENTS.
C                     LEAF RESPIRATION IS ESTIMATED WITHIN THE PHTSYN
C                     SUBROUTINE.
C
C     INPUTS 
C
C     FCAN      - FRACTIONAL COVERAGE OF CTEM's 9 PFTs OVER THE GIVEN
C                 SUB-AREA
C     FCT       - SUM OF ALL FCAN
C                 FCAN & FCT ARE NOT USED AT THIS TIME BUT COULD
C                 BE USED AT SOME LATER STAGE
C     STEMMASS  - STEM BIOMASS FOR THE 9 PFTs IN KG C/M2
C     ROOTMASS  - ROOT BIOMASS FOR THE 9 PFTs IN KG C/M2
C     ICC       - NO. OF CTEM PFTs (CURRENTLY 9)
C     IG        - NO. OF SOIL LAYERS (CURRENTLY 3)
C     ILG       - NO. OF GRID CELLS IN LATITUDE CIRCLE
C     IL1,IL2   - IL1=1, IL2=ILG
C     TCAN      - CANOPY TEMPERATURE, K
C     TBAR      - SOIL TEMPERATURE, K
C     RMATCTEM  - FRACTION OF ROOTS IN EACH LAYER FOR EACH PFT
C     SORT      - INDEX FOR CORRESPONDENCE BETWEEN 9 PFTs AND 12 VALUES
C                 IN THE PARAMETER VECTORS
C     NOL2PFTS  - NUMBER OF LEVEL 2 CTEM PFTs
C     IC        - NUMBER OF CLASS PFTs, CURRENTLY 4
C
C     OUTPUTS 
C
C     RMSVEG    - MAINTENANCE RESPIRATION FOR STEM FOR THE 9 PFTs
C     RMRVEG    - MAINTENANCE RESPIRATION FOR ROOT FOR THE 9 PFTs
C                 BOTH in u MOL CO2/M2. SEC
C     ROOTTEMP  - ROOT TEMPERATURE
C
      IMPLICIT NONE
C
      INTEGER ILG,  ICC,   IG,  IL1, IL2, I, J, K, SORT(ICC), KK, 
     1        K1,   K2,    IC,  M,   NOL2PFTS(IC)   
C
      PARAMETER(KK=12) ! PRODUCT OF CLASS PFTs AND L2MAX (4 x 3 = 12)
C
      REAL  FCAN(ILG,ICC),         FCT(ILG),      STEMMASS(ILG,ICC), 
     1          TCAN(ILG),     TBAR(ILG,IG),      ROOTMASS(ILG,ICC),   
     2    RMSVEG(ILG,ICC),  RMRVEG(ILG,ICC),   RMATCTEM(ILG,ICC,IG)   
C
      REAL  BSRTSTEM(KK),      BSRTROOT(KK),      TEMPQ10R(ILG,ICC), 
     1     TEMPQ10S(ILG), ROOTTEMP(ILG,ICC),                    Q10, 
     2           Q10FUNC,              ZERO,      LIVSTMFR(ILG,ICC),
     3 LIVROTFR(ILG,ICC),           MINLVFR
C
      LOGICAL CONSQ10

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (f.m.YUAN)
      REAL  AILVEG(ILG,ICC),  NRUB(ILG,ICC),      
     1      RMLVEG(ILG,ICC),   BSRTLEAF(KK)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (f.m.YUAN)
C
C     CONSTANTS AND PARAMETERS 
C
C     NOTE THE STRUCTURE OF VECTORS WHICH CLEARLY SHOWS THE CLASS
C     PFTs (ALONG ROWS) AND CTEM SUB-PFTs (ALONG COLUMNS)
C
C     NEEDLE LEAF |  EVG       DCD       ---
C     BROAD LEAF  |  EVG   DCD-CLD   DCD-DRY
C     CROPS       |   C3        C4       ---
C     GRASSES     |   C3        C4       ---
C
C     ---------------------------------------------------
C
C     BASE RESPIRATION RATES FOR STEM AND ROOT FOR CTEM PFTs IN
C     KG C/KG C.YEAR AT 15 DEGREES CELCIUS. NOTE THAT MAINTENANCE
C     RESPIRATION RATES OF ROOT ARE HIGHER BECAUSE THEY CONTAIN
C     BOTH WOOD (COARSE ROOTS) AND FINE ROOTS.
C
!     &              0.0365, 0.0365, 0.0000,
     DATA BSRTSTEM/0.0900, 0.0550, 0.0000, 
     &             0.0600, 0.0335, 0.0300, 
     &             0.0365, 0.0365, 0.0000, 
     &             0.0000, 0.0000, 0.0000/     

!     &              0.1600, 0.1600, 0.0000,
     DATA BSRTROOT/0.5000, 0.2850, 0.0000,   
     &             0.6500, 0.2250, 0.0550, 
     &             0.1600, 0.1600, 0.0000, 
     &             0.1000, 0.1000, 0.0000/  
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (f.m.YUAN)
C	BASE RESPIRATION RATES FOR GREEN LEAF (kgC/yr/<gNrub/m2>)
!      DATA BSRTLEAF/0.0030, 0.0030, 0.0000,  ! IF need TO BE IGNORED, SET THEM TO ZEROS
     &             0.3030, 0.3030, 0.0030,
     &             0.0001, 0.0001, 0.0000,
     &             0.0001, 0.0030, 0.0000/
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (f.m.YUAN)
C
C     SET THE FOLLOWING SWITCH TO .TRUE. FOR USING CONSTANT TEMPERATURE
C     INDEPEDENT Q10 SPECIFIED BELOW
      DATA CONSQ10 /.FALSE./
C
C     Q10 - IF USING A CONSTANT TEMPERATURE INDEPENDENT VALUE, I.E.
C     IF CONSQ10 IS SET TO TRUE
      DATA Q10/2.00/
C
C     MINIMUM LIVE WOOD FRACTION
      DATA MINLVFR/0.05/
C
C     ZERO
      DATA ZERO/1E-20/
C
C     ---------------------------------------------------
C
C     INITIALIZE REQUIRED ARRAYS TO ZERO
C
      DO 100 J = 1, ICC
        DO 110 I = IL1, IL2
          ROOTTEMP(I,J) = 0.0        ! ROOT TEMPERATURE
          RMSVEG(I,J) = 0.0          ! STEM MAINTENANCE RESPIRATION
          RMRVEG(I,J) = 0.0          ! ROOT MAINTENANCE RESPIRATION
          LIVSTMFR(I,J)= 0.0         ! LIVE STEM FRACTION
          LIVROTFR(I,J)= 0.0         ! LIVE ROOT FRACTION
110     CONTINUE 
100   CONTINUE 
C
C     INITIALIZATION ENDS
C
C     BASED ON ROOT AND STEM BIOMASS, FIND FRACTION WHICH IS LIVE.
C     FOR STEM THIS WOULD BE THE SAPWOOD TO TOTAL WOOD RATIO.
C
      K1=0
      DO 120 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 125 M = K1, K2
         DO 130 I = IL1, IL2
          IF(J.LE.2)THEN     ! TREES
            LIVSTMFR(I,M) = EXP(-0.2835*STEMMASS(I,M))  !FOLLOWING CENTURY MODEL              
            LIVSTMFR(I,M) = MAX(MINLVFR,MIN(LIVSTMFR(I,M),1.0))
            LIVROTFR(I,M) = EXP(-0.2835*ROOTMASS(I,M))               
            LIVROTFR(I,M) = MAX(MINLVFR,MIN(LIVROTFR(I,M),1.0))
          ELSE                 ! CROP AND GRASS ARE ALL LIVE
            LIVSTMFR(I,M) = 1.0
            LIVROTFR(I,M) = 1.0
          ENDIF
130     CONTINUE 
125    CONTINUE 
120   CONTINUE 
C
C     FRACTION OF ROOTS FOR EACH VEGETATION TYPE, FOR EACH SOIL LAYER, 
C     IN EACH GRID CELL IS GIVEN BY RMATCTEM (grid cell, veg type, soil layer) 
C     WHICH BIO2STR SUBROUTINE CALCULATES. RMATCTEM CAN THUS BE USED 
C     TO FIND AVERAGE ROOT TEMPERATURE FOR EACH PLANT FUNCTIONAL TYPE 
C
      DO 180 J = 1, ICC
        DO 190 I = IL1, IL2
          ROOTTEMP(I,J)=TBAR(I,1)*RMATCTEM(I,J,1) + 
     &       TBAR(I,2)*RMATCTEM(I,J,2) +  
     &       TBAR(I,3)*RMATCTEM(I,J,3)
          ROOTTEMP(I,J)=ROOTTEMP(I,J) /
     &       (RMATCTEM(I,J,1)+RMATCTEM(I,J,2)+RMATCTEM(I,J,3))
190     CONTINUE 
180   CONTINUE 
C
C     WE ASSUME THAT STEM TEMPERATURE IS SAME AS CANOPY TEMPERATURE TCAN.
C     USING STEM AND ROOT TEMPERATURES WE CAN FIND THEIR MAINTENANCE 
C     RESPIRATIONS RATES
C
      DO 210 J = 1, ICC
        DO 200 I = IL1, IL2
C
C         FIRST FIND THE Q10 RESPONSE FUNCTION TO SCALE BASE RESPIRATION
C         RATE FROM 15 C TO CURRENT TEMPERATURE, WE DO THE STEM FIRST.
C
          IF (.NOT.CONSQ10) THEN
C           WHEN FINDING TEMPERATURE DEPENDENT Q10, USE TEMPERATURE WHICH
C           IS CLOSE TO AVERAGE OF ACTUAL TEMPERATURE AND THE TEMPERATURE
C           AT WHICH BASE RATE IS SPECIFIED
            TEMPQ10S(I)=(15.0+273.16+TCAN(I))/1.9
            Q10 = 3.22 - 0.046*(TEMPQ10S(I)-273.16)       
            Q10 = MIN(4.0, MAX(1.5, Q10))
          ENDIF
C
          Q10FUNC = Q10**(0.1*(TCAN(I)-288.16))
C
          RMSVEG(I,J)=STEMMASS(I,J)* LIVSTMFR(I,J)* Q10FUNC*
     &     (BSRTSTEM(SORT(J))/365.0)
C
C         CONVERT Kg C/M2.DAY -> u MOL CO2/M2.SEC
          RMSVEG(I,J)= RMSVEG(I,J) * 963.62
C
C         ROOT RESPIRATION
C    
          IF (.NOT.CONSQ10) THEN
            TEMPQ10R(I,J)=(15.0+273.16+ROOTTEMP(I,J))/1.9
            Q10 = 3.22 - 0.046*(TEMPQ10R(I,J)-273.16)       
            Q10 = MIN(4.0, MAX(1.5, Q10))
          ENDIF
C
          Q10FUNC = Q10**(0.1*(ROOTTEMP(I,J)-288.16))
          RMRVEG(I,J)=ROOTMASS(I,J)* LIVROTFR(I,J)* Q10FUNC*
     &     (BSRTROOT(SORT(J))/365.0)
C
C         CONVERT Kg C/M2.DAY -> u MOL CO2/M2.SEC
          RMRVEG(I,J)= RMRVEG(I,J) * 963.62 
C
200     CONTINUE 
210   CONTINUE 
C     
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (f.m.YUAN)
C	MAINTANENCE RESPIRATION FOR GREEN LEAF
C
      DO 220 J = 1, ICC
        DO 230 I = IL1, IL2
C
C         FIRST FIND THE Q10 RESPONSE FUNCTION TO SCALE BASE RESPIRATION
C         RATE FROM 15 C TO CURRENT TEMPERATURE, WE DO THE STEM FIRST.
C
          IF (.NOT.CONSQ10) THEN
C           WHEN FINDING TEMPERATURE DEPENDENT Q10, USE TEMPERATURE WHICH
C           IS CLOSE TO AVERAGE OF ACTUAL TEMPERATURE AND THE TEMPERATURE
C           AT WHICH BASE RATE IS SPECIFIED
            TEMPQ10S(I)=(15.0+273.16+TCAN(I))/1.9
            Q10 = 3.22 - 0.046*(TEMPQ10S(I)-273.16)       
            Q10 = MIN(4.0, MAX(1.5, Q10))
          ENDIF
C
          Q10FUNC = Q10**(0.1*(TCAN(I)-288.16))
C
          RMLVEG(I,J) = NRUB(I,J)/AILVEG(I,J)         ! CONVERT NRUB from ground-based to LAI-based
     1      *Q10FUNC*(BSRTLEAF(SORT(J))/365.0)
     2                  * 963.62	                    ! CONVERT Kg C/M2.DAY -> u MOL CO2/M2.SEC
C
230     CONTINUE 
220   CONTINUE 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (f.m.YUAN)
C     
      RETURN
      END

