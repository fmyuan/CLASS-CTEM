      SUBROUTINE APREP(FROOT,FC,FG,FCS,FGS,FRAINC,FSNOWC,FSVF,FSVFS,              
     1            RAICAN,RAICNS,SNOCAN,SNOCNS,DISP,DISPS,                         
     2            CHCAP,CHCAPS,ZOMLNC,ZOMLCS,ZOELNC,ZOELCS,ZOMLNG,                
     3            ZOMLNS,ZOELNG,ZOELNS,RCMIN,RCMINS,CMASSC,CMASCS,                
     4            CMAI,CWCAP,CWCAPS,ZPLIMC,ZPLIMG,ZPLMCS,ZPLMGS,
     5            HTCC,HTCS,HTC,WTRC,WTRS,WTRG,AIL,AILS,FCAN,FCANS,
     6            FCANMX,ZOLN,AILMAX,AILMIN,CWGTMX,ZRTMAX,THLIQ,THICE,
     7            GROWTH,SAND,CLAY,ORGM,FSNOW,ZSNOW,RHOSNO,SNO,
     8            RCAN,SCAN,TCAN,TSNOW,TBAR,TA,RADJ,ILAND,ILSL,LONSL,
     9            ZORAT,DELZ,DELZW,ZBOTW,
     A            IC,ICP1,IG,ILG,IL1,IL2,IDAY,IDISP,
     B            RMAT,H,HS,CWCPAV,AILCAN,AILCNS,GROWA,GROWN,GROWB,               
     C            RRESID,SRESID,ISAND,THPORI,HCPS,N,    
C
C    ----------------- CTEM MODIFICATIONS ------------------------\
C
     D            FCANCMX,    ICC,    CTEM1,   CTEM2,    RMATC, ZOLNC, 
     E               AILC,  AILCG, CMASVEGC, AILCMIN,  AILCMAX, L2MAX,
     F           NOL2PFTS,
C    ------------- CTEM INPUTS ABOVE THIS LINE, OUTPUTS BELOW -----|
     G             AILCGS,  FCANCS, FCANC)

C
C     * CTEM1  - LOGICAL BOOLEAN FOR USING CTEM's STOMATAL RESISTANCE
C                OR NOT
C     * CTEM2  - LOGICAL BOOLEAN FOR USING CTEM's STRUCTUTAL
C                ATTRIBUTES OR NOT
C     * AILCG  - GREEN LAI FOR USE WITH PHTSYN SUBROUTINE
C     * AILCGS - GREEN LAI FOR CANOPY OVER SNOW SUB-AREA
C     * AILCMIN- MIN. LAI FOR CTEM PFTs
C     * AILCMAX- MAX. LAI FOR CTEM PFTs
C     * L2MAX  - MAXIMUM NUMBER OF LEVEL 2 CTEM PFTs
C     * NOL2PFTS - NUMBER OF LEVEL 2 CTEM PFTs
C     * FCANC  - FRACTION OF CANOPY OVER GROUND FOR CTEM's 9 PFTs
C     * FCANCS - FRACTION OF CANOPY OVER SNOW FOR CTEM's 9 PFTs
C     * SEE BIO2STR SUBROUTINE FOR EXPLANATION OF OTHER CTEM VARIABLES
C
C    ----------------- CTEM MODIFICATIONS ------------------------/
C
C     * MAR 19/03 - V.ARORA     MODIFICATIONS MADE SO THAT LEAF AREA
C     *                         INDEX, ROUGHNLESS LENGTH, AND FRACTION
C     *                         OF ROOTS IN EACH SOIL LAYER CALCULATED
C     *                         BY CTEM (V1.0) CAN BE USED. ALL 
C     *                         MODIFICATIONS ARE CLEARLY MARKED.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * OCT 11/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUG FIX: TO AVOID ROUND-OFF ERRORS,
C     *                         SET CANOPY COVER EQUAL TO 1 IF THE
C     *                         CALCULATED SUM OF FC AND FCS IS
C     *                         VERY CLOSE TO 1.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     *                         ALSO CORRECT BUG IN CALCULATION OF
C     *                         DEGLON, AND USE IDISP TO DETERMINE
C     *                         METHOD OF CALCULATING DISP AND DISPS.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 16/95 - D.VERSEGHY. THREE NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * NOV 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         RATIONALIZE CALCULATION OF RCMIN. 
C     * NOV 12/94 - D.VERSEGHY. FIX BUGS IN SENESCING LIMB OF CROP
C     *                         GROWTH INDEX AND IN CANOPY MASS
C     *                         CALCULATION.
C     * MAY 06/93 - M.LAZARE/D.VERSEGHY. CLASS - VERSION 2.1.
C     *                                  USE NEW "CANEXT" CANOPY 
C     *                                  EXTINCTION ARRAY TO DEFINE
C     *                                  SKY-VIEW FACTORS. ALSO, CORRECT
C     *                                  MINOR BUG WHERE HAD "IF(IN.LE.9)..."
C     *                                  INSTEAD OF "IF(IN.GT.9)...".  
C     * DEC 12/92 - M.LAZARE.   MODIFIED FOR MULTIPLE LATITUDES.
C     * OCT 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATION OF LAND SURFACE CANOPY 
C     *                         PARAMETERS.
C                                                                                 
C     * OUTPUT ARRAYS USED ELSEWHERE IN CLASS.                                    
C                                                                                 
      REAL FC    (ILG),     FG    (ILG),     FCS   (ILG),   FGS   (ILG),          
     1     FRAINC(ILG),     FSNOWC(ILG),     FSVF  (ILG),   FSVFS (ILG),          
     2     RAICAN(ILG),     RAICNS(ILG),     SNOCAN(ILG),   SNOCNS(ILG),          
     3     DISP  (ILG),     DISPS (ILG),     CHCAP (ILG),   CHCAPS(ILG),          
     4     ZOMLNC(ILG),     ZOMLCS(ILG),     ZOELNC(ILG),   ZOELCS(ILG),          
     5     ZOMLNG(ILG),     ZOMLNS(ILG),     ZOELNG(ILG),   ZOELNS(ILG),          
     6     RCMIN (ILG),     RCMINS(ILG),     CMASSC(ILG),   CMASCS(ILG),          
     7     CWCAP (ILG),     CWCAPS(ILG),     ZPLIMC(ILG),   ZPLIMG(ILG),
     8     ZPLMCS(ILG),     ZPLMGS(ILG),     HTCC  (ILG),   HTCS  (ILG),
     9     WTRC  (ILG),     WTRS  (ILG),     WTRG  (ILG),   CMAI  (ILG)
C                                                                                 
      REAL FROOT (ILG,IG),  HTC   (ILG,IG)
C                                                                                 
C     * OUTPUT ARRAYS ONLY USED ELSEWHERE IN CLASSA.                              
C                                                                                 
      REAL AIL   (ILG,IC),  AILS  (ILG,IC),  FCAN  (ILG,IC),                      
     1     FCANS (ILG,IC)                                                         
C                                                                                 
C    ----------------- CTEM MODIFICATIONS ------------------------\
C
      REAL     AILCG(ILG,ICC),       AILC(ILG,IC),    AILCGS(ILG,ICC),
     1       RMATC(ILG,IC,IG),   FCANCMX(ILG,ICC),     FCANC(ILG,ICC),    
     2        FCANCS(ILG,ICC),      ZOLNC(ILG,IC),   CMASVEGC(ILG,IC),
     3       AILCMIN(ILG,ICC),   AILCMAX(ILG,ICC),   SFCANCMX(ILG,IC)
C
      INTEGER ICC, M, N, K1, K2, L2MAX, NOL2PFTS(IC)
C
      LOGICAL CTEM1, CTEM2
C
C    ----------------- CTEM MODIFICATIONS ------------------------/
C
C     * INPUT ARRAYS DEPENDENT ON LONGITUDE.                                      
C                                                                                 
      REAL FCANMX(ILG,ICP1),                 ZOLN  (ILG,ICP1),                    
     1     AILMAX(ILG,IC),  AILMIN(ILG,IC),  CWGTMX(ILG,IC),                      
     2     ZRTMAX(ILG,IC),  THLIQ (ILG,IG),  THICE (ILG,IG),
     3     TBAR  (ILG,IG), 
     4     SAND  (ILG,IG),  CLAY  (ILG,IG),  ORGM  (ILG,IG) 
C                                                                                 
      REAL GROWTH(ILG),     FSNOW (ILG),     ZSNOW (ILG),          
     1     RHOSNO(ILG),     SNO   (ILG),     
     2     RCAN  (ILG),     SCAN  (ILG),
     3     TCAN  (ILG),     TSNOW (ILG),     TA    (ILG)

      REAL*8 RADJ(ILG) 
C
      INTEGER ILAND (ILG),  ILSL  (ILG)            
C                                                                                 
C     * BACKGROUND ARRAYS - INDEPENDENT OF LONGITUDE.                             
C                                                                                 
      REAL GROWYR(18,4,2),  ZORAT(IC),  CANEXT(4),  DELZ(IG)
C                                                                                 
C     * SOIL PROPERTY ARRAYS.                                     
C                                                                                 
      REAL DELZW(ILG,IG), ZBOTW(ILG,IG)
     2     
C                                                                                 
C     * VECTORIZING ARRAYS NOT USED ELSEWHERE IN CLASSA.                          
C                                                                                 
      REAL RMAT (ILG,IC,IG),H     (ILG,IC),  HS    (ILG,IC),                      
     1     CWCPAV(ILG),     AILCAN(ILG),     AILCNS(ILG),                         
     2     GROWA (ILG),     GROWN (ILG),     GROWB (ILG),                         
     3     RRESID(ILG),     SRESID(ILG),     
     4     THPORI(ILG,IG),  HCPS  (ILG,IG)
C
      INTEGER               ISAND (ILG,IG)
C                                               
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
      COMMON /CLASS6/ PI,GROWYR,ZOLNG,ZOLNS,ZOLNI,ZORATG     
      COMMON /CLASS7/ CANEXT
C-----------------------------------------------------------------------          
C
      IF(IC.NE.4)                               CALL XIT('APREP',-2)
C
C     * CALCULATE SOIL HEAT CAPACITY.
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
      DO 50 J=1,IG
      DO 50 I=IL1,IL2                                                               
          IF(ISAND(I,J).EQ.-4) THEN
              HCPS(I,J)=HCPICE
          ELSEIF(ISAND(I,J).EQ.-3) THEN
              HCPS(I,J)=HCPSND
          ELSEIF(ISAND(I,J).EQ.-2) THEN
              HCPS(I,J)=HCPOM
          ELSEIF(SAND(I,J).GT.0) THEN
              VSAND=SAND(I,J)/(RHOSOL*100.0)
              VORG=ORGM(I,J)/(RHOOM*100.0)
              VCLAY=(100.0-SAND(I,J)-ORGM(I,J))/(RHOSOL*100.0)
              VTOT=VSAND+VCLAY+VORG
              THSAND=(1.0-THPORI(I,J))*VSAND/VTOT
              THORG=(1.0-THPORI(I,J))*VORG/VTOT
              THCLAY=1.0-THPORI(I,J)-THSAND-THORG
              HCPS(I,J)=(HCPSND*THSAND+HCPCLY*THCLAY+HCPOM*THORG)/
     1             (1.0-THPORI(I,J))
          ENDIF
   50 CONTINUE
C
C     * INITIALIZE DIAGNOSTIC ARRAYS.
C
      DO 100 I=IL1,IL2
          HTCC(I) =0.0
          HTCS(I) =0.0
          HTC(I,1)=0.0
          HTC(I,2)=0.0
          HTC(I,3)=0.0
          WTRC(I) =0.0
          WTRS(I) =0.0
          WTRG(I) =0.0
  100 CONTINUE
C 
C     * DETERMINE GROWTH INDEX FOR CROPS (VEGETATION TYPE 3).
C     * MUST USE UN-GATHERED LONGITUDES TO COMPUTE ACTUAL LONGITUDE/LATITUDE VALUES.  
C                                                                                 
      DAY=FLOAT(IDAY)                                                             
      DO 120 I=IL1,IL2
          IL = ILAND(I)
          IN = INT( (RADJ(IL)+PI/2.0)*18.0/PI ) + 1
          DEGLON=REAL(ILSL(IL)-1)*360.0/REAL(LONSL)
          IF(DEGLON.GT.190. .AND. DEGLON.LT.330.)              THEN           
              NL=2                                                            
          ELSE                                                                
              NL=1                                                            
          ENDIF                                                               
          IF(GROWYR(IN,1,NL).LT.0.1)                           THEN           
              GROWA(I)=1.0                                                    
          ELSE                                                                
              IF(IN.GT.9)                                 THEN
                IF(DAY.GE.GROWYR(IN,2,NL).AND.DAY.LT.GROWYR(IN,3,NL))           
     1              GROWA(I)=1.0                                                
                IF(DAY.GE.GROWYR(IN,4,NL).OR.DAY.LT.GROWYR(IN,1,NL))            
     1              GROWA(I)=0.0                                
              ELSE
                IF(DAY.GE.GROWYR(IN,2,NL).OR.DAY.LT.GROWYR(IN,3,NL))           
     1              GROWA(I)=1.0                                                
                IF(DAY.GE.GROWYR(IN,4,NL).AND.DAY.LT.GROWYR(IN,1,NL))            
     1              GROWA(I)=0.0                                
              ENDIF                
              IF(DAY.GE.GROWYR(IN,1,NL).AND.DAY.LT.GROWYR(IN,2,NL))           
     1            GROWA(I)=(DAY-GROWYR(IN,1,NL))/(GROWYR(IN,2,NL)-            
     2                     GROWYR(IN,1,NL))                                   
              IF(DAY.GE.GROWYR(IN,3,NL).AND.DAY.LT.GROWYR(IN,4,NL))           
     1            GROWA(I)=(GROWYR(IN,4,NL)-DAY)/(GROWYR(IN,4,NL)-            
     2                     GROWYR(IN,3,NL))                                   
          ENDIF                                                               
  120 CONTINUE                                                                
C
C     * DETERMINE GROWTH INDICES FOR NEEDLELEAF TREES, BROADLEAF
C     * TREES AND GRASS (VEGETATION TYPES 1, 2 AND 4); CALCULATE
C     * VEGETATION HEIGHT, CORRECTED FOR GROWTH STAGE FOR CROPS
C     * AND FOR SNOW COVER FOR CROPS AND GRASS; CALCULATE CURRENT
C     * LEAF AREA INDEX FOR FOUR VEGETATION TYPES.
C
      DO 150 I=IL1,IL2                                                            
          GROWN(I)=ABS(GROWTH(I))                                                 
          GROWG=1.0                                                               
          IF(GROWTH(I).GT.0.0)                      THEN                          
              GROWB(I)=MIN(1.0,GROWTH(I)*2.0)                                   
          ELSE                                                                    
              GROWB(I)=MAX(0.0,(ABS(GROWTH(I))*2.0-1.0))                        
          ENDIF                                                                   
C                                                                                 
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
C    IF USING CTEM's STRUCTURAL ATTRIBUTES OVERWRITE ZOLN
C 
          IF (CTEM2) THEN
            ZOLN(I,1)=ZOLNC(I,1)
            ZOLN(I,2)=ZOLNC(I,2)
            ZOLN(I,3)=ZOLNC(I,3)
            ZOLN(I,4)=ZOLNC(I,4)
          ENDIF
C
C     *   IF USING CTEM's ZOLN THEN NO NEED FOR MULTIPLYING WITH CLASS'
C     *   CROP GROWTH INDEX
C
          IF (CTEM2) THEN
            H(I,3)=10.0*EXP(ZOLN(I,3))
          ELSE
            H(I,3)=10.0*EXP(ZOLN(I,3))*GROWA(I)                                     
          ENDIF
C
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
          H(I,1)=10.0*EXP(ZOLN(I,1))                                              
          H(I,2)=10.0*EXP(ZOLN(I,2))                                              
          H(I,4)=10.0*EXP(ZOLN(I,4))                                              
C
          HS(I,1)=H(I,1)                                                          
          HS(I,2)=H(I,2)                                                          
          HS(I,3)=MAX(H(I,3)-ZSNOW(I),1.0E-8)                                       
          HS(I,4)=MAX(H(I,4)-ZSNOW(I),1.0E-8)                                       
C
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
C     *   USE CTEM GENERATED LAI OR CLASS' OWN SPECIFIED LAI
C
          IF (CTEM2) THEN                        
            AIL(I,1)=AILC(I,1)
            AIL(I,2)=AILC(I,2)
            AIL(I,3)=AILC(I,3)
            AIL(I,4)=AILC(I,4)
          ELSE
C    ----------------- CTEM MODIFICATIONS -----------------------------/
            AIL(I,1)=AILMIN(I,1)+GROWN(I)*(AILMAX(I,1)-AILMIN(I,1))                 
            AIL(I,2)=AILMIN(I,2)+GROWB(I)*(AILMAX(I,2)-AILMIN(I,2))                 
            AIL(I,3)=AILMIN(I,3)+GROWA(I)*(AILMAX(I,3)-AILMIN(I,3))                 
            AIL(I,4)=AILMIN(I,4)+GROWG   *(AILMAX(I,4)-AILMIN(I,4))                 
          ENDIF                                  ! CTEM MODIFICATION
C
          AILS(I,1)=AIL(I,1)                                                      
          AILS(I,2)=AIL(I,2)                                                      

          IF(H(I,3).GT.0.0) THEN                                                  
              AILS(I,3)  =AIL(I,3)*HS(I,3)/H(I,3)                                   
          ELSE                                                                    
              AILS(I,3)=0.0                                                       
          ENDIF                                                                   
          IF(H(I,4).GT.0.0) THEN                                                  
              AILS(I,4)  =AIL(I,4)*HS(I,4)/H(I,4)                                   
          ELSE                                                                    
              AILS(I,4)=0.0                                                       
          ENDIF                                                                   

C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
C         IF ONLY PHOTOSYNTHESIS AND STOMATAL RESISTANCE PART OF CTEM IS 
C         USED, AND CTEM's STRUCTURAL ATTRIBUTES ARE NOT BEING USED THEN 
C         FIND GREEN LAI FOR USE BY PHOTOSYNTHESIS, USING CLASS'
C         APPROACH. FOLLOWING IS NOT THE BEST WAY TO DO THIS, BUT AT
C         THIS STAGE LETS NOT TO PUT AN ADDITIONAL LOOP INSIDE THE
C         LATITUDE LOOP
C
          IF ( CTEM1 .AND. (.NOT.CTEM2) ) THEN
            AILCG(I,1)=AILCMIN(I,1)+GROWN(I)*(AILCMAX(I,1)-AILCMIN(I,1))!NDL EVG
            AILCG(I,2)=AILCMIN(I,2)+GROWN(I)*(AILCMAX(I,2)-AILCMIN(I,2))!NDL DCD
            AILCG(I,3)=AILCMIN(I,3)+GROWB(I)*(AILCMAX(I,3)-AILCMIN(I,3))!BDL EVG
            AILCG(I,4)=AILCMIN(I,4)+GROWB(I)*(AILCMAX(I,4)-AILCMIN(I,4))!BDL DCD CLD
            AILCG(I,5)=AILCMIN(I,5)+GROWB(I)*(AILCMAX(I,5)-AILCMIN(I,5))!BDL DCD DRY
            AILCG(I,6)=AIL(I,3)                           !C3 CROP
            AILCG(I,7)=AIL(I,3)                           !C4 CROP
            AILCG(I,8)=MAX(0.0, GROWB(I)*AILCMAX(I,4))    !C3 GRASS
            AILCG(I,9)=MAX(0.0, GROWB(I)*AILCMAX(I,4))    !C4 GRASS
          ENDIF
C
C         ESTIMATE GREEN LAI FOR CANOPY OVER SNOW FRACTION FOR CTEM's
C         9 PFTs, JUST LIKE CLASS DOES.
C
          AILCGS(I,1)=AILCG(I,1)    !NDL EVG 
          AILCGS(I,2)=AILCG(I,2)    !NDL DCD
          AILCGS(I,3)=AILCG(I,3)    !BDL EVG
          AILCGS(I,4)=AILCG(I,4)    !BDL DCD CLD
          AILCGS(I,5)=AILCG(I,5)    !BDL DCD DRY
          IF(H(I,3).GT.0.0) THEN                                                  
              AILCGS(I,6)=AILCG(I,6)*HS(I,3)/H(I,3)  !C3 CROP
              AILCGS(I,7)=AILCG(I,7)*HS(I,3)/H(I,3)  !C4 CROP
          ELSE                                                                    
              AILCGS(I,6)=0.0                      
              AILCGS(I,7)=0.0                      
          ENDIF                                                                   
          IF(H(I,4).GT.0.0) THEN                                                  
              AILCGS(I,8)=AILCG(I,8)*HS(I,4)/H(I,4)  !C3 GRASS
              AILCGS(I,9)=AILCG(I,9)*HS(I,4)/H(I,4)  !C4 GRASS
          ELSE                                                                    
              AILCGS(I,8)=0.0                       
              AILCGS(I,9)=0.0                       
          ENDIF                                                                   
C    ----------------- CTEM MODIFICATIONS -----------------------------/
  150 CONTINUE                                                                    
C
C
C     * ADJUST FRACTIONAL COVERAGE OF GRID CELL FOR CROPS AND
C     * GRASS IF LAI FALLS BELOW 1.0 DUE TO GROWTH STAGE OR
C     * SNOW COVER; RESET LAI TO 1.0; CALCULATE RESULTANT
C     * GRID CELL COVERAGE BY CANOPY, BARE GROUND, CANOPY OVER
C     * SNOW AND SNOW OVER BARE GROUND.
C     * ALSO CALCULATE SURFACE DETENTION CAPACITY FOR FOUR
C     * GRID CELL SUBAREAS BASED ON VALUES SUPPLIED BY 
C     * U. OF WATERLOO:
C     *        IMPERMEABLE SURFACES: 0.001 M.
C     *        BARE SOIL:            0.002 M.
C     *        LOW VEGETATION:       0.003 M.
C     *        FOREST:               0.01  M.
C     * FOR NOW, ASSIGN WETLANDS A VALUE OF 0.10 M.
C                                                                                 
      DO 175 I=IL1,IL2                                                            
          FCAN(I,1)=FCANMX(I,1)*(1.0-FSNOW(I))                                    
          FCAN(I,2)=FCANMX(I,2)*(1.0-FSNOW(I))                                    
C    ----------------- CTEM MODIFICATIONS ------------------------\
C
C         CHANGE LAI THRESHOLD FOR REDUCING FCAN FROM 1.0 TO 0.01 
C         FOR CROPS AND GRASSES
C
          IF(AIL(I,3).LT.0.01) THEN                                                
              FCAN(I,3)=FCANMX(I,3)*(1.0-FSNOW(I))*AIL(I,3)                       
              AIL (I,3)=0.01                                                      
          ELSE                                                                    
              FCAN(I,3)=FCANMX(I,3)*(1.0-FSNOW(I))                                
          ENDIF                                                                   
          IF(AIL(I,4).LT.0.01) THEN                                                
              FCAN(I,4)=FCANMX(I,4)*(1.0-FSNOW(I))*AIL(I,4)                       
              AIL (I,4)=0.01                                                       
C    ----------------- CTEM MODIFICATIONS ------------------------/
          ELSE                                                                    
              FCAN(I,4)=FCANMX(I,4)*(1.0-FSNOW(I))                                
          ENDIF                                                                   
C                                                                                 
          FCANS(I,1)=FCANMX(I,1)*FSNOW(I)                                         
          FCANS(I,2)=FCANMX(I,2)*FSNOW(I)                                         
C    ----------------- CTEM MODIFICATIONS ------------------------\
          IF(AILS(I,3).LT.0.01) THEN                                               
              FCANS(I,3)=FCANMX(I,3)*FSNOW(I)*AILS(I,3)                           
              AILS (I,3)=0.01                                                     
          ELSE                                                                    
              FCANS(I,3)=FCANMX(I,3)*FSNOW(I)                                     
          ENDIF                                                                   
          IF(AILS(I,4).LT.0.01) THEN                                               
              FCANS(I,4)=FCANMX(I,4)*FSNOW(I)*AILS(I,4)                           
              AILS (I,4)=0.01                                                      
C    ----------------- CTEM MODIFICATIONS ------------------------/
          ELSE                                                                    
              FCANS(I,4)=FCANMX(I,4)*FSNOW(I)                                     
          ENDIF                                                                   
C                                                                                 
          FC (I)=FCAN(I,1)+FCAN(I,2)+FCAN(I,3)+FCAN(I,4)                          
          FG (I)=1.0-FSNOW(I)-FC(I)                                               
          FCS(I)=FCANS(I,1)+FCANS(I,2)+FCANS(I,3)+FCANS(I,4)                      
          FGS(I)=FSNOW(I)-FCS(I)                                                  
          IF(ABS(1.0-FCS(I)-FC(I)).LT.1.0E-9) THEN
              FCS(I)=MIN(FSNOW(I),1.0)
              FC(I)=1.0-FCS(I)
              FGS(I)=0.0
              FG(I)=0.0
          ENDIF
          IF(ABS(1.0-FCS(I)-FGS(I)-FC(I)-FG(I)).GT.1.0E-12) 
     1                                   CALL XIT('APREP',-1)
C
          IF(ISAND(I,1).EQ.-2) THEN
              ZPLIMG(I)=0.10
              ZPLMGS(I)=0.10
              ZPLIMC(I)=0.10
              ZPLMCS(I)=0.10
          ELSE
              IF(ISAND(I,1).EQ.-4) ZPLIMG(I)=0.0
              IF(ISAND(I,1).EQ.-3) ZPLIMG(I)=0.001
              IF( SAND(I,1).GT. 0) ZPLIMG(I)=0.002
              IF(FGS(I).GT.0.0) THEN
                  ZPLMGS(I)=(ZPLIMG(I)*FSNOW(I)*(1.0-FCANMX(I,1)-
     1                      FCANMX(I,2)-FCANMX(I,3)-FCANMX(I,4))+
     2                      ZPLIMG(I)*(FSNOW(I)*FCANMX(I,3)-
     3                      FCANS(I,3))+0.003*(FSNOW(I)*FCANMX(I,4)-
     4                      FCANS(I,4)))/FGS(I)
              ELSE
                  ZPLMGS(I)=0.0
              ENDIF
              IF(FC(I).GT.0.0) THEN
                  ZPLIMC(I)=(0.01*(FCAN(I,1)+FCAN(I,2))+0.003*
     1                      (FCAN(I,3)+FCAN(I,4)))/FC(I)
              ELSE
                  ZPLIMC(I)=0.0
              ENDIF
              IF(FCS(I).GT.0.0) THEN
                  ZPLMCS(I)=(0.01*(FCANS(I,1)+FCANS(I,2))+0.003*
     1                      (FCANS(I,3)+FCANS(I,4)))/FCS(I)
              ELSE
                  ZPLMCS(I)=0.0
              ENDIF
          ENDIF
C
  175 CONTINUE                                                                    
C                                                                                 
C     * PARTITION INTERCEPTED LIQUID AND FROZEN MOISTURE BETWEEN
C     * CANOPY OVERLYING BARE GROUND AND CANOPY OVERLYING SNOW; ADD
C     * RESIDUAL TO SOIL MOISTURE OR SNOW (IF PRESENT); CALCULATE
C     * RELATIVE FRACTIONS OF LIQUID AND FROZEN INTERCEPTED 
C     * MOISTURE ON CANOPY.
C                                                                                 
      DO 200 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                     THEN                
              AILCAN(I)=(FCAN(I,1)*AIL(I,1)+FCAN(I,2)*AIL(I,2)+                   
     1                   FCAN(I,3)*AIL(I,3)+FCAN(I,4)*AIL(I,4))/FC(I)             
          ELSE                                                                    
              AILCAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              AILCNS(I)=(FCANS(I,1)*AILS(I,1)+FCANS(I,2)*AILS(I,2)+               
     1                   FCANS(I,3)*AILS(I,3)+FCANS(I,4)*AILS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              AILCNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
C          CWCAP (I)=0.20*AILCAN(I)                                                
C          CWCAPS(I)=0.20*AILCNS(I)                                                
          CWCAP (I)=0.0*AILCAN(I)                                                
          CWCAPS(I)=0.0*AILCNS(I)                                                
          RRESID(I)=0.0
          SRESID(I)=0.0
          IF(RCAN(I).GT.0. .AND. (FC(I)+FCS(I)).LE.1.0E-8)    THEN
              RRESID(I)=RRESID(I)+RCAN(I)
              RCAN(I)=0.0
          ENDIF
          IF(SCAN(I).GT.0. .AND. (FC(I)+FCS(I)).LE.1.0E-8)    THEN
              SRESID(I)=SRESID(I)+SCAN(I)
              SCAN(I)=0.0
          ENDIF
          IF(RCAN(I).GT.0. .AND. (FC(I)+FCS(I)).GT.0.)        THEN                
              RCAN(I)=RCAN(I)/(FC(I)+FCS(I))                                      
              IF(AILCAN(I).GT.0.0)                 THEN                           
                  RAICAN(I)=RCAN(I)*(FC(I)+FCS(I))/(FC(I)+FCS(I)*                 
     1                      AILCNS(I)/AILCAN(I))                                  
              ELSE                                                                
                  RAICAN(I)=0.0                                                   
              ENDIF                                                               
              IF(AILCNS(I).GT.0.0)                 THEN                           
                  RAICNS(I)=RCAN(I)*(FC(I)+FCS(I))/(FCS(I)+FC(I)*                 
     1                      AILCAN(I)/AILCNS(I))                                  
              ELSE                                                                
                  RAICNS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              RAICAN(I)=0.0                                                       
              RAICNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF(SCAN(I).GT.0. .AND. (FC(I)+FCS(I)).GT.0.)        THEN                
              SCAN(I)=SCAN(I)/(FC(I)+FCS(I))                                      
              IF(AILCAN(I).GT.0.0)                 THEN                           
                  SNOCAN(I)=SCAN(I)*(FC(I)+FCS(I))/(FC(I)+FCS(I)*                 
     1                      AILCNS(I)/AILCAN(I))                                  
              ELSE                                                                
                  SNOCAN(I)=0.0                                                   
              ENDIF                                                               
              IF(AILCNS(I).GT.0.0)                 THEN                           
                  SNOCNS(I)=SCAN(I)*(FC(I)+FCS(I))/(FCS(I)+FC(I)*                 
     1                      AILCAN(I)/AILCNS(I))                                  
              ELSE                                                                
                  SNOCNS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              SNOCAN(I)=0.0                                                       
              SNOCNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF((FC(I)+FCS(I)).GT.0.)                 THEN                           
              CWCPAV(I)=(FC(I)*CWCAP(I)+FCS(I)*CWCAPS(I))/(FC(I)+FCS(I))          
          ELSE                                                                    
              CWCPAV(I)=0.0                                                       
          ENDIF                                                                   
          IF(CWCPAV(I).GT.0.0)                     THEN                           
              FRAINC(I)=RCAN(I)/MAX((RCAN(I)+SCAN(I)),CWCPAV(I))                
              FSNOWC(I)=SCAN(I)/MAX((RCAN(I)+SCAN(I)),CWCPAV(I))                
          ELSE                                                                    
              FRAINC(I)=0.0                                                       
              FSNOWC(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF((RAICAN(I)+SNOCAN(I)).GT.CWCAP(I)) THEN
              RRESID(I)=RRESID(I)+FC(I)*(RAICAN(I)-FRAINC(I)*CWCAP(I))
              SRESID(I)=SRESID(I)+FC(I)*(SNOCAN(I)-FSNOWC(I)*CWCAP(I))
              RAICAN(I)=FRAINC(I)*CWCAP(I)
              SNOCAN(I)=FSNOWC(I)*CWCAP(I)
          ENDIF
C
          IF((RAICNS(I)+SNOCNS(I)).GT.CWCAPS(I)) THEN
              RRESID(I)=RRESID(I)+FCS(I)*(RAICNS(I)-FRAINC(I)*CWCAPS(I))
              SRESID(I)=SRESID(I)+FCS(I)*(SNOCNS(I)-FSNOWC(I)*CWCAPS(I))
              RAICNS(I)=FRAINC(I)*CWCAPS(I)
              SNOCNS(I)=FSNOWC(I)*CWCAPS(I)
          ENDIF
C
          WTRC (I)=WTRC(I)-(RRESID(I)+SRESID(I))/DELT
          WTRS (I)=WTRS(I)+SRESID(I)/DELT
          WTRG (I)=WTRG(I)+RRESID(I)/DELT
          HTCC (I)=HTCC(I)-TCAN(I)*(SPHW*RRESID(I)+SPHICE*SRESID(I))/
     1             DELT
          IF(FSNOW(I).GT.0.0)    THEN 
              SNOI=SNO(I)
              ZSNADD=SRESID(I)/(RHOSNO(I)*FSNOW(I))                               
              ZSNOW(I)=ZSNOW(I)+ZSNADD
              SNO(I)=ZSNOW(I)*FSNOW(I)*RHOSNO(I)                                  
              HTCS (I)=HTCS(I)+TCAN(I)*SPHICE*SRESID(I)/DELT
              TSNOW(I)=(TCAN(I)*SPHICE*SRESID(I)+TSNOW(I)*HCPICE*
     1                 SNOI/RHOICE)/(HCPICE*SNO(I)/RHOICE)
              SRESID(I)=0.0
          ENDIF                                                                   
          THICEI=THICE(I,1) 
          THLIQI=THLIQ(I,1)
          IF(DELZW(I,1).GT.0.0)                        THEN
              THICE(I,1)=THICE(I,1)+SRESID(I)/(RHOICE*DELZW(I,1))                        
              THLIQ(I,1)=THLIQ(I,1)+RRESID(I)/(RHOW*DELZW(I,1))                             
          ENDIF
          WTRS (I)=WTRS(I)-SRESID(I)/DELT
          WTRG (I)=WTRG(I)+SRESID(I)/DELT
          HTC(I,1)=HTC(I,1)+TCAN(I)*(RRESID(I)*HCPW/RHOW+SRESID(I)*
     1             HCPICE/RHOICE)/DELT
          IF(DELZW(I,1).GT.0.0)                        THEN
              TBAR(I,1)=(TBAR(I,1)*((DELZ(1)-DELZW(I,1))*HCPSND+
     1            DELZW(I,1)*(THLIQI*HCPW+THICEI*HCPICE+
     2            (1.0-THPORI(I,1))*HCPS(I,1)))+TCAN(I)*(RRESID(I)*
     3            HCPW/RHOW+SRESID(I)*HCPICE/RHOICE))/((DELZ(1)-
     4            DELZW(I,1))*HCPSND+DELZW(I,1)*(HCPW*THLIQ(I,1)+
     5            HCPICE*THICE(I,1)+HCPS(I,1)*(1.0-THPORI(I,1))))
          ENDIF
C
  200 CONTINUE                                                                    
C                                                                                 
C     * REMAINING CANOPY PARAMETERS.                                                
C     * FIRST, INITIALIZE WORK FIELDS FOR SUBSEQUENT CALCULATIONS FOR             
C     * BOTH SNOW-FREE AND SNOW-COVERED CASES.                                    
C                                                                                 
      DO 250 I=IL1,IL2                                                            
          DISP  (I)=0.                                                            
          ZOMLNC(I)=0.                                                            
          ZOELNC(I)=0.                                                            
          DISPS (I)=0.                                                            
          ZOMLCS(I)=0.                                                            
          ZOELCS(I)=0.                                                            
          ZOMLNG(I)=0.                                                            
          ZOELNG(I)=0.                                                            
          ZOMLNS(I)=0.                                                            
          ZOELNS(I)=0.                                                            
          RCMIN (I)=0.                                                            
          RCMINS(I)=0.                                                            
          CMASSC(I)=0.                                                            
          CMASCS(I)=0.                                                            
  250 CONTINUE                                                                    
C
C     * CALCULATION OF ROUGHNESS LENGTHS FOR HEAT AND MOMENTUM AND
C     * ZERO-PLANE DISPLACEMENT FOR CANOPY OVERLYING BARE SOIL AND
C     * CANOPY OVERLYING SNOW.
C                                                                                 
      DO 275 J=1,IC                                                               
      DO 275 I=IL1,IL2                                                            
          IF(FC(I).GT.0. .AND. H(I,J).GT.0.)                     THEN             
              IF(IDISP.EQ.1)   DISP(I)=DISP(I)+FCAN (I,J)*
     1                                 LOG(0.7*H(I,J))                     
              ZOMLNC(I)=ZOMLNC(I)+FCAN (I,J)*LOG(0.1*H(I,J))                     
              ZOELNC(I)=ZOELNC(I)+FCAN (I,J)*LOG(0.1*H(I,J)/ZORAT(J))            
          ENDIF                                                                   
          IF(FCS(I).GT.0. .AND. HS(I,J).GT.0.)                   THEN             
              IF(IDISP.EQ.1)   DISPS(I)=DISPS (I)+FCANS(I,J)*
     1                         LOG(0.7*HS(I,J))                    
              ZOMLCS(I)=ZOMLCS(I)+FCANS(I,J)*LOG(0.1*HS(I,J))                    
              ZOELCS(I)=ZOELCS(I)+FCANS(I,J)*LOG(0.1*HS(I,J)/ZORAT(J))           
          ENDIF                                                                   
  275 CONTINUE                                                                    
C                                                                                 
      DO 290 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              IF(IDISP.EQ.1)   DISP(I)=EXP(DISP(I)/FC(I))                                        
              ZOMLNC(I)=ZOMLNC(I)/FC(I)                                           
              ZOELNC(I)=ZOELNC(I)/FC(I)                                           
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                       THEN             
              IF(IDISP.EQ.1)   DISPS(I)=EXP(DISPS(I)/FCS(I))                                      
              ZOMLCS(I)=ZOMLCS(I)/FCS(I)                                          
              ZOELCS(I)=ZOELCS(I)/FCS(I)                                          
          ENDIF                                                                   
  290 CONTINUE                                                                    
C                                                                                 
C     * ADJUST ROUGHNESS LENGTHS OF BARE SOIL AND SNOW-COVERED BARE
C     * SOIL FOR URBAN ROUGHNESS IF PRESENT.
C                                                                                 
      DO 300 I=IL1,IL2                                                            
          IF(FG(I).GT.0.)                                        THEN             
              IF(ISAND(I,1).NE.-4)                   THEN                         
                  ZOMLNG(I)=((FG(I)-FCANMX(I,5)*(1.0-FSNOW(I)))*ZOLNG+            
     1                      FCANMX(I,5)*(1.0-FSNOW(I))*ZOLN(I,5))/FG(I)           
              ELSE                                                                
                  ZOMLNG(I)=ZOLNI                                                 
              ENDIF                                                               
              ZOELNG(I)=ZOMLNG(I)-LOG(ZORATG)                                    
          ENDIF                                                                   
          IF(FGS(I).GT.0.)                                       THEN             
              ZOMLNS(I)=((FGS(I)-FCANMX(I,5)*FSNOW(I))*ZOLNS+                     
     1                  FCANMX(I,5)*FSNOW(I)*ZOLN(I,5))/FGS(I)                    
              ZOELNS(I)=ZOMLNS(I)-LOG(ZORATG)                                    
          ENDIF                                                                   
  300 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE MINIMUM BULK STOMATAL RESISTANCE (ADJUSTED FOR 
C     * LEAF AREA INDEX) AND HEAT CAPACITY FOR CANOPY OVERLYING 
C     * BARE SOIL AND CANOPY OVERLYING SNOW.
C     * ALSO CALCULATE INSTANTANEOUS GRID-CELL AVERAGED CANOPY MASS.
C                                                                                 
      DO 350 J=1,IC                                                               
      DO 350 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              IF((AILMAX(I,J)-AILMIN(I,J)).GT.0.05 .AND.                          
     1            AIL(I,J).GT.0.5)                              THEN              
                  RCMIN(I)=RCMIN(I)+FCAN(I,J)/MIN(25.*AILMAX(I,J)/                            
     1                     AIL(I,J),5000.)                            
              ELSE IF((AILMAX(I,J)-AILMIN(I,J)).GT.0.05)        THEN                     
                  RCMIN(I)=RCMIN(I)+FCAN(I,J)/5000.0                                            
              ELSE                                                                
                  RCMIN(I)=RCMIN(I)+FCAN(I,J)/25.0                                              
              ENDIF                                                               
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                      THEN                     
              IF((AILMAX(I,J)-AILMIN(I,J)).GT.0.05 .AND.                                  
     1            AILS(I,J).GT.0.5)                              THEN             
                  RCMINS(I)=RCMINS(I)+FCANS(I,J)/MIN(25.*AILMAX(I,J)/                         
     1                     AILS(I,J),5000.)                           
              ELSE IF((AILMAX(I,J)-AILMIN(I,J)).GT.0.05)         THEN                     
                  RCMINS(I)=RCMINS(I)+FCANS(I,J)/5000.0                                         
              ELSE                                                                
                  RCMINS(I)=RCMINS(I)+FCANS(I,J)/25.0                             
              ENDIF                                                               
          ENDIF                                                                   
  350 CONTINUE                                                                    
C                                                                                 
      DO 375 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                       THEN                     
              RCMIN (I)=RCMIN(I)/FC(I)                                                   
              RCMIN (I)=1.0/RCMIN(I)                                                     
C    ----------------- CTEM MODIFICATIONS -----------------------------\
              IF (CTEM2) THEN
                CMASSC(I)=(FCAN(I,1)*CMASVEGC(I,1)+
     1                     FCAN(I,2)*CMASVEGC(I,2)+                   
     2                     FCAN(I,3)*CMASVEGC(I,3)+
     3                     FCAN(I,4)*CMASVEGC(I,4))/FC (I)           
              ELSE
C    ----------------- CTEM MODIFICATIONS -----------------------------/
                CMASSC(I)=(FCAN(I,1)*CWGTMX(I,1)+FCAN (I,2)*CWGTMX(I,2)+                      
     1                     FCAN(I,3)*CWGTMX(I,3)*GROWA(I)+
     2                     FCAN(I,4)*CWGTMX(I,4))/FC (I)           
              ENDIF       ! CTEM MODIFICATION
          ENDIF                                                                          
          IF(FCS(I).GT.0.)                                      THEN                     
              RCMINS(I)=RCMINS(I)/FCS(I)                                                 
              RCMINS(I)=1.0/RCMINS(I)                                                    
C    ----------------- CTEM MODIFICATIONS -----------------------------\
              IF (CTEM2) THEN
                CMASCS(I)=(FCANS(I,1)*CMASVEGC(I,1)+
     1                     FCANS(I,2)*CMASVEGC(I,2)+                  
     2                     FCANS(I,3)*CMASVEGC(I,3)
     3                    *HS(I,3)/MAX(H(I,3),1.0E-12)+                            
     4                     FCANS(I,4)*CMASVEGC(I,4)                         
     5                    *HS(I,4)/MAX(H(I,4),1.0E-12))/FCS(I)                     
              ELSE
C    ----------------- CTEM MODIFICATIONS -----------------------------/
                CMASCS(I)=(FCANS(I,1)*CWGTMX(I,1)+
     1                     FCANS(I,2)*CWGTMX(I,2)+                  
     2                     FCANS(I,3)*CWGTMX(I,3)
     3                    *HS(I,3)/MAX(H(I,3),1.0E-12)+                            
     4                     FCANS(I,4)*CWGTMX(I,4)                         
     5                    *HS(I,4)/MAX(H(I,4),1.0E-12))/FCS(I)                     
              ENDIF       ! CTEM MODIFICATION
          ENDIF                                                                   
          CHCAP (I)=SPHVEG*CMASSC(I)+SPHW*RAICAN(I)+SPHICE*SNOCAN(I)              
          CHCAPS(I)=SPHVEG*CMASCS(I)+SPHW*RAICNS(I)+SPHICE*SNOCNS(I)              
          HTCC  (I)=HTCC(I)-SPHVEG*CMAI(I)*TCAN(I)/DELT
          IF(CMAI(I).LT.1.0E-8 .AND. (CMASSC(I).GT.0.0 .OR.
     1              CMASCS(I).GT.0.0)) TCAN(I)=TA(I)
          CMAI  (I)=FC(I)*CMASSC(I)+FCS(I)*CMASCS(I)
          HTCC  (I)=HTCC(I)+SPHVEG*CMAI(I)*TCAN(I)/DELT
  375 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE VEGETATION ROOTING DEPTH AND FRACTION OF ROOTS 
C     * IN EACH SOIL LAYER (SAME FOR SNOW/BARE SOIL CASES).
C                                                                                 
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
C     * EXECUTE THE 400 LOOP ONLY WHEN CTEM FRACTION OF ROOTS IN EACH
C     * SOIL LAYER ARE NOT TO BE USED.
C
      IF (CTEM2) THEN                       
        DO 380 J = 1,IC
          DO 381 I = IL1, IL2
            RMAT(I,J,1)=RMATC(I,J,1)
            RMAT(I,J,2)=RMATC(I,J,2)
            RMAT(I,J,3)=RMATC(I,J,3)
381       CONTINUE
380     CONTINUE
      ELSE
C
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
      DO 400 J=1,IC                                                               
      DO 400 I=IL1,IL2                                                            
          ZROOT=ZRTMAX(I,J)
          IF(J.EQ.3) ZROOT=ZRTMAX(I,J)*GROWA(I)                                   
          ZROOT=MIN(ZROOT,(DELZW(I,1)+DELZW(I,2)+DELZW(I,3)))
          IF(ZROOT.LE.ZBOTW(I,1))                                   THEN             
              RMAT(I,J,1)=1.0                                                     
              RMAT(I,J,2)=0.0                                                     
              RMAT(I,J,3)=0.0                                                     
          ELSE                                                                    
              FCOEFF=EXP(-3.0*ZROOT)                                              
              RMAT(I,J,1)=1.0-(EXP(-3.0*ZBOTW(I,1))-FCOEFF)/(1.0-FCOEFF)             
              IF(ZROOT.LE.ZBOTW(I,2)) THEN                                           
                  RMAT(I,J,2)=1.0-RMAT(I,J,1)                                     
                  RMAT(I,J,3)=0.0                                                 
              ELSE                                                                
                  RMAT(I,J,3)=(EXP(-3.0*ZBOTW(I,2))-FCOEFF)/(1.0-FCOEFF)             
                  RMAT(I,J,2)=1.0-RMAT(I,J,1)-RMAT(I,J,3)                         
              ENDIF                                                               
          ENDIF                                                                   
  400 CONTINUE                                                                    
      ENDIF                                       ! CTEM MODIFICATION
C                                                                                 
      DO 500 J=1,IG                                                               
      DO 500 I=IL1,IL2                                                            
          IF((FC(I)+FCS(I)).GT.0.)                               THEN             
              FROOT(I,J)=((FCAN(I,1)+FCANS(I,1))*RMAT(I,1,J) +                    
     1                    (FCAN(I,2)+FCANS(I,2))*RMAT(I,2,J) +                    
     2                    (FCAN(I,3)+FCANS(I,3))*RMAT(I,3,J) +                    
     3                    (FCAN(I,4)+FCANS(I,4))*RMAT(I,4,J))/                    
     4                    (FC(I)+FCS(I))                                          
          ELSE                                                                    
              FROOT(I,J)=0.0                                                      
          ENDIF                                                                   
  500 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE SKY-VIEW FACTORS FOR BARE GROUND AND SNOW 
C     * UNDERLYING CANOPY.                                                         
C                                                                                 
      DO 600 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              FSVF (I)=(FCAN (I,1)*EXP(CANEXT(1)*AIL (I,1)) +                          
     1                  FCAN (I,2)*EXP(CANEXT(2)*AIL (I,2)) +                          
     2                  FCAN (I,3)*EXP(CANEXT(3)*AIL (I,3)) +                          
     3                  FCAN (I,4)*EXP(CANEXT(4)*AIL (I,4)))/FC (I)                    
          ELSE                                                                    
              FSVF (I)=0.                                                         
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                       THEN             
              FSVFS(I)=(FCANS(I,1)*EXP(CANEXT(1)*AILS(I,1)) +                          
     1                  FCANS(I,2)*EXP(CANEXT(2)*AILS(I,2)) +                          
     2                  FCANS(I,3)*EXP(CANEXT(3)*AILS(I,3)) +                          
     3                  FCANS(I,4)*EXP(CANEXT(4)*AILS(I,4)))/FCS(I)                    
          ELSE                                                                    
              FSVFS(I)=0.                                                         
          ENDIF                                                                   
  600 CONTINUE                                       
C                                                                                  
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
C     ESTIMATE FCANC AND FCANCS FOR USE BY PHTSYN SUBROUTINE BASED ON
C     FCAN AND FCANS FOR CTEM PFTs
C
      DO 650 J = 1, IC
        DO 660 I = IL1, IL2
          SFCANCMX(I,J)=0.0  ! SUM OF FCANCMXs
660     CONTINUE
650   CONTINUE
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
            SFCANCMX(I,J)=SFCANCMX(I,J)+FCANCMX(I,M)
720       CONTINUE
710     CONTINUE
700   CONTINUE
C
      K1=0
      DO 750 J = 1, IC
        IF(J.EQ.1) THEN
          K1 = K1 + 1
        ELSE
          K1 = K1 + NOL2PFTS(J-1)
        ENDIF
        K2 = K1 + NOL2PFTS(J) - 1
        DO 760 M = K1, K2
          DO 770 I = IL1, IL2
             IF(SFCANCMX(I,J).GT.1E-20) THEN
               FCANC(I,M)  = FCAN(I,J) * (FCANCMX(I,M)/SFCANCMX(I,J))    
               FCANCS(I,M) = FCANS(I,J)* (FCANCMX(I,M)/SFCANCMX(I,J))    
             ELSE
               FCANC(I,M)  = 0.0
               FCANCS(I,M) = 0.0
             ENDIF
770       CONTINUE
760     CONTINUE
750   CONTINUE
C
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
      RETURN                                                           
      END 
