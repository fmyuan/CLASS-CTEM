      SUBROUTINE TPREP(THLIQC, THLIQG, THICEC, THICEG, TBARC,  TBARG,             
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
     H                 PSIGND, FRTOT,  TCSAT                         )           
C                                                                                 
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 24/97 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         SET RC AND RCS TO ZERO FOR GRID CELLS
C     *                         WITH NO VEGETATION.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REMOVE SUBTRACTION OF RESIDUAL SOIL
C     *                         MOISTURE CONTENT IN CALCULATIONS OF
C     *                         "PSIZRO" AND "PSII".
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         INITIALIZE THREE NEW DIAGNOSTIC FIELDS.
C     * NOV 12/94 - D.VERSEGHY. SET INITIAL TEMPERATURE OF EMERGING
C     *                         CANOPY TO TA INSTEAD OF TO ZERO.
C     * JAN 31/94 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         INTRODUCE LIMITING VALUES INTO
C     *                         CALCULATION OF "PSIZRO" TO AVOID
C     *                         OVERFLOWS.
C     * JUL 27/93 - D.VERSEGHY/M.LAZARE. INITIALIZE NEW DIAGNOSTIC 
C     *                                  FIELDS FSGV,FSGG,FLGV,FLGG,
C     *                                  HFSC,HFSG,HMFC.
C     * MAY 06/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  MODIFICATIONS TO CANOPY
C     *                                  RESISTANCE TO ADD "RCS"
C     *                                  FIELD FOR SNOW-COVERED
C     *                                  CANOPY. 
C     * JUL 04/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.                            
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).            
C     * APR 11/89 - D.VERSEGHY. PREPARATION AND INITIALIZATION FOR
C     *                         LAND SURFACE ENERGY BUDGET 
C     *                         CALCULATIONS.
C                                                                                 
C     * OUTPUT ARRAYS.                                                            
C                                                                                 
      REAL TBARC (ILG,IG),TBARG (ILG,IG),TBARCS(ILG,IG),TBARGS(ILG,IG),
     1     THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),           
     2     HCPC  (ILG,IG),HCPG  (ILG,IG),TCTOP (ILG,IG),TCBOT (ILG,IG)            
C                                                                                 
      REAL ZPOND (ILG),   PSIZRO(ILG),   RC    (ILG),   HCPSNO(ILG),              
     1     TCSNOW(ILG),   TSNOGS(ILG),   TSNOCS(ILG),   TCANO (ILG),              
     2     TCANS (ILG),   CEVAP (ILG),   RCS   (ILG)
C                                                                                 
      INTEGER             IEVAP (ILG)                                             
C                                                                                 
C     * OUTPUT ARRAYS WHICH ARE INTERNAL WORK ARRAYS FOR CLASST                   
C     * (INITIALIZED TO ZERO HERE).                                               
C     * FOR 2-D ARRAYS, THE SECOND INDEX REFERS TO THE GRID CELL
C     * SUBAREAS: CANOPY-SNOW, SNOW-GROUND, CANOPY-GROUND AND 
C     * BARE GROUND (VALUES 1 TO 4, RESPECTIVELY).                                                     
C                                                                                 
      REAL STT   (ILG,4), SUT   (ILG,4), SVT   (ILG,4), SQT   (ILG,4),            
     1     CDHT  (ILG,4), CDMT  (ILG,4), RIBT  (ILG,4), TSURT (ILG,4),            
     2     QSURT (ILG,4), QSWT  (ILG,4), QLWT  (ILG,4), QSENT (ILG,4),            
     3     QEVAT (ILG,4)                                                          
C                                                                                 
      REAL EVAPC (ILG),   EVAPCG(ILG),   EVAPG (ILG),   EVAPCS(ILG),              
     1     EVPCSG(ILG),   EVAPGS(ILG),   EVAPBC(ILG),   EVAPBS(ILG),              
     2     GSNOWC(ILG),   GSNOWG(ILG),   GZEROC(ILG),   GZEROG(ILG),              
     3     QMELTC(ILG),   QMELTG(ILG),   FSGV  (ILG),   FSGS  (ILG),
     4     FSGG  (ILG),   FLGV  (ILG),   FLGS  (ILG),   FLGG  (ILG),
     5     HFSC  (ILG),   HFSS  (ILG),   HFSG  (ILG),   HEVC  (ILG),
     6     HEVS  (ILG),   HEVG  (ILG),   HMFC  (ILG)
C
      REAL FROOT (ILG,IG)
C                                                                                 
C     * INPUT ARRAYS.                                                             
C                                                                                 
      REAL THLIQ (ILG,IG),THICE (ILG,IG),
     1     SAND  (ILG,IG),CLAY  (ILG,IG),ORGM  (ILG,IG)
C                                                                                 
      REAL QSWINV(ILG),   QSWINI(ILG),   RCMIN (ILG),   TCAN  (ILG),              
     1     RHOSNO(ILG),   TSNOW (ILG),   VPD   (ILG),   FC    (ILG),              
     2     FCS   (ILG),   TA    (ILG),   ZSNOW (ILG)                              
C                                                                                 
      INTEGER             ISAND (ILG,IG)
C                                                                                 
C     * SOIL PROPERTY ARRAYS.                                     
C                                                                                 
      REAL DELZW(ILG,IG), THPORI(ILG,IG), DELZ(IG) 
C                                                                                 
C     * INTERNAL WORK FIELDS FOR THIS ROUTINE.                                    
C                                                                                 
      REAL PSISTI(ILG,IG),BI    (ILG,IG),
     1     FVEG  (ILG),   PSIGND(ILG),   FRTOT (ILG),   TCSAT (ILG),
     2     THSAND(ILG,IG),THORG (ILG,IG)
C                                                                                 
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,SLTHICK,BEEM,ALFAH,              
     1                FAC,GAMRH,GAMRM,VMIN                                        
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C                                                                                 
      DATA PSIMAX /150./
C----------------------------------------------------------------------           
C     * INITIALIZE 2-D ARRAYS.                                                    
C                                                                                 
      DO 50 J=1,IG                                                                
      DO 50 I=IL1,IL2                                                             
          BI    (I,J)=0.159*CLAY(I,J)+2.91
          PSISTI(I,J)=(10.0**(-0.0131*SAND(I,J)+1.88))/100.0
          IF(ISAND(I,J).EQ.-2) THEN
              THSAND(I,J)=0.0
              THORG(I,J)=1.0-THPORI(I,J)
          ELSEIF(ISAND(I,J).EQ.-3) THEN
              THSAND(I,J)=1.0
              THORG(I,J)=0.0
          ELSEIF(SAND(I,J).GT.0) THEN
              VSAND=SAND(I,J)/(RHOSOL*100.0)
              VORG=ORGM(I,J)/(RHOOM*100.0)
              VCLAY=(100.0-SAND(I,J)-ORGM(I,J))/(RHOSOL*100.0)
              VTOT=VSAND+VCLAY+VORG
              THSAND(I,J)=(1.0-THPORI(I,J))*VSAND/VTOT
              THORG(I,J)=(1.0-THPORI(I,J))*VORG/VTOT
          ENDIF
          THLIQG(I,J)=THLIQ(I,J)                                                  
          THICEG(I,J)=THICE(I,J)                                                  
          THLIQC(I,J)=THLIQ(I,J)                                                  
          THICEC(I,J)=THICE(I,J)                                                  
          TBARCS(I,J)=0.0                                                         
          TBARGS(I,J)=0.0                                                         
          TBARC (I,J)=0.0                                                         
          TBARG (I,J)=0.0
   50 CONTINUE                                                                    
C                                                                                 
C     * INITIALIZE 1-D INTERNAL WORK FIELDS FOR LATER USE.                        
C                                                                                 
      DO 100 I=IL1,IL2                                                            
          FVEG  (I)=FC(I)+FCS(I)                                                  
          PSIGND(I)=PSIMAX                                                        
          FRTOT (I)=0.                                                            
C          IF(TCAN(I).GT.5.0) THEN
C              TCANS (I)=TCAN(I)                                                       
C              TCANO (I)=TCAN(I)                                                       
C          ELSE
              TCANS (I)=TA(I)                                                       
              TCANO (I)=TA(I)                                                       
C          ENDIF
          EVAPC (I)=0.                                                            
          EVAPCG(I)=0.                                                            
          EVAPG (I)=0.                                                            
          EVAPCS(I)=0.                                                            
          EVPCSG(I)=0.                                                            
          EVAPGS(I)=0.                                                            
          EVAPBC(I)=0.                                                            
          EVAPBS(I)=0.                                                            
          GSNOWC(I)=0.                                                            
          GSNOWG(I)=0.                                                            
          GZEROC(I)=0.                                                            
          GZEROG(I)=0.                                                            
          QMELTC(I)=0.                                                            
          QMELTG(I)=0.
          FSGV  (I)=0.
          FSGS  (I)=0.
          FSGG  (I)=0.
          FLGV  (I)=0. 
          FLGS  (I)=0. 
          FLGG  (I)=0.
          HFSC  (I)=0.
          HFSS  (I)=0.
          HFSG  (I)=0.
          HEVC  (I)=0.
          HEVS  (I)=0.
          HEVG  (I)=0.
          HMFC  (I)=0.
C                                                                                 
C     * DECOMPOSE FIRST SOIL LAYER LIQUID MOISTURE CONTENT INTO SOIL
C     * MOISTURE AND PONDED WATER. 
C
          IF(FVEG(I).LE.0.)                                    THEN               
              THLMXG=MAX(THLMIN,(THPORI(I,1)-THICEG(I,1)*RHOICE/RHOW))            
              IF((ISAND(I,1).EQ.-3).OR.(ISAND(I,1).EQ.-4)) THLMXG=0.0
              IF(THLIQG(I,1).GT.THLMXG)                        THEN                          
                  ZPOND(I)=(THLIQG(I,1)-THLMXG)*DELZW(I,1)                           
                  THLIQG(I,1)=THLMXG                                              
              ELSE                                                                
                  ZPOND(I)=0.0                                                    
              ENDIF                                                               
          ELSE IF(FVEG(I).GE.1.)                               THEN               
              THLMXC=MAX(THLMIN,(THPORI(I,1)-THICEC(I,1)*RHOICE/RHOW))            
              IF((ISAND(I,1).EQ.-3).OR.(ISAND(I,1).EQ.-4)) THLMXC=0.0
              IF(THLIQC(I,1).GT.THLMXC)                        THEN                          
                  ZPOND(I)=(THLIQC(I,1)-THLMXC)*DELZW(I,1)                           
                  THLIQC(I,1)=THLMXC                                              
              ELSE                                                                
                  ZPOND(I)=0.0                                                    
              ENDIF                                                               
          ELSE                                                                    
              IF((ISAND(I,1).EQ.-3).OR.(ISAND(I,1).EQ.-4))    THEN
                  THLMXG=0.0
                  THLMXC=0.0
              ELSE
              THLMXG=MAX(THLMIN,(THPORI(I,1)-THICEG(I,1)*RHOICE/
     1               RHOW))            
              THLMXC=MAX(THLMIN,(THPORI(I,1)-THICEC(I,1)*RHOICE/
     1               RHOW))            
              ENDIF
              IF(THLIQG(I,1).GT.THLMXG)                       THEN                
                  THLADD=(THLIQG(I,1)-THLMXG)*(1.0-FVEG(I))/FVEG(I)               
                  THLIQG(I,1)=THLMXG                                              
                  THLIQC(I,1)=THLIQC(I,1)+THLADD                                  
              ENDIF                                                               
              IF(THLIQC(I,1).GT.THLMXC)                       THEN                
                  ZPOND(I)=(THLIQC(I,1)-THLMXC)*DELZW(I,1)*FVEG(I)                   
                  THLIQC(I,1)=THLMXC                                              
              ELSE                                                                
                  ZPOND(I)=0.0                                                    
              ENDIF                                                               
          ENDIF                                                                   
          IF(ZPOND(I).LT.1.0E-12) ZPOND(I)=0.0
  100 CONTINUE                                                                    
C                                                                                 
C     * SURFACE MOISTURE VARIABLES FOR BARE SOIL ENERGY BALANCE
C     * CALCULATIONS.                                 
C
      DO 150 I=IL1,IL2                                                                       
          IF(DELZW(I,1).LT.0.10) THEN
C          IF(DELZW(I,1).LE.0.10) THEN
              THLZRO=THLIQG(I,1)
          ELSE
              THLZRO=MAX(0.0,MIN(THPORI(I,1),0.5*(3.0*THLIQG(I,1)-             
     1                     THLIQG(I,2))))                                              
          ENDIF
          IF(THLZRO.GT.(THLMIN+0.001)) THEN                                          
              PSIZRO(I)=PSISTI(I,1)*(THLZRO/THPORI(I,1))**(-BI(I,1)) 
              IEVAP(I)=1                                                          
          ELSE                                                                    
              PSIZRO(I)=1.0E+8                                                   
              IEVAP(I)=0                                                          
          ENDIF                                                                   
          CEVAP(I)=-GRAV*PSIZRO(I)/RGASV                                          
  150 CONTINUE  
C                                                                                 
C     * CALCULATE BULK SOIL MOISTURE SUCTION FOR STOMATAL RESISTANCE.
C     * CALCULATE FRACTIONAL TRANSPIRATION EXTRACTED FROM SOIL LAYERS.
C
      DO 200 J=1,IG                                                               
      DO 200 I=IL1,IL2                                                            
          IF(FVEG(I).GT.0.0)                                       THEN          
              IF(THLIQC(I,J).GT.(THLMIN+0.01) .AND. FROOT(I,J).GT.0.)             
     1                                                         THEN               
                  PSII=PSISTI(I,J)*(THLIQC(I,J)/THPORI(I,J))**(-BI(I,J))
                  PSII=MIN(PSII,PSIMAX)                                         
                  PSIGND(I)=MIN(PSIGND(I),PSII)                                 
                  FROOT(I,J)=FROOT(I,J)*(PSIMAX-PSII)/(PSIMAX-
     1                       PSISTI(I,J))          
                  FRTOT(I)=FRTOT(I)+FROOT(I,J)                                    
              ELSE
                  FROOT(I,J)=0.0
              ENDIF                                                               
          ENDIF                                                                   
  200 CONTINUE                                                                    
C                                                                                 
      DO 225 J=1,IG                                                               
      DO 225 I=IL1,IL2                                                            
          IF(FRTOT(I).GT.0.)                                       THEN           
              FROOT(I,J)=FROOT(I,J)/FRTOT(I)                                      
          ENDIF                                                                   
  225 CONTINUE                                                                    
C                                                                                 
C     * BULK STOMATAL RESISTANCES FOR CANOPY OVERLYING SNOW AND CANOPY
C     * OVERLYING BARE SOIL.
C
      DO 250 I=IL1,IL2                                                            
          IF(FVEG(I).GT.0.)                                        THEN
              RCS(I)=5000.0
              IF(PSIGND(I).GE.PSIMAX)                          THEN               
C    -------------------- CTEM MODIFICATIONS -------------------------\
C             CHANGE 1.0E+20 TO 1.0E+4, BECAUSE ITS HARD TO WRITE A
C             HUGE VALUE OF STOMATAL RESISTANCE TO .OF4 FILE
                  RC (I)=1.0E+4                   
                  RCS(I)=1.0E+4                                
C    -------------------- CTEM MODIFICATIONS -------------------------/
              ELSE IF(TA(I).LE.TFREZ .OR. TA(I).GE.(TFREZ+40.0) .OR.              
     1               (QSWINV(I)+QSWINI(I)).LT.5.0)             THEN               
                  RC(I)=5000.0  
              ELSE                                                                
                  RCV=MAX(1.0,(VPD(I)/5.0))                                     
                  RCG=MAX(1.0,(PSIGND(I)/40.0))                                 
                  RCQ=MAX(1.0,(500.0/(QSWINV(I)+QSWINI(I))-1.5))                
                  RC(I)=MIN(RCMIN(I)*RCV*RCG*RCQ,5000.0)                        
              ENDIF                                                               
          ELSE
              RC(I)=0.0
              RCS(I)=0.0
          ENDIF                                                                   
  250 CONTINUE                                                                    
C                                                                                 
C     * VOLUMETRIC HEAT CAPACITIES OF SOIL LAYERS.                                
C                                                                                 
      DO 300 J=1,IG                                                               
      DO 300 I=IL1,IL2                                                            
          IF(ISAND(I,1).GT.-4)                                     THEN          
              HCPC(I,J)=0.                                                        
              HCPG(I,J)=0.                                                        
              IF(FVEG(I).LT.1.)                                THEN               
                  IF(ISAND(I,J).EQ.-2)          THEN
                      HCPG(I,J)=HCPW*THLIQG(I,J)+HCPICE*THICEG(I,J)+
     1                    HCPOM*THORG(I,J)
                  ELSE
                      HCPG(I,J)=HCPW*THLIQG(I,J)+HCPICE*THICEG(I,J)+
     1                    HCPSND*THSAND(I,J)+HCPOM*THORG(I,J)+HCPCLY*
     2                    (1.0-THPORI(I,J)-THSAND(I,J)-THORG(I,J))
                  ENDIF
              ENDIF                                                               
              IF(FVEG(I).GT.0.)                                    THEN                    
                  IF(ISAND(I,J).EQ.-2)          THEN
                      HCPC(I,J)=HCPW*THLIQC(I,J)+HCPICE*THICEC(I,J)+
     1                    HCPOM*THORG(I,J)
                  ELSE
                      HCPC(I,J)=HCPW*THLIQC(I,J)+HCPICE*THICEC(I,J)+
     1                    HCPSND*THSAND(I,J)+HCPOM*THORG(I,J)+HCPCLY*
     2                    (1.0-THPORI(I,J)-THSAND(I,J)-THORG(I,J))
                  ENDIF
              ENDIF                                                               
          ELSE                                                                    
              HCPC(I,J)=HCPICE                                                    
              HCPG(I,J)=HCPICE                                                    
          ENDIF                                                                   
  300 CONTINUE                                                                    
C                                                                                 
C     * THERMAL PROPERTIES OF SNOW.
C                                                                                 
      DO 400 I=IL1,IL2                                                            
          IF(ZSNOW(I).GT.0.)                                        THEN          
              THSNOW=RHOSNO(I)/RHOICE                                             
              HCPSNO(I)=HCPICE*THSNOW                                             
              TCSNOW(I)=2.576E-6*RHOSNO(I)*RHOSNO(I)+0.074                        
              IF(FVEG(I).LT.1.)                                 THEN              
                  TSNOGS(I)=TSNOW(I)                                              
              ELSE                                                                
                  TSNOGS(I)=0.0                                                   
              ENDIF                                                               
              IF(FVEG(I).GT.0.)                                 THEN              
                  TSNOCS(I)=TSNOW(I)                                              
              ELSE                                                                
                  TSNOCS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              TSNOGS(I)=0.0                                                       
              TSNOCS(I)=0.0                                                       
          ENDIF                                                                   
  400 CONTINUE                                                                    
C                                                                                 
C     * THERMAL CONDUCTIVITIES OF SOIL LAYERS.                                         
C                                                                                 
      DO 500 J=1,IG                                                               
      DO 500 I=IL1,IL2                                                            
          IF    (ISAND(I,1).EQ.-4)                              THEN          
              TCTOP(I,J)=TCGLAC                                                     
              TCBOT(I,J)=TCGLAC
          ELSEIF(ISAND(I,J).EQ.-3)                              THEN
              TCTOP(I,J)=TCSAND
              TCBOT(I,J)=TCSAND
          ELSE 
              IF(ISAND(I,J).EQ.-2)          THEN
                  TCDRY=TCDRYP
              ELSE
                  TCDRY=TCDRYS
              ENDIF
              SATRAT=MIN((THLIQG(I,J)+THICEG(I,J))/THPORI(I,J), 1.0)              
              THLSAT=THPORI(I,J)*THLIQG(I,J)/(THLIQG(I,J)+
     1               THICEG(I,J))          
              THISAT=THPORI(I,J)*THICEG(I,J)/(THLIQG(I,J)+
     1               THICEG(I,J))          
              THCLAY=1.0-THPORI(I,J)-THSAND(I,J)-THORG(I,J)
              TCSAT(I)=(TCSAND**THSAND(I,J))*(TCOM**THORG(I,J))*
     1               (TCCLAY**THCLAY)*(TCW**THLSAT)*(TCICE**THISAT)
              TCSOIL=(TCSAT(I)-TCDRY)*SATRAT+TCDRY                              
              IF(DELZW(I,J).GT.0.0) THEN
                  TCTOP(I,J)=TCSOIL
              ELSE
                  TCTOP(I,J)=TCSAND
              ENDIF
              IF(DELZW(I,J).LT.DELZ(J)) THEN
                  TCBOT(I,J)=TCSAND
              ELSE
                  TCBOT(I,J)=TCSOIL
              ENDIF
              IF(J.EQ.1.AND.ZPOND(I).GT.1.0E-3) TCTOP(I,J)=TCW
          ENDIF                                                                   
  500 CONTINUE                                                                    
C                                                                                 
C     * INITIALIZE REMAINING WORK ARRAYS FOR CLASST -                              
C     * 4-VALUE ARRAYS FOR EACH OF CANOPY-SNOW, SNOW-GROUND,          
C     * CANOPY-GROUND AND BARE GROUND. 
C                                                                                 
      DO 600 J=1,4                                                                
      DO 600 I=IL1,IL2                                                            
          STT   (I,J)=0.                                                          
          SUT   (I,J)=0.                                                          
          SVT   (I,J)=0.                                                          
          SQT   (I,J)=0.                                                          
          CDHT  (I,J)=0.                                                          
          CDMT  (I,J)=0.                                                          
          RIBT  (I,J)=0.                                                          
          TSURT (I,J)=0.                                                          
          QSURT (I,J)=0.                                                          
          QSWT  (I,J)=0.                                                          
          QLWT  (I,J)=0.                                                          
          QSENT (I,J)=0.                                                          
          QEVAT (I,J)=0.                                                          
  600 CONTINUE                                                                    
C                                                                                 
      RETURN                                                                      
      END 
