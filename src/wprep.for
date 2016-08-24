      SUBROUTINE WPREP(THLQCO, THLQGO, THLQCS, THLQGS, THICCO, THICGO,
     1                 THICCS, THICGS, HCPCO,  HCPGO,  HCPCS,  HCPGS,
     2                 ST,     TST,    RT,     TRT,    ALBST,  RHOST,
     3                 ZPONDT, ZSNOWT, EVT,    HCPST,  RUNOFT, XSNOWT,
     4                 SUBLC,  SUBLCS, WLOSTC, WLOSTG, WLSTCS, WLSTGS,
     5                 RAC,    RACS,   SNC,    SNCS,   TSNOWC, TSNOWG,
     6                 PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     7                 QFN,    QFG,    QFC,    HMFN,   HMFG,   
     8                 ROFC,   ROFN,   OVRFLW, HCPS,   FSVF,   FSVFS,
     9                 THLIQC, THLIQG, THICEC, THICEG, HCPC,   HCPG,
     A                 FC,     FG,     FCS,    FGS,    PCPR,   TA,
     B                 ZPOND,  ZSNOW,  ALBSNO, RHOSNO, EVAPC,  EVAPCG,
     C                 EVAPG,  EVAPCS, EVPCSG, EVAPGS, RAICAN, SNOCAN,
     D                 RAICNS, SNOCNS, THPORI, SAND,   ORGM,   ISAND,  
     E                 ILG,    IL1,    IL2,    JL,     IG,                        
     F                 NLANDCS,NLANDGS,NLANDC, NLANDG,
     G                 R,      S,      TR,     TS,     RADD,   SADD)
C
C     * NOV 09/00 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS FROM 
C     *                         SUBCAN INTO THIS ROUTINE.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 02/95 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION OF
C     *                         OVERLAND FLOW.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE USE OF "WLOST":
C     *                         COMPLETION OF WATER BUDGET DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         INITIALIZE TWO NEW DIAGNOSTIC FIELDS.
C     * AUG 20/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISED CALCULATION OF CANOPY 
C     *                         SUBLIMATION RATE.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NEW DIAGNOSTIC FIELDS. 
C     * APR 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. PREPARATION AND INITIALIZATION FOR
C     *                         LAND SURFACE WATER BUDGET CALCULATIONS.
C                                                     
C     * OUTPUT ARRAYS.
C
      REAL THLQCO(ILG,IG),THLQGO(ILG,IG),THLQCS(ILG,IG),THLQGS(ILG,IG),               
     1     THICCO(ILG,IG),THICGO(ILG,IG),THICCS(ILG,IG),THICGS(ILG,IG),               
     2     HCPCO (ILG,IG),HCPGO (ILG,IG),HCPCS (ILG,IG),HCPGS (ILG,IG), 
     3     QFC   (ILG,IG),HMFG  (ILG,IG),HCPS  (ILG,IG)
C
      REAL SUBLC (ILG),   SUBLCS(ILG),   WLOSTC(ILG),   WLOSTG(ILG),
     1     WLSTCS(ILG),   WLSTGS(ILG),   RAC   (ILG),   RACS  (ILG),
     2     SNC   (ILG),   SNCS  (ILG),   TSNOWC(ILG),   TSNOWG(ILG),
     3     PCFC  (ILG),   PCLC  (ILG),   PCPN  (ILG),   PCPG  (ILG),
     4     QFCF  (ILG),   QFCL  (ILG),   QFN   (ILG),   QFG   (ILG),
     5     HMFN  (ILG),   ROFC  (ILG),   ROFN  (ILG),   OVRFLW(ILG)
C
C
C     * 2-D ARRAYS: THE SECOND INDEX REFERS TO THE GRID CELL SUBAREAS
C     * OF CANOPY-SNOW, GROUND-SNOW, CANOPY-GROUND AND BARE GROUND
C     * (VALUES 1 TO 4, RESPECTIVELY).
C
      REAL ST    (ILG,4), TST   (ILG,4), RT    (ILG,4), TRT   (ILG,4),
     1     ALBST (ILG,4), RHOST (ILG,4), ZPONDT(ILG,4), ZSNOWT(ILG,4),
     2     EVT   (ILG,4), HCPST (ILG,4), RUNOFT(ILG,4), XSNOWT(ILG,4)
C
C     * INPUT ARRAYS.
C
      REAL THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),           
     1     HCPC  (ILG,IG),HCPG  (ILG,IG),THPORI(ILG,IG),SAND  (ILG,IG),
     2     ORGM  (ILG,IG)
C
      INTEGER             ISAND (ILG,IG)
C
      REAL FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),
     1     PCPR  (ILG),   TA    (ILG),   ZPOND (ILG),   ZSNOW (ILG),
     2     ALBSNO(ILG),   RHOSNO(ILG),   EVAPC (ILG),   EVAPCG(ILG),
     3     EVAPG (ILG),   EVAPCS(ILG),   EVPCSG(ILG),   EVAPGS(ILG),
     4     RAICAN(ILG),   SNOCAN(ILG),   RAICNS(ILG),   SNOCNS(ILG),
     5     FSVF  (ILG),   FSVFS (ILG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL R     (ILG),   S     (ILG),   TR    (ILG),   TS    (ILG),
     1     RADD  (ILG),   SADD  (ILG)  
C                                                                                  
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * INITIALIZE 2-D ARRAYS.
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
          THLQCO(I,J)=0.0                                                           
          THLQGO(I,J)=0.0                                                           
          THLQCS(I,J)=0.0                                                           
          THLQGS(I,J)=0.0
          THICCO(I,J)=0.0                                                           
          THICGO(I,J)=0.0                                                           
          THICCS(I,J)=0.0                                                           
          THICGS(I,J)=0.0
          HCPCO (I,J)=0.0                                                            
          HCPGO (I,J)=0.0                                                            
          HCPCS (I,J)=0.0                                                            
          HCPGS (I,J)=0.0                                                            
          QFC   (I,J)=0.0
          HMFG  (I,J)=0.0
   50 CONTINUE
C
C     * INITIALIZE 2-D WORK ARRAYS FOR CLASSW.
C     * THESE ARE 4-VALUE ARRAYS FOR EACH OF CANOPY-SNOW, SNOW-GROUND,
C     * CANOPY-GROUND AND BARE GROUND.
C
      DO 150 J=1,4
      DO 150 I=IL1,IL2                                                                    
          ZPONDT(I,J)=0.0                                                                  
          ZSNOWT(I,J)=0.0
          ALBST (I,J)=0.0
          RHOST (I,J)=0.0
          HCPST (I,J)=0.0
          RUNOFT(I,J)=0.0
          XSNOWT(I,J)=0.0
  150 CONTINUE
C
C     * INITIALIZE OTHER ARRAYS.
C
      DO 175 I=IL1,IL2
          EVT   (I,1)=EVAPCS(I)+EVPCSG(I)
          EVT   (I,2)=EVAPGS(I)
          EVT   (I,3)=EVAPC (I)+EVAPCG(I)
          EVT   (I,4)=EVAPG (I)
          TSNOWC(I)=0.0
          TSNOWG(I)=0.0
          WLOSTC(I)=0.0                                                                  
          WLOSTG(I)=0.0                                                                  
          WLSTCS(I)=0.0                                                                  
          WLSTGS(I)=0.0                                                                  
          RAC   (I)=RAICAN(I)
          RACS  (I)=RAICNS(I)                                                                 
          SNC   (I)=SNOCAN(I)                                                                  
          SNCS  (I)=SNOCNS(I)
          PCFC  (I)=0.0
          PCLC  (I)=0.0
          PCPN  (I)=0.0
          PCPG  (I)=0.0
          QFCF  (I)=0.0
          QFCL  (I)=0.0
          QFN   (I)=0.0
          QFG   (I)=0.0
          HMFN  (I)=0.0
          ROFC  (I)=0.0
          ROFN  (I)=0.0
          OVRFLW(I)=0.0
C                                                                 
C     * DIAGNOSE PRECIPITATION AS RAIN OR SNOW.
C
          IF(PCPR(I).GT.0.)                                         THEN                                                        
              IF(TA(I).GT.TFREZ)              THEN                                                    
                  R   (I)=PCPR(I)/RHOW                                                         
                  TR  (I)=TA(I)-TFREZ                                                         
                  S   (I)=0.0                                                               
                  TS  (I)=0.0                                                              
                  PCLC(I)=(FCS(I)*(1.0-FSVFS(I))+FC(I)*(1.0-FSVF(I)))*
     1                    R(I)*RHOW
                  PCPN(I)=(FCS(I)*FSVFS(I)+FGS(I))*R(I)*RHOW
                  PCPG(I)=(FC(I)*FSVF(I)+FG(I))*R(I)*RHOW
              ELSE                                                                    
                  S   (I)=PCPR(I)/RHOSNI                                                       
                  TS  (I)=TA(I)-TFREZ                                                         
                  R   (I)=0.0                                                               
                  TR  (I)=0.0                                                              
                  PCFC(I)=(FCS(I)*(1.0-FSVFS(I))+FC(I)*(1.0-FSVF(I)))*
     1                    S(I)*RHOSNI
                  PCPN(I)=(FCS(I)*FSVFS(I)+FGS(I)+FC(I)*FSVF(I)+FG(I))*
     1                    S(I)*RHOSNI
              ENDIF                                                                   
          ELSE                                                                        
              R (I)=0.0                                                                   
              TR(I)=0.0                                                                  
              S (I)=0.0                                                                   
              TS(I)=0.0                                                                  
          ENDIF
  175 CONTINUE
C
C     * IN THE FOLLOWING SECTIONS, DOWNWARD WATER VAPOUR FLUXES ARE 
C     * LUMPED TOGETHER WITH SNOWFALL/RAINFALL.
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C
      IF(NLANDCS.GT.0)                                              THEN
C
          DO 200 J=1,IG
          DO 200 I=IL1,IL2
              IF(FCS(I).GT.0.)                           THEN 
                  THLQCS(I,J)=THLIQC(I,J)                                               
                  THICCS(I,J)=THICEC(I,J)                                               
                  HCPCS (I,J)=HCPC  (I,J)
              ENDIF                                                  
  200     CONTINUE
C
          DO 250 I=IL1,IL2
              IF(FCS(I).GT.0.)                           THEN  
                  IF(SNOCNS(I).GT.0.)      THEN                                                  
                      SUBLCS(I)=EVAPCS(I)*(CLHMLT+CLHVAP)*SNOCNS(I)/
     1                          (CLHVAP*RAICNS(I)+(CLHVAP+CLHMLT)*
     2                          SNOCNS(I))                                         
                      EVAPCS(I)=EVAPCS(I)-SUBLCS(I)
                  ELSE                                                                    
                      SUBLCS(I)=0.0                                                          
                  ENDIF
                  IF(SUBLCS(I).GT.0.0) THEN
                      QFCF(I)=QFCF(I)+FCS(I)*SUBLCS(I)*RHOW
                  ELSE
                      QFCF(I)=QFCF(I)+FCS(I)*(1.0-FSVFS(I))*SUBLCS(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*SUBLCS(I)*RHOW
                  ENDIF
                  IF(EVAPCS(I).GT.0.0) THEN
                      QFCL(I)=QFCL(I)+FCS(I)*EVAPCS(I)*RHOW
                  ELSE
                      QFCL(I)=QFCL(I)+FCS(I)*(1.0-FSVFS(I))*EVAPCS(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*EVAPCS(I)*RHOW
                  ENDIF
C
                  IF(S(I).GT.0. .OR. SUBLCS(I).LT.0.) THEN                                      
                      SADD(I)=S(I)-SUBLCS(I)*RHOW/RHOSNI                                           
                      IF(SADD(I).GT.0.0) THEN                                                
                          IF(SUBLCS(I).GT.0.) THEN
                              QFCF(I)=QFCF(I)-FCS(I)*FSVFS(I)*
     1                                SUBLCS(I)*RHOW
                              QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*
     1                                SUBLCS(I)*RHOW
                          ENDIF
                          ST  (I,1)=SADD(I)                                                        
                          TST (I,1)=TS(I)+TFREZ                                                   
                          SUBLCS(I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FCS(I)*FSVFS(I)*S(I)*RHOSNI
                          PCFC(I)=PCFC(I)+FCS(I)*FSVFS(I)*S(I)*RHOSNI
                          SUBLCS(I)=-SADD(I)*RHOSNI/RHOW                                        
                          ST  (I,1)=0.0                                                         
                          TST (I,1)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      ST (I,1)=0.0                                                             
                      TST(I,1)=0.0                                                            
                  ENDIF
C
                  IF(R(I).GT.0. .OR. EVAPCS(I).LT.0.) THEN                                      
                      RADD(I)=R(I)-EVAPCS(I)                                                       
                      IF(RADD(I).GT.0.)   THEN                                                
                          IF(EVAPCS(I).GT.0.) THEN
                              QFCL(I)=QFCL(I)-FCS(I)*FSVFS(I)*
     1                                EVAPCS(I)*RHOW
                              QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*
     1                                EVAPCS(I)*RHOW
                          ENDIF
                          RT  (I,1)=RADD(I)                                                        
                          TRT (I,1)=TR(I)+TFREZ                                                   
                          EVAPCS(I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FCS(I)*FSVFS(I)*R(I)*RHOW
                          PCLC(I)=PCLC(I)+FCS(I)*FSVFS(I)*R(I)*RHOW
                          EVAPCS(I)=-RADD(I)                                                    
                          RT  (I,1)=0.0                                                         
                          TRT (I,1)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RT (I,1)=0.0                                                             
                      TRT(I,1)=0.0                                                            
                  ENDIF                                                                   
                  ZPONDT(I,1)=ZPOND (I)                                                            
                  ZSNOWT(I,1)=ZSNOW (I)                                                            
                  ALBST (I,1)=ALBSNO(I)                                                           
                  RHOST (I,1)=RHOSNO(I)                                                           
                  HCPST (I,1)=HCPICE*RHOSNO(I)/RHOICE                                             
              ENDIF
  250     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
C
          DO 300 J=1,IG
          DO 300 I=IL1,IL2
              IF(FGS(I).GT.0.)                           THEN 
                  THLQGS(I,J)=THLIQG(I,J)                                               
                  THICGS(I,J)=THICEG(I,J)                                               
                  HCPGS (I,J)=HCPG  (I,J)
              ENDIF                                                  
  300     CONTINUE
C
          DO 350 I=IL1,IL2
              IF(FGS(I).GT.0.)                           THEN 
                  QFN(I)=QFN(I)+FGS(I)*EVAPGS(I)*RHOW
                  IF(S(I).GT.0. .OR. EVAPGS(I).LT.0.) THEN                                      
                      SADD(I)=S(I)-EVAPGS(I)*RHOW/RHOSNI                                           
                      IF(SADD(I).GT.0.0) THEN                                                
                          ST  (I,2)=SADD(I)                                                        
                          TST (I,2)=TS(I)
                          EVAPGS(I)=0.0                                                      
                      ELSE                                                                
                          EVAPGS(I)=-SADD(I)*RHOSNI/RHOW                                        
                          ST  (I,2)=0.0                                                         
                          TST (I,2)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      ST (I,2)=0.0                                                             
                      TST(I,2)=0.0                                                            
                  ENDIF
C
                  IF(R(I).GT.0.)                         THEN                                      
                      RADD(I)=R(I)-EVAPGS(I)                                                       
                      IF(RADD(I).GT.0.)   THEN                                                
                          RT  (I,2)=RADD(I)                                                        
                          TRT (I,2)=TR(I)
                          EVAPGS(I)=0.0                                                      
                      ELSE                                                                
                          EVAPGS(I)=-RADD(I)                                                    
                          RT  (I,2)=0.0                                                         
                          TRT (I,2)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RT (I,2)=0.0                                                             
                      TRT(I,2)=0.0                                                            
                  ENDIF                                                                   
                  ZPONDT(I,2)=ZPOND (I)                                                            
                  ZSNOWT(I,2)=ZSNOW (I)                                                            
                  ALBST (I,2)=ALBSNO(I)                                                           
                  RHOST (I,2)=RHOSNO(I)                                                           
                  HCPST (I,2)=HCPICE*RHOSNO(I)/RHOICE                                             
              ENDIF
  350     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C
      IF(NLANDC.GT.0)                                               THEN
C
          DO 400 J=1,IG
          DO 400 I=IL1,IL2
              IF(FC(I).GT.0.)                            THEN  
                  THLQCO(I,J)=THLIQC(I,J)                                               
                  THICCO(I,J)=THICEC(I,J)                                               
                  HCPCO (I,J)=HCPC  (I,J)
              ENDIF                                                  
  400     CONTINUE
C
          DO 450 I=IL1,IL2
              IF(FC(I).GT.0.)                            THEN 
                  IF(SNOCAN(I).GT.0.)      THEN                                                  
                      SUBLC(I)=EVAPC(I)*(CLHMLT+CLHVAP)*SNOCAN(I)/
     1                          (CLHVAP*RAICAN(I)+(CLHVAP+CLHMLT)*
     2                          SNOCAN(I))                                         
                      EVAPC(I)=EVAPC(I)-SUBLC(I)
                  ELSE                                                                    
                      SUBLC(I)=0.0                                                          
                  ENDIF
                  IF(SUBLC(I).GT.0.0) THEN
                      QFCF(I)=QFCF(I)+FC(I)*SUBLC(I)*RHOW
                  ELSE
                      QFCF(I)=QFCF(I)+FC(I)*(1.0-FSVF(I))*SUBLC(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FC(I)*FSVF(I)*SUBLC(I)*RHOW
                  ENDIF
                  IF(EVAPC(I).GT.0.0) THEN
                      QFCL(I)=QFCL(I)+FC(I)*EVAPC(I)*RHOW
                  ELSE
                      QFCL(I)=QFCL(I)+FC(I)*(1.0-FSVF(I))*EVAPC(I)*
     1                        RHOW
                      QFG(I)=QFG(I)+FC(I)*FSVF(I)*EVAPC(I)*RHOW
                  ENDIF
C
                  IF(S(I).GT.0. .OR. SUBLC(I).LT.0.)  THEN                                      
                      SADD(I)=S(I)-SUBLC(I)*RHOW/RHOSNI                                           
                      IF(SADD(I).GT.0.0) THEN                                                
                          IF(SUBLC(I).GT.0.) THEN
                              QFCF(I)=QFCF(I)-FC(I)*FSVF(I)*SUBLC(I)*
     1                                RHOW
                              QFN(I)=QFN(I)+FC(I)*FSVF(I)*SUBLC(I)*
     1                                RHOW
                          ELSE
                              IF(FSVF(I)*SUBLC(I).GT.EVAPCG(I)) THEN
                                  QFN(I)=QFN(I)+FC(I)*EVAPCG(I)*RHOW
                                  QFG(I)=QFG(I)-FC(I)*EVAPCG(I)*RHOW
                              ELSE
                                  PCPN(I)=PCPN(I)-FC(I)*FSVF(I)*
     1                                SUBLC(I)*RHOW
                                  PCPG(I)=PCPG(I)+FC(I)*FSVF(I)*
     1                                SUBLC(I)*RHOW
                              ENDIF
                          ENDIF
                          ST  (I,3)=SADD(I)                                                        
                          TST (I,3)=TS(I)+TFREZ                                                   
                          SUBLC (I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FC(I)*FSVF(I)*S(I)*RHOSNI
                          PCFC(I)=PCFC(I)+FC(I)*FSVF(I)*S(I)*RHOSNI
                          SUBLC (I)=-SADD(I)*RHOSNI/RHOW                                        
                          ST  (I,3)=0.0                                                         
                          TST (I,3)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      ST (I,3)=0.0                                                             
                      TST(I,3)=0.0                                                            
                  ENDIF
C
                  IF(R(I).GT.0. .OR. EVAPC(I).LT.0.)  THEN                                      
                      RADD(I)=R(I)-EVAPC(I)                                                       
                      IF(RADD(I).GT.0.)   THEN                                                
                          IF(EVAPC(I).GT.0.) THEN
                              QFCL(I)=QFCL(I)-FC(I)*FSVF(I)*EVAPC(I)*
     1                                RHOW
                              QFG(I)=QFG(I)+FC(I)*FSVF(I)*EVAPC(I)*
     1                                RHOW
                          ENDIF
                          RT  (I,3)=RADD(I)                                                        
                          TRT (I,3)=TR(I)+TFREZ                                                   
                          EVAPC (I)=0.0                                                      
                      ELSE   
                          PCPG(I)=PCPG(I)-FC(I)*FSVF(I)*R(I)*RHOW
                          PCLC(I)=PCLC(I)+FC(I)*FSVF(I)*R(I)*RHOW
                          EVAPC (I)=-RADD(I)                                                    
                          RT  (I,3)=0.0                                                         
                          TRT (I,3)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RT (I,3)=0.0                                                             
                      TRT(I,3)=0.0                                                            
                  ENDIF         
                  ZPONDT(I,3)=ZPOND (I)                                                            
                  ZSNOWT(I,3)=0.
                  RHOST (I,3)=0.
                  HCPST (I,3)=0.
              ENDIF
  450     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR BARE GROUND.
C
      IF(NLANDG.GT.0)                                               THEN
C
          DO 500 J=1,IG
          DO 500 I=IL1,IL2
              IF(FG(I).GT.0.)                            THEN 
                  THLQGO(I,J)=THLIQG(I,J)                                               
                  THICGO(I,J)=THICEG(I,J)                                               
                  HCPGO (I,J)=HCPG  (I,J)
              ENDIF                                                  
  500     CONTINUE
C
          DO 550 I=IL1,IL2
              IF(FG(I).GT.0.)                            THEN 
                  QFG(I)=QFG(I)+FG(I)*EVAPG(I)*RHOW
                  IF(S(I).GT.0.)                 THEN                                      
                      SADD(I)=S(I)-EVAPG(I)*RHOW/RHOSNI                                           
                      IF(SADD(I).GT.0.0) THEN                                                
                          QFN(I)=QFN(I)+FG(I)*EVAPG(I)*RHOW
                          QFG(I)=QFG(I)-FG(I)*EVAPG(I)*RHOW
                          ST  (I,4)=SADD(I)                                                        
                          TST (I,4)=TS(I)
                          EVAPG (I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FG(I)*S(I)*RHOSNI
                          PCPG(I)=PCPG(I)+FG(I)*S(I)*RHOSNI
                          EVAPG (I)=-SADD(I)*RHOSNI/RHOW                                        
                          ST  (I,4)=0.0                                                         
                          TST (I,4)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      ST (I,4)=0.0                                                             
                      TST(I,4)=0.0                                                            
                  ENDIF
C
                  IF(R(I).GT.0. .OR. EVAPG(I).LT.0.)   THEN                                      
                      RADD(I)=R(I)-EVAPG(I)                                                       
                      IF(RADD(I).GT.0.)   THEN                                                
                          RT  (I,4)=RADD(I)                                                        
                          TRT (I,4)=TR(I)
                          EVAPG (I)=0.0                                                      
                      ELSE                                                                
                          EVAPG (I)=-RADD(I)                                                    
                          RT  (I,4)=0.0                                                         
                          TRT (I,4)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RT (I,4)=0.0                                                             
                      TRT(I,4)=0.0                                                            
                  ENDIF     
                  ZPONDT(I,4)=ZPOND (I)                                                            
                  ZSNOWT(I,4)=0.
                  RHOST (I,4)=0.
                  HCPST (I,4)=0.
              ENDIF
  550     CONTINUE
      ENDIF
C
      RETURN                                                                      
      END
