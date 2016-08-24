      SUBROUTINE TSPOST(TBARPR,GZERO,G12,G23,TPOND,GSNOW,TSNOW,
     1                  QMELTG,GCONST,GCOEFF,
     2                  TBAR,TCTOP,TCBOT,HCP,ZPOND,ZSNOW,TSURF,TBASE,
     3                  HCPSNO,QTRANS,A1,A2,A3,B1,B2,B3,C2,C3,D3,FCT,
     4                  DELZ,DELZW,ILG,IL1,IL2,JL,IG            )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND
C     *                         BOTTOMS OF SOIL LAYERS, AND
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         FIX BUG IN CALCULATION OF FLUXES
C     *                         BETWEEN SOIL LAYERS (PRESENT SINCE 
C     *                         RELEASE OF CLASS VERSION 2.5).
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF TBARPR(I,1).
C     * APR 10/92 - M.LAZARE.   CLASS - VERSION 2.2.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T4LAYR" INTO
C     *                         "TSPREP" AND "TSPOST" AND VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE HEAT FLUXES BETWEEN SNOW/SOIL
C     *                         LAYERS; CONSISTENCY CHECK ON CALCULATED 
C     *                         SURFACE LATENT HEAT OF MELTING/
C     *                         FREEZING; STEP AHEAD SNOW LAYER 
C     *                         TEMPERATURE AND ASSIGN EXCESS HEAT TO
C     *                         MELTING IF NECESSARY; DISAGGREGATE
C     *                         FIRST SOIL LAYER TEMPERATURE INTO
C     *                         PONDED WATER AND SOIL TEMPERATURES;
C     *                         ADD SHORTWAVE RADIATION TRANSMITTED
C     *                         THROUGH SNOWPACK TO HEAT FLUX AT TOP
C     *                         OF FIRST SOIL LAYER; CONVERT LAYER
C     *                         TEMPERATURES TO DEGREES C.
C                                                                                 
C     * OUTPUT ARRAYS.
C
      REAL TBARPR(ILG,IG)
C
      REAL GZERO (ILG),    G12   (ILG),    G23   (ILG),    TPOND (ILG)
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL GSNOW (ILG),    TSNOW (ILG),    QMELTG(ILG)
C
C     * INPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), TCTOP (ILG,IG), TCBOT (ILG,IG),
     1     HCP   (ILG,IG), DELZW (ILG,IG)
C
      REAL ZPOND (ILG),    ZSNOW (ILG),    TSURF (ILG),    TBASE (ILG),
     1     HCPSNO(ILG),    QTRANS(ILG),    A1    (ILG),    A2    (ILG),
     2     A3    (ILG),    B1    (ILG),    B2    (ILG),    B3    (ILG),
     3     C2    (ILG),    C3    (ILG),    D3    (ILG),    FCT   (ILG),
     4     GCONST(ILG),    GCOEFF(ILG)
C
      REAL DELZ  (IG)
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C
      DO 200 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              GSNOLD=GCOEFF(I)*TSURF(I)+GCONST(I)
              GZERO(I)=(TSURF(I)-TSNOW(I)-A1(I)*GSNOLD)/B1(I) 
              G12(I)=(TSURF(I)-TBAR(I,1)-A2(I)*GSNOLD-B2(I)*GZERO(I))/
     1               C2(I)                                  
              G23(I)=(TSURF(I)-TBAR(I,2)-A3(I)*GSNOLD-B3(I)*GZERO(I)-
     1               C3(I)*G12(I))/D3(I)                           
              IF(QMELTG(I).LT.0.)                               THEN
                  GSNOW(I)=GSNOW(I)+QMELTG(I)                                                      
                  QMELTG(I)=0.                                                              
              ENDIF                                                                       
              TSNOW(I)=TSNOW(I)+(GSNOW(I)-GZERO(I))*DELT/
     1                          (HCPSNO(I)*ZSNOW(I))-TFREZ                         
              IF(TSNOW(I).GT.0.)                                THEN
                  QMELTG(I)=QMELTG(I)+TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  GSNOW(I)=GSNOW(I)-TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  TSNOW(I)=0.                                                               
              ENDIF                                                                       
              IF(ZPOND(I).GT.0.)                                THEN 
                  DELZ1=DELZ(1)+ZPOND(I)
                  TPOND(I)=(GZERO(I)/TCTOP(I,1)-G12(I)/TCBOT(I,1))*
     1                     (ZPOND(I)*ZPOND(I)-DELZ1*DELZ1)/(6.0*DELZ1)-
     2                     GZERO(I)*(ZPOND(I)-DELZ1)/(2.0*TCTOP(I,1))+
     3                     TBAR(I,1)-TFREZ
                  TBARPR(I,1)=((HCP(I,1)*DELZW(I,1)+HCPSND*(DELZ(1)-
     1                        DELZW(I,1))+HCPW*ZPOND(I))*TBAR(I,1)-
     2                        HCPW*ZPOND(I)*(TPOND(I)+TFREZ))/
     3                        (HCP(I,1)*DELZW(I,1)+HCPSND*(DELZ(1)-
     4                        DELZW(I,1)))-TFREZ
              ELSE                                                                        
                  TPOND(I)=0.                                                               
                  TBARPR(I,1)=TBAR(I,1)-TFREZ                                             
              ENDIF           
C
              GZERO(I)=GZERO(I)+QTRANS(I)
          ENDIF
  200 CONTINUE
C 
      DO 300 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              TBARPR(I,2)=TBAR(I,2)-TFREZ
              IF(DELZW(I,3).GT.0.0.AND.DELZW(I,3).LT.DELZ(3))  THEN
                  TBARPR(I,3)=(TBAR(I,3)*(HCP(I,3)*DELZW(I,3)+HCPSND*
     1                        (DELZ(3)-DELZW(I,3)))-TBASE(I)*HCPSND*
     2                        (DELZ(3)-DELZW(I,3)))/(HCP(I,3)*
     3                         DELZW(I,3))-TFREZ
              ELSE
                  TBARPR(I,3)=TBAR(I,3)-TFREZ
              ENDIF
          ENDIF
  300 CONTINUE                                                                    
C                                                                                  
      RETURN                                                                      
      END
