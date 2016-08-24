      SUBROUTINE TSPREP(A1,A2,A3,B1,B2,B3,C2,C3,D3,GDENOM,GCOEFF,
     1                  GCONST,CPHCHG,TSTART,IWATER, 
     2                  TBAR,TCTOP,TCBOT,FCT,ZPOND,ZSNOW,TSNOW,TCSNOW,
     3                  DELZ,ILG,IL1,IL2,JL,IG                        )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND 
C     *                         BOTTOMS OF SOIL LAYERS.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * NOV 28/94 - M. LAZARE.  CLASS - VERSION 2.3.
C     *                         TCSATW,TCSATI DECLARED REAL(16).
C     * APR 10/92 - M. LAZARE.  CLASS - VERSION 2.1.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T4LAYR" 
C     *                         INTO "TSPREP" AND "TSPOST" AND
C     *                         VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE COEFFICIENTS FOR GROUND HEAT
C     *                         FLUX, EXPRESSED AS A LINEAR FUNCTION OF
C     *                         SURFACE TEMPERATURE. COEFFICIENTS ARE
C     *                         CALCULATED FROM LAYER TEMPERATURES, 
C     *                         THICKNESSES AND THERMAL CONDUCTIVITIES,
C     *                         ASSUMING A QUADRATIC VARIATION OF
C     *                         TEMPERATURE WITH DEPTH WITHIN EACH
C     *                         SOIL/SNOW LAYER. SET THE SURFACE 
C     *                         LATENT HEAT OF VAPORIZATION OF WATER
C     *                         AND THE STARTING TEMPERATURE FOR THE
C     *                         ITERATION IN "TSOLVC"/"TSOLVE".
C                                                                                 
C     * OUTPUT ARRAYS.
C
      REAL A1    (ILG),    A2    (ILG),    A3    (ILG),
     1     B1    (ILG),    B2    (ILG),    B3    (ILG),    C2    (ILG),
     2     C3    (ILG),    D3    (ILG),    GDENOM(ILG),    GCOEFF(ILG),
     3     GCONST(ILG),    CPHCHG(ILG),    TSTART(ILG)
C
      INTEGER              IWATER(ILG)     
C
C     * INPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), TCTOP (ILG,IG), TCBOT(ILG,IG)
C
      REAL FCT   (ILG),    ZPOND (ILG),    ZSNOW (ILG),    TSNOW (ILG),
     1     TCSNOW(ILG)     

C
      REAL DELZ  (IG)
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * INITIALIZATION OF WORK ARRAYS.
C
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              DELZ1=DELZ(1)+ZPOND(I)                                                         
              A1(I)=ZSNOW(I)/(3.0*TCSNOW(I))
              A2(I)=ZSNOW(I)/(2.0*TCSNOW(I))
              A3(I)=A2(I)                                                                       
              A4=A2(I)                                                                       
              B1(I)=ZSNOW(I)/(6.0*TCSNOW(I))
              B2(I)=ZSNOW(I)/(2.0*TCSNOW(I))+DELZ1/(3.0*TCTOP(I,1))
              B3(I)=ZSNOW(I)/(2.0*TCSNOW(I))+DELZ1/(2.0*TCTOP(I,1))
              B4=B3(I)                                                                       
              C2(I)=DELZ1/(6.0*TCBOT(I,1))
              C3(I)=DELZ1/(2.0*TCBOT(I,1))+DELZ(2)/(3.0*TCTOP(I,2))
              C4=DELZ1/(2.0*TCBOT(I,1))+DELZ(2)/(2.0*TCTOP(I,2))
              D3(I)=DELZ(2)/(6.0*TCBOT(I,2))
              D4=DELZ(2)/(2.0*TCBOT(I,2))+DELZ(3)/(3.0*TCTOP(I,3))
              GDENOM(I)=A1(I)*(B2(I)*(C3(I)*D4-C4*D3(I))-
     1                         C2(I)*(B3(I)*D4-B4*D3(I)))
     2                 -B1(I)*(A2(I)*(C3(I)*D4-C4*D3(I))-
     3                         C2(I)*(A3(I)*D4-A4*D3(I)))                                               
              GCOEFF(I)=(B2(I)*(C3(I)*D4-C4*D3(I))-
     1                   C2(I)*(B3(I)*D4-B4*D3(I))-
     2                   B1(I)*(C3(I)*D4-C4*D3(I)-
     2                   C2(I)*(D4-D3(I)))) /GDENOM(I)                                                     
              GCONST(I)=(-TSNOW(I)*(B2(I)*(C3(I)*D4-C4*D3(I))-
     1                   C2(I)*(B3(I)*D4-B4*D3(I)))+TBAR(I,1)*B1(I)*            
     2                  (C3(I)*D4-C4*D3(I))-TBAR(I,2)*B1(I)*C2(I)*D4+
     3                  TBAR(I,3)*B1(I)*C2(I)*D3(I))/GDENOM(I)
              IWATER(I)=2                                                                    
              CPHCHG(I)=CLHVAP+CLHMLT
              TSTART(I)=TSNOW(I)
          ENDIF
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END
