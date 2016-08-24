      SUBROUTINE TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                  GCONST,CPHCHG,TSTART,IWATER, 
     2                  TBAR,TCTOP,TCBOT,FCT,ZPOND,DELZ,ISAND,
     3                  ILG,IL1,IL2,JL,IG                        )
C
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND
C     *                         BOTTOMS OF SOIL LAYERS.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         SURFACE TREATED AS WATER ONLY IF
C     *                         ZPOND > 1 MM.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * NOV 28/94 - M. LAZARE.  CLASS - VERSION 2.3.
C     *                         TCSATW,TCSATI DECLARED REAL(16).
C     * APR 10/92 - M. LAZARE.  CLASS - VERSION 2.1.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T3LAYR"
C     *                         INTO "TNPREP" AND "TNPOST" AND
C     *                         VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE COEFFICIENTS FOR GROUND HEAT
C     *                         FLUX, EXPRESSED AS A LINEAR FUNCTION
C     *                         OF SURFACE TEMPERATURE.  COEFFICIENTS
C     *                         ARE CALCULATED FROM LAYER TEMPERATURES,
C     *                         THICKNESSES AND THERMAL CONDUCTIVITIES,
C     *                         ASSUMING A QUADRATIC VARIATION OF
C     *                         TEMPERATURE WITH DEPTH WITHIN EACH
C     *                         SOIL LAYER. SET THE SURFACE LATENT 
C     *                         HEAT OF VAPORIZATION OF WATER AND 
C     *                         THE STARTING TEMPERATURE FOR THE 
C     *                         ITERATION IN "TSOLVC"/"TSOLVE".
C                                                                                 
C     * OUTPUT ARRAYS.
C
      REAL A1    (ILG),    A2    (ILG),    B1    (ILG),
     1     B2    (ILG),    C2    (ILG),    GDENOM(ILG),    GCOEFF(ILG),
     2     GCONST(ILG),    CPHCHG(ILG),    TSTART(ILG)
C
      INTEGER              IWATER(ILG)     
C
C     * INPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), TCTOP (ILG,IG), TCBOT (ILG,IG)
C
      REAL FCT   (ILG),    ZPOND (ILG)
C
      INTEGER              ISAND (ILG,IG)
C
      REAL DELZ  (IG)

      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,TCDRYP,
     1                TCSAPW,TCSAPI,RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,HCPSNI,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,RHOSNI,
     2                TCGLAC,CLHMLT,CLHVAP,THLMIN
C-----------------------------------------------------------------------
C     * INITIALIZATION OF ARRAYS.
C
      DO 100 I=IL1,IL2
          IF(FCT(I).GT.0.)                                          THEN
              DELZ1=DELZ(1)+ZPOND(I)                                                         
              IF(ZPOND(I).GT.1.E-3)                          THEN
                  IWATER(I)=1
              ELSE                                                                        
                  IF(ISAND(I,1).GT.-4)                THEN
                      IWATER(I)=0
                  ELSE
                      IWATER(I)=2
                  ENDIF
              ENDIF    
C
              IF(IWATER(I).EQ.2)                                    THEN
                  CPHCHG(I)=CLHVAP+CLHMLT
              ELSE                                                                        
                  CPHCHG(I)=CLHVAP
              ENDIF                                                                   
C
              A1(I)=DELZ1/(3.0*TCTOP(I,1))
              A2(I)=DELZ1/(2.0*TCTOP(I,1))
              A3=A2(I)                                                                       
              B1(I)=DELZ1/(6.0*TCBOT(I,1))
              B2(I)=DELZ1/(2.0*TCBOT(I,1))+DELZ(2)/(3.0*TCTOP(I,2))
              B3=DELZ1/(2.0*TCBOT(I,1))+DELZ(2)/(2.0*TCTOP(I,2))
              C2(I)=DELZ(2)/(6.0*TCBOT(I,2))
              C3=DELZ(2)/(2.0*TCBOT(I,2))+DELZ(3)/(3.0*TCTOP(I,3))
              GDENOM(I)=A1(I)*(B2(I)*C3-B3*C2(I))-B1(I)*(A2(I)*C3-
     1                  A3*C2(I))                                    
              GCOEFF(I)=(B2(I)*C3-B3*C2(I)-B1(I)*(C3-C2(I)))/GDENOM(I) 
              GCONST(I)=(-TBAR(I,1)*(B2(I)*C3-B3*C2(I))+
     1                    TBAR(I,2)*B1(I)*C3-
     2                    TBAR(I,3)*B1(I)*C2(I))/GDENOM(I)           
              TSTART(I)=TBAR(I,1)
          ENDIF                                                            
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END       
