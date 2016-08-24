      SUBROUTINE GAUSSG(NZERO,F,WT,SIA,RAD,WOCS)
C 
C     * JUL 14/92 - E. CHAN (ADD REAL*8 DECLARATIONS)
C     * THIS ROUTINE CALCULATES THE ROOTS (F) OF THE ORDINARY LEGENDRE
C     * POLYNOMIALS OF ORDER NZERO.  THE FIRST STEP IS TO MAKE AN 
C     * INITIAL GUESS FOR EACH ROOT AND THEN TO USE THE ORDINARY
C     * LEGENDRE ALGORITHM (ORDLEG) AND NEWTONS METHOD TO REFINE
C     * THE SOLUTION UNTIL THE CRITERION XLIM IS SATISFIED. 
C     *    F = COSINE OF COLATITUDE 
C     *   WT = CORRESPONDING GAUSSIAN WEIGHT
C     *  SIA = SINE OF COLATITUDE 
C     *  RAD = COLATITUDE IN RADIANS
C     * WOCS = GAUSSIAN WEIGHT / COS(COLAT)**2
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 F(1),WT(1),SIA(1),RAD(1),WOCS(1)
C-----------------------------------------------------------------------
      XLIM=1.E-13 
      PI = 3.1415926535898
      IR = 2*NZERO
      FI=FLOAT(IR)
      FI1=FI+1. 
      FN=FLOAT(NZERO) 
C 
      DO 20 I=1,NZERO 
      DOT=FLOAT(I-1)
      F(I)=-PI*.5*(DOT+.5)/FN + PI*.5 
      F(I) =  SIN(F(I)) 
   20 CONTINUE
C 
      DN = FI/SQRT(4.*FI*FI-1.) 
      DN1=FI1/SQRT(4.*FI1*FI1-1.) 
      A = DN1*FI
      B = DN*FI1
      IRP = IR + 1
      IRM = IR -1 
C 
      DO 50 I=1,NZERO 
   42 CALL ORDLEG(G,F(I),IR)
      CALL ORDLEG(GM,F(I),IRM)
      CALL ORDLEG(GP,F(I),IRP)
      GT = (A*GP-B*GM)/(F(I)*F(I)-1.) 
      FTEMP = F(I) - G/GT 
      GTEMP = F(I) - FTEMP
      F(I) = FTEMP
      IF( ABS(GTEMP).GT.XLIM) GO TO 42
   50 CONTINUE
C 
      DO 60 I=1,NZERO 
      A=2.*(1.-F(I)**2) 
      CALL ORDLEG(B,F(I),IRM) 
      B = B*B*FI*FI 
      WT(I)=A*(FI-.5)/B 
      RAD(I) =   ACOS(F(I)) 
      SIA(I) =  SIN(RAD(I)) 
      WOCS(I) =WT(I)/SIA(I)**2
   60 CONTINUE
C 
      RETURN
      END
