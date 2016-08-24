      SUBROUTINE CANALB(ALVSCN,ALIRCN,ALVSCS,ALIRCS,TRVSCN,TRIRCN,
     1                  TRVSCS,TRIRCS,
     2                  FCAN,FCANS,ALVSC,ALIRC,AIL,AILS,AILMAX,AILMIN,
     3                  FC,FCS,FSNOW,FSNOWC,FCLOUD,COSZS,
     4                  IC,ICP1,JL,ILG,IL1,IL2)                                                                      
C
C     * NOV 29/94 - M.LAZARE. CALL ABORT CHANGED TO CALL XIT TO RUN ON PC'S
C     *                       (CLASS VER 2.3).  
C     * MAY 06/93 - D.VERSEGHY. EXTENSIVE MODIFICATIONS TO CANOPY
C     *                         ALBEDO LOOPS. 
C     * MAR 03/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CANOPY ALBEDOS AND TRANSMISSIVITIES.
C
C     * OUTPUT ARRAYS.
C
      REAL ALVSCN(ILG),   ALIRCN(ILG),   ALVSCS(ILG),   ALIRCS(ILG),
     1     TRVSCN(ILG),   TRIRCN(ILG),   TRVSCS(ILG),   TRIRCS(ILG)
C
C     * 2-D INPUT ARRAYS.                                                   
C                        
      REAL FCAN  (ILG,IC),           FCANS (ILG,IC),                                                         
     1     ALVSC (ILG,ICP1),         ALIRC (ILG,ICP1),
     2     AIL   (ILG,IC),           AILS  (ILG,IC),                              
     3     AILMAX(ILG,IC),           AILMIN(ILG,IC)
C
C     * 1-D INPUT ARRAYS.
C
      REAL FC    (ILG),   FCS   (ILG),   FSNOW (ILG),   FSNOWC(ILG),
     1     FCLOUD(ILG),   COSZS (ILG)    
                                                                                  
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS7/ CANEXT(4)
 
      DATA ALVSWG,ALIRWG,ALVSWS,ALIRWS,ALVSWC,ALIRWC                              
     1    /  0.03,  0.23,  0.61,  0.38,  0.17,  0.23/
C----------------------------------------------------------------------
C     * ALBEDO CALCULATIONS FOR CANOPY OVER BARE SOIL.
C
C     * NEEDLELEAF AND BROADLEAF TREES.
C
      DO 125 J=1,2
         DO 100 I=IL1,IL2                                               
            IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                   THEN
               ALIRWC=ALIRC(I,J)+0.04
               ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSC(I,J)
               ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRC(I,J)
               FACT=EXP(CANEXT(J)*AIL(I,J))
               ALVSN=(1.0-FACT)*ALVSCX+FACT*ALVSWG                  
               ALIRN=(1.0-FACT)*ALIRCX+FACT*ALIRWG                  
               ALVSCN(I)=ALVSCN(I)+FCAN(I,J)*ALVSN
               ALIRCN(I)=ALIRCN(I)+FCAN(I,J)*ALIRN
            ENDIF
  100    CONTINUE
  125 CONTINUE
C
C     * CROPS AND GRASS.
C
      DO 175 J=3,IC
         DO 150 I=IL1,IL2                                                     
            IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                   THEN
               ALAVG=0.5*(ALVSC(I,J)+ALIRC(I,J))                               
               IF(COSZS(I).LE.0.5) THEN                                           
                  ALCLR=ALAVG/(0.5+COSZS(I))                                     
               ELSE                                                            
                  ALCLR=ALAVG*(0.5+1.0/(1.0+2.0*COSZS(I)))                       
               ENDIF                                                           
               ALTOT=FCLOUD(I)*ALAVG+(1.0-FCLOUD(I))*ALCLR                           
               ALVSN=ALVSC(I,J)                                             
               ALIRN=2.0*ALTOT-ALVSN
               ALIRWC=ALIRC(I,J)+0.04
               ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSN
               ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRN
               FACT=EXP(CANEXT(J)*AIL(I,J))
               ALVSN=(1.0-FACT)*ALVSCX+FACT*ALVSWG                  
               ALIRN=(1.0-FACT)*ALIRCX+FACT*ALIRWG                  
               ALVSCN(I)=ALVSCN(I)+FCAN(I,J)*ALVSN
               ALIRCN(I)=ALIRCN(I)+FCAN(I,J)*ALIRN
             ENDIF   
  150     CONTINUE                                      
  175 CONTINUE
C
C     * TOTAL ALBEDOS.
C
      IPTBAD=0                           
      DO 190 I=IL1,IL2
         IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                      THEN
            ALVSCN(I)=ALVSCN(I)/FC(I)                                                        
            ALIRCN(I)=ALIRCN(I)/FC(I)
         ENDIF
         IF(ALVSCN(I).GT.1. .OR. ALVSCN(I).LT.0.) IPTBAD=I
         IF(ALIRCN(I).GT.1. .OR. ALIRCN(I).LT.0.) IPTBAD=I
  190 CONTINUE                                                                
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSCN(IPTBAD),ALIRCN(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSCN,ALIRCN = ',2F10.5)
         CALL XIT('CANALB',-1)    
      ENDIF                                                                                  
C----------------------------------------------------------------------
C     * ALBEDO CALCULATIONS FOR CANOPY OVER SNOW.
C
C     * NEEDLELEAF AND BROADLEAF TREES.
C
      DO 225 J=1,2
         DO 200 I=IL1,IL2                                               
            IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                 THEN
               ALIRWC=ALIRC(I,J)+0.04
               ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSC(I,J)
               ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRC(I,J)
               FACT=EXP(CANEXT(J)*AILS(I,J))
               ALVSS=(1.0-FACT)*ALVSCX+FACT*ALVSWS                 
               ALIRS=(1.0-FACT)*ALIRCX+FACT*ALIRWS                  
               ALVSCS(I)=ALVSCS(I)+FCANS(I,J)*ALVSS
               ALIRCS(I)=ALIRCS(I)+FCANS(I,J)*ALIRS
            ENDIF
  200    CONTINUE
  225 CONTINUE
C
C     * CROPS AND GRASS.
C
      DO 275 J=3,IC
         DO 250 I=IL1,IL2                                                     
            IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                  THEN
               ALIRWC=ALIRC(I,J)+0.04
               ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSC(I,J)
               ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRC(I,J)
               FACT=EXP(CANEXT(J)*AILS(I,J))
               ALVSS=(1.0-FACT)*ALVSCX+FACT*ALVSWS                 
               ALIRS=(1.0-FACT)*ALIRCX+FACT*ALIRWS                  
               ALVSCS(I)=ALVSCS(I)+FCANS(I,J)*ALVSS
               ALIRCS(I)=ALIRCS(I)+FCANS(I,J)*ALIRS
            ENDIF 
  250     CONTINUE                                      
  275 CONTINUE
C
C     * TOTAL ALBEDOS.
C
      IPTBAD=0                           
      DO 290 I=IL1,IL2
         IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                     THEN
            ALVSCS(I)=ALVSCS(I)/FCS(I)                                                        
            ALIRCS(I)=ALIRCS(I)/FCS(I)
         ENDIF
         IF(ALVSCS(I).GT.1. .OR. ALVSCS(I).LT.0.) IPTBAD=I
         IF(ALIRCS(I).GT.1. .OR. ALIRCS(I).LT.0.) IPTBAD=I
  290 CONTINUE                                                                
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6200) IPTBAD,JL,ALVSCS(IPTBAD),ALIRCS(IPTBAD)
 6200    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSCS,ALIRCS = ',2F10.5)
         CALL XIT('CANALB',-2)    
      ENDIF                                                                                  
C-----------------------------------------------------------------------
C     * TRANSMISSIVITY CALCULATIONS FOR CANOPY OVER BARE SOIL.
C
C     * NEEDLELEAF TREES.
C
      J=1
      DO 300 I=IL1,IL2                                                                                  
         IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                      THEN                                                          
            TRCLRS=EXP(-0.4*AIL(I,J)/COSZS(I))                                    
            TRCLDS=0.30*EXP(-0.4*AIL(I,J)/0.9659)+0.50*EXP(-0.4*               
     1             AIL(I,J)/0.7071)+0.20*EXP(-0.4*AIL(I,J)/0.2588)   
            TRCLRT=EXP(-0.3*AIL(I,J)/COSZS(I))                                    
            TRCLDT=0.30*EXP(-0.3*AIL(I,J)/0.9659)+0.50*EXP(-0.3*              
     1             AIL(I,J)/0.7071)+0.20*EXP(-0.3*AIL(I,J)/0.2588)   
            TRVS  =FCLOUD(I)*TRCLDS+(1.0-FCLOUD(I))*TRCLRS
            TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
            TRIR  = 2.*TRTOT-TRVS
            TRVSCN(I)=TRVSCN(I)+FCAN(I,J)*TRVS
            TRIRCN(I)=TRIRCN(I)+FCAN(I,J)*TRIR
         ENDIF
  300 CONTINUE
C
C     * BROADLEAF TREES.
C
      J=2
      DO 325 I=IL1,IL2                                                                                  
         IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                      THEN
            TRCLRS=MIN(EXP(-0.7*AIL(I,J)),EXP(-0.4/COSZS(I)))                   
            TRCLDS=0.30*MIN(EXP(-0.7*AIL(I,J)),EXP(-0.4/0.9659))             
     1            +0.50*MIN(EXP(-0.7*AIL(I,J)),EXP(-0.4/0.7071))              
     2            +0.20*MIN(EXP(-0.7*AIL(I,J)),EXP(-0.4/0.2588))              
            TRCLRT=MIN(EXP(-0.4*AIL(I,J)),EXP(-0.4/COSZS(I)))                   
            TRCLDT=0.30*MIN(EXP(-0.4*AIL(I,J)),EXP(-0.4/0.9659))+            
     1             0.50*MIN(EXP(-0.4*AIL(I,J)),EXP(-0.4/0.7071))+              
     2             0.20*MIN(EXP(-0.4*AIL(I,J)),EXP(-0.4/0.2588))               
            TRVS  =FCLOUD(I)*TRCLDS+(1.0-FCLOUD(I))*TRCLRS
            TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
            TRIR  = 2.*TRTOT-TRVS
            TRVSCN(I)=TRVSCN(I)+FCAN(I,J)*TRVS
            TRIRCN(I)=TRIRCN(I)+FCAN(I,J)*TRIR
         ENDIF
  325 CONTINUE
C
C     * CROPS AND GRASS.
C
      DO 340 J=3,IC
      DO 340 I=IL1,IL2                                                                                  
         IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                      THEN
            TRCLRS=EXP(-0.5*AIL(I,J)/COSZS(I))                                    
            TRCLDS=0.30*EXP(-0.5*AIL(I,J)/0.9659)+0.50*EXP(-0.5*               
     1             AIL(I,J)/0.7071)+0.20*EXP(-0.5*AIL(I,J)/0.2588)
            TRCLRT=EXP(-0.4*AIL(I,J)/COSZS(I))                                    
            TRCLDT=0.30*EXP(-0.4*AIL(I,J)/0.9659)+0.50*EXP(-0.4*              
     1             AIL(I,J)/0.7071)+0.20*EXP(-0.4*AIL(I,J)/0.2588)                
            TRVS  =FCLOUD(I)*TRCLDS+(1.0-FCLOUD(I))*TRCLRS
            TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
            TRIR  = 2.*TRTOT-TRVS
            TRVSCN(I)=TRVSCN(I)+FCAN(I,J)*TRVS
            TRIRCN(I)=TRIRCN(I)+FCAN(I,J)*TRIR
         ENDIF
  340 CONTINUE
C
C     * TOTAL TRANSMISSIVITIES.
C
      IPTBAD=0
      DO 350 I=IL1,IL2
         IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                      THEN
            TRVSCN(I)=TRVSCN(I)/FC(I)
            TRIRCN(I)=TRIRCN(I)/FC(I)
            TRVSCN(I)=MIN( TRVSCN(I), 0.90*(1.0-ALVSCN(I)) )
            TRIRCN(I)=MIN( TRIRCN(I), 0.90*(1.0-ALIRCN(I)) )
         ENDIF
         IF(TRVSCN(I).GT.1. .OR. TRVSCN(I).LT.0.) IPTBAD=I
         IF(TRIRCN(I).GT.1. .OR. TRIRCN(I).LT.0.) IPTBAD=I
  350 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6300) IPTBAD,JL,TRVSCN(IPTBAD),TRIRCN(IPTBAD)
 6300    FORMAT('0AT (I,J)= (',I3,',',I3,'), TRVSCN,TRIRCN = ',2F10.5)
         CALL XIT('CANALB',-3)    
      ENDIF                                                                                  
C-----------------------------------------------------------------------
C     * TRANSMISSIVITY CALCULATIONS FOR CANOPY OVER SNOW.
C
C     * NEEDLELEAF TREES.
C
      J=1
      DO 400 I=IL1,IL2                                                                                  
         IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                     THEN                                                          
            TRCLRS=EXP(-0.4*AILS(I,J)/COSZS(I))                                    
            TRCLDS=0.30*EXP(-0.4*AILS(I,J)/0.9659)+0.50*EXP(-0.4*               
     1             AILS(I,J)/0.7071)+0.20*EXP(-0.4*AILS(I,J)/0.2588)   
            TRCLRT=EXP(-0.3*AILS(I,J)/COSZS(I))                                    
            TRCLDT=0.30*EXP(-0.3*AILS(I,J)/0.9659)+0.50*EXP(-0.3*              
     1             AILS(I,J)/0.7071)+0.20*EXP(-0.3*AILS(I,J)/0.2588)   
            TRVS  =FCLOUD(I)*TRCLDS+(1.0-FCLOUD(I))*TRCLRS
            TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
            TRIR  = 2.*TRTOT-TRVS
            TRVSCS(I)=TRVSCS(I)+FCANS(I,J)*TRVS
            TRIRCS(I)=TRIRCS(I)+FCANS(I,J)*TRIR
         ENDIF
  400 CONTINUE
C
C     * BROADLEAF TREES.
C
      J=2
      DO 425 I=IL1,IL2                                                                                  
         IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                     THEN
            TRCLRS=MIN(EXP(-0.7*AILS(I,J)),EXP(-0.4/COSZS(I)))                   
            TRCLDS=0.30*MIN(EXP(-0.7*AILS(I,J)),EXP(-0.4/0.9659))             
     1            +0.50*MIN(EXP(-0.7*AILS(I,J)),EXP(-0.4/0.7071))              
     2            +0.20*MIN(EXP(-0.7*AILS(I,J)),EXP(-0.4/0.2588))              
            TRCLRT=MIN(EXP(-0.4*AILS(I,J)),EXP(-0.4/COSZS(I)))                   
            TRCLDT=0.30*MIN(EXP(-0.4*AILS(I,J)),EXP(-0.4/0.9659))+            
     1             0.50*MIN(EXP(-0.4*AILS(I,J)),EXP(-0.4/0.7071))+              
     2             0.20*MIN(EXP(-0.4*AILS(I,J)),EXP(-0.4/0.2588))               
            TRVS  =FCLOUD(I)*TRCLDS+(1.0-FCLOUD(I))*TRCLRS
            TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
            TRIR  = 2.*TRTOT-TRVS
            TRVSCS(I)=TRVSCS(I)+FCANS(I,J)*TRVS
            TRIRCS(I)=TRIRCS(I)+FCANS(I,J)*TRIR
         ENDIF
  425 CONTINUE
C
C     * CROPS AND GRASS.
C
      DO 440 J=3,IC
      DO 440 I=IL1,IL2                                                                                  
         IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                     THEN
            TRCLRS=EXP(-0.5*AILS(I,J)/COSZS(I))                                    
            TRCLDS=0.30*EXP(-0.5*AILS(I,J)/0.9659)+0.50*EXP(-0.5*               
     1             AILS(I,J)/0.7071)+0.20*EXP(-0.5*AILS(I,J)/0.2588)
            TRCLRT=EXP(-0.4*AILS(I,J)/COSZS(I))                                    
            TRCLDT=0.30*EXP(-0.4*AILS(I,J)/0.9659)+0.50*EXP(-0.4*              
     1             AILS(I,J)/0.7071)+0.20*EXP(-0.4*AILS(I,J)/0.2588)                
            TRVS  =FCLOUD(I)*TRCLDS+(1.0-FCLOUD(I))*TRCLRS
            TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
            TRIR  = 2.*TRTOT-TRVS
            TRVSCS(I)=TRVSCS(I)+FCANS(I,J)*TRVS
            TRIRCS(I)=TRIRCS(I)+FCANS(I,J)*TRIR
         ENDIF
  440 CONTINUE
C
C     * TOTAL TRANSMISSIVITIES AND CONSISTENCY CHECKS.
C
      IPTBAD=0
      JPTBAD=0
      DO 450 I=IL1,IL2
         IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                     THEN
            TRVSCS(I)=TRVSCS(I)/FCS(I)
            TRIRCS(I)=TRIRCS(I)/FCS(I)
            TRVSCS(I)=MIN( TRVSCS(I), 0.90*(1.0-ALVSCS(I)) )
            TRIRCS(I)=MIN( TRIRCS(I), 0.90*(1.0-ALIRCS(I)) )
         ENDIF
         IF(TRVSCS(I).GT.1. .OR. TRVSCS(I).LT.0.) IPTBAD=I
         IF(TRIRCS(I).GT.1. .OR. TRIRCS(I).LT.0.) IPTBAD=I
         IF((1.-ALVSCN(I)-TRVSCN(I)).LT.0.)     THEN
            JPTBAD=1000+I
            JPTBDI=I
         ENDIF  
         IF((1.-ALVSCS(I)-TRVSCS(I)).LT.0.)     THEN
            JPTBAD=2000+I
            JPTBDI=I
         ENDIF
         IF((1.-ALIRCN(I)-TRIRCN(I)).LT.0.)     THEN
            JPTBAD=3000+I
            JPTBDI=I
         ENDIF
         IF((1.-ALIRCS(I)-TRIRCS(I)).LT.0.)     THEN
            JPTBAD=4000+I
            JPTBDI=I
         ENDIF
  450 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6400) IPTBAD,JL,TRVSCS(IPTBAD),TRIRCS(IPTBAD)
 6400    FORMAT('0AT (I,J)= (',I3,',',I3,'), TRVSCS,TRIRCS = ',2F10.5)
         CALL XIT('CANALB',-4)    
      ENDIF
C
      IF(JPTBAD.NE.0) THEN
         WRITE(6,6500) JPTBDI,JL,JPTBAD
 6500    FORMAT('0AT (I,J)= (',I3,',',I3,'), JPTBAD =  ',I5)
         CALL XIT('CANALB',-5)    
      ENDIF                      
                                                                                  
      RETURN                                                                      
      END
