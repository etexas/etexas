      SUBROUTINE SEDMPD
C
C *** ************************************************************** ***
C *** *                                                            * ***
C *** *  COPYRIGHT (C) 1989 by The University of Texas at Austin   * ***
C *** *                                                            * ***
C *** * Permission is hereby granted to use, modify, copy, and     * ***
C *** * distribute this software and its documentation for any     * ***
C *** * purpose only without profit, provided that the above       * ***
C *** * Copyright Notice appears in all copies and that both the   * ***
C *** * Copyright Notice and this Permission Notice appears in     * ***
C *** * every copy of supporting documentation.  No title to nor   * ***
C *** * ownership of the software is transferred hereby.  The name * ***
C *** * of The University of Texas at Austin shall not be used in  * ***
C *** * advertising or publicity related to the distribution of    * ***
C *** * the software without specific, written, prior permission.  * ***
C *** * This software is provided as-delivered without expressed   * ***
C *** * or implied warranty.  The University of Texas at Austin    * ***
C *** * makes no representation about the suitability of this      * ***
C *** * software for any purpose and accepts no responsibility for * ***
C *** * its use.                                                   * ***
C *** *                                                            * ***
C *** ************************************************************** ***
C *** *                                                            * ***
C *** * This program is free software; you can redistribute it     * ***
C *** * and/or modify it under the terms of the GNU General Public * ***
C *** * License as published by the Free Software Foundation;      * ***
C *** * either version 2 of the License, or (at your option) any   * ***
C *** * later version.                                             * ***
C *** *                                                            * ***
C *** * This program is distributed in the hope that it will be    * ***
C *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
C *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
C *** * PURPOSE.  See the GNU General Public License for more      * ***
C *** * details.                                                   * ***
C *** *                                                            * ***
C *** * You should have received a copy of the GNU General Public  * ***
C *** * License along with this program; if not, write to the Free * ***
C *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
C *** * Floor, Boston, MA 02110-1301, USA.                         * ***
C *** *                                                            * ***
C *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
C *** *                                                            * ***
C *** ************************************************************** ***
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*6 FMT1
      PARAMETER  (FMT1='(/(A))')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXRD'
      INCLUDE 'CMTXSR'
      INCLUDE 'CMTXHL'
      INCLUDE 'CMTXAR'
      INCLUDE 'CMNEMA'
      CHARACTER*10 EDREQ,FMT3,EDREQU
      CHARACTER*80 TLINE,TDAT1,HELPFC(15),OLTX(0:5)*3,MLINEU,TLINEU
      CHARACTER ALT(8)*62,ALT1(3)*8,MKFMT*26,FMT2*8,DASH*40
      CHARACTER TTTTT*20,DETLEG*2,BLANK*5,DETLEGU*2
      LOGICAL EFV,FFDEC,IL,DIGREF,EXPDF,FLAG,MFLAG,EUALLT,TXDSS
      SAVE MFLAG
      INTEGER   NFS1         ,NFS2     ,NCHLP    ,NCHEFC    ,
     1          LOOPLG
      DIMENSION NFS1(0:NLGP1),NFS2(NAL),NCHLP(19),NCHEFC(15),
     1          LOOPLG(10)
      INTEGER   I,I1,ICODET,IFI,II,IJ,ILE,ILNB,INO,IOFFT,IOFFU,IPCT,
     1          IREC,IT1,IT2,IT3,IT4,ITA,ITAM1,ITZ,J,JJ,K,NC,NC1,NC2,
     2          NCALT2,NCBLAN,NCLN,NCML,NCML1,NCOVL,NCSDR,NCSS,NCSS1,
     3          NCTD1,NCTD2,NCTD3,NCTL,NCTX,NF,NF1,NFL,NFST,NLHEF,
     4          NLHEFC,NLHET,NLSTL,NN,NN1,NSDR,NSPL,NT1,NT2
      REAL      RNGT12,RNGT13,RNGT22,RNGT23,RNGT24,RNGT25,X,X1,X2,X3,X4,
     1          X5,XSUM
      SAVE NLHEF,NLHET,NCHLP,NCHEFC,NLHEFC,HELPFC
      INTEGER              NDFIO
      EQUIVALENCE (NOUT(2),NDFIO)
      INTEGER              NRF          ,NSFIO          ,NPHA
      EQUIVALENCE (MOUT(1),NRF),(MOUT(2),NSFIO),(MOUT(9),NPHA)
      INTEGER              NOLP          ,ICYC           ,NDET
      EQUIVALENCE (MOUT(8),NOLP),(MOUT(8),ICYC),(MOUT(13),NDET)
      DATA NLHEF / -1 /
      DATA TTTTT / 'TTTTTTTTTT' /
      DATA FMT2,FMT3 / '(A,I1,A)','(A,F4.1,A)' /
      DATA (ALT(I),I=3,8) / ' ',' ',' ',' ','HELP','NONE' /
      DATA (ALT1(I),I=1,3) / '$"only".','only','NONE' /
      DATA DASH / '----------------------------------------' /
C
C  LOOPLG - LEG THAT TEXAS DIAMOND LOOP DETECTORS ARE ON
C
      DATA (LOOPLG(I),I=1,10) / 5,6,6,7,7,0,0,2,3,3 /
      DATA BLANK / ' ' /
      IOFORM='(A)'
      ITA=ICHAR( 'A' )
      ITZ=ICHAR( 'Z' )
      ITAM1=ITA-1
      FLAG=.FALSE.
      IF(REVISE)FLAG=.TRUE.
      NF1=NFIELD
      NF=ABS(NFIELD)
      NC=0
      DO 6800 I=1,NF
 6800 NC=NC+NFS(I)
      NN=NPHA
      NSPL=1
      IF(ICODE.EQ.48)THEN
C
C ----- SIGNAL SEQ. DATA FOR TEXAS DIAMOND CONTROLLER
C
        ICODE=28
        TXDSS=.TRUE.
      ELSE
        TXDSS=.FALSE.
      END IF
      IF(ICODE.EQ.23)NSPL=8
C
C ----- IF NSPL=8, MAKE ROOM FOR REF. DATA AFTER FIELDS
C
      CALL PRFLDS(TDAT1,NF,NFS,1,0,NSPL,NCTD3,I,I)
C
C ----- READ EDIT HELP TEXT
C
      IT1=NHMAX(2)+1
      IF(ICODE.EQ.33)THEN
        IOFFT=NPHAST
        OLTX(0)=ALT(5)
        DO 6810 I=1,4+1
        OLTX(I)=PHOLTX(I+IOFFT)
 6810   CONTINUE
      ELSE
        PHOLTX(0)=ALT(5)
        IOFFT=0
      END IF
      IF(ICODE.EQ.9)THEN
        PHOLTX(1)='1'
        PHOLTX(2)='2'
        NLHET=-1
        CALL RDHLP('gdver4',NHMAX(4),ALT(2),HLP(IT1),
     1             NCHLP(IT1),NLHET)
        NLHEF=-1
        GO TO 6830
      END IF
      IF(ICODE.EQ.16)THEN
        NCSDR=0
        DO 6814 I=1,NFIELD
        NCSDR=NCSDR+NFS(I)
 6814   CONTINUE
        READ(NDFIO,'(I4)',REC=NOUT(4))NSDR
        NOUT(4)=NOUT(4)+1
        DO 6815 I=1,NSDR
        WRITE(PHOLTX(I),'(I1)')I
 6815   CONTINUE
        NLHET=-1
        CALL RDHLP('gdverb',NHMAX(4),ALT(2),HLP(IT1),
     1             NCHLP(IT1),NLHET)
        NLHEF=-1
        GO TO 6830
      END IF
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        NLHET=-1
        CALL RDHLP('gdver3',NHMAX(4),ALT(2),HLP(IT1),
     1             NCHLP(IT1),NLHET)
        NLHEF=-1
        GO TO 6830
      END IF
      IF(NEMA .AND. (ICODE.EQ.28))THEN
        CALL RDHLP('gdver7',NHMAX(3),ALT(2),HLP(IT1),
     1             NCHLP(IT1),NLHEF)
        GO TO 6820
      END IF
      IF((NEMA .AND. (ICODE.EQ.32)) .OR. (ICODE.EQ.47) .OR. TXDSS)THEN
        CALL RDHLP('gdver7',NHMAX(3),ALT(2),HLP(IT1),
     1             NCHLP(IT1),NLHEF)
        GO TO 6820
      END IF
      CALL RDHLP('gdver2',NHMAX(3),ALT(2),HLP(IT1),
     1           NCHLP(IT1),NLHEF)
 6820 CONTINUE
      NLHET=NLHEF
 6830 CONTINUE
      ALT(1)='$a '//HLP(IT1)
C
C ----- READ FIELD COLUMN EDIT HELP
C
      NLHEFC=-1
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        CALL RDHLP('gdver9',15,ALT(5),HELPFC,NCHEFC,NLHEFC)
      ELSE
        IF(ICODE.EQ.9)THEN
          CALL RDHLP('gdver6',15,ALT(5),HELPFC,NCHEFC,NLHEFC)
        ELSE
          IF(ICODE.EQ.16)THEN
            CALL RDHLP('gdvera',15,ALT(5),HELPFC,NCHEFC,NLHEFC)
          ELSE
            CALL RDHLP('gdver8',15,ALT(5),HELPFC,NCHEFC,NLHEFC)
          END IF
        END IF
      END IF
      ALT(6)='$a '//HELPFC(1)
      IF(((NF1.LT.0) .AND. (ICODE.NE.33)) .OR. (ICODE.EQ.29) .OR.
     1                                         (ICODE.EQ.49))THEN
C
C ----- PREPARE TO DISPLAY LEG AND LANE NUMBERS ABOVE DATA FIELDS
C
        IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
          NT1=NCTD3+7
          MLINE(NPHOL+2)(1:NT1)=' '
          MLINE(NPHOL+3)(1:NT1)=' '
          NFST=1
        ELSE
          NT1=1
          IF(ICODE.EQ.31)THEN
            NFST=1
          ELSE
            NFST=NFS(1)
          END IF
        END IF
        DO 6900 I=1,NAL
        NFS2(I)=NFST
 6900   CONTINUE
        MLINE(NPHOL+2)(NT1:)=' Leg: '
        MLINE(NPHOL+3)(NT1:)='Lane: '
        NT1=NT1+6
        NT2=NT1
        IF(DIA)THEN
          NFL=0
          NLSTL=NLGP1
        ELSE
          NFL=1
          NLSTL=NLEGS
        END IF
        DO 7000 I=NFL,NLSTL
        IT1=NINLNS(I)
        IF(IT1.GT.0)THEN
          CALL PRLNS(MLINE(NPHOL+3)(NT2:),IT1,NFS2,1,0,1,
     1               NCTD1,I,I)
          NT2=NT2+NCTD1
          NFS1(I)=(NFST+1)*NINLNS(I)-1
        ELSE
          NFS1(I)=-1
        END IF
 7000   CONTINUE
        CALL PRLEGS(MLINE(NPHOL+2)(NT1:),NLSTL,NFS1(1),NFL,0,1,
     1              NCTD1,I,I)
        NCOVL=NCTD1
        NCTD1=NCTD1+NT1-1
      END IF
      IF(ICODE.EQ.9)THEN
        IREC=NOUT(7)
        NN=2
        GO TO 7710
      END IF
      IF(ICODE.EQ.16)THEN
        IREC=NOUT(4)-1
        NN=NSDR
        GO TO 7710
      END IF
      IF(((ICODE.GE.23) .AND. (ICODE.LE.27)) .OR.
     1   (ICODE.EQ.32)  .OR.  (ICODE.EQ.35)  .OR.
     2   (ICODE.EQ.47)                           )IREC=MOUT(7)-1
      IF(ICODE.EQ.28)THEN
        IREC=MOUT(10)-1
        MLINE(NPHOL+1)='Green interval sequence data (for reference '//
     1                  'only):'
C
C ----- PREPARE TO DISPLAY LANE CONTROL AND MOVEMENT CODE DATA
C
        MFLAG=.FALSE.
        NCTD2=6
        MLINE(NPHOL+4)(1:NCTD2)='  MC: '
        MLINE(NPHOL+5)(1:NCTD2)='  LC: '
        IT2=7
        READ(NSFIO,IOFORM,REC=MOUT(6)+3)HELP(16)
        IT3=1
        IT4=2
        DO 7500 I=NFL,NLSTL
        DO 7400 J=1,NINLNS(I)
        NCTD2=NCTD2+3
        MLINE(NPHOL+4)(IT2:NCTD2)=MVIL(J,I)(1:2)
        MLINE(NPHOL+5)(IT2:NCTD2)=HELP(16)(IT3:IT4)
        IT2=IT2+3
        IT3=IT3+2
        IT4=IT3+1
C
C ----- IS A SECOND LINE NEEDED FOR MOVEMENT CODES ?
C
        IF(MVIL(J,I)(3:3).NE.' ')MFLAG=.TRUE.
 7400   CONTINUE
 7500   CONTINUE
        NCTD2=NCTD2-1
        IF(MFLAG)THEN
C
C ----- SECOND LINE OF MOVEMENT CODE
C
          IT2=7
          TLINE(1:6)=' '
          DO 7520 I=NFL,NLSTL
          DO 7510 J=1,NINLNS(I)
          TLINE(IT2:IT2+1)=MVIL(J,I)(3:)
          IT2=IT2+2
          TLINE(IT2:IT2)=' '
          IT2=IT2+1
 7510     CONTINUE
 7520     CONTINUE
          IT2=IT2-2
          MLINE(NPHOL+NPHOL+6)=TLINE(1:IT2)
        END IF
      END IF
      IF((ICODE.EQ.28) .AND. NEMA)NN=NN+NOLP
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        NN=NDET
        IREC=MOUT(12)-1
        MLINE(NPHOL+NPHOL+16)=MLINE(NPHOL+2)
        MLINE(NPHOL+NPHOL+17)=MLINE(NPHOL+3)
        IF(ICODE.EQ.29)THEN
          RANGE(2,1)=NLEGS
          ALT(4)='ADD'
        ELSE
          ALT(4)=' '
        END IF
        GO TO  7600
      END IF
      IF((ICODE.EQ.30) .OR. (ICODE.EQ.34))THEN
        NN=NPHA
        ALT(4)=' '
        IREC=MOUT(14)-1
        NCML1=ILNB( MLINE(NPHOL+1) )
        NCML=ILNB( MLINE(NPHOL+2) )
        IF(NCML.LT.29)NCML=29
        GO TO 8020
      END IF
      IF(ICODE.EQ.31)THEN
        IREC=MOUT(11)-1
        NCSS1=ILNB( MLINE(NPHOL+1) )
        NCSS=ILNB( MLINE(NPHOL+2) )
      END IF
      IF(ICODE.EQ.33)THEN
        NN=NOLP
        IREC=MOUT(11)+3
      END IF
      IF(ICODE.EQ.35)THEN
        NN=NPHAST
      END IF
 7600 CONTINUE
      IF(ICODE.EQ.28)GO TO 8020
      IF((.NOT.REVISE) .AND.
     1   ((ICODE.EQ.31) .OR. (ICODE.EQ.35)))GO TO 8020
      IT1=NFS(1)+1
      IF(ICODE.EQ.28)GO TO 8020
      IOFFU=0
      DO 7700 I=1,NN
      READ(NSFIO,IOFORM,REC=IREC+I+IOFFU)MLINE(I)
      IF(ICODE.EQ.35)CALL CHARSH(MLINE(I),NCVDF1)
C
C ----- THERE ARE BLANK LINES IN FILE FOR UNUSED PHASES UP TO 8
C
      IF(NEMA .AND. (ICODE.EQ.28) .AND. (I.EQ.NPHA))IOFFU=8-NPHA
 7700 CONTINUE
 7710 CONTINUE
 8020 WRITE(*,IOFORM)' '
 8025 IF((ICODE.EQ.30) .OR. (ICODE.EQ.34))THEN
C
C ----- DISPLAY DETECTOR REF. DATA
C
        WRITE(*,IOFORM)' '
        WRITE(*,IOFORM)MLINE(NPHOL+1)(1:NCML1)
        DO 8027 JJ=NPHOL+2,NPHOL+NDET+3
        WRITE(*,IOFORM)MLINE(JJ)(1:NCML)
 8027   CONTINUE
        WRITE(*,IOFORM)' '
        GO TO 8033
      END IF
      IF(ICODE.EQ.31)THEN
C
C ----- DISPLAY GREEN SIGNAL SEQUENCE REF. DATA
C
        WRITE(*,IOFORM)' '
        WRITE(*,IOFORM)MLINE(NPHOL+1)(1:NCSS1)
        DO 8031 JJ=NPHOL+2,NPHOL+5+NPHA+NOLP
        WRITE(*,IOFORM)MLINE(JJ)(1:NCSS)
        IF(MFLAG.AND.JJ.EQ.NPHOL+4)THEN
C
C ----- SECOND LINE OF MOVEMENT CODES
C
          WRITE(*,IOFORM)MLINE(NPHOL+NPHOL+6)(1:NCSS)
        END IF
 8031   CONTINUE
        WRITE(*,IOFORM)' '
      END IF
 8033 CONTINUE
      WRITE(*,IOFORM)TEXT(1:NCTEXT)//':'
      WRITE(*,IOFORM)(HELP(I)(1:NCHELP(I)),I=1,NLHELP)
 8034 WRITE(*,IOFORM)' '
      IF(ICODE.EQ.23)THEN
C
C ----- PRETIMED, ENTRY OF PERCENTAGES OF CYCLE
C ----- CHECK IF PERCENTAGES OF CYCLE = 100
C ----- CALCULATE ACTUAL CYCLE LENGTH
C
        XSUM=0.0
        X1=ICYC/100.
        IPCT=0
        TLINE=MKFMT(3,NFT,NCTL)
        DO 8035 I=1,NN
        READ(MLINE(I),TLINE(1:NCTL))IT1,IT2,IT3
        IPCT=IPCT+IT1+IT2+IT3
        X=X1*IT1
        IT1=X/DT+0.5
        XSUM=XSUM+IT1*DT
        X=X1*IT2
        IT1=X/DT+0.5
        XSUM=XSUM+IT1*DT
        X=X1*IT3
        IT1=X/DT+0.5
        XSUM=XSUM+IT1*DT
 8035   CONTINUE
        IF((IPCT.NE.100) .OR. (XSUM.NE.ICYC))THEN
          X=ICYC
          IT1=ALOG10(X)+1.
          WRITE(FMT2(5:5),'(I1)')IT1
          WRITE(*,FMT2)'Requested cycle length is ',ICYC,' seconds.'
          IF(IPCT.EQ.100)GO TO 8040
          X=IPCT
          IT1=ALOG10(X)+1.0
          WRITE(FMT2(5:5),'(I1)')IT1
          WRITE(*,FMT2)'Sum of cycle length percentages is ',IPCT,'%.'
 8040     CONTINUE
          IT1=ALOG10(XSUM)+3.
          WRITE(FMT3(5:5),'(I1)')IT1
          WRITE(*,FMT3)'Cycle length as sum of calculated time '//
     1                 'intervals is ',XSUM,' seconds.'
          GO TO 8280
        END IF
        GO TO 8250
      END IF
      IF(ICODE.EQ.24)THEN
C
C ----- PRETIMED, ENTRY OF SECONDS
C ----- CALCULATE CYCLE LENGTH
C
        XSUM=0.
        TLINE=MKFMT(3,NFT,NCTL)
        DO 8045 I=1,NN
        READ(MLINE(I),TLINE(1:NCTL))X1,X2,X3
        IF(FLAG)THEN
C
C ----- ROUND TO NEAREST "DT",ONLY FIRST TIME WHEN REVISING
C
          IT1=X1/DT+0.5
          X1=IT1*DT
          IT1=X2/DT+0.5
          X2=IT1*DT
          IT1=X3/DT+0.5
          X3=IT1*DT
          WRITE(MLINE(I),TLINE(1:NCTL))X1,X2,X3
        END IF
        XSUM=XSUM+X1+X2+X3
 8045   CONTINUE
        FLAG=.FALSE.
        GO TO 8250
      END IF
      IF((ICODE.EQ.26) .OR. (ICODE.EQ.27) .OR. (ICODE.EQ.32) .OR.
     1                                         (ICODE.EQ.47))THEN
C
C ----- ACTUATED, NEMA OR TEXAS DIAMOND CONTROLLER
C
        IF(FLAG)THEN
C
C ----- ROUND TO NEAREST "DT", ONLY FIRST TIME WHEN REVISING
C
          NC2=NFS(1)+NFS(2)+NFS(3)+NFS(4)
          IF(ICODE.NE.32)NC2=NC2+NFS(5)
          TLINE=MKFMT(5,NFT,NCTL)
          DO 8050 I=1,NN
          IF((I.EQ.1) .AND. (ICODE.EQ.26))THEN
C
C ----- UNACTUATED PHASE OF SEMI-ACTUATED CONTROLLER
C
            TLINE(7:7)='A'
            TLINE(9:10)=' '
            READ(MLINE(1),TLINE(1:NCTL))X1,EDREQ(1:NFS(2)),X3,X4
          ELSE
            IF(ICODE.NE.32)THEN
              READ(MLINE(I),TLINE(1:NCTL))X1,X2,X3,X4,X5
            ELSE
              READ(MLINE(I),TLINE(1:NCTL))X1,X2,X3,X4
            END IF
          END IF
          IT1=X1/DT+0.5
          X1=IT1*DT
          IT1=X3/DT+0.5
          X3=IT1*DT
          IT1=X4/DT+0.5
          X4=IT1*DT
          IF((I.EQ.1) .AND. (ICODE.EQ.26))THEN
            WRITE(MLINE(1)(1:NC2-NFS(5)),TLINE(1:NCTL))X1,
     1           EDREQ(1:NFS(2)),X3,X4
            TLINE(7:10)=NFT(2)
          ELSE
            IT1=X2/DT+0.5
            X2=IT1*DT
            IT1=X5/DT+0.5
            X5=IT1*DT
            IF(ICODE.NE.32)THEN
              WRITE(MLINE(I)(1:NC2),TLINE(1:NCTL))X1,X2,X3,X4,X5
            ELSE
              WRITE(MLINE(I)(1:NC2),TLINE(1:NCTL))X1,X2,X3,X4
            END IF
          END IF
 8050     CONTINUE
          FLAG=.FALSE.
        END IF
        IF(ICODE.NE.32)GO TO 8300
      END IF
      GO TO 8280
 8250 CONTINUE
      IT1=ALOG10(XSUM)+3.
      WRITE(FMT3(5:5),'(I1)')IT1
      WRITE(*,FMT3)'Cycle length is ',XSUM,' seconds.'
      WRITE(*,IOFORM)' '
 8280 CONTINUE
      IF((NF1.LT.0) .AND. (ICODE.NE.33))THEN
C
C ----- LEG AND LANE HEADER
C
        WRITE(*,IOFORM)MLINE(NPHOL+2)(1:NCTD1)
        WRITE(*,IOFORM)MLINE(NPHOL+3)(1:NCTD1)
      END IF
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        WRITE(*,IOFORM)MLINE(NPHOL+NPHOL+16)(1:NCTD1)
        WRITE(*,IOFORM)MLINE(NPHOL+NPHOL+17)(1:NCTD1)
      END IF
      IF(ICODE.EQ.28)THEN
        WRITE(*,IOFORM)MLINE(NPHOL+4)(1:NCTD2)
        IF(MFLAG)THEN
C
C ----- SECOND LINE OF MOVEMENT CODES
C
          IT2=1
          DO 8290 I=NFL,NLSTL
          DO 8285 J=1,NINLNS(I)
          TLINE(IT2:IT2+1)=MVIL(J,I)(3:)
          IT2=IT2+2
          TLINE(IT2:IT2)=' '
          IT2=IT2+1
 8285     CONTINUE
 8290     CONTINUE
          WRITE(*,IOFORM)'      '//TLINE(1:IT2-2)
        END IF
        WRITE(*,IOFORM)MLINE(NPHOL+5)(1:NCTD2)
      END IF
 8300 CONTINUE
      DO 8525 I=1,NN
      CALL SPACED (MLINE(I),NF,NFS,NSPL,NLINE)
      IF(ICODE.EQ.16)THEN
C
C ----- IF FIELD 1 = 0, SHOW THIS SDR INACTIVE
C
        IF(NLINE(1:NFS(1)).EQ.' ')GO TO 8400
        TLINE=MKFMT(1,NFT,NCTL)
        I1=-1
        READ(NLINE,TLINE(1:NCTL),ERR=8400)I1
        IF(I1.EQ.0)THEN
          NLINE(NCSDR+(NFIELD-1)*NSPL+2:)='(INACTIVE)'
        END IF
      END IF
      IF(ICODE.EQ.23)THEN
C
C ----- PREPARE REF DATA FOR DISPLAY IN PARENTHESIS AFTER DATA FIELD
C
        X1=ICYC/100.
        X2=X1
        X3=X1
        CALL REFSEC(NLINE,NFS,NSPL,1,NFT,'F4.1',X1,DT)
        CALL REFSEC(NLINE,NFS,NSPL,2,NFT,'F4.1',X2,DT)
        CALL REFSEC(NLINE,NFS,NSPL,3,NFT,'F4.1',X3,DT)
      END IF
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
C
C ----- PREPARE DETECTOR COVERAGE REF. DATA
C
C ----- CHECK FOR INACTIVE DETECTOR
C
        IT1=NFS(1)+NFS(2)+NFS(3)+NFS(4)+NFS(5)+1
        MLINEU=MLINE(I)
        CALL  TOUPR   ( MLINEU )
        IF(MLINEU(IT1:IT1).EQ.'I')THEN
          IF(ICODE.EQ.29)
     1    WRITE(NLINE(NCTD3+1:),IOFORM)'<inactive.  Don''t include '//
     2                                 'in connection list.'
          IF(ICODE.EQ.49)
     1    WRITE(NLINE(NCTD3+1:),IOFORM)'<WARNING: Field 6 is "IN".  '//
     2                                 'This detector is inactive.'
          GO TO 8400
        END IF
        IT1=1
        IT2=NFS(1)
        IF(MLINEU(IT1:IT2).EQ.' ')GO TO 8400
        IT1=IT2+1
        IT2=IT2+NFS(2)
        IF(MLINEU(IT1:IT2).EQ.' ')GO TO 8400
        IT1=IT2+1
        IT2=IT2+NFS(3)
        IF(MLINEU(IT1:IT2).EQ.' ')GO TO 8400
        IF(ICODE.EQ.49)THEN
          TLINE=MKFMT(2,NFT(2),NCTL)
          READ(MLINE(I)(NFS(1)+1:),TLINE(1:NCTL))IFI,INO
          IF(INO.EQ.0)THEN
            WRITE(NLINE(NCTD3+1:),IOFORM)'<WARNING: Field 3 is 0.  '//
     1                                   'This detector is inactive.'
            GO TO 8400
          END IF
        END IF
        TLINE=MKFMT(3,NFT,NCTL)
        IF((ICODE.EQ.49).OR.
     1     (XDEF(1)(1:NFS(1)).EQ.TTTTT(1:NFS(1))))THEN
          READ(MLINE(I),TLINE(1:NCTL))DETLEG,IFI,INO
          ILE=-1
          DETLEGU=DETLEG
          CALL  TOUPR   ( DETLEGU )
          IF(DETLEGU.EQ.'IR')ILE=0
          IF(DETLEGU.EQ.' 2')ILE=2
          IF(DETLEGU.EQ.' 3')ILE=3
          IF(DETLEGU.EQ.' 5')ILE=5
          IF(DETLEGU.EQ.' 6')ILE=6
          IF(DETLEGU.EQ.'IL')ILE=NLGP1
        ELSE
          READ(MLINE(I),TLINE(1:NCTL))ILE,IFI,INO
        END IF
        IF((ILE.LT.0) .OR. ( ILE.GT.NLGP1))THEN
          WRITE(NLINE(NCTD3+1:),'(A)')'<ERROR: Invalid data in field 1'
          GO TO 8400
        END IF
        IF((IFI+INO-1) .GT. NINLNS(ILE))THEN
          WRITE(NLINE(NCTD3+1:),FMT2)'<ERROR: Fields 2+3 exceed '//
     1                               'no. of inbound lanes (',
     2                               NINLNS(ILE),').'
          GO TO 8400
        END IF
        IT2=0
        DO 8380 K=0,ILE-1
        IT2=IT2+NINLNS(K)
 8380   CONTINUE
        IT2=IT2+IFI-1
        IT2=IT2+IT2+NCTD3+5
        DO 8390 K=1,INO
        IT2=IT2+2
        NLINE(IT2:IT2)='X'
 8390   CONTINUE
      END IF
      IF(ICODE.EQ.33)THEN
C
C ----- IF FIELD 1 = 0, BLANK THE OTHER FIELDS, SHOW THIS OVERLAP INACTIVE
C
        IF(NLINE(1:NFS(1)).EQ.' ')GO TO 8400
        TLINE=MKFMT(1,NFT,NCTL)
        I1=-1
        READ(NLINE,TLINE(1:NCTL),ERR=8400)I1
        IF(I1.EQ.0)THEN
          MLINE(I)(NFS(1)+1:)=' '
          NLINE(NFS(1)+NSPL+1:)='(INACTIVE)'
        END IF
      END IF
 8400 CONTINUE
      NCLN=MAX0(1,ILNB( NLINE ))
      NCBLAN=1
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
C
C ----- DETECTORS
C
        IF(I.EQ.1)THEN
          WRITE(*,IOFORM)ALT(2)(1:1)//'('//DETTXT(1)(1:1)//
     1                   '): '//NLINE(1:NCLN)
        ELSE
          NCTX=ILNB( DETTXT(I) )
          IF(NCTX.EQ.2)
     1    WRITE(*,IOFORM)'('//DETTXT(I)(1:NCTX)//'): '//NLINE(1:NCLN)
          IF(NCTX.EQ.1)
     1    WRITE(*,IOFORM)' ('//DETTXT(I)(1:NCTX)//'): '//NLINE(1:NCLN)
        END IF
        GO TO 8525
      ELSE
C
C ----- PHASES
C
 
        NCALT2=ILNB( ALT(2) )
        IF(I.EQ.1)THEN
          WRITE(*,IOFORM)ALT(2)(1:NCALT2)//'('//
     1                   PHOLTX(I+IOFFT)(1:1)//'): '//
     2                   NLINE(1:NCLN)
        ELSE
          IF(NCALT2.GT.1)NCBLAN=NCALT2
          NCTX=ILNB( PHOLTX(I+IOFFT) )
          IF(NCTX.EQ.3)WRITE(*,IOFORM)PHOLTX(I+IOFFT)(1:NCTX)//'): '//
     1                                NLINE(1:NCLN)
          IF(NCTX.EQ.2)WRITE(*,IOFORM)'('//PHOLTX(I+IOFFT)(1:NCTX)//
     1                                '): '//NLINE(1:NCLN)
          IF(NCTX.EQ.1)WRITE(*,IOFORM)BLANK(1:NCBLAN)//'('//
     1                                PHOLTX(I+IOFFT)(1:NCTX)//
     2                                '): '//NLINE(1:NCLN)
        END IF
        IF((ICODE.EQ.28) .AND. (NEMA.OR.TXDSS) .AND.
     1     (I.EQ.NPHAST) .AND. (NOLP.GT.0))THEN
          NC1=MAX0((NCOVL-10)/2,1)
          NC1=MIN0(NC1,LEN( DASH ))
          NC2=MAX0(NCOVL-10-NC1,1)
          NC2=MIN0(NC2,LEN( DASH ))
          WRITE(*,IOFORM)'      '//DASH(1:NC1)//
     *                   ' Overlaps '//DASH(1:NC2)
        END IF
      END IF
 8525 CONTINUE
      WRITE(*,IOFORM)BLANK(1:NCBLAN)//'Fld: '//TDAT1(1:NCTD3)
      WRITE(*,IOFORM)' '
 8530 CONTINUE
      IF(ICODE.EQ.33)THEN
        CALL YESNO('Are '//TEXT(1:NCTEXT)//' OK ?',TLINE,ALT)
      ELSE
        CALL YESNO('Is '//TEXT(1:NCTEXT)//' OK ?',TLINE,ALT)
      END IF
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:4).EQ.'HELP')THEN
        IF((ICODE.EQ.30) .OR. (ICODE.EQ.34))THEN
          CALL YESNO('Do you want to see the detector coverage data ?',
     1               TLINE,ALT1)
          TLINEU=TLINE
          CALL  TOUPR   ( TLINEU )
          IF(TLINEU(1:1).EQ.'N')GO TO 8535
          WRITE(*,IOFORM)' '
          WRITE(*,IOFORM)MLINE(NPHOL+1)(1:NCML1)
          DO 8531 JJ=NPHOL+2,NPHOL+NDET+3
          WRITE(*,IOFORM)MLINE(JJ)(1:NCML)
 8531     CONTINUE
          TLINEU=TLINE
          CALL  TOUPR   ( TLINEU )
          IF(TLINEU(1:1).EQ.'O')GO TO 8034
          WRITE(*,IOFORM)' '
          GO TO 8535
        END IF
        IF(ICODE.EQ.31)THEN
          CALL YESNO('Do you want to see the green signal sequence '//
     1               'reference data ?',TLINE,ALT1)
          TLINEU=TLINE
          CALL  TOUPR   ( TLINEU )
          IF(TLINEU(1:1).EQ.'N')GO TO 8535
          WRITE(*,IOFORM)' '
          WRITE(*,IOFORM)MLINE(NPHOL+1)(1:NCSS1)
          DO 8534 JJ=NPHOL+2,NPHOL+NPHOL+5
          WRITE(*,IOFORM)MLINE(JJ)(1:NCSS)
          IF(MFLAG.AND.JJ.EQ.NPHA+4)THEN
C
C ----- SECOND LINE OF MOVEMENT CODES
C
            WRITE(*,IOFORM)MLINE(NPHOL+NPHOL+6)(1:NCSS)
          END IF
 8534     CONTINUE
          WRITE(*,IOFORM)' '
          TLINEU=TLINE
          CALL  TOUPR   ( TLINEU )
          IF(TLINEU(1:1).EQ.'O')GO TO 8034
        END IF
 8535   CONTINUE
        IF(ICODE.EQ.49)THEN
          IF(DIGREF(.TRUE.))GO TO 8034
          WRITE(*,IOFORM)' '
        END IF
        CALL YESNO('Do you want a description of each data field ?',
     1             TLINE,ALT1)
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:1).EQ.'N')GO TO 8536
        IF(NLHELP.GT.0)THEN
          WRITE(*,FMT1)(HELP(I)(1:NCHELP(I)),I=1,NLHELP)
        ELSE
          WRITE(*,IOFORM)'Description not available.'
        END IF
        WRITE(*,IOFORM)' '
        IF(TLINEU(1:1).EQ.'O')GO TO 8034
 8536   CONTINUE
        CALL YESNO('Do you want an explanation of the data '//
     1             'edit request ?',TLINE,ALT1)
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:1).EQ.'N')GO TO 8537
        WRITE(*,FMT1)(HLP(I)(1:NCHLP(I)),I=NHMAX(2)+1,NHMAX(2)+NLHET)
        WRITE(*,IOFORM)' '
        IF(TLINEU(1:1).EQ.'O')GO TO 8034
 8537   CONTINUE
        CALL YESNO('Do you want an explanation of the field column '//
     1             'edit request ?',TLINE,ALT1)
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:1).EQ.'N')GO TO 8034
        WRITE(*,FMT1)(HELPFC(I)(1:NCHEFC(I)),I=1,NLHEFC)
        WRITE(*,IOFORM)' '
        GO TO 8034
      END IF
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:1).EQ.'Y')GO TO 9900
      IF(TLINEU.EQ.' ')GO TO  8020
      IF(TLINEU(1:3).EQ.'ADD')THEN
C
C ----- ADD ANOTHER DETECTOR
C
        II=-1
        I=1
        CALL READIN(TLINE,EDREQ,II,I,K,K,X,NLINE,II,K,'dummy')
        EDREQU=EDREQ
        CALL  TOUPR   ( EDREQU )
        IF(EDREQU.EQ.'READ ERROR')GO TO 8550
        IF(NDET.EQ.20)THEN
          WRITE(*,IOFORM)'There are now 20 detectors, the maximum '//
     1                   'number allowed.  Detector not added.'
          GO TO 8020
        END IF
        IF((I.LT.1) .OR. (I.GT.NF))GO TO 8550
        NDET=NDET+1
        NN=NDET
        CALL NCFMT(FMT2(5:),NN)
        WRITE(*,IOFORM)' '
        WRITE(*,FMT2)'Adding detector number ',NN,'.'
        IF(I.GT.1)THEN
C
C ----- ADD LEADING COMMAS TO FORCE DEFAULTS OF FIELDS BEFORE <I>
C
          NLINE(I:I+II-1)=NLINE(1:II)
          NLINE(1:I-1)=',,,,,'
        END IF
        IF(.NOT.EXPDF(NLINE))GO TO 8550
        IF(FFDEC(1,NF,NFT,NLINE,II,MLINE(NDET),IDEF,-99))GO TO 8025
        GO TO 8020
      END IF
      IT1=ILNB( ALT(5) )
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:IT1).EQ.ALT(5)(1:IT1))GO TO 8542
      IF(TLINEU(1:1).NE.'N')GO TO 8545
 8538 WRITE(*,IOFORM)'Keyin '//ALT(1)(2:)
      WRITE(*,IOFORM)'or '//ALT(6)(2:)
      READ(*,IOFORM)TLINE
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU.EQ.' ')GO TO 8020
      IF(TLINEU(1:4).EQ.'HELP')THEN
        CALL YESNO('Do you want an explanation of the data '//
     1             'edit request ?',TLINE,ALT1)
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:1).NE.'N')THEN
          WRITE(*,FMT1)(HLP(I)(1:NCHLP(I)),I=NHMAX(2)+1,NHMAX(2)+NLHET)
          WRITE(*,IOFORM)' '
        END IF
        IF(TLINEU(1:1).NE.'O')THEN
          CALL YESNO('Do you want an explanation of the field '//
     1               'column edit request ?',TLINE,ALT1)
          TLINEU=TLINE
          CALL  TOUPR   ( TLINEU )
          IF(TLINEU(1:1).NE.'N')THEN
            WRITE(*,FMT1)(HELPFC(I)(1:NCHEFC(I)),I=1,NLHEFC)
            WRITE(*,IOFORM)' '
          END IF
        END IF
        GO TO 8538
      END IF
      IT1=ILNB( ALT(5) )
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:IT1).EQ.ALT(5)(1:IT1))GO TO 8542
      IT1=ILNB( ALT(2) )
      IF(TLINEU(1:IT1).EQ.ALT(2)(1:IT1))GO TO 8545
 8540 CONTINUE
      WRITE(*,IOFORM)'Error in data edit request.'
      WRITE(*,IOFORM)' '
      GO TO 8025
 8542 CONTINUE
C
C ----- FIELD COLUMN EDIT REQUEST
C
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
C
C ----- DETECTOR NAME TEXT FOR FIRST SUBSCRIPT
C
        DETTXT(0)=ALT(5)
        IL=EFV(MLINE,TLINE,NLINE,NN,1,1,DETTXT)
      ELSE
C
C ----- PHASE TEXT FOR FIRST SUBSCRIPT
C
        IF(TXDSS)THEN
          ICODET=ICODE
          ICODE=48
        END IF
        IF(ICODE.EQ.33)THEN
          OLTX(0)=ALT(5)
          IL=EFV(MLINE,TLINE,NLINE,NN,1,-1,OLTX)
        ELSE
          PHOLTX(0)=ALT(5)
          IL=EFV(MLINE,TLINE,NLINE,NN,1,1,PHOLTX)
        END IF
        IF(TXDSS)THEN
          ICODE=ICODET
        END IF
      END IF
 8543 CONTINUE
      IF(.NOT.IL)THEN
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:10).EQ.'REQUEST ER')GO TO 8540
        IF(TLINEU(1:10).EQ.'KEYIN ERRO')GO TO 8540
      END IF
      GO TO 8020
 8545 CONTINUE
      II=-1
      J=0
      I=0
C
C ----- ARRAY ELEMENTS FOR FIRST SUBSCRIPT ?
C
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        CALL READIN(TLINE,EDREQ,II,I,J,K,X,NLINE,II,-199,
     1              DETTXT(1))
      ELSE
        CALL READIN(TLINE,EDREQ,II,I,J,K,X,NLINE,II,-199,
     1              PHOLTX(1+IOFFT))
      END IF
 8547 CONTINUE
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      IF(EDREQU.NE.'READ ERROR')GO TO 8555
 8550 CONTINUE
      WRITE(*,IOFORM)'Error in keyin.'
      GO TO 8538
 8555 CONTINUE
      IF(I.EQ.0)I=1
      IF(J.EQ.0)J=1
      IF((I.GT.NN) .OR. (I.LT.1))GO TO 8550
      IF((J.GT.NF) .OR. (J.LT.1))GO TO 8550
      IF(.NOT.EXPDF(NLINE))GO TO 8530
      WRITE(*,IOFORM)' '
      IF((I.EQ.1) .AND. (ICODE.EQ.26))THEN
        IL=FFDEC(J,NF,NFT,NLINE,II,MLINE(1),IDEF,-99)
      ELSE
        IF(((ICODE.EQ.27) .OR. (ICODE.EQ.40)) .AND. (.NOT.NEMA))THEN
          IF(ARREST(I))THEN
            RNGT12=RANGE(1,2)
            RNGT22=RANGE(2,2)
            RNGT13=RANGE(1,3)
            RNGT23=RANGE(2,3)
            RNGT24=RANGE(2,4)
            RNGT25=RANGE(2,5)
            RANGE(1,2)=DT
            RANGE(2,2)=DT
            RANGE(1,3)=0.0
            RANGE(2,3)=0.0
            RANGE(2,4)=0.0
            RANGE(2,5)=0.0
          END IF
        END IF
        IL=FFDEC(J,NF,NFT,NLINE,II,MLINE(I),XDEF,-99)
        IF(((ICODE.EQ.27) .OR. (ICODE.EQ.40)) .AND. (.NOT.NEMA))THEN
          IF(ARREST(I))THEN
            RANGE(1,2)=RNGT12
            RANGE(2,2)=RNGT22
            RANGE(1,3)=RNGT13
            RANGE(2,3)=RNGT23
            RANGE(2,4)=RNGT24
            RANGE(2,5)=RNGT25
          END IF
        END IF
      END IF
      IF((ICODE.EQ.28) .AND. (.NOT.NEMA))THEN
C
C ----- CHECK FOR ALL RED REST PHASE
C
        IF(MLINE(I).EQ.' ')THEN
          IF(ANYARR)THEN
            IF(IARR.EQ.I)GO TO 9800
            WRITE(*,IOFORM)'I('//CHAR( ITAM1+I )//') is now the '//
     1                     'all red rest phase.'
            WRITE(*,IOFORM)'All fields of the previous '//
     1                     'all red rest phase have been set to "XX" '
            WRITE(*,IOFORM)'and must be revised.'
            WRITE(*,IOFORM)' '
            WRITE(MLINE(IARR),'(25A2)')('XX',II=1,NINLNT)
            ARREST(IARR)=.FALSE.
          ELSE
            ANYARR=.TRUE.
            NLHELP=NLHELP+1
          END IF
          IARR=I
          ARREST(I)=ANYARR
          IT1=7
          WRITE(HELP(NLHELP)(IT1:IT1),IOFORM)CHAR( ITAM1+I )
        ELSE
          IF(IARR.EQ.I)THEN
            ANYARR=.FALSE.
            ARREST(I)=ANYARR
            IARR=0
            NLHELP=NLHELP-1
          END IF
        END IF
 9800   CONTINUE
C
C ----- CHECK FOR DUAL LEFTS, FOLLOWED BY TWO SINGLE LEFTS
C
        DO 9850 I=1,NPHA
        DLT(I)=EUALLT(I,DLTS(I))
 9850   CONTINUE
      END IF
      IF(IL)GO TO 8025
      GO TO 8020
 9900 CONTINUE
C
C ----- LIST DATA
C
      IF(ICODE.GT.19)CALL LISTDT
      IF(ICODE.EQ.28)THEN
        FMT2(5:5)='1'
        II=NPHOL+5
      END IF
      NN1=NN
      IF(NEMA)THEN
        IF(ICODE.EQ.28)NN1=NPHOL
        IF((ICODE.EQ.32) .OR. (ICODE.EQ.35))NN1=8
      END IF
      DO 9960 I=1,NN1
      IF(NEMA .AND.
     1   ((ICODE.EQ.28) .OR. (ICODE.EQ.32) .OR. (ICODE.EQ.35)))THEN
        IF(I.LE.8)THEN
C
C ----- PHASES
C
          IF(I.GT.NPHA)THEN
C
C ----- BLANK THE UNUSED PHASES
C
            WRITE(NSFIO,IOFORM,REC=IREC+I)' '
          ELSE
            IF(ICODE.EQ.35)THEN
C
C ----- PUT VOLUME DENSITY DATA AT END OF TIMING DATA
C
              READ(NSFIO,IOFORM,REC=IREC+I)NLINE(1:NCVDF1-1)
              NLINE(NCVDF1:)=MLINE(I)(1:NC)
              WRITE(NSFIO,IOFORM,REC=IREC+I)NLINE
            ELSE
              WRITE(NSFIO,IOFORM,REC=IREC+I)MLINE(I)
            END IF
          END IF
        ELSE
C
C ----- OVERLAPS
C
          IF(I.GT.8+NOLP)THEN
C
C ----- BLANK THE UNUSED OVERLAPS
C
            WRITE(NSFIO,IOFORM,REC=IREC+I)' '
          ELSE
C
C ----- INDEX OVER THE UNUSED PHASES THAT AREN'T INCLUDED IN MLINE
C
            II=I-(8-NPHA)
            WRITE(NSFIO,IOFORM,REC=IREC+I)MLINE(II)(1:NC)
          END IF
        END IF
      ELSE
        IF((ICODE.EQ.9) .OR. (ICODE.EQ.16))THEN
          WRITE(NDFIO,IOFORM,REC=IREC+I)MLINE(I)(1:NC)
        ELSE
          WRITE(NSFIO,IOFORM,REC=IREC+I)MLINE(I)(1:NC)
        END IF
      END IF
      IF(ICODE.EQ.28)THEN
C
C ----- SAVE GREEN SIGNAL SEQUENCE DATA FOR REF.
C
        IJ=II+I
        IF(I.EQ.1)THEN
          MLINE(IJ)(1:2)=ALT(2)(1:1)//'('
        ELSE
          MLINE(IJ)(1:2)=' ('
        END IF
        MLINE(IJ)(3:6)=PHOLTX(I)(1:1)//'):'
        CALL SPACED(MLINE(I),NF,NFS,NSPL,MLINE(IJ)(7:))
      END IF
 9960 CONTINUE
      IF(ICODE.EQ.16)THEN
        NOUT(4)=IREC+NSDR+1
        GO TO 9985
      END IF
      IF(ICODE.EQ.28)THEN
        NOUT(4)=IREC+1+NN1
        GO TO 9985
      END IF
      IF(NEMA .AND.
     1   ((ICODE.EQ.32) .OR. (ICODE.EQ.34) .OR. (ICODE.EQ.35)))THEN
        NOUT(4)=IREC+9
C
C ----- UP TO 8 PHASES
C ----- BLANK LINES FOR UNUSED SLOTS
C
        DO 9970 I=NN+1,8
        WRITE(NSFIO,IOFORM,REC=IREC+I)' '
 9970   CONTINUE
        GO TO 9985
      END IF
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        NOUT(4)=IREC+21
        IF(REVISE)GO TO 9985
C
C ----- UP TO 20 DETECTORS
C ----- BLANK LINES FOR UNUSED DETECTOR SLOTS
C
        DO 9980 I=NN+1,20
        WRITE(NSFIO,IOFORM,REC=IREC+I)' '
 9980   CONTINUE
        GO TO 9985
      END IF
      IF(ICODE.GE.23)NOUT(4)=IREC+NN+1
 9985 CONTINUE
 9990 CONTINUE
      RETURN
      END
 
