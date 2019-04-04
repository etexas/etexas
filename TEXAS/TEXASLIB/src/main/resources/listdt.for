      SUBROUTINE LISTDT
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
      CHARACTER*1 ICBLNK
      CHARACTER*4            FMT1
      CHARACTER*16                       ICPHNU
      PARAMETER  (ICBLNK=' ',FMT1='(I1)',ICPHNU='controller phase')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXRD'
      INCLUDE 'CMTXSR'
      INCLUDE 'CMTXAR'
      INCLUDE 'CMNEMA'
      CHARACTER FMT2*8,TLINE*40
      CHARACTER*80 HELPU,NFTU,MLINEU,NLINEU
      LOGICAL VD
      SAVE VD
      INTEGER              NLFIO          ,ICYC          ,IFIG,
     1                     NOLP          ,NPHA           ,NDET
      EQUIVALENCE (NOUT(3),NLFIO),(MOUT(8),ICYC),(MOUT(8),IFIG),
     1            (MOUT(8),NOLP),(MOUT(9),NPHA),(MOUT(13),NDET)
      INTEGER   I,IFS,II,III,ILNB,IS,ISP,IT1,IT2,IT3,IT4,IT5,IT6,ITAM1,
     1          J,NC,NC1,NC1P1,NC1P39,NC2,NC2P1,NCDATA,NDET1,NDET2,
     2          NDETT,NF,NFL,NFSI,NLHP1,NLSTL,NN,NS
      DATA FMT2 / '(A,I1,A)' /
      IOFORM='(A)'
      ITAM1=ICHAR( 'A' )-1
      IF(((ICODE.GE.22) .AND. (ICODE.LE.27)) .OR.
     1   (ICODE.EQ.29) .OR. (ICODE.EQ.31) .OR.
     2   (ICODE.EQ.32) .OR. (ICODE.EQ.47) .OR. (ICODE.EQ.49))
     3     CALL NEWPG(ICBLNK,ICBLNK,ICBLNK,0,0,1,NLFIO)
      WRITE(NLFIO,'(//3X,A/)')TEXT(1:NCTEXT)//':'
      IF(DIA)THEN
        NFL=0
        NLSTL=NLGP1
      ELSE
        NFL=1
        NLSTL=NLEGS
      END IF
      IF((ICODE.EQ.22) .OR. (ICODE.EQ.28))THEN
        NLHP1=NLHELP+1
        HELP(NLHP1)(1:1)=ICBLNK
        IT4=1-NFS(1)
        II=0
        IF(ICODE.EQ.28)THEN
          NC1=34
          IF(NEMA)THEN
C
C ----- SHOW PHASE AND OVERLAP DATA
C
            IF(NOLP.GT.0)THEN
              WRITE(NLFIO,'(A,I2,A,I2,A)')'   There are',NPHAST,
     *                                    ' timed phases and',NOLP,
     *                                    ' overlaps'
              WRITE(NLFIO,'(A/)')'   The overlaps are designated by '//
     *                           'letters '
            ELSE
              WRITE(NLFIO,'(A,I2,A/)')'   There are',NPHAST,
     *                                ' timed phases and no overlaps'
            END IF
            NC=NPHASS*4
            NC1=25
            WRITE(NLINE,'(2X,A1,15(3X,A1))')(PHOLTX(I)(1:1),I=1,NPHASS)
          ELSE
            NC=NPHASS*6-3
            WRITE(NLINE,'(2X,12(A3,:,3X))')(PHOLTX(I),I=1,NPHASS)
          END IF
          CALL PTDD(1,NC+NC1,ICPHNU,NLINE(1:NC),NLFIO)
        ELSE
          NC1=25
        END IF
        DO 400 I=NFL,NLSTL
        IF((I.EQ.0) .OR. (I.EQ.NLGP1))THEN
          TLINE(1:21)='               Lane 1'
        ELSE
          WRITE(TLINE(1:20),FMT2)'Leg ',I,': inbound lane '
        END IF
        IF(I.EQ.0)WRITE(NLFIO,'(A)')' inbound to center R:'
        IF(I.EQ.NLGP1)WRITE(NLFIO,'(A)')' inbound to center L:'
        DO 350 J=1,NINLNS(I)
        II=II+1
        IF(J.EQ.2)TLINE(1:14)=ICBLNK
        WRITE(TLINE(21:21),FMT1)J
        IT4=IT4+NFS(II)
        IF(ICODE.EQ.28)THEN
          IF(NEMA)THEN
C
C ----- ROOM FOR UP TO 8 PHASES AND 4 OVERLAPS
C
            NS=2
          ELSE
            NS=4
          END IF
          CALL FITDAT(NPHASS,NFS(1),NFS(1),NS,HELP(NLHP1)(2:),
     1                MLINE(1),IT4,NFT(1))
        ELSE
          CALL GETTXT(NLHELP-1,NLINE(IT4:IT4+1),HELP(2),
     1                HELP(NLHP1),NC)
        END IF
        CALL PTDD(1,NC+NC1,TLINE(1:21),HELP(NLHP1)(1:NC),NLFIO)
  350   CONTINUE
        WRITE(NLFIO,IOFORM)' '
  400   CONTINUE
        GO TO 9990
      END IF
      IF((ICODE.GT.21) .AND. (ICODE.NE.40))GO TO 1000
      IFS=5
      IT1=8
      IF(ICODE.EQ.40)THEN
        IT3=57
      ELSE
        IT3=65
      END IF
      IT5=0
      II=1
      DO 500 I=1,NFIELD
      NFSI=NFS(I)
      IF (I.EQ.10)IT1=IT1+1
      IF((I.EQ.4) .AND. ((ICODE.EQ.20) .OR. (ICODE.EQ.40)))THEN
        IT4=IT5+1
        IT5=IT5+NFSI
        IS=0
        DO 460 III=1,7
C
C ----- COUNT THE LINES THAT ARE NOT THE FIRST LINE FOR THE NEXT FIELD
C
        HELPU=HELP(II+III)
        CALL  TOUPR   ( HELPU )
        IF(HELPU(1:2).EQ.'F(')GO TO 465
        IS=III
  460   CONTINUE
  465   CONTINUE
        CALL GETTXT(IS,NLINE(IT4:IT4+1),HELP(II+1),TLINE,NC)
        IT2=INDEX(HELP(II)(IT1:),':')+IT1-2
        IF(NC.EQ.0)THEN
          TLINE='ERROR'
          NC=5
        END IF
        IF(IT2.LT.IT1)IT2=IT1
        GO TO 470
      END IF
      IT2=INDEX(HELP(II)(IT1:),'.')
      IF(IT2.EQ.0)IT2=INDEX(HELP(II)(IT1:),'?')+1
      IT2=IT2+IT1-2
      IT4=IT5+1
      IT5=IT5+NFSI
      NFTU=NFT(I)
      CALL  TOUPR   ( NFTU )
      IF(NFTU(1:1).EQ.'A')THEN
C
C ----- LEFT JUSTIFY TEXT FIELDS
C
        TLINE(1:NFSI)=NLINE(IT4:IT5)
        NC=NFSI
      ELSE
C
C ----- RIGHT JUSTIFY NUMERIC FIELDS
C ----- FIELD IS <IFS> WIDE
C
        IF(NFSI.EQ.IFS)THEN
          TLINE(1:IFS)=NLINE(IT4:IT5)
        ELSE
          CALL RJTXT(NLINE(IT4:IT5),TLINE(1:IFS))
        END IF
        NC=IFS
      END IF
  470 CONTINUE
      CALL PTDD(1,IT3+NC-IFS,HELP(II)(IT1:IT2),TLINE(1:NC),NLFIO)
      IF(I.EQ.NFIELD)GO TO 500
      II=II+1
      DO 480 III=1,10
C
C ----- SKIP THE LINES THAT ARE NOT THE FIRST LINE FOR A FIELD
C
      HELPU=HELP(II)
      CALL  TOUPR   ( HELPU )
      IF(HELPU(1:2).EQ.'F(')GO TO 485
      II=II+1
      IF(II.GT.20)GO TO 9990
  480 CONTINUE
C
C ----- BAD GDSV FILE
C
      GO TO 9990
  485 CONTINUE
  500 CONTINUE
      GO TO 9990
 1000 CONTINUE
      IF((ICODE.LE.27) .OR. (ICODE.EQ.32)  .OR.
     1   (ICODE.EQ.35) .OR. (ICODE.EQ.47))THEN
C
C ----- LIST SIGNAL TIMING DATA
C
        IF(ICODE.EQ.32)THEN
          NCDATA=0
          DO 1037 I=1,NFIELD-1
          NCDATA=NCDATA+NFS(I)
 1037     CONTINUE
          VD=.FALSE.
          DO 1040 I=1,NPHAST
          MLINEU=MLINE(I)
          CALL  TOUPR   ( MLINEU )
          IF(MLINEU(NCDATA+1:NCDATA+NFS(NFIELD)) .NE.'NO ')THEN
            VD=.TRUE.
            GO TO 1045
          END IF
 1040     CONTINUE
 1045     CONTINUE
        END IF
        IF(ICODE.EQ.35)THEN
          IF(.NOT.VD)GO TO 9990
          GO TO 1050
        END IF
        IF(ICODE.EQ.47)THEN
           WRITE(NLFIO,IOFORM)'  There are 6 controller phases '//
     1                        'and 2 overlaps.'
           WRITE(NLFIO,FMT2)'  Figure ',IFIG,' operation selected.'
        ELSE
          IF(NEMA)THEN
            WRITE(NLFIO,FMT2)'  There are ',NPHAST,
     1                       ' timed controller phases.'
            IF(NOLP.GT.0)WRITE(NLFIO,FMT2)'  There are ',NOLP,
     1                                    ' overlaps.'
          ELSE
            WRITE(NLFIO,FMT2)'  There are ',NPHA,
     1                       ' signal controller phases.'
          END IF
          IF(ICODE.EQ.26)WRITE(NLFIO,IOFORM)'  Phase A is unactuated.'
          IF(ICODE.EQ.23)THEN
            CALL INTFMT(NC,FMT2(5:),ICYC)
            WRITE(NLFIO,FMT2)'  Requested cycle length is ',ICYC,
     1                       ' seconds.'
            WRITE(NLFIO,IOFORM)'  Data are percents of this cycle '//
     1                         'length.'
            FMT2(5:5)='1'
          END IF
        END IF
 1050   CONTINUE
        WRITE(NLFIO,IOFORM)' '
        WRITE(NLINE,'(1X,8(A,:,3X))')(PHOLTX(I),I=1,NPHAST)
        IF(ICODE.EQ.35)THEN
          IFS=5
          ISP=1
        ELSE
          IFS=4
          ISP=2
        END IF
        NCDATA=NPHAST*(IFS+ISP)-ISP
        IT5=34+NCDATA
        IF(VD)IT5=IT5+17
        CALL PTDD(1,IT5,ICPHNU,NLINE(1:NCDATA),NLFIO)
        IT1=1
        NS=8
        DO 1100 I=1,NFIELD
        IF(I.EQ.10)NS=9
        CALL FITDAT(NPHAST,NFS(I),IFS,ISP,NLINE,MLINE(1),IT1,NFT(I))
        IT1=IT1+NFS(I)
        IF((ICODE.EQ.26) .AND. (I.EQ.1))THEN
          NF=INDEX(HELP(1)(NS:),'(')+5
          GO TO 1080
        END IF
        NF=INDEX(HELP(I)(NS:),'.')+NS-2
        IF(.NOT.NEMA .AND. (I.EQ.9))HELP(I)(18:)=','//HELP(I)(36:)
        IF(NF.EQ.NS-2)NF=INDEX(HELP(I)(NS:),'?')+NS-1
 1080   CONTINUE
        CALL PTDD(1,IT5,HELP(I)(NS:NF),NLINE(1:NCDATA),NLFIO)
 1100   CONTINUE
        GO TO 9990
      END IF
      IF(ICODE.EQ.31)THEN
        NLINE='  PHASES THAT CAN BE CLEARED TO FROM PHASE '//
     1        PHOLTX(1)(1:1)//':'
        IT1=NPHAST+NPHAST-2
        NLINE(47:)=MLINE(1)(1:IT1)
        NC=46+IT1
        WRITE(NLFIO,IOFORM)NLINE(1:NC)//'(IN PRIORITY ORDER)'
        NLINE(3:36)=ICBLNK
        DO 2010 I=2,NPHA
        NLINE(44:44)=PHOLTX(I)(1:1)
        NLINE(47:)=MLINE(I)(1:IT1)
 2010   WRITE(NLFIO,IOFORM)NLINE(1:NC)
        GO TO 9990
      END IF
      IF((ICODE.EQ.29) .OR. (ICODE.EQ.49))THEN
        CALL INTFMT(NC,FMT2(5:),NDET)
        WRITE(NLFIO,FMT2)'  THERE ARE ',NDET,' DETECTORS.'
        FMT2(5:5)='1'
        NDET1=1
        IF(NDET.LE.4)THEN
          NDET2=NDET
        ELSE
          NDET2=4
        END IF
        IT1=8
        IT6=54
 2030   CONTINUE
        WRITE(NLFIO,IOFORM)ICBLNK
        NDETT=NDET2-NDET1+1
        NCDATA=NDETT*7-2
        IT5=IT6+NCDATA
        WRITE(NLINE,'(2X,A2,8(5X,A2))')(DETTXT(I),I=NDET1,NDET2)
        CALL PTDD(1,IT5-1,'detector number',NLINE(1:NCDATA-1),NLFIO)
        IT3=1
        DO 2100 I=1,NFIELD
        IF((ICODE.EQ.49) .AND. (I.EQ.1))THEN
          NLINE(1:3)=ICBLNK
          CALL FITDAT(NDETT,NFS(I),5,2,NLINE(4:),MLINE(NDET1),IT3,
     1                NFT(I))
        ELSE
          CALL FITDAT(NDETT,NFS(I),5,2,NLINE,MLINE(NDET1),IT3,NFT(I))
        END IF
        IT3=IT3+NFS(I)
        IT2=INDEX(HELP(I)(IT1:),'.')+IT1-2
        IF(I.EQ.6)THEN
          IT4=2
          DO 2040 J=1,NDETT
          NLINEU=NLINE
          CALL  TOUPR   ( NLINEU )
          IF(NLINEU(IT4:IT4).EQ.'N')THEN
            NLINE(IT4-1:IT4+3)='INAC.'
            GO TO 2035
          END IF
          IF(NLINEU(IT4:IT4).EQ.'R')THEN
            NLINE(IT4+1:IT4+3)='ES.'
          ELSE
            NLINE(IT4+1:IT4+3)='LSE'
           END IF
 2035     CONTINUE
          IT4=IT4+7
 2040     CONTINUE
        END IF
        CALL PTDD(1,IT5,HELP(I)(IT1:IT2),NLINE(1:NCDATA),NLFIO)
 2100   CONTINUE
        IF(NDET2.EQ.NDET)GO TO 9990
        IF(NDET2.EQ.4)THEN
          HELP(1)='Leg.'
          HELP(2)='First Lane.'
          HELP(3)='Number of Lanes.'
          HELP(4)='Spacing.'
          HELP(5)='Length.'
          HELP(6)='Type.'
          IT1=1
          IT6=19
        END IF
        NDET1=NDET2+1
        NDET2=NDET2+9
        IF(NDET2.GT.NDET)NDET2=NDET
        GO TO 2030
      END IF
      IF((ICODE.EQ.45) .OR. (ICODE.EQ.46))THEN
        IT2=0
        NC1=NFS(1)
        DO 2150 I=1,IABS(NFIELD)
        IT1=IT2+1
        IT2=IT2+NFS(I)
        IT3=INDEX(HELP(I),' - ')+3
        IT4=INDEX(HELP(I)(IT3:),'.')+IT3-2
        IF((ICODE.EQ.45) .AND. ((I.EQ.4) .OR. (I.EQ.5)))THEN
          TLINE(1:NC1)=NLINE(IT1:IT2)
        ELSE
          CALL RJTXT(NLINE(IT1:IT2),TLINE(1:NC1))
        END IF
        CALL PTDD(1,50,HELP(I)(IT3:IT4),TLINE(1:NC1),NLFIO)
 2150   CONTINUE
        WRITE(NLFIO,IOFORM)' '
        WRITE(NLFIO,IOFORM)' '//HELP(NLHELP)(1:NCHELP(NLHELP))
        IF(ICODE.EQ.46)CALL NEWPG(ICBLNK,ICBLNK,ICBLNK,0,0,1,NLFIO)
        GO TO 9990
      END IF
      IF(ICODE.EQ.30)THEN
        NC1=NFS(1)
        NC1P1=NC1+1
        NC1P39=NC1+39
        NC=ILNB( MLINE(1)(1:NC1) )
        NC2=NC1+27
        NC2P1=NC2+1
        NLINE(1:NC2)='Phase '//PHOLTX(1)(1:1)//'   connection: "'//
     *              MLINE(1)(1:NC)//'"'
        NLINE(NC2P1:NC1P39)='detectors:'
        CALL SPACED(MLINE(1)(NC1P1:),10,NFS(2),1,NLINE(NC1P39:))
        GO TO 2170
      END IF
      IF(ICODE.EQ.33)THEN
        NLINE='Overlap '//PHOLTX(1+NPHAST)(1:1)//'    phases:'
        CALL SPACED(MLINE(1),5,NFS(1),1,NLINE(23:))
        GO TO 2170
      END IF
      IF(ICODE.EQ.34)THEN
        NLINE='Phase '//PHOLTX(1)(1:1)//'    detectors:'
        CALL SPACED(MLINE(1),5,NFS(1),1,NLINE(24:))
        GO TO 2170
      END IF
      GO TO 9990
 2170 CONTINUE
      WRITE(NLFIO,'(1X,A)')NLINE(1:ILNB( NLINE ))
      IF(ICODE.EQ.30)THEN
        NN=NPHAST
        NLINE(1:21)=' '
        NLINE(NC2P1:NC1P39)= ' '
        GO TO 2190
      END IF
      IF(ICODE.EQ.33)THEN
        NN=NOLP
        NLINE(1:22)=' '
        GO TO 2190
      END IF
      IF(ICODE.EQ.34)THEN
        NN=NPHAST
        NLINE(1:23)=' '
        GO TO 2190
      END IF
 2190 CONTINUE
      DO 2220 I=2,NN
      IF(ICODE.EQ.30)THEN
        NLINE(7:7)=PHOLTX(I)(1:1)
        NC=ILNB( MLINE(I)(1:NC1) )
        NLINE(24:NC1+24)=MLINE(I)(1:NC)//'"'
        CALL SPACED(MLINE(I)(NC1P1:),0,NFS(2),1,NLINE(NC1P39:))
        GO TO 2210
      END IF
      IF(ICODE.EQ.33)THEN
        NLINE(9:9)=PHOLTX(I+NPHAST)(1:1)
        CALL SPACED(MLINE(I),5,NFS(1),1,NLINE(23:))
        GO TO 2210
      END IF
      IF(ICODE.EQ.34)THEN
        NLINE(7:7)=PHOLTX(I)(1:1)
        CALL SPACED(MLINE(I),5,NFS(1),1,NLINE(24:))
        GO TO 2210
      END IF
 2210 CONTINUE
      WRITE(NLFIO,'(1X,A)')NLINE(1:ILNB( NLINE ))
 2220 CONTINUE
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE GETTXT(NL,CC1,HELP,TEXT,NC)
C
C ----- THIS SUBROUTINE USES CHARACTER VARIABLE <CC1> TO SELECT
C ----- A SPECIFIC STRING OF TEXT AND PUTS THE TEXT IN <TEXT>.
C ----- THE KEYS TO SELECTION ARE BETWEEN QUOTES IN THE LINES OF
C ----- THE SUBSCRIPTED CHARACTER VARIABLE <HELP>, WITH THE TEXT
C ----- TO BE SELECTED FOLLOWING, BETWEEN A MINUS AND A PERIOD.
C ----- <NL> IS THE NUMBER OF LINES OF <HELP> TO BE CHECKED.
C ----- <NC> RETURNS THE CHARACTER COUNT FOR <TEXT>.
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           NL              ,NC
      CHARACTER*(*)        CC1,HELP(7),
     1                              TEXT
      CHARACTER*1 ICQUO
      PARAMETER  (ICQUO='"')
      CHARACTER CC1QUO*3,CC1QUOU*3,CC2QUO*4,CC2QUOU*4,HELPU*80
      INTEGER   I,ILNB,IT1,IT2,IT3,NL1
      NL1=NL
      IF(NL1.LT.0)NL1=-NL1
      NC=ILNB( CC1 )
      IF(NC.LE.0)GO TO 110
      CC1QUO=ICQUO//CC1(1:1)//ICQUO
      CC1QUOU=CC1QUO
      CALL  TOUPR   ( CC1QUOU )
      IF(NC.GT.2)NC=2
      CC2QUO=ICQUO//CC1(1:NC)//ICQUO
      CC2QUOU=CC2QUO
      CALL  TOUPR   ( CC2QUOU )
      DO 100 I=1,NL1
      HELPU=HELP(I)
      CALL  TOUPR   ( HELPU )
      IT1=INDEX(HELPU,CC2QUOU(1:NC+2))
      IF(IT1.GT.0)THEN
        IT2=INDEX(HELPU(IT1:),'-')+IT1+1
        IF(IT2.EQ.IT1+1)GO TO 110
        IT3=INDEX(HELPU(IT2:),'.')+IT2-2
        IF(IT3.EQ.IT2-2)GO TO 110
        IF(IT3.LT.IT2)GO TO 110
        TEXT=HELP(I)(IT2:IT3)
        NC=IT3-IT2+1
        GO TO 9990
      END IF
      IT1=INDEX(HELPU,CC1QUOU)
      IF(IT1.GT.0)THEN
        IT2=INDEX(HELPU(IT1:),'-')+IT1+1
        IF(IT2.EQ.IT1+1)GO TO 110
        IT3=INDEX(HELPU(IT2:),'.')+IT2-2
        IF(IT3.EQ.IT2-2)GO TO 110
        IF(IT3.LT.IT2)GO TO 110
        TEXT=HELP(I)(IT2:IT3)
        NC=IT3-IT2+1
        GO TO 9990
      END IF
  100 CONTINUE
  110 CONTINUE
      TEXT=' '
      NC=0
 9990 RETURN
      END
C
C
C
      SUBROUTINE FITDAT(N,NFS,NFSMAX,NSP,NLINE,MLINE,IT4,NFT)
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           N,NFS,NFSMAX,NSP            ,IT4
      CHARACTER*(*)                      NLINE,MLINE(*) ,NFT
      CHARACTER*80 NFTU
      INTEGER      I,IT1,IT2,IT3
      IT2=0
      IT3=IT4+NFS-1
      DO 100 I=1,N
      IT1=IT2+1
      IT2=IT2+NFSMAX+NSP
      NFTU=NFT
      CALL  TOUPR   ( NFTU )
      IF((NFS.LT.NFSMAX) .AND. (NFTU(1:1).NE.'A'))THEN
        CALL RJTXT(MLINE(I)(IT4:IT3),NLINE(IT1:IT2-NSP))
      ELSE
        NLINE(IT1:IT2)=MLINE(I)(IT4:IT3)
      END IF
  100 CONTINUE
      RETURN
      END
