      SUBROUTINE SEDFLD
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
      INTEGER     NZERO  ,N1  ,N2  ,N3  ,N4  ,N5  ,N6  ,N7  ,N8
      PARAMETER  (NZERO=0,N1=1,N2=2,N3=3,N4=4,N5=5,N6=6,N7=7,N8=8)
      INTEGER     N19   ,N20   ,N23   ,N29   ,N36
      PARAMETER  (N19=19,N20=20,N23=23,N29=29,N36=36)
      CHARACTER*6 FMT1
      CHARACTER*8               FMT2
      PARAMETER  (FMT1='(/(A))',FMT2='(A,I1,A)')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXRD'
      INCLUDE 'CMTXSR'
      INCLUDE 'CMTXHL'
      CHARACTER*47 ALT(N5),EDREQ*10,MKFMT*16,TTTTT*20,EDREQU*10
      CHARACTER*80 EDTXT,TLINE,TDAT1,TDAT2,TDAT3,ALT1(N3)*8
      CHARACTER*80 NLINEU,TLINEU
      LOGICAL FFDEC,EXPDF
      INTEGER   NFS1         ,NFS2     ,NCHLP
      DIMENSION NFS1(0:NLGP1),NFS2(NAL),NCHLP(19)
      INTEGER              ICYC
      EQUIVALENCE(MOUT(N8),ICYC)
      INTEGER   I,IFI,ILE,ILNB,INO,IT1,IT2,J,K,L,NC,NCTD1,NCTD3,NCTL,NF,
     1          NFL,NFST,NLHLP,NLSTL,NSPL,NT1,NT2
      REAL      X1,X2,X3
      SAVE NCHLP,NLHLP,ALT
      DATA NLHLP/NZERO/
      DATA TTTTT/'TTTTTTTTTT'/
      DATA ALT(N2),ALT(N3),ALT(N4),ALT(5)/'F',' ','HELP','NONE'/
      DATA (ALT1(I),I=N1,N3)/'$"only".','only','NONE'/
      IOFORM='(A)'
      IF(NLHLP.EQ.NZERO)THEN
        NLHLP=-N1
        CALL RDHLP('gdver1',NHMAX(N2),ALT(N2),HLP,NCHLP,NLHLP)
        ALT(N1)='$a '//HLP(N1)
      END IF
      NC=ILNB( NLINE )
      NF=ABS(NFIELD)
      NSPL=N1
      IF(ICODE.EQ.N23)NSPL=N8
C
C ----- IF = 23, MAKE ROOM FOR REF. DATA AFTER FIELDS
C
      CALL PRFLDS(TDAT3,NF,NFS,N1,NZERO,NSPL,NCTD3,I,I)
      IF((NFIELD.LT.NZERO) .OR. (ICODE.EQ.N29))THEN
C
C ----- PREPARE TO DISPLAY LEG AND LANE NUMBERS ABOVE DATA FIELDS
C
        IF(ICODE.EQ.N29)THEN
          NT1=NCTD3+N7
          TDAT1(1:NT1)=' '
          TDAT2(1:NT1)=' '
          NFST=N1
        ELSE
          NT1=N1
          NFST=NFS(N1)
        END IF
        DO 6900 I=N1,NAL
        NFS2(I)=NFST
 6900   CONTINUE
        TDAT1(NT1:)=' Leg: '
        TDAT2(NT1:)='Lane: '
        NT1=NT1+N6
        NT2=NT1
        NCTD1=NZERO
        IF(DIA)THEN
          NFL=0
          NLSTL=NLGP1
        ELSE
          NFL=1
          NLSTL=NLEGS
        END IF
        DO 7000 I=NFL,NLSTL
        IT1=NINLNS(I)
        IF(IT1.GT.NZERO)THEN
          CALL PRLNS(TDAT2(NT2:),IT1,NFS2,N1,NZERO,N1,NCTD1,I,I)
          NT2=NT2+NCTD1
          NFS1(I)=(NFST+N1)*IT1-N1
        ELSE
          NFS1(I)=-N1
        END IF
 7000   CONTINUE
        CALL PRLEGS(TDAT1(NT1:),NLSTL,NFS1(N1),NFL,NZERO,
     1              N1,NCTD1,I,I)
        NCTD1=NCTD1+NT1-N1
      END IF
 8020 WRITE(*,IOFORM)' '
 8025 CALL SPACED(NLINE,NF,NFS,NSPL,EDTXT)
      IF((NFIELD.LT.NZERO) .OR. (ICODE.EQ.N29))THEN
        WRITE(*,IOFORM)TDAT1(1:NCTD1)
        WRITE(*,IOFORM)TDAT2(1:NCTD1)
      END IF
      IF(ICODE.EQ.N23)THEN
C
C ----- PREPARE REF DATA FOR DISPLAY IN PARENTHESIS AFTER DATA FIELD
C
        X1=ICYC/100.
        X2=X1
        X3=X1
        CALL REFSEC(EDTXT,NFS,NSPL,N1,NFT,'F4.1',X1,DT)
        CALL REFSEC(EDTXT,NFS,NSPL,N2,NFT,'F4.1',X2,DT)
        CALL REFSEC(EDTXT,NFS,NSPL,N3,NFT,'F4.1',X3,DT)
      END IF
      IF(ICODE.EQ.N29)THEN
C
C ----- PREPARE DETECTOR COVERAGE REF. DATA
C
C ----- CHECK FOR INACTIVE DETECTOR
C
        IT1=NFS(N1)+NFS(N2)+NFS(N3)+NFS(N4)+NFS(N5)+N1
        NLINEU=NLINE
        CALL  TOUPR   ( NLINEU )
        IF(NLINEU(IT1:IT1).EQ.'I')THEN
          WRITE(EDTXT(NCTD3+N1:),IOFORM)'<inactive.  Don''t include '//
     1                                  'in connection list.'
          GO TO 8400
        END IF
        IT1=N1
        IT2=NFS(N1)
        IF(NLINEU(IT1:IT2).EQ.' ')GO TO 8400
        IT1=IT2+N1
        IT2=IT2+NFS(N2)
        IF(NLINEU(IT1:IT2).EQ.' ')GO TO 8400
        IT1=IT2+N1
        IT2=IT2+NFS(N3)
        IF(NLINEU(IT1:IT2).EQ.' ')GO TO 8400
        IF((ICODE.EQ.49).OR.(XDEF(1)(1:NFS(1)).EQ.TTTTT(1:NFS(1))))THEN
          ILE=-1
          IF(NLINEU(1:NFS(1)).EQ.'IR')ILE=0
          IF(NLINEU(1:NFS(1)).EQ.' 2')ILE=2
          IF(NLINEU(1:NFS(1)).EQ.' 3')ILE=3
          IF(NLINEU(1:NFS(1)).EQ.' 5')ILE=5
          IF(NLINEU(1:NFS(1)).EQ.' 6')ILE=6
          IF(NLINEU(1:NFS(1)).EQ.'IL')ILE=NLGP1
          TLINE=MKFMT(2,NFT(2),NCTL)
          READ(NLINE(NFS(1)+1:),TLINE(1:NCTL))IFI,INO
        ELSE
          TLINE=MKFMT(3,NFT,NCTL)
          READ(NLINE,TLINE(1:NCTL))ILE,IFI,INO
        END IF
        IF((ILE.LT.0) .OR. (ILE.GT.NLGP1))THEN
          WRITE(NLINE(NCTD3+1:),'(A)')'<ERROR: Invalid data in field 1'
          GO TO 8400
        END IF
        IF((IFI+INO-N1) .GT. NINLNS(ILE))THEN
          WRITE(EDTXT(NCTD3+N1:),FMT2)'<ERROR: Fields 2+3 exceed '//
     1                                'number of inbound lanes (',
     2                                NINLNS(ILE),').'
          GO TO 8400
        END IF
        IT2=NZERO
        DO 8380 K=0,ILE-N1
        IT2=IT2+NINLNS(K)
 8380   CONTINUE
        IT2=IT2+IFI-N1
        IT2=IT2+IT2+NCTD3+N5
        DO 8390 K=N1,INO
        IT2=IT2+N2
        EDTXT(IT2:IT2)='X'
 8390   CONTINUE
      END IF
 8400 CONTINUE
      WRITE(*,IOFORM)'Data: '//EDTXT(1:ILNB( EDTXT ))
      WRITE(*,IOFORM)' Fld: '//TDAT3(1:NCTD3)
      WRITE(*,IOFORM)' '
 8420 CONTINUE
      IF((ICODE.EQ.45) .OR. (ICODE.EQ.46))THEN
        CALL YESNO('Are '//TEXT(1:NCTEXT)//' OK ?',TLINE,ALT)
      ELSE
        CALL YESNO('Is ' //TEXT(1:NCTEXT)//' OK ?',TLINE,ALT)
      END IF
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:N1).EQ.'H')THEN
        CALL YESNO('Do you want a description of each data field ?',
     1              TLINE,ALT1)
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:N1).EQ.'N')GO TO 8430
        IF(NLHELP.GT.NZERO)THEN
          WRITE(*,FMT1)(HELP(I)(1:NCHELP(I)),I=N1,NLHELP)
        ELSE
          WRITE(*,IOFORM)'Description not available.'
        END IF
        IF(TLINEU(1:N1).EQ.'O')GO TO 8020
 8430   CONTINUE
        CALL YESNO('Do you want an explanation of the data field '//
     1             'edit request ?',TLINE,ALT1)
        TLINEU=TLINE
        CALL  TOUPR   ( TLINEU )
        IF(TLINEU(1:N1).EQ.'N')GO TO 8020
        WRITE(*,FMT1)(HLP(I)(1:NCHLP(I)),I=N1,NLHLP)
        GO TO 8020
      END IF
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:N1).EQ.'Y')GO TO 9990
      IF(TLINEU.EQ.' ')GO TO  8020
      IF(TLINEU(1:N1).NE.'N')GO TO 8445
 8435 WRITE(*,IOFORM)'Keyin '//ALT(N1)(N2:)
      READ(*,IOFORM)TLINE
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU.EQ.' ')GO TO 8020
      IF(TLINEU(1:N4).EQ.'HELP')THEN
        WRITE(*,FMT1)(HLP(I)(1:NCHLP(I)),I=N1,NLHLP)
        WRITE(*,IOFORM)' '
        GO TO 8435
      END IF
      IT1=ILNB( ALT(N2) )
      IF(TLINEU(1:IT1).EQ.ALT(N2)(1:IT1))GO TO 8445
 8440 WRITE(*,IOFORM)'Error in data field edit request.'
      WRITE(*,IOFORM)' '
      GO TO 8025
 8445 CONTINUE
      I=NZERO
      L=-N1
      CALL READIN(TLINE,EDREQ,J,I,J,J,J,EDTXT,L,I,'dummy')
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      IF(EDREQU.EQ.'READ ERROR')GO TO 8440
      IT1=ILNB( EDREQ )
      IF(EDREQU(1:IT1).NE.ALT(N2)(1:IT1))GO TO 8440
      IF(I.EQ.NZERO)I=N1
      IF(I.GT.NF.OR.I.LT.N1)GO TO 8440
      IF(.NOT.EXPDF(EDTXT))GO TO 8420
      TLINE=EDTXT
      WRITE(*,IOFORM)' '
      IF(.NOT.FFDEC(I,NF,NFT,TLINE,NC,NLINE,XDEF,-99))GO TO 8020
      GO TO 8025
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE REFSEC(NLINE,NFS,NSPL,IFN,IFMT,FMT1,XFACT,RFACT)
C
C   THIS SUBROUTINE INSERTS CALCULATED REF. DATA AFTER A DATA FIELD
C   IN A DATA LINE.  THE REF. DATA IS ENCLOSED IN PARENTHESIS WITH
C   A BLANK OUTSIDE EACH PARENTHESIS: " (12.3) ".
C   NLINE - DATA LINE.  MUST ALREADY HAVE <NSPL> SPACES AVAILABLE
C           AFTER THE FIELD.
C   NFS - DIMENSIONED VARIABLE OF FIELD SIZES.
C   NSPL - NUMBER OF SPACES BETWEEN FIELDS.
C   IFN - NUMBER OF THE FIELD FOR WHICH REF. DATA IS TO BE ADDED.
C   IFMT - DIMENSIONED CHARACTER VARIABLE WITH FORMAT SPEC FOR EACH
C          FIELD IN THE DATA LINE.
C   FMT1 - FORMAT SPEC FOR REF. DATA.
C   XFACT - FLOATING POINT MULTIPLIER FOR CALCULATING REF. DATA FROM
C           DATA IN FIELD.
C   RFACT - REF. DATA ROUNDED OF TO NEAREST <RFACT>
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     NLINE
      INTEGER                 NFS(*),
     1                            NSPL,IFN
      CHARACTER*4                          IFMT(*),
     1                                          FMT1
      REAL                                           XFACT,RFACT
      INTEGER   N1  ,N2  ,N3
      PARAMETER(N1=1,N2=2,N3=3)
      CHARACTER*80 IFMTU
      INTEGER      I,IT1,IT2,IT3
      REAL         X
      IT1=N1
      DO 100 I=N1,IFN-N1
      IT1=IT1+NFS(I)+NSPL
  100 CONTINUE
      IT2=IT1+NFS(IFN)-N1
      IF(NLINE(IT1:IT2).EQ.' ')THEN
        IT1=IT2+N1
        IT2=IT1+NSPL-N2
        NLINE(IT1:IT2)=' '
        GO TO 9990
      END IF
      IFMTU=IFMT(IFN)
      CALL  TOUPR   ( IFMTU )
      IF(IFMTU(1:N1).EQ.'I')THEN
        READ(NLINE(IT1:IT2),'('//IFMT(IFN)//')')I
        X=I
        GO TO 140
      END IF
      IF(IFMTU(1:N1).EQ.'F')THEN
        READ(NLINE(IT1:IT2),'('//IFMT(IFN)//')')X
        GO TO 140
      END IF
      GO TO 9990
  140 XFACT=X*XFACT
      IT3=XFACT/RFACT+0.5
      XFACT=IT3*RFACT
      IT1=IT2+N1
      IT2=IT1+NSPL-N2
      WRITE(NLINE(IT1:IT2),'(A,'//FMT1//',A)',ERR=145)'%(',XFACT,')'
      IT1=IT1+N1
      GO TO 150
  145 WRITE(NLINE(IT1:IT2),'(A)')' <ERROR'
      GO TO 9990
  150 IT1=IT1+N1
      IF(NLINE(IT1:IT1).NE.' ')GO TO 9990
      NLINE(IT1:IT2)=NLINE(IT1+N1:IT2)
      GO TO 150
 9990 RETURN
      END
