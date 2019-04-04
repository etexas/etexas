      SUBROUTINE READIN(NLINE,LINE,N,I,J,K,X,S1,IS2,IS3,S4)
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
C   THIS SUBROUTINE HELPS THE USER DECODE A TEXT LINE
C   OF THE FORM "LINE(I,J,K)=X".  "I", "J" AND "X" ARE
C   OPTIONAL.
C   <NLINE> IS AN ADJUSTABLE LENGTH CHARACTER STRING THAT
C           CONTAINS THE TEXT LINE TO BE DECODED.
C   <LINE> IS A CHARACTER VARIABLE THAT IS SET TO THE
C          CHARACTERS (UP TO THE FIRST 10) TO  THE LEFT OF
C          THE "=".
C   <N> IS SET TO THE NUMBER OF CHARACTERS IN <LINE>.
C   <I>, <J> AND <K> ARE SET TO THE FIRST, SECOND AND THIRD
C                    INTEGER VALUES BETWEEN THE PARENTHESIS.
C   <X>   A FLOATING POINT VARIABLE THAT, IF <IS2> IS = 0(ZERO),
C         IS SET TO THE VALUE DECODED FROM THE TEXT TO THE
C         RIGHT OF THE "=".  IF <IS2> IS NOT = 0, <X> IS NOT CHANGED.
C   <IS2> IF = 0(ZERO), <X> IS AS ABOVE
C         IF < 0, THE TEXT TO THE RIGHT OF THE =
C                 IS TREATED AS TEXT AND ALL CHARACTERS ARE PUT INTO
C                 <S1>.  <IS2> IS THEN SET TO THE NUMBER OF NON-BLANK
C                 CHARACTERS IN <S1>.
C         IF > 0, TEXT TO THE RIGHT OF THE =, INCLUDING
C                 ALL TRAILING BLANKS ARE PUT INTO <S1>.
C                 <IS2> IS SET TO THE NUMBER OF CHARACTERS IN <S1>.
C   <IS3> IF = -99, TRY TO DECODE A SINGLE CHARACTER FROM THE
C                   <I> POSITION. IF SUCCESFUL, SET <I>
C                   TO 1 IF ="A", 2 IF = "B",ETC.
C         IF = -199, COMPARE THE <I> POSITION TO ELEMENTS OF CHARACTER
C                    ARRAY <S4(*)>. IF A MATCH IS FOUND, <I> IS SET TO THE
C                    MATCHING ARRAY ELEMENT NUMBER. UP TO 20 ELEMENTS
C                    WILL BE TESTED.  IF * < 20, A BLANK ARRAY ELEMENT
C                    MUST BE SUPPLIED TO END THE TESTING.
C         IF = -299, SAME AS -199, BUT FOR THE <J> POSITION.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     NLINE               ,S1        ,S4(*)
      CHARACTER*10            LINE
      INTEGER                      N,I,J,K     ,IS2,IS3
      REAL                                 X
      INTEGER    NZERO  ,N1  ,N2  ,N3  ,N4  ,N5  ,N6
      PARAMETER (NZERO=0,N1=1,N2=2,N3=3,N4=4,N5=5,N6=6)
      INTEGER    N7  ,N8  ,N9  ,N10
      PARAMETER (N7=7,N8=8,N9=9,N10=10)
      CHARACTER*10 RERROR,NBLANK
      CHARACTER IFMT*4,FMT1*7
      INTEGER   IEQ,IERR,IFNB,II,ILEN,ILNB,ILP,IRP,ISTART,ISTOP,IT1,ITA,
     1          ITCHAR,ITEMP,ITZ,JCOMMA,JSTART,JSTOP,KCOMMA,KSTART,
     2          KSTOP,NC
      DATA NBLANK / ' ' /
      DATA RERROR / 'READ ERROR' /
      DATA IFMT / '(I )' /
      DATA FMT1 / '(E  .0)' /
      ITA=ICHAR( 'A' )
      ITZ=ICHAR( 'Z' )
      ILEN=LEN( NLINE )
C     WRITE(6,*)' NLINE:',NLINE(1:ILEN)
C     WRITE(6,*)' IS2 = ',IS2
      IFNB=ILNB( NLINE )
      IF(IFNB.EQ.NZERO)THEN
        LINE=NBLANK
        N=NZERO
        RETURN
      END IF
   20 CONTINUE
      N=IFNB
C     WRITE(6,*)' FNB AT ',IFNB
      IEQ=INDEX(NLINE(1:IFNB),'=')
      IF(IEQ.EQ.NZERO)GO TO 21
      N=IEQ-N1
C     WRITE(6,*)' = AT ',IEQ
      ITEMP=IFNB-IEQ
      IF(ITEMP.GT.NZERO)GO TO 22
   21 IS2=NZERO
      IF(LEN( S1 ).GT.NZERO)S1=NBLANK
      GO TO 25
   22 ITEMP=IEQ+N1
      IF(IS2.EQ.NZERO)THEN
        WRITE(FMT1(3:4),'(I2.2)')IFNB-ITEMP+N1
        READ(NLINE(ITEMP:IFNB),FMT1,ERR=23,IOSTAT=IERR)X
        GO TO 25
   23   CONTINUE
C       WRITE(6,*)' ERROR READING DATA TO RIGHT OF ='
C       WRITE(6,*)' IOSTAT = ',IERR
        I=N1
        GO TO 775
      ELSE
        IF(IS2.GT.NZERO)THEN
          IS2=ILEN-IEQ
          S1=NLINE(ITEMP:ILEN)
        ELSE
          IS2=IFNB-IEQ
          S1=NLINE(ITEMP:IFNB)
        END IF
        ITEMP=LEN( S1 )
        IF(IS2.GT.ITEMP)IS2=ITEMP
      END IF
   25 CONTINUE
      ITEMP=IFNB
      IF(IEQ.GT.NZERO)ITEMP=IEQ-N1
      ILP=INDEX(NLINE(1:ITEMP),'(')
      IRP=NZERO
      IF(ILP.EQ.NZERO)GO TO 1010
C     WRITE(6,*)' ( AT ',ILP
      ISTART=ILP+N1
      IRP=INDEX(NLINE(ISTART:ITEMP),')')
      IF(IRP.LE.N1)GO TO 750
      IRP=ISTART+IRP-N1
C     WRITE(6,*)' ) AT ',IRP
      ISTART=ILP+N1
      ISTOP=IRP-N1
      JCOMMA=INDEX(NLINE(ISTART:ISTOP),',')
      IF(JCOMMA.EQ.NZERO)GO TO 900
      JCOMMA=ISTART+JCOMMA-N1
      ITEMP=IRP-JCOMMA
      IF(ITEMP.LE.N1)GO TO 750
C     WRITE(6,*)' FIRST , AT ',JCOMMA
      ISTOP=JCOMMA-N1
      JSTART=JCOMMA+N1
      JSTOP=IRP-N1
      KCOMMA=INDEX(NLINE(JSTART:JSTOP),',')
      IF(KCOMMA.EQ.NZERO)GO TO 850
      KCOMMA=JSTART+KCOMMA-N1
      ITEMP=IRP-KCOMMA
      IF(ITEMP.LE.N1)GO TO 750
C     WRITE(6,*)' SECOND , AT ',KCOMMA
      JSTOP=KCOMMA-N1
      KSTART=KCOMMA+N1
      KSTOP=IRP-N1
      GO TO 800
  750 CONTINUE
C     WRITE(6,*)'  ERROR READING DATA BETWEEN ( )'
      I=N2
  775 LINE=RERROR
      N=N10
C     WRITE(6,*)NLINE(1:IFNB)
      IS2=NZERO
      S1=NBLANK
      RETURN
  800 IT1=KSTOP-KSTART+N1
      IF(IT1.GT.NZERO)THEN
        WRITE(IFMT(N3:N3),'(I1)')IT1
        READ(NLINE(KSTART:KSTOP),IFMT,ERR=750)K
      END IF
  850 IT1=JSTOP-JSTART+N1
      IF(IT1.GT.NZERO)THEN
        IF(IS3.EQ.-299)THEN
C
C ----- COMPARE THE <J> POSITION TO ELEMENTS OF <S4>
C
          DO 880 II=1,20
          IF(S4(II).EQ.' ')GO TO 750
          NC=ILNB( S4(II) )
          IF(NC.EQ.0)GO TO 750
          IF(NLINE(JSTART:JSTOP).EQ.S4(II)(1:NC))THEN
            J=II
            GO TO 900
          END IF
  880     CONTINUE
          GO TO 750
        END IF
        WRITE(IFMT(N3:N3),'(I1)')IT1
        READ(NLINE(JSTART:JSTOP),IFMT,ERR=750)J
      END IF
  900 IT1=ISTOP-ISTART+N1
      IF(IT1.GT.NZERO)THEN
        IF(IS3.EQ.-99)THEN
          IF(ISTART.NE.ISTOP)GO TO 750
          ITCHAR=ICHAR( NLINE(ISTART:ISTART) )
          IF((ITCHAR.LT.ITA) .OR. (ITCHAR.GT.ITZ))GO TO 750
          I=ITCHAR-ITA+N1
          GO TO 950
        END IF
        IF(IS3.EQ.-199)THEN
C
C ----- COMPARE THE <I> POSITION TO ELEMENTS OF <S4>
C
          DO 930 II=1,20
          IF(S4(II).EQ.' ')GO TO 750
          NC=ILNB( S4(II) )
          IF(NC.EQ.0)GO TO 750
          IF(NLINE(ISTART:ISTOP).EQ.S4(II)(1:NC))THEN
            I=II
            GO TO 950
          END IF
  930     CONTINUE
          GO TO 750
        END IF
        WRITE(IFMT(N3:N3),'(I1)')IT1
        READ(NLINE(ISTART:ISTOP),IFMT,ERR=750)I
      END IF
  950 N=ILP-N1
 1010 IF(N.GT.N10)N=N10
      LINE=NLINE(1:N)
      RETURN
      END
