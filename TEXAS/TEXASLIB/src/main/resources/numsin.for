      SUBROUTINE NUMSIN(PROMPT,PRE,POST,IN,IL,IH,ILIST)
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
C ----- THIS SUBROUTINE PROMPTS, USING TEXT IN <PROMPT>, FOR A
C ----- LIST OF 1 TO <IN> NUMBERS. THESE MUST BE IN THE
C ----- RANGE OF <IL> THRU <IH>, WITH <IL> AND <IH> BOTH POSITIVE.
C ----- IF <IN> LT 0,THEN NEGATIVE NUMBERS WHOSE ABSOLUTE VALUE
C ----- ARE WITHIN THE RANGE ARE OK. THE KEYED IN NUMBERS ARE SORTED
C ----- AND DISPLAYED FOR APPROVAL WITH A PREFIX AND SUFFIX ADDED.
C ----- TO BYPASS SORTING, PUT "NOSORT" IN <NLINE> BEFORE CALLING.
C ----- TO READ AND DISPLAY LETTERS INSTEAD OF NUMBERS, PUT
C ----- "LETTER" IN <NLINE> BEFORE CALLING. THE "LETTER" MODE
C ----- WILL STILL PROCESS <IL>, <IH> AND <ILIST> AS INTEGERS,
C ----- WITH 1 FOR "A", 2 FOR "B", ETC.
C ----- PRE(1) - PREFIX FOR SINGLE NUMBER
C ----- POST(1) - SUFFIX FOR SINGLE NUMBER
C ----- PRE(2) - PREFIX FOR MULTIPLE NUMBERS
C ----- POST(2) - SUFFIX FOR MULTIPLE NUMBERS
C ----- THE KEYED IN NUMBERS ARE RETURNED IN <ILIST>, WITH THE NUMBER
C ----- OF RETURNED NUMBERS IN <IN>.
C ----- ALL UNUSED, UP TO <IN>, <ILIST> ITEMS ARE SET TO 0.
C ----- COMMON BLOCK /TEX2/ PASSES THE UNIT NUMBER OF THE I/O
C ----- (TERMINAL) FILE IN <NOUT(1)>, AND THE I/O FORMAT IN
C ----- <IOFORM>.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     PROMPT,PRE(2),
     1                             POST(2)
      INTEGER                           IN,IL,IH,ILIST(*)
      CHARACTER*4 AAFORM
      PARAMETER(AAFORM='(9A)')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      CHARACTER FMT1*8,FMT2*9,IFMT*4
      CHARACTER*80 NLINEU,TLINE,TLINEU
      LOGICAL SHONLY,NOSORT,LETS
      INTEGER   I,II,ILNB,IMAX,IT1,IT2,IT3,IT4,ITA,ITAM1,ITEMP,ITIH,
     1          ITIL,J,NC,NC1,NC2
      DATA FMT1,FMT2,IFMT/'(A,I1,A)','(1X,I1,A)','(I1)'/
      IOFORM='(A)'
      ITA=ICHAR( 'A' )
      ITAM1=ITA-1
      SHONLY=.FALSE.
      NOSORT=.FALSE.
      LETS=.FALSE.
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(INDEX(NLINEU,'NOSORT').GT.0)NOSORT=.TRUE.
      IF(INDEX(NLINEU,'LETTER').GT.0)LETS=.TRUE.
      IMAX=IABS(IN)
  820 WRITE(*,AAFORM)'Keyin ',PROMPT
      IF(IMAX.GT.1)THEN
        IF(IMAX.GT.9)THEN
          FMT1(5:5)='2'
        ELSE
          FMT1(5:5)='1'
        END IF
        IF(LETS)THEN
          ITIL=ITAM1+IL
          ITIH=ITAM1+IH
          WRITE(*,FMT1)'1 to ',IMAX,' single letters ('//CHAR( ITIL )//
     1                 ' thru '//CHAR( ITIH )//'), separated by commas:'
        ELSE
          WRITE(*,FMT1)'1 to ',IMAX,' integers, separated by commas:'
        END IF
      END IF
      READ(*,IOFORM)TLINE
      TLINEU=TLINE
      CALL  TOUPR   ( TLINEU )
      IF(TLINEU(1:4).EQ.'NONE')THEN
        IN=0
        GO TO 870
      END IF
      IF(TLINEU(1:3).EQ.'ALL')THEN
C
C ----- ALL RED REST PHASE REQUESTED
C
        CALL YESNO('Do you want this phase to be the all red rest '//
     1             'phase ?',NLINE,'NONE')
        NLINEU=NLINE
        CALL  TOUPR   ( NLINEU )
        IF(NLINEU(1:1).EQ.'N')GO TO 820
        IN=0
        GO TO 9990
      END IF
      IT2=-1
      I=1
  840 IT1=IT2+2
      IT3=INDEX(TLINE(IT1:),',')
      IF(IT3.EQ.1)GO TO 860
      IF(IT3.EQ.0)THEN
C
C ----- LAST NUMBER BEING READ
C
        IT2=ILNB( TLINE(IT1:) )+IT1-1
        GO TO 845
      END IF
      IT2=IT1+IT3-2
  845 IT4=IT2-IT1+1
      IF(IT4.GT.9)GO TO 860
      IF(IT4.LE.0)GO TO 860
      IF(LETS)THEN
        IF(IT2.NE.IT1)GO TO 860
        ITEMP=ICHAR( TLINE(IT1:IT2) )-ITAM1
        IF((ITEMP.LT.IL) .OR. (ITEMP.GT.IH))GO TO 860
        GO TO 855
      END IF
      WRITE(IFMT(3:3),'(I1)')IT4
      READ(TLINE(IT1:IT2),IFMT,ERR=860)ITEMP
      IF((ITEMP.GE.IL) .AND. (ITEMP.LE.IH))GO TO 855
      IF(IN.LT.0)THEN
        IF((ITEMP.GE.-IH) .AND. (ITEMP.LE.-IL))GO TO 855
      END IF
      GO TO 860
  855 ILIST(I)=ITEMP
      IF(I.EQ.IMAX)GO TO 880
      IF(IT3.EQ.0)GO TO 880
      I=I+1
      GO TO 840
  860 WRITE(*,IOFORM)'Error in keyin.'
      GO TO 820
      ENTRY SHOWNU(PROMPT,PRE,POST,IN,IL,IH,ILIST)
C
C ----- ENTRY POINT TO DISPLAY THE EXISTING LIST ONLY, NOT EDIT
C
      SHONLY=.TRUE.
      GO TO 870
      ENTRY NUMSHO1(PROMPT,PRE,POST,IN,IL,IH,ILIST)
C
C ----- ENTRY POINT TO DISPLAY AND EDIT <IN> NUMBERS ALREADY IN <ILIST>
C ----- MAX NUMBERS DIFFERENT FRON <IN>
C
      SHONLY=.FALSE.
      IMAX=IABS(IN)
      I=IMAX
      DO 872 J=2,IMAX
      IF(ILIST(J).GT.IH)THEN
        I=J-1
        GO TO 875
      END IF
  872 CONTINUE
      GO TO 875
      ENTRY NUMSHO(PROMPT,PRE,POST,IN,IL,IH,ILIST)
C
C ----- ENTRY POINT TO DISPLAY AND EDIT NUMBERS ALREADY IN <ILIST>
C
      SHONLY=.FALSE.
  870 CONTINUE
      IMAX=IABS(IN)
      I=IMAX
  875 CONTINUE
      NOSORT=.FALSE.
      LETS=.FALSE.
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(INDEX(NLINEU,'NOSORT').GT.0)NOSORT=.TRUE.
      IF(INDEX(NLINEU,'LETTER').GT.0)LETS=.TRUE.
      CALL INTFMT(IT4,IFMT(3:),ILIST(1))
  880 CONTINUE
      IF(I.EQ.0)THEN
        TLINE='No '//PRE(2)(1:ILNB( PRE(2) ))//
     *               POST(2)(1:ILNB( POST(2) ))
        NC=ILNB( TLINE )
        GO TO 935
      END IF
      IF(I.EQ.1)THEN
        FMT1(5:5)=IFMT(3:3)
        IF(SHONLY)THEN
          NC1=MIN0(ILNB( PROMPT )+1,LEN( PROMPT ))
          IF(LETS)THEN
            WRITE(TLINE,AAFORM)PROMPT(1:NC1),CHAR( ILIST(1)+ITAM1 )
            IT4=1
          ELSE
            WRITE(TLINE,FMT1)PROMPT(1:NC1),ILIST(1)
          END IF
          NC=NC1+IT4+1
          GO TO 1000
        END IF
        NC1=MIN0(ILNB( PRE(1) )+1,LEN( PRE(1) ))
        NC2=MAX0(1,ILNB( POST(1) ))
        NC=NC1+NC2+IT4
        IF(LETS)THEN
          WRITE(TLINE,AAFORM)PRE(1)(1:NC1),CHAR( ILIST(1)+ITAM1 ),
     1         POST(1)(1:NC2)
          IT4=1
        ELSE
          WRITE(TLINE,FMT1)PRE(1)(1:NC1),ILIST(1),POST(1)(1:NC2)
        END IF
      ELSE
        IF(NOSORT)GO TO 915
C
C ----- SORT KEYED IN NUMBERS INTO ASCENDING ORDER
C
        DO 910 IT1=1,I-1
        ITEMP=ILIST(IT1)
        DO 900 IT2=IT1+1,I
        IF(ITEMP.GT.ILIST(IT2))THEN
          ILIST(IT1)=ILIST(IT2)
          ILIST(IT2)=ITEMP
          ITEMP=ILIST(IT1)
        END IF
  900   CONTINUE
  910   CONTINUE
  915   CONTINUE
        IF(SHONLY)THEN
          TLINE=PROMPT
        ELSE
          TLINE=PRE(2)
        END IF
        NC=MIN0(ILNB( TLINE )+1,LEN( TLINE ))
        FMT2(6:6)='1'
        DO 920 II=1,I-1
        ITEMP=ILIST(II)
        IF(LETS)THEN
          IT1=NC+2
          TLINE(NC:IT1)=' '//CHAR( ITEMP+ITAM1 )//','
        ELSE
          CALL INTFMT(IT1,FMT2(6:),ITEMP)
          IT1=IT1+NC+1
          WRITE(TLINE(NC:IT1),FMT2)ITEMP,','
        END IF
  920   NC=IT1+1
        IF(SHONLY)THEN
          ITEMP=ILIST(I)
          IF(LETS)THEN
            IT1=1
            TLINE(NC:NC+IT1)=' '//CHAR( ITEMP+ITAM1 )
          ELSE
            CALL INTFMT(IT1,FMT2(6:),ITEMP)
            WRITE(TLINE(NC:),FMT2)ITEMP,' '
          END IF
          NC=NC+IT1
          GO TO 1000
        END IF
        NC=NC-1
        ITEMP=ILIST(I)
        IT2=MAX0(1,ILNB( POST(2) ))
        IF(LETS)THEN
          IT1=1
          TLINE(NC:)=' and '//CHAR( ITEMP+ITAM1 )//POST(2)(1:IT2)
         ELSE
          CALL INTFMT(IT1,FMT1(5:),ITEMP)
          WRITE(TLINE(NC:),FMT1)' and ',ITEMP,POST(2)(1:IT2)
        END IF
        NC=IT2+IT1+NC+4
      END IF
  935 CONTINUE
      NC=NC+2
      TLINE(NC:)='Is this OK ?'
      NC=NC+12
      CALL YESNO(TLINE(1:NC),NLINE,'NONE')
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(1:1).EQ.'N')GO TO 820
      DO 940 J=I+1,IMAX
      ILIST(J)=0
  940 CONTINUE
      IN=I
      GO TO 9990
 1000 CONTINUE
      WRITE(*,IOFORM)TLINE(1:NC)
 9990 CONTINUE
      RETURN
      END
