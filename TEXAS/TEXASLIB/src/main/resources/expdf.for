      FUNCTION EXPDF(NLINE)
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
C   THIS LOGICAL FUNCTION EXPANDS DUPLICATE TEXT FIELDS IN THE
C   CHARACTER VARIABLE <NLINE>.
C
C   FIELDS ARE SEPERATED BY A ",".
C   COMMAS WITHIN A SET OF PARENTHESIS ARE IGNORED: ...(..,..)..
C
C   DUPLICATE FIELDS ARE DEFINED BY A "*", PRECEEDED BY A
C   DUPLICATION FACTOR: "II*TEXT..."
C
C   THE MAXIMUM TEXT SIZE IS 20 CHARACTERS.
C
C   <EXPDF> IS SET TO "TRUE" IF THE EXPANSION IS SUCESSFUL,
C          "FALSE" IF THE EXPANSION IS UNSUCESSFUL.
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  EXPDF
      CHARACTER*(*)  NLINE
      CHARACTER IDF*20,IFMT*4,IOFORM*3
      INTEGER   ISTAR
      DIMENSION ISTAR(10)
      INTEGER   I,IADD,IFACT,IFLEN,II,ILNB,IMOVE,IT1,IT2,IT3,IT4,J,
     1          JSTAR,NC
      DATA IFMT / '(I )' /
      DATA IOFORM / '(A)' /
      EXPDF=.TRUE.
      NC=ILNB( NLINE )
      IF(NC.EQ.0)GO TO 9990
      IT1=1
      DO 1000 I=1,10
C
C   FIND THE *'S AND STORE THEIR CHARACTER POSITIONS
C   <II> IS THE NUMBER OF STARS IN <NLINE>
C
      II=I
      IT2=INDEX(NLINE(IT1:NC),'*')
      IF(IT2.EQ.0)GO TO 1100
      IT1=IT1+IT2
 1000 ISTAR(I)=IT1-1
      GO TO 1110
 1100 IF(II.EQ.1)GO TO 9990
      II=II-1
 1110 IT1=1
      DO 2000 I=1,II
C
C   EXPAND EACH MULTIPLE FIELD
C   <IT1> IS THE FIRST CHARACTER POSITION OF THE M. F.
C   <IT2> IS THE LAST CHARACTER POSITION FO THE M. F.
C
      JSTAR=ISTAR(I)
 1900 IT3=INDEX(NLINE(IT1:JSTAR),',')
      IF(IT3.GT.0)THEN
        IT1=IT1+IT3
        GO TO 1900
      END IF
      IF(JSTAR.EQ.IT1)THEN
C
C   EXPANSION FACTOR DEFAULTS TO 1
C
        IFACT=1
      ELSE
C
C   READ EXPANSION FACTOR
C
        IT2=JSTAR-IT1
        WRITE(IFMT(3:3),'(I1)',ERR=9000)IT2
        READ(NLINE(IT1:JSTAR-1),IFMT,ERR=9000)IFACT
        IF(IFACT.LE.0)GO TO 9000
      END IF
      IT2=INDEX(NLINE(JSTAR:NC),',')
      IF(IT2.EQ.0)THEN
        IT2=NC
      ELSE
        IT2=IT2+JSTAR-2
C
C   CHECK FOR "(" IN FIELD
C
        IT3=INDEX(NLINE(JSTAR:IT2),'(')
        IF(IT3.GT.0)THEN
C
C   IGNORE COMMAS WITHIN PARENTHESIS
C
          IT3=IT3+JSTAR
          IT4=INDEX(NLINE(IT3:NC),')')
          IF(IT4.EQ.0)THEN
            WRITE(*,IOFORM)'Error in keyin, unclosed '//
     1                     'parenthesis in duplicate field.'
            GO TO 9500
          END IF
          IT4=IT4+IT3
          IT2=INDEX(NLINE(IT4:NC),',')
          IF(IT2.EQ.0)THEN
            IT2=NC
          ELSE
            IT2=IT2+IT4-2
          END IF
        END IF
      END IF
C   <IFLEN> IS THE LENGTH OF THE FIELD TO BE DUPLICATED
      IFLEN=IT2-JSTAR
C   <IMOVE> IS THE NUMBER OF CHARACTER POSITIONS TO SHIFT
C           TRAILING CHARACTERS
      IMOVE=IFLEN*IFACT
C   ALLOW FOR POSITIONS ALREADY OCCUPIED BY M. F. REQUEST
      IMOVE=IMOVE-IT2+IT1-1
C
C   MAKE ROOM FOR FIELD SEPARATORS (",")
C
      IMOVE=IMOVE+IFACT-1
      IF(IMOVE.NE.0)THEN
        IF(NC+IMOVE.GT.LEN( NLINE ))THEN
          WRITE(*,IOFORM)'Input character string too small to '//
     1                   'expand dulpicate fields.'
          GO TO 9500
        END IF
        IF((IT2+1).LE.NC)NLINE(IT2+1+IMOVE:NC+IMOVE)=NLINE(IT2+1:NC)
        IF(IMOVE.LT.0)NLINE(NC+IMOVE+1:NC)=' '
        NC=NC+IMOVE
        DO 1960 IT3=I+1,II
 1960   ISTAR(IT3)=ISTAR(IT3)+IMOVE
      END IF
      IDF=NLINE(JSTAR+1:IT2)
      DO 1950 J=1,IFACT
C
C   PUT EACH DUPLICATE FIELD IN <NLINE>
C   <IADD> IS THE OFFSET FOR EACH SUCCESSIVE FIELD
C
      IADD=(J-1)*(IFLEN+1)
      IT3=IT1+IFLEN+IADD
      IF(J.LT.IFACT)NLINE(IT3:IT3)=','
      IT3=IT3-1
 1950 NLINE(IT1+IADD:IT3)=IDF(1:IFLEN)
 2000 IT1=IT3+1
      GO TO 9990
 9000 WRITE(*,IOFORM)'Error in field duplication factor.'
 9500 EXPDF=.FALSE.
 9990 RETURN
      END
