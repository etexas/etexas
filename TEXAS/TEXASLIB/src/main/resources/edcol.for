      FUNCTION EDCOL(NLINE,EDTXTL,ER)
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
C ----- THIS FUNCTION EDITS ALPHANUMERIC DATA IN A COLUMN-WISE MANNER
C
C   NLINE - CHARACTER VARIABLE WITH TEXT TO BE EDITED
C   EDTXTL - CHARACTER VARIABLE WITH EDIT REQUEST: ER(I,J,K)=NEWTEXT
C   ER - CHARACTER VARIABLE WITH EDIT REQUEST KEYWORD
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  EDCOL
      CHARACTER*(*)  NLINE,EDTXTL,ER
      CHARACTER*3 IOFORM
      INTEGER                  NZERO  ,N1
      PARAMETER  (IOFORM='(A)',NZERO=0,N1=1)
      CHARACTER EDTXT*80,EDCOM*10
      CHARACTER*80 EDCOMU,ERU
      INTEGER   I,II,III,ILNB,IT1,IT2,IT3,IT4,J,K,L,LNL,NC1,NC2
      REAL      X
      L=-N1
      I=N1
      J=NZERO
      K=NZERO
      CALL READIN(EDTXTL,EDCOM,III,I,J,K,X,EDTXT,L,III,'dummy')
      EDCOMU=EDCOM
      CALL  TOUPR   ( EDCOMU )
      IF(EDCOMU(1:10).EQ.'READ ERROR')GO TO 9000
      NC1=ILNB( ER )
      NC2=ILNB( EDCOM )
      IF(NC1.NE.NC2)GO TO 9000
      ERU=ER
      CALL  TOUPR   ( ERU )
      IF(ERU(1:NC1).NE.EDCOMU(1:NC2))GO TO 9000
      EDCOL=.TRUE.
      LNL=LEN( NLINE )
C
C   <I> IS THE KEYED IN NUMBER OF THE FIRST COLUMN TO REVISE
C
      IF(I.LT.N1)GO TO 9000
      IF(I.GT.LNL)GO TO 9000
C
C   <J> IS THE KEYED IN NUMBER OF THE LAST COLUMN TO REVISE
C
      IF(J.LE.NZERO)J=I
      IF(J.GT.LNL)J=LNL
C
C   <K> IS THE KEYED IN NUMBER OF REPLACING CHARACTERS
C   <L> IS THE LENGTH OF THE REPLACING CHARACTER STRING
C
      IF(K.GT.NZERO)L=K
      IF(I+L-N1.GT.LNL)L=LNL-I+N1
      IT1=J-I+N1
      IF(L.GT.IT1)THEN
C
C   SHIFT TRAILING CHARACTERS TO RIGHT TO MAKE ROOM
C
        IT2=L-IT1
        IT3=ILNB( NLINE )
        IF(IT3+IT2.GT.LNL)IT3=LNL-IT2
        DO 200 II=IT3,I+IT1-N1,-N1
  200   NLINE(II+IT2:II+IT2)=NLINE(II:II)
      END IF
      IF(L.LT.IT1)THEN
C
C   SHIFT TRAILING CHARACTERS TO LEFT TO CLOSE GAP
C
        IT2=IT1-L
        IT3=ILNB( NLINE )
        IF(IT3.LT.J)IT3=J
        IT4=J+N1
        DO 300 II=IT4,IT3
  300   NLINE(II-IT2:II-IT2)=NLINE(II:II)
        IT2=IT3-IT2+N1
        NLINE(IT2:IT3)=' '
      END IF
      IF(L.LE.NZERO)RETURN
      NLINE(I:I+L-N1)=EDTXT(1:L)
      RETURN
 9000 WRITE(*,IOFORM)'Error in text edit request.'
      EDCOL=.FALSE.
      RETURN
      END
