      FUNCTION DFF(NLINE,TLINE,NFIELD,FLAG)
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
      LOGICAL  DFF
      CHARACTER*(*)NLINE,TLINE
      INTEGER                  NFIELD,FLAG
      CHARACTER*3 IOFORM
      PARAMETER  (IOFORM='(A)')
      INTEGER     NZERO  ,N1
      PARAMETER  (NZERO=0,N1=1)
      CHARACTER*10 EDREQ,EDREQU
      LOGICAL      DFF1
      INTEGER      I,II,J,K,NC
      REAL         X
      II=-N1
      I=NZERO
      J=NZERO
      CALL READIN(NLINE,EDREQ,NC,I,J,K,X,TLINE,II,K,'dummy')
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      IF(EDREQU.EQ.'READ ERROR')THEN
        WRITE(*,IOFORM)'Error in keyed in data.'
        DFF=.FALSE.
        GO TO 9900
      END IF
      IF(FLAG.LT.NZERO)I=J
      DFF=DFF1(TLINE,I,NFIELD)
 9900 CONTINUE
      RETURN
      END
C
C
C
      FUNCTION DFF1(TLINE,I,NFIELD)
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  DFF1
      CHARACTER*(*) TLINE
      INTEGER             I,NFIELD
      INTEGER   N1
      PARAMETER(N1=1)
      LOGICAL EXPDF
      INTEGER II,ILNB,IS,IS1
      IF(.NOT.EXPDF(TLINE))GO TO 9900
      II=ILNB( TLINE )
      IF(I.GT.N1)THEN
        IF(I.GT.NFIELD)I=NFIELD+N1
C
C   ADD LEADING COMMAS TO FORCE DEFAULT OF FIELDS BEFORE <I>
C
        IS1=II+I
        DO 100 IS=II,N1,-N1
        IS1=IS1-N1
        TLINE(IS1:IS1)=TLINE(IS:IS)
  100   CONTINUE
        TLINE(1:I-N1)=',,,,,,,,,,,,,,,,,,,,,,,,,'
      END IF
      DFF1=.TRUE.
      GO TO 9990
 9900 CONTINUE
      DFF1=.FALSE.
 9990 RETURN
      END
