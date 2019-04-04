      FUNCTION DIGREF(INQUIRE)
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
C ----- DISPLAY DIAMOND INTERSECTION GEOMETRY REFERENCE SKETCH
C ----- IF RESPONSE TO PROMPT IS "ONLY",       DIGREF IS SET .TRUE.
C                                "YES" OR "NO, DIGREF IS SET .FALSE.
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  DIGREF
      LOGICAL         INQUIRE
      INTEGER   N1
      PARAMETER(N1=1)
      INCLUDE 'CMPR01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXSR'
      INCLUDE 'CMCH01'
      INTEGER   NC
      DIMENSION NC(22)
      CHARACTER*80 SLINE(22),TLINE*4,ALT(3)*8,TLINEU*4
      LOGICAL FIRST
      INTEGER    I,NL1
      SAVE FIRST,NC,NL1
      DATA (ALT(I),I=N1,3)/'$"only".','only','NONE'/
      DATA FIRST,NL1 / .TRUE.,0 /
      IOFORM='(A)'
      DIGREF=.FALSE.
      IF(DIA .AND.
     1  ((ICODE.EQ.13) .OR. (ICODE.EQ.12) .OR. (ICODE.EQ.49)))THEN
        IF(INQUIRE)THEN
          CALL YESNO('Do you want to see the Diamond Intersection '//
     1               'geometry reference data ?',TLINE,ALT)
          TLINEU=TLINE
          CALL  TOUPR   ( TLINEU )
          IF(TLINEU(1:N1).EQ.'N')GO TO 9990
          IF(TLINEU(1:N1).EQ.'O')DIGREF=.TRUE.
        END IF
        IF(FIRST)THEN
          FIRST=.FALSE.
          CALL RDHLP('gdvrf1',22,' ',SLINE,NC,NL1)
        END IF
        WRITE(*,IOFORM)' ','Diamond Intersection geometry '//
     1                     'reference data:'
        WRITE(*,IOFORM)(SLINE(I)(1:NC(I)),I=N1,NL1)
 9990   CONTINUE
      END IF
      RETURN
      END
