      SUBROUTINE SHONAM(TEXT,NAME,NLN)
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
C ----- PRINT THE DESCRIPTION AND A FILE NAME ON 1 OR 2 LINES
C ----- FORMATS OF PRINT:
C
C          TEXT "NAME"
C
C                 OR
C
C          TEXT:
C            "NAME"
C
C ----- IF TOTAL NUMBER OF CHARATERS EXCEEDS THE MAX, USE SECOND FORMAT
C
C --- TEXT - DESCRIPTION
C --- NAME - FILENAME
C --- NLN - MAX CHARACTERS TO PUT ON ONE LINE
C           IF NEGATIVE, PUT A PERIOD AT END
C --- IO - UNIT NUMBER OF OPEN PRINT FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     TEXT,NAME
      INTEGER                     NLN
      CHARACTER*2 SUFFIX
      INTEGER     ILNB,NC1,NC2,NLN1,NS
      IF(NLN.LT.0)THEN
        NLN1=IABS(NLN)
        SUFFIX='".'
        NS=2
      ELSE
        NLN1=NLN
        SUFFIX='"'
        NS=1
      END IF
      NC1=ILNB( TEXT )
      NC2=ILNB( NAME )
      IF(NC1+NC2+3.GT.NLN1)THEN
        WRITE(*,'(2A/3A)')TEXT(1:NC1),':','     "',NAME(1:NC2),
     1                    SUFFIX(1:NS)
      ELSE
        WRITE(*,'(4A)')TEXT(1:NC1),' "',NAME(1:NC2),SUFFIX(1:NS)
      END IF
      RETURN
      END
