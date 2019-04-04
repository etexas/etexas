      SUBROUTINE PLOTS(I,XDUM,PLFILE)
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
C-----CALCOMP CALLS TO GENERATE A NON-DEVICE-SPECIFIC FILE
C
C-----I - UNIT TO OPEN FOR OUTPUT
C-----XDUM - MAXIMUM DIMENSION (HEIGHT OR WIDTH) OF PLOT
C-----PLFILE(1) - NAME OF FILE TO OPEN FOR OUTPUT
C-----PLFILE(2) - OTHER DESCRIPTIVE DATA TO BE PUT INTO PLOT FILE
C
      IMPLICIT NONE                                                     CCODE=C.
      INTEGER I
      REAL XDUM
      CHARACTER*(*)PLFILE(2)
      COMMON /GENPLTC/ IUNIT
      INTEGER          IUNIT
      IUNIT=I
      OPEN(IUNIT,FILE=PLFILE(1)
C|   1                         ,STATUS='NEW'
     1                         ,STATUS='UNKNOWN'                        CCODE=C{
C%   1                         ,STATUS='UNKNOWN'
     2                                          )
      WRITE(IUNIT,'(A10,I10,1PE15.8)')'PLOTS     ',0,XDUM
      WRITE(IUNIT,'(A)')PLFILE(2)
      RETURN
      END
C
C
C
      SUBROUTINE PLOT(X,Y,IPEN)
      IMPLICIT NONE                                                     CCODE=C.
      REAL X,Y
      INTEGER IPEN
      COMMON /GENPLTC/ IUNIT
      INTEGER          IUNIT
      WRITE(IUNIT,'(A10,1P2E15.8,I10)')'PLOT      ',X,Y,IPEN
      RETURN
      END
C
C
C
      SUBROUTINE SYMBOL(X,Y,H0,TEXT,ANG,NC)
      IMPLICIT NONE                                                     CCODE=C.
      REAL X,Y,H0,ANG
      CHARACTER TEXT*(*)
      INTEGER NC
      COMMON /GENPLTC/ IUNIT
      INTEGER          IUNIT
      INTEGER IT
      IF(NC.GT.0)THEN
        WRITE(IUNIT,'(A10,1P4E15.8,I10/A)')'SYMBOL    ',X,Y,H0,ANG,NC,
     1                                     TEXT
      ELSE
        CALL IYAD(TEXT,IT)
        WRITE(IUNIT,'(A10,1P4E15.8,I10/I10)')'SYMBOL    ',X,Y,H0,ANG,NC,
     1                                       IT
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE IYAD(TEXT,IT)
      IMPLICIT NONE                                                     CCODE=C.
C\    CHARACTER*(*) TEXT
C|    CHARACTER*(*) TEXT
C?    INTEGER TEXT
C@    CHARACTER*(*) TEXT
C~    CHARACTER*(*) TEXT
      CHARACTER*(*) TEXT                                                CCODE=C{
C%    CHARACTER*(*) TEXT
      INTEGER IT
C
C ----- TEXT WAS RECEIVED BY SYMBOL AS CHARACTER
C
C\    IT=ICHAR( TEXT(1:1) )
C|    IT=ICHAR( TEXT(1:1) )
C?    IT=TEXT
C@    IT=ICHAR( TEXT(1:1) )
C~    IT=ICHAR( TEXT(1:1) )
      IT=ICHAR( TEXT(1:1) )                                             CCODE=C{
C%    IT=ICHAR( TEXT(1:1) )
      RETURN
      END
C
C
C
      SUBROUTINE NUMBER(X,Y,H0,FPN,ANG,NDEC)
      IMPLICIT NONE                                                     CCODE=C.
      REAL X,Y,H0,FPN,ANG
      INTEGER NDEC
      COMMON /GENPLTC/ IUNIT
      INTEGER          IUNIT
      WRITE(IUNIT,'(A10,1P5E15.8,I10)')'NUMBER    ',X,Y,H0,FPN,ANG,NDEC
      RETURN
      END
C
C
C
      SUBROUTINE NEWPEN(IPEN)
      IMPLICIT NONE                                                     CCODE=C.
      INTEGER IPEN
      COMMON /GENPLTC/ IUNIT
      INTEGER          IUNIT
      WRITE(IUNIT,'(A10,I10)')'NEWPEN    ',IPEN
      RETURN
      END
C
C
C
      SUBROUTINE AXIS(X0,Y0,TEXT,NC,ALEN,ANG,FV,DV)
      IMPLICIT NONE                                                     CCODE=C.
      REAL X0,Y0,ALEN,ANG,FV,DV
      CHARACTER TEXT*(*)
      INTEGER NC
      COMMON /GENPLTC/ IUNIT
      INTEGER          IUNIT
      WRITE(IUNIT,'(A10,1P2E15.8,I10,4E15.8/A)')'AXIS      ',X0,Y0,NC,
     1                                          ALEN,ANG,FV,DV,TEXT
      RETURN
      END
