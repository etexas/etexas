      SUBROUTINE PRINRG(NPHPS,LPHPS,IR,IG,NRG,NGR,PRG,NR,NC)
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
C  NPHPS - NUMBER OF PHASES IN THE LISTS
C  LPHPS - LIST OF THE PHASES IN EACH RING AND GROUP
C  IR - NUMBER OF RINGS
C  IG - NUMBER OF GROUPS
C  NRG - MAX NUMBER OF RINGS
C  NGR - MAX NUMBER OF GROUPS
C  PRG - DIMENSIONED CHARACTER VARIABLE TO RETURN TEXT TO PRINT
C  NR - RETURN NUMBER OF ROWS THAT WERE USED
C  NC - IF NOT NEGATIVE, RETURN NUMBER OF CHARACTERS TO PRINT ON A ROW
C       IF NEGATIVE, PRINT DIRECTLY TO UNIT THAT IS ABS(NC), DON'T USE PRG
C
C-----PRINT A DIAGRAM OF THE PREFERRED SEQUENCES
C-----IF PHASE ON LIST IS ZERO, PRINT AS BLANK
C-----IF PHASE ON LIST IS NEGATIVE, PRINT AS CHAR OF ABSOLUTE VALUE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER                       IR,IG,NRG,NGR    ,NR,NC
      INTEGER           NPHPS(NRG,NGR),
     1                        LPHPS(NRG,NGR,*)
      CHARACTER*(*)                               PRG(NRG+NRG+5)
      INTEGER   MAX
      DIMENSION MAX(10)
      CHARACTER NLINE1*80,NLINE2*80,NLINE2X*80,DASH*30
      INTEGER   I,J,K,L,LAST,NC1,NEXT,NIO,NR1,NRMAX
      DATA DASH / '------------------------------' /
      IF (NC .LT. 0) THEN
        NIO = IABS(NC)
      ELSE
        NRMAX = NRG + NRG + 5
        NR1 = 0
        NIO = 0
      END IF
      NEXT = 11
      NLINE1 = ' '
      NLINE2 = ' '
      DO 100 J = 1, IG
      MAX(J) = 1
      DO 50 I = 1, IR
C-----FOR EACH GROUP, FIND MAX NUMBER OF PHASES IN ANY RING
      IF (NPHPS(I,J) .GT. MAX(J)) MAX(J) = NPHPS(I,J)
   50 CONTINUE
      LAST = NEXT + (3 * MAX(J)) - 1
      IF (MAX(J) .EQ. 1) THEN
        NLINE1(NEXT:LAST) = 'GR'
        WRITE (NLINE2(NEXT:LAST),'(I2)') J
      ELSE
        IF (MAX(J) .GT. 2) NEXT = NEXT + ((((MAX(J) - 2) * 3) + 1) / 2)
        NLINE1(NEXT:LAST) = 'GROUP'
        WRITE (NLINE2(NEXT:LAST),'(I3)') J
      END IF
      NEXT = LAST + 2
  100 CONTINUE
      NC1 = LAST
      IF (NIO .GT. 0) THEN
        WRITE (NIO,'(A)') NLINE1(1:NC1)
        WRITE (NIO,'(A)') NLINE2(1:NC1)
      ELSE
        NR1 = NR1 + 1
        IF (NR1 .GT. NRMAX) THEN
          NR1 = NR1 - 1
          GO TO 9990
        END IF
        PRG(NR1) = NLINE1(1:NC1)
        NR1 = NR1 + 1
        IF (NR1. GT. NRMAX) THEN
          NR1 = NR1 - 1
          GO TO 9990
        END IF
        PRG(NR1) = NLINE2(1:NC1)
      END IF
      NLINE1(1:10) = '         |'
      NLINE2      = ' RING    |'
      NEXT = 11
      DO 200 J = 1, IG
      NC1 = 3 * MAX(J)
      LAST = NEXT + NC1 - 1
      WRITE (NLINE1(NEXT:LAST),'(A)',ERR=190) DASH(1:NC1)
  190 CONTINUE
      NEXT = LAST + 1
      NLINE1(NEXT:NEXT) = '|'
      NLINE2(NEXT:NEXT) = '|'
      NEXT = NEXT + 1
  200 CONTINUE
      NC1 = NEXT - 1
      IF (NIO .GT. 0) THEN
        WRITE (NIO,'(A)') NLINE1(1:NC1)
      ELSE
        NR1 = NR1 + 1
        IF (NR1 .GT. NRMAX) THEN
          NR1 = NR1 - 1
          GO TO 9990
        END IF
        PRG(NR1) = NLINE1(1:NC1)
      END IF
      NLINE2X = NLINE2
      DO 300 I = 1, IR
      NEXT = 11
      WRITE (NLINE2(6:7),'(I2)',ERR=240) I
  240 CONTINUE
      DO 250 J = 1, IG
      LAST = NEXT + 2
      DO 245 K = 1, MAX0(NPHPS(I,J),MAX(J))
      L = LPHPS(I,J,K)
      IF (L .NE. 0) THEN
        IF (L. GT. 0) THEN
          WRITE (NLINE2(NEXT:LAST),'(I2)',ERR=242) L
        ELSE
          WRITE (NLINE2(NEXT:LAST),'(1X,A1)',ERR=242) CHAR( IABS(L) )
        END IF
      END IF
  242 CONTINUE
      NEXT = NEXT + 3
      LAST = LAST + 3
  245 CONTINUE
      NEXT = NEXT + 1
      LAST = LAST + 1
  250 CONTINUE
      IF (NIO .GT. 0) THEN
        WRITE (NIO,'(A)') NLINE2(1:NC1)
        WRITE (NIO,'(A)') NLINE1(1:NC1)
      ELSE
        NR1 = NR1 + 1
        IF (NR1 .GT. NRMAX) THEN
          NR1 = NR1 - 1
          GO TO 9990
        END IF
        PRG(NR1) = NLINE2(1:NC1)
        NR1 = NR1 + 1
        IF (NR1.GT.NRMAX) THEN
          NR1 = NR1 - 1
          GO TO 9990
        END IF
        PRG(NR1) = NLINE1(1:NC1)
      END IF
      NLINE2 = NLINE2X
  300 CONTINUE
 9990 CONTINUE
      IF(NC.GE.0)THEN
        NR=NR1
        NC=NC1
      END IF
      RETURN
      END                                                               PRINRG
