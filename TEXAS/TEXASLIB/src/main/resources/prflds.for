      SUBROUTINE PRFLDS(FDL,ITNF,NFS,IFF,FLAG,NS,IT2,K,L)
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
C   THIS SUBROUTINE BUILDS AND PRINTS A TEXT LINE(TO 80 CHAR.)
C   THAT "BRACKETS" MULTIPLE DATA FIELDS TO AID THE
C   USER IN ENTERING AND EDTING CARD IMAGE TYPE DATA.
C   METHOD OF BRACKETING:  \..1./ \2/ \.3./
C   FDL - IS THE CHARACTER VARIABLE THAT IS SET TO THE TEXT LINE.
C   ITNF - IS TOTAL NUMBER OF FIELDS.
C   NFS - IS A DIMENSIONED ARRAY OF FIELD SIZES.
C   IFF - IS THE NUMBER OF THE FIRST FIELD.
C   NS - IS THE NUMBER OF SPACES TO BE INSERTED BETWEEN FIELDS.
C   IT2 - IS SET TO THE NUMBER OF CHARACTERS PUT INTO <FDL>.
C   FLAG - IF LESS THAN OR EQUAL 0 TEXT LINE IS BUILT BUT NOT PRINTED.
C   K, L - SPARES.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     FDL
      INTEGER               ITNF,NFS(*),
     1                               IFF,FLAG,NS,IT2,K,L
      CHARACTER*4 OAAFMT
      INTEGER                   NZERO  ,N1  ,N2  ,N3
      PARAMETER  (OAAFMT='(4A)',NZERO=0,N1=1,N2=2,N3=3)
      CHARACTER*80 DOT
      PARAMETER   (DOT='........................................'//
     1                 '........................................')
      CHARACTER*80 DASH
      PARAMETER  (DASH='----------------------------------------'//
     1                 '----------------------------------------')
      INCLUDE 'CMPR01'
      CHARACTER*80 FMT1*4,LD*1,TD*1
      LOGICAL  INTLN
      INTEGER  I,IFLAG,IT1,IT3,IT5,ITMAX,NCFNO,NS1
      DATA FMT1 / '(I1)' /
      FDL=DOT
C
C ----- UNIX F77 NEEDS DOUBLE BACKSLASHES
C
      LD='\\'
      TD='/'
      IFLAG=N1
      GO TO 50
      ENTRY PRLEGS(FDL,ITNF,NFS,IFF,FLAG,NS,IT2,K,L)
C
C   METHOD OF BRACKETING:  /--1-\ /2\ /-3-\
C
      FDL=DASH
      LD='/'
C
C ----- UNIX F77 NEEDS DOUBLE BACKSLASHES
C
      TD='\\'
      IFLAG=N2
      GO TO 50
      ENTRY PRLNS(FDL,ITNF,NFS,IFF,FLAG,NS,IT2,K,L)
      FDL=' '
      LD=' '
      TD=' '
      IFLAG=N3
   50 CONTINUE
      ITMAX=LEN( FDL )
      IT1=80
      IF(FLAG.GE.N1)IT1=63
      IF(ITMAX.GT.IT1)ITMAX=IT1
      IT2=0
      DO 100 I=IFF,ITNF
      IT1=IT2+N1
      IF(IT1.EQ.ITMAX)GO TO 80
      IT5=NFS(I)
      IT2=IT2+IT5
      IF(IT2.LE.ITMAX)GO TO 90
      IF(IT1.LT.ITMAX)FDL(IT1:IT1)=LD
   80 IT2=ITMAX
      FDL(IT2:IT2)='>'
      GO TO 120
   90 CONTINUE
      IF((IFLAG.EQ.N2) .AND. ((I.EQ.NZERO) .OR. (I.EQ.NLGP1)) )THEN
C
C ----- LEG FIELDS FOR INTERNAL LANES OF DIAMOND INTERSECTION
C
        INTLN=.TRUE.
        NCFNO=N2
        IF(IT5.EQ.N1)GO TO 95
      ELSE
        INTLN=.FALSE.
        NCFNO=N1
        FMT1(N3:N3)='1'
        IF(I.GT.9)THEN
C
C ----- FIELD NUMBER HAS TWO DIGITS
C
          IF(IT5.EQ.N1)GO TO 95
          NCFNO=N2
          FMT1(N3:N3)='2'
        END IF
      END IF
      IF(IT5.GT.NCFNO)THEN
C
C ----- PUT LEADING DELIMITER INTO FIRST POSITION
C
        FDL(IT1:IT1)=LD
        IT1=IT1+N1
      END IF
C
C ----- CENTER FIELD NUMBER TEXT INTO FIELD
C
      IT1=IT1+(IT2-IT1+N1-NCFNO)/N2
      IT3=IT1+NCFNO-N1
      IF(INTLN)THEN
        IF(I.EQ.NZERO)THEN
          FDL(IT1:IT3)='IR'
        ELSE
          FDL(IT1:IT3)='IL'
        END IF
      ELSE
        WRITE(FDL(IT1:IT3),FMT1)I
      END IF
      IF(IT3.LT.IT2)FDL(IT2:IT2)=TD
      IF(NS.EQ.NZERO)GO TO 100
      IF(IT2+NS.GT.ITMAX)THEN
        NS1=ITMAX-IT2
      ELSE
        NS1=NS
      END IF
   95 IT3=IT2+N1
      IT2=IT2+NS1
      FDL(IT3:IT2)=' '
  100 CONTINUE
  120 CONTINUE
      IF(FLAG.LT.N1)GO TO 130
      IF(IFLAG.EQ.N1)WRITE(*,OAAFMT)'Field Numbers:   ',FDL(1:IT2)
      IF(IFLAG.EQ.N2)WRITE(*,OAAFMT)'  Leg Numbers:   ',FDL(1:IT2)
  130 CONTINUE
      IF(IT2.EQ.ITMAX)GO TO 9990
      FDL(IT2+N1:)=' '
 9990 RETURN
      END
