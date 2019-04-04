      SUBROUTINE PTDD(NB1,LW,TXT,DAT,NLFIO)
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
C   THIS SUBROUTINE PRINTS A DATA LINE CONSISTING OF A
C   DESCRIPTIION OF THE DATA, A VARIABLE NUMBER OF DASHES AND A
C   DATA FIELD.
C   THE NUMBER OF DASHES VARIES ACCORDING TO THE DESCRIPTION
C   LENGTH SO THAT THE DATA FIELDS ARE LINED UP AT THE
C   LAST POSITION.
C
C   NB1 - NUMBER OF BLANKS TO PRECEED THE DATA DESCRIPTION
C   LW - DESIRED CHARACTER POSITION OF THE LAST DATA CHARACTER
C   TXT - CHARACTER VARIABLE CONTAINING THE DATA DESCRIPTION
C   DAT - CHARACTER VARIABLE CONTAINING THE DATA
C   NLFIO - UNIT NUMBER OF THE OUTPUT FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER         NB1,LW        ,NLFIO
      CHARACTER*(*)          TXT,DAT
      INTEGER   N1
      CHARACTER*4    IFMT
      PARAMETER(N1=1,IFMT='(6A)')
      INCLUDE 'CMPR01'
      CHARACTER BLANK*140,DASH*140,TXTU*140,DATU*140
      INTEGER   ILNB,ITEMP,NC,NCD,NCT
      DATA BLANK / ' ' /
      DATA DASH(1:35)   /'-----------------------------------'/
      DATA DASH(36:70)  /'-----------------------------------'/
      DATA DASH(71:105) /'-----------------------------------'/
      DATA DASH(106:140)/'-----------------------------------'/
      NCT=ILNB( TXT )
      ITEMP=LW-NB1
      DATU=DAT
      CALL  TOUPR   ( DATU )
      TXTU=TXT
      CALL  TOUPR   ( TXTU )
      IF(ITEMP.LE.NCT)THEN
C
C ----- PRINT ONLY <NB1> BLANKS AND <ITEMP> CHARACTERS OF <TXT>
C
        IF(ITEMP.GT.0)WRITE(NLFIO,IFMT)BLANK(1:NB1),TXTU(1:ITEMP)
        GO TO 9990
      END IF
      NCD=LEN( DAT )
      NC=MAX0(LW-NB1-NCT-NCD-2,1)
      NCD=MAX0(ILNB( DAT ),1)
      IF(NB1.LE.0)THEN
        WRITE(NLFIO,IFMT)TXTU(1:NCT),BLANK(1:N1),DASH(1:NC),BLANK(1:N1),
     1                   DATU(1:NCD)
      ELSE
        WRITE(NLFIO,IFMT)BLANK(1:NB1),TXTU(1:NCT),BLANK(1:N1),
     1                   DASH(1:NC),BLANK(1:N1),DATU(1:NCD)
      END IF
 9990 CONTINUE
      RETURN
      END
