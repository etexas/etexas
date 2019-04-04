      SUBROUTINE SPACED(NLINE,NF,NFS,NS,MLINE)
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
C   THIS SUBROUTINE INSERTS <NS> SPACES BETWEEN VARIABLE
C   SIZED FIELDS OF TEXT IN THE CHARACTER VARIABLE <NLINE>.
C   <NFS> IS A DIMENSIONED ARRAY WHOSE ELEMENTS CONTAIN
C   THE NUMBER OF CHARACTERS IN EACH OF THE <NF> FIELDS
C   TO BE "SPACED".
C    <MLINE> IS THE CHARACTER VARIABLE THAT RETURNS THE
C   SPACED TEXT.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     NLINE          ,MLINE
      INTEGER                 NF,NFS(*),
     1                               NS
      INTEGER   NZERO  ,N1
      PARAMETER(NZERO=0,N1=1)
      INTEGER   I,IT1,IT2,IT3,IT4,IT5,NSM1
      IT2=NZERO
      IT3=N1-NFS(N1)
      IT4=NZERO
      NSM1=NS-N1
      DO 100 I=N1,NF-N1
      IT5=NFS(I)
      IT1=IT2+N1
      IT2=IT1+IT5+NSM1
      IT3=IT4+N1
      IT4=IT4+IT5
  100 MLINE(IT1:IT2)=NLINE(IT3:IT4)
      IT5=NFS(NF)
      IT1=IT2+N1
      IT2=IT2+IT5
      IT3=IT4+N1
      IT4=IT4+IT5
      MLINE(IT1:IT2)=NLINE(IT3:IT4)
      IF(IT2.LT.LEN( MLINE ))MLINE(IT2+N1:)=' '
      RETURN
      END
