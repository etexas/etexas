      FUNCTION STOH(NF1,NF2,NFS,NFT,NLINE)
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
      LOGICAL  STOH
      INTEGER       NF1,NF2,NFS(*)
      CHARACTER*4               NFT(*)
      CHARACTER*(*)                 NLINE
      CHARACTER*1 IC1    ,IC2
      PARAMETER  (IC1='1',IC2='2')
      INTEGER     NZERO  ,N1  ,N2  ,N8  ,N9  ,N14   ,N19   ,N29
      PARAMETER  (NZERO=0,N1=1,N2=2,N8=8,N9=9,N14=14,N19=19,N29=29)
      INTEGER     N34
      PARAMETER  (N34=34)
      CHARACTER*4 FMT1
      PARAMETER  (FMT1='(I1)')
      INCLUDE 'CMPR01'
      CHARACTER FMT*22,TEXT*37,NFTU*4
      INTEGER   I,II,IPCT,IT1,IT2,NC
      DATA FMT/'(/A,I ,A,I ,A,I ,A/,A)'/
      DATA TEXT/'Sum of percentages in fields thru is '/
      IT2=NZERO
      DO 100 I=N1,NF1-N1
  100 IT2=IT2+NFS(I)
      NFTU=NFT(NF1)
      CALL  TOUPR   ( NFTU )
      IF(NFTU(1:N1).NE.'I')GO TO 9900
      IPCT=NZERO
      DO 300 I=NF1,NF2
      IT1=IT2+N1
      IT2=IT2+NFS(I)
      READ(NLINE(IT1:IT2),'('//NFT(I)//')',ERR=300)II
      IPCT=IPCT+II
  300 CONTINUE
      IF(IPCT.EQ.100)GO TO 9900
      CALL NCFMT(FMT(6:),NF1)
      CALL NCFMT(FMT(11:),NF2)
      CALL INTFMT(NC,FMT(16:),IPCT)
      WRITE(*,FMT)TEXT(1:N29),NF1,TEXT(N29:N34),NF2,
     1            TEXT(N34:37),IPCT,'%',
     2            'Revise this data so that the sum is 100%'
      STOH=.FALSE.
      GO TO 9990
 9900 STOH=.TRUE.
 9990 RETURN
      END
