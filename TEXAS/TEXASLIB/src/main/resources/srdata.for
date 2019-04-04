      SUBROUTINE SRDATA
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
      INTEGER   NZERO  ,N1  ,N2  ,N3  ,N6  ,N20   ,N36   ,N80
      PARAMETER(NZERO=0,N1=1,N2=2,N3=3,N6=6,N20=20,N36=36,N80=80)
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXSR'
      CHARACTER*80 MKFMT,NFTU
      LOGICAL   E
      INTEGER   I,II,ILNB,IT1,IT2,ITT,NC,NF1
      IOFORM='(A)'
      WRITE(TEXT,'(A,I2.2)')'gdvs',ICODE
      CALL TXINQ(NZERO,TEXT,TDFILE,TEXT,E)
      IF(.NOT.E)CALL OPNERR(TEXT)
      NC=ILNB( TEXT )
      ITT=NOUT(N2)-N1
      CLOSE(ITT)
      CALL OPENRO(ITT,TEXT(1:NC),'OLD','SEQUENTIAL','FORMATTED',0,E)
      IF(E)CALL OPNERR(TEXT)
      REWIND ITT
C
C ----- NUMBER OF FIELDS
C
      READ(ITT,'(I2)')NF1
      IF(NF1.EQ.NZERO)THEN
C
C ----- USE SAME INFO. FOR EACH FIELD
C
        NF1=-NFIELD
        NFIELD=N1
      ELSE
        NFIELD=NF1
      END IF
C
C ----- FORMAT SPEC. FOR EACH FIELD
C
      READ(ITT,'(20A4)')(NFT(I),I=N1,NFIELD)
      DO 2000 I=N1,NFIELD
C
C ----- GET FIELD SIZE FOR FORMAT SPEC,
C
      NFTU=NFT(I)
      CALL  TOUPR   ( NFTU )
      IF(NFTU(1:N1).EQ.'F')GO TO 1020
      IF(NFTU(N3:N3).EQ.' ')GO TO 1020
      READ(NFT(I)(N2:N3),'(I2)')NFS(I)
      GO TO 2000
 1020 CONTINUE
      READ(NFT(I)(N2:N2),'(I1)')NFS(I)
 2000 CONTINUE
      TEXT=MKFMT(NFIELD,NFT,NC)
C
C ----- BOTH SETS OF DEFAULTS FOR EACH FIELD
C
      READ(ITT,IOFORM)HELP(N1),HELP(N2)
      IT2=NZERO
      IT1=N1-NFS(N1)
c     write(77,'(a,i4)')'nfield =',nfield
      DO 2490 I=N1,NFIELD
      IT1=IT2+N1
      IT2=IT2+NFS(I)
c     write(77,'(a,3i5)')'i,it1,it2 =',i,it1,it2
c     IDEF(I)=HELP(N1)(IT1:IT2)
      XDEF(I)=HELP(N2)(IT1:IT2)
 2490 CONTINUE
      IT2=NZERO
      IT1=N1-NFS(N1)
      DO 2500 I=N1,NFIELD
      IT1=IT2+N1
      IT2=IT2+NFS(I)
c     write(77,'(a,3i5)')'i,it1,it2 =',i,it1,it2
      IDEF(I)=HELP(N1)(IT1:IT2)
c     XDEF(I)=HELP(N2)(IT1:IT2)
 2500 CONTINUE
C
C ----- MAXIMUMS AND MINIMUMS FOR EACH FIELD
C
      READ(ITT,IOFORM)HELP(N1),HELP(N2)
      IT2=NZERO
      IT1=N1-NFS(N1)
      DO 2700 I=N1,NFIELD
      IT1=IT2+N1
      IT2=IT1+NFS(I)-N1
      NFTU=NFT(I)
      CALL  TOUPR   ( NFTU )
      IF(NFTU(N1:N1).EQ.'A')THEN
        RANGE(N1,I)=NZERO
        RANGE(N2,I)=NZERO
        GO TO 2700
      END IF
      TEXT='('//NFT(I)//')'
      IF(NFTU(N1:N1).EQ.'F')THEN
        READ(HELP(N1)(IT1:IT2),TEXT(1:N6))RANGE(N1,I)
        READ(HELP(N2)(IT1:IT2),TEXT(1:N6))RANGE(N2,I)
        GO TO 2700
      END IF
      READ(HELP(N1)(IT1:IT2),TEXT(1:N6))II
      RANGE(N1,I)=II
      READ(HELP(N2)(IT1:IT2),TEXT(1:N6))II
      RANGE(N2,I)=II
 2700 CONTINUE
      IF(NF1.GT.NZERO)GO TO 2800
      NFIELD=-NF1
      DO 2750 I=N2,NFIELD
      NFT(I)=NFT(N1)
      IDEF(I)=IDEF(N1)
      XDEF(I)=XDEF(N1)
      NFS(I)=NFS(N1)
      RANGE(N1,I)=RANGE(N1,N1)
      RANGE(N2,I)=RANGE(N2,N1)
 2750 CONTINUE
      NFIELD=NF1
 2800 CONTINUE
      READ(ITT,IOFORM)TEXT
      NCTEXT=ILNB( TEXT )
      NLHELP=NZERO
      DO 3000 I=N1,N20
 2900 READ(ITT,IOFORM,END=3020)HELP(I)
      IF(ILNB( HELP(I) ).EQ.0)GO TO 2900
      NCHELP(I)=MAX0(1,ILNB( HELP(I) ))
      NLHELP=I
 3000 CONTINUE
 3020 CONTINUE
 9999 CONTINUE
      CLOSE(ITT)
      RETURN
      END
