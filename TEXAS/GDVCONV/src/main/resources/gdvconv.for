      PROGRAM GDVCONV
C                      PRE   C
C                      SYSDAT
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
      IMPLICIT NONE                                                     CCODE=C.
      INTEGER      PARNUM
      PARAMETER   (PARNUM=5)
      INCLUDE 'CONSTN'
      INCLUDE 'CMTX01'
      INCLUDE 'CWDDIR'
      INCLUDE 'USFILE'
      INTEGER      NCPV  (PARNUM)
      CHARACTER*10 PAR   (PARNUM)
      CHARACTER*60 PARVAL(PARNUM),
     *                       PFILE  ,           CFILE  ,
     *                       SYSDAT ,           WRKDIR ,
     *                       PAUSND
      EQUIVALENCE (PARVAL(1),PFILE ),(PARVAL(2),CFILE ),
     *            (PARVAL(3),SYSDAT),(PARVAL(4),WRKDIR),
     *            (PARVAL(5),PAUSND)
      CHARACTER    DEFILE*60,TAG*3,GDFILE*60
      INTEGER      I,ILNB,N,NIN,NIO
      DATA NCPV   / -1   ,-1 ,-1       ,-1       ,-1        /
      DATA PAR    / 'PRE','C','SYS_DAT','WRK_DIR','PAUSEND' /
      DATA PARVAL / ' '  ,' ',' '      ,' '      ,'YES'     /
  884 FORMAT(' GDVCONV ERROR IN GDVS0 CALL')
C-----INITIALIZE CONSTANTS
      CALL  INITCN
C
C ----- PARAMETERS FROM COMMAND LINE
C
C   FIRST POSITIONAL PARAMETER (PRE= ) - PREPROCESSOR FILE TO BE CONVERTED
C   SECOND POSITIONAL PARAMETER (C= ) - CONVERTED DATA FILE
C
      CALL CLFILE(PARNUM,PAR,PARVAL,NCPV,'gdvconv')
      CWDDIR=WRKDIR
      CALL TOUPR(PAUSND)
C
C ----- READ INITIALIZATION DATA
C
      N=1
      NIO=0
      CALL GDVS0(N,1,NOUT,NIO,I,I,I,I,SYSDAT,
     1            ' ','NO',DEFILE,USFILE,'NO','NO','NO','NO')
      IF(N.EQ.-1)THEN
        WRITE(*,884)
        STOP    884
      END IF
      IF(ILNB( CFILE  ).EQ.0)CFILE='gdv'
C%    IF(ILNB( CWDDIR ).GT.0)USFILE=CWDDIR
C%    IF(ILNB( CFILE  ).GT.0)CALL PCFS(CFILE ,USFILE,CFILE )
C%    IF(ILNB( DEFILE ).GT.0)CALL PCFS(DEFILE,USFILE,DEFILE)
C%    IF(ILNB( PFILE  ).GT.0)CALL PCFS(PFILE ,USFILE,PFILE )
      NIN=NOUT(1)-1
      NOUT(1)=NIO
      GDFILE='ECHO'
      CALL GDVCON(-NIN,TAG,NOUT,GDFILE,PFILE,CFILE,DEFILE,USFILE)
      IF ( PAUSND . EQ . 'YES' )                 THEN
        CALL EXIT ( 0 )
      END IF
      END
C
C
C
      BLOCK DATA
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'IVERSN'
      DATA IVERSN / 'V6.00' /
      END
