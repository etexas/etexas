C     PROGRAM TEST
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
C     CHARACTER*10 PAR(4)
C     CHARACTER*60 PARVAL(4)
C     DIMENSION NCPV(4)
C     DATA PAR    / 'I','O',' ','END' /
C     DATA PARVAL / ' ',' ',' ','DNE' /
C     CALL CLFILE(4,PAR,PARVAL,NCPV,'test')
C     DO 100 I=1,4
C     NC=NCPV(I)
C     IF      ( NC.EQ.-1 ) THEN
C       WRITE(6,'(A,I1,A)')' PAR(',I,') not specified'
C     ELSE IF ( NC.EQ.0  )
C       WRITE(6,'(A,I1,A)')' PAR(',I,') specified without a value'
C     ELSE
C       WRITE(6,'(A,I1,3A)')' PAR(',I,') = "',PARVAL(I)(1:NC),'"'
C     END IF
C 100 CONTINUE
C     END
C
C
C
      SUBROUTINE CLFILE(PARNUM,PAR,PARVAL,NCPV,PROG)
C
C ----- THIS SUBROUTINE GETS POSITIONAL AND KEYWORD (NOT VAX VMS)
C ----- PARAMETERS FROM THE COMMAND LINE.
C ----- POSITIONAL PARAMETERS ARE NUMBERED LEFT TO RIGHT, WITH
C ----- FIRST PARAMETER TO RIGHT OF PROGRAM NAME AS 1.
C ----- KEYWORD PARAMETERS HAVE THE FORM: KEYWORD=PARAMETER .
C ----- KEYWORD PARAMETERS ARE NOT CONSIDERED WHEN NUMBERING
C ----- POSITIONAL PARAMETERS.
C
C   PARNUM - MAXIMUM NUMBER OF PARAMETERS
C   PAR    - DIMENSIONED CHARACTER ARRAY WITH KEYWORDS
C   PARVAL - DIMENSIONED CHARACTER ARRAY TO RETURN THE PARAMETER VALUES
C   NCPV   - NUMBER OF CHARACTERS IN RETURNED <PARVAL>
C          -1 - PARAMETER NOT FOUND
C           0 - KEYWORD FOUND WITHOUT PARAMETER TO RIGHT OF =
C          >0 - NUMBER OF CHARACTERS
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           PARNUM           ,NCPV(PARNUM)
      CHARACTER*(*)            PAR(PARNUM)
      CHARACTER*(*)                PARVAL(PARNUM)
      CHARACTER*(*)                            PROG
      CHARACTER*1 SDSEP
C?    PARAMETER  (SDSEP='/')
      PARAMETER  (SDSEP='\')                                            CCODE=C#
C}    PARAMETER  (SDSEP='/')
C%    PARAMETER  (SDSEP=':')
      CHARACTER*10 CLPAR,CLPARU
      CHARACTER*60 VALPAR,CLPARM
      INTEGER      I,ILNB,J,K,NCCL,NCEQ,NCP,NCPAR,NP,NPP,NPT
C?    INTEGER*4 IARGC
C@    CHARACTER*60 ARGV
C@    INTEGER*2 ARGC,NP
C~    CHARACTER*60 ARGV
C~    INTEGER*2 ARGC,NP
C|    INTEGER*4 SYS$TRNLOG
      INTEGER*4 IARGC                                                   CCODE=C{
      NCP=ILNB( PROG )
      IF(NCP.EQ.0)THEN
        WRITE(*,'(A)')'Error in CLFILE: program name not specified'
        STOP 900
      END IF
      DO 10 I=1,PARNUM
      NCPV(I)=-1
   10 CONTINUE
C?    NPT=IARGC()
C@    NPT=ARGC()-1
C~    NPT=ARGC()-1
C|    NC=0
C|    NPT=0
      NPT=IARGC()                                                       CCODE=C{
C%    NPT=PARNUM
C
C ----- THE LOGICAL NAME "NCLP" MUST BE SET TO NUMBER OF
C ----- COMMAND LINE PARAMETERS
C
C|    IF(SYS$TRNLOG('NCLP',NC,CLPARM,,,).EQ.1)THEN
C|      IF(NC.EQ.1)THEN
C|        READ(CLPARM(1:1),'(I1)')NPT
C|      ELSE
C|        READ(CLPARM(1:2),'(I2)')NPT
C|      END IF
C|    END IF
C
C%    OPEN(9,FILE=PROG(1:NCP)//'.par',STATUS='OLD',ACTION='READ',ERR=901)
      IF(NPT.GT.PARNUM)NPT=PARNUM
      NPP=0
      DO 100 NP=1,NPT
C
C ----- FOR VAX VMS, THE LOGICAL NAME "CLPi" MUST BE SET
C ----- FOR EACH OF THE i COMMAND LINE PARAMETERS
C
C|    CLPAR='CLP'
C|    IF(NP.LT.10)THEN
C|      WRITE(CLPAR(4:4),'(I1)')NP
C|      I=4
C|    ELSE
C|      WRITE(CLPAR(4:5),'(I2)')NP
C|      I=5
C|    END IF
C|    CLPARM=' '
C|    NCPAR=0
C|    IF(SYS$TRNLOG(CLPAR(1:I),NCPAR,CLPARM,,,).NE.1)GO TO 100
C|    IF(CLPARM(1:NCPAR).EQ.'0')GO TO 100
C|    PARVAL(NP)=CLPARM(1:NCPAR)
C|    NCPV(NP)=ILNB( PARVAL(NP) )
C|    GO TO 100
C|101 CONTINUE
C@    CLPARM=ARGV(NP)
C~    CLPARM=ARGV(NP)
C?    CALL GETARG(NP,CLPARM)
      CALL GETARG(NP,CLPARM)                                            CCODE=C{
C%    READ(9,'(A)',END=102,ERR=902)CLPARM
      NCCL=ILNB( CLPARM )
      IF(NCCL.EQ.0)GO TO 100
      CLPARM=CLPARM(1:NCCL)
      CLPAR=CLPARM
      CLPARU=CLPAR
      CALL TOUPR(CLPARU)
      VALPAR=' '
      DO 40 J=1,PARNUM
      NCP=ILNB( PAR(J) )
      IF((NCCL.EQ.NCP) .AND. (CLPARU(1:NCCL).EQ.PAR(J)(1:NCP)))THEN
C
C ----- FOUND KEYWORD WITHOUT =
C
        NCPV(J)=0
        GO TO 100
      END IF
   40 CONTINUE
      NCEQ=INDEX(CLPARM,'=')
C
C ----- DOS REMOVES THE = BEFORE SENDING COMMAND LINE PARAMETERS
C ----- TO AN APPLICATIOM
C ----- + IS USED FOR DOS TO SEPARATE KEYWORDS FROM PARAMETERS
C
      IF(NCEQ.EQ.0)NCEQ=INDEX(CLPARM,'+')
      IF(NCEQ.EQ.1)THEN
        WRITE(*,'(A,I2,A)')'Error in CLFILE for '//
     *                     PROG(1:ILNB( PROG ))//
     *                     ': parameter ',NP,' = "'//
     *                     CLPARM(1:ILNB( CLPARM ))//
     *                     '" starts with "'//CLPARM(1:1)//'"'
        STOP 903
      END IF
      IF(NCEQ.GT.0)THEN
        CLPAR=CLPARM(1:NCEQ-1)
        NCPAR=ILNB( CLPAR )
        CLPARU=CLPAR
        CALL TOUPR(CLPARU)
        NCPAR=MAX0(NCPAR,1)
        IF(NCEQ+1.LE.LEN( CLPARM ))VALPAR=CLPARM(NCEQ+1:)
C
C ----- LOOK FOR KEYWORD MATCH
C
        DO 50 J=1,PARNUM
        NCP=ILNB( PAR(J) )
        IF((NCPAR.EQ.NCP) .AND. (CLPARU(1:NCPAR).EQ.PAR(J)(1:NCP)))THEN
C       PRINT(*,PARNUM)
C ----- MATCHES ONE OF THE KEYWORDS
C
          PARVAL(J)=VALPAR
          IF(PAR(J).EQ.'GDVS00' .OR.
     *       PAR(J).EQ.'SYS_DAT'.OR.
     *       PAR(J).EQ.'JSEED'.OR.
     *       PAR(J).EQ.'REP'.OR.
     *       PAR(J).EQ.'WRK_DIR')THEN
            K=ILNB( PARVAL(J) )
            IF(PARVAL(J)(K:K).NE.SDSEP)PARVAL(J)=PARVAL(J)(1:K)//SDSEP
          END IF
          NCPV(J)=ILNB( PARVAL(J) )
          GO TO 100
        END IF
   50   CONTINUE
        WRITE(*,'(A,I2,A)')'Error in CLFILE for '//
     *                     PROG(1:ILNB( PROG ))//
     *                     ': parameter ',NP,' = "'//
     *                     CLPARM(1:ILNB( CLPARM ))//
     *                     '" - keyword = "'//CLPARU(1:NCPAR)//
     *                     '" is not valid'
        STOP 904
      END IF
C
C ----- POSITIONAL PARAMETER
C
      NPP=NPP+1
      PARVAL(NPP)=CLPARM
      NCPV(NPP)=ILNB( PARVAL(NPP) )
  100 CONTINUE
C%102 CONTINUE
C%    CLOSE(9)
C!    write (*,*) 'clfile for '//prog(1:ilnb( prog ))
C!    do i=1,parnum
C!    ncp=ncpv(i)
C!    if      ( ncp.eq.-1 )  then
C!      write (*,'(a,i2,a)') ' par(',i,')="'//par(i)(1:ilnb( par(i) ))//
C!   *                       '" not specified'
C!    else if ( ncp.eq.0  )  then
C!      write (*,'(a,i2,a)') ' par(',i,')="'//par(i)(1:ilnb( par(i) ))//
C!   *                       '" specified without a value'
C!    else
C!      write (*,'(a,i2,a)') ' par(',i,')="'//par(i)(1:ilnb( par(i) ))//
C!   *                       '"  parval="'//parval(i)(1:ncp)//'"'
C!    end if
C!    end do
      RETURN
C%901 CONTINUE
C%    WRITE(*,'(A)')'Error in CLFILE opening '//
C%   *              'command line parameter file '//
C%   *              PROG(1:NCP)//'.par'
C%    STOP 901
C%902 CONTINUE
C%    WRITE(*,'(A,I2,A)')'Error in CLFILE reading line ',NP,
C%   *                   ' from command line parameter file '//
C%   *                   PROG(1:NCP)//'.par'
C%    STOP 902
      END
C
C
C
C~    INTEGER*2 FUNCTION ARGC()
C~    CHARACTER*127 CL
C
C ----- THIS IS JUST LIKE THE RYAN-MCFARLAND SUPPLIED FUNCTION
C
C~    CALL FIXCL(CL,NCLP)
C~    ARGC=NCLP+1
C~    RETURN
C~    END
C
C
C
C~    CHARACTER*(*) FUNCTION ARGV(N)
C
C ----- THIS IS JUST LIKE THE RYAN-MCFARLAND SUPPLIED FUNCTION
C
C~    CHARACTER*127 PARM
C~    INTEGER*2 N
C~    NN=N
C~    CALL GETPN(NN,PARM)
C~    ARGV=PARM
C~    RETURN
C~    END
C
C
C
C~    BLOCK DATA CLFBD
C
C   CLF - CHARACTER VARIABLE FOR EDITED COMMAND LINE
C   ICPOS - ARRAY WITH CHARACTER POSITIONS OF SEPERATORS
C   NCLP - NUMBER OF COMMAND LINE PARAMETERS (INCLUDING EMPTY ONES)
C   NCCL - NUMBER OF CHARACTERS ON COMMAND LINE
C   FIRST - LOGICAL VARIABLE, WHEN .TRUE., VARIABLES IN THIS COMMON
C           BLOCK NOT DEFINED
C~    COMMON /J4CLPS/ CLF,ICPOS(20),NCLP,NCCL,FIRST
C~    CHARACTER CLF*127
C~    LOGICAL FIRST
C~    SAVE /J4CLPS/
C~    DATA CLF / ' ' /
C~    DATA (ICPOS(I),I=1,20),NCLP / 21*0 /
C~    DATA FIRST / .TRUE. /
C~    END
C
C
C
C~    SUBROUTINE  FIXCL(CLT,NCLPT)
C
C ----- THIS SUBROUTINE GETS THE COMMAND LINE PARAMETERS, REMOVES
C ----- MULTIPLE SPACES AND FINDS THE SEPERATORS (SPACE, SLASH, COMMA).
C ----- MAX COMMAND LINE LENGTH IS 127.  MAX NUMBER OF PARAMETERS IS 20.
C
C  CLT - CHARACTER VARIABLE TO RETURN THE EDITED COMMAND LINE
C  NCLPT - NUMBER OF COMMAND LINE PARAMETERS FOUND(INCLUDING EMPTY ONES)
C
C~    CHARACTER*1 CLT*(*),CL1,SPACE,COMMA,SLASH
C~    LOGICAL CSP,PSP,PSEP
C~    PARAMETER(SPACE=' ',COMMA=',',SLASH='/',N0=0,N1=1,N20=20)
C~    COMMON /J4CLPS/ CLF,ICPOS(20),NCLP,NCCL,FIRST
C~    CHARACTER CLF*127
C~    LOGICAL FIRST
C~    SAVE /J4CLPS/
C~    DATA PSP,PSEP / 2*.TRUE. /
C~    IF(.NOT.FIRST)GO TO 990
C~    NCCL=N0
C~    FIRST=.FALSE.
C~    CALL GETCL(CLF)
C~    ICSP=ICHAR( ' ' )
C~    DO 100 I=127,N1,-N1
C~    ILNB=I
C~    II=ICHAR( CLF(I:I) )
C~    IF(II.EQ.N0)GO TO 100
C~    IF(II.NE.ICSP)GO TO 110
C~100 CONTINUE
C~    GO TO 990
C~110 CONTINUE
C~    NCCL=N0
C~    DO 980 I=N1,ILNB
C~    CL1=CLF(I:I)
C
C ----- REMOVE MULTIPLE SPACES OR SPACES AFTER SEPERATORS
C
C~    IF(CL1.EQ.SPACE)THEN
C~      IF(PSEP)GO TO 980
C~      IF(PSP)GO TO 980
C~      CSP=.TRUE.
C~    ELSE
C~      CSP=.FALSE.
C~    END IF
C
C ----- FIND SEPERATORS
C
C~    IF(CSP .OR. (CL1.EQ.COMMA) .OR. (CL1.EQ.SLASH))THEN
C~      PSEP=.TRUE.
C~      IF(PSP)THEN
C
C ----- OVERWRITE PREVIOUS SPACE WITH THIS SEPERATOR
C
C~        PSP=.FALSE.
C~        GO TO 950
C~      END IF
C~      NCCL=NCCL+N1
C~      IF(NCLP.LT.N20)THEN
C~        NCLP=NCLP+N1
C~        ICPOS(NCLP)=NCCL
C~      END IF
C~      GO TO 950
C~    ELSE
C~      PSEP=.FALSE.
C~    END IF
C~    NCCL=NCCL+N1
C~950 CONTINUE
C~    CLF(NCCL:NCCL)=CL1
C~    PSP=CSP
C~980 CONTINUE
C~    IF(NCCL.LT.ILNB)CLF(NCCL+N1:ILNB)=' '
C~    IF(NCLP.LT.N20)THEN
C
C ----- ASSUME A SEPERATOR AT THE END
C
C~      NCLP=NCLP+N1
C~      ICPOS(NCLP)=NCCL+N1
C~    END IF
C~990 CONTINUE
C~    CLT=CLF
C~    NCLPT=NCLP
C~    RETURN
C~    END
C
C
C
C~    SUBROUTINE GETPN(NP,CLPAR)
C
C ----- THIS SUBROUTINE RETURNS PARAMETERS FROM THE COMMAND LINE
C ----- A PARAMETER IS ANY TEXT BETWEEN SEPERATORS
C ----- PARAMETERS ARE NUMBERED FROM LEFT TO RIGHT
C ----- PARAMETER AFTER THE PROGRAM NAME IS 1
C
C  NP - PARAMETER TO RETURN, 0 FOR NEXT
C  CLPAR -  CHARACTER VARIABLE TO RETURN PARAMETER
C
C~    CHARACTER CLPAR*(*),CLT*1
C~    INTEGER*2 NP
C~    PARAMETER(N0=0,N1=1)
C~    SAVE NPT
C~    COMMON /J4CLPS/ CLF,ICPOS(20),NCLP,NCCL,FIRST
C~    CHARACTER CLF*127
C~    LOGICAL FIRST
C~    SAVE /J4CLPS/
C~    DATA NPT / N0 /
C~    IF(FIRST)CALL FIXCL(CLT,NCLPT)
C~    CLPAR=' '
C~    IF(NP.LT.N0)GO TO 990
C~    IF (NP .EQ. N0 ) THEN
C~      NPT = NPT +N1
C~    ELSE
C~      NPT = NP
C~    END IF
C~    IF(NPT.GT.NCLP)GOTO 990
C~    IF (NPT.EQ.N1) THEN
C~      NS1 = N1
C~      NS2= ICPOS(NPT) -N1
C~    ELSE
C~      NS1= ICPOS(NPT-N1)+N1
C~      NS2= ICPOS(NPT)-N1
C~    END IF
C~    IF(NS2.LT.NS1)GO TO 990
C~    CLPAR=CLF(NS1:NS2)
C~    GO TO 990
C~990 CONTINUE
C~    RETURN
C~    END
