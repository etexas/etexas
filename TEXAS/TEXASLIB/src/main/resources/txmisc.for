C!    program test
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
C!    include 'cwddir'
C!    character fileold*80,filenew*80,default*80
C!    integer   ilnb,iunit
C!    logical   ex
C!101 continue
C!    write (*,*) 'enter fileold'
C!    read  (*,*,end=201) fileold
C!    if(fileold.eq.'0')fileold=' '
C!    write (*,*) 'enter default'
C!    read  (*,*) default
C!    if(default.eq.'0')default=' '
C!    write (*,*) 'enter cwddir'
C!    read  (*,*) cwddir
C!    if(cwddir.eq.'0')cwddir=' '
C!    filenew=' '
C!    write (*,*) 'before pcfs'
C!    write (*,*) 'fileold="'//fileold(1:ilnb( fileold ))//'"'
C!    write (*,*) 'default="'//default(1:ilnb( default ))//'"'
C!    write (*,*) 'cwddir="'//cwddir(1:ilnb( cwddir ))//'"'
C!    read  (*,*)
C!    call pcfs ( fileold,default,filenew )
C!    write (*,*) 'fileold="'//fileold(1:ilnb( fileold ))//'"'
C!    write (*,*) 'default="'//default(1:ilnb( default ))//'"'
C!    write (*,*) 'cwddir="'//cwddir(1:ilnb( cwddir ))//'"'
C!    write (*,*) 'filenew="'//filenew(1:ilnb( filenew ))//'"'
C!    read  (*,*)
C!    write (*,*) 'enter fileold'
C!    read  (*,*,end=201) fileold
C!    if(fileold.eq.'0')fileold=' '
C!    write (*,*) 'enter default'
C!    read  (*,*) default
C!    if(default.eq.'0')default=' '
C!    write (*,*) 'enter cwddir'
C!    read  (*,*) cwddir
C!    if(cwddir.eq.'0')cwddir=' '
C!    iunit=0
C!    filenew=' '
C!    ex=.false.
C!    write (*,*) 'before txinq'
C!    write (*,'(a,i2)') ' iunit=',iunit
C!    write (*,*) 'fileold="'//fileold(1:ilnb( fileold ))//'"'
C!    write (*,*) 'default="'//default(1:ilnb( default ))//'"'
C!    write (*,*) 'cwddir="'//cwddir(1:ilnb( cwddir ))//'"'
C!    read  (*,*)
C!    call txinq ( iunit,fileold,default,filenew,ex )
C!    write (*,*) 'after txinq'
C!    write (*,'(a,i2)') ' iunit=',iunit
C!    write (*,*) 'fileold="'//fileold(1:ilnb( fileold ))//'"'
C!    write (*,*) 'default="'//default(1:ilnb( default ))//'"'
C!    write (*,*) 'cwddir="'//cwddir(1:ilnb( cwddir ))//'"'
C!    write (*,*) 'filenew="'//filenew(1:ilnb( filenew ))//'"'
C!    write (*,'(a,l5)') ' ex=',ex
C!    read  (*,*)
C!    go to 101
C!201 continue
C!    end
 
C
C-----THIS FILE CONTAINS MISCELLANEOUS SUBROUTINES AND
C-----FUNCTIONS THAT ARE USED BY THE TEXAS MODEL INPUT DATA PREPROCESSOR
C
C
C
      FUNCTION DEFFN(IUNIT)
C
C-----THIS FUNCTION MAKES A DEFAULT FILE NAME FOR SPECIFIC SYSTEMS
C     IUNIT - UNIT NUMBER
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*60 DEFFN
      INTEGER        IUNIT
C%    INCLUDE 'CWDDIR'
C%    INCLUDE 'USFILE'
      CHARACTER DEFNA*60,FMT*6
      INTEGER   NC
C%    INTEGER   ILNB
      DATA  FMT / '(A,I0)' /
      DEFNA=' '
C\    CALL INTFMT(NC,FMT(5:),IUNIT)
C\    IF(NC.EQ.0)GO TO 100
C\    WRITE(DEFNA,FMT,ERR=100)'TAPE',IUNIT
C|    WRITE(DEFNA,'(A,I3.3,A)',ERR=100)'for',IUNIT,'.dat'
C?    CALL INTFMT(NC,FMT(5:),IUNIT)
C?    IF(NC.EQ.0)GO TO 100
C?    WRITE(DEFNA,FMT,ERR=100)'fort.',IUNIT
      CALL INTFMT(NC,FMT(5:),IUNIT)                                     CCODE=C#
      IF(NC.EQ.0)GO TO 100                                              CCODE=C#
      WRITE(DEFNA,FMT,ERR=100)'fort',IUNIT                              CCODE=C#
C}    CALL INTFMT(NC,FMT(5:),IUNIT)
C}    IF(NC.EQ.0)GO TO 100
C}    WRITE(DEFNA,FMT,ERR=100)'fort',IUNIT
C%    CALL INTFMT(NC,FMT(5:),IUNIT)
C%    IF(NC.EQ.0)GO TO 100
C%    WRITE(DEFNA,FMT,ERR=100)'fort',IUNIT
  100 CONTINUE
      DEFFN=DEFNA
C%    IF(ILNB( CWDDIR ).EQ.0)THEN
C%      CALL PCFS(DEFNA,USFILE,DEFFN)
C%    ELSE
C%      CALL PCFS(DEFNA,CWDDIR,DEFFN)
C%    END IF
      RETURN
      END
C
C
C
      SUBROUTINE RDHLP(RDFILE,MAXNL,EDITKY,TEXT,NCTEXT,NLTEXT)
C
C-----RDFILE - CHARACTER VARIABLE WITH NAME OF FILE WITH TEXT TO BE READ
C-----IUNIT  - INTEGER, NUMBER OF UNIT TO BE OPENED TO READ <RDFILE>.
C-----MAXNL  - MAXIMUM NUMBER OF LINES TO BE READ FROM <RDFILE>,
C-----         EXCLUDING <EDITKY>.
C-----EDITKY - CHARACTER VARIABLE TO GET KEY TO USE IN EDIT REQUEST.
C-----         IF <NLTEXT> EQ 0, <EDITKY> IS NOT READ (AND MUST NOT BE
C-----         IN <RDFILE>).
C-----TEXT   - DIMENSIONED CHARACTER VARIABLE TO GET TEXT READ FROM
C-----         <RDFILE>.
C-----NCTEXT - DIMENSIONED INTEGER WITH NUMBER OF CHARACTERS IN EACH
C-----         LINE.
C-----NLTEXT - INTEGER, NUMBER OF LINES OF TEXT RETURNED IN <TEXT>.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)    RDFILE      ,EDITKY,TEXT(*)
      INTEGER                 MAXNL            ,NCTEXT(*),
     1                                                 NLTEXT
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      CHARACTER FMT1*25,TLINE*60
      LOGICAL  OPENED
      INTEGER  I,ILNB,IUNIT,NC
      DATA FMT1/'(//31X,A/3A,I1,A/A,I1,A/)'/
      IOFORM='(A)'
      IUNIT=NOUT(2)-1
      CALL TXINQ(0,RDFILE,TDFILE,TLINE,OPENED)
      NC=ILNB( TLINE )
      OPEN(IUNIT,FILE=TLINE(1:NC),STATUS='OLD'
C|   1                                       ,READONLY
     1                                       ,ACTION='READ'             CCODE=C{
C%   1                                       ,ACTION='READ'
     2                                                     )
      REWIND IUNIT
      IF(NLTEXT.NE.0)READ(IUNIT,IOFORM)EDITKY
      NLTEXT=1
      DO 100 I=1,MAXNL+1
   90 READ(IUNIT,IOFORM,END=9900)TEXT(NLTEXT)
      IF(ILNB( TEXT(NLTEXT) ).EQ.0)GO TO 90
      NCTEXT(NLTEXT)=ILNB( TEXT(NLTEXT) )
      NLTEXT=NLTEXT+1
  100 CONTINUE
      CALL NCFMT(FMT1(14:),MAXNL)
      FMT1(21:21)=FMT1(14:14)
      WRITE(*,FMT1)'***** WARNING *****','File "',TLINE(1:NC),
     1             '" contains more than ',MAXNL,' lines of text.',
     2             'Only the first ',MAXNL,' lines will be used.'
      NLTEXT=NLTEXT-2
      GO TO 9910
 9900 NLTEXT=NLTEXT-1
 9910 CLOSE(IUNIT)
      RETURN
      END
C
C
C
      SUBROUTINE PRTDEF
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER   NMAX
      PARAMETER(NMAX=20)
      INCLUDE 'CMPR01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMCH01'
      LOGICAL RDDEF
      INTEGER   NC
      DIMENSION NC(NMAX)
      SAVE RDDEF,NLDEF
      INTEGER   I,NLDEF
      DATA RDDEF,NLDEF/.FALSE.,0/
      IOFORM='(A)'
      IF(.NOT.RDDEF)THEN
        CALL RDHLP('gdvdf0',NMAX,' ',MLINE,NC,NLDEF)
        RDDEF=.TRUE.
      END IF
      WRITE(*,IOFORM)' ','definitions:'
      WRITE(*,IOFORM)(MLINE(I)(1:NC(I)),I=1,NLDEF)
      WRITE(*,IOFORM)' '
      RETURN
      END
C
C
C
      SUBROUTINE NCFMT(IFM,N)
C
C-----THIS SUBROUTINE DETERMINES THE NUMBER OF CHARACTER POSITIONS
C-----REQUIRED TO PRINT THE INTEGER <N>.  THE CHARACTER REPRESENTATION
C-----OF <N> IS PUT INTO THE FIRST POSITION OF <IFM>.  <N> MUST BE
C-----0 THRU 99.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)    IFM
      INTEGER              N
      IF(N.LT.10)THEN
        IFM(1:1)='1'
      ELSE
        IFM(1:1)='2'
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE INTFMT(NC,FMT,NUM)
C
C-----THIS SUBROUTINE DETERMINES THE NUMBER OF CHARACTER POSITIONS
C-----REQUIRED TO PRINT THE INTEGER <NUM>. <NC> IS SET TO THIS.
C-----THE CHARACTER REPRESENTATION OF <NC> IS PUT INTO THE FIRST
C-----POSITION OF <FMT>. ON AN ERROR, <NC> IS SET TO ZERO.
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           NC    ,NUM
      CHARACTER            FMT*(*)
      REAL    X
      IF(NUM.EQ.0)THEN
        NC=1
        GO TO 1000
      END IF
      X=IABS(NUM)
      NC=LOG10(X)+1
      IF(NUM.LT.0)NC=NC+1
      IF(NC.GT.9)THEN
        NC=0
        GO TO 9990
      END IF
 1000 WRITE(FMT(1:1),'(I1)')NC
 9990 RETURN
      END
C
C
C
      FUNCTION TOALP(MLINE,NFT,NFS,NF)
C
C-----THIS CHARACTER FUNCTION READS UP TO 9 INTEGER NUMBERS IN <NF>
C-----TEXT FIELDS OF <MLINE>, CONVERTS THESE TO LETTERS WITH 1 => A,
C-----2 => B, ETC. AND PUTS THEM INTO <TOALP>.  THE FIELDS ARE
C-----DESCRIBED BY FORMAT DESCRIPTIONS IN THE DIMENSIONED CHARACTER
C-----VARIABLE <NFT> WITH FIELD SIZES IN THE INTEGER SUBSCRIPTED
C-----VARIABLE <NFS>.  <NFT>, <NFS> AND <NF> ARE "CAPTURED" WHEN
C-----<NF> IS GREATER THAN 0.  THE CAPTURED DATA IS USED WHEN
C-----<NF> IS = 0.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*) TOALP
      CHARACTER*(*)  MLINE,NFT(*)
      INTEGER                  NFS(*),
     1                             NF
      INTEGER   IDAT   ,NFS1
      DIMENSION IDAT(9),NFS1(9)
      CHARACTER*46 FMT,MKFMT
      INTEGER   I,IT2,ITAM1,NC,NF1
      SAVE FMT,NF1,NC,NFS1
      ITAM1=ICHAR( 'A' )-1
      TOALP=' '
      IF(NF.GT.0)THEN
        FMT=MKFMT(NF,NFT,NC)
        NF1=NF
        DO 50 I=1,NF
   50   NFS1(I)=NFS(I)
        GO TO 9990
      END IF
      READ(MLINE,FMT(1:NC))(IDAT(I),I=1,NF1)
      IT2=0
      DO 100 I=1,NF1
      IF(IDAT(I).EQ.0)GO TO 100
      TOALP(IT2:IT2)=CHAR( IDAT(I)+ITAM1 )
  100 CONTINUE
 9990 RETURN
      END
C
C
C
      FUNCTION FINDFI(PROMPT,DIFILE,FFILE,NIO)
C
C-----THIS LOGICAL FUNCTION PROMPTS FOR A FILE NAME, CHECKS IF
C-----THE FILE IS IN THE DEFAULT DATA AREA OR A SPECIFIED USER
C-----DATA STORAGE AREA.  IF THE FILE IS FOUND, <FINDFI> IS SET
C-----TO <.TRUE.>, OTHERWISE <.FALSE.>.
C
C     PROMPT - CHARACTER VARIABLE WITH TEXT TO PROMPT FOR FILE NAME.
C     DIFILE - CHARACTER VARIABLE WITH DEFAULT FILE NAME TO USE WHEN
C              SEARCHING FOR THE FILE.
C     FFILE  - NAME OF FILE THAT WAS LOCATED.
C     NIO    - UNIT NUMBER OF THE OPEN TERMINAL FILE.
C              IF <NIO> IS NEGATIVE, SKIP THE PROMPT AND ASSUME THE FILE
C              NAME IS IN <PROMPT>.
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  FINDFI
      CHARACTER*(*)   PROMPT,DIFILE,FFILE
      INTEGER                             NIO
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'USFILE'
      CHARACTER FMT1*4
      LOGICAL   EXI,EXI1
      INTEGER   ILNB,NC,NC1,NC2,NCDI
C|    INTEGER   NC3
      DATA FMT1/'(3A)'/
      IOFORM='(A)'
      IF(NIO.LT.0)THEN
        NLINE=PROMPT
        NIO=-NIO
        GO TO 1000
      END IF
      WRITE(*,IOFORM)PROMPT
      READ(*,IOFORM)NLINE
 1000 CONTINUE
      IF(NLINE.EQ.' ')NLINE=DFILE
      NCDI=0
C|    NCDI=INDEX(DIFILE,';')-1
      IF(NCDI.LE.0)NCDI=MAX0(ILNB( DIFILE ),1)
      CALL TXINQ(0,NLINE,DIFILE(1:NCDI),FFILE,EXI)
      DIFILE=FFILE
      IF(EXI)GO TO 9900
      NC1=1
C
C-----MASK OUT THE UIC SO THAT THE UIC FROM USFILE WILL BE USED
C
C|    NC1=INDEX(FFILE,']')+1
      NC2=MAX0(ILNB( FFILE ),NC1)
C
C-----MASK OUT THE VERSION NUMBER
C
C|    NCV=INDEX(FFILE,';')
C|    IF(NCV.GE.NC1)NC2=NCV
      CALL TXINQ(0,DIFILE(NC1:NC2),USFILE,FFILE,EXI1)
      IF(EXI1)GO TO 9900
      NC=ILNB( DIFILE )
      IF(DIFILE.EQ.FFILE)THEN
        WRITE(*,FMT1)'File "',DIFILE(1:NC),'" not found.'
      ELSE
        NC1=ILNB( FFILE )
        WRITE(*,FMT1)'Files not found: "',DIFILE(1:NC),'"'
        WRITE(*,FMT1)'                 "',FFILE(1:NC1),'"'
C       WRITE(*,FMT1)'File "',FFILE(1:NC1),'" not found in the'
C
C-----MASK FILE NAME,TYPE & EXTENSION FOR PRINTING
C
C|      NC2=ILNB( USFILE )
C|      NC3=INDEX(USFILE(1:NC2),']')
C|      IF(NC3.GT.0)THEN
C|        NC2=NC3
C|      ELSE
C|        NC3=INDEX(USFILE(1:NC2),':')
C|        IF(NC3.GT.0)NC2=NC3
C|      END IF
C     WRITE(*,FMT1)'Specified user data storage area ("',
C    1             USFILE(1:NC2),'").'
      END IF
      FINDFI=.FALSE.
      GO TO 9990
 9900 CONTINUE
      FINDFI=.TRUE.
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE TXINQ(IUNIT,FI,DEF,NA,EX)
C
C-----THIS SUBROUTINE EXECUTES AN INQUIRE TO SEE IF THE FILE
C-----IS (OR CAN BE) DIRECTLY ACCESSABLE BY FORTRAN.
C
C     IUNIT - IF NE 0, SEE IF A FILE IS OPEN ON UNIT <IUNIT>,
C                    IF SO, SET <NA> TO FILE NAME, <EX> TO .TRUE.
C             IF EQ 0, INQUIRE TO SEE IF FILE <FI> EXISTS.  THE METHOD
C                    OF CHECKING DEPENDS ON THE COMPUTER SYSTEM AND
C                    THE INFORMATION IN <DEF>.
C     FI    - CHARACTER VARIABLE FOR FILE NAME.
C     DEF   - CHARACTER VARIABLE WITH ADDITIONAL INFORMATION TO BE USED
C             TO LOCATE THE FILE.
C     NA    - CHARACTER VARIABLE TO RETURN NAME OF THE FILE.
C     EX    - LOGICAL VARIABLE TO RETURN .TRUE. IF FILE EXISTS,
C             OTHERWISE .FALSE.
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER          IUNIT
      CHARACTER*(*)          FI,DEF,NA
      LOGICAL                          EX
      INCLUDE 'CMTX01'                                                  CCODE=C#
C?    CHARACTER NA1*40
      CHARACTER NA1*120,VOL*25,DR*1,DI*120,NAME*120,EXT*3,DRU*1         CCODE=C#
C}    CHARACTER NA1*40
C%    CHARACTER NA1*80
C\    CHARACTER FIT*7,DEFU*60
      INTEGER   ILNB,IT1,IT2,IT3
C-----TWR the following 3 lines were added to remove compiler warnings
      IT1=ILNB( FI )
      IT2=IT1
      IT3=IT2
C!    write (*,*) 'in txinq'
C!    write (*,'(a,i2)') ' iunit=',iunit
C!    write (*,*) 'fi="'//fi(1:ilnb( fi ))//'"'
C!    write (*,*) 'def="'//def(1:ilnb( def ))//'"'
C!    read  (*,*)
      IF(IUNIT.NE.0)THEN
C!      write (*,*) 'in txinq iunit!=0'
C!      read  (*,*)
        INQUIRE(IUNIT,NAME=NA,EXIST=EX)
C!      write (*,*) 'in txinq after inquire'
C!      write (*,*) 'na="'//na(1:ilnb( na ))//'"'
C!      write (*,'(a,l5)') ' ex=',ex
C!      read  (*,*)
C!      write (*,*) 'in txinq before pcfs'
C!      write (*,*) 'na="'//na(1:ilnb( na ))//'"'
C!      read  (*,*)
C?      CALL PCFS(NA,' ',NA)
        CALL PCFS(NA,' ',NA)                                            CCODE=C#
C}      CALL PCFS(NA,' ',NA)
C%      CALL PCFS(NA,' ',NA)
C!      write (*,*) 'in txinq after pcfs'
C!      write (*,*) 'na="'//na(1:ilnb( na ))//'"'
C!      read  (*,*)
      ELSE
C!      write (*,*) 'in txinq iunit=0'
C!      read  (*,*)
C|      NCVN=INDEX(DEF,';')-1
C|      IF(NCVN.LE.0)NCVN=MAX0(ILNB( DEF ),1)
C|      INQUIRE(FILE=FI,DEFAULTFILE=DEF(1:NCVN),NAME=NA,EXIST=EX)
C?      CALL PCFS(FI,DEF,NA1)
C?      INQUIRE(FILE=NA1,NAME=NA,EXIST=EX)
C?      IF(.NOT.EX)NA=NA1
C}      CALL PCFS(FI,DEF,NA1)
C}      INQUIRE(FILE=NA1,NAME=NA,EXIST=EX)
C}      IF(.NOT.EX)NA=NA1
C
C-----<DEF> IS USED TO IDENTIFY A PERMANENT FILE SET TO SEARCH.
C-----"PFIIIIJJJJ" WHERE "PF" IS REQUIRED, IIII IS THE
C-----PERMANENT FILE ID, JJJJ IS THE PASSWORD.
C
C\      DEFU=DEF
C\      CALL  TOUPR   ( DEFU )
C\      IF((FI.EQ.' ') .AND. (DEFU(1:2).NE.'PF'))FI=DEF
C\      INQUIRE(FILE=FI,EXIST=EX)
C\      NA=FI
C\      IF(EX)GO TO 9990
C\      IF(DEFU(1:2).NE.'PF')GO TO 9990
C\      READ(DEF(3:6),'(A4)')II
C\      NCFN=ILNB( FI )
C\      FIT=FI(1:NCFN)
C\      READ(FIT,'(A7)')IFN
C\      CALL READPF(II,IFN,IRET)
C\      IF(IRET.EQ.0)THEN
C\        EX=.TRUE.
C\      ELSE
C\        EX=.FALSE.
C\      END IF
C
C-----IF THE FILE NAME CONTAINS "::", THE CHARACTERS BEFORE "::" ARE
C-----USED AS A VOLUME LABEL TO PROMPT THE USER TO INSERT A DISK
C-----INTO A DRIVE.  THIS VOLUME LABEL IS MASKED FROM THE TEXT
C-----RETURNED IN <NA>.
C
        CALL PCFS(FI,DEF,NA1)                                           CCODE=C#
        IT1=INDEX(NA1,'::')                                             CCODE=C#
        IF(IT1.EQ.0)THEN                                                CCODE=C#
          IT1=1                                                         CCODE=C#
        ELSE                                                            CCODE=C#
          IT1=IT1+2                                                     CCODE=C#
        END IF                                                          CCODE=C#
        NA=NA1(IT1:)                                                    CCODE=C#
        IT1=INDEX(NA,':')                                               CCODE=C#
        IF(IT1.EQ.0)THEN                                                CCODE=C#
          IT1=1                                                         CCODE=C#
        ELSE                                                            CCODE=C#
          IT1=IT1+1                                                     CCODE=C#
        END IF                                                          CCODE=C#
        IT2=ILNB( NA )                                                  CCODE=C#
        CALL PCFD(NA1,VOL,DR,DI,NAME,EXT)                               CCODE=C#
        NA1=NA                                                          CCODE=C#
        IF(VOL.EQ.' ')THEN                                              CCODE=C#
          DRU=DR                                                        CCODE=C#
          CALL  TOUPR   ( DRU )                                         CCODE=C#
          IF(DRU.EQ.' ')GO TO 900                                       CCODE=C#
          IF((DRU.EQ.'A') .OR. (DRU.EQ.'B'))THEN                        CCODE=C#
            WRITE(*,'(5A)')'Be sure that disk for file "',NA(IT1:IT2),  CCODE=C#
     1                     '" is in drive ',DR,':.'                     CCODE=C#
          ELSE                                                          CCODE=C#
            GO TO 900                                                   CCODE=C#
          END IF                                                        CCODE=C#
        ELSE                                                            CCODE=C#
          IT3=ILNB( VOL )                                               CCODE=C#
          IF(DR.EQ.' ')THEN                                             CCODE=C#
            WRITE(*,'(5A)')'Be sure that disk labeled "',VOL(1:IT3),    CCODE=C#
     1               '" for file "',NA(IT1:IT2),'" is in default drive.'CCODE=C#
          ELSE                                                          CCODE=C#
            WRITE(*,'(7A)')'Be sure that disk labeled "',VOL(1:IT3),    CCODE=C#
     1               '" for file "',NA(IT1:IT2),'" is in drive ',DR,':.'CCODE=C#
          END IF                                                        CCODE=C#
        END IF                                                          CCODE=C#
        WRITE(*,'(A)')'Press <enter> when ready.'                       CCODE=C#
        READ(*,'(A)')                                                   CCODE=C#
  900   CONTINUE                                                        CCODE=C#
        INQUIRE(FILE=NA1,NAME=NA,EXIST=EX)                              CCODE=C#
C!      write (*,*) 'in txinq before pcfs'
C!      write (*,*) 'fi="'//fi(1:ilnb( fi ))//'"'
C!      write (*,*) 'def="'//def(1:ilnb( def ))//'"'
C!      read  (*,*)
C%      CALL PCFS(FI,DEF,NA1)
C!      write (*,*) 'in txinq after pcfs'
C!      write (*,*) 'fi="'//fi(1:ilnb( fi ))//'"'
C!      write (*,*) 'def="'//def(1:ilnb( def ))//'"'
C!      write (*,*) 'na1="'//na1(1:ilnb( na1 ))//'"'
C!      read  (*,*)
C%      INQUIRE(FILE=NA1,NAME=NA,EXIST=EX)
C%      IF(.NOT.EX)NA=NA1
C!      write (*,*) 'in txinq after inquire'
C!      write (*,*) 'na="'//na(1:ilnb( na ))//'"'
C!      write (*,'(a,l5)') ' ex=',ex
C!      read  (*,*)
      END IF
C|    NC=MAX0(1,ILNB( NA ))
C|    IF((NA(NC:NC).EQ.';') .AND. (NC.LT.LEN( NA )))THEN
C|      NC=NC+1
C|      NA(NC:NC)='1'
C|    END IF
 9990 CONTINUE
C|    CALL TOUPR(NA)
C!    write (*,*) 'in txinq return'
C!    write (*,*) 'na="'//na(1:ilnb( na ))//'"'
C!    write (*,'(a,l5)') ' ex=',ex
C!    read  (*,*)
      RETURN
      END
C
C
C
      SUBROUTINE ADEDIR(DR,DIR)
C
C-----IF NEEDED, INCLUDE DEFAULT DIRECTORY NAME IN A DIRECTORY NAME
C
C  DR - DRIVE FOR WHICH DEFAULT DIRECTORY IS TO BE FOUND
C  DIR - DIRECTORY NAME
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'CWDDIR'
      CHARACTER*(*)     DR,DIR
      CHARACTER*1 SDSEP
C?    PARAMETER  (SDSEP='/')
      PARAMETER  (SDSEP='\')                                            CCODE=C#
C}    PARAMETER  (SDSEP='/')
C%    PARAMETER  (SDSEP=':')
      CHARACTER*60 TEMP
      INTEGER   ILEN,ILNB,NC,NC1,NCSDSEP
C%    INTEGER   SDSEPNC1,SDSEPNC2
C
C----- IS DIRECTORY SPECIFIED FROM ROOT LEVEL ?
C
C?    IF(DIR(1:1).EQ.SDSEP)RETURN
      IF(DIR(1:1).EQ.SDSEP)RETURN                                       CCODE=C#
C}    IF(DIR(1:1).EQ.SDSEP)RETURN
C%    IF(DIR(1:1).NE.SDSEP.AND.DIR(1:1).NE.' ')RETURN
      TEMP=DIR
      ILEN=LEN( DIR )
      DIR = ' '
C?    CALL  GETENV  ( "PWD",DIR )
C@    CALL  DEFDIR  (    DR,DIR )
      CALL  GETCWD  (       DIR )                                       CCODE=C{
C!    write (*,*) 'in adedir before getcwd'
C!    read  (*,*)
C%    CALL  GETCWD  (       DIR )
C!    write (*,*) 'in adedir after getcwd'
C!    write (*,*) 'dir="'//dir(1:ilnb( dir ))//'"'
C!    read  (*,*)
C!    write (*,'(a,i2)') ' cwddir len=',ilnb( cwddir )
C!    read  (*,*)
      IF(ILNB( CWDDIR ).GT.0)DIR=CWDDIR
C!    write (*,*) 'dir="'//dir(1:ilnb( dir ))//'"'
C!    read  (*,*)
      NC=ILNB( DIR )
C
C-----DEFAULT DIRECTORY IS BEYOND THE ROOT LEVEL
C-----APPEND A TRAILING SUB-DIRECORY SEPARATOR
C
C?    IF(NC.GT.1)THEN
C?      IF(DIR(NC:NC).NE.SDSEP)THEN
C?        NC=NC+1
C?        IF(NC.GT.ILEN)RETURN
C?        DIR(NC:NC)=SDSEP
C?      ENDIF
C?    END IF
      IF(NC.GT.1)THEN                                                   CCODE=C#
        IF(DIR(NC:NC).NE.SDSEP)THEN                                     CCODE=C#
          NC=NC+1                                                       CCODE=C#
          IF(NC.GT.ILEN)RETURN                                          CCODE=C#
          DIR(NC:NC)=SDSEP                                              CCODE=C#
        END IF                                                          CCODE=C#
      END IF                                                            CCODE=C#
C}    IF(NC.GT.1)THEN
C}      IF(DIR(NC:NC).NE.SDSEP)THEN
C}        NC=NC+1
C}        IF(NC.GT.ILEN)RETURN
C}        DIR(NC:NC)=SDSEP
C}      ENDIF
C}    END IF
      NC=NC+1
      IF(NC.GT.ILEN)RETURN
      NC1=1
C?    IF(TEMP(1:1).EQ.'.')THEN
      IF(TEMP(1:1).EQ.'.')THEN                                          CCODE=C#
C}    IF(TEMP(1:1).EQ.'.')THEN
C?      NCSDSEP=2
        NCSDSEP=2                                                       CCODE=C#
C}      NCSDSEP=2
C%      NCSDSEP=1
C!      write (*,*) 'temp(ncsdsep:ncsdsep)="'//temp(ncsdsep:ncsdsep)//
C!   *              '"'
C!      read  (*,*)
        IF(TEMP(NCSDSEP:NCSDSEP).EQ.SDSEP)NC1=NCSDSEP+1
C?    END IF
      END IF                                                            CCODE=C#
C}    END IF
      DIR(NC:)=TEMP(NC1:)
C!    write (*,*) 'in adedir return dir="'//dir(1:ilnb( dir ))//'"'
C!    read  (*,*)
      RETURN
      END
C
C
C
      SUBROUTINE PCFD(NLINE,VOL,DR,DI,NA,EXT)
C
C-----THIS SUBROUTINE CHECKS A TEXT LINE TO GET THE VOLUME LABEL,
C-----DRIVE DESIGNATOR, DIRECTORY, FILE NAME AND EXTENSION.
C
C     NLINE - CHARACTER VARIABLE WITH TEXT LINE IN THE FORM
C             "VOLUME::DRIVE:\DIRECTORY\NAME.EXT"
C     VOL   - CHARACTER VARIABLE TO RETURN THE VOLUME LABEL
C     DR    - CHARACTER VARIABLE TO RETURN THE DRIVE DESIGNATOR LETTER
C     DI    - CHARACTER VARIABLE TO RETURN THE DIRECTORY
C     NA    - CHARACTER VARIABLE TO RETURN THE FILE NAME
C     EXT   - CHARACTER VARIABLE TO RETURN THE EXTENSION
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)   NLINE,VOL,DR,DI,NA,EXT
      CHARACTER*1 BLANK
      PARAMETER  (BLANK=' ')
      CHARACTER*1 COL              ,DOT                                 CCODE=C#
      CHARACTER*2         DCOL                                          CCODE=C#
      PARAMETER  (COL=':',DCOL='::',DOT='.')                            CCODE=C#
      CHARACTER*1 SDSEP
C?    PARAMETER  (SDSEP='/')
      PARAMETER  (SDSEP='\')                                            CCODE=C#
C}    PARAMETER  (SDSEP='/')
C%    PARAMETER  (SDSEP=':')
      INTEGER     I,ILNB,IT1,ITD1,ITD2,ITDI1,ITDI2,ITE1,ITLNB,ITN1,ITN2,
     1            ITV2
  999 FORMAT('Error in PCFD: length of ',A,
     1       ' in file specification = "',A,'" is greater than ',I3)
C-----TWR the following line was added to remove compiler warning
      ITE1=0
C
C-----LAST NONBLANK CHARACTER
C
      ITLNB=ILNB( NLINE )
      IF(ITLNB.EQ.0)GO TO  900
C
C-----GET VOLUME LABEL
C
      VOL=BLANK
C?    ITV2=-2
      IT1=INDEX(NLINE,DCOL)                                             CCODE=C#
      IF(IT1.GT.0)THEN                                                  CCODE=C#
        ITV2=IT1-1                                                      CCODE=C#
        IF(ITV2.GT.0)THEN                                               CCODE=C#
          IF(ITV2.GT.LEN( VOL ))GO TO 961                               CCODE=C#
          VOL=NLINE(1:ITV2)                                             CCODE=C#
        ELSE                                                            CCODE=C#
          IF(2.GT.LEN( VOL ))GO TO 961                                  CCODE=C#
          VOL=DCOL                                                      CCODE=C#
        END IF                                                          CCODE=C#
      ELSE                                                              CCODE=C#
        ITV2=-2                                                         CCODE=C#
      END IF                                                            CCODE=C#
  100 CONTINUE                                                          CCODE=C#
C}    ITV2=-2
C%    ITV2=-2
C
C-----GET DRIVE DESIGNATOR
C
      DR=BLANK
      ITD1=ITV2+3
C?    ITD2=ITD1-2
      IF(ITD1.GT.ITLNB)GO TO  910                                       CCODE=C#
      IT1=INDEX(NLINE(ITD1:),COL)                                       CCODE=C#
      IF(IT1.GT.0)THEN                                                  CCODE=C#
        ITD2=ITD1+IT1-2                                                 CCODE=C#
        IF(ITD2.GE.ITD1)THEN                                            CCODE=C#
          IF(ITD2-ITD1+1.GT.LEN( DR ))GO TO 962                         CCODE=C#
          DR=NLINE(ITD1:ITD2)                                           CCODE=C#
        ELSE                                                            CCODE=C#
          IF(1.GT.LEN( DR ))GO TO 962                                   CCODE=C#
          DR=COL                                                        CCODE=C#
        END IF                                                          CCODE=C#
      ELSE                                                              CCODE=C#
        ITD2=ITD1-2                                                     CCODE=C#
      END IF                                                            CCODE=C#
C}    ITD2=ITD1-2
C%    ITD2=ITD1-2
  200 CONTINUE                                                          CCODE=C#
C
C-----LOOK FOR DIRECTORY
C
      DI=BLANK
      ITDI1=ITD2+2
      IF(ITDI1.GT.ITLNB)GO TO 920
C     WRITE(10,'(A,2I5)')'ITLNB,ITDI1: ',ITLNB,ITDI1
      DO 210 I=ITLNB,ITDI1,-1
      ITDI2=I
      IF(NLINE(I:I).EQ.SDSEP)GO TO 220
  210 CONTINUE
      ITDI2=ITDI1-1
      GO TO 250
  220 CONTINUE
      IF(ITDI2.GT.ITLNB)ITDI2=ITLNB
C     WRITE(10,'(A,2I5)')'ITDI1,ITDI2: ',ITDI1,ITDI2
      IF(ITDI2-ITDI1+1.GT.LEN( DI ))GO TO 963
      DI=NLINE(ITDI1:ITDI2)
      IF(ITDI2.EQ.ITLNB)GO TO 930
  250 CONTINUE
C
C-----GET FILE NAME
C
      NA=BLANK
      ITN1=ITDI2+1
      IF(ITN1.GT.ITLNB)GO TO  930
C?    IT1=0
      IT1=INDEX(NLINE(ITN1:),DOT)                                       CCODE=C#
C}    IT1=0
C%    IT1=0
      IF(IT1.GT.0)THEN
        ITN2=ITN1+IT1-2
      ELSE
        ITN2=ITLNB
      END IF
      IF(ITN1.LE.ITN2)THEN
        IF(ITN2-ITN1+1.GT.LEN( NA ))GO TO 964
        NA=NLINE(ITN1:ITN2)
      END IF
      IF(ITN2.EQ.ITLNB)GO TO 940
      IF(IT1.EQ.0)GO TO 940
C
C-----GET EXTENSION
C
      EXT=BLANK
      ITE1=ITN2+2                                                       CCODE=C#
      IF(ITLNB.GE.ITE1)THEN                                             CCODE=C#
        IF(ITLNB-ITE1+1.GT.LEN( EXT ))GO TO 965                         CCODE=C#
        EXT=NLINE(ITE1:ITLNB)                                           CCODE=C#
      ELSE                                                              CCODE=C#
        IF(1.GT.LEN( EXT ))GO TO 965                                    CCODE=C#
        EXT=DOT                                                         CCODE=C#
      END IF                                                            CCODE=C#
      GO TO  950                                                        CCODE=C#
  900 CONTINUE
      VOL=BLANK
  910 CONTINUE
      DR=BLANK
  920 CONTINUE
      DI=BLANK
  930 CONTINUE
      NA=BLANK
  940 CONTINUE
      EXT=BLANK
  950 CONTINUE
C     WRITE(10,'(3A)')'PCFD, NLINE: "',NLINE,'"'
C     WRITE(10,'(3A)')'     VOLUME: "',VOL,'"'
C     WRITE(10,'(3A)')'      DRIVE: "',DR,'"'
C     WRITE(10,'(3A)')'  DIRECTORY: "',DI,'"'
C     WRITE(10,'(3A)')'       NAME: "',NA,'"'
C     WRITE(10,'(3A)')'  EXTENSION: "',EXT,'"'
      RETURN
  961 CONTINUE                                                          CCODE=C#
      WRITE(*,999)'volume'   ,NLINE,LEN( VOL )                          CCODE=C#
      STOP 961                                                          CCODE=C#
  962 CONTINUE                                                          CCODE=C#
      WRITE(*,999)'drive'    ,NLINE,LEN( DR  )                          CCODE=C#
      STOP 962                                                          CCODE=C#
C%962 CONTINUE
C%    WRITE(*,999)'drive'    ,NLINE,LEN( DR  )
C%    STOP 962
  963 CONTINUE
      WRITE(*,999)'directory',NLINE,LEN( DI  )
      STOP 963
  964 CONTINUE
      WRITE(*,999)'file name',NLINE,LEN( NA  )
      STOP 964
  965 CONTINUE                                                          CCODE=C#
      WRITE(*,999)'extension',NLINE,LEN( EXT )                          CCODE=C#
      STOP 965                                                          CCODE=C#
      END
C
C
C
      SUBROUTINE PCFS(FI,DEF,FO)
C
C-----THIS SUBROUTINE BUILDS A COMPOSITE FILE SPECIFICATION BY
C-----SUPPLYING ITEMS THAT ARE NOT IN <FI> WITH ITEMS FROM <DEF>.
C-----DEFAULT DRIVE AND DIRECTORY ARE SUPPLIED AS NEEDED.
C-----SEE SUBROUTINE "PCFD" FOR A DESCRIPTION OF EACH ITEM.
C
C     FI  - CHARACTER VARIABLE WITH ORIGINAL FILE SPECIFICATION.
C     DEF - CHARACTER VARIABLE WITH DEFAULT
C     FO  - CHARACTER VARIABLE TO RETURN COMPOSITE FILE SPECIFICATION.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER *(*)  FI,DEF,FO
      CHARACTER*1 SDSEP
C?    PARAMETER  (SDSEP='/')
      PARAMETER  (SDSEP='\')                                            CCODE=C#
C}    PARAMETER  (SDSEP='/')
C%    PARAMETER  (SDSEP=':')
      CHARACTER*1 BLANK
      PARAMETER  (BLANK=' ')
      CHARACTER*1 COL              ,DOT                                 CCODE=C#
      CHARACTER*2         DCOL                                          CCODE=C#
      PARAMETER  (COL=':',DCOL='::',DOT='.')                            CCODE=C#
      INCLUDE 'CWDDIR'
C?    CHARACTER VOL1*2 ,VOL2*2 ,DR1*1,DR2*1
      CHARACTER VOL1*25,VOL2*25,DR1*1,DR2*1                             CCODE=C#
C}    CHARACTER VOL1*2 ,VOL2*2 ,DR1*1,DR2*1
C%    CHARACTER VOL1*2 ,VOL2*2 ,DR1*1,DR2*1
      CHARACTER DI1*80,DI2*80,NA1*32,NA2*32,EXT1*3,EXT2*3,DR*1
C%    CHARACTER DI1U*80,DI2U*80,CWD*80,DIR*80
      INTEGER   IL1,IL2,ILNB,IT1,IT2,LENFO
C%    INTEGER   I,NCDIR
C-----TWR the following line was added to remove compiler warning
      IL2 = 0
      DR=BLANK                                                          CCODE=C#
      NA1=BLANK                                                         CCODE=C#
      NA2=BLANK                                                         CCODE=C#
C!    write (*,*) 'in pcfs before pcfd call for fi'
C!    write (*,*) 'fi="'//fi(1:ilnb( fi ))//'"'
C!    read  (*,*)
      CALL PCFD(FI ,VOL1,DR1,DI1,NA1,EXT1)
C!    write (*,*) 'in pcfs after pcfd call for fi'
C!    write (*,*) 'vol1="'//vol1(1:ilnb( vol1 ))//'"'
C!    write (*,*) 'dr1="'//dr1(1:ilnb( dr1 ))//'"'
C!    write (*,*) 'di1="'//di1(1:ilnb( di1 ))//'"'
C!    write (*,*) 'na1="'//na1(1:ilnb( na1 ))//'"'
C!    write (*,*) 'ext1="'//ext1(1:ilnb( ext1 ))//'"'
C!    read  (*,*)
C!    write (*,*) 'in pcfs before pcfd call for def'
C!    write (*,*) 'def="'//def(1:ilnb( def ))//'"'
C!    read  (*,*)
      CALL PCFD(DEF,VOL2,DR2,DI2,NA2,EXT2)
C!    write (*,*) 'in pcfs after pcfd call for def'
C!    write (*,*) 'vol2="'//vol2(1:ilnb( vol2 ))//'"'
C!    write (*,*) 'dr2="'//dr2(1:ilnb( dr2 ))//'"'
C!    write (*,*) 'di2="'//di2(1:ilnb( di2 ))//'"'
C!    write (*,*) 'na2="'//na2(1:ilnb( na2 ))//'"'
C!    write (*,*) 'ext2="'//ext2(1:ilnb( ext2 ))//'"'
C!    read  (*,*)
C
C----- IF NO DRIVE SPECIFIED, USE THE DEFAULT
C
C@    IF((DR1.EQ.BLANK) .AND. (DR2.EQ.BLANK))CALL DEFDR(DR1)
C~    IF((DR1.EQ.BLANK) .AND. (DR2.EQ.BLANK))CALL DEFDR(DR1)
      LENFO=LEN( FO )
      FO=BLANK
      IT2=0
      IT1=1
C
C-----DISK VOLUME LABEL (UP TO 25 CHARACTERS)
C
      IL1=ILNB( VOL1 )                                                  CCODE=C#
      IF(IL1.EQ.0)THEN                                                  CCODE=C#
C
C-----USE VOLUME LABEL FROM DEFAULT
C
        IF(VOL2(1:2).EQ.DCOL)THEN                                       CCODE=C#
C
C-----"::" ONLY, FORCES UNSPECIFIED VOLUME LABEL
C
          GO TO 100                                                     CCODE=C#
        END IF                                                          CCODE=C#
        IL1=ILNB( VOL2 )                                                CCODE=C#
        IF(IL1.EQ.0)GO TO 100                                           CCODE=C#
        IT2=IL1+2                                                       CCODE=C#
        IF(IT2.GT.LENFO)IT2=LENFO                                       CCODE=C#
        FO(1:IT2)=VOL2(1:IL1)//DCOL                                     CCODE=C#
        IF(IT2.EQ.LENFO)GO TO 550                                       CCODE=C#
      ELSE                                                              CCODE=C#
C
C-----USE VOLUME LABEL FROM ORIGINAL
C
        IF(VOL1(1:2).EQ.DCOL)THEN                                       CCODE=C#
C
C-----"::" ONLY, FORCES UNSPECIFIED VOLUME LABEL
C
          GO TO 100                                                     CCODE=C#
        END IF                                                          CCODE=C#
        IT2=IL1+2                                                       CCODE=C#
        IF(IT2.GT.LENFO)IT2=LENFO                                       CCODE=C#
        FO(1:IT2)=VOL1(1:IL1)//DCOL                                     CCODE=C#
        IF(IT2.EQ.LENFO)GO TO 550                                       CCODE=C#
      END IF                                                            CCODE=C#
      IT1=IT2+1                                                         CCODE=C#
      IF(IT1.GT.LENFO)GO TO 550                                         CCODE=C#
  100 CONTINUE                                                          CCODE=C#
C
C-----DRIVE DESIGNATOR LETTER (1 LETTER)
C
      IL1=ILNB( DR1 )                                                   CCODE=C#
      IF(IL1.EQ.0)THEN                                                  CCODE=C#
C
C-----USE DRIVE LETTER FROM DEFAULT
C
        IF(DR2(1:1).EQ.COL)THEN                                         CCODE=C#
C
C-----":" ONLY, FORCES UNSPECIFIED DRIVE LETTER
C
          GO TO 200                                                     CCODE=C#
        END IF                                                          CCODE=C#
        DR=DR2                                                          CCODE=C#
        IL1=ILNB( DR2 )                                                 CCODE=C#
        IF(IL1.EQ.0)GO TO 200                                           CCODE=C#
        IT2=IT1+IL1                                                     CCODE=C#
        IF(IT2.GT.LENFO)IT2=LENFO                                       CCODE=C#
        FO(IT1:IT2)=DR2(1:IL1)//COL                                     CCODE=C#
        IF(IT2.EQ.LENFO)GO TO 550                                       CCODE=C#
      ELSE                                                              CCODE=C#
C
C-----USE DRIVE LETTER FROM ORIGINAL
C
        IF(DR1(1:1).EQ.COL)THEN                                         CCODE=C#
C
C-----":" ONLY, FORCES UNSPECIFIED DRIVE LETTER
C
          GO TO 200                                                     CCODE=C#
        END IF                                                          CCODE=C#
        DR=DR1                                                          CCODE=C#
        IT2=IT1+IL1                                                     CCODE=C#
        IF(IT2.GT.LENFO)IT2=LENFO                                       CCODE=C#
        FO(IT1:IT2)=DR1(1:IL1)//COL                                     CCODE=C#
        DR=DR1                                                          CCODE=C#
        IF(IT2.EQ.LENFO)GO TO 550                                       CCODE=C#
      END IF                                                            CCODE=C#
      IT1=IT2+1                                                         CCODE=C#
      IF(IT1.GT.LENFO)GO TO 550                                         CCODE=C#
  200 CONTINUE                                                          CCODE=C#
C
C-----DIRECTORY
C
      IL1=ILNB( DI1 )
      IF(IL1.EQ.0)THEN
C
C-----USE DIRECTORY FROM DEFAULT FILE SPEC
C
C!      write (*,*) 'in pcfs di1 blank'
C!      read  (*,*)
C%      IF(DI2(1:2).EQ.'::')THEN
C!        write (*,*) 'in pcfs di2(1:2)="::"'
C!        write (*,*) 'in pcfs before getcwd'
C!        read  (*,*)
C%        CALL GETCWD ( CWD )
C!        write (*,*) 'in pcfs after getcwd'
C!        write (*,*) 'cwd="'//cwd(1:ilnb( cwd ))//'"'
C!        read  (*,*)
C%        DI2U=DI2
C%        CALL  TOUPR   ( DI2U )
C%        IF(DI2U(1:10).EQ.'::SYS_DAT:')THEN
C%          DIR=CWD
C%        ELSE
C!          write (*,'(a,i2)') ' cwddir len=',ilnb( cwddir )
C!          read  (*,*)
C%          IF(ILNB( CWDDIR ).EQ.0)THEN
C%            DIR=CWD
C%          ELSE
C%            DIR=CWDDIR
C%          END IF
C%        END IF
C!        write (*,*) 'dir="'//dir(1:ilnb( dir ))//'"'
C!        read  (*,*)
C%        NCDIR=ILNB( DIR )
C%        DO 210 I = NCDIR-1 , 1 , -1
C%        IF ( DIR(I:I) . EQ . SDSEP ) GO TO 220
C%210     CONTINUE
C%        I=NCDIR
C%220     CONTINUE
C%        FO(IT1:)=DIR(1:I)//DI2(3:)
C!        write (*,*) 'fo="'//fo(1:ilnb( fo ))//'"'
C!        read  (*,*)
C%        IT1=ILNB( FO )+1
C%        IF(IT1.GT.LENFO)GO TO 550
C%        GO TO 300
C%      END IF
        IF((DI2(1:1).EQ.SDSEP) .AND. (DI2(2:2).EQ.SDSEP))THEN
C
C-----"\\" OR "//" AT BEGINNING, FORCES UNSPECIFIED DIRECTORY
C
          GO TO 300
        END IF
C!      write (*,*) 'in pcfs before adedir di2'
C!      write (*,*) 'di2="'//di2(1:ilnb( di2 ))//'"'
C!      read  (*,*)
        CALL ADEDIR(DR,DI2)
C!      write (*,*) 'in pcfs after adedir'
C!      write (*,*) 'di2="'//di2(1:ilnb( di2 ))//'"'
C!      read  (*,*)
        IL1=ILNB( DI2 )
        IF(IL1.EQ.0)GO TO 300
        IT2=IT1+IL1-1
        IF(IT2.GT.LENFO)IT2=LENFO
        FO(IT1:IT2)=DI2(1:IL1)
C!      write (*,*) 'fo="'//fo(1:ilnb( fo ))//'"'
C!      read  (*,*)
        IF(IT2.EQ.LENFO)GO TO 550
      ELSE
C
C-----USE DIRECTORY FROM ORIGINAL FILE SPEC
C
C!      write (*,*) 'in pcfs di1 not blank'
C!      read  (*,*)
C%      IF(DI1(1:2).EQ.'::')THEN
C!        write (*,*) 'in pcfs di1(1:2)="::"'
C!        write (*,*) 'in pcfs before getcwd'
C!        read  (*,*)
C%        CALL GETCWD ( CWD )
C!        write (*,*) 'in pcfs after getcwd'
C!        write (*,*) 'cwd="'//cwd(1:ilnb( cwd ))//'"'
C!        read  (*,*)
C%        DI1U=DI1
C%        CALL  TOUPR   ( DI1U )
C%        IF(DI1U(1:10).EQ.'::SYS_DAT:')THEN
C%          DIR=CWD
C%        ELSE
C!          write (*,'(a,i2)') ' cwddir len=',ilnb( cwddir )
C!          read  (*,*)
C%          IF(ILNB( CWDDIR ).EQ.0)THEN
C%            DIR=CWD
C%          ELSE
C%            DIR=CWDDIR
C%          END IF
C%        END IF
C!        write (*,*) 'dir="'//dir(1:ilnb( dir ))//'"'
C!        read  (*,*)
C%        NCDIR=ILNB( DIR )
C%        DO 230 I = NCDIR-1 , 1 , -1
C%        IF ( DIR(I:I) . EQ . SDSEP ) GO TO 240
C%230     CONTINUE
C%        I=NCDIR
C%240     CONTINUE
C%        FO(IT1:)=DIR(1:I)//DI1(3:)
C!        write (*,*) 'fo="'//fo(1:ilnb( fo ))//'"'
C!        read  (*,*)
C%        IT1=ILNB( FO )+1
C%        IF(IT1.GT.LENFO)GO TO 550
C%        GO TO 300
C%      END IF
        IF((DI1(1:1).EQ.SDSEP) .AND. (DI1(2:2).EQ.SDSEP))THEN
C
C-----"\\" OR "//" AT BEGINNING, FORCES UNSPECIFIED DIRECTORY
C
          GO TO 300
        END IF
C!      write (*,*) 'in pcfs before adedir di1'
C!      write (*,*) 'di1="'//di1(1:ilnb( di1 ))//'"'
C!      read  (*,*)
        CALL ADEDIR(DR,DI1)
C!      write (*,*) 'after adedir'
C!      write (*,*) 'di1="'//di1(1:ilnb( di1 ))//'"'
C!      read  (*,*)
        IL1=ILNB( DI1 )
        IT2=IT1+IL1-1
        IF(IT2.GT.LENFO)IT2=LENFO
        FO(IT1:IT2)=DI1(1:IL1)
C!      write (*,*) 'fo="'//fo(1:ilnb( fo ))//'"'
C!      read  (*,*)
        IF(IT2.EQ.LENFO)GO TO 550
      END IF
      IT1=IT2+1
      IF(IT1.GT.LENFO)GO TO 550
  300 CONTINUE
C
C-----FILE NAME (UP TO 32 CHARACTERS)
C
      IL1=ILNB( NA1 )
      IF(IL1.EQ.0)THEN
C
C-----USE NAME FROM DEFAULT
C
C!      write (*,*) 'in pcfs na1 blank'
C!      read  (*,*)
        IL1=ILNB( NA2 )
        IF(IL1.EQ.0)GO TO 400
        IT2=IT1+IL1-1
        IF(IT2.GT.LENFO)IT2=LENFO
        FO(IT1:IT2)=NA2(1:IL1)
C!      write (*,*) 'fo="'//fo(1:ilnb( fo ))//'"'
C!      read  (*,*)
        IF(IT2.EQ.LENFO)GO TO 550
      ELSE
C
C-----USE NAME FROM ORIGINAL
C
C!      write (*,*) 'in pcfs na1 not blank'
C!      read  (*,*)
        IT2=IT1+IL1-1
        IF(IT2.GT.LENFO)IT2=LENFO
        FO(IT1:IT2)=NA1(1:IL1)
C!      write (*,*) 'fo="'//fo(1:ilnb( fo ))//'"'
C!      read  (*,*)
        IF(IT2.EQ.LENFO)GO TO 550
      END IF
      IT1=IT2+1
      IF(IT1.GT.LENFO)GO TO 550
  400 CONTINUE
C
C-----FILE EXTENSION (UP TO 3 CHARACTERS)
C
      IL2=ILNB( EXT1 )                                                  CCODE=C#
      IF(IL2.EQ.0)THEN                                                  CCODE=C#
C
C-----USE EXTENSION FROM DEFAULT
C
        IF(EXT2(1:1).EQ.DOT)THEN                                        CCODE=C#
C
C-----'.' ONLY, FORCES BLANK EXTENSION
C
          GO TO 500                                                     CCODE=C#
        END IF                                                          CCODE=C#
        IL2=ILNB( EXT2 )                                                CCODE=C#
        IF(IL2.EQ.0)GO TO 500                                           CCODE=C#
        FO(IT1:)=DOT//EXT2(1:IL2)                                       CCODE=C#
      ELSE                                                              CCODE=C#
C
C-----USE EXTENSION FROM ORIGINAL
C
        IF(EXT1(1:1).EQ.DOT)THEN                                        CCODE=C#
C
C-----'.' ONLY, FORCES BLANK EXTENSION
C
          GO TO 500                                                     CCODE=C#
        END IF                                                          CCODE=C#
        FO(IT1:)=DOT//EXT1(1:IL2)                                       CCODE=C#
      END IF                                                            CCODE=C#
      GO TO 550                                                         CCODE=C#
  500 CONTINUE
      FO(IT1:)=' '
  550 CONTINUE
C!    write (*,*) 'in pcfs return fo="'//fo(1:ilnb( fo ))//'"'
C!    read  (*,*)
      RETURN
      END
C
C
C
      SUBROUTINE TXCLOS(IUNIT,FI,USF)
C
C-----THIS SUBROUTINE CLOSES A FILE AND MAKES A PERMANENT COPY
C-----OF THE FILE.  THE METHOD OF STORAGE DEPENDS ON THE COMPUTER.
C     IUNIT - UNIT WITH OPEN FILE.
C     FI    - CHARACTER VARIABLE WITH NAME OF FILE TO BE MADE PERMANENT.
C     USF   - SEE <DEF> IN SUBROUTINE "TXINQ".
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IUNIT
      CHARACTER*(*)           FI,USF
C\    CHARACTER FIT*7,USFU*60
C\    INTEGER   II,IFN,ILNB,IRET,JJ,NCFN
      CHARACTER FMTA4*4     ,FMT1*15
      PARAMETER(FMTA4='(A4)',FMT1='(3A,A4,A1,A4,A)')
      CLOSE(IUNIT)
C\    IRET=-1
C\    READ(USF(3:6),FMTA4)II
C\    READ(USF(7:10),FMTA4)JJ
C\    NCFN=ILNB( FI )
C\    IF(NCFN.EQ.0)GO TO 900
C\    USFU=USF
C\    CALL  TOUPR   ( USFU )
C\    IF(USFU(1:2).NE.'PF')GO TO 900
C\    FIT=FI(1:NCFN)
C\    READ(FIT,'(A7)')IFN
C\    CALL SAVEPF(II,IFN,JJ,IRET)
C\    IF(IRET.EQ.0)THEN
C\      WRITE(*,FMT1)'File "',FI(1:NCFN),'" has been saved '//
c\   1               'on permanent file ',II,'/',JJ,'.'
C\      GO TO 9990
C\    END IF
C\900 CONTINUE
C\    WRITE(*,FMT1)'File "',FI(1:NCFN),'" was not '//
c\   1             'saved on permanent file ',II,'/',JJ,'.'
C\    IF(IRET.GT.0)WRITE(*,'(A,I2)')'Error code: ',IRET
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE TXDELF(IUNIT,FI,USF,NOUT,STA)
C
C-----THIS SUBROUTINE CLOSES AN OPEN FILE AND DELETES THIS FILE FROM
C-----STORAGE.  THE METHOD OF DELETION DEPENDS ON THE COMPUTER.
C
C     IUNIT - UNIT WITH OPEN FILE.  IF 0 (ZERO), THE FORTRAN CLOSE
C             STATEMENT IS NOT EXECUTED.
C     FI    - CHARACTER VARIABLE WITH NAME OF FILE TO BE DELETED.
C     USF   - SEE <DEF> IN SUBROUTINE TXINQ.
C     NOUT  - UNIT NUMBER OF I/O (TERMINAL) FILE FOR ERROR MESSAGE.
C     STA   - LOGICAL VARIABLE RETURNS .TRUE. IF FILE DELETION IS
C             SUCCESSFUL, OTHERWISE .FALSE.
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IUNIT       ,NOUT
      CHARACTER *(*)          FI,USF
      LOGICAL                             STA
      CHARACTER*4 FMTA4
      PARAMETER  (FMTA4='(A4)')
C\    CHARACTER*7 FIT
C\    INTEGER     II,IFN,ILNB,IRET,JJ,NCFN
      IF(IUNIT.GT.0)THEN
        CLOSE(IUNIT,STATUS='DELETE',ERR=100)
        GO TO 500
  100   CONTINUE
        STA=.FALSE.
        GO TO 600
      END IF
  500 CONTINUE
      STA=.TRUE.
  600 CONTINUE
C\    IRET=-1
C\    READ(USF(3:6),FMTA4)II
C\    READ(USF(7:10),FMTA4)JJ
C\    NCFN=ILNB( FI )
C\    IF(NCFN.EQ.0)GO TO 900
C\    USFU=USF
C\    CALL  TOUPR   ( USFU )
C\    IF(USFU(1:2).NE.'PF')GO TO 900
C\    NCFN=ILNB( FI )
C\    FIT=FI(1:NCFN)
C\    READ(FIT,'(A7)')IFN
C\    CALL KILLPF(II,IFN,JJ,IRET)
C\    IF(IRET.EQ.0)GO TO 9990
C\    IF(IRET.EQ.13)GO TO 9990
C\900 CONTINUE
C\    STA=.FALSE.
C\    WRITE(NOUT,'(3A,A4,A1,A4,A)')'File "',FI(1:NCFN),
C\   1      '" was not deleted from permanent file ',II,'/',JJ,'.'
C\    IF(IRET.GT.0)WRITE(NOUT,'(A,I2)')' ERROR CODE: ',IRET
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE CHARSH(NLINE,NC)
C
C-----THIS SUBROUTINE LEFT-SHIFTS CHARACTERS IN A CHARACTER VARIABLE,
C-----ELIMINATING THE LEFT-MOST CHARACTERS AND BLANK-FILLING THE
C-----RIGHT-MOST.
C
C     NLINE - CHARACTER VARIABLE WITH CHARACTERS TO BE SHIFTED.
C     NC    - INTEGER, NUMBER OF CHARACTER TO BE SHIFTED TO POSITION 1.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     NLINE
      INTEGER                 NC
      INTEGER  I,ILNB,J,K
      J=0
      K=ILNB( NLINE )
      DO 100 I=NC,K
      J=J+1
      NLINE(J:J)=NLINE(I:I)
  100 CONTINUE
      J=J+1
      IF(J.LE.K)NLINE(J:K)=' '
      RETURN
      END
C
C
C
      SUBROUTINE RJTXT(TXTIN,TXTOUT)
C
C-----THIS SUBROUTINE RIGHT JUSTIFIES THE TEXT FROM CHARACTER
C-----VARIABLE<TXTIN> INTO CHARACTER VARIABLE <TXTOUT>.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)    TXTIN,TXTOUT
      CHARACTER*40 NBLANK,TEMP
      INTEGER  ILNB,NBF,NCI,NLNI,NLNO
      DATA NBLANK / ' ' /
      NLNI=LEN( TXTIN )
      NCI=ILNB( TXTIN )
      IF(NCI.EQ.0)THEN
        TXTOUT=NBLANK
        GO TO 9990
      END IF
      NLNO=LEN( TXTOUT )
      NBF=NLNO-NCI
      IF(NBF.LE.0)THEN
        TXTOUT=TXTIN
      ELSE
        TEMP=NBLANK(1:NBF)//TXTIN(1:NCI)
        TXTOUT=TEMP
      END IF
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE PCTRNG(I,NINLNS,R1,R2,XR1,XR2)
C
C-----THIS SUBROUTINE FORCES RANGE TO ZERO FOR OUTBOUND AND
C-----RESETS RANGE TO THE ORIGINAL VALUE FOR INBOUND LANES
C
C     I      - LANE NUMBER
C     NINLNS - NUMBER OF INBOUND LANES
C     R1     - LOWER RANGE
C     R2     - UPPER RANGE
C     XR1    - LOWER RANGE, ORIGINAL
C     XR2    - UPPER RANGE, ORIGINAL
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           I,NINLNS
      REAL                       R1,R2,XR1,XR2
      REAL      X0
      PARAMETER(X0=0.0)
      IF(I.GT.NINLNS)THEN
C
C-----FORCE TRAFFIC PERCENTS TO ZERO FOR OUTBOUND LANES
C
        IF(R2.NE.X0)THEN
          R1=X0
          R2=X0
        END IF
      ELSE
        IF(R2.NE.XR2)THEN
          R1=XR1
          R2=XR2
        END IF
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE OPENDG(IU,FI,EXT
C|   1                           ,CC
C~   1                           ,CC
C%   1                           ,CC
     2                              )
C
C-----OPEN A GENERIC DATA LISTING FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU
      CHARACTER*(*)        FI,EXT
C|    CHARACTER*(*)               CC
C~    CHARACTER*(*)               CC
C%    CHARACTER*(*)               CC
C|    CHARACTER*7 STAT
      LOGICAL EXI
C|    IF(INDEX(FI,';').GT.0)THEN
C|      STAT='UNKNOWN'
C|    ELSE
C|      STAT='NEW'
C|    END IF
      OPEN(IU,FILE=FI
C|   1               ,STATUS=STAT,DEFAULTFILE=EXT
C|   1                                           ,CARRIAGECONTROL=CC
C~   1                                           ,CARRIAGECONTROL=CC
C%   1                                           ,CARRIAGECONTROL=CC
     2                                                              )
      CALL TXINQ(IU,FI,' ',FI,EXI)
      RETURN
      END
C
C
C
      SUBROUTINE OPENDL(IU,FI)
C
C-----OPEN A DATA LISTING FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU
      CHARACTER*(*)        FI
      CALL OPENDG(IU,FI,'.txt'
C|   1                        ,'LIST'
C~   1                        ,'LIST'
C%   1                        ,'LIST'
     2                                  )
      RETURN
      END
C
C
C
      SUBROUTINE OPENDD(IU,FI)
C
C-----OPEN A DATA DAT FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU
      CHARACTER*(*)        FI
      CALL OPENDG(IU,FI,'.dat'
C|   1                        ,'LIST'
C~   1                        ,'LIST'
C%   1                        ,'LIST'
     2                               )
      RETURN
      END
C
C
C
      SUBROUTINE OPENDF(IU,FI,IRE,ERR)
C
C-----OPEN DIRECT ACCESS, FORMATTED FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU   ,IRE
      CHARACTER*(*)        FI
      LOGICAL                     ERR
      CHARACTER*60 LFI,DEFFN
C|    CHARACTER*7  STAT
      IF(FI.EQ.' ')THEN
        LFI=DEFFN(IU)
      ELSE
        LFI=FI
      END IF
C|    IF(INDEX(LFI,';').GT.0)THEN
C|      STAT='UNKNOWN'
C|    ELSE
C|      STAT='NEW'
C|    END IF
      OPEN(IU,FILE=LFI,ACCESS='DIRECT',FORM='FORMATTED',RECL=IRE
C|   1     ,STATUS=STAT
     2                 ,ERR=1000)
      ERR=.FALSE.
      GO TO 9900
 1000 CONTINUE
      ERR=.TRUE.
 9900 RETURN
      END
C
C
C
      SUBROUTINE OPENFI(IU,FI,ERR)
C
C-----OPEN A PLAIN OLD FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU
      CHARACTER*(*)        FI
      LOGICAL                 ERR
C|    CHARACTER STAT*7
      CHARACTER*60 LFI,DEFFN
      IF(FI.EQ.' ')THEN
        LFI=DEFFN(IU)
      ELSE
        LFI=FI
      END IF
C|    IF(INDEX(LFI,';').GT.0)THEN
C|      STAT='UNKNOWN'
C|    ELSE
C|      STAT='NEW'
C|    END IF
      OPEN(IU,FILE=LFI,ERR=1000
C|   1                         ,STATUS=STAT
     2                                     )
      ERR=.FALSE.
      GO TO 9900
 1000 CONTINUE
      ERR=.TRUE.
 9900 RETURN
      END
C
C
C
      SUBROUTINE OPENRO(IU,FI,ST,AC,FO,IRE,ERR)
C
C-----OPEN FILE WITH READONLY ACCESS
C-----FILE MUST EXIST
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU            ,IRE
      CHARACTER*(*)        FI,ST,AC,FO
      LOGICAL                              ERR
      LOGICAL EXI
      CHARACTER LST*7,LFI*60,DEFFN*60,ACU*10
      IF(FI.EQ.' ')THEN
        LFI=DEFFN(IU)
      ELSE
        LFI=FI
      END IF
      IF(LFI.EQ.' ')GO TO 1000
      INQUIRE(FILE=LFI,EXIST=EXI)
      IF(.NOT.EXI)GO TO 1000
      IF(ST.EQ.' ')THEN
        LST='UNKNOWN'
C|      IF(INDEX(LFI,';').EQ.0)LST='NEW'
      ELSE
        LST=ST
      END IF
      ACU=AC
      CALL  TOUPR   ( ACU )
      IF(ACU.EQ.'DIRECT')THEN
        OPEN(IU,FILE=LFI,STATUS=LST,ACCESS=AC,FORM=FO,RECL=IRE,ERR=1000,
C|   1       READONLY
C?   1       READONLY
     1       ACTION='READ'                                              CCODE=C{
C%   1       ACTION='READ'
     2                    )
      ELSE
        OPEN(IU,FILE=LFI,STATUS=LST,ACCESS=AC,FORM=FO,ERR=1000,
C|   1       READONLY
C?   1       READONLY
     1       ACTION='READ'                                              CCODE=C{
C%   1       ACTION='READ'
     2                    )
      END IF
      ERR=.FALSE.
      GO TO 9900
 1000 CONTINUE
      ERR=.TRUE.
 9900 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE OPNERR(FILE)
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     FILE
      INCLUDE 'CMTX01'
      CALL SHONAM('Error when accessing file',FILE,78)
      STOP
      END
C
C
C
      SUBROUTINE OPENTF(IU,FI,ERR)
C
C-----OPEN A FILE FOR TEXT OUTPUT
C-----FILE MUST NOT BE READ-ONLY
C
C     IU - UNIT NUMBER FOR FILE
C     FI - FILE NAME
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU
      CHARACTER*(*)        FI
      LOGICAL                 ERR
      OPEN(IU,FILE=FI,ERR=1000
C|   1                        ,CARRIAGECONTROL='LIST',STATUS='NEW'
C?   1                        ,CARRIAGECONTROL='LIST'
C~   1                        ,CARRIAGECONTROL='LIST'
C%   1                        ,CARRIAGECONTROL='LIST'
     2                                                            )
      ERR=.FALSE.
      GO TO 9900
 1000 CONTINUE
      ERR=.TRUE.
 9900 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE GDVS0(N,IA,NOUT,NIO,NFCF,NPRCNT,IS1,IS2,SYSDAT,
     1                 L1,L2,L3,L4,L5,L7,L8,L9)
C
C     N      - PASSES 1 FOR READING GDV SETUP INFO
C                     2 FOR READING SIM
C              RETURNS  0 FOR NO ERROR
C                      -1 FOR ERRORS
C     IA     - ABSOLUTE VALUE IS UNIT NUMBER FOR READING GDVS00
C              <0 - NO ERROR MESSAGE
C     NOUT   - RETURNS INTEGERS FROM FIRST LINE
C     NIO    - RETURNS UNIT NUMBER WITH OPENED CONSOLE FILE
C              IF <0 DON'T TRY TO OPEN THE CONSOLE FILE
C     NFCF   - RETURNS HOW MAMY FILES TO KEEP IN CATALOG
C              IF LINE IS BLANK OR MISSING, USE 16
C              IF -1, CATALOGING WILL NOT BE ACTIVE
C     NPRCNT - RETURNS NUMBER OF DT'S BETWEEN SCREEN PRINTS
C     IS1    - SPARE
C     IS2    - SPARE
C     SYSDAT - LOCATION OF TEXAS MODEL SYSTEM DATA (FROM COMMAND LINE)
C     L1     - "NO" SKIP FIRST   LINE OF GDVS00
C              OTHERWISE READ FIRST  LINE AND RETURN 20 INTGRS IN <NOUT>
C     L2     - "NO" SKIP SECOND  LINE OF GDVS00
C              OTHERWISE READ SECOND LINE AND RETURN TEXT IN <L2>
C     L3     - "NO" SKIP THIRD   LINE OF GDVS00
C              OTHERWISE READ THIRD  LINE AND RETURN TEXT IN <L3>
C     L4     - "NO" SKIP FOURTH  LINE OF GDVS00
C              OTHERWISE READ FOURTH LINE AND RETURN TEXT IN <L4>
C     L5     - "NO" SKIP FIFTH   LINE OF GDVS00
C              OTHERWISE READ USER GROUP LIBRARY FILE NAME AND NUMBER OF
C              FILES TO KEEP IN LIBRARY. RETURN USER GROUP LIBRARY FILE
C              NAME IN <L5> AND NUMBER OF FILES IN <NFCF>
C     L7     - "NO" SKIP SEVENTH LINE OF GDVS00
C              "NPRCNT" - READ SPEED INFO ONLY
C              OTHERWISE READ DISPLAY INFO FROM COL 1-40 (RETURN IN
C              <L7>) AND MACHINE SPEED INFO FROM COL 41-42 (RETURN IN
C              <NPRCNT>)
C     L8     - "PHASE" - TRY TO READ PHASE INFORMATION FROM EIGHT LINE
C              IF THE PHASE INFO RECORD IS NOT PRESENT, USE DEFAULTS
C     L9     - SPARE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER          N,IA,NOUT(*),
     1                           NIO,NFCF,NPRCNT,IS1,IS2
      CHARACTER*(*)                                      SYSDAT,
     1                 L1,L2,L3,L4,L5,L7,L8,L9
      CHARACTER*1 SDSEP
C?    PARAMETER  (SDSEP='/')
      PARAMETER  (SDSEP='\')                                            CCODE=C#
C}    PARAMETER  (SDSEP='/')
C%    PARAMETER  (SDSEP=':')
      INCLUDE 'CMPR01'
      INCLUDE 'CMTXAR'
      CHARACTER*3 L1U,L2U,L3U,L4U,L5U,L7U*6,L8U
      CHARACTER*9 FORM1(2),IOFORM*3,NLINE*80
      LOGICAL EXI,ERRMSG,LP(8),LPD(8)
      INTEGER I,ILNB,J,NC,NF
      DATA IOFORM,FORM1(1),FORM1(2) / '(A)','(A40)','(40X,A40)' /
      DATA ERRMSG / .TRUE. /
      IF(IA.LT.0)THEN
        I=-IA
        ERRMSG=.FALSE.
      ELSE
        I=IA
      END IF
      NF=N
      N=0
      NPRCNT=1
C
C ----- TRAFFIC PHASE INFORMATION
C
      NEWPH(1)=0
      NEWPHD(1)=0
      DO 100 J=1,NSP
      LP(J)=.FALSE.
      LPD(J)=.FALSE.
  100 CONTINUE
      NLINE='gdvs00'
      INQUIRE (FILE=NLINE,EXIST=EXI)
      IF(.NOT.EXI)THEN
        NC=ILNB( SYSDAT )
        IF(NC.EQ.0)GO TO 8840
        CALL TXINQ(0,SYSDAT,'gdvs00'
C|   1                              //'.dat'
     2                                      ,NLINE,EXI)
        IF(.NOT.EXI)GOTO 8840
      END IF
      OPEN (I,FILE=NLINE,ERR=8840,STATUS='OLD'
C|   1                                        ,READONLY
C?   1                                        ,READONLY
     1                                        ,ACTION='READ'            CCODE=C{
C%   1                                        ,ACTION='READ'
     2                                                      )
      REWIND (I,ERR=8840)
      L1U=L1
      CALL  TOUPR   ( L1U )
      IF(L1U.EQ.'NO')THEN
        READ(I,IOFORM,ERR=8840,END=8840)
      ELSE
        READ(I,'(20I4)',ERR=8840,END=8840)(NOUT(J),J=1,20)
      END IF
      L2U=L2
      CALL  TOUPR   ( L2U )
      IF(L2U.EQ.'NO')THEN
        READ(I,IOFORM,ERR=8840,END=8840)
      ELSE
        READ(I,FORM1(NF),ERR=8840,END=8840)L2
        J=ILNB( L2 )
        IF(L2(J:J).NE.SDSEP)L2=L2(1:J)//SDSEP
      END IF
      L3U=L3
      CALL  TOUPR   ( L3U )
      IF(L3U.EQ.'NO')THEN
        READ(I,IOFORM,ERR=8840,END=8840)
      ELSE
        READ(I,FORM1(NF),ERR=8840,END=8840)L3
      END IF
      L4U=L4
      CALL  TOUPR   ( L4U )
      IF(L4U.EQ.'NO')THEN
        READ(I,IOFORM,ERR=8840,END=8840)
      ELSE
        READ(I,FORM1(NF),ERR=8840,END=8840)L4
      END IF
      L5U=L5
      CALL  TOUPR   ( L5U )
      IF(L5U.EQ.'NO')THEN
        READ(I,IOFORM,ERR=8840,END=8840)
      ELSE
        NFCF=0
        READ(I,'(A40,I2)',ERR=8840,END=8840)L5,NFCF
        IF(NFCF.EQ.0)NFCF=16
      END IF
      N=0
      IF(NIO.LT.0)THEN
        NIO=-1
        READ(I,IOFORM,ERR=8840,END=8840)
      ELSE
        NIO=-1
        READ(I,IOFORM,ERR=8840,END=8840) NLINE
        READ(NLINE(41:42),'(I2)',ERR=8840) NIO
C
C-----OPEN TERMINAL FILE
C
        IF(NIO.NE.6)THEN
          OPEN (NIO,FILE=NLINE(1:40),ERR=8840
C|   1                         ,CARRIAGECONTROL='LIST',STATUS='UNKNOWN'
C?   1                         ,CARRIAGECONTROL='LIST'
C~   1                         ,CARRIAGECONTROL='LIST'
C%   1                         ,CARRIAGECONTROL='LIST'
     2                                                                 )
        END IF
      END IF
C
C-----NPRCNT IS THE NUMBER OF DT'S BETWEEN PRINTS TO SCREEN
C
      READ (I,IOFORM,ERR=180,END=180) NLINE
      L7U=L7
      CALL  TOUPR   ( L7U )
      IF(L7U.NE.'NO')THEN
        IF(L7U.NE.'NPRCNT')THEN
          L7=' '
          L7=NLINE(1:40)
        END IF
        IF(NLINE(43:43).GE.'0'.AND.NLINE(43:43).LE.'9')THEN
          read (NLINE(41:43),'(i3)',err=180) NPRCNT
        ELSE
          read (NLINE(41:42),'(i2)',err=180) NPRCNT
        END IF
        if(NPRCNT.le.0)NPRCNT=1
      END IF
C
C ----- TRAFFIC PHASE INFORMATION
C
      READ(I,IOFORM,ERR=170,END=180)NLINE
      L8U=L8
      CALL  TOUPR ( L8U )
      IF(L8U.NE.'PHASE')GO TO 180
      IF(NLINE(1:NSP).NE.' ')THEN
        READ(NLINE(1:NSP),'(12I1)',ERR=171)(NEWPH(I),I=1,NSP)
        DO 120 J=1,NSP
        IF((NEWPH(J).LT.1).OR.(NEWPH(J).GT.NSP))GO TO 160
        IF(NEWPH(J).GT.0)THEN
C
C ----- IS THIS NUMBER A DUPLICATE ?
C
          IF(LP(NEWPH(J)))GO TO 140
          LP(NEWPH(J))=.TRUE.
        END IF
  120   CONTINUE
      END IF
      IF(NLINE(NSP+1:NSP+NSP).NE.' ')THEN
        READ(NLINE(NSP+1:NSP+NSP),'(12I1)',ERR=172)(NEWPHD(I),I=1,NSP)
        DO 130 J=1,NSP
        IF((NEWPHD(J).LT.1).OR.(NEWPHD(J).GT.NSP))GO TO 160
        IF(NEWPHD(J).GT.0)THEN
C
C ----- IS THIS NUMBER A DUPLICATE ?
C
          IF(LPD(NEWPHD(J)))GO TO 150
          LPD(NEWPHD(J))=.TRUE.
        END IF
  130   CONTINUE
      END IF
      GO TO 180
  140 CONTINUE
      WRITE(*,'(A/A,I2,A)')'Error reading traffic phase numbers '//
     1                     'from file "gdvs00".',
     2                     'One of the',NSP,' single intersection '//
     3                     'traffic phase numbers is duplicated.'
      GO TO 175
  150 CONTINUE
      WRITE(*,'(A/A,I2,A)')'Error reading traffic phase numbers '//
     1                     'from file "gdvs00".',
     2                     'One of the',NSP,' diamond intersection '//
     3                     'traffic phase numbers is duplicated.'
      GO TO 175
  160 CONTINUE
      WRITE(*,'(A/A,I2,A)')'Error reading traffic phase numbers '//
     1                     'from file "gdvs00".',
     2                     'One of the traffic phase numbers is '//
     3                     'not 1 thru',NSP,'.'
      GO TO 175
  170 CONTINUE
      WRITE(*,'(A)')'Error reading traffic phase numbers '//
     1              'from file "gdvs00".'
      GO TO 175
  171 CONTINUE
      WRITE(*,'(A)')'Error reading single intersection traffic phase '//
     1              'numbers from file "gdvs00".'
      GO TO 175
  172 CONTINUE
      WRITE(*,'(A)')'Error reading diamond intersection traffic '//
     1              'phase numbers from file "gdvs00".'
  175 CONTINUE
      N=-1
      WRITE(*,'(A)')'Line 8 (if present) of this file must have '//
     1              'phase numbers:'
      WRITE(*,'(A)')'  COL  1 - single intersection North leg'
      WRITE(*,'(A)')'  COL  2 - single intersection East  leg'
      WRITE(*,'(A)')'  COL  3 - single intersection South leg'
      WRITE(*,'(A)')'  COL  4 - single intersection West  leg'
      WRITE(*,'(A)')'  COL  5 - single intersection North leg '//
     1              '(left turns)'
      WRITE(*,'(A)')'  COL  6 - single intersection East  leg '//
     1              '(left turns)'
      WRITE(*,'(A)')'  COL  7 - single intersection South leg '//
     1              '(left turns)'
      WRITE(*,'(A)')'  COL  8 - single intersection West  leg '//
     1              '(left turns)'
      WRITE(*,'(A)')'  COL  9 - diamond leg 2'
      WRITE(*,'(A)')'  COL 10 - diamond leg 3'
      WRITE(*,'(A)')'  COL 11 - diamond internal to right'
      WRITE(*,'(A)')'  COL 12 - diamond internal to left'
      WRITE(*,'(A)')'  COL 13 - diamond leg 5'
      WRITE(*,'(A)')'  COL 14 - diamond leg 6'
      WRITE(*,'(A)')'  COL 15 - diamond internal to right '//
     1              '(left turns)'
      WRITE(*,'(A)')'  COL 16 - diamond internal to left  '//
     1              '(left turns)'
      WRITE(*,'(A)')'  Columns 1 thru 8 all blank will use '//
     1              'defaults for single intersection'
      WRITE(*,'(A)')'  Columns 9 thru 15 all blank will use '//
     1              'defaults for diamond'
  180 CONTINUE
      CLOSE (I)
      GO TO 9900
 8840 CONTINUE
      N=-1
      IF(ERRMSG)WRITE (*,884)
  884 FORMAT(' Error when accessing file "gdvs00'
C|   *                                           ,'.dat'
     *                                                  ,'".',/,
     *      ' Line 6 of this file must have the i/o terminal name ',
     *      'in columns 1-40 and',/,
     *      ' the unit number to be used for the i/o terminal in ',
     *      'columns 41-42.')
 9900 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE CRNDRW(IU,FI,ST,MASK)
C
C-----CREATE A NEW, DIRECT ACCESS FILE WITH SPECIFIC FILE
C-----PROTECTION ON SOME SYSTEMS
C
C     IU   - UNIT NUMBER FOR OPENING FILE
C     FI   - FILE NAME
C     ST   - OPEN STATUS
C     MASK - PROTECTION MASK
C            FOR VAX VMS - DEWRDEWRDEWRDEWR
C                                      |  |---SYSTEM
C                                  |  |-------OWNER
C                              |  |-----------GROUP
C                          |  |---------------WORLD
C                          0 - ACCESS ENABLED
C                          1 - ACCESS DENIED
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           IU      ,MASK
      CHARACTER*(*)        FI,ST
C?    CHARACTER*80  SYSPAR
C}    CHARACTER*80  SYSPAR
C|    INTEGER*4 STAT SYS$GETDFPROT
C|    INTEGER*2 CURPRO,TMPPRO
C?    INTEGER   ILNB
C}    INTEGER   ILNB
C?    INTEGER*2 SYSLEN
C}    INTEGER*2 SYSLEN
C|    EQUIVALENCE(MASK1,TMPPRO)
C|    MASK1=IABS ( MASK )
C|    STAT=SYS$SETDFPROT(TMPPRO,CURPRO)
      OPEN(IU,FILE=FI,STATUS=ST,ACCESS='DIRECT',
     1     FORM='FORMATTED',RECL=80)
C?    IF ( MASK .LE. 0 ) THEN
C?      SYSPAR = 'chmod a+r ' // FI(1:ILNB( FI ))
C?      SYSLEN = ILNB ( SYSPAR ) + 1
C?      SYSPAR(SYSLEN:SYSLEN) = CHAR ( 0 )
C?      CALL  SYSTEM  ( SYSPAR(1:SYSLEN) )
C?      SYSPAR = 'chgrp 0 '   // FI(1:ILNB( FI ))
C?      SYSLEN = ILNB ( SYSPAR ) + 1
C?      SYSPAR(SYSLEN:SYSLEN) = CHAR ( 0 )
C?      CALL  SYSTEM  ( SYSPAR(1:SYSLEN) )
C?      SYSPAR = 'chown 0 '   // FI(1:ILNB( FI ))
C?      SYSLEN = ILNB ( SYSPAR ) + 1
C?      SYSPAR(SYSLEN:SYSLEN) = CHAR ( 0 )
C?      CALL  SYSTEM  ( SYSPAR(1:SYSLEN) )
C?    END IF
C|    STAT=SYS$SETDFPROT(CURPRO,%VAL(0))
C}    IF ( MASK .LE. 0 ) THEN
C}      SYSPAR = 'chmod a+r ' // FI(1:ILNB( FI ))
C}      SYSLEN = ILNB ( SYSPAR ) + 1
C}      SYSPAR(SYSLEN:SYSLEN) = CHAR ( 0 )
C}      CALL  SYSTEM  ( SYSPAR(1:SYSLEN) )
C}      SYSPAR = 'chgrp 0 '   // FI(1:ILNB( FI ))
C}      SYSLEN = ILNB ( SYSPAR ) + 1
C}      SYSPAR(SYSLEN:SYSLEN) = CHAR ( 0 )
C}      CALL  SYSTEM  ( SYSPAR(1:SYSLEN) )
C}      SYSPAR = 'chown 0 '   // FI(1:ILNB( FI ))
C}      SYSLEN = ILNB ( SYSPAR ) + 1
C}      SYSPAR(SYSLEN:SYSLEN) = CHAR ( 0 )
C}      CALL  SYSTEM  ( SYSPAR(1:SYSLEN) )
C}    END IF
      RETURN
      END
C
C
C
      SUBROUTINE TXGFNL(FNI,DFN,FNO)
C
C ----- GET FILE NAME FOR A LIST FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     FNI,DFN,FNO
      CALL TXGFNG(FNI,DFN,FNO
C|   1                       ,'.lst'
     2                              )
      RETURN
      END
C
C
C
      SUBROUTINE TXGFND(FNI,DFN,FNO)
C
C ----- GET FILE NAME FOR A DATA FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     FNI,DFN,FNO
      CALL TXGFNG(FNI,DFN,FNO
C|   1                       ,'.dat'
     2                              )
      RETURN
      END
C
C
C
      SUBROUTINE TXGFNG(FNI,DFN,FNO
C|   1                             ,EXT
     2                                 )
C
C ----- GET GENERIC FILE NAME
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)     FNI,DFN,FNO
C|    CHARACTER*(*)                 EXT
      LOGICAL EXI
      INTEGER ILNB
      CALL TXINQ(0,FNI,DFN(1:MAX0(ILNB( DFN ),1))
C|   1                                        //EXT
     2                                             ,FNO,EXI)
C|    NCI=INDEX(FNI,';')
C|    NCO=INDEX(FNO,';')
C|    IF(NCI.GT.0)THEN
C
C ----- IF VERSION FROM THE COMMAND LINE, USE IT
C
C|      IF(NCO.GT.0)FNO(NCO:)=FNI(NCI:)
C|    ELSE
C
C ----- NO VERSION FROM THE COMMAND LINE, LET THE OPEN PICK THE VERSION
C
C|      IF(NCO.GT.0)FNO(NCO:)=' '
C|    END IF
      RETURN
      END
C
C
C
      SUBROUTINE NEXTIT ( IT,NLHLPX,XHELP )
C
C ----- FIND NEXT IT
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER             IT,NLHLPX
      CHARACTER*(*)                 XHELP(*)
      CHARACTER*2 XHELPU
      INTEGER     ILNB,ITSAVE
  901 FORMAT(' Error in NEXTIT - "F(" not found starting at line ',I2)
  902 FORMAT(' Line ',I2.2,' = "',A,'"')
      ITSAVE = IT
      DO IT = IT+1 , NLHLPX
        XHELPU = XHELP(IT)(1:2)
        CALL  TOUPR   ( XHELPU )
        IF ( XHELPU . EQ . 'F(' )                RETURN
      END DO
      WRITE (*,901) ITSAVE
      DO IT = 1 , NLHLPX
      WRITE (*,902) IT,XHELP(IT)(1:ILNB( XHELP(IT) ))
      END DO
      STOP
      END
C
C
C
      SUBROUTINE FINDBE ( IB,IE,XHELP,NS,S1,S2,S3 )
C
C ----- FIND BEGINNING AND ENDING OF MESSAGE
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER             IB,IE,      NS
      CHARACTER*(*)             XHELP,   S1,S2,S3
      CHARACTER*80 XHELPU
      INTEGER      IT,NC
      NC = LEN ( XHELP )
      XHELPU = XHELP
      CALL  TOUPR   ( XHELPU )
      IB = INDEX ( XHELPU,'-' ) + 2
      IE = NC
      IF ( NS . GE . 1 )                         THEN
        IT = INDEX ( XHELPU(IB:NC),S1 )
        IF ( IT.GT. 0 . AND . S1.EQ.'?')         IT = IT + 1
        IT = IT + IB - 1
        IF ( IT.GE.IB . AND . IT.LT.IE )         IE = IT
      END IF
      IF ( NS . GE . 2 )                         THEN
        IT = INDEX ( XHELPU(IB:NC),S2 )
        IF ( IT.GT. 0 . AND . S2.EQ.'?')         IT = IT + 1
        IT = IT + IB - 1
        IF ( IT.GE.IB . AND . IT.LT.IE )         IE = IT
      END IF
      IF ( NS . GE . 3 )                         THEN
        IT = INDEX ( XHELPU(IB:NC),S3 )
        IF ( IT.GT. 0 . AND . S3.EQ.'?')         IT = IT + 1
        IT = IT + IB - 1
        IF ( IT.GE.IB . AND . IT.LT.IE )         IE = IT
      END IF
      IF ( IE . LT . NC )                        IE = IE - 1
      RETURN
      END
