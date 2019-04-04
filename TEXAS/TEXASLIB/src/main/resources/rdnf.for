      FUNCTION RDNF(EFILE,NFILE,ILINE,NREF,NFCF)
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
C --- CODED     5-17-84 BY R. F. INMAN
C --- REVISED   9-10-87 BY R. F. INMAN
C
C --- TITLE - REVISED DATA , NEW FILE
C
C --- FUNCTION - THIS LOGICAL FUNCTION PROMPTS USER TO SEE IF REVISED DATA
C ---            IS TO BE KEPT UNDER A NEW FILE NAME.
C
C --- ARGUMENTS - RDNF = RETURNS LOGICAL .TRUE. IF REVISED DATA
C ---                    IS TO BE SAVED, OTHERWISE .FALSE.
C ---             EFILE = CHARACTER VARIABLE WITH EXISTING DATA
C ---                     FILE NAME
C ---             NFILE = CHARACTER VARIABLE TO GET NEW FILE NAME
C ---             ILINE = CHARACTER VARIABLE CONTAINING "SIMDATA" TO
C ---                     INDICATE SIMDATA AND FOR FOR SCRATCH USE
C ---                     ALSO RETURNS 3 CHARACTER TEXT OF
C ---                     CATALOG NUMBER.  BLANK FOR REVISED
C ---                     FILE NOT CATALOGED.
C ---             NREF = LOGICAL VARIABLE. PUT NON-REVISED EXISTING
C ---                    FILE INTO USER-GROUP LIBRARY ?
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  RDNF
      CHARACTER*(*) EFILE,NFILE,ILINE
      LOGICAL                         NREF
      INTEGER                              NFCF
      CHARACTER*4 AAFMT
      CHARACTER*6              DELETE
      CHARACTER*1                              ICN    ,ICY    ,ICF
      PARAMETER  (AAFMT='(4A)',DELETE='DELETE',ICN='N',ICY='Y',ICF='F')
      INCLUDE 'CMTX01'
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'USFILE'
      CHARACTER*60 EXT,DI,DR,NA,NAU,VOL
      CHARACTER*60 DDFILE,TFILE,EFILEU,TFILEU,USFILEU
      CHARACTER TLINE*80,CHATMP*10,ALT(3)*52,ILINEU*80,CHATMPU*10
      CHARACTER ALT1(5)*52,CHADUM*10
      LOGICAL   ADDED,REVEF,EXIST,DELEF,CCFF,AFTC,DEGDVF,ISGDV
      INTEGER   I,IERR,IFN,ILNB,IT1,IT2,MAXFIC,NC,NC1,NCC,NDFIO,NFIC
      REAL      X
      DATA EXIST,REVEF / 2*.FALSE. /
      DATA ALT(1)/'$a file name for new/revised data ("FILE=...")'/
      DATA (ALT(I),I=2,3)/'FILE=','NONE'/
      DATA ALT1(1)(1:35)/'$ file name ("FILE=...") or number '/
      DATA ALT1(1)(36:)/'("NUM=...")'/
      DATA ALT1(2)/'$of file name to be deleted: '/
      DATA (ALT1(I),I=3,5)/'FILE=','NUM','NONE'/
      IOFORM='(A)'
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU.EQ.'SIMDATA')THEN
        ISGDV=.FALSE.
        NDFIO=MOUT(2)
      ELSE
        ISGDV=.TRUE.
        NDFIO=NOUT(2)
      END IF
      ADDED=.TRUE.
      IF(ISGDV)THEN
        IF(NREF)THEN
          RDNF=.FALSE.
          IT1=NDFIO-1
          TFILE=EFILE
          READ(IT1,'(2I5)',REC=1)NFIC,MAXFIC
          GO TO 830
        END IF
      END IF
      RDNF=.TRUE.
      IF(EFILE.EQ.' ')THEN
        DDFILE=USFILE
        CHADUM='new'
        NCC=3
      ELSE
        EFILEU=EFILE
        CALL  TOUPR   ( EFILEU )
        IT1=INDEX(EFILEU,'GDV')
        IF(IT1.GT.0)THEN
C
C ----- EXISTING FILE IS "GDV X " OR 'GDV T "
C
          DDFILE=USFILE
        ELSE
          USFILEU=USFILE
          CALL  TOUPR   ( USFILEU )
          IF(USFILEU(1:2).EQ.'PF')THEN
C
C ----- FOR UT CYBER
C
            DDFILE=USFILE
          ELSE
            DDFILE=EFILE
          END IF
        END IF
        CHADUM='revised'
        NCC=7
      END IF
      CALL YESNO('Do you want to save the '//CHADUM(1:NCC)//' data ?',
     1           TFILE,ALT)
      TFILEU=TFILE
      CALL  TOUPR   ( TFILEU )
      IF(TFILEU(1:1).EQ.ICF)GO TO 650
      IF(TFILEU(1:1).EQ.ICN)THEN
        RDNF=.FALSE.
        TFILE=' '
        ILINE=' '
        GO TO 9990
      END IF
C
C --- KEYIN IS "YES", PROMPT FOR FILE NAME
C
  500 WRITE(*,IOFORM)'Keyin'//ALT(1)(3:33)//': '
      READ(*,IOFORM)TFILE
      TFILEU=TFILE
      CALL  TOUPR   ( TFILEU )
      IF(TFILEU(1:5).NE.ALT(2)(1:5))GO TO 680
  650 CONTINUE
      CALL CHARSH(TFILE,6)
  680 CONTINUE
      IF((TFILE.EQ.' ') .AND. (IT1.EQ.0))TFILE=EFILE
      IF(TFILE.EQ.' ')TFILE=DFILE
  700 CONTINUE
      CALL TXINQ(0,TFILE,DDFILE,TFILE,EXIST)
      IF(.NOT.EXIST)THEN
C
C ----- FOR VAX VMS
C
        IT1=INDEX(TFILE,';0')+1
        IF(IT1.GT.1)TFILE(IT1:IT1)='1'
      END IF
  710 NC=ILNB( TFILE )
      NC1=ILNB( EFILE )
      IF(NC.EQ.0)THEN
        TFILE=EFILE
        NC=NC1
      END IF
      CALL YESNO('Is file name "'//TFILE(1:NC)//'" OK ?',ILINE,ALT)
      DDFILE=TFILE
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU(1:1).EQ.ICN)GO TO 500
      IF(ILINEU(1:1).EQ.ICF)THEN
        USFILEU=USFILE
        CALL  TOUPR   ( USFILEU )
        IF(USFILEU(1:2).NE.'PF')DDFILE=TFILE
        TFILE=ILINE(6:)
        GO TO 700
      END IF
      IF(ILINEU(1:1).EQ.' ')GO TO 710
C
C ----- SEE IF FILE NAME CONTAINS STANDARD FILE NAME CHARACTERS
C
      CALL PCFD(TFILE,VOL,DR,DI,NA,EXT)
      NAU=NA
      CALL  TOUPR   ( NAU )
      IF(INDEX(NAU,'GDV') .GE. 1)GO TO 750
      IF(INDEX(NAU,'GPO') .GE. 1)GO TO 750
      IF(INDEX(NAU,'DVO') .GE. 1)GO TO 750
      GO TO 800
  750 WRITE(*,IOFORM)'File named to save revised data'//
     1               ' has reserved characters in the name .'
      WRITE(*,IOFORM)'File name must not contain '//
     1               '"GDV", "GPO", or "DVO".'
      GO TO 500
  800 DELEF=.FALSE.
      IF(NC1.NE.NC)GO TO 807
      EFILEU=EFILE
      CALL  TOUPR   ( EFILEU )
      TFILEU=TFILE
      CALL  TOUPR   ( TFILEU )
      IF(EFILEU(1:NC).NE.TFILEU(1:NC))GO TO 807
C
C ----- NEW FILE IS SAME AS EXISTING FILE
C
      WRITE(*,IOFORM)'File named to save revised data is '//
     1               'the file that contains the existing data.'
  805 CALL YESNO('Do you want to save the revised data on the '//
     1           'existing data file ?',ILINE,ALT)
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU(1:1).EQ.ICN)GO TO 500
      IF(ILINEU(1:1).EQ.ICF)THEN
        USFILEU=USFILE
        CALL  TOUPR   ( USFILEU )
        IF(USFILEU(1:2).NE.'PF')DDFILE=TFILE
        TFILE=ILINE(6:)
        GO TO 700
      END IF
      IF(ILINEU(1:1).EQ.' ')GO TO 805
C
C ----- KEYIN IS "YES" - SAVE REVISED DATA ON EXISTING DATA FILE
C
      GO TO 825
  807 CONTINUE
      IF(EXIST)THEN
        WRITE(*,IOFORM)'File "'//TFILE(1:NC)//
     1                 '" is an existing file.'
  810   CALL YESNO('Do you want to use this file and erase '//
     1             'it''s contents ?',ILINE,ALT)
        ILINEU=ILINE
        CALL  TOUPR   ( ILINEU )
        IF(ILINEU(1:1).EQ.ICN)GO TO 500
        IF(ILINEU(1:1).EQ.ICF)THEN
          USFILEU=USFILE
          CALL  TOUPR   ( USFILEU )
          IF(USFILEU(1:2).NE.'PF')DDFILE=TFILE
          TFILE=ILINE(6:)
          GO TO 700
        END IF
        IF(ILINEU(1:1).EQ.' ')GO TO 810
C
C ----- KEYIN IS "YES" - DELETE THE FILE
C
        DELEF=.TRUE.
      END IF
C
C -----SEE IF FILE IS CATALOGED
C
  825 IT1=NOUT(2)-1
      IF((.NOT.ISGDV) .OR. (NFCF.LT.0))THEN
C
C ----- THIS IS SIMDATA OR FILE CATALOGING IS INACTIVE
C
        ADDED=.FALSE.
        GO TO 862
      END IF
      OPEN(IT1,FILE=CFILE,STATUS='OLD',ACCESS=DIR,FORM=FMTD,RECL=80)
      READ(IT1,'(2I5)',REC=1)NFIC,MAXFIC
      IF( CCFF(TFILE,ILINE,IFN,NFIC,IT1))THEN
C
C ----- FILE IS CATALOGED, SEE IF GPO AND/OR DVO FILES EXIST
C ----- IF SO, DELETE THEM
C
        IF(.NOT.DEGDVF(IFN,ILINE,TFILE,0,NOUT))GO TO 500
        GO TO 860
      END IF
  830 CONTINUE
C
C ----- FILE NOT YET CATALOGED, CATALOG FILE
C
C
C -----SEE IF CATALOG IS FULL
C
      IT2=0
      IF(NFIC.LT.MAXFIC)GO TO 850
C
C ----- CATALOG IS FULL
C
  835 WRITE(*,IOFORM)'User-Group Library now contains these file '//
     1               'names and is full:'
      WRITE(*,'(A,8X,A)')'Number','File Name'
      DO 837 I=1,MAXFIC
      READ(IT1,IOFORM,REC=I+1)ILINE
      NC=ILNB( ILINE )
      WRITE(*,'(I7,6X,A)')I,ILINE(1:NC)
  837 CONTINUE
  840 CALL YESNO('Do you want to delete a file name from the User-'//
     1           'Group Library ?',ILINE,ALT1)
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU(1:2).EQ.'NO')THEN
        WRITE(*,IOFORM)'File "'//TFILE(1:ILNB( TFILE ))//
     1                 '" won''t be added to the User-Group Library.'
        CLOSE(IT1)
        ADDED=.FALSE.
        GO TO 862
      END IF
      IF(ILINEU(1:1).NE.ICY)GO TO 843
      WRITE(*,IOFORM)'Keyin '//ALT1(1)(2:46)//' of file'
      WRITE(*,IOFORM)'to be deleted from the User-Group Library:'
      READ(*,IOFORM)ILINE
  843 CONTINUE
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU(1:3).NE.ALT1(4)(1:3))GO TO 844
C
C ----- KEYIN IS NUMBER OF FILE TO DELETE FROM U-G LIBRARY
C
      CALL READIN(ILINE,CHATMP,NC,I,I,I,X,CHATMP,0,I,'dummy')
      CHATMPU=CHATMP
      CALL  TOUPR   ( CHATMPU )
      IF(CHATMPU.EQ.'READ ERROR')THEN
        WRITE(*,IOFORM)'Error in keyin'
        GO TO 840
      END IF
      IFN=X
      IF(IFN.LT.1.OR.IFN.GT.MAXFIC)THEN
        WRITE(*,'(A,I3,A)')'Keyed in number is outside '//
     1                     'the range of 1 to',MAXFIC,'.'
        GO TO 840
      END IF
      READ(IT1,IOFORM,REC=IFN+1)ILINE
      GO TO 846
  844 CONTINUE
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU(1:5).NE.ALT1(3)(1:5))GO TO 845
C
C ----- KEYIN IS FILE NAME TO DELETE FROM U-G LIBRARY
C
      CALL CHARSH(ILINE,6)
      CALL TXINQ(0,ILINE,DFILE,ILINE,EXIST)
C
C ----- FOR VAX VMS
C
      IT2=INDEX(ILINE,';0')
      IF(IT2.GT.0)ILINE(IT2+1:IT2+1)='1'
      IF(CCFF(ILINE,TLINE,IFN,NFIC,IT1))GO TO 846
      NC1=ILNB( ILINE )
      WRITE(*,'(3A)')'File "',ILINE(1:NC1),'" not found in '//
     1               'the User-Group Library.'
      GO TO 840
  845 WRITE(*,IOFORM)'Error in keyin.'
      GO TO 840
  846 CONTINUE
      NC=ILNB( ILINE )
      IF(NC.GT.56)THEN
        NC=56
        ILINE(NC:NC)='>'
      END IF
      WRITE(TLINE,AAFMT)'Delete "',ILINE(1:NC),'" from '//
     1                  'the User-Group Library ?'
      CALL YESNO(TLINE(1:NC+24),ILINE,ALT(3))
      ILINEU=ILINE
      CALL  TOUPR   ( ILINEU )
      IF(ILINEU(1:1).EQ.ICN)GO TO 840
      IF(.NOT.DEGDVF(IFN,ILINE,TFILE,1,NOUT))THEN
        OPEN(IT1,FILE=CFILE,STATUS='OLD',ACCESS=DIR,FORM=FMTD,RECL=80)
        GO TO 835
      END IF
      ADDED=AFTC(TFILE,IFN,NFIC,MAXFIC,ILINE,IT1)
      IF(.NOT.ADDED)GO TO 862
      GO TO 860
  850 CONTINUE
C
C -----CATALOG NOT FULL, PREPARE TO ADD <TFILE> TO CATALOG
C
      IT2=NFIC+1
      IF(.NOT.DEGDVF(IT2,ILINE,TFILE,-1,NOUT))THEN
C
C ----- PROBLEM CATALOGING FILE INTO BLANK CAT. NO. <IT2>
C ----- PUT A DUMMY FILE NAME INTO CAT. NO. <IT2>
C
        ADDED=AFTC('SCRATCH',IT2,NFIC,MAXFIC,ILINE,IT1)
        GO TO 830
      END IF
      IFN=IT2
      ADDED=AFTC(TFILE,IFN,NFIC,MAXFIC,ILINE,IT1)
  860 CONTINUE
C
C ----- SAVE REVISED CATALOG FILE
C
      CALL TXCLOS(IT1,CFILE,USFILE)
  862 CONTINUE
      NC=MAX0(ILNB( TFILE ),1)
      IF(DELEF)THEN
        OPEN(IT1,FILE=TFILE(1:NC),ERR=870,IOSTAT=IERR,STATUS='UNKNOWN')
        CLOSE(IT1,STATUS=DELETE,ERR=870,IOSTAT=IERR)
      ELSE
        CLOSE(IT1)
      END IF
      ILINE=' '
      IF(.NOT.ADDED)GO TO 865
      WRITE(*,IOFORM)'File name "'//TFILE(1:NC)//'" now in User-'//
     1               'Group Library.'
      WRITE(ILINE(1:3),'(I3.3)')IFN
  865 CONTINUE
      IF(NREF)GO TO 9999
      IF(REVEF)GO TO 9990
      CLOSE(NDFIO)
      OPEN(NDFIO,FILE=TFILE(1:NC),ACCESS=DIR,FORM=FMTD,RECL=80,
     *     ERR=880,IOSTAT=IERR,STATUS='UNKNOWN')
      GO TO 9990
  870 WRITE(*,IOFORM)'Error erasing file "'//TFILE(1:NC)//'".'
      GO TO 890
  880 WRITE(*,IOFORM)'Error when accessing file "'//TFILE(1:NC)//'".'
  890 CONTINUE
      WRITE(*,'(A,I5)')' Error code (IOSTAT) = ',IERR
      WRITE(*,IOFORM)'This file cannot be used to save the '//
     1     CHADUM(1:NCC)//' data.'
      GO TO 500
 9990 CONTINUE
      NFILE=TFILE
 9999 CONTINUE
      RETURN
      END
C
C
C
      FUNCTION CCFF(NFILE,NLINE,IFN,NFIC,NIC)
C
C
C --- CODED     5-15-84 BY R. F. INMAN
C --- REVISED  12-20-84 BY R. F. INMAN
C
C --- TITLE - CHECK CATALOG FOR FILE
C
C --- FUNCTION - THIS LOGICAL FUNCTION CHECKS A CATALOG TO SEE IF
C ---            A SPECIFIED FILE NAME IS LISTED
C
C --- ARGUMENTS - CCFF = RETURNS LOGICAL .TRUE. IF FILE IS IN
C ---                    CATALOG, .FALSE. IF NOT
C ---             NFILE = CHARACTER VARIABLE WITH NAME OF FILE
C ---                     BE CHECKED
C ---             NLINE = CHARACTER VARIABLE FOR SCRATCH USE
C ---             IFN = RETURNS CATALOG NUMBER OF FILE,
C ---                   0(ZERO) IF FILE NOT IN CATALOG
C ---             NFIC = THE NUMBER OF FILES IN CATALOG
C ---             NIC = UNIT NUMBER THAT CALLER HAS OPENED WITH THE
C ---                   CATALOG FILE
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*) NFILE,NLINE
      INTEGER                   IFN,NFIC,NIC
      LOGICAL CCFF
      INTEGER I,ILNB,NC
      DO 100 I=1,NFIC
      NC=ILNB( NFILE )
      IFN=I+1
      READ(NIC,'(A)',REC=IFN)NLINE
      IF(NLINE.EQ.NFILE(1:NC))GO TO 120
  100 CONTINUE
C
C ----- FILE NOT FOUND IN CATALOG
C
      IFN=0
      CCFF=.FALSE.
      GO TO 9990
  120 CONTINUE
C
C -----FILE FOUND IN CATALOG
C
      IFN=IFN-1
      CCFF=.TRUE.
 9990 RETURN
      END
C
C
C
      FUNCTION DEGDVF(IFN,ILINE,NFILE,IFLAG,NOUT)
C
C
C --- CODED     5-18-84 BY R. F. INMAN
C --- REVISED   1-29-85 BY R. F. INMAN
C
C --- TITLE - DELETE EXISTING GEOMETRY & DRIVER-VEHICLE OUTPUT FILES
C
C --- FUNCTION - THIS LOGICAL FUNCTION CHECKS TO SEE IF GEOMETRY
C ---            AND/OR DRIVER-VEHICLE OUTPUT FILES CORRESPONDING
C ---            TO FILE CATALOGED AS NUMBER <IFN> EXIST.  IF SO
C ---            THESE FILES ARE DELETED, IF POSSIBLE.
C
C --- ARGUMENTS - IFN = CATALOG NUMBER OF FILE TO BE CHECKED
C ---                     OR, IF = 0 (ZERO) CHECK STANDARD FILE,
C ---                     STD FILE ID IS IN <ILINE(1:3)>
C ---             ILINE = CHARACTER VARIABLE FOR SCRATCH USE
C ---             NFILE = CHARACTER VARIABLE WITH NAME OF FILE TO
C ---                     TO BE ADDED TO CATALOG
C ---             IFLAG = FLAG FOR TYPE OF ERROR MESSAGE:
C ---                     0 - CATALOG NUMBER PROBLEM
C ---                     GT 0 - FILE NAME PROBLEM
C ---                     LT 0 - BYPASS MESSAGE PRINT
C ---             NOUT = DIMENSIONED VARIABLE WITH I/O UNIT NUMBERS
C ---                    NOUT(1) - TERMINAL
C ---                    NOUT(2)-1 - REFERENCE FOR SCRATCH UNIT
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  DEGDVF
      INTEGER         IFN            ,IFLAG,NOUT(*)
      CHARACTER*(*)       ILINE,NFILE
      CHARACTER*4 AAFMT
      PARAMETER  (AAFMT='(4A)')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'USFILE'
      CHARACTER FMT*9,IDSTD*3
      CHARACTER TEMP*6,TLINE*80
      LOGICAL   EXIST,STA
      INTEGER   ILNB,IT1,NC,NC1,NCFN
      DATA FMT/'(A,I2,3A)'/
      IOFORM='(A)'
      IDSTD=ILINE(1:3)
      IF(IDSTD(1:1).GE.'3'.AND.IDSTD(1:1).LE.'7'.AND.
     *   IDSTD(3:3).GE.'2'.AND.IDSTD(3:3).LE.'7')THEN
        IF(IDSTD(2:2).EQ.'T')                    IDSTD(2:2)='t'
        IF(IDSTD(2:2).EQ.'X')                    IDSTD(2:2)='x'
      END IF
      IF(IDSTD(3:3).GE.'1'.AND.IDSTD(3:3).LE.'3')THEN
        IF(IDSTD(1:2).EQ.'EX')                   IDSTD(1:2)='ex'
      END IF
C
C ----- PUT GPO FILE NAME IN <TEMP>
C
      IT1=NOUT(2)-2
      TEMP(1:3)='gpo'
      IF(IFN.GT.0)THEN
        WRITE(TEMP(4:6),'(I3.3)')IFN
      ELSE
        TEMP(4:6)=IDSTD
      END IF
      CALL TXINQ(0,TEMP(1:6),USFILE,ILINE,EXIST)
      IF(EXIST)THEN
C
C ----- GPO FILE EXISTS, DELETE IT
C
        OPEN(IT1,FILE=ILINE,ERR=100,STATUS='UNKNOWN')
        CALL TXDELF(IT1,ILINE,USFILE,NOUT,STA)
        IF(.NOT.STA)GO TO 100
      END IF
      TEMP(1:2)='dv'
      CALL TXINQ(0,TEMP,USFILE,ILINE,EXIST)
      IF(EXIST)THEN
C
C ----- DVO FILE EXISTS, DELETE IT
C
        NCFN=ILNB( ILINE )
        OPEN(IT1,FILE=ILINE(1:NCFN),ERR=100,STATUS='UNKNOWN')
        CALL TXDELF(IT1,ILINE(1:NCFN),USFILE,NOUT,STA)
        IF(.NOT.STA)GO TO 100
      END IF
      DEGDVF=.TRUE.
      GO TO 9990
  100 NC=ILNB( NFILE )
      IF(IFN.GT.0)GO TO 200
      WRITE(TLINE,AAFMT)'User-Group Library file "',NFILE(1:NC),
     1                  '" can''t be created.'
      NC1=NC+34
      GO TO 250
  200 IF(IFLAG.LT.0)GO TO 9990
      NC=ILNB( NFILE )
      IF(IFLAG.EQ.0)THEN
        WRITE(TLINE,AAFMT)'File "',NFILE(1:NC),'" can''t be ',
     1                    'used to save data.'
        NC1=NC+35
        WRITE(*,IOFORM)TLINE(1:NC1)
      ELSE
        IF(IFN.LT.10)THEN
          FMT(5:5)='2'
          NC1=NC+37
        ELSE
          FMT(5:5)='3'
          NC1=NC+38
        END IF
        WRITE(TLINE,FMT)'User-Group Library number',IFN,
     1                  ' can''t be used for "',NFILE(1:NC),'".'
        WRITE(*,IOFORM)TLINE(1:NC1)
      END IF
  250 WRITE(*,IOFORM)'A corresponding Geometry '//
     1               'Processor and/or Driver-Vehicle Processor ',
     2               'output file exists and cannot be deleted.'
      DEGDVF=.FALSE.
 9990 RETURN
      END
C
C
C
      FUNCTION AFTC(NFILE,IFN,NFIC,MAXFIC,NLINE,NIC)
C
C
C --- CODED     5-16-84 BY R. F. INMAN
C --- REVISED  02-20-86 BY R. F. INMAN
C
C --- TITLE - ADD FILE TO CATALOG
C
C --- FUNCTION - THIS LOGICAL FUNCTION ADDS A FILE NAME TO
C ---            A CATALOG OF FILE NAMES
C
C --- ARGUMENTS - AFTC = LOGICAL VARIABLE RETURNS .TRUE. IF FILE
C ---                    NAME IS SUCESSFULLY ADDED OR BLANKED,
C ---                    .FALSE. IF NOT
C ---             NFILE = CHARACTER VARIABLE WITH NAME OF FILE
C ---                     TO BE ADDED TO CATALOG
C ---             IFN = KEY TO HOW FILE NAME IS TO BE PROCESSED
C ---                   0 - PUT INTO FIRST AVAILABLE POSITION
C ---                   GT 0 - PUT IN AT POSITION <IFN>
C ---                   LT 0 - FILE NAMED IN <NFILE> IS BLANKED
C ---                          OUT, <IFN> IS SET TO THE NUMBER
C ---                          OF THE BLANKED FILE, OR 0 IF THE
C ---                          FILE NOT FOUND IN THE CATALOG
C ---             NFIC = NUMBER  OF FILES IN CATALOG
C ---             MAXFIC = MAXIMUM NUMBER OF FILES ALLOWED IN CATALOG
C ---             NLINE = CHARACTER VARIABLE FOR SCRATCH USE
C ---             NIC = UNIT NUMBER THAT CALLER HAS OPENED WITH
C ---                   FILE CONTAINING CATALOG
C
C --------------------------- DESCRIPTION -----------------------------
C                             -----------
C
C
C
C
C
C -----------------------THIS ROUTINE CALLED BY -----------------------
C                        ----------------------
C
C
C
C ------------------------ THIS ROUTINE CALLS -------------------------
C                          -------------------
C
C                                 ILNB
C
C --------------------- GLOSSARY OF VARIABLE NAMES --------------------
C                       --------------------------
C
C ---------------------------------------------------------------------
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  AFTC
      CHARACTER*(*) NFILE,                NLINE
      INTEGER             IFN,NFIC,MAXFIC      ,NIC
      CHARACTER*5 IIFORM
      CHARACTER*3                IOFORM
      CHARACTER*1                             NBLANK
      PARAMETER  (IIFORM='(2I5)',IOFORM='(A)',NBLANK=' ')
      INTEGER  I,ILNB,IT1,NC
      READ(NIC,IIFORM,REC=1,ERR=210)NFIC
      IF(IFN.LT.0)THEN
C
C ----- LOOK FOR FILE NAME TO BLANK OUT
C
        NC=ILNB( NFILE )
        IFN=0
        DO 60 I=1,NFIC
        READ(NIC,IOFORM,REC=I+1,ERR=210)NLINE(1:NC)
        IF(NLINE(1:NC).EQ.NFILE(1:NC))THEN
C
C ----- FOUND FILE TO BLANK
C
          WRITE(NIC,IOFORM,REC=I+1,ERR=210)NBLANK
          IFN=I
          GO TO 9900
        END IF
   60   CONTINUE
        GO TO 210
      END IF
      IF(IFN.EQ.0)THEN
C
C --- PUT FILE IN FIRST BLANK POSITION
C
        DO 100 I=1,NFIC
        READ(NIC,IOFORM,REC=I+1,ERR=210)NLINE
        IFN=I
        IF(NLINE.EQ.NBLANK)GO TO 120
  100   CONTINUE
        GO TO 160
      END IF
  120 CONTINUE
      IF(IFN.GT.0)THEN
C
C ----- PUT FILE INTO POSITION <IFN>
C
        IF(IFN.GT.MAXFIC)GO TO 210
        NC=ILNB( NFILE )
        IF(IFN.GT.NFIC)GO TO 160
        IT1=IFN+1
        WRITE(NIC,IOFORM,REC=IT1,ERR=210)NFILE(1:NC)
        GO TO 9900
      END IF
  160 CONTINUE
C
C ----- PUT FILE AFTER LAST FILE NOW IN CATALOG
C
      IF(NFIC.GE.MAXFIC)GO TO 210
      IT1=NFIC+2
      NFIC=NFIC+1
      WRITE(NIC,IOFORM,REC=IT1,ERR=200)NFILE(1:NC)
      WRITE(NIC,IIFORM,REC=1,ERR=200)NFIC,MAXFIC
      IFN=NFIC
      GO TO 9900
  200 NFIC=NFIC-1
  210 AFTC=.FALSE.
      IFN=0
      GO TO 9990
 9900 AFTC=.TRUE.
 9990 CONTINUE
      RETURN
      END
