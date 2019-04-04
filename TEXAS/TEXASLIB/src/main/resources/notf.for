      FUNCTION NOTF(IUNIT,ITEXT,FT,FILE)
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
C --- CODED     5-08-84 BY R. F. INMAN
C --- REVISED
C
C --- TITLE - NAME OUT TO FILE
C
C --- FUNCTION - THIS LOGICAL FUNCTION CHECKS IF A DATA FILE EXISTS.
C ---            IF SO, IT OPENS ANOTHER FILE (WITH THE DEFAULT NAME
C ---            FOR UNIT <IUNIT>), WRITES A CODE AND THE DATA
C ---            FILE NAME INTO THIS FILE.  THIS CODE AND NAME
C ----           POINT A LATER PROCESSOR TO THE DATA FILE.
C ----           A MESSAGE TO THE TERMINAL TELLS THE USER OF
C ----           SUCCESS.
C ---
C --- ARGUMENTS - NOTF = RETURNS .TRUE. IF FILE FOUND, OTHERWISE .FALSE.
C ---             IUNIT = UNIT NUMBER USED TO OPEN FILE
C ---                     FILE NAME IS SYSTEM DEFAULT FOR THIS
C ---                     UNIT NUMBER (FOR021,TAPE21,....)
C ---             ITEXT = TEXT TO BE USED IN REPORTING COMPLETION
C ---             FT = CODE TO INDICATE TYPE OF FILE BEING
C ---                  POINTED TO
C ---                  "GPO" - GEOMETRY PROCESSOR OUTPUT
C ---                  "DVO" - DRIVER-VEHICLE PROCESSOR OUTPUT
C ---                  "GDV" - G&D-V INPUT DATA
C ---                  "SIM" - SIMULATION PROCESSOR INPUT DATA
C ---             FILE = DATA FILE NAME
C
C ------------------------------ DESCRIPTION --------------------------
C                                -----------
C
C
C ------------------------- THIS ROUTINE CALLED BY --------------------
C                           ----------------------
C
C                                  STDINT
C                                  TXGDVD
C
C --------------------------- THIS ROUTINE CALLS ----------------------
C                             ------------------
C
C                                   ILNB
C
C --------------------- GLOSSARY OF VARIABLE NAMES --------------------
C                       --------------------------
C
C    NC - NUMBER OF NONBLANK CHARACTERS IN <FILE>
C    NC1- NUMBER OF NONBLANK CHARACTERS IN <ITEXT>
C
C ---------------------------------------------------------------------
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  NOTF
      INTEGER       IUNIT
      CHARACTER*(*)       ITEXT,FT,FILE
      CHARACTER*4  AAFORM
      CHARACTER*8  FTU
      CHARACTER*60 DEFILE,DEFFN
      LOGICAL TOTF
      INTEGER ILNB,NC,NC1
      DATA AAFORM/'(4A)'/
      NC=ILNB( FILE )
      INQUIRE(FILE=FILE(1:NC),EXIST=TOTF)
      NOTF=TOTF
      IF(NOTF)THEN
        DEFILE=DEFFN(IUNIT)
        OPEN (IUNIT,FILE=DEFILE,RECL=80,STATUS='UNKNOWN',
     1        FORM='FORMATTED',ACCESS='DIRECT')
        FTU=FT
        CALL  TOUPR   ( FTU )
        WRITE (IUNIT, '(2A,T19,2A)', REC=1) ' #%&$FILE',
     1        FTU(1:LEN( FT )),'=',FILE(1:NC)
        CLOSE (IUNIT)
        NC1 = ILNB( ITEXT )
        IF((NC1+NC) .LT. 66)THEN
C
C ----- SUCCESS MESSAGE ON SINGLE LINE
C
          WRITE (*, AAFORM) ITEXT , ' are now on "' , FILE(1:NC) , '"'
        ELSE
C
C ----- SUCCESS MESSAGE ON TWO LINES
C
          WRITE(*,AAFORM) ITEXT , ' are now on:'
          WRITE(*,AAFORM) '    "' , FILE(1:NC) , '"'
        END IF
      END IF
      RETURN
      END
