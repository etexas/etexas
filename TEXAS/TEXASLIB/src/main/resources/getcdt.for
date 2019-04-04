      SUBROUTINE GETCDT ( DATTIM )
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
      CHARACTER*1       IBLNK1
      CHARACTER*10      CTIME
      CHARACTER*11      CDATE
      CHARACTER*20      DATTIM
C\    CHARACTER*10      IDATE,ITIME
C\    CHARACTER*10      DATE,TIME
C|    CHARACTER*8       ITIME
C|    CHARACTER*9       IDATE
C?    CHARACTER*8       ITIME
C?    CHARACTER*9       IDATE
      CHARACTER*3       MONTHS(12)                                      CCODE=C#
C}    CHARACTER*3       MONTHS(12)
C%    CHARACTER*3       MONTHS(12)
C@    INTEGER*2         IDAY,IDEC,IHRS,IMIN,IMTH,ISEC,IYRS
C~    CHARACTER*8       IDATE
C~    CHARACTER*11      ITIME
      CHARACTER*5       ZONE                                            CCODE=C{
C%    CHARACTER*5       ZONE
      INTEGER           VALUES(8)                                       CCODE=C{
      INTEGER                      IYRS ,           IMTH ,              CCODE=C{
     1                             IDAY ,           IHRS ,              CCODE=C{
     2                             IMIN ,           ISEC                CCODE=C{
      EQUIVALENCE       (VALUES(1),IYRS),(VALUES(2),IMTH),              CCODE=C{
     1                  (VALUES(3),IDAY),(VALUES(5),IHRS),              CCODE=C{
     2                  (VALUES(6),IMIN),(VALUES(7),ISEC)               CCODE=C{
C%    INTEGER           VALUES(8)
C%    INTEGER                      IYRS ,           IMTH ,
C%   1                             IDAY ,           IHRS ,
C%   2                             IMIN ,           ISEC
C%    EQUIVALENCE       (VALUES(1),IYRS),(VALUES(2),IMTH),
C%   1                  (VALUES(3),IDAY),(VALUES(5),IHRS),
C%   2                  (VALUES(6),IMIN),(VALUES(7),ISEC)
      DATA  IBLNK1  / ' ' /
      DATA  MONTHS  / 'JAN','FEB','MAR','APR','MAY','JUN',              CCODE=C#
     *                'JUL','AUG','SEP','OCT','NOV','DEC' /             CCODE=C#
C}    DATA  MONTHS  / 'JAN','FEB','MAR','APR','MAY','JUN',
C}   *                'JUL','AUG','SEP','OCT','NOV','DEC' /
C%    DATA  MONTHS  / 'JAN','FEB','MAR','APR','MAY','JUN',
C%   *                'JUL','AUG','SEP','OCT','NOV','DEC' /
C@101 FORMAT(I2,'-',A3,'-',I4)
  101 FORMAT(I2,'-',A3,'-',I4)                                          CCODE=C{
C%101 FORMAT(I2,'-',A3,'-',I4)
C@102 FORMAT(I2,':',I2,':',I2)
  102 FORMAT(I2,':',I2,':',I2)                                          CCODE=C{
C%102 FORMAT(I2,':',I2,':',I2)
C~103 FORMAT(I2)
C
C-----GET CURRENT DATE AND TIME AS "DD-MMM-YYYY HH:MM:SS"
C
C\    IDATE = DATE()
C\    ITIME = TIME()
C\    IF ( IDATE( 9: 9) . GE . '9' )             THEN
C\          CDATE = IDATE( 2: 3)//'-'//IDATE( 5: 7)//'-19'//IDATE( 9:10)
C\    ELSE
C\          CDATE = IDATE( 2: 3)//'-'//IDATE( 5: 7)//'-20'//IDATE( 9:10)
C\    END IF
C\    CTIME = ITIME( 2: 3)//':'//ITIME( 5: 6)//':'//ITIME( 8: 9)
C|    CALL  DATE    ( IDATE )
C|    CALL  TIME    ( ITIME )
C|    IF ( IDATE( 8: 8) . GE . '9' )             THEN
C|          CDATE = IDATE( 1: 7)//'19'//IDATE( 8: 9)
C|    ELSE
C|          CDATE = IDATE( 1: 7)//'20'//IDATE( 8: 9)
C|    END IF
C|    CTIME = ITIME
C?    CALL  DATE    ( IDATE )
C?    CALL  TIME    ( ITIME )
C?    IF ( IDATE( 8: 8) . GE . '9' )             THEN
C?          CDATE = IDATE( 1: 7)//'19'//IDATE( 8: 9)
C?    ELSE
C?          CDATE = IDATE( 1: 7)//'20'//IDATE( 8: 9)
C?    END IF
C?    CTIME = ITIME
C@    CALL  GETDAT  ( IYRS,IMTH,IDAY )
C@    CALL  GETTIM  ( IHRS,IMIN,ISEC,IDEC )
C@    WRITE (CDATE,101) IDAY,MONTHS(IMTH),IYRS
C@    WRITE (CTIME,102) IHRS,IMIN,ISEC
C~    CALL  DATE    ( IDATE )
C~    CALL  TIME    ( ITIME )
C~    READ (IDATE( 1: 2),103) IMTH
C~    IF ( IDATE( 7: 7) . GE . '9' )             THEN
C~          CDATE = IDATE( 4: 5)//'-'//MONTHS(IMTH)//'-19'//IDATE( 7: 8)
C~    ELSE
C~          CDATE = IDATE( 4: 5)//'-'//MONTHS(IMTH)//'-20'//IDATE( 7: 8)
C~    END IF
C~    CTIME = ITIME( 1: 8)
      CALL  DATE_AND_TIME ( CDATE,CTIME,ZONE,VALUES )                   CCODE=C{
      WRITE (CDATE,101) IDAY,MONTHS(IMTH),IYRS                          CCODE=C{
      WRITE (CTIME,102) IHRS,IMIN,ISEC                                  CCODE=C{
C%    CALL  DATE_AND_TIME ( CDATE,CTIME,ZONE,VALUES )
C%    WRITE (CDATE,101) IDAY,MONTHS(IMTH),IYRS
C%    WRITE (CTIME,102) IHRS,IMIN,ISEC
      DATTIM = CDATE( 1:11)//' '//CTIME( 1: 8)
            IF ( DATTIM( 1: 1) . EQ . IBLNK1 )   DATTIM( 1: 1) = '0'
            IF ( DATTIM(13:13) . EQ . IBLNK1 )   DATTIM(13:13) = '0'
            IF ( DATTIM(16:16) . EQ . IBLNK1 )   DATTIM(16:16) = '0'
            IF ( DATTIM(19:19) . EQ . IBLNK1 )   DATTIM(19:19) = '0'
      RETURN
      END                                                               GETCDT
