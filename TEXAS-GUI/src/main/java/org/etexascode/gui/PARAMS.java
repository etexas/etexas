package org.etexascode.gui;

/******************************************************************************/
/*                                 PARAMS.java                                */
/******************************************************************************/
/*                                                                            */
/*     PARAMS COPYRIGHT (C) 2008 by Rioux Engineering, Austin, Texas USA      */
/*                                                                            */
/*   Permission is hereby granted to use, modify, copy, and distribute this   */
/*   software and its documentation for any purpose only without profit,      */
/*   provided that the above Copyright Notice appears in all copies and that  */
/*   both the Copyright Notice and this Permission Notice appears in every    */
/*   copy of supporting documentation.  No title to nor ownership of the      */
/*   software is transferred hereby.  The name of Rioux Engineering shall not */
/*   be used in advertising or publicity related to the distribution of the   */
/*   software without specific, written, prior permission.  This software is  */
/*   provided as-delivered without expressed or implied warranty.  Rioux      */
/*   Engineering makes no representation about the suitability of this        */
/*   software for any purpose and accepts no responsibility for its use.      */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*   This program is free software; you can redistribute it and/or modify     */
/*   it under the terms of the GNU General Public License as published by     */
/*   the Free Software Foundation; either version 2 of the License, or        */
/*   (at your option) any later version.                                      */
/*                                                                            */
/*   This program is distributed in the hope that it will be useful,          */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*   GNU General Public License for more details.                             */
/*                                                                            */
/*   You should have received a copy of the GNU General Public License        */
/*   along with this program; if not, write to the Free Software              */
/*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  */
/*                                                                            */
/******************************************************************************/

/*********************/
/* import statements */
/*********************/

class PARAMS /* PARAMS Class */
{

    /************************/
    /* constant definitions */
    /************************/

    public static final double TEXAS_MODEL_DECCOL = 966.0; /*
                                                            * TEXAS Model VMS collision deceleration
                                                            * maximum
                                                            */

    public static final int TEXAS_MODEL_DIA = 7; /*
                                                  * TEXAS Model maximum number of Texas diamond
                                                  * controller figures
                                                  */

    public static final double TEXAS_MODEL_DISCON = 200.0; /*
                                                            * TEXAS Model distance to start reducing
                                                            * desired speed for collision
                                                            */

    public static final int TEXAS_MODEL_EMPROY = 1975; /*
                                                        * TEXAS Model EMPRO minimum model year
                                                        */

    public static final double TEXAS_MODEL_EVEHFZ = 15.0; /*
                                                           * TEXAS Model emergency vehicle front
                                                           * zone in seconds
                                                           */

    public static final double TEXAS_MODEL_EVEHRZ = 7.5; /*
                                                          * TEXAS Model emergency vehicle rear zone
                                                          * in seconds
                                                          */

    public static final double TEXAS_MODEL_HDTMAX = 0.10; /*
                                                           * TEXAS Model hardware-in-the-loop DT
                                                           * maximum
                                                           */

    public static final double TEXAS_MODEL_HDTMIN = 0.02; /*
                                                           * TEXAS Model hardware-in-the-loop DT
                                                           * minimum
                                                           */

    public static final int TEXAS_MODEL_HOV = 4; /*
                                                  * TEXAS Model number of overlaps for
                                                  * hardware-in-the-loop signal
                                                  */

    public static final int TEXAS_MODEL_HPH = 8; /*
                                                  * TEXAS Model number of phases for
                                                  * hardware-in-the-loop signal
                                                  */

    public static final double TEXAS_MODEL_IACC00 = 0.0000001; /*
                                                                * TEXAS Model acceleration
                                                                * /deceleration zero value
                                                                */

    public static final double TEXAS_MODEL_IACCFR = 20000000.0; /*
                                                                 * TEXAS Model acceleration
                                                                 * /deceleration factor
                                                                 */

    public static final double TEXAS_MODEL_IACCSH = 40.0; /*
                                                           * TEXAS Model acceleration /deceleration
                                                           * shift
                                                           */

    public static final String TEXAS_MODEL_ICBL = " "; /*
                                                        * TEXAS Model 1 blank character
                                                        */

    public static final double TEXAS_MODEL_IPOSFR = 400000.0; /*
                                                               * TEXAS Model position factor
                                                               */

    public static final double TEXAS_MODEL_ISLP00 = 0.0000001; /*
                                                                * TEXAS Model slope zero value
                                                                */

    public static final double TEXAS_MODEL_ISLPFR = 50000000.0; /*
                                                                 * TEXAS Model slope factor
                                                                 */

    public static final double TEXAS_MODEL_ISLPSH = 15.0; /*
                                                           * TEXAS Model slope shift
                                                           */

    public static final double TEXAS_MODEL_IVELFR = 10000000.0; /*
                                                                 * TEXAS Model velocity factor
                                                                 */

    public static final double TEXAS_MODEL_LATCLR = 3.0; /*
                                                          * TEXAS MOdel lateral clearance for a lane
                                                          * change
                                                          */

    public static final double TEXAS_MODEL_LATPFR = 1000.0; /*
                                                             * TEXAS Model lane change factor
                                                             */

    public static final double TEXAS_MODEL_LATPSH = 15.0; /*
                                                           * TEXAS Model lane change shift
                                                           */

    public static final double TEXAS_MODEL_LCSPER = 0.95; /*
                                                           * TEXAS Model percent of desired speed to
                                                           * check slow vehicle lane
                                                           */

    public static final double TEXAS_MODEL_LCSTIM = 15.0; /*
                                                           * TEXAS Model time from stop line to
                                                           * check slow vehicle lane change
                                                           */

    public static final int TEXAS_MODEL_LDC = 13; /*
                                                   * TEXAS Model number of classify detector vehicle
                                                   * classes
                                                   */

    public static final int TEXAS_MODEL_LNCHMX = 10; /*
                                                      * TEXAS Model maximum number of lane changes
                                                      * per approach
                                                      */

    public static final double TEXAS_MODEL_LPGAP = 0.0; /*
                                                         * TEXAS Model allowance for possible lane
                                                         * path gap
                                                         */

    public static final int TEXAS_MODEL_LWBMAX = 7; /*
                                                     * TEXAS Model lane width for bicycles maximum
                                                     * (feet)
                                                     */

    public static final int TEXAS_MODEL_LWBMIN = 3; /*
                                                     * TEXAS Model lane width for bicycles minimum
                                                     * (feet)
                                                     */

    public static final int TEXAS_MODEL_LWVMAX = 15; /*
                                                      * TEXAS Model lane width for vehicles maximum
                                                      * (feet)
                                                      */

    public static final int TEXAS_MODEL_LWVMIN = 8; /*
                                                     * TEXAS Model lane width for vehicles minimum
                                                     * (feet)
                                                     */

    public static final int TEXAS_MODEL_MAXFIL = 60; /*
                                                      * TEXAS Model maximum length of gdvdata and
                                                      * simdata full file name
                                                      */

    public static final int TEXAS_MODEL_MAXHIG = 14; /*
                                                      * TEXAS Model maximum vehicle height (feet)
                                                      */

    public static final int TEXAS_MODEL_MAXLEN = 400; /*
                                                       * TEXAS Model maximum vehicle length (feet)
                                                       */

    public static final int TEXAS_MODEL_MAXTOL = 50; /*
                                                      * TEXAS Model maximum tolerance for reptol
                                                      */

    public static final int TEXAS_MODEL_MAXWID = 12; /*
                                                      * TEXAS Model maximum vehicle width (feet)
                                                      */

    public static final int TEXAS_MODEL_MDS = 161; /*
                                                    * TEXAS Model maximum desired speed (feet per
                                                    * second)
                                                    */

    public static final int TEXAS_MODEL_MINHIG = 3; /*
                                                     * TEXAS Model minimum vehicle height (feet)
                                                     */

    public static final int TEXAS_MODEL_MINLEN = 4; /*
                                                     * TEXAS Model minimum vehicle length (feet)
                                                     */

    public static final int TEXAS_MODEL_MINNPH = 2; /*
                                                     * TEXAS Model minimum number of signal phases
                                                     */

    public static final int TEXAS_MODEL_MINPCL = 40; /*
                                                      * TEXAS Model minimum pretimed cycle length
                                                      * (seconds)
                                                      */

    public static final int TEXAS_MODEL_MINWID = 2; /*
                                                     * TEXAS Model minimum vehicle width (feet)
                                                     */

    public static final int TEXAS_MODEL_MLP = 5; /*
                                                  * TEXAS Model number of links present and previous
                                                  */

    public static final int TEXAS_MODEL_MNU = 8; /*
                                                  * TEXAS Model maximum number of units for a
                                                  * vehicle
                                                  */

    public static final int TEXAS_MODEL_MSTMIN = 166; /*
                                                       * TEXAS Model maximum simulation time
                                                       * (minutes) (9960.00 seconds)
                                                       */

    public static final int TEXAS_MODEL_MTABR = 65; /*
                                                     * TEXAS Model number of rows in summary
                                                     * statistics table
                                                     */

    public static final int TEXAS_MODEL_MUL = 100; /*
                                                    * TEXAS Model maximum unit length (feet)
                                                    */

    public static final int TEXAS_MODEL_MVL = 4; /*
                                                  * TEXAS Model maximum vehicles concurrently
                                                  * occupying a loop
                                                  */

    public static final int TEXAS_MODEL_MVMCOL = 9; /*
                                                     * TEXAS Model number of vehicles involved in a
                                                     * major collision
                                                     */

    public static final int TEXAS_MODEL_MXPVOL = 4000; /*
                                                        * TEXAS Model maximum pedestrian volume per
                                                        * phase
                                                        */

    public static final int TEXAS_MODEL_MXTVOL = 8000; /*
                                                        * TEXAS Model maximum traffic volume per leg
                                                        */

    public static final int TEXAS_MODEL_NAL = 6; /*
                                                  * TEXAS Model number of lanes per approach
                                                  */

    public static final int TEXAS_MODEL_NAR = 20; /* TEXAS Model number of arcs */

    public static final int TEXAS_MODEL_NAS = 4; /*
                                                  * TEXAS Model number of sight distance
                                                  * restrictions per approach
                                                  */

    public static final int TEXAS_MODEL_NBU = 20; /*
                                                   * TEXAS Model number of buckets for empro
                                                   */

    public static final int TEXAS_MODEL_NCO = 999; /*
                                                    * TEXAS Model number of intersection conflicts
                                                    */

    public static final int TEXAS_MODEL_NCP = 99; /*
                                                   * TEXAS Model number of intersection conflicts
                                                   * per intersection path
                                                   */

    public static final int TEXAS_MODEL_NDC = 9; /*
                                                  * TEXAS Model number of driver classes
                                                  */

    public static final int TEXAS_MODEL_NDCD = 3; /*
                                                   * TEXAS Model number of driver classes for
                                                   * default (fixed characteristics)
                                                   */

    public static final int TEXAS_MODEL_NFU = 2; /*
                                                  * TEXAS Model number of free u-turns at a diamond
                                                  * interchange
                                                  */

    public static final int TEXAS_MODEL_NGR = 4; /*
                                                  * TEXAS Model number of groups in NEMA actuated
                                                  * signal
                                                  */

    public static final int TEXAS_MODEL_NIA = 8; /*
                                                  * TEXAS Model number of inbound approaches
                                                  */

    public static final int TEXAS_MODEL_NIL = 25; /*
                                                   * TEXAS Model number of inbound lanes
                                                   */

    public static final int TEXAS_MODEL_NLG = 6; /* TEXAS Model number of legs */

    public static final int TEXAS_MODEL_NLI = 20; /* TEXAS Model number of lines */

    public static final int TEXAS_MODEL_NLO = 6; /*
                                                  * TEXAS Model number of loop detectors per lane
                                                  */

    public static final int TEXAS_MODEL_NLP = 9; /*
                                                  * TEXAS Model number of intersection paths per
                                                  * lane
                                                  */

    public static final int TEXAS_MODEL_NLS = 30; /*
                                                   * TEXAS Model number of loop detectors total for
                                                   * the intersection
                                                   */

    public static final int TEXAS_MODEL_NMP = 8; /*
                                                  * TEXAS Model number of NEMA/HARDWARE movements
                                                  * per phase
                                                  */

    public static final int TEXAS_MODEL_NND = 37; /*
                                                   * TEXAS Model number of notes for dvpro
                                                   */

    public static final int TEXAS_MODEL_NON = 16; /*
                                                   * TEXAS Model number of overlaps for NEMA signal
                                                   */

    public static final int TEXAS_MODEL_NOP = 12; /*
                                                   * TEXAS Model number of options for Texas diamond
                                                   * signal
                                                   */

    public static final int TEXAS_MODEL_NOT = 2; /*
                                                  * TEXAS Model number of overlaps for Texas diamond
                                                  * signal
                                                  */

    public static final int TEXAS_MODEL_NOV = 4; /*
                                                  * TEXAS Model number of overlaps for
                                                  * pretimed/semi-full -act/tex-dia
                                                  */

    public static final int TEXAS_MODEL_NPA = 99; /*
                                                   * TEXAS Model number of intersection paths
                                                   */

    public static final int TEXAS_MODEL_NPC = 9; /*
                                                  * TEXAS Model number of phase combinations for
                                                  * Texas diamond signal
                                                  */

    public static final int TEXAS_MODEL_NPH = 8; /*
                                                  * TEXAS Model number of phases for
                                                  * pretimed/semi-act/full-act signal
                                                  */

    public static final int TEXAS_MODEL_NPL = 10; /*
                                                   * TEXAS Model number of loop detectors per signal
                                                   * phase
                                                   */

    public static final int TEXAS_MODEL_NPN = 16; /*
                                                   * TEXAS Model number of phases for NEMA signal
                                                   */

    public static final int TEXAS_MODEL_NRC = 2; /*
                                                  * TEXAS Model number of rings for controller
                                                  * interface device
                                                  */

    public static final int TEXAS_MODEL_NRG = 4; /*
                                                  * TEXAS Model number of rings for NEMA signal
                                                  */

    public static final int TEXAS_MODEL_NRGP = 16; /*
                                                    * TEXAS Model number of phases in a ring-group
                                                    * for NEMA signal
                                                    */

    public static final int TEXAS_MODEL_NRP = 1000; /*
                                                     * TEXAS Model number of replicate runs
                                                     */

    public static final double TEXAS_MODEL_NRZERO = 0.005; /*
                                                            * TEXAS Model near zero
                                                            */

    public static final int TEXAS_MODEL_NSR = 10; /*
                                                   * TEXAS Model number of sight distance
                                                   * restriction points
                                                   */

    public static final int TEXAS_MODEL_NSS = 160; /*
                                                    * TEXAS Model number of 25-ft sections for sight
                                                    * distance restriction
                                                    */

    public static final int TEXAS_MODEL_NSV = 50; /*
                                                   * TEXAS Model number of special vehicles
                                                   */

    public static final int TEXAS_MODEL_NTC = 6; /*
                                                  * TEXAS Model number of turn codes
                                                  */

    public static final int TEXAS_MODEL_NTM = 12; /*
                                                   * TEXAS Model number of timers for Texas diamond
                                                   * signal
                                                   */

    public static final int TEXAS_MODEL_NVC = 99; /*
                                                   * TEXAS Model number of vehicle classes
                                                   */

    public static final int TEXAS_MODEL_NVCD = 12; /*
                                                    * TEXAS Model number of vehicle classes for
                                                    * default (fixed characteristics)
                                                    */

    public static final int TEXAS_MODEL_NVE = 500; /*
                                                    * TEXAS Model number of vehicles in the system
                                                    * at one time
                                                    */

    public static final int TEXAS_MODEL_NVMSMM = 100; /*
                                                       * TEXAS Model number of VMS messages maximum
                                                       */

    public static final int TEXAS_MODEL_NVMSMV = 11; /*
                                                      * TEXAS Model number of VMS messages for
                                                      * vehicle
                                                      */

    public static final int TEXAS_MODEL_NVP = 15000; /*
                                                      * TEXAS Model number of vehicles generated per
                                                      * approach
                                                      */

    public static final int TEXAS_MODEL_NVT = 12; /*
                                                   * TEXAS Model number of varying traffic periods
                                                   * per approach
                                                   */

    public static final int TEXAS_MODEL_PARNDF = 4; /*
                                                     * TEXAS Model number of command line parameters
                                                     * for disfit
                                                     */

    public static final int TEXAS_MODEL_PARNDP = 8; /*
                                                     * TEXAS Model number of command line parameters
                                                     * for dispre
                                                     */

    public static final int TEXAS_MODEL_PARNDV = 13; /*
                                                      * TEXAS Model number of command line
                                                      * parameters for dvpro
                                                      */

    public static final int TEXAS_MODEL_PARNEP = 11; /*
                                                      * TEXAS Model number of command line
                                                      * parameters for empro
                                                      */

    public static final int TEXAS_MODEL_PARNGP = 13; /*
                                                      * TEXAS Model number of command line
                                                      * parameters for geopro
                                                      */

    public static final int TEXAS_MODEL_PARNSP = 12; /*
                                                      * TEXAS Model number of command line
                                                      * parameters for simpro
                                                      */

    public static final int TEXAS_MODEL_PARNSS = 7; /*
                                                     * TEXAS Model number of command line parameters
                                                     * for simsta
                                                     */

    public static final int TEXAS_MODEL_PARNSV = 5; /*
                                                     * TEXAS Model number of command line parameters
                                                     * for simconv
                                                     */

    public static final int TEXAS_MODEL_POSMAX = 4000; /*
                                                        * TEXAS Model maximum position in feet
                                                        */

    public static final double TEXAS_MODEL_POSRMX = -10.0; /*
                                                            * TEXAS Model POSREL maximum for
                                                            * defining a major collision
                                                            */

    public static final double TEXAS_MODEL_PSMALL = 0.2; /*
                                                          * TEXAS Model small position
                                                          */

    public static final double TEXAS_MODEL_SAFCON = 10.0; /*
                                                           * TEXAS Model factor for SAF for
                                                           * collision
                                                           */

    public static final double TEXAS_MODEL_SAFDIS = 1.75; /*
                                                           * TEXAS Model distance for SAF for
                                                           * collision
                                                           */

    public static final double TEXAS_MODEL_SAFSPD = 44.0; /*
                                                           * TEXAS Model speed for SAF for collision
                                                           */

    public static final double TEXAS_MODEL_SLPCOL = 9660.0; /*
                                                             * TEXAS Model slope for collisions in
                                                             * ft/sec/sec/sec
                                                             */

    public static final double TEXAS_MODEL_TIMELC = 3.5; /*
                                                          * TEXAS Model time to lane change in
                                                          * seconds
                                                          */

    public static final double TEXAS_MODEL_TIMERR = 1.0e99; /*
                                                             * TEXAS Model time error
                                                             */

    public static final double TEXAS_MODEL_TIMOIP = 300.0; /*
                                                            * TEXAS Model time to stop checking
                                                            * other intersection paths (secs)
                                                            */

    public static final double TEXAS_MODEL_VELMAX = 120.0; /*
                                                            * TEXAS Model maximum velocity in mph
                                                            */

    public static final double TEXAS_MODEL_VELRMX = 18.0; /*
                                                           * TEXAS Model VELREL maximum for defining
                                                           * a major collision
                                                           */

    public static final double TEXAS_MODEL_VELSTP = 0.1; /*
                                                          * TEXAS Model velocity considered stopped
                                                          */

    public static final double TEXAS_MODEL_VSMALL = 1.0; /*
                                                          * TEXAS Model small velocity
                                                          */

    public static final int TEXAS_MODEL_XYCNTR = 5000; /*
                                                        * TEXAS Model intersection x/y center
                                                        * coordinate location
                                                        */

    /*********************************************/
    /* constants calculated from other constants */
    /*********************************************/

    public static final int TEXAS_MODEL_MSTSEC = TEXAS_MODEL_MSTMIN * 60; /*
                                                                           * TEXAS Model maximum
                                                                           * simulation time (
                                                                           * seconds ) (166 minutes
                                                                           * )
                                                                           */

    public static final int TEXAS_MODEL_NGI = TEXAS_MODEL_NPN + TEXAS_MODEL_NON; /*
                                                                                  * TEXAS Model
                                                                                  * number of green
                                                                                  * interval
                                                                                  * sequence items
                                                                                  */

    public static final int TEXAS_MODEL_NIS = TEXAS_MODEL_NIA + 2; /*
                                                                    * TEXAS Model number of
                                                                    * approaches for statistics
                                                                    */

    public static final int TEXAS_MODEL_NLGP1 = TEXAS_MODEL_NLG + 1; /*
                                                                      * TEXAS Model number of legs
                                                                      * plus 1
                                                                      */

    public static final int TEXAS_MODEL_NLGP2 = TEXAS_MODEL_NLG + 2; /*
                                                                      * TEXAS Model number of legs
                                                                      * plus 2
                                                                      */

    public static final int TEXAS_MODEL_NOA = TEXAS_MODEL_NIA; /*
                                                                * TEXAS Model number of outbound
                                                                * approaches
                                                                */

    public static final int TEXAS_MODEL_NOL = TEXAS_MODEL_NIL; /*
                                                                * TEXAS Model number of outbound
                                                                * lanes
                                                                */

    public static final int TEXAS_MODEL_NTS = TEXAS_MODEL_NTC + 1; /*
                                                                    * TEXAS Model number of turn
                                                                    * codes for statistics
                                                                    */

    public static final int TEXAS_MODEL_NVEP1 = TEXAS_MODEL_NVE + 1; /*
                                                                      * TEXAS Model number of
                                                                      * vehicles in the system at
                                                                      * one time plus 1
                                                                      */

    public static final double TEXAS_MODEL_POSBIG = TEXAS_MODEL_POSMAX * 4.0; /*
                                                                               * TEXAS Model
                                                                               * position that will
                                                                               * never be possible
                                                                               */

    /********************************************************/
    /* constants calculated from other calculated constants */
    /********************************************************/

    public static final int TEXAS_MODEL_NAP = TEXAS_MODEL_NIA + TEXAS_MODEL_NOA; /*
                                                                                  * TEXAS Model
                                                                                  * number of
                                                                                  * approaches
                                                                                  */

    public static final int TEXAS_MODEL_NLA = TEXAS_MODEL_NIL + TEXAS_MODEL_NOL; /*
                                                                                  * TEXAS Model
                                                                                  * number of lanes
                                                                                  */

    public static final int TEXAS_MODEL_NCM = Math.max(72, TEXAS_MODEL_NGI * 2); /*
                                                                                  * TEXAS Model
                                                                                  * number of cam
                                                                                  * stack entries
                                                                                  */

    /************************/
    /* controller intervals */
    /************************/

    public static final int TEXAS_MODEL_INTERG = 1; /*
                                                     * TEXAS Model controller interval green
                                                     */

    public static final int TEXAS_MODEL_INTERY = 2; /*
                                                     * TEXAS Model controller interval yellow
                                                     */

    public static final int TEXAS_MODEL_INTERR = 3; /*
                                                     * TEXAS Model controller interval red clearance
                                                     * or red
                                                     */

    /*************************/
    /* VMS system parameters */
    /*************************/

    public static final double TEXAS_MODEL_VMSPOS = 10.0; /*
                                                           * TEXAS Model position behind front
                                                           * bumper that VMS messages can be
                                                           * detected
                                                           */

    public static final int TEXAS_MODEL_VMSTDD = 1; /*
                                                     * TEXAS Model VMS message type - driver DMS
                                                     */

    public static final int TEXAS_MODEL_VMSTDI = 2; /*
                                                     * TEXAS Model VMS message type - driver IVDMS
                                                     */

    public static final int TEXAS_MODEL_VMSTVI = 3; /*
                                                     * TEXAS Model VMS message type - vehicle IVDMS
                                                     */

    public static final int TEXAS_MODEL_VMSMAN = 1; /*
                                                     * TEXAS Model VMS message - accelerate or
                                                     * decelerate to speed xx using normal
                                                     * acceleration or deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSMAM = 2; /*
                                                     * TEXAS Model VMS message - accelerate or
                                                     * decelerate to speed xx using maximum vehicle
                                                     * acceleration or deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSMSI = 3; /*
                                                     * TEXAS Model VMS message - stop at the
                                                     * intersection stop line
                                                     */

    public static final int TEXAS_MODEL_VMSMSL = 4; /*
                                                     * TEXAS Model VMS message - stop at location xx
                                                     */

    public static final int TEXAS_MODEL_VMSMSM = 5; /*
                                                     * TEXAS Model VMS message - stop immediately
                                                     * using maximum vehicle deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSMSC = 6; /*
                                                     * TEXAS Model VMS message - stop immediately
                                                     * using collision deceleration xx
                                                     */

    public static final int TEXAS_MODEL_VMSMCL = 7; /*
                                                     * TEXAS Model VMS message - change lanes to the
                                                     * left
                                                     */

    public static final int TEXAS_MODEL_VMSMCR = 8; /*
                                                     * TEXAS Model VMS message - change lanes to the
                                                     * right
                                                     */

    public static final int TEXAS_MODEL_VMSMGO = 9; /*
                                                     * TEXAS Model VMS message - forced go
                                                     */

    public static final int TEXAS_MODEL_VMSMRR = 10; /*
                                                      * TEXAS Model VMS message - forced run the red
                                                      * signal
                                                      */

    public static final int TEXAS_MODEL_VMSMDD = 11; /*
                                                      * TEXAS Model VMS message - distracted driver
                                                      */

    public static final int TEXAS_MODEL_VMSPSC = 1; /*
                                                     * TEXAS Model VMS priority for message - stop
                                                     * immediately using collision deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSPDD = 2; /*
                                                     * TEXAS Model VMS priority for message -
                                                     * distracted driver
                                                     */

    public static final int TEXAS_MODEL_VMSPSM = 3; /*
                                                     * TEXAS Model VMS priority for message - stop
                                                     * immediately using maximum vehicle
                                                     * deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSPSL = 4; /*
                                                     * TEXAS Model VMS priority for message - stop
                                                     * at location xx
                                                     */

    public static final int TEXAS_MODEL_VMSPSI = 5; /*
                                                     * TEXAS Model VMS priority for message - stop
                                                     * at the intersection stop line
                                                     */

    public static final int TEXAS_MODEL_VMSPRR = 6; /*
                                                     * TEXAS Model VMS priority for message - forced
                                                     * run the red signal
                                                     */

    public static final int TEXAS_MODEL_VMSPGO = 7; /*
                                                     * TEXAS Model VMS priority for message - forced
                                                     * go
                                                     */

    public static final int TEXAS_MODEL_VMSPAM = 8; /*
                                                     * TEXAS Model VMS priority for message -
                                                     * accelerate or decelerate to speed xx using
                                                     * maximum vehicle acceleration or deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSPAN = 9; /*
                                                     * TEXAS Model VMS priority for message -
                                                     * accelerate or decelerate to speed xx using
                                                     * normal acceleration or deceleration
                                                     */

    public static final int TEXAS_MODEL_VMSPCL = 10; /*
                                                      * TEXAS Model VMS priority for message -
                                                      * change lanes to the left
                                                      */

    public static final int TEXAS_MODEL_VMSPCR = 11; /*
                                                      * TEXAS Model VMS priority for message -
                                                      * change lanes to the right
                                                      */

    public static final int TEXAS_MODEL_VMSMCC = 1; /*
                                                     * TEXAS Model VMS message category change lanes
                                                     */

    public static final int TEXAS_MODEL_VMSMCS = 2; /*
                                                     * TEXAS Model VMS message category speed
                                                     */

    public static final int TEXAS_MODEL_VMSCSC = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - stop immediately
                                                                      * using collision deceleration
                                                                      */

    public static final int TEXAS_MODEL_VMSCDD = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS priority for
                                                                      * message - distracted driver
                                                                      */

    public static final int TEXAS_MODEL_VMSCSM = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - stop immediately
                                                                      * using maximum vehicle
                                                                      * deceleration
                                                                      */

    public static final int TEXAS_MODEL_VMSCSL = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - stop at location
                                                                      * xx
                                                                      */

    public static final int TEXAS_MODEL_VMSCSI = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - stop at the
                                                                      * intersection stop line
                                                                      */

    public static final int TEXAS_MODEL_VMSCRR = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - forced run the red
                                                                      * signal
                                                                      */

    public static final int TEXAS_MODEL_VMSCGO = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - forced go
                                                                      */

    public static final int TEXAS_MODEL_VMSCAM = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - accelerate or
                                                                      * decelerate to speed xx using
                                                                      * maximum vehicle acceleration
                                                                      * or deceleration
                                                                      */

    public static final int TEXAS_MODEL_VMSCAN = TEXAS_MODEL_VMSMCS; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - accelerate or
                                                                      * decelerate to speed xx using
                                                                      * normal acceleration or
                                                                      * deceleration
                                                                      */

    public static final int TEXAS_MODEL_VMSCCL = TEXAS_MODEL_VMSMCC; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - change lanes to
                                                                      * the left
                                                                      */

    public static final int TEXAS_MODEL_VMSCCR = TEXAS_MODEL_VMSMCC; /*
                                                                      * TEXAS Model VMS category for
                                                                      * message - change lanes to
                                                                      * the right / ********* *
                                                                      * ********* *****
                                                                      */

    /* leg/lane designations */
    /*************************/

    public static final int TEXAS_MODEL_INBNDL = 1; /*
                                                     * TEXAS Model inbound leg/lane
                                                     */

    public static final int TEXAS_MODEL_DINBDL = 2; /*
                                                     * TEXAS Model diamond internal inbound leg/lane
                                                     */

    public static final int TEXAS_MODEL_OUTBDL = 3; /*
                                                     * TEXAS Model outbound leg/lane
                                                     */

    /*************************/
    /* intersection controls */
    /*************************/

    public static final int TEXAS_MODEL_ICUNCT = 1; /*
                                                     * TEXAS Model intersection control -
                                                     * uncontrolled
                                                     */

    public static final int TEXAS_MODEL_ICYELD = 2; /*
                                                     * TEXAS Model intersection control - yield sign
                                                     */

    public static final int TEXAS_MODEL_ICLTAS = 3; /*
                                                     * TEXAS Model intersection control -
                                                     * less-than-all-way stop sign
                                                     */

    public static final int TEXAS_MODEL_ICAWST = 4; /*
                                                     * TEXAS Model intersection control - all-way
                                                     * stop sign
                                                     */

    public static final int TEXAS_MODEL_ICPSIG = 5; /*
                                                     * TEXAS Model intersection control - pre-timed
                                                     * signal
                                                     */

    public static final int TEXAS_MODEL_ICSACT = 6; /*
                                                     * TEXAS Model intersection control -
                                                     * semi-actuated signal
                                                     */

    public static final int TEXAS_MODEL_ICFACT = 7; /*
                                                     * TEXAS Model intersection control -
                                                     * full-actuated signal
                                                     */

    public static final int TEXAS_MODEL_ICTDF3 = 8; /*
                                                     * TEXAS Model intersection control - Texas
                                                     * diamond fig 3 signal
                                                     */

    public static final int TEXAS_MODEL_ICTDF4 = 9; /*
                                                     * TEXAS Model intersection control - Texas
                                                     * diamond fig 4 signal
                                                     */

    public static final int TEXAS_MODEL_ICTDF6 = 10; /*
                                                      * TEXAS Model intersection control - Texas
                                                      * diamond fig 6 signal
                                                      */

    public static final int TEXAS_MODEL_ICTDF7 = 11; /*
                                                      * TEXAS Model intersection control - Texas
                                                      * diamond fig 7 signal
                                                      */

    public static final int TEXAS_MODEL_ICDDF3 = 12; /*
                                                      * TEXAS Model intersection control - Dallas
                                                      * diamond fig 3 signal
                                                      */

    public static final int TEXAS_MODEL_ICDDF4 = 13; /*
                                                      * TEXAS Model intersection control - Dallas
                                                      * diamond fig 4 signal
                                                      */

    public static final int TEXAS_MODEL_ICDDF6 = 14; /*
                                                      * TEXAS Model intersection control - Dallas
                                                      * diamond fig 6 signal
                                                      */

    public static final int TEXAS_MODEL_ICDDF7 = 15; /*
                                                      * TEXAS Model intersection control - Dallas
                                                      * diamond fig 7 signal
                                                      */

    public static final int TEXAS_MODEL_ICNEMA = 16; /*
                                                      * TEXAS Model intersection control - NEMA
                                                      * signal
                                                      */

    public static final int TEXAS_MODEL_ICNEMV = 17; /*
                                                      * TEXAS Model intersection control - NEMA
                                                      * volume density signal
                                                      */

    public static final int TEXAS_MODEL_ICHDWR = 18; /*
                                                      * TEXAS Model intersection control -
                                                      * hardware-in-the-loop signal
                                                      */

    /*****************/
    /* lane controls */
    /*****************/

    public static final int TEXAS_MODEL_LCOUTB = 1; /*
                                                     * TEXAS Model lane control - outbound (or
                                                     * blocked inbound) lane
                                                     */

    public static final int TEXAS_MODEL_LCUNCT = 2; /*
                                                     * TEXAS Model lane control - uncontrolled
                                                     */

    public static final int TEXAS_MODEL_LCYELD = 3; /*
                                                     * TEXAS Model lane control - yield sign
                                                     */

    public static final int TEXAS_MODEL_LCSTOP = 4; /*
                                                     * TEXAS Model lane control - stop sign
                                                     */

    public static final int TEXAS_MODEL_LCSIGX = 5; /*
                                                     * TEXAS Model lane control - signal without
                                                     * left or right turn on red
                                                     */

    public static final int TEXAS_MODEL_LCSLTR = 6; /*
                                                     * TEXAS Model lane control - signal with left
                                                     * turn on red
                                                     */

    public static final int TEXAS_MODEL_LCSRTR = 7; /*
                                                     * TEXAS Model lane control - signal with right
                                                     * turn on red
                                                     */

    /*****************/
    /* vehicle codes */
    /*****************/

    public static final double TEXAS_MODEL_DECBRK = -2.0; /*
                                                           * TEXAS Model deceleration rate for brake
                                                           * lights
                                                           */

    public static final int TEXAS_MODEL_ICNONE = 0; /*
                                                     * TEXAS Model vehicle code - none
                                                     */

    public static final int TEXAS_MODEL_ICYLTS = 1; /*
                                                     * TEXAS Model vehicle code - yellow left turn
                                                     * signals front & rear
                                                     */

    public static final int TEXAS_MODEL_ICYRTS = 2; /*
                                                     * TEXAS Model vehicle code - yellow right turn
                                                     * signals front & rear
                                                     */

    public static final int TEXAS_MODEL_ICRBRK = 4; /*
                                                     * TEXAS Model vehicle code - red brake lights
                                                     * in rear
                                                     */

    public static final int TEXAS_MODEL_ICVBMC = 8; /*
                                                     * TEXAS Model vehicle code - vehicle blocked by
                                                     * a major collision
                                                     */

    public static final int TEXAS_MODEL_ICVIMC = 16; /*
                                                      * TEXAS Model vehicle code - vehicle involved
                                                      * in a major collision
                                                      */

    public static final int TEXAS_MODEL_ICEVRC = 32; /*
                                                      * TEXAS Model vehicle code - emergency vehicle
                                                      * running a call
                                                      */

    public static final int TEXAS_MODEL_ICVREV = 64; /*
                                                      * TEXAS Model vehicle code - vehicle reacting
                                                      * to emergency vehicle running a call
                                                      */

    public static final int TEXAS_MODEL_ICRVMS = 128; /*
                                                       * TEXAS Model vehicle code - vehicle reacting
                                                       * to VMS message
                                                       */

    /*************************************/
    /* vehicle/pedestrian detector codes */
    /*************************************/

    public static final int TEXAS_MODEL_TDNONE = 0; /*
                                                     * TEXAS Model vehicle/pedestrian detector code
                                                     * - none
                                                     */

    public static final int TEXAS_MODEL_TDCROS = 1; /*
                                                     * TEXAS Model vehicle/pedestrian detector code
                                                     * - front bumper crossed
                                                     */

    public static final int TEXAS_MODEL_TDTRIP = 2; /*
                                                     * TEXAS Model vehicle/pedestrian detector code
                                                     * - tripped
                                                     */

    public static final int TEXAS_MODEL_TDCLER = 4; /*
                                                     * TEXAS Model vehicle/pedestrian detector code
                                                     * - rear bumper cleared
                                                     */

    public static final int TEXAS_MODEL_TDLOCK = 8; /*
                                                     * TEXAS Model vehicle/pedestrian detector code
                                                     * - detector locked
                                                     */

    /****************/
    /* posdat codes */
    /****************/

    public static final int TEXAS_MODEL_POSNON = 0; /*
                                                     * TEXAS Model posdat code - none
                                                     */

    public static final int TEXAS_MODEL_POSTIM = 1; /*
                                                     * TEXAS Model posdat code - time
                                                     */

    public static final int TEXAS_MODEL_POSTSI = 2; /*
                                                     * TEXAS Model posdat code - traffic signal
                                                     * indications
                                                     */

    public static final int TEXAS_MODEL_POSPSI = 4; /*
                                                     * TEXAS Model posdat code - pedestrian signal
                                                     * indications
                                                     */

    public static final int TEXAS_MODEL_POSTDA = 8; /*
                                                     * TEXAS Model posdat code - traffic detector
                                                     * actuations
                                                     */

    public static final int TEXAS_MODEL_POSPDA = 16; /*
                                                      * TEXAS Model posdat code - pedestrian
                                                      * detector actuations
                                                      */

    /************************/
    /* path type priorities */
    /************************/

    public static final int TEXAS_MODEL_LPTP01 = 1; /*
                                                     * TEXAS Model path type priority - 1
                                                     */

    public static final int TEXAS_MODEL_LPTP02 = 2; /*
                                                     * TEXAS Model path type priority - 2
                                                     */

    public static final int TEXAS_MODEL_LPTP03 = 3; /*
                                                     * TEXAS Model path type priority - 3
                                                     */

    public static final int TEXAS_MODEL_LPTP04 = 4; /*
                                                     * TEXAS Model path type priority - 4
                                                     */

    public static final int TEXAS_MODEL_LPTP05 = 5; /*
                                                     * TEXAS Model path type priority - 5
                                                     */

    public static final int TEXAS_MODEL_LPTP06 = 6; /*
                                                     * TEXAS Model path type priority - 6
                                                     */

    public static final int TEXAS_MODEL_LPTP07 = 7; /*
                                                     * TEXAS Model path type priority - 7
                                                     */

    public static final int TEXAS_MODEL_LPTP08 = 8; /*
                                                     * TEXAS Model path type priority - 8
                                                     */

    public static final int TEXAS_MODEL_LPTP09 = 9; /*
                                                     * TEXAS Model path type priority - 9
                                                     */

    public static final int TEXAS_MODEL_LPTP10 = 10; /*
                                                      * TEXAS Model path type priority - 10
                                                      */

    public static final int TEXAS_MODEL_LPTP11 = 11; /*
                                                      * TEXAS Model path type priority - 11
                                                      */

    public static final int TEXAS_MODEL_LPTP12 = 12; /*
                                                      * TEXAS Model path type priority - 12
                                                      */

    public static final int TEXAS_MODEL_LPTPNS = 13; /*
                                                      * TEXAS Model path type priority - 13 (not
                                                      * set)
                                                      */

    /**************/
    /* LAVT codes */
    /**************/

    public static final int TEXAS_MODEL_LAVTV = 1; /*
                                                    * TEXAS Model LAVT code - Normal Vehicles
                                                    */

    public static final int TEXAS_MODEL_LAVTR = 2; /*
                                                    * TEXAS Model LAVT code - Rail Vehicles
                                                    */

    public static final int TEXAS_MODEL_LAVTE = 4; /*
                                                    * TEXAS Model LAVT code - Emergency Vehicles
                                                    */

    public static final int TEXAS_MODEL_LAVTB = 8; /*
                                                    * TEXAS Model LAVT code - Bicycles
                                                    */

    /**********************/
    /* SIMPRO ITURN codes */
    /**********************/

    public static final int TEXAS_MODEL_ITURNU = 1; /*
                                                     * TEXAS Model ITURN code - U-Turn
                                                     */

    public static final int TEXAS_MODEL_ITURNL = 2; /*
                                                     * TEXAS Model ITURN code - Left Turn
                                                     */

    public static final int TEXAS_MODEL_ITURNS = 3; /*
                                                     * TEXAS Model ITURN code - Straight
                                                     */

    public static final int TEXAS_MODEL_ITURNR = 4; /*
                                                     * TEXAS Model ITURN code - Right Turn
                                                     */

    /********************************/
    /* LTURN, IPT, and IPTURN codes */
    /********************************/

    public static final int TEXAS_MODEL_LTURNR = 1; /*
                                                     * TEXAS Model LTURN, IPT, and IPTURN code -
                                                     * Right Turn
                                                     */

    public static final int TEXAS_MODEL_LTURNS = 2; /*
                                                     * TEXAS Model LTURN, IPT, and IPTURN code -
                                                     * Straight
                                                     */

    public static final int TEXAS_MODEL_LTURNL = 4; /*
                                                     * TEXAS Model LTURN, IPT, and IPTURN code -
                                                     * Left Turn
                                                     */

    public static final int TEXAS_MODEL_LTURNU = 8; /*
                                                     * TEXAS Model LTURN, IPT, and IPTURN code -
                                                     * U-Turn
                                                     */

    /*********************/
    /* DVPRO ITURN codes */
    /*********************/

    public static final int TEXAS_MODEL_DTURNL = 1; /*
                                                     * TEXAS Model DVPRO ITURN code - Left Turn and
                                                     * U-Turn
                                                     */

    public static final int TEXAS_MODEL_DTURNS = 2; /*
                                                     * TEXAS Model DVPRO ITURN code - Straight
                                                     */

    public static final int TEXAS_MODEL_DTURNR = 3; /*
                                                     * TEXAS Model DVPRO ITURN code - Right Turn
                                                     */

    /**************/
    /* IBUF codes */
    /**************/

    public static final int TEXAS_MODEL_IBUFVC = 1; /*
                                                     * TEXAS Model IBUF code - IVEHCL - vehicle
                                                     * class
                                                     */

    public static final int TEXAS_MODEL_IBUFDC = 2; /*
                                                     * TEXAS Model IBUF code - IDRICL - driver class
                                                     */

    public static final int TEXAS_MODEL_IBUFDV = 3; /*
                                                     * TEXAS Model IBUF code - DESVEL - desired
                                                     * speed
                                                     */

    public static final int TEXAS_MODEL_IBUFDO = 4; /*
                                                     * TEXAS Model IBUF code - NOBAPD - desired
                                                     * outbound approach
                                                     */

    public static final int TEXAS_MODEL_IBUFIA = 5; /*
                                                     * TEXAS Model IBUF code - IA - inbound approach
                                                     */

    public static final int TEXAS_MODEL_IBUFLN = 6; /*
                                                     * TEXAS Model IBUF code - ILN - inbound lane
                                                     */

    public static final int TEXAS_MODEL_IBUFPL = 7; /*
                                                     * TEXAS Model IBUF code - IPRTLO - print
                                                     * vehicle on logout
                                                     */

    public static final int TEXAS_MODEL_IBUFVN = 8; /*
                                                     * TEXAS Model IBUF code - IVN - vehicle number
                                                     */

    public static final int TEXAS_MODEL_IBUFFS = 9; /*
                                                     * TEXAS Model IBUF code - FSTPIA - forced stop
                                                     * +=approach or -=path
                                                     */

    /******************************************/
    /* vehicle detector call and extend codes */
    /******************************************/

    public static final int TEXAS_MODEL_LDCALL = 1; /*
                                                     * TEXAS Model vehicle detector call and extend
                                                     * codes - LDCALL - call
                                                     */

    public static final int TEXAS_MODEL_LDEXTN = 2; /*
                                                     * TEXAS Model vehicle detector call and extend
                                                     * codes - LDEXTN - extend
                                                     */

    public static final int TEXAS_MODEL_LDCNEX = 3; /*
                                                     * TEXAS Model vehicle detector call and extend
                                                     * codes - LDCNEX - call&extend
                                                     */

    public PARAMS() {} // end of method PARAMS
} // end of class PARAMS

/******************************************************************************/
/* PARAMS.java */
/******************************************************************************/
