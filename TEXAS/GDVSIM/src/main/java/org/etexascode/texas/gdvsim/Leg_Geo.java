package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                Leg_Geo.java                                */
/******************************************************************************/
/*                                                                            */
/*     gdvsim COPYRIGHT (C) 2004 by Rioux Engineering, Austin, Texas USA      */
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

import java.io.*;
import java.lang.*;
import java.util.*;

class Leg_Geo /* leg geometry data */
{

    int msiv_leg_no; /* leg number */

    int msiv_ang; /* leg angle */

    int msiv_len_inb; /* length of inbound lanes */

    int msiv_len_out; /* length of outbound lanes */

    int msiv_no_inb; /* number of inbound lanes */

    int msiv_no_out; /* number of outbound lanes */

    int msiv_sl_inb; /* speed limit on inbound lanes */

    int msiv_sl_out; /* speed limit on outbound lanes */

    int msiv_cl_off; /* centerline offset */

    int msiv_med_w; /* median width */

    int msiv_ang_s; /* limiting angle for straight movement */

    int msiv_ang_u; /* limiting angle for u-turn */

    int msiv_num_var_traf_periods; /* number of varying traffic periods */

    /* the following variables are defined but are not read or written */
    int msiv_ixl; /* x coordinate for leg inbound approach */

    int msiv_iyl; /* y coordinate for leg inbound approach */

    int msiv_oxl; /* x coordinate for leg outbound approach */

    int msiv_oyl; /* y coordinate for leg outbound approach */

    int msiv_ixu; /* x coordinate for leg u-turn */

    int msiv_iyu; /* y coordinate for leg u-turn */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_LEG_GEO]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_leg_no
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_LEG_NO ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_ANG ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_LEN_INB]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_out
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_LEN_OUT]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_NO_INB ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_NO_OUT ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_sl_inb
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_SL_INB ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_sl_out
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_SL_OUT ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_CL_OFF ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_med_w
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_MED_W ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang_s
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_ANG_S ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang_u
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_ANG_U ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_TRAFPER]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ixl
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_IXL ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_iyl
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_IYL ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_oxl
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_OXL ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_oyl
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_OYL ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ixu
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_IXU ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_iyu
    // .mclv_aux.msia_stat[TX_FMT_GDV_LEG_GEO_IYU ]

    public Leg_Geo() {
        mclv_aux = new TX_Aux();
    } // end of method Leg_Geo

    public void checkForErrors(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Leg_Geo data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Leg_Geo.checkForErrors psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LEG_GEO];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Leg_Geo.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Leg_Geo.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for leg " + psiv_leg + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Leg_Geo.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for leg " + psiv_leg + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg // leg number
    ) {
        boolean lbov_free_uturn_defined;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_max;
        String lstv_name;

        // printToFile prints all Leg_Geo data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Leg_Geo.printToFile psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.printToFile: psiv_leg = " + psiv_leg + " is < 1 or > " + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
                    + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LEG_GEO];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Leg_Geo.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(13);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_leg_no, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEG_NO, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        msiv_ang = Intersection.bound_angle_0_to_359(msiv_ang);
        Intersection.filesPrintGDV_IntToFile(msiv_ang, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_len_inb, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_len_out, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEN_OUT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check special condition for diamond interchange free u-turn lane
        // special code that must also be performed by the menu system
        lbov_free_uturn_defined = false;
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB];
        if (Intersection.mbov_is_diamond_interchange) {
            if (Intersection.mbov_free_uturns_defined) {
                if (psiv_leg == 3) {
                    // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                    // check if free u-turn 1 lane width is valid
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    // decrease maximum number of lanes if free u-turn 1 defined
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width > 0) {
                        lbov_free_uturn_defined = true;
                    }
                }
                if (psiv_leg == 6) {
                    // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                    // check if free u-turn 2 lane width is valid
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    // decrease maximum number of lanes if free u-turn 2 defined
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width > 0) {
                        lbov_free_uturn_defined = true;
                    }
                }
                if (lbov_free_uturn_defined) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB]--;
                }
            } // end if ( Intersection.mbov_free_uturns_defined )
        } // end if ( Intersection.mbov_is_diamond_interchange )

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_no_inb, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_NO_INB, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (lbov_free_uturn_defined) {
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] = lsiv_max;
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_no_out, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_sl_inb, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_SL_INB, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_sl_out, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_SL_OUT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_cl_off, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_CL_OFF, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_med_w, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_MED_W, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_ang_s, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG_S, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_ang_u, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG_U, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_num_var_traf_periods, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg // leg number
    ) {
        boolean lbov_free_uturn_defined;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_max;
        String lstv_name;

        // readFromCards reads all Leg_Geo fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Leg_Geo.readFromCards psiv_leg=" + psiv_leg);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set card number
        lsiv_card = -1;
        switch (psiv_leg) {
            case 1:
                // check if leg 1 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1];
                break;

            case 2:
                // check if leg 2 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2];
                break;

            case 3:
                // check if leg 3 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3];
                break;

            case 4:
                // check if leg 4 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4];
                break;

            case 5:
                // check if leg 5 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5];
                break;

            case 6:
                // check if leg 6 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LEG_GEO];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Leg_Geo.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_leg_no = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEG_NO, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_ang = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead,
                lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;
        msiv_ang = Intersection.bound_angle_0_to_359(msiv_ang);

        // read data from cards
        msiv_len_inb = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_len_out = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEN_OUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check special condition for diamond interchange free u-turn lane
        // special code that must also be performed by the menu system
        lbov_free_uturn_defined = false;
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB];
        if (Intersection.mbov_is_diamond_interchange) {
            if (Intersection.mbov_free_uturns_defined) {
                if (psiv_leg == 3) {
                    // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                    // check if free u-turn 1 lane width is valid
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    // decrease maximum number of lanes if free u-turn 1 defined
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width > 0) {
                        lbov_free_uturn_defined = true;
                    }
                }
                if (psiv_leg == 6) {
                    // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                    // check if free u-turn 2 lane width is valid
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    // decrease maximum number of lanes if free u-turn 2 defined
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width > 0) {
                        lbov_free_uturn_defined = true;
                    }
                }
                if (lbov_free_uturn_defined) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB]--;
                }
            } // end if ( Intersection.mbov_free_uturns_defined )
        } // end if ( Intersection.mbov_is_diamond_interchange )

        // read data from cards
        msiv_no_inb = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_NO_INB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (lbov_free_uturn_defined) {
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] = lsiv_max;
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_no_out = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_sl_inb = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_SL_INB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_sl_out = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_SL_OUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_cl_off = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_CL_OFF, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_med_w = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_MED_W, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_ang_s = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG_S, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_ang_u = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG_U, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_num_var_traf_periods = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Leg_Geo fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Leg_Geo.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg // leg number
    ) {
        boolean lbov_free_uturn_defined;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_max;
        String lstv_name;

        // writeToCards writes all Leg_Geo fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Leg_Geo.writeToCards psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > " + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
                    + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set card number
        lsiv_card = -1;
        switch (psiv_leg) {
            case 1:
                // check if leg 1 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1];
                break;

            case 2:
                // check if leg 2 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2];
                break;

            case 3:
                // check if leg 3 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3];
                break;

            case 4:
                // check if leg 4 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4];
                break;

            case 5:
                // check if leg 5 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5];
                break;

            case 6:
                // check if leg 6 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Leg_Geo.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LEG_GEO];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Leg_Geo.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_leg_no, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEG_NO, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        msiv_ang = Intersection.bound_angle_0_to_359(msiv_ang);
        Intersection.writeIntToCard(msiv_ang, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten,
                lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_len_inb, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_len_out, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_LEN_OUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check special condition for diamond interchange free u-turn lane
        // special code that must also be performed by the menu system
        lbov_free_uturn_defined = false;
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB];
        if (Intersection.mbov_is_diamond_interchange) {
            if (Intersection.mbov_free_uturns_defined) {
                if (psiv_leg == 3) {
                    // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                    // check if free u-turn 1 lane width is valid
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    // decrease maximum number of lanes if free u-turn 1 defined
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width > 0) {
                        lbov_free_uturn_defined = true;
                    }
                }
                if (psiv_leg == 6) {
                    // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                    // check if free u-turn 2 lane width is valid
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in Leg_Geo.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    // decrease maximum number of lanes if free u-turn 2 defined
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width > 0) {
                        lbov_free_uturn_defined = true;
                    }
                }
                if (lbov_free_uturn_defined) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB]--;
                }
            } // end if ( Intersection.mbov_free_uturns_defined )
        } // end if ( Intersection.mbov_is_diamond_interchange )

        // write data to cards
        Intersection.writeIntToCard(msiv_no_inb, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_NO_INB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (lbov_free_uturn_defined) {
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] = lsiv_max;
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_no_out, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_sl_inb, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_SL_INB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_sl_out, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_SL_OUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_cl_off, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_CL_OFF, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_med_w, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_MED_W, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_ANG_S] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeIntToCard(msiv_ang_s, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG_S, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_ANG_U] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeIntToCard(msiv_ang_u, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_ANG_U, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        Intersection.writeIntToCard(msiv_num_var_traf_periods, lstv_name, Intersection.TX_FMT_GDV_LEG_GEO, Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Leg_Geo

/******************************************************************************/
/* Leg_Geo.java */
/******************************************************************************/
