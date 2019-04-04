package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              Traf_Hdway.java                               */
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

class Traf_Hdway /* traffic headway frequency distribution data */
{

    String mstv_dist; /* name for inbound traffic headway frequency distribution */

    int msiv_vol; /* total hourly volume on leg */

    double mdfv_par; /* parameter for headway frequency distribution */

    double mdfv_mean; /* mean speed of entering vehicles */

    double mdfv_85th; /* 85th percentile speed of entering vehicles */

    String mstv_tm; /* traffic mix data to follow ? */

    int msiv_seed; /* seed for random numbers */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_TRAF_HDWAY]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_dist
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_DIST]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.msiv_vol
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_VOL ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mdfv_par
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_PAR ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_MEAN]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_85TH]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_tm
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_TM ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.msiv_seed
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_HDWAY_SEED]

    // mdfv_par can also be read/printed using
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_HDWAY_PARAM]
    // [TX_FMT_GDV_HDWAY_PARAM_CONSTAN]
    // [TX_FMT_GDV_HDWAY_PARAM_ERLANG ]
    // [TX_FMT_GDV_HDWAY_PARAM_GAMMA ]
    // [TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
    // [TX_FMT_GDV_HDWAY_PARAM_NEGEXP ]
    // [TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
    // [TX_FMT_GDV_HDWAY_PARAM_UNIFORM]

    public Traf_Hdway() {
        mclv_aux = new TX_Aux();
    } // end of method Traf_Hdway

    public void checkForErrors(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Traf_Hdway data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Traf_Hdway.checkForErrors psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_HDWAY];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Traf_Hdway.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Traf_Hdway.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for leg " + psiv_leg + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for leg " + psiv_leg + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_member;
        String lstv_name;

        // printToFile prints all Traf_Hdway data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Hdway.printToFile psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_dist);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data to check traffic headway parameter
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Hdway.printToFile checking " + lstv_name);

        lsiv_member = 0;
        if (Intersection.mbov_is_hd_ERLANG) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_GAMMA) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_LOGNRML) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_SNEGEXP) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_UNIFORM) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_HDWAY];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Hdway.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        if (Intersection.mbov_is_hd_parameter_needed) {
            Intersection.filesPrintGDV_check_newpage(7);
        }
        else {
            Intersection.filesPrintGDV_check_newpage(6);
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_dist, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_DIST, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_vol, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_VOL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_hd_parameter_needed) {
            // print data to a file - use TX_FMT_GDV_HDWAY_PARAM
            Intersection.filesPrintGDV_DoubleToFile(mdfv_par, lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, lsiv_member, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_mean, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_MEAN, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_85th, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_85TH, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_tm, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_TM, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_seed, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_SEED, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all Traf_Hdway fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Hdway.readFromCards psiv_leg=" + psiv_leg);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 1);

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_HDWAY];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Hdway.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_dist = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_DIST, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_dist);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_vol = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_VOL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_hd_parameter_needed) {
            // read data from cards
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_PAR, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else {
            mdfv_par = 0.0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_PAR] = Intersection.TX_SET_BY_SOFTWARE;
        }

        // read data from cards
        mdfv_mean = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_MEAN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_85th = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_85TH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_tm = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_TM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_seed = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_SEED, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data for reading data from cards to re-read traffic headway parameter
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Hdway.readFromCards reading " + lstv_name);

        if (Intersection.mbov_is_hd_CONSTAN) {
            mdfv_par = 0.0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_PAR] = Intersection.TX_SET_BY_SOFTWARE;
        }
        else if (Intersection.mbov_is_hd_ERLANG) {
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_GAMMA) {
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_LOGNRML) {
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_NEGEXP) {
            mdfv_par = 0.0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_PAR] = Intersection.TX_SET_BY_SOFTWARE;
        }
        else if (Intersection.mbov_is_hd_SNEGEXP) {
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_UNIFORM) {
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Traf_Hdway fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Traf_Hdway.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Traf_Hdway fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Hdway.writeToCards psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 1);

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_dist);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data to check traffic headway parameter
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Hdway.writeToCards checking " + lstv_name);

        if (Intersection.mbov_is_hd_ERLANG) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG]
                        + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG]
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_GAMMA) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]
                        + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_LOGNRML) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                        + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " or > "
                        + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_SNEGEXP) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                        + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " or > "
                        + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_UNIFORM) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM])) {
                Intersection.mstv_errorMessage = "Error in Traf_Hdway.writeToCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                        + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " or > "
                        + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_HDWAY];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Hdway.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeStringToCard(mstv_dist, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_DIST, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_vol, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_VOL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_hd_parameter_needed) {
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_par, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_PAR, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_mean, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_MEAN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_85th, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_85TH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_tm, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_TM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_seed, lstv_name, Intersection.TX_FMT_GDV_TRAF_HDWAY, Intersection.TX_FMT_GDV_TRAF_HDWAY_SEED, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Traf_Hdway

/******************************************************************************/
/* Traf_Hdway.java */
/******************************************************************************/
