package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_DriverMix.java                             */
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

import org.etexascode.texas.gdvsim.Intersection;
import org.etexascode.texas.gdvsim.PARAMS;
import org.etexascode.texas.gdvsim.TX_Aux;
import org.etexascode.texas.gdvsim.TX_Fmt;
import java.io.*;
import java.lang.*;
import java.util.*;

class GDV_DriverMix {

    double mdfa_driver_mix[][]; /* User-defined driver mix [veh_cl][drv_cl] */

    TX_Aux[] mcla_aux = new TX_Aux[PARAMS.TEXAS_MODEL_NVC + 1]; /*
                                                                 * Auxiliary data [veh_cl]
                                                                 */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_DRIVER_MIX_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][1] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][2] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][3] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_3]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][4] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_4]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][5] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_5]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][6] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_6]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][7] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_7]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][8] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_8]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[ 1][9] .mcla_aux[
    // 1].msia_stat[TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_9]
    //
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][1]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_1]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][2]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][3]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_3]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][4]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_4]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][5]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_5]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][6]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_6]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][7]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_7]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][8]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_8]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[99][9]
    // .mcla_aux[99].msia_stat[TX_FMT_GDV_DRIVER_MIX_99_DRIVERMIX_9]

    public GDV_DriverMix() {
        int lsiv_veh;
        mdfa_driver_mix = new double[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_NDC + 1];
        for (lsiv_veh = 0; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            mcla_aux[lsiv_veh] = new TX_Aux();
        }
    } // end of method GDV_DriverMix

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_drv;
        int lsiv_veh;
        String lstv_name;

        // checkForErrors checks all GDV_DriverMix data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_DriverMix.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check all data for errors
        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            // set local data for checking data for errors
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
                System.out.println("GDV_DriverMix.checkForErrors checking " + lstv_name);
            // process all drivers
            for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                if (mcla_aux[lsiv_veh].msia_stat[lsiv_drv] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_DriverMix.checkForErrors: " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mcla_aux[lsiv_veh].msia_stat[lsiv_drv] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DriverMix.checkForErrors: " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            } // end of for lsiv_drv loop
        } // end of for lsiv_veh loop

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile() {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_driver_mix;
        int lsiv_drv;
        int lsiv_veh;
        String lstv_name;

        // printToFile prints all GDV_DriverMix data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_DriverMix.printToFile");
        // check if driver mix is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if no driver mix card then return success and do not print
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            // set local data for printing data to a file
            ldfv_sum_driver_mix = 0;
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_printGDV)
                System.out.println("GDV_DriverMix.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintGDV_check_newpage(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // process all drivers
            for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                // print data to file
                Intersection.filesPrintGDV_DoubleToFile(mdfa_driver_mix[lsiv_veh][lsiv_drv], lstv_name, Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh,
                        Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv, mcla_aux[lsiv_veh], Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                ldfv_sum_driver_mix += mdfa_driver_mix[lsiv_veh][lsiv_drv];
            } // end of for lsiv_drv loop

            // check that the sum of the driver mix must be 100 percent
            // special code that must also be performed by the menu system
            if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_driver_mix, 100.0)) {
                Intersection.mstv_warningMessage = "Warning in GDV_DriverMix.printToFile: Sum of percent of driver mix for vehicle class " + lsiv_veh + " = " + ldfv_sum_driver_mix + " is not 100.";
                Intersection.warningMessage();
                for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                    mcla_aux[lsiv_veh].msia_stat[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] = Intersection.TX_DATA_ERROR;
                    /* debug */if (Intersection.mbov_debug_printGDV)
                        System.out.println("GDV_DriverMix.printToFile "
                                + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] + "="
                                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv] + "  "
                                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].status(Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv)
                                + " TX_DATA_ERROR");
                }
            }
        } // end of for lsiv_veh loop

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        boolean lbov_forceDefault;
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_driver_mix;
        int lsiv_card;
        int lsiv_drv;
        int lsiv_veh;
        String lstv_name;

        // readFromCards reads all GDV_DriverMix fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DriverMix.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if driver mix card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_DRIVER_MIX_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if driver mix is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix.equals("NO"));

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            // set local data for reading data from cards
            ldfv_sum_driver_mix = 0;
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
            lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card - 1 + lsiv_veh;
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_filesReadGDV)
                System.out.println("GDV_DriverMix.readFromCards reading " + lstv_name);

            // process all drivers
            for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                // read data from cards
                mdfa_driver_mix[lsiv_veh][lsiv_drv] = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh,
                        Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mcla_aux[lsiv_veh],
                        lbov_forceDefault);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                ldfv_sum_driver_mix += mdfa_driver_mix[lsiv_veh][lsiv_drv];
            } // end of for lsiv_drv loop

            // check that the sum of the driver mix must be 100 percent
            // special code that must also be performed by the menu system
            if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_driver_mix, 100.0)) {
                Intersection.mstv_warningMessage = "Warning in GDV_DriverMix.readFromCards: Sum of percent of driver mix for vehicle class " + lsiv_veh + " = " + ldfv_sum_driver_mix + " is not 100.";
                Intersection.warningMessage();
                for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                    mcla_aux[lsiv_veh].msia_stat[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] = Intersection.TX_DATA_ERROR;
                    /* debug */if (Intersection.mbov_debug_filesReadGDV)
                        System.out.println("GDV_DriverMix.readFromCards "
                                + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] + "="
                                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv] + "  "
                                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].status(Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv)
                                + " TX_DATA_ERROR");
                }
            }
        } // end of for lsiv_veh loop

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        int lsiv_veh;

        // setAllInvalid sets all GDV_DriverMix fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_DriverMix.setAllInvalid");
        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            mcla_aux[lsiv_veh].setAllInvalid();
        }

        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_driver_mix;
        int lsiv_card;
        int lsiv_drv;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_DriverMix fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_DriverMix.writeToCards");
        // check if driver mix is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if no driver mix card then return success
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check if driver mix card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_DRIVER_MIX_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DriverMix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            // set local data for writing data to cards
            ldfv_sum_driver_mix = 0;
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
            lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card - 1 + lsiv_veh;
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_filesWriteGDV)
                System.out.println("GDV_DriverMix.writeToCards writing " + lstv_name);

            // process all drivers
            for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                // write data to cards
                Intersection.writeDoubleToCard(mdfa_driver_mix[lsiv_veh][lsiv_drv], lstv_name, Intersection.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1
                        - 1 + lsiv_drv, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mcla_aux[lsiv_veh]);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                ldfv_sum_driver_mix += mdfa_driver_mix[lsiv_veh][lsiv_drv];
            } // end of for lsiv_drv loop

            // check that the sum of the driver mix must be 100 percent
            // special code that must also be performed by the menu system
            if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_driver_mix, 100.0)) {
                Intersection.mstv_warningMessage = "Warning in GDV_DriverMix.writeToCards: Sum of percent of driver mix for vehicle class " + lsiv_veh + " = " + ldfv_sum_driver_mix + " is not 100.";
                Intersection.warningMessage();
                for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
                    mcla_aux[lsiv_veh].msia_stat[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] = Intersection.TX_DATA_ERROR;
                    /* debug */if (Intersection.mbov_debug_filesWriteGDV)
                        System.out.println("GDV_DriverMix.writeToCards "
                                + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv] + "="
                                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv] + "  "
                                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].status(Intersection.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv)
                                + " TX_DATA_ERROR");
                }
            }
        } // end of for lsiv_veh loop

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_DriverMix

/******************************************************************************/
/* GDV_DriverMix.java */
/******************************************************************************/
