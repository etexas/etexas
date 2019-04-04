package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              TX_Det_Data.java                              */
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

class TX_Det_Data {

    int msiv_leg; /* leg where detector is located */

    int msiv_fl; /* first lane covered by detector */

    int msiv_nl; /* number of lanes covered by detector */

    int msiv_spa; /* spacing between nominal stop line and detector */

    /* + is toward and - is away from the intersection center */
    int msiv_len; /* length of detector */

    String mstv_type; /* detector type "CL", "IN", "PR", or "PU" */
    ; /* CL=classify IN=inactive PR=presence PU=pulse */

    int msiv_delay; /* delay value */

    double mdfv_extend; /* extend value */

    int msiv_class_num; /* classfiy detector number of vehicle classes */

    String msta_class_name[]; /* classify detector class name */

    int msia_class_length_lower[]; /*
                                    * classify detector class length lower values
                                    */

    int msia_class_length_upper[]; /*
                                    * classify detector class length upper values
                                    */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_DETECT_DATNDI]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_leg
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_LEG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_fl
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_FL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_nl
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_NL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_spa
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_SPA ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_len
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_LEN ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_TYPE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_delay
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_DELAY ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mdfv_extend
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_EXTEND ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msta_class_name
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msia_class_length_lower
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msia_class_length_upper
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_DETECT_DATDIA]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_leg
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_LEG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_fl
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_FL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_nl
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_NL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_spa
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_SPA ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_len
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_LEN ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_TYPE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_delay
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_DELAY ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mdfv_extend
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_EXTEND ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_CLASS_NUM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msta_class_name
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msia_class_length_lower
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msia_class_length_upper
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01]

    public TX_Det_Data() {
        msta_class_name = new String[PARAMS.TEXAS_MODEL_LDC + 1];
        msia_class_length_lower = new int[PARAMS.TEXAS_MODEL_LDC + 1];
        msia_class_length_upper = new int[PARAMS.TEXAS_MODEL_LDC + 1];
        mclv_aux = new TX_Aux();
    } // end of method TX_Det_Data

    public void checkForErrors(int psiv_detector // detector number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_field_class_llim;
        int lsiv_field_class_name;
        int lsiv_field_class_ulim;
        int lsiv_field_type;
        String lstv_name;

        // checkForErrors checks all TX_Det_Data data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("TX_Det_Data.checkForErrors psiv_detector=" + psiv_detector);
        // check if number of detectors is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_detector < 1) || (psiv_detector > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: psiv_detector = " + psiv_detector + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_data) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        // data format for diamond interchange and non-diamond interchange is different
        if (Intersection.mbov_is_diamond_interchange) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATDIA];
            lsiv_field_type = Intersection.TX_FMT_SIM_DETECT_DATDIA_TYPE;
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01;
        }
        else {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATNDI];
            lsiv_field_type = Intersection.TX_FMT_SIM_DETECT_DATNDI_TYPE;
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01;
        }
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("TX_Det_Data.checkForErrors reading " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field < lsiv_field_class_name; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            if (lsiv_field == lsiv_field_type) {
                if (!mstv_type.equals("CL")) {
                    msiv_class_num = 0;
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM] = Intersection.TX_SET_BY_SOFTWARE;
                }
            }
        }

        if (msiv_class_num > 0) {
            for (lsiv_field = lsiv_field_class_name; lsiv_field <= (lsiv_field_class_name - 1 + msiv_class_num); lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            for (lsiv_field = lsiv_field_class_llim; lsiv_field <= (lsiv_field_class_llim - 1 + msiv_class_num); lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            for (lsiv_field = lsiv_field_class_ulim; lsiv_field <= (lsiv_field_class_ulim - 1 + msiv_class_num); lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_detector // detector number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_class;
        int lsiv_leg_max;
        int lsiv_field_class_llim;
        int lsiv_field_class_name;
        int lsiv_field_class_ulim;
        int lsiv_fl_max;
        int lsiv_nl_max;
        int lsiv_lines;
        String lstv_leg;
        String lstv_name;

        // printToFile prints all TX_Det_Data data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("TX_Det_Data.printToFile psiv_detector=" + psiv_detector);
        // check if number of detectors is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_detector < 1) || (psiv_detector > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: psiv_detector = " + psiv_detector + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_data) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lsiv_lines = 8;
        if (mstv_type.equals("CL")) {
            lsiv_lines += (1 + 3 * msiv_class_num);
        }

        // data format for diamond interchange and non-diamond interchange is different
        if (Intersection.mbov_is_diamond_interchange) {
            // set local data for printing data to a file
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATDIA];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01;
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("TX_Det_Data.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(lsiv_lines);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            switch (msiv_leg) {
                case 0:
                    lstv_leg = "IR";
                    break;

                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                case 6:
                    lstv_leg = " " + Integer.toString(msiv_leg);
                    break;

                case 7:
                    lstv_leg = "IL";
                    break;

                default:
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + msiv_leg
                            + " is < 0 or > 7.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
            }
            if (msiv_leg == (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1)) {
                lstv_leg = "IL";
            }
            Intersection.filesPrintSIM_StringToFile(lstv_leg, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_fl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_fl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_FL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL] = lsiv_fl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_nl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb - msiv_fl + 1;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_NL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL] = lsiv_nl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_spa, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_SPA, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_len, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_LEN, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_type, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_TYPE, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_delay, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_DELAY, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_extend, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_EXTEND, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_type.equals("CL")) {
                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msiv_class_num, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NUM, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_class = 1; lsiv_class <= msiv_class_num; lsiv_class++) {
                    // print data to a file
                    Intersection.filesPrintSIM_StringToFile(msta_class_name[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_name - 1 + lsiv_class, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // print data to a file
                    Intersection.filesPrintSIM_IntToFile(msia_class_length_lower[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_llim - 1 + lsiv_class, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // print data to a file
                    Intersection.filesPrintSIM_IntToFile(msia_class_length_upper[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_ulim - 1 + lsiv_class, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }
        else {
            // set local data for printing data to a file
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATNDI];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATNDI];
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01;
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("TX_Det_Data.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(lsiv_lines);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_leg_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_leg, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG] = lsiv_leg_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.printToFile: Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_fl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_fl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_FL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL] = lsiv_fl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_nl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb - msiv_fl + 1;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_NL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL] = lsiv_nl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_spa, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_SPA, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_len, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_LEN, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_type, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_TYPE, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_delay, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_DELAY, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_extend, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_EXTEND, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_type.equals("CL")) {
                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msiv_class_num, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_class = 1; lsiv_class <= msiv_class_num; lsiv_class++) {
                    // print data to a file
                    Intersection.filesPrintSIM_StringToFile(msta_class_name[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_name - 1 + lsiv_class, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // print data to a file
                    Intersection.filesPrintSIM_IntToFile(msia_class_length_lower[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_llim - 1 + lsiv_class, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // print data to a file
                    Intersection.filesPrintSIM_IntToFile(msia_class_length_upper[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_ulim - 1 + lsiv_class, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_detector // detector number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_class;
        int lsiv_detector;
        int lsiv_field_class_llim;
        int lsiv_field_class_name;
        int lsiv_field_class_ulim;
        int lsiv_fl_max;
        int lsiv_leg_max;
        int lsiv_nl_max;
        int lsiv_ul_min;
        String lstv_leg;
        String lstv_name;

        // readFromCards reads all TX_Det_Data fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("TX_Det_Data.readFromCards psiv_detector=" + psiv_detector);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of detectors is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_detector < 1) || (psiv_detector > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: psiv_detector = " + psiv_detector + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_data) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first detector data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_FIRST_DET_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_det_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_det_card;
        for (lsiv_detector = 1; lsiv_detector < psiv_detector; lsiv_detector++) {
            lsiv_card += 1;
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
            }
        }

        // data format for diamond interchange and non-diamond interchange is different
        if (Intersection.mbov_is_diamond_interchange) {
            // set local data for reading data from cards
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATDIA];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01;
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("TX_Det_Data.readFromCards reading " + lstv_name);

            // read data from cards
            lstv_leg = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set leg number
            // special code that must also be performed by the menu system
            if (lstv_leg.equals("IR")) {
                msiv_leg = 0;
            }
            else if (lstv_leg.equals(" 1")) {
                msiv_leg = 1;
                if (msiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs) {
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + lstv_leg + " is > "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            else if (lstv_leg.equals(" 2")) {
                msiv_leg = 2;
                if (msiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs) {
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + lstv_leg + " is > "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            else if (lstv_leg.equals(" 3")) {
                msiv_leg = 3;
                if (msiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs) {
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + lstv_leg + " is > "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            else if (lstv_leg.equals(" 4")) {
                msiv_leg = 4;
                if (msiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs) {
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + lstv_leg + " is > "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            else if (lstv_leg.equals(" 5")) {
                msiv_leg = 5;
                if (msiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs) {
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + lstv_leg + " is > "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            else if (lstv_leg.equals(" 6")) {
                msiv_leg = 6;
                if (msiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs) {
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + lstv_leg + " is > "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            else if (lstv_leg.equals("IL")) {
                msiv_leg = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
            }
            else {
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] = Intersection.TX_DATA_IS_INVALID;
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG]
                        + " = " + lstv_leg + " is not \" 1\", \" 2\", \" 3\", \" 4\", \" 5\", \" 6\", \"IL\", or \"IR\".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_fl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            // read data from cards
            msiv_fl = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_FL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL] = lsiv_fl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_nl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb - msiv_fl + 1;
            // read data from cards
            msiv_nl = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_NL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL] = lsiv_nl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msiv_spa = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_SPA, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msiv_len = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_LEN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_TYPE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msiv_delay = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_DELAY, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            mdfv_extend = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_EXTEND, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_type.equals("CL")) {
                // read data from cards
                msiv_class_num = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NUM, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_class = 1; lsiv_class <= msiv_class_num; lsiv_class++) {
                    // read data from cards
                    msta_class_name[lsiv_class] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_name - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT,
                            Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // read data from cards
                    msia_class_length_lower[lsiv_class] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_llim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    lsiv_ul_min = lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class];
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lclv_tx_fmt.msia_max[lsiv_field_class_ulim - 1 + lsiv_class];
                    }
                    // read data from cards
                    msia_class_length_upper[lsiv_class] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_ulim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lsiv_ul_min;
                    }
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
            else {
                msiv_class_num = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM] = Intersection.TX_FROM_FILE;
            }
        }
        else {
            // set local data for reading data from cards
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATNDI];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01;
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("TX_Det_Data.readFromCards reading " + lstv_name);

            lsiv_leg_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
            // read data from cards
            msiv_leg = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG] = lsiv_leg_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.readFromCards: Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_fl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            // read data from cards
            msiv_fl = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_FL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL] = lsiv_fl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_nl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb - msiv_fl + 1;
            // read data from cards
            msiv_nl = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_NL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL] = lsiv_nl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msiv_spa = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_SPA, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msiv_len = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_LEN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_TYPE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msiv_delay = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_DELAY, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            mdfv_extend = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_EXTEND, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_type.equals("CL")) {
                // read data from cards
                msiv_class_num = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_class = 1; lsiv_class <= msiv_class_num; lsiv_class++) {
                    // read data from cards
                    msta_class_name[lsiv_class] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_name - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT,
                            Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // read data from cards
                    msia_class_length_lower[lsiv_class] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_llim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    lsiv_ul_min = lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class];
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lclv_tx_fmt.msia_max[lsiv_field_class_ulim - 1 + lsiv_class];
                    }
                    // read data from cards
                    msia_class_length_upper[lsiv_class] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_ulim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lsiv_ul_min;
                    }
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
            else {
                msiv_class_num = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM] = Intersection.TX_FROM_FILE;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all TX_Det_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_Det_Data.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_detector // detector number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_class;
        int lsiv_detector;
        int lsiv_field_class_llim;
        int lsiv_field_class_name;
        int lsiv_field_class_ulim;
        int lsiv_fl_max;
        int lsiv_leg_max;
        int lsiv_nl_max;
        int lsiv_ul_min;
        String lstv_leg;
        String lstv_name;

        // writeToCards writes all TX_Det_Data fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("TX_Det_Data.writeToCards psiv_detector=" + psiv_detector);
        // check if number of detectors is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_detector < 1) || (psiv_detector > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: psiv_detector = " + psiv_detector + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_data) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first detector data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_FIRST_DET_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_det_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_det_card;
        for (lsiv_detector = 1; lsiv_detector < psiv_detector; lsiv_detector++) {
            lsiv_card += 1;
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
            }
        }

        // data format for diamond interchange and non-diamond interchange is different
        if (Intersection.mbov_is_diamond_interchange) {
            // set local data for writing data to cards
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATDIA];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01;
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("TX_Det_Data.writeToCards writing " + lstv_name);

            // write data to cards
            switch (msiv_leg) {
                case 0:
                    lstv_leg = "IR";
                    break;

                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                case 6:
                    lstv_leg = " " + Integer.toString(msiv_leg);
                    break;

                case 7:
                    lstv_leg = "IL";
                    break;

                default:
                    Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG] + " = " + msiv_leg
                            + " is < 0 or > 7.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
            }
            if (msiv_leg == (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1)) {
                lstv_leg = "IL";
            }
            Intersection.writeStringToCard(lstv_leg, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_LEG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_fl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            // write data to cards
            Intersection.writeIntToCard(msiv_fl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_FL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_FL] = lsiv_fl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_nl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb - msiv_fl + 1;
            // write data to cards
            Intersection.writeIntToCard(msiv_nl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_NL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATDIA_NL] = lsiv_nl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_spa, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_SPA, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_len, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_LEN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_type, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_TYPE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_delay, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_DELAY, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_extend, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_EXTEND, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_type.equals("CL")) {
                // write data to cards
                Intersection.writeIntToCard(msiv_class_num, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, Intersection.TX_FMT_SIM_DETECT_DATDIA_CLASS_NUM, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_class = 1; lsiv_class <= msiv_class_num; lsiv_class++) {
                    // write data to cards
                    Intersection.writeStringToCard(msta_class_name[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_name - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // write data to cards
                    Intersection.writeIntToCard(msia_class_length_lower[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_llim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    lsiv_ul_min = lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class];
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lclv_tx_fmt.msia_max[lsiv_field_class_ulim - 1 + lsiv_class];
                    }
                    // write data to cards
                    Intersection.writeIntToCard(msia_class_length_lower[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATDIA, lsiv_field_class_ulim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux);
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lsiv_ul_min;
                    }
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }
        else {
            // set local data for writing data to cards
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_DATNDI];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_detector));
            lsiv_field_class_name = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01;
            lsiv_field_class_llim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01;
            lsiv_field_class_ulim = Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01;
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("TX_Det_Data.writeToCards writing " + lstv_name);

            lsiv_leg_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
            // write data to cards
            Intersection.writeIntToCard(msiv_leg, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_LEG] = lsiv_leg_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Data.writeToCards: Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_fl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            // write data to cards
            Intersection.writeIntToCard(msiv_fl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_FL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_FL] = lsiv_fl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            lsiv_nl_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL];
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL] = Intersection.mcla_leg[msiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb - msiv_fl + 1;
            // write data to cards
            Intersection.writeIntToCard(msiv_nl, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_NL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_DETECT_DATNDI_NL] = lsiv_nl_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_spa, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_SPA, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_len, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_LEN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_type, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_TYPE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_delay, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_DELAY, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_extend, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_EXTEND, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_type.equals("CL")) {
                // write data to cards
                Intersection.writeIntToCard(msiv_class_num, lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, Intersection.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_class = 1; lsiv_class <= msiv_class_num; lsiv_class++) {
                    // write data to cards
                    Intersection.writeStringToCard(msta_class_name[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_name - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // write data to cards
                    Intersection.writeIntToCard(msia_class_length_lower[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_llim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    lsiv_ul_min = lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class];
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lclv_tx_fmt.msia_max[lsiv_field_class_ulim - 1 + lsiv_class];
                    }
                    // write data to cards
                    Intersection.writeIntToCard(msia_class_length_lower[lsiv_class], lstv_name, Intersection.TX_FMT_SIM_DETECT_DATNDI, lsiv_field_class_ulim - 1 + lsiv_class,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card + ((lsiv_class + 4) / 5), mclv_aux);
                    if (lsiv_class == msiv_class_num) {
                        lclv_tx_fmt.msia_min[lsiv_field_class_ulim - 1 + lsiv_class] = lsiv_ul_min;
                    }
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class TX_Det_Data

/******************************************************************************/
/* TX_Det_Data.java */
/******************************************************************************/
