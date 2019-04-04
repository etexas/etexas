package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              VMS_Message.java                              */
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

class VMS_Message {

    int msiv_vms_message_type; /* VMS Message type */

    int msiv_vms_message_message; /* VMS Message message */

    double mdfv_vms_message_parameter; /* VMS Message parameter */

    double mdfv_vms_message_start_time; /* VMS Message starting time */

    double mdfv_vms_message_active_time; /* VMS Message active time */

    String mstv_vms_message_inb_out_path; /* VMS Message location */

    /*
     * (I=leg Inbound, L=inbound to center Left, O=leg Outbound, P=intersection Path, R=inbound to
     * center Right)
     */
    int msiv_vms_message_leg_or_path; /*
                                       * VMS Message leg or path number (if
                                       * mstv_vms_message_inb_out_path is I, O, or P)
                                       */

    int msiv_vms_message_lane_beg; /*
                                    * VMS Message beg lane (if mstv_vms_message_inb_out_path is I,
                                    * L, O, or R)
                                    */

    int msiv_vms_message_lane_end; /*
                                    * VMS Message end lane (if mstv_vms_message_inb_out_path is I,
                                    * L, O, or R)
                                    */

    double mdfv_vms_message_pos_beg; /* VMS Message beg position */

    double mdfv_vms_message_pos_end; /* VMS Message end position */

    int msiv_vms_message_vehicle_num; /* VMS Message vehicle number (0=all) */

    String mstv_vms_message_dist_name; /*
                                        * VMS Message reaction time distribution name
                                        */

    double mdfv_vms_message_dist_mean; /*
                                        * VMS Message reaction time distribution mean (seconds)
                                        */

    double mdfv_vms_message_dist_param; /*
                                         * VMS Message reaction time distribution parameter
                                         */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_VMS_MESSAGE]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].msiv_vms_message_type
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_TYPE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].msiv_vms_message_message
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_MESSAGE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_parameter
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_PARAMETER ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_start_time
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_START_TIME ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_active_time
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mstv_vms_message_inb_out_path
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].msiv_vms_message_leg_or_path
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].msiv_vms_message_lane_beg
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_LANE_BEG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].msiv_vms_message_lane_end
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_LANE_END ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_pos_beg
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_POS_BEG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_pos_end
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_POS_END ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].msiv_vms_message_vehicle_num
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mstv_vms_message_dist_name
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_DIST_NAME ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_dist_mean
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms_message].mdfv_vms_message_dist_param
    // .mclv_aux.msia_stat[TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM ]

    public VMS_Message() {
        mclv_aux = new TX_Aux();
    } // end of method VMS_Message

    public void checkForErrors(int psiv_vms_message) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all VMS_Message data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("VMS_Message.checkForErrors");
        // check if num VMS messages is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vms_message < 1) || (psiv_vms_message > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages)) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.checkForErrors: psiv_vms_message = " + psiv_vms_message + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VMS_MESSAGE];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("VMS_Message.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in VMS_Message.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in VMS_Message.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_vms_message) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lines;
        String lstv_name;

        // printToFile prints all VMS_Message data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("VMS_Message.printToFile");
        // check if num VMS messages is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vms_message < 1) || (psiv_vms_message > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages)) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.printToFile: psiv_vms_message = " + psiv_vms_message + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_vms_message_dist_name);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VMS_MESSAGE];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("VMS_Message.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        /* VMS Message type */
        /* VMS Message type detail */
        /* VMS Message message */
        /* VMS Message message detail */
        /* VMS Message starting time */
        /* VMS Message active time */
        /* VMS Message location */
        /* VMS Message beg position */
        /* VMS Message end position */
        /* VMS Message vehicle number (0=all) */
        /* VMS Message reaction time distribution name */
        /* VMS Message reaction time distribution mean (seconds) */
        lsiv_lines = 12;
        /* VMS Message parameter */
        /* VMS Message parameter detail */
        switch (msiv_vms_message_message) {
            case PARAMS.TEXAS_MODEL_VMSMAN: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using normal
                                             * acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMAM: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using maximum
                                             * vehicle acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSL: /*
                                             * TEXAS Model Vehicle Message System message - stop at
                                             * location xx
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSC: /*
                                             * TEXAS Model Vehicle Message System message - stop
                                             * immediately using collision deceleration xx
                                             */
                lsiv_lines += 2;
                break;
        }
        /*
         * VMS Message leg or path number (if mstv_vms_message_inb_out_path is I, O, or P)
         */
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("P"))) {
            lsiv_lines++;
        }
        /*
         * VMS Message beg lane (if mstv_vms_message_inb_out_path is I, L, O, or R)
         */
        /*
         * VMS Message end lane (if mstv_vms_message_inb_out_path is I, L, O, or R)
         */
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("L")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("R"))) {
            lsiv_lines += 2;
        }
        /* VMS Message reaction time distribution parameter */
        if (Intersection.mbov_is_hd_parameter_needed) {
            lsiv_lines++;
        }
        Intersection.filesPrintSIM_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_vms_message_type, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_TYPE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        switch (msiv_vms_message_type) {
            case PARAMS.TEXAS_MODEL_VMSTDD:
                Intersection.filesPrintSIM_CommentToFile("Message Type - Driver  DMS", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSTDI:
                Intersection.filesPrintSIM_CommentToFile("Message Type - Driver  IVDMS", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSTVI:
                Intersection.filesPrintSIM_CommentToFile("Message Type - Vehicle IVDMS", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            default:
                Intersection.filesPrintSIM_CommentToFile("Message Type - UNKNOWN", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_vms_message_message, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_MESSAGE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        switch (msiv_vms_message_message) {
            case PARAMS.TEXAS_MODEL_VMSMAN:
                Intersection.filesPrintSIM_CommentToFile("Message - Accelerate or Decelerate to Speed XX using Normal Acceleration or Deceleration", Intersection.GDVSIM_PRINT_LEFT_MARGIN
                        + Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMAM:
                Intersection.filesPrintSIM_CommentToFile("Message - Accelerate or Decelerate to Speed XX using Maximum Vehicle Acceleration or Deceleration", Intersection.GDVSIM_PRINT_LEFT_MARGIN
                        + Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMSI:
                Intersection.filesPrintSIM_CommentToFile("Message - Stop at the Intersection Stop Line", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMSL:
                Intersection.filesPrintSIM_CommentToFile("Message - Stop at Location XX", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMSM:
                Intersection.filesPrintSIM_CommentToFile("Message - Stop Immediately using Maximum Vehicle Deceleration", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMSC:
                Intersection.filesPrintSIM_CommentToFile("Message - Stop Immediately using Collision Deceleration XX", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMCL:
                Intersection.filesPrintSIM_CommentToFile("Message - Change Lanes to the Left", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMCR:
                Intersection.filesPrintSIM_CommentToFile("Message - Change Lanes to the Right", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMGO:
                Intersection.filesPrintSIM_CommentToFile("Message - Forced Go", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMRR:
                Intersection.filesPrintSIM_CommentToFile("Message - Forced Run the Red Signal", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMDD:
                Intersection.filesPrintSIM_CommentToFile("Message - Distracted Driver", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            default:
                Intersection.filesPrintSIM_CommentToFile("Message - UNKNOWN", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        switch (msiv_vms_message_message) {
            case PARAMS.TEXAS_MODEL_VMSMAN: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using normal
                                             * acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMAM: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using maximum
                                             * vehicle acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSL: /*
                                             * TEXAS Model Vehicle Message System message - stop at
                                             * location xx
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSC: /*
                                             * TEXAS Model Vehicle Message System message - stop
                                             * immediately using collision deceleration xx
                                             */
                Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_parameter, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_PARAMETER, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                break;
        }

        switch (msiv_vms_message_message) {
            case PARAMS.TEXAS_MODEL_VMSMAN:
                Intersection.filesPrintSIM_CommentToFile("Parameter - Desired Speed (mph)", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMAM:
                Intersection.filesPrintSIM_CommentToFile("Parameter - Desired Speed (mph)", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMSL:
                Intersection.filesPrintSIM_CommentToFile("Parameter - Location (feet)", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;

            case PARAMS.TEXAS_MODEL_VMSMSC:
                Intersection.filesPrintSIM_CommentToFile("Parameter - Collision Deceleration (ft/sec/sec)", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
                break;
        }
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_start_time, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_START_TIME, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_active_time, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_vms_message_inb_out_path, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("P"))) {
            Intersection.filesPrintSIM_IntToFile(msiv_vms_message_leg_or_path, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("L")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("R"))) {
            Intersection.filesPrintSIM_IntToFile(msiv_vms_message_lane_beg, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_vms_message_lane_end, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LANE_END, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_pos_beg, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_POS_BEG, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_pos_end, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_POS_END, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_vms_message_vehicle_num, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_vms_message_dist_name, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_NAME, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_dist_mean, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        if (Intersection.mbov_is_hd_parameter_needed) {
            Intersection.filesPrintSIM_DoubleToFile(mdfv_vms_message_dist_param, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_vms_message) {
        boolean lbov_force_default;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_detector;
        int lsiv_first_con_card;
        int lsiv_first_det_card;
        int lsiv_first_grn_int_seq_card;
        int lsiv_lane_cont_card;
        int lsiv_nema_ovlp_def_card;
        int lsiv_nema_ph_diag_card;
        int lsiv_nema_ph_ov_tx_card;
        int lsiv_nema_ring_grp_card;
        int lsiv_no_of_det;
        int lsiv_no_of_phases;
        int lsiv_par_opt_card;
        int lsiv_par_opt_2_card;
        int lsiv_precyclen_diafig_nemaovlp;
        int lsiv_psfps_nemam_diaint_card;
        int lsiv_tex_dia_opt_card;
        int lsiv_timing_card;
        String lstv_message;
        String lstv_name;

        // readFromCards reads all VMS_Message fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("VMS_Message.readFromCards");
        // check if num VMS messages is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vms_message < 1) || (psiv_vms_message > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages)) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: psiv_vms_message = " + psiv_vms_message + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VMS_MESSAGE];
        lsiv_card = 0;

        // if intersection control is signal controlled
        if (Intersection.mbov_is_ic_signal_controlled) {
            // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of phases is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check number of phases for errors
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            if (Intersection.mbov_is_ic_PRETIMED) {
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_PHASE_SEQ
            }
            else if (Intersection.mbov_is_ic_SEMI_ACT) {
                // check if number of detectors is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_first_det_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_PHASE_SEQ
                lsiv_first_con_card = lsiv_first_det_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        lsiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                lsiv_card = lsiv_first_con_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONSFA
            }
            else if (Intersection.mbov_is_ic_FULL_ACT) {
                // check if number of detectors is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_first_det_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_PHASE_SEQ
                lsiv_first_con_card = lsiv_first_det_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        lsiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                lsiv_card = lsiv_first_con_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONSFA
            }
            else if (Intersection.mbov_is_ic_TEX_DIA) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check diamond figure number of errors
                if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                        && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                        && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                        && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if number of detectors is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_tex_dia_opt_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card + 4; // TX_FMT_SIM_TEX_DIA_INT
                lsiv_nema_ph_ov_tx_card = 0;
                lsiv_nema_ring_grp_card = 0;
                lsiv_nema_ph_diag_card = 0;
                lsiv_nema_ovlp_def_card = 0;
                lsiv_first_grn_int_seq_card = lsiv_tex_dia_opt_card + 4; // TX_FMT_SIM_TEX_DIA_OPT
                lsiv_first_det_card = lsiv_first_grn_int_seq_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_GREEN_INT_SEQ
                lsiv_first_con_card = 0;
                lsiv_card = lsiv_first_det_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; // TX_FMT_SIM_DETECT_DATDIA
                for (lsiv_detector = 1; lsiv_detector <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
            }
            else if (Intersection.mbov_is_ic_NEMA) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check NEMA/HARDWARE number of overlaps for errors
                if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 1)
                        || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 1 or > " + PARAMS.TEXAS_MODEL_NON + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if number of detectors is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_nema_ph_ov_tx_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card + 1; // TX_FMT_SIM_NEMA_MOVEMENT
                lsiv_nema_ring_grp_card = lsiv_nema_ph_ov_tx_card + 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0) {
                    lsiv_nema_ring_grp_card += 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                    lsiv_nema_ph_diag_card = lsiv_nema_ring_grp_card + 1; // TX_FMT_SIM_NEMA_RING_GRP
                    lsiv_nema_ovlp_def_card = lsiv_nema_ph_diag_card + 1; // TX_FMT_SIM_NEMA_PH_DIAG
                    lsiv_first_grn_int_seq_card = lsiv_nema_ovlp_def_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; // TX_FMT_SIM_NEMA_OVLP_DEF
                }
                else {
                    lsiv_nema_ph_diag_card = 0;
                    lsiv_nema_ovlp_def_card = 0;
                    lsiv_first_grn_int_seq_card = lsiv_nema_ring_grp_card + 1; // TX_FMT_SIM_NEMA_RING_GRP
                }
                lsiv_first_det_card = lsiv_first_grn_int_seq_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + // TX_FMT_SIM_GREEN_INT_SEQ
                        Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; // TX_FMT_SIM_GREEN_INT_SEQ
                lsiv_first_con_card = lsiv_first_det_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        lsiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                lsiv_card = lsiv_first_con_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONN2H
            }
            else if (Intersection.mbov_is_ic_HARDWARE) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check NEMA/HARDWARE number of overlaps for errors
                if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 1)
                        || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_HOV)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 1 or > " + PARAMS.TEXAS_MODEL_HOV + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if number of detectors is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_nema_ph_ov_tx_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card + 1; // TX_FMT_SIM_NEMA_MOVEMENT
                lsiv_nema_ring_grp_card = 0;
                lsiv_nema_ph_diag_card = 0;
                lsiv_nema_ovlp_def_card = 0;
                lsiv_first_grn_int_seq_card = lsiv_nema_ph_ov_tx_card + 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0) {
                    lsiv_first_grn_int_seq_card += 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                }
                lsiv_first_det_card = lsiv_first_grn_int_seq_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + // TX_FMT_SIM_GREEN_INT_SEQ
                        Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; // TX_FMT_SIM_GREEN_INT_SEQ
                lsiv_first_con_card = lsiv_first_det_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        lsiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                lsiv_card = lsiv_first_con_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONN2H
            }
        } // end if intersection control is signal controlled
        else {
            // check if title card number is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in VMS_Message.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            lsiv_timing_card = 0;
            lsiv_precyclen_diafig_nemaovlp = 0;
            lsiv_no_of_phases = 0;
            lsiv_first_grn_int_seq_card = 0;
            lsiv_psfps_nemam_diaint_card = 0;
            lsiv_first_det_card = 0;
            lsiv_no_of_det = 0;
            lsiv_first_con_card = 0;
            lsiv_par_opt_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card + 1; // TX_FMT_SIM_TITLE
            lsiv_par_opt_2_card = lsiv_par_opt_card + 1; // TX_FMT_SIM_PAR_OPT
            lsiv_lane_cont_card = lsiv_par_opt_2_card + 1; // TX_FMT_SIM_PAR_OPT_2
            lsiv_tex_dia_opt_card = 0;
            lsiv_nema_ph_ov_tx_card = 0;
            lsiv_nema_ring_grp_card = 0;
            lsiv_nema_ph_diag_card = 0;
            lsiv_nema_ovlp_def_card = 0;
            lsiv_card = lsiv_lane_cont_card + 1; // TX_FMT_SIM_LANE_CONT
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VMS_MESSAGE];
        lsiv_card = lsiv_card + psiv_vms_message - 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("VMS_Message.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_vms_message_type = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_TYPE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_vms_message_message = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_MESSAGE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        switch (msiv_vms_message_message) {
            case PARAMS.TEXAS_MODEL_VMSMAN: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using normal
                                             * acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMAM: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using maximum
                                             * vehicle acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSL: /*
                                             * TEXAS Model Vehicle Message System message - stop at
                                             * location xx
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSC: /*
                                             * TEXAS Model Vehicle Message System message - stop
                                             * immediately using collision deceleration xx
                                             */
                lbov_force_default = false;
                break;

            default:
                lbov_force_default = true;
                break;
        }
        mdfv_vms_message_parameter = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_PARAMETER, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_vms_message_start_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_START_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_vms_message_active_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_vms_message_inb_out_path = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH,
                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY,
                Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("P"))) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msiv_vms_message_leg_or_path = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("L")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("R"))) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msiv_vms_message_lane_beg = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_vms_message_lane_end = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LANE_END, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_vms_message_pos_beg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_POS_BEG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_vms_message_pos_end = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_POS_END, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_vms_message_vehicle_num = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_vms_message_dist_name = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_NAME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_vms_message_dist_name);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_vms_message_dist_mean = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (Intersection.mbov_is_hd_parameter_needed) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        mdfv_vms_message_dist_param = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (msiv_vms_message_type == PARAMS.TEXAS_MODEL_VMSTVI) {
            lstv_message = "UNKNOWN";
            switch (msiv_vms_message_message) {
                case PARAMS.TEXAS_MODEL_VMSMCL: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the left
                                                 */
                    lstv_message = "change lanes to the left";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMCR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the right
                                                 */
                    lstv_message = "change lanes to the right";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMGO: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced go
                                                 */
                    lstv_message = "forced go";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMRR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced run the red signal
                                                 */
                    lstv_message = "forced run the red signal";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMDD: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * distracted driver
                                                 */
                    lstv_message = "distracted driver";
                    break;
            }
            switch (msiv_vms_message_message) {
                case PARAMS.TEXAS_MODEL_VMSMCL: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the left
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMCR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the right
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMGO: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced go
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMRR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced run the red signal
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMDD: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * distracted driver
                                                 */
                    Intersection.mstv_warningMessage = "Warning in VMS_Message.readFromCards: VMS Message " + psiv_vms_message + " Message=" + msiv_vms_message_message + "='" + lstv_message
                            + "' is invalid for IVDMS Message.";
                    Intersection.warningMessage();
                    break;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all VMS_Message fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("VMS_Message.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_vms_message) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_message;
        String lstv_name;

        // writeToCards writes all VMS_Message fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("VMS_Message.writeToCards");
        // check if num VMS messages is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vms_message < 1) || (psiv_vms_message > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages)) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.writeToCards: psiv_vms_message = " + psiv_vms_message + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if VMS message card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_VMS_MESSAGE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in VMS_Message.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_vms_message_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_vms_message_dist_name);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (msiv_vms_message_type == PARAMS.TEXAS_MODEL_VMSTVI) {
            lstv_message = "UNKNOWN";
            switch (msiv_vms_message_message) {
                case PARAMS.TEXAS_MODEL_VMSMCL: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the left
                                                 */
                    lstv_message = "change lanes to the left";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMCR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the right
                                                 */
                    lstv_message = "change lanes to the right";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMGO: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced go
                                                 */
                    lstv_message = "forced go";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMRR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced run the red signal
                                                 */
                    lstv_message = "forced run the red signal";
                    break;

                case PARAMS.TEXAS_MODEL_VMSMDD: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * distracted driver
                                                 */
                    lstv_message = "distracted driver";
                    break;
            }
            switch (msiv_vms_message_message) {
                case PARAMS.TEXAS_MODEL_VMSMCL: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the left
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMCR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * change lanes to the right
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMGO: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced go
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMRR: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * forced run the red signal
                                                 */
                case PARAMS.TEXAS_MODEL_VMSMDD: /*
                                                 * TEXAS Model Vehicle Message System message -
                                                 * distracted driver
                                                 */
                    Intersection.mstv_errorMessage = "Error in VMS_Message.writeToCards: VMS Message " + psiv_vms_message + " Message=" + msiv_vms_message_message + "='" + lstv_message
                            + "' is invalid for IVDMS Message.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
            }
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VMS_MESSAGE];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_vms_message_card + psiv_vms_message - 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("VMS_Message.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_vms_message_type, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_TYPE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_vms_message_message, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_MESSAGE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        switch (msiv_vms_message_message) {
            case PARAMS.TEXAS_MODEL_VMSMAN: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using normal
                                             * acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMAM: /*
                                             * TEXAS Model Vehicle Message System message -
                                             * accelerate or decelerate to speed xx using maximum
                                             * vehicle acceleration or deceleration
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSL: /*
                                             * TEXAS Model Vehicle Message System message - stop at
                                             * location xx
                                             */
            case PARAMS.TEXAS_MODEL_VMSMSC: /*
                                             * TEXAS Model Vehicle Message System message - stop
                                             * immediately using collision deceleration xx
                                             */
                Intersection.writeDoubleToCard(mdfv_vms_message_parameter, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_PARAMETER,
                        Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                break;
        }

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_vms_message_start_time, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_START_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_vms_message_active_time, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_vms_message_inb_out_path, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("P"))) {
            Intersection.writeIntToCard(msiv_vms_message_leg_or_path, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if ((mstv_vms_message_inb_out_path.equals("I")) || (mstv_vms_message_inb_out_path.equals("L")) || (mstv_vms_message_inb_out_path.equals("O")) || (mstv_vms_message_inb_out_path.equals("R"))) {
            Intersection.writeIntToCard(msiv_vms_message_lane_beg, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_vms_message_lane_end, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_LANE_END, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_vms_message_pos_beg, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_POS_BEG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_vms_message_pos_end, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_POS_END, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_vms_message_vehicle_num, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_vms_message_dist_name, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_NAME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_vms_message_dist_mean, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        if (Intersection.mbov_is_hd_parameter_needed) {
            Intersection.writeDoubleToCard(mdfv_vms_message_dist_param, lstv_name, Intersection.TX_FMT_SIM_VMS_MESSAGE, Intersection.TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class VMS_Message

/******************************************************************************/
/* VMS_Message.java */
/******************************************************************************/
