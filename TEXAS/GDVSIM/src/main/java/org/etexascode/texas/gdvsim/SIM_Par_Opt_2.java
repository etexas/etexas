package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             SIM_Par_Opt_2.java                             */
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

class SIM_Par_Opt_2 {

    int msiv_ds_speed; /*
                        * speed below which a special delay statistic is collected
                        */

    int msiv_queue_dist; /* maximum clear distance for being in a queue */

    double mdfv_lambda; /* car following equation parameter lambda */

    double mdfv_mu; /* car following equation parameter mu */

    int msiv_alpha; /* car following equation parameter alpha */

    double mdfv_t_lead; /* time for lead zone used in conflict checking */

    double mdfv_t_lag; /* time for lag zone used in conflict checking */

    double mdfv_hesitation_factor; /* hesitation factor */

    String mstv_majcol_stop; /* major collision - stop on major collision */

    int msiv_majcol_percent_des_speed; /*
                                        * major collision - percent of desired speed for passing
                                        * major collision
                                        */

    int msiv_majcol_evasive_act_mean; /*
                                       * major collision - evasive action time mean
                                       */

    int msiv_majcol_evasive_act_stddev; /*
                                         * major collision - evasive action time standard deviation
                                         */

    String mstv_enable_ssam_file; /* enable ssam file */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_PAR_OPT_2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_ds_speed
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_DS_SPEED ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_queue_dist
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_lambda
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_LAMBDA ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_mu
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_MU ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_alpha
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_ALPHA ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_t_lead
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_T_LEAD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_t_lag
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_T_LAG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_hesitation_factor
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mstv_majcol_stop
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_percent_des_speed
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_evasive_act_mean
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_evasive_act_stddev
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mstv_enable_ssam_file
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_2_SSAM_FILE ]

    public SIM_Par_Opt_2() {
        mclv_aux = new TX_Aux();
    } // end of method SIM_Par_Opt_2

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all SIM_Par_Opt_2 data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Par_Opt_2.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT_2];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Par_Opt_2.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in SIM_Par_Opt_2.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SIM_Par_Opt_2.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile() {
        TX_Fmt lclv_tx_fmt = null;
        String lstv_name;

        // printToFile prints all SIM_Par_Opt_2 data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("SIM_Par_Opt_2.printToFile");
        // set local data for printing data to file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT_2];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("SIM_Par_Opt_2.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintSIM_check_newpage(13);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_ds_speed, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_DS_SPEED, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_queue_dist, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_lambda, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_LAMBDA, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_mu, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MU, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_alpha, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_ALPHA, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_t_lead, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_T_LEAD, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_t_lag, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_T_LAG, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_hesitation_factor, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_majcol_stop, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_majcol_evasive_act_mean, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_majcol_evasive_act_stddev, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_enable_ssam_file, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all SIM_Par_Opt_2 fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Par_Opt_2.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt_2.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt_2.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // SIM_Par_Opt_2 card is always after the SIM_Par_Opt card after the SIM_Title card
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT_2];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card + 2;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Par_Opt_2.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_ds_speed = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_DS_SPEED, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_queue_dist = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_lambda = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_LAMBDA, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_mu = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MU, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_alpha = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_ALPHA, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_t_lead = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_T_LEAD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_t_lag = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_T_LAG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_hesitation_factor = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_majcol_stop = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_majcol_percent_des_speed = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_majcol_evasive_act_mean = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_majcol_evasive_act_stddev = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_enable_ssam_file = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all SIM_Par_Opt_2 fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("SIM_Par_Opt_2.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all SIM_Par_Opt_2 fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Par_Opt_2.writeToCards");
        // check if sim par opt 2 card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PAR_OPT_2_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt_2.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_par_opt_2_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT_2];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_par_opt_2_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Par_Opt_2.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_ds_speed, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_DS_SPEED, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_queue_dist, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_lambda, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_LAMBDA, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_mu, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MU, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_alpha, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_ALPHA, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_t_lead, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_T_LEAD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_t_lag, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_T_LAG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_hesitation_factor, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_majcol_stop, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_majcol_percent_des_speed, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_majcol_evasive_act_mean, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_majcol_evasive_act_stddev, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_enable_ssam_file, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT_2, Intersection.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class SIM_Par_Opt_2

/******************************************************************************/
/* SIM_Par_Opt_2.java */
/******************************************************************************/
