package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                        Varying_Traffic_Period.java                         */
/******************************************************************************/
/*                                                                            */
/*     gdvsim COPYRIGHT (C) 2007 by Rioux Engineering, Austin, Texas USA      */
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

class Varying_Traffic_Period {

    int msiv_period_duration; /* LVTP period duration (minutes) */

    int msiv_pct_lt; /*
                      * FPERLV percent of left turning vehicles to enter in median lane
                      */

    int msiv_pct_rt; /*
                      * FPERRV percent of right turning vehicles to enter in curb lane
                      */

    int msiv_percent_using; /*
                             * FUTPRV percent using diamond interchange free u-turn lane
                             */

    int msia_pct[]; /* XPERLV percent of traffic to enter on the lane */

    String mstv_dist; /*
                       * CDISTV name for inbound traffic headway frequency distribution
                       */

    int msiv_vol; /* JVOLV total hourly volume on leg during period */

    double mdfv_par; /* PDISTV parameter for headway frequency distribution */

    double mdfv_mean; /* XMEANV mean speed of entering vehicles */

    double mdfv_85th; /* X85PRV 85th percentile speed of entering vehicles */

    int msia_dest_per[]; /*
                          * YPERTV outbound traffic destination percentages by leg
                          */

    String mstv_tm; /* IYES traffic mix data to follow ? */

    Traf_Mix mclv_mix; /* inbound traffic vehicle class percentages */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VAR_TRAF_PER]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_period_duration
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_pct_lt
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_LT ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_pct_rt
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_RT ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_percent_using
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_1 ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_2 ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_3 ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_4 ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_5 ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PCT_6 ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_dist
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DIST ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_vol
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_VOL ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_par
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_PAR ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_mean
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_MEAN ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_85th
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_85TH ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_TM ]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_mix
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 ]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VAR_TRAF_PER_TM_PER_99 ]

    // mdfv_par can also be read/printed using
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_HDWAY_PARAM]
    // [TX_FMT_GDV_HDWAY_PARAM_CONSTAN]
    // [TX_FMT_GDV_HDWAY_PARAM_ERLANG ]
    // [TX_FMT_GDV_HDWAY_PARAM_GAMMA ]
    // [TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
    // [TX_FMT_GDV_HDWAY_PARAM_NEGEXP ]
    // [TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
    // [TX_FMT_GDV_HDWAY_PARAM_UNIFORM]

    public Varying_Traffic_Period() {
        msia_pct = new int[PARAMS.TEXAS_MODEL_NAL + 1];
        msia_dest_per = new int[PARAMS.TEXAS_MODEL_NLG + 1];
        mclv_mix = new Traf_Mix();
        mclv_aux = new TX_Aux();
    } // end of method Varying_Traffic_Period

    public void checkForErrors(int psiv_leg, // leg number
            int psiv_vtp // varying traffic period
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_nf;
        String lstv_name;

        // checkForErrors checks all Varying_Traffic_Period data for invalid data and data with
        // errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Varying_Traffic_Period.checkForErrors");
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.checkForErrors: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vtp < 1) || (psiv_vtp > Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.checkForErrors: psiv_vtp = " + psiv_vtp + " is < 1 or > "
                    + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VAR_TRAF_PER];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_vtp));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        if (mstv_tm.equals("YES")) {
            lsiv_nf = Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 - 1 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
        }
        else {
            lsiv_nf = Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM;
        }

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Varying_Traffic_Period.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lsiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Varying_Traffic_Period.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg, // leg number
            int psiv_vtp // varying traffic period
    ) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_per_veh_class;
        int lsiv_lane;
        int lsiv_leg;
        int lsiv_lines;
        int lsiv_member;
        int lsiv_veh;
        String lstv_name;

        // printToFile prints all Varying_Traffic_Period data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Varying_Traffic_Period.printToFile psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vtp < 1) || (psiv_vtp > Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: psiv_vtp = " + psiv_vtp + " is < 1 or > "
                    + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
            System.out.println("Varying_Traffic_Period.printToFile checking " + lstv_name);

        lsiv_member = 0;
        if (Intersection.mbov_is_hd_ERLANG) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_GAMMA) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_LOGNRML) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_SNEGEXP) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_UNIFORM) {
            lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM;
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[lsiv_member]) || (mdfv_par > lclv_tx_fmt.mdfa_max[lsiv_member])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member] + " = " + mdfv_par + " is < "
                        + lclv_tx_fmt.mdfa_min[lsiv_member] + " or > " + lclv_tx_fmt.mdfa_max[lsiv_member] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VAR_TRAF_PER];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_vtp));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Varying_Traffic_Period.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        lsiv_lines = 9 + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        if (Intersection.mbov_is_hd_parameter_needed) {
            lsiv_lines++;
        }
        if (mstv_tm.equals("YES")) {
            lsiv_lines += Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
        }
        Intersection.filesPrintGDV_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_period_duration, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_pct_lt, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_pct_rt, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_percent_using, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_lane = 1; lsiv_lane <= Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb; lsiv_lane++) {
            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_pct[lsiv_lane], lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_1 - 1 + lsiv_lane, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_dist, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_DIST, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_vol, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_VOL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
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
        Intersection.filesPrintGDV_DoubleToFile(mdfv_mean, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_MEAN, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_85th, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_85TH, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_leg = 1; lsiv_leg <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[lsiv_leg], lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1 - 1 + lsiv_leg, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_tm, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mstv_tm.equals("YES")) {
            ldfv_sum_per_veh_class = 0.0;
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                Intersection.filesPrintGDV_DoubleToFile(mclv_mix.mdfa_per_veh_class[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 - 1
                        + lsiv_veh, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE,
                        Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                ldfv_sum_per_veh_class += mclv_mix.mdfa_per_veh_class[lsiv_veh];
            }

            // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 then
            // warning
            // special code that must also be performed by the menu system
            if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) {
                Intersection.mstv_warningMessage = "Warning in Varying_Traffic_Period.printToFile: Sum of percent of inbound traffic in vehicle classes on Leg " + psiv_leg + " = "
                        + ldfv_sum_per_veh_class + " is not 100.0.";
                Intersection.warningMessage();
                for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                    mclv_mix.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DATA_ERROR;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg, // leg number
            int psiv_vtp // varying traffic period
    ) {
        boolean lbov_force_default;
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_per_veh_class;
        int lsiv_card;
        int lsiv_fo;
        int lsiv_fo_save;
        int lsiv_lane;
        int lsiv_leg;
        int lsiv_member;
        int lsiv_tm_card;
        int lsiv_veh;
        int lsiv_vtp;
        String lstv_name;

        // readFromCards reads all Varying_Traffic_Period fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Varying_Traffic_Period.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vtp < 1) || (psiv_vtp > Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: psiv_vtp = " + psiv_vtp + " is < 1 or > "
                    + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 1
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15) + 2);
        for (lsiv_vtp = 1; lsiv_vtp < psiv_vtp; lsiv_vtp++) {
            lsiv_card++;
            if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                lsiv_card += ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15);
            }
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VAR_TRAF_PER];
        lsiv_fo = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_VAR_TRAF_PER_PAR];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_vtp));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Varying_Traffic_Period.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_period_duration = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_pct_lt = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_pct_rt = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_percent_using = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lbov_force_default = false;
        for (lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            // read data from cards
            if (lsiv_lane > Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb)
                lbov_force_default = true;
            msia_pct[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_1 - 1 + lsiv_lane,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // read data from cards
        mstv_dist = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_DIST, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_dist);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_vol = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_VOL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_hd_parameter_needed) {
            // read data from cards
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PAR, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else {
            mdfv_par = 0.0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_VAR_TRAF_PER_PAR] = Intersection.TX_SET_BY_SOFTWARE;
        }

        // read data from cards
        mdfv_mean = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_MEAN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_85th = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_85TH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lbov_force_default = false;
        for (lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            // read data from cards
            if (lsiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)
                lbov_force_default = true;
            msia_dest_per[lsiv_leg] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1 - 1 + lsiv_leg,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // read data from cards
        mstv_tm = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mstv_tm.equals("YES")) {
            ldfv_sum_per_veh_class = 0.0;
            lsiv_tm_card = lsiv_card + 1;
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                if ((lsiv_veh >= 16) && ((((lsiv_veh - 1) / 15) * 15) == (lsiv_veh - 1)))
                    lsiv_tm_card++;
                mclv_mix.mdfa_per_veh_class[lsiv_veh] = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 - 1 + lsiv_veh,
                        Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_tm_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                ldfv_sum_per_veh_class += mclv_mix.mdfa_per_veh_class[lsiv_veh];
            }

            // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 then
            // warning
            // special code that must also be performed by the menu system
            if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) {
                Intersection.mstv_warningMessage = "Warning in Varying_Traffic_Period.readFromCards: Sum of percent of inbound traffic in vehicle classes on Leg " + psiv_leg + " = "
                        + ldfv_sum_per_veh_class + " is not 100.0.";
                Intersection.warningMessage();
                for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                    mclv_mix.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DATA_ERROR;
                }
            }
        }

        // set local data for reading data from cards to re-read traffic headway parameter
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Varying_Traffic_Period.readFromCards reading " + lstv_name);

        if (Intersection.mbov_is_hd_CONSTAN) {
            mdfv_par = 0.0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_VAR_TRAF_PER_PAR] = Intersection.TX_SET_BY_SOFTWARE;
        }
        else if (Intersection.mbov_is_hd_ERLANG) {
            lsiv_fo_save = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = lsiv_fo;
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = lsiv_fo_save;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_GAMMA) {
            lsiv_fo_save = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = lsiv_fo;
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = lsiv_fo_save;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_LOGNRML) {
            lsiv_fo_save = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = lsiv_fo;
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = lsiv_fo_save;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_NEGEXP) {
            mdfv_par = 0.0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_VAR_TRAF_PER_PAR] = Intersection.TX_SET_BY_SOFTWARE;
        }
        else if (Intersection.mbov_is_hd_SNEGEXP) {
            lsiv_fo_save = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = lsiv_fo;
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = lsiv_fo_save;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_hd_UNIFORM) {
            lsiv_fo_save = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = lsiv_fo;
            mdfv_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = lsiv_fo_save;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Varying_Traffic_Period fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Varying_Traffic_Period.setAllInvalid");
        mclv_mix.setAllInvalid();
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg, // leg number
            int psiv_vtp // varying traffic period
    ) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_per_veh_class;
        int lsiv_card;
        int lsiv_lane;
        int lsiv_leg;
        int lsiv_member;
        int lsiv_veh;
        int lsiv_vtp;
        String lstv_name;

        // writeToCards writes all Varying_Traffic_Period fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Varying_Traffic_Period.writeToCards");
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_vtp < 1) || (psiv_vtp > Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods)) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: psiv_vtp = " + psiv_vtp + " is < 1 or > "
                    + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 1
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15) + 2);
        for (lsiv_vtp = 1; lsiv_vtp < psiv_vtp; lsiv_vtp++) {
            lsiv_card++;
            if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                lsiv_card += ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15);
            }
        }

        // check and set headway distribution Intersection.mbov_is_hd_* values
        Intersection.check_and_set_headway_distribution(mstv_dist);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data to check traffic headway parameter
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Varying_Traffic_Period.writeToCards checking " + lstv_name);

        if (Intersection.mbov_is_hd_ERLANG) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " or > "
                        + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_GAMMA) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " or > "
                        + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_LOGNRML) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                        + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_SNEGEXP) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                        + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }
        else if (Intersection.mbov_is_hd_UNIFORM) {
            if ((mdfv_par < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]) || (mdfv_par > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM])) {
                Intersection.mstv_errorMessage = "Error in Varying_Traffic_Period.writeToCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " = " + mdfv_par + " is < " + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                        + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VAR_TRAF_PER];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_vtp));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Varying_Traffic_Period.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_period_duration, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_pct_lt, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_pct_rt, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_percent_using, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_lane = 1; lsiv_lane <= Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb; lsiv_lane++) {
            // write data to cards
            Intersection.writeIntToCard(msia_pct[lsiv_lane], lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PCT_1 - 1 + lsiv_lane,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.writeStringToCard(mstv_dist, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_DIST, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_vol, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_VOL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_hd_parameter_needed) {
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_par, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_PAR, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_mean, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_MEAN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_85th, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_85TH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_leg = 1; lsiv_leg <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            // write data to cards
            Intersection.writeIntToCard(msia_dest_per[lsiv_leg], lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1 - 1 + lsiv_leg,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.writeStringToCard(mstv_tm, lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mstv_tm.equals("YES")) {
            ldfv_sum_per_veh_class = 0.0;
            lsiv_card++;
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                if ((lsiv_veh >= 16) && ((((lsiv_veh - 1) / 15) * 15) == (lsiv_veh - 1)))
                    lsiv_card++;
                Intersection.writeDoubleToCard(mclv_mix.mdfa_per_veh_class[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VAR_TRAF_PER, Intersection.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 - 1 + lsiv_veh,
                        Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                ldfv_sum_per_veh_class += mclv_mix.mdfa_per_veh_class[lsiv_veh];
            }

            // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 then
            // warning
            // special code that must also be performed by the menu system
            if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) {
                Intersection.mstv_warningMessage = "Warning in Varying_Traffic_Period.writeToCards: Sum of percent of inbound traffic in vehicle classes on Leg " + psiv_leg + " = "
                        + ldfv_sum_per_veh_class + " is not 100.0.";
                Intersection.warningMessage();
                for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                    mclv_mix.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DATA_ERROR;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Varying_Traffic_Period

/******************************************************************************/
/* Varying_Traffic_Period.java */
/******************************************************************************/
