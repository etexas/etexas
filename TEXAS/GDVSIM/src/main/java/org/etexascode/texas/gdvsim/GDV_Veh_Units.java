package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_Veh_Units.java                             */
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

class GDV_Veh_Units {

    int msiv_veh_num_units; /* User-defined vehicle number of units */

    int msia_veh_unit_length[]; /* User-defined vehicle unit length */

    int msia_veh_unit_width[]; /* User-defined vehicle unit width */

    int msia_veh_unit_draw_seq[]; /* User-defined vehicle unit drawing sequence */

    int msia_veh_unit_fpd[]; /* User-defined vehicle unit front point distance */

    int msia_veh_unit_rpd[]; /* User-defined vehicle unit rear point distance */

    int msia_veh_unit_rhpd[]; /*
                               * User-defined vehicle unit rear hitch point distance
                               */

    int msia_veh_trans_length[]; /* User-defined vehicle unit transition length */

    int msia_veh_trans_width[]; /* User-defined vehicle unit transition width */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_UNITS_01] through [TX_FMT_GDV_VEH_UNITS_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_7 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_8 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_7 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_8 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_3]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_4]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_5]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_6]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_7]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_8]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_7 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_8 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_7 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_8 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_7 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_8 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_3]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_4]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_5]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_6]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_7]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_8]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_3]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_4]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_5]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_6]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_7]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width [8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_8]

    public GDV_Veh_Units() {
        msia_veh_unit_length = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_unit_width = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_unit_draw_seq = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_unit_fpd = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_unit_rpd = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_unit_rhpd = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_trans_length = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        msia_veh_trans_width = new int[PARAMS.TEXAS_MODEL_MNU + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_Units

    public void checkForErrors(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_attr;
        int lsiv_field;
        int lsiv_unit;
        int lsiv_veh;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_Units data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Units.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.checkForErrors: psiv_veh = " + psiv_veh + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lsiv_veh = Math.min(psiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1);
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Units.checkForErrors checking " + lstv_name);

        // check all data for errors
        lsiv_field = Intersection.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS;
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in GDV_Veh_Units.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        for (lsiv_attr = 1; lsiv_attr <= 8; lsiv_attr++) {
            for (lsiv_unit = 1; lsiv_unit <= msiv_veh_num_units; lsiv_unit++) {
                lsiv_field = Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 + (lsiv_attr - 1) * PARAMS.TEXAS_MODEL_MNU + (lsiv_unit - 1);
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_Veh_Units.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_unit;
        int lsiv_veh;
        String lstv_name;

        // printToFile prints all GDV_Veh_Units data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Units.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if no vehicle data card then return success
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.printToFile: psiv_veh = " + psiv_veh + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lsiv_veh = Math.min(psiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1);
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Units.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(1 + 8 * msiv_veh_num_units);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_veh_num_units, lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_unit = 1; lsiv_unit <= msiv_veh_num_units; lsiv_unit++) {
            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_unit_length[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                    + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_unit_width[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1
                    + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_unit_draw_seq[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1
                    - 1 + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE,
                    Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_unit_fpd[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1
                    + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_unit_rpd[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1
                    + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_unit_rhpd[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1
                    + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_trans_length[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1
                    - 1 + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE,
                    Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_trans_width[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                    + lsiv_unit, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_veh) {
        boolean lbov_forceDefault;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_unit;
        int lsiv_veh;
        String lstv_name;

        // readFromCards reads all GDV_Veh_Units fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Units.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.readFromCards: psiv_veh = " + psiv_veh + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle attributes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_ACCEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_VEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MIN_RAD for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_CLASSIFY for every 10 vehicles
        // 1 card for TX_FMT_GDV_VEH_HEIGHT for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_WIDTH for every 20 vehicles
        lsiv_veh = Math.min(psiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1);
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 8
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
        lbov_forceDefault = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"))
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes < 10));
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        for (lsiv_veh = 1; lsiv_veh <= (psiv_veh - 1); lsiv_veh++) {
            lsiv_card += ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units + 3) / 4);
        }
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Units.readFromCards reading " + lstv_name);
        lsiv_veh = Math.min(psiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1);

        // read data from cards
        msiv_veh_num_units = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_unit = 1; lsiv_unit <= msiv_veh_num_units; lsiv_unit++) {
            // read data from cards
            msia_veh_unit_length[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_unit_width[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_unit_draw_seq[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_unit_fpd[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh,
                    Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1 + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_unit_rpd[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh,
                    Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1 + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_unit_rhpd[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_trans_length[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            msia_veh_trans_width[lsiv_unit] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (lsiv_unit == 4)
                lsiv_card++;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_Units fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_Units.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_unit;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_Units fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Units.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if no vehicle data card then return success
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Units.writeToCards: psiv_veh = " + psiv_veh + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_ACCEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_VEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MIN_RAD for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_CLASSIFY for every 10 vehicles
        // 1 card for TX_FMT_GDV_VEH_HEIGHT for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_WIDTH for every 20 vehicles
        lsiv_veh = Math.min(psiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1);
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 8
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        for (lsiv_veh = 1; lsiv_veh <= (psiv_veh - 1); lsiv_veh++) {
            lsiv_card += ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units + 3) / 4);
        }
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Units.writeToCards writing " + lstv_name);
        lsiv_veh = Math.min(psiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1);

        // write data to cards
        Intersection.writeIntToCard(msiv_veh_num_units, lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_unit = 1; lsiv_unit <= msiv_veh_num_units; lsiv_unit++) {
            // write data to cards
            Intersection.writeIntToCard(msia_veh_unit_length[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_unit_width[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh,
                    Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1 + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_unit_draw_seq[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_unit_fpd[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1 + lsiv_unit,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_unit_rpd[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1 + lsiv_unit,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_unit_rhpd[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh,
                    Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1 + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_trans_length[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_veh_trans_width[lsiv_unit], lstv_name, Intersection.TX_FMT_GDV_VEH_UNITS_01 - 1 + lsiv_veh, Intersection.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                    + lsiv_unit, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (lsiv_unit == 4)
                lsiv_card++;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_Units

/******************************************************************************/
/* GDV_Veh_Units.java */
/******************************************************************************/
