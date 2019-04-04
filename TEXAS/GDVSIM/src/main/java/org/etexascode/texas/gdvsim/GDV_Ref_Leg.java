package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              GDV_Ref_Leg.java                              */
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

class GDV_Ref_Leg {

    int msiv_gdv_ref_nlegs; /* GDV reference number of legs */

    int msiv_gdv_ref_ninlnt; /* GDV reference number of inbound lanes total */

    int msiv_gdv_ref_iuwdth1; /* GDV reference free u-turn width 1 */

    int msiv_gdv_ref_iuwdth2; /* GDV reference free u-turn width 2 */

    int msia_gdv_ref_nlns[]; /* GDV reference leg number of lanes total */

    int msia_gdv_ref_ninlns[]; /* GDV reference leg number of inbound lanes */

    int msia_gdv_ref_langle[]; /* GDV reference leg angle */

    int msia_gdv_ref_lleng[]; /* GDV reference leg length */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_GDV_REF_LEGD1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msiv_gdv_ref_nlegs
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLEGS ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msiv_gdv_ref_ninlnt
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msiv_gdv_ref_iuwdth1
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_IUWDTH1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msiv_gdv_ref_iuwdth2
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_IUWDTH2 ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_GDV_REF_LEGD2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [0]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_0 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[0]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_0]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[0]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_0]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [0]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_0 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_5 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_5 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_6 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_6 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [7]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_7 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[7]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[7]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [7]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_7 ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_GDV_REF_LEGND]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msiv_gdv_ref_nlegs
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLEGS ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msiv_gdv_ref_ninlnt
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_5 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_5 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_nlns [6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NLNS_6 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_NINLNS_6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_langle[6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LANGLE_6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_leg.msia_gdv_ref_lleng [6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_LEG_LLEN_6 ]

    public GDV_Ref_Leg() {
        msia_gdv_ref_nlns = new int[PARAMS.TEXAS_MODEL_NLGP2];
        msia_gdv_ref_ninlns = new int[PARAMS.TEXAS_MODEL_NLGP2];
        msia_gdv_ref_langle = new int[PARAMS.TEXAS_MODEL_NLGP2];
        msia_gdv_ref_lleng = new int[PARAMS.TEXAS_MODEL_NLGP2];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Ref_Leg

    public void calculateValues(TX_Aux pclv_tx_aux // set aux values if not null
    ) {
        double ldfv_a;
        double ldfv_cosa;
        double ldfv_d;
        double ldfv_d1;
        double ldfv_d2;
        double ldfv_d3;
        double ldfv_d4;
        double ldfv_dr;
        double ldfv_dx;
        double ldfv_dy;
        double ldfv_dyt;
        int lsia_islwi[]; // sum of lane widths inbound per leg
        int lsia_islwo[]; // sum of lane widths outbound per leg
        int lsiv_islwil; // sum of lane widths inbound to left
        int lsiv_islwir; // sum of lane widths inbound to right
        int lsiv_ixlil;
        int lsiv_ixlir;
        int lsiv_ixlol;
        int lsiv_ixlor;
        int lsiv_ixofft;
        int lsiv_lane;
        int lsiv_leg;
        int lsiv_leg_cl;
        int lsiv_leg_cr;
        int lsiv_mxofol;
        int lsiv_mxofor;

        lsia_islwi = new int[PARAMS.TEXAS_MODEL_NLGP2];
        lsia_islwo = new int[PARAMS.TEXAS_MODEL_NLGP2];

        // calculateValues calculates all GDV_Ref_Leg fields from other GDV data

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_Leg.calculateValues");
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_Leg.calculateValues");
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // get GDV reference number of legs
        // get GDV reference free u-turn width 1
        // get GDV reference free u-turn width 2
        msiv_gdv_ref_nlegs = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        msiv_gdv_ref_iuwdth1 = 0;
        msiv_gdv_ref_iuwdth2 = 0;

        // set aux data
        if (pclv_tx_aux != null) {
            pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLEGS] = Intersection.TX_SET_BY_SOFTWARE;
            pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH1] = Intersection.TX_SET_BY_SOFTWARE;
            pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH2] = Intersection.TX_SET_BY_SOFTWARE;
        }

        // get GDV reference number of inbound lanes total
        // get GDV reference leg number of lanes total
        // get GDV reference leg number of inbound lanes
        // get GDV reference leg angle
        // get GDV reference leg length
        msiv_gdv_ref_ninlnt = 0;
        for (lsiv_leg = 1; lsiv_leg <= msiv_gdv_ref_nlegs; lsiv_leg++) {
            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of outbound lanes is valid
            if (Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if leg angle is valid
            if (Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_ANG] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_ang is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if length of inbound lanes is valid
            if (Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if median width is valid
            if (Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_MED_W] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_med_w is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if centerline offset is valid
            if (Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_CL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            msiv_gdv_ref_ninlnt += Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            msia_gdv_ref_nlns[lsiv_leg] = Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
            msia_gdv_ref_ninlns[lsiv_leg] = Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            msia_gdv_ref_langle[lsiv_leg] = Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang;
            msia_gdv_ref_lleng[lsiv_leg] = Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;

            lsia_islwi[lsiv_leg] = 0;
            for (lsiv_lane = 1; lsiv_lane <= Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb; lsiv_lane++) {
                // check if lane width is valid
                if (Intersection.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mcla_inb_lane[" + lsiv_lane
                            + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lsia_islwi[lsiv_leg] += Intersection.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width;
            }

            lsia_islwo[lsiv_leg] = 0;
            for (lsiv_lane = 1; lsiv_lane <= Intersection.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; lsiv_lane++) {
                // check if lane width is valid
                if (Intersection.mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mcla_leg[" + lsiv_leg + "].mcla_out_lane[" + lsiv_lane
                            + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lsia_islwo[lsiv_leg] += Intersection.mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width;
            }

            // set aux data
            if (pclv_tx_aux != null) {
                switch (lsiv_leg) {
                    case 1:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_1] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_1] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_1] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 2:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_2] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_2] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_2] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 3:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_3] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_3] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_3] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 4:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_4] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_4] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_4] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 5:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_5] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_5] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_5] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 6:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_6] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_6] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_6] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = Intersection.TX_SET_BY_SOFTWARE;
                        break;
                }
            } // end if ( pclv_tx_aux != null )
        } // end for ( lsiv_leg = 1 ; lsiv_leg <= msiv_gdv_ref_nlegs ; lsiv_leg++ )

        // set aux data
        if (pclv_tx_aux != null) {
            pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNT] = Intersection.TX_SET_BY_SOFTWARE;
        }

        // set diamond interchange data
        if (Intersection.mbov_is_diamond_interchange) {
            // check if diamond number of inbound lanes to Right Intersection is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if diamond number of inbound lanes to Left Intersection is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if diamond distance between is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if diamond median width is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if curb return radius is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_1] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1] is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if curb return radius is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_4] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4] is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // get lsiv_islwir and lsiv_mxofol
            lsiv_islwir = 0;
            lsiv_mxofol = 0;
            for (lsiv_lane = 1; lsiv_lane <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right; lsiv_lane++) {
                // check if lane width is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[" + lsiv_lane
                            + "].msiv_int_lane_width is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if offset near Left Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[" + lsiv_lane
                            + "].msiv_offset_near_center_left is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_islwir += Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width;
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left > lsiv_mxofol) {
                    lsiv_mxofol = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left;
                }
            }

            // get lsiv_islwil and lsiv_mxofor
            lsiv_islwil = 0;
            lsiv_mxofor = 0;
            for (lsiv_lane = 1; lsiv_lane <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left; lsiv_lane++) {
                // check if lane width is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left [" + lsiv_lane
                            + "].msiv_int_lane_width is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if offset near Right Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[" + lsiv_lane
                            + "].msiv_offset_near_center_right is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                lsiv_islwil += Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_int_lane_width;
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_right > lsiv_mxofor) {
                    lsiv_mxofor = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_right;
                }
            }

            // set internal leg numbers
            lsiv_leg_cr = 0;
            lsiv_leg_cl = msiv_gdv_ref_nlegs + 1;

            // the following code is from SIMDATA subroutine GDVRD and calculates the internal lane
            // lengths
            msia_gdv_ref_langle[lsiv_leg_cr] = 270;
            msia_gdv_ref_langle[lsiv_leg_cl] = 90;
            lsiv_ixofft = PARAMS.TEXAS_MODEL_XYCNTR;
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1] < 0) {
                lsiv_ixlor = lsiv_ixofft + (int)(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / 2.0 + 0.5)
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1];
                lsiv_ixlir = lsiv_ixlor;
            }
            else {
                if (msia_gdv_ref_langle[1] == 0) {
                    lsiv_ixlor = lsiv_ixofft
                            + (int)((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_med_w) / 2.0 + 0.5)
                            - Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off - lsia_islwi[1] - Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1];
                }
                else {
                    ldfv_dy = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width / 2.0 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1];
                    ldfv_dyt = ldfv_dy + lsiv_islwil;
                    ldfv_a = msia_gdv_ref_langle[1] * Intersection.RADIANS_PER_DEGREE;
                    ldfv_dr = ldfv_dyt / Math.sin(ldfv_a);
                    ldfv_cosa = Math.cos(ldfv_a);
                    ldfv_dx = ldfv_dr * ldfv_cosa;
                    ldfv_d = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1] + lsia_islwi[1] + Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_med_w / 2.0
                            + Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off;
                    ldfv_d = ldfv_dr - ldfv_d;
                    ldfv_d = ldfv_d / ldfv_cosa;
                    lsiv_ixlor = lsiv_ixofft + (int)(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / 2.0 - ldfv_dx + ldfv_d + 0.5);
                }
                if (msia_gdv_ref_langle[3] == 180) {
                    lsiv_ixlir = lsiv_ixofft
                            + (int)((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_med_w) / 2.0)
                            + Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off - lsia_islwo[3] - Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1];
                }
                else {
                    ldfv_dy = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width / 2.0 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1];
                    ldfv_dyt = ldfv_dy + lsiv_islwir;
                    ldfv_a = msia_gdv_ref_langle[3] * Intersection.RADIANS_PER_DEGREE;
                    ldfv_dr = ldfv_dyt / Math.sin(ldfv_a);
                    ldfv_cosa = Math.cos(ldfv_a);
                    ldfv_dx = ldfv_dr * ldfv_cosa;
                    ldfv_d = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1] + lsia_islwo[3] + Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_med_w / 2.0
                            - Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off;
                    ldfv_d = ldfv_dr - ldfv_d;
                    ldfv_d = ldfv_d / ldfv_cosa;
                    lsiv_ixlir = lsiv_ixofft + (int)(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / 2.0 + ldfv_dx - ldfv_d + 0.5);
                }
            } // end if ( Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1]
              // < 0 )

            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4] < 0) {
                lsiv_ixlil = lsiv_ixofft - (int)(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / 2.0 + 0.5)
                        - Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4];
                lsiv_ixlol = lsiv_ixlil;
            }
            else {
                if (msia_gdv_ref_langle[6] == 0) {
                    lsiv_ixlil = lsiv_ixofft
                            - (int)((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_med_w) / 2.0 + 0.5)
                            - Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off + lsia_islwo[6] + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4];
                }
                else {
                    ldfv_dy = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width / 2.0 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4];
                    ldfv_dyt = ldfv_dy + lsiv_islwil;
                    ldfv_a = msia_gdv_ref_langle[6] * Intersection.RADIANS_PER_DEGREE;
                    ldfv_cosa = Math.cos(ldfv_a);
                    ldfv_d1 = ldfv_dyt / Math.sin(ldfv_a);
                    ldfv_d2 = ldfv_d1 * ldfv_cosa;
                    ldfv_d3 = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4] + lsia_islwo[6] + Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_med_w / 2.0
                            - Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off;
                    ldfv_d4 = (ldfv_d1 + ldfv_d3) / ldfv_cosa;
                    lsiv_ixlil = lsiv_ixofft - (int)(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / 2.0 - ldfv_d4 + ldfv_d2 + 0.5);
                }
                if (msia_gdv_ref_langle[4] == 180) {
                    lsiv_ixlol = lsiv_ixofft
                            - (int)((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_med_w) / 2.0 + 0.5)
                            + Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off + lsia_islwi[4] + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4];
                }
                else {
                    ldfv_dy = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width / 2.0 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4];
                    ldfv_dyt = ldfv_dy + lsiv_islwir;
                    ldfv_a = msia_gdv_ref_langle[4] * Intersection.RADIANS_PER_DEGREE;
                    ldfv_cosa = Math.cos(ldfv_a);
                    ldfv_d1 = ldfv_dyt / Math.sin(ldfv_a);
                    ldfv_d2 = ldfv_d1 * ldfv_cosa;
                    ldfv_d3 = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4] + lsia_islwi[4] + Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_med_w / 2.0
                            + Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off;
                    ldfv_d4 = (ldfv_d1 + ldfv_d3) / ldfv_cosa;
                    lsiv_ixlol = lsiv_ixofft - (int)(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / 2.0 + ldfv_d4 - ldfv_d2 + 0.5);
                }
            } // end if ( Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4]
              // < 0 )

            msiv_gdv_ref_ninlnt += Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
            msia_gdv_ref_nlns[lsiv_leg_cr] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
            msia_gdv_ref_ninlns[lsiv_leg_cr] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
            msia_gdv_ref_lleng[lsiv_leg_cr] = lsiv_ixlir - lsiv_ixlol + lsiv_mxofol;
            msiv_gdv_ref_ninlnt += Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
            msia_gdv_ref_nlns[lsiv_leg_cl] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
            msia_gdv_ref_ninlns[lsiv_leg_cl] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
            msia_gdv_ref_lleng[lsiv_leg_cl] = lsiv_ixlor - lsiv_ixlil + lsiv_mxofor;

            // set aux data
            if (pclv_tx_aux != null) {
                pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_0] = Intersection.TX_SET_BY_SOFTWARE;
                pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_0] = Intersection.TX_SET_BY_SOFTWARE;
                pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_0] = Intersection.TX_SET_BY_SOFTWARE;
                pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_0] = Intersection.TX_SET_BY_SOFTWARE;

                switch (lsiv_leg_cl) {
                    case 1:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_1] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_1] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_1] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 2:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_2] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_2] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_2] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 3:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_3] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_3] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_3] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 4:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_4] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_4] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_4] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 5:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_5] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_5] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_5] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 6:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_6] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_6] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_6] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = Intersection.TX_SET_BY_SOFTWARE;
                        break;

                    case 7:
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_7] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_7] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_7] = Intersection.TX_SET_BY_SOFTWARE;
                        pclv_tx_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7] = Intersection.TX_SET_BY_SOFTWARE;
                        break;
                }
            } // end if ( pclv_tx_aux != null )

            // get GDV reference free u-turn width 1
            // get GDV reference free u-turn width 2
            if (Intersection.mbov_free_uturns_defined) {
                // check if free u-turn lane width is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.calculateValues: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_gdv_ref_iuwdth1 = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width;
                msiv_gdv_ref_iuwdth2 = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width;

                // increase number of lanes if free u-turn 1 defined
                if (msiv_gdv_ref_iuwdth1 > 0) {
                    // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                    msiv_gdv_ref_ninlnt++;
                    msia_gdv_ref_nlns[3]++;
                    msia_gdv_ref_ninlns[3]++;
                    msia_gdv_ref_nlns[4]++;
                }

                // increase number of lanes if free u-turn 2 defined
                if (msiv_gdv_ref_iuwdth2 > 0) {
                    // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                    msiv_gdv_ref_ninlnt++;
                    msia_gdv_ref_nlns[6]++;
                    msia_gdv_ref_ninlns[6]++;
                    msia_gdv_ref_nlns[1]++;
                }
            } // end if ( Intersection.mbov_free_uturns_defined )
        } // end if ( Intersection.mbov_is_diamond_interchange )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method calculateValues

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_field_beg;
        int lsiv_field_end;
        String lstv_name;

        // checkForErrors checks all GDV_Ref_Leg data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GDV_Ref_Leg.checkForErrors");
        if (Intersection.mbov_is_diamond_interchange) {
            // check TX_FMT_SIM_GDV_REF_LEGD1
            // set local data for checking data for errors
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_LEGD1];
            lsiv_field_beg = Intersection.TX_FMT_SIM_GDV_REF_LEG_NLEGS;
            lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH2;
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
                System.out.println("GDV_Ref_Leg.checkForErrors checking " + lstv_name);
            // check all data for errors
            for (lsiv_field = lsiv_field_beg; lsiv_field <= lsiv_field_end; lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            // check TX_FMT_SIM_GDV_REF_LEGD2
            // set local data for checking data for errors
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_LEGD2];
            lsiv_field_beg = Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_0;
            lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1;
            if (msiv_gdv_ref_nlegs == 1)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2;
            if (msiv_gdv_ref_nlegs == 2)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3;
            if (msiv_gdv_ref_nlegs == 3)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4;
            if (msiv_gdv_ref_nlegs == 4)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5;
            if (msiv_gdv_ref_nlegs == 5)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6;
            if (msiv_gdv_ref_nlegs == 6)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7;
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
                System.out.println("GDV_Ref_Leg.checkForErrors checking " + lstv_name);
            // check all data for errors
            for (lsiv_field = lsiv_field_beg; lsiv_field <= lsiv_field_end; lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }
        else {
            // check TX_FMT_SIM_GDV_REF_LEGND
            // set local data for checking data for errors
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_LEGND];
            lsiv_field_beg = Intersection.TX_FMT_SIM_GDV_REF_LEG_NLEGS;
            lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNT;
            lstv_name = lclv_tx_fmt.mstv_name.toString();
            /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
                System.out.println("GDV_Ref_Leg.checkForErrors checking " + lstv_name);
            // check all data for errors
            for (lsiv_field = lsiv_field_beg; lsiv_field <= lsiv_field_end; lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
            // set local data for checking data for errors
            lsiv_field_beg = Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_1;
            lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1;
            if (msiv_gdv_ref_nlegs == 1)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1;
            if (msiv_gdv_ref_nlegs == 2)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2;
            if (msiv_gdv_ref_nlegs == 3)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3;
            if (msiv_gdv_ref_nlegs == 4)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4;
            if (msiv_gdv_ref_nlegs == 5)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5;
            if (msiv_gdv_ref_nlegs == 6)
                lsiv_field_end = Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6;
            // check all data for errors
            for (lsiv_field = lsiv_field_beg; lsiv_field <= lsiv_field_end; lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_class;
        int lsiv_def;
        int lsiv_max;
        int lsiv_min;
        int lsiv_nlegs;
        String lstv_name;

        // readFromCards reads all GDV_Ref_Leg fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_Leg.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if last SIM data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // calculate values based upon current GDV data, store in temporary location, and do not set
        // aux data
        Intersection.mclv_gdv_ref_leg.calculateValues(null);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data for reading data from cards
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGD1;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGND;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card + 2;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_Leg.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_gdv_ref_nlegs = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLEGS, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check against actual GDV data
        if (msiv_gdv_ref_nlegs != Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_nlegs) {
            Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLEGS]
                    + " = " + msiv_gdv_ref_nlegs + " is not " + Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_nlegs + ".";
            Intersection.warningMessage();
        }

        // read data from cards
        msiv_gdv_ref_ninlnt = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNT, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check against actual GDV data
        if (msiv_gdv_ref_ninlnt != Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_ninlnt) {
            Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNT]
                    + " = " + msiv_gdv_ref_ninlnt + " is not " + Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_ninlnt + ".";
            Intersection.warningMessage();
        }

        if (Intersection.mbov_is_diamond_interchange) {
            // read data from cards
            msiv_gdv_ref_iuwdth1 = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH1, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msiv_gdv_ref_iuwdth1 != Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_iuwdth1) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH1] + " = " + msiv_gdv_ref_iuwdth1 + " is not " + Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_iuwdth1 + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msiv_gdv_ref_iuwdth2 = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH2, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msiv_gdv_ref_iuwdth2 != Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_iuwdth2) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH2] + " = " + msiv_gdv_ref_iuwdth2 + " is not " + Intersection.mclv_gdv_ref_leg.msiv_gdv_ref_iuwdth2 + ".";
                Intersection.warningMessage();
            }
        }

        // set local data for reading data from cards
        lsiv_def = 0;
        lsiv_max = 0;
        lsiv_min = 0;
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGD2;
            lsiv_card++;
            lsiv_nlegs = msiv_gdv_ref_nlegs + 1;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGND;
            lsiv_nlegs = msiv_gdv_ref_nlegs;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_Leg.readFromCards reading " + lstv_name);

        if (Intersection.mbov_is_diamond_interchange) {
            // read data from cards
            msia_gdv_ref_nlns[0] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_0, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[0] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[0]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_0] + " = " + msia_gdv_ref_nlns[0] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[0] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[0] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_0, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[0] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[0]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_0] + " = " + msia_gdv_ref_ninlns[0] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[0]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[0] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_0, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[0] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[0]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_0] + " = " + msia_gdv_ref_langle[0] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[0]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_lleng[0] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_0, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_lleng[0] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[0]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_0] + " = " + msia_gdv_ref_lleng[0] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[0] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 1) {
            // read data from cards
            msia_gdv_ref_nlns[1] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_1, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[1] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[1]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_1] + " = " + msia_gdv_ref_nlns[1] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[1] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[1] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[1] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[1]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_1] + " = " + msia_gdv_ref_ninlns[1] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[1]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[1] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[1] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[1]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_1] + " = " + msia_gdv_ref_langle[1] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[1]
                        + ".";
                Intersection.warningMessage();
            }

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 1)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // read data from cards
            msia_gdv_ref_lleng[1] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 1)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lsiv_def;
            }

            // check against actual GDV data
            if (msia_gdv_ref_lleng[1] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[1]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] + " = " + msia_gdv_ref_lleng[1] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[1] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 2) {
            // read data from cards
            msia_gdv_ref_nlns[2] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_2, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[2] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[2]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_2] + " = " + msia_gdv_ref_nlns[2] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[2] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[2] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[2] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[2]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_2] + " = " + msia_gdv_ref_ninlns[2] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[2]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[2] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[2] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[2]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_2] + " = " + msia_gdv_ref_langle[2] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[2]
                        + ".";
                Intersection.warningMessage();
            }

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 2)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // read data from cards
            msia_gdv_ref_lleng[2] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 2)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lsiv_def;
            }

            // check against actual GDV data
            if (msia_gdv_ref_lleng[2] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[2]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] + " = " + msia_gdv_ref_lleng[2] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[2] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 3) {
            // read data from cards
            msia_gdv_ref_nlns[3] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_3, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[3] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[3]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_3] + " = " + msia_gdv_ref_nlns[3] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[3] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[3] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_3, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[3] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[3]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_3] + " = " + msia_gdv_ref_ninlns[3] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[3]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[3] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_3, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[3] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[3]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_3] + " = " + msia_gdv_ref_langle[3] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[3]
                        + ".";
                Intersection.warningMessage();
            }

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 3)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // read data from cards
            msia_gdv_ref_lleng[3] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 3)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lsiv_def;
            }

            // check against actual GDV data
            if (msia_gdv_ref_lleng[3] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[3]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] + " = " + msia_gdv_ref_lleng[3] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[3] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 4) {
            // read data from cards
            msia_gdv_ref_nlns[4] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_4, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[4] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[4]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_4] + " = " + msia_gdv_ref_nlns[4] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[4] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[4] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_4, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[4] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[4]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_4] + " = " + msia_gdv_ref_ninlns[4] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[4]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[4] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_4, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[4] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[4]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_4] + " = " + msia_gdv_ref_langle[4] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[4]
                        + ".";
                Intersection.warningMessage();
            }

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 4)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // read data from cards
            msia_gdv_ref_lleng[4] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 4)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lsiv_def;
            }

            // check against actual GDV data
            if (msia_gdv_ref_lleng[4] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[4]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] + " = " + msia_gdv_ref_lleng[4] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[4] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 5) {
            // read data from cards
            msia_gdv_ref_nlns[5] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_5, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[5] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[5]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_5] + " = " + msia_gdv_ref_nlns[5] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[5] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[5] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_5, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[5] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[5]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_5] + " = " + msia_gdv_ref_ninlns[5] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[5]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[5] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_5, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[5] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[5]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_5] + " = " + msia_gdv_ref_langle[5] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[5]
                        + ".";
                Intersection.warningMessage();
            }

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 5)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // read data from cards
            msia_gdv_ref_lleng[5] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 5)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lsiv_def;
            }

            // check against actual GDV data
            if (msia_gdv_ref_lleng[5] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[5]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] + " = " + msia_gdv_ref_lleng[5] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[5] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 6) {
            // read data from cards
            msia_gdv_ref_nlns[6] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_6, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[6] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[6]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_6] + " = " + msia_gdv_ref_nlns[6] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[6] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[6] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_6, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[6] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[6]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_6] + " = " + msia_gdv_ref_ninlns[6] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[6]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[6] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_6, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[6] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[6]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_6] + " = " + msia_gdv_ref_langle[6] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[6]
                        + ".";
                Intersection.warningMessage();
            }

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 6)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // read data from cards
            msia_gdv_ref_lleng[6] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 6)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lsiv_def;
            }

            // check against actual GDV data
            if (msia_gdv_ref_lleng[6] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[6]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] + " = " + msia_gdv_ref_lleng[6] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[6] + ".";
                Intersection.warningMessage();
            }
        }

        if (lsiv_nlegs >= 7) {
            // read data from cards
            msia_gdv_ref_nlns[7] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_7, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_nlns[7] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[7]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_7] + " = " + msia_gdv_ref_nlns[7] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_nlns[7] + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_ninlns[7] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_7, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_ninlns[7] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[7]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_7] + " = " + msia_gdv_ref_ninlns[7] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_ninlns[7]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_langle[7] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_7, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_langle[7] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[7]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_7] + " = " + msia_gdv_ref_langle[7] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_langle[7]
                        + ".";
                Intersection.warningMessage();
            }

            // read data from cards
            msia_gdv_ref_lleng[7] = Intersection.readIntFromCard(lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                    lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check against actual GDV data
            if (msia_gdv_ref_lleng[7] != Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[7]) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Leg.readFromCards: Card " + lsiv_card + " " + lstv_name + " - "
                        + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7] + " = " + msia_gdv_ref_lleng[7] + " is not " + Intersection.mclv_gdv_ref_leg.msia_gdv_ref_lleng[7] + ".";
                Intersection.warningMessage();
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Ref_Leg fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Ref_Leg.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_class;
        int lsiv_def = 0;
        int lsiv_max = 0;
        int lsiv_min = 0;
        int lsiv_nlegs;
        String lstv_name;

        // writeToCards writes all GDV_Ref_Leg fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_Leg.writeToCards");
        // check if ref file leg card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_REF_FILE_LEG_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Leg.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_leg_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // calculate values based upon current GDV data, store in this instance, and set aux data
        this.calculateValues(mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_leg_card;
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGD1;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGND;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_Leg.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_gdv_ref_nlegs, lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLEGS, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_gdv_ref_ninlnt, lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNT, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_diamond_interchange) {
            // write data to cards
            Intersection.writeIntToCard(msiv_gdv_ref_iuwdth1, lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_gdv_ref_iuwdth2, lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_IUWDTH2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // set local data for writing data to cards
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGD2;
            lsiv_card++;
            lsiv_nlegs = msiv_gdv_ref_nlegs + 1;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_GDV_REF_LEGND;
            lsiv_nlegs = msiv_gdv_ref_nlegs;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_Leg.writeToCards writing " + lstv_name);

        if (Intersection.mbov_is_diamond_interchange) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[0], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_0, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[0], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_0, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[0], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_0, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[0], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_0, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if (lsiv_nlegs >= 1) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[1], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_1, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[1], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[1], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 1)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[1], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 1)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_1] = lsiv_def;
            }
        }

        if (lsiv_nlegs >= 2) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[2], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_2, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[2], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[2], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 2)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[2], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 2)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_2] = lsiv_def;
            }
        }

        if (lsiv_nlegs >= 3) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[3], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_3, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[3], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_3, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[3], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_3, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 3)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[3], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 3)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_3] = lsiv_def;
            }
        }

        if (lsiv_nlegs >= 4) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[4], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_4, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[4], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_4, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[4], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_4, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 4)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[4], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 4)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_4] = lsiv_def;
            }
        }

        if (lsiv_nlegs >= 5) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[5], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_5, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[5], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_5, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[5], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_5, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 5)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[5], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 5)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_5] = lsiv_def;
            }
        }

        if (lsiv_nlegs >= 6) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[6], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_6, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[6], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_6, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[6], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_6, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set max, min, and def for diamond inbound to Left Intersection
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 6)) {
                lsiv_min = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6];
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6];
                lsiv_def = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6];
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7];
            }

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[6], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // restore max, min, and def
            if (Intersection.mbov_is_diamond_interchange && (lsiv_nlegs == 6)) {
                lclv_tx_fmt.msia_min[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lsiv_min;
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lsiv_max;
                lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_6] = lsiv_def;
            }
        }

        if (lsiv_nlegs >= 7) {
            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_nlns[7], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NLNS_7, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_ninlns[7], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_NINLNS_7, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_langle[7], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LANGLE_7, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msia_gdv_ref_lleng[7], lstv_name, lsiv_class, Intersection.TX_FMT_SIM_GDV_REF_LEG_LLEN_7, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Ref_Leg

/******************************************************************************/
/* GDV_Ref_Leg.java */
/******************************************************************************/
