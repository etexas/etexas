package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                SIM_Hdr.java                                */
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

class SIM_Hdr {

    String mstv_sim_sig; /* signature */

    int msiv_sim_title_card; /* card number for title */

    int msiv_timing_card; /* card number for phase timing data */

    int msiv_preclen_diafig_nemahitlov; /*
                                         * pretimed controller cycle length (40 to 200)
                                         */

    /* greater than 0 - data is being input in percent of cycle */
    /* - or - */
    /* TEXAS Diamond Controller figure number (3, 4, 6, or 7) */
    /* - or - */
    /* NEMA/HARDWARE Controller number of overlaps */
    int msiv_no_of_phases; /* number of signal controller phases */

    int msiv_first_grn_int_seq_card; /*
                                      * card number for green interval sequence data
                                      */

    int msiv_psfps_nhmov_diaint_card; /*
                                       * card number for Pretimed/Semi-Act/Full-Act Phase Sequence
                                       * data
                                       */

    /* - or - */
    /* card number for NEMA/HARDWARE Movement data */
    /* - or - */
    /* card number for TEX-DIA Interval data */
    int msiv_first_det_card; /* card number for detector data */

    int msiv_no_of_det; /* number of detectors */

    int msiv_first_con_card; /* card number for detector connection */

    int msiv_last_SIM_dat_card; /* card number for last sim data */

    int msiv_last_GDV_ref_card; /* card number for last gdv reference data */

    TX_Aux mclv_aux; /* Auxiliary data */

    /* the following variables are defined but are not read or written */
    int msiv_par_opt_card; /* card number for parameter options data */

    int msiv_par_opt_2_card; /* card number for parameter options 2 data */

    int msiv_lane_cont_card; /* card number for lane control data */

    int msiv_tex_dia_opt_card; /*
                                * card number for TEXAS Diamond Controller Options data
                                */

    int msiv_nema_ph_ov_tx_card; /*
                                  * card number for NEMA/HARDWARE Phase and Overlap Text data
                                  */

    int msiv_nema_ring_grp_card; /* card number for NEMA Ring and Group data */

    int msiv_nema_ph_diag_card; /* card number for NEMA Phase Diagram data */

    int msiv_nema_ovlp_def_card; /* card number for NEMA Overlap Definition data */

    int msiv_vms_message_card; /* card number for VMS message data */

    int msiv_ref_file_name_card; /* card number for reference file name */

    int msiv_ref_file_leg_card; /* card number for reference file leg data */

    int msiv_ref_file_lane_card; /* card number for reference file lane data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_HEADER]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mstv_sim_sig
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_SIM_SIG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_SIM_TITLE_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_TIMING_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_NO_OF_PHASES ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_det_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_FIRST_DET_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_NO_OF_DET ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_con_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_FIRST_CON_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_GDV_ref_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_LAST_GDV_REF_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_par_opt_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_PAR_OPT_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_par_opt_2_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_PAR_OPT_2_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_lane_cont_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_LANE_CONT_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_tex_dia_opt_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_TEX_DIA_OPT_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ph_ov_tx_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_NEMA_PH_OV_TX_CARD]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ring_grp_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_NEMA_RING_GRP_CARD]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ph_diag_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_NEMA_PH_DIA_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ovlp_def_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_NEMA_OVLP_DEF_CARD]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_vms_message_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_VMS_MESSAGE_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_name_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_REF_FILE_NAME_CARD]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_leg_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_REF_FILE_LEG_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_lane_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_HEADER_REF_FILE_LANE_CARD]

    // order of SIM data records
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_PRET_SIGNAL or TX_FMT_SIM_SUNA_SIGNAL or TX_FMT_SIM_SEMI_SIGNAL or
    // TX_FMT_SIM_FULL_SIGNAL or TX_FMT_SIM_TEXD_SIGNAL or TX_FMT_SIM_NEM1_SIGNAL or
    // TX_FMT_SIM_NEM2_SIGNAL or TX_FMT_SIM_HITL_SIGNAL
    // TX_FMT_SIM_TEX_DIA_INT
    // TX_FMT_SIM_TEX_DIA_OPT
    // TX_FMT_SIM_NEMA_MOVEMENT
    // TX_FMT_SIM_NEMA_PH_OV_TX
    // TX_FMT_SIM_NEMA_RING_GRP
    // TX_FMT_SIM_NEMA_PH_DIAG (only if number of overlaps > 0)
    // TX_FMT_SIM_NEMA_OVLP_DEF (only if number of overlaps > 0)
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_PHASE_SEQ
    // TX_FMT_SIM_DETECT_DATNDI or TX_FMT_SIM_DETECT_DATDIA
    // TX_FMT_SIM_DETECT_CONSFA or TX_FMT_SIM_DETECT_CONNM1 or TX_FMT_SIM_DETECT_CONN2H
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS

    // SIM data records
    // for intersection control UNCONTRL
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control YIELD
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control STOP
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control ALL-STOP
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control PRETIMED
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_PRET_SIGNAL
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_PHASE_SEQ (set to defaults)
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control SEMI-ACT
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_SUNA_SIGNAL and TX_FMT_SIM_SEMI_SIGNAL
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_PHASE_SEQ (from user)
    // TX_FMT_SIM_DETECT_DATNDI or TX_FMT_SIM_DETECT_DATDIA
    // TX_FMT_SIM_DETECT_CONSFA
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control FULL-ACT
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_FULL_SIGNAL
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_PHASE_SEQ (from user)
    // TX_FMT_SIM_DETECT_DATNDI or TX_FMT_SIM_DETECT_DATDIA
    // TX_FMT_SIM_DETECT_CONSFA
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control TEX-DIA
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_TEXD_SIGNAL
    // TX_FMT_SIM_TEX_DIA_INT
    // TX_FMT_SIM_TEX_DIA_OPT
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_DETECT_DATDIA
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control NEMA
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_NEM1_SIGNAL or TX_FMT_SIM_NEM2_SIGNAL
    // TX_FMT_SIM_NEMA_MOVEMENT
    // TX_FMT_SIM_NEMA_PH_OV_TX
    // TX_FMT_SIM_NEMA_RING_GRP
    // TX_FMT_SIM_NEMA_PH_DIAG (only if number of overlaps > 0)
    // TX_FMT_SIM_NEMA_OVLP_DEF (only if number of overlaps > 0)
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_DETECT_DATNDI or TX_FMT_SIM_DETECT_DATDIA
    // TX_FMT_SIM_DETECT_CONNM1 or TX_FMT_SIM_DETECT_CONN2H
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS
    // for intersection control HARDWARE
    // TX_FMT_SIM_HEADER
    // TX_FMT_SIM_VERSION
    // TX_FMT_SIM_TITLE
    // TX_FMT_SIM_PAR_OPT
    // TX_FMT_SIM_PAR_OPT_2
    // TX_FMT_SIM_LANE_CONT
    // TX_FMT_SIM_HITL_SIGNAL
    // TX_FMT_SIM_NEMA_MOVEMENT
    // TX_FMT_SIM_NEMA_PH_OV_TX
    // TX_FMT_SIM_GREEN_INT_SEQ
    // TX_FMT_SIM_DETECT_DATNDI or TX_FMT_SIM_DETECT_DATDIA
    // TX_FMT_SIM_DETECT_CONN2H
    // TX_FMT_SIM_VMS_MESSAGE
    // TX_FMT_SIM_GDV_REF_FILE
    // TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or TX_FMT_SIM_GDV_REF_LEGND
    // TX_FMT_SIM_GDV_REF_LANE
    // TX_FMT_SIM_GDV_REF_OFFS

    public SIM_Hdr() {
        mclv_aux = new TX_Aux();
    } // end of method SIM_Hdr

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all SIM_Hdr data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Hdr.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Hdr.checkForErrors checking " + lstv_name);

        // set defaults -------------------- usage ---------------------
        // msiv_preclen_diafig_nemahitlov PRETIMED TEX-DIA NEMA HARDWARE
        // msiv_no_of_phases PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // msiv_no_of_det SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP || Intersection.mbov_is_ic_SEMI_ACT
                    || Intersection.mbov_is_ic_FULL_ACT) {
                msiv_preclen_diafig_nemahitlov = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP) {
                msiv_no_of_phases = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID)) {
            if (!Intersection.mbov_is_ic_detector_data) {
                msiv_no_of_det = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // skip checkForErrors for fields set by software
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_SIM_SIG)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_TIMING_CARD)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_FIRST_DET_CARD)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_FIRST_CON_CARD)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD)
                continue;
            if (lsiv_field == Intersection.TX_FMT_SIM_HEADER_LAST_GDV_REF_CARD)
                continue;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in SIM_Hdr.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SIM_Hdr.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_inb_leg;
        int lsiv_out_leg;
        String lstv_name;

        // readFromCards reads all SIM_Hdr fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Hdr.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // TX_FMT_SIM_HEADER card is always card 1
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
        lsiv_card = 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Hdr.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_sim_sig = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_SIM_SIG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_sim_title_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_timing_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_TIMING_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_preclen_diafig_nemahitlov = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_no_of_phases = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_first_grn_int_seq_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_psfps_nhmov_diaint_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_first_det_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_FIRST_DET_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_no_of_det = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_NO_OF_DET, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_first_con_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_FIRST_CON_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_last_SIM_dat_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_last_GDV_ref_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_LAST_GDV_REF_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // if last GDV ref card is not number of cards then error
        if (msiv_last_GDV_ref_card != Intersection.msiv_simdataCardsRead) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_last_GDV_ref_card = " + msiv_last_GDV_ref_card + " is not " + Intersection.msiv_simdataCardsRead + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if card number > last GDV ref card then error
        if (msiv_sim_title_card > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_sim_title_card = " + msiv_sim_title_card + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_timing_card > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_timing_card = " + msiv_timing_card + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_first_grn_int_seq_card > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_first_grn_int_seq_card = " + msiv_first_grn_int_seq_card + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_psfps_nhmov_diaint_card > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_psfps_nhmov_diaint_card = " + msiv_psfps_nhmov_diaint_card + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_first_det_card > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_first_det_card = " + msiv_first_det_card + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_first_con_card > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_first_con_card = " + msiv_first_con_card + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Math.abs(msiv_last_SIM_dat_card) > msiv_last_GDV_ref_card) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: msiv_last_SIM_dat_card = " + Math.abs(msiv_last_SIM_dat_card) + " is > " + msiv_last_GDV_ref_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Hdr.readFromCards Intersection.mbov_is_diamond_interchange=" + Intersection.mbov_is_diamond_interchange);
        // if msiv_last_SIM_dat_card < 0 then this is a diamond interchange
        if (msiv_last_SIM_dat_card < 0) {
            if (!Intersection.mbov_is_diamond_interchange) {
                Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: GDV data indicated the intersection is a Standard Intersection while the SIM data indicated a Diamond Interchange.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }
        else {
            if (Intersection.mbov_is_diamond_interchange) {
                Intersection.mstv_errorMessage = "Error in SIM_Hdr.readFromCards: GDV data indicated the intersection is a Diamond Interchange while the SIM data indicated a Standard Intersection.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }
        msiv_last_SIM_dat_card = Math.abs(msiv_last_SIM_dat_card);

        // set free u-turn data
        // special code that must also be performed by the menu system
        if (Intersection.mbov_is_diamond_interchange && Intersection.mbov_free_uturns_defined) {
            // check if free u-turn 1 lane width is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SDR_Hdr.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0) {
                // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                lsiv_inb_leg = 3;
                lsiv_out_leg = 4;
                // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("SIM_Hdr.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                            + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont=\"UN\"");
                Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_LANE_CONT_CONT_0] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("SIM_Hdr.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                            + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont=\"UN\"");
                Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_LANE_CONT_CONT_0] = Intersection.TX_SET_BY_SOFTWARE;
            }

            // check if free u-turn 2 lane width is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SDR_Hdr.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0) {
                // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                lsiv_inb_leg = 6;
                lsiv_out_leg = 1;
                // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("SIM_Hdr.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                            + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont=\"UN\"");
                Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_LANE_CONT_CONT_0] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("SIM_Hdr.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                            + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont=\"UN\"");
                Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_LANE_CONT_CONT_0] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all SIM_Hdr fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("SIM_Hdr.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_detector;
        int lsiv_no_legd;
        int lsiv_no_legs;
        int lsiv_phase;
        String lstv_name;

        // writeToCards write all SIM_Hdr fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards");
        // set defaults -------------------- usage ---------------------
        // msiv_preclen_diafig_nemahitlov PRETIMED TEX-DIA NEMA HARDWARE
        // msiv_no_of_phases PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // msiv_no_of_det SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP || Intersection.mbov_is_ic_SEMI_ACT
                    || Intersection.mbov_is_ic_FULL_ACT) {
                msiv_preclen_diafig_nemahitlov = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP) {
                msiv_no_of_phases = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID)) {
            if (!Intersection.mbov_is_ic_detector_data) {
                msiv_no_of_det = 0;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }

        // set SIM version defined
        Intersection.mbov_version_defined_sim = true;

        // set signature
        mstv_sim_sig = " #%&$DATASIMC2=";

        // set card numbers
        // TX_FMT_SIM_HEADER card is always card 1
        if (Intersection.mbov_version_defined_sim) {
            msiv_sim_title_card = 3; // TX_FMT_SIM_HEADER
                                     // TX_FMT_SIM_VERSION
        }
        else {
            msiv_sim_title_card = 2; // TX_FMT_SIM_HEADER
        }

        // if intersection control is signal controlled
        if (Intersection.mbov_is_ic_signal_controlled) {
            msiv_par_opt_card = msiv_sim_title_card + 1; // TX_FMT_SIM_TITLE
            msiv_par_opt_2_card = msiv_par_opt_card + 1; // TX_FMT_SIM_PAR_OPT
            msiv_lane_cont_card = msiv_par_opt_2_card + 1; // TX_FMT_SIM_PAR_OPT_2
            msiv_timing_card = msiv_lane_cont_card + 1; // TX_FMT_SIM_LANE_CONT

            // check if number of phases is valid
            if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_no_of_phases is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check number of phases for errors
            if (msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = " + msiv_no_of_phases + " is < "
                        + PARAMS.TEXAS_MODEL_MINNPH + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            if (Intersection.mbov_is_ic_PRETIMED) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check pretimed cycle length for errors
                if ((msiv_preclen_diafig_nemahitlov > 0) && (msiv_preclen_diafig_nemahitlov < PARAMS.TEXAS_MODEL_MINPCL)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < " + PARAMS.TEXAS_MODEL_MINPCL + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_tex_dia_opt_card = 0;
                msiv_nema_ph_ov_tx_card = 0;
                msiv_nema_ring_grp_card = 0;
                msiv_nema_ph_diag_card = 0;
                msiv_nema_ovlp_def_card = 0;
                msiv_first_grn_int_seq_card = msiv_timing_card + msiv_no_of_phases; // TX_FMT_SIM_PRET_SIGNAL
                msiv_psfps_nhmov_diaint_card = msiv_first_grn_int_seq_card + msiv_no_of_phases; // TX_FMT_SIM_GREEN_INT_SEQ
                msiv_first_det_card = 0;
                msiv_no_of_det = 0;
                msiv_first_con_card = 0;
                msiv_vms_message_card = msiv_psfps_nhmov_diaint_card + msiv_no_of_phases; // TX_FMT_SIM_PHASE_SEQ
            }
            else if (Intersection.mbov_is_ic_SEMI_ACT) {
                // check if number of detectors is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_preclen_diafig_nemahitlov = 0;
                msiv_tex_dia_opt_card = 0;
                msiv_nema_ph_ov_tx_card = 0;
                msiv_nema_ring_grp_card = 0;
                msiv_nema_ph_diag_card = 0;
                msiv_nema_ovlp_def_card = 0;
                msiv_first_grn_int_seq_card = msiv_timing_card + msiv_no_of_phases; // TX_FMT_SIM_SUNA_SIGNAL
                                                                                    // (1) and
                                                                                    // TX_FMT_SIM_SEMI_SIGNAL
                                                                                    // (msiv_no_of_phases-1)
                msiv_psfps_nhmov_diaint_card = msiv_first_grn_int_seq_card + msiv_no_of_phases; // TX_FMT_SIM_GREEN_INT_SEQ
                msiv_first_det_card = msiv_psfps_nhmov_diaint_card + msiv_no_of_phases; // TX_FMT_SIM_PHASE_SEQ
                msiv_first_con_card = msiv_first_det_card + msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        msiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                msiv_vms_message_card = msiv_first_con_card + msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONSFA
            }
            else if (Intersection.mbov_is_ic_FULL_ACT) {
                // check if number of detectors is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_preclen_diafig_nemahitlov = 0;
                msiv_tex_dia_opt_card = 0;
                msiv_nema_ph_ov_tx_card = 0;
                msiv_nema_ring_grp_card = 0;
                msiv_nema_ph_diag_card = 0;
                msiv_nema_ovlp_def_card = 0;
                msiv_first_grn_int_seq_card = msiv_timing_card + msiv_no_of_phases; // TX_FMT_SIM_FULL_SIGNAL
                msiv_psfps_nhmov_diaint_card = msiv_first_grn_int_seq_card + msiv_no_of_phases; // TX_FMT_SIM_GREEN_INT_SEQ
                msiv_first_det_card = msiv_psfps_nhmov_diaint_card + msiv_no_of_phases; // TX_FMT_SIM_PHASE_SEQ
                msiv_first_con_card = msiv_first_det_card + msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        msiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                msiv_vms_message_card = msiv_first_con_card + msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONSFA
            }
            else if (Intersection.mbov_is_ic_TEX_DIA) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check diamond figure number of errors
                if ((msiv_preclen_diafig_nemahitlov != 3) && (msiv_preclen_diafig_nemahitlov != 4) && (msiv_preclen_diafig_nemahitlov != 6) && (msiv_preclen_diafig_nemahitlov != 7)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if number of detectors is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_psfps_nhmov_diaint_card = msiv_timing_card + 6; // TX_FMT_SIM_TEXD_SIGNAL
                msiv_tex_dia_opt_card = msiv_psfps_nhmov_diaint_card + 4; // TX_FMT_SIM_TEX_DIA_INT
                msiv_nema_ph_ov_tx_card = 0;
                msiv_nema_ring_grp_card = 0;
                msiv_nema_ph_diag_card = 0;
                msiv_nema_ovlp_def_card = 0;
                msiv_first_grn_int_seq_card = msiv_tex_dia_opt_card + 4; // TX_FMT_SIM_TEX_DIA_OPT
                msiv_first_det_card = msiv_first_grn_int_seq_card + msiv_no_of_phases - 2 + // TX_FMT_SIM_GREEN_INT_SEQ
                        PARAMS.TEXAS_MODEL_NOT; // TX_FMT_SIM_GREEN_INT_SEQ
                msiv_first_con_card = 0;
                msiv_vms_message_card = msiv_first_det_card + msiv_no_of_det; // TX_FMT_SIM_DETECT_DATDIA
                for (lsiv_detector = 1; lsiv_detector <= msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        msiv_vms_message_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
            }
            else if (Intersection.mbov_is_ic_NEMA) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check NEMA/HARDWARE number of overlaps for errors
                if ((msiv_preclen_diafig_nemahitlov < 0) || (msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + msiv_preclen_diafig_nemahitlov + " is < 0 or > " + PARAMS.TEXAS_MODEL_NON + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if number of detectors is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_tex_dia_opt_card = 0;
                msiv_psfps_nhmov_diaint_card = msiv_timing_card + msiv_no_of_phases; // TX_FMT_SIM_NEM2_SIGNAL
                for (lsiv_phase = 1; lsiv_phase <= msiv_no_of_phases; lsiv_phase++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt.equals("YES")) {
                        msiv_psfps_nhmov_diaint_card++;
                    }
                }
                msiv_nema_ph_ov_tx_card = msiv_psfps_nhmov_diaint_card + 1; // TX_FMT_SIM_NEMA_MOVEMENT
                msiv_nema_ring_grp_card = msiv_nema_ph_ov_tx_card + 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                if (msiv_preclen_diafig_nemahitlov > 0) {
                    msiv_nema_ring_grp_card += 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                    msiv_nema_ph_diag_card = msiv_nema_ring_grp_card + 1; // TX_FMT_SIM_NEMA_RING_GRP
                    msiv_nema_ovlp_def_card = msiv_nema_ph_diag_card + 1; // TX_FMT_SIM_NEMA_PH_DIAG
                    msiv_first_grn_int_seq_card = msiv_nema_ovlp_def_card + msiv_preclen_diafig_nemahitlov; // TX_FMT_SIM_NEMA_OVLP_DEF
                }
                else {
                    msiv_nema_ph_diag_card = 0;
                    msiv_nema_ovlp_def_card = 0;
                    msiv_first_grn_int_seq_card = msiv_nema_ring_grp_card + 1; // TX_FMT_SIM_NEMA_RING_GRP
                }
                msiv_first_det_card = msiv_first_grn_int_seq_card + msiv_no_of_phases + // TX_FMT_SIM_GREEN_INT_SEQ
                        msiv_preclen_diafig_nemahitlov; // TX_FMT_SIM_GREEN_INT_SEQ
                msiv_first_con_card = msiv_first_det_card + msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        msiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                msiv_vms_message_card = msiv_first_con_card + msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONN2H
            }
            else if (Intersection.mbov_is_ic_HARDWARE) {
                // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number
                // of overlaps is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_preclen_diafig_nemahitlov is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check NEMA/HARDWARE number of overlaps for errors
                if ((msiv_preclen_diafig_nemahitlov < 0) || (msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_HOV)) {
                    lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                            + msiv_preclen_diafig_nemahitlov + " is < 0 or > " + PARAMS.TEXAS_MODEL_HOV + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if number of detectors is valid
                if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_DET] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: msiv_no_of_det is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                msiv_tex_dia_opt_card = 0;
                msiv_psfps_nhmov_diaint_card = msiv_timing_card + msiv_no_of_phases; // TX_FMT_SIM_HITL_SIGNAL
                for (lsiv_phase = 1; lsiv_phase <= msiv_no_of_phases; lsiv_phase++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt.equals("YES")) {
                        msiv_psfps_nhmov_diaint_card++;
                    }
                }
                msiv_nema_ph_ov_tx_card = msiv_psfps_nhmov_diaint_card + 1; // TX_FMT_SIM_NEMA_MOVEMENT
                msiv_nema_ring_grp_card = 0;
                msiv_nema_ph_diag_card = 0;
                msiv_nema_ovlp_def_card = 0;
                msiv_first_grn_int_seq_card = msiv_nema_ph_ov_tx_card + 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                if (msiv_preclen_diafig_nemahitlov > 0) {
                    msiv_first_grn_int_seq_card += 1; // TX_FMT_SIM_NEMA_PH_OV_TX
                }
                msiv_first_det_card = msiv_first_grn_int_seq_card + msiv_no_of_phases + // TX_FMT_SIM_GREEN_INT_SEQ
                        msiv_preclen_diafig_nemahitlov; // TX_FMT_SIM_GREEN_INT_SEQ
                msiv_first_con_card = msiv_first_det_card + msiv_no_of_det; // TX_FMT_SIM_DETECT_DATNDI
                for (lsiv_detector = 1; lsiv_detector <= msiv_no_of_det; lsiv_detector++) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].mstv_type.equals("CL")) {
                        msiv_first_con_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_detector].msiv_class_num + 4) / 5;
                    }
                }
                msiv_vms_message_card = msiv_first_con_card + msiv_no_of_phases; // TX_FMT_SIM_DETECT_CONN2H
            }
        }
        else {
            msiv_timing_card = 0;
            msiv_preclen_diafig_nemahitlov = 0;
            msiv_no_of_phases = 0;
            msiv_first_grn_int_seq_card = 0;
            msiv_psfps_nhmov_diaint_card = 0;
            msiv_first_det_card = 0;
            msiv_no_of_det = 0;
            msiv_first_con_card = 0;
            msiv_par_opt_card = msiv_sim_title_card + 1; // TX_FMT_SIM_TITLE
            msiv_par_opt_2_card = msiv_par_opt_card + 1; // TX_FMT_SIM_PAR_OPT
            msiv_lane_cont_card = msiv_par_opt_2_card + 1; // TX_FMT_SIM_PAR_OPT_2
            msiv_tex_dia_opt_card = 0;
            msiv_nema_ph_ov_tx_card = 0;
            msiv_nema_ring_grp_card = 0;
            msiv_nema_ph_diag_card = 0;
            msiv_nema_ovlp_def_card = 0;
            msiv_vms_message_card = msiv_lane_cont_card + 1; // TX_FMT_SIM_LANE_CONT
        } // end if intersection control is signal controlled

        // check if number of VMS messages is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        msiv_last_SIM_dat_card = msiv_vms_message_card + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages // TX_FMT_SIM_VMS_MESSAGE
                - 1;

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set number of cards for TX_FMT_SIM_GDV_REF_LEGD1 and TX_FMT_SIM_GDV_REF_LEGD2 or
        // TX_FMT_SIM_GDV_REF_LEGND
        // set number of cards for TX_FMT_SIM_GDV_REF_LANE and TX_FMT_SIM_GDV_REF_OFFS
        lsiv_no_legd = 1;
        lsiv_no_legs = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_no_legd += 1;
            lsiv_no_legs += 2;
        }

        msiv_ref_file_name_card = msiv_last_SIM_dat_card + 1;
        msiv_ref_file_leg_card = msiv_ref_file_name_card + 1; // TX_FMT_SIM_GDV_REF_FILE
        msiv_ref_file_lane_card = msiv_ref_file_leg_card + lsiv_no_legd; // TX_FMT_SIM_GDV_REF_LEGD1
                                                                         // and
                                                                         // TX_FMT_SIM_GDV_REF_LEGD2
                                                                         // or
                                                                         // TX_FMT_SIM_GDV_REF_LEGND
        msiv_last_GDV_ref_card = msiv_ref_file_lane_card + lsiv_no_legs + // TX_FMT_SIM_GDV_REF_LANE
                lsiv_no_legs - 1; // TX_FMT_SIM_GDV_REF_OFFS

        // set diamond interchange
        if (Intersection.mbov_is_diamond_interchange) {
            msiv_last_SIM_dat_card = -msiv_last_SIM_dat_card;
        }

        // set number of SIM cards written to last gdv reference data
        Intersection.msiv_simdataCardsWritten = msiv_last_GDV_ref_card;

        // set all cards as set by software
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_SIG] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TIMING_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_FIRST_DET_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_FIRST_CON_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LAST_GDV_REF_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PAR_OPT_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PAR_OPT_2_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LANE_CONT_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TEX_DIA_OPT_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_PH_OV_TX_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_RING_GRP_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_PH_DIA_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_OVLP_DEF_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_VMS_MESSAGE_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_REF_FILE_NAME_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_REF_FILE_LEG_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_REF_FILE_LANE_CARD] = Intersection.TX_SET_BY_SOFTWARE;

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards mstv_sim_sig                  =" + mstv_sim_sig);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_sim_title_card           =" + msiv_sim_title_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_timing_card              =" + msiv_timing_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_preclen_diafig_nemahitlov=" + msiv_preclen_diafig_nemahitlov);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_no_of_phases             =" + msiv_no_of_phases);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_first_grn_int_seq_card   =" + msiv_first_grn_int_seq_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_psfps_nhmov_diaint_card  =" + msiv_psfps_nhmov_diaint_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_first_det_card           =" + msiv_first_det_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_no_of_det                =" + msiv_no_of_det);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_first_con_card           =" + msiv_first_con_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_last_SIM_dat_card        =" + msiv_last_SIM_dat_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_last_GDV_ref_card        =" + msiv_last_GDV_ref_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_par_opt_card             =" + msiv_par_opt_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_par_opt_2_card           =" + msiv_par_opt_2_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_lane_cont_card           =" + msiv_lane_cont_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_tex_dia_opt_card         =" + msiv_tex_dia_opt_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_nema_ph_ov_tx_card       =" + msiv_nema_ph_ov_tx_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_nema_ring_grp_card       =" + msiv_nema_ring_grp_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_nema_ph_diag_card        =" + msiv_nema_ph_diag_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_nema_ovlp_def_card       =" + msiv_nema_ovlp_def_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_ref_file_name_card       =" + msiv_ref_file_name_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_ref_file_leg_card        =" + msiv_ref_file_leg_card);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards msiv_ref_file_lane_card       =" + msiv_ref_file_lane_card);

        // set local data for writing data to cards
        // TX_FMT_SIM_HEADER card is always card 1
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
        lsiv_card = 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Hdr.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeStringToCard(mstv_sim_sig, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_SIM_SIG, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_sim_title_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_timing_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_TIMING_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_preclen_diafig_nemahitlov, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_no_of_phases, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_first_grn_int_seq_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_psfps_nhmov_diaint_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_first_det_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_FIRST_DET_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_no_of_det, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_NO_OF_DET, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_first_con_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_FIRST_CON_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_last_SIM_dat_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_last_GDV_ref_card, lstv_name, Intersection.TX_FMT_SIM_HEADER, Intersection.TX_FMT_SIM_HEADER_LAST_GDV_REF_CARD, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class SIM_Hdr

/******************************************************************************/
/* SIM_Hdr.java */
/******************************************************************************/
