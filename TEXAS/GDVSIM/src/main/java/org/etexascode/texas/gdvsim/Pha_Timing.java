package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              Pha_Timing.java                               */
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
import javax.swing.JOptionPane;

class Pha_Timing {

    double mdfv_pre_grn_int; /* Pretimed: green interval */

    double mdfv_pre_yel_chg; /* Pretimed: yellow change interval */

    double mdfv_pre_red_clr; /* Pretimed: all-red clearance interval */

    double mdfv_una_min_grn; /*
                              * Semi-Actuated: non-actuated minimum green interval
                              */

    double mdfv_una_yel_chg; /*
                              * Semi-Actuated: non-actuated yellow change interval
                              */

    double mdfv_una_red_clr; /*
                              * Semi-Actuated: non-actuated all-red clearance interval
                              */

    double mdfv_act_ini_int; /* SA/FA/TexDia : actuated initial interval */

    double mdfv_act_veh_int; /* SA/FA/TexDia : actuated vehicle interval */

    double mdfv_act_yel_chg; /* SA/FA/TexDia : actuated yellow change interval */

    double mdfv_act_red_clr; /*
                              * SA/FA/TexDia : actuated all-red clearance interval
                              */

    double mdfv_act_max_ext; /* SA/FA/TexDia : actuated maximum extension */

    String mstv_act_skip_ph; /*
                              * SemAct/FulAct: actuated skip phase switch position (ON or OFF)
                              */

    String mstv_act_recall; /*
                             * SemAct/FulAct: actuated recall switch position (ON or OFF)
                             */

    String mstv_act_minor; /*
                            * SemAct/FulAct: actuated minor movement controller (YES or NO)
                            */

    String mstv_act_dual_lt; /*
                              * SemAct/FulAct: actuated dual lefts to be followed by two single
                              * lefts (YES or NO)
                              */

    double mdfv_nem1_ini_int; /* NEMA 1: initial interval */

    double mdfv_nem1_veh_int; /* NEMA 1: vehicle interval */

    double mdfv_nem1_yel_chg; /* NEMA 1: yellow change interval */

    double mdfv_nem1_red_clr; /* NEMA 1: all-red clearance interval */

    int msiv_nem1_max_ext; /* NEMA 1: maximum extension */

    int msiv_nem1_dual_ent_ph; /* NEMA 1: dual entry phase */

    String mstv_nem1_store_demand; /*
                                    * NEMA 1: provision for storing demand (YES or NO)
                                    */

    String mstv_nem1_max_recall; /* NEMA 1: maximum recall (YES or NO) */

    String mstv_nem1_min_recall; /* NEMA 1: minimum recall (YES or NO) */

    String mstv_nem1_call_on_maxout; /* NEMA 1: place call on maxout (YES or NO) */

    String mstv_nem1_use_volden_options; /*
                                          * NEMA 1: use volume density options (YES or NO)
                                          */

    double mdfv_nem1_volden_add_ini; /*
                                      * NEMA 1: volume density added initial interval per actuation
                                      */

    int msiv_nem1_volden_max_ini; /*
                                   * NEMA 1: volume density maximum initial interval
                                   */

    double mdfv_nem1_volden_min_veh; /*
                                      * NEMA 1: volume density minimum vehicle interval
                                      */

    int msiv_nem1_volden_tbr_veh; /*
                                   * NEMA 1: volume density time before reducing vehicle interval
                                   */

    int msiv_nem1_volden_tfr_veh; /*
                                   * NEMA 1: volume density time period for reducing vehicle
                                   * interval
                                   */

    int msiv_nem2_min_grn; /* NEMA 2: min green */

    double mdfv_nem2_pas_tim; /* NEMA 2: passage time */

    int msiv_nem2_max_1; /* NEMA 2: maximum 1 */

    int msiv_nem2_max_2; /* NEMA 2: maximum 2 */

    int msiv_nem2_max_1to2_time; /*
                                  * NEMA 2: time to switch from maximum 1 to maximim 2
                                  */

    double mdfv_nem2_yel_chg; /* NEMA 2: yellow change interval */

    double mdfv_nem2_red_clr; /* NEMA 2: all-red clearance interval */

    double mdfv_nem2_red_revert; /* NEMA 2: red revert */

    int msiv_nem2_walk; /* NEMA 2: walk */

    int msiv_nem2_ped_clr; /* NEMA 2: pedestrian clearance */

    int msiv_nem2_dual_ent_ph; /* NEMA 2: dual entry phase */

    String mstv_nem2_store_demand; /*
                                    * NEMA 2: provision for storing demand (YES or NO)
                                    */

    String mstv_nem2_max_recall; /* NEMA 2: maximum recall (YES or NO) */

    String mstv_nem2_min_recall; /* NEMA 2: minimum recall (YES or NO) */

    String mstv_nem2_ped_recall; /* NEMA 2: pedestrian recall (YES or NO) */

    String mstv_nem2_ped_recycle; /* NEMA 2: pedestrian recycle (YES or NO) */

    String mstv_nem2_call_at_phase_term; /*
                                          * NEMA 2: place call at phase termination (maximum time
                                          * out) (YES or NO)
                                          */

    String mstv_nem2_conditional_service; /*
                                           * NEMA 2: conditional service (YES or NO)
                                           */

    String mstv_nem2_use_volden_options; /*
                                          * NEMA 2: use volume density options (YES or NO)
                                          */

    double mdfv_nem2_volden_add_ini; /*
                                      * NEMA 2: volume density added initial interval per actuation
                                      */

    int msiv_nem2_volden_max_ini; /*
                                   * NEMA 2: volume density maximum initial interval
                                   */

    int msiv_nem2_volden_t2r_veh; /* NEMA 2: volume density time to reduce */

    int msiv_nem2_volden_tb4r_veh; /*
                                    * NEMA 2: volume density time before reduction
                                    */

    double mdfv_nem2_volden_min_gap; /* NEMA 2: volume density minimum gap */

    String mstv_nem2_ped_mvmt; /*
                                * NEMA 2/HITL: pedestrian movement data specified
                                */

    String mstv_nem2_ped_mvmt_dist; /*
                                     * NEMA 2/HITL: pedestrian movement name for headway frequency
                                     * distribution
                                     */

    int msiv_nem2_ped_mvmt_vol; /*
                                 * NEMA 2/HITL: pedestrian movement total hourly volume for traffic
                                 * signal phase
                                 */

    double mdfv_nem2_ped_mvmt_par; /*
                                    * NEMA 2/HITL: pedestrian movement parameter for headway
                                    * frequency distribution
                                    */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_PRET_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_grn_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_PRET_SIGNAL_GRN_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_PRET_SIGNAL_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_PRET_SIGNAL_RED_CLR ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_SUNA_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_una_min_grn
    // .mclv_aux.msia_stat[TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_una_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_una_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_SEMI_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_skip_ph
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_minor
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_dual_lt
    // .mclv_aux.msia_stat[TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_FULL_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_INI_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_VEH_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_RED_CLR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_MAX_EXT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_skip_ph
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_SKIP_PH ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_minor
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_MINOR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_dual_lt
    // .mclv_aux.msia_stat[TX_FMT_SIM_FULL_SIGNAL_ACT_DUAL_LT ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_NEM1_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem1_ini_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_INI_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem1_veh_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_VEH_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem1_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem1_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_RED_CLR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem1_max_ext
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_MAX_EXT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem1_dual_ent_ph
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_DUAL_ENT_PH ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem1_store_demand
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_STORE_DEMAND ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem1_max_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_MAX_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem1_min_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_MIN_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem1_call_on_maxout
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_CALL_ON_MAXOT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem1_use_volden_options
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_USE_VOLDENOPT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem1_volden_add_ini
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_ADDINI]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem1_volden_max_ini
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_MAXINI]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem1_volden_min_veh
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_MINVEH]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem1_volden_tbr_veh
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_TBRVEH]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem1_volden_tfr_veh
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_TFRVEH]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_NEM2_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_min_grn
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_pas_tim
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_1
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_MAX_1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_2
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_MAX_2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_1to2_time
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_RED_CLR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_red_revert
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_walk
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_WALK ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_CLR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_dual_ent_ph
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_store_demand
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_max_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_min_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_recall
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_recycle
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_call_at_phase_term
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_conditional_service
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_use_volden_options
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_volden_add_ini
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_max_ini
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_t2r_veh
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_tb4r_veh
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_volden_min_gap
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt_dist
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_mvmt_vol
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_HITL_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt
    // .mclv_aux.msia_stat[TX_FMT_SIM_HITL_SIGNAL_PED_MVMT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt_dist
    // .mclv_aux.msia_stat[TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_mvmt_vol
    // .mclv_aux.msia_stat[TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par
    // .mclv_aux.msia_stat[TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR ]

    // mdfv_nem2_ped_mvmt_par can also be read/printed using
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_HDWAY_PARAM]
    // [TX_FMT_GDV_HDWAY_PARAM_CONSTAN]
    // [TX_FMT_GDV_HDWAY_PARAM_ERLANG ]
    // [TX_FMT_GDV_HDWAY_PARAM_GAMMA ]
    // [TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
    // [TX_FMT_GDV_HDWAY_PARAM_NEGEXP ]
    // [TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
    // [TX_FMT_GDV_HDWAY_PARAM_UNIFORM]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_TEXD_SIGNAL]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEXD_SIGNAL_ACT_VEH_INT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEXD_SIGNAL_ACT_YEL_CHG ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEXD_SIGNAL_ACT_RED_CLR ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEXD_SIGNAL_ACT_MAX_EXT ]

    public Pha_Timing() {
        mclv_aux = new TX_Aux();
    } // end of method Pha_Timing

    public void checkForErrors(int psiv_phase // signal phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_field_beg;
        int lsiv_field_end;
        String lstv_name;

        // checkForErrors checks all Pha_Timing data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Pha_Timing.checkForErrors psiv_phase=" + psiv_phase);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_VERSION_GDV_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_gdv_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is PRETIMED, SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (Intersection.mbov_is_ic_PRETIMED) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PRET_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT;
            lsiv_field_end = Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase == 1)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SUNA_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN;
            lsiv_field_end = Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase >= 2)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SEMI_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT;
            lsiv_field_end = Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT;
        }
        else if (Intersection.mbov_is_ic_FULL_ACT) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_FULL_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_INI_INT;
            lsiv_field_end = Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_DUAL_LT;
        }
        else if (Intersection.mbov_is_ic_TEX_DIA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEXD_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT;
            lsiv_field_end = Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_MAX_EXT;
        }
        else if (Intersection.mbov_is_ic_NEMA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEM2_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN;
            lsiv_field_end = Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR;
        }
        else if (Intersection.mbov_is_ic_HARDWARE) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HITL_SIGNAL];
            lsiv_field_beg = Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT;
            lsiv_field_end = Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR;
        }
        else {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check beginning and ending fields
        if ((lsiv_field_beg < 1) || (lsiv_field_beg > lclv_tx_fmt.msiv_nf)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: lsiv_field_beg = " + lsiv_field_beg + " is < 1 or > " + lclv_tx_fmt.msiv_nf + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if ((lsiv_field_end < 1) || (lsiv_field_end > lclv_tx_fmt.msiv_nf)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: lsiv_field_end = " + lsiv_field_end + " is < 1 or > " + lclv_tx_fmt.msiv_nf + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (lsiv_field_end < lsiv_field_beg) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: lsiv_field_end = " + lsiv_field_end + " is < lsiv_field_beg = " + lsiv_field_beg + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mbov_is_ic_NEMA || Intersection.mbov_is_ic_HARDWARE) {
            if (mstv_nem2_ped_mvmt.equals("YES")) {
                // check and set headway distribution Intersection.mbov_is_hd_* values
                Intersection.check_and_set_headway_distribution(mstv_nem2_ped_mvmt_dist);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
            }
        }

        // set name
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Pha_Timing.checkForErrors checking " + lstv_name + " - fields " + lsiv_field_beg + " through " + lsiv_field_end);
        // check all data for errors
        for (lsiv_field = lsiv_field_beg; lsiv_field <= lsiv_field_end; lsiv_field++) {
            if (Intersection.mbov_is_ic_NEMA) {
                if ((lsiv_field == Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST) || (lsiv_field == Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL)
                        || (lsiv_field == Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR)) {
                    if (mstv_nem2_ped_mvmt.equals("YES")) {
                        if (!Intersection.mbov_is_hd_parameter_needed) {
                            // if pedestrian movements specified then ignore parameter if not needed
                            if (lsiv_field == Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR)
                                continue;
                        }
                    }
                    else {
                        // if pedestrian movements not specified then ignore
                        // mstv_nem2_ped_mvmt_dist, msiv_nem2_ped_mvmt_vol, and
                        // mdfv_nem2_ped_mvmt_par
                        continue;
                    }
                }
            }
            else if (Intersection.mbov_is_ic_HARDWARE) {
                if ((lsiv_field == Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST) || (lsiv_field == Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL)
                        || (lsiv_field == Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR)) {
                    if (mstv_nem2_ped_mvmt.equals("YES")) {
                        if (!Intersection.mbov_is_hd_parameter_needed) {
                            // if pedestrian movements specified then ignore parameter if not needed
                            if (lsiv_field == Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR)
                                continue;
                        }
                    }
                    else {
                        // if pedestrian movements not specified then ignore
                        // mstv_nem2_ped_mvmt_dist, msiv_nem2_ped_mvmt_vol, and
                        // mdfv_nem2_ped_mvmt_par
                        continue;
                    }
                }
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Pha_Timing.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_phase // signal phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lines;
        int lsiv_member;
        String lstv_name;

        // printToFile prints all Pha_Timing data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Pha_Timing.printToFile psiv_phase=" + psiv_phase);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is PRETIMED, SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (Intersection.mbov_is_ic_PRETIMED) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PRET_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);
            // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
            // overlaps is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check pretimed cycle length for errors
            if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0)
                    && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < PARAMS.TEXAS_MODEL_MINPCL)) {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < " + PARAMS.TEXAS_MODEL_MINPCL + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(3);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_pre_grn_int, lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_pre_yel_chg, lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_pre_red_clr, lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase == 1)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SUNA_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(3);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_una_min_grn, lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_una_yel_chg, lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_una_red_clr, lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase >= 2)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SEMI_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(9);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_ini_int, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_veh_int, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_yel_chg, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_red_clr, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_max_ext, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_skip_ph, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_recall, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_minor, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_dual_lt, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_FULL_ACT) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_FULL_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(9);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_ini_int, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_INI_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_veh_int, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_VEH_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_yel_chg, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_YEL_CHG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_red_clr, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_RED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_max_ext, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_MAX_EXT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_skip_ph, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_SKIP_PH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_recall, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_RECALL, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_minor, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_MINOR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_act_dual_lt, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_DUAL_LT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_TEX_DIA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEXD_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(5);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_ini_int, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_veh_int, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_VEH_INT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_yel_chg, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_YEL_CHG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_red_clr, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_RED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_act_max_ext, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_MAX_EXT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_NEMA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEM2_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);

            // set number of lines to print
            lsiv_lines = 20;

            // check if volume density option is valid
            if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem1_use_volden_options is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            if (mstv_nem2_use_volden_options.equals("YES")) {
                lsiv_lines += 5;
            }

            // check if pedestrian movement is valid
            if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[psiv_phase].mstv_nem2_ped_mvmt is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if pedestrian movement is "YES"
            if (mstv_nem2_ped_mvmt.equals("YES")) {
                lsiv_lines += 2;
                if ((Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                                .startsWith(mstv_nem2_ped_mvmt_dist))) {
                    lsiv_lines += 1;
                }
            }

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(lsiv_lines);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_min_grn, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_nem2_pas_tim, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_max_1, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_max_2, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_2, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_max_1to2_time, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_nem2_yel_chg, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_nem2_red_clr, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_nem2_red_revert, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_walk, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_WALK, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_ped_clr, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_nem2_dual_ent_ph, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_store_demand, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_max_recall, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_min_recall, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_ped_recall, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_ped_recycle, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_call_at_phase_term, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_conditional_service, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_use_volden_options, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write volume density parameter if volume density option is YES or non-default value
            if (mstv_nem2_use_volden_options.equals("YES")) {
                // print data to a file
                Intersection.filesPrintSIM_DoubleToFile(mdfv_nem2_volden_add_ini, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msiv_nem2_volden_max_ini, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msiv_nem2_volden_t2r_veh, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msiv_nem2_volden_tb4r_veh, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // print data to a file
                Intersection.filesPrintSIM_DoubleToFile(mdfv_nem2_volden_min_gap, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
            } // end if ( mstv_nem2_use_volden_options.equals ( "YES" ) )

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_ped_mvmt, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_nem2_ped_mvmt.equals("YES")) {
                // print data to a file
                Intersection.filesPrintGDV_StringToFile(mstv_nem2_ped_mvmt_dist, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // print data to a file
                Intersection.filesPrintGDV_IntToFile(msiv_nem2_ped_mvmt_vol, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                if ((Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                                .startsWith(mstv_nem2_ped_mvmt_dist))) {
                    lsiv_member = 0;
                    if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }

                    // print data to a file - use TX_FMT_GDV_HDWAY_PARAM
                    Intersection.filesPrintGDV_DoubleToFile(mdfv_nem2_ped_mvmt_par, lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, lsiv_member, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                            Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            } // end if ( mstv_nem2_ped_mvmt.equals ( "YES" ) )
        }
        else if (Intersection.mbov_is_ic_HARDWARE) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HITL_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_printSIM)
                System.out.println("Pha_Timing.printToFile printing " + lstv_name);

            // set number of lines to print
            lsiv_lines = 1;

            // check if pedestrian movement is valid
            if (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[psiv_phase].mstv_nem2_ped_mvmt is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if pedestrian movement is "YES"
            if (mstv_nem2_ped_mvmt.equals("YES")) {
                lsiv_lines += 2;
                if ((Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                                .startsWith(mstv_nem2_ped_mvmt_dist))) {
                    lsiv_lines += 1;
                }
            }

            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(lsiv_lines);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_ped_mvmt, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_nem2_ped_mvmt.equals("YES")) {
                // print data to a file
                Intersection.filesPrintGDV_StringToFile(mstv_nem2_ped_mvmt_dist, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // print data to a file
                Intersection.filesPrintGDV_IntToFile(msiv_nem2_ped_mvmt_vol, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                if ((Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                                .startsWith(mstv_nem2_ped_mvmt_dist))) {
                    lsiv_member = 0;
                    if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_member = Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM;
                        if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member])
                                || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member])) {
                            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: " + lstv_name + " - "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[lsiv_member] + " = " + mdfv_nem2_ped_mvmt_par + " is < "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[lsiv_member] + " or > "
                                    + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[lsiv_member] + ".";
                            Intersection.errorMessage();
                            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                            return;
                        }
                    }

                    // print data to a file - use TX_FMT_GDV_HDWAY_PARAM
                    Intersection.filesPrintGDV_DoubleToFile(mdfv_nem2_ped_mvmt_par, lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, lsiv_member, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                            Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            } // end if ( mstv_nem2_ped_mvmt.equals ( "YES" ) )
        }
        else {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_phase, // signal phase number
            int psiv_card // card number in pcla_cards
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_fo_new;
        int lsiv_fo_old;
        String lstv_name;

        // readFromCards reads all Pha_Timing fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Pha_Timing.readFromCards psiv_phase=" + psiv_phase + " psiv_card=" + psiv_card);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if green interval sequence card is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if ((psiv_card < 1) || (psiv_card > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: psiv_card = " + psiv_card + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card + ".";
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            Intersection.errorMessage();
            return;
        }

        // check if timing card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TIMING_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_gdv_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is PRETIMED, SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (Intersection.mbov_is_ic_PRETIMED) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PRET_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
            // read data from cards
            mdfv_pre_grn_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_pre_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_pre_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
            // overlaps is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check pretimed cycle length for errors
            if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0)
                    && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < PARAMS.TEXAS_MODEL_MINPCL)) {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < " + PARAMS.TEXAS_MODEL_MINPCL + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // convert pretimed signal cycle length and percent of cycle length to phase lengths
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0) {
                mdfv_pre_grn_int *= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov;
                mdfv_pre_yel_chg *= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov;
                mdfv_pre_red_clr *= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov;
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PRET_SIGNAL];
                if ((mdfv_pre_grn_int < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT]) || (mdfv_pre_grn_int > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT])) {
                    Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Card " + psiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT] + " = " + mdfv_pre_grn_int + " is < "
                            + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT] + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT] + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                if ((mdfv_pre_yel_chg < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG]) || (mdfv_pre_yel_chg > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG])) {
                    Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Card " + psiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG] + " = " + mdfv_pre_yel_chg + " is < "
                            + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG] + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG] + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                if ((mdfv_pre_red_clr < lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR]) || (mdfv_pre_red_clr > lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR])) {
                    Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Card " + psiv_card + " " + lstv_name + " - "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR] + " = " + mdfv_pre_red_clr + " is < "
                            + lclv_tx_fmt.mdfa_min[Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR] + " or > " + lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR] + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
                Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = Intersection.TX_SET_BY_SOFTWARE;
            }
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase == 1)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SUNA_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
            // read data from cards
            mdfv_una_min_grn = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_una_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_una_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase >= 2)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SEMI_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
            // read data from cards
            mdfv_act_ini_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_veh_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_max_ext = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_skip_ph = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_minor = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_dual_lt = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_FULL_ACT) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_FULL_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
            // read data from cards
            mdfv_act_ini_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_INI_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_veh_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_VEH_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_max_ext = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_MAX_EXT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_skip_ph = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_SKIP_PH, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_minor = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_MINOR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mstv_act_dual_lt = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_DUAL_LT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_TEX_DIA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEXD_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
            // read data from cards
            mdfv_act_ini_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_veh_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_VEH_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // read data from cards
            mdfv_act_max_ext = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_MAX_EXT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_NEMA) {
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mdfv_sim_ver < 6.00) {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEM1_SIGNAL];
                lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
                // read data from cards
                mdfv_nem1_ini_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_INI_INT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem1_veh_int = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_VEH_INT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem1_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_YEL_CHG, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem1_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_RED_CLR, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem1_max_ext = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_MAX_EXT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem1_dual_ent_ph = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_DUAL_ENT_PH, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem1_store_demand = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_STORE_DEMAND,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem1_max_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_MAX_RECALL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem1_min_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_MIN_RECALL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem1_call_on_maxout = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_CALL_ON_MAXOT,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem1_use_volden_options = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_USE_VOLDENOPT,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem1_volden_add_ini = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_ADDINI,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem1_volden_max_ini = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_MAXINI,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem1_volden_min_veh = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_MINVEH,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem1_volden_tbr_veh = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_TBRVEH,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem1_volden_tfr_veh = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM1_SIGNAL, Intersection.TX_FMT_SIM_NEM1_SIGNAL_VOLDEN_TFRVEH,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // set NEMA 2 parameters using NEMA 1 data
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEM2_SIGNAL];
                msiv_nem2_min_grn = Intersection.nint(mdfv_nem1_ini_int + mdfv_nem1_veh_int);
                mdfv_nem2_pas_tim = mdfv_nem1_veh_int;
                msiv_nem2_max_1 = msiv_nem1_max_ext;
                msiv_nem2_max_2 = msiv_nem1_max_ext;
                msiv_nem2_max_1to2_time = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME];
                mdfv_nem2_yel_chg = mdfv_nem1_yel_chg;
                mdfv_nem2_red_clr = mdfv_nem1_red_clr;
                mdfv_nem2_red_revert = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT];
                msiv_nem2_walk = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_WALK];
                msiv_nem2_ped_clr = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR];
                msiv_nem2_dual_ent_ph = msiv_nem1_dual_ent_ph;
                mstv_nem2_store_demand = mstv_nem1_store_demand;
                mstv_nem2_max_recall = mstv_nem1_max_recall;
                mstv_nem2_min_recall = mstv_nem1_min_recall;
                mstv_nem2_ped_recall = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL];
                mstv_nem2_ped_recycle = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE];
                mstv_nem2_call_at_phase_term = mstv_nem1_call_on_maxout;
                mstv_nem2_conditional_service = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE];
                mstv_nem2_use_volden_options = mstv_nem1_use_volden_options;
                mdfv_nem2_volden_add_ini = mdfv_nem1_volden_add_ini;
                msiv_nem2_volden_max_ini = msiv_nem1_volden_max_ini;
                msiv_nem2_volden_t2r_veh = msiv_nem1_volden_tfr_veh;
                msiv_nem2_volden_tb4r_veh = msiv_nem1_volden_tbr_veh;
                mdfv_nem2_volden_min_gap = mdfv_nem1_volden_min_veh;
                mstv_nem2_ped_mvmt = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT];
                mstv_nem2_ped_mvmt_dist = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST];
                msiv_nem2_ped_mvmt_vol = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL];
                mdfv_nem2_ped_mvmt_par = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR];

                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_2] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_WALK] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP] = Intersection.TX_FROM_FILE;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL] = Intersection.TX_DATA_IS_INVALID;
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR] = Intersection.TX_DATA_IS_INVALID;
            }
            else {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEM2_SIGNAL];
                lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
                // read data from cards
                msiv_nem2_min_grn = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_pas_tim = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                msiv_nem2_max_1 = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                msiv_nem2_max_2 = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_2, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                msiv_nem2_max_1to2_time = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_yel_chg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_red_clr = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_red_revert = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                msiv_nem2_walk = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_WALK, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                msiv_nem2_ped_clr = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem2_dual_ent_ph = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_store_demand = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_max_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_min_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_ped_recall = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_ped_recycle = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_call_at_phase_term = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_conditional_service = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_use_volden_options = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_volden_add_ini = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem2_volden_max_ini = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem2_volden_t2r_veh = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem2_volden_tb4r_veh = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_volden_min_gap = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mstv_nem2_ped_mvmt = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                if (mstv_nem2_ped_mvmt.equals("YES")) {
                    // read data from cards
                    mstv_nem2_ped_mvmt_dist = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT,
                            Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // read data from cards
                    msiv_nem2_ped_mvmt_vol = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // read data from cards
                    mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;

                    // set local data for reading data from cards to re-read traffic headway
                    // parameter
                    lsiv_fo_new = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR];
                    lstv_name = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
                    /* debug */if (Intersection.mbov_debug_filesReadSIM)
                        System.out.println("Pha_Timing.readFromCards reading " + lstv_name);

                    if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_CONSTAN].startsWith(mstv_nem2_ped_mvmt_dist)) {
                        mdfv_nem2_ped_mvmt_par = 0.0;
                        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR] = Intersection.TX_SET_BY_SOFTWARE;
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = lsiv_fo_new;
                        mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG,
                                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = lsiv_fo_old;
                        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                            return;
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = lsiv_fo_new;
                        mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA,
                                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = lsiv_fo_old;
                        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                            return;
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = lsiv_fo_new;
                        mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML,
                                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = lsiv_fo_old;
                        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                            return;
                    }
                    else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]
                            .startsWith(mstv_nem2_ped_mvmt_dist)) {
                        mdfv_nem2_ped_mvmt_par = 0.0;
                        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR] = Intersection.TX_SET_BY_SOFTWARE;
                    }
                    if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP].startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = lsiv_fo_new;
                        mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP,
                                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = lsiv_fo_old;
                        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                            return;
                    }
                    if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM].startsWith(mstv_nem2_ped_mvmt_dist)) {
                        lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = lsiv_fo_new;
                        mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM,
                                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                        Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = lsiv_fo_old;
                        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                            return;
                    }
                }
            }
        }
        else if (Intersection.mbov_is_ic_HARDWARE) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HITL_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesReadSIM)
                System.out.println("Pha_Timing.readFromCards reading " + lstv_name);
            // read data from cards
            mstv_nem2_ped_mvmt = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, psiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_nem2_ped_mvmt.equals("YES")) {
                // read data from cards
                mstv_nem2_ped_mvmt_dist = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                msiv_nem2_ped_mvmt_vol = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // read data from cards
                mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // set local data for reading data from cards to re-read traffic headway parameter
                lsiv_fo_new = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR];
                lstv_name = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("Pha_Timing.readFromCards reading " + lstv_name);

                if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_CONSTAN].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    mdfv_nem2_ped_mvmt_par = 0.0;
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR] = Intersection.TX_SET_BY_SOFTWARE;
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = lsiv_fo_new;
                    mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = lsiv_fo_old;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = lsiv_fo_new;
                    mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA, Intersection.mcla_simdataCards,
                            Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = lsiv_fo_old;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = lsiv_fo_new;
                    mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = lsiv_fo_old;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_NEGEXP].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    mdfv_nem2_ped_mvmt_par = 0.0;
                    mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR] = Intersection.TX_SET_BY_SOFTWARE;
                }
                if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = lsiv_fo_new;
                    mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = lsiv_fo_old;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    lsiv_fo_old = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = lsiv_fo_new;
                    mdfv_nem2_ped_mvmt_par = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_HDWAY_PARAM, Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, psiv_card + 1, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msia_fo[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = lsiv_fo_old;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }
        else {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Pha_Timing fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Pha_Timing.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_phase, // signal phase number
            int psiv_card // card number in pcla_cards
    ) {
        TX_Fmt lclv_tx_fmt = null;
        String lstv_name;

        // writeToCards writes all Pha_Timing fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Pha_Timing.writeToCards psiv_phase=" + psiv_phase + " psiv_card=" + psiv_card);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if green interval sequence card is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if ((psiv_card < 1) || (psiv_card > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card)) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: psiv_card = " + psiv_card + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card + ".";
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            Intersection.errorMessage();
            return;
        }

        // check if timing card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TIMING_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is PRETIMED, SEMI-ACT, FULL-ACT, TEX-DIA, NEMA, or HARDWARE
        if (Intersection.mbov_is_ic_PRETIMED) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PRET_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
            // overlaps is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check pretimed cycle length for errors
            if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0)
                    && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < PARAMS.TEXAS_MODEL_MINPCL)) {
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
                Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < " + PARAMS.TEXAS_MODEL_MINPCL + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_pre_grn_int, lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_GRN_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_pre_yel_chg, lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_pre_red_clr, lstv_name, Intersection.TX_FMT_SIM_PRET_SIGNAL, Intersection.TX_FMT_SIM_PRET_SIGNAL_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase == 1)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SUNA_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_una_min_grn, lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_una_yel_chg, lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_una_red_clr, lstv_name, Intersection.TX_FMT_SIM_SUNA_SIGNAL, Intersection.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_SEMI_ACT && (psiv_phase >= 2)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_SEMI_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_ini_int, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_veh_int, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_yel_chg, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_red_clr, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_max_ext, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_skip_ph, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_recall, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_minor, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_dual_lt, lstv_name, Intersection.TX_FMT_SIM_SEMI_SIGNAL, Intersection.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_FULL_ACT) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_FULL_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_ini_int, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_INI_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_veh_int, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_VEH_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_yel_chg, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_red_clr, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_max_ext, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_MAX_EXT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_skip_ph, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_SKIP_PH, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_recall, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_minor, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_MINOR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_act_dual_lt, lstv_name, Intersection.TX_FMT_SIM_FULL_SIGNAL, Intersection.TX_FMT_SIM_FULL_SIGNAL_ACT_DUAL_LT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_TEX_DIA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEXD_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_ini_int, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_veh_int, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_VEH_INT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_yel_chg, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_red_clr, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_act_max_ext, lstv_name, Intersection.TX_FMT_SIM_TEXD_SIGNAL, Intersection.TX_FMT_SIM_TEXD_SIGNAL_ACT_MAX_EXT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else if (Intersection.mbov_is_ic_NEMA) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEM2_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_min_grn, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_nem2_pas_tim, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_max_1, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_max_2, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_max_1to2_time, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_nem2_yel_chg, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_nem2_red_clr, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_nem2_red_revert, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_walk, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_WALK, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_ped_clr, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_dual_ent_ph, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_store_demand, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_max_recall, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_min_recall, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_ped_recall, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_ped_recycle, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_call_at_phase_term, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_conditional_service, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_use_volden_options, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_nem2_volden_add_ini, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_volden_max_ini, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_volden_t2r_veh, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeIntToCard(msiv_nem2_volden_tb4r_veh, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_nem2_volden_min_gap, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_ped_mvmt, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_nem2_ped_mvmt.equals("YES")) {
                // write data to cards
                Intersection.writeStringToCard(mstv_nem2_ped_mvmt_dist, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card + 1, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // write data to cards
                Intersection.writeIntToCard(msiv_nem2_ped_mvmt_vol, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, psiv_card + 1, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }

                if ((Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                                .startsWith(mstv_nem2_ped_mvmt_dist))) {
                    // write data to cards
                    Intersection.writeDoubleToCard(mdfv_nem2_ped_mvmt_par, lstv_name, Intersection.TX_FMT_SIM_NEM2_SIGNAL, Intersection.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card + 1, mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }
        else if (Intersection.mbov_is_ic_HARDWARE) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HITL_SIGNAL];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
            /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                System.out.println("Pha_Timing.writeToCards writing " + lstv_name);
            // write data to cards
            Intersection.writeStringToCard(mstv_nem2_ped_mvmt, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, psiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_nem2_ped_mvmt.equals("YES")) {
                // write data to cards
                Intersection.writeStringToCard(mstv_nem2_ped_mvmt_dist, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST,
                        Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card + 1, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                // write data to cards
                Intersection.writeIntToCard(msiv_nem2_ped_mvmt_vol, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, psiv_card + 1, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }
                else if (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM].startsWith(mstv_nem2_ped_mvmt_dist)) {
                    if ((mdfv_nem2_ped_mvmt_par < Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM])
                            || (mdfv_nem2_ped_mvmt_par > Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM])) {
                        Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Card " + psiv_card + " " + lstv_name + " - "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " = "
                                + mdfv_nem2_ped_mvmt_par + " is < "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_min[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " or > "
                                + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].mdfa_max[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + ".";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                }

                if ((Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_ERLANG].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_GAMMA].startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]
                                .startsWith(mstv_nem2_ped_mvmt_dist))
                        || (Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HDWAY_PARAM].msta_desc[Intersection.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]
                                .startsWith(mstv_nem2_ped_mvmt_dist))) {
                    // write data to cards
                    Intersection.writeDoubleToCard(mdfv_nem2_ped_mvmt_par, lstv_name, Intersection.TX_FMT_SIM_HITL_SIGNAL, Intersection.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, psiv_card + 1, mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }
        else {
            Intersection.mstv_errorMessage = "Error in Pha_Timing.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Pha_Timing

/******************************************************************************/
/* Pha_Timing.java */
/******************************************************************************/
