package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            NEMA_ring_group.java                            */
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

class NEMA_ring_group {

    int msiv_num_rings; /* NEMA number of rings */

    int msiv_num_groups; /* NEMA number of groups */

    int msia_ring_group_num_ph[][]; /*
                                     * NEMA ring group number of phases [ring][group]
                                     */

    int msia_ring_group_phases[][][]; /*
                                       * NEMA ring group phase numbers [ring][group][phase]
                                       */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_NEMA_RING_GRP]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[1][1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[1][2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[1][3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[1][4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[2][1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R2G1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[2][2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R2G2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[2][3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R2G3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[2][4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R2G4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[3][1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R3G1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[3][2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R3G2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[3][3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R3G3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[3][4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R3G4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[4][1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R4G1 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[4][2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R4G2 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[4][3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R4G3 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[4][4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_NPH_R4G4 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][1][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G1_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][2][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G2_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][3][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G3_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[1][4][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R1G4_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][1][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G1_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][2][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G2_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][3][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G3_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[2][4][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R2G4_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][1][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G1_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][2][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G2_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][3][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G3_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[3][4][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R3G4_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][1][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G1_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][2][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G2_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][3][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G3_P16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[4][4][16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_RING_GRP_R4G4_P16 ]

    public NEMA_ring_group() {
        msia_ring_group_num_ph = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NGR + 1];
        msia_ring_group_phases = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NGR + 1][PARAMS.TEXAS_MODEL_NRGP + 1];
        mclv_aux = new TX_Aux();
    } // end of method NEMA_ring_group

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_group;
        int lsiv_phase;
        int lsiv_ring;
        String lstv_name;

        // checkForErrors checks all NEMA_ring_group data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("NEMA_ring_group.checkForErrors");
        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_RING_GRP];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("NEMA_ring_group.checkForErrors checking " + lstv_name);

        // check all data for errors
        lsiv_field = Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS;
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        lsiv_field = Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS;
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        for (lsiv_ring = 1; lsiv_ring <= msiv_num_rings; lsiv_ring++) {
            for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
                lsiv_field = Intersection.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR + lsiv_group;
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                for (lsiv_phase = 1; lsiv_phase <= msia_ring_group_num_ph[lsiv_ring][lsiv_group]; lsiv_phase++) {
                    lsiv_field = Intersection.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR * PARAMS.TEXAS_MODEL_NRGP + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                            + lsiv_phase;
                    if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                        Intersection.mstv_warningMessage = "Warning in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                        Intersection.warningMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                        return;
                    }

                    if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in NEMA_ring_group.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                } // end for ( lsiv_phase = 1 ; lsiv_phase <=
                  // msia_ring_group_num_ph[lsiv_ring][lsiv_group] ; lsiv_phase++ )
            } // end for ( lsiv_group = 1 ; lsiv_group <= msiv_num_groups ; lsiv_group++ )
        } // end for ( lsiv_ring = 1 ; lsiv_ring <= msiv_num_rings ; lsiv_ring++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_group;
        int lsiv_lines;
        int lsiv_max;
        int lsiv_member;
        int lsiv_phase;
        int lsiv_ring;
        String lstv_name;

        // printToFile prints all NEMA_ring_group data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("NEMA_ring_group.printToFile");
        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_RING_GRP];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("NEMA_ring_group.printToFile printing " + lstv_name);

        lsiv_lines = 2;
        for (lsiv_ring = 1; lsiv_ring <= msiv_num_rings; lsiv_ring++) {
            for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
                lsiv_lines++;
                for (lsiv_phase = 1; lsiv_phase <= msia_ring_group_num_ph[lsiv_ring][lsiv_group]; lsiv_phase++) {
                    lsiv_lines++;
                } // end for ( lsiv_phase = 1 ; lsiv_phase <=
                  // msia_ring_group_num_ph[lsiv_ring][lsiv_group] ; lsiv_phase++ )
            } // end for ( lsiv_group = 1 ; lsiv_group <= msiv_num_groups ; lsiv_group++ )
        } // end for ( lsiv_ring = 1 ; lsiv_ring <= msiv_num_rings ; lsiv_ring++ )

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintSIM_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_num_rings, lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_num_groups, lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_ring = 1; lsiv_ring <= msiv_num_rings; lsiv_ring++) {
            for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
                lsiv_member = Intersection.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR + lsiv_group;
                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msia_ring_group_num_ph[lsiv_ring][lsiv_group], lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, lsiv_member, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                for (lsiv_phase = 1; lsiv_phase <= msia_ring_group_num_ph[lsiv_ring][lsiv_group]; lsiv_phase++) {
                    lsiv_member = Intersection.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR * PARAMS.TEXAS_MODEL_NRGP + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                            + lsiv_phase;
                    lsiv_max = lclv_tx_fmt.msia_max[lsiv_member];
                    lclv_tx_fmt.msia_max[lsiv_member] = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;
                    // print data to a file
                    Intersection.filesPrintSIM_IntToFile(msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase], lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, lsiv_member, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    lclv_tx_fmt.msia_max[lsiv_member] = lsiv_max;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                } // end for ( lsiv_phase = 1 ; lsiv_phase <=
                  // msia_ring_group_num_ph[lsiv_ring][lsiv_group] ; lsiv_phase++ )
            } // end for ( lsiv_group = 1 ; lsiv_group <= msiv_num_groups ; lsiv_group++ )
        } // end for ( lsiv_ring = 1 ; lsiv_ring <= msiv_num_rings ; lsiv_ring++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsia_group_num_phases[];
        int lsiv_group;
        int lsiv_max;
        int lsiv_member;
        int lsiv_phase;
        int lsiv_ring;
        String lstv_name;

        // readFromCards reads all NEMA_ring_group fields from cards

        lsia_group_num_phases = new int[PARAMS.TEXAS_MODEL_NGR + 1];

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("NEMA_ring_group.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 0)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 0 or > " + PARAMS.TEXAS_MODEL_NON + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_RING_GRP];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card + 2;
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mdfv_sim_ver >= 6.00)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0)) {
            lsiv_card++;
        }
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("NEMA_ring_group.readFromCards reading " + lstv_name);

        // initialize number phases in group
        for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
            lsia_group_num_phases[lsiv_group] = 0;
        }

        // read data from cards
        msiv_num_rings = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_num_groups = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1];
        for (lsiv_ring = 1; lsiv_ring <= msiv_num_rings; lsiv_ring++) {
            for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
                lsiv_member = Intersection.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR + lsiv_group;
                lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
                // read data from cards
                msia_ring_group_num_ph[lsiv_ring][lsiv_group] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, lsiv_member, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];

                // sum number phases in group
                lsia_group_num_phases[lsiv_group] += msia_ring_group_num_ph[lsiv_ring][lsiv_group];

                for (lsiv_phase = 1; lsiv_phase <= msia_ring_group_num_ph[lsiv_ring][lsiv_group]; lsiv_phase++) {
                    lsiv_member = Intersection.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR * PARAMS.TEXAS_MODEL_NRGP + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                            + lsiv_phase;
                    lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
                    lsiv_max = lclv_tx_fmt.msia_max[lsiv_member];
                    lclv_tx_fmt.msia_max[lsiv_member] = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;
                    // read data from cards
                    msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, lsiv_member,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    lclv_tx_fmt.msia_max[lsiv_member] = lsiv_max;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                    lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];
                } // end for ( lsiv_phase = 1 ; lsiv_phase <=
                  // msia_ring_group_num_ph[lsiv_ring][lsiv_group] ; lsiv_phase++ )
            } // end for ( lsiv_group = 1 ; lsiv_group <= msiv_num_groups ; lsiv_group++ )
        } // end for ( lsiv_ring = 1 ; lsiv_ring <= msiv_num_rings ; lsiv_ring++ )

        // check number phases in group
        for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
            if (lsia_group_num_phases[lsiv_group] == 0) {
                Intersection.mstv_warningMessage = "Warning in NEMA_ring_group.readFromCards: Group " + lsiv_group
                        + " total number of phases = 0 - at least one Ring in a Group must have at least one Phase.";
                Intersection.warningMessage();
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all NEMA_ring_group fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("NEMA_ring_group.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsia_group_num_phases[];
        int lsiv_group;
        int lsiv_max;
        int lsiv_member;
        int lsiv_phase;
        int lsiv_ring;
        String lstv_name;

        // writeToCards writes all NEMA_ring_group fields to cards

        lsia_group_num_phases = new int[PARAMS.TEXAS_MODEL_NGR + 1];

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("NEMA_ring_group.writeToCards");
        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if NEMA ring group card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_RING_GRP_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ring_group.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ring_grp_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_RING_GRP];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ring_grp_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("NEMA_ring_group.writeToCards writing " + lstv_name);

        // initialize number phases in group
        for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
            lsia_group_num_phases[lsiv_group] = 0;
        }

        // sum number phases in group
        for (lsiv_ring = 1; lsiv_ring <= msiv_num_rings; lsiv_ring++) {
            for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
                lsia_group_num_phases[lsiv_group] += msia_ring_group_num_ph[lsiv_ring][lsiv_group];
            }
        }

        // check number phases in group
        for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
            if (lsia_group_num_phases[lsiv_group] == 0) {
                Intersection.mstv_warningMessage = "Warning in NEMA_ring_group.writeToCards: Group " + lsiv_group
                        + " total number of phases = 0 - at least one Ring in a Group must have at least one Phase.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }
        }

        // write data to cards
        Intersection.writeIntToCard(msiv_num_rings, lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_num_groups, lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, Intersection.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1];
        for (lsiv_ring = 1; lsiv_ring <= msiv_num_rings; lsiv_ring++) {
            for (lsiv_group = 1; lsiv_group <= msiv_num_groups; lsiv_group++) {
                lsiv_member = Intersection.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR + lsiv_group;
                lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
                // write data to cards
                Intersection.writeIntToCard(msia_ring_group_num_ph[lsiv_ring][lsiv_group], lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, lsiv_member, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];

                for (lsiv_phase = 1; lsiv_phase <= msia_ring_group_num_ph[lsiv_ring][lsiv_group]; lsiv_phase++) {
                    lsiv_member = Intersection.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NGR * PARAMS.TEXAS_MODEL_NRGP + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                            + lsiv_phase;
                    lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
                    lsiv_max = lclv_tx_fmt.msia_max[lsiv_member];
                    lclv_tx_fmt.msia_max[lsiv_member] = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;
                    // write data to cards
                    Intersection.writeIntToCard(msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase], lstv_name, Intersection.TX_FMT_SIM_NEMA_RING_GRP, lsiv_member,
                            Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                    lclv_tx_fmt.msia_max[lsiv_member] = lsiv_max;
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                    lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];
                } // end for ( lsiv_phase = 1 ; lsiv_phase <=
                  // msia_ring_group_num_ph[lsiv_ring][lsiv_group] ; lsiv_phase++ )
            } // end for ( lsiv_group = 1 ; lsiv_group <= msiv_num_groups ; lsiv_group++ )
        } // end for ( lsiv_ring = 1 ; lsiv_ring <= msiv_num_rings ; lsiv_ring++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class NEMA_ring_group

/******************************************************************************/
/* NEMA_ring_group.java */
/******************************************************************************/
