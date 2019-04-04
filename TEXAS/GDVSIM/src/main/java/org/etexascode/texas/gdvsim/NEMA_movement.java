package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             NEMA_movement.java                             */
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

class NEMA_movement {

    int msiv_movement_defmov; /* NEMA/HARDWARE movement defmov number */

    int msia_movement_num[]; /*
                              * NEMA/HARDWARE number of movements per controller phase [phase]
                              */

    int msia_movement_list[][]; /*
                                 * NEMA/HARDWARE list of movements per controller phase
                                 * [phase][item]
                                 */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_NEMA_MOVEMENT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msiv_movement_defmov
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH02]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH03]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH04]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH05]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH06]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH07]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH08]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num [
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH09]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [10] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH10]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [11] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH11]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [12] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH12]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [13] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH13]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [14] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH14]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [15] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH15]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num
    // [16] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH16]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 1][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 2][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH02_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 3][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH03_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 4][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH04_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 5][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH05_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 6][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH06_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 7][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH07_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 8][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH08_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list [
    // 9][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH09_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [10][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH10_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [11][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH11_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [12][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH12_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [13][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH13_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [14][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH14_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [15][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH15_MV8]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list
    // [16][8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_MOVEMENT_PH16_MV8]

    // NEMA/HARDWARE movement 1 is Eastbound Left Turn
    // NEMA/HARDWARE movement 2 is Westbound Straight
    // NEMA/HARDWARE movement 3 is Northbound Left Turn
    // NEMA/HARDWARE movement 4 is Southbound Straight
    // NEMA/HARDWARE movement 5 is Westbound Left Turn
    // NEMA/HARDWARE movement 6 is Eastbound Straught
    // NEMA/HARDWARE movement 7 is Southbound Left Turn
    // NEMA/HARDWARE movement 8 is Northbound Straight

    // Leg 1
    //
    // 4 7
    // | \
    // -2 Leg
    // Leg 1/ /5 2
    // 4 6-
    // \ |
    // 3 8
    //
    // Leg 3

    public NEMA_movement() {
        msia_movement_num = new int[PARAMS.TEXAS_MODEL_NPN + 1];
        msia_movement_list = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NMP + 1];
        mclv_aux = new TX_Aux();
    } // end of method NEMA_movement

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_mv;
        int lsiv_ph;
        String lstv_name;

        // checkForErrors checks all NEMA_movement data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("NEMA_movement.checkForErrors");
        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_movement.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_MOVEMENT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("NEMA_movement.checkForErrors checking " + lstv_name);

        // check all data for errors
        lsiv_field = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV;
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in NEMA_movement.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        for (lsiv_ph = 1; lsiv_ph <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_ph++) {
            lsiv_field = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_ph;
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in NEMA_movement.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in NEMA_movement.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            for (lsiv_mv = 1; lsiv_mv <= msia_movement_num[lsiv_ph]; lsiv_mv++) {
                lsiv_field = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + PARAMS.TEXAS_MODEL_NMP * (lsiv_ph - 1) + lsiv_mv;
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in NEMA_movement.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in NEMA_movement.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            } // end for ( lsiv_mv = 1 ; lsiv_mv <= msia_movement_num[lsiv_ph] ; lsiv_mv++ )
        } // end for ( lsiv_ph = 1 ; lsiv_ph <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_ph++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lines;
        int lsiv_member;
        int lsiv_mv;
        int lsiv_ph;
        String lstv_name;

        // printToFile prints all NEMA_movement data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("NEMA_movement.printToFile");
        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_movement.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_MOVEMENT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("NEMA_movement.printToFile printing " + lstv_name);

        lsiv_lines = 1;
        for (lsiv_ph = 1; lsiv_ph <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_ph++) {
            lsiv_lines += (1 + msia_movement_num[lsiv_ph]);
        } // end for ( lsiv_ph = 1 ; lsiv_ph <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_ph++ )

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintSIM_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_movement_defmov, lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, Intersection.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_ph = 1; lsiv_ph <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_ph++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_ph;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msia_movement_num[lsiv_ph], lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, lsiv_member, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            for (lsiv_mv = 1; lsiv_mv <= msia_movement_num[lsiv_ph]; lsiv_mv++) {
                lsiv_member = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + PARAMS.TEXAS_MODEL_NMP * (lsiv_ph - 1) + lsiv_mv;
                // print data to a file
                Intersection.filesPrintSIM_IntToFile(msia_movement_list[lsiv_ph][lsiv_mv], lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, lsiv_member, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
            } // end for ( lsiv_mv = 1 ; lsiv_mv <= msia_movement_num[lsiv_ph] ; lsiv_mv++ )
        } // end for ( lsiv_ph = 1 ; lsiv_ph <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_ph++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_member;
        int lsiv_mv;
        int lsiv_ph;
        String lstv_name;

        // readFromCards reads all NEMA_movement fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("NEMA_movement.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_movement.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_MOVEMENT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("NEMA_movement.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_movement_defmov = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, Intersection.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01];
        for (lsiv_ph = 1; lsiv_ph <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_ph++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_ph;
            lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
            // read data from cards
            msia_movement_num[lsiv_ph] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, lsiv_member, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];

            for (lsiv_mv = 1; lsiv_mv <= msia_movement_num[lsiv_ph]; lsiv_mv++) {
                lsiv_member = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + PARAMS.TEXAS_MODEL_NMP * (lsiv_ph - 1) + lsiv_mv;
                lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
                // read data from cards
                msia_movement_list[lsiv_ph][lsiv_mv] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, lsiv_member, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];
            } // end for ( lsiv_mv = 1 ; lsiv_mv <= msia_movement_num[lsiv_ph] ; lsiv_mv++ )
        } // end for ( lsiv_ph = 1 ; lsiv_ph <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_ph++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all NEMA_movement fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("NEMA_movement.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_member;
        int lsiv_mv;
        int lsiv_ph;
        String lstv_name;

        // writeToCards writes all NEMA_movement fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("NEMA_movement.writeToCards");
        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_movement.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_movement.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_MOVEMENT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("NEMA_movement.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_movement_defmov, lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, Intersection.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01];
        for (lsiv_ph = 1; lsiv_ph <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_ph++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_ph;
            lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
            // write data to cards
            Intersection.writeIntToCard(msia_movement_num[lsiv_ph], lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, lsiv_member, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];

            for (lsiv_mv = 1; lsiv_mv <= msia_movement_num[lsiv_ph]; lsiv_mv++) {
                lsiv_member = Intersection.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + PARAMS.TEXAS_MODEL_NMP * (lsiv_ph - 1) + lsiv_mv;
                lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
                // write data to cards
                Intersection.writeIntToCard(msia_movement_list[lsiv_ph][lsiv_mv], lstv_name, Intersection.TX_FMT_SIM_NEMA_MOVEMENT, lsiv_member, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
                lsiv_col += lclv_tx_fmt.msia_fs[lsiv_member];
            } // end for ( lsiv_mv = 1 ; lsiv_mv <= msia_movement_num[lsiv_ph] ; lsiv_mv++ )
        } // end for ( lsiv_ph = 1 ; lsiv_ph <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_ph++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class NEMA_movement

/******************************************************************************/
/* NEMA_movement.java */
/******************************************************************************/
