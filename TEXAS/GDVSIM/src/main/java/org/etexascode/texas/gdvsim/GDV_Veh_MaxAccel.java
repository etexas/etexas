package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           GDV_Veh_MaxAccel.java                            */
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

class GDV_Veh_MaxAccel {

    int msia_veh_max_accel[]; /*
                               * User-defined vehicle maximum acceleration for each vehicle class
                               */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_MAX_ACCEL]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_ACCEL_99]

    public GDV_Veh_MaxAccel() {
        msia_veh_max_accel = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_MaxAccel

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_MaxAccel data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MaxAccel.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_ACCEL];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MaxAccel.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_MaxAccel.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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
        int lsiv_veh;
        String lstv_name;

        // printToFile prints all GDV_Veh_MaxAccel data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MaxAccel.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_ACCEL];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MaxAccel.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_veh_max_accel[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_ACCEL, Intersection.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        boolean lbov_forceDefault;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // readFromCards reads all GDV_Veh_MaxAccel fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MaxAccel.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_ACCEL];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 3
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MaxAccel.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_max_accel[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_ACCEL, Intersection.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_MaxAccel fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_MaxAccel.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_MaxAccel fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MaxAccel.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxAccel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_ACCEL];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 3
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MaxAccel.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_max_accel[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_ACCEL, Intersection.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_MaxAccel

/******************************************************************************/
/* GDV_Veh_MaxAccel.java */
/******************************************************************************/
