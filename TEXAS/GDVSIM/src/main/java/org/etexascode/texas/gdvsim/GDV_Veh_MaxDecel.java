package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           GDV_Veh_MaxDecel.java                            */
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

class GDV_Veh_MaxDecel {

    int msia_veh_max_decel[]; /*
                               * User-defined vehicle maximum deceleration for each vehicle class
                               */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_MAX_DECEL]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_DECEL_99]

    public GDV_Veh_MaxDecel() {
        msia_veh_max_decel = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_MaxDecel

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_MaxDecel data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MaxDecel.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_DECEL];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MaxDecel.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_MaxDecel.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_MaxDecel data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MaxDecel.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_DECEL];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MaxDecel.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_max_decel[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_DECEL, Intersection.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_MaxDecel fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MaxDecel.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_DECEL];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 2
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MaxDecel.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_max_decel[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_DECEL, Intersection.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_MaxDecel fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_MaxDecel.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_MaxDecel fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MaxDecel.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxDecel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_DECEL];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 2
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MaxDecel.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_max_decel[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_DECEL, Intersection.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_MaxDecel

/******************************************************************************/
/* GDV_Veh_MaxDecel.java */
/******************************************************************************/
