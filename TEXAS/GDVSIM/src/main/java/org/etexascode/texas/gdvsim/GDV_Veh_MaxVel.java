package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_Veh_MaxVel.java                             */
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

class GDV_Veh_MaxVel {

    int msia_veh_max_vel[]; /*
                             * User-defined vehicle max velocity for each vehicle class
                             */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_MAX_VEL]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MAX_VEL_99]

    public GDV_Veh_MaxVel() {
        msia_veh_max_vel = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_MaxVel

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_MaxVel data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MaxVel.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_VEL];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MaxVel.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_MaxVel.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_MaxVel data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MaxVel.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_VEL];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MaxVel.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_max_vel[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_VEL, Intersection.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_MaxVel fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MaxVel.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_ACCEL for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_VEL];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 4
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MaxVel.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_max_vel[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_VEL, Intersection.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_MaxVel fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_MaxVel.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_MaxVel fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MaxVel.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MaxVel.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
        // 1 card for TX_FMT_GDV_VEH_MAX_ACCEL for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MAX_VEL];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 4
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MaxVel.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_max_vel[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MAX_VEL, Intersection.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_MaxVel

/******************************************************************************/
/* GDV_Veh_MaxVel.java */
/******************************************************************************/
