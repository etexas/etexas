package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Traf_Mix.java                                */
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

class Traf_Mix /* vehicle class mix data */
{

    double mdfa_per_veh_class[]; /*
                                  * mix (percentages) of vehicle classes in inbound traffic
                                  */

    /* [1] is for class 1 */
    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_TRAF_MIX]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_02]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_03]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_04]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_05]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_06]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_07]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_08]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_09]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_10]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_11]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_12]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_13]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_14]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_15]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_16]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_17]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_18]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_19]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_20]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_21]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_22]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_23]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_24]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_25]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_26]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_27]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_28]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_29]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_30]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_31]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_32]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_33]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_34]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_35]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_36]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_37]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_38]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_39]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_40]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_41]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_42]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_43]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_44]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_45]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_46]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_47]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_48]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_49]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_50]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_51]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_52]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_53]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_54]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_55]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_56]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_57]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_58]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_59]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_60]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_61]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_62]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_63]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_64]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_65]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_66]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_67]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_68]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_69]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_70]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_71]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_72]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_73]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_74]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_75]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_76]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_77]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_78]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_79]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_80]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_81]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_82]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_83]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_84]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_85]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_86]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_87]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_88]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_89]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_90]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_91]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_92]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_93]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_94]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_95]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_96]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_97]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_98]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_99]

    public Traf_Mix() {
        mdfa_per_veh_class = new double[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method Traf_Mix

    public void checkForErrors(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Traf_Mix data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Traf_Mix.checkForErrors psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if traffic mix is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_TM] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.checkForErrors: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_hdway.mstv_tm is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // return if traffic mix not specified
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_tm.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_MIX];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Traf_Mix.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Traf_Mix.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for leg " + psiv_leg + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Traf_Mix.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for leg " + psiv_leg + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_per_veh_class;
        int lsiv_veh;
        String lstv_name;

        // printToFile prints all Traf_Mix data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Mix.printToFile psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.printToFile: psiv_leg = " + psiv_leg + " is < 1 or > " + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
                    + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if traffic mix is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_TM] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_hdway.mstv_tm is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // return if traffic mix not specified
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_tm.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_MIX];
        ldfv_sum_per_veh_class = 0.0;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Mix.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            Intersection.filesPrintGDV_DoubleToFile(mdfa_per_veh_class[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_TRAF_MIX, Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh,
                    mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            ldfv_sum_per_veh_class += mdfa_per_veh_class[lsiv_veh];
        }

        // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 then
        // warning
        // special code that must also be performed by the menu system
        if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) {
            Intersection.mstv_warningMessage = "Warning in Traf_Mix.printToFile: Sum of percent of inbound traffic in vehicle classes on Leg " + psiv_leg + " = " + ldfv_sum_per_veh_class
                    + " is not 100.0.";
            Intersection.warningMessage();
            /* debug */lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_MIX];
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DATA_ERROR;
                /* debug */if (Intersection.mbov_debug_printGDV)
                    System.out.println("Traf_Mix.printToFile  " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh]
                            + "[" + psiv_leg + "]" + "=" + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_veh] + "  "
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_mix.mclv_aux.status(Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh) + " TX_DATA_ERROR");
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg) {
        boolean lbov_force_default;
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_per_veh_class;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // readFromCards reads all Traf_Mix fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Mix.readFromCards psiv_leg=" + psiv_leg);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if traffic mix is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_TM] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_hdway.mstv_tm is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Traf_Mix.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 2);

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_MIX];
        ldfv_sum_per_veh_class = 0.0;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        lbov_force_default = true;
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_tm.equals("YES")) {
            lbov_force_default = false;
        }
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Mix.readFromCards reading " + lstv_name);

        // read data from cards
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 16) && ((((lsiv_veh - 1) / 15) * 15) == (lsiv_veh - 1)))
                lsiv_card++;
            mdfa_per_veh_class[lsiv_veh] = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_MIX, Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            ldfv_sum_per_veh_class += mdfa_per_veh_class[lsiv_veh];
        }

        // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 and
        // percents are not specified then set defaults
        // special code that must also be performed by the menu system
        if ((!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) && (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_tm.equals("NO"))) {
            ldfv_sum_per_veh_class = 0.0;
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                mdfa_per_veh_class[lsiv_veh] = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh];
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DEFAULT;
                ldfv_sum_per_veh_class += mdfa_per_veh_class[lsiv_veh];
            }
        }

        // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 then
        // warning
        // special code that must also be performed by the menu system
        if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) {
            Intersection.mstv_warningMessage = "Warning in Traf_Mix.readFromCards: Sum of percent of inbound traffic in vehicle classes on Leg " + psiv_leg + " = " + ldfv_sum_per_veh_class
                    + " is not 100.0.";
            Intersection.warningMessage();
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DATA_ERROR;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("Traf_Mix.readFromCards "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] + "[" + psiv_leg + "]" + "="
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_veh] + "  "
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_mix.mclv_aux.status(Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh) + " TX_DATA_ERROR");
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Traf_Mix fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Traf_Mix.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_sum_per_veh_class;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all Traf_Mix fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Mix.writeToCards psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if traffic mix is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_HDWAY_TM] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_hdway.mstv_tm is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Traf_Mix.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 2);

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_MIX];
        ldfv_sum_per_veh_class = 0.0;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Mix.writeToCards writing " + lstv_name);

        // write data to cards
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 16) && ((((lsiv_veh - 1) / 15) * 15) == (lsiv_veh - 1)))
                lsiv_card++;
            Intersection.writeDoubleToCard(mdfa_per_veh_class[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_TRAF_MIX, Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            ldfv_sum_per_veh_class += mdfa_per_veh_class[lsiv_veh];
        }

        // if sum of percent of inbound traffic in vehicle classes in this leg is not 100 then
        // warning
        // special code that must also be performed by the menu system
        if (!Intersection.close(Intersection.GDVSIM_CLOSE_100, ldfv_sum_per_veh_class, 100.0)) {
            Intersection.mstv_warningMessage = "Warning in Traf_Mix.writeToCards: Sum of percent of inbound traffic in vehicle classes on Leg " + psiv_leg + " = " + ldfv_sum_per_veh_class
                    + " is not 100.0.";
            Intersection.warningMessage();
            /* debug */lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_MIX];
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] = Intersection.TX_DATA_ERROR;
                /* debug */if (Intersection.mbov_debug_filesWriteGDV)
                    System.out.println("Traf_Mix.writeToCards  "
                            + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh] + "[" + psiv_leg + "]" + "="
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_veh] + "  "
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_mix.mclv_aux.status(Intersection.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_veh) + " TX_DATA_ERROR");
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Traf_Mix

/******************************************************************************/
/* Traf_Mix.java */
/******************************************************************************/
