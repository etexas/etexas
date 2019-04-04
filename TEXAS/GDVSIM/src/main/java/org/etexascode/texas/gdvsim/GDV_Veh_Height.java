package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_Veh_Height.java                             */
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

class GDV_Veh_Height {

    int msia_veh_height[]; /*
                            * User-defined vehicle height for each vehicle class
                            */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_HEIGHT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_HEIGHT_99]

    public GDV_Veh_Height() {
        msia_veh_height = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_Height

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_Height data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Height.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_HEIGHT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Height.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_Height.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_Height data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Height.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_HEIGHT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Height.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_height[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_HEIGHT, Intersection.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_Height fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Height.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle attributes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes is invalid.";
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
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_HEIGHT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 6
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
        lbov_forceDefault = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"))
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes < 10));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Height.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_height[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_HEIGHT, Intersection.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_Height fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_Height.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_Height fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Height.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Height.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_HEIGHT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 6
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Height.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_height[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_HEIGHT, Intersection.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_Height

/******************************************************************************/
/* GDV_Veh_Height.java */
/******************************************************************************/
