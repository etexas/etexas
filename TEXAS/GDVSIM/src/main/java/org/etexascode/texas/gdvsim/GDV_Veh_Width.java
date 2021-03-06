package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_Veh_Width.java                             */
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

class GDV_Veh_Width {

    int msia_veh_width[]; /* User-defined vehicle width for each vehicle class */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_WIDTH]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_WIDTH_99]

    public GDV_Veh_Width() {
        msia_veh_width = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_Width

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_Width data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Width.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_WIDTH];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Width.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_Width.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_Width data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Width.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_WIDTH];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Width.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_width[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_WIDTH, Intersection.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_Width fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Width.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle attributes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes is invalid.";
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
        // 1 card for TX_FMT_GDV_VEH_HEIGHT for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_WIDTH];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 7
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
        lbov_forceDefault = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"))
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes < 10));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Width.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_width[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_WIDTH, Intersection.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_Width fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_Width.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_Width fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Width.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Width.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
        // 1 card for TX_FMT_GDV_VEH_HEIGHT for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_WIDTH];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 7
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Width.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_width[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_WIDTH, Intersection.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_Width

/******************************************************************************/
/* GDV_Veh_Width.java */
/******************************************************************************/
