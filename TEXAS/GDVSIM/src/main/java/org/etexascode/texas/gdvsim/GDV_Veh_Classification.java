package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                        GDV_Veh_Classification.java                         */
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

class GDV_Veh_Classification {

    String msta_veh_classification[]; /*
                                       * User-defined vehicle classification for each vehicle class
                                       */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_CLASSIFY]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_CLASSIFY_99]

    public GDV_Veh_Classification() {
        msta_veh_classification = new String[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            msta_veh_classification[lsiv_i] = new String();
        }
    } // end of method GDV_Veh_Classification

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_Classification data for invalid data and data with
        // errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Classification.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_CLASSIFY];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Classification.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_Classification.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_Classification data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Classification.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_CLASSIFY];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Classification.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_StringToFile(msta_veh_classification[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_CLASSIFY, Intersection.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh,
                    mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
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

        // readFromCards reads all GDV_Veh_Classification fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Classification.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle attributes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes is invalid.";
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
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_CLASSIFY];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 6
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lbov_forceDefault = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"))
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes < 10));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Classification.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 11) && ((((lsiv_veh - 1) / 10) * 10) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msta_veh_classification[lsiv_veh] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_CLASSIFY, Intersection.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_Classification fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_Classification.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_Classification fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Classification.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Classification.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_CLASSIFY];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 6
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Classification.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 11) && ((((lsiv_veh - 1) / 10) * 10) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeStringToCard(msta_veh_classification[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_CLASSIFY, Intersection.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_Classification

/******************************************************************************/
/* GDV_Veh_Classification.java */
/******************************************************************************/
