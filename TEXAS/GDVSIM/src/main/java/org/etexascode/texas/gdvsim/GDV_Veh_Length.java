package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_Veh_Length.java                             */
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

class GDV_Veh_Length {

    int msia_veh_length[]; /*
                            * User-defined vehicle length for each vehicle class
                            */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_LENGTH]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_LENGTH_99]

    public GDV_Veh_Length() {
        msia_veh_length = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_Length

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_Length data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Length.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_LENGTH];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_Length.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_Length.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_Length data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Length.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_LENGTH];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_Length.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_length[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_LENGTH, Intersection.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_Length fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Length.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_LENGTH];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card;
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_Length.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_length[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_LENGTH, Intersection.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_Length fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_Length.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_Length fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Length.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_Length.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_LENGTH];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_Length.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_length[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_LENGTH, Intersection.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_Length

/******************************************************************************/
/* GDV_Veh_Length.java */
/******************************************************************************/
