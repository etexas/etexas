package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           GDV_Veh_OperChar.java                            */
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

class GDV_Veh_OperChar {

    int msia_veh_oper_char[]; /*
                               * User-defined vehicle operatinal factor for each vehicle class
                               */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_OPER_CHAR]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_OPER_CHAR_99]

    public GDV_Veh_OperChar() {
        msia_veh_oper_char = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_OperChar

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_OperChar data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_OperChar.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_OPER_CHAR];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_OperChar.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_OperChar.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_OperChar data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_OperChar.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_OPER_CHAR];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_OperChar.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_oper_char[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_OPER_CHAR, Intersection.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_OperChar fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_OperChar.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_OPER_CHAR];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_OperChar.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            msia_veh_oper_char[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_OPER_CHAR, Intersection.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_OperChar fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_OperChar.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_OperChar fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_OperChar.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_OperChar.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_OPER_CHAR];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_OperChar.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_oper_char[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_OPER_CHAR, Intersection.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_OperChar

/******************************************************************************/
/* GDV_Veh_OperChar.java */
/******************************************************************************/
