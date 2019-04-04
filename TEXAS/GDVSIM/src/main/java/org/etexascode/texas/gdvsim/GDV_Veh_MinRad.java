package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_Veh_MinRad.java                             */
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

class GDV_Veh_MinRad {

    int msia_veh_min_rad[]; /*
                             * User-defined vehicle minimum inside turning radius for each vehicle
                             * class
                             */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_VEH_MIN_RAD]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_VEH_MIN_RAD_99]

    public GDV_Veh_MinRad() {
        msia_veh_min_rad = new int[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Veh_MinRad

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Veh_MinRad data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MinRad.checkForErrors");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MIN_RAD];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Veh_MinRad.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Veh_MinRad.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Veh_MinRad data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MinRad.printToFile");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MIN_RAD];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Veh_MinRad.printToFile printing " + lstv_name);

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
            Intersection.filesPrintGDV_IntToFile(msia_veh_min_rad[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MIN_RAD, Intersection.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh, mclv_aux,
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

        // readFromCards reads all GDV_Veh_MinRad fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MinRad.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MIN_RAD];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 5
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lbov_forceDefault = (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("NO"));
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Veh_MinRad.readFromCards reading " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // read data from cards
            msia_veh_min_rad[lsiv_veh] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_VEH_MIN_RAD, Intersection.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Veh_MinRad fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Veh_MinRad.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Veh_MinRad fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MinRad.writeToCards");
        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
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
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Veh_MinRad.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
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
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_VEH_MIN_RAD];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + 5
                * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Veh_MinRad.writeToCards writing " + lstv_name);

        // process all vehicles
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            if ((lsiv_veh >= 21) && ((((lsiv_veh - 1) / 20) * 20) == (lsiv_veh - 1)))
                lsiv_card++;
            // write data to cards
            Intersection.writeIntToCard(msia_veh_min_rad[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_VEH_MIN_RAD, Intersection.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Veh_MinRad

/******************************************************************************/
/* GDV_Veh_MinRad.java */
/******************************************************************************/
