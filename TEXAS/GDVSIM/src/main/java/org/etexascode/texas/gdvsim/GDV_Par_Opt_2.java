package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_Par_Opt_2.java                             */
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

class GDV_Par_Opt_2 {

    String mstv_driver_mix; /* User-defined driver mix (YES or NO) */

    String mstv_vehicle_data; /* User-defined vehicle data (YES or NO) */

    String mstv_driver_data; /* User-defined driver data (YES or NO) */

    String msta_sum_veh_cl[]; /*
                               * Logout summary for driver-vehicle unit by vehicle class (YES or NO)
                               */

    String msta_sum_drv_cl[]; /*
                               * Logout summary for driver-vehicle unit by driver class (YES or NO)
                               */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_PAR_OPT_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_02]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_03]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_04]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_05]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_06]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_07]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_08]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_09]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[10]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_10]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[11]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_11]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[12]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_12]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[13]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_13]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[14]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_14]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[15]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_15]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[16]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_16]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[17]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_17]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[18]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_18]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[19]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_19]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[20]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_20]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[21]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_21]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[22]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_22]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[23]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_23]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[24]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_24]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[25]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_25]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[26]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_26]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[27]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_27]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[28]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_28]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[29]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_29]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[30]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_30]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[31]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_31]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[32]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_32]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[33]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_33]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[34]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_34]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[35]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_35]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[36]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_36]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[37]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_37]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[38]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_38]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[39]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_39]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[40]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_40]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[41]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_41]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[42]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_42]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[43]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_43]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[44]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_44]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[45]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_45]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[46]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_46]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[47]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_47]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[48]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_48]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[49]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_49]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[50]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_50]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[51]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_51]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[52]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_52]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[53]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_53]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[54]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_54]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[55]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_55]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[56]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_56]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[57]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_57]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[58]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_58]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[59]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_59]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[60]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_60]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[61]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_61]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[62]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_62]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[63]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_63]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[64]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_64]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[65]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_65]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[66]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_66]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[67]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_67]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[68]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_68]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[69]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_69]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[70]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_70]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[71]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_71]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[72]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_72]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[73]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_73]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[74]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_74]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[75]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_75]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[76]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_76]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[77]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_77]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[78]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_78]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[79]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_79]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[80]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_80]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[81]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_81]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[82]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_82]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[83]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_83]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[84]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_84]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[85]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_85]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[86]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_86]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[87]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_87]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[88]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_88]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[89]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_89]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[90]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_90]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[91]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_91]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[92]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_92]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[93]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_93]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[94]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_94]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[95]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_95]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[96]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_96]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[97]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_97]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[98]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_98]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[99]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_99]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 7]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_7 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 8]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_8 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[ 9]
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_9 ]

    public GDV_Par_Opt_2() {
        msta_sum_veh_cl = new String[PARAMS.TEXAS_MODEL_NVC + 1];
        msta_sum_drv_cl = new String[PARAMS.TEXAS_MODEL_NDC + 1];
        mclv_aux = new TX_Aux();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            msta_sum_veh_cl[lsiv_i] = new String();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NDC + 1); lsiv_i++) {
            msta_sum_drv_cl[lsiv_i] = new String();
        }
    } // end of method GDV_Par_Opt_2

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Par_Opt_2 data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Par_Opt_2.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT_2];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Par_Opt_2.checkForErrors checking " + lstv_name);

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check vehicle classes beyond msiv_no_veh_cl through PARAMS.TEXAS_MODEL_NVC
            if ((lsiv_field > (Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl))
                    && (lsiv_field <= (Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + PARAMS.TEXAS_MODEL_NVC))) {
                continue;
            }

            // do not check driver classes beyond msiv_no_drv_cl through PARAMS.TEXAS_MODEL_NDC
            if ((lsiv_field > (Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl))
                    && (lsiv_field <= (Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + PARAMS.TEXAS_MODEL_NDC))) {
                continue;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Par_Opt_2.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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
        int lsiv_drv;
        int lsiv_veh;
        String lstv_name;

        // printToFile prints all GDV_Par_Opt_2 data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Par_Opt_2.printToFile");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT_2];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Par_Opt_2.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(3 + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_driver_mix, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_vehicle_data, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_driver_data, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            Intersection.filesPrintGDV_StringToFile(msta_sum_veh_cl[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
            Intersection.filesPrintGDV_StringToFile(msta_sum_drv_cl[lsiv_drv], lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        boolean lbov_one_card;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_drv;
        int lsiv_fo;
        int lsiv_fo_saved;
        int lsiv_veh;
        String lstv_name;

        // readFromCards reads all GDV_Par_Opt_2 fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Par_Opt_2.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if par opts 2 card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PAR_OPT_2_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lbov_one_card = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl <= 15)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl <= 5));
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT_2];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Par_Opt_2.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_driver_mix = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_vehicle_data = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_driver_data = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (!lbov_one_card)
            lsiv_card++;
        lsiv_fo = 1;
        // read data from cards
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            lsiv_fo_saved = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh];
            if (lsiv_veh >= 27) {
                if ((((lsiv_veh - 1) / 26) * 26) == (lsiv_veh - 1)) {
                    lsiv_card++;
                    lsiv_fo = 1;
                }
            }
            if (lbov_one_card)
                lsiv_fo = lsiv_fo_saved;
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh] = lsiv_fo;
            msta_sum_veh_cl[lsiv_veh] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                    Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_fo += lclv_tx_fmt.msia_fs[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh] = lsiv_fo_saved;
        }

        if (!lbov_one_card)
            lsiv_card++;
        lsiv_fo = 1;
        // read data from cards
        for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
            lsiv_fo_saved = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv];
            if (lbov_one_card)
                lsiv_fo = lsiv_fo_saved;
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv] = lsiv_fo;
            msta_sum_drv_cl[lsiv_drv] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                    Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_fo += lclv_tx_fmt.msia_fs[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv] = lsiv_fo_saved;
        }

        // set driver mix card to par opt 2 card + 1 or more
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card + 1;
        if (!lbov_one_card) {
            Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card += (((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 25) / 26)
                    + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl + 25) / 26));
        }
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_DRIVER_MIX_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Par_Opt_2.readFromCards msiv_driver_mix_card ="
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card + "  "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.status(Intersection.TX_FMT_GDV_HEADER_DRIVER_MIX_CARD));

        // set vehicle data card to driver mix card + number of vehicle classes (if driver mix
        // specified)
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card;
        if (mstv_driver_mix.equals("YES")) {
            Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card += Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
        }
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Par_Opt_2.readFromCards msiv_veh_data_card ="
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card + "  "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.status(Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD));

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Par_Opt_2 fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Par_Opt_2.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        boolean lbov_one_card;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_drv;
        int lsiv_fo;
        int lsiv_fo_saved;
        int lsiv_veh;
        String lstv_name;

        // writeToCards writes all GDV_Par_Opt_2 fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Par_Opt_2.writeToCards");
        // check if par opts 2 card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PAR_OPT_2_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of driver classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt_2.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lbov_one_card = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl <= 15)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl <= 5));
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT_2];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Par_Opt_2.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeStringToCard(mstv_driver_mix, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_vehicle_data, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_driver_data, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (!lbov_one_card)
            lsiv_card++;
        lsiv_fo = 1;
        // write data to cards
        for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
            lsiv_fo_saved = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh];
            if (lsiv_veh >= 27) {
                if ((((lsiv_veh - 1) / 26) * 26) == (lsiv_veh - 1)) {
                    lsiv_card++;
                    lsiv_fo = 1;
                }
            }
            if (lbov_one_card)
                lsiv_fo = lsiv_fo_saved;
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh] = lsiv_fo;
            Intersection.writeStringToCard(msta_sum_veh_cl[lsiv_veh], lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_fo += lclv_tx_fmt.msia_fs[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1 + lsiv_veh] = lsiv_fo_saved;
        }

        if (!lbov_one_card)
            lsiv_card++;
        lsiv_fo = 1;
        // write data to cards
        for (lsiv_drv = 1; lsiv_drv <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl; lsiv_drv++) {
            lsiv_fo_saved = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv];
            if (lbov_one_card)
                lsiv_fo = lsiv_fo_saved;
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv] = lsiv_fo;
            Intersection.writeStringToCard(msta_sum_drv_cl[lsiv_drv], lstv_name, Intersection.TX_FMT_GDV_PAR_OPT_2, Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_fo += lclv_tx_fmt.msia_fs[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv];
            lclv_tx_fmt.msia_fo[Intersection.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv] = lsiv_fo_saved;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Par_Opt_2

/******************************************************************************/
/* GDV_Par_Opt_2.java */
/******************************************************************************/
