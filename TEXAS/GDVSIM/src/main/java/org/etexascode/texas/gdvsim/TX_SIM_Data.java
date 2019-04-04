package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              TX_SIM_Data.java                              */
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

class TX_SIM_Data {

    SIM_Hdr mclv_sim_header; /* SIM header */

    SIM_Ver mclv_sim_version; /* SIM version */

    TX_Title mclv_sim_title; /* title text */

    SIM_Par_Opt mclv_sim_par_opt; /* parameter-option data */

    SIM_Par_Opt_2 mclv_sim_par_opt_2; /* parameter-option data 2 */

    TX_Sig_Setup mclv_tx_sig_setup; /* signal controller setup data */

    TX_Dets mclv_det; /* vehicle detector data */

    VMS_Message mcla_vms_message[]; /* VMS Message data */

    GDV_Ref_File mclv_gdv_ref_file; /* GDV reference file data */

    GDV_Ref_Leg mclv_gdv_ref_leg; /* GDV reference leg data */

    GDV_Ref_Lane mcla_gdv_ref_lane[]; /* GDV reference lane data */

    GDV_Ref_Offs mcla_gdv_ref_offs[]; /* GDV reference offset data */

    public TX_SIM_Data() {
        mclv_sim_header = new SIM_Hdr();
        mclv_sim_version = new SIM_Ver();
        mclv_sim_title = new TX_Title();
        mclv_sim_par_opt = new SIM_Par_Opt();
        mclv_sim_par_opt_2 = new SIM_Par_Opt_2();
        mclv_tx_sig_setup = new TX_Sig_Setup();
        mclv_det = new TX_Dets();
        mcla_vms_message = new VMS_Message[PARAMS.TEXAS_MODEL_NVMSMM + 1];
        mclv_gdv_ref_file = new GDV_Ref_File();
        mclv_gdv_ref_leg = new GDV_Ref_Leg();
        mcla_gdv_ref_lane = new GDV_Ref_Lane[PARAMS.TEXAS_MODEL_NLGP2];
        mcla_gdv_ref_offs = new GDV_Ref_Offs[PARAMS.TEXAS_MODEL_NLGP2];
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVMSMM + 1); lsiv_i++) {
            mcla_vms_message[lsiv_i] = new VMS_Message();
        }
        for (int lsiv_i = 0; lsiv_i < PARAMS.TEXAS_MODEL_NLGP2; lsiv_i++) {
            mcla_gdv_ref_lane[lsiv_i] = new GDV_Ref_Lane();
            mcla_gdv_ref_offs[lsiv_i] = new GDV_Ref_Offs();
        }
    } // end of method TX_SIM_Data

    public void setAllInvalid() {
        // setAllInvalid sets all TX_SIM_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_SIM_Data.setAllInvalid");
        mclv_sim_header.setAllInvalid();
        mclv_sim_version.setAllInvalid();
        mclv_sim_title.setAllInvalid();
        mclv_sim_par_opt.setAllInvalid();
        mclv_sim_par_opt_2.setAllInvalid();
        mclv_tx_sig_setup.setAllInvalid();
        mclv_det.setAllInvalid();
        mclv_gdv_ref_file.setAllInvalid();
        mclv_gdv_ref_leg.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVMSMM + 1); lsiv_i++) {
            mcla_vms_message[lsiv_i].setAllInvalid();
        }
        for (int lsiv_i = 0; lsiv_i < PARAMS.TEXAS_MODEL_NLGP2; lsiv_i++) {
            mcla_gdv_ref_lane[lsiv_i].setAllInvalid();
            mcla_gdv_ref_offs[lsiv_i].setAllInvalid();
        }
        return;
    } // end of method setAllInvalid
} // end of class TX_SIM_Data

/******************************************************************************/
/* TX_SIM_Data.java */
/******************************************************************************/
