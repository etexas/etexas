package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              TX_GDV_Data.java                              */
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

class TX_GDV_Data {

    GDV_Hdr mclv_gdv_header;

    GDV_Ver mclv_gdv_version;

    TX_Title mclv_gdv_title; /* title text */

    GDV_Par_Opt mclv_gdv_par_opt; /* parameter-option data */

    Curb_Ret mclv_curb;

    GDV_FreeUTurns mcla_gdv_free_uturn[];

    GDV_DiamondLeg mclv_diamond_leg;

    GDV_DiamondLane mcla_diamond_lane_inb_right[];

    GDV_DiamondLane mcla_diamond_lane_inb_left[];

    Arcs mclv_arcs;

    Lines mclv_lines;

    SDRs mclv_sdrs;

    GDV_Plot_Opts mclv_gdv_plot_opts;

    GDV_Par_Opt_2 mclv_gdv_par_opt_2;

    GDV_DriverMix mclv_gdv_driver_mix;

    GDV_Veh_Length mclv_gdv_veh_length;

    GDV_Veh_OperChar mclv_gdv_veh_oper_char;

    GDV_Veh_MaxDecel mclv_gdv_veh_max_decel;

    GDV_Veh_MaxAccel mclv_gdv_veh_max_accel;

    GDV_Veh_MaxVel mclv_gdv_veh_max_vel;

    GDV_Veh_MinRad mclv_gdv_veh_min_rad;

    GDV_Veh_Classification mclv_gdv_veh_classification;

    GDV_Veh_Height mclv_gdv_veh_height;

    GDV_Veh_Width mclv_gdv_veh_width;

    GDV_Veh_Units mcla_gdv_veh_units[];

    GDV_Drv_OperChar mclv_gdv_drv_oper_char;

    GDV_Drv_PIJRTime mclv_gdv_drv_PIJR_time;

    GDV_SpecialVeh mcla_gdv_special_veh[];

    GDV_Image_File mclv_gdv_image_file;

    public TX_GDV_Data() {
        mclv_gdv_header = new GDV_Hdr();
        mclv_gdv_version = new GDV_Ver();
        mclv_gdv_title = new TX_Title();
        mclv_gdv_par_opt = new GDV_Par_Opt();
        mclv_curb = new Curb_Ret();
        mcla_gdv_free_uturn = new GDV_FreeUTurns[PARAMS.TEXAS_MODEL_NFU + 1];
        mclv_diamond_leg = new GDV_DiamondLeg();
        mcla_diamond_lane_inb_right = new GDV_DiamondLane[PARAMS.TEXAS_MODEL_NAL + 1];
        mcla_diamond_lane_inb_left = new GDV_DiamondLane[PARAMS.TEXAS_MODEL_NAL + 1];
        mclv_arcs = new Arcs();
        mclv_lines = new Lines();
        mclv_sdrs = new SDRs();
        mclv_gdv_plot_opts = new GDV_Plot_Opts();
        mclv_gdv_par_opt_2 = new GDV_Par_Opt_2();
        mclv_gdv_driver_mix = new GDV_DriverMix();
        mclv_gdv_veh_length = new GDV_Veh_Length();
        mclv_gdv_veh_oper_char = new GDV_Veh_OperChar();
        mclv_gdv_veh_max_decel = new GDV_Veh_MaxDecel();
        mclv_gdv_veh_max_accel = new GDV_Veh_MaxAccel();
        mclv_gdv_veh_max_vel = new GDV_Veh_MaxVel();
        mclv_gdv_veh_min_rad = new GDV_Veh_MinRad();
        mclv_gdv_veh_classification = new GDV_Veh_Classification();
        mclv_gdv_veh_height = new GDV_Veh_Height();
        mclv_gdv_veh_width = new GDV_Veh_Width();
        mcla_gdv_veh_units = new GDV_Veh_Units[PARAMS.TEXAS_MODEL_NVC + 1];
        mclv_gdv_drv_oper_char = new GDV_Drv_OperChar();
        mclv_gdv_drv_PIJR_time = new GDV_Drv_PIJRTime();
        mcla_gdv_special_veh = new GDV_SpecialVeh[PARAMS.TEXAS_MODEL_NSV + 1];
        mclv_gdv_image_file = new GDV_Image_File();

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NFU + 1); lsiv_i++) {
            mcla_gdv_free_uturn[lsiv_i] = new GDV_FreeUTurns();
        }

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NAL + 1); lsiv_i++) {
            mcla_diamond_lane_inb_right[lsiv_i] = new GDV_DiamondLane();
            mcla_diamond_lane_inb_left[lsiv_i] = new GDV_DiamondLane();
        }

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            mcla_gdv_veh_units[lsiv_i] = new GDV_Veh_Units();
        }

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NSV + 1); lsiv_i++) {
            mcla_gdv_special_veh[lsiv_i] = new GDV_SpecialVeh();
        }
    } // end of method TX_GDV_Data

    public void setAllInvalid() {
        // setAllInvalid sets all TX_GDV_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_GDV_Data.setAllInvalid");
        mclv_gdv_header.setAllInvalid();
        mclv_gdv_version.setAllInvalid();
        mclv_gdv_title.setAllInvalid();
        mclv_gdv_par_opt.setAllInvalid();
        mclv_curb.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NFU + 1); lsiv_i++) {
            mcla_gdv_free_uturn[lsiv_i].setAllInvalid();
        }
        mclv_diamond_leg.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NAL + 1); lsiv_i++) {
            mcla_diamond_lane_inb_right[lsiv_i].setAllInvalid();
            mcla_diamond_lane_inb_left[lsiv_i].setAllInvalid();
        }
        mclv_arcs.setAllInvalid();
        mclv_lines.setAllInvalid();
        mclv_sdrs.setAllInvalid();
        mclv_gdv_plot_opts.setAllInvalid();
        mclv_gdv_par_opt_2.setAllInvalid();
        mclv_gdv_driver_mix.setAllInvalid();
        mclv_gdv_veh_length.setAllInvalid();
        mclv_gdv_veh_oper_char.setAllInvalid();
        mclv_gdv_veh_max_decel.setAllInvalid();
        mclv_gdv_veh_max_accel.setAllInvalid();
        mclv_gdv_veh_max_vel.setAllInvalid();
        mclv_gdv_veh_min_rad.setAllInvalid();
        mclv_gdv_veh_classification.setAllInvalid();
        mclv_gdv_veh_height.setAllInvalid();
        mclv_gdv_veh_width.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            mcla_gdv_veh_units[lsiv_i].setAllInvalid();
        }
        mclv_gdv_drv_oper_char.setAllInvalid();
        mclv_gdv_drv_PIJR_time.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NSV + 1); lsiv_i++) {
            mcla_gdv_special_veh[lsiv_i].setAllInvalid();
        }
        return;
    } // end of method setAllInvalid
} // end of class TX_GDV_Data

/******************************************************************************/
/* TX_GDV_Data.java */
/******************************************************************************/
