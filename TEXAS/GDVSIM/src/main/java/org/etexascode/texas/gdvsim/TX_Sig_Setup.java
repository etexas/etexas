package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             TX_Sig_Setup.java                              */
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

class TX_Sig_Setup {

    int msiv_nema_dual_ring; /* NEMA dual ring controller */

    Pha_Timing mcla_ph[]; /* phase timing data for a 16-phase controller */

    Tex_Dia_Int mcla_tex_dia_int[]; /*
                                     * Texas Diamond Controller special intervals
                                     */

    Tex_Dia_Opt mcla_tex_dia_opt[]; /* Texas Diamond Controller special options */

    NEMA_movement mclv_NEMA_movement; /*
                                       * NEMA/HARDWARE traffic movements per controller phase
                                       */

    NEMA_ph_ov_tx mclv_NEMA_ph_ov_tx; /* NEMA/HARDWARE phase and overlap text */

    NEMA_ring_group mclv_NEMA_ring_group; /* NEMA ring and group controller phases */

    NEMA_phase_diag mclv_NEMA_phase_diag; /*
                                           * NEMA phase diagram (0=read from GDVS00, 1=use hard
                                           * coded)
                                           */

    Overlap_Def mcla_overlap_def[]; /*
                                     * overlap definition lists don't use mcla_overlap_def[0]
                                     */

    Pha_Sequence mcla_phase_sequence[]; /* phase sequence */

    TX_Det_Conn mcla_det_conn[]; /*
                                  * detector connection lists don't use mcla_det_conn[0]
                                  */

    public TX_Sig_Setup() {
        mcla_ph = new Pha_Timing[PARAMS.TEXAS_MODEL_NPN + 1];
        mcla_tex_dia_int = new Tex_Dia_Int[PARAMS.TEXAS_MODEL_DIA + 1];
        mcla_tex_dia_opt = new Tex_Dia_Opt[PARAMS.TEXAS_MODEL_DIA + 1];
        mclv_NEMA_movement = new NEMA_movement();
        mclv_NEMA_ph_ov_tx = new NEMA_ph_ov_tx();
        mclv_NEMA_ring_group = new NEMA_ring_group();
        mclv_NEMA_phase_diag = new NEMA_phase_diag();
        mcla_overlap_def = new Overlap_Def[PARAMS.TEXAS_MODEL_NON + 1];
        mcla_phase_sequence = new Pha_Sequence[PARAMS.TEXAS_MODEL_NPH + 1];
        mcla_det_conn = new TX_Det_Conn[PARAMS.TEXAS_MODEL_NPN + 1];
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPN + 1); lsiv_i++) {
            mcla_ph[lsiv_i] = new Pha_Timing();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_DIA + 1); lsiv_i++) {
            mcla_tex_dia_int[lsiv_i] = new Tex_Dia_Int();
            mcla_tex_dia_opt[lsiv_i] = new Tex_Dia_Opt();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NON + 1); lsiv_i++) {
            mcla_overlap_def[lsiv_i] = new Overlap_Def();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPH + 1); lsiv_i++) {
            mcla_phase_sequence[lsiv_i] = new Pha_Sequence();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPN + 1); lsiv_i++) {
            mcla_det_conn[lsiv_i] = new TX_Det_Conn();
        }
    } // end of method TX_Sig_Setup

    public void setAllInvalid() {
        // setAllInvalid sets all TX_Sig_Setup fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_Sig_Setup.setAllInvalid");
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPN + 1); lsiv_i++) {
            mcla_ph[lsiv_i].setAllInvalid();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_DIA + 1); lsiv_i++) {
            mcla_tex_dia_int[lsiv_i].setAllInvalid();
            mcla_tex_dia_opt[lsiv_i].setAllInvalid();
        }
        mclv_NEMA_movement.setAllInvalid();
        mclv_NEMA_ph_ov_tx.setAllInvalid();
        mclv_NEMA_ring_group.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NON + 1); lsiv_i++) {
            mcla_overlap_def[lsiv_i].setAllInvalid();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPH + 1); lsiv_i++) {
            mcla_phase_sequence[lsiv_i].setAllInvalid();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPN + 1); lsiv_i++) {
            mcla_det_conn[lsiv_i].setAllInvalid();
        }
        return;
    } // end of method setAllInvalid
} // end of class TX_Sig_Setup

/******************************************************************************/
/* TX_Sig_Setup.java */
/******************************************************************************/
