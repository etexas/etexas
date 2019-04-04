package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              TX_Leg_Data.java                              */
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

class TX_Leg_Data /* leg data */
{

    Leg_Geo mclv_geo; /* leg geometry */

    Traf_Hdway mclv_hdway; /* inbound traffic headway data */

    Traf_Mix mclv_mix; /* inbound traffic vehicle class percentages */

    Traf_Dest mclv_dest; /* outbound traffic destination percentages by leg */

    Varying_Traffic_Period mcla_var_traf_period[]; /*
                                                    * varying traffic period data
                                                    */

    public TX_Leg_Data() {
        mclv_geo = new Leg_Geo();
        mclv_hdway = new Traf_Hdway();
        mclv_mix = new Traf_Mix();
        mclv_dest = new Traf_Dest();
        mcla_var_traf_period = new Varying_Traffic_Period[PARAMS.TEXAS_MODEL_NVT + 1];
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVT + 1); lsiv_i++) {
            mcla_var_traf_period[lsiv_i] = new Varying_Traffic_Period();
        }
    } // end of method TX_Leg_Data

    public void setAllInvalid() {
        // setAllInvalid sets all TX_Leg_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_Leg_Data.setAllInvalid");
        mclv_geo.setAllInvalid();
        mclv_hdway.setAllInvalid();
        mclv_mix.setAllInvalid();
        mclv_dest.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVT + 1); lsiv_i++) {
            mcla_var_traf_period[lsiv_i].setAllInvalid();
        }
        return;
    } // end of method setAllInvalid
} // end of class TX_Leg_Data

/******************************************************************************/
/* TX_Leg_Data.java */
/******************************************************************************/
