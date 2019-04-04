package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                 Arcs.java                                  */
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

import org.etexascode.texas.gdvsim.Intersection;
import org.etexascode.texas.gdvsim.PARAMS;
import java.io.*;
import java.lang.*;
import java.util.*;

class Arcs {

    Arc_Hdr mclv_arc_header;

    Arc_Data mcla_arc_data[];

    public Arcs() {
        mclv_arc_header = new Arc_Hdr();
        mcla_arc_data = new Arc_Data[PARAMS.TEXAS_MODEL_NAR + 1];
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NAR + 1); lsiv_i++) {
            mcla_arc_data[lsiv_i] = new Arc_Data();
        }
    } // end of method Arcs

    public void setAllInvalid() {
        // setAllInvalid sets all Arcs fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Arcs.setAllInvalid");
        mclv_arc_header.setAllInvalid();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NAR + 1); lsiv_i++) {
            mcla_arc_data[lsiv_i].setAllInvalid();
        }
        return;
    } // end of method setAllInvalid
} // end of class Arcs

/******************************************************************************/
/* Arcs.java */
/******************************************************************************/
