package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                TX_Fmt.java                                 */
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

class TX_Fmt {

    String mstv_name; /* descriptive name of data card */

    int msiv_nf; /* number of fields */

    String msta_desc[]; /* description of a data field */

    String msta_help[]; /* help information for a data field */

    int msia_fo[]; /* field offset from the start of the card (1=first column) */

    int msia_fs[]; /* field size, columns */

    int msia_fnd[]; /* field number of digits to the right of the decimal point */

    int msia_ft[]; /*
                    * field type 1 = int, 2 = double precision floating point, and 3 = text
                    */

    boolean mboa_def[]; /* field default value specified */

    double mdfa_min[]; /* double field minimum value */

    double mdfa_max[]; /* double field maximum value */

    double mdfa_inc[]; /* double field increment value (must be non zero) */

    double mdfa_def[]; /* double field default value */

    int msia_min[]; /* int field minimum value */

    int msia_max[]; /* int field maximum value */

    int msia_inc[]; /* int field increment value */

    int msia_def[]; /* int field default value */

    String msta_val[]; /* String field allowable values */

    String msta_def[]; /* String field default value */

    public TX_Fmt() {
        msta_desc = new String[Intersection.TX_FMT_MAX_FIELDS + 1];
        msta_help = new String[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_fo = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_fs = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_fnd = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_ft = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        mboa_def = new boolean[Intersection.TX_FMT_MAX_FIELDS + 1];
        mdfa_min = new double[Intersection.TX_FMT_MAX_FIELDS + 1];
        mdfa_max = new double[Intersection.TX_FMT_MAX_FIELDS + 1];
        mdfa_inc = new double[Intersection.TX_FMT_MAX_FIELDS + 1];
        mdfa_def = new double[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_min = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_max = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_inc = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msia_def = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
        msta_val = new String[Intersection.TX_FMT_MAX_FIELDS + 1];
        msta_def = new String[Intersection.TX_FMT_MAX_FIELDS + 1];
    } // end of method TX_Fmt
} // end of class TX_Fmt

/******************************************************************************/
/* TX_Fmt.java */
/******************************************************************************/
