package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                TX_Aux.java                                 */
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
import java.text.DecimalFormat;
import java.util.*;

class TX_Aux /* Auxiliary data needed for processing TEXAS Model data */
{

    int msia_stat[]; /* current status of data in this field: */

    /* TX_DATA_ERROR data in field has an error */
    /* TX_DATA_IS_INVALID data in field is invalid ( not set ) */
    /* TX_DEFAULT data in field has been assigned a default value */
    /* TX_FROM_FILE data in field has been read from card images */
    /* TX_FROM_USER data in field has been collected from the user */
    /* TX_SET_BY_SOFTWARE data field has been set by software */

    public TX_Aux() {
        msia_stat = new int[Intersection.TX_FMT_MAX_FIELDS + 1];
    } // end of method TX_Aux

    public void printAllStatus(int psiv_num // number of fields to process
    ) {
        // printAllStatus prints the status information for TX_Aux fields 1 through psiv_num

        if ((psiv_num < 1) || (psiv_num > Intersection.TX_FMT_MAX_FIELDS)) {
            Intersection.mstv_errorMessage = "Error in TX_Aux.printAllStatus: psiv_num = " + psiv_num + " is < 1 or > " + Intersection.TX_FMT_MAX_FIELDS + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        for (int field = 1; field <= psiv_num; field++) {
            printStatus(field);
        }
        return;
    } // end of method printAllStatus

    public void printStatus(int psiv_num // field number
    ) {
        int lsiv_num_min;

        // printStatus prints the status information for TX_Aux field psiv_num

        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_num_min = 0;
        }
        else {
            lsiv_num_min = 1;
        }
        if ((psiv_num < lsiv_num_min) || (psiv_num > Intersection.TX_FMT_MAX_FIELDS)) {
            Intersection.mstv_errorMessage = "Error in TX_Aux.printStatus: psiv_num = " + psiv_num + " is < " + lsiv_num_min + " or > " + Intersection.TX_FMT_MAX_FIELDS + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        System.out.println("TX_Aux.printStatus: " + status(psiv_num));
        return;
    } // end of method printStatus

    public void setAllInvalid() {
        // setAllInvalid sets all TX_Aux fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_Aux.setAllInvalid");
        for (int field = 0; field <= Intersection.TX_FMT_MAX_FIELDS; field++) {
            msia_stat[field] = Intersection.TX_DATA_IS_INVALID;
        }
        return;
    } // end of method setAllInvalid

    public String status(int psiv_num // field number
    ) {
        int lsiv_num_min;
        String lstv_status;
        DecimalFormat format2Int = new DecimalFormat("00");

        // status returns the status of field psiv_num as a string

        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_num_min = 0;
        }
        else {
            lsiv_num_min = 1;
        }
        if ((psiv_num < lsiv_num_min) || (psiv_num > Intersection.TX_FMT_MAX_FIELDS)) {
            return ("TX_Aux.printStatus Error: psiv_num = " + psiv_num + " is < " + lsiv_num_min + " or > " + Intersection.TX_FMT_MAX_FIELDS);
        }
        switch (msia_stat[psiv_num]) {
            case Intersection.TX_DATA_ERROR:
                lstv_status = "TX_DATA_ERROR";
                break;

            case Intersection.TX_DATA_IS_INVALID:
                lstv_status = "TX_DATA_IS_INVALID";
                break;

            case Intersection.TX_DEFAULT:
                lstv_status = "TX_DEFAULT";
                break;

            case Intersection.TX_FROM_FILE:
                lstv_status = "TX_FROM_FILE";
                break;

            case Intersection.TX_FROM_USER:
                lstv_status = "TX_FROM_USER";
                break;

            case Intersection.TX_SET_BY_SOFTWARE:
                lstv_status = "TX_SET_BY_SOFTWARE";
                break;

            default:
                lstv_status = "ERROR";
        }

        return ("status[" + format2Int.format(psiv_num) + "] = " + msia_stat[psiv_num] + " = " + lstv_status);
    } // end of method status
} // end of class TX_Aux

/******************************************************************************/
/* TX_Aux.java */
/******************************************************************************/
