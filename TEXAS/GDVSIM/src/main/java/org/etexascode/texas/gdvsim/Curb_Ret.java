package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Curb_Ret.java                                */
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
import org.etexascode.texas.gdvsim.TX_Aux;
import org.etexascode.texas.gdvsim.TX_Fmt;
import java.io.*;
import java.lang.*;
import java.util.*;

class Curb_Ret {

    int msia_radius[]; /*
                        * curb return radii (negative indicates setback of from stopline)
                        */

    TX_Aux mclv_aux; /* Auxiliary data */

    /* the following variables are defined but are not read or written */
    int msiv_center_x; /* Center X coordinate */

    int msiv_center_y; /* Center Y coordinate */

    int msiv_begin_azimuth; /* Beginning azimuth in degrees */

    int msiv_sweep_angle; /* Sweep angle in degrees */

    int msiv_radius; /* Radius */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_CURB_RETURN]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_CURB_RETURN_RADIUS_1]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_CURB_RETURN_RADIUS_2]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_CURB_RETURN_RADIUS_3]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_CURB_RETURN_RADIUS_4]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_CURB_RETURN_RADIUS_5]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_CURB_RETURN_RADIUS_6]

    // Diamond Interchange Numbering Convention
    //
    // | /|\ | |
    // 6 1 | |
    // \|/ | 6 | 4 1 | 2
    // <-5-> <-7-> <-0-> <-2-> ----|--------------|----
    // | /|\ 5 | 4 1 | 3
    // 4 3 | |
    // \|/ | | |
    //
    // Leg Numbers Curb Return Radius Numbers
    //

    public Curb_Ret() {
        msia_radius = new int[PARAMS.TEXAS_MODEL_NLGP1];
        mclv_aux = new TX_Aux();
    } // end of method Curb_Ret

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Curb_Ret data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Curb_Ret.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_CURB_RETURN];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Curb_Ret.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Curb_Ret.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Curb_Ret.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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
        int lsiv_leg;
        int lsiv_member;
        String lstv_name;

        // printToFile prints all Curb_Ret data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Curb_Ret.printToFile");
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Curb_Ret.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_CURB_RETURN];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        lsiv_member = Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1;
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Curb_Ret.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_leg = 1; lsiv_leg <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msia_radius[lsiv_leg], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, lsiv_member + lsiv_leg, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        boolean lbov_force_default;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all Curb_Ret fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Curb_Ret.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Curb_Ret.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if curb return card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Curb_Ret.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Curb_Ret.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lbov_force_default = false;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_CURB_RETURN];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Curb_Ret.readFromCards reading " + lstv_name);

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 1) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msia_radius[1] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_1, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 2) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msia_radius[2] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_2, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 3) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msia_radius[3] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_3, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 4) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msia_radius[4] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_4, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 5) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msia_radius[5] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_5, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 6) {
            lbov_force_default = false;
        }
        else {
            lbov_force_default = true;
        }
        msia_radius[6] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_6, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Curb_Ret fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Curb_Ret.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Curb_Ret fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Curb_Ret.writeToCards");
        // check if curb return card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Curb_Ret.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Curb_Ret.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_CURB_RETURN];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Curb_Ret.writeToCards writing " + lstv_name);

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 1) {
            Intersection.writeIntToCard(msia_radius[1], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_1, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 2) {
            Intersection.writeIntToCard(msia_radius[2], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_2, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 3) {
            Intersection.writeIntToCard(msia_radius[3], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_3, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 4) {
            Intersection.writeIntToCard(msia_radius[4], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_4, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 5) {
            Intersection.writeIntToCard(msia_radius[5], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_5, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 6) {
            Intersection.writeIntToCard(msia_radius[6], lstv_name, Intersection.TX_FMT_GDV_CURB_RETURN, Intersection.TX_FMT_GDV_CURB_RETURN_RADIUS_6, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Curb_Ret

/******************************************************************************/
/* Curb_Ret.java */
/******************************************************************************/
