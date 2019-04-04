package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_DiamondLeg.java                             */
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
import org.etexascode.texas.gdvsim.TX_Aux;
import org.etexascode.texas.gdvsim.TX_Fmt;
import java.io.*;
import java.lang.*;
import java.util.*;

class GDV_DiamondLeg {

    int msiv_int_leg_number; /* Internal leg number */

    int msiv_dist_between; /*
                            * Distance between Right Intersection and Left Intersection
                            */

    int msiv_lanes_inb_to_center_right; /*
                                         * Number of lanes inbound to Right Intersection
                                         */

    int msiv_lanes_inb_to_center_left; /*
                                        * Number of lanes inbound to Left Intersection
                                        */

    int msiv_speed_inb_to_center_right; /*
                                         * Speed limit on lanes inbound to Right Intersection
                                         */

    int msiv_speed_inb_to_center_left; /*
                                        * Speed limit on lanes inbound to Left Intersection
                                        */

    int msiv_median_width; /* Median width */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_DIAMOND_LEG]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_int_leg_number
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_INT_LEG_NUM ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_right
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_left
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH]

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
    //
    // | /|\ | /|\ | /|\
    // 8 16 1 9 8 -16-> -6--> 9
    // \|/ | \|/ | \|/ |
    // <-15- <--5- <-12- <-2-- <-15- <--5- <-12- <-2--
    // --7-> -13-> --4-> -10-> --7-> -13-> --4-> -10->
    // | /|\ | /|\ | /|\
    // 14 6 11 3 14 <--1- <-11- 3
    // \|/ | \|/ | \|/ |
    //
    // Approach Numbers Approach Numbers
    // (without free u-turns) (with free u-turns)

    public GDV_DiamondLeg() {
        mclv_aux = new TX_Aux();
    } // end of method GDV_DiamondLeg

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_DiamondLeg data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_DiamondLeg.checkForErrors");
        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.checkForErrors: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LEG];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_DiamondLeg.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_DiamondLeg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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
        String lstv_name;

        // printToFile prints all GDV_DiamondLeg data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_DiamondLeg.printToFile");
        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.printToFile: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LEG];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_DiamondLeg.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(6);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // do not print msiv_int_leg_number Intersection.TX_FMT_GDV_DIAMOND_LEG_INT_LEG_NUM

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_dist_between, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_lanes_inb_to_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_lanes_inb_to_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_speed_inb_to_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_speed_inb_to_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_median_width, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_leg;
        int lsiv_leg_cl;
        int lsiv_leg_cr;
        String lstv_name;

        // readFromCards reads all GDV_DiamondLeg fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLeg.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.readFromCards: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if leg 0 data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0] is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LEG];
        lsiv_card = Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0]);
        lsiv_leg_cl = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        lsiv_leg_cr = 0;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLeg.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_int_leg_number = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_INT_LEG_NUM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_dist_between = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_lanes_inb_to_center_right = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection and leg to Left Intersection
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLeg.readFromCards setting leg cr Intersection.Intersection.mcla_leg[" + lsiv_leg_cr
                    + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb=" + msiv_lanes_inb_to_center_right);
        Intersection.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = msiv_lanes_inb_to_center_right;
        Intersection.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLeg.readFromCards setting leg cl Intersection.Intersection.mcla_leg[" + lsiv_leg_cl
                    + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out=" + msiv_lanes_inb_to_center_right);
        Intersection.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = msiv_lanes_inb_to_center_right;
        Intersection.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] = Intersection.TX_SET_BY_SOFTWARE;

        // read data from cards
        msiv_lanes_inb_to_center_left = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection and leg to Left Intersection
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLeg.readFromCards setting leg cl Intersection.Intersection.mcla_leg[" + lsiv_leg_cl
                    + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb=" + msiv_lanes_inb_to_center_left);
        Intersection.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = msiv_lanes_inb_to_center_left;
        Intersection.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLeg.readFromCards setting leg cr Intersection.Intersection.mcla_leg[" + lsiv_leg_cr
                    + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out=" + msiv_lanes_inb_to_center_left);
        Intersection.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = msiv_lanes_inb_to_center_left;
        Intersection.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] = Intersection.TX_SET_BY_SOFTWARE;

        // read data from cards
        msiv_speed_inb_to_center_right = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_speed_inb_to_center_left = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_median_width = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_DiamondLeg fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_DiamondLeg.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all GDV_DiamondLeg fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_DiamondLeg.writeToCards");
        // check if leg 0 data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0] is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLeg.writeToCards: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LEG];
        lsiv_card = Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0]);
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_DiamondLeg.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_int_leg_number, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_INT_LEG_NUM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_dist_between, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_lanes_inb_to_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_lanes_inb_to_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_speed_inb_to_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_speed_inb_to_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_median_width, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LEG, Intersection.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_DiamondLeg

/******************************************************************************/
/* GDV_DiamondLeg.java */
/******************************************************************************/
