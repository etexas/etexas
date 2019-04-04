package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_Image_File.java                             */
/******************************************************************************/
/*                                                                            */
/*     gdvsim COPYRIGHT (C) 2007 by Rioux Engineering, Austin, Texas USA      */
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

class GDV_Image_File {

    String mstv_image_file_name; /* image file name */

    String mstv_image_file_type; /* image file type (jpg) */

    String mstv_image_file_attachment; /*
                                        * image file attachment (absolute, relative)
                                        */

    double mdfv_image_file_translation_x; /* image file translation x value (feet) */

    double mdfv_image_file_translation_y; /* image file translation x value (feet) */

    double mdfv_image_file_rotation; /*
                                      * image file rotation (degrees) (-=CCW, +=CW)
                                      */

    double mdfv_image_file_scale; /* image file scale (pixels per foot) */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_IMAGE_FILE]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_NAME ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_TYPE ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_ATTACHMENT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_ROTATION ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale
    // .mclv_aux.msia_stat[TX_FMT_GDV_IMAGE_FILE_SCALE ]

    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt .mstv_image_file_attached
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH]

    public GDV_Image_File() {
        mclv_aux = new TX_Aux();
    } // end of method GDV_Image_File

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Image_File data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Image_File.checkForErrors");
        // check
        // Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Intersection.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_IMAGE_FILE];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Image_File.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Image_File.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Image_File.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Image_File data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Image_File.printToFile");
        // check
        // Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Intersection.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_IMAGE_FILE];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Image_File.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(7);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_image_file_name, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_NAME, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_image_file_type, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TYPE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_image_file_attachment, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_image_file_translation_x, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_image_file_translation_y, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_image_file_rotation, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_ROTATION, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_image_file_scale, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_SCALE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(String pstv_gdvdataFileName) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_gdvdataFileDirectory;
        String lstv_name;

        // readFromCards reads all GDV_Image_File fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Image_File.readFromCards");
        // check parameters
        if (pstv_gdvdataFileName == null) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.readFromCards: pstv_gdvdataFileName is null.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (pstv_gdvdataFileName.trim().length() == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.readFromCards: pstv_gdvdataFileName is empty.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if special vehicle card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SPECIAL_VEH_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check
        // Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Intersection.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_IMAGE_FILE];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card + Intersection.msiv_specialVehicles;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Image_File.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_image_file_name = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_NAME, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_card++;

        // read data from cards
        mstv_image_file_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_image_file_attachment = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_image_file_translation_x = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_image_file_translation_y = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_image_file_rotation = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_ROTATION, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_image_file_scale = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_SCALE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // fix mstv_image_file_name
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached != null) {
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("YES")) {
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment != null) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment.equals("RELATIVE")) {
                        if (mstv_image_file_name == null) {
                            Intersection.mstv_warningMessage = "Warning in Intersection.readFromCards mstv_image_file_name is null.";
                            Intersection.warningMessage();
                        }
                        else {
                            if (mstv_image_file_name.trim().length() <= 0) {
                                Intersection.mstv_warningMessage = "Warning in Intersection.readFromCards mstv_image_file_name is empty.";
                                Intersection.warningMessage();
                            }
                            else {
                                Intersection.mstv_errorMessage = "Error in Intersection.readFromCards making mstv_image_file_name relative.";
                                try {
                                    lstv_gdvdataFileDirectory = Intersection.getDirectory(pstv_gdvdataFileName);
                                    if ((Intersection.msiv_returnCode == Intersection.RETURN_SUCCESS) && (lstv_gdvdataFileDirectory.trim().length() > 0)) {
                                        mstv_image_file_name = lstv_gdvdataFileDirectory + File.separator + mstv_image_file_name;
                                    }
                                }
                                catch (Exception e) {
                                    e.printStackTrace();
                                    Intersection.errorMessage();
                                    Intersection.errorMessage(e.getLocalizedMessage());
                                    Intersection.closeAll();
                                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Image_File fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Image_File.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(String pstv_gdvdataFileName) {
        File lclv_image_file_name = null;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_gdvdataFileDirectory;
        String lstv_gdvdataFileDirectoryU;
        String lstv_image_file_name;
        String lstv_image_file_nameU;
        String lstv_name;

        // writeToCards writes all GDV_Image_File fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Image_File.writeToCards");
        // check parameters
        if (pstv_gdvdataFileName == null) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.writeToCards: pstv_gdvdataFileName is null.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (pstv_gdvdataFileName.trim().length() == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.writeToCards: pstv_gdvdataFileName is empty.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if image file card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_IMAGE_FILE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Image_File.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_image_file_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check
        // Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Intersection.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("NO")) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_IMAGE_FILE];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_image_file_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Image_File.writeToCards writing " + lstv_name);

        // fix GDV_Image_File mstv_image_file_name
        lstv_image_file_name = mstv_image_file_name;
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached != null) {
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("YES")) {
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment != null) {
                    if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment.equals("RELATIVE")) {
                        if (lstv_image_file_name == null) {
                            Intersection.mstv_warningMessage = "Warning in Intersection.writeToCards mstv_image_file_name is null.";
                            Intersection.warningMessage();
                        }
                        else {
                            if (lstv_image_file_name.trim().length() <= 0) {
                                Intersection.mstv_warningMessage = "Warning in Intersection.writeToCards mstv_image_file_name is empty.";
                                Intersection.warningMessage();
                            }
                            else {
                                Intersection.mstv_errorMessage = "Error in Intersection.writeToCards making mstv_image_file_name relative.";
                                try {
                                    lstv_image_file_nameU = lstv_image_file_name;
                                    lstv_gdvdataFileDirectory = Intersection.getDirectory(pstv_gdvdataFileName);
                                    lstv_gdvdataFileDirectoryU = lstv_gdvdataFileDirectory;
                                    if (File.separator.equals("\\")) {
                                        lstv_image_file_nameU = lstv_image_file_name.toUpperCase();
                                        lstv_gdvdataFileDirectoryU = lstv_gdvdataFileDirectory.toUpperCase();
                                    }
                                    if (lstv_image_file_nameU.startsWith(lstv_gdvdataFileDirectoryU)) {
                                        lstv_image_file_name = lstv_image_file_name.substring(lstv_gdvdataFileDirectory.length() + 1);
                                    }
                                    else {
                                        Intersection.mstv_warningMessage = "Warning in Intersection.writeToCards Image File Name = \"" + lstv_image_file_nameU
                                                + "\" does not start with GDVDATA file device and directory = \"" + lstv_gdvdataFileDirectoryU + "\".\nImage File Attachment changed to ABSOLUTE.";
                                        Intersection.warningMessage();
                                        mstv_image_file_attachment = "ABSOLUTE";
                                    }
                                }
                                catch (Exception e) {
                                    e.printStackTrace();
                                    Intersection.errorMessage();
                                    Intersection.errorMessage(e.getLocalizedMessage());
                                    Intersection.closeAll();
                                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }

        // write data to cards
        Intersection.writeStringToCard(lstv_image_file_name, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_NAME, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_card++;

        // write data to cards
        Intersection.writeStringToCard(mstv_image_file_type, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_image_file_attachment, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_image_file_translation_x, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_image_file_translation_y, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_image_file_rotation, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_ROTATION, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_image_file_scale, lstv_name, Intersection.TX_FMT_GDV_IMAGE_FILE, Intersection.TX_FMT_GDV_IMAGE_FILE_SCALE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Image_File

/******************************************************************************/
/* GDV_Image_File.java */
/******************************************************************************/
