package org.etexascode.gui;

/******************************************************************************/
/*                                 texas.java                                 */
/******************************************************************************/
/*                                                                            */
/*      texas COPYRIGHT (C) 2007 by Rioux Engineering, Austin, Texas USA      */
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

import java.io.*;
import java.lang.Runtime.*;
import javax.swing.JOptionPane;

public class texas {

    // disdat Output from DISPRE Input to DISPRO Animation data in converted format for use by
    // DISPRO with '.r<n>' added for REPRUN/REPTOL.
    // dispar Input to DISPRE Animation setup data.
    // dvplist.txt Output from DVPRO /GDVPRO Listing of input data and processing output with
    // '_r<n>' added for REPRUN/REPTOL.
    // emerr.txt Output from EMPRO Listing of errors.
    // emlist.txt Output from EMPRO Listing of input data and processing output.
    // emsta.txt Output from EMPRO Emissions statistics details.
    // emstafin.txt Output from EMPRO Emissions statistics final results.
    // emstaave.txt Output from EMPRO Emissions statistics average final results.
    // fort8 Output from GEOPRO/GDVPRO Input to SIMPRO GEOPRO data for use by SIMPRO with '.rep'
    // added for REPRUN/REPTOL.
    // fort9 Output from DVPRO /GDVPRO Input to SIMPRO DVPRO data for use by SIMPRO with '.r<n>'
    // added for REPRUN/REPTOL.
    // gdv Output from GDVCONV Input to GEOPRO /DVPRO /GDVPRO GEOPRO and DVPRO data in converted
    // format for use by GEOPRO/DVPRO/GDVPRO with '.rep' added for REPRUN/REPTOL.
    // gdvdata Output from GDVSIM Input to GDVCONV/REPRUN/REPTOL GEOPRO and DVPRO data in GDVSIM
    // format; will be converted to file gdv.
    // geolist.txt Output from GEOPRO/GDVPRO Listing of input data and processing output with '_rep'
    // added for REPRUN/REPTOL.
    // geoplot Output from GEOPRO/GDVPRO Input to GEOPLOT Geometry plot data for use by GEOPLOT with
    // '.rep' added for REPRUN/REPTOL.
    // params.txt Output from REPRUN/REPTOL Replicate run parameters file for use by REPRUN/REPTOL
    // with '_rep' added for REPRUN/REPTOL.
    // posdat Output from SIMPRO Input to DISPRE /EMPRO Animation/Emissions data for use by
    // DISPRO/EMPRO with '.r<n>' added for REPRUN/REPTOL; will be converted to file disdat.
    // sim Output from SIMCONV Input to SIMPRO SIMPRO data in converted format for use by SIMPRO
    // with '.rep' added for REPRUN/REPTOL.
    // simdata Output from GDVSIM Input to SIMCONV/REPRUN/REPTOL SIMPRO data in gdvsim format; will
    // be converted to file sim.
    // simerr.txt Output from SIMPRO Listing of errors with '_r<n>' added for REPRUN/REPTOL.
    // simplst.txt Output from SIMPRO Listing of input data and processing output with '_r<n>' added
    // for REPRUN/REPTOL.
    // simslst.txt Output from SIMSTA Replicate run summary statistics with '_rep' added for
    // REPRUN/REPTOL.
    // simstat Output from SIMPRO Input to SIMSTA Replicate run summary statistics in compact format
    // with '.r<n>' added for REPRUN/REPTOL.
    // sprdsht.csv Output from SIMSTA Spread sheet compatible data file.
    // ssam.trj Output from SIMPRO Surrogate Safety Assessment Methodology data with '_r<n>' added
    // for REPRUN/REPTOL.

    public static String TexasDirectory;

    public static String TexasExeDirectory;

    public static String TexasSysDatDirectory;

    public static String TexasUtilityDirectory;

    public static String ProjectDirectory;

    public static String ProjectName = "";

    public static String DISDAT_file;

    public static String DISDAT_name = "disdat";

    public static String DISPAR_file;

    public static String DISPAR_name = "dispar";

    public static String DVPLIST_file;

    public static String DVPLIST_name = "dvplist.txt";

    public static String EMERR_file;

    public static String EMERR_name = "emerr.txt";

    public static String EMLIST_file;

    public static String EMLIST_name = "emlist.txt";

    public static String EMSTA_file;

    public static String EMSTA_name = "emsta.txt";

    public static String EMSTAFIN_file;

    public static String EMSTAFIN_name = "emstafin.txt";

    public static String EMSTAVE_file;

    public static String EMSTAVE_name = "emstaave.txt";

    public static String FORT8_file;

    public static String FORT8_name = "fort8";

    public static String FORT9_file;

    public static String FORT9_name = "fort9";

    public static String GDV_file;

    public static String GDV_name = "gdv";

    public static String GDVDATA_file;

    public static String GDVDATA_name = "gdvdata";

    public static String GEOLIST_file;

    public static String GEOLIST_name = "geolist.txt";

    public static String GEOPLOT_file;

    public static String GEOPLOT_name = "geoplot";

    public static String PARAMS_file;

    public static String PARAMS_name = "params.txt";

    public static String POSDAT_file;

    public static String POSDAT_name = "posdat";

    public static String SIM_file;

    public static String SIM_name = "sim";

    public static String SIMDATA_file;

    public static String SIMDATA_name = "simdata";

    public static String SIMERR_file;

    public static String SIMERR_name = "simerr.txt";

    public static String SIMPLST_file;

    public static String SIMPLST_name = "simplst.txt";

    public static String SIMSLST_file;

    public static String SIMSLST_name = "simslst.txt";

    public static String SIMSTAT_file;

    public static String SIMSTAT_name = "simstat";

    public static String SPRDSHT_file;

    public static String SPRDSHT_name = "sprdsht.csv";

    public static String SSAM_file;

    public static String SSAM_name = "ssam.trj";

    public static void main(String psta_args[] /* command line parameters */
    ) {

        String SketchFileName;
        File SketchFile;
        String mstv_errorMessage = "";

        try {
            if (File.separator.equals("\\")) {
                // Windows operating system
                while (true) {
                    TexasDirectory = "c:\\texas";
                    SketchFileName = TexasDirectory.concat("\\sys_dat\\sketch");
                    SketchFile = new File(SketchFileName);
                    mstv_errorMessage = "Error in texas processing SketchFile.exists () for SketchFileName = \"" + SketchFileName + ".";
                    if (SketchFile.exists()) {
                        mstv_errorMessage = "Error in texas processing SketchFile.canRead () for SketchFileName = \"" + SketchFileName + ".";
                        if (SketchFile.canRead()) {
                            mstv_errorMessage = "Error in texas processing SketchFile.isDirectory () for SketchFileName = \"" + SketchFileName + ".";
                            if (!SketchFile.isDirectory()) {
                                break;
                            }
                        }
                    }
                    TexasDirectory = "d:\\texas";
                    SketchFileName = TexasDirectory.concat("\\sys_dat\\sketch");
                    SketchFile = new File(SketchFileName);
                    mstv_errorMessage = "Error in texas processing SketchFile.exists () for SketchFileName = \"" + SketchFileName + ".";
                    if (SketchFile.exists()) {
                        mstv_errorMessage = "Error in texas processing SketchFile.canRead () for SketchFileName = \"" + SketchFileName + ".";
                        if (SketchFile.canRead()) {
                            mstv_errorMessage = "Error in texas processing SketchFile.isDirectory () for SketchFileName = \"" + SketchFileName + ".";
                            if (!SketchFile.isDirectory()) {
                                break;
                            }
                        }
                    }
                    TexasDirectory = "e:\\texas";
                    SketchFileName = TexasDirectory.concat("\\sys_dat\\sketch");
                    SketchFile = new File(SketchFileName);
                    mstv_errorMessage = "Error in texas processing SketchFile.exists () for SketchFileName = \"" + SketchFileName + ".";
                    if (SketchFile.exists()) {
                        mstv_errorMessage = "Error in texas processing SketchFile.canRead () for SketchFileName = \"" + SketchFileName + ".";
                        if (SketchFile.canRead()) {
                            mstv_errorMessage = "Error in texas processing SketchFile.isDirectory () for SketchFileName = \"" + SketchFileName + ".";
                            if (!SketchFile.isDirectory()) {
                                break;
                            }
                        }
                    }
                    mstv_errorMessage = "Error in texas: can not locate sketch file in \"c:\\texas\\sys_dat\\sketch\", \"d:\\texas\\sys_dat\\sketch\", or \"e:\\texas\\sys_dat\\sketch\".";
                    System.out.println(mstv_errorMessage);
                    JOptionPane.showMessageDialog(null, mstv_errorMessage, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            else if (File.separator.equals("/")) {
                // Unix/Linux operating system
                while (true) {
                    TexasDirectory = "/usr/texas";
                    SketchFileName = TexasDirectory.concat("/sys_dat/sketch");
                    SketchFile = new File(SketchFileName);
                    mstv_errorMessage = "Error in texas processing SketchFile.exists () for SketchFileName = \"" + SketchFileName + ".";
                    if (SketchFile.exists()) {
                        mstv_errorMessage = "Error in texas processing SketchFile.canRead () for SketchFileName = \"" + SketchFileName + ".";
                        if (SketchFile.canRead()) {
                            mstv_errorMessage = "Error in texas processing SketchFile.isDirectory () for SketchFileName = \"" + SketchFileName + ".";
                            if (!SketchFile.isDirectory()) {
                                break;
                            }
                        }
                    }
                    mstv_errorMessage = "Error in texas: can not locate sketch file in \"" + SketchFileName + "\".";
                    System.out.println(mstv_errorMessage);
                    JOptionPane.showMessageDialog(null, mstv_errorMessage, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            else if (File.separator.equals(":")) {
                // Mac operating system
                while (true) {
                    TexasDirectory = "::texas";
                    SketchFileName = TexasDirectory.concat(":sys_dat:sketch");
                    SketchFile = new File(SketchFileName);
                    mstv_errorMessage = "Error in texas processing SketchFile.exists () for SketchFileName = \"" + SketchFileName + ".";
                    if (SketchFile.exists()) {
                        mstv_errorMessage = "Error in texas processing SketchFile.canRead () for SketchFileName = \"" + SketchFileName + ".";
                        if (SketchFile.canRead()) {
                            mstv_errorMessage = "Error in texas processing SketchFile.isDirectory () for SketchFileName = \"" + SketchFileName + ".";
                            if (!SketchFile.isDirectory()) {
                                break;
                            }
                        }
                    }
                    mstv_errorMessage = "Error in texas: can not locate sketch file in \"" + SketchFileName + "\".";
                    System.out.println(mstv_errorMessage);
                    JOptionPane.showMessageDialog(null, mstv_errorMessage, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            else {
                mstv_errorMessage = "Error in texas: File.separator =  \"" + File.separator + "\" is not \"\\\", \"/\", or \":\".";
                System.out.println(mstv_errorMessage);
                JOptionPane.showMessageDialog(null, mstv_errorMessage, "Error Message", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            System.out.println(mstv_errorMessage);
            JOptionPane.showMessageDialog(null, mstv_errorMessage, "Error Message", JOptionPane.ERROR_MESSAGE);
            System.out.println(e.getLocalizedMessage());
            return;
        }

        TexasExeDirectory = TexasDirectory + File.separator + "exe";
        TexasSysDatDirectory = TexasDirectory + File.separator + "sys_dat";
        TexasUtilityDirectory = TexasDirectory + File.separator + "utility";

        // open main texas dialog box and go away

        new TexasModelDialog();
    }
}
