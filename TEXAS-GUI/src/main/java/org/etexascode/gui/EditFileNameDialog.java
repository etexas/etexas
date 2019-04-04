package org.etexascode.gui;

/******************************************************************************/
/*                          EditFileNameDialog.java                           */
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

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import java.lang.*;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.*;
import java.io.*;
import javax.swing.JOptionPane;
import java.awt.Graphics;

public class EditFileNameDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title, label_num_1, label_num_2, label_num_3, label_num_4, label_num_5, label_num_6, label_num_7, label_num_8, label_num_9, label_num_10, label_num_11, label_num_12, label_num_13,
            label_num_14, label_num_15, label_num_16, label_num_17, label_num_18, label_num_19, label_num_20, label_num_21, label_num_22, label_num_23, label_num_24, label_Heading_0, label_Heading_1,
            label_Heading_2, label_Heading_3, label_Heading_4, label_DISDAT_name_1, label_DISDAT_name_2, label_DISDAT_name_3, label_DISDAT_name_4, label_DISPAR_name_1, label_DISPAR_name_2,
            label_DISPAR_name_3, label_DISPAR_name_4, label_DVPLIST_name_1, label_DVPLIST_name_2, label_DVPLIST_name_3, label_DVPLIST_name_4, label_EMERR_name_1, label_EMERR_name_2,
            label_EMERR_name_3, label_EMERR_name_4, label_EMLIST_name_1, label_EMLIST_name_2, label_EMLIST_name_3, label_EMLIST_name_4, label_EMSTA_name_1, label_EMSTA_name_2, label_EMSTA_name_3,
            label_EMSTA_name_4, label_EMSTAFIN_name_1, label_EMSTAFIN_name_2, label_EMSTAFIN_name_3, label_EMSTAFIN_name_4, label_EMSTAVE_name_1, label_EMSTAVE_name_2, label_EMSTAVE_name_3,
            label_EMSTAVE_name_4, label_FORT8_name_1, label_FORT8_name_2, label_FORT8_name_3, label_FORT8_name_4, label_FORT9_name_1, label_FORT9_name_2, label_FORT9_name_3, label_FORT9_name_4,
            label_GDV_name_1, label_GDV_name_2, label_GDV_name_3, label_GDV_name_4, label_GDVDATA_name_1, label_GDVDATA_name_2, label_GDVDATA_name_3, label_GDVDATA_name_4, label_GEOLIST_name_1,
            label_GEOLIST_name_2, label_GEOLIST_name_3, label_GEOLIST_name_4, label_GEOPLOT_name_1, label_GEOPLOT_name_2, label_GEOPLOT_name_3, label_GEOPLOT_name_4, label_PARAMS_name_1,
            label_PARAMS_name_2, label_PARAMS_name_3, label_PARAMS_name_4, label_POSDAT_name_1, label_POSDAT_name_2, label_POSDAT_name_3, label_POSDAT_name_4, label_SIM_name_1, label_SIM_name_2,
            label_SIM_name_3, label_SIM_name_4, label_SIMDATA_name_1, label_SIMDATA_name_2, label_SIMDATA_name_3, label_SIMDATA_name_4, label_SIMERR_name_1, label_SIMERR_name_2, label_SIMERR_name_3,
            label_SIMERR_name_4, label_SIMPLST_name_1, label_SIMPLST_name_2, label_SIMPLST_name_3, label_SIMPLST_name_4, label_SIMSLST_name_1, label_SIMSLST_name_2, label_SIMSLST_name_3,
            label_SIMSLST_name_4, label_SIMSTAT_name_1, label_SIMSTAT_name_2, label_SIMSTAT_name_3, label_SIMSTAT_name_4, label_SPRDSHT_name_1, label_SPRDSHT_name_2, label_SPRDSHT_name_3,
            label_SPRDSHT_name_4, label_SSAM_name_1, label_SSAM_name_2, label_SSAM_name_3, label_SSAM_name_4;

    JTextField text_DISDAT_name, text_DISPAR_name, text_DVPLIST_name, text_EMERR_name, text_EMLIST_name, text_EMSTA_name, text_EMSTAFIN_name, text_EMSTAVE_name, text_FORT8_name, text_FORT9_name,
            text_GDV_name, text_GDVDATA_name, text_GEOLIST_name, text_GEOPLOT_name, text_PARAMS_name, text_POSDAT_name, text_SIM_name, text_SIMDATA_name, text_SIMERR_name, text_SIMPLST_name,
            text_SIMSLST_name, text_SIMSTAT_name, text_SPRDSHT_name, text_SSAM_name;

    JButton okButton, applyButton, cancelButton;

    Font font1, font2;

    HelpListener helpListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    public EditFileNameDialog() {
        aFrame = new JFrame();

        aFrame.setTitle("Edit File Name");

        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        container.setLayout(gbLayout);
        gbConstraints.fill = GridBagConstraints.BOTH;

        font1 = new Font("TimesRoman", Font.PLAIN, 18);
        font2 = new Font("TimesRoman", Font.BOLD, 14);

        label_Heading_0 = new JLabel("Current Name ");

        label_Heading_1 = new JLabel("Normal Name ");
        label_DISDAT_name_1 = new JLabel("disdat ");
        label_DISPAR_name_1 = new JLabel("dispar ");
        label_DVPLIST_name_1 = new JLabel("dvplist.txt ");
        label_EMERR_name_1 = new JLabel("emerr.txt ");
        label_EMLIST_name_1 = new JLabel("emlist.txt ");
        label_EMSTA_name_1 = new JLabel("emsta.txt ");
        label_EMSTAFIN_name_1 = new JLabel("emstafin.txt ");
        label_EMSTAVE_name_1 = new JLabel("emstaave.txt ");
        label_FORT8_name_1 = new JLabel("fort8 ");
        label_FORT9_name_1 = new JLabel("fort9 ");
        label_GDV_name_1 = new JLabel("gdv ");
        label_GDVDATA_name_1 = new JLabel("gdvdata ");
        label_GEOLIST_name_1 = new JLabel("geolist.txt ");
        label_GEOPLOT_name_1 = new JLabel("geoplot ");
        label_PARAMS_name_1 = new JLabel("params.txt ");
        label_POSDAT_name_1 = new JLabel("posdat ");
        label_SIM_name_1 = new JLabel("sim ");
        label_SIMDATA_name_1 = new JLabel("simdata ");
        label_SIMERR_name_1 = new JLabel("simerr.txt ");
        label_SIMPLST_name_1 = new JLabel("simplst.txt ");
        label_SIMSLST_name_1 = new JLabel("simslst.txt ");
        label_SIMSTAT_name_1 = new JLabel("simstat ");
        label_SPRDSHT_name_1 = new JLabel("sprdsht.csv ");
        label_SSAM_name_1 = new JLabel("ssam.trj ");

        label_Heading_2 = new JLabel("Usage ");
        label_DISDAT_name_2 = new JLabel("Output from DISPRE ");
        label_DISPAR_name_2 = new JLabel("Input  to   DISPRE ");
        label_DVPLIST_name_2 = new JLabel("Output from DVPRO /GDVPRO ");
        label_EMERR_name_2 = new JLabel("Output from EMPRO ");
        label_EMLIST_name_2 = new JLabel("Output from EMPRO ");
        label_EMSTA_name_2 = new JLabel("Output from EMPRO ");
        label_EMSTAFIN_name_2 = new JLabel("Output from EMPRO ");
        label_EMSTAVE_name_2 = new JLabel("Output from EMPRO ");
        label_FORT8_name_2 = new JLabel("Output from GEOPRO/GDVPRO ");
        label_FORT9_name_2 = new JLabel("Output from DVPRO /GDVPRO ");
        label_GDV_name_2 = new JLabel("Output from GDVCONV ");
        label_GDVDATA_name_2 = new JLabel("Output from GDVSIM ");
        label_GEOLIST_name_2 = new JLabel("Output from GEOPRO/GDVPRO ");
        label_GEOPLOT_name_2 = new JLabel("Output from GEOPRO/GDVPRO ");
        label_PARAMS_name_2 = new JLabel("Output from REPRUN/REPTOL ");
        label_POSDAT_name_2 = new JLabel("Output from SIMPRO ");
        label_SIM_name_2 = new JLabel("Output from SIMCONV ");
        label_SIMDATA_name_2 = new JLabel("Output from GDVSIM ");
        label_SIMERR_name_2 = new JLabel("Output from SIMPRO ");
        label_SIMPLST_name_2 = new JLabel("Output from SIMPRO ");
        label_SIMSLST_name_2 = new JLabel("Output from SIMSTA ");
        label_SIMSTAT_name_2 = new JLabel("Output from SIMPRO ");
        label_SPRDSHT_name_2 = new JLabel("Output from SIMSTA ");
        label_SSAM_name_2 = new JLabel("Output from SIMPRO ");

        label_Heading_3 = new JLabel("Usage ");
        label_DISDAT_name_3 = new JLabel("Input to DISPRO ");
        label_DISPAR_name_3 = new JLabel(" ");
        label_DVPLIST_name_3 = new JLabel(" ");
        label_EMERR_name_3 = new JLabel(" ");
        label_EMLIST_name_3 = new JLabel(" ");
        label_EMSTA_name_3 = new JLabel(" ");
        label_EMSTAFIN_name_3 = new JLabel(" ");
        label_EMSTAVE_name_3 = new JLabel(" ");
        label_FORT8_name_3 = new JLabel("Input to SIMPRO ");
        label_FORT9_name_3 = new JLabel("Input to SIMPRO ");
        label_GDV_name_3 = new JLabel("Input to GEOPRO /DVPRO /GDVPRO ");
        label_GDVDATA_name_3 = new JLabel("Input to GDVCONV/REPRUN/REPTOL ");
        label_GEOLIST_name_3 = new JLabel(" ");
        label_GEOPLOT_name_3 = new JLabel("Input to GEOPLOT ");
        label_PARAMS_name_3 = new JLabel(" ");
        label_POSDAT_name_3 = new JLabel("Input to DISPRE /EMPRO ");
        label_SIM_name_3 = new JLabel("Input to SIMPRO ");
        label_SIMDATA_name_3 = new JLabel("Input to SIMCONV/REPRUN/REPTOL ");
        label_SIMERR_name_3 = new JLabel(" ");
        label_SIMPLST_name_3 = new JLabel(" ");
        label_SIMSLST_name_3 = new JLabel(" ");
        label_SIMSTAT_name_3 = new JLabel("Input to SIMSTA ");
        label_SPRDSHT_name_3 = new JLabel(" ");
        label_SSAM_name_3 = new JLabel(" ");

        label_Heading_4 = new JLabel("Description ");
        label_DISDAT_name_4 = new JLabel("Animation data in converted format for use by DISPRO with '.r<n>' added for REPRUN/REPTOL ");
        label_DISPAR_name_4 = new JLabel("Animation setup data ");
        label_DVPLIST_name_4 = new JLabel("Listing of input data and processing output with '_r<n>' added for REPRUN/REPTOL ");
        label_EMERR_name_4 = new JLabel("Listing of errors ");
        label_EMLIST_name_4 = new JLabel("Listing of input data and processing output ");
        label_EMSTA_name_4 = new JLabel("Emissions statistics details ");
        label_EMSTAFIN_name_4 = new JLabel("Emissions statistics final results ");
        label_EMSTAVE_name_4 = new JLabel("Emissions statistics average final results ");
        label_FORT8_name_4 = new JLabel("GEOPRO data for use by SIMPRO with '.rep'  added for REPRUN/REPTOL ");
        label_FORT9_name_4 = new JLabel("DVPRO  data for use by SIMPRO with '.r<n>' added for REPRUN/REPTOL ");
        label_GDV_name_4 = new JLabel("GEOPRO and DVPRO data in converted format for use by GEOPRO/DVPRO/GDVPRO with '.rep' added for REPRUN/REPTOL ");
        label_GDVDATA_name_4 = new JLabel("GEOPRO and DVPRO data in GDVSIM format; will be converted to file gdv ");
        label_GEOLIST_name_4 = new JLabel("Listing of input data and processing output with '_rep' added for REPRUN/REPTOL ");
        label_GEOPLOT_name_4 = new JLabel("Geometry plot data for use by GEOPLOT with '.rep' added for REPRUN/REPTOL ");
        label_PARAMS_name_4 = new JLabel("Replicate run parameters file for use by REPRUN/REPTOL ");
        label_POSDAT_name_4 = new JLabel("Animation/Emissions data for use by DISPRO/EMPRO with '.r<n>' added for REPRUN/REPTOL; will be converted to file disdat ");
        label_SIM_name_4 = new JLabel("SIMPRO data in converted format for use by SIMPRO with '.rep' added for REPRUN/REPTOL ");
        label_SIMDATA_name_4 = new JLabel("SIMPRO data in gdvsim format; will be converted to file sim ");
        label_SIMERR_name_4 = new JLabel("Listing of errors with '_r<n>' added for REPRUN/REPTOL ");
        label_SIMPLST_name_4 = new JLabel("Listing of input data and processing output with '_r<n>' added for REPRUN/REPTOL ");
        label_SIMSLST_name_4 = new JLabel("Replicate run summary statistics with '_rep' added for REPRUN/REPTOL ");
        label_SIMSTAT_name_4 = new JLabel("Replicate run summary statistics in compact format with '.r<n>' added for REPRUN/REPTOL ");
        label_SPRDSHT_name_4 = new JLabel("Spread sheet compatible data file ");
        label_SSAM_name_4 = new JLabel("Surrogate Safety Assessment Methodology data with '_r<n>' added for REPRUN/REPTOL ");

        label_num_1 = new JLabel("  1");
        label_num_2 = new JLabel("  2");
        label_num_3 = new JLabel("  3");
        label_num_4 = new JLabel("  4");
        label_num_5 = new JLabel("  5");
        label_num_6 = new JLabel("  6");
        label_num_7 = new JLabel("  7");
        label_num_8 = new JLabel("  8");
        label_num_9 = new JLabel("  9");
        label_num_10 = new JLabel("10");
        label_num_11 = new JLabel("11");
        label_num_12 = new JLabel("12");
        label_num_13 = new JLabel("13");
        label_num_14 = new JLabel("14");
        label_num_15 = new JLabel("15");
        label_num_16 = new JLabel("16");
        label_num_17 = new JLabel("17");
        label_num_18 = new JLabel("18");
        label_num_19 = new JLabel("19");
        label_num_20 = new JLabel("20");
        label_num_21 = new JLabel("21");
        label_num_22 = new JLabel("22");
        label_num_23 = new JLabel("23");
        label_num_24 = new JLabel("24");

        text_DISDAT_name = new JTextField("", 20);
        text_DISPAR_name = new JTextField("", 20);
        text_DVPLIST_name = new JTextField("", 20);
        text_EMERR_name = new JTextField("", 20);
        text_EMLIST_name = new JTextField("", 20);
        text_EMSTA_name = new JTextField("", 20);
        text_EMSTAFIN_name = new JTextField("", 20);
        text_EMSTAVE_name = new JTextField("", 20);
        text_FORT8_name = new JTextField("", 20);
        text_FORT9_name = new JTextField("", 20);
        text_GDV_name = new JTextField("", 20);
        text_GDVDATA_name = new JTextField("", 20);
        text_GEOLIST_name = new JTextField("", 20);
        text_GEOPLOT_name = new JTextField("", 20);
        text_PARAMS_name = new JTextField("", 20);
        text_POSDAT_name = new JTextField("", 20);
        text_SIM_name = new JTextField("", 20);
        text_SIMDATA_name = new JTextField("", 20);
        text_SIMERR_name = new JTextField("", 20);
        text_SIMPLST_name = new JTextField("", 20);
        text_SIMSLST_name = new JTextField("", 20);
        text_SIMSTAT_name = new JTextField("", 20);
        text_SPRDSHT_name = new JTextField("", 20);
        text_SSAM_name = new JTextField("", 20);

        text_DISDAT_name.setText(texas.DISDAT_name);
        text_DISPAR_name.setText(texas.DISPAR_name);
        text_DVPLIST_name.setText(texas.DVPLIST_name);
        text_EMERR_name.setText(texas.EMERR_name);
        text_EMLIST_name.setText(texas.EMLIST_name);
        text_EMSTA_name.setText(texas.EMSTA_name);
        text_EMSTAFIN_name.setText(texas.EMSTAFIN_name);
        text_EMSTAVE_name.setText(texas.EMSTAVE_name);
        text_FORT8_name.setText(texas.FORT8_name);
        text_FORT9_name.setText(texas.FORT9_name);
        text_GDV_name.setText(texas.GDV_name);
        text_GDVDATA_name.setText(texas.GDVDATA_name);
        text_GEOLIST_name.setText(texas.GEOLIST_name);
        text_GEOPLOT_name.setText(texas.GEOPLOT_name);
        text_PARAMS_name.setText(texas.PARAMS_name);
        text_POSDAT_name.setText(texas.POSDAT_name);
        text_SIM_name.setText(texas.SIM_name);
        text_SIMDATA_name.setText(texas.SIMDATA_name);
        text_SIMERR_name.setText(texas.SIMERR_name);
        text_SIMPLST_name.setText(texas.SIMPLST_name);
        text_SIMSLST_name.setText(texas.SIMSLST_name);
        text_SIMSTAT_name.setText(texas.SIMSTAT_name);
        text_SPRDSHT_name.setText(texas.SPRDSHT_name);
        text_SSAM_name.setText(texas.SSAM_name);

        label_title = new JLabel("Edit File Name");
        label_title.setFont(font1);

        okButton = new JButton("   OK   ");
        applyButton = new JButton("  Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel panel_okButton;
        panel_okButton = new JPanel();
        panel_okButton.add(okButton);
        panel_okButton.add(applyButton);
        panel_okButton.add(cancelButton);

        JPanel panel_title;
        panel_title = new JPanel();
        panel_title.add(label_title);

        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        text_DISDAT_name.addKeyListener(helpListener);
        text_DISPAR_name.addKeyListener(helpListener);
        text_DVPLIST_name.addKeyListener(helpListener);
        text_EMERR_name.addKeyListener(helpListener);
        text_EMLIST_name.addKeyListener(helpListener);
        text_EMSTA_name.addKeyListener(helpListener);
        text_EMSTAFIN_name.addKeyListener(helpListener);
        text_EMSTAVE_name.addKeyListener(helpListener);
        text_FORT8_name.addKeyListener(helpListener);
        text_FORT9_name.addKeyListener(helpListener);
        text_GDV_name.addKeyListener(helpListener);
        text_GDVDATA_name.addKeyListener(helpListener);
        text_GEOLIST_name.addKeyListener(helpListener);
        text_GEOPLOT_name.addKeyListener(helpListener);
        text_PARAMS_name.addKeyListener(helpListener);
        text_POSDAT_name.addKeyListener(helpListener);
        text_SIM_name.addKeyListener(helpListener);
        text_SIMDATA_name.addKeyListener(helpListener);
        text_SIMERR_name.addKeyListener(helpListener);
        text_SIMPLST_name.addKeyListener(helpListener);
        text_SIMSLST_name.addKeyListener(helpListener);
        text_SIMSTAT_name.addKeyListener(helpListener);
        text_SPRDSHT_name.addKeyListener(helpListener);
        text_SSAM_name.addKeyListener(helpListener);

        text_DISDAT_name.getAccessibleContext().setAccessibleName(label_DISDAT_name_1.getText());
        text_DISPAR_name.getAccessibleContext().setAccessibleName(label_DISPAR_name_1.getText());
        text_DVPLIST_name.getAccessibleContext().setAccessibleName(label_DVPLIST_name_1.getText());
        text_EMERR_name.getAccessibleContext().setAccessibleName(label_EMERR_name_1.getText());
        text_EMLIST_name.getAccessibleContext().setAccessibleName(label_EMLIST_name_1.getText());
        text_EMSTA_name.getAccessibleContext().setAccessibleName(label_EMSTA_name_1.getText());
        text_EMSTAFIN_name.getAccessibleContext().setAccessibleName(label_EMSTAFIN_name_1.getText());
        text_EMSTAVE_name.getAccessibleContext().setAccessibleName(label_EMSTAVE_name_1.getText());
        text_FORT8_name.getAccessibleContext().setAccessibleName(label_FORT8_name_1.getText());
        text_FORT9_name.getAccessibleContext().setAccessibleName(label_FORT9_name_1.getText());
        text_GDV_name.getAccessibleContext().setAccessibleName(label_GDV_name_1.getText());
        text_GDVDATA_name.getAccessibleContext().setAccessibleName(label_GDVDATA_name_1.getText());
        text_GEOLIST_name.getAccessibleContext().setAccessibleName(label_GEOLIST_name_1.getText());
        text_GEOPLOT_name.getAccessibleContext().setAccessibleName(label_GEOPLOT_name_1.getText());
        text_PARAMS_name.getAccessibleContext().setAccessibleName(label_PARAMS_name_1.getText());
        text_POSDAT_name.getAccessibleContext().setAccessibleName(label_POSDAT_name_1.getText());
        text_SIM_name.getAccessibleContext().setAccessibleName(label_SIM_name_1.getText());
        text_SIMDATA_name.getAccessibleContext().setAccessibleName(label_SIMDATA_name_1.getText());
        text_SIMERR_name.getAccessibleContext().setAccessibleName(label_SIMERR_name_1.getText());
        text_SIMPLST_name.getAccessibleContext().setAccessibleName(label_SIMPLST_name_1.getText());
        text_SIMSLST_name.getAccessibleContext().setAccessibleName(label_SIMSLST_name_1.getText());
        text_SIMSTAT_name.getAccessibleContext().setAccessibleName(label_SIMSTAT_name_1.getText());
        text_SPRDSHT_name.getAccessibleContext().setAccessibleName(label_SPRDSHT_name_1.getText());
        text_SSAM_name.getAccessibleContext().setAccessibleName(label_SSAM_name_1.getText());

        text_DISDAT_name.getAccessibleContext().setAccessibleDescription(label_DISDAT_name_4.getText() + label_DISDAT_name_2.getText() + label_DISDAT_name_3.getText());
        text_DISPAR_name.getAccessibleContext().setAccessibleDescription(label_DISPAR_name_4.getText() + label_DISPAR_name_2.getText() + label_DISPAR_name_3.getText());
        text_DVPLIST_name.getAccessibleContext().setAccessibleDescription(label_DVPLIST_name_4.getText() + label_DVPLIST_name_2.getText() + label_DVPLIST_name_3.getText());
        text_EMERR_name.getAccessibleContext().setAccessibleDescription(label_EMERR_name_4.getText() + label_EMERR_name_2.getText() + label_EMERR_name_3.getText());
        text_EMLIST_name.getAccessibleContext().setAccessibleDescription(label_EMLIST_name_4.getText() + label_EMLIST_name_2.getText() + label_EMLIST_name_3.getText());
        text_EMSTA_name.getAccessibleContext().setAccessibleDescription(label_EMSTA_name_4.getText() + label_EMSTA_name_2.getText() + label_EMSTA_name_3.getText());
        text_EMSTAFIN_name.getAccessibleContext().setAccessibleDescription(label_EMSTAFIN_name_4.getText() + label_EMSTAFIN_name_2.getText() + label_EMSTAFIN_name_3.getText());
        text_EMSTAVE_name.getAccessibleContext().setAccessibleDescription(label_EMSTAVE_name_4.getText() + label_EMSTAVE_name_2.getText() + label_EMSTAVE_name_3.getText());
        text_FORT8_name.getAccessibleContext().setAccessibleDescription(label_FORT8_name_4.getText() + label_FORT8_name_2.getText() + label_FORT8_name_3.getText());
        text_FORT9_name.getAccessibleContext().setAccessibleDescription(label_FORT9_name_4.getText() + label_FORT9_name_2.getText() + label_FORT9_name_3.getText());
        text_GDV_name.getAccessibleContext().setAccessibleDescription(label_GDV_name_4.getText() + label_GDV_name_2.getText() + label_GDV_name_3.getText());
        text_GDVDATA_name.getAccessibleContext().setAccessibleDescription(label_GDVDATA_name_4.getText() + label_GDVDATA_name_2.getText() + label_GDVDATA_name_3.getText());
        text_GEOLIST_name.getAccessibleContext().setAccessibleDescription(label_GEOLIST_name_4.getText() + label_GEOLIST_name_2.getText() + label_GEOLIST_name_3.getText());
        text_GEOPLOT_name.getAccessibleContext().setAccessibleDescription(label_GEOPLOT_name_4.getText() + label_GEOPLOT_name_2.getText() + label_GEOPLOT_name_3.getText());
        text_PARAMS_name.getAccessibleContext().setAccessibleDescription(label_PARAMS_name_4.getText() + label_PARAMS_name_2.getText() + label_PARAMS_name_3.getText());
        text_POSDAT_name.getAccessibleContext().setAccessibleDescription(label_POSDAT_name_4.getText() + label_POSDAT_name_2.getText() + label_POSDAT_name_3.getText());
        text_SIM_name.getAccessibleContext().setAccessibleDescription(label_SIM_name_4.getText() + label_SIM_name_2.getText() + label_SIM_name_3.getText());
        text_SIMDATA_name.getAccessibleContext().setAccessibleDescription(label_SIMDATA_name_4.getText() + label_SIMDATA_name_2.getText() + label_SIMDATA_name_3.getText());
        text_SIMERR_name.getAccessibleContext().setAccessibleDescription(label_SIMERR_name_4.getText() + label_SIMERR_name_2.getText() + label_SIMERR_name_3.getText());
        text_SIMPLST_name.getAccessibleContext().setAccessibleDescription(label_SIMPLST_name_4.getText() + label_SIMPLST_name_2.getText() + label_SIMPLST_name_3.getText());
        text_SIMSLST_name.getAccessibleContext().setAccessibleDescription(label_SIMSLST_name_4.getText() + label_SIMSLST_name_2.getText() + label_SIMSLST_name_3.getText());
        text_SIMSTAT_name.getAccessibleContext().setAccessibleDescription(label_SIMSTAT_name_4.getText() + label_SIMSTAT_name_2.getText() + label_SIMSTAT_name_3.getText());
        text_SPRDSHT_name.getAccessibleContext().setAccessibleDescription(label_SPRDSHT_name_4.getText() + label_SPRDSHT_name_2.getText() + label_SPRDSHT_name_3.getText());
        text_SSAM_name.getAccessibleContext().setAccessibleDescription(label_SSAM_name_4.getText() + label_SSAM_name_2.getText() + label_SSAM_name_3.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");
        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        int iRow = 0;

        gbConstraints.insets = new Insets(1, 10, 10, 10);
        addComponent(panel_title, iRow++, 0, 7, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 5);
        addComponent(label_Heading_0, iRow, 1, 1, 1);
        addComponent(label_Heading_1, iRow, 2, 1, 1);
        addComponent(label_Heading_2, iRow, 3, 1, 1);
        addComponent(label_Heading_3, iRow, 4, 1, 1);
        addComponent(label_Heading_4, iRow++, 5, 2, 1);

        addComponent(label_num_1, iRow, 0, 1, 1);
        addComponent(text_DISDAT_name, iRow, 1, 1, 1);
        addComponent(label_DISDAT_name_1, iRow, 2, 1, 1);
        addComponent(label_DISDAT_name_2, iRow, 3, 1, 1);
        addComponent(label_DISDAT_name_3, iRow, 4, 1, 1);
        addComponent(label_DISDAT_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_2, iRow, 0, 1, 1);
        addComponent(text_DISPAR_name, iRow, 1, 1, 1);
        addComponent(label_DISPAR_name_1, iRow, 2, 1, 1);
        addComponent(label_DISPAR_name_2, iRow, 3, 1, 1);
        addComponent(label_DISPAR_name_3, iRow, 4, 1, 1);
        addComponent(label_DISPAR_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_3, iRow, 0, 1, 1);
        addComponent(text_DVPLIST_name, iRow, 1, 1, 1);
        addComponent(label_DVPLIST_name_1, iRow, 2, 1, 1);
        addComponent(label_DVPLIST_name_2, iRow, 3, 1, 1);
        addComponent(label_DVPLIST_name_3, iRow, 4, 1, 1);
        addComponent(label_DVPLIST_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_4, iRow, 0, 1, 1);
        addComponent(text_EMERR_name, iRow, 1, 1, 1);
        addComponent(label_EMERR_name_1, iRow, 2, 1, 1);
        addComponent(label_EMERR_name_2, iRow, 3, 1, 1);
        addComponent(label_EMERR_name_3, iRow, 4, 1, 1);
        addComponent(label_EMERR_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_5, iRow, 0, 1, 1);
        addComponent(text_EMLIST_name, iRow, 1, 1, 1);
        addComponent(label_EMLIST_name_1, iRow, 2, 1, 1);
        addComponent(label_EMLIST_name_2, iRow, 3, 1, 1);
        addComponent(label_EMLIST_name_3, iRow, 4, 1, 1);
        addComponent(label_EMLIST_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_6, iRow, 0, 1, 1);
        addComponent(text_EMSTA_name, iRow, 1, 1, 1);
        addComponent(label_EMSTA_name_1, iRow, 2, 1, 1);
        addComponent(label_EMSTA_name_2, iRow, 3, 1, 1);
        addComponent(label_EMSTA_name_3, iRow, 4, 1, 1);
        addComponent(label_EMSTA_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_7, iRow, 0, 1, 1);
        addComponent(text_EMSTAFIN_name, iRow, 1, 1, 1);
        addComponent(label_EMSTAFIN_name_1, iRow, 2, 1, 1);
        addComponent(label_EMSTAFIN_name_2, iRow, 3, 1, 1);
        addComponent(label_EMSTAFIN_name_3, iRow, 4, 1, 1);
        addComponent(label_EMSTAFIN_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_8, iRow, 0, 1, 1);
        addComponent(text_EMSTAVE_name, iRow, 1, 1, 1);
        addComponent(label_EMSTAVE_name_1, iRow, 2, 1, 1);
        addComponent(label_EMSTAVE_name_2, iRow, 3, 1, 1);
        addComponent(label_EMSTAVE_name_3, iRow, 4, 1, 1);
        addComponent(label_EMSTAVE_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_9, iRow, 0, 1, 1);
        addComponent(text_FORT8_name, iRow, 1, 1, 1);
        addComponent(label_FORT8_name_1, iRow, 2, 1, 1);
        addComponent(label_FORT8_name_2, iRow, 3, 1, 1);
        addComponent(label_FORT8_name_3, iRow, 4, 1, 1);
        addComponent(label_FORT8_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_10, iRow, 0, 1, 1);
        addComponent(text_FORT9_name, iRow, 1, 1, 1);
        addComponent(label_FORT9_name_1, iRow, 2, 1, 1);
        addComponent(label_FORT9_name_2, iRow, 3, 1, 1);
        addComponent(label_FORT9_name_3, iRow, 4, 1, 1);
        addComponent(label_FORT9_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_11, iRow, 0, 1, 1);
        addComponent(text_GDV_name, iRow, 1, 1, 1);
        addComponent(label_GDV_name_1, iRow, 2, 1, 1);
        addComponent(label_GDV_name_2, iRow, 3, 1, 1);
        addComponent(label_GDV_name_3, iRow, 4, 1, 1);
        addComponent(label_GDV_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_12, iRow, 0, 1, 1);
        addComponent(text_GDVDATA_name, iRow, 1, 1, 1);
        addComponent(label_GDVDATA_name_1, iRow, 2, 1, 1);
        addComponent(label_GDVDATA_name_2, iRow, 3, 1, 1);
        addComponent(label_GDVDATA_name_3, iRow, 4, 1, 1);
        addComponent(label_GDVDATA_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_13, iRow, 0, 1, 1);
        addComponent(text_GEOLIST_name, iRow, 1, 1, 1);
        addComponent(label_GEOLIST_name_1, iRow, 2, 1, 1);
        addComponent(label_GEOLIST_name_2, iRow, 3, 1, 1);
        addComponent(label_GEOLIST_name_3, iRow, 4, 1, 1);
        addComponent(label_GEOLIST_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_14, iRow, 0, 1, 1);
        addComponent(text_GEOPLOT_name, iRow, 1, 1, 1);
        addComponent(label_GEOPLOT_name_1, iRow, 2, 1, 1);
        addComponent(label_GEOPLOT_name_2, iRow, 3, 1, 1);
        addComponent(label_GEOPLOT_name_3, iRow, 4, 1, 1);
        addComponent(label_GEOPLOT_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_15, iRow, 0, 1, 1);
        addComponent(text_PARAMS_name, iRow, 1, 1, 1);
        addComponent(label_PARAMS_name_1, iRow, 2, 1, 1);
        addComponent(label_PARAMS_name_2, iRow, 3, 1, 1);
        addComponent(label_PARAMS_name_3, iRow, 4, 1, 1);
        addComponent(label_PARAMS_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_16, iRow, 0, 1, 1);
        addComponent(text_POSDAT_name, iRow, 1, 1, 1);
        addComponent(label_POSDAT_name_1, iRow, 2, 1, 1);
        addComponent(label_POSDAT_name_2, iRow, 3, 1, 1);
        addComponent(label_POSDAT_name_3, iRow, 4, 1, 1);
        addComponent(label_POSDAT_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_17, iRow, 0, 1, 1);
        addComponent(text_SIM_name, iRow, 1, 1, 1);
        addComponent(label_SIM_name_1, iRow, 2, 1, 1);
        addComponent(label_SIM_name_2, iRow, 3, 1, 1);
        addComponent(label_SIM_name_3, iRow, 4, 1, 1);
        addComponent(label_SIM_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_18, iRow, 0, 1, 1);
        addComponent(text_SIMDATA_name, iRow, 1, 1, 1);
        addComponent(label_SIMDATA_name_1, iRow, 2, 1, 1);
        addComponent(label_SIMDATA_name_2, iRow, 3, 1, 1);
        addComponent(label_SIMDATA_name_3, iRow, 4, 1, 1);
        addComponent(label_SIMDATA_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_19, iRow, 0, 1, 1);
        addComponent(text_SIMERR_name, iRow, 1, 1, 1);
        addComponent(label_SIMERR_name_1, iRow, 2, 1, 1);
        addComponent(label_SIMERR_name_2, iRow, 3, 1, 1);
        addComponent(label_SIMERR_name_3, iRow, 4, 1, 1);
        addComponent(label_SIMERR_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_20, iRow, 0, 1, 1);
        addComponent(text_SIMPLST_name, iRow, 1, 1, 1);
        addComponent(label_SIMPLST_name_1, iRow, 2, 1, 1);
        addComponent(label_SIMPLST_name_2, iRow, 3, 1, 1);
        addComponent(label_SIMPLST_name_3, iRow, 4, 1, 1);
        addComponent(label_SIMPLST_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_21, iRow, 0, 1, 1);
        addComponent(text_SIMSLST_name, iRow, 1, 1, 1);
        addComponent(label_SIMSLST_name_1, iRow, 2, 1, 1);
        addComponent(label_SIMSLST_name_2, iRow, 3, 1, 1);
        addComponent(label_SIMSLST_name_3, iRow, 4, 1, 1);
        addComponent(label_SIMSLST_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_22, iRow, 0, 1, 1);
        addComponent(text_SIMSTAT_name, iRow, 1, 1, 1);
        addComponent(label_SIMSTAT_name_1, iRow, 2, 1, 1);
        addComponent(label_SIMSTAT_name_2, iRow, 3, 1, 1);
        addComponent(label_SIMSTAT_name_3, iRow, 4, 1, 1);
        addComponent(label_SIMSTAT_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_23, iRow, 0, 1, 1);
        addComponent(text_SPRDSHT_name, iRow, 1, 1, 1);
        addComponent(label_SPRDSHT_name_1, iRow, 2, 1, 1);
        addComponent(label_SPRDSHT_name_2, iRow, 3, 1, 1);
        addComponent(label_SPRDSHT_name_3, iRow, 4, 1, 1);
        addComponent(label_SPRDSHT_name_4, iRow++, 5, 2, 1);

        addComponent(label_num_24, iRow, 0, 1, 1);
        addComponent(text_SSAM_name, iRow, 1, 1, 1);
        addComponent(label_SSAM_name_1, iRow, 2, 1, 1);
        addComponent(label_SSAM_name_2, iRow, 3, 1, 1);
        addComponent(label_SSAM_name_3, iRow, 4, 1, 1);
        addComponent(label_SSAM_name_4, iRow++, 5, 2, 1);

        gbConstraints.insets = new Insets(10, 10, 1, 10);
        addComponent(panel_okButton, iRow++, 0, 7, 1);

        aFrame.setSize(950, 700);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    } // end of method EditFileNameDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == text_DISDAT_name) {
                    new HelpDialog(true, label_DISDAT_name_1.getText(), label_DISDAT_name_4.getText() + label_DISDAT_name_2.getText() + label_DISDAT_name_3.getText(), " ", text_DISDAT_name.getText()
                            .trim(), texas.DISDAT_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_DISPAR_name) {
                    new HelpDialog(true, label_DISPAR_name_1.getText(), label_DISPAR_name_4.getText() + label_DISPAR_name_2.getText() + label_DISPAR_name_3.getText(), " ", text_DISPAR_name.getText()
                            .trim(), texas.DISPAR_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_DVPLIST_name) {
                    new HelpDialog(true, label_DVPLIST_name_1.getText(), label_DVPLIST_name_4.getText() + label_DVPLIST_name_2.getText() + label_DVPLIST_name_3.getText(), " ", text_DVPLIST_name
                            .getText().trim(), texas.DVPLIST_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_EMERR_name) {
                    new HelpDialog(true, label_EMERR_name_1.getText(), label_EMERR_name_4.getText() + label_EMERR_name_2.getText() + label_EMERR_name_3.getText(), " ", text_EMERR_name.getText()
                            .trim(), texas.EMERR_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_EMLIST_name) {
                    new HelpDialog(true, label_EMLIST_name_1.getText(), label_EMLIST_name_4.getText() + label_EMLIST_name_2.getText() + label_EMLIST_name_3.getText(), " ", text_EMLIST_name.getText()
                            .trim(), texas.EMLIST_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_EMSTA_name) {
                    new HelpDialog(true, label_EMSTA_name_1.getText(), label_EMSTA_name_4.getText() + label_EMSTA_name_2.getText() + label_EMSTA_name_3.getText(), " ", text_EMSTA_name.getText()
                            .trim(), texas.EMSTA_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_EMSTAFIN_name) {
                    new HelpDialog(true, label_EMSTAFIN_name_1.getText(), label_EMSTAFIN_name_4.getText() + label_EMSTAFIN_name_2.getText() + label_EMSTAFIN_name_3.getText(), " ", text_EMSTAFIN_name
                            .getText().trim(), texas.EMSTAFIN_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_EMSTAVE_name) {
                    new HelpDialog(true, label_EMSTAVE_name_1.getText(), label_EMSTAVE_name_4.getText() + label_EMSTAVE_name_2.getText() + label_EMSTAVE_name_3.getText(), " ", text_EMSTAVE_name
                            .getText().trim(), texas.EMSTAVE_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_FORT8_name) {
                    new HelpDialog(true, label_FORT8_name_1.getText(), label_FORT8_name_4.getText() + label_FORT8_name_2.getText() + label_FORT8_name_3.getText(), " ", text_FORT8_name.getText()
                            .trim(), texas.FORT8_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_FORT9_name) {
                    new HelpDialog(true, label_FORT9_name_1.getText(), label_FORT9_name_4.getText() + label_FORT9_name_2.getText() + label_FORT9_name_3.getText(), " ", text_FORT9_name.getText()
                            .trim(), texas.FORT9_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_GDV_name) {
                    new HelpDialog(true, label_GDV_name_1.getText(), label_GDV_name_4.getText() + label_GDV_name_2.getText() + label_GDV_name_3.getText(), " ", text_GDV_name.getText().trim(),
                            texas.GDV_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_GDVDATA_name) {
                    new HelpDialog(true, label_GDVDATA_name_1.getText(), label_GDVDATA_name_4.getText() + label_GDVDATA_name_2.getText() + label_GDVDATA_name_3.getText(), " ", text_GDVDATA_name
                            .getText().trim(), texas.GDVDATA_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_GEOLIST_name) {
                    new HelpDialog(true, label_GEOLIST_name_1.getText(), label_GEOLIST_name_4.getText() + label_GEOLIST_name_2.getText() + label_GEOLIST_name_3.getText(), " ", text_GEOLIST_name
                            .getText().trim(), texas.GEOLIST_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_GEOPLOT_name) {
                    new HelpDialog(true, label_GEOPLOT_name_1.getText(), label_GEOPLOT_name_4.getText() + label_GEOPLOT_name_2.getText() + label_GEOPLOT_name_3.getText(), " ", text_GEOPLOT_name
                            .getText().trim(), texas.GEOPLOT_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_PARAMS_name) {
                    new HelpDialog(true, label_PARAMS_name_1.getText(), label_PARAMS_name_4.getText() + label_PARAMS_name_2.getText() + label_PARAMS_name_3.getText(), " ", text_PARAMS_name.getText()
                            .trim(), texas.PARAMS_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_POSDAT_name) {
                    new HelpDialog(true, label_POSDAT_name_1.getText(), label_POSDAT_name_4.getText() + label_POSDAT_name_2.getText() + label_POSDAT_name_3.getText(), " ", text_POSDAT_name.getText()
                            .trim(), texas.POSDAT_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SIM_name) {
                    new HelpDialog(true, label_SIM_name_1.getText(), label_SIM_name_4.getText() + label_SIM_name_2.getText() + label_SIM_name_3.getText(), " ", text_SIM_name.getText().trim(),
                            texas.SIM_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SIMDATA_name) {
                    new HelpDialog(true, label_SIMDATA_name_1.getText(), label_SIMDATA_name_4.getText() + label_SIMDATA_name_2.getText() + label_SIMDATA_name_3.getText(), " ", text_SIMDATA_name
                            .getText().trim(), texas.SIMDATA_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SIMERR_name) {
                    new HelpDialog(true, label_SIMERR_name_1.getText(), label_SIMERR_name_4.getText() + label_SIMERR_name_2.getText() + label_SIMERR_name_3.getText(), " ", text_SIMERR_name.getText()
                            .trim(), texas.SIMERR_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SIMPLST_name) {
                    new HelpDialog(true, label_SIMPLST_name_1.getText(), label_SIMPLST_name_4.getText() + label_SIMPLST_name_2.getText() + label_SIMPLST_name_3.getText(), " ", text_SIMPLST_name
                            .getText().trim(), texas.SIMPLST_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SIMSLST_name) {
                    new HelpDialog(true, label_SIMSLST_name_1.getText(), label_SIMSLST_name_4.getText() + label_SIMSLST_name_2.getText() + label_SIMSLST_name_3.getText(), " ", text_SIMSLST_name
                            .getText().trim(), texas.SIMSLST_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SIMSTAT_name) {
                    new HelpDialog(true, label_SIMSTAT_name_1.getText(), label_SIMSTAT_name_4.getText() + label_SIMSTAT_name_2.getText() + label_SIMSTAT_name_3.getText(), " ", text_SIMSTAT_name
                            .getText().trim(), texas.SIMSTAT_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SPRDSHT_name) {
                    new HelpDialog(true, label_SPRDSHT_name_1.getText(), label_SPRDSHT_name_4.getText() + label_SPRDSHT_name_2.getText() + label_SPRDSHT_name_3.getText(), " ", text_SPRDSHT_name
                            .getText().trim(), texas.SPRDSHT_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == text_SSAM_name) {
                    new HelpDialog(true, label_SSAM_name_1.getText(), label_SSAM_name_4.getText() + label_SSAM_name_2.getText() + label_SSAM_name_3.getText(), " ", text_SSAM_name.getText().trim(),
                            texas.SSAM_name, " ", " ", " ", " ");
                }
                else if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == applyButton) {
                    new HelpDialog(true, "Apply button", "The Apply button saves the data but does not close the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        } // end of keyPressed
    } // end of HelpListener

    void saveData() {
        texas.DISDAT_name = text_DISDAT_name.getText().trim();
        texas.DISPAR_name = text_DISPAR_name.getText().trim();
        texas.DVPLIST_name = text_DVPLIST_name.getText().trim();
        texas.EMERR_name = text_EMERR_name.getText().trim();
        texas.EMLIST_name = text_EMLIST_name.getText().trim();
        texas.EMSTA_name = text_EMSTA_name.getText().trim();
        texas.EMSTAFIN_name = text_EMSTAFIN_name.getText().trim();
        texas.EMSTAVE_name = text_EMSTAVE_name.getText().trim();
        texas.FORT8_name = text_FORT8_name.getText().trim();
        texas.FORT9_name = text_FORT9_name.getText().trim();
        texas.GDV_name = text_GDV_name.getText().trim();
        texas.GDVDATA_name = text_GDVDATA_name.getText().trim();
        texas.GEOLIST_name = text_GEOLIST_name.getText().trim();
        texas.GEOPLOT_name = text_GEOPLOT_name.getText().trim();
        texas.PARAMS_name = text_PARAMS_name.getText().trim();
        texas.POSDAT_name = text_POSDAT_name.getText().trim();
        texas.SIM_name = text_SIM_name.getText().trim();
        texas.SIMDATA_name = text_SIMDATA_name.getText().trim();
        texas.SIMERR_name = text_SIMERR_name.getText().trim();
        texas.SIMPLST_name = text_SIMPLST_name.getText().trim();
        texas.SIMSLST_name = text_SIMSLST_name.getText().trim();
        texas.SIMSTAT_name = text_SIMSTAT_name.getText().trim();
        texas.SPRDSHT_name = text_SPRDSHT_name.getText().trim();
        texas.SSAM_name = text_SSAM_name.getText().trim();
    } // end of saveData

    boolean isError() {
        if (text_DISDAT_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 1 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_DISPAR_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 2 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_DVPLIST_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 3 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_EMERR_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 4 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_EMLIST_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 5 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_EMSTA_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 6 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_EMSTAFIN_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 7 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_EMSTAVE_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 8 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_FORT8_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 9 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_FORT9_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 10 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_GDV_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 11 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_GDVDATA_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 12 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_GEOLIST_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 13 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_GEOPLOT_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 14 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_PARAMS_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 15 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_POSDAT_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 16 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SIM_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 17 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SIMDATA_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 18 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SIMERR_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 19 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SIMPLST_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 20 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SIMSLST_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 21 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SIMSTAT_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 22 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SPRDSHT_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 23 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (text_SSAM_name.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "File Name 24 is empty.  You must enter a name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_DISDAT_name.getText().trim().equals(text_DISPAR_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_DVPLIST_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_EMERR_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_EMLIST_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_EMSTA_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_EMSTAFIN_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_EMSTAVE_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_FORT8_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_FORT9_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_GDV_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_GDVDATA_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_GEOLIST_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_DISDAT_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_DISDAT_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 1 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_DISPAR_name.getText().trim().equals(text_DVPLIST_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_EMERR_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_EMLIST_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_EMSTA_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_EMSTAFIN_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_EMSTAVE_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_FORT8_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_FORT9_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_GDV_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_GDVDATA_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_GEOLIST_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_GEOPLOT_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_DISPAR_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_DISPAR_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 2 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_DVPLIST_name.getText().trim().equals(text_EMERR_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_EMLIST_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_EMSTA_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_EMSTAFIN_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_EMSTAVE_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_FORT8_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_FORT9_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_GDV_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_GDVDATA_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_GEOLIST_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_DVPLIST_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_DVPLIST_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 3 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_EMERR_name.getText().trim().equals(text_EMLIST_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_EMSTA_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_EMSTAFIN_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_EMSTAVE_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_FORT8_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_FORT9_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_GDV_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_GDVDATA_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_GEOLIST_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_GEOPLOT_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_EMERR_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_EMERR_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 4 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_EMLIST_name.getText().trim().equals(text_EMSTA_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_EMSTAFIN_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_EMSTAVE_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_FORT8_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_FORT9_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_GDV_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_GDVDATA_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_GEOLIST_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_EMLIST_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_EMLIST_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 5 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_EMSTA_name.getText().trim().equals(text_EMSTAFIN_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_EMSTAVE_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_FORT8_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_FORT9_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_GDV_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_GDVDATA_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_GEOLIST_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_GEOPLOT_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_EMSTA_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_EMSTA_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 6 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_EMSTAFIN_name.getText().trim().equals(text_EMSTAVE_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_FORT8_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_FORT9_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_GDV_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_GDVDATA_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_GEOLIST_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_EMSTAFIN_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_EMSTAFIN_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 7 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_EMSTAVE_name.getText().trim().equals(text_FORT8_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_FORT9_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_GDV_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_GDVDATA_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_GEOLIST_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_GEOPLOT_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_EMSTAVE_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_EMSTAVE_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 8 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_FORT8_name.getText().trim().equals(text_FORT9_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_GDV_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_GDVDATA_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_GEOLIST_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_FORT8_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_FORT8_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 9 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_FORT9_name.getText().trim().equals(text_GDV_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_GDVDATA_name.getText().trim()))
                || (text_FORT9_name.getText().trim().equals(text_GEOLIST_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_GEOPLOT_name.getText().trim()))
                || (text_FORT9_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_FORT9_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_FORT9_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_FORT9_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_FORT9_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_FORT9_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 10 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_GDV_name.getText().trim().equals(text_GDVDATA_name.getText().trim())) || (text_GDV_name.getText().trim().equals(text_GEOLIST_name.getText().trim()))
                || (text_GDV_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_GDV_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_GDV_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_GDV_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_GDV_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_GDV_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_GDV_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_GDV_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_GDV_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_GDV_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_GDV_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 11 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_GDVDATA_name.getText().trim().equals(text_GEOLIST_name.getText().trim())) || (text_GDVDATA_name.getText().trim().equals(text_GEOPLOT_name.getText().trim()))
                || (text_GDVDATA_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_GDVDATA_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_GDVDATA_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_GDVDATA_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_GDVDATA_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_GDVDATA_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_GDVDATA_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_GDVDATA_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_GDVDATA_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_GDVDATA_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 12 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_GEOLIST_name.getText().trim().equals(text_GEOPLOT_name.getText().trim())) || (text_GEOLIST_name.getText().trim().equals(text_PARAMS_name.getText().trim()))
                || (text_GEOLIST_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_GEOLIST_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_GEOLIST_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_GEOLIST_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_GEOLIST_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_GEOLIST_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_GEOLIST_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_GEOLIST_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_GEOLIST_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 13 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_GEOPLOT_name.getText().trim().equals(text_PARAMS_name.getText().trim())) || (text_GEOPLOT_name.getText().trim().equals(text_POSDAT_name.getText().trim()))
                || (text_GEOPLOT_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_GEOPLOT_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_GEOPLOT_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_GEOPLOT_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_GEOPLOT_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_GEOPLOT_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_GEOPLOT_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_GEOPLOT_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 14 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_PARAMS_name.getText().trim().equals(text_POSDAT_name.getText().trim())) || (text_PARAMS_name.getText().trim().equals(text_SIM_name.getText().trim()))
                || (text_PARAMS_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_PARAMS_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_PARAMS_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_PARAMS_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_PARAMS_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_PARAMS_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_PARAMS_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 15 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_POSDAT_name.getText().trim().equals(text_SIM_name.getText().trim())) || (text_POSDAT_name.getText().trim().equals(text_SIMDATA_name.getText().trim()))
                || (text_POSDAT_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_POSDAT_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_POSDAT_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_POSDAT_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_POSDAT_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_POSDAT_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 16 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SIM_name.getText().trim().equals(text_SIMDATA_name.getText().trim())) || (text_SIM_name.getText().trim().equals(text_SIMERR_name.getText().trim()))
                || (text_SIM_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_SIM_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_SIM_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_SIM_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_SIM_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 17 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SIMDATA_name.getText().trim().equals(text_SIMERR_name.getText().trim())) || (text_SIMDATA_name.getText().trim().equals(text_SIMPLST_name.getText().trim()))
                || (text_SIMDATA_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_SIMDATA_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_SIMDATA_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_SIMDATA_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 18 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SIMERR_name.getText().trim().equals(text_SIMPLST_name.getText().trim())) || (text_SIMERR_name.getText().trim().equals(text_SIMSLST_name.getText().trim()))
                || (text_SIMERR_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_SIMERR_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_SIMERR_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 19 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SIMPLST_name.getText().trim().equals(text_SIMSLST_name.getText().trim())) || (text_SIMPLST_name.getText().trim().equals(text_SIMSTAT_name.getText().trim()))
                || (text_SIMPLST_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_SIMPLST_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 20 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SIMSLST_name.getText().trim().equals(text_SIMSTAT_name.getText().trim())) || (text_SIMSLST_name.getText().trim().equals(text_SPRDSHT_name.getText().trim()))
                || (text_SIMSLST_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 21 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SIMSTAT_name.getText().trim().equals(text_SPRDSHT_name.getText().trim())) || (text_SIMSTAT_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 22 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((text_SPRDSHT_name.getText().trim().equals(text_SSAM_name.getText().trim()))) {
            JOptionPane.showMessageDialog(null, "File Name 23 must not match any other File Name.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        return false;
    } // end of isError

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if ((event.getSource() == okButton) || (event.getSource() == applyButton)) {
                if (!isError()) {
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            } // end of if((event.getSource() == okButton) || (event.getSource() == applyButton))
            if (event.getSource() == cancelButton) {
                aFrame.dispose();
            } // end of if(event.getSource() == cancelButton)
        } // end of method actionPerformed
    } // end of class OkApplyActionListener

    class OkApplyKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if ((event.getSource() == okButton) || (event.getSource() == applyButton)) {
                    if (!isError()) {
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                } // end of if((event.getSource() == okButton) || (event.getSource() ==
                  // applyButton))
                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                } // end of if(event.getSource() == cancelButton)
            }
        } // end of method keyPressed
    }// end of OkApplyKeyListener

} // end of class EditFileNameDialog
