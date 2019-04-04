package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                        DiamondFreeUturnDialog.java                         */
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

import org.etexascode.texas.gdvsim.HelpDialog;
import org.etexascode.texas.gdvsim.PARAMS;
import org.etexascode.texas.gdvsim.TX_Fmt;
import org.etexascode.texas.gdvsim.gdvsim;
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

public class DiamondFreeUturnDialog extends JDialog {

    // ROW 1 - free_uturn_lane_width,
    // ROW 2 - free_uturn_space_between,
    // ROW 3 - free_uturn_entr_length,
    // ROW 4 - free_uturn_entr_radius,
    // ROW 5 - free_uturn_exit_length,
    // ROW 6 - free_uturn_exit_radius,
    // ROW 7 - free_uturn_percent_using,
    // ROW 8 - free_uturn_allow_veh_type,
    // COLUMN 1 - leg3_to_leg4,
    // COLUMN 2 - leg6_to_leg1;

    static final int FIELD_LANE_WIDTH = 1;

    static final int FIELD_SPACE_BTWN = 2;

    static final int FIELD_ENTR_LNGTH = 3;

    static final int FIELD_ENTR_RADUS = 4;

    static final int FIELD_EXIT_LNGTH = 5;

    static final int FIELD_EXIT_RADUS = 6;

    static final int FIELD_PERC_USING = 7;

    static final int FIELD_ALLOWED_VT = 8;

    static final int NUMOFFIELD = 8;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JPanel ok_panel, panel_title;

    JButton okButton, applyButton, cancelButton;

    Font font;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    CboActionListener cboActionListener;

    CboKeyListener cboKeyListener;

    JLabel label_title;

    JLabel[] label_field = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_uturn = new JLabel[PARAMS.TEXAS_MODEL_NFU + 1];

    JLabel[] label_note = new JLabel[6];

    JComboBox[][] comboBox_free_uturn = new JComboBox[NUMOFFIELD + 1][PARAMS.TEXAS_MODEL_NFU + 1];

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public DiamondFreeUturnDialog() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN];
        titleString = lclv_tx_fmt.mstv_name.substring(0, 31);

        aFrame = new JFrame(titleString);
        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        container.setLayout(gbLayout);
        gbConstraints = new GridBagConstraints();

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        int SizeOfArray;
        int intArrayElementValue;

        font = new Font("TimesRoman", Font.BOLD, 18);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field] = new JLabel(lsiv_field + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]);
        }

        label_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4] = new JLabel("   Leg 3 to Leg 4   ");
        label_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1] = new JLabel("   Leg 6 to Leg 1   ");

        label_note[0] = new JLabel("NOTE:");
        label_note[1] = new JLabel("Item #" + FIELD_ALLOWED_VT + " Vehicle Type 'B' = Bicycles (Vehicle Class attribute Classification is 'BC' or 'BC-1')");
        label_note[2] = new JLabel("Item #" + FIELD_ALLOWED_VT + " Vehicle Type 'E' = Emergency Vehicles (Special Vehicle attribute Emergency Vehicle is 'YES')");
        label_note[3] = new JLabel("Item #" + FIELD_ALLOWED_VT + " Vehicle Type 'R' = Rail Vehicles (Vehicle Class attribute Classification is 'RAIL')");
        label_note[4] = new JLabel("Item #" + FIELD_ALLOWED_VT + " Vehicle Type 'V' = all other Vehicles except Bicycles, Emergency Vehicles, and Rail Vehicles");
        label_note[5] = new JLabel("END NOTE");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                if (lsiv_field == FIELD_LANE_WIDTH) {
                    lsiv_min = 8;
                    lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field];
                    lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field];

                    SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                    intArrayElementValue = lsiv_min;
                    String[] array_free_uturn = new String[SizeOfArray + 1];

                    array_free_uturn[0] = "0";

                    for (int ArrayIndex = 1; ArrayIndex <= SizeOfArray; ArrayIndex++) {
                        array_free_uturn[ArrayIndex] = Integer.toString(intArrayElementValue);
                        intArrayElementValue += lsiv_inc;
                    }

                    comboBox_free_uturn[lsiv_field][lsiv_uturn] = new JComboBox(array_free_uturn);
                }
                else if (lsiv_field == FIELD_ALLOWED_VT) {
                    String[] array_allowvt = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE].substring(1).split("\\|");

                    comboBox_free_uturn[lsiv_field][lsiv_uturn] = new JComboBox(array_allowvt);
                }
                else {
                    lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field];
                    lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field];
                    lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field];

                    SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                    intArrayElementValue = lsiv_min;
                    String[] array_free_uturn = new String[SizeOfArray];

                    for (int ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                        array_free_uturn[ArrayIndex] = Integer.toString(intArrayElementValue);
                        intArrayElementValue += lsiv_inc;
                    }

                    comboBox_free_uturn[lsiv_field][lsiv_uturn] = new JComboBox(array_free_uturn);
                }
            }
        }

        if (gdvsim.gclv_inter.mbov_Free_UTurns_OK) {
            for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_LANE_WIDTH][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_lane_width));
                }
                else {
                    comboBox_free_uturn[FIELD_LANE_WIDTH][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_SPACE_BTWN][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_space_between));
                }
                else {
                    comboBox_free_uturn[FIELD_SPACE_BTWN][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_ENTR_LNGTH][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_entr_length));
                }
                else {
                    comboBox_free_uturn[FIELD_ENTR_LNGTH][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_ENTR_RADUS][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_entr_radius));
                }
                else {
                    comboBox_free_uturn[FIELD_ENTR_RADUS][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_EXIT_LNGTH][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_exit_length));
                }
                else {
                    comboBox_free_uturn[FIELD_EXIT_LNGTH][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_EXIT_RADUS][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_exit_radius));
                }
                else {
                    comboBox_free_uturn[FIELD_EXIT_RADUS][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_PERC_USING][lsiv_uturn].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_percent_using));
                }
                else {
                    comboBox_free_uturn[FIELD_PERC_USING][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_free_uturn[FIELD_ALLOWED_VT][lsiv_uturn].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mstv_allowed_veh_type);
                }
                else {
                    comboBox_free_uturn[FIELD_ALLOWED_VT][lsiv_uturn].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE]);
                }
            }
        }
        else {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_ALLOWED_VT) {
                    for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                        comboBox_free_uturn[lsiv_field][lsiv_uturn].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]);
                    }
                }
                else {
                    for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                        comboBox_free_uturn[lsiv_field][lsiv_uturn].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]));
                    }
                }
            }
        } // end of if(gdvsim.gclv_inter.mbov_Free_UTurns_OK)

        if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].getSelectedItem().toString()).intValue() == 0) {
            for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].setEnabled(false);
            }
        }
        else {
            for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].setEnabled(true);
            }
        }

        if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].getSelectedItem().toString()).intValue() == 0) {
            for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].setEnabled(false);
            }
        }
        else {
            for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].setEnabled(true);
            }
        }

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();
        cboActionListener = new CboActionListener();
        cboKeyListener = new CboKeyListener();

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        okButton.addKeyListener(openComboMenuListener);
        applyButton.addKeyListener(openComboMenuListener);
        cancelButton.addKeyListener(openComboMenuListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                comboBox_free_uturn[lsiv_field][lsiv_uturn].addKeyListener(openComboMenuListener);
                comboBox_free_uturn[lsiv_field][lsiv_uturn].addKeyListener(helpListener);

                if (lsiv_field == FIELD_LANE_WIDTH) {
                    comboBox_free_uturn[lsiv_field][lsiv_uturn].addActionListener(cboActionListener);
                    comboBox_free_uturn[lsiv_field][lsiv_uturn].addKeyListener(cboKeyListener);
                }
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                String s;

                if (lsiv_uturn == gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4) {
                    s = " for leg3 to leg4";
                }
                else {
                    s = " for leg6 to leg1";
                }

                comboBox_free_uturn[lsiv_field][lsiv_uturn].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field] + s);
                comboBox_free_uturn[lsiv_field][lsiv_uturn].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field] + s);
            }
        }

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");
        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        int iRow = 0;

        gbConstraints.fill = GridBagConstraints.BOTH;

        gbConstraints.insets = new Insets(0, 0, 10, 0);

        addComponent(panel_title, iRow++, 0, 3, 1);

        gbConstraints.insets = new Insets(2, 5, 2, 5);

        for (int lsiv_label = 0; lsiv_label < label_note.length; lsiv_label++) {
            addComponent(label_note[lsiv_label], iRow++, 0, 3, 1);
        }

        addComponent(label_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4], iRow, 1, 1, 1);
        addComponent(label_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1], iRow++, 2, 1, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 0, 1, 1);
            addComponent(comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4], iRow, 1, 1, 1);
            addComponent(comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1], iRow, 2, 1, 1);
            iRow++;
        }

        addComponent(ok_panel, iRow, 0, 3, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    } // end of method DiamondFreeUturnDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class OpenComboMenuListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            JComboBox cb = (JComboBox)event.getSource();

            if (event.getKeyCode() == KeyEvent.VK_UP) {
                if (cb.isPopupVisible()) {}
                else {
                    if (event.getModifiers() != InputEvent.ALT_MASK) {
                        if (cb.getSelectedIndex() == 0) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() - 1);
                        }
                    }
                    else {
                        cb.setSelectedIndex(cb.getSelectedIndex());
                    }
                }
            }

            if (event.getKeyCode() == KeyEvent.VK_DOWN) {
                event.consume();
                if (cb.isPopupVisible()) {
                    if (event.getModifiers() == InputEvent.ALT_MASK) {
                        cb.hidePopup();
                    }
                    else {
                        if (cb.getSelectedIndex() == (cb.getItemCount() - 1)) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() + 1);
                        }
                    }
                }
                else {
                    if (event.getModifiers() != InputEvent.ALT_MASK) {
                        if (cb.getSelectedIndex() == (cb.getItemCount() - 1)) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() + 1);
                        }
                    }
                    else {
                        cb.showPopup();
                    }
                }
            }
        }
    } // end of OpenComboMenuListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == applyButton) {
                    new HelpDialog(true, "Apply button", "The Apply button saves the data but does not close the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                        if (event.getSource() == comboBox_free_uturn[lsiv_field][lsiv_uturn]) {
                            String s;
                            s = lclv_tx_fmt.mstv_name;

                            if (lsiv_uturn == gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4) {
                                s = s.replaceFirst("#", Integer.toString(gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4));
                                s = s.replaceFirst("#", Integer.toString(3));
                                s = s.replaceFirst("#", Integer.toString(4));
                            }
                            else {
                                s = s.replaceFirst("#", Integer.toString(gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1));
                                s = s.replaceFirst("#", Integer.toString(6));
                                s = s.replaceFirst("#", Integer.toString(1));
                            }

                            if (lsiv_field == FIELD_ALLOWED_VT) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE], s,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE],
                                        comboBox_free_uturn[lsiv_field][lsiv_uturn].getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE],
                                        lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE], " ", " ", " ");
                            }
                            else {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field], s,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field],
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field], comboBox_free_uturn[lsiv_field][lsiv_uturn].getSelectedItem()
                                                .toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1 + lsiv_field]));
                            }
                        }
                    }
                }
            }
        }
    }

    void saveData() {
        int lsiv_status;

        if ((Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].getSelectedItem().toString()).intValue() > 0)
                || (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].getSelectedItem().toString()).intValue() > 0)) {
            gdvsim.gclv_inter.mbov_free_uturns_defined = true;
        }
        else {
            gdvsim.gclv_inter.mbov_free_uturns_defined = false;
        }

        gdvsim.gclv_inter.mbov_Free_UTurns_OK = true;

        for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
            if (comboBox_free_uturn[FIELD_LANE_WIDTH][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_lane_width = Integer.valueOf(
                        comboBox_free_uturn[FIELD_LANE_WIDTH][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_lane_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH];
            }

            if (comboBox_free_uturn[FIELD_SPACE_BTWN][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_space_between = Integer.valueOf(
                        comboBox_free_uturn[FIELD_SPACE_BTWN][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_space_between = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN];
            }

            if (comboBox_free_uturn[FIELD_ENTR_LNGTH][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_entr_length = Integer.valueOf(
                        comboBox_free_uturn[FIELD_ENTR_LNGTH][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_entr_length = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH];
            }

            if (comboBox_free_uturn[FIELD_ENTR_RADUS][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_entr_radius = Integer.valueOf(
                        comboBox_free_uturn[FIELD_ENTR_RADUS][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_entr_radius = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS];
            }

            if (comboBox_free_uturn[FIELD_EXIT_LNGTH][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_exit_length = Integer.valueOf(
                        comboBox_free_uturn[FIELD_EXIT_LNGTH][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_exit_length = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH];
            }

            if (comboBox_free_uturn[FIELD_EXIT_RADUS][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_exit_radius = Integer.valueOf(
                        comboBox_free_uturn[FIELD_EXIT_RADUS][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_exit_radius = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS];
            }

            if (comboBox_free_uturn[FIELD_PERC_USING][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_percent_using = Integer.valueOf(
                        comboBox_free_uturn[FIELD_PERC_USING][lsiv_uturn].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].msiv_percent_using = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING];
            }

            if (comboBox_free_uturn[FIELD_ALLOWED_VT][lsiv_uturn].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mstv_allowed_veh_type = comboBox_free_uturn[FIELD_ALLOWED_VT][lsiv_uturn].getSelectedItem()
                        .toString();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE];
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_uturn = 1; lsiv_uturn <= PARAMS.TEXAS_MODEL_NFU; lsiv_uturn++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_uturn].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].getSelectedItem().toString()).intValue() > 0) {
            lsiv_status = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }
        else {
            lsiv_status = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        int lsiv_inb_leg = 3; // fut=1
        int lsiv_out_leg = 4;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_entr_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_exit_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mstv_allowed_veh_type;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mstv_allowed_veh_type;

        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = lsiv_status;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NGI; lsiv_phase++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                    + lsiv_phase] = lsiv_status;
        }

        if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].getSelectedItem().toString()).intValue() > 0) {
            lsiv_status = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }
        else {
            lsiv_status = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        lsiv_inb_leg = 6; // fut=2
        lsiv_out_leg = 1;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_entr_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_exit_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mstv_allowed_veh_type;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mstv_allowed_veh_type;

        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = lsiv_status;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = lsiv_status;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NGI; lsiv_phase++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                    + lsiv_phase] = lsiv_status;
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    }

    class CboActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4]) {
                if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].getSelectedItem().toString()).intValue() == 0) {
                    for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].setEnabled(false);
                    }
                }
                else {
                    for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].setEnabled(true);
                    }
                }
            }
            else if (event.getSource() == comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1]) {
                if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].getSelectedItem().toString()).intValue() == 0) {
                    for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].setEnabled(false);
                    }
                }
                else {
                    for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].setEnabled(true);
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class CboActionListener

    class CboKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4]) {
                    if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].getSelectedItem().toString()).intValue() == 0) {
                        for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].setEnabled(true);
                        }
                    }
                }
                else if (event.getSource() == comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1]) {
                    if (Integer.valueOf(comboBox_free_uturn[FIELD_LANE_WIDTH][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].getSelectedItem().toString()).intValue() == 0) {
                        for (int lsiv_field = FIELD_SPACE_BTWN; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            comboBox_free_uturn[lsiv_field][gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].setEnabled(true);
                        }
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class CboKeyListener

    boolean isError() {
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                    || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                if (gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                            + " Inbound Lane" + (gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
                if (gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
                            + " Outbound Lane" + (gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
                if (gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                            + " Inbound Lane" + (gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
                if (gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
                            + " Outbound Lane" + (gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        }
        return false;
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }

            if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

        } // end of method actionPerformed
    } // end of class OkApplyActionListener

    class OkApplyKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of method keyPressed
    }// end of OkApplyButtonKeyListener

} // end of class DiamondFreeUturnDialog
