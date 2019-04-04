package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              PlotDialog.java                               */
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

class PlotDialog extends JDialog {

    int NUMOFFIELD = 8;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    ComboBoxActionListener comboBoxActionListener;

    ComboBoxKeyListener comboBoxKeyListener;

    JLabel label_title;

    JLabel[] label_plot = new JLabel[NUMOFFIELD + 1];

    JComboBox[] comboBox_plot = new JComboBox[NUMOFFIELD + 1];

    JButton okButton, applyButton, cancelButton;

    JPanel panel_title, ok_panel;

    Font font1, font2;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public PlotDialog() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS];

        titleString = lclv_tx_fmt.mstv_name;

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        double ldfv_min;
        double ldfv_max;
        double ldfv_inc;

        int SizeOfArray, ArrayIndex;
        int intArrayElementValue;
        double doubleArrayElementValue;
        String s;

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
        gbConstraints.fill = GridBagConstraints.BOTH;

        font1 = new Font("TimesRoman", Font.BOLD, 20);
        font2 = new Font("TimesRoman", Font.BOLD, 16);

        String[] array_path_type = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE].substring(1).split("\\|");

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH];
        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        String[] array_max_radius_path = new String[SizeOfArray];
        intArrayElementValue = lsiv_min;
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_max_radius_path[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS];
        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        String[] array_min_dist_paths = new String[SizeOfArray];
        intArrayElementValue = lsiv_min;
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_min_dist_paths[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        String[] array_plot_option = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION].substring(1).split("\\|");
        String[] array_plot_type = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE].substring(1).split("\\|");

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        String[] array_plot_scale_appr = new String[SizeOfArray];
        intArrayElementValue = lsiv_min;
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_plot_scale_appr[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        String[] array_plot_scale_intr = new String[SizeOfArray];
        intArrayElementValue = lsiv_min;
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_plot_scale_intr[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        ldfv_min = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];
        ldfv_max = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];
        ldfv_inc = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];
        SizeOfArray = (int)((ldfv_max - ldfv_min) / ldfv_inc) + 1;
        doubleArrayElementValue = ldfv_min;
        String[] array_plot_paper_wdth = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_plot_paper_wdth[ArrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += ldfv_inc;
        }

        comboBox_plot[1] = new JComboBox(array_path_type);
        comboBox_plot[2] = new JComboBox(array_max_radius_path);
        comboBox_plot[3] = new JComboBox(array_min_dist_paths);
        comboBox_plot[4] = new JComboBox(array_plot_option);
        comboBox_plot[5] = new JComboBox(array_plot_type);
        comboBox_plot[6] = new JComboBox(array_plot_scale_appr);
        comboBox_plot[7] = new JComboBox(array_plot_scale_intr);
        comboBox_plot[8] = new JComboBox(array_plot_paper_wdth);

        comboBox_plot[1].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE]);
        comboBox_plot[2].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]));
        comboBox_plot[3].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]));
        comboBox_plot[4].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION]);
        comboBox_plot[5].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE]);
        comboBox_plot[6].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]));
        comboBox_plot[7].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]));
        comboBox_plot[8].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]));

        label_plot[1] = new JLabel("1. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE]);
        label_plot[2] = new JLabel("2. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]);
        label_plot[3] = new JLabel("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]);
        label_plot[4] = new JLabel("4. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION]);
        label_plot[5] = new JLabel("5. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE]);
        label_plot[6] = new JLabel("6. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]);
        label_plot[7] = new JLabel("7. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]);
        label_plot[8] = new JLabel("8. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]);

        comboBox_plot[1].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE]);
        comboBox_plot[1].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE]);
        comboBox_plot[2].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]);
        comboBox_plot[2].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]);
        comboBox_plot[3].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]);
        comboBox_plot[3].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]);
        comboBox_plot[4].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION]);
        comboBox_plot[4].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION]);
        comboBox_plot[5].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE]);
        comboBox_plot[5].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE]);
        comboBox_plot[6].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]);
        comboBox_plot[6].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]);
        comboBox_plot[7].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]);
        comboBox_plot[7].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]);
        comboBox_plot[8].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]);
        comboBox_plot[8].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]);

        openComboMenuListener = new OpenComboMenuListener();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            comboBox_plot[lsiv_field].addKeyListener(openComboMenuListener);
        }

        if (gdvsim.flag_plotOpt_ok) {
            comboBox_plot[1].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_path_type);
            comboBox_plot[2].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_max_radius_path));
            comboBox_plot[3].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_min_dist_paths));
            comboBox_plot[4].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option);
            comboBox_plot[5].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type);
            comboBox_plot[6].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr));
            comboBox_plot[7].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr));
            comboBox_plot[8].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth));

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option.equals("NOPLOT")) {
                comboBox_plot[5].setEnabled(false);
                comboBox_plot[6].setEnabled(false);
                comboBox_plot[7].setEnabled(false);
                comboBox_plot[8].setEnabled(false);
            }
        }

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        panel_title = new JPanel();
        panel_title.add(label_title);

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        comboBoxActionListener = new ComboBoxActionListener();
        comboBoxKeyListener = new ComboBoxKeyListener();

        comboBox_plot[4].addActionListener(comboBoxActionListener);
        comboBox_plot[4].addKeyListener(comboBoxKeyListener);

        helpListener = new HelpListener();
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            comboBox_plot[lsiv_field].addKeyListener(helpListener);
        }

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okApplyActionListener = new OkApplyActionListener();
        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okApplyKeyListener = new OkApplyKeyListener();
        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        int iRow = 0;
        gbConstraints.insets = new Insets(2, 5, 2, 5);

        addComponent(panel_title, iRow++, 0, 2, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(comboBox_plot[lsiv_field], iRow, 0, 1, 1);
            addComponent(label_plot[lsiv_field], iRow++, 1, 1, 1);
        }

        addComponent(ok_panel, iRow, 0, 2, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method DriveVehicleByVehicleClassDialog

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
                        if (cb.getSelectedIndex() == cb.getItemCount() - 1) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() + 1);
                        }
                    }
                }
                else {
                    if (event.getModifiers() != InputEvent.ALT_MASK) {
                        if (cb.getSelectedIndex() == cb.getItemCount() - 1) {
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
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS];

                if (event.getSource() == comboBox_plot[1]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE], comboBox_plot[1]
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_plot[2]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH],
                            comboBox_plot[2].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]));
                }
                else if (event.getSource() == comboBox_plot[3]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS],
                            comboBox_plot[3].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS]));
                }
                else if (event.getSource() == comboBox_plot[4]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION], comboBox_plot[4]
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_plot[5]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE], comboBox_plot[5]
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_plot[6]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR],
                            comboBox_plot[6].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]));
                }
                else if (event.getSource() == comboBox_plot[7]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR],
                            comboBox_plot[7].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]));
                }
                else if (event.getSource() == comboBox_plot[8]) {
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH],
                            comboBox_plot[8].getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]));
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
            }
        }
    } // end of class HelpListener

    class ComboBoxActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (comboBox_plot[4].getSelectedItem().toString().equals("NOPLOT")) {
                comboBox_plot[5].setEnabled(false);
                comboBox_plot[6].setEnabled(false);
                comboBox_plot[7].setEnabled(false);
                comboBox_plot[8].setEnabled(false);
            }
            else {
                comboBox_plot[5].setEnabled(true);
                comboBox_plot[6].setEnabled(true);
                comboBox_plot[7].setEnabled(true);
                comboBox_plot[8].setEnabled(true);
            }
        }
    }

    class ComboBoxKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (comboBox_plot[4].getSelectedItem().toString().equals("NOPLOT")) {
                    comboBox_plot[5].setEnabled(false);
                    comboBox_plot[6].setEnabled(false);
                    comboBox_plot[7].setEnabled(false);
                    comboBox_plot[8].setEnabled(false);
                }
                else {
                    comboBox_plot[5].setEnabled(true);
                    comboBox_plot[6].setEnabled(true);
                    comboBox_plot[7].setEnabled(true);
                    comboBox_plot[8].setEnabled(true);
                }
            }
        }
    }

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_path_type = comboBox_plot[1].getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_max_radius_path = Integer.valueOf(comboBox_plot[2].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_min_dist_paths = Integer.valueOf(comboBox_plot[3].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option = comboBox_plot[4].getSelectedItem().toString();

        if (comboBox_plot[4].isEnabled()) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type = comboBox_plot[5].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr = Integer.valueOf(comboBox_plot[6].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr = Integer.valueOf(comboBox_plot[7].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth = Double.valueOf(comboBox_plot[8].getSelectedItem().toString()).doubleValue();
        }
        else {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE - 1
                    + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_plotOpt_ok = true;
                saveData();

                if (event.getSource() == okButton) {
                    aFrame.dispose();
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
                    gdvsim.flag_plotOpt_ok = true;
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of keyPressed
    } // end of OkApplyKeyListener

} // end of class PlotDialog
