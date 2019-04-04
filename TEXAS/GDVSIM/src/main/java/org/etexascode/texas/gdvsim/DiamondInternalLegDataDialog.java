package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                     DiamondInternalLegDataDialog.java                      */
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

class DiamondInternalLegDataDialog extends JDialog {

    int NUMOFFIELD = 6;

    // Row 1 diamond_leg_dist_between,
    // Row 2 diamond_leg_lanes_inb_R,
    // Row 3 diamond_leg_lanes_inb_L,
    // Row 4 diamond_leg_speed_inb_R,
    // Row 5 diamond_leg_speed_inb_L,
    // Row 6 diamond_leg_median_width;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JPanel ok_panel, panel_title;

    JButton okButton, applyButton, cancelButton;

    Font font1, font2;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    LaneButtonActionListener laneButtonActionListener;

    LaneButtonKeyListener laneButtonKeyListener;

    JLabel label_title;

    JLabel[] label_field = new JLabel[NUMOFFIELD + 1];

    JComboBox[] comboBox_legData = new JComboBox[NUMOFFIELD + 1];

    JButton laneButton;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public DiamondInternalLegDataDialog() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG];
        titleString = lclv_tx_fmt.mstv_name;

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

        font1 = new Font("TimesRoman", Font.BOLD, 18);
        font2 = new Font("TimesRoman", Font.BOLD, 16);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            int SizeOfArray, intArrayElementValue;

            int lsiv_min;
            int lsiv_max;
            int lsiv_inc;

            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field];

            SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            intArrayElementValue = lsiv_min;
            String[] array_leg = new String[SizeOfArray];

            for (int ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                array_leg[ArrayIndex] = Integer.toString(intArrayElementValue);
                intArrayElementValue += lsiv_inc;
            }

            label_field[lsiv_field] = new JLabel(lsiv_field + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]);
            comboBox_legData[lsiv_field] = new JComboBox(array_leg);
        }

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        panel_title = new JPanel();
        panel_title.add(label_title);

        if (gdvsim.gclv_inter.mbov_Diamond_Leg_OK) {
            comboBox_legData[1].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between));
            comboBox_legData[2].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right));
            comboBox_legData[3].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left));
            comboBox_legData[4].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_right));
            comboBox_legData[5].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_left));
            comboBox_legData[6].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width));
        }
        else {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_legData[lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]));
            }
        }

        okButton = new JButton("   OK   ");
        applyButton = new JButton(" Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        laneButton = new JButton("Diamond Interchange Internal Lane Data");
        laneButton.setMnemonic(KeyEvent.VK_I);

        laneButtonActionListener = new LaneButtonActionListener();
        laneButtonKeyListener = new LaneButtonKeyListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();
        helpListener = new HelpListener();
        openComboMenuListener = new OpenComboMenuListener();

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        laneButton.addActionListener(laneButtonActionListener);
        laneButton.addKeyListener(laneButtonKeyListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);
        laneButton.addKeyListener(helpListener);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            comboBox_legData[lsiv_field].addKeyListener(openComboMenuListener);
            comboBox_legData[lsiv_field].addKeyListener(helpListener);
        }

        if (gdvsim.gclv_inter.mbov_Diamond_Leg_OK) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_legData[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]);
                comboBox_legData[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]);
            }
        }

        laneButton.getAccessibleContext().setAccessibleName("internal lane data");
        laneButton.getAccessibleContext().setAccessibleDescription("internal lane data");

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        int iRow = 0;

        gbConstraints.fill = GridBagConstraints.BOTH;

        gbConstraints.insets = new Insets(0, 0, 10, 0);
        addComponent(panel_title, iRow++, 0, 4, 1);

        gbConstraints.insets = new Insets(2, 5, 2, 5);
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(comboBox_legData[lsiv_field], iRow, 0, 1, 1);
            addComponent(label_field[lsiv_field], iRow++, 1, 3, 1);
        }

        addComponent(laneButton, iRow++, 0, 2, 1);
        addComponent(ok_panel, iRow, 0, 4, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method DiamondInternalLegDataDialog

    class LaneButtonActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between != Integer.valueOf(comboBox_legData[1].getSelectedItem().toString()).intValue()) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "The Distance Between Right Intersection and Left Intersection(ft) has been modified.\nPlease OK or Apply the Diamond Interchange Internal Leg Data before accessing the Diamond Interchange Internal Lane Data.\nPlease also OK or Apply the Diamond Interchange Internal Lane Data items #4 & #5 Length of Usable Lane for each lane.",
                                "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
            else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right != Integer.valueOf(comboBox_legData[2].getSelectedItem().toString())
                    .intValue()) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "The Number of Lanes Inbound to Right Intersection has been modified.\nPlease OK or Apply the Diamond Interchange Internal Leg Data before accessing the Diamond Interchange Internal Lane Data.",
                                "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
            else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left != Integer.valueOf(comboBox_legData[3].getSelectedItem().toString())
                    .intValue()) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "The Number of Lanes Inbound to Left Intersection has been modified.\nPlease OK or Apply the Diamond Interchange Internal Leg Data before accessing the Diamond Interchange Internal Lane Data.",
                                "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
            else {
                new DiamondInternalLaneDataDialog();
            }
        }
    }

    class LaneButtonKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between != Integer.valueOf(comboBox_legData[1].getSelectedItem().toString()).intValue()) {
                    JOptionPane
                            .showMessageDialog(
                                    null,
                                    "The Distance Between Right Intersection and Left Intersection(ft) has been modified.\nPlease OK or Apply the Diamond Interchange Internal Leg Data before accessing the Diamond Interchange Internal Lane Data.\nPlease also OK or Apply the Diamond Interchange Internal Lane Data items #4 & #5 Length of Usable Lane for each lane.",
                                    "Warning Message", JOptionPane.WARNING_MESSAGE);
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right != Integer.valueOf(comboBox_legData[2].getSelectedItem().toString())
                        .intValue()) {
                    JOptionPane
                            .showMessageDialog(
                                    null,
                                    "The Number of Lanes Inbound to Right Intersection has been modified.\nPlease OK or Apply the Diamond Interchange Internal Leg Data before accessing the Diamond Interchange Internal Lane Data.",
                                    "Warning Message", JOptionPane.WARNING_MESSAGE);
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left != Integer.valueOf(comboBox_legData[3].getSelectedItem().toString())
                        .intValue()) {
                    JOptionPane
                            .showMessageDialog(
                                    null,
                                    "The Number of Lanes Inbound to Left Intersection has been modified.\nPlease OK or Apply the Diamond Interchange Internal Leg Data before accessing the Diamond Interchange Internal Lane Data.",
                                    "Warning Message", JOptionPane.WARNING_MESSAGE);
                }
                else {
                    new DiamondInternalLaneDataDialog();
                }
            }
        }
    }

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
                else if (event.getSource() == laneButton) {
                    new HelpDialog(true, "Internal Lane Data", "Internal Lane Data opens Diamond Interchange Internal Lane Data Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == comboBox_legData[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field], comboBox_legData[lsiv_field].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN - 1 + lsiv_field]));
                    }
                }
            }
        }
    }

    void saveData() {
        TX_Fmt lclv_tx_fmt;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_int_leg_number = 0;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between = Integer.valueOf(comboBox_legData[1].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right = Integer.valueOf(comboBox_legData[2].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left = Integer.valueOf(comboBox_legData[3].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_right = Integer.valueOf(comboBox_legData[4].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_left = Integer.valueOf(comboBox_legData[5].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width = Integer.valueOf(comboBox_legData[6].getSelectedItem().toString()).intValue();

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_INT_LEG_NUM] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;

        int lsiv_leg_cl = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        int lsiv_leg_cr = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    }

    void invalidInboundLaneForRightIntersection() {
        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of invalidInboundLaneForRightIntersection()

    void invalidInboundLaneForLeftIntersection() {
        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of invalidInboundLaneForLeftIntersection()

    void invalidLaneUsable() {
        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between != Integer.valueOf(comboBox_legData[1].getSelectedItem().toString()).intValue()) {
                    gdvsim.gclv_inter.mbov_Diamond_Lane_OK = false;
                    invalidLaneUsable();
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right != Integer.valueOf(comboBox_legData[2].getSelectedItem().toString())
                        .intValue()) {
                    gdvsim.gclv_inter.mbov_Diamond_Lane_OK = false;
                    invalidInboundLaneForRightIntersection();
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left != Integer.valueOf(comboBox_legData[3].getSelectedItem().toString())
                        .intValue()) {
                    gdvsim.gclv_inter.mbov_Diamond_Lane_OK = false;
                    invalidInboundLaneForLeftIntersection();
                }

                gdvsim.gclv_inter.mbov_Diamond_Leg_OK = true;
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
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between != Integer.valueOf(comboBox_legData[1].getSelectedItem().toString()).intValue()) {
                        gdvsim.gclv_inter.mbov_Diamond_Lane_OK = false;
                        invalidLaneUsable();
                    }
                    else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right != Integer.valueOf(comboBox_legData[2].getSelectedItem().toString())
                            .intValue()) {
                        gdvsim.gclv_inter.mbov_Diamond_Lane_OK = false;
                        invalidInboundLaneForRightIntersection();
                    }
                    else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left != Integer.valueOf(comboBox_legData[3].getSelectedItem().toString())
                            .intValue()) {
                        gdvsim.gclv_inter.mbov_Diamond_Lane_OK = false;
                        invalidInboundLaneForLeftIntersection();
                    }

                    gdvsim.gclv_inter.mbov_Diamond_Leg_OK = true;
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of method keyPressed
    }// end of OKApplyKeyListener

} // end of class DiamondInternalLegDataDialog
