package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                          NemaMovementDialog.java                           */
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

class NemaMovementDialog extends JDialog {

    ComboBoxActionListener comboBoxActionListener;

    ComboBoxKeyListener comboBoxKeyListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title;

    JLabel[] label_note = new JLabel[11];

    JLabel[] label_movement = new JLabel[PARAMS.TEXAS_MODEL_NMP + 1];

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JComboBox[][] comboBox_movement = new JComboBox[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NMP + 1];

    JButton okButton, applyButton, cancelButton;

    JPanel panel_title;

    Font font;

    TX_Fmt lclv_tx_fmt;

    int number_of_phases;

    String titleString;

    public NemaMovementDialog() {
        int lsiv_max_phases;

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
        }
        else {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
        }

        number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        if (number_of_phases < 2)
            number_of_phases = 2;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT];
        titleString = lclv_tx_fmt.mstv_name;

        aFrame = new JFrame(titleString);

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

        label_title = new JLabel(titleString);
        font = new Font("TimesRoman", Font.BOLD, 18);
        label_title.setFont(font);

        panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            label_movement[lsiv_mvt] = new JLabel("Movement " + lsiv_mvt);
        }

        for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("Phase " + lsiv_phase);
        }

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        int count;
        int int_number;

        for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];

                count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                int_number = lsiv_min;
                String[] array_movement = new String[count + 1];
                for (int lsiv_i = 0; lsiv_i <= count; lsiv_i++) {
                    if (lsiv_i == 0) {
                        array_movement[lsiv_i] = Integer.toString(0);
                    }
                    else {
                        array_movement[lsiv_i] = Integer.toString(int_number);
                        int_number += lsiv_inc;
                    }
                }
                comboBox_movement[lsiv_phase][lsiv_mvt] = new JComboBox(array_movement);

                if (lsiv_mvt == 1) {
                    comboBox_movement[lsiv_phase][lsiv_mvt].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1)
                            * PARAMS.TEXAS_MODEL_NMP]));
                }
                else {
                    comboBox_movement[lsiv_phase][lsiv_mvt].setSelectedItem("0");
                }

            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            comboBox_movement[lsiv_phase][1].setEnabled(true);
            comboBox_movement[lsiv_phase][2].setEnabled(true);

            for (int lsiv_mvt = 3; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                comboBox_movement[lsiv_phase][lsiv_mvt].setEnabled(false);
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (gdvsim.movementStat[lsiv_phase][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_movement[lsiv_phase][lsiv_mvt].setSelectedItem(Integer.toString(gdvsim.movementValue[lsiv_phase][lsiv_mvt]));
                    comboBox_movement[lsiv_phase][lsiv_mvt].setEnabled(true);
                }
                else {
                    comboBox_movement[lsiv_phase][lsiv_mvt].setEnabled(true);
                    break;
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                comboBox_movement[lsiv_phase][lsiv_mvt].getAccessibleContext().setAccessibleName("movement for phase " + lsiv_phase + " for movement " + lsiv_mvt);
                comboBox_movement[lsiv_phase][lsiv_mvt].getAccessibleContext().setAccessibleDescription("movement for phase " + lsiv_phase + " for movement " + lsiv_mvt);
            }
        }

        JPanel panel_message = new JPanel();
        label_note[1] = new JLabel("NOTE:");
        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            label_note[2] = new JLabel("NEMA Movement 1 is Eastbound  Left Turn.");
            label_note[3] = new JLabel("NEMA Movement 2 is Westbound  Straight. ");
            label_note[4] = new JLabel("NEMA Movement 3 is Northbound Left Turn.");
            label_note[5] = new JLabel("NEMA Movement 4 is Southbound Straight. ");
            label_note[6] = new JLabel("NEMA Movement 5 is Westbound  Left Turn.");
            label_note[7] = new JLabel("NEMA Movement 6 is Eastbound  Straight. ");
            label_note[8] = new JLabel("NEMA Movement 7 is Southbound Left Turn.");
            label_note[9] = new JLabel("NEMA Movement 8 is Northbound Straight. ");
        }
        else {
            label_note[2] = new JLabel("NEMA/HARDWARE Movement 1 is Eastbound  Left Turn.");
            label_note[3] = new JLabel("NEMA/HARDWARE Movement 2 is Westbound  Straight. ");
            label_note[4] = new JLabel("NEMA/HARDWARE Movement 3 is Northbound Left Turn.");
            label_note[5] = new JLabel("NEMA/HARDWARE Movement 4 is Southbound Straight. ");
            label_note[6] = new JLabel("NEMA/HARDWARE Movement 5 is Westbound  Left Turn.");
            label_note[7] = new JLabel("NEMA/HARDWARE Movement 6 is Eastbound  Straight. ");
            label_note[8] = new JLabel("NEMA/HARDWARE Movement 7 is Southbound Left Turn.");
            label_note[9] = new JLabel("NEMA/HARDWARE Movement 8 is Northbound Straight. ");
        }
        label_note[10] = new JLabel("END NOTE");

        panel_message = new JPanel();

        GridBagLayout gbLayoutPanelMessage = new GridBagLayout();
        GridBagConstraints gbConstraintsPanelMessage;

        panel_message.setLayout(gbLayoutPanelMessage);
        gbConstraintsPanelMessage = new GridBagConstraints();

        for (int lsiv_i = 1; lsiv_i <= 10; lsiv_i++) {
            gbConstraintsPanelMessage.fill = GridBagConstraints.BOTH;
            gbConstraintsPanelMessage.insets = new Insets(1, 1, 1, 1);
            gbConstraintsPanelMessage.gridx = 1;
            gbConstraintsPanelMessage.gridy = lsiv_i;
            gbConstraintsPanelMessage.gridwidth = 1;
            gbConstraintsPanelMessage.gridheight = 1;
            gbLayoutPanelMessage.setConstraints(label_note[lsiv_i], gbConstraintsPanelMessage);
            panel_message.add(label_note[lsiv_i]);
        }

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
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

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        comboBoxActionListener = new ComboBoxActionListener();
        comboBoxKeyListener = new ComboBoxKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                comboBox_movement[lsiv_phase][lsiv_mvt].addActionListener(comboBoxActionListener);
                comboBox_movement[lsiv_phase][lsiv_mvt].addKeyListener(comboBoxKeyListener);
                comboBox_movement[lsiv_phase][lsiv_mvt].addKeyListener(openComboMenuListener);
                comboBox_movement[lsiv_phase][lsiv_mvt].addKeyListener(helpListener);
            }
        }
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        gbConstraints.insets = new Insets(2, 5, 2, 5);

        int iRow = 0;

        addComponent(panel_title, iRow++, 0, 10, 1);
        addComponent(panel_message, iRow++, 0, 10, 1);

        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            addComponent(label_movement[lsiv_mvt], iRow, lsiv_mvt + 1, 1, 1);
        }

        iRow++;

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            addComponent(label_phase[lsiv_phase], iRow, 0, 1, 1);

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                addComponent(comboBox_movement[lsiv_phase][lsiv_mvt], iRow, lsiv_mvt + 1, 1, 1);
            }
            iRow++;
        }

        addComponent(ok_panel, iRow, 0, 10, 1);

        aFrame.setSize(950, 650);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method NemaMovement

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class ComboBoxActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int lsiv_max_phases;

            if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
            }
            else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
            }
            else {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
            }

            for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
                for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                    if (event.getSource() == comboBox_movement[lsiv_phase][lsiv_mvt]) {
                        functionComboBox(lsiv_phase, lsiv_mvt);
                    }
                }
            }
        }
    } // end of ComboBoxActionListener

    void functionComboBox(int lsiv_phase, int lsiv_mvt) {
        int lsiv_max_phases;

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
        }
        else {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
        }

        for (int lsiv_phase1 = 1; lsiv_phase1 <= lsiv_max_phases; lsiv_phase1++) {
            for (int lsiv_mvt1 = 1; lsiv_mvt1 <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt1++) {
                comboBox_movement[lsiv_phase1][lsiv_mvt1].removeActionListener(comboBoxActionListener);
                comboBox_movement[lsiv_phase1][lsiv_mvt1].removeKeyListener(comboBoxKeyListener);
            }
        }

        if (lsiv_mvt != PARAMS.TEXAS_MODEL_NMP) {
            if (Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString()).intValue() == 0) {
                if (!(lsiv_mvt == 1 && Integer.valueOf(comboBox_movement[lsiv_phase][2].getSelectedItem().toString()).intValue() == 0)) {
                    int lsiv_i;

                    for (lsiv_i = lsiv_mvt; lsiv_i < PARAMS.TEXAS_MODEL_NMP; lsiv_i++) {
                        if (comboBox_movement[lsiv_phase][lsiv_i + 1].isEnabled()) {
                            comboBox_movement[lsiv_phase][lsiv_i].setSelectedItem(comboBox_movement[lsiv_phase][lsiv_i + 1].getSelectedItem().toString());
                        }
                        else {
                            break;
                        }
                    }

                    if (lsiv_mvt < PARAMS.TEXAS_MODEL_NMP) {
                        comboBox_movement[lsiv_phase][lsiv_i].setEnabled(false);
                    }
                }
            }
            else {
                if (!comboBox_movement[lsiv_phase][lsiv_mvt + 1].isEnabled()) {
                    comboBox_movement[lsiv_phase][lsiv_mvt + 1].setEnabled(true);
                    comboBox_movement[lsiv_phase][lsiv_mvt + 1].setSelectedItem("0");
                }
            }
        }

        for (int lsiv_phase1 = 1; lsiv_phase1 <= lsiv_max_phases; lsiv_phase1++) {
            for (int lsiv_mvt1 = 1; lsiv_mvt1 <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt1++) {
                comboBox_movement[lsiv_phase1][lsiv_mvt1].addActionListener(comboBoxActionListener);
                comboBox_movement[lsiv_phase1][lsiv_mvt1].addKeyListener(comboBoxKeyListener);
            }
        }

    } // end of functionComboBox

    class ComboBoxKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            int lsiv_max_phases;

            if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
            }
            else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
            }
            else {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
            }

            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
                    for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                        if (event.getSource() == comboBox_movement[lsiv_phase][lsiv_mvt]) {
                            functionComboBox(lsiv_phase, lsiv_mvt);
                        }
                    }
                }
            }
        }
    }

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
            int lsiv_max_phases;

            if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
            }
            else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
            }
            else {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
            }

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

                for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
                    for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                        if (event.getSource() == comboBox_movement[lsiv_phase][lsiv_mvt]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP], lclv_tx_fmt.mstv_name,
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP],
                                    comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1
                                            + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP]),
                                    " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1
                                            - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1
                                            + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1
                                            + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP]));
                        }
                    }
                }
            }
        }
    } // end of HelpListener()

    void saveData() {
        int lsiv_max_phases;

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
        }
        else {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            int numOfMovments = 0;

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (comboBox_movement[lsiv_phase][lsiv_mvt].isEnabled() && Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString()).intValue() != 0) {
                    numOfMovments++;
                }
            }

            gdvsim.numOfMovementValue[lsiv_phase] = numOfMovments;
            gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (comboBox_movement[lsiv_phase][lsiv_mvt].isEnabled() && Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString()).intValue() != 0) {
                    gdvsim.movementValue[lsiv_phase][lsiv_mvt] = Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString()).intValue();
                    gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
        }

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            JOptionPane.showMessageDialog(null, "The NEMA movement data has beed saved in a temporary location.\nPlease OK or Apply the NEMA Signal Controller Timing Data to save this data.",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            JOptionPane.showMessageDialog(null,
                    "The NEMA/HARDWARE movement data has beed saved in a temporary location.\nPlease OK or Apply the Hardware-in-the-Loop Signal Controller Timing Data to save this data.",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else {
            JOptionPane.showMessageDialog(null, "Error in intersection control.", "Error Message", JOptionPane.ERROR_MESSAGE);
        }
    } // end of saveData

    boolean isError() {
        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            if (Integer.valueOf(comboBox_movement[lsiv_phase][1].getSelectedItem().toString()).intValue() == 0) {
                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " movement 1 can not be 0.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        if (!gdvsim.gclv_inter.mbov_is_diamond_interchange && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 4) {
            for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                int numOfMovments = 0;

                for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                    if (comboBox_movement[lsiv_phase][lsiv_mvt].isEnabled() && (Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString()).intValue() > 0)) {
                        numOfMovments++;
                    }
                } // end of for(int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++)

                // NEMA/HARDWARE Movement Numbering Convention
                //
                // Leg 1
                //
                // 4 7 : :
                // | | : :
                // | | : :
                // | \ : :
                // ...... -----2
                // ...... /----5 Leg 2
                // Leg 4 1----/ ......
                // 6----- ......
                // : : \ |
                // : : | |
                // : : | |
                // : : 3 8
                //
                // Leg 3
                //
                //
                // Leg 6 Leg 1
                //
                // 2 2 : :
                // | | : :
                // | | : :
                // | \ : :
                // ...... -----4.......... -----6
                // ...... /----3.......... /----6 Leg 2
                // Leg 5 1----/ ..........5----/ ......
                // 1----- ..........8----- ......
                // : : \ |
                // : : | |
                // : : | |
                // : : 7 7
                //
                // Leg 4 Leg 3

                for (int lsiv_mvt_i = 1; lsiv_mvt_i <= (numOfMovments - 1); lsiv_mvt_i++) {
                    int lsiv_value_i = Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt_i].getSelectedItem().toString()).intValue();

                    for (int lsiv_mvt_j = (lsiv_mvt_i + 1); lsiv_mvt_j <= numOfMovments; lsiv_mvt_j++) {
                        int lsiv_value_j = Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt_j].getSelectedItem().toString()).intValue();

                        if ((lsiv_value_i == 1) || (lsiv_value_i == 2) || (lsiv_value_i == 5) || (lsiv_value_i == 6)) {
                            if ((lsiv_value_j == 3) || (lsiv_value_j == 4) || (lsiv_value_j == 7) || (lsiv_value_j == 8)) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Movement " + lsiv_mvt_i + " = " + lsiv_value_i + " conflicts with Movement " + lsiv_mvt_j + " = "
                                        + lsiv_value_j + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        else {
                            if ((lsiv_value_j == 1) || (lsiv_value_j == 2) || (lsiv_value_j == 5) || (lsiv_value_j == 6)) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Movement " + lsiv_mvt_i + " = " + lsiv_value_i + " conflicts with Movement " + lsiv_mvt_j + " = "
                                        + lsiv_value_j + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                    }
                } // end of for(int lsiv_mvt_i = 1; lsiv_mvt_i <= (numOfMovments-1); lsiv_mvt_i++)

                for (int lsiv_mvt = 1; lsiv_mvt <= numOfMovments; lsiv_mvt++) {
                    int mv = Integer.valueOf(comboBox_movement[lsiv_phase][lsiv_mvt].getSelectedItem().toString()).intValue();
                    switch (mv) {
                        case 1:
                            if (gdvsim.gclv_inter.mcla_leg[4].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") == -1) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Movement " + lsiv_mvt + " = " + mv
                                        + " is wrong because the left-most inbound lane for Leg 4 didn't include an 'L' .", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            break;

                        case 3:
                            if (gdvsim.gclv_inter.mcla_leg[3].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") == -1) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Movement " + lsiv_mvt + " = " + mv
                                        + " is wrong because the left-most inbound lane for Leg 3 didn't include an 'L' .", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            break;

                        case 5:
                            if (gdvsim.gclv_inter.mcla_leg[2].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") == -1) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Movement " + lsiv_mvt + " = " + mv
                                        + " is wrong because the left-most inbound lane for Leg 2 didn't include an 'L' .", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            break;

                        case 7:
                            if (gdvsim.gclv_inter.mcla_leg[1].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") == -1) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Movement " + lsiv_mvt + " = " + mv
                                        + " is wrong because the left-most inbound lane for Leg 1 didn't include an 'L' .", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            break;
                    }
                } // end of for(int lsiv_mvt = 1; lsiv_mvt <= numOfMovments; lsiv_mvt++)
            } // end of for(int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++)
        } // end of if (!gdvsim.gclv_inter.mbov_is_diamond_interchange)
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
    }

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
        }
    }
} // end of class NemaMovementDialog
