package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           NemaOverlapDialog.java                           */
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

class NemaOverlapDialog extends JDialog {

    ComboBoxActionListener comboBoxActionListener;

    ComboBoxKeyListener comboBoxKeyListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title;

    JLabel label_total_phases;

    JLabel[] label_overlap = new JLabel[PARAMS.TEXAS_MODEL_NON + 1];

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JComboBox[][] comboBox_ov_ph = new JComboBox[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];

    JButton okButton, applyButton, cancelButton;

    Font font;

    TX_Fmt lclv_tx_fmt;

    int lsiv_min;

    int lsiv_max;

    int lsiv_inc;

    int count;

    int int_number;

    JLabel label_total;

    JLabel label_total_overlap;

    JComboBox cbo_total;

    int number_of_phases;

    int initial_number_of_overlaps;

    String titleString;

    public NemaOverlapDialog() {
        initial_number_of_overlaps = 0;

        number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF];

        titleString = "NEMA Overlap Data";

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

        font = new Font("TimesRoman", Font.BOLD, 18);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        label_total_phases = new JLabel("Total Phases = " + number_of_phases);

        JPanel panel_total_phases = new JPanel();
        panel_total_phases.add(label_total_phases);

        char x = 'A';

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            label_overlap[lsiv_overlap] = new JLabel("Overlap  " + x);
            x = (char)('A' + lsiv_overlap);
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NPN; lsiv_overlap++) {
            label_phase[lsiv_overlap] = new JLabel("Phase " + lsiv_overlap);
        }

        String[] array_ov_ph = new String[number_of_phases + 1];

        for (int lsiv_i = 0; lsiv_i <= number_of_phases; lsiv_i++) {
            array_ov_ph[lsiv_i] = Integer.toString(lsiv_i);
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_ov_ph[lsiv_overlap][lsiv_phase] = new JComboBox(array_ov_ph);
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].getAccessibleContext().setAccessibleName("overlap " + lsiv_overlap + " phase " + lsiv_phase);
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].getAccessibleContext().setAccessibleDescription("overlap " + lsiv_overlap + " phase " + lsiv_phase);
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase]));
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].setEnabled(false);
            }
        }

        if (gdvsim.numOfOverlap == 0) {
            comboBox_ov_ph[1][1].setEnabled(true);
        }
        else {
            for (int lsiv_overlap = 1; lsiv_overlap <= gdvsim.numOfOverlap; lsiv_overlap++) {
                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_overlap][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        if (lsiv_phase == 1) {
                            comboBox_ov_ph[lsiv_overlap][lsiv_phase].setEnabled(true);
                            comboBox_ov_ph[lsiv_overlap][lsiv_phase].setSelectedItem(Integer.toString(gdvsim.overlapValue[lsiv_overlap][lsiv_phase]));
                        }
                        else {
                            if (gdvsim.overlapValue[lsiv_overlap][lsiv_phase - 1] > 0) {
                                comboBox_ov_ph[lsiv_overlap][lsiv_phase].setEnabled(true);
                                comboBox_ov_ph[lsiv_overlap][lsiv_phase].setSelectedItem(Integer.toString(gdvsim.overlapValue[lsiv_overlap][lsiv_phase]));
                            }
                        }
                    }
                    else {
                        if (lsiv_phase == 1) {
                            comboBox_ov_ph[lsiv_overlap][lsiv_phase].setEnabled(true);
                        }
                        else {
                            if (gdvsim.overlapValue[lsiv_overlap][lsiv_phase - 1] > 0) {
                                comboBox_ov_ph[lsiv_overlap][lsiv_phase].setEnabled(true);
                            }
                        }
                        break;
                    }
                }
            }

            if (gdvsim.numOfOverlap < PARAMS.TEXAS_MODEL_NON) {
                comboBox_ov_ph[gdvsim.numOfOverlap + 1][1].setEnabled(true);
            }
        } // end of if (gdvsim.numOfOverlap == 0)

        label_total = new JLabel("Total Overlaps ");
        label_total_overlap = new JLabel(Integer.toString(calculateNumOverlap()));
        label_total_overlap.setBorder(BorderFactory.createTitledBorder(""));
        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(calculateNumOverlap()));

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

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

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].addActionListener(comboBoxActionListener);
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].addKeyListener(comboBoxKeyListener);
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].addKeyListener(openComboMenuListener);
                comboBox_ov_ph[lsiv_overlap][lsiv_phase].addKeyListener(helpListener);
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

        cbo_total.addKeyListener(helpListener);
        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        gbConstraints.insets = new Insets(5, 3, 5, 3);

        int iRow = 0;

        addComponent(panel_title, iRow++, 0, number_of_phases + 1, 1);
        addComponent(panel_total_phases, iRow++, 0, number_of_phases + 1, 1);

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            addComponent(label_phase[lsiv_phase], iRow, lsiv_phase, 1, 1);
        }

        iRow++;

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            addComponent(label_overlap[lsiv_overlap], iRow, 0, 1, 1);

            for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                addComponent(comboBox_ov_ph[lsiv_overlap][lsiv_phase], iRow, lsiv_phase, 1, 1);
            }
            iRow++;
        }

        addComponent(label_total, iRow, 2, 3, 1);
        addComponent(cbo_total, iRow++, 1, 1, 1);
        addComponent(ok_panel, iRow, 0, number_of_phases + 1, 1);

        aFrame.setSize(1000, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method detectorDataForDiamond

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

    int calculateNumOverlap() {
        int sum = 0;

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            if (Integer.valueOf(comboBox_ov_ph[lsiv_overlap][1].getSelectedItem().toString()).intValue() != 0) {
                sum++;
            }
        }
        return sum;
    }

    void ComboBoxFunction(int lsiv_overlap, int lsiv_phase) {
        if (Integer.valueOf(comboBox_ov_ph[lsiv_overlap][lsiv_phase].getSelectedItem().toString()).intValue() == 0) {
            if ((lsiv_phase == 1) && (comboBox_ov_ph[lsiv_overlap][2].isEnabled()) && (!comboBox_ov_ph[lsiv_overlap][3].isEnabled()) && (lsiv_overlap != PARAMS.TEXAS_MODEL_NON)) {
                int numOfOverlap;
                numOfOverlap = calculateNumOverlap() + 1;

                int[] numOfPhasePerOverlap = new int[PARAMS.TEXAS_MODEL_NON + 1];

                for (int lsiv_olp_i = 1; lsiv_olp_i <= PARAMS.TEXAS_MODEL_NON; lsiv_olp_i++) {
                    numOfPhasePerOverlap[lsiv_olp_i] = 0;
                }

                for (int lsiv_olp_i = 1; lsiv_olp_i <= PARAMS.TEXAS_MODEL_NON; lsiv_olp_i++) {
                    for (int lsiv_phase_i = 1; lsiv_phase_i <= number_of_phases; lsiv_phase_i++) {
                        if (comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].isEnabled()) {
                            numOfPhasePerOverlap[lsiv_olp_i]++;
                        }
                    }
                }

                for (int lsiv_olp_i = lsiv_overlap; lsiv_olp_i <= Math.min(numOfOverlap, PARAMS.TEXAS_MODEL_NON - 1); lsiv_olp_i++) {
                    for (int lsiv_phase_i = numOfPhasePerOverlap[lsiv_olp_i]; lsiv_phase_i > numOfPhasePerOverlap[lsiv_olp_i + 1]; lsiv_phase_i--) {
                        comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase_i]));
                        comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].setEnabled(false);
                    }

                    for (int lsiv_phase_i = 1; lsiv_phase_i <= numOfPhasePerOverlap[lsiv_olp_i + 1]; lsiv_phase_i++) {
                        if (!comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].isEnabled()) {
                            comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].setEnabled(true);
                        }

                        comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].setSelectedItem(comboBox_ov_ph[lsiv_olp_i + 1][lsiv_phase_i].getSelectedItem().toString());
                    }
                }

                if (numOfOverlap < PARAMS.TEXAS_MODEL_NON) {
                    comboBox_ov_ph[numOfOverlap + 1][1].setEnabled(false);
                }

                for (int lsiv_phase_i = numOfPhasePerOverlap[numOfOverlap]; lsiv_phase_i >= 2; lsiv_phase_i--) {
                    if (comboBox_ov_ph[numOfOverlap][lsiv_phase_i].isEnabled()) {
                        comboBox_ov_ph[numOfOverlap][lsiv_phase_i].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase_i]));
                        comboBox_ov_ph[numOfOverlap][lsiv_phase_i].setEnabled(false);
                    }
                }

                comboBox_ov_ph[numOfOverlap][1].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01]));
                comboBox_ov_ph[numOfOverlap][1].setEnabled(true);

            }
            else {
                int[] numOfPhasePerOverlap = new int[PARAMS.TEXAS_MODEL_NON + 1];

                for (int lsiv_olp_i = 1; lsiv_olp_i <= PARAMS.TEXAS_MODEL_NON; lsiv_olp_i++) {
                    numOfPhasePerOverlap[lsiv_olp_i] = 0;
                }

                for (int lsiv_olp_i = 1; lsiv_olp_i <= PARAMS.TEXAS_MODEL_NON; lsiv_olp_i++) {
                    for (int lsiv_phase_i = 1; lsiv_phase_i <= number_of_phases; lsiv_phase_i++) {
                        if (comboBox_ov_ph[lsiv_olp_i][lsiv_phase_i].isEnabled()) {
                            numOfPhasePerOverlap[lsiv_olp_i]++;
                        }
                    }
                }

                if (lsiv_phase != numOfPhasePerOverlap[lsiv_overlap]) {
                    for (int lsiv_phase_i = lsiv_phase; lsiv_phase_i < numOfPhasePerOverlap[lsiv_overlap]; lsiv_phase_i++) {
                        if (comboBox_ov_ph[lsiv_overlap][lsiv_phase_i + 1].isEnabled()) {
                            comboBox_ov_ph[lsiv_overlap][lsiv_phase_i].setSelectedItem(comboBox_ov_ph[lsiv_overlap][lsiv_phase_i + 1].getSelectedItem().toString());
                        }
                    }

                    if (numOfPhasePerOverlap[lsiv_overlap] == number_of_phases) {
                        if (Integer.valueOf(comboBox_ov_ph[lsiv_overlap][numOfPhasePerOverlap[lsiv_overlap]].getSelectedItem().toString()).intValue() == 0) {
                            comboBox_ov_ph[lsiv_overlap][numOfPhasePerOverlap[lsiv_overlap]].setEnabled(false);
                        }
                        else {
                            comboBox_ov_ph[lsiv_overlap][numOfPhasePerOverlap[lsiv_overlap]].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01
                                    - 1 + numOfPhasePerOverlap[lsiv_overlap]]));
                        }
                    }
                    else {
                        comboBox_ov_ph[lsiv_overlap][numOfPhasePerOverlap[lsiv_overlap]].setEnabled(false);
                    }
                }
            }
        }
        else {
            if (lsiv_phase < number_of_phases) {
                comboBox_ov_ph[lsiv_overlap][lsiv_phase + 1].setEnabled(true);
            }

            if (lsiv_overlap < PARAMS.TEXAS_MODEL_NON) {
                comboBox_ov_ph[lsiv_overlap + 1][1].setEnabled(true);
            }
        }

        label_total_overlap.setText(Integer.toString(calculateNumOverlap()));
        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(calculateNumOverlap()));

    }

    class ComboBoxActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    if (event.getSource() == comboBox_ov_ph[lsiv_overlap][lsiv_phase]) {
                        ComboBoxFunction(lsiv_overlap, lsiv_phase);
                    }
                }
            }
        }
    }

    class ComboBoxKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
                    for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                        if (event.getSource() == comboBox_ov_ph[lsiv_overlap][lsiv_phase]) {
                            ComboBoxFunction(lsiv_overlap, lsiv_phase);
                        }
                    }
                }
            }
        }
    }

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
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the " + label_total.getText()
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NON), "1");
                }

                for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF];
                        if (event.getSource() == comboBox_ov_ph[lsiv_overlap][lsiv_phase]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase], lclv_tx_fmt.mstv_name.replace("#",
                                    ((char)('A' - 1 + lsiv_overlap) + " ")), "Overlap " + (char)('A' - 1 + lsiv_overlap) + "  "
                                            + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase], comboBox_ov_ph[lsiv_overlap][lsiv_phase].getSelectedItem().toString(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase]), " ",
                                    Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase]), Integer.toString(number_of_phases),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase]));
                        }
                    }
                }
            }
        }
    }

    void saveData() {
        gdvsim.numOfOverlap = calculateNumOverlap();
        gdvsim.numOfOverlapStat = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                gdvsim.overlapStat[lsiv_overlap][lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.overlapValue[lsiv_overlap][lsiv_phase] = 0;
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                if (comboBox_ov_ph[lsiv_overlap][lsiv_phase].isEnabled()) {
                    if (Integer.valueOf(comboBox_ov_ph[lsiv_overlap][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        gdvsim.overlapStat[lsiv_overlap][lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.overlapValue[lsiv_overlap][lsiv_phase] = Integer.valueOf(comboBox_ov_ph[lsiv_overlap][lsiv_phase].getSelectedItem().toString()).intValue();
                    }
                }
            }
        }

        JOptionPane.showMessageDialog(null, "The NEMA overlap data has beed saved in a temporary location.\nPlease OK or Apply the NEMA Signal Controller Timing Data to save this data.",
                "Warning Message", JOptionPane.WARNING_MESSAGE);
    } // end of saveData()

    boolean isError() {
        int numOfOverlap;
        numOfOverlap = calculateNumOverlap();

        int[] numOfPhasePerOverlap = new int[PARAMS.TEXAS_MODEL_NON + 1];

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            numOfPhasePerOverlap[lsiv_overlap] = 0;
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                if (comboBox_ov_ph[lsiv_overlap][lsiv_phase].isEnabled()) {
                    if (Integer.valueOf(comboBox_ov_ph[lsiv_overlap][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        numOfPhasePerOverlap[lsiv_overlap]++;
                    }
                }
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= numOfOverlap; lsiv_overlap++) {
            for (int lsiv_phase_i = 1; lsiv_phase_i < numOfPhasePerOverlap[lsiv_overlap]; lsiv_phase_i++) {
                for (int lsiv_phase_j = lsiv_phase_i + 1; lsiv_phase_j <= numOfPhasePerOverlap[lsiv_overlap]; lsiv_phase_j++) {
                    if (Integer.valueOf(comboBox_ov_ph[lsiv_overlap][lsiv_phase_i].getSelectedItem().toString()).intValue() == Integer.valueOf(
                            comboBox_ov_ph[lsiv_overlap][lsiv_phase_j].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "Overlap " + ((char)('A' + lsiv_overlap - 1)) + " Phase " + lsiv_phase_i + " = "
                                + comboBox_ov_ph[lsiv_overlap][lsiv_phase_i].getSelectedItem().toString() + " should not be the same as Phase " + lsiv_phase_j + " = "
                                + comboBox_ov_ph[lsiv_overlap][lsiv_phase_j].getSelectedItem().toString() + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
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
} // end of class NemaOverlapDialog
