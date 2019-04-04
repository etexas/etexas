package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*              DetectorConnectionForNonNemaDialog.java                       */
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
import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;
import javax.swing.*;

class DetectorConnectionForNonNemaDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFNOTES = 5;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JLabel label_title;

    JTextArea lbl_semi_note;

    JLabel[] label_note = new JLabel[NUMOFNOTES + 1];

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPH + 1];

    JTextArea[] label_detector = new JTextArea[PARAMS.TEXAS_MODEL_NPL + 1];

    JComboBox[][] comboBox_detector = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    JComboBox[] comboBox_detector_type = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton okButton, applyButton, cancelButton;

    TX_Fmt lclv_tx_fmt;

    Font font;

    int numOfPhases, numOfDetectors, loopDetectors;

    String titleString;

    public DetectorConnectionForNonNemaDialog() {
        numOfPhases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        numOfDetectors = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det;

        loopDetectors = Math.min(PARAMS.TEXAS_MODEL_NPL, gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det);

        if (numOfPhases < 2)
            numOfPhases = 2;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA];

        if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            titleString = "Semi-Actuated Detector Connection Data";
        }
        else {
            titleString = "Full-Actuated Detector Connection Data";
        }

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

        label_title = new JLabel(titleString);
        font = new Font("TimesRoman", Font.BOLD, 18);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        Font font1 = new Font("TimesRoman", Font.BOLD, 14);

        lbl_semi_note = new JTextArea("Phase 1 for a Semi-Actuated Controller is the non-actuated phase thus no detectors can be connected.");
        lbl_semi_note.setBackground(aFrame.getBackground());
        lbl_semi_note.setEditable(false);
        lbl_semi_note.setFocusable(false);
        lbl_semi_note.setWrapStyleWord(true);
        lbl_semi_note.setLineWrap(true);
        lbl_semi_note.setFont(font1);
        lbl_semi_note.setSize(new Dimension(350, 26));

        JPanel panel_semiact = new JPanel();
        panel_semiact.add(lbl_semi_note);

        label_note[1] = new JLabel("NOTE:");
        label_note[2] = new JLabel("-  = not connection   ");
        label_note[3] = new JLabel("+ = normal connection");
        label_note[4] = new JLabel("Total Detectors = " + numOfDetectors);
        label_note[5] = new JLabel("END NOTE");

        JPanel panel_note = new JPanel();

        GridBagLayout gbLayoutPanelNote = new GridBagLayout();
        GridBagConstraints gbConstraintsPanelNote;

        panel_note.setLayout(gbLayoutPanelNote);
        gbConstraintsPanelNote = new GridBagConstraints();

        for (int lsiv_i = 1; lsiv_i <= NUMOFNOTES; lsiv_i++) {
            gbConstraintsPanelNote.fill = GridBagConstraints.BOTH;
            gbConstraintsPanelNote.insets = new Insets(1, 1, 1, 1);
            gbConstraintsPanelNote.gridx = 1;
            gbConstraintsPanelNote.gridy = lsiv_i;
            gbConstraintsPanelNote.gridwidth = 1;
            gbConstraintsPanelNote.gridheight = 1;
            gbLayoutPanelNote.setConstraints(label_note[lsiv_i], gbConstraintsPanelNote);
            panel_note.add(label_note[lsiv_i]);
        }

        for (int lsiv_det = 0; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
            label_detector[lsiv_det] = new JTextArea();
            label_detector[lsiv_det].setBackground(aFrame.getBackground());
            label_detector[lsiv_det].setEditable(false);
            label_detector[lsiv_det].setEditable(false);
            label_detector[lsiv_det].setWrapStyleWord(true);
            label_detector[lsiv_det].setLineWrap(true);
            label_detector[lsiv_det].setFont(font1);
        }

        label_detector[0].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE]);

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
            label_detector[lsiv_det].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det]);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("Phase " + lsiv_phase);
        }

        String[] array_connection_type = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE].substring(1).split("\\|");

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_detector_type[lsiv_phase] = new JComboBox(array_connection_type);
        }

        String[] array_detector_1 = new String[numOfDetectors + 1];
        for (int lsiv_i = 0; lsiv_i <= numOfDetectors; lsiv_i++) {
            array_detector_1[lsiv_i] = Integer.toString(lsiv_i);
        }

        String[] array_detector = new String[numOfDetectors * 2 + 1];
        for (int lsiv_i = -numOfDetectors; lsiv_i <= numOfDetectors; lsiv_i++) {
            array_detector[lsiv_i + numOfDetectors] = Integer.toString(lsiv_i);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_detector[lsiv_phase][1] = new JComboBox(array_detector_1);

            for (int lsiv_det = 2; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                comboBox_detector[lsiv_phase][lsiv_det] = new JComboBox(array_detector);
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_detector_type[lsiv_phase].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE]);

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                comboBox_detector[lsiv_phase][lsiv_det].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det]));
            }
        }

        if (numOfDetectors == 0) {
            for (int lsiv_phase = 1; lsiv_phase <= numOfPhases; lsiv_phase++) {
                comboBox_detector_type[lsiv_phase].setEnabled(false);

                for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                    comboBox_detector[lsiv_phase][lsiv_det].setEnabled(false);
                }
            }
        }
        else {
            int startPhase;

            if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
                startPhase = 2;

                comboBox_detector_type[1].setEnabled(false);

                for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                    comboBox_detector[1][lsiv_det].setEnabled(false);
                }
            }
            else {
                startPhase = 1;
            }

            for (int lsiv_phase = startPhase; lsiv_phase <= numOfPhases; lsiv_phase++) {
                comboBox_detector_type[lsiv_phase].setEnabled(true);
                comboBox_detector[lsiv_phase][1].setEnabled(true);

                for (int lsiv_det = 2; lsiv_det <= loopDetectors; lsiv_det++) {
                    comboBox_detector[lsiv_phase][lsiv_det].setEnabled(false);
                }
            }
        } // end of if (numOfDetectors == 0)

        if (gdvsim.flag_detConnForNonNema_ok) {
            int startPhase;

            if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
                startPhase = 2;
            }
            else {
                startPhase = 1;
            }

            boolean lbov_value;
            for (int lsiv_phase = startPhase; lsiv_phase <= numOfPhases; lsiv_phase++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_detector_type[lsiv_phase].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type);
                }

                comboBox_detector_type[lsiv_phase].setEnabled(true);

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_detector[lsiv_phase][1].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[1]));
                }

                comboBox_detector[lsiv_phase][1].setEnabled(true);

                for (int lsiv_det = 2; lsiv_det <= loopDetectors; lsiv_det++) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                            + lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        comboBox_detector[lsiv_phase][lsiv_det].setSelectedItem(Integer
                                .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det]));
                        comboBox_detector[lsiv_phase][lsiv_det].setEnabled(true);

                        if (lsiv_phase > startPhase) {
                            lbov_value = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] > 0;

                            if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01
                                    - 1 + lsiv_det - 1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                                    && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det - 1] != 0)) {
                                comboBox_detector[lsiv_phase][lsiv_det].setEnabled(true);
                            }
                            else {
                                comboBox_detector[lsiv_phase][lsiv_det].setEnabled(lbov_value);
                            }
                        }
                    }
                    else {
                        comboBox_detector[lsiv_phase][lsiv_det].setEnabled(false);

                        if (lsiv_phase > startPhase) {
                            lbov_value = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] > 0;

                            if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01
                                    - 1 + lsiv_det - 1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                                    && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det - 1] != 0)) {
                                comboBox_detector[lsiv_phase][lsiv_det].setEnabled(true);
                            }
                            else {
                                comboBox_detector[lsiv_phase][lsiv_det].setEnabled(lbov_value);
                            }
                        }
                    }
                }
            }
        }

        okButton = new JButton("OK");
        applyButton = new JButton("Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_detector_type[lsiv_phase].addKeyListener(openComboMenuListener);
            comboBox_detector_type[lsiv_phase].addKeyListener(helpListener);

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                comboBox_detector[lsiv_phase][lsiv_det].addActionListener(componentActionListener);
                comboBox_detector[lsiv_phase][lsiv_det].addKeyListener(componentKeyListener);
                comboBox_detector[lsiv_phase][lsiv_det].addKeyListener(openComboMenuListener);
                comboBox_detector[lsiv_phase][lsiv_det].addKeyListener(helpListener);
            }
        }

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        setAccessiblility();
        setSize();

        int iRow = 0;
        int totalColumn = 2 + loopDetectors;
        int pad = 0;

        if (loopDetectors == 1) {
            pad = 80;
        }
        else if (loopDetectors == 2) {
            pad = 60;
        }
        else if (loopDetectors == 3) {
            pad = 40;
        }
        else if (loopDetectors == 4) {
            pad = 20;
        }
        else if (loopDetectors == 5) {
            pad = 5;
        }
        else {
            pad = 1;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        addComponent(panel_title, iRow++, 0, totalColumn, 1);
        addComponent(panel_note, iRow++, 0, totalColumn, 1);

        if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            addComponent(panel_semiact, iRow++, 0, totalColumn, 1);
        }

        addComponent(label_detector[0], iRow, 1, 1, 1);

        for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
            if (lsiv_det == loopDetectors) {
                gbConstraints.insets = new Insets(1, 1, 1, pad);
            }
            else {
                gbConstraints.insets = new Insets(1, 1, 1, 1);
            }

            addComponent(label_detector[lsiv_det], iRow, lsiv_det + 1, 1, 1);
        }

        iRow++;

        for (int lsiv_phase = 1; lsiv_phase <= numOfPhases; lsiv_phase++) {
            gbConstraints.insets = new Insets(1, pad, 1, 1);
            addComponent(label_phase[lsiv_phase], iRow, 0, 1, 1);

            gbConstraints.insets = new Insets(1, 1, 1, 1);
            addComponent(comboBox_detector_type[lsiv_phase], iRow, 1, 1, 1);

            for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                if (lsiv_det == loopDetectors) {
                    gbConstraints.insets = new Insets(1, 1, 1, pad);
                }
                else {
                    gbConstraints.insets = new Insets(1, 1, 1, 1);
                }
                addComponent(comboBox_detector[lsiv_phase][lsiv_det], iRow, lsiv_det + 1, 1, 1);
            }

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(ok_panel, iRow, 0, totalColumn, 1);

        aFrame.setSize(950, 680);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method DetectorConnectionForNema

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;
        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    void setAccessiblility() {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_detector_type[lsiv_phase].getAccessibleContext().setAccessibleName("Detector Connection Type for phase " + lsiv_phase);
            comboBox_detector_type[lsiv_phase].getAccessibleContext().setAccessibleDescription("Detector Connection Type for phase " + lsiv_phase);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                comboBox_detector[lsiv_phase][lsiv_det].getAccessibleContext().setAccessibleName("Detector Connection for phase " + lsiv_phase + " Detector " + (char)('A' + lsiv_det - 1));
                comboBox_detector[lsiv_phase][lsiv_det].getAccessibleContext().setAccessibleDescription("Detector Connection for phase " + lsiv_phase + " Detector " + (char)('A' + lsiv_det - 1));
            }
        }

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");
    } // end of method setAccessiblility

    void setSize() {
        for (int lsiv_det = 0; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
            label_detector[lsiv_det].setPreferredSize(new Dimension(70, 50));
            label_detector[lsiv_det].setMaximumSize(new Dimension(70, 50));
        }

        okButton.setPreferredSize(new Dimension(80, 25));
        applyButton.setPreferredSize(new Dimension(80, 25));
        cancelButton.setPreferredSize(new Dimension(80, 25));

        okButton.setMaximumSize(new Dimension(80, 25));
        applyButton.setMaximumSize(new Dimension(80, 25));
        cancelButton.setMaximumSize(new Dimension(80, 25));
    } // end of method setSize

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
    } // end of class OpenComboMenuListener

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

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    if (event.getSource() == comboBox_detector_type[lsiv_phase]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE], titleString,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE] + " for Phase " + lsiv_phase,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE], comboBox_detector_type[lsiv_phase].getSelectedItem().toString(),
                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE], " ", " ", " ");
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    for (int lsiv_det = 1; lsiv_det <= 1; lsiv_det++) {
                        if (event.getSource() == comboBox_detector[lsiv_phase][lsiv_det]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det], titleString,
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det] + " for Phase " + lsiv_phase,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det], comboBox_detector[lsiv_phase][lsiv_det].getSelectedItem().toString(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det]), " ", Integer.toString(0),
                                    Integer.toString(numOfDetectors), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det]));

                        }
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    for (int lsiv_det = 2; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                        if (event.getSource() == comboBox_detector[lsiv_phase][lsiv_det]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det], titleString,
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det] + " for Phase " + lsiv_phase,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det], comboBox_detector[lsiv_phase][lsiv_det].getSelectedItem().toString(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det]), " ", Integer.toString(numOfDetectors * (-1)),
                                    Integer.toString(numOfDetectors), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det]));

                        }
                    }
                }
            }
        }
    } // end of class HelpListener

    void moveZeroToLastColumn(int lsiv_phase, int lsiv_det) {
        if (Integer.valueOf(comboBox_detector[lsiv_phase][lsiv_det].getSelectedItem().toString()).intValue() == 0) {
            int lsiv_det_i;
            for (lsiv_det_i = lsiv_det; lsiv_det_i < loopDetectors; lsiv_det_i++) {
                if (comboBox_detector[lsiv_phase][lsiv_det_i + 1].isEnabled()) {
                    comboBox_detector[lsiv_phase][lsiv_det_i].setSelectedItem(comboBox_detector[lsiv_phase][lsiv_det_i + 1].getSelectedItem().toString());
                }
                else {
                    break;
                }
            }

            int x = Integer.valueOf(comboBox_detector[lsiv_phase][lsiv_det_i].getSelectedItem().toString()).intValue();

            comboBox_detector[lsiv_phase][lsiv_det_i].setSelectedItem("0");

            if (lsiv_det == loopDetectors) {}
            else if (!comboBox_detector[lsiv_phase][lsiv_det + 1].isEnabled()) {}
            else {
                if (x == 0)
                    comboBox_detector[lsiv_phase][lsiv_det_i].setEnabled(false);
            }
        }
        else {
            if (lsiv_det < loopDetectors) {
                comboBox_detector[lsiv_phase][lsiv_det + 1].setEnabled(true);
            }
        }
    } // end of method moveZeroToLastColumn

    void saveData() {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            gdvsim.detConnTypeNonNemaValue[lsiv_phase] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE];
            gdvsim.detConnTypeNonNemaStat[lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01
                        - 1 + lsiv_det];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                        + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det];
                gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        int startPhase;

        if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            startPhase = 2;

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.detConnTypeNonNemaStat[1] = gdvsim.gclv_inter.TX_FROM_USER;

            for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                        + lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.detConnNonNemaStat[1][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }
        else {
            startPhase = 1;
        }

        for (int lsiv_phase = startPhase; lsiv_phase <= numOfPhases; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type = comboBox_detector_type[lsiv_phase].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;

            gdvsim.detConnTypeNonNemaValue[lsiv_phase] = comboBox_detector_type[lsiv_phase].getSelectedItem().toString();
            gdvsim.detConnTypeNonNemaStat[lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                        + lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                if (comboBox_detector[lsiv_phase][lsiv_det].isEnabled()) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] = Integer.valueOf(
                            comboBox_detector[lsiv_phase][lsiv_det].getSelectedItem().toString()).intValue();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                            + lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;

                    gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det] = Integer.valueOf(comboBox_detector[lsiv_phase][lsiv_det].getSelectedItem().toString()).intValue();
                    gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                            + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }
    } // end of method saveData

    boolean isError() {
        int[] numOfDetectorPerPhase = new int[PARAMS.TEXAS_MODEL_NPH + 1];

        for (int lsiv_phase = 1; lsiv_phase <= numOfPhases; lsiv_phase++) {
            numOfDetectorPerPhase[lsiv_phase] = 0;
        }

        for (int lsiv_phase = 1; lsiv_phase <= numOfPhases; lsiv_phase++) {
            for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                if (comboBox_detector[lsiv_phase][lsiv_det].isEnabled()) {
                    if (Integer.valueOf(comboBox_detector[lsiv_phase][lsiv_det].getSelectedItem().toString()).intValue() != 0) {
                        numOfDetectorPerPhase[lsiv_phase]++;
                    }
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= numOfPhases; lsiv_phase++) {
            for (int lsiv_det_i = 1; lsiv_det_i < numOfDetectorPerPhase[lsiv_phase]; lsiv_det_i++) {
                for (int lsiv_det_j = lsiv_det_i + 1; lsiv_det_j <= numOfDetectorPerPhase[lsiv_phase]; lsiv_det_j++) {
                    if ((Integer.valueOf(comboBox_detector[lsiv_phase][lsiv_det_i].getSelectedItem().toString()).intValue() == Integer.valueOf(
                            comboBox_detector[lsiv_phase][lsiv_det_j].getSelectedItem().toString()).intValue())
                            || (Integer.valueOf(comboBox_detector[lsiv_phase][lsiv_det_i].getSelectedItem().toString()).intValue() == -Integer.valueOf(
                                    comboBox_detector[lsiv_phase][lsiv_det_j].getSelectedItem().toString()).intValue())) {
                        JOptionPane.showMessageDialog(null, "For Phase " + lsiv_phase + ", Detector " + (char)('A' + lsiv_det_i - 1) + " should not have the same number as Detector "
                                + (char)('A' + lsiv_det_j - 1) + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }
                }
            }
        }
        return false;
    } // end of method isError()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (numOfDetectors == 0) {
                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else {
                    if (!isError()) {
                        gdvsim.flag_detConnForNonNema_ok = true;
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                    if (event.getSource() == comboBox_detector[lsiv_phase][lsiv_det]) {
                        moveZeroToLastColumn(lsiv_phase, lsiv_det);
                        break;
                    }
                }
            }
        }
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (numOfDetectors == 0) {
                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                    else {
                        if (!isError()) {
                            gdvsim.flag_detConnForNonNema_ok = true;
                            saveData();

                            if (event.getSource() == okButton) {
                                aFrame.dispose();
                            }
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    for (int lsiv_det = 1; lsiv_det <= loopDetectors; lsiv_det++) {
                        if (event.getSource() == comboBox_detector[lsiv_phase][lsiv_det]) {
                            moveZeroToLastColumn(lsiv_phase, lsiv_det);
                            break;
                        }
                    }
                }
            }
        } // end of keyPressed
    } // end of class ComponentKeyListener
} // end of class DetectorConnectionForNema
