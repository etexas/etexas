package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           HardwareDialog.java                              */
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

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.text.DecimalFormat;

class HardwareDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 4;

    static final int PED_MVMT = 1;

    static final int PED_MVMT_DIST = 2;

    static final int PED_MVMT_VOL = 3;

    static final int PED_MVMT_PAR = 4;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstr;

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_HPH + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_HPH + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_HPH + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_HPH + 1];

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_HPH + 1];

    JComboBox[][] cbo_hardware = new JComboBox[PARAMS.TEXAS_MODEL_HPH + 1][NUMOFFIELD + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JButton[] setAllButton = new JButton[NUMOFFIELD + 1];

    JComboBox[] setAllComboBox = new JComboBox[NUMOFFIELD + 1];

    JLabel label_title, label_total, label_NOVLP;

    JComboBox cbo_total, cbo_NOVLP;

    JButton btnNEMAMovement, okButton, applyButton, cancelButton;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    TX_Fmt lclv_tx_fmt;

    int[][] movementDefault = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NMP + 1];

    int[] numOfMovementDefault = new int[PARAMS.TEXAS_MODEL_NPN + 1];

    public HardwareDialog() {
        int initial_number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;
        if (initial_number_of_phases < 2)
            initial_number_of_phases = 2;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT];

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            numOfMovementDefault[lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_phase];
            gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01
                    - 1 + lsiv_phase];

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                movementDefault[lsiv_phase][lsiv_mvt] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1
                        - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                    + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.numOfMovementValue[lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num[lsiv_phase];
            }
            else {
                gdvsim.numOfMovementValue[lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_phase];
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt
                        + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.movementValue[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt];
                }
                else {
                    gdvsim.movementValue[lsiv_phase][lsiv_mvt] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];

        String titleString = "Hardware-in-the-Loop Signal Controller Data";

        aFrame = new JFrame(titleString);

        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstr = new GridBagConstraints();
        container.setLayout(gbLayout);

        gbConstr.fill = GridBagConstraints.BOTH;

        Font font1 = new Font("TimesRoman", Font.BOLD, 18);
        Font font2 = new Font("TimesRoman", Font.BOLD, 14);

        label_title = new JLabel(titleString);
        label_total = new JLabel("Total Phases");

        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int lsiv_min_int;
        int lsiv_max_int;
        int lsiv_inc_int;

        int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

        int count, index;
        double double_number, doubleArrayElementValue;

        String[] array_ped_mvmt = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT].substring(1).split("\\|");
        String[] array_ped_mvmt_dist = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST].substring(1).split("\\|");

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_ped_mvmt_vol = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_ped_mvmt_vol[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_ped_mvmt_par = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_ped_mvmt_par[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        setAllComboBox[PED_MVMT] = new JComboBox(array_ped_mvmt);
        setAllComboBox[PED_MVMT_DIST] = new JComboBox(array_ped_mvmt_dist);
        setAllComboBox[PED_MVMT_VOL] = new JComboBox(array_ped_mvmt_vol);
        setAllComboBox[PED_MVMT_PAR] = new JComboBox(array_ped_mvmt_par);

        setAllComboBox[PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        setAllComboBox[PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
        setAllComboBox[PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
        setAllComboBox[PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));

        for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
            setAllButton[lsiv_field] = new JButton("Set All");
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font2);
            setAllButton[lsiv_field].setSize(new Dimension(80, 10));
            label_field[lsiv_field].setSize(new Dimension(80, 10));
        }

        label_field[PED_MVMT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        label_field[PED_MVMT_DIST].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
        label_field[PED_MVMT_VOL].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]);
        label_field[PED_MVMT_PAR].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (lsiv_phase % 2 == 0) {
                cbo_hardware[lsiv_phase][PED_MVMT] = new JComboBox(array_ped_mvmt);
                cbo_hardware[lsiv_phase][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
            }
            else {
                cbo_hardware[lsiv_phase][PED_MVMT] = new JComboBox();
                cbo_hardware[lsiv_phase][PED_MVMT].addItem("NO");
            }
            cbo_hardware[lsiv_phase][PED_MVMT_DIST] = new JComboBox(array_ped_mvmt_dist);
            cbo_hardware[lsiv_phase][PED_MVMT_VOL] = new JComboBox(array_ped_mvmt_vol);
            cbo_hardware[lsiv_phase][PED_MVMT_PAR] = new JComboBox(array_ped_mvmt_par);

            cbo_hardware[lsiv_phase][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            cbo_hardware[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
        }

        lsiv_min_int = 0;
        lsiv_max_int = PARAMS.TEXAS_MODEL_HOV;
        lsiv_inc_int = 1;

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_NOVLP = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_NOVLP[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        cbo_NOVLP = new JComboBox(array_NOVLP);

        label_NOVLP = new JLabel("Number of Overlaps");

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            cbo_NOVLP.setSelectedItem("0");
        }
        else {
            cbo_NOVLP.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov));
        }

        if (gdvsim.flag_nema_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                if (lsiv_phase % 2 == 0) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        cbo_hardware[lsiv_phase][PED_MVMT].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt);
                    }

                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        cbo_hardware[lsiv_phase][PED_MVMT_DIST].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt_dist);
                        resetMovementParameter(lsiv_phase);
                    }

                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        cbo_hardware[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer
                                .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_mvmt_vol));
                    }

                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits
                                .format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par));
                    }
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("Phase " + Integer.toString(lsiv_phase));

            add[lsiv_phase] = new JButton("Add");
            del[lsiv_phase] = new JButton("Delete");
            up[lsiv_phase] = new JButton("Up");
            down[lsiv_phase] = new JButton("Down");

            add[lsiv_phase].setSize(new Dimension(5, 10));
            del[lsiv_phase].setSize(new Dimension(5, 10));
            up[lsiv_phase].setSize(new Dimension(5, 10));
            down[lsiv_phase].setSize(new Dimension(5, 10));
        }

        btnNEMAMovement = new JButton("NEMA/HARDWARE Movement Data");

        cbo_total = new JComboBox();

        setStatus(initial_number_of_phases);

        cbo_total.addItem(Integer.toString(initial_number_of_phases));

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        btnNEMAMovement.setMnemonic(KeyEvent.VK_N);
        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        cbo_NOVLP.addKeyListener(helpListener);
        cbo_NOVLP.addKeyListener(openComboMenuListener);

        for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
            setAllButton[lsiv_field].addActionListener(componentActionListener);
            setAllButton[lsiv_field].addKeyListener(componentKeyListener);
            setAllButton[lsiv_field].addKeyListener(helpListener);
            setAllComboBox[lsiv_field].addKeyListener(openComboMenuListener);
            setAllComboBox[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                cbo_hardware[lsiv_phase][lsiv_field].addKeyListener(openComboMenuListener);
                cbo_hardware[lsiv_phase][lsiv_field].addKeyListener(helpListener);
            }

            add[lsiv_phase].addActionListener(componentActionListener);
            del[lsiv_phase].addActionListener(componentActionListener);
            up[lsiv_phase].addActionListener(componentActionListener);
            down[lsiv_phase].addActionListener(componentActionListener);

            add[lsiv_phase].addKeyListener(componentKeyListener);
            del[lsiv_phase].addKeyListener(componentKeyListener);
            up[lsiv_phase].addKeyListener(componentKeyListener);
            down[lsiv_phase].addKeyListener(componentKeyListener);

            add[lsiv_phase].addKeyListener(helpListener);
            del[lsiv_phase].addKeyListener(helpListener);
            up[lsiv_phase].addKeyListener(helpListener);
            down[lsiv_phase].addKeyListener(helpListener);

            cbo_hardware[lsiv_phase][PED_MVMT].addKeyListener(componentKeyListener);
            cbo_hardware[lsiv_phase][PED_MVMT].addActionListener(componentActionListener);
            cbo_hardware[lsiv_phase][PED_MVMT_DIST].addKeyListener(componentKeyListener);
            cbo_hardware[lsiv_phase][PED_MVMT_DIST].addActionListener(componentActionListener);
        }

        btnNEMAMovement.addKeyListener(helpListener);
        btnNEMAMovement.addKeyListener(componentKeyListener);
        btnNEMAMovement.addActionListener(componentActionListener);

        cbo_total.addKeyListener(helpListener);
        cbo_total.addKeyListener(openComboMenuListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        setAccessibility();

        int iRow = 0;
        int numOfColumns = 9;

        gbConstr.insets = new Insets(1, 1, 1, 1);
        addComponent(panel_title, iRow++, 0, numOfColumns, 1, 0);

        gbConstr.insets = new Insets(90, 1, 1, 1);
        addComponent(btnNEMAMovement, iRow, 1, 4, 1, 0);
        gbConstr.insets = new Insets(1, 1, 1, 1);
        for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
        }
        iRow++;

        addComponent(cbo_NOVLP, iRow, 1, 1, 1, 0);
        addComponent(label_NOVLP, iRow, 2, 3, 1, 0);
        for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
            addComponent(setAllComboBox[lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
        }
        iRow++;

        for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
            addComponent(setAllButton[lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
        }
        iRow++;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            addComponent(label_phase[lsiv_phase], iRow, 0, 1, 1, 0);
            addComponent(add[lsiv_phase], iRow, 1, 1, 1, 0);
            addComponent(del[lsiv_phase], iRow, 2, 1, 1, 0);
            addComponent(up[lsiv_phase], iRow, 3, 1, 1, 0);
            addComponent(down[lsiv_phase], iRow, 4, 1, 1, 0);

            for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                addComponent(cbo_hardware[lsiv_phase][lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
            }
            iRow++;
        }

        addComponent(label_total, iRow, 0, 1, 1, 0);
        addComponent(cbo_total, iRow++, 1, 1, 1, 0);
        addComponent(ok_panel, iRow, 0, numOfColumns, 1, 0);

        aFrame.setSize(950, 700);
        aFrame.setVisible(true);

        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method HardwareDialog

    void addComponent(Component c, int row, int column, int width, int height, int ipadx) {
        gbConstr.ipadx = ipadx;

        gbConstr.gridx = column;
        gbConstr.gridy = row;

        gbConstr.gridwidth = width;
        gbConstr.gridheight = height;

        gbLayout.setConstraints(c, gbConstr);
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
    } // end of class OpenComboMenuListener

    int getNumberOfPhases() {
        int numOfPh = 0;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (cbo_hardware[lsiv_phase][PED_MVMT].isEnabled()) {
                numOfPh++;
            }
        }

        return numOfPh;
    }

    void setAccessibility() {
        for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];

            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for set all");
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for set all");

            setAllButton[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for set all");
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for set all");
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                if (lsiv_field == NUMOFFIELD) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                    if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("CONSTAN")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM] + " for phase " + lsiv_phase);
                    }
                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("ERLANG")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " for phase " + lsiv_phase);
                    }
                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("GAMMA")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " for phase " + lsiv_phase);
                    }
                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("LOGNRML")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " for phase " + lsiv_phase);
                    }
                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("NEGEXP")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP] + " for phase " + lsiv_phase);
                    }
                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("SNEGEXP")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " for phase " + lsiv_phase);
                    }
                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("UNIFORM")) {
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " for phase " + lsiv_phase);
                        cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " for phase " + lsiv_phase);
                    }
                }
                else {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                    cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for phase " + lsiv_phase);
                    cbo_hardware[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for phase " + lsiv_phase);
                }
            }

            add[lsiv_phase].getAccessibleContext().setAccessibleName("Add    for phase " + lsiv_phase);
            add[lsiv_phase].getAccessibleContext().setAccessibleDescription("Add    for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleName("Delete for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleDescription("Delete for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleName("Up     for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleDescription("Up     for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleName("Down   for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleDescription("Down   for phase " + lsiv_phase);
        }

        btnNEMAMovement.getAccessibleContext().setAccessibleName(btnNEMAMovement.getText());
        btnNEMAMovement.getAccessibleContext().setAccessibleDescription(btnNEMAMovement.getText());
        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());
        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");
        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HEADER];

        cbo_NOVLP.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV]);
        cbo_NOVLP.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV]);

    } // end of method setAccessibility

    void resetMovementParameter(int lsiv_phase) {
        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

        double double_number, doubleArrayElementValue;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

        if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("CONSTAN")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
        }
        else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("ERLANG")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
        }
        else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("GAMMA")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;

            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
        }
        else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("LOGNRML")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
        }
        else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("NEGEXP")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
        }
        else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("SNEGEXP")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
        }
        else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("UNIFORM")) {
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
        }
    } // end of method resetMovementParameter

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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_HPH), "1");
                }
                else if (event.getSource() == cbo_NOVLP) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HEADER];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV], cbo_NOVLP
                                    .getSelectedItem().toString(),
                            Integer.toString(0), " ", Integer.toString(0), Integer.toString(PARAMS.TEXAS_MODEL_HOV), Integer.toString(1));
                }
                else if (event.getSource() == btnNEMAMovement) {
                    new HelpDialog(true, btnNEMAMovement.getText(), "The NEMA/HARDWARE Movement button opens NEMA/HARDWARE Movement window", " ", " ", " ", " ", " ", " ", " ");
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                    if (event.getSource() == add[lsiv_phase]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 phase for all phases below this phase, inserts a new phase at the current position, copies the values of all parameters from the previous phase to the new phase, and increases the Total Phases by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_phase]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 phase for all phases below this phase and decreases the Total Phases by 1.", " ", " ", " ", " ",
                                " ", " ", " ");
                    }
                    else if (event.getSource() == up[lsiv_phase]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous phase and the current phase.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_phase]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next phase and the current phase.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }

                for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];

                    if (event.getSource() == setAllButton[lsiv_field]) {
                        new HelpDialog(true, "Set All Button", "The Set All button sets the value of " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]
                                + " for all phases to the value selected above.", " ", " ", " ", " ", " ", " ", " ");
                    }

                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        switch (lsiv_field) {
                            case PED_MVMT_PAR:
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for all phases",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                        twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]), " ",
                                        twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]),
                                        twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]),
                                        twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]));
                                break;

                            case PED_MVMT_VOL:
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for all phases",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]));
                                break;

                            case PED_MVMT:
                            case PED_MVMT_DIST:
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field] + " for all phases",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT
                                                - 1 + lsiv_field],
                                        " ", " ", " ");
                                break;
                        }
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                    for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                        if (event.getSource() == cbo_hardware[lsiv_phase][lsiv_field]) {
                            switch (lsiv_field) {
                                case PED_MVMT_PAR:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                                    String name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase));

                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                                    if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("CONSTAN")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN],
                                                cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                                    }
                                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("ERLANG")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                                    }
                                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("GAMMA")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                                    }
                                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("LOGNRML")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML],
                                                cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                                    }
                                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("NEGEXP")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                                    }
                                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("SNEGEXP")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP],
                                                cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                                    }
                                    else if (cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString().equals("UNIFORM")) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM],
                                                cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                                    }

                                    break;

                                case PED_MVMT_VOL:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                            cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]), " ",
                                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field]));
                                    break;

                                case PED_MVMT:
                                    if (lsiv_phase % 2 == 0) {
                                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                                Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem()
                                                        .toString(),
                                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], " ", " ", " ");
                                    }
                                    else {
                                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                                Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem()
                                                        .toString(),
                                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], "NO", " ", " ", " ");
                                    }
                                    break;

                                case PED_MVMT_DIST:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                            cbo_hardware[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT - 1 + lsiv_field], " ", " ", " ");
                                    break;
                            }
                        }
                    }
                }
            }
        }
    } // end of class help

    void setStatus(int sumOfPhases) {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                label_phase[lsiv_phase].setEnabled(true);

                for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                    cbo_hardware[lsiv_phase][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_phase[lsiv_phase].setEnabled(false);

                for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                    cbo_hardware[lsiv_phase][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_HPH) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                add[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                if (lsiv_phase <= sumOfPhases + 1) {
                    add[lsiv_phase].setEnabled(true);
                }
                else {
                    add[lsiv_phase].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_HPH) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                del[lsiv_phase].setEnabled(true);
            }
        }
        else if (sumOfPhases <= 2) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                del[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                if (lsiv_phase <= sumOfPhases) {
                    del[lsiv_phase].setEnabled(true);
                }
                else {
                    del[lsiv_phase].setEnabled(false);
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                up[lsiv_phase].setEnabled(true);
            }
            else {
                up[lsiv_phase].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (lsiv_phase < sumOfPhases) {
                down[lsiv_phase].setEnabled(true);
            }
            else {
                down[lsiv_phase].setEnabled(false);
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (cbo_hardware[lsiv_phase][PED_MVMT].getSelectedItem().toString().equals("YES")) {
                cbo_hardware[lsiv_phase][PED_MVMT_DIST].setEnabled(true);
                cbo_hardware[lsiv_phase][PED_MVMT_VOL].setEnabled(true);
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].setEnabled(true);
            }
            else {
                cbo_hardware[lsiv_phase][PED_MVMT_DIST].setEnabled(false);
                cbo_hardware[lsiv_phase][PED_MVMT_VOL].setEnabled(false);
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].setEnabled(false);
            }
        }
    } // end of method setStatus

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
            if (lsiv_phase % 2 == 0) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                cbo_hardware[lsiv_phase][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
                resetMovementParameter(lsiv_phase);
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
                cbo_hardware[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
                cbo_hardware[lsiv_phase][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
            }
        }

        for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
            if (gdvsim.numOfMovementStat[lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.numOfMovementValue[lsiv_phase + 1] = gdvsim.numOfMovementValue[lsiv_phase];
                gdvsim.numOfMovementStat[lsiv_phase + 1] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.numOfMovementValue[lsiv_phase + 1] = numOfMovementDefault[lsiv_phase];
                gdvsim.numOfMovementStat[lsiv_phase + 1] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (gdvsim.movementStat[lsiv_phase][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.movementValue[lsiv_phase + 1][lsiv_mvt] = gdvsim.movementValue[lsiv_phase][lsiv_mvt];
                    gdvsim.movementStat[lsiv_phase + 1][lsiv_mvt] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.movementValue[lsiv_phase + 1][lsiv_mvt] = movementDefault[lsiv_phase][lsiv_mvt];
                    gdvsim.movementStat[lsiv_phase + 1][lsiv_mvt] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }
        gdvsim.numOfMovementStat[sum + 1] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.movementStat[sum + 1][1] = gdvsim.gclv_inter.TX_FROM_USER;
    } // end of method setValueAfterAdd

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
            if (lsiv_phase % 2 == 0) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                cbo_hardware[lsiv_phase][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
                resetMovementParameter(lsiv_phase);
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
                cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
                cbo_hardware[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
                cbo_hardware[lsiv_phase][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
            }
        }

        if (sum % 2 == 0) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[sum][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            resetMovementParameter(sum);
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[sum][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
            cbo_hardware[sum][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[sum][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        }

        for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
            if (gdvsim.numOfMovementStat[lsiv_phase + 1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.numOfMovementValue[lsiv_phase] = gdvsim.numOfMovementValue[lsiv_phase + 1];
                gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.numOfMovementValue[lsiv_phase] = numOfMovementDefault[lsiv_phase + 1];
                gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (gdvsim.movementStat[lsiv_phase + 1][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.movementValue[lsiv_phase][lsiv_mvt] = gdvsim.movementValue[lsiv_phase + 1][lsiv_mvt];
                    gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.movementValue[lsiv_phase][lsiv_mvt] = movementDefault[lsiv_phase + 1][lsiv_mvt];
                    gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        if (gdvsim.numOfMovementStat[sum] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            gdvsim.numOfMovementValue[sum] = numOfMovementDefault[sum];
            gdvsim.numOfMovementStat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            if (gdvsim.numOfMovementStat[sum] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.numOfMovementValue[sum] = numOfMovementDefault[sum];
                gdvsim.numOfMovementStat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }
    } // end of method setValueAfterDel

    void setValueAfterUp(int index) {
        if (index % 2 == 0) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            resetMovementParameter(index);
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
            cbo_hardware[index][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[index][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        }
        else {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index - 1][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            resetMovementParameter(index - 1);
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index - 1][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
            cbo_hardware[index - 1][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[index - 1][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        }

        int temp;
        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            temp = gdvsim.movementValue[index][lsiv_mvt];
            gdvsim.movementValue[index][lsiv_mvt] = gdvsim.movementValue[index - 1][lsiv_mvt];
            gdvsim.movementValue[index - 1][lsiv_mvt] = temp;
        }

        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            temp = gdvsim.movementStat[index][lsiv_mvt];
            gdvsim.movementStat[index][lsiv_mvt] = gdvsim.movementStat[index - 1][lsiv_mvt];
            gdvsim.movementStat[index - 1][lsiv_mvt] = temp;
        }

        temp = gdvsim.numOfMovementValue[index];
        gdvsim.numOfMovementValue[index] = gdvsim.numOfMovementValue[index - 1];
        gdvsim.numOfMovementValue[index - 1] = temp;

        temp = gdvsim.numOfMovementStat[index];
        gdvsim.numOfMovementStat[index] = gdvsim.numOfMovementStat[index - 1];
        gdvsim.numOfMovementStat[index - 1] = temp;

    } // end of method setValueAfterUp

    void setValueAfterDown(int index) {
        if (index % 2 == 0) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            resetMovementParameter(index);
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
            cbo_hardware[index][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[index][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        }
        else {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index + 1][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            resetMovementParameter(index + 1);
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];
            cbo_hardware[index + 1][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));
            cbo_hardware[index + 1][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[index + 1][PED_MVMT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT]);
        }

        int temp;
        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            temp = gdvsim.movementValue[index][lsiv_mvt];
            gdvsim.movementValue[index][lsiv_mvt] = gdvsim.movementValue[index + 1][lsiv_mvt];
            gdvsim.movementValue[index + 1][lsiv_mvt] = temp;
        }

        for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
            temp = gdvsim.movementStat[index][lsiv_mvt];
            gdvsim.movementStat[index][lsiv_mvt] = gdvsim.movementStat[index + 1][lsiv_mvt];
            gdvsim.movementStat[index + 1][lsiv_mvt] = temp;
        }

        temp = gdvsim.numOfMovementValue[index];
        gdvsim.numOfMovementValue[index] = gdvsim.numOfMovementValue[index + 1];
        gdvsim.numOfMovementValue[index + 1] = temp;

        temp = gdvsim.numOfMovementStat[index];
        gdvsim.numOfMovementStat[index] = gdvsim.numOfMovementStat[index + 1];
        gdvsim.numOfMovementStat[index + 1] = temp;

    } // end of method setValueAfterDown

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = Integer.valueOf(cbo_NOVLP.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.flag_nemaOverlap_ok = true;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msiv_movement_defmov = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV] = gdvsim.gclv_inter.TX_DEFAULT;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = getNumberOfPhases();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt = cbo_hardware[lsiv_phase][PED_MVMT].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt_dist = cbo_hardware[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_mvmt_vol = Integer.valueOf(
                    cbo_hardware[lsiv_phase][PED_MVMT_VOL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par = Double.valueOf(
                    cbo_hardware[lsiv_phase][PED_MVMT_PAR].getSelectedItem().toString()).doubleValue();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        gdvsim.flag_nemaMovement_ok = true;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
            if (gdvsim.numOfMovementStat[lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num[lsiv_phase] = gdvsim.numOfMovementValue[lsiv_phase];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                        + lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                        + lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (gdvsim.movementStat[lsiv_phase][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] = gdvsim.movementValue[lsiv_phase][lsiv_mvt];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt
                            + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt
                            + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msiv_num_chars_tx = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_phase = 1; lsiv_phase <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text[lsiv_phase] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01
                    - 1 + lsiv_phase];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01 - 1
                    + lsiv_phase] = gdvsim.gclv_inter.TX_DEFAULT;
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; lsiv_overlap++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[lsiv_overlap] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A
                    - 1 + lsiv_overlap];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A - 1
                    + lsiv_overlap] = gdvsim.gclv_inter.TX_DEFAULT;
        }
    } // end of method saveData

    void AddAction(int lsiv_phase) {
        int numOfPhasesBeforeClick = getNumberOfPhases();

        for (int lsiv_phase_i = lsiv_phase; lsiv_phase_i <= numOfPhasesBeforeClick + 1; lsiv_phase_i++) {
            if (lsiv_phase <= PARAMS.TEXAS_MODEL_HPH) {
                cbo_hardware[lsiv_phase_i][PED_MVMT].removeActionListener(componentActionListener);
                cbo_hardware[lsiv_phase_i][PED_MVMT].removeKeyListener(componentKeyListener);
                cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].removeActionListener(componentActionListener);
                cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].removeKeyListener(componentKeyListener);
            }
        }

        setStatus(numOfPhasesBeforeClick + 1);
        setValueAfterAdd(numOfPhasesBeforeClick, lsiv_phase);
        setStatus(numOfPhasesBeforeClick + 1);

        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(getNumberOfPhases()));

        for (int lsiv_phase_i = lsiv_phase; lsiv_phase_i <= numOfPhasesBeforeClick + 1; lsiv_phase_i++) {
            if (lsiv_phase <= PARAMS.TEXAS_MODEL_HPH) {
                cbo_hardware[lsiv_phase_i][PED_MVMT].addActionListener(componentActionListener);
                cbo_hardware[lsiv_phase_i][PED_MVMT].addKeyListener(componentKeyListener);
                cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].addActionListener(componentActionListener);
                cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].addKeyListener(componentKeyListener);
            }
        }
    } // end of method AddAction

    void DelAction(int lsiv_phase) {
        int numOfPhasesBeforeClick = getNumberOfPhases();

        for (int lsiv_phase_i = lsiv_phase; lsiv_phase_i <= numOfPhasesBeforeClick; lsiv_phase_i++) {
            cbo_hardware[lsiv_phase_i][PED_MVMT].removeActionListener(componentActionListener);
            cbo_hardware[lsiv_phase_i][PED_MVMT].removeKeyListener(componentKeyListener);
            cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].removeActionListener(componentActionListener);
            cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].removeKeyListener(componentKeyListener);
        }

        setValueAfterDel(numOfPhasesBeforeClick, lsiv_phase);
        setStatus(numOfPhasesBeforeClick - 1);

        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(getNumberOfPhases()));

        if (!del[lsiv_phase].isEnabled()) {
            okButton.requestFocus();
        }

        for (int lsiv_phase_i = lsiv_phase; lsiv_phase_i <= numOfPhasesBeforeClick; lsiv_phase_i++) {
            cbo_hardware[lsiv_phase_i][PED_MVMT].addActionListener(componentActionListener);
            cbo_hardware[lsiv_phase_i][PED_MVMT].addKeyListener(componentKeyListener);
            cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].addActionListener(componentActionListener);
            cbo_hardware[lsiv_phase_i][PED_MVMT_DIST].addKeyListener(componentKeyListener);
        }
    } // end of method DelAction

    void UpAction(int lsiv_phase) {
        cbo_hardware[lsiv_phase][PED_MVMT].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT].removeKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].removeKeyListener(componentKeyListener);

        cbo_hardware[lsiv_phase - 1][PED_MVMT].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase - 1][PED_MVMT].removeKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase - 1][PED_MVMT_DIST].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase - 1][PED_MVMT_DIST].removeKeyListener(componentKeyListener);

        setValueAfterUp(lsiv_phase);
        setStatus(getNumberOfPhases());

        cbo_hardware[lsiv_phase][PED_MVMT].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT].addKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].addKeyListener(componentKeyListener);

        cbo_hardware[lsiv_phase - 1][PED_MVMT].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase - 1][PED_MVMT].addKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase - 1][PED_MVMT_DIST].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase - 1][PED_MVMT_DIST].addKeyListener(componentKeyListener);

    } // end of method UpAction

    void DownAction(int lsiv_phase) {
        cbo_hardware[lsiv_phase][PED_MVMT].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT].removeKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].removeKeyListener(componentKeyListener);

        cbo_hardware[lsiv_phase + 1][PED_MVMT].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase + 1][PED_MVMT].removeKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase + 1][PED_MVMT_DIST].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase + 1][PED_MVMT_DIST].removeKeyListener(componentKeyListener);

        setValueAfterDown(lsiv_phase);
        setStatus(getNumberOfPhases());

        cbo_hardware[lsiv_phase][PED_MVMT].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT].addKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].addKeyListener(componentKeyListener);

        cbo_hardware[lsiv_phase + 1][PED_MVMT].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase + 1][PED_MVMT].addKeyListener(componentKeyListener);
        cbo_hardware[lsiv_phase + 1][PED_MVMT_DIST].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase + 1][PED_MVMT_DIST].addKeyListener(componentKeyListener);

    } // end of method DownAction

    void PedMvmtAction(int lsiv_phase) {
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].removeActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].removeKeyListener(componentKeyListener);

        if (cbo_hardware[lsiv_phase][PED_MVMT].getSelectedItem().toString().equals("YES")) {
            cbo_hardware[lsiv_phase][PED_MVMT_DIST].setEnabled(true);
            cbo_hardware[lsiv_phase][PED_MVMT_VOL].setEnabled(true);
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setEnabled(true);
        }
        else {
            cbo_hardware[lsiv_phase][PED_MVMT_DIST].setEnabled(false);
            cbo_hardware[lsiv_phase][PED_MVMT_VOL].setEnabled(false);
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setEnabled(false);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL];

            cbo_hardware[lsiv_phase][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_DIST]);
            cbo_hardware[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_VOL]));
            cbo_hardware[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_HITL_SIGNAL_PED_MVMT_PAR]));

            resetMovementParameter(lsiv_phase);
        }

        cbo_hardware[lsiv_phase][PED_MVMT_DIST].addActionListener(componentActionListener);
        cbo_hardware[lsiv_phase][PED_MVMT_DIST].addKeyListener(componentKeyListener);

    } // end of method PedMvmtAction

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_nema_ok = true;

                saveData();

                if (event.getSource() == okButton) {
                    aFrame.dispose();
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                if (event.getSource() == add[lsiv_phase]) {
                    AddAction(lsiv_phase);
                    break;
                }
                else if (event.getSource() == del[lsiv_phase]) {
                    DelAction(lsiv_phase);
                    break;
                }
                else if (event.getSource() == up[lsiv_phase]) {
                    UpAction(lsiv_phase);
                    break;
                }
                else if (event.getSource() == down[lsiv_phase]) {
                    DownAction(lsiv_phase);
                    break;
                }
            }

            for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                if (event.getSource() == setAllButton[lsiv_field]) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                        if (lsiv_phase % 2 == 0) {
                            if (cbo_hardware[lsiv_phase][lsiv_field].isEnabled()) {
                                cbo_hardware[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                            }
                        }
                    }
                    break;
                }
            }

            int sumOfPhase = getNumberOfPhases();
            for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                if (event.getSource() == cbo_hardware[lsiv_phase][PED_MVMT]) {
                    PedMvmtAction(lsiv_phase);
                    break;
                }
                else if (event.getSource() == cbo_hardware[lsiv_phase][PED_MVMT_DIST]) {
                    resetMovementParameter(lsiv_phase);
                    break;
                }
            }

            if (event.getSource() == btnNEMAMovement) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != Integer.valueOf(cbo_NOVLP.getSelectedItem().toString()).intValue())
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == gdvsim.gclv_inter.TX_DATA_IS_INVALID))

                {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply Hardware-in-the-Loop Signal Controller Data before accessing the NEMA/HARDWARE Movement Data.", "Warning Message",
                            JOptionPane.WARNING_MESSAGE);
                }
                else {
                    new NemaMovementDialog();
                }
            }

        } // end of actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    gdvsim.flag_nema_ok = true;

                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                    if (event.getSource() == add[lsiv_phase]) {
                        AddAction(lsiv_phase);
                        break;
                    }
                    else if (event.getSource() == del[lsiv_phase]) {
                        DelAction(lsiv_phase);
                        break;
                    }
                    else if (event.getSource() == up[lsiv_phase]) {
                        UpAction(lsiv_phase);
                        break;
                    }
                    else if (event.getSource() == down[lsiv_phase]) {
                        DownAction(lsiv_phase);
                        break;
                    }
                }

                for (int lsiv_field = PED_MVMT; lsiv_field <= PED_MVMT_PAR; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_HPH; lsiv_phase++) {
                            if (lsiv_phase % 2 == 0) {
                                if (cbo_hardware[lsiv_phase][lsiv_field].isEnabled()) {
                                    cbo_hardware[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                                }
                            }
                        }
                        break;
                    }
                }

                int sumOfPhase = getNumberOfPhases();
                for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                    if (event.getSource() == cbo_hardware[lsiv_phase][PED_MVMT]) {
                        PedMvmtAction(lsiv_phase);
                        break;
                    }
                    else if (event.getSource() == cbo_hardware[lsiv_phase][PED_MVMT_DIST]) {
                        resetMovementParameter(lsiv_phase);
                        break;
                    }
                }

                if (event.getSource() == btnNEMAMovement) {
                    if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                            || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                            || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != Integer.valueOf(cbo_NOVLP.getSelectedItem().toString())
                                    .intValue())
                            || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply Hardware-in-the-Loop Signal Controller Data before accessing the NEMA/HARDWARE Movement Data.", "Warning Message",
                                JOptionPane.WARNING_MESSAGE);
                    }
                    else {
                        new NemaMovementDialog();
                    }
                }

            }
        } // end of keyPressed
    } // end of method ComponentKeyListener

} // end of class HardwareDialog

/******************************************************************************/
/* HardwareDialog.java */
/******************************************************************************/
