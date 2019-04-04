package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           PretimedDialog.java                              */
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

class PretimedDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 3;

    int GRN_INT = 1;

    int YEL_CHG = 2;

    int RED_CLR = 3;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JComboBox[][] comboBox_pretime = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1][NUMOFFIELD + 1];

    JComboBox[] cbo_length = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1];

    JComboBox[] setAllComboBox = new JComboBox[NUMOFFIELD + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] setAllButton = new JButton[NUMOFFIELD + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JComboBox cbo_total_length, cbo_total_phase;

    JLabel label_title, label_phase_length, label_total_length, label_total_phase;

    JButton okButton, applyButton, cancelButton;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    int initial_number_of_phases;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    String[][][] greenSeqValue = new String[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    String[][][] greenSeqDefault = new String[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    int[][][] greenSeqStat = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    public PretimedDialog() {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    String defaultValue = " ";

                    if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0 - 1
                            + lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        String lc = " ";
                        lc = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;

                        if (lc.equals("BL") || lc.equals("UN") || lc.equals("YI") || lc.equals("ST")) {
                            defaultValue = "UN";
                        }
                    }

                    greenSeqDefault[lsiv_phase][lsiv_leg][lsiv_lane] = defaultValue;
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                            - 1 + lsiv_phase];

                    if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                            + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase];
                    }
                }
            }
        }

        initial_number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        if (initial_number_of_phases < 2)
            initial_number_of_phases = 2;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL];

        titleString = lclv_tx_fmt.mstv_name.substring(8);

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

        font = new Font("TimesRoman", Font.BOLD, 18);
        font1 = new Font("TimesRoman", Font.BOLD, 14);

        label_title = new JLabel(titleString);
        label_phase_length = new JLabel("Phase Length");
        label_total_length = new JLabel("Cycle Length", JLabel.RIGHT);
        label_total_phase = new JLabel("Total Phases");

        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        int count;
        int int_number;

        lsiv_min = Intersection.nint(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT]);
        lsiv_max = Intersection.nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT]);
        lsiv_inc = Intersection.nint(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT]);

        count = (int)((lsiv_max - lsiv_min) / lsiv_inc) + 1;
        int_number = lsiv_min;
        String[] array_green = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_green[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        lsiv_min = Intersection.nint(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG]);
        lsiv_max = Intersection.nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG]);
        lsiv_inc = Intersection.nint(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG]);

        count = (int)((lsiv_max - lsiv_min) / lsiv_inc) + 1;
        int_number = lsiv_min;
        String[] array_yellow = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_yellow[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        lsiv_min = Intersection.nint(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR]);
        lsiv_max = Intersection.nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR]);
        lsiv_inc = Intersection.nint(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR]);

        count = (int)((lsiv_max - lsiv_min) / lsiv_inc) + 1;
        int_number = lsiv_min;
        String[] array_red = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_red[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        setAllComboBox[GRN_INT] = new JComboBox(array_green);
        setAllComboBox[YEL_CHG] = new JComboBox(array_yellow);
        setAllComboBox[RED_CLR] = new JComboBox(array_red);

        setAllComboBox[GRN_INT].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT])));
        setAllComboBox[YEL_CHG].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG])));
        setAllComboBox[RED_CLR].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR])));

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field] = new JButton("Set All");

            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
        }

        label_field[GRN_INT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT]);
        label_field[YEL_CHG].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG]);
        label_field[RED_CLR].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR]);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_pretime[lsiv_phase][GRN_INT] = new JComboBox(array_green);
            comboBox_pretime[lsiv_phase][YEL_CHG] = new JComboBox(array_yellow);
            comboBox_pretime[lsiv_phase][RED_CLR] = new JComboBox(array_red);

            comboBox_pretime[lsiv_phase][GRN_INT].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT])));
            comboBox_pretime[lsiv_phase][YEL_CHG].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG])));
            comboBox_pretime[lsiv_phase][RED_CLR].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR])));
        }

        if (gdvsim.flag_pretimed_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_pretime[lsiv_phase][GRN_INT].setSelectedItem(Integer.toString(Intersection
                            .nint(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_grn_int)));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_pretime[lsiv_phase][YEL_CHG].setSelectedItem(Integer.toString(Intersection
                            .nint(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_yel_chg)));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_pretime[lsiv_phase][RED_CLR].setSelectedItem(Integer.toString(Intersection
                            .nint(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_red_clr)));
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("Phase " + lsiv_phase);

            add[lsiv_phase] = new JButton("Add");
            del[lsiv_phase] = new JButton("Delete");
            up[lsiv_phase] = new JButton("Up");
            down[lsiv_phase] = new JButton("Down");
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            cbo_length[lsiv_phase] = new JComboBox();
        }

        cbo_total_length = new JComboBox();
        cbo_total_length.addItem(Integer.toString(getCycleLength()));

        cbo_total_phase = new JComboBox();
        cbo_total_phase.addItem(Integer.toString(initial_number_of_phases));

        setStatus(initial_number_of_phases);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            cbo_length[lsiv_phase].addItem(Integer.toString(getPhaseLength(lsiv_phase)));
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for set all");
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for set all");

            setAllButton[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for set all");
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for set all");
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_pretime[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for phase " + lsiv_phase);
                comboBox_pretime[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for phase " + lsiv_phase);
            }

            add[lsiv_phase].getAccessibleContext().setAccessibleName("Add    for phase " + lsiv_phase);
            add[lsiv_phase].getAccessibleContext().setAccessibleDescription("Add    for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleName("Delete for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleDescription("Delete for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleName("Up     for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleDescription("Up     for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleName("Down   for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleDescription("Down   for phase " + lsiv_phase);
            cbo_length[lsiv_phase].getAccessibleContext().setAccessibleName("length  for phase " + lsiv_phase);
            cbo_length[lsiv_phase].getAccessibleContext().setAccessibleDescription("length  for phase " + lsiv_phase);
        }

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        cbo_total_length.getAccessibleContext().setAccessibleName(label_total_length.getText());
        cbo_total_length.getAccessibleContext().setAccessibleDescription(label_total_length.getText());

        cbo_total_phase.getAccessibleContext().setAccessibleName(label_total_phase.getText());
        cbo_total_phase.getAccessibleContext().setAccessibleDescription(label_total_phase.getText());

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

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field].addActionListener(componentActionListener);
            setAllButton[lsiv_field].addKeyListener(componentKeyListener);
            setAllButton[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].addKeyListener(openComboMenuListener);
            setAllComboBox[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_pretime[lsiv_phase][lsiv_field].addActionListener(componentActionListener);
                comboBox_pretime[lsiv_phase][lsiv_field].addKeyListener(componentKeyListener);
                comboBox_pretime[lsiv_phase][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_pretime[lsiv_phase][lsiv_field].addKeyListener(helpListener);
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
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
            cbo_length[lsiv_phase].addKeyListener(helpListener);
        }

        cbo_total_length.addKeyListener(helpListener);
        cbo_total_phase.addKeyListener(helpListener);
        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);
        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);
        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        gbConstraints.insets = new Insets(2, 2, 20, 2);

        int iRow = 0;
        int numOfColumns = 9;

        addComponent(panel_title, iRow++, 0, 9, 1, 0);

        gbConstraints.insets = new Insets(2, 2, 2, 2);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllComboBox[lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllButton[lsiv_field], iRow, 4 + lsiv_field, 1, 1, 0);
        }

        addComponent(label_phase_length, iRow++, 8, 1, 1, 0);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            addComponent(label_phase[lsiv_phase], iRow, 0, 1, 1, 0);
            addComponent(add[lsiv_phase], iRow, 1, 1, 1, 0);
            addComponent(del[lsiv_phase], iRow, 2, 1, 1, 0);
            addComponent(up[lsiv_phase], iRow, 3, 1, 1, 0);
            addComponent(down[lsiv_phase], iRow, 4, 1, 1, 0);
            addComponent(comboBox_pretime[lsiv_phase][GRN_INT], iRow, 5, 1, 1, 0);
            addComponent(comboBox_pretime[lsiv_phase][YEL_CHG], iRow, 6, 1, 1, 0);
            addComponent(comboBox_pretime[lsiv_phase][RED_CLR], iRow, 7, 1, 1, 0);
            addComponent(cbo_length[lsiv_phase], iRow++, 8, 1, 1, 0);
        }

        addComponent(cbo_total_phase, iRow, 1, 1, 1, 0);
        addComponent(label_total_phase, iRow, 2, 2, 1, 0);
        addComponent(label_total_length, iRow, 6, 2, 1, 0);
        addComponent(cbo_total_length, iRow++, 8, 1, 1, 0);
        addComponent(ok_panel, iRow, 0, 9, 1, 0);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method PretimedDialog

    void addComponent(Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;
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
    } // end of class OpenComboMenuListener

    int getPhaseLength(int phase_number) {
        int sum = 0;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (comboBox_pretime[phase_number][lsiv_field].isEnabled()) {
                sum = sum + Integer.valueOf(comboBox_pretime[phase_number][lsiv_field].getSelectedItem().toString()).intValue();
            }
        }

        return sum;
    } // end of method getPhaseLength

    int getCycleLength() {
        int sum = 0;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (comboBox_pretime[lsiv_phase][lsiv_field].isEnabled()) {
                    sum = sum + Integer.valueOf(comboBox_pretime[lsiv_phase][lsiv_field].getSelectedItem().toString()).intValue();
                }
            }
        }

        return sum;
    } // end of method getCycleLength

    int getNumberOfPhase() {
        int numOfPh = 0;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (comboBox_pretime[lsiv_phase][GRN_INT].isEnabled()) {
                numOfPh++;
            }
        }

        return numOfPh;
    } // end of method getNumberOfPhase

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
                else if (event.getSource() == cbo_total_length) {
                    int max = 0;
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        max = max + Intersection.nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field]);
                    }

                    new HelpDialog(true, label_total_length.getText(), label_total_length.getText(), "The user cannot specify this item.  This item is the " + label_total_length.getText()
                            + " and is determined by the program.", cbo_total_length.getSelectedItem().toString(), "0", " ", "0", Integer.toString(max * PARAMS.TEXAS_MODEL_NPH), "1");
                }
                else if (event.getSource() == cbo_total_phase) {
                    new HelpDialog(true, label_total_phase.getText(), label_total_phase.getText(), "The user cannot specify this item.  This item is the " + label_total_phase.getText()
                            + " and is determined by the program.", cbo_total_phase.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NPH), "1");
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
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
                    else if (event.getSource() == cbo_length[lsiv_phase]) {
                        int max = 0;
                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            max = max + Intersection.nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field]);
                        }

                        new HelpDialog(true, "Length for phase " + lsiv_phase, "Length for phase " + lsiv_phase, "The user cannot specify this item.  This item is the Length for phase " + lsiv_phase
                                + " and is determined by the program.", cbo_length[lsiv_phase].getSelectedItem().toString(), "0", " ", "0", Integer.toString(max), "1");
                    }
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        new HelpDialog(true, "Set All Button", "The Set All button sets the value of " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field]
                                + " for all phases to the value selected above.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for all phases",
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])), " ", Integer.toString(Intersection
                                        .nint(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])),
                                Integer.toString(Intersection
                                        .nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])),
                                Integer.toString(Intersection
                                        .nint(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])));
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_pretime[lsiv_phase][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(1)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field] + " for Phase " + lsiv_phase,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field], comboBox_pretime[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                    Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])), " ", Integer.toString(Intersection
                                            .nint(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])),
                                    Integer.toString(Intersection
                                            .nint(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])),
                                    Integer.toString(Intersection
                                            .nint(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT - 1 + lsiv_field])));
                        }
                    }
                }
            }
        }
    } // end of class HelpListener

    void setStatus(int sumOfPhases) {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                label_phase[lsiv_phase].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_pretime[lsiv_phase][lsiv_field].setEnabled(true);
                }

                cbo_length[lsiv_phase].setEnabled(true);
            }
            else {
                label_phase[lsiv_phase].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_pretime[lsiv_phase][lsiv_field].setEnabled(false);
                }

                cbo_length[lsiv_phase].setEnabled(false);
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_NPH) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                add[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (lsiv_phase <= (sumOfPhases + 1)) {
                    add[lsiv_phase].setEnabled(true);
                }
                else {
                    add[lsiv_phase].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_NPH) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                del[lsiv_phase].setEnabled(true);
            }
        }
        else if (sumOfPhases <= 2) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                del[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (lsiv_phase <= sumOfPhases) {
                    del[lsiv_phase].setEnabled(true);
                }
                else {
                    del[lsiv_phase].setEnabled(false);
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                up[lsiv_phase].setEnabled(true);
            }
            else {
                up[lsiv_phase].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase < sumOfPhases) {
                down[lsiv_phase].setEnabled(true);
            }
            else {
                down[lsiv_phase].setEnabled(false);
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            cbo_length[lsiv_phase].removeAllItems();
            cbo_length[lsiv_phase].addItem(Integer.toString(getPhaseLength(lsiv_phase)));
        }

        cbo_total_length.removeAllItems();
        cbo_total_length.addItem(Integer.toString(getCycleLength()));

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                cbo_length[lsiv_phase].setEnabled(true);
            }
            else {
                cbo_length[lsiv_phase].setEnabled(false);
            }
        }

    } // end of method setStatus

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_pretime[lsiv_phase + 1][lsiv_field].setSelectedItem(comboBox_pretime[lsiv_phase][lsiv_field].getSelectedItem().toString());
            }
        }

        for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane] = greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                        greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                }
            }
        }
    } // end of method setValueAfterAdd

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_pretime[lsiv_phase][lsiv_field].setSelectedItem(comboBox_pretime[lsiv_phase + 1][lsiv_field].getSelectedItem().toString());
            }
        }

        comboBox_pretime[sum][GRN_INT].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT])));
        comboBox_pretime[sum][YEL_CHG].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG])));
        comboBox_pretime[sum][RED_CLR].setSelectedItem(Integer.toString(Intersection.nint(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR])));

        for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane];
                        greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                }
            }
        }

        for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                if (greenSeqStat[sum][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    greenSeqValue[sum][lsiv_leg][lsiv_lane] = greenSeqDefault[sum][lsiv_leg][lsiv_lane];
                    greenSeqStat[sum][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }
    } // end of method setValueAfterDel

    void AddAction(int lsiv_phase) {
        int numOfPhasesBeforeClick;
        numOfPhasesBeforeClick = getNumberOfPhase();
        setStatus(numOfPhasesBeforeClick + 1);
        setValueAfterAdd(numOfPhasesBeforeClick, lsiv_phase);
        cbo_total_phase.removeAllItems();
        cbo_total_phase.addItem(Integer.toString(getNumberOfPhase()));
    } // end of method AddAction

    void DelAction(int lsiv_phase) {
        int numOfPhasesBeforeClick;
        numOfPhasesBeforeClick = getNumberOfPhase();
        setValueAfterDel(numOfPhasesBeforeClick, lsiv_phase);
        setStatus(numOfPhasesBeforeClick - 1);
        cbo_total_phase.removeAllItems();
        cbo_total_phase.addItem(Integer.toString(getNumberOfPhase()));
        if (!del[lsiv_phase].isEnabled()) {
            okButton.requestFocus();
        }
    } // end of method DelAction

    void UpAction(int lsiv_phase) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String temp;
            temp = comboBox_pretime[lsiv_phase][lsiv_field].getSelectedItem().toString();
            comboBox_pretime[lsiv_phase][lsiv_field].setSelectedItem(comboBox_pretime[lsiv_phase - 1][lsiv_field].getSelectedItem().toString());
            comboBox_pretime[lsiv_phase - 1][lsiv_field].setSelectedItem(temp);
        }

        for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                if (greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    String temp;
                    temp = greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                    greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = greenSeqValue[lsiv_phase - 1][lsiv_leg][lsiv_lane];
                    greenSeqValue[lsiv_phase - 1][lsiv_leg][lsiv_lane] = temp;
                }
            }
        }
    } // end of method UpAction

    void DownAction(int lsiv_phase) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String temp;
            temp = comboBox_pretime[lsiv_phase][lsiv_field].getSelectedItem().toString();
            comboBox_pretime[lsiv_phase][lsiv_field].setSelectedItem(comboBox_pretime[lsiv_phase + 1][lsiv_field].getSelectedItem().toString());
            comboBox_pretime[lsiv_phase + 1][lsiv_field].setSelectedItem(temp);
        }

        for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                if (greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    String temp;
                    temp = greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                    greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane];
                    greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane] = temp;
                }
            }
        }
    } // end of method DownAction

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = getNumberOfPhase();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_FROM_USER;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ];

        for (int lsiv_phase = 1; lsiv_phase <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase].msia_ph_sequence[1] = lsiv_phase + 1;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01] = gdvsim.gclv_inter.TX_FROM_USER;
            for (int lsiv_seq = 2; lsiv_seq <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_seq++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase].msia_ph_sequence[lsiv_seq] = 0;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1
                        + lsiv_seq] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases].msia_ph_sequence[1] = 1;
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        for (int lsiv_phase = 1; lsiv_phase <= getNumberOfPhase(); lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_grn_int = Double.valueOf(
                    comboBox_pretime[lsiv_phase][GRN_INT].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_yel_chg = Double.valueOf(
                    comboBox_pretime[lsiv_phase][YEL_CHG].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_pre_red_clr = Double.valueOf(
                    comboBox_pretime[lsiv_phase][RED_CLR].getSelectedItem().toString()).doubleValue();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_GRN_INT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_YEL_CHG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PRET_SIGNAL_RED_CLR] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                            + lsiv_phase] = greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane];

                    if (greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                    }
                }
            }
        }
    } // end of method saveData()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_pretimed_ok = true;

                saveData();

                if (event.getSource() == okButton) {
                    aFrame.dispose();
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
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

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == comboBox_pretime[lsiv_phase][lsiv_field]) {
                        cbo_length[lsiv_phase].removeAllItems();
                        cbo_length[lsiv_phase].addItem(Integer.toString(getPhaseLength(lsiv_phase)));
                        cbo_total_length.removeAllItems();
                        cbo_total_length.addItem(Integer.toString(getCycleLength()));
                        break;
                    }
                }
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (event.getSource() == setAllButton[lsiv_field]) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                        comboBox_pretime[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                    }
                    break;
                }
            }
        }
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    gdvsim.flag_pretimed_ok = true;

                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
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

                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_pretime[lsiv_phase][lsiv_field]) {
                            cbo_length[lsiv_phase].removeAllItems();
                            cbo_length[lsiv_phase].addItem(Integer.toString(getPhaseLength(lsiv_phase)));
                            cbo_total_length.removeAllItems();
                            cbo_total_length.addItem(Integer.toString(getCycleLength()));
                            break;
                        }
                    }
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                            comboBox_pretime[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                        }
                        break;
                    }
                }
            }
        } // end of keyPressed
    } // end of method ComponentKeyListener

} // end of class PretimedDialog
