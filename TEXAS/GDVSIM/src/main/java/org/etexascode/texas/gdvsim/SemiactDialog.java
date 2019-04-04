package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              SemiactDialog.java                            */
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
import java.text.DecimalFormat;
import javax.swing.*;

class SemiactDialog extends JDialog {

    int NUMOFFIELD = 9;

    int NUMOFFLDOFPH1 = 3;

    int ACT_INI_INT = 1;

    int ACT_VEH_INT = 2;

    int ACT_YEL_CHG = 3;

    int ACT_RED_CLR = 4;

    int ACT_MAX_EXT = 5;

    int ACT_SKIP_PH = 6;

    int ACT_RECALL = 7;

    int ACT_MINOR = 8;

    int ACT_DUAL_LT = 9;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JComboBox[][] comboBox_semiact = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1][NUMOFFIELD + 1];

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPH + 1];

    JTextArea[] lbl_ph1_fld = new JTextArea[NUMOFFLDOFPH1 + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JButton[] setAllButton = new JButton[NUMOFFIELD + 1];

    JComboBox[] setAllComboBox = new JComboBox[NUMOFFIELD + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NPH + 1];

    JComboBox[][] comboBox_phaseSeq = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1][PARAMS.TEXAS_MODEL_NPH];

    JLabel[] label_phase_col1 = new JLabel[PARAMS.TEXAS_MODEL_NPH];

    JLabel[] label_phase_col2 = new JLabel[PARAMS.TEXAS_MODEL_NPH];

    JLabel label_title_actuated, label_title_nonactuated, label_total;

    JTextArea label_sequence1, label_sequence2;

    JButton okButton, applyButton, cancelButton;

    JComboBox cbo_total;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    int index;

    int initial_number_of_phases;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    String[][][] greenSeqDefault = new String[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    int[][] detConnNonNemaDefault = new int[PARAMS.TEXAS_MODEL_NPH + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    String[] detConnTypeNonNemaDefault = new String[PARAMS.TEXAS_MODEL_NPH + 1];

    int[][] temp_phaseSeq = new int[PARAMS.TEXAS_MODEL_NPH + 1][PARAMS.TEXAS_MODEL_NPH];

    public SemiactDialog() {
        if (gdvsim.flag_greenSequence_ok) {
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
                        gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                - 1 + lsiv_phase];

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                - 1 + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase];
                        }
                        else {
                            gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = greenSeqDefault[lsiv_phase][lsiv_leg][lsiv_lane];
                        }
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNonNema_ok) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA];

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                detConnTypeNonNemaDefault[lsiv_phase] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1 + lsiv_phase];
                gdvsim.detConnTypeNonNemaStat[lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE
                        - 1 + lsiv_phase];

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    detConnNonNemaDefault[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det];
                    gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01
                            - 1 + lsiv_det];
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1
                        + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnTypeNonNemaValue[lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type;
                }
                else {
                    gdvsim.detConnTypeNonNemaValue[lsiv_phase] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1 + lsiv_phase];
                }

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                            + lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det];
                    }
                    else {
                        gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det];
                    }
                }
            }
        }

        initial_number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        if (initial_number_of_phases < 2)
            initial_number_of_phases = 2;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL];

        titleString = "Semi-Actuated Signal Controller Timing Data";

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

        label_title_actuated = new JLabel(lclv_tx_fmt.mstv_name.substring(8));
        label_total = new JLabel("Total Phases");
        label_title_actuated.setFont(font);
        JPanel panel_title_actuated = new JPanel();
        panel_title_actuated.add(label_title_actuated);

        JPanel panel_numOfPhase = new JPanel();
        panel_numOfPhase.add(label_total);

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int count;
        double double_number;

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_initial = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_initial[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_vehicle = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_vehicle[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_yellow = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_yellow[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_red = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_red[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_extension = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_extension[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        String[] array_skipPhase = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH].substring(1).split("\\|");
        String[] array_recall = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL].substring(1).split("\\|");
        String[] array_minor = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR].substring(1).split("\\|");
        String[] array_dualLeft = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT].substring(1).split("\\|");

        setAllComboBox[ACT_INI_INT] = new JComboBox(array_initial);
        setAllComboBox[ACT_VEH_INT] = new JComboBox(array_vehicle);
        setAllComboBox[ACT_YEL_CHG] = new JComboBox(array_yellow);
        setAllComboBox[ACT_RED_CLR] = new JComboBox(array_red);
        setAllComboBox[ACT_MAX_EXT] = new JComboBox(array_extension);
        setAllComboBox[ACT_SKIP_PH] = new JComboBox(array_skipPhase);
        setAllComboBox[ACT_RECALL] = new JComboBox(array_recall);
        setAllComboBox[ACT_MINOR] = new JComboBox(array_minor);
        setAllComboBox[ACT_DUAL_LT] = new JComboBox(array_dualLeft);

        setAllComboBox[ACT_INI_INT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT]));
        setAllComboBox[ACT_VEH_INT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT]));
        setAllComboBox[ACT_YEL_CHG].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG]));
        setAllComboBox[ACT_RED_CLR].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR]));
        setAllComboBox[ACT_MAX_EXT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT]));
        setAllComboBox[ACT_SKIP_PH].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH]);
        setAllComboBox[ACT_RECALL].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL]);
        setAllComboBox[ACT_MINOR].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR]);
        setAllComboBox[ACT_DUAL_LT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT]);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field] = new JButton("Set All");
            setAllButton[lsiv_field].setSize(new Dimension(10, 26));

            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
            label_field[lsiv_field].setSize(new Dimension(10, 26));
        }

        label_field[ACT_INI_INT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT]);
        label_field[ACT_VEH_INT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT]);
        label_field[ACT_YEL_CHG].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG]);
        label_field[ACT_RED_CLR].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR]);
        label_field[ACT_MAX_EXT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT]);
        label_field[ACT_SKIP_PH].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH]);
        label_field[ACT_RECALL].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL]);
        label_field[ACT_MINOR].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR]);
        label_field[ACT_DUAL_LT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT]);

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            comboBox_semiact[lsiv_phase][ACT_INI_INT] = new JComboBox(array_initial);
            comboBox_semiact[lsiv_phase][ACT_VEH_INT] = new JComboBox(array_vehicle);
            comboBox_semiact[lsiv_phase][ACT_YEL_CHG] = new JComboBox(array_yellow);
            comboBox_semiact[lsiv_phase][ACT_RED_CLR] = new JComboBox(array_red);
            comboBox_semiact[lsiv_phase][ACT_MAX_EXT] = new JComboBox(array_extension);
            comboBox_semiact[lsiv_phase][ACT_SKIP_PH] = new JComboBox(array_skipPhase);
            comboBox_semiact[lsiv_phase][ACT_RECALL] = new JComboBox(array_recall);
            comboBox_semiact[lsiv_phase][ACT_MINOR] = new JComboBox(array_minor);
            comboBox_semiact[lsiv_phase][ACT_DUAL_LT] = new JComboBox(array_dualLeft);

            comboBox_semiact[lsiv_phase][ACT_INI_INT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT]));
            comboBox_semiact[lsiv_phase][ACT_VEH_INT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT]));
            comboBox_semiact[lsiv_phase][ACT_YEL_CHG].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG]));
            comboBox_semiact[lsiv_phase][ACT_RED_CLR].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR]));
            comboBox_semiact[lsiv_phase][ACT_MAX_EXT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT]));
            comboBox_semiact[lsiv_phase][ACT_SKIP_PH].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH]);
            comboBox_semiact[lsiv_phase][ACT_RECALL].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL]);
            comboBox_semiact[lsiv_phase][ACT_MINOR].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR]);
            comboBox_semiact[lsiv_phase][ACT_DUAL_LT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT]);
        }

        if (gdvsim.flag_semiact_ok) {
            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_INI_INT].setSelectedItem(Double
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_VEH_INT].setSelectedItem(Double
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_YEL_CHG].setSelectedItem(Double
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_RED_CLR].setSelectedItem(Double
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_MAX_EXT].setSelectedItem(Double
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_SKIP_PH].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_skip_ph);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_RECALL].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_recall);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_MINOR].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_minor);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_semiact[lsiv_phase][ACT_DUAL_LT].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_dual_lt);
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL];

        label_title_nonactuated = new JLabel(lclv_tx_fmt.mstv_name.substring(8));
        label_title_nonactuated.setFont(font);
        JPanel panel_title_nonactuated = new JPanel();
        panel_title_nonactuated.add(label_title_nonactuated);

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_phase_1_green = new String[count];
        for (index = 0; index < count; index++) {
            array_phase_1_green[index] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_phase_1_yellow = new String[count];
        for (index = 0; index < count; index++) {
            array_phase_1_yellow[index] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_phase_1_red = new String[count];
        for (index = 0; index < count; index++) {
            array_phase_1_red[index] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        comboBox_semiact[1][ACT_INI_INT] = new JComboBox(array_phase_1_green);
        comboBox_semiact[1][ACT_VEH_INT] = new JComboBox(array_phase_1_yellow);
        comboBox_semiact[1][ACT_YEL_CHG] = new JComboBox(array_phase_1_red);

        comboBox_semiact[1][ACT_INI_INT].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN]));
        comboBox_semiact[1][ACT_VEH_INT].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG]));
        comboBox_semiact[1][ACT_YEL_CHG].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR]));

        if (gdvsim.flag_semiact_ok) {
            comboBox_semiact[1][ACT_INI_INT].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mdfv_una_min_grn));
            comboBox_semiact[1][ACT_VEH_INT].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mdfv_una_yel_chg));
            comboBox_semiact[1][ACT_YEL_CHG].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mdfv_una_red_clr));
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFLDOFPH1; lsiv_field++) {
            lbl_ph1_fld[lsiv_field] = new JTextArea();
            lbl_ph1_fld[lsiv_field].setBackground(aFrame.getBackground());
            lbl_ph1_fld[lsiv_field].setEditable(false);
            lbl_ph1_fld[lsiv_field].setFocusable(false);
            lbl_ph1_fld[lsiv_field].setWrapStyleWord(true);
            lbl_ph1_fld[lsiv_field].setLineWrap(true);
            lbl_ph1_fld[lsiv_field].setFont(font1);
            lbl_ph1_fld[lsiv_field].setSize(new Dimension(10, 26));
        }

        lbl_ph1_fld[ACT_INI_INT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN]);
        lbl_ph1_fld[ACT_VEH_INT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG]);
        lbl_ph1_fld[ACT_YEL_CHG].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR]);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("Phase " + lsiv_phase);
            add[lsiv_phase] = new JButton("Add");
            del[lsiv_phase] = new JButton("Delete");
            up[lsiv_phase] = new JButton("Up");
            down[lsiv_phase] = new JButton("Down");
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH - 1; lsiv_phase++) {
            label_phase_col1[lsiv_phase] = new JLabel("Phase " + lsiv_phase);
            label_phase_col1[lsiv_phase].setVerticalAlignment(JLabel.BOTTOM);
            label_phase_col2[lsiv_phase] = new JLabel("Phase " + lsiv_phase);
            label_phase_col2[lsiv_phase].setVerticalAlignment(JLabel.BOTTOM);
        }

        label_sequence1 = new JTextArea("Priority Sequence of Phases to enter after Termination");
        label_sequence1.setBackground(aFrame.getBackground());
        label_sequence1.setEditable(false);
        label_sequence1.setFocusable(false);
        label_sequence1.setWrapStyleWord(true);
        label_sequence1.setLineWrap(true);
        label_sequence1.setFont(font1);

        label_sequence2 = new JTextArea("Priority Sequence of Phases to enter after Termination");
        label_sequence2.setBackground(aFrame.getBackground());
        label_sequence2.setEditable(false);
        label_sequence2.setFocusable(false);
        label_sequence2.setWrapStyleWord(true);
        label_sequence2.setLineWrap(true);
        label_sequence2.setFont(font1);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(initial_number_of_phases));

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                if ((lsiv_phase_row <= initial_number_of_phases) && (lsiv_phase_col <= initial_number_of_phases)) {
                    if (lsiv_phase_col == 1) {
                        String[] arr = new String[initial_number_of_phases - 1];
                        int index = 0;
                        for (int lsiv_phase = 1; lsiv_phase <= initial_number_of_phases; lsiv_phase++) {
                            if (lsiv_phase_row == lsiv_phase)
                                continue;
                            arr[index++] = Integer.toString(lsiv_phase);
                        }
                        comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col] = new JComboBox(arr);
                    }
                    else {
                        String[] arr = new String[initial_number_of_phases];
                        int index = 0;
                        for (int lsiv_phase = 0; lsiv_phase <= initial_number_of_phases; lsiv_phase++) {
                            if (lsiv_phase_row == lsiv_phase)
                                continue;
                            arr[index++] = Integer.toString(lsiv_phase);
                        }
                        comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col] = new JComboBox(arr);
                    }
                }
                else {
                    String[] arr = new String[PARAMS.TEXAS_MODEL_NPH + 1];
                    for (int lsiv_phase = 0; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                        arr[lsiv_phase] = Integer.toString(lsiv_phase);
                    }
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col] = new JComboBox(arr);
                }
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= initial_number_of_phases; lsiv_phase_row++) {
            comboBox_phaseSeq[lsiv_phase_row][1].setSelectedItem(Integer.toString(getColumnOnePhaseSequenceDefault(initial_number_of_phases, lsiv_phase_row)));
        }

        if (gdvsim.flag_semiact_ok) {
            for (int lsiv_phase_row = 1; lsiv_phase_row <= initial_number_of_phases; lsiv_phase_row++) {
                for (int lsiv_phase_col = 1; lsiv_phase_col <= initial_number_of_phases; lsiv_phase_col++) {
                    if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01
                                - 1 + lsiv_phase_col] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setSelectedItem(Integer
                                    .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].msia_ph_sequence[lsiv_phase_col]));
                        }
                    }
                }
            }
        }

        setStatus(initial_number_of_phases);

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

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

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field].addActionListener(componentActionListener);
            setAllButton[lsiv_field].addKeyListener(componentKeyListener);
            setAllButton[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].addKeyListener(openComboMenuListener);
            setAllComboBox[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFLDOFPH1; lsiv_field++) {
            comboBox_semiact[1][lsiv_field].addKeyListener(openComboMenuListener);
            comboBox_semiact[1][lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_semiact[lsiv_phase][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_semiact[lsiv_phase][lsiv_field].addKeyListener(helpListener);
            }
        }

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
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
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addActionListener(componentActionListener);
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addKeyListener(componentKeyListener);
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addKeyListener(openComboMenuListener);
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addKeyListener(helpListener);
            }
        }

        cbo_total.addKeyListener(helpListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        setAccessiblity();

        gbConstraints.insets = new Insets(2, 2, 20, 2);

        int iRow = 0;
        int numOfColumns = 14;
        int ic;

        addComponent(panel_title_nonactuated, iRow++, 0, 14, 1, 0);

        gbConstraints.insets = new Insets(2, 2, 2, 2);
        ic = 5;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFLDOFPH1; lsiv_field++) {
            addComponent(lbl_ph1_fld[lsiv_field], iRow, ic++, 1, 2, 0);
        }
        ic = 5 + NUMOFFIELD;
        addComponent(label_sequence1, iRow++, ic, 7, 1, 0);

        ic = 5 + NUMOFFIELD;
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH - 1; lsiv_phase++) {
            addComponent(label_phase_col1[lsiv_phase], iRow, ic++, 1, 1, 0);
        }
        iRow++;

        addComponent(label_phase[1], iRow, 0, 1, 1, 0);

        ic = 5;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFLDOFPH1; lsiv_field++) {
            addComponent(comboBox_semiact[1][lsiv_field], iRow, ic++, 1, 1, 0);
        }
        ic = 5 + NUMOFFIELD;
        for (int lsiv_phase_i = 1; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NPH - 1; lsiv_phase_i++) {
            addComponent(comboBox_phaseSeq[1][lsiv_phase_i], iRow, ic++, 1, 1, 0);
        }
        iRow++;

        gbConstraints.insets = new Insets(20, 2, 20, 2);

        addComponent(panel_title_actuated, iRow++, 0, 14, 1, 0);
        iRow++;

        gbConstraints.insets = new Insets(2, 2, 2, 2);

        ic = 5;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, ic++, 1, 1, 0);
        }
        addComponent(label_sequence2, iRow++, ic, 7, 1, 0);

        ic = 5;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllComboBox[lsiv_field], iRow, ic++, 1, 1, 0);
        }
        iRow++;

        ic = 5;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllButton[lsiv_field], iRow, ic++, 1, 1, 0);
        }
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH - 1; lsiv_phase++) {
            addComponent(label_phase_col2[lsiv_phase], iRow, ic++, 1, 1, 0);
        }
        iRow++;

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            addComponent(label_phase[lsiv_phase], iRow, 0, 1, 1, 0);
            addComponent(add[lsiv_phase], iRow, 1, 1, 1, 0);
            addComponent(del[lsiv_phase], iRow, 2, 1, 1, 0);
            addComponent(up[lsiv_phase], iRow, 3, 1, 1, 0);
            addComponent(down[lsiv_phase], iRow, 4, 1, 1, 0);

            ic = 5;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(comboBox_semiact[lsiv_phase][lsiv_field], iRow, ic++, 1, 1, 0);
            }
            for (int lsiv_phase_i = 1; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NPH - 1; lsiv_phase_i++) {
                addComponent(comboBox_phaseSeq[lsiv_phase][lsiv_phase_i], iRow, ic++, 1, 1, 0);
            }
            iRow++;
        }

        addComponent(label_total, iRow, 0, 1, 1, 0);
        addComponent(cbo_total, iRow++, 1, 1, 1, 0);
        addComponent(ok_panel, iRow, 0, 14, 1, 0);

        aFrame.setSize(1000, 680);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    } // end of method SemiactDialog

    void addComponent(Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;

        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getColumnOnePhaseSequenceDefault(int sumOfPhase, int lsiv_phase) {
        if (sumOfPhase == lsiv_phase)
            return 1;
        return lsiv_phase + 1;
    } // end of method setPhaseSequenceColumnOneDefault()

    void setAccessiblity() {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for set all");
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for set all");
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for set all");
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for set all");
        }

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_semiact[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for phase " + lsiv_phase);
                comboBox_semiact[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for phase " + lsiv_phase);
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFLDOFPH1; lsiv_field++) {
            comboBox_semiact[1][lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field] + " for phase 1");
            comboBox_semiact[1][lsiv_field].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field] + " for phase 1");
        }

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            add[lsiv_phase].getAccessibleContext().setAccessibleName("Add    for phase " + lsiv_phase);
            add[lsiv_phase].getAccessibleContext().setAccessibleDescription("Add    for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleName("Delete for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleDescription("Delete for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleName("Up     for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleDescription("Up     for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleName("Down   for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleDescription("Down   for phase " + lsiv_phase);
        }

        cbo_total.getAccessibleContext().setAccessibleName("Total Phases");
        cbo_total.getAccessibleContext().setAccessibleDescription("Total Phases");

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ];
        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col].replace("#", Integer.toString(lsiv_phase_row)));
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col].replace("#", Integer.toString(lsiv_phase_row)));
            }
        }
    } // end of method setAccessiblity

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

    int getNumberOfPhase() {
        int numOfPh = 1;

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (comboBox_semiact[lsiv_phase][ACT_INI_INT].isEnabled()) {
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
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the " + label_total.getText()
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NPH), "1");
                }

                for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
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

                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL];

                for (int lsiv_field = 1; lsiv_field <= NUMOFFLDOFPH1; lsiv_field++) {
                    if (event.getSource() == comboBox_semiact[1][lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field] + " for Phase 1",
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field], comboBox_semiact[1][lsiv_field].getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN - 1 + lsiv_field]));
                    }
                }

                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL];

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        new HelpDialog(true, "Set All Button", "The Set All button sets the value of " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]
                                + " for all phases to the value selected above.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }

                for (int lsiv_field = 1; lsiv_field <= 5; lsiv_field++) {
                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for all phases",
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]));
                    }
                }

                for (int lsiv_field = 6; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for all phases",
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT
                                        - 1 + lsiv_field],
                                " ", " ", " ");
                    }
                }

                for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    for (int lsiv_field = 1; lsiv_field <= 5; lsiv_field++) {
                        if (event.getSource() == comboBox_semiact[lsiv_phase][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for Phase " + lsiv_phase,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field],
                                    comboBox_semiact[lsiv_phase][lsiv_field].getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT
                                            - 1 + lsiv_field]),
                                    " ", Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                    Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                    Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field]));
                        }
                    }

                    for (int lsiv_field = 6; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_semiact[lsiv_phase][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for Phase " + lsiv_phase,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field],
                                    comboBox_semiact[lsiv_phase][lsiv_field].getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field],
                                    lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT - 1 + lsiv_field], " ", " ", " ");
                        }
                    }
                }

                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ];
                for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
                    for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                        if (event.getSource() == comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col], lclv_tx_fmt.mstv_name.replace("#",
                                    Integer.toString(lsiv_phase_row)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col], comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col]
                                            .getSelectedItem().toString(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col]), " ",
                                    Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1 + lsiv_phase_col]));
                        }
                    }
                }
            }
        }
    } // end of class HelpListener

    void setStatus(int sumOfPhases) {
        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                label_phase[lsiv_phase].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_semiact[lsiv_phase][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_phase[lsiv_phase].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_semiact[lsiv_phase][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_NPH) {
            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                add[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (lsiv_phase <= (sumOfPhases + 1)) {
                    add[lsiv_phase].setEnabled(true);
                }
                else {
                    add[lsiv_phase].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_NPH) {
            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                del[lsiv_phase].setEnabled(true);
            }
        }
        else if (sumOfPhases <= 2) {
            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                del[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (lsiv_phase <= sumOfPhases) {
                    del[lsiv_phase].setEnabled(true);
                }
                else {
                    del[lsiv_phase].setEnabled(false);
                }
            }
        }

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                up[lsiv_phase].setEnabled(true);
            }
            else {
                up[lsiv_phase].setEnabled(false);
            }

            up[1].setEnabled(false);
            up[2].setEnabled(false);
        }

        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (lsiv_phase < sumOfPhases) {
                down[lsiv_phase].setEnabled(true);
            }
            else {
                down[lsiv_phase].setEnabled(false);
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            if (lsiv_phase_row <= sumOfPhases) {
                for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                    if (lsiv_phase_col == 1) {
                        comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setEnabled(true);
                    }
                    else if (lsiv_phase_col < sumOfPhases) {
                        if (Integer.valueOf(comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col - 1].getSelectedItem().toString()).intValue() == 0) {
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setEnabled(false);
                        }
                        else {
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setEnabled(true);
                        }

                        if (((lsiv_phase_col + 1) <= (sumOfPhases - 1)) && (Integer.valueOf(comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].getSelectedItem().toString()).intValue() != 0)) {
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col + 1].setEnabled(true);
                        }
                    }
                    else {
                        comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setEnabled(false);
                    }
                }
            }
            else {
                for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setEnabled(false);
                }
            }
        }
    } // end of method setStatus

    void setSequenceRowValue(int row) {
        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeActionListener(componentActionListener);
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeKeyListener(componentKeyListener);
                }
            }
        }

        int sumOfPhase = getNumberOfPhase();
        int lastColumn = Math.min(sumOfPhase, PARAMS.TEXAS_MODEL_NPH - 1);

        int start = 1;
        int end = 0;

        for (int col = 1; col <= lastColumn; col++) {
            if (Integer.valueOf(comboBox_phaseSeq[row][col].getSelectedItem().toString()).intValue() == 0) {
                start = col;
                for (int lsiv_phase_col = lastColumn; lsiv_phase_col > col; lsiv_phase_col--) {
                    if (Integer.valueOf(comboBox_phaseSeq[row][lsiv_phase_col].getSelectedItem().toString()).intValue() > 0) {
                        end = lsiv_phase_col;
                        break;
                    }
                }
                break;
            }
        }

        while (end > 0) {
            for (int lsiv_phase_col = start + 1; lsiv_phase_col <= lastColumn; lsiv_phase_col++) {
                int temp = Integer.valueOf(comboBox_phaseSeq[row][lsiv_phase_col].getSelectedItem().toString()).intValue();
                comboBox_phaseSeq[row][lsiv_phase_col - 1].setSelectedItem(Integer.toString(temp));
            }
            comboBox_phaseSeq[row][lastColumn].setSelectedItem(Integer.toString(0));

            start = 1;
            end = 0;

            for (int col = 1; col <= lastColumn; col++) {
                if (Integer.valueOf(comboBox_phaseSeq[row][col].getSelectedItem().toString()).intValue() == 0) {
                    start = col;
                    for (int lsiv_phase_col = lastColumn; lsiv_phase_col > col; lsiv_phase_col--) {
                        if (Integer.valueOf(comboBox_phaseSeq[row][lsiv_phase_col].getSelectedItem().toString()).intValue() > 0) {
                            end = lsiv_phase_col;
                            break;
                        }
                    }
                    break;
                }
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addActionListener(componentActionListener);
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addKeyListener(componentKeyListener);
                }
            }
        }
        setStatus(getNumberOfPhase());

    } // end of method setSequenceRowValue

    void setValueAfterAdd(int sum, int index) {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL];

        for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_semiact[lsiv_phase + 1][lsiv_field].setSelectedItem(comboBox_semiact[lsiv_phase][lsiv_field].getSelectedItem().toString());
            }
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        if (gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                            gdvsim.greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                        }
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNonNema_ok) {
            for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
                if (gdvsim.detConnTypeNonNemaStat[lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnTypeNonNemaValue[lsiv_phase + 1] = gdvsim.detConnTypeNonNemaValue[lsiv_phase];
                    gdvsim.detConnTypeNonNemaStat[lsiv_phase + 1] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.detConnTypeNonNemaValue[lsiv_phase + 1] = detConnTypeNonNemaDefault[lsiv_phase];
                    gdvsim.detConnTypeNonNemaStat[lsiv_phase + 1] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNonNemaValue[lsiv_phase + 1][lsiv_det] = gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det];
                        gdvsim.detConnNonNemaStat[lsiv_phase + 1][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.detConnNonNemaValue[lsiv_phase + 1][lsiv_det] = detConnNonNemaDefault[lsiv_phase][lsiv_det];
                        gdvsim.detConnNonNemaStat[lsiv_phase + 1][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }

            gdvsim.detConnTypeNonNemaStat[sum + 1] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.detConnNonNemaStat[sum + 1][ACT_INI_INT] = gdvsim.gclv_inter.TX_FROM_USER;
        }
    } // end of method setValueAfterAdd

    void setValueAfterDel(int sum, int index) {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL];

        for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_semiact[lsiv_phase][lsiv_field].setSelectedItem(comboBox_semiact[lsiv_phase + 1][lsiv_field].getSelectedItem().toString());
            }
        }

        comboBox_semiact[sum][ACT_INI_INT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT]));
        comboBox_semiact[sum][ACT_VEH_INT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT]));
        comboBox_semiact[sum][ACT_YEL_CHG].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG]));
        comboBox_semiact[sum][ACT_RED_CLR].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR]));
        comboBox_semiact[sum][ACT_MAX_EXT].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT]));
        comboBox_semiact[sum][ACT_SKIP_PH].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH]);
        comboBox_semiact[sum][ACT_RECALL].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL]);
        comboBox_semiact[sum][ACT_MINOR].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR]);
        comboBox_semiact[sum][ACT_DUAL_LT].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT]);

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        if (gdvsim.greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane];
                            gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                        }
                    }
                }
            }

            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (gdvsim.greenSeqStat[sum][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.greenSeqValue[sum][lsiv_leg][lsiv_lane] = greenSeqDefault[sum][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqStat[sum][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNonNema_ok) {
            for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
                if (gdvsim.detConnTypeNonNemaStat[lsiv_phase + 1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnTypeNonNemaValue[lsiv_phase] = gdvsim.detConnTypeNonNemaValue[lsiv_phase + 1];
                    gdvsim.detConnTypeNonNemaStat[lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.detConnTypeNonNemaValue[lsiv_phase] = detConnTypeNonNemaDefault[lsiv_phase + 1];
                    gdvsim.detConnTypeNonNemaStat[lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.detConnNonNemaStat[lsiv_phase + 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det] = gdvsim.detConnNonNemaValue[lsiv_phase + 1][lsiv_det];
                        gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det] = detConnNonNemaDefault[lsiv_phase + 1][lsiv_det];
                        gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }

            if (gdvsim.detConnTypeNonNemaStat[sum] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.detConnTypeNonNemaValue[sum] = detConnTypeNonNemaDefault[sum];
                gdvsim.detConnTypeNonNemaStat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                if (gdvsim.detConnTypeNonNemaStat[sum] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnTypeNonNemaValue[sum] = detConnTypeNonNemaDefault[sum];
                    gdvsim.detConnTypeNonNemaStat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }
    } // end of method setValueAfterDel

    void AddAction(int lsiv_phase) {
        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeActionListener(componentActionListener);
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeKeyListener(componentKeyListener);
            }
        }

        int numOfPhasesBeforeClick;
        numOfPhasesBeforeClick = getNumberOfPhase();
        int add = 0;

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick + 1; lsiv_phase_row++) {
            if (lsiv_phase_row == lsiv_phase) {
                for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
                    if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                        if (lsiv_phase_col == 1) {
                            temp_phaseSeq[lsiv_phase_row][lsiv_phase_col] = getColumnOnePhaseSequenceDefault(numOfPhasesBeforeClick + 1, lsiv_phase_row);
                        }
                        else {
                            temp_phaseSeq[lsiv_phase_row][lsiv_phase_col] = 0;
                        }
                    }
                }
                add = 1;
            }

            for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH && (lsiv_phase_row + add) <= PARAMS.TEXAS_MODEL_NPH) {
                    temp_phaseSeq[lsiv_phase_row + add][lsiv_phase_col] = Integer.valueOf(comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].getSelectedItem().toString()).intValue();
                }
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick + 1; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick + 1; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeAllItems();
                    if (lsiv_phase_col == 1) {
                        for (int lsiv_phase_i = 1; lsiv_phase_i <= numOfPhasesBeforeClick + 1; lsiv_phase_i++) {
                            if (lsiv_phase_row == lsiv_phase_i)
                                continue;
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addItem(Integer.toString(lsiv_phase_i));
                        }
                    }
                    else {
                        for (int lsiv_phase_i = 0; lsiv_phase_i <= numOfPhasesBeforeClick + 1; lsiv_phase_i++) {
                            if (lsiv_phase_row == lsiv_phase_i)
                                continue;
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addItem(Integer.toString(lsiv_phase_i));
                        }
                    }
                }
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick + 1; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setSelectedItem(Integer.toString(temp_phaseSeq[lsiv_phase_row][lsiv_phase_col]));
                }
            }
        }

        setStatus(numOfPhasesBeforeClick + 1);
        setValueAfterAdd(numOfPhasesBeforeClick, lsiv_phase);
        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(getNumberOfPhase()));

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addActionListener(componentActionListener);
                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addKeyListener(componentKeyListener);
            }
        }
    } // end of method AddAction

    void DeleteAction(int lsiv_phase) {
        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeActionListener(componentActionListener);
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeKeyListener(componentKeyListener);
                }
            }
        }

        int numOfPhasesBeforeClick;
        numOfPhasesBeforeClick = getNumberOfPhase();
        int add = 0;

        for (int lsiv_phase_row = 1; lsiv_phase_row < numOfPhasesBeforeClick; lsiv_phase_row++) {
            if (lsiv_phase_row == lsiv_phase) {
                add = 1;
            }

            for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    temp_phaseSeq[lsiv_phase_row][lsiv_phase_col] = Integer.valueOf(comboBox_phaseSeq[lsiv_phase_row + add][lsiv_phase_col].getSelectedItem().toString()).intValue();
                }
            }
        }

        for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
            if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                temp_phaseSeq[numOfPhasesBeforeClick][lsiv_phase_col] = 0;
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick; lsiv_phase_row++) {
            temp_phaseSeq[lsiv_phase_row][numOfPhasesBeforeClick - 1] = 0;
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick - 1; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick - 1; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].removeAllItems();
                    if (lsiv_phase_col == 1) {
                        for (int lsiv_phase_i = 1; lsiv_phase_i <= numOfPhasesBeforeClick - 1; lsiv_phase_i++) {
                            if (lsiv_phase_row == lsiv_phase_i)
                                continue;
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addItem(Integer.toString(lsiv_phase_i));
                        }
                    }
                    else {
                        for (int lsiv_phase_i = 0; lsiv_phase_i <= numOfPhasesBeforeClick - 1; lsiv_phase_i++) {
                            if (lsiv_phase_row == lsiv_phase_i)
                                continue;
                            comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addItem(Integer.toString(lsiv_phase_i));
                        }
                    }
                }
            }
        }

        for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
            if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                comboBox_phaseSeq[numOfPhasesBeforeClick][lsiv_phase_col].removeAllItems();
                for (int lsiv_phase_i = 0; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_i++) {
                    comboBox_phaseSeq[numOfPhasesBeforeClick][lsiv_phase_col].addItem(Integer.toString(lsiv_phase_i));
                }
            }
        }

        if (numOfPhasesBeforeClick < PARAMS.TEXAS_MODEL_NPH) {
            for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick; lsiv_phase_row++) {
                comboBox_phaseSeq[lsiv_phase_row][numOfPhasesBeforeClick].removeAllItems();
                for (int lsiv_phase_i = 0; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_i++) {
                    comboBox_phaseSeq[lsiv_phase_row][numOfPhasesBeforeClick].addItem(Integer.toString(lsiv_phase_i));
                }
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhasesBeforeClick; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col < numOfPhasesBeforeClick; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].setSelectedItem(Integer.toString(temp_phaseSeq[lsiv_phase_row][lsiv_phase_col]));
                }
            }
        }

        setValueAfterDel(numOfPhasesBeforeClick, lsiv_phase);
        setStatus(numOfPhasesBeforeClick - 1);
        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(getNumberOfPhase()));
        if (!del[lsiv_phase].isEnabled()) {
            okButton.requestFocus();
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addActionListener(componentActionListener);
                    comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].addKeyListener(componentKeyListener);
                }
            }
        }
    } // end of method DeleteAction

    void setValueAfterUp(int index) {
        String tempStr;
        int tempInt;
        int numOfPhase = getNumberOfPhase();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            tempStr = comboBox_semiact[index][lsiv_field].getSelectedItem().toString();
            comboBox_semiact[index][lsiv_field].setSelectedItem(comboBox_semiact[index - 1][lsiv_field].getSelectedItem().toString());
            comboBox_semiact[index - 1][lsiv_field].setSelectedItem(tempStr);
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (gdvsim.greenSeqStat[index][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        tempStr = gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[index - 1][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index - 1][lsiv_leg][lsiv_lane] = tempStr;
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNonNema_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                if (gdvsim.detConnNonNemaStat[index][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    tempInt = gdvsim.detConnNonNemaValue[index][lsiv_det];
                }
                else {
                    tempInt = detConnNonNemaDefault[index][lsiv_det];
                }

                if (gdvsim.detConnNonNemaStat[index - 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnNonNemaValue[index][lsiv_det] = gdvsim.detConnNonNemaValue[index - 1][lsiv_det];
                }
                else {
                    gdvsim.detConnNonNemaValue[index][lsiv_det] = detConnNonNemaDefault[index - 1][lsiv_det];
                }

                gdvsim.detConnNonNemaValue[index - 1][lsiv_det] = tempInt;
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                tempInt = gdvsim.detConnNonNemaStat[index][lsiv_det];
                gdvsim.detConnNonNemaStat[index][lsiv_det] = gdvsim.detConnNonNemaStat[index - 1][lsiv_det];
                gdvsim.detConnNonNemaStat[index - 1][lsiv_det] = tempInt;
            }

            tempStr = gdvsim.detConnTypeNonNemaValue[index];
            gdvsim.detConnTypeNonNemaValue[index] = gdvsim.detConnTypeNonNemaValue[index - 1];
            gdvsim.detConnTypeNonNemaValue[index - 1] = tempStr;

            tempInt = gdvsim.detConnTypeNonNemaStat[index];
            gdvsim.detConnTypeNonNemaStat[index] = gdvsim.detConnTypeNonNemaStat[index - 1];
            gdvsim.detConnTypeNonNemaStat[index - 1] = tempInt;
        }
    } // end of method setValueAfterUp()

    void setValueAfterDown(int index) {
        String tempStr;
        int tempInt;
        int numOfPhase = getNumberOfPhase();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            tempStr = comboBox_semiact[index][lsiv_field].getSelectedItem().toString();
            comboBox_semiact[index][lsiv_field].setSelectedItem(comboBox_semiact[index + 1][lsiv_field].getSelectedItem().toString());
            comboBox_semiact[index + 1][lsiv_field].setSelectedItem(tempStr);
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (gdvsim.greenSeqStat[index + 1][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        tempStr = gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[index + 1][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index + 1][lsiv_leg][lsiv_lane] = tempStr;
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNonNema_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                if (gdvsim.detConnNonNemaStat[index][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    tempInt = gdvsim.detConnNonNemaValue[index][lsiv_det];
                }
                else {
                    tempInt = detConnNonNemaDefault[index][lsiv_det];
                }

                if (gdvsim.detConnNonNemaStat[index + 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnNonNemaValue[index][lsiv_det] = gdvsim.detConnNonNemaValue[index + 1][lsiv_det];
                }
                else {
                    gdvsim.detConnNonNemaValue[index][lsiv_det] = detConnNonNemaDefault[index + 1][lsiv_det];
                }

                gdvsim.detConnNonNemaValue[index + 1][lsiv_det] = tempInt;
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                tempInt = gdvsim.detConnNonNemaStat[index][lsiv_det];
                gdvsim.detConnNonNemaStat[index][lsiv_det] = gdvsim.detConnNonNemaStat[index + 1][lsiv_det];
                gdvsim.detConnNonNemaStat[index + 1][lsiv_det] = tempInt;
            }

            tempStr = gdvsim.detConnTypeNonNemaValue[index];
            gdvsim.detConnTypeNonNemaValue[index] = gdvsim.detConnTypeNonNemaValue[index + 1];
            gdvsim.detConnTypeNonNemaValue[index + 1] = tempStr;

            tempInt = gdvsim.detConnTypeNonNemaStat[index];
            gdvsim.detConnTypeNonNemaStat[index] = gdvsim.detConnTypeNonNemaStat[index + 1];
            gdvsim.detConnTypeNonNemaStat[index + 1] = tempInt;
        }
    } // end of method setValueAfterDown

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = getNumberOfPhase();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_FROM_USER;
        /*
         * lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim
         * .gclv_inter.TX_FMT_SIM_PHASE_SEQ]; for ( int lsiv_phase = 1 ; lsiv_phase <=
         * gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header .msiv_no_of_phases;
         * lsiv_phase++ ) { gdvsim.gclv_inter.mclv_TX_Inter_Data .mclv_tx_sim_data.mclv_tx_sig_setup
         * .mcla_phase_sequence[lsiv_phase].msia_ph_sequence[1] = lsiv_phase + 1;
         * gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup .
         * mcla_phase_sequence[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.
         * TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01] = gdvsim.gclv_inter.TX_FROM_USER; for ( int lsiv_seq =
         * 2 ; lsiv_seq <= gdvsim.gclv_inter.mclv_TX_Inter_Data
         * .mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_seq++ ) { gdvsim
         * .gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup
         * .mcla_phase_sequence[lsiv_phase].msia_ph_sequence[lsiv_seq] = 0; gdvsim
         * .gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup .mcla_phase_sequence
         * [lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter. TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1
         * + lsiv_seq] = gdvsim.gclv_inter.TX_FROM_USER; } gdvsim.gclv_inter.mclv_TX_Inter_Data
         * .mclv_tx_sim_data.mclv_tx_sig_setup .mcla_phase_sequence[gdvsim.gclv_inter
         * .mclv_TX_Inter_Data.mclv_tx_sim_data
         * .mclv_sim_header.msiv_no_of_phases].msia_ph_sequence[1] = 1; }
         */
        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        for (int lsiv_phase = 2; lsiv_phase <= getNumberOfPhase(); lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int = Double.valueOf(
                    comboBox_semiact[lsiv_phase][ACT_INI_INT].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int = Double.valueOf(
                    comboBox_semiact[lsiv_phase][ACT_VEH_INT].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg = Double.valueOf(
                    comboBox_semiact[lsiv_phase][ACT_YEL_CHG].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr = Double.valueOf(
                    comboBox_semiact[lsiv_phase][ACT_RED_CLR].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext = Double.valueOf(
                    comboBox_semiact[lsiv_phase][ACT_MAX_EXT].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_skip_ph = comboBox_semiact[lsiv_phase][ACT_SKIP_PH].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_recall = comboBox_semiact[lsiv_phase][ACT_RECALL].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_minor = comboBox_semiact[lsiv_phase][ACT_MINOR].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_act_dual_lt = comboBox_semiact[lsiv_phase][ACT_DUAL_LT].getSelectedItem().toString();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_INI_INT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_VEH_INT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_YEL_CHG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RED_CLR] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MAX_EXT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_SKIP_PH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_RECALL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_MINOR] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SEMI_SIGNAL_ACT_DUAL_LT] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mdfv_una_min_grn = Double.valueOf(comboBox_semiact[1][ACT_INI_INT].getSelectedItem().toString())
                .doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mdfv_una_yel_chg = Double.valueOf(comboBox_semiact[1][ACT_VEH_INT].getSelectedItem().toString())
                .doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mdfv_una_red_clr = Double.valueOf(comboBox_semiact[1][ACT_YEL_CHG].getSelectedItem().toString())
                .doubleValue();

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_MIN_GRN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_YEL_CHG] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_SUNA_SIGNAL_UNA_RED_CLR] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_phase_row = 1; lsiv_phase_row <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase_row++) {
            if (lsiv_phase_row <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases) {
                for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                    if (lsiv_phase_col <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases) {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].msia_ph_sequence[lsiv_phase_col] = Integer.valueOf(
                                comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].getSelectedItem().toString()).intValue();
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01
                                - 1 + lsiv_phase_col] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].msia_ph_sequence[lsiv_phase_col] = 0;
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01
                                - 1 + lsiv_phase_col] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
            else {
                for (int lsiv_phase_col = 1; lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH; lsiv_phase_col++) {
                    if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].msia_ph_sequence[lsiv_phase_col] = 0;
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phase_row].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01
                                - 1 + lsiv_phase_col] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                                + lsiv_phase] = gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane];

                        if (gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                        }
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNonNema_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if (gdvsim.detConnTypeNonNemaStat[lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type = gdvsim.detConnTypeNonNemaValue[lsiv_phase];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1
                            + lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1
                            + lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] = gdvsim.detConnNonNemaValue[lsiv_phase][lsiv_det];
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                                + lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                                + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }
    } // end of methos saveData()

    boolean isError() {
        int numOfPhase = getNumberOfPhase();

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhase; lsiv_phase_row++) {
            for (int lsiv_phase_col = 1; lsiv_phase_col <= numOfPhase; lsiv_phase_col++) {
                if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                    temp_phaseSeq[lsiv_phase_row][lsiv_phase_col] = Integer.valueOf(comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col].getSelectedItem().toString()).intValue();
                }
            }
        }

        for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhase; lsiv_phase_row++) {
            for (int lsiv_phase_col_i = 1; lsiv_phase_col_i < numOfPhase; lsiv_phase_col_i++) {
                for (int lsiv_phase_col_j = lsiv_phase_col_i + 1; lsiv_phase_col_j <= numOfPhase; lsiv_phase_col_j++) {
                    if (lsiv_phase_col_j < PARAMS.TEXAS_MODEL_NPH) {
                        if ((temp_phaseSeq[lsiv_phase_row][lsiv_phase_col_i] == temp_phaseSeq[lsiv_phase_row][lsiv_phase_col_j]) && (temp_phaseSeq[lsiv_phase_row][lsiv_phase_col_i] != 0)) {
                            JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase_row + " Priority Sequence for Phase " + lsiv_phase_col_i + " = "
                                    + temp_phaseSeq[lsiv_phase_row][lsiv_phase_col_i] + " should not be the same as for Phase " + lsiv_phase_col_j + " = "
                                    + temp_phaseSeq[lsiv_phase_row][lsiv_phase_col_j] + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    } // end of method isError

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_semiact_ok = true;

                if (!isError()) {
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
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
                    DeleteAction(lsiv_phase);
                    break;
                }
                else if (event.getSource() == up[lsiv_phase]) {
                    setValueAfterUp(lsiv_phase);
                    break;
                }
                else if (event.getSource() == down[lsiv_phase]) {
                    setValueAfterDown(lsiv_phase);
                    break;
                }
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (event.getSource() == setAllButton[lsiv_field]) {
                    for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                        comboBox_semiact[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                    }
                    break;
                }
            }

            int numOfPhase = getNumberOfPhase();

            for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhase; lsiv_phase_row++) {
                for (int lsiv_phase_col = 1; lsiv_phase_col <= numOfPhase; lsiv_phase_col++) {
                    if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                        if (event.getSource() == comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col]) {
                            setSequenceRowValue(lsiv_phase_row);
                            break;
                        }
                    }
                }
            }
        }
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    gdvsim.flag_semiact_ok = true;

                    if (!isError()) {
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
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
                        DeleteAction(lsiv_phase);
                        break;
                    }
                    else if (event.getSource() == up[lsiv_phase]) {
                        setValueAfterUp(lsiv_phase);
                        break;
                    }
                    else if (event.getSource() == down[lsiv_phase]) {
                        setValueAfterDown(lsiv_phase);
                        break;
                    }
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                            comboBox_semiact[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                        }
                        break;
                    }
                }

                int numOfPhase = getNumberOfPhase();

                for (int lsiv_phase_row = 1; lsiv_phase_row <= numOfPhase; lsiv_phase_row++) {
                    for (int lsiv_phase_col = 1; lsiv_phase_col <= numOfPhase; lsiv_phase_col++) {
                        if (lsiv_phase_col < PARAMS.TEXAS_MODEL_NPH) {
                            if (event.getSource() == comboBox_phaseSeq[lsiv_phase_row][lsiv_phase_col]) {
                                setSequenceRowValue(lsiv_phase_row);
                                break;
                            }
                        }
                    }
                }
            }
        } // end of keyPressed
    } // end of class ComponentKeyListener
} // end of class SemiactDialog
