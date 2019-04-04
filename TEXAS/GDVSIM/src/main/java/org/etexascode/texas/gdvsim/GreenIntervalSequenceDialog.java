package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                      GreenIntervalSequenceDialog.java                      */
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

class GreenIntervalSequenceDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title;

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JLabel[] label_overlap = new JLabel[PARAMS.TEXAS_MODEL_NON + 1];

    JLabel[] label_note = new JLabel[9];

    JLabel[] label_legs = new JLabel[PARAMS.TEXAS_MODEL_NIA];

    JLabel[][] label_lanes = new JLabel[PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    JComboBox[][][] comboBox_phase = new JComboBox[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    JComboBox[][][] comboBox_overlap = new JComboBox[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    int[] number_of_lanes = new int[PARAMS.TEXAS_MODEL_NIA];

    JButton okButton, applyButton, cancelButton;

    JPanel panel_title, panel_message;

    Font font;

    int number_of_legs;

    int number_of_phases;

    int number_of_overlaps;

    String titleString;

    int startLeg;

    int endLeg;

    String defaultValue;

    String[] legString = new String[PARAMS.TEXAS_MODEL_NIA];

    TX_Fmt lclv_tx_fmt;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    public GreenIntervalSequenceDialog() {
        int lsiv_max_phases;
        int lsiv_max_overlaps;

        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NON;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_HOV;
        }
        else {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NOV;
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            startLeg = 0;
            endLeg = 7;
        }
        else {
            startLeg = 1;
            endLeg = number_of_legs;
        }

        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            number_of_lanes[lsiv_leg] = 0;
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                }
            }
        }

        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            legString[lsiv_leg] = "Leg " + Integer.toString(lsiv_leg);
        }

        legString[0] = "Leg IR";
        legString[7] = "Leg IL";

        number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        number_of_overlaps = 0;

        if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            number_of_overlaps = PARAMS.TEXAS_MODEL_NOT;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[1] > 0) {
                        number_of_overlaps++;
                    }
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            number_of_overlaps = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ];

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

        font = new Font("TimesRoman", Font.BOLD, 18);

        label_title = new JLabel(titleString);

        label_title.setFont(font);

        panel_title = new JPanel();
        panel_title.add(label_title);

        label_note[1] = new JLabel("NOTE:");
        label_note[2] = new JLabel("C             = Circular Green");
        label_note[3] = new JLabel("L             = Left Green Arrow");
        label_note[4] = new JLabel("S             = Straight Green Arrow");
        label_note[5] = new JLabel("R             = Right Green Arrow");
        label_note[6] = new JLabel("UN           = UNcontrolled By Signal");
        label_note[7] = new JLabel("<blank> = Red");
        label_note[8] = new JLabel("END NOTE");

        panel_message = new JPanel();

        GridBagLayout gbLayoutPanelNote = new GridBagLayout();
        GridBagConstraints gbConstraintsPanelNote;

        panel_message.setLayout(gbLayoutPanelNote);
        gbConstraintsPanelNote = new GridBagConstraints();

        for (int lsiv_i = 1; lsiv_i <= 8; lsiv_i++) {
            gbConstraintsPanelNote.fill = GridBagConstraints.BOTH;
            gbConstraintsPanelNote.insets = new Insets(1, 1, 1, 1);
            gbConstraintsPanelNote.gridx = 1;
            gbConstraintsPanelNote.gridy = lsiv_i;
            gbConstraintsPanelNote.gridwidth = 1;
            gbConstraintsPanelNote.gridheight = 1;
            gbLayoutPanelNote.setConstraints(label_note[lsiv_i], gbConstraintsPanelNote);
            panel_message.add(label_note[lsiv_i]);
        }

        for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("  Phase " + lsiv_phase);
        }

        char x = 'A';

        for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
            label_overlap[lsiv_overlap] = new JLabel("Overlap  " + x);
            x = (char)('A' + lsiv_overlap);
        }

        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            label_legs[lsiv_leg] = new JLabel(legString[lsiv_leg]);
        }

        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                label_lanes[lsiv_leg][lsiv_lane] = new JLabel("Lane " + lsiv_lane);
            }
        }

        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                String lstv_mc = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code;
                // movement code is |XXXX|U|UL|US|UR|ULS|ULR|USR|ULSR|L|LS|LR|LSR|S|SR|R|
                if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1
                        + lsiv_lane] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    JOptionPane.showMessageDialog(null, "Lane Control Data is not valid.\nPlease OK or Apply Lane Control Data before accessing the Green Interval Sequence Data.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                    return;
                }
                String lstv_lc = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;
                // lane control is |BL|UN|YI|ST|SI|LT|RT|
                String lstv_gis = "";
                // green interval sequence is ||C|L|S|R|CL|CS|CR|LC|SC|SR|RC|RS|UN|
                if (lstv_lc.equals("BL") || lstv_lc.equals("UN") || lstv_lc.equals("YI") || lstv_lc.equals("ST") || lstv_mc.equals("XXXX")) {
                    lstv_gis = "|UN|";
                }
                else {
                    lstv_gis = "||C|";
                    if (lstv_mc.contains("U") || lstv_mc.contains("L")) {
                        lstv_gis = lstv_gis.concat("L|CL|LC|");
                    }
                    if (lstv_mc.contains("S")) {
                        lstv_gis = lstv_gis.concat("S|CS|SC|");
                    }
                    if (lstv_mc.contains("R")) {
                        lstv_gis = lstv_gis.concat("R|CR|RC|");
                    }
                    if (lstv_mc.contains("R") && lstv_mc.contains("S")) {
                        lstv_gis = lstv_gis.concat("SR|RS|");
                    }
                }
                String[] array_phase = lstv_gis.substring(1).split("\\|");
                int num = array_phase.length;
                boolean swapped = true;
                while (swapped) {
                    swapped = false;
                    for (int lsiv_i = 0; lsiv_i < (num - 1); lsiv_i++) {
                        if (array_phase[lsiv_i].compareTo(array_phase[lsiv_i + 1]) > 0) {
                            String temp = array_phase[lsiv_i];
                            array_phase[lsiv_i] = array_phase[lsiv_i + 1];
                            array_phase[lsiv_i + 1] = temp;
                            swapped = true;
                        }
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
                    comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane] = new JComboBox(array_phase);
                    comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].setSelectedItem(" ");
                }

                for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
                    comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane] = new JComboBox(array_phase);
                    comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].setSelectedItem(" ");
                }
            }
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                - 1 + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane]
                                    .setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase]);
                        }
                    }
                }
            }

            for (int lsiv_overlap = 1; lsiv_overlap <= number_of_phases; lsiv_overlap++) {
                for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                - 1 + lsiv_overlap] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane]
                                    .setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_max_phases
                                            + lsiv_overlap]);
                        }
                    }
                }
            }
        }
        else {
            if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
                    int leg0LeftTurnLane = 0;
                    int leg7LeftTurnLane = 0;

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[2]; lsiv_lane++) {
                        comboBox_phase[6][2][lsiv_lane].setSelectedItem("C");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[3]; lsiv_lane++) {
                        comboBox_phase[7][3][lsiv_lane].setSelectedItem("C");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[5]; lsiv_lane++) {
                        comboBox_phase[1][5][lsiv_lane].setSelectedItem("C");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[6]; lsiv_lane++) {
                        comboBox_phase[2][6][lsiv_lane].setSelectedItem("C");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[0]; lsiv_lane++) {
                        if (gdvsim.gclv_inter.mcla_leg[0].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") != -1) {
                            leg0LeftTurnLane = lsiv_lane;
                        }
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= leg0LeftTurnLane; lsiv_lane++) {
                        comboBox_phase[5][0][lsiv_lane].setSelectedItem("L");
                    }

                    for (int lsiv_lane = leg0LeftTurnLane + 1; lsiv_lane <= number_of_lanes[0]; lsiv_lane++) {
                        comboBox_phase[5][0][lsiv_lane].setSelectedItem(" ");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[0]; lsiv_lane++) {
                        comboBox_overlap[2][0][lsiv_lane].setSelectedItem("C");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[7]; lsiv_lane++) {
                        if (gdvsim.gclv_inter.mcla_leg[7].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") != -1) {
                            leg7LeftTurnLane = lsiv_lane;
                        }
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= leg7LeftTurnLane; lsiv_lane++) {
                        comboBox_phase[3][7][lsiv_lane].setSelectedItem("L");
                    }

                    for (int lsiv_lane = leg7LeftTurnLane + 1; lsiv_lane <= number_of_lanes[7]; lsiv_lane++) {
                        comboBox_phase[3][7][lsiv_lane].setSelectedItem(" ");
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[7]; lsiv_lane++) {
                        comboBox_overlap[1][7][lsiv_lane].setSelectedItem("C");
                    }
                } // end of if( gdvsim.gclv_inter.mbov_is_ic_TEX_DIA )
            } // end of if (gdvsim.gclv_inter.mbov_is_diamond_interchange )
            else {
                int leg1LeftTurnLane = 0;
                int leg2LeftTurnLane = 0;
                int leg3LeftTurnLane = 0;
                int leg4LeftTurnLane = 0;

                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    for (int lsiv_mvt = 1; lsiv_mvt <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num[lsiv_phase]; lsiv_mvt++) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 7) {
                            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[1]; lsiv_lane++) {
                                if (gdvsim.gclv_inter.mcla_leg[1].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") != -1) {
                                    leg1LeftTurnLane = lsiv_lane;
                                }
                            }

                            for (int lsiv_lane = 1; lsiv_lane <= leg1LeftTurnLane; lsiv_lane++) {
                                comboBox_phase[lsiv_phase][1][lsiv_lane].setSelectedItem("L");
                            }
                        }

                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 5) {
                            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[2]; lsiv_lane++) {
                                if (gdvsim.gclv_inter.mcla_leg[2].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") != -1) {
                                    leg2LeftTurnLane = lsiv_lane;
                                }
                            }

                            for (int lsiv_lane = 1; lsiv_lane <= leg2LeftTurnLane; lsiv_lane++) {
                                comboBox_phase[lsiv_phase][2][lsiv_lane].setSelectedItem("L");
                            }
                        }

                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 3) {
                            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[3]; lsiv_lane++) {
                                if (gdvsim.gclv_inter.mcla_leg[3].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") != -1) {
                                    leg3LeftTurnLane = lsiv_lane;
                                }
                            }

                            for (int lsiv_lane = 1; lsiv_lane <= leg3LeftTurnLane; lsiv_lane++) {
                                comboBox_phase[lsiv_phase][3][lsiv_lane].setSelectedItem("L");
                            }
                        }

                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 1) {
                            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[4]; lsiv_lane++) {
                                if (gdvsim.gclv_inter.mcla_leg[4].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") != -1) {
                                    leg4LeftTurnLane = lsiv_lane;
                                }
                            }

                            for (int lsiv_lane = 1; lsiv_lane <= leg4LeftTurnLane; lsiv_lane++) {
                                comboBox_phase[lsiv_phase][4][lsiv_lane].setSelectedItem("L");
                            }
                        }
                    }
                } // end of for(int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++)

                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    for (int lsiv_mvt = 1; lsiv_mvt <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num[lsiv_phase]; lsiv_mvt++) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 4) {
                            if (leg1LeftTurnLane == 0) {
                                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[1]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][1][lsiv_lane].setSelectedItem("C");
                                }
                            }
                            else {
                                for (int lsiv_lane = leg1LeftTurnLane + 1; lsiv_lane <= number_of_lanes[1]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][1][lsiv_lane].setSelectedItem("C");
                                }
                            }
                        }

                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 2) {
                            if (leg2LeftTurnLane == 0) {
                                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[2]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][2][lsiv_lane].setSelectedItem("C");
                                }
                            }
                            else {
                                for (int lsiv_lane = leg2LeftTurnLane + 1; lsiv_lane <= number_of_lanes[2]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][2][lsiv_lane].setSelectedItem("C");
                                }
                            }
                        }

                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 8) {
                            if (leg3LeftTurnLane == 0) {
                                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[3]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][3][lsiv_lane].setSelectedItem("C");
                                }
                            }
                            else {
                                for (int lsiv_lane = leg3LeftTurnLane + 1; lsiv_lane <= number_of_lanes[3]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][3][lsiv_lane].setSelectedItem("C");
                                }
                            }
                        }

                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] == 6) {
                            if (leg4LeftTurnLane == 0) {
                                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[4]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][4][lsiv_lane].setSelectedItem("C");
                                }
                            }
                            else {
                                for (int lsiv_lane = leg4LeftTurnLane + 1; lsiv_lane <= number_of_lanes[4]; lsiv_lane++) {
                                    comboBox_phase[lsiv_phase][4][lsiv_lane].setSelectedItem("C");
                                }
                            }
                        }
                    }
                } // end of for(int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++)
            } // end of if (gdvsim.gclv_inter.mbov_is_diamond_interchange )
            /*
             * for(int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
             * for(int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) { for(int lsiv_lane = 1;
             * lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) { defaultValue = " "; if (
             * gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane]. mclv_TX_Lane_Data
             * .mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter .TX_FMT_SIM_LANE_CONT_CONT_0 - 1 +
             * lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID ) { String lc = " "; lc =
             * gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].
             * mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont; if(lc.equals("BL")||lc.equals
             * ("UN")||lc.equals("YI")||lc.equals("ST")) { defaultValue = "UN"; } }
             * comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].setSelectedItem (defaultValue); }
             * } } // end of for(int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps;
             * lsiv_overlap++)
             */
        } // end of if( gdvsim.flag_greenSequence_ok )

        for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleName(
                            "Green Interval Sequence for phase " + lsiv_phase + " leg " + legString[lsiv_leg] + " lane " + lsiv_lane);
                    comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleDescription(
                            "Green Interval Sequence for phase " + lsiv_phase + " leg " + legString[lsiv_leg] + " lane " + lsiv_lane);
                }
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleName(
                            "Overlap for phase " + lsiv_overlap + " leg " + legString[lsiv_leg] + " lane " + lsiv_lane);
                    comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleDescription(
                            "Overlap for phase " + lsiv_overlap + " leg " + legString[lsiv_leg] + " lane " + lsiv_lane);
                }
            }
        }

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        for (int lsiv_phase = 1; lsiv_phase <= lsiv_max_phases; lsiv_phase++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].addKeyListener(openComboMenuListener);
                    comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].addKeyListener(helpListener);
                }
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].addKeyListener(openComboMenuListener);
                    comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].addKeyListener(helpListener);
                }
            }
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

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        int iRow = 0;
        int numOfColumns = 0;
        int iCol = 1;

        gbConstraints.insets = new Insets(2, 5, 2, 5);

        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                numOfColumns++;
            }
        }

        addComponent(panel_title, iRow++, 0, numOfColumns + 1, 1);

        addComponent(panel_message, iRow++, 0, numOfColumns + 1, 1);

        iCol = 1;
        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            if (number_of_lanes[lsiv_leg] != 0) {
                addComponent(label_legs[lsiv_leg], iRow, iCol, number_of_lanes[lsiv_leg], 1);
                iCol = iCol + number_of_lanes[lsiv_leg];
            }
        }

        iRow++;

        iCol = 0;
        for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                addComponent(label_lanes[lsiv_leg][lsiv_lane], iRow, iCol + lsiv_lane, 1, 1);
            }
            if (number_of_lanes[lsiv_leg] != 0) {
                iCol = iCol + number_of_lanes[lsiv_leg];
            }
        }

        iRow++;

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
                if ((lsiv_phase == 4) || (lsiv_phase == 8))
                    continue;
            }

            iCol = 0;
            addComponent(label_phase[lsiv_phase], iRow, 0, 1, 1);
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    addComponent(comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane], iRow, iCol + lsiv_lane, 1, 1);
                }
                if (number_of_lanes[lsiv_leg] != 0) {
                    iCol = iCol + number_of_lanes[lsiv_leg];
                }
            }
            iRow++;
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= number_of_overlaps; lsiv_overlap++) {
            iCol = 0;

            addComponent(label_overlap[lsiv_overlap], iRow, 0, 1, 1);

            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    addComponent(comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane], iRow, iCol + lsiv_lane, 1, 1);
                }

                if (number_of_lanes[lsiv_leg] != 0) {
                    iCol = iCol + number_of_lanes[lsiv_leg];
                }
            }

            iRow++;

        }

        addComponent(ok_panel, iRow, 0, numOfColumns + 1, 1);

        aFrame.setSize(1000, 720);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    } // end of method GreenIntervalSequenceDialog

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
        } // end of method keyPressed
    } // end of class OpenComboMenuListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            int lsiv_max_phases;
            int lsiv_max_overlaps;

            if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
                lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NON;
            }
            else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
                lsiv_max_overlaps = PARAMS.TEXAS_MODEL_HOV;
            }
            else {
                lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
                lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NOV;
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
                    for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (event.getSource() == comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane]) {
                                String value;
                                String possibleValue = "|";
                                int n = comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getItemCount();
                                for (int i = 0; i < n; i++) {
                                    value = comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getItemAt(i).toString();
                                    if (value.length() == 0)
                                        value = "<blank>";
                                    possibleValue = possibleValue + value + "|";
                                }
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + lsiv_phase], lclv_tx_fmt.mstv_name,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + lsiv_phase] + " for Leg " + legString[lsiv_leg] + " Lane " + lsiv_lane,
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + lsiv_phase], comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem()
                                                .toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + lsiv_phase], possibleValue, " ", " ", " ");
                            }
                        }
                    }
                }

                for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
                    for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (event.getSource() == comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane]) {
                                String possibleString;
                                possibleString = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1 + lsiv_overlap];
                                possibleString = "|<blank>" + possibleString.substring(1);

                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1 + lsiv_overlap], lclv_tx_fmt.mstv_name,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1 + lsiv_overlap] + " for Leg " + legString[lsiv_leg] + " Lane " + lsiv_lane,
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1 + lsiv_overlap], comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane]
                                                .getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1 + lsiv_overlap], possibleString, " ", " ",
                                        " ");
                            }
                        }
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        } // end of method keyPressed
    } // end of class HelpListener

    boolean isError() {
        String lc = " ";

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    if (!comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("")) {
                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1
                                + lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            lc = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;

                            if (lc.equals("BL") || lc.equals("UN") || lc.equals("YI") || lc.equals("ST")) {
                                if (!comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("UN")) {
                                    JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " leg " + lsiv_leg + " lane " + lsiv_lane + " should be UN because the movement code is " + lc + ".",
                                            "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }

                            if (lc.equals("SI") || lc.equals("LT") || lc.equals("RT")) {
                                if (comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("UN")) {
                                    JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " leg " + lsiv_leg + " lane " + lsiv_lane + " should not be UN because the movement code is " + lc
                                            + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                        }

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            if ((comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString().indexOf("L") != -1)
                                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("L") == -1)) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " leg " + lsiv_leg + " lane including L is wrong because the movement code didn't include a L .",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }

                            if ((comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString().indexOf("S") != -1)
                                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("S") == -1)) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " leg " + lsiv_leg + " lane including S is wrong because the movement code didn't include a S .",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }

                            if ((comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString().indexOf("R") != -1)
                                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code.indexOf("R") == -1)) {
                                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " leg " + lsiv_leg + " lane including R is wrong because the movement code didn't include a R .",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                    }
                }
            }
        }

        return false;
    } // end of method isError

    void saveData() {
        int lsiv_max_phases;
        int lsiv_max_overlaps;
        int lsiv_status;

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPN;
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NON;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_HPH;
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_HOV;
        }
        else {
            lsiv_max_phases = PARAMS.TEXAS_MODEL_NPH;
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NOV;
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                            + lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= lsiv_max_overlaps; lsiv_overlap++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1
                            + lsiv_overlap] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.greenSeqStat[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
            if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
                if ((lsiv_phase == 4) || (lsiv_phase == 8))
                    continue;
            }

            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                            + lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = comboBox_phase[lsiv_phase][lsiv_leg][lsiv_lane].getSelectedItem().toString();
                    gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= number_of_overlaps; lsiv_overlap++) {
            for (int lsiv_leg = startLeg; lsiv_leg <= endLeg; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_max_phases
                            + lsiv_overlap] = comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane]
                                    .getSelectedItem().toString();
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1
                            + lsiv_overlap] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.greenSeqValue[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] = comboBox_overlap[lsiv_overlap][lsiv_leg][lsiv_lane].getSelectedItem().toString();
                    gdvsim.greenSeqStat[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
        }

        if ((gdvsim.gclv_inter.mbov_is_diamond_interchange)
                && (gdvsim.gclv_inter.mbov_free_uturns_defined)
                && ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0))) {
            int lsiv_inb_leg = 3; // fut=1
            int lsiv_out_leg = 4;
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0) {
                lsiv_status = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }
            else {
                lsiv_status = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NGI; lsiv_phase++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                        + lsiv_phase] = lsiv_status;
            }

            lsiv_inb_leg = 6; // fut=2
            lsiv_out_leg = 1;
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0) {
                lsiv_status = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }
            else {
                lsiv_status = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NGI; lsiv_phase++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[lsiv_phase] = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                        + lsiv_phase] = lsiv_status;
            }
        }
    } // end of method saveData

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_greenSequence_ok = true;
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
                        gdvsim.flag_greenSequence_ok = true;
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
        } // end of keyPressed
    } // end of OkApplyKeyListener
} // end of class GreenIntervalSequence
