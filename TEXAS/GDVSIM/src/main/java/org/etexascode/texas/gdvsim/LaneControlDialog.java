package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           LaneControlDialog.java                           */
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

class LaneControlDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    JLabel label_title;

    JLabel[] label_legs = new JLabel[PARAMS.TEXAS_MODEL_NIA];

    JLabel[] label_lanes = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JLabel[] label_note = new JLabel[11];

    JComboBox[][] comboBox_lane_control = new JComboBox[PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    int[] number_of_lanes = new int[PARAMS.TEXAS_MODEL_NIA];

    JButton okButton, applyButton, cancelButton;

    Font font1, font2;

    int number_of_legs;

    boolean error_flag = false;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public LaneControlDialog() {
        boolean ltor;
        boolean rtor;
        String mvmt_code;
        String mvmt_code_left;
        String mvmt_code_rght;
        int n;

        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
            number_of_lanes[lsiv_leg] = 2;
        }

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT];

        titleString = lclv_tx_fmt.mstv_name.substring(6);

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

        font1 = new Font("TimesRoman", Font.BOLD, 18);
        font2 = new Font("TimesRoman", Font.BOLD, 14);

        label_title = new JLabel(titleString);

        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                label_legs[lsiv_leg] = new JLabel("Leg " + lsiv_leg);
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    if (lsiv_leg == 0) {
                        label_legs[lsiv_leg] = new JLabel("Leg IR");
                    }

                    if (lsiv_leg == number_of_legs + 1) {
                        label_legs[lsiv_leg] = new JLabel("Leg IL");
                    }
                }
            }
        }

        int max = 0;

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                if (max < number_of_lanes[lsiv_leg]) {
                    max = number_of_lanes[lsiv_leg];
                }
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    if (max < number_of_lanes[lsiv_leg]) {
                        max = number_of_lanes[lsiv_leg];
                    }
                }
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= max; lsiv_lane++) {
            label_lanes[lsiv_lane] = new JLabel("Inbound " + lsiv_lane);
        }

        for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn > 0) {
                    String[] array_lane_control = new String[1];
                    array_lane_control[0] = "BL";

                    comboBox_lane_control[lsiv_leg][lsiv_lane] = new JComboBox(array_lane_control);
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_ic_UNCONTRL) {
                        String[] array_lane_control = new String[1];
                        array_lane_control[0] = "UN";

                        comboBox_lane_control[lsiv_leg][lsiv_lane] = new JComboBox(array_lane_control);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_YIELD) {
                        String[] array_lane_control = new String[2];
                        array_lane_control[0] = "UN";
                        array_lane_control[1] = "YI";

                        comboBox_lane_control[lsiv_leg][lsiv_lane] = new JComboBox(array_lane_control);

                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_STOP) {
                        String[] array_lane_control = new String[3];
                        array_lane_control[0] = "ST";
                        array_lane_control[1] = "UN";
                        array_lane_control[2] = "YI";

                        comboBox_lane_control[lsiv_leg][lsiv_lane] = new JComboBox(array_lane_control);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_ALL_STOP) {
                        String[] array_lane_control = new String[2];
                        array_lane_control[0] = "ST";
                        array_lane_control[1] = "YI";

                        comboBox_lane_control[lsiv_leg][lsiv_lane] = new JComboBox(array_lane_control);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_signal_controlled) {
                        ltor = false;
                        rtor = false;
                        mvmt_code = "";
                        mvmt_code_left = "";
                        mvmt_code_rght = "";

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            mvmt_code = "";
                        }
                        else {
                            mvmt_code = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code;
                        }

                        if (lsiv_lane == 1) {
                            ltor = (mvmt_code.contains("L"));
                        }

                        if (lsiv_lane == 2) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                mvmt_code_left = "";
                            }
                            else {
                                mvmt_code_left = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code;
                            }
                            ltor = ((mvmt_code_left.equals("L") || mvmt_code_left.equals("UL")) && mvmt_code.contains("L"));
                        }

                        if ((lsiv_lane == (number_of_lanes[lsiv_leg] - 1)) && (number_of_lanes[lsiv_leg] >= 2)) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[number_of_lanes[lsiv_leg]].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                mvmt_code_rght = "";
                            }
                            else {
                                mvmt_code_rght = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[number_of_lanes[lsiv_leg]].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code;
                            }
                            rtor = (mvmt_code.contains("R") && mvmt_code_rght.equals("R"));
                        }

                        if (lsiv_lane == number_of_lanes[lsiv_leg]) {
                            rtor = (mvmt_code.contains("R"));
                        }

                        n = 1;
                        if (ltor)
                            n++;
                        if (rtor)
                            n++;
                        if (ltor || rtor)
                            n += 2;
                        String[] array_lane_control = new String[n];
                        n = 0;
                        if (ltor)
                            array_lane_control[n++] = "LT";
                        if (rtor)
                            array_lane_control[n++] = "RT";
                        array_lane_control[n++] = "SI";
                        if (ltor || rtor)
                            array_lane_control[n++] = "ST";
                        if (ltor || rtor)
                            array_lane_control[n++] = "YI";

                        comboBox_lane_control[lsiv_leg][lsiv_lane] = new JComboBox(array_lane_control);
                    }
                }
            }
        }

        if (gdvsim.gclv_inter.mbov_is_ic_UNCONTRL) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.flag_laneControl_ok) {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                        }
                        else {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("UN");
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (gdvsim.flag_laneControl_ok) {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                            }
                            else {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("UN");
                            }
                        }
                    }
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_YIELD) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.flag_laneControl_ok) {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                        }
                        else {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("YI");
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (gdvsim.flag_laneControl_ok) {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                            }
                            else {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("YI");
                            }
                        }
                    }
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_STOP) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.flag_laneControl_ok) {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                        }
                        else {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("ST");
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (gdvsim.flag_laneControl_ok) {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                            }
                            else {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("ST");
                            }
                        }
                    }
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_ALL_STOP) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.flag_laneControl_ok) {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                        }
                        else {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("ST");
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (gdvsim.flag_laneControl_ok) {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                            }
                            else {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("ST");
                            }
                        }
                    }
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_signal_controlled) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (gdvsim.flag_laneControl_ok) {
                            comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                        }
                        else {
                            if (lsiv_lane == number_of_lanes[lsiv_leg]) {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("RT");
                                if (!comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("RT")) {
                                    comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("SI");
                                }
                            }
                            else {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("SI");
                            }
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (gdvsim.flag_laneControl_ok) {
                                comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem(gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont);
                            }
                            else {
                                if (lsiv_lane == number_of_lanes[lsiv_leg]) {
                                    comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("RT");
                                    if (!comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("RT")) {
                                        comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("SI");
                                    }
                                }
                                else {
                                    comboBox_lane_control[lsiv_leg][lsiv_lane].setSelectedItem("SI");
                                }
                            }
                        }
                    }
                }
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    comboBox_lane_control[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleName("lane control for leg " + lsiv_leg + " lane " + lsiv_lane);
                    comboBox_lane_control[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleDescription("lane control for leg " + lsiv_leg + " lane " + lsiv_lane);
                }
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    String s;
                    if (lsiv_leg == 0) {
                        s = "IR";
                    }
                    else {
                        s = "IL";
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        comboBox_lane_control[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleName("lane control for leg " + s + " lane " + lsiv_lane);
                        comboBox_lane_control[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleDescription("lane control for leg " + s + " lane " + lsiv_lane);
                    }
                }
            }
        }

        int lsiv_inb_leg = 3;
        int lsiv_out_leg = 4;
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange && gdvsim.gclv_inter.mbov_free_uturns_defined) {
            // check if free u-turn 1 lane width is valid
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mstv_errorMessage = "Error in SDR_Hdr.readFromCards: gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width is invalid.";
                gdvsim.gclv_inter.errorMessage();
                gdvsim.gclv_inter.msiv_returnCode = gdvsim.gclv_inter.RETURN_FATAL_ERROR;
                return;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0) {
                // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                lsiv_inb_leg = 3;
                lsiv_out_leg = 4;

                // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

                // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }

            // check if free u-turn 2 lane width is valid
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mstv_errorMessage = "Error in SDR_Hdr.readFromCards: gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width is invalid.";
                gdvsim.gclv_inter.errorMessage();
                gdvsim.gclv_inter.msiv_returnCode = gdvsim.gclv_inter.RETURN_FATAL_ERROR;
                return;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0) {
                // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                lsiv_inb_leg = 6;
                lsiv_out_leg = 1;

                // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }

            // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
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

        helpListener = new HelpListener();
        openComboMenuListener = new OpenComboMenuListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                comboBox_lane_control[lsiv_leg][lsiv_lane].addKeyListener(openComboMenuListener);
                comboBox_lane_control[lsiv_leg][lsiv_lane].addKeyListener(helpListener);
            }
        }

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        label_note[1] = new JLabel("NOTE:");
        label_note[2] = new JLabel("Type of control for the inbound lane:                                       ");
        label_note[3] = new JLabel("1. BL - Blocked Lane (lane ends before the intersection)                    ");
        label_note[4] = new JLabel("2. UN - UNcontrolled (only intersection control = UNCONTROL, YIELD, or STOP)");
        label_note[5] = new JLabel("3. YI - YIeld sign (not if intersection control = UNCONTROL)                ");
        label_note[6] = new JLabel("4. ST - STop sign (only intersection control = STOP or ALL-STOP)            ");
        label_note[7] = new JLabel("5. SI - SIgnal without left or right turn on red (signalized intersection)  ");
        label_note[8] = new JLabel("6. LT - signal with Left Turn on red (signalized intersection)              ");
        label_note[9] = new JLabel("7. RT - signal with Right Turn on red (signalized intersection)             ");
        label_note[10] = new JLabel("END NOTE");

        for (int lsiv_note = 1; lsiv_note <= 10; lsiv_note++) {
            label_note[lsiv_note].setFont(font2);
        }

        int iRow = 0;

        gbConstraints.insets = new Insets(4, 5, 4, 5);

        addComponent(panel_title, iRow++, 0, 8, 1);

        for (int lsiv_lane = 1; lsiv_lane <= max; lsiv_lane++) {
            addComponent(label_lanes[lsiv_lane], iRow, lsiv_lane, 1, 1);
        }

        iRow++;

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                addComponent(label_legs[lsiv_leg], iRow, 0, 1, 1);
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    addComponent(comboBox_lane_control[lsiv_leg][lsiv_lane], iRow, lsiv_lane, 1, 1);
                }
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    addComponent(label_legs[lsiv_leg], iRow, 0, 1, 1);
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        addComponent(comboBox_lane_control[lsiv_leg][lsiv_lane], iRow, lsiv_lane, 1, 1);
                    }
                }
            }

            iRow++;

        }

        addComponent(ok_panel, iRow++, 0, 8, 1);

        for (int lsiv_note = 1; lsiv_note <= 10; lsiv_note++) {
            addComponent(label_note[lsiv_note], iRow++, 0, 8, 1);
        }

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method laneControlDialog

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

    boolean isError() {
        boolean error = true;

        if (gdvsim.gclv_inter.mbov_is_ic_YIELD) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("YI")) {
                            error = false;
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("YI")) {
                                error = false;
                            }
                        }
                    }
                }
            }

            if (error) {
                JOptionPane.showMessageDialog(null, "Please enter at least one \"YI\".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_STOP || gdvsim.gclv_inter.mbov_is_ic_ALL_STOP) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("ST")) {
                            error = false;
                        }
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("ST")) {
                                error = false;
                            }
                        }
                    }
                }
            }

            if (error) {
                JOptionPane.showMessageDialog(null, "Please enter at least one \"ST\".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_signal_controlled) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                    error = true;

                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("SI")) {
                            error = false;
                        }

                        if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("LT")) {
                            error = false;
                        }

                        if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("RT")) {
                            error = false;
                        }
                    }

                    if (error && (number_of_lanes[lsiv_leg] != 0)) {
                        JOptionPane.showMessageDialog(null, "Please enter at least one \"SI\", or \"LT\" or \"RT\" in leg " + lsiv_leg + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        error = true;

                        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("SI")) {
                                error = false;
                            }

                            if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("LT")) {
                                error = false;
                            }

                            if (comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString().equals("RT")) {
                                error = false;
                            }
                        }

                        if (error && (number_of_lanes[lsiv_leg] != 0)) {
                            String str;

                            if (lsiv_leg == 0) {
                                str = "IR";
                            }
                            else {
                                str = "IL";
                            }

                            JOptionPane.showMessageDialog(null, "Please enter at least one \"SI\", or \"LT\" or \"RT\" in leg " + str, "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                    }
                }
            }
        }

        return false;

    } // end of isError()

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

                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        String s;

                        if (lsiv_leg == 0) {
                            s = "IR";
                        }
                        else if (lsiv_leg == PARAMS.TEXAS_MODEL_NAL) {
                            s = "IL";
                        }
                        else {
                            s = Integer.toString(lsiv_leg);
                        }

                        if (event.getSource() == comboBox_lane_control[lsiv_leg][lsiv_lane]) {
                            String possibleValue = "|";
                            int n = comboBox_lane_control[lsiv_leg][lsiv_lane].getItemCount();
                            for (int i = 0; i < n; i++) {
                                possibleValue = possibleValue + comboBox_lane_control[lsiv_leg][lsiv_lane].getItemAt(i).toString() + "|";
                            }

                            String defaultValue;
                            if (gdvsim.gclv_inter.mbov_is_ic_UNCONTRL) {
                                defaultValue = "UN";
                            }
                            else if (gdvsim.gclv_inter.mbov_is_ic_YIELD) {
                                defaultValue = "YI";
                            }
                            else if (gdvsim.gclv_inter.mbov_is_ic_STOP) {
                                defaultValue = "ST";
                            }
                            else if (gdvsim.gclv_inter.mbov_is_ic_ALL_STOP) {
                                defaultValue = "ST";
                            }
                            else if (gdvsim.gclv_inter.mbov_is_ic_signal_controlled) {
                                if (lsiv_lane == number_of_lanes[lsiv_leg]) {
                                    defaultValue = "RT";
                                }
                                else {
                                    defaultValue = "SI";
                                }
                            }
                            else {
                                defaultValue = "";
                            }

                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1 + lsiv_lane], lclv_tx_fmt.mstv_name.replace("#", s),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1 + lsiv_lane], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1
                                            + lsiv_lane],
                                    comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString(), defaultValue, possibleValue, " ", " ", " ");
                        }
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of HelpListener()

    void saveData() {
        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((lsiv_leg >= 1) && (lsiv_leg <= number_of_legs)) {
                for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString();
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1
                            + lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = comboBox_lane_control[lsiv_leg][lsiv_lane].getSelectedItem().toString();
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1 - 1
                                + lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                }
            }
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    } // end of saveData()

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();
                    gdvsim.flag_laneControl_ok = true;

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
                        gdvsim.flag_laneControl_ok = true;

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

} // end of class LaneControlDialog
