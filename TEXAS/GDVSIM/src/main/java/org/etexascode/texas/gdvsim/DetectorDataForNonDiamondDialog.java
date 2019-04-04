package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                  DetectorDataForNonDiamondDialog.java                      */
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

class DetectorDataForNonDiamondDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    int NUMOFFIELD = 9;

    int DATNDI_LEG = 1;

    int DATNDI_FL = 2;

    int DATNDI_NL = 3;

    int DATNDI_SPA = 4;

    int DATNDI_LEN = 5;

    int DATNDI_TYPE = 6;

    int DATNDI_DELAY = 7;

    int DATNDI_EXTEND = 8;

    int DATNDI_CLASS_NUM = 9;

    JLabel label_title, label_total, lbl_tbl_title;

    JComboBox cbo_total;

    JButton okButton, applyButton, cancelButton;

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JComboBox[][] cbo_det = new JComboBox[PARAMS.TEXAS_MODEL_NLS + 1][NUMOFFIELD];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NLS + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NLS + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NLS + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NLS + 1];

    JButton[] classify = new JButton[PARAMS.TEXAS_MODEL_NLS + 1];

    JLabel[] label_det = new JLabel[PARAMS.TEXAS_MODEL_NLS + 1];

    JLabel[] label_lane = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JLabel[] label_leg = new JLabel[PARAMS.TEXAS_MODEL_NLGP1];

    JComboBox[][] cbo_table = new JComboBox[PARAMS.TEXAS_MODEL_NLGP1][PARAMS.TEXAS_MODEL_NAL + 1];

    int[][] num_of_dets_per_lane = new int[PARAMS.TEXAS_MODEL_NLGP1][PARAMS.TEXAS_MODEL_NAL + 1];

    int[] number_of_lanes = new int[PARAMS.TEXAS_MODEL_NLGP1];

    Font font, font1, font2;

    TX_Fmt lclv_tx_fmt;

    int max_number_of_lanes;

    int number_of_detectors;

    int number_of_legs;

    String titleString;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    JButton detectorConnButton;

    int[] det_classify_num_default = new int[PARAMS.TEXAS_MODEL_NLS + 1];

    int[][] det_classify_lower_default = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    int[][] det_classify_upper_default = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    String[][] det_classify_name_default = new String[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public DetectorDataForNonDiamondDialog(JButton preDetectorConnButton) {
        detectorConnButton = preDetectorConnButton;

        number_of_detectors = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det;
        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            number_of_lanes[lsiv_leg] = 2;
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI];

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            det_classify_num_default[lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM + lsiv_det];

            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                det_classify_name_default[lsiv_det][lsiv_class] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01 - 1 + lsiv_class];
                det_classify_lower_default[lsiv_det][lsiv_class] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01 - 1 + lsiv_class];
                det_classify_upper_default[lsiv_det][lsiv_class] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01 - 1 + lsiv_class];
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM
                    - 1] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.det_classify_num_stat[lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.det_classify_name_stat[lsiv_det][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.det_classify_lower_stat[lsiv_det][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.det_classify_upper_stat[lsiv_det][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                gdvsim.det_classify_num[lsiv_det] = det_classify_num_default[lsiv_det];

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.det_classify_name[lsiv_det][lsiv_class] = det_classify_name_default[lsiv_det][lsiv_class];
                    gdvsim.det_classify_lower[lsiv_det][lsiv_class] = det_classify_lower_default[lsiv_det][lsiv_class];
                    gdvsim.det_classify_upper[lsiv_det][lsiv_class] = det_classify_upper_default[lsiv_det][lsiv_class];
                }
            }
            else {
                gdvsim.det_classify_num_stat[lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM];

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.det_classify_name_stat[lsiv_det][lsiv_class] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01
                            - 1 + lsiv_class];
                    gdvsim.det_classify_lower_stat[lsiv_det][lsiv_class] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01
                            - 1 + lsiv_class];
                    gdvsim.det_classify_upper_stat[lsiv_det][lsiv_class] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01
                            - 1 + lsiv_class];
                }

                gdvsim.det_classify_num[lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_class_num;

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.det_classify_name[lsiv_det][lsiv_class] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msta_class_name[lsiv_class];
                    gdvsim.det_classify_lower[lsiv_det][lsiv_class] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msia_class_length_lower[lsiv_class];
                    gdvsim.det_classify_upper[lsiv_det][lsiv_class] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msia_class_length_upper[lsiv_class];
                }
            }
        }

        if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            titleString = "Detector Data For Non-Diamond Semi-Actuated";
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            titleString = "Detector Data For Non-Diamond Full-Actuated";
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            titleString = "Detector Data For Non-Diamond NEMA";
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            titleString = "Detector Data For Non-Diamond Hardware-in-the-Loop";
        }
        else {
            titleString = "error";
        }

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
        font1 = new Font("TimesRoman", Font.BOLD, 14);
        font2 = new Font("TimesRoman", Font.BOLD, 12);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        JLabel lbl_tbl_title = new JLabel("Number Of Detectors Per Lane");
        JPanel panel_lbl_tbl_title = new JPanel();
        panel_lbl_tbl_title.add(lbl_tbl_title);

        label_det[0] = new JLabel("Detector", JLabel.LEFT);
        label_det[0].setVerticalAlignment(JLabel.BOTTOM);
        label_det[0].setFont(font1);

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            label_det[lsiv_det] = new JLabel(Integer.toString(lsiv_det));
            label_det[lsiv_det].setFont(font1);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == NUMOFFIELD) {
                label_field[lsiv_field] = new JTextArea("Classify Detector Data");
            }
            else {
                label_field[lsiv_field] = new JTextArea(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG - 1 + lsiv_field]);
            }

            if (lsiv_field == DATNDI_TYPE) {
                if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                    label_field[lsiv_field] = new JTextArea(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG - 1 + lsiv_field]);
                }
                else {
                    label_field[lsiv_field] = new JTextArea("Detector Type (INactive, PResence, or PUlse)");
                }
            }

            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
        }

        int leg_num_has_lane = 0;

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                continue;
            leg_num_has_lane++;
        }

        if (leg_num_has_lane == 0) {
            JOptionPane.showMessageDialog(null, "Number of legs with inbound lanes is 0", "The number of lanes is error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        String[] array_DATNDI_LEG = new String[leg_num_has_lane];

        int index = 0;

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                continue;
            array_DATNDI_LEG[index++] = Integer.toString(lsiv_leg);
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            cbo_det[lsiv_det][DATNDI_LEG] = new JComboBox(array_DATNDI_LEG);
        }

        if (gdvsim.flag_detDataForNonDiamond_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_LEG].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_leg));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_LEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG]));
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                cbo_det[lsiv_det][DATNDI_LEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG]));
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();

            String[] array_DATNDI_FL = new String[number_of_lanes[legNum]];

            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[legNum]; lsiv_lane++) {
                array_DATNDI_FL[lsiv_lane - 1] = Integer.toString(lsiv_lane);
            }

            cbo_det[lsiv_det][DATNDI_FL] = new JComboBox(array_DATNDI_FL);
        }

        if (gdvsim.flag_detDataForNonDiamond_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_FL].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_fl));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_FL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL]));
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                cbo_det[lsiv_det][DATNDI_FL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL]));
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();
            int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString()).intValue();
            int numCovered = number_of_lanes[legNum] - firstLane + 1;

            String[] array_DATNDI_NL = new String[numCovered];

            for (int lsiv_lane = 1; lsiv_lane <= numCovered; lsiv_lane++) {
                array_DATNDI_NL[lsiv_lane - 1] = Integer.toString(lsiv_lane);
            }
            cbo_det[lsiv_det][DATNDI_NL] = new JComboBox(array_DATNDI_NL);
        }

        if (gdvsim.flag_detDataForNonDiamond_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_NL].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_nl));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_NL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL]));
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                cbo_det[lsiv_det][DATNDI_NL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL]));
            }
        }

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        int count;
        int int_number;

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA];

        count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        int_number = lsiv_min;
        String[] array_DATNDI_SPA = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_DATNDI_SPA[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN];

        count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        int_number = lsiv_min;
        String[] array_DATNDI_LEN = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_DATNDI_LEN[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        String[] array_DATNDI_TYPE = new String[4];

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            array_DATNDI_TYPE = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE].substring(1).split("\\|");
        }
        else {
            String type = "|IN|PR|PU|";
            array_DATNDI_TYPE = type.substring(1).split("\\|");
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY];

        count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        int_number = lsiv_min;
        String[] array_DATNDI_DELAY = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_DATNDI_DELAY[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        Double lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND];
        Double lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND];
        Double lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        Double double_number = lsiv_min_double;
        String[] array_DATNDI_EXTEND = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_DATNDI_EXTEND[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            cbo_det[lsiv_det][DATNDI_SPA] = new JComboBox(array_DATNDI_SPA);
            cbo_det[lsiv_det][DATNDI_LEN] = new JComboBox(array_DATNDI_LEN);
            cbo_det[lsiv_det][DATNDI_TYPE] = new JComboBox(array_DATNDI_TYPE);
            cbo_det[lsiv_det][DATNDI_DELAY] = new JComboBox(array_DATNDI_DELAY);
            cbo_det[lsiv_det][DATNDI_EXTEND] = new JComboBox(array_DATNDI_EXTEND);
        }

        if (gdvsim.flag_detDataForNonDiamond_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_SPA].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_spa));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_SPA].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_LEN].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_len));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_TYPE].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mstv_type);
                }
                else {
                    cbo_det[lsiv_det][DATNDI_TYPE].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE]);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_DELAY].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_delay));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_DELAY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATNDI_EXTEND].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mdfv_extend));
                }
                else {
                    cbo_det[lsiv_det][DATNDI_EXTEND].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]));
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                cbo_det[lsiv_det][DATNDI_SPA].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]));
                cbo_det[lsiv_det][DATNDI_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]));
                cbo_det[lsiv_det][DATNDI_TYPE].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE]);
                cbo_det[lsiv_det][DATNDI_DELAY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]));
                cbo_det[lsiv_det][DATNDI_EXTEND].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]));
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            add[lsiv_det] = new JButton("Add");
            del[lsiv_det] = new JButton("Delete");
            up[lsiv_det] = new JButton("Up");
            down[lsiv_det] = new JButton("Down");
            classify[lsiv_det] = new JButton("Edit");
        }

        setStatus(number_of_detectors);

        label_total = new JLabel("Total Detectors");
        label_total.setFont(font1);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfDetector()));

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

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            label_lane[lsiv_lane] = new JLabel("lane " + lsiv_lane);
            label_lane[lsiv_lane].setFont(font1);
            label_lane[lsiv_lane].setMinimumSize(new Dimension(5, 10));
            label_lane[lsiv_lane].setMaximumSize(new Dimension(5, 10));
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            label_leg[lsiv_leg] = new JLabel("leg " + lsiv_leg);
            label_leg[lsiv_leg].setFont(font1);
            label_leg[lsiv_leg].setMinimumSize(new Dimension(5, 10));
            label_leg[lsiv_leg].setMaximumSize(new Dimension(5, 10));
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                num_of_dets_per_lane[lsiv_leg][lsiv_lane] = 0;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane] = new JComboBox();
                cbo_table[lsiv_leg][lsiv_lane].addItem(Integer.toString(num_of_dets_per_lane[lsiv_leg][lsiv_lane]));
                cbo_table[lsiv_leg][lsiv_lane].setFont(font1);
                cbo_table[lsiv_leg][lsiv_lane].setMinimumSize(new Dimension(5, 10));
                cbo_table[lsiv_leg][lsiv_lane].setMaximumSize(new Dimension(5, 10));
            }
        }

        calculateNumberOfDetectorsPerLane();

        JPanel panel_table = new JPanel();
        GridBagLayout gbLayoutPanelTable = new GridBagLayout();
        GridBagConstraints gbConstraintsPanelTable;
        panel_table.setLayout(gbLayoutPanelTable);
        gbConstraintsPanelTable = new GridBagConstraints();

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gbConstraintsPanelTable.fill = GridBagConstraints.BOTH;
            gbConstraintsPanelTable.insets = new Insets(1, 1, 1, 1);
            gbConstraintsPanelTable.gridx = lsiv_lane + 2;
            gbConstraintsPanelTable.gridy = 1;
            gbConstraintsPanelTable.gridwidth = 1;
            gbConstraintsPanelTable.gridheight = 1;
            gbLayoutPanelTable.setConstraints(label_lane[lsiv_lane], gbConstraintsPanelTable);
            panel_table.add(label_lane[lsiv_lane]);
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            gbConstraintsPanelTable.fill = GridBagConstraints.BOTH;
            gbConstraintsPanelTable.insets = new Insets(1, 1, 1, 1);
            gbConstraintsPanelTable.gridx = 1;
            gbConstraintsPanelTable.gridy = lsiv_leg + 2;
            gbConstraintsPanelTable.gridwidth = 1;
            gbConstraintsPanelTable.gridheight = 1;
            gbLayoutPanelTable.setConstraints(label_leg[lsiv_leg], gbConstraintsPanelTable);
            panel_table.add(label_leg[lsiv_leg]);
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                gbConstraintsPanelTable.fill = GridBagConstraints.BOTH;
                gbConstraintsPanelTable.insets = new Insets(1, 1, 1, 1);
                gbConstraintsPanelTable.gridx = lsiv_lane + 2;
                gbConstraintsPanelTable.gridy = lsiv_leg + 2;
                gbConstraintsPanelTable.gridwidth = 1;
                gbConstraintsPanelTable.gridheight = 1;
                gbLayoutPanelTable.setConstraints(cbo_table[lsiv_leg][lsiv_lane], gbConstraintsPanelTable);
                panel_table.add(cbo_table[lsiv_leg][lsiv_lane]);
            }
        }

        max_number_of_lanes = 0;

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            if (number_of_lanes[lsiv_leg] > max_number_of_lanes) {
                max_number_of_lanes = number_of_lanes[lsiv_leg];
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            label_leg[lsiv_leg].setVisible(true);
        }

        for (int lsiv_leg = number_of_legs + 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            label_leg[lsiv_leg].setVisible(false);
        }

        for (int lsiv_lane = 1; lsiv_lane <= max_number_of_lanes; lsiv_lane++) {
            label_lane[lsiv_lane].setVisible(true);
        }

        for (int lsiv_lane = max_number_of_lanes + 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            label_lane[lsiv_lane].setVisible(false);
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane].setVisible(false);
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane].setVisible(true);
            }
        }

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                classify[lsiv_det].setVisible(true);
                label_field[NUMOFFIELD].setVisible(true);
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                classify[lsiv_det].setVisible(false);
                label_field[NUMOFFIELD].setVisible(false);
            }
        }

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            cbo_det[lsiv_det][DATNDI_LEG].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_FL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_NL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_TYPE].addActionListener(componentActionListener);

            cbo_det[lsiv_det][DATNDI_LEG].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_FL].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_NL].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_TYPE].addKeyListener(componentKeyListener);
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            add[lsiv_det].addActionListener(componentActionListener);
            del[lsiv_det].addActionListener(componentActionListener);
            up[lsiv_det].addActionListener(componentActionListener);
            down[lsiv_det].addActionListener(componentActionListener);

            add[lsiv_det].addKeyListener(componentKeyListener);
            del[lsiv_det].addKeyListener(componentKeyListener);
            up[lsiv_det].addKeyListener(componentKeyListener);
            down[lsiv_det].addKeyListener(componentKeyListener);

            add[lsiv_det].addKeyListener(helpListener);
            del[lsiv_det].addKeyListener(helpListener);
            up[lsiv_det].addKeyListener(helpListener);
            down[lsiv_det].addKeyListener(helpListener);

            classify[lsiv_det].addActionListener(componentActionListener);
            classify[lsiv_det].addKeyListener(componentKeyListener);
            classify[lsiv_det].addKeyListener(helpListener);

            for (int lsiv_field = 1; lsiv_field < NUMOFFIELD; lsiv_field++) {
                cbo_det[lsiv_det][lsiv_field].addKeyListener(openComboMenuListener);
                cbo_det[lsiv_det][lsiv_field].addKeyListener(helpListener);
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane].addKeyListener(openComboMenuListener);
                cbo_table[lsiv_leg][lsiv_lane].addKeyListener(helpListener);
            }
        }

        cbo_total.addKeyListener(openComboMenuListener);
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

        setAccessiblility();
        setSize();

        int iRow = 0;
        int numOfColumns = NUMOFFIELD + 1 + 4;

        gbConstraints.insets = new Insets(1, 1, 4, 1);
        addComponent(panel_title, iRow++, 0, numOfColumns, 1);

        gbConstraints.insets = new Insets(12, 1, 1, 1);
        addComponent(label_det[0], iRow, 0, 2, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1);
        }

        iRow++;

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            addComponent(label_det[lsiv_det], iRow, 0, 1, 1);
            addComponent(add[lsiv_det], iRow, 1, 1, 1);
            addComponent(del[lsiv_det], iRow, 2, 1, 1);
            addComponent(up[lsiv_det], iRow, 3, 1, 1);
            addComponent(down[lsiv_det], iRow, 4, 1, 1);

            for (int lsiv_field = 1; lsiv_field < NUMOFFIELD; lsiv_field++) {
                addComponent(cbo_det[lsiv_det][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
            }
            addComponent(classify[lsiv_det], iRow, numOfColumns - 1, 1, 1);

            iRow++;
        }

        addComponent(cbo_total, iRow, 1, 1, 1);
        addComponent(label_total, iRow++, 2, 3, 1);
        addComponent(panel_lbl_tbl_title, iRow++, 0, numOfColumns, 1);
        addComponent(panel_table, iRow++, 0, numOfColumns, 1);
        addComponent(ok_panel, iRow++, 0, numOfColumns, 1);

        aFrame.setSize(1000, 750);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        // aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method DetectorDataForNonDiamond

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    void setAccessiblility() {
        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            add[lsiv_det].getAccessibleContext().setAccessibleName("Add                    for Detector " + lsiv_det);
            add[lsiv_det].getAccessibleContext().setAccessibleDescription("Add                    for Detector " + lsiv_det);
            del[lsiv_det].getAccessibleContext().setAccessibleName("Delete                 for Detector " + lsiv_det);
            del[lsiv_det].getAccessibleContext().setAccessibleDescription("Delete                 for Detector " + lsiv_det);
            up[lsiv_det].getAccessibleContext().setAccessibleName("Up                     for Detector " + lsiv_det);
            up[lsiv_det].getAccessibleContext().setAccessibleDescription("Up                     for Detector " + lsiv_det);
            down[lsiv_det].getAccessibleContext().setAccessibleName("Down                   for Detector " + lsiv_det);
            down[lsiv_det].getAccessibleContext().setAccessibleDescription("Down                   for Detector " + lsiv_det);
            classify[lsiv_det].getAccessibleContext().setAccessibleName("Classify Detector Data for Detector " + lsiv_det);
            classify[lsiv_det].getAccessibleContext().setAccessibleDescription("Classify Detector Data for Detector " + lsiv_det);
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            for (int lsiv_field = 1; lsiv_field < NUMOFFIELD; lsiv_field++) {
                cbo_det[lsiv_det][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG - 1 + lsiv_field] + " for Detector " + lsiv_det);
                cbo_det[lsiv_det][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG - 1 + lsiv_field] + " for Detector " + lsiv_det);
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleName("Number of Detectors for leg " + lsiv_leg + " inbound lane " + lsiv_lane);
                cbo_table[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleDescription("Number of Detectors for leg " + lsiv_leg + " inbound lane " + lsiv_lane);
            }
        }

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

    } // end of method setAccessiblility

    void setSize() {
        label_field[DATNDI_LEG].setPreferredSize(new Dimension(55, 120));
        label_field[DATNDI_FL].setPreferredSize(new Dimension(55, 120));
        label_field[DATNDI_NL].setPreferredSize(new Dimension(55, 120));
        label_field[DATNDI_SPA].setPreferredSize(new Dimension(60, 120));
        label_field[DATNDI_LEN].setPreferredSize(new Dimension(55, 120));
        label_field[DATNDI_TYPE].setPreferredSize(new Dimension(65, 120));
        label_field[DATNDI_DELAY].setPreferredSize(new Dimension(60, 120));
        label_field[DATNDI_EXTEND].setPreferredSize(new Dimension(60, 120));
        label_field[NUMOFFIELD].setPreferredSize(new Dimension(55, 120));

        label_field[DATNDI_LEG].setMaximumSize(new Dimension(55, 120));
        label_field[DATNDI_FL].setMaximumSize(new Dimension(55, 120));
        label_field[DATNDI_NL].setMaximumSize(new Dimension(55, 120));
        label_field[DATNDI_SPA].setMaximumSize(new Dimension(60, 120));
        label_field[DATNDI_LEN].setMaximumSize(new Dimension(55, 120));
        label_field[DATNDI_TYPE].setMaximumSize(new Dimension(65, 120));
        label_field[DATNDI_DELAY].setMaximumSize(new Dimension(60, 120));
        label_field[DATNDI_EXTEND].setMaximumSize(new Dimension(60, 120));
        label_field[NUMOFFIELD].setMaximumSize(new Dimension(55, 120));

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            label_det[lsiv_det].setPreferredSize(new Dimension(20, 25));
            label_det[lsiv_det].setMaximumSize(new Dimension(20, 25));

            add[lsiv_det].setPreferredSize(new Dimension(60, 25));
            del[lsiv_det].setPreferredSize(new Dimension(75, 25));
            up[lsiv_det].setPreferredSize(new Dimension(50, 25));
            down[lsiv_det].setPreferredSize(new Dimension(70, 25));

            add[lsiv_det].setMaximumSize(new Dimension(60, 25));
            del[lsiv_det].setMaximumSize(new Dimension(75, 25));
            up[lsiv_det].setMaximumSize(new Dimension(50, 25));
            down[lsiv_det].setMaximumSize(new Dimension(70, 25));

            cbo_det[lsiv_det][DATNDI_LEG].setPreferredSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_FL].setPreferredSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_NL].setPreferredSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_SPA].setPreferredSize(new Dimension(60, 25));
            cbo_det[lsiv_det][DATNDI_LEN].setPreferredSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_TYPE].setPreferredSize(new Dimension(65, 25));
            cbo_det[lsiv_det][DATNDI_DELAY].setPreferredSize(new Dimension(60, 25));
            cbo_det[lsiv_det][DATNDI_EXTEND].setPreferredSize(new Dimension(60, 25));

            cbo_det[lsiv_det][DATNDI_LEG].setMaximumSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_FL].setMaximumSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_NL].setMaximumSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_SPA].setMaximumSize(new Dimension(60, 25));
            cbo_det[lsiv_det][DATNDI_LEN].setMaximumSize(new Dimension(55, 25));
            cbo_det[lsiv_det][DATNDI_TYPE].setMaximumSize(new Dimension(65, 25));
            cbo_det[lsiv_det][DATNDI_DELAY].setMaximumSize(new Dimension(60, 25));
            cbo_det[lsiv_det][DATNDI_EXTEND].setMaximumSize(new Dimension(60, 25));

            classify[lsiv_det].setPreferredSize(new Dimension(55, 25));
            classify[lsiv_det].setMaximumSize(new Dimension(55, 25));
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
    } // end of OpenComboMenuListener

    int getNumberOfDetector() {
        int numOfDetector = 0;

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (add[lsiv_det].isEnabled()) {
                numOfDetector++;
            }
        }

        if (numOfDetector == 0) {
            return PARAMS.TEXAS_MODEL_NLS;
        }
        else {
            return numOfDetector - 1;
        }
    } // end of method getNumberOfDetector()

    void calculateNumberOfDetectorsPerLane() {
        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                num_of_dets_per_lane[lsiv_leg][lsiv_lane] = 0;
            }
        }

        int numOfDetector = getNumberOfDetector();

        for (int lsiv_det = 1; lsiv_det <= numOfDetector; lsiv_det++) {
            int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();
            int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString()).intValue();
            int numCovered = Integer.valueOf(cbo_det[lsiv_det][DATNDI_NL].getSelectedItem().toString()).intValue();

            for (int lsiv_lane = firstLane; lsiv_lane < firstLane + numCovered; lsiv_lane++) {
                num_of_dets_per_lane[legNum][lsiv_lane]++;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane].removeAllItems();
                cbo_table[lsiv_leg][lsiv_lane].addItem(Integer.toString(num_of_dets_per_lane[lsiv_leg][lsiv_lane]));
            }
        }
    } // end of method calculateNumberOfDetectorsPerLane()

    void setStatus(int sumOfDetectors) {
        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (lsiv_det <= sumOfDetectors) {
                label_det[lsiv_det].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field < NUMOFFIELD; lsiv_field++) {
                    cbo_det[lsiv_det][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_det[lsiv_det].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field < NUMOFFIELD; lsiv_field++) {
                    cbo_det[lsiv_det][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfDetectors == PARAMS.TEXAS_MODEL_NLS) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                add[lsiv_det].setEnabled(false);
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (lsiv_det <= (sumOfDetectors + 1)) {
                    add[lsiv_det].setEnabled(true);
                }
                else {
                    add[lsiv_det].setEnabled(false);
                }
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (lsiv_det <= sumOfDetectors) {
                del[lsiv_det].setEnabled(true);
            }
            else {
                del[lsiv_det].setEnabled(false);
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (lsiv_det <= sumOfDetectors) {
                up[lsiv_det].setEnabled(true);
            }
            else {
                up[lsiv_det].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (lsiv_det < sumOfDetectors) {
                down[lsiv_det].setEnabled(true);
            }
            else {
                down[lsiv_det].setEnabled(false);
            }
        }

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                    classify[lsiv_det].setEnabled(true);
                    classify[lsiv_det].setForeground(new Color(0, 0, 0));
                }
                else {
                    classify[lsiv_det].setForeground(new Color(176, 197, 218));
                    classify[lsiv_det].setEnabled(false);
                }
            }
        }
    } // end of method setStatus

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_det = sum + 1; lsiv_det >= index; lsiv_det--) {
            cbo_det[lsiv_det][DATNDI_LEG].removeActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_LEG].removeKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_FL].removeActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_FL].removeKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_NL].removeActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_NL].removeKeyListener(componentKeyListener);
        }

        for (int lsiv_det = sum; lsiv_det >= index; lsiv_det--) {
            for (int lsiv_field = 2; lsiv_field <= 3; lsiv_field++) {
                JComboBox temp = new JComboBox();

                String val = cbo_det[lsiv_det][lsiv_field].getSelectedItem().toString();

                int numoflist = cbo_det[lsiv_det][lsiv_field].getItemCount();

                for (int lsiv_lane = 1; lsiv_lane <= numoflist; lsiv_lane++) {
                    temp.addItem(Integer.toString(lsiv_lane));
                }

                cbo_det[lsiv_det + 1][lsiv_field].removeAllItems();

                for (int lsiv_i = 0; lsiv_i < numoflist; lsiv_i++) {
                    cbo_det[lsiv_det + 1][lsiv_field].addItem(temp.getItemAt(lsiv_i));
                }

                cbo_det[lsiv_det + 1][lsiv_field].setSelectedItem(val);
            }
        }

        for (int lsiv_det = sum; lsiv_det >= index; lsiv_det--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == 2 || lsiv_field == 3 || lsiv_field == NUMOFFIELD)
                    continue;

                cbo_det[lsiv_det + 1][lsiv_field].setSelectedItem(cbo_det[lsiv_det][lsiv_field].getSelectedItem().toString());

                gdvsim.det_classify_num[lsiv_det + 1] = gdvsim.det_classify_num[lsiv_det];
                gdvsim.det_classify_num_stat[lsiv_det + 1] = gdvsim.det_classify_num_stat[lsiv_det];

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.det_classify_name[lsiv_det + 1][lsiv_class] = gdvsim.det_classify_name[lsiv_det][lsiv_class];
                    gdvsim.det_classify_lower[lsiv_det + 1][lsiv_class] = gdvsim.det_classify_lower[lsiv_det][lsiv_class];
                    gdvsim.det_classify_upper[lsiv_det + 1][lsiv_class] = gdvsim.det_classify_upper[lsiv_det][lsiv_class];

                    gdvsim.det_classify_name_stat[lsiv_det + 1][lsiv_class] = gdvsim.det_classify_name_stat[lsiv_det][lsiv_class];
                    gdvsim.det_classify_lower_stat[lsiv_det + 1][lsiv_class] = gdvsim.det_classify_lower_stat[lsiv_det][lsiv_class];
                    gdvsim.det_classify_upper_stat[lsiv_det + 1][lsiv_class] = gdvsim.det_classify_upper_stat[lsiv_det][lsiv_class];
                }
            }
        }

        if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                    classify[lsiv_det].setEnabled(true);
                    classify[lsiv_det].setForeground(new Color(0, 0, 0));
                }
                else {
                    classify[lsiv_det].setForeground(new Color(176, 197, 218));
                    classify[lsiv_det].setEnabled(false);
                }
            }
        }

        for (int lsiv_det = sum + 1; lsiv_det >= index; lsiv_det--) {
            cbo_det[lsiv_det][DATNDI_LEG].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_LEG].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_FL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_FL].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_NL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_NL].addKeyListener(componentKeyListener);
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_det = index; lsiv_det <= sum; lsiv_det++) {
            cbo_det[lsiv_det][DATNDI_LEG].removeActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_LEG].removeKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_FL].removeActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_FL].removeKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_NL].removeActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_NL].removeKeyListener(componentKeyListener);
        }

        for (int lsiv_det = index; lsiv_det < sum; lsiv_det++) {
            for (int lsiv_field = 2; lsiv_field <= 3; lsiv_field++) {
                JComboBox temp = new JComboBox();

                String val = cbo_det[lsiv_det + 1][lsiv_field].getSelectedItem().toString();

                int numoflist = cbo_det[lsiv_det + 1][lsiv_field].getItemCount();

                for (int lsiv_lane = 1; lsiv_lane <= numoflist; lsiv_lane++) {
                    temp.addItem(Integer.toString(lsiv_lane));
                }

                cbo_det[lsiv_det][lsiv_field].removeAllItems();

                for (int lsiv_i = 0; lsiv_i < numoflist; lsiv_i++) {
                    cbo_det[lsiv_det][lsiv_field].addItem(temp.getItemAt(lsiv_i));
                }

                cbo_det[lsiv_det][lsiv_field].setSelectedItem(val);
            }
        }

        for (int lsiv_det = index; lsiv_det < sum; lsiv_det++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == 2 || lsiv_field == 3 || lsiv_field == NUMOFFIELD)
                    continue;

                cbo_det[lsiv_det][lsiv_field].setSelectedItem(cbo_det[lsiv_det + 1][lsiv_field].getSelectedItem().toString());

                gdvsim.det_classify_num[lsiv_det] = gdvsim.det_classify_num[lsiv_det + 1];
                gdvsim.det_classify_num_stat[lsiv_det] = gdvsim.det_classify_num_stat[lsiv_det + 1];

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.det_classify_name[lsiv_det][lsiv_class] = gdvsim.det_classify_name[lsiv_det + 1][lsiv_class];
                    gdvsim.det_classify_lower[lsiv_det][lsiv_class] = gdvsim.det_classify_lower[lsiv_det + 1][lsiv_class];
                    gdvsim.det_classify_upper[lsiv_det][lsiv_class] = gdvsim.det_classify_upper[lsiv_det + 1][lsiv_class];

                    gdvsim.det_classify_name_stat[lsiv_det][lsiv_class] = gdvsim.det_classify_name_stat[lsiv_det + 1][lsiv_class];
                    gdvsim.det_classify_lower_stat[lsiv_det][lsiv_class] = gdvsim.det_classify_lower_stat[lsiv_det + 1][lsiv_class];
                    gdvsim.det_classify_upper_stat[lsiv_det][lsiv_class] = gdvsim.det_classify_upper_stat[lsiv_det + 1][lsiv_class];
                }
            }
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                classify[lsiv_det].setEnabled(true);
                classify[lsiv_det].setForeground(new Color(0, 0, 0));
            }
            else {
                classify[lsiv_det].setForeground(new Color(176, 197, 218));
                classify[lsiv_det].setEnabled(false);
            }
        }

        cbo_det[sum][DATNDI_SPA].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]));
        cbo_det[sum][DATNDI_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]));
        cbo_det[sum][DATNDI_TYPE].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE]);
        cbo_det[sum][DATNDI_DELAY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]));
        cbo_det[sum][DATNDI_EXTEND].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]));

        gdvsim.det_classify_num[sum] = det_classify_num_default[sum];
        gdvsim.det_classify_num_stat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            gdvsim.det_classify_name[sum][lsiv_class] = det_classify_name_default[sum][lsiv_class];
            gdvsim.det_classify_lower[sum][lsiv_class] = det_classify_lower_default[sum][lsiv_class];
            gdvsim.det_classify_upper[sum][lsiv_class] = det_classify_upper_default[sum][lsiv_class];

            gdvsim.det_classify_name_stat[sum][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.det_classify_lower_stat[sum][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.det_classify_upper_stat[sum][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        int firstLeg = 1;

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                continue;

            firstLeg = lsiv_leg;
            break;
        }

        cbo_det[sum][DATNDI_LEG].setSelectedItem(Integer.toString(firstLeg));

        cbo_det[sum][DATNDI_FL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[firstLeg]; lsiv_lane++) {
            cbo_det[sum][DATNDI_FL].addItem(Integer.toString(lsiv_lane));
        }

        int firstLane = Integer.valueOf(cbo_det[sum][DATNDI_FL].getSelectedItem().toString()).intValue();
        int rangeForField3 = number_of_lanes[firstLeg] - firstLane + 1;

        cbo_det[sum][DATNDI_NL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= rangeForField3; lsiv_lane++) {
            cbo_det[sum][DATNDI_NL].addItem(Integer.toString(lsiv_lane));
        }

        for (int lsiv_det = index; lsiv_det <= sum; lsiv_det++) {
            cbo_det[lsiv_det][DATNDI_LEG].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_LEG].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_FL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_FL].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATNDI_NL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATNDI_NL].addKeyListener(componentKeyListener);
        }
    } // end of method setValueAfterDel()

    void setValueAfterUp(int index) {
        cbo_det[index][DATNDI_LEG].removeActionListener(componentActionListener);
        cbo_det[index][DATNDI_LEG].removeKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_FL].removeActionListener(componentActionListener);
        cbo_det[index][DATNDI_FL].removeKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_NL].removeActionListener(componentActionListener);
        cbo_det[index][DATNDI_NL].removeKeyListener(componentKeyListener);

        cbo_det[index - 1][DATNDI_LEG].removeActionListener(componentActionListener);
        cbo_det[index - 1][DATNDI_LEG].removeKeyListener(componentKeyListener);
        cbo_det[index - 1][DATNDI_FL].removeActionListener(componentActionListener);
        cbo_det[index - 1][DATNDI_FL].removeKeyListener(componentKeyListener);
        cbo_det[index - 1][DATNDI_NL].removeActionListener(componentActionListener);
        cbo_det[index - 1][DATNDI_NL].removeKeyListener(componentKeyListener);

        for (int lsiv_field = 2; lsiv_field <= 3; lsiv_field++) {
            JComboBox temp1 = new JComboBox();
            JComboBox temp2 = new JComboBox();

            String first = cbo_det[index - 1][lsiv_field].getSelectedItem().toString();
            String second = cbo_det[index][lsiv_field].getSelectedItem().toString();

            int numoflist1 = cbo_det[index - 1][lsiv_field].getItemCount();
            int numoflist2 = cbo_det[index][lsiv_field].getItemCount();

            for (int lsiv_lane = 1; lsiv_lane <= numoflist1; lsiv_lane++) {
                temp1.addItem(Integer.toString(lsiv_lane));
            }

            for (int lsiv_lane = 1; lsiv_lane <= numoflist2; lsiv_lane++) {
                temp2.addItem(Integer.toString(lsiv_lane));
            }

            cbo_det[index - 1][lsiv_field].removeAllItems();
            cbo_det[index][lsiv_field].removeAllItems();

            for (int lsiv_i = 0; lsiv_i < numoflist1; lsiv_i++) {
                cbo_det[index][lsiv_field].addItem(temp1.getItemAt(lsiv_i));
            }

            for (int lsiv_i = 0; lsiv_i < numoflist2; lsiv_i++) {
                cbo_det[index - 1][lsiv_field].addItem(temp2.getItemAt(lsiv_i));
            }

            cbo_det[index - 1][lsiv_field].setSelectedItem(second);
            cbo_det[index][lsiv_field].setSelectedItem(first);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == 2 || lsiv_field == 3 || lsiv_field == NUMOFFIELD)
                continue;

            String temp;

            temp = cbo_det[index][lsiv_field].getSelectedItem().toString();
            cbo_det[index][lsiv_field].setSelectedItem(cbo_det[index - 1][lsiv_field].getSelectedItem().toString());
            cbo_det[index - 1][lsiv_field].setSelectedItem(temp);
        }

        int intTemp;
        String strTemp;

        intTemp = gdvsim.det_classify_num[index];
        gdvsim.det_classify_num[index] = gdvsim.det_classify_num[index - 1];
        gdvsim.det_classify_num[index - 1] = intTemp;

        intTemp = gdvsim.det_classify_num_stat[index];
        gdvsim.det_classify_num_stat[index] = gdvsim.det_classify_num_stat[index - 1];
        gdvsim.det_classify_num_stat[index - 1] = intTemp;

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            strTemp = gdvsim.det_classify_name[index][lsiv_class];
            gdvsim.det_classify_name[index][lsiv_class] = gdvsim.det_classify_name[index - 1][lsiv_class];
            gdvsim.det_classify_name[index - 1][lsiv_class] = strTemp;

            intTemp = gdvsim.det_classify_name_stat[index][lsiv_class];
            gdvsim.det_classify_name_stat[index][lsiv_class] = gdvsim.det_classify_name_stat[index - 1][lsiv_class];
            gdvsim.det_classify_name_stat[index - 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_lower[index][lsiv_class];
            gdvsim.det_classify_lower[index][lsiv_class] = gdvsim.det_classify_lower[index - 1][lsiv_class];
            gdvsim.det_classify_lower[index - 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_lower_stat[index][lsiv_class];
            gdvsim.det_classify_lower_stat[index][lsiv_class] = gdvsim.det_classify_lower_stat[index - 1][lsiv_class];
            gdvsim.det_classify_lower_stat[index - 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_upper[index][lsiv_class];
            gdvsim.det_classify_upper[index][lsiv_class] = gdvsim.det_classify_upper[index - 1][lsiv_class];
            gdvsim.det_classify_upper[index - 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_upper_stat[index][lsiv_class];
            gdvsim.det_classify_upper_stat[index][lsiv_class] = gdvsim.det_classify_upper_stat[index - 1][lsiv_class];
            gdvsim.det_classify_upper_stat[index - 1][lsiv_class] = intTemp;
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                classify[lsiv_det].setEnabled(true);
                classify[lsiv_det].setForeground(new Color(0, 0, 0));
            }
            else {
                classify[lsiv_det].setForeground(new Color(176, 197, 218));
                classify[lsiv_det].setEnabled(false);
            }
        }

        cbo_det[index][DATNDI_LEG].addActionListener(componentActionListener);
        cbo_det[index][DATNDI_LEG].addKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_FL].addActionListener(componentActionListener);
        cbo_det[index][DATNDI_FL].addKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_NL].addActionListener(componentActionListener);
        cbo_det[index][DATNDI_NL].addKeyListener(componentKeyListener);

        cbo_det[index - 1][DATNDI_LEG].addActionListener(componentActionListener);
        cbo_det[index - 1][DATNDI_LEG].addKeyListener(componentKeyListener);
        cbo_det[index - 1][DATNDI_FL].addActionListener(componentActionListener);
        cbo_det[index - 1][DATNDI_FL].addKeyListener(componentKeyListener);
        cbo_det[index - 1][DATNDI_NL].addActionListener(componentActionListener);
        cbo_det[index - 1][DATNDI_NL].addKeyListener(componentKeyListener);

    } // end of method setValueAfterUp()

    void setValueAfterDown(int index) {
        cbo_det[index][DATNDI_LEG].removeActionListener(componentActionListener);
        cbo_det[index][DATNDI_LEG].removeKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_FL].removeActionListener(componentActionListener);
        cbo_det[index][DATNDI_FL].removeKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_NL].removeActionListener(componentActionListener);
        cbo_det[index][DATNDI_NL].removeKeyListener(componentKeyListener);

        cbo_det[index + 1][DATNDI_LEG].removeActionListener(componentActionListener);
        cbo_det[index + 1][DATNDI_LEG].removeKeyListener(componentKeyListener);
        cbo_det[index + 1][DATNDI_FL].removeActionListener(componentActionListener);
        cbo_det[index + 1][DATNDI_FL].removeKeyListener(componentKeyListener);
        cbo_det[index + 1][DATNDI_NL].removeActionListener(componentActionListener);
        cbo_det[index + 1][DATNDI_NL].removeKeyListener(componentKeyListener);

        for (int lsiv_field = 2; lsiv_field <= 3; lsiv_field++) {
            JComboBox temp1 = new JComboBox();
            JComboBox temp2 = new JComboBox();

            String first = cbo_det[index][lsiv_field].getSelectedItem().toString();
            String second = cbo_det[index + 1][lsiv_field].getSelectedItem().toString();

            int numoflist1 = cbo_det[index][lsiv_field].getItemCount();
            int numoflist2 = cbo_det[index + 1][lsiv_field].getItemCount();

            for (int lsiv_lane = 1; lsiv_lane <= numoflist1; lsiv_lane++) {
                temp1.addItem(Integer.toString(lsiv_lane));
            }

            for (int lsiv_lane = 1; lsiv_lane <= numoflist2; lsiv_lane++) {
                temp2.addItem(Integer.toString(lsiv_lane));
            }

            cbo_det[index][lsiv_field].removeAllItems();
            cbo_det[index + 1][lsiv_field].removeAllItems();

            for (int lsiv_i = 0; lsiv_i < numoflist1; lsiv_i++) {
                cbo_det[index + 1][lsiv_field].addItem(temp1.getItemAt(lsiv_i));
            }

            for (int lsiv_i = 0; lsiv_i < numoflist2; lsiv_i++) {
                cbo_det[index][lsiv_field].addItem(temp2.getItemAt(lsiv_i));
            }

            cbo_det[index][lsiv_field].setSelectedItem(second);
            cbo_det[index + 1][lsiv_field].setSelectedItem(first);

        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == 2 || lsiv_field == 3 || lsiv_field == NUMOFFIELD)
                continue;

            String temp;

            temp = cbo_det[index][lsiv_field].getSelectedItem().toString();
            cbo_det[index][lsiv_field].setSelectedItem(cbo_det[index + 1][lsiv_field].getSelectedItem().toString());
            cbo_det[index + 1][lsiv_field].setSelectedItem(temp);
        }

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                classify[lsiv_det].setEnabled(true);
                classify[lsiv_det].setForeground(new Color(0, 0, 0));
            }
            else {
                classify[lsiv_det].setForeground(new Color(176, 197, 218));
                classify[lsiv_det].setEnabled(false);
            }
        }

        int intTemp;
        String strTemp;

        intTemp = gdvsim.det_classify_num[index];
        gdvsim.det_classify_num[index] = gdvsim.det_classify_num[index + 1];
        gdvsim.det_classify_num[index + 1] = intTemp;

        intTemp = gdvsim.det_classify_num_stat[index];
        gdvsim.det_classify_num_stat[index] = gdvsim.det_classify_num_stat[index + 1];
        gdvsim.det_classify_num_stat[index + 1] = intTemp;

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            strTemp = gdvsim.det_classify_name[index][lsiv_class];
            gdvsim.det_classify_name[index][lsiv_class] = gdvsim.det_classify_name[index + 1][lsiv_class];
            gdvsim.det_classify_name[index + 1][lsiv_class] = strTemp;

            intTemp = gdvsim.det_classify_name_stat[index][lsiv_class];
            gdvsim.det_classify_name_stat[index][lsiv_class] = gdvsim.det_classify_name_stat[index + 1][lsiv_class];
            gdvsim.det_classify_name_stat[index + 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_lower[index][lsiv_class];
            gdvsim.det_classify_lower[index][lsiv_class] = gdvsim.det_classify_lower[index + 1][lsiv_class];
            gdvsim.det_classify_lower[index + 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_lower_stat[index][lsiv_class];
            gdvsim.det_classify_lower_stat[index][lsiv_class] = gdvsim.det_classify_lower_stat[index + 1][lsiv_class];
            gdvsim.det_classify_lower_stat[index + 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_upper[index][lsiv_class];
            gdvsim.det_classify_upper[index][lsiv_class] = gdvsim.det_classify_upper[index + 1][lsiv_class];
            gdvsim.det_classify_upper[index + 1][lsiv_class] = intTemp;

            intTemp = gdvsim.det_classify_upper_stat[index][lsiv_class];
            gdvsim.det_classify_upper_stat[index][lsiv_class] = gdvsim.det_classify_upper_stat[index + 1][lsiv_class];
            gdvsim.det_classify_upper_stat[index + 1][lsiv_class] = intTemp;
        }

        cbo_det[index][DATNDI_LEG].addActionListener(componentActionListener);
        cbo_det[index][DATNDI_LEG].addKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_FL].addActionListener(componentActionListener);
        cbo_det[index][DATNDI_FL].addKeyListener(componentKeyListener);
        cbo_det[index][DATNDI_NL].addActionListener(componentActionListener);
        cbo_det[index][DATNDI_NL].addKeyListener(componentKeyListener);

        cbo_det[index + 1][DATNDI_LEG].addActionListener(componentActionListener);
        cbo_det[index + 1][DATNDI_LEG].addKeyListener(componentKeyListener);
        cbo_det[index + 1][DATNDI_FL].addActionListener(componentActionListener);
        cbo_det[index + 1][DATNDI_FL].addKeyListener(componentKeyListener);
        cbo_det[index + 1][DATNDI_NL].addActionListener(componentActionListener);
        cbo_det[index + 1][DATNDI_NL].addKeyListener(componentKeyListener);

    } // end of method setValueAfterDown()

    void LegAction(int lsiv_det) {
        cbo_det[lsiv_det][DATNDI_FL].removeActionListener(componentActionListener);
        cbo_det[lsiv_det][DATNDI_FL].removeKeyListener(componentKeyListener);
        cbo_det[lsiv_det][DATNDI_NL].removeActionListener(componentActionListener);
        cbo_det[lsiv_det][DATNDI_NL].removeKeyListener(componentKeyListener);

        int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();

        cbo_det[lsiv_det][DATNDI_FL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[legNum]; lsiv_lane++) {
            cbo_det[lsiv_det][DATNDI_FL].addItem(Integer.toString(lsiv_lane));
        }

        int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString()).intValue();
        int rangeForField3 = number_of_lanes[legNum] - firstLane + 1;

        cbo_det[lsiv_det][DATNDI_NL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= rangeForField3; lsiv_lane++) {
            cbo_det[lsiv_det][DATNDI_NL].addItem(Integer.toString(lsiv_lane));
        }

        calculateNumberOfDetectorsPerLane();

        cbo_det[lsiv_det][DATNDI_FL].addActionListener(componentActionListener);
        cbo_det[lsiv_det][DATNDI_FL].addKeyListener(componentKeyListener);
        cbo_det[lsiv_det][DATNDI_NL].addActionListener(componentActionListener);
        cbo_det[lsiv_det][DATNDI_NL].addKeyListener(componentKeyListener);

    } // end of method LegAction

    void FirstLaneAction(int lsiv_det) {
        cbo_det[lsiv_det][DATNDI_NL].removeActionListener(componentActionListener);
        cbo_det[lsiv_det][DATNDI_NL].removeKeyListener(componentKeyListener);

        int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();
        int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString()).intValue();

        int rangeForField3 = number_of_lanes[legNum] - firstLane + 1;

        cbo_det[lsiv_det][DATNDI_NL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= rangeForField3; lsiv_lane++) {
            cbo_det[lsiv_det][DATNDI_NL].addItem(Integer.toString(lsiv_lane));
        }

        calculateNumberOfDetectorsPerLane();

        cbo_det[lsiv_det][DATNDI_NL].addActionListener(componentActionListener);
        cbo_det[lsiv_det][DATNDI_NL].addKeyListener(componentKeyListener);

    } // end of method FirstLaneAction

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
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the Total Detectors and is determined by the program.",
                            cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NLS), "1");

                }

                for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (event.getSource() == cbo_table[lsiv_leg][lsiv_lane]) {
                            cbo_table[lsiv_leg][lsiv_lane].addKeyListener(helpListener);
                            new HelpDialog(true, "Number of Detectors for leg " + lsiv_leg + " inbound lane " + lsiv_lane, "Number of Detectors for leg " + lsiv_leg + " inbound lane " + lsiv_lane,
                                    "The user cannot specify this item.  This item is the Number of Detectors for leg " + lsiv_leg + " inbound lane " + lsiv_lane
                                            + " and is determined by the program.",
                                    cbo_table[lsiv_leg][lsiv_lane].getSelectedItem().toString(), "0", " ", "0",
                                    Integer.toString(PARAMS.TEXAS_MODEL_NLS), "1");
                        }
                    }
                }

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                    if (event.getSource() == add[lsiv_det]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 detector for all detectors below this detector, inserts a new detector at the current position, copies the values of all parameters from the previous detector to the new detector, and increases the Total Detectors by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_det]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 detector for all detectors below this detector and decreases the Total Detectors by 1.", " ", " ",
                                " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == up[lsiv_det]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous detector and the current detector.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_det]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next detector and the current detector.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_LEG]) {
                        int minLeg = 1;
                        int maxLeg = 1;

                        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            minLeg = lsiv_leg;
                            break;
                        }

                        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            maxLeg = lsiv_leg;
                        }

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG], cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString(), Integer.toString(minLeg), " ",
                                Integer.toString(minLeg), Integer.toString(maxLeg), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG]));

                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_FL]) {
                        int minLeg = 1;
                        int maxLeg = 1;

                        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            minLeg = lsiv_leg;
                            break;
                        }

                        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            maxLeg = lsiv_leg;
                        }

                        int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();

                        int minLaneField2 = 1;
                        int maxLaneField2 = number_of_lanes[legNum];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL], cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString(), Integer.toString(minLaneField2), " ",
                                Integer.toString(minLaneField2), Integer.toString(maxLaneField2), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_NL]) {
                        int minLeg = 1;
                        int maxLeg = 1;

                        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            minLeg = lsiv_leg;
                            break;
                        }

                        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            maxLeg = lsiv_leg;
                        }

                        int legNum = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();

                        int minLaneField2 = 1;
                        int maxLaneField2 = number_of_lanes[legNum];

                        int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString()).intValue();
                        int rangeForField3 = number_of_lanes[legNum] - firstLane + 1;

                        int minLaneField3 = 1;
                        int maxLaneField3 = rangeForField3;

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL], cbo_det[lsiv_det][DATNDI_NL].getSelectedItem().toString(), Integer.toString(minLaneField3), " ",
                                Integer.toString(minLaneField3), Integer.toString(maxLaneField3), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_SPA]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA], cbo_det[lsiv_det][DATNDI_SPA].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_LEN]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN], cbo_det[lsiv_det][DATNDI_LEN].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_TYPE]) {
                        String possibleValue = "|";
                        int n = cbo_det[lsiv_det][DATNDI_TYPE].getItemCount();
                        for (int i = 0; i < n; i++) {
                            possibleValue = possibleValue + cbo_det[lsiv_det][DATNDI_TYPE].getItemAt(i).toString() + "|";
                        }
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE], cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString(),
                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE], possibleValue, " ", " ", " ");
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_DELAY]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY], cbo_det[lsiv_det][DATNDI_DELAY].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_EXTEND]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND], cbo_det[lsiv_det][DATNDI_EXTEND].getSelectedItem().toString(),
                                oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]), " ",
                                oneDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]),
                                oneDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]),
                                oneDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND]));
                    }
                    else if (event.getSource() == classify[lsiv_det]) {
                        new HelpDialog(true, "Classify Detector Data Button",
                                "The Classify Detector Data Button opens the Classify Detector Data window to allow the user to define the vehicle class data.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            }
        }
    } // end of class HelpListener()

    boolean isError() {
        boolean flag_error = false;

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                if (num_of_dets_per_lane[lsiv_leg][lsiv_lane] > PARAMS.TEXAS_MODEL_NLO) {
                    flag_error = true;
                    break;
                }
            }
            if (flag_error)
                break;
        }

        if (flag_error) {
            JOptionPane.showMessageDialog(null, "Number of detectors per lane should be less than or equal to " + PARAMS.TEXAS_MODEL_NLO, "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        int numOfDetectors = getNumberOfDetector();
        for (int lsiv_det = 1; lsiv_det <= numOfDetectors; lsiv_det++) {
            if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                if (gdvsim.det_classify_num_stat[lsiv_det] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply the Classify Detector Data for detector " + lsiv_det, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        }

        return false;

    } // end of method isError()

    void saveClassify() {
        int numOfDetectors = getNumberOfDetector();

        for (int lsiv_det = 1; lsiv_det <= numOfDetectors; lsiv_det++) {
            if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msta_class_name[lsiv_class] = gdvsim.det_classify_name[lsiv_det][lsiv_class];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msia_class_length_lower[lsiv_class] = gdvsim.det_classify_lower[lsiv_det][lsiv_class];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msia_class_length_upper[lsiv_class] = gdvsim.det_classify_upper[lsiv_det][lsiv_class];
                }

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01 - 1
                            + lsiv_class] = gdvsim.det_classify_name_stat[lsiv_det][lsiv_class];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01 - 1
                            + lsiv_class] = gdvsim.det_classify_lower_stat[lsiv_det][lsiv_class];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01 - 1
                            + lsiv_class] = gdvsim.det_classify_upper_stat[lsiv_det][lsiv_class];
                }
            }
            else {
                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01 - 1
                            + lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01 - 1
                            + lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01 - 1
                            + lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        for (int lsiv_det = numOfDetectors + 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NAM01 - 1
                        + lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_LL_01 - 1
                        + lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_UL_01 - 1
                        + lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }
    } // end of method saveClassify()

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = getNumberOfDetector();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        for (int lsiv_det = 1; lsiv_det <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det; lsiv_det++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_leg = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEG].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_fl = Integer.valueOf(cbo_det[lsiv_det][DATNDI_FL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_nl = Integer.valueOf(cbo_det[lsiv_det][DATNDI_NL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_spa = Integer.valueOf(cbo_det[lsiv_det][DATNDI_SPA].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_len = Integer.valueOf(cbo_det[lsiv_det][DATNDI_LEN].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mstv_type = cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_delay = Integer.valueOf(cbo_det[lsiv_det][DATNDI_DELAY].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mdfv_extend = Double.valueOf(cbo_det[lsiv_det][DATNDI_EXTEND].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_class_num = gdvsim.det_classify_num[lsiv_det];

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_FL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_NL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_SPA] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_LEN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_DELAY] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_EXTEND] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATNDI_CLASS_NUM] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        saveClassify();
        gdvsim.gclv_inter.calculate_graphics_and_paint();

    } // end of method saveData()

    void invalidDetectorConnection() {
        if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            gdvsim.flag_detConnForNema_ok = false;

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1
                            + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1
                            + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                    gdvsim.detConnNemaVStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.detConnNemaTStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }
        else {
            gdvsim.flag_detConnForNonNema_ok = false;

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                gdvsim.detConnTypeNonNemaStat[lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                            + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                    gdvsim.detConnNonNemaStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }
    } // end of method invalidDetectorConnection

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det != getNumberOfDetector()) {
                        invalidDetectorConnection();
                    }

                    gdvsim.flag_detDataForNonDiamond_ok = true;
                    saveData();

                    if (getNumberOfDetector() == 0) {
                        detectorConnButton.setEnabled(false);
                    }
                    else {
                        detectorConnButton.setEnabled(true);
                    }

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                if (event.getSource() == cbo_det[lsiv_det][DATNDI_LEG]) {
                    LegAction(lsiv_det);
                    break;
                }
                else if (event.getSource() == cbo_det[lsiv_det][DATNDI_FL]) {
                    FirstLaneAction(lsiv_det);
                    break;
                }
                else if (event.getSource() == cbo_det[lsiv_det][DATNDI_NL]) {
                    calculateNumberOfDetectorsPerLane();
                    break;
                }
                else if (event.getSource() == cbo_det[lsiv_det][DATNDI_TYPE]) {
                    if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                        classify[lsiv_det].setEnabled(true);
                        classify[lsiv_det].setForeground(new Color(0, 0, 0));
                    }
                    else {
                        classify[lsiv_det].setForeground(new Color(176, 197, 218));
                        classify[lsiv_det].setEnabled(false);
                    }
                    break;
                }
                else if (event.getSource() == classify[lsiv_det]) {
                    new ClassifyDetectorForDiamondDialog(lsiv_det);
                    break;
                }
                else if (event.getSource() == down[lsiv_det]) {
                    setValueAfterDown(lsiv_det);
                    break;
                }
                else if (event.getSource() == up[lsiv_det]) {
                    setValueAfterUp(lsiv_det);
                    break;
                }
                else if (event.getSource() == del[lsiv_det]) {
                    int numOfDetectorBeforeClick;
                    numOfDetectorBeforeClick = getNumberOfDetector();
                    setValueAfterDel(numOfDetectorBeforeClick, lsiv_det);
                    setStatus(numOfDetectorBeforeClick - 1);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfDetector()));

                    calculateNumberOfDetectorsPerLane();

                    if (!del[lsiv_det].isEnabled()) {
                        okButton.requestFocus();
                    }

                    break;
                }
                else if (event.getSource() == add[lsiv_det]) {
                    int numOfDetectorBeforeClick;
                    numOfDetectorBeforeClick = getNumberOfDetector();
                    setStatus(numOfDetectorBeforeClick + 1);
                    setValueAfterAdd(numOfDetectorBeforeClick, lsiv_det);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfDetector()));

                    calculateNumberOfDetectorsPerLane();
                    break;
                }
            }

        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det != getNumberOfDetector()) {
                            invalidDetectorConnection();
                        }

                        gdvsim.flag_detDataForNonDiamond_ok = true;
                        saveData();

                        if (getNumberOfDetector() == 0) {
                            detectorConnButton.setEnabled(false);
                        }
                        else {
                            detectorConnButton.setEnabled(true);
                        }

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NLS; lsiv_det++) {
                    if (event.getSource() == cbo_det[lsiv_det][DATNDI_LEG]) {
                        LegAction(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_FL]) {
                        FirstLaneAction(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_NL]) {
                        calculateNumberOfDetectorsPerLane();
                        break;
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATNDI_TYPE]) {
                        if (cbo_det[lsiv_det][DATNDI_TYPE].getSelectedItem().toString().equals("CL")) {
                            classify[lsiv_det].setEnabled(true);
                            classify[lsiv_det].setForeground(new Color(0, 0, 0));
                        }
                        else {
                            classify[lsiv_det].setForeground(new Color(176, 197, 218));
                            classify[lsiv_det].setEnabled(false);
                        }
                        break;
                    }
                    else if (event.getSource() == classify[lsiv_det]) {
                        new ClassifyDetectorForDiamondDialog(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == down[lsiv_det]) {
                        setValueAfterDown(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == up[lsiv_det]) {
                        setValueAfterUp(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == del[lsiv_det]) {
                        int numOfDetectorBeforeClick;
                        numOfDetectorBeforeClick = getNumberOfDetector();
                        setValueAfterDel(numOfDetectorBeforeClick, lsiv_det);
                        setStatus(numOfDetectorBeforeClick - 1);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfDetector()));

                        calculateNumberOfDetectorsPerLane();

                        if (!del[lsiv_det].isEnabled()) {
                            okButton.requestFocus();
                        }

                        break;
                    }
                    else if (event.getSource() == add[lsiv_det]) {
                        int numOfDetectorBeforeClick;
                        numOfDetectorBeforeClick = getNumberOfDetector();
                        setStatus(numOfDetectorBeforeClick + 1);
                        setValueAfterAdd(numOfDetectorBeforeClick, lsiv_det);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfDetector()));

                        calculateNumberOfDetectorsPerLane();
                        break;
                    }
                }
            }
        }
    } // end of class ComponentKeyListener

} // end of class DetectorDataForNonDiamond
