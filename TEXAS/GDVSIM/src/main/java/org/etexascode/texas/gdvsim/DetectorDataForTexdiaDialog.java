package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                    DetectorDataForTexdiaDialog.java                        */
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

class DetectorDataForTexdiaDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    int NUMOFFIELD = 6;

    int NUMOFDETECTORS = 10;

    int DATDIA_LEG = 1;

    int DATDIA_FL = 2;

    int DATDIA_NL = 3;

    int DATDIA_SPA = 4;

    int DATDIA_LEN = 5;

    int DATDIA_TYPE = 6;

    int DATDIA_DELAY = 7;

    int DATDIA_EXTEND = 8;

    int DATDIA_CLASS_NUM = 9;

    JLabel label_title, lbl_tbl_title;

    JButton okButton, applyButton, cancelButton;

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JLabel[] label_det = new JLabel[NUMOFDETECTORS + 1];

    JComboBox[][] cbo_det = new JComboBox[NUMOFDETECTORS + 1][NUMOFFIELD + 1];

    JLabel[] label_lane = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JLabel[] label_leg = new JLabel[PARAMS.TEXAS_MODEL_NLGP2];

    JComboBox[][] cbo_table = new JComboBox[PARAMS.TEXAS_MODEL_NLGP2][PARAMS.TEXAS_MODEL_NAL + 1];

    JLabel[][] label_value = new JLabel[PARAMS.TEXAS_MODEL_NLGP2][PARAMS.TEXAS_MODEL_NAL + 1];

    int[][] num_of_dets_per_lane = new int[PARAMS.TEXAS_MODEL_NLGP2][PARAMS.TEXAS_MODEL_NAL + 1];

    int[] number_of_lanes = new int[PARAMS.TEXAS_MODEL_NLGP2];

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    int max_number_of_lanes;

    int number_of_legs;

    String titleString;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    public DetectorDataForTexdiaDialog() {
        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA];

        titleString = "Detector Data For TEX-DIA";

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

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        JLabel lbl_tbl_title = new JLabel("Number Of Detector Per Lane");
        JPanel panel_lbl_tbl_title = new JPanel();
        panel_lbl_tbl_title.add(lbl_tbl_title);

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            label_det[lsiv_det] = new JLabel("Detector " + lsiv_det);
        }

        label_det[1].setText("Detector < 1 >");
        label_det[2].setText("Detector < 2 >");
        label_det[3].setText("Detector <2A>");
        label_det[4].setText("Detector < 3 >");
        label_det[5].setText("Detector <13>");
        label_det[6].setText("Detector < 5 >");
        label_det[7].setText("Detector <56>");
        label_det[8].setText("Detector < 6 >");
        label_det[9].setText("Detector < 7 >");
        label_det[10].setText("Detector <7A>");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == DATDIA_TYPE) {
                label_field[lsiv_field] = new JTextArea("Detector Type (INactive, PResence, or PUlse)");
            }
            else {
                label_field[lsiv_field] = new JTextArea(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG - 1 + lsiv_field]);
            }

            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
        }

        int leg_num_has_lane = 0;

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                continue;
            leg_num_has_lane++;
        }

        if (leg_num_has_lane == 0) {
            JOptionPane.showMessageDialog(null, "Number of legs with inbound lanes is 0", "The number of lanes is error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        cbo_det[1][DATDIA_LEG] = new JComboBox();
        cbo_det[2][DATDIA_LEG] = new JComboBox();
        cbo_det[3][DATDIA_LEG] = new JComboBox();
        cbo_det[4][DATDIA_LEG] = new JComboBox();
        cbo_det[5][DATDIA_LEG] = new JComboBox();
        cbo_det[6][DATDIA_LEG] = new JComboBox();
        cbo_det[7][DATDIA_LEG] = new JComboBox();
        cbo_det[8][DATDIA_LEG] = new JComboBox();
        cbo_det[9][DATDIA_LEG] = new JComboBox();
        cbo_det[10][DATDIA_LEG] = new JComboBox();

        cbo_det[1][DATDIA_LEG].addItem("5");
        cbo_det[2][DATDIA_LEG].addItem("6");
        cbo_det[3][DATDIA_LEG].addItem("6");
        cbo_det[4][DATDIA_LEG].addItem("IL");
        cbo_det[5][DATDIA_LEG].addItem("IL");
        cbo_det[6][DATDIA_LEG].addItem("IR");
        cbo_det[7][DATDIA_LEG].addItem("IR");
        cbo_det[8][DATDIA_LEG].addItem("2");
        cbo_det[9][DATDIA_LEG].addItem("3");
        cbo_det[10][DATDIA_LEG].addItem("3");

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            int legNum;
            if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
                legNum = 0;
            }
            else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
                legNum = PARAMS.TEXAS_MODEL_NLGP1;
            }
            else {
                legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
            }

            String[] array_DATDIA_FL = new String[number_of_lanes[legNum]];

            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[legNum]; lsiv_lane++) {
                array_DATDIA_FL[lsiv_lane - 1] = Integer.toString(lsiv_lane);
            }

            cbo_det[lsiv_det][DATDIA_FL] = new JComboBox(array_DATDIA_FL);
        }

        if (gdvsim.flag_detDataForTexdia_ok) {
            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATDIA_FL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL]));
                }
                else {
                    cbo_det[lsiv_det][DATDIA_FL].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_fl));
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                cbo_det[lsiv_det][DATDIA_FL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL]));
            }
        }

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            int legNum;
            if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
                legNum = 0;
            }
            else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
                legNum = PARAMS.TEXAS_MODEL_NLGP1;
            }
            else {
                legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
            }

            int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString()).intValue();
            int numCovered = number_of_lanes[legNum] - firstLane + 1;

            String[] array_DATDIA_NL = new String[numCovered];

            for (int lsiv_lane = 1; lsiv_lane <= numCovered; lsiv_lane++) {
                array_DATDIA_NL[lsiv_lane - 1] = Integer.toString(lsiv_lane);
            }
            cbo_det[lsiv_det][DATDIA_NL] = new JComboBox(array_DATDIA_NL);
        }

        if (gdvsim.flag_detDataForTexdia_ok) {
            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATDIA_NL].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_nl));
                }
                else {
                    cbo_det[lsiv_det][DATDIA_NL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL]));
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                cbo_det[lsiv_det][DATDIA_NL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL]));
            }
        }

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        int count;
        int int_number;

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA];

        count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        int_number = lsiv_min;
        String[] array_DATDIA_SPA = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_DATDIA_SPA[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN];

        count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        int_number = lsiv_min;
        String[] array_DATDIA_LEN = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_DATDIA_LEN[lsiv_i] = Integer.toString(int_number);
            int_number += lsiv_inc;
        }

        String type = "|IN|PR|PU|";
        String[] array_DATDIA_TYPE = type.substring(1).split("\\|");

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            cbo_det[lsiv_det][DATDIA_SPA] = new JComboBox(array_DATDIA_SPA);
            cbo_det[lsiv_det][DATDIA_LEN] = new JComboBox(array_DATDIA_LEN);
            cbo_det[lsiv_det][DATDIA_TYPE] = new JComboBox(array_DATDIA_TYPE);
        }

        if (gdvsim.flag_detDataForTexdia_ok) {
            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATDIA_SPA].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_spa));
                }
                else {
                    cbo_det[lsiv_det][DATDIA_SPA].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATDIA_LEN].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_len));
                }
                else {
                    cbo_det[lsiv_det][DATDIA_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN]));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_det[lsiv_det][DATDIA_TYPE].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mstv_type);
                }
                else {
                    cbo_det[lsiv_det][DATDIA_TYPE].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE]);
                }
            }
        }
        else {
            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                cbo_det[lsiv_det][DATDIA_SPA].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA]));
                cbo_det[lsiv_det][DATDIA_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN]));
                cbo_det[lsiv_det][DATDIA_TYPE].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE]);
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

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            label_lane[lsiv_lane] = new JLabel("lane " + lsiv_lane);
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            if (lsiv_leg == 0) {
                label_leg[lsiv_leg] = new JLabel("leg IR");
            }
            else if (lsiv_leg == PARAMS.TEXAS_MODEL_NLGP1) {
                label_leg[lsiv_leg] = new JLabel("leg IL");
            }
            else {
                label_leg[lsiv_leg] = new JLabel("leg " + lsiv_leg);
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                num_of_dets_per_lane[lsiv_leg][lsiv_lane] = 0;
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                label_value[lsiv_leg][lsiv_lane] = new JLabel(Integer.toString(num_of_dets_per_lane[lsiv_leg][lsiv_lane]));
                label_value[lsiv_leg][lsiv_lane].setBorder(BorderFactory.createTitledBorder(""));

                cbo_table[lsiv_leg][lsiv_lane] = new JComboBox();
                cbo_table[lsiv_leg][lsiv_lane].addItem(Integer.toString(num_of_dets_per_lane[lsiv_leg][lsiv_lane]));

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

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            gbConstraintsPanelTable.fill = GridBagConstraints.BOTH;
            gbConstraintsPanelTable.insets = new Insets(1, 1, 1, 1);
            gbConstraintsPanelTable.gridx = 1;
            gbConstraintsPanelTable.gridy = lsiv_leg + 2;
            gbConstraintsPanelTable.gridwidth = 1;
            gbConstraintsPanelTable.gridheight = 1;
            gbLayoutPanelTable.setConstraints(label_leg[lsiv_leg], gbConstraintsPanelTable);
            panel_table.add(label_leg[lsiv_leg]);
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
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

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            if (number_of_lanes[lsiv_leg] > max_number_of_lanes) {
                max_number_of_lanes = number_of_lanes[lsiv_leg];
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            label_leg[lsiv_leg].setVisible(true);
        }

        for (int lsiv_lane = 1; lsiv_lane <= max_number_of_lanes; lsiv_lane++) {
            label_lane[lsiv_lane].setVisible(true);
        }

        for (int lsiv_lane = max_number_of_lanes + 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            label_lane[lsiv_lane].setVisible(false);
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                label_value[lsiv_leg][lsiv_lane].setVisible(false);
                cbo_table[lsiv_leg][lsiv_lane].setVisible(false);
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                label_value[lsiv_leg][lsiv_lane].setVisible(true);
                cbo_table[lsiv_leg][lsiv_lane].setVisible(true);
            }
        }

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            cbo_det[lsiv_det][DATDIA_LEG].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATDIA_FL].addActionListener(componentActionListener);
            cbo_det[lsiv_det][DATDIA_NL].addActionListener(componentActionListener);

            cbo_det[lsiv_det][DATDIA_LEG].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATDIA_FL].addKeyListener(componentKeyListener);
            cbo_det[lsiv_det][DATDIA_NL].addKeyListener(componentKeyListener);
        }

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cbo_det[lsiv_det][lsiv_field].addKeyListener(openComboMenuListener);
                cbo_det[lsiv_det][lsiv_field].addKeyListener(helpListener);
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                cbo_table[lsiv_leg][lsiv_lane].addKeyListener(openComboMenuListener);
                cbo_table[lsiv_leg][lsiv_lane].addKeyListener(helpListener);
            }
        }

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
        int numOfColumns = 7;

        gbConstraints.insets = new Insets(1, 1, 4, 1);
        addComponent(panel_title, iRow++, 0, numOfColumns, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == NUMOFFIELD) {
                gbConstraints.insets = new Insets(1, 1, 1, 5);
            }
            else {
                gbConstraints.insets = new Insets(1, 1, 1, 1);
            }
            addComponent(label_field[lsiv_field], iRow, lsiv_field, 1, 1);
        }

        iRow++;

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            gbConstraints.insets = new Insets(1, 5, 1, 1);
            addComponent(label_det[lsiv_det], iRow, 0, 1, 1);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == NUMOFFIELD) {
                    gbConstraints.insets = new Insets(1, 1, 1, 5);
                }
                else {
                    gbConstraints.insets = new Insets(1, 1, 1, 1);
                }
                addComponent(cbo_det[lsiv_det][lsiv_field], iRow, lsiv_field, 1, 1);
            }

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(panel_lbl_tbl_title, iRow++, 0, numOfColumns, 1);
        addComponent(panel_table, iRow++, 0, numOfColumns, 1);
        addComponent(ok_panel, iRow++, 0, numOfColumns, 1);

        aFrame.setSize(1000, 750);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
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
        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cbo_det[lsiv_det][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG - 1 + lsiv_field] + " for Detector " + lsiv_det);
                cbo_det[lsiv_det][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG - 1 + lsiv_field] + " for Detector " + lsiv_det);
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                String legName;

                if (lsiv_leg == 0) {
                    legName = "IR";
                }
                else if (lsiv_leg == PARAMS.TEXAS_MODEL_NLGP1) {
                    legName = "IL";
                }
                else {
                    legName = Integer.toString(lsiv_leg);
                }

                cbo_table[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleName("Number of Detectors for leg " + legName + " inbound lane " + lsiv_lane);
                cbo_table[lsiv_leg][lsiv_lane].getAccessibleContext().setAccessibleDescription("Number of Detectors for leg " + legName + " inbound lane " + lsiv_lane);
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

    void calculateNumberOfDetectorsPerLane() {
        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                num_of_dets_per_lane[lsiv_leg][lsiv_lane] = 0;
            }
        }

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            int legNum;
            if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
                legNum = 0;
            }
            else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
                legNum = PARAMS.TEXAS_MODEL_NLGP1;
            }
            else {
                legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
            }

            int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString()).intValue();
            int numCovered = Integer.valueOf(cbo_det[lsiv_det][DATDIA_NL].getSelectedItem().toString()).intValue();

            for (int lsiv_lane = firstLane; lsiv_lane < firstLane + numCovered; lsiv_lane++) {
                num_of_dets_per_lane[legNum][lsiv_lane]++;
            }
        }

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
            for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                label_value[lsiv_leg][lsiv_lane].setText(Integer.toString(num_of_dets_per_lane[lsiv_leg][lsiv_lane]));

                cbo_table[lsiv_leg][lsiv_lane].removeAllItems();
                cbo_table[lsiv_leg][lsiv_lane].addItem(Integer.toString(num_of_dets_per_lane[lsiv_leg][lsiv_lane]));
            }
        }
    } // end of method calculateNumberOfDetectorsPerLane()

    void LegAction(int lsiv_det) {
        cbo_det[lsiv_det][DATDIA_FL].removeActionListener(componentActionListener);
        cbo_det[lsiv_det][DATDIA_FL].removeKeyListener(componentKeyListener);
        cbo_det[lsiv_det][DATDIA_NL].removeActionListener(componentActionListener);
        cbo_det[lsiv_det][DATDIA_NL].removeKeyListener(componentKeyListener);

        int legNum;
        if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
            legNum = 0;
        }
        else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
            legNum = PARAMS.TEXAS_MODEL_NLGP1;
        }
        else {
            legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
        }

        cbo_det[lsiv_det][DATDIA_FL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[legNum]; lsiv_lane++) {
            cbo_det[lsiv_det][DATDIA_FL].addItem(Integer.toString(lsiv_lane));
        }

        int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString()).intValue();
        int rangeForField3 = number_of_lanes[legNum] - firstLane + 1;

        cbo_det[lsiv_det][DATDIA_NL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= rangeForField3; lsiv_lane++) {
            cbo_det[lsiv_det][DATDIA_NL].addItem(Integer.toString(lsiv_lane));
        }

        calculateNumberOfDetectorsPerLane();

        cbo_det[lsiv_det][DATDIA_FL].addActionListener(componentActionListener);
        cbo_det[lsiv_det][DATDIA_FL].addKeyListener(componentKeyListener);
        cbo_det[lsiv_det][DATDIA_NL].addActionListener(componentActionListener);
        cbo_det[lsiv_det][DATDIA_NL].addKeyListener(componentKeyListener);

    } // end of method LegAction

    void FirstLaneAction(int lsiv_det) {
        cbo_det[lsiv_det][DATDIA_NL].removeActionListener(componentActionListener);
        cbo_det[lsiv_det][DATDIA_NL].removeKeyListener(componentKeyListener);

        int legNum;
        if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
            legNum = 0;
        }
        else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
            legNum = PARAMS.TEXAS_MODEL_NLGP1;
        }
        else {
            legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
        }

        int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString()).intValue();

        int rangeForField3 = number_of_lanes[legNum] - firstLane + 1;

        cbo_det[lsiv_det][DATDIA_NL].removeAllItems();

        for (int lsiv_lane = 1; lsiv_lane <= rangeForField3; lsiv_lane++) {
            cbo_det[lsiv_det][DATDIA_NL].addItem(Integer.toString(lsiv_lane));
        }

        calculateNumberOfDetectorsPerLane();

        cbo_det[lsiv_det][DATDIA_NL].addActionListener(componentActionListener);
        cbo_det[lsiv_det][DATDIA_NL].addKeyListener(componentKeyListener);
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

                for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        if (event.getSource() == cbo_table[lsiv_leg][lsiv_lane]) {
                            String legName;

                            if (lsiv_leg == 0) {
                                legName = "IR";
                            }
                            else if (lsiv_leg == PARAMS.TEXAS_MODEL_NLGP1) {
                                legName = "IL";
                            }
                            else {
                                legName = Integer.toString(lsiv_leg);
                            }

                            new HelpDialog(
                                    true,
                                    "Number of Detectors for leg " + legName + " inbound lane " + lsiv_lane,
                                    "Number of Detectors for leg " + legName + " inbound lane " + lsiv_lane,
                                    "The user cannot specify this item.  This item is the Number of Detectors for leg " + legName + " inbound lane " + lsiv_lane + " and is determined by the program.",
                                    cbo_table[lsiv_leg][lsiv_lane].getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NLS), "1");
                        }
                    }
                }

                for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                    if (event.getSource() == cbo_det[lsiv_det][DATDIA_LEG]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG], cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString(), cbo_det[lsiv_det][DATDIA_LEG]
                                        .getSelectedItem().toString(),
                                cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString(), " ", " ", " ");
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_FL]) {
                        int minLeg = 0;
                        int maxLeg = PARAMS.TEXAS_MODEL_NLGP1;

                        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            minLeg = lsiv_leg;
                            break;
                        }

                        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            maxLeg = lsiv_leg;
                        }

                        int legNum;
                        if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
                            legNum = 0;
                        }
                        else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
                            legNum = PARAMS.TEXAS_MODEL_NLGP1;
                        }
                        else {
                            legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
                        }

                        int minLaneField2 = 1;
                        int maxLaneField2 = number_of_lanes[legNum];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL], cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString(), Integer.toString(minLaneField2), " ",
                                Integer.toString(minLaneField2), Integer.toString(maxLaneField2), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_NL]) {
                        int minLeg = 0;
                        int maxLeg = PARAMS.TEXAS_MODEL_NLGP1;

                        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            minLeg = lsiv_leg;
                            break;
                        }

                        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
                            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                                continue;

                            maxLeg = lsiv_leg;
                        }

                        int legNum;
                        if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
                            legNum = 0;
                        }
                        else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
                            legNum = PARAMS.TEXAS_MODEL_NLGP1;
                        }
                        else {
                            legNum = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString()).intValue();
                        }

                        int minLaneField2 = 1;
                        int maxLaneField2 = number_of_lanes[legNum];

                        int firstLane = Integer.valueOf(cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString()).intValue();
                        int rangeForField3 = number_of_lanes[legNum] - firstLane + 1;

                        int minLaneField3 = 1;
                        int maxLaneField3 = rangeForField3;

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL], cbo_det[lsiv_det][DATDIA_NL].getSelectedItem().toString(), Integer.toString(minLaneField3), " ",
                                Integer.toString(minLaneField3), Integer.toString(maxLaneField3), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_SPA]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA], cbo_det[lsiv_det][DATDIA_SPA].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_LEN]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN], cbo_det[lsiv_det][DATDIA_LEN].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN]));
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_TYPE]) {
                        String possibleValue = "|";
                        int n = cbo_det[lsiv_det][DATDIA_TYPE].getItemCount();
                        for (int i = 0; i < n; i++) {
                            possibleValue = possibleValue + cbo_det[lsiv_det][DATDIA_TYPE].getItemAt(i).toString() + "|";
                        }
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_det)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE] + " for Detector " + lsiv_det,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE], cbo_det[lsiv_det][DATDIA_TYPE].getSelectedItem().toString(),
                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE], possibleValue, " ", " ", " ");
                    }
                }
            }
        }
    } // end of class HelpListener()

    boolean isError() {
        boolean flag_error = false;

        for (int lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NLGP1; lsiv_leg++) {
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
        return false;
    } // end of method isError()

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = NUMOFDETECTORS;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
            if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IR")) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_leg = 0;
            }
            else if (cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString().equals("IL")) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_leg = PARAMS.TEXAS_MODEL_NLGP1;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_leg = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEG].getSelectedItem().toString())
                        .intValue();
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_fl = Integer.valueOf(cbo_det[lsiv_det][DATDIA_FL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_nl = Integer.valueOf(cbo_det[lsiv_det][DATDIA_NL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_spa = Integer.valueOf(cbo_det[lsiv_det][DATDIA_SPA].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_len = Integer.valueOf(cbo_det[lsiv_det][DATDIA_LEN].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mstv_type = cbo_det[lsiv_det][DATDIA_TYPE].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_delay = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mdfv_extend = 0.0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msiv_class_num = 0;

            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msta_class_name[lsiv_class] = "";
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msia_class_length_lower[lsiv_class] = 0;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].msia_class_length_upper[lsiv_class] = 999;
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_FL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_NL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_SPA] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_LEN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_DELAY] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_EXTEND] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NUM] = gdvsim.gclv_inter.TX_FROM_USER;

            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1
                        + lsiv_class] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1
                        + lsiv_class] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_det.mcla_det_data[lsiv_det].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1
                        + lsiv_class] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }
        gdvsim.gclv_inter.calculate_graphics_and_paint();
    } // end of method saveData()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_detDataForTexdia_ok = true;

                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                if (event.getSource() == cbo_det[lsiv_det][DATDIA_LEG]) {
                    LegAction(lsiv_det);
                    break;
                }
                else if (event.getSource() == cbo_det[lsiv_det][DATDIA_FL]) {
                    FirstLaneAction(lsiv_det);
                    break;
                }
                else if (event.getSource() == cbo_det[lsiv_det][DATDIA_NL]) {
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
                        gdvsim.flag_detDataForTexdia_ok = true;

                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_det = 1; lsiv_det <= NUMOFDETECTORS; lsiv_det++) {
                    if (event.getSource() == cbo_det[lsiv_det][DATDIA_LEG]) {
                        LegAction(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_FL]) {
                        FirstLaneAction(lsiv_det);
                        break;
                    }
                    else if (event.getSource() == cbo_det[lsiv_det][DATDIA_NL]) {
                        calculateNumberOfDetectorsPerLane();
                        break;
                    }
                }
            }
        }
    } // end of class ComponentKeyListener

} // end of class DetectorDataForTexdiaDialog
