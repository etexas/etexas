package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                     DiamondInternalLaneDataDialog.java                     */
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
import javax.accessibility.*;

class DiamondInternalLaneDataDialog extends JDialog {

    static final int FIELD_LANE_WIDTH = 1;

    static final int FIELD_MVMTCODERI = 2;

    static final int FIELD_MVMTCODELI = 3;

    static final int FIELD_LENULANERI = 4;

    static final int FIELD_LENULANELI = 5;

    static final int FIELD_OFFSETLTRI = 6;

    static final int FIELD_OFFSETLTLI = 7;

    static final int FIELD_VTYPEALLOW = 8;

    static final int NUMOFFIELD = 8;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel[] label_field_right = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_field_left = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_field_right_num = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_field_left_num = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_lane_right = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JLabel[] label_lane_left = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JComboBox[][] comboBox_right = new JComboBox[PARAMS.TEXAS_MODEL_NAL + 1][NUMOFFIELD + 1];

    JComboBox[][] comboBox_left = new JComboBox[PARAMS.TEXAS_MODEL_NAL + 1][NUMOFFIELD + 1];

    JLabel label_title;

    JLabel label_title_right, label_title_left;

    JLabel[] label_note = new JLabel[11];

    JButton okButton, applyButton, cancelButton;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    Font font1, font2, font3;

    int num_of_lanes_right, num_of_lanes_left;

    public DiamondInternalLaneDataDialog() {
        num_of_lanes_right = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
        num_of_lanes_left = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;

        titleString = "Diamond Interchange Internal Lane Data";
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

        font1 = new Font("TimesRoman", Font.BOLD, 20);
        font2 = new Font("TimesRoman", Font.BOLD, 18);
        font3 = new Font("TimesRoman", Font.BOLD, 16);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE];

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        label_title_right = new JLabel("Inbound to Right Intersection");
        label_title_left = new JLabel("Inbound to Left Intersection");
        label_title_right.setFont(font2);
        label_title_left.setFont(font2);

        label_note[0] = new JLabel("NOTE:");
        label_note[1] = new JLabel("Item #" + FIELD_MVMTCODERI + " & #" + FIELD_MVMTCODELI + " Movement Code 'U' = U-Turn, 'L' = Left turn, 'S' = Straight, and 'R' = Right");
        label_note[2] = new JLabel("Item #" + FIELD_LENULANERI + "=0 and Item #" + FIELD_LENULANELI + "=0 specifies a lane open at all locations");
        label_note[3] = new JLabel("Item #" + FIELD_LENULANERI + ">0 and Item #" + FIELD_LENULANELI
                + "=0 specifies a lane open near the right intersection    and blocked near the left   intersection");
        label_note[4] = new JLabel("Item #" + FIELD_LENULANERI + "=0 and Item #" + FIELD_LENULANELI
                + ">0 specifies a lane open near the left   intersection    and blocked near the right intersection");
        label_note[5] = new JLabel("Item #" + FIELD_LENULANERI + ">0 and Item #" + FIELD_LENULANELI + ">0 specifies a lane open near the both intersections and blocked in middle");
        label_note[6] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'B' = Bicycles (Vehicle Class attribute Classification is 'BC' or 'BC-1')");
        label_note[7] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'E' = Emergency Vehicles (Special Vehicle attribute Emergency Vehicle is 'YES')");
        label_note[8] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'R' = Rail Vehicles (Vehicle Class attribute Classification is 'RAIL')");
        label_note[9] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'V' = all other Vehicles except Bicycles, Emergency Vehicles, and Rail Vehicles");
        label_note[10] = new JLabel("END NOTE");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field_right_num[lsiv_field] = new JLabel(lsiv_field + ". ");
            label_field_left_num[lsiv_field] = new JLabel(lsiv_field + ". ");
            label_field_right[lsiv_field] = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]);
            label_field_left[lsiv_field] = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]);
        }

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            label_lane_right[lsiv_lane] = new JLabel("Inbound " + lsiv_lane);
            label_lane_left[lsiv_lane] = new JLabel("Inbound " + lsiv_lane);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == FIELD_MVMTCODERI) {
                String stringToTokenizer;

                if (gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                    // there are no outbound lanes for diamond interchange leg 3 thus right turns
                    // are not allowed
                    stringToTokenizer = "|XX|L|LS|S|";
                }
                else {
                    stringToTokenizer = "|XX|L|LS|LR|S|SR|R|";
                }

                String[] array_lane = stringToTokenizer.substring(1).split("\\|");

                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                    comboBox_left[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                }
            }
            else if (lsiv_field == FIELD_MVMTCODELI) {
                String stringToTokenizer;

                if (gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                    // there are no outbound lanes for diamond interchange leg 6 thus right turns
                    // are not allowed
                    stringToTokenizer = "|XX|L|LS|S|";
                }
                else {
                    stringToTokenizer = "|XX|L|LS|LR|S|SR|R|";
                }

                String[] array_lane = stringToTokenizer.substring(1).split("\\|");

                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                    comboBox_left[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                }
            }
            else if (lsiv_field == FIELD_VTYPEALLOW) {
                String[] array_lane = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE].substring(1).split("\\|");

                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                    comboBox_left[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                }
            }
            else {
                int ArrayIndex, SizeOfArray, intArrayElementValue;

                int lsiv_min;
                int lsiv_max;
                int lsiv_inc;

                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field];

                if ((lsiv_field == FIELD_LENULANERI) || (lsiv_field == FIELD_LENULANELI)) {
                    lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - 1;
                }

                SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                intArrayElementValue = lsiv_min;
                String[] array_lane = new String[SizeOfArray];

                for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                    array_lane[ArrayIndex] = Integer.toString(intArrayElementValue);
                    intArrayElementValue += lsiv_inc;
                }

                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                    comboBox_left[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                }
            } // end of if ( ( lsiv_field == FIELD_MVMTCODERI ) || ( lsiv_field == FIELD_MVMTCODELI
              // ) )
        } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if ((lsiv_field == FIELD_MVMTCODERI) || (lsiv_field == FIELD_MVMTCODELI)) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field].setSelectedItem(setDefaultForMovementCode(lsiv_lane));
                    comboBox_left[lsiv_lane][lsiv_field].setSelectedItem(setDefaultForMovementCode(lsiv_lane));
                }
            }
            else if (lsiv_field == FIELD_VTYPEALLOW) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE]);
                    comboBox_left[lsiv_lane][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE]);
                }
            }
            else {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_right[lsiv_lane][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]));
                    comboBox_left[lsiv_lane][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]));
                }
            }
        }

        if (gdvsim.gclv_inter.mbov_Diamond_Lane_OK) {
            for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
                comboBox_right[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width));
                comboBox_right[lsiv_lane][FIELD_MVMTCODERI]
                        .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_right.mstv_code);
                comboBox_right[lsiv_lane][FIELD_MVMTCODELI]
                        .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_left.mstv_code);
                comboBox_right[lsiv_lane][FIELD_LENULANERI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_right));
                comboBox_right[lsiv_lane][FIELD_LENULANELI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_left));
                comboBox_right[lsiv_lane][FIELD_OFFSETLTRI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_right));
                comboBox_right[lsiv_lane][FIELD_OFFSETLTLI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left));
                comboBox_right[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mstv_allowed_veh_type);
            }

            for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
                comboBox_left[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_int_lane_width));
                comboBox_left[lsiv_lane][FIELD_MVMTCODERI]
                        .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_right.mstv_code);
                comboBox_left[lsiv_lane][FIELD_MVMTCODELI]
                        .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_left.mstv_code);
                comboBox_left[lsiv_lane][FIELD_LENULANERI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_right));
                comboBox_left[lsiv_lane][FIELD_LENULANELI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_left));
                comboBox_left[lsiv_lane][FIELD_OFFSETLTRI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_right));
                comboBox_left[lsiv_lane][FIELD_OFFSETLTLI].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_left));
                comboBox_left[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mstv_allowed_veh_type);
            }
        } // end of else for if(gdvsim.gclv_inter.mboa_Leg_Lane_Data_OK[leg_number])
        else {
            for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH]));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_MVMTCODERI].setSelectedItem(setDefaultForMovementCode(lsiv_lane));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_MVMTCODERI]
                            .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_right.mstv_code);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_MVMTCODELI].setSelectedItem(setDefaultForMovementCode(lsiv_lane));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_MVMTCODELI]
                            .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_left.mstv_code);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_LENULANERI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT]));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_LENULANERI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_right));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_LENULANELI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT]));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_LENULANELI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_left));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_OFFSETLTRI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT]));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_OFFSETLTRI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_right));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_OFFSETLTLI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT]));
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_OFFSETLTLI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_right[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE]);
                }
                else {
                    comboBox_right[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mstv_allowed_veh_type);
                }
            }

            for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH]));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_int_lane_width));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_MVMTCODERI].setSelectedItem(setDefaultForMovementCode(lsiv_lane));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_MVMTCODERI]
                            .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_right.mstv_code);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_MVMTCODELI].setSelectedItem(setDefaultForMovementCode(lsiv_lane));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_MVMTCODELI]
                            .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_left.mstv_code);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_LENULANERI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT]));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_LENULANERI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_right));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_LENULANELI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT]));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_LENULANELI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_left));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_OFFSETLTRI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT]));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_OFFSETLTRI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_right));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_OFFSETLTLI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT]));
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_OFFSETLTLI].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_left));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_left[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE]);
                }
                else {
                    comboBox_left[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mstv_allowed_veh_type);
                }
            }
        } // end of else for if(gdvsim.gclv_inter.mboa_Leg_Lane_Data_OK[leg_number])

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel ok_Panel = new JPanel();
        ok_Panel.add(okButton);
        ok_Panel.add(applyButton);
        ok_Panel.add(cancelButton);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                comboBox_right[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleName(
                        label_title.getText() + " " + label_field_right[lsiv_field].getText() + " for " + label_title_right.getText() + " " + label_lane_right[lsiv_lane].getText());
                comboBox_right[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        label_title.getText() + " " + label_field_right[lsiv_field].getText() + " for " + label_title_right.getText() + " " + label_lane_right[lsiv_lane].getText());
                comboBox_left[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleName(
                        label_title.getText() + " " + label_field_left[lsiv_field].getText() + " for " + label_title_left.getText() + " " + label_lane_left[lsiv_lane].getText());
                comboBox_left[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        label_title.getText() + " " + label_field_left[lsiv_field].getText() + " for " + label_title_left.getText() + " " + label_lane_left[lsiv_lane].getText());
            }
        }

        okButton.getAccessibleContext().setAccessibleName("OK ");
        okButton.getAccessibleContext().setAccessibleDescription("OK ");
        applyButton.getAccessibleContext().setAccessibleName("Apply ");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply ");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel ");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel ");

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                comboBox_right[lsiv_lane][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_left[lsiv_lane][lsiv_field].addKeyListener(openComboMenuListener);

                comboBox_right[lsiv_lane][lsiv_field].addKeyListener(helpListener);
                comboBox_left[lsiv_lane][lsiv_field].addKeyListener(helpListener);
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

        int iRow = 0;
        int iCol;

        if (num_of_lanes_right >= num_of_lanes_left) {
            iCol = num_of_lanes_right + 2;
        }
        else {
            iCol = num_of_lanes_left + 2;
        }

        gbConstraints.fill = GridBagConstraints.BOTH;

        gbConstraints.insets = new Insets(2, 2, 2, 2);

        addComponent(panel_title, iRow++, 0, iCol, 1);

        for (int lsiv_i = 0; lsiv_i < label_note.length; lsiv_i++) {
            addComponent(label_note[lsiv_i], iRow++, 0, iCol, 1);
        }

        if (num_of_lanes_right != 0) {
            addComponent(label_title_right, iRow, 0, 2, 1);

            for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
                addComponent(label_lane_right[lsiv_lane], iRow, lsiv_lane + 1, 1, 1);
            }

            iRow++;

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(label_field_right_num[lsiv_field], iRow, 0, 1, 1);
                addComponent(label_field_right[lsiv_field], iRow, 1, 1, 1);

                for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
                    addComponent(comboBox_right[lsiv_lane][lsiv_field], iRow, lsiv_lane + 1, 1, 1);
                }

                iRow++;
            }
        }

        if (num_of_lanes_left != 0) {
            addComponent(label_title_left, iRow, 0, 2, 1);

            for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
                addComponent(label_lane_left[lsiv_lane], iRow, lsiv_lane + 1, 1, 1);
            }

            iRow++;

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(label_field_left_num[lsiv_field], iRow, 0, 1, 1);
                addComponent(label_field_left[lsiv_field], iRow, 1, 1, 1);

                for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
                    addComponent(comboBox_left[lsiv_lane][lsiv_field], iRow, lsiv_lane + 1, 1, 1);
                }

                iRow++;
            }
        }

        addComponent(ok_Panel, iRow, 0, iCol, 1);

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method DiamondInternalLaneDataDialog

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

    String setDefaultForMovementCode(int lane) {
        String retValue;

        if (lane == 1) {
            retValue = "LS";
        }
        else {
            retValue = "S";
        }

        return retValue;

    } // end of setDefaultForMovementCode

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        if (event.getSource() == comboBox_right[lsiv_lane][lsiv_field]) {
                            String desc;
                            desc = lclv_tx_fmt.mstv_name;
                            desc = desc.replaceFirst("#", Integer.toString(lsiv_lane));
                            desc = desc.replaceFirst("#", "Right");

                            if ((lsiv_field == FIELD_MVMTCODERI) || (lsiv_field == FIELD_MVMTCODELI)) {
                                String possibleValue = "|";
                                int n = comboBox_right[lsiv_lane][lsiv_field].getItemCount();
                                for (int i = 0; i < n; i++) {
                                    possibleValue = possibleValue + comboBox_right[lsiv_lane][lsiv_field].getItemAt(i).toString() + "|";
                                }
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field] + " for Inbound Lane " + lsiv_lane + " to Right Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], comboBox_right[lsiv_lane][lsiv_field].getSelectedItem()
                                                .toString(),
                                        setDefaultForMovementCode(lsiv_lane), possibleValue, " ", " ", " ");
                            }
                            else if ((lsiv_field == FIELD_LENULANERI) || (lsiv_field == FIELD_LENULANELI)) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field] + " for Inbound Lane " + lsiv_lane + " to Right Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], comboBox_right[lsiv_lane][lsiv_field].getSelectedItem()
                                                .toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - 1),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]));
                            }
                            else if (lsiv_field == FIELD_VTYPEALLOW) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] + " for Inbound Lane " + lsiv_lane + " to Right Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE], comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE],
                                        " ", " ", " ");
                            }
                            else {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field] + " for Inbound Lane " + lsiv_lane + " to Right Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], comboBox_right[lsiv_lane][lsiv_field].getSelectedItem()
                                                .toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]));
                            }
                        }

                        if (event.getSource() == comboBox_left[lsiv_lane][lsiv_field]) {
                            String desc;
                            desc = lclv_tx_fmt.mstv_name;
                            desc = desc.replaceFirst("#", Integer.toString(lsiv_lane));
                            desc = desc.replaceFirst("#", "Left");

                            if ((lsiv_field == FIELD_MVMTCODERI) || (lsiv_field == FIELD_MVMTCODELI)) {
                                String possibleValue = "|";
                                int n = comboBox_left[lsiv_lane][lsiv_field].getItemCount();
                                for (int i = 0; i < n; i++) {
                                    possibleValue = possibleValue + comboBox_left[lsiv_lane][lsiv_field].getItemAt(i).toString() + "|";
                                }
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field] + " for Inbound Lane " + lsiv_lane + " to Left Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field],
                                        comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString(), setDefaultForMovementCode(lsiv_lane), possibleValue, " ", " ", " ");
                            }
                            else if ((lsiv_field == FIELD_LENULANERI) || (lsiv_field == FIELD_LENULANELI)) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field] + " for Inbound Lane " + lsiv_lane + " to Left Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field],
                                        comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH
                                                - 1 + lsiv_field]),
                                        " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between - 1),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]));
                            }
                            else if (lsiv_field == FIELD_VTYPEALLOW) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] + " for Inbound Lane " + lsiv_lane + " to Left Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE], comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE],
                                        " ", " ", " ");
                            }
                            else {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field], desc,
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field] + " for Inbound Lane " + lsiv_lane + " to Left Intersection",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field],
                                        comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH
                                                - 1 + lsiv_field]),
                                        " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH - 1 + lsiv_field]));
                            }
                        }
                    }
                }

                if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == applyButton) {
                    new HelpDialog(true, "Apply button", "The Apply button saves the data but does not close the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                } // end of else if(event.getSource() == cancelButton)
            }
        }
    } // end of HelpListener

    void saveData() {
        int lsiv_leg_cl = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        int lsiv_leg_cr = 0;

        for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width = Integer.valueOf(
                    comboBox_right[lsiv_lane][FIELD_LANE_WIDTH].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_right.mstv_code = comboBox_right[lsiv_lane][FIELD_MVMTCODERI]
                    .getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_left.mstv_code = comboBox_right[lsiv_lane][FIELD_MVMTCODELI]
                    .getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_right = Integer.valueOf(
                    comboBox_right[lsiv_lane][FIELD_LENULANERI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_left = Integer.valueOf(
                    comboBox_right[lsiv_lane][FIELD_LENULANELI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_right = Integer.valueOf(
                    comboBox_right[lsiv_lane][FIELD_OFFSETLTRI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left = Integer.valueOf(
                    comboBox_right[lsiv_lane][FIELD_OFFSETLTLI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mstv_allowed_veh_type = comboBox_right[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem()
                    .toString();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] = gdvsim.gclv_inter.TX_FROM_USER;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_right.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mstv_allowed_veh_type;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_left.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mstv_allowed_veh_type;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }

        for (int lsiv_lane = num_of_lanes_right + 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_int_lane_width = Integer.valueOf(
                    comboBox_left[lsiv_lane][FIELD_LANE_WIDTH].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_right.mstv_code = comboBox_left[lsiv_lane][FIELD_MVMTCODERI]
                    .getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_left.mstv_code = comboBox_left[lsiv_lane][FIELD_MVMTCODELI]
                    .getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_right = Integer.valueOf(
                    comboBox_left[lsiv_lane][FIELD_LENULANERI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_left = Integer.valueOf(
                    comboBox_left[lsiv_lane][FIELD_LENULANELI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_right = Integer.valueOf(
                    comboBox_left[lsiv_lane][FIELD_OFFSETLTRI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_left = Integer.valueOf(
                    comboBox_left[lsiv_lane][FIELD_OFFSETLTLI].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mstv_allowed_veh_type = comboBox_left[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem().toString();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] = gdvsim.gclv_inter.TX_FROM_USER;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_left.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mstv_allowed_veh_type;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_mvmt_near_center_right.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].msiv_offset_near_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mstv_allowed_veh_type;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }

        for (int lsiv_lane = num_of_lanes_left + 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_lane].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    }

    boolean isErrorMovementCode() {
        for (int lsiv_lane = 2; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
            if (comboBox_right[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem().toString().indexOf("V") == -1) {
                continue;
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_MVMTCODERI) {
                    if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                        if (comboBox_right[lsiv_lane - 1][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                            JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] + " for Inbound Lane "
                                    + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"L\"!", "Error Message",
                                    JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                    }

                    if ((comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1)
                            || (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1)) {
                        if (comboBox_right[lsiv_lane - 1][lsiv_field].getSelectedItem().toString().indexOf("S") != -1) {
                            if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"S\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"L\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"S\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }

                    if ((comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1)
                            || (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1)
                            || (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("S") != -1)) {
                        if (comboBox_right[lsiv_lane - 1][lsiv_field].getSelectedItem().toString().indexOf("R") != -1) {
                            if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"R\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            else if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"L\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"R\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"R\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }
                } // end of if ( lsiv_field == FIELD_MVMTCODERI )
            }
        } // end of for(int lsiv_lane = 2; lsiv_lane <= num_of_lanes_right; lsiv_lane++)

        for (int lsiv_lane = num_of_lanes_right - 1; lsiv_lane >= 1; lsiv_lane--) {
            if (comboBox_right[lsiv_lane + 1][FIELD_VTYPEALLOW].getSelectedItem().toString().indexOf("V") == -1) {
                continue;
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_MVMTCODERI) {
                    if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("R") != -1) {
                        if (comboBox_right[lsiv_lane + 1][lsiv_field].getSelectedItem().toString().indexOf("S") != -1) {
                            JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] + " for Inbound Lane "
                                    + lsiv_lane + " containing \"R\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"S\"!", "Error Message",
                                    JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                    }

                    if ((comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("R") != -1)
                            || (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("S") != -1)) {
                        if (comboBox_right[lsiv_lane + 1][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                            if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("R") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"R\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"L\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"L\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }

                    if ((comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1)
                            || (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1)
                            || (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("S") != -1)) {
                        if (comboBox_right[lsiv_lane + 1][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                            if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"U\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            else if (comboBox_right[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"L\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"U\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Right Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT]
                                        + " for Inbound Lane " + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"U\"!",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }
                } // end of if ( lsiv_field == FIELD_MVMTCODERI )
            }
        } // end of for(int lsiv_lane = num_of_lanes_right - 1; lsiv_lane >= 1; lsiv_lane--)

        for (int lsiv_lane = 2; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
            if (comboBox_left[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem().toString().indexOf("V") == -1) {
                continue;
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_MVMTCODELI) {
                    if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                        if (comboBox_left[lsiv_lane - 1][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                            JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                    + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"L\"!", "Error Message",
                                    JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                    }

                    if ((comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1)
                            || (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1)) {
                        if (comboBox_left[lsiv_lane - 1][lsiv_field].getSelectedItem().toString().indexOf("S") != -1) {
                            if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"S\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"L\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"S\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }

                    if ((comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1)
                            || (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1)
                            || (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("S") != -1)) {
                        if (comboBox_left[lsiv_lane - 1][lsiv_field].getSelectedItem().toString().indexOf("R") != -1) {
                            if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"R\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            else if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"L\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"R\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane - 1) + " containing \"R\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }
                } // end of if ( lsiv_field == FIELD_MVMTCODELI )
            }
        } // end of for(int lsiv_lane = 2; lsiv_lane <= num_of_lanes_left; lsiv_lane++)

        for (int lsiv_lane = num_of_lanes_left - 1; lsiv_lane >= 1; lsiv_lane--) {
            if (comboBox_left[lsiv_lane + 1][FIELD_VTYPEALLOW].getSelectedItem().toString().indexOf("V") == -1) {
                continue;
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_MVMTCODELI) {
                    if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("R") != -1) {
                        if (comboBox_left[lsiv_lane + 1][lsiv_field].getSelectedItem().toString().indexOf("S") != -1) {
                            JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                    + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"R\"!", "Error Message",
                                    JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                    }

                    if ((comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("R") != -1)
                            || (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("S") != -1)) {
                        if (comboBox_left[lsiv_lane + 1][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                            if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("R") != -1) {
                                JOptionPane.showMessageDialog(null, "Left inbound to Right Intersection movement code for inbound lane " + lsiv_lane
                                        + " containing \"R\" is incompatible with the movement code for inbound lane " + (lsiv_lane + 1) + " containing \"L\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"L\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            return true;
                        }
                    }

                    if ((comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1)
                            || (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1)
                            || (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("S") != -1)) {
                        if (comboBox_left[lsiv_lane + 1][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                            if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("U") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"U\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"U\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            else if (comboBox_left[lsiv_lane][lsiv_field].getSelectedItem().toString().indexOf("L") != -1) {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"L\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"U\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                            else {
                                JOptionPane.showMessageDialog(null, "Inbound to Left Intersection " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] + " for Inbound Lane "
                                        + lsiv_lane + " containing \"S\" is incompatible with the movement code for Inbound Lane " + (lsiv_lane + 1) + " containing \"U\"!", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                            }
                        }
                    }
                } // end of if ( lsiv_field == FIELD_MVMTCODELI )
            }
        } // end of for(int lsiv_lane = num_of_lanes_left - 1; lsiv_lane >= 1; lsiv_lane--)

        return false;

    } // end of isErrorMovementCode

    boolean isErrorLaneLength() {
        int x, y, z;
        for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_right; lsiv_lane++) {
            x = Integer.valueOf(comboBox_right[lsiv_lane][FIELD_LENULANERI].getSelectedItem().toString()).intValue();
            y = Integer.valueOf(comboBox_right[lsiv_lane][FIELD_LENULANELI].getSelectedItem().toString()).intValue();
            z = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
            if ((x + y) > z) {
                JOptionPane.showMessageDialog(null, "For Inbound " + lsiv_lane + " to Right Intersection, field " + FIELD_LENULANERI + " plus field " + FIELD_LENULANELI + " = " + (x + y)
                        + " should be less than Diamond Interchange Internal Leg Data field " + FIELD_LANE_WIDTH + " = " + z + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= num_of_lanes_left; lsiv_lane++) {
            x = Integer.valueOf(comboBox_left[lsiv_lane][FIELD_LENULANERI].getSelectedItem().toString()).intValue();
            y = Integer.valueOf(comboBox_left[lsiv_lane][FIELD_LENULANELI].getSelectedItem().toString()).intValue();
            z = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
            if ((x + y) > z) {
                JOptionPane.showMessageDialog(null, "For Inbound " + lsiv_lane + " to Left Intersection, field " + FIELD_LENULANERI + " plus field " + FIELD_LENULANELI + " = " + (x + y)
                        + " should be less than Diamond Interchange Internal Leg Data field " + FIELD_LANE_WIDTH + " = " + z + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        return false;
    }

    boolean isError() {
        // twr if (isErrorMovementCode())
        // twr {
        // twr return true;
        // twr }
        if (isErrorLaneLength()) {
            return true;
        }

        return false;
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();

                    gdvsim.gclv_inter.mbov_Diamond_Lane_OK = true;

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

                        gdvsim.gclv_inter.mbov_Diamond_Lane_OK = true;

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

            }
        } // end of method keyPressed
    }// end of OKApplyKeyListener

} // end of class DiamondInternalLaneDataDialog
