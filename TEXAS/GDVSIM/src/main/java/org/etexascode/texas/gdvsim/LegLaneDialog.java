package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             LegLaneDialog.java                             */
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
import javax.accessibility.*;

class LegLaneDialog extends JDialog {

    static final int FIELD_LANE_WIDTH = 1;

    static final int FIELD_MOVEMENTCD = 2;

    static final int FIELD_LENULNINTC = 3;

    static final int FIELD_LENULFOUTE = 4;

    static final int FIELD_OFFSETSLEL = 5;

    static final int FIELD_PERGENTRAF = 6;

    static final int FIELD_VTYPEALLOW = 7;

    static final int NUMOFFIELD = 7;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel[] label_field_inbound = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_field_outbound = new JLabel[NUMOFFIELD + 1];

    JLabel[] label_inbound = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JLabel[] label_outbound = new JLabel[PARAMS.TEXAS_MODEL_NAL + 1];

    JComboBox[][] comboBox_inbound = new JComboBox[PARAMS.TEXAS_MODEL_NAL + 1][NUMOFFIELD + 1];

    JComboBox[][] comboBox_outbound = new JComboBox[PARAMS.TEXAS_MODEL_NAL + 1][NUMOFFIELD + 1];

    JComboBox cbo_total;

    JLabel label_title, label_total;

    JLabel[] label_note = new JLabel[11];

    JButton okButton, applyButton, cancelButton;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    ComboBoxActionListener comboBoxActionListener;

    ComboBoxKeyListener comboBoxKeyListener;

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    Font font1, font2, font3;

    int leg_number, number_of_inbounds, number_of_outbounds;

    public LegLaneDialog(int leg_num) {
        leg_number = leg_num;
        number_of_inbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        number_of_outbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;

        titleString = "Leg " + leg_number + " Lane Data";
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

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA];

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        label_note[0] = new JLabel("NOTE:");
        label_note[1] = new JLabel("Item #" + FIELD_MOVEMENTCD + " Movement Code 'U' = U-Turn, 'L' = Left turn, 'S' = Straight, and 'R' = Right");
        label_note[2] = new JLabel("Item #" + FIELD_LENULNINTC + "=0 and Item #" + FIELD_LENULFOUTE + "=0 specifies a lane open at all locations");
        label_note[3] = new JLabel("Item #" + FIELD_LENULNINTC + ">0 and Item #" + FIELD_LENULFOUTE + "=0 specifies a lane open near intersection center and blocked on outer end");
        label_note[4] = new JLabel("Item #" + FIELD_LENULNINTC + "=0 and Item #" + FIELD_LENULFOUTE + ">0 specifies a lane open on outer end and blocked near intersection center");
        label_note[5] = new JLabel("Item #" + FIELD_LENULNINTC + ">0 and Item #" + FIELD_LENULFOUTE + ">0 specifies a lane open near intersection center, blocked in middle, and open on outer end");
        label_note[6] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'B' = Bicycles (Vehicle Class attribute Classification is 'BC' or 'BC-1')");
        label_note[7] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'E' = Emergency Vehicles (Special Vehicle attribute Emergency Vehicle is 'YES')");
        label_note[8] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'R' = Rail Vehicles (Vehicle Class attribute Classification is 'RAIL')");
        label_note[9] = new JLabel("Item #" + FIELD_VTYPEALLOW + " Vehicle Type 'V' = all other Vehicles except Bicycles, Emergency Vehicles, and Rail Vehicles");
        label_note[10] = new JLabel("END NOTE");

        JLabel label_1 = new JLabel("Inbound");
        JLabel label_2 = new JLabel("Outbound");
        label_1.setFont(font2);
        label_2.setFont(font2);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field_inbound[lsiv_field] = new JLabel(lsiv_field + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]);
            label_field_outbound[lsiv_field] = new JLabel(lsiv_field + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]);
        }

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            label_inbound[lsiv_lane] = new JLabel("Inbound " + lsiv_lane);
            label_outbound[lsiv_lane] = new JLabel("Outbound " + lsiv_lane);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == FIELD_MOVEMENTCD) {
                String[] array_lane = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT].substring(1).split("\\|");

                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_inbound[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                    comboBox_outbound[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                }
            }
            else if (lsiv_field == FIELD_VTYPEALLOW) {
                String[] array_allowed_veh_type = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE].substring(1).split("\\|");

                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    comboBox_inbound[lsiv_lane][lsiv_field] = new JComboBox(array_allowed_veh_type);
                    comboBox_outbound[lsiv_lane][lsiv_field] = new JComboBox(array_allowed_veh_type);
                }
            }
            else {
                int ArrayIndex, SizeOfArray, intArrayElementValue;

                int lsiv_min;
                int lsiv_max;
                int lsiv_inc;

                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field];

                if ((lsiv_field == FIELD_LENULNINTC) || (lsiv_field == FIELD_LENULFOUTE)) {
                    lsiv_max = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb - 1;

                    SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                    intArrayElementValue = lsiv_min;
                    String[] array_inbound = new String[SizeOfArray];

                    for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                        array_inbound[ArrayIndex] = Integer.toString(intArrayElementValue);
                        intArrayElementValue += lsiv_inc;
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_inbound[lsiv_lane][lsiv_field] = new JComboBox(array_inbound);
                    }

                    lsiv_max = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out - 1;

                    SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                    intArrayElementValue = lsiv_min;
                    String[] array_outbound = new String[SizeOfArray];

                    for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                        array_outbound[ArrayIndex] = Integer.toString(intArrayElementValue);
                        intArrayElementValue += lsiv_inc;
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_outbound[lsiv_lane][lsiv_field] = new JComboBox(array_outbound);
                    }
                }
                else {
                    SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                    intArrayElementValue = lsiv_min;
                    String[] array_lane = new String[SizeOfArray];

                    for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                        array_lane[ArrayIndex] = Integer.toString(intArrayElementValue);
                        intArrayElementValue += lsiv_inc;
                    }

                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_inbound[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                        comboBox_outbound[lsiv_lane][lsiv_field] = new JComboBox(array_lane);
                    }
                } // end of if ((lsiv_field == FIELD_LENULNINTC) || (lsiv_field ==
                  // FIELD_LENULFOUTE))
            } // end of if (lsiv_field == FIELD_MOVEMENTCD)
        } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)

        if (number_of_inbounds == 0) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_MOVEMENTCD) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_inbound[lsiv_lane][lsiv_field].setSelectedItem(setDefaultForMovementCode(lsiv_lane, number_of_inbounds));
                    }
                }
                else if (lsiv_field == FIELD_PERGENTRAF) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_inbound[lsiv_lane][lsiv_field].setSelectedItem(Integer.toString(setDefaultForPctValue(lsiv_lane, number_of_inbounds)));
                    }
                }
                else if (lsiv_field == FIELD_VTYPEALLOW) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_inbound[lsiv_lane][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]);
                    }
                }
                else {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_inbound[lsiv_lane][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
                    }
                }
            }
        }

        if (number_of_outbounds == 0) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == FIELD_MOVEMENTCD) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_outbound[lsiv_lane][lsiv_field].setSelectedItem(setDefaultForMovementCode(lsiv_lane, number_of_outbounds));
                    }
                }
                else if (lsiv_field == FIELD_PERGENTRAF) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_outbound[lsiv_lane][lsiv_field].setSelectedItem(Integer.toString(setDefaultForPctValue(lsiv_lane, number_of_outbounds)));
                    }
                }
                else if (lsiv_field == FIELD_VTYPEALLOW) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_outbound[lsiv_lane][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]);
                    }
                }
                else {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        comboBox_outbound[lsiv_lane][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
                    }
                }
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH]));
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_MOVEMENTCD].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT]);
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_MOVEMENTCD].setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code);
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_LENULNINTC].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN]));
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_LENULNINTC].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_LENULFOUTE].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN]));
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_LENULFOUTE].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_OFFSETSLEL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF]));
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_OFFSETSLEL].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_PERGENTRAF].setSelectedItem(Integer.toString(setDefaultForPctValue(lsiv_lane, number_of_inbounds)));
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_PERGENTRAF].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_inbound[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE]);
            }
            else {
                comboBox_inbound[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type);
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= number_of_outbounds; lsiv_lane++) {
            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_outbound[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH]));
            }
            else {
                comboBox_outbound[lsiv_lane][FIELD_LANE_WIDTH].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_outbound[lsiv_lane][FIELD_MOVEMENTCD].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT]);
            }
            else {
                comboBox_outbound[lsiv_lane][FIELD_MOVEMENTCD].setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code);
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_outbound[lsiv_lane][FIELD_LENULNINTC].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN]));
            }
            else {
                comboBox_outbound[lsiv_lane][FIELD_LENULNINTC].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_outbound[lsiv_lane][FIELD_LENULFOUTE].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN]));
            }
            else {
                comboBox_outbound[lsiv_lane][FIELD_LENULFOUTE].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_outbound[lsiv_lane][FIELD_OFFSETSLEL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF]));
            }
            else {
                comboBox_outbound[lsiv_lane][FIELD_OFFSETSLEL].setSelectedItem(Integer
                        .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_outbound[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE]);
            }
            else {
                comboBox_outbound[lsiv_lane][FIELD_VTYPEALLOW].setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type);
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                comboBox_inbound[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + " for inbound lane " + lsiv_lane + " for leg " + leg_number);
                comboBox_inbound[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + " for inbound lane " + lsiv_lane + " for leg " + leg_number);
                comboBox_outbound[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + " for outbound lane " + lsiv_lane + " for leg " + leg_number);
                comboBox_outbound[lsiv_lane][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + " for outbound lane " + lsiv_lane + " for leg " + leg_number);
            }
        }

        label_total = new JLabel("Total:");

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getSum()));

        JPanel panel_total = new JPanel();
        panel_total.add(label_total);
        panel_total.add(cbo_total);

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

        cbo_total.getAccessibleContext().setAccessibleName("Total Percent of Inbound Traffic to enter in this lane");
        cbo_total.getAccessibleContext().setAccessibleDescription("Total Percent of Inbound Traffic to enter in this lane");

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        comboBoxActionListener = new ComboBoxActionListener();
        comboBoxKeyListener = new ComboBoxKeyListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            comboBox_inbound[lsiv_lane][FIELD_PERGENTRAF].addActionListener(comboBoxActionListener);
        }

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            comboBox_inbound[lsiv_lane][FIELD_PERGENTRAF].addKeyListener(comboBoxKeyListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                comboBox_inbound[lsiv_lane][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_outbound[lsiv_lane][lsiv_field].addKeyListener(openComboMenuListener);

                comboBox_inbound[lsiv_lane][lsiv_field].addKeyListener(helpListener);
                comboBox_outbound[lsiv_lane][lsiv_field].addKeyListener(helpListener);
            }
        }

        cbo_total.addKeyListener(helpListener);
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

        if (number_of_inbounds >= number_of_outbounds) {
            iCol = number_of_inbounds + 3;
        }
        else {
            iCol = number_of_outbounds + 3;
        }

        gbConstraints.fill = GridBagConstraints.BOTH;

        gbConstraints.insets = new Insets(2, 2, 2, 2);

        addComponent(panel_title, iRow++, 0, iCol, 1);

        for (int lsiv_i = 0; lsiv_i < label_note.length; lsiv_i++) {
            addComponent(label_note[lsiv_i], iRow++, 0, iCol, 1);
        }

        if (number_of_inbounds != 0) {
            addComponent(label_1, iRow, 0, iCol, 1);

            for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
                addComponent(label_inbound[lsiv_lane], iRow, lsiv_lane, 1, 1);
            }

            iRow++;

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(label_field_inbound[lsiv_field], iRow, 0, 1, 1);

                for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
                    addComponent(comboBox_inbound[lsiv_lane][lsiv_field], iRow, lsiv_lane, 1, 1);
                }

                if (lsiv_field == FIELD_PERGENTRAF) {
                    addComponent(label_total, iRow, number_of_inbounds + 1, 1, 1);
                    addComponent(cbo_total, iRow, number_of_inbounds + 2, 1, 1);
                }

                iRow++;
            }
        }

        if (number_of_outbounds != 0) {
            addComponent(label_2, iRow, 0, iCol, 1);

            for (int lsiv_lane = 1; lsiv_lane <= number_of_outbounds; lsiv_lane++) {
                addComponent(label_outbound[lsiv_lane], iRow, lsiv_lane, 1, 1);
            }

            iRow++;

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(label_field_outbound[lsiv_field], iRow, 0, 1, 1);

                if (lsiv_field != FIELD_PERGENTRAF) {
                    for (int lsiv_lane = 1; lsiv_lane <= number_of_outbounds; lsiv_lane++) {
                        addComponent(comboBox_outbound[lsiv_lane][lsiv_field], iRow, lsiv_lane, 1, 1);
                    }
                }
                iRow++;
            }
        }

        addComponent(ok_Panel, iRow, 0, iCol, 1);

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method LegLaneDialog

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

    int getSum() {
        int sum = 0;

        for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
            sum = sum + Integer.valueOf(comboBox_inbound[lsiv_lane][FIELD_PERGENTRAF].getSelectedItem().toString()).intValue();
        }

        return sum;

    } // end of method getSum

    int setDefaultForPctValue(int lane, int number_of_bounds) {
        int retValue = 0;

        if (number_of_bounds == 3) {
            if (lane == 3) {
                retValue = 34;
            }
            else {
                retValue = 33;
            }
        }
        else if (number_of_bounds == 6) {
            if (lane == 1 || lane == 2) {
                retValue = 16;
            }
            else {
                retValue = 17;
            }
        }
        else {
            if (number_of_bounds != 0) {
                retValue = 100 / number_of_bounds;
            }
        }

        return retValue;

    } // end of setDefaultForPctValue

    String setDefaultForMovementCode(int lane, int number_of_bounds) {
        String retValue = "XXXX";

        if (number_of_bounds == 1) {
            if (lane == 1) {
                retValue = "LSR";

                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "SR";
                            break;
                    }
                }
            }
        }

        if (number_of_bounds == 2) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }

            if (lane == 2) {
                retValue = "SR";
            }
        }

        if (number_of_bounds == 3) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 3) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        if (number_of_bounds == 4) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 4) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        if (number_of_bounds == 5) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 5) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        if (number_of_bounds == 6) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 6) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        return retValue;
    } // end of setDefaultForMovementCode

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        if (event.getSource() == comboBox_inbound[lsiv_lane][lsiv_field]) {
                            if (lsiv_field == FIELD_MOVEMENTCD) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT], ("Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] + (" for Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT], comboBox_inbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem().toString(),
                                        setDefaultForMovementCode(lsiv_lane, number_of_inbounds), lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT], " ", " ", " ");
                            }
                            else if ((lsiv_field == FIELD_LENULNINTC) || (lsiv_field == FIELD_LENULFOUTE)) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], ("Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + (" for Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], comboBox_inbound[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb - 1),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
                            }
                            else if (lsiv_field == FIELD_PERGENTRAF) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], ("Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + (" for Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], comboBox_inbound[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(setDefaultForPctValue(lsiv_lane, number_of_inbounds)), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH
                                                - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
                            }
                            else if (lsiv_field == FIELD_VTYPEALLOW) {
                                String possibleValue = "|";
                                int n = comboBox_inbound[lsiv_lane][FIELD_VTYPEALLOW].getItemCount();
                                for (int i = 0; i < n; i++) {
                                    possibleValue = possibleValue + comboBox_inbound[lsiv_lane][FIELD_VTYPEALLOW].getItemAt(i).toString() + "|";
                                }
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE], ("Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] + (" for Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE], comboBox_inbound[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE], possibleValue, " ", " ", " ");
                            }
                            else {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], ("Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + (" for Leg " + leg_number + " Inbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], comboBox_inbound[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
                            }
                        }

                        if (event.getSource() == comboBox_outbound[lsiv_lane][lsiv_field]) {
                            if (lsiv_field == FIELD_MOVEMENTCD) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT], ("Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] + (" for Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT], comboBox_outbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem().toString(),
                                        setDefaultForMovementCode(lsiv_lane, number_of_outbounds), lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT], " ", " ", " ");
                            }
                            else if ((lsiv_field == FIELD_LENULNINTC) || (lsiv_field == FIELD_LENULFOUTE)) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], ("Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + (" for Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], comboBox_outbound[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out - 1),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
                            }
                            else if (lsiv_field == FIELD_VTYPEALLOW) {
                                String possibleValue = "|";
                                int n = comboBox_outbound[lsiv_lane][FIELD_VTYPEALLOW].getItemCount();
                                for (int i = 0; i < n; i++) {
                                    possibleValue = possibleValue + comboBox_outbound[lsiv_lane][FIELD_VTYPEALLOW].getItemAt(i).toString() + "|";
                                }
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE], ("Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] + (" for Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE], comboBox_outbound[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE], possibleValue, " ", " ", " ");
                            }
                            else {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], ("Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field] + (" for Leg " + leg_number + " Outbound Lane " + lsiv_lane),
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field], comboBox_outbound[lsiv_lane][lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1 + lsiv_field]));
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
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, "Total Percent of Inbound Traffic to enter in this lane", "Total Percent of Inbound Traffic to enter in this lane",
                            "The user cannot specify this item.  This item is the Total Percent of Inbound Traffic to enter in this lane and is determined by the program.", cbo_total
                                    .getSelectedItem().toString(),
                            "0", " ", "0", "100", "1");
                }
            }
        }
    } // end of HelpListener

    class ComboBoxActionListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            cbo_total.removeAllItems();
            cbo_total.addItem(Integer.toString(getSum()));
        }
    } // end of class ComboBoxActionListener

    class ComboBoxKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                cbo_total.removeAllItems();
                cbo_total.addItem(Integer.toString(getSum()));
            }
        } // end of method keyPressed
    } // end of class ComboBoxKeyListener

    void saveInboundData() {
        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            for (int lsiv_field = 1; lsiv_field <= PARAMS.TEXAS_MODEL_NAL; lsiv_field++) {
                gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = Integer.valueOf(
                    comboBox_inbound[lsiv_lane][FIELD_LANE_WIDTH].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = comboBox_inbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem()
                    .toString();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = Integer.valueOf(
                    comboBox_inbound[lsiv_lane][FIELD_LENULNINTC].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = Integer.valueOf(
                    comboBox_inbound[lsiv_lane][FIELD_LENULFOUTE].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = Integer.valueOf(
                    comboBox_inbound[lsiv_lane][FIELD_OFFSETSLEL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(
                    comboBox_inbound[lsiv_lane][FIELD_PERGENTRAF].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = comboBox_inbound[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem()
                    .toString();

            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    }

    void saveOutboundData() {
        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= number_of_outbounds; lsiv_lane++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = Integer.valueOf(
                    comboBox_outbound[lsiv_lane][FIELD_LANE_WIDTH].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = comboBox_outbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem()
                    .toString();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = Integer.valueOf(
                    comboBox_outbound[lsiv_lane][FIELD_LENULNINTC].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = Integer.valueOf(
                    comboBox_outbound[lsiv_lane][FIELD_LENULFOUTE].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = Integer.valueOf(
                    comboBox_outbound[lsiv_lane][FIELD_OFFSETSLEL].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = comboBox_outbound[lsiv_lane][FIELD_VTYPEALLOW].getSelectedItem()
                    .toString();

            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    }

    boolean isErrorMovementCode() {
        for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
            if (Integer.valueOf(comboBox_inbound[lsiv_lane][FIELD_LENULNINTC].getSelectedItem().toString()).intValue() == 0
                    && Integer.valueOf(comboBox_inbound[lsiv_lane][FIELD_LENULFOUTE].getSelectedItem().toString()).intValue() > 0) {
                if (!comboBox_inbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem().toString().equals("XXXX")) {
                    JOptionPane.showMessageDialog(null, "            Movement Code for Inbound Lane " + lsiv_lane
                            + " should be \"XXXX\"\n because geometry indicates the lane is blocked near intersection center!", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else {
                if (comboBox_inbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem().toString().equals("XXXX")) {
                    JOptionPane.showMessageDialog(null, "            Movement Code for Inbound Lane " + lsiv_lane
                            + " should not be \"XXXX\"\n because geometry indicates the lane is not blocked near intersection center!", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        } // end of for

        for (int lsiv_lane = 1; lsiv_lane <= number_of_outbounds; lsiv_lane++) {
            if (Integer.valueOf(comboBox_outbound[lsiv_lane][FIELD_LENULNINTC].getSelectedItem().toString()).intValue() == 0
                    && Integer.valueOf(comboBox_outbound[lsiv_lane][FIELD_LENULFOUTE].getSelectedItem().toString()).intValue() > 0) {
                if (!comboBox_outbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem().toString().equals("XXXX")) {
                    JOptionPane.showMessageDialog(null, "            Movement Code for Outbound Lane " + lsiv_lane
                            + " should be \"XXXX\"\n because geometry indicates the lane is blocked near intersection center!", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else {
                if (comboBox_outbound[lsiv_lane][FIELD_MOVEMENTCD].getSelectedItem().toString().equals("XXXX")) {
                    JOptionPane.showMessageDialog(null, "            Movement Code for Outbound Lane " + lsiv_lane
                            + " should not be \"X\"\n because geometry indicates the lane is not blocked near intersection center!", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        } // end of for

        if (number_of_inbounds > 1) {
            int last, lane;

            for (last = 1; last <= number_of_inbounds; last++) {
                if (comboBox_inbound[last][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("X") != -1) {
                    continue;
                }
                else {
                    break;
                }
            }

            for (lane = last + 1; lane <= number_of_inbounds; lane++) {
                if (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("X") != -1) {
                    continue;
                }

                if (comboBox_inbound[lane][FIELD_VTYPEALLOW].getSelectedItem().toString().indexOf("V") == -1) {
                    continue;
                }

                if (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("U") != -1) {
                    if (comboBox_inbound[last][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("L") != -1) {
                        JOptionPane.showMessageDialog(null, "Movement Code for Inbound Lane " + lane + " containing \"U\" is incompatible with the Movement Code for Inbound Lane " + last
                                + " containing \"L\"!", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }
                }

                if ((comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("U") != -1)
                        || (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("L") != -1)) {
                    if (comboBox_inbound[last][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("S") != -1) {
                        if (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("U") != -1) {
                            JOptionPane.showMessageDialog(null, "Movement Code for Inbound Lane " + lane + " containing \"U\" is incompatible with the Movement Code for Inbound Lane " + last
                                    + " containing \"S\"!", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            JOptionPane.showMessageDialog(null, "Movement Code for Inbound Lane " + lane + " containing \"L\" is incompatible with the Movement Code for Inbound Lane " + last
                                    + " containing \"S\"!", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }

                        return true;
                    }
                }

                if ((comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("U") != -1)
                        || (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("L") != -1)
                        || (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("S") != -1)) {
                    if (comboBox_inbound[last][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("R") != -1) {
                        if (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("U") != -1) {
                            JOptionPane.showMessageDialog(null, "Movement Code for Inbound Lane " + lane + " containing \"U\" is incompatible with the Movement Code for Inbound Lane " + last
                                    + " containing \"R\"!", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else if (comboBox_inbound[lane][FIELD_MOVEMENTCD].getSelectedItem().toString().indexOf("L") != -1) {
                            JOptionPane.showMessageDialog(null, "Movement Code for Inbound Lane " + lane + " containing \"L\" is incompatible with the Movement Code for Inbound Lane " + last
                                    + " containing \"R\"!", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            JOptionPane.showMessageDialog(null, "Movement Code for Inbound Lane " + lane + " containing \"S\" is incompatible with the Movement Code for Inbound Lane " + last
                                    + " containing \"R\"!", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        return true;
                    }
                }
                last = lane;
            } // end of for ( lane = last + 1; lane<= number_of_inbounds; lane++)
        } // end of if (number_of_inbounds > 1)

        return false;

    } // end of checkMovementCode

    boolean isErrorLaneLength() {
        int x, y, z;
        for (int lsiv_lane = 1; lsiv_lane <= number_of_inbounds; lsiv_lane++) {
            x = Integer.valueOf(comboBox_inbound[lsiv_lane][FIELD_LENULNINTC].getSelectedItem().toString()).intValue();
            y = Integer.valueOf(comboBox_inbound[lsiv_lane][FIELD_LENULFOUTE].getSelectedItem().toString()).intValue();
            z = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
            if ((x + y) > (z - 1)) {
                JOptionPane.showMessageDialog(null, "For Inbound Lane " + lsiv_lane + ", field " + FIELD_LENULNINTC + " plus field " + FIELD_LENULFOUTE + " = " + (x + y) + " should be less than Leg "
                        + leg_number + " Geometry Data field 4 = " + z + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        for (int lsiv_lane = 1; lsiv_lane <= number_of_outbounds; lsiv_lane++) {
            x = Integer.valueOf(comboBox_outbound[lsiv_lane][FIELD_LENULNINTC].getSelectedItem().toString()).intValue();
            y = Integer.valueOf(comboBox_outbound[lsiv_lane][FIELD_LENULFOUTE].getSelectedItem().toString()).intValue();
            z = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
            if ((x + y) > (z - 1)) {
                JOptionPane.showMessageDialog(null, "For Outbound Lane " + lsiv_lane + ", field " + FIELD_LENULNINTC + " plus field " + FIELD_LENULFOUTE + " = " + (x + y)
                        + " should be less than Leg " + leg_number + " Geometry Data field 5 = " + z + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        return false;
    }

    boolean isErrorPercent() {
        if (getSum() != 100) {
            JOptionPane.showMessageDialog(null, "Sum of percentages is " + getSum() + "%. Revise this data so that the sum is 100%.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        return false;
    }

    boolean isError() {
        if (isErrorLaneLength()) {
            return true;
        }
        else {
            if (number_of_inbounds != 0) {
                if (isErrorPercent()) {
                    return true;
                }
            }
        }

        return false;
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveInboundData();
                    saveOutboundData();

                    gdvsim.gclv_inter.mboa_Leg_Lane_Data_OK[leg_number] = true;

                    gdvsim.myIntersection.setVisible(true);
                    gdvsim.myIntersection.draw();

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
                        saveInboundData();
                        saveOutboundData();

                        gdvsim.gclv_inter.mboa_Leg_Lane_Data_OK[leg_number] = true;

                        gdvsim.myIntersection.setVisible(true);
                        gdvsim.myIntersection.draw();

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

} // end of class LegLaneDialog
