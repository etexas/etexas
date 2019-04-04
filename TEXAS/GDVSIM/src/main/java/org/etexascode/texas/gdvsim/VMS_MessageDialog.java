package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           VMS_MessageDialog.java                           */
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

class VMS_MessageDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int MAXNUMOFVMSMESSAGE;

    int NUMOFFIELD = 15;

    int NOTES = 27;

    static final int VMS_MESSAGE_TYPE = 1;

    static final int VMS_MESSAGE_MESSAGE = 2;

    static final int VMS_MESSAGE_PARAMETER = 3;

    static final int VMS_MESSAGE_START_TIME = 4;

    static final int VMS_MESSAGE_ACTIVE_TIME = 5;

    static final int VMS_MESSAGE_INB_OUT_PATH = 6;

    static final int VMS_MESSAGE_LEG_OR_PATH = 7;

    static final int VMS_MESSAGE_LANE_BEG = 8;

    static final int VMS_MESSAGE_LANE_END = 9;

    static final int VMS_MESSAGE_POS_BEG = 10;

    static final int VMS_MESSAGE_POS_END = 11;

    static final int VMS_MESSAGE_VEHICLE_NUM = 12;

    static final int VMS_MESSAGE_DIST_NAME = 13;

    static final int VMS_MESSAGE_DIST_MEAN = 14;

    static final int VMS_MESSAGE_DIST_PARAM = 15;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    AddActionListener addActionListener;

    DelActionListener delActionListener;

    UpActionListener upActionListener;

    DownActionListener downActionListener;

    AddKeyListener addKeyListener;

    DelKeyListener delKeyListener;

    UpKeyListener upKeyListener;

    DownKeyListener downKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    TextFieldFocusListener textFieldFocusListener;

    DistActionListener distActionListener;

    DistKeyListener distKeyListener;

    LocationActionListener locationActionListener;

    LocationKeyListener locationKeyListener;

    MessageActionListener messageActionListener;

    MessageKeyListener messageKeyListener;

    LegPathActionListener legPathActionListener;

    LegPathKeyListener legPathKeyListener;

    JLabel[] label_notes = new JLabel[NOTES + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JComboBox[][] comboBox_vms = new JComboBox[PARAMS.TEXAS_MODEL_NVMSMM + 1][NUMOFFIELD + 1];

    JTextField[][] text_vms = new JTextField[PARAMS.TEXAS_MODEL_NVMSMM + 1][NUMOFFIELD + 1];

    JLabel[] label_vms = new JLabel[PARAMS.TEXAS_MODEL_NVMSMM + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NVMSMM + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NVMSMM + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NVMSMM + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NVMSMM + 1];

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_total;

    JComboBox cbo_total;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    int numOfvms;

    String titleString;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    int number_of_legs;

    int number_of_legs_with_inbound_lanes;

    int number_of_legs_with_outbound_lanes;

    int[] number_of_inbound_lanes = new int[PARAMS.TEXAS_MODEL_NLGP1];

    int[] number_of_outbound_lanes = new int[PARAMS.TEXAS_MODEL_NLGP1];

    String[] leg_list_with_inbound_lanes = new String[PARAMS.TEXAS_MODEL_NLG];

    String[] leg_list_with_outbound_lanes = new String[PARAMS.TEXAS_MODEL_NLG];

    int number_of_inbound_to_right;

    int number_of_inbound_to_left;

    public VMS_MessageDialog() {
        if (gdvsim.flag_VMSmesg_ok) {
            numOfvms = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages;
        }
        else {
            numOfvms = 0;
        }

        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        int arrIndex = 0;

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            number_of_inbound_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        }

        number_of_legs_with_inbound_lanes = 0;

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0)
                continue;

            leg_list_with_inbound_lanes[arrIndex++] = Integer.toString(lsiv_leg);
            number_of_legs_with_inbound_lanes++;
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            number_of_outbound_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
        }

        number_of_legs_with_outbound_lanes = 0;

        arrIndex = 0;
        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)
                continue;

            leg_list_with_outbound_lanes[arrIndex++] = Integer.toString(lsiv_leg);
            number_of_legs_with_outbound_lanes++;
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            number_of_inbound_to_right = gdvsim.gclv_inter.mcla_leg[0].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            number_of_inbound_to_left = gdvsim.gclv_inter.mcla_leg[7].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        }

        MAXNUMOFVMSMESSAGE = Math.min(numOfvms + 5, PARAMS.TEXAS_MODEL_NVMSMM);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

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
        font1 = new Font("TimesRoman", Font.BOLD, 12);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        label_notes[1] = new JLabel("NOTE:");
        label_notes[2] = new JLabel(" ");
        label_notes[3] = new JLabel("VMS Message Types:");
        label_notes[4] = new JLabel("   1 = Driver DMS (Dynamic Message Sign); an external visual message to the driver for all vehicles that can see the message");
        label_notes[5] = new JLabel("   2 = Driver IVDMS (In-Vehicle Driver Messaging System); an internal visual or auditory message to the driver");
        label_notes[6] = new JLabel("   3 = Vehicle IVDMS (In-Vehicle Driver Messaging System); an internal computer message to the vehicle");
        label_notes[7] = new JLabel(" ");
        label_notes[8] = new JLabel("VMS Message Messages:");
        label_notes[9] = new JLabel("   1 = accelerate or decelerate to speed XX using normal acceleration or deceleration");
        label_notes[10] = new JLabel("   2 = accelerate or decelerate to speed XX using maximum vehicle acceleration or deceleration");
        label_notes[11] = new JLabel("   3 = stop at the intersection");
        label_notes[12] = new JLabel("   4 = stop at location XX");
        label_notes[13] = new JLabel("   5 = stop immediately using maximum vehicle deceleration");
        label_notes[14] = new JLabel("   6 = stop immediately using a specified collision deceleration rate XX");
        label_notes[15] = new JLabel("   7 = change lanes to the left");
        label_notes[16] = new JLabel("   8 = change lanes to the right");
        label_notes[17] = new JLabel("   9 = forced go");
        label_notes[18] = new JLabel("  10 = run the red signal");
        label_notes[19] = new JLabel("  11 = distracted driver");
        label_notes[20] = new JLabel(" ");
        label_notes[21] = new JLabel("VMS Message Parameters:");
        label_notes[22] = new JLabel("  for VMS Messages 1 and 2, the parameter is the desired speed in miles per hour");
        label_notes[23] = new JLabel("  for VMS Message 4, the parameter is the location on the lane or intersection path to stop");
        label_notes[24] = new JLabel("  for VMS Message 6, the parameter is the collision deceleration rate in feet per second squared");
        label_notes[25] = new JLabel("  for VMS Messages 3, 5, 7, 8, 9, 10, and 11 the parameter is not specified");
        label_notes[26] = new JLabel(" ");
        label_notes[27] = new JLabel("END NOTE");

        label_vms[0] = new JLabel("VMS Message ");
        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            label_vms[lsiv_vms] = new JLabel(Integer.toString(lsiv_vms));
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            add[lsiv_vms] = new JButton("Add");
            del[lsiv_vms] = new JButton("Delete");
            up[lsiv_vms] = new JButton("Up");
            down[lsiv_vms] = new JButton("Down");
        }

        label_total = new JLabel("Total VMS Messages");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);

            label_field[lsiv_field].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]);
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                double lsiv_min_double;
                double lsiv_max_double;
                double lsiv_inc_double;

                int lsiv_min_int;
                int lsiv_max_int;
                int lsiv_inc_int;

                int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

                int count, index, int_number;
                double double_number, doubleArrayElementValue;

                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                    case VMS_MESSAGE_MESSAGE:
                    case VMS_MESSAGE_LEG_OR_PATH:
                    case VMS_MESSAGE_LANE_BEG:
                    case VMS_MESSAGE_LANE_END:

                        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];
                        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];
                        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];

                        count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
                        int_number = lsiv_min_int;
                        String[] array_vms_int = new String[count];
                        for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                            array_vms_int[lsiv_index] = Integer.toString(int_number);
                            int_number += lsiv_inc_int;
                        }

                        comboBox_vms[lsiv_vms][lsiv_field] = new JComboBox(array_vms_int);
                        break;

                    case VMS_MESSAGE_DIST_PARAM:

                        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];
                        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];
                        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];
                        lsiv_max_double = 1;

                        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
                        double_number = lsiv_min_double;
                        String[] array_vms_double = new String[count];
                        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                            array_vms_double[lsiv_i] = twoDigits.format(double_number);
                            double_number += lsiv_inc_double;
                        }

                        comboBox_vms[lsiv_vms][lsiv_field] = new JComboBox(array_vms_double);
                        break;

                    case VMS_MESSAGE_DIST_NAME:

                        String[] array_vms_string = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field].substring(1).split("\\|");
                        comboBox_vms[lsiv_vms][lsiv_field] = new JComboBox(array_vms_string);

                        break;

                    case VMS_MESSAGE_INB_OUT_PATH:

                        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                            String[] array_inb_out_path = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field].substring(1).split("\\|");
                            comboBox_vms[lsiv_vms][lsiv_field] = new JComboBox(array_inb_out_path);
                        }
                        else {
                            String[] array_inb_out_path = new String[3];
                            array_inb_out_path[0] = "I";
                            array_inb_out_path[1] = "O";
                            array_inb_out_path[2] = "P";

                            comboBox_vms[lsiv_vms][lsiv_field] = new JComboBox(array_inb_out_path);
                        }

                        break;

                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_DIST_MEAN:

                        text_vms[lsiv_vms][lsiv_field] = new JTextField();
                        text_vms[lsiv_vms][lsiv_field].setBackground(aFrame.getBackground());
                        text_vms[lsiv_vms][lsiv_field].setFont(font1);
                        break;
                }
            }
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                        label_field[lsiv_field].setSize(new Dimension(45, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(45, 26));
                        break;
                    case VMS_MESSAGE_MESSAGE:
                        label_field[lsiv_field].setSize(new Dimension(45, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(45, 26));
                        break;
                    case VMS_MESSAGE_LEG_OR_PATH:
                        label_field[lsiv_field].setSize(new Dimension(70, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(70, 26));
                        break;
                    case VMS_MESSAGE_LANE_BEG:
                        label_field[lsiv_field].setSize(new Dimension(55, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(55, 26));
                        break;
                    case VMS_MESSAGE_LANE_END:
                        label_field[lsiv_field].setSize(new Dimension(50, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(50, 26));
                        break;
                    case VMS_MESSAGE_DIST_PARAM:
                        label_field[lsiv_field].setSize(new Dimension(65, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(65, 26));
                        break;
                    case VMS_MESSAGE_INB_OUT_PATH:
                        label_field[lsiv_field].setSize(new Dimension(50, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(50, 26));
                        break;
                    case VMS_MESSAGE_DIST_NAME:
                        label_field[lsiv_field].setSize(new Dimension(40, 26));
                        comboBox_vms[lsiv_vms][lsiv_field].setSize(new Dimension(40, 26));
                        break;
                    case VMS_MESSAGE_PARAMETER:
                        label_field[lsiv_field].setSize(new Dimension(60, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(60, 26));
                        break;
                    case VMS_MESSAGE_START_TIME:
                        label_field[lsiv_field].setSize(new Dimension(50, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(50, 26));
                        break;
                    case VMS_MESSAGE_ACTIVE_TIME:
                        label_field[lsiv_field].setSize(new Dimension(50, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(50, 26));
                        break;
                    case VMS_MESSAGE_POS_BEG:
                        label_field[lsiv_field].setSize(new Dimension(60, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(60, 26));
                        break;
                    case VMS_MESSAGE_POS_END:
                        label_field[lsiv_field].setSize(new Dimension(60, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(60, 26));
                        break;
                    case VMS_MESSAGE_VEHICLE_NUM:
                        label_field[lsiv_field].setSize(new Dimension(60, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(60, 26));
                        break;
                    case VMS_MESSAGE_DIST_MEAN:
                        label_field[lsiv_field].setSize(new Dimension(65, 26));
                        text_vms[lsiv_vms][lsiv_field].setSize(new Dimension(65, 26));
                        break;
                }
            }
        }

        if (gdvsim.flag_VMSmesg_ok) {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_TYPE].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE]));
                }
                else {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_TYPE].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_type));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_MESSAGE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_MESSAGE]));
                }
                else {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_message));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_PARAMETER] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setText(twoDigits.format(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_PARAMETER]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_parameter));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_START_TIME] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_START_TIME].setText(twoDigits.format(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_START_TIME]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_START_TIME].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_start_time));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_ACTIVE_TIME].setText(twoDigits.format(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_ACTIVE_TIME]
                            .setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_active_time));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].removeActionListener(locationActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].removeKeyListener(locationKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeKeyListener(legPathKeyListener);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].setSelectedItem(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH]);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addKeyListener(legPathKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].addKeyListener(locationKeyListener);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].addActionListener(locationActionListener);
                }
                else {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].removeActionListener(locationActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].removeKeyListener(locationKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeKeyListener(legPathKeyListener);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mstv_vms_message_inb_out_path);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addKeyListener(legPathKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].addKeyListener(locationKeyListener);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].addActionListener(locationActionListener);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeKeyListener(legPathKeyListener);

                    resetLegPathList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH]));

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addKeyListener(legPathKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
                }
                else {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeKeyListener(legPathKeyListener);

                    resetLegPathList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_leg_or_path));

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addKeyListener(legPathKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    resetLaneBegList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG]));
                }
                else {
                    resetLaneBegList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_lane_beg));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_END] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    resetLaneEndList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_END]));
                }
                else {
                    resetLaneEndList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_lane_end));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_BEG] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_POS_BEG].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_BEG]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_POS_BEG].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_pos_beg));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_END] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_POS_END].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_END]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_POS_END].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_pos_end));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_VEHICLE_NUM].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_VEHICLE_NUM]
                            .setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_vehicle_num));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_NAME] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_NAME]);
                }
                else {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mstv_vms_message_dist_name);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_vms[lsiv_vms][VMS_MESSAGE_DIST_MEAN].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN]));
                }
                else {
                    text_vms[lsiv_vms][VMS_MESSAGE_DIST_MEAN].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_dist_mean));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    resetDistParameterList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM]));
                }
                else {
                    resetDistParameterList(lsiv_vms);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits
                            .format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_dist_param));
                }
            }
        }
        else {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case VMS_MESSAGE_TYPE:
                        case VMS_MESSAGE_MESSAGE:
                            comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                            break;

                        case VMS_MESSAGE_LEG_OR_PATH:
                            comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);
                            comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                            resetLegPathList(lsiv_vms);

                            comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                            comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);

                            break;

                        case VMS_MESSAGE_LANE_BEG:
                            resetLaneBegList(lsiv_vms);
                            break;

                        case VMS_MESSAGE_LANE_END:
                            resetLaneEndList(lsiv_vms);
                            break;

                        case VMS_MESSAGE_INB_OUT_PATH:

                            comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(locationActionListener);
                            comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                            comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(locationKeyListener);
                            comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                            comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]);

                            comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                            comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(locationKeyListener);

                            comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                            comboBox_vms[lsiv_vms][lsiv_field].addActionListener(locationActionListener);

                            break;

                        case VMS_MESSAGE_DIST_NAME:

                            comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]);
                            break;

                        case VMS_MESSAGE_DIST_PARAM:
                            resetDistParameterList(lsiv_vms);
                            break;

                        case VMS_MESSAGE_VEHICLE_NUM:
                            text_vms[lsiv_vms][lsiv_field].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                            break;

                        case VMS_MESSAGE_PARAMETER:
                        case VMS_MESSAGE_START_TIME:
                        case VMS_MESSAGE_ACTIVE_TIME:
                        case VMS_MESSAGE_POS_BEG:
                        case VMS_MESSAGE_POS_END:
                        case VMS_MESSAGE_DIST_MEAN:
                            text_vms[lsiv_vms][lsiv_field].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                            break;
                    }
                }
            }
        }

        setStatus(numOfvms);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfVMS()));

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

        addActionListener = new AddActionListener();
        delActionListener = new DelActionListener();
        upActionListener = new UpActionListener();
        downActionListener = new DownActionListener();

        addKeyListener = new AddKeyListener();
        delKeyListener = new DelKeyListener();
        upKeyListener = new UpKeyListener();
        downKeyListener = new DownKeyListener();

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();
        textFieldFocusListener = new TextFieldFocusListener();

        distActionListener = new DistActionListener();
        distKeyListener = new DistKeyListener();

        locationActionListener = new LocationActionListener();
        locationKeyListener = new LocationKeyListener();

        messageActionListener = new MessageActionListener();
        messageKeyListener = new MessageKeyListener();

        legPathActionListener = new LegPathActionListener();
        legPathKeyListener = new LegPathKeyListener();

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            add[lsiv_vms].addActionListener(addActionListener);
            del[lsiv_vms].addActionListener(delActionListener);
            up[lsiv_vms].addActionListener(upActionListener);
            down[lsiv_vms].addActionListener(downActionListener);

            add[lsiv_vms].addKeyListener(addKeyListener);
            del[lsiv_vms].addKeyListener(delKeyListener);
            up[lsiv_vms].addKeyListener(upKeyListener);
            down[lsiv_vms].addKeyListener(downKeyListener);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                    case VMS_MESSAGE_LANE_BEG:
                    case VMS_MESSAGE_LANE_END:
                    case VMS_MESSAGE_DIST_PARAM:

                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(openComboMenuListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(helpListener);
                        break;

                    case VMS_MESSAGE_MESSAGE:

                        comboBox_vms[lsiv_vms][lsiv_field].addActionListener(messageActionListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(messageKeyListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(openComboMenuListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(helpListener);
                        break;

                    case VMS_MESSAGE_INB_OUT_PATH:

                        comboBox_vms[lsiv_vms][lsiv_field].addActionListener(locationActionListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(locationKeyListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(openComboMenuListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(helpListener);
                        break;

                    case VMS_MESSAGE_LEG_OR_PATH:

                        comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(openComboMenuListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(helpListener);
                        break;

                    case VMS_MESSAGE_DIST_NAME:

                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(openComboMenuListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(helpListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(distKeyListener);
                        comboBox_vms[lsiv_vms][lsiv_field].addActionListener(distActionListener);
                        break;

                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_DIST_MEAN:

                        text_vms[lsiv_vms][lsiv_field].addKeyListener(helpListener);
                        text_vms[lsiv_vms][lsiv_field].addFocusListener(textFieldFocusListener);
                        break;
                }
            }

            add[lsiv_vms].addKeyListener(helpListener);
            del[lsiv_vms].addKeyListener(helpListener);
            up[lsiv_vms].addKeyListener(helpListener);
            down[lsiv_vms].addKeyListener(helpListener);
        }

        cbo_total.addKeyListener(helpListener);
        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        setAccessiblility();

        int iRow = 0;
        int iCol = 20;

        gbConstraints.insets = new Insets(1, 0, 10, 0);
        addComponent(panel_title, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1);
        }

        iRow++;

        addComponent(label_vms[0], iRow++, 0, 4, 1);
        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            gbConstraints.insets = new Insets(1, 1, 1, 1);
            addComponent(label_vms[lsiv_vms], iRow, 0, 1, 1);
            addComponent(add[lsiv_vms], iRow, 1, 1, 1);
            addComponent(del[lsiv_vms], iRow, 2, 1, 1);
            addComponent(up[lsiv_vms], iRow, 3, 1, 1);
            addComponent(down[lsiv_vms], iRow, 4, 1, 1);

            gbConstraints.insets = new Insets(1, 2, 1, 2);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                    case VMS_MESSAGE_MESSAGE:
                    case VMS_MESSAGE_LEG_OR_PATH:
                    case VMS_MESSAGE_LANE_BEG:
                    case VMS_MESSAGE_LANE_END:
                    case VMS_MESSAGE_INB_OUT_PATH:
                    case VMS_MESSAGE_DIST_NAME:
                    case VMS_MESSAGE_DIST_PARAM:
                        addComponent(comboBox_vms[lsiv_vms][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
                        break;

                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_DIST_MEAN:
                        addComponent(text_vms[lsiv_vms][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
                        break;
                }
            }

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 10, 1);
        addComponent(cbo_total, iRow, 1, 1, 1);
        addComponent(label_total, iRow++, 2, 2, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        for (int lsiv_note = 1; lsiv_note <= NOTES; lsiv_note++) {
            addComponent(label_notes[lsiv_note], iRow++, 4, iCol - 4, 1);
        }

        addComponent(ok_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);

        // aFrame.setFocusTraversalPolicy(new focusPolicy());
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        // aFrame.pack();
        // aFrame.setLocation(SCREEN_SIZE.width/2 - aFrame.getWidth()/2,
        // SCREEN_SIZE.height/2 - aFrame.getHeight()/2);

    } // end of method VMS_MessageDialog()

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getNumberOfVMS() {
        int sum = 0;

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            if (comboBox_vms[lsiv_vms][VMS_MESSAGE_TYPE].isEnabled())
                sum++;
        }

        return sum;
    } // end of getNumberOfVMS()

    class TextFieldFocusListener implements FocusListener {

        public void focusLost(FocusEvent event) {
            int sumOfVMSs = getNumberOfVMS();

            for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case VMS_MESSAGE_PARAMETER:
                        case VMS_MESSAGE_START_TIME:
                        case VMS_MESSAGE_ACTIVE_TIME:
                        case VMS_MESSAGE_POS_BEG:
                        case VMS_MESSAGE_POS_END:
                        case VMS_MESSAGE_VEHICLE_NUM:
                        case VMS_MESSAGE_DIST_MEAN:

                            if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                text_vms[lsiv_vms][lsiv_field].setBackground(aFrame.getBackground());
                            }
                    }
                }
            }
        }

        public void focusGained(FocusEvent event) {
            int sumOfVMSs = getNumberOfVMS();

            for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case VMS_MESSAGE_PARAMETER:
                        case VMS_MESSAGE_START_TIME:
                        case VMS_MESSAGE_ACTIVE_TIME:
                        case VMS_MESSAGE_POS_BEG:
                        case VMS_MESSAGE_POS_END:
                        case VMS_MESSAGE_VEHICLE_NUM:
                        case VMS_MESSAGE_DIST_MEAN:

                            if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                text_vms[lsiv_vms][lsiv_field].setBackground(new Color(176, 197, 218));
                            }
                    }
                }
            }
        }

    } // end of TextFieldFocusListener()

    void setAccessiblility() {
        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                    case VMS_MESSAGE_MESSAGE:
                    case VMS_MESSAGE_LEG_OR_PATH:
                    case VMS_MESSAGE_LANE_BEG:
                    case VMS_MESSAGE_LANE_END:
                    case VMS_MESSAGE_INB_OUT_PATH:
                    case VMS_MESSAGE_DIST_NAME:
                    case VMS_MESSAGE_DIST_PARAM:
                        comboBox_vms[lsiv_vms][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms);
                        comboBox_vms[lsiv_vms][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms);
                        break;

                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_DIST_MEAN:
                        text_vms[lsiv_vms][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms);
                        text_vms[lsiv_vms][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms);
                        break;
                }
            }

            add[lsiv_vms].getAccessibleContext().setAccessibleName("Add    for VMS Message " + lsiv_vms);
            add[lsiv_vms].getAccessibleContext().setAccessibleDescription("Add    for VMS Message " + lsiv_vms);
            del[lsiv_vms].getAccessibleContext().setAccessibleName("Delete for VMS Message " + lsiv_vms);
            del[lsiv_vms].getAccessibleContext().setAccessibleDescription("Delete for VMS Message " + lsiv_vms);
            up[lsiv_vms].getAccessibleContext().setAccessibleName("Up     for VMS Message " + lsiv_vms);
            up[lsiv_vms].getAccessibleContext().setAccessibleDescription("Up     for VMS Message " + lsiv_vms);
            down[lsiv_vms].getAccessibleContext().setAccessibleName("Down   for VMS Message " + lsiv_vms);
            down[lsiv_vms].getAccessibleContext().setAccessibleDescription("Down   for VMS Message " + lsiv_vms);
        }

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

    } // end of setAccessiblility()

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
    } // end of OpenComboMenuListener()

    void setStatus(int sumOfVMSs) {
        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            if (lsiv_vms <= sumOfVMSs) {
                label_vms[lsiv_vms].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case VMS_MESSAGE_TYPE:
                        case VMS_MESSAGE_MESSAGE:
                        case VMS_MESSAGE_LEG_OR_PATH:
                        case VMS_MESSAGE_LANE_BEG:
                        case VMS_MESSAGE_LANE_END:
                        case VMS_MESSAGE_INB_OUT_PATH:
                        case VMS_MESSAGE_DIST_NAME:
                        case VMS_MESSAGE_DIST_PARAM:
                            comboBox_vms[lsiv_vms][lsiv_field].setEnabled(true);
                            break;

                        case VMS_MESSAGE_VEHICLE_NUM:
                        case VMS_MESSAGE_PARAMETER:
                        case VMS_MESSAGE_START_TIME:
                        case VMS_MESSAGE_ACTIVE_TIME:
                        case VMS_MESSAGE_POS_BEG:
                        case VMS_MESSAGE_POS_END:
                        case VMS_MESSAGE_DIST_MEAN:
                            text_vms[lsiv_vms][lsiv_field].setEditable(true);
                            text_vms[lsiv_vms][lsiv_field].setFocusable(true);
                            break;
                    }
                }
            }
            else {
                label_vms[lsiv_vms].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case VMS_MESSAGE_TYPE:
                        case VMS_MESSAGE_MESSAGE:
                        case VMS_MESSAGE_LEG_OR_PATH:
                        case VMS_MESSAGE_LANE_BEG:
                        case VMS_MESSAGE_LANE_END:
                        case VMS_MESSAGE_INB_OUT_PATH:
                        case VMS_MESSAGE_DIST_NAME:
                        case VMS_MESSAGE_DIST_PARAM:
                            comboBox_vms[lsiv_vms][lsiv_field].setEnabled(false);
                            break;

                        case VMS_MESSAGE_VEHICLE_NUM:
                        case VMS_MESSAGE_PARAMETER:
                        case VMS_MESSAGE_START_TIME:
                        case VMS_MESSAGE_ACTIVE_TIME:
                        case VMS_MESSAGE_POS_BEG:
                        case VMS_MESSAGE_POS_END:
                        case VMS_MESSAGE_DIST_MEAN:
                            text_vms[lsiv_vms][lsiv_field].setEditable(false);
                            text_vms[lsiv_vms][lsiv_field].setFocusable(false);
                            break;
                    }
                }
            }

            if (!(comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("1") || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("2")
                    || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("4") || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("6"))) {
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setText("0.00");
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(false);
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(false);
            }

            if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("L")
                    || comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("R")) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setEnabled(false);
            }

            if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].setEnabled(false);
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].setEnabled(false);
            }
        }

        if (sumOfVMSs == MAXNUMOFVMSMESSAGE) {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                add[lsiv_vms].setEnabled(false);
            }
        }
        else {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                if (lsiv_vms <= (sumOfVMSs + 1)) {
                    add[lsiv_vms].setEnabled(true);
                }
                else {
                    add[lsiv_vms].setEnabled(false);
                }
            }
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            if (lsiv_vms <= sumOfVMSs) {
                del[lsiv_vms].setEnabled(true);
            }
            else {
                del[lsiv_vms].setEnabled(false);
            }
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            if (lsiv_vms <= sumOfVMSs) {
                up[lsiv_vms].setEnabled(true);
            }
            else {
                up[lsiv_vms].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            if (lsiv_vms < sumOfVMSs) {
                down[lsiv_vms].setEnabled(true);
            }
            else {
                down[lsiv_vms].setEnabled(false);
            }
        }

        setTextFieldColor();

    } // end of setStatus

    void setTextFieldColor() {
        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_DIST_MEAN:

                        if (text_vms[lsiv_vms][lsiv_field].isEditable()) {
                            text_vms[lsiv_vms][lsiv_field].setBackground(aFrame.getBackground());
                            text_vms[lsiv_vms][lsiv_field].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
                            text_vms[lsiv_vms][lsiv_field].setForeground(new Color(0, 0, 0));
                        }
                        else {
                            text_vms[lsiv_vms][lsiv_field].setBackground(aFrame.getBackground());
                            text_vms[lsiv_vms][lsiv_field].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
                            text_vms[lsiv_vms][lsiv_field].setForeground(new Color(176, 197, 218));
                        }
                        break;
                }
            }
        }
    } // end of setTextFieldColor

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_vms = sum; lsiv_vms >= index; lsiv_vms--) {
            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].removeActionListener(locationActionListener);
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);
        }

        for (int lsiv_vms = sum; lsiv_vms >= index; lsiv_vms--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                String temp;
                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                    case VMS_MESSAGE_MESSAGE:
                    case VMS_MESSAGE_DIST_NAME:
                        comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString());
                        break;

                    case VMS_MESSAGE_LEG_OR_PATH:
                        comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString());
                        break;

                    case VMS_MESSAGE_INB_OUT_PATH:
                        temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                        resetLegPathList(lsiv_vms + 1);
                        comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_LANE_BEG:
                        temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                        resetLaneBegList(lsiv_vms + 1);
                        comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_LANE_END:
                        temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                        resetLaneEndList(lsiv_vms + 1);
                        comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_DIST_PARAM:
                        temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                        resetDistParameterList(lsiv_vms + 1);
                        comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_DIST_MEAN:
                        text_vms[lsiv_vms + 1][lsiv_field].setText(text_vms[lsiv_vms][lsiv_field].getText());
                        break;
                }
            }
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].addActionListener(locationActionListener);
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
        }

    } // end of setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_vms = index; lsiv_vms <= sum; lsiv_vms++) {
            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].removeActionListener(locationActionListener);
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);
        }

        for (int lsiv_vms = index; lsiv_vms < sum; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                String temp;
                switch (lsiv_field) {
                    case VMS_MESSAGE_TYPE:
                    case VMS_MESSAGE_MESSAGE:
                    case VMS_MESSAGE_DIST_NAME:
                        comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString());
                        break;

                    case VMS_MESSAGE_LEG_OR_PATH:
                        comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString());
                        break;

                    case VMS_MESSAGE_INB_OUT_PATH:
                        temp = comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString();
                        resetLegPathList(lsiv_vms);
                        comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_LANE_BEG:
                        temp = comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString();
                        resetLaneBegList(lsiv_vms);
                        comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_LANE_END:
                        temp = comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString();
                        resetLaneEndList(lsiv_vms);
                        comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_DIST_PARAM:
                        temp = comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString();
                        resetDistParameterList(lsiv_vms);
                        comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(temp);
                        break;

                    case VMS_MESSAGE_VEHICLE_NUM:
                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_DIST_MEAN:
                        text_vms[lsiv_vms][lsiv_field].setText(text_vms[lsiv_vms + 1][lsiv_field].getText());
                        break;
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            switch (lsiv_field) {
                case VMS_MESSAGE_TYPE:
                case VMS_MESSAGE_MESSAGE:
                    comboBox_vms[sum][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                    break;

                case VMS_MESSAGE_LEG_OR_PATH:
                    resetLegPathList(sum);
                    break;

                case VMS_MESSAGE_LANE_BEG:
                    resetLaneBegList(sum);
                    break;

                case VMS_MESSAGE_LANE_END:
                    resetLaneEndList(sum);
                    break;

                case VMS_MESSAGE_INB_OUT_PATH:
                    comboBox_vms[sum][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]);
                    break;

                case VMS_MESSAGE_DIST_NAME:
                    comboBox_vms[sum][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]);
                    break;

                case VMS_MESSAGE_DIST_PARAM:
                    resetDistParameterList(sum);
                    break;

                case VMS_MESSAGE_VEHICLE_NUM:
                    text_vms[sum][lsiv_field].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                    break;

                case VMS_MESSAGE_PARAMETER:
                case VMS_MESSAGE_START_TIME:
                case VMS_MESSAGE_ACTIVE_TIME:
                case VMS_MESSAGE_POS_BEG:
                case VMS_MESSAGE_POS_END:
                case VMS_MESSAGE_DIST_MEAN:
                    text_vms[sum][lsiv_field].setText(twoDigits.format(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                    break;
            }
        }

        for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].addActionListener(locationActionListener);
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
        }
    } // end of setValueAfterDel()

    class AddActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                if (event.getSource() == add[lsiv_vms]) {
                    int numOfvmsBeforeClick;
                    numOfvmsBeforeClick = getNumberOfVMS();
                    setStatus(numOfvmsBeforeClick + 1);
                    setValueAfterAdd(numOfvmsBeforeClick, lsiv_vms);
                    setStatus(numOfvmsBeforeClick + 1);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfVMS()));
                    break;
                }
            }
        }
    } // end of AddActionListener

    class AddKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    if (event.getSource() == add[lsiv_vms]) {
                        int numOfvmsBeforeClick;
                        numOfvmsBeforeClick = getNumberOfVMS();
                        setStatus(numOfvmsBeforeClick + 1);
                        setValueAfterAdd(numOfvmsBeforeClick, lsiv_vms);
                        setStatus(numOfvmsBeforeClick + 1);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfVMS()));
                        break;
                    }
                }
            }
        }
    } // end of AddKeyListener

    class DelActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                if (event.getSource() == del[lsiv_vms]) {
                    int numOfvmsBeforeClick;
                    numOfvmsBeforeClick = getNumberOfVMS();
                    setValueAfterDel(numOfvmsBeforeClick, lsiv_vms);
                    setStatus(numOfvmsBeforeClick - 1);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfVMS()));

                    if (!del[lsiv_vms].isEnabled()) {
                        okButton.requestFocus();
                    }

                    break;
                }
            }
        }
    } // end of DelActionListener

    class DelKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    if (event.getSource() == del[lsiv_vms]) {
                        int numOfvmsBeforeClick;
                        numOfvmsBeforeClick = getNumberOfVMS();
                        setValueAfterDel(numOfvmsBeforeClick, lsiv_vms);
                        setStatus(numOfvmsBeforeClick - 1);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfVMS()));

                        if (!del[lsiv_vms].isEnabled()) {
                            okButton.requestFocus();
                        }

                        break;
                    }
                }
            }
        }
    } // end of DelKeyListener

    class UpActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
            }

            for (int lsiv_vms = 2; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                if (event.getSource() == up[lsiv_vms]) {
                    String temp;
                    String LegPath1, LegPath2;
                    String LaneBeg1, LaneBeg2;
                    String LaneEnd1, LaneEnd2;
                    String DistParam1, DistParam2;

                    LegPath1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();
                    LegPath2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();

                    LaneBeg1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();
                    LaneBeg2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();

                    LaneEnd1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_LANE_END].getSelectedItem().toString();
                    LaneEnd2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].getSelectedItem().toString();

                    DistParam1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();
                    DistParam2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();

                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        switch (lsiv_field) {
                            case VMS_MESSAGE_TYPE:
                            case VMS_MESSAGE_MESSAGE:
                            case VMS_MESSAGE_DIST_NAME:
                                temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms - 1][lsiv_field].getSelectedItem().toString());
                                comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(temp);
                                break;

                            case VMS_MESSAGE_INB_OUT_PATH:

                                comboBox_vms[lsiv_vms - 1][lsiv_field].removeActionListener(locationActionListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].removeActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(locationActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                comboBox_vms[lsiv_vms - 1][lsiv_field].removeKeyListener(locationKeyListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(locationKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms - 1][lsiv_field].getSelectedItem().toString());
                                comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(temp);

                                comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(locationKeyListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].addKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].addKeyListener(locationKeyListener);

                                comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].addActionListener(locationActionListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].addActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].addActionListener(locationActionListener);

                                break;

                            case VMS_MESSAGE_LEG_OR_PATH:

                                comboBox_vms[lsiv_vms - 1][lsiv_field].removeActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                comboBox_vms[lsiv_vms - 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                resetLegPathList(lsiv_vms - 1);
                                resetLegPathList(lsiv_vms);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(LegPath2);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LegPath1);

                                comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].addKeyListener(legPathKeyListener);

                                comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].addActionListener(legPathActionListener);

                                break;

                            case VMS_MESSAGE_LANE_BEG:
                                resetLaneBegList(lsiv_vms - 1);
                                resetLaneBegList(lsiv_vms);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(LaneBeg2);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneBeg1);
                                break;

                            case VMS_MESSAGE_LANE_END:
                                resetLaneEndList(lsiv_vms - 1);
                                resetLaneEndList(lsiv_vms);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(LaneEnd2);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneEnd1);
                                break;

                            case VMS_MESSAGE_DIST_PARAM:
                                resetDistParameterList(lsiv_vms - 1);
                                resetDistParameterList(lsiv_vms);
                                comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(DistParam2);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(DistParam1);
                                break;

                            case VMS_MESSAGE_VEHICLE_NUM:
                            case VMS_MESSAGE_PARAMETER:
                            case VMS_MESSAGE_START_TIME:
                            case VMS_MESSAGE_ACTIVE_TIME:
                            case VMS_MESSAGE_POS_BEG:
                            case VMS_MESSAGE_POS_END:
                            case VMS_MESSAGE_DIST_MEAN:
                                temp = text_vms[lsiv_vms][lsiv_field].getText();
                                text_vms[lsiv_vms][lsiv_field].setText(text_vms[lsiv_vms - 1][lsiv_field].getText());
                                text_vms[lsiv_vms - 1][lsiv_field].setText(temp);
                                break;
                        }
                    }
                }
            }

            int sumOfVMSs = getNumberOfVMS();
            setStatus(sumOfVMSs);
        }
    } // end of UpActionListener()

    class UpKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
                    text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
                }

                for (int lsiv_vms = 2; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    if (event.getSource() == up[lsiv_vms]) {
                        String temp;
                        String LegPath1, LegPath2;
                        String LaneBeg1, LaneBeg2;
                        String LaneEnd1, LaneEnd2;
                        String DistParam1, DistParam2;

                        LegPath1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();
                        LegPath2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();

                        LaneBeg1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();
                        LaneBeg2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();

                        LaneEnd1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_LANE_END].getSelectedItem().toString();
                        LaneEnd2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].getSelectedItem().toString();

                        DistParam1 = comboBox_vms[lsiv_vms - 1][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();
                        DistParam2 = comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();

                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            switch (lsiv_field) {
                                case VMS_MESSAGE_TYPE:
                                case VMS_MESSAGE_MESSAGE:
                                case VMS_MESSAGE_DIST_NAME:
                                    temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms - 1][lsiv_field].getSelectedItem().toString());
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(temp);
                                    break;

                                case VMS_MESSAGE_INB_OUT_PATH:

                                    comboBox_vms[lsiv_vms - 1][lsiv_field].removeActionListener(locationActionListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].removeActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(locationActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                    comboBox_vms[lsiv_vms - 1][lsiv_field].removeKeyListener(locationKeyListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(locationKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                    temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms - 1][lsiv_field].getSelectedItem().toString());
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(temp);

                                    comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(locationKeyListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].addKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].addKeyListener(locationKeyListener);

                                    comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].addActionListener(locationActionListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].addActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].addActionListener(locationActionListener);

                                    break;

                                case VMS_MESSAGE_LEG_OR_PATH:

                                    comboBox_vms[lsiv_vms - 1][lsiv_field].removeActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                    comboBox_vms[lsiv_vms - 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                    resetLegPathList(lsiv_vms - 1);
                                    resetLegPathList(lsiv_vms);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(LegPath2);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LegPath1);

                                    comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].addKeyListener(legPathKeyListener);

                                    comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].addActionListener(legPathActionListener);

                                    break;

                                case VMS_MESSAGE_LANE_BEG:
                                    resetLaneBegList(lsiv_vms - 1);
                                    resetLaneBegList(lsiv_vms);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(LaneBeg2);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneBeg1);

                                case VMS_MESSAGE_LANE_END:
                                    resetLaneEndList(lsiv_vms - 1);
                                    resetLaneEndList(lsiv_vms);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(LaneEnd2);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneEnd1);

                                case VMS_MESSAGE_DIST_PARAM:
                                    resetDistParameterList(lsiv_vms - 1);
                                    resetDistParameterList(lsiv_vms);
                                    comboBox_vms[lsiv_vms - 1][lsiv_field].setSelectedItem(DistParam2);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(DistParam1);
                                    break;

                                case VMS_MESSAGE_VEHICLE_NUM:
                                case VMS_MESSAGE_PARAMETER:
                                case VMS_MESSAGE_START_TIME:
                                case VMS_MESSAGE_ACTIVE_TIME:
                                case VMS_MESSAGE_POS_BEG:
                                case VMS_MESSAGE_POS_END:
                                case VMS_MESSAGE_DIST_MEAN:
                                    temp = text_vms[lsiv_vms][lsiv_field].getText();
                                    text_vms[lsiv_vms][lsiv_field].setText(text_vms[lsiv_vms - 1][lsiv_field].getText());
                                    text_vms[lsiv_vms - 1][lsiv_field].setText(temp);
                                    break;
                            }
                        }
                    }
                }

                int sumOfVMSs = getNumberOfVMS();
                setStatus(sumOfVMSs);
            }
        }
    } // end of UpKeyListener()

    class DownActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
                text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
            }

            for (int lsiv_vms = 1; lsiv_vms < MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                if (event.getSource() == down[lsiv_vms]) {
                    String temp;
                    String LegPath1, LegPath2;
                    String LaneBeg1, LaneBeg2;
                    String LaneEnd1, LaneEnd2;
                    String DistParam1, DistParam2;

                    LegPath1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();
                    LegPath2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();

                    LaneBeg1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();
                    LaneBeg2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();

                    LaneEnd1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].getSelectedItem().toString();
                    LaneEnd2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_LANE_END].getSelectedItem().toString();

                    DistParam1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();
                    DistParam2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();

                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        switch (lsiv_field) {
                            case VMS_MESSAGE_TYPE:
                            case VMS_MESSAGE_MESSAGE:
                            case VMS_MESSAGE_DIST_NAME:
                                temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString());
                                comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);
                                break;

                            case VMS_MESSAGE_INB_OUT_PATH:

                                comboBox_vms[lsiv_vms + 1][lsiv_field].removeActionListener(locationActionListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].removeActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(locationActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                comboBox_vms[lsiv_vms + 1][lsiv_field].removeKeyListener(locationKeyListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(locationKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString());
                                comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);

                                comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(locationKeyListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].addKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].addKeyListener(locationKeyListener);

                                comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].addActionListener(locationActionListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].addActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].addActionListener(locationActionListener);

                                break;

                            case VMS_MESSAGE_LEG_OR_PATH:

                                comboBox_vms[lsiv_vms + 1][lsiv_field].removeActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                comboBox_vms[lsiv_vms + 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                resetLegPathList(lsiv_vms);
                                resetLegPathList(lsiv_vms + 1);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LegPath2);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LegPath1);

                                comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].addKeyListener(legPathKeyListener);

                                comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].addActionListener(legPathActionListener);

                                break;

                            case VMS_MESSAGE_LANE_BEG:
                                resetLaneBegList(lsiv_vms);
                                resetLaneBegList(lsiv_vms + 1);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneBeg2);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LaneBeg1);
                                break;

                            case VMS_MESSAGE_LANE_END:
                                resetLaneEndList(lsiv_vms);
                                resetLaneEndList(lsiv_vms + 1);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneEnd2);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LaneEnd1);
                                break;

                            case VMS_MESSAGE_DIST_PARAM:
                                resetDistParameterList(lsiv_vms);
                                resetDistParameterList(lsiv_vms + 1);
                                comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(DistParam2);
                                comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(DistParam1);
                                break;

                            case VMS_MESSAGE_VEHICLE_NUM:
                            case VMS_MESSAGE_PARAMETER:
                            case VMS_MESSAGE_START_TIME:
                            case VMS_MESSAGE_ACTIVE_TIME:
                            case VMS_MESSAGE_POS_BEG:
                            case VMS_MESSAGE_POS_END:
                            case VMS_MESSAGE_DIST_MEAN:
                                temp = text_vms[lsiv_vms][lsiv_field].getText();
                                text_vms[lsiv_vms][lsiv_field].setText(text_vms[lsiv_vms + 1][lsiv_field].getText());
                                text_vms[lsiv_vms + 1][lsiv_field].setText(temp);
                                break;
                        }
                    }
                }
            }

            int sumOfVMSs = getNumberOfVMS();
            setStatus(sumOfVMSs);
        }
    } // end of DownActionListener()

    class DownKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
                    text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
                }

                for (int lsiv_vms = 1; lsiv_vms < MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    if (event.getSource() == down[lsiv_vms]) {
                        String temp;
                        String LegPath1, LegPath2;
                        String LaneBeg1, LaneBeg2;
                        String LaneEnd1, LaneEnd2;
                        String DistParam1, DistParam2;

                        LegPath1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();
                        LegPath2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString();

                        LaneBeg1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();
                        LaneBeg2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString();

                        LaneEnd1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].getSelectedItem().toString();
                        LaneEnd2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_LANE_END].getSelectedItem().toString();

                        DistParam1 = comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();
                        DistParam2 = comboBox_vms[lsiv_vms + 1][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString();

                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            switch (lsiv_field) {
                                case VMS_MESSAGE_TYPE:
                                case VMS_MESSAGE_MESSAGE:
                                case VMS_MESSAGE_DIST_NAME:
                                    temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString());
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);
                                    break;

                                case VMS_MESSAGE_INB_OUT_PATH:

                                    comboBox_vms[lsiv_vms + 1][lsiv_field].removeActionListener(locationActionListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].removeActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(locationActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                    comboBox_vms[lsiv_vms + 1][lsiv_field].removeKeyListener(locationKeyListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(locationKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                    temp = comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString();
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(comboBox_vms[lsiv_vms + 1][lsiv_field].getSelectedItem().toString());
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(temp);

                                    comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(locationKeyListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].addKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].addKeyListener(locationKeyListener);

                                    comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].addActionListener(locationActionListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].addActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].addActionListener(locationActionListener);

                                    break;

                                case VMS_MESSAGE_LEG_OR_PATH:

                                    comboBox_vms[lsiv_vms + 1][lsiv_field].removeActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeActionListener(legPathActionListener);

                                    comboBox_vms[lsiv_vms + 1][lsiv_field].removeKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms][lsiv_field].removeKeyListener(legPathKeyListener);

                                    resetLegPathList(lsiv_vms);
                                    resetLegPathList(lsiv_vms + 1);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LegPath2);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LegPath1);

                                    comboBox_vms[lsiv_vms][lsiv_field].addKeyListener(legPathKeyListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].addKeyListener(legPathKeyListener);

                                    comboBox_vms[lsiv_vms][lsiv_field].addActionListener(legPathActionListener);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].addActionListener(legPathActionListener);

                                    break;

                                case VMS_MESSAGE_LANE_BEG:
                                    resetLaneBegList(lsiv_vms);
                                    resetLaneBegList(lsiv_vms + 1);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneBeg2);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LaneBeg1);
                                    break;

                                case VMS_MESSAGE_LANE_END:
                                    resetLaneEndList(lsiv_vms);
                                    resetLaneEndList(lsiv_vms + 1);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LaneEnd2);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LaneEnd1);
                                    break;

                                case VMS_MESSAGE_DIST_PARAM:
                                    resetDistParameterList(lsiv_vms);
                                    resetDistParameterList(lsiv_vms + 1);
                                    comboBox_vms[lsiv_vms][lsiv_field].setSelectedItem(LegPath2);
                                    comboBox_vms[lsiv_vms + 1][lsiv_field].setSelectedItem(LegPath1);
                                    break;

                                case VMS_MESSAGE_VEHICLE_NUM:
                                case VMS_MESSAGE_PARAMETER:
                                case VMS_MESSAGE_START_TIME:
                                case VMS_MESSAGE_ACTIVE_TIME:
                                case VMS_MESSAGE_POS_BEG:
                                case VMS_MESSAGE_POS_END:
                                case VMS_MESSAGE_DIST_MEAN:
                                    temp = text_vms[lsiv_vms][lsiv_field].getText();
                                    text_vms[lsiv_vms][lsiv_field].setText(text_vms[lsiv_vms + 1][lsiv_field].getText());
                                    text_vms[lsiv_vms + 1][lsiv_field].setText(temp);
                                    break;
                            }
                        }
                    }
                }

                int sumOfVMSs = getNumberOfVMS();
                setStatus(sumOfVMSs);
            }
        }
    } // end of DownKeyListener()

    class DistActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfVMSs = getNumberOfVMS();
            for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME]) {
                    resetDistParameterList(lsiv_vms);
                }
            }
        }
    } // end of DistActionListener()

    class DistKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfVMSs = getNumberOfVMS();

                for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                    if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME]) {
                        resetDistParameterList(lsiv_vms);
                    }
                }
            }
        }
    } // end of DistKeyListener()

    class MessageActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfVMSs = getNumberOfVMS();
            for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE]) {
                    if (comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("1") || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("2")
                            || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("4")
                            || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("6")) {
                        text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
                        text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
                    }
                    else {
                        text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setText("0.00");
                        text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(false);
                        text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(false);
                    }
                }
            }
            setTextFieldColor();
        }
    } // end of MessageActionListener()

    class MessageKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfVMSs = getNumberOfVMS();
                for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                    if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE]) {
                        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("1")
                                || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("2")
                                || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("4")
                                || comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString().equals("6")) {
                            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(true);
                            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(true);
                        }
                        else {
                            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setText("0.00");
                            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setEditable(false);
                            text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].setFocusable(false);
                        }
                    }
                }
                setTextFieldColor();
            }
        }
    } // end of MessageKeyListener()

    void resetLegPathList(int lsiv_vms) {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("I")) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeAllItems();

            for (int lsiv_vms_i = 0; lsiv_vms_i < number_of_legs_with_inbound_lanes; lsiv_vms_i++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addItem(leg_list_with_inbound_lanes[lsiv_vms_i]);
            }

            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setEnabled(true);
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("O")) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeAllItems();

            for (int lsiv_vms_i = 0; lsiv_vms_i < number_of_legs_with_inbound_lanes; lsiv_vms_i++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addItem(leg_list_with_outbound_lanes[lsiv_vms_i]);
            }

            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setEnabled(true);
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeAllItems();

            for (int lsiv_vms_i = 1; lsiv_vms_i <= PARAMS.TEXAS_MODEL_NPA; lsiv_vms_i++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addItem(Integer.toString(lsiv_vms_i));
            }

            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setEnabled(true);
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("L")
                || comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("R")) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeAllItems();

            for (int lsiv_vms_i = 0; lsiv_vms_i < number_of_legs_with_inbound_lanes; lsiv_vms_i++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addItem(leg_list_with_inbound_lanes[lsiv_vms_i]);
            }

            comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].setEnabled(false);
        }
    } // end of resetLegPathList

    void resetLaneEndList(int lsiv_vms) {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
            lsiv_min = 1;
            lsiv_inc = 1;

            if (number_of_legs_with_inbound_lanes == 0) {
                lsiv_max = number_of_inbound_lanes[Integer.valueOf(leg_list_with_inbound_lanes[0]).intValue()];
            }
            else {
                lsiv_max = number_of_outbound_lanes[Integer.valueOf(leg_list_with_outbound_lanes[0]).intValue()];
            }
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("O")) {
            lsiv_min = 1;
            lsiv_max = number_of_outbound_lanes[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue()];
            lsiv_inc = 1;
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("L")) {
            lsiv_min = 1;
            lsiv_max = gdvsim.gclv_inter.mcla_leg[7].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            lsiv_inc = 1;
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("R")) {
            lsiv_min = 1;
            lsiv_max = gdvsim.gclv_inter.mcla_leg[0].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            lsiv_inc = 1;
        }
        else {
            lsiv_min = 1;
            lsiv_max = number_of_inbound_lanes[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue()];
            lsiv_inc = 1;
        }

        comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].removeAllItems();

        for (int lsiv_i = lsiv_min; lsiv_i <= lsiv_max; lsiv_i++) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].addItem(Integer.toString(lsiv_i));
        }

        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].setEnabled(false);
        }
        else {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].setEnabled(true);
        }
    } // end of resetLaneEndList

    void resetLaneBegList(int lsiv_vms) {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
            lsiv_min = 1;
            lsiv_inc = 1;

            if (number_of_legs_with_inbound_lanes == 0) {
                lsiv_max = number_of_inbound_lanes[Integer.valueOf(leg_list_with_inbound_lanes[0]).intValue()];
            }
            else {
                lsiv_max = number_of_outbound_lanes[Integer.valueOf(leg_list_with_outbound_lanes[0]).intValue()];
            }
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("O")) {
            lsiv_min = 1;
            lsiv_max = number_of_outbound_lanes[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue()];
            lsiv_inc = 1;
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("L")) {
            lsiv_min = 1;
            lsiv_max = gdvsim.gclv_inter.mcla_leg[7].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            lsiv_inc = 1;
        }
        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("R")) {
            lsiv_min = 1;
            lsiv_max = gdvsim.gclv_inter.mcla_leg[0].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            lsiv_inc = 1;
        }
        else {
            lsiv_min = 1;
            lsiv_max = number_of_inbound_lanes[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue()];
            lsiv_inc = 1;
        }

        comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].removeAllItems();

        for (int lsiv_i = lsiv_min; lsiv_i <= lsiv_max; lsiv_i++) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].addItem(Integer.toString(lsiv_i));
        }

        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].setEnabled(false);
        }
        else {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].setEnabled(true);
        }
    } // end of resetLaneBegList

    class LocationActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfVMSs = getNumberOfVMS();

            for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH]) {
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeKeyListener(legPathKeyListener);

                    resetLegPathList(lsiv_vms);
                    resetLaneBegList(lsiv_vms);
                    resetLaneEndList(lsiv_vms);

                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addKeyListener(legPathKeyListener);
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
                }
            }
        }
    } // end of LocationActionListener()

    class LocationKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfVMSs = getNumberOfVMS();

                for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                    if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH]) {
                        comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeActionListener(legPathActionListener);
                        comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].removeKeyListener(legPathKeyListener);

                        resetLegPathList(lsiv_vms);
                        resetLaneBegList(lsiv_vms);
                        resetLaneEndList(lsiv_vms);

                        comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addKeyListener(legPathKeyListener);
                        comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].addActionListener(legPathActionListener);
                    }
                }
            }
        }
    } // end of LocationKeyListener()

    class LegPathActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfVMSs = getNumberOfVMS();
            for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH]) {
                    resetLaneBegList(lsiv_vms);
                    resetLaneEndList(lsiv_vms);
                }
            }
        }
    } // end of LegPathActionListener()

    class LegPathKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfVMSs = getNumberOfVMS();
                for (int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) {
                    if (event.getSource() == comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH]) {
                        resetLaneBegList(lsiv_vms);
                        resetLaneEndList(lsiv_vms);
                    }
                }
            }
        }
    } // end of LegPathKeyListener()

    void resetDistParameterList(int lsiv_vms) {
        for (int lsiv_vms_i = 1; lsiv_vms_i <= MAXNUMOFVMSMESSAGE; lsiv_vms_i++) {
            comboBox_vms[lsiv_vms_i][VMS_MESSAGE_DIST_NAME].removeActionListener(distActionListener);
            comboBox_vms[lsiv_vms_i][VMS_MESSAGE_DIST_NAME].removeKeyListener(distKeyListener);
        }

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

        double double_number, doubleArrayElementValue;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

        // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
        gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME].getSelectedItem().toString());
        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
            return;

        if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
        }

        for (int lsiv_vms_i = 1; lsiv_vms_i <= MAXNUMOFVMSMESSAGE; lsiv_vms_i++) {
            comboBox_vms[lsiv_vms_i][VMS_MESSAGE_DIST_NAME].addActionListener(distActionListener);
            comboBox_vms[lsiv_vms_i][VMS_MESSAGE_DIST_NAME].addKeyListener(distKeyListener);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

    } // resetDistParameterList
    /*
     * public class focusPolicy extends FocusTraversalPolicy { public Component
     * getComponentAfter(Container focusCycleRoot, Component aComponent) { int sumOfVMSs =
     * getNumberOfVMS(); for(int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) { for(int
     * lsiv_field = 1; lsiv_field <= 2; lsiv_field++) { if
     * (aComponent.equals(text_vms[lsiv_vms][lsiv_field])) { if (lsiv_field == 2) { return
     * comboBox_vms[lsiv_vms][lsiv_field + 1]; } else { return text_vms[lsiv_vms][lsiv_field + 1]; }
     * } } for(int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) { if
     * (aComponent.equals(comboBox_vms[lsiv_vms][lsiv_field])) { if (lsiv_field == NUMOFFIELD) { if
     * (sumOfVMSs == MAXNUMOFVMSMESSAGE) { if (lsiv_vms == MAXNUMOFVMSMESSAGE) { return cbo_total; }
     * else { return del[lsiv_vms + 1]; } } else { return add[lsiv_vms + 1]; } } else { return
     * comboBox_vms[lsiv_vms][lsiv_field + 1]; } } } } if (sumOfVMSs < MAXNUMOFVMSMESSAGE) { for(int
     * lsiv_vms = 1; lsiv_vms <= (sumOfVMSs + 1); lsiv_vms++) { if
     * (aComponent.equals(add[lsiv_vms])) { if (lsiv_vms == sumOfVMSs + 1) { return cbo_total; }
     * else { return del[lsiv_vms]; } } } } for(int lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++)
     * { if (aComponent.equals(del[lsiv_vms])) { if (lsiv_vms == 1) { if (sumOfVMSs == 1) { return
     * text_vms[1][1]; } else { return down[lsiv_vms]; } } else { return up[lsiv_vms]; } } } for(int
     * lsiv_vms = 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) { if (aComponent.equals(up[lsiv_vms])) { if
     * (lsiv_vms == sumOfVMSs) { return text_vms[lsiv_vms][1]; } else { return down[lsiv_vms]; } } }
     * for(int lsiv_vms = 1; lsiv_vms <= (sumOfVMSs-1); lsiv_vms++) { if
     * (aComponent.equals(down[lsiv_vms])) { return text_vms[lsiv_vms][1]; } } if
     * (aComponent.equals(cbo_total)) { return okButton; } else if (aComponent.equals(okButton)) {
     * return applyButton; } else if (aComponent.equals(applyButton)) { return cancelButton; } else
     * if (aComponent.equals(cancelButton)) { if (add[1].isEnabled()) { return add[1]; } else {
     * return del[1]; } } return okButton; } public Component getComponentBefore(Container
     * focusCycleRoot, Component aComponent) { int sumOfVMSs = getNumberOfVMS(); for(int lsiv_vms =
     * 1; lsiv_vms <= sumOfVMSs; lsiv_vms++) { for(int lsiv_field = 1; lsiv_field <= 2;
     * lsiv_field++) { if (aComponent.equals(text_vms[lsiv_vms][lsiv_field])) { if (lsiv_field == 1)
     * { if (lsiv_vms == sumOfVMSs) { if (sumOfVMSs == 1) { return del[lsiv_vms]; } else { return
     * up[lsiv_vms]; } } else { return down[lsiv_vms]; } } else { return
     * text_vms[lsiv_vms][lsiv_field - 1]; } } } for(int lsiv_field = 3; lsiv_field <= NUMOFFIELD;
     * lsiv_field++) { if (aComponent.equals(comboBox_vms[lsiv_vms][lsiv_field])) { if (lsiv_field
     * == 3) { return text_vms[lsiv_vms][lsiv_field - 1]; } else { return
     * comboBox_vms[lsiv_vms][lsiv_field - 1]; } } } } if (sumOfVMSs < MAXNUMOFVMSMESSAGE) { for(int
     * lsiv_vms = 1; lsiv_vms <= (sumOfVMSs + 1); lsiv_vms++) { if
     * (aComponent.equals(add[lsiv_vms])) { if (lsiv_vms == 1) { return cancelButton; } else {
     * return comboBox_vms[lsiv_vms - 1][NUMOFFIELD]; } } } } for(int lsiv_vms = 1; lsiv_vms <=
     * sumOfVMSs; lsiv_vms++) { if (aComponent.equals(del[lsiv_vms])) { if (sumOfVMSs ==
     * MAXNUMOFVMSMESSAGE) { if (lsiv_vms == 1) { return cancelButton; } else { return
     * comboBox_vms[lsiv_vms - 1][NUMOFFIELD]; } } else { return add[lsiv_vms]; } } } for(int
     * lsiv_vms = 2; lsiv_vms <= sumOfVMSs; lsiv_vms++) { if (aComponent.equals(up[lsiv_vms])) {
     * return del[lsiv_vms]; } } for(int lsiv_vms = 1; lsiv_vms <= (sumOfVMSs-1); lsiv_vms++) { if
     * (aComponent.equals(down[lsiv_vms])) { if (lsiv_vms == 1) { return del[lsiv_vms]; } else {
     * return up[lsiv_vms]; } } } if (aComponent.equals(cbo_total)) { if (sumOfVMSs ==
     * MAXNUMOFVMSMESSAGE) { return comboBox_vms[MAXNUMOFVMSMESSAGE][NUMOFFIELD]; } else { return
     * add[sumOfVMSs + 1]; } } if (aComponent.equals(okButton)) { return cbo_total; } else if
     * (aComponent.equals(applyButton)) { return okButton; } else if
     * (aComponent.equals(cancelButton)) { return applyButton; } else if (aComponent.equals(add[1]))
     * { return cancelButton; } return okButton; } public Component getDefaultComponent(Container
     * focusCycleRoot) { if (add[1].isEnabled()) { return add[1]; } else { return del[1]; } } public
     * Component getLastComponent(Container focusCycleRoot) { return cancelButton; } public
     * Component getFirstComponent(Container focusCycleRoot) { if (add[1].isEnabled()) { return
     * add[1]; } else { return del[1]; } } } // end of class focusPolicy
     */

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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NVMSMM), "1");
                }

                for (int lsiv_vms = 1; lsiv_vms <= MAXNUMOFVMSMESSAGE; lsiv_vms++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        double maxTime;
                        switch (lsiv_field) {
                            case VMS_MESSAGE_TYPE:
                            case VMS_MESSAGE_MESSAGE:

                                if (event.getSource() == comboBox_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ",
                                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                                }
                                break;

                            case VMS_MESSAGE_PARAMETER:
                                if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                    double ldfv_min = 0.0, ldfv_max = 0.0, ldfv_inc = 0.0;
                                    switch (Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString()).intValue()) {
                                        case 1:
                                        case 2:
                                            ldfv_min = 0.0;
                                            ldfv_max = PARAMS.TEXAS_MODEL_VELMAX;
                                            ldfv_inc = 0.01;
                                            break;

                                        case 4:
                                            ldfv_min = 0.0;
                                            ldfv_max = PARAMS.TEXAS_MODEL_POSMAX;
                                            ldfv_inc = 0.01;
                                            break;

                                        case 6:
                                            ldfv_min = -PARAMS.TEXAS_MODEL_DECCOL;
                                            ldfv_max = -0.01;
                                            ldfv_inc = 0.01;
                                            break;
                                    }

                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], text_vms[lsiv_vms][lsiv_field].getText(),
                                            twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ", twoDigits.format(ldfv_min),
                                            twoDigits.format(ldfv_max), twoDigits.format(ldfv_inc));
                                }
                                break;

                            case VMS_MESSAGE_LEG_OR_PATH:

                                if (event.getSource() == comboBox_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

                                    String dValue;
                                    String pValue = "";

                                    if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("I")) {
                                        dValue = leg_list_with_inbound_lanes[0];
                                        pValue = "|";
                                        for (int lsiv_leg = 0; lsiv_leg < number_of_legs_with_inbound_lanes; lsiv_leg++) {
                                            pValue = pValue + leg_list_with_inbound_lanes[lsiv_leg] + "|";
                                        }
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE
                                                        - 1 + lsiv_field],
                                                comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(), dValue, pValue, " ", " ", " ");
                                    }
                                    else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("O")) {
                                        dValue = leg_list_with_outbound_lanes[0];
                                        pValue = "|";
                                        for (int lsiv_leg = 0; lsiv_leg < number_of_legs_with_outbound_lanes; lsiv_leg++) {
                                            pValue = pValue + leg_list_with_outbound_lanes[lsiv_leg] + "|";
                                        }
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE
                                                        - 1 + lsiv_field],
                                                comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(), dValue, pValue, " ", " ", " ");
                                    }
                                    else {
                                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ",
                                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                                    }
                                }
                                break;

                            case VMS_MESSAGE_INB_OUT_PATH:

                                if (event.getSource() == comboBox_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

                                    String possibleValue = "|";
                                    int n = comboBox_vms[lsiv_vms][lsiv_field].getItemCount();
                                    for (int i = 0; i < n; i++) {
                                        possibleValue = possibleValue + comboBox_vms[lsiv_vms][lsiv_field].getItemAt(i).toString() + "|";
                                    }
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1
                                                    + lsiv_field],
                                            comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1
                                                    + lsiv_field],
                                            possibleValue, " ", " ", " ");
                                }
                                break;

                            case VMS_MESSAGE_LANE_BEG:
                            case VMS_MESSAGE_LANE_END:
                                if (event.getSource() == comboBox_vms[lsiv_vms][lsiv_field]) {
                                    int lsiv_min;
                                    int lsiv_max;
                                    int lsiv_inc;

                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];

                                    if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("P")) {
                                        lsiv_min = 1;
                                        if (number_of_legs_with_inbound_lanes == 0) {
                                            lsiv_max = number_of_inbound_lanes[Integer.valueOf(leg_list_with_inbound_lanes[0]).intValue()];
                                        }
                                        else {
                                            lsiv_max = number_of_outbound_lanes[Integer.valueOf(leg_list_with_outbound_lanes[0]).intValue()];
                                        }
                                        lsiv_inc = 1;
                                    }
                                    else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("O")) {
                                        lsiv_min = 1;
                                        lsiv_max = number_of_inbound_lanes[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue()];
                                        lsiv_inc = 1;
                                    }
                                    else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("L")) {
                                        lsiv_min = 1;
                                        lsiv_max = gdvsim.gclv_inter.mcla_leg[7].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                                        lsiv_inc = 1;
                                    }
                                    else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("R")) {
                                        lsiv_min = 1;
                                        lsiv_max = gdvsim.gclv_inter.mcla_leg[0].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                                        lsiv_inc = 1;
                                    }
                                    else {
                                        lsiv_min = 1;
                                        lsiv_max = number_of_inbound_lanes[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue()];
                                        lsiv_inc = 1;
                                    }

                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                            Integer.toString(lsiv_min), " ", Integer.toString(lsiv_min), Integer.toString(lsiv_max), Integer.toString(lsiv_inc));
                                }
                                break;

                            case VMS_MESSAGE_DIST_NAME:

                                if (event.getSource() == comboBox_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1
                                                    + lsiv_field],
                                            comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1
                                                    + lsiv_field],
                                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], " ", " ", " ");
                                }
                                break;

                            case VMS_MESSAGE_VEHICLE_NUM:

                                if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], text_vms[lsiv_vms][lsiv_field].getText(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ",
                                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                                }
                                break;

                            case VMS_MESSAGE_POS_BEG:
                            case VMS_MESSAGE_POS_END:
                            case VMS_MESSAGE_DIST_MEAN:

                                if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], text_vms[lsiv_vms][lsiv_field].getText(),
                                            twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ",
                                            twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                            twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]),
                                            twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                                }
                                break;

                            case VMS_MESSAGE_DIST_PARAM:

                                if (event.getSource() == comboBox_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    String name = lclv_tx_fmt.mstv_name;

                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                                    // check and set headway distribution
                                    // gdvsim.gclv_inter..mbov_is_hd_* values
                                    gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME].getSelectedItem().toString());
                                    if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                                        return;

                                    if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN],
                                                comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                " ", twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML],
                                                comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                " ", twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP],
                                                comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                " ", twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM],
                                                comboBox_vms[lsiv_vms][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                " ", twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                                    }
                                }
                                break;

                            case VMS_MESSAGE_START_TIME:

                                maxTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time * 60
                                        + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time * 60 - 0.01;

                                if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], text_vms[lsiv_vms][lsiv_field].getText(),
                                            twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ",
                                            twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), twoDigits.format(maxTime),
                                            twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                                }
                                break;

                            case VMS_MESSAGE_ACTIVE_TIME:

                                maxTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time * 60
                                        + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time * 60
                                        - Double.parseDouble(text_vms[lsiv_vms][VMS_MESSAGE_START_TIME].getText());

                                if (event.getSource() == text_vms[lsiv_vms][lsiv_field]) {
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], lclv_tx_fmt.mstv_name,
                                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field] + " for VMS Message " + lsiv_vms,
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field], text_vms[lsiv_vms][lsiv_field].getText(),
                                            twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), " ",
                                            twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]), twoDigits.format(maxTime),
                                            twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field]));
                                }
                                break;

                        }
                    }

                    if (event.getSource() == up[lsiv_vms]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous VMS Message and the current VMS Message.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_vms]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next VMS Message and the current VMS Message.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_vms]) {
                        new HelpDialog(true, "Delete Button",
                                "The Delete button moves the data up 1 VMS Message for all VMS messages below this VMS Message and decreases the Total VMS Messages by 1.", " ", " ", " ", " ", " ",
                                " ", " ");
                    }
                    else if (event.getSource() == add[lsiv_vms]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 VMS Message for all VMS messages below this VMS Message, inserts a new VMS Message at the current position, copies the values of all parameters from the previous VMS Message to the new VMS Message, and increases the Total VMS Messages by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of HelpListener

    void saveData() {
        numOfvms = getNumberOfVMS();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages = numOfvms;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_vms = 1; lsiv_vms <= numOfvms; lsiv_vms++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_type = Integer.valueOf(
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_TYPE].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_message = Integer.valueOf(
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_parameter = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_PARAMETER].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_start_time = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_START_TIME].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_active_time = Double
                    .valueOf(text_vms[lsiv_vms][VMS_MESSAGE_ACTIVE_TIME].getText().trim()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mstv_vms_message_inb_out_path = comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem()
                    .toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_leg_or_path = Integer.valueOf(
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_lane_beg = Integer.valueOf(
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_lane_end = Integer.valueOf(
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_pos_beg = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_POS_BEG].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_pos_end = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_POS_END].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].msiv_vms_message_vehicle_num = Integer.valueOf(
                    text_vms[lsiv_vms][VMS_MESSAGE_VEHICLE_NUM].getText().trim()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mstv_vms_message_dist_name = comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_NAME].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_dist_mean = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_DIST_MEAN].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mdfv_vms_message_dist_param = Double.valueOf(
                    comboBox_vms[lsiv_vms][VMS_MESSAGE_DIST_PARAM].getSelectedItem().toString()).doubleValue();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_MESSAGE] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_PARAMETER] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_START_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_INB_OUT_PATH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LEG_OR_PATH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_END] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_BEG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_END] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_VEHICLE_NUM] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_NAME] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_MEAN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_DIST_PARAM] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_vms = numOfvms + 1; lsiv_vms <= PARAMS.TEXAS_MODEL_NVMSMM; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_vms_message[lsiv_vms].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();

    } // end of saveData()

    boolean isError() {
        numOfvms = getNumberOfVMS();

        String fld_name;

        for (int lsiv_vms = 1; lsiv_vms <= numOfvms; lsiv_vms++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_VEHICLE_NUM:
                        fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];

                        if (text_vms[lsiv_vms][lsiv_field].isEditable()) {
                            if (text_vms[lsiv_vms][lsiv_field].getText().trim().length() == 0) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }

                            int lsiv_value;

                            try {
                                lsiv_value = Integer.parseInt(text_vms[lsiv_vms][lsiv_field].getText().trim());
                            }
                            catch (Exception e) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " = '" + text_vms[lsiv_vms][lsiv_field].getText().trim()
                                        + "' contains an illegal character for an integer.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        break;

                    case VMS_MESSAGE_PARAMETER:
                    case VMS_MESSAGE_START_TIME:
                    case VMS_MESSAGE_ACTIVE_TIME:
                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:
                    case VMS_MESSAGE_DIST_MEAN:

                        fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_TYPE - 1 + lsiv_field];

                        if (text_vms[lsiv_vms][lsiv_field].isEditable()) {
                            if (text_vms[lsiv_vms][lsiv_field].getText().trim().length() == 0) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }

                            Double lsiv_value;

                            try {
                                lsiv_value = Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim());
                            }
                            catch (Exception e) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " = '" + text_vms[lsiv_vms][lsiv_field].getText().trim()
                                        + "' contains an illegal character for a double.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        break;
                }
            } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)
        } // end of for(int lsiv_vms = 1; lsiv_vms <= numOfvms; lsiv_vms++)

        double ldfv_min = 0.0, ldfv_max = 0.0;
        int lsiv_min = 0, lsiv_max = 0, lsiv_vms_message_type, lsiv_vms_message_message;
        String fld_name1, fld_name2, lstv_message;
        double totalTime, startTime, activeTime;
        int laneBeg, laneEnd;
        double posBeg, posEnd;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE];
        for (int lsiv_vms = 1; lsiv_vms <= numOfvms; lsiv_vms++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case VMS_MESSAGE_MESSAGE:
                        fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_MESSAGE];
                        lsiv_vms_message_type = Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_TYPE].getSelectedItem().toString()).intValue();
                        lsiv_vms_message_message = Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString()).intValue();
                        if (lsiv_vms_message_type == PARAMS.TEXAS_MODEL_VMSTVI) {
                            lstv_message = "UNKNOWN";
                            switch (lsiv_vms_message_message) {
                                case PARAMS.TEXAS_MODEL_VMSMCL: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - change lanes to
                                                                 * the left
                                                                 */
                                    lstv_message = "change lanes to the left";
                                    break;

                                case PARAMS.TEXAS_MODEL_VMSMCR: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - change lanes to
                                                                 * the right
                                                                 */
                                    lstv_message = "change lanes to the right";
                                    break;

                                case PARAMS.TEXAS_MODEL_VMSMGO: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - forced go
                                                                 */
                                    lstv_message = "forced go";
                                    break;

                                case PARAMS.TEXAS_MODEL_VMSMRR: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - forced run the
                                                                 * red signal
                                                                 */
                                    lstv_message = "forced run the red signal";
                                    break;

                                case PARAMS.TEXAS_MODEL_VMSMDD: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - distracted
                                                                 * driver
                                                                 */
                                    lstv_message = "distracted driver";
                                    break;
                            }
                            switch (lsiv_vms_message_message) {
                                case PARAMS.TEXAS_MODEL_VMSMCL: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - change lanes to
                                                                 * the left
                                                                 */
                                case PARAMS.TEXAS_MODEL_VMSMCR: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - change lanes to
                                                                 * the right
                                                                 */
                                case PARAMS.TEXAS_MODEL_VMSMGO: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - forced go
                                                                 */
                                case PARAMS.TEXAS_MODEL_VMSMRR: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - forced run the
                                                                 * red signal
                                                                 */
                                case PARAMS.TEXAS_MODEL_VMSMDD: /*
                                                                 * TEXAS Model Vehicle Message
                                                                 * System message - distracted
                                                                 * driver
                                                                 */
                                    JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + lsiv_vms_message_message + " = '" + lstv_message
                                            + "' is invalid for IVDMS Message.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                            }
                        }
                        break;

                    case VMS_MESSAGE_PARAMETER:
                        fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_PARAMETER];
                        switch (Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_MESSAGE].getSelectedItem().toString()).intValue()) {
                            case 1:
                            case 2:
                                ldfv_min = 0.0;
                                ldfv_max = PARAMS.TEXAS_MODEL_VELMAX;

                                if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > ldfv_max) {
                                    JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                            + "  should be less than the maximum value = " + ldfv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                break;

                            case 4:
                                ldfv_min = 0.0;
                                ldfv_max = PARAMS.TEXAS_MODEL_POSMAX;

                                if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > ldfv_max) {
                                    JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                            + "  should be less than the maximum value = " + ldfv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                break;

                            case 6:
                                ldfv_min = -PARAMS.TEXAS_MODEL_DECCOL;
                                ldfv_max = -0.01;

                                if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > ldfv_max) {
                                    JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                            + "  should be less than the maximum value = " + ldfv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                break;
                        }
                        break;

                    case VMS_MESSAGE_POS_BEG:
                    case VMS_MESSAGE_POS_END:

                        fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_PARAMETER];

                        if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("I")) {
                            lsiv_max = gdvsim.gclv_inter.mcla_leg[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString())
                                    .intValue()].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;

                            if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > lsiv_max) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                        + "  should be less than the maximum value = " + lsiv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("O")) {
                            lsiv_max = gdvsim.gclv_inter.mcla_leg[Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LEG_OR_PATH].getSelectedItem().toString())
                                    .intValue()].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;

                            if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > lsiv_max) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                        + "  should be less than the maximum value = " + lsiv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("L")) {
                            lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;

                            if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > lsiv_max) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                        + "  should be less than the maximum value = " + lsiv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        else if (comboBox_vms[lsiv_vms][VMS_MESSAGE_INB_OUT_PATH].getSelectedItem().toString().equals("R")) {
                            lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;

                            if (Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText().trim()) > lsiv_max) {
                                JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name + " value = " + Double.parseDouble(text_vms[lsiv_vms][lsiv_field].getText())
                                        + "  should be less than the maximum value = " + lsiv_max, "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }

                        fld_name1 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_BEG];
                        fld_name2 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_POS_END];

                        posBeg = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_POS_BEG].getText().trim().toString()).doubleValue();
                        posEnd = Double.valueOf(text_vms[lsiv_vms][VMS_MESSAGE_POS_END].getText().trim().toString()).doubleValue();

                        if (posBeg > posEnd) {
                            JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name1 + " values = " + posBeg + " should be less than " + fld_name2 + " values = " + posEnd,
                                    "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                        break;

                    case VMS_MESSAGE_START_TIME:
                        fld_name1 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_START_TIME];
                        totalTime = ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time
                                + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time) * 60) + 0.0001;
                        startTime = Double.parseDouble(text_vms[lsiv_vms][VMS_MESSAGE_START_TIME].getText().trim());

                        if (startTime < 0.0) {
                            JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name1 + " value = " + twoDigits.format(startTime) + " should be greater than or equal to zero.",
                                    "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }

                        if (startTime > totalTime) {
                            JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name1 + " value = " + twoDigits.format(startTime)
                                    + " should be less than or equal to the maximum value = " + twoDigits.format(totalTime) + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                        break;

                    case VMS_MESSAGE_ACTIVE_TIME:
                        fld_name1 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_START_TIME];
                        fld_name2 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_ACTIVE_TIME];
                        totalTime = ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time
                                + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time) * 60) + 0.0001;
                        startTime = Double.parseDouble(text_vms[lsiv_vms][VMS_MESSAGE_START_TIME].getText().trim());
                        activeTime = Double.parseDouble(text_vms[lsiv_vms][VMS_MESSAGE_ACTIVE_TIME].getText().trim());

                        if (activeTime <= 0.0) {
                            JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name2 + " value = " + twoDigits.format(activeTime) + " should be greater than zero.",
                                    "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }

                        if ((startTime + activeTime) > totalTime) {
                            JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name1 + " + " + fld_name2 + " value = " + twoDigits.format(startTime + activeTime)
                                    + " should be less than or equal to the maximum value = " + twoDigits.format(totalTime) + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                        break;

                    case VMS_MESSAGE_LANE_BEG:
                    case VMS_MESSAGE_LANE_END:
                        fld_name1 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_BEG];
                        fld_name2 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_VMS_MESSAGE_LANE_END];

                        laneBeg = Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_BEG].getSelectedItem().toString()).intValue();
                        laneEnd = Integer.valueOf(comboBox_vms[lsiv_vms][VMS_MESSAGE_LANE_END].getSelectedItem().toString()).intValue();

                        if (laneBeg > laneEnd) {
                            JOptionPane.showMessageDialog(null, "VMS Message " + lsiv_vms + " " + fld_name1 + " values = " + laneBeg + " should be greater than or equal to " + fld_name2 + " values = "
                                    + laneEnd, "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                        break;
                }
            }
        }

        return false;

    } // end of isError

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_VMSmesg_ok = true;
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
                        gdvsim.flag_VMSmesg_ok = true;
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
        } // end of method actionPerformed
    } // end of class OkApplyKeyListener

} // end of class VMS_MessageDialog.java
