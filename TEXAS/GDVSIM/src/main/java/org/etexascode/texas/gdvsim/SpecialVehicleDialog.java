package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                        SpecialVehicleDialog.java                           */
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
import java.net.*;
import java.text.DecimalFormat;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

class SpecialVehicleDialog extends JDialog {

    int MAXNUMOFSPECIALVEHICLE;

    static final int QUEUE_IN_TIME = 1;

    static final int VEHICLE_CLASS = 2;

    static final int DRIVER_CLASS = 3;

    static final int DESIRED_SPEED = 4;

    static final int DESIRED_O_LEG = 5;

    static final int INBOUND_LEG = 6;

    static final int INBOUND_LANE = 7;

    static final int LOGOUT_SUMMRY = 8;

    static final int FREE_UTURN = 9;

    static final int FORCEDSTOPTIM = 10;

    static final int FORCEDSTOPIOP = 11;

    static final int FORCEDSTOPLIP = 12;

    static final int FORCEDSTOPPOS = 13;

    static final int FORCEDSTOPDWT = 14;

    static final int FORCED_GO_TIM = 15;

    static final int FORCED_GO_ACT = 16;

    static final int FORCED_RRSTIM = 17;

    static final int FORCED_RRSACT = 18;

    static final int EMERGENCY_VEH = 19;

    static final int NUMOFFIELD = 19;

    static final int NUMOFICON = 39;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    TextFieldFocusListener textFieldFocusListener;

    JComboBox[][] comboBox_vehicle = new JComboBox[PARAMS.TEXAS_MODEL_NSV + 1][NUMOFFIELD + 1];

    JTextField[][] text_vehicle = new JTextField[PARAMS.TEXAS_MODEL_NSV + 1][NUMOFFIELD + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JLabel[] label_class = new JLabel[PARAMS.TEXAS_MODEL_NSV + 1];

    JButton[] add_button = new JButton[PARAMS.TEXAS_MODEL_NSV + 1];

    JButton[] del_button = new JButton[PARAMS.TEXAS_MODEL_NSV + 1];

    JButton[] up_button = new JButton[PARAMS.TEXAS_MODEL_NSV + 1];

    JButton[] down_button = new JButton[PARAMS.TEXAS_MODEL_NSV + 1];

    JLabel[] label_note = new JLabel[8];

    String[] imgName = new String[NUMOFICON + 1];

    ImageIcon[] imgIcon = new ImageIcon[NUMOFICON + 1];

    JButton[] VehicleIcon = new JButton[PARAMS.TEXAS_MODEL_NSV + 1];

    JComboBox[] VehicleName = new JComboBox[PARAMS.TEXAS_MODEL_NSV + 1];

    JComboBox cbo_total;

    JLabel label_title, label_total, label_classification;

    JButton okButton, applyButton, cancelButton;

    Font font1, font2, font3;

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    TX_Fmt lclv_tx_fmt;

    int number_of_specialVehicle;

    int gsiv_min_desired_leg, gsiv_max_desired_leg;

    int gsiv_min_inbound_leg, gsiv_max_inbound_leg;

    int numberOfLegs, maxLengthOfInboundLanes;

    public SpecialVehicleDialog() {
        number_of_specialVehicle = gdvsim.gclv_inter.msiv_specialVehicles;

        numberOfLegs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        maxLengthOfInboundLanes = 0;

        for (int lsiv_leg = 1; lsiv_leg <= numberOfLegs; lsiv_leg++) {
            if (maxLengthOfInboundLanes < gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb) {
                maxLengthOfInboundLanes = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
            }
        }

        MAXNUMOFSPECIALVEHICLE = Math.min(number_of_specialVehicle + 5, PARAMS.TEXAS_MODEL_NSV);

        double ldfv_min;
        double ldfv_max;
        double ldfv_inc;

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        int SizeOfArray, ArrayIndex;
        int intArrayElementValue;
        double doubleArrayElementValue;
        String s;

        aFrame = new JFrame("Special Vehicle Data");

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
        font3 = new Font("TimesRoman", Font.BOLD, 12);

        label_title = new JLabel("Special Vehicle Data");
        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH];

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font2);
            label_field[lsiv_field].setSize(new Dimension(70, 26));
        }

        label_classification = new JLabel("Vehicle Classification");
        label_classification.setVerticalAlignment(JLabel.TOP);

        label_field[FORCEDSTOPLIP].setSize(new Dimension(80, 26));

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]);
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            label_class[lsiv_veh] = new JLabel(Integer.toString(lsiv_veh));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            add_button[lsiv_veh] = new JButton("Add");
            del_button[lsiv_veh] = new JButton("Del");
            up_button[lsiv_veh] = new JButton("Up");
            down_button[lsiv_veh] = new JButton("Down");

            VehicleIcon[lsiv_veh] = new JButton();
            VehicleName[lsiv_veh] = new JComboBox();
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS];

        lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_vehicle_class = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_vehicle_class[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS];

        lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_driver_class = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_driver_class[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_desired_speed = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_desired_speed[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG];

        lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        SizeOfArray = 0;
        for (int lsiv_leg = lsiv_min; lsiv_leg <= lsiv_max; lsiv_leg += lsiv_inc) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                SizeOfArray++;
            }
        }

        String[] array_desired_outbound_leg = new String[SizeOfArray];
        ArrayIndex = 0;
        for (int lsiv_leg = lsiv_min; lsiv_leg <= lsiv_max; lsiv_leg += lsiv_inc) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                array_desired_outbound_leg[ArrayIndex] = Integer.toString(lsiv_leg);
                if (ArrayIndex == 0)
                    gsiv_min_desired_leg = lsiv_leg;
                gsiv_max_desired_leg = lsiv_leg;
                ArrayIndex++;
            }
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG];

        lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        SizeOfArray = 0;
        for (int lsiv_leg = lsiv_min; lsiv_leg <= lsiv_max; lsiv_leg += lsiv_inc) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                SizeOfArray++;
            }
        }

        String[] array_inbound_leg = new String[SizeOfArray];
        ArrayIndex = 0;
        for (int lsiv_leg = lsiv_min; lsiv_leg <= lsiv_max; lsiv_leg += lsiv_inc) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                array_inbound_leg[ArrayIndex] = Integer.toString(lsiv_leg);
                if (ArrayIndex == 0)
                    gsiv_min_inbound_leg = lsiv_leg;
                gsiv_max_inbound_leg = lsiv_leg;
                ArrayIndex++;
            }
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_inbound_lane = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_inbound_lane[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_logout_summary = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_logout_summary[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        String stringToTokenizer, string;
        StringTokenizer tokens;
        String[] array_free_uturn;
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            stringToTokenizer = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN];
            tokens = new StringTokenizer(stringToTokenizer, "|");
            int numTokens = tokens.countTokens();
            if (stringToTokenizer.contains("N"))
                numTokens--;
            array_free_uturn = new String[numTokens];
            ArrayIndex = 0;
            while (tokens.hasMoreTokens()) {
                string = tokens.nextToken();
                if (string.equals("N"))
                    continue;
                array_free_uturn[ArrayIndex++] = string;
            }
        }
        else {
            array_free_uturn = new String[1];
            array_free_uturn[0] = "N";
        }

        String[] array_forced_stop_inb_out_path;
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            array_forced_stop_inb_out_path = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP].substring(1).split("\\|");
        }
        else {
            array_forced_stop_inb_out_path = new String[3];
            array_forced_stop_inb_out_path[0] = "I";
            array_forced_stop_inb_out_path[1] = "O";
            array_forced_stop_inb_out_path[2] = "P";
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP] = new JComboBox(array_forced_stop_inb_out_path);
        } // end for lsiv_veh

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_forced_stop_leg_or_path = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_forced_stop_leg_or_path[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        String[] array_emergency = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH].substring(1).split("\\|");

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            comboBox_vehicle[lsiv_veh][VEHICLE_CLASS] = new JComboBox(array_vehicle_class);
            comboBox_vehicle[lsiv_veh][DRIVER_CLASS] = new JComboBox(array_driver_class);
            comboBox_vehicle[lsiv_veh][DESIRED_SPEED] = new JComboBox(array_desired_speed);
            comboBox_vehicle[lsiv_veh][DESIRED_O_LEG] = new JComboBox(array_desired_outbound_leg);
            comboBox_vehicle[lsiv_veh][INBOUND_LEG] = new JComboBox(array_inbound_leg);
            comboBox_vehicle[lsiv_veh][INBOUND_LANE] = new JComboBox(array_inbound_lane);
            comboBox_vehicle[lsiv_veh][LOGOUT_SUMMRY] = new JComboBox(array_logout_summary);
            comboBox_vehicle[lsiv_veh][FREE_UTURN] = new JComboBox(array_free_uturn);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP] = new JComboBox(array_forced_stop_leg_or_path);
            comboBox_vehicle[lsiv_veh][EMERGENCY_VEH] = new JComboBox(array_emergency);

        } // end for lsiv_veh

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        text_vehicle[lsiv_veh][lsiv_field] = new JTextField();
                        text_vehicle[lsiv_veh][lsiv_field].setBackground(aFrame.getBackground());
                        text_vehicle[lsiv_veh][lsiv_field].setFont(font3);
                        break;
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
        String[] allImgs = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_13].substring(1).split("\\|");

        if (allImgs.length != NUMOFICON) {
            JOptionPane.showMessageDialog(null, "allImgs.length = " + allImgs.length + " is not NUMOFICON = " + NUMOFICON + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
            return;
        }
        URL urlUnknown = getClass().getResource("vehicle_class_unknown.jpg");
        for (int lsiv_icon = 1; lsiv_icon <= NUMOFICON; lsiv_icon++) {
            imgName[lsiv_icon] = allImgs[lsiv_icon - 1];
            URL url = getClass().getResource("vehicle_class_" + imgName[lsiv_icon] + ".jpg");
            if (url == null)
                url = urlUnknown;
            Image img = Toolkit.getDefaultToolkit().createImage(url);
            imgIcon[lsiv_icon] = new ImageIcon(img);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH];

        setDefaultValue();

        if (gdvsim.flag_specialVehicle_ok) {
            setValue();
        }

        int veh_class;
        String veh_name;
        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            veh_class = Integer.valueOf(comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].getSelectedItem().toString()).intValue();
            veh_name = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[veh_class];
            VehicleIcon[lsiv_veh].setIcon(getImageIcon(veh_name));
            VehicleName[lsiv_veh].addItem(veh_name);
        }

        label_total = new JLabel("Number of Special Vehicle (maximum = " + PARAMS.TEXAS_MODEL_NSV + ")");

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(calculateNumOfSpecialVehicle()));

        label_note[0] = new JLabel("NOTE:");
        label_note[1] = new JLabel("    Forced Stop Location values:");
        label_note[2] = new JLabel("    I   = Leg Inbound");
        label_note[3] = new JLabel("    L  = Diamond Interchange Inbound to Left Intersection");
        label_note[4] = new JLabel("    O = Leg Outbound");
        label_note[5] = new JLabel("    P = Intesection Path");
        label_note[6] = new JLabel("    R = Diamond Interchange Inbound to Right Intersection");
        label_note[7] = new JLabel("END NOTE");

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        textFieldFocusListener = new TextFieldFocusListener();

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        text_vehicle[lsiv_veh][lsiv_field].addKeyListener(helpListener);
                        break;

                    case VEHICLE_CLASS:
                    case DRIVER_CLASS:
                    case DESIRED_SPEED:
                    case DESIRED_O_LEG:
                    case INBOUND_LEG:
                    case INBOUND_LANE:
                    case LOGOUT_SUMMRY:
                    case FREE_UTURN:
                    case FORCEDSTOPIOP:
                    case FORCEDSTOPLIP:
                    case EMERGENCY_VEH:
                        comboBox_vehicle[lsiv_veh][lsiv_field].addKeyListener(openComboMenuListener);
                        comboBox_vehicle[lsiv_veh][lsiv_field].addKeyListener(helpListener);
                        break;
                }
            }
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            text_vehicle[lsiv_veh][FORCEDSTOPTIM].addActionListener(componentActionListener);
            text_vehicle[lsiv_veh][FORCED_GO_TIM].addActionListener(componentActionListener);
            text_vehicle[lsiv_veh][FORCED_RRSTIM].addActionListener(componentActionListener);

            text_vehicle[lsiv_veh][FORCEDSTOPTIM].addKeyListener(componentKeyListener);
            text_vehicle[lsiv_veh][FORCED_GO_TIM].addKeyListener(componentKeyListener);
            text_vehicle[lsiv_veh][FORCED_RRSTIM].addKeyListener(componentKeyListener);
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        text_vehicle[lsiv_veh][lsiv_field].addFocusListener(textFieldFocusListener);
                }
            }
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].addActionListener(componentActionListener);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].addKeyListener(componentKeyListener);

            comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].addActionListener(componentActionListener);
            comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].addKeyListener(componentKeyListener);
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            add_button[lsiv_veh].addActionListener(componentActionListener);
            del_button[lsiv_veh].addActionListener(componentActionListener);
            up_button[lsiv_veh].addActionListener(componentActionListener);
            down_button[lsiv_veh].addActionListener(componentActionListener);

            add_button[lsiv_veh].addKeyListener(componentKeyListener);
            del_button[lsiv_veh].addKeyListener(componentKeyListener);
            up_button[lsiv_veh].addKeyListener(componentKeyListener);
            down_button[lsiv_veh].addKeyListener(componentKeyListener);

            add_button[lsiv_veh].addKeyListener(helpListener);
            del_button[lsiv_veh].addKeyListener(helpListener);
            up_button[lsiv_veh].addKeyListener(helpListener);
            down_button[lsiv_veh].addKeyListener(helpListener);
        }

        cbo_total.addKeyListener(helpListener);
        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        int iRow = 0;
        int iCol = 25;

        gbConstraints.insets = new Insets(2, 2, 2, 2);

        addComponent(panel_title, iRow, 0, iCol, 1);
        iRow++;

        addComponent(label_field[1], iRow, 5, 1, 1);
        addComponent(label_field[2], iRow, 6, 1, 1);
        addComponent(label_classification, iRow, 7, 2, 1);

        for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, lsiv_field + 6, 1, 1);
        }

        iRow++;

        for (int lsiv_veh = 1; lsiv_veh <= Math.min(number_of_specialVehicle + 5, MAXNUMOFSPECIALVEHICLE); lsiv_veh++) {
            addComponent(label_class[lsiv_veh], iRow, 0, 1, 1);

            addComponent(add_button[lsiv_veh], iRow, 1, 1, 1);
            addComponent(del_button[lsiv_veh], iRow, 2, 1, 1);
            addComponent(up_button[lsiv_veh], iRow, 3, 1, 1);
            addComponent(down_button[lsiv_veh], iRow, 4, 1, 1);

            addComponent(text_vehicle[lsiv_veh][1], iRow, 5, 1, 1);
            addComponent(comboBox_vehicle[lsiv_veh][2], iRow, 6, 1, 1);
            addComponent(VehicleName[lsiv_veh], iRow, 7, 1, 1);
            addComponent(VehicleIcon[lsiv_veh], iRow, 8, 1, 1);

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    // case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        addComponent(text_vehicle[lsiv_veh][lsiv_field], iRow, lsiv_field + 6, 1, 1);
                        break;

                    // case VEHICLE_CLASS:
                    case DRIVER_CLASS:
                    case DESIRED_SPEED:
                    case DESIRED_O_LEG:
                    case INBOUND_LEG:
                    case INBOUND_LANE:
                    case LOGOUT_SUMMRY:
                    case FREE_UTURN:
                    case FORCEDSTOPIOP:
                    case FORCEDSTOPLIP:
                    case EMERGENCY_VEH:
                        addComponent(comboBox_vehicle[lsiv_veh][lsiv_field], iRow, lsiv_field + 6, 1, 1);
                        break;
                }

            }

            iRow++;
        }

        addComponent(cbo_total, iRow, 1, 1, 1);
        addComponent(label_total, iRow++, 2, 21, 1);

        for (int lsiv_i = 0; lsiv_i < 8; lsiv_i++) {
            if ((!gdvsim.gclv_inter.mbov_is_diamond_interchange) && (lsiv_i == 3 || lsiv_i == 6)) {
                continue;
            }
            addComponent(label_note[lsiv_i], iRow++, 11, 6, 1);
        }

        addComponent(ok_panel, iRow, 0, iCol, 1);

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);
        aFrame.setFocusTraversalPolicy(new focusPolicy());
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method SpecialVehicleDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    ImageIcon getImageIcon(String vehicleName) {
        ImageIcon icn = new ImageIcon();

        for (int lsiv_icon = 1; lsiv_icon <= NUMOFICON; lsiv_icon++) {
            if (vehicleName.equals(imgName[lsiv_icon])) {
                icn = imgIcon[lsiv_icon];
            }
        }
        return icn;
    } // end of method getImageIcon
    /*
     * ImageIcon getImageIcon(int VehicleNum) { ImageIcon icn = new ImageIcon(); icn =
     * imgIcon[VehicleNum]; return icn; } // end of method getImageIcon
     */

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            int sumOfSpecialVehicles = calculateNumOfSpecialVehicle();

            if (sumOfSpecialVehicles == 0) {
                if (aComponent.equals(add_button[1])) {
                    return cbo_total;
                }
                else if (aComponent.equals(cancelButton)) {
                    return add_button[1];
                }
                else if (aComponent.equals(del_button[1])) {
                    return cbo_total;
                }
            }
            else if (sumOfSpecialVehicles == 1) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (aComponent.equals(text_vehicle[1][lsiv_field])) {
                        switch (lsiv_field) {
                            case QUEUE_IN_TIME:
                                if (isErrorInNumber(1, QUEUE_IN_TIME)) {
                                    return text_vehicle[1][QUEUE_IN_TIME];
                                }

                                text_vehicle[1][QUEUE_IN_TIME].setBackground(aFrame.getBackground());

                                return comboBox_vehicle[1][lsiv_field + 1];

                            case FORCEDSTOPTIM:
                                if (isErrorInNumber(1, FORCEDSTOPTIM)) {
                                    return text_vehicle[1][FORCEDSTOPTIM];
                                }

                                text_vehicle[1][FORCEDSTOPTIM].setBackground(aFrame.getBackground());

                                if (Double.valueOf(text_vehicle[1][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                                    comboBox_vehicle[1][FORCEDSTOPIOP].setEnabled(false);
                                    comboBox_vehicle[1][FORCEDSTOPLIP].setEnabled(false);
                                    text_vehicle[1][FORCEDSTOPPOS].setEditable(false);
                                    text_vehicle[1][FORCEDSTOPDWT].setEditable(false);

                                    comboBox_vehicle[1][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
                                    comboBox_vehicle[1][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
                                    text_vehicle[1][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
                                    text_vehicle[1][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));

                                    setTextFieldColor();

                                    text_vehicle[1][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[1][FORCED_GO_TIM];
                                }
                                else {
                                    comboBox_vehicle[1][FORCEDSTOPIOP].setEnabled(true);
                                    comboBox_vehicle[1][FORCEDSTOPLIP].setEnabled(true);
                                    text_vehicle[1][FORCEDSTOPPOS].setEditable(true);
                                    text_vehicle[1][FORCEDSTOPDWT].setEditable(true);
                                    setTextFieldColor();

                                    return comboBox_vehicle[1][FORCEDSTOPIOP];
                                }

                            case FORCEDSTOPPOS:
                                if (isErrorInNumber(1, FORCEDSTOPPOS)) {
                                    return text_vehicle[1][FORCEDSTOPPOS];
                                }

                                text_vehicle[1][FORCEDSTOPPOS].setBackground(aFrame.getBackground());
                                text_vehicle[1][FORCEDSTOPDWT].setBackground(new Color(176, 197, 218));

                                return text_vehicle[1][FORCEDSTOPDWT];

                            case FORCEDSTOPDWT:
                                if (isErrorInNumber(1, FORCEDSTOPDWT)) {
                                    return text_vehicle[1][FORCEDSTOPDWT];
                                }

                                text_vehicle[1][FORCEDSTOPDWT].setBackground(aFrame.getBackground());
                                text_vehicle[1][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));

                                return text_vehicle[1][FORCED_GO_TIM];

                            case FORCED_GO_TIM:
                                if (isErrorInNumber(1, FORCED_GO_TIM)) {
                                    return text_vehicle[1][FORCED_GO_TIM];
                                }

                                text_vehicle[1][FORCED_GO_TIM].setBackground(aFrame.getBackground());

                                if (Double.valueOf(text_vehicle[1][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                                    text_vehicle[1][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
                                    text_vehicle[1][FORCED_GO_ACT].setEditable(false);
                                    setTextFieldColor();

                                    text_vehicle[1][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[1][FORCED_RRSTIM];
                                }
                                else {
                                    text_vehicle[1][FORCED_GO_ACT].setEditable(true);
                                    setTextFieldColor();

                                    text_vehicle[1][FORCED_GO_ACT].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[1][FORCED_GO_ACT];
                                }

                            case FORCED_GO_ACT:
                                if (isErrorInNumber(1, FORCED_GO_ACT)) {
                                    return text_vehicle[1][FORCED_GO_ACT];
                                }

                                text_vehicle[1][FORCED_GO_ACT].setBackground(aFrame.getBackground());

                                text_vehicle[1][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));

                                return text_vehicle[1][FORCED_RRSTIM];

                            case FORCED_RRSTIM:
                                if (isErrorInNumber(1, FORCED_RRSTIM)) {
                                    return text_vehicle[1][FORCED_RRSTIM];
                                }

                                text_vehicle[1][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                if (Double.valueOf(text_vehicle[1][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                                    text_vehicle[1][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));
                                    text_vehicle[1][FORCED_RRSACT].setEditable(false);
                                    setTextFieldColor();

                                    // return add_button[2];
                                    return comboBox_vehicle[1][EMERGENCY_VEH];
                                }
                                else {
                                    text_vehicle[1][FORCED_RRSACT].setEditable(true);
                                    setTextFieldColor();

                                    text_vehicle[1][FORCED_RRSACT].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[1][FORCED_RRSACT];
                                }

                            case FORCED_RRSACT:
                                if (isErrorInNumber(1, FORCED_RRSACT)) {
                                    return text_vehicle[1][FORCED_RRSACT];
                                }

                                text_vehicle[1][FORCED_RRSACT].setBackground(aFrame.getBackground());

                                // return add_button[2];
                                return comboBox_vehicle[1][EMERGENCY_VEH];
                        }
                    }

                    if (aComponent.equals(comboBox_vehicle[1][lsiv_field])) {
                        switch (lsiv_field) {
                            case VEHICLE_CLASS:
                            case DRIVER_CLASS:
                            case DESIRED_SPEED:
                            case DESIRED_O_LEG:
                            case INBOUND_LEG:
                            case INBOUND_LANE:
                            case LOGOUT_SUMMRY:
                            case FREE_UTURN:
                            case FORCEDSTOPIOP:
                            case FORCEDSTOPLIP:
                            case EMERGENCY_VEH:

                                if (lsiv_field == FREE_UTURN || lsiv_field == FORCEDSTOPLIP) {
                                    text_vehicle[1][lsiv_field + 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[1][lsiv_field + 1];
                                }
                                else if (lsiv_field == FORCEDSTOPIOP) {
                                    if (comboBox_vehicle[1][FORCEDSTOPLIP].isEnabled()) {
                                        return comboBox_vehicle[1][FORCEDSTOPLIP];
                                    }
                                    else {
                                        text_vehicle[1][FORCEDSTOPPOS].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[1][FORCEDSTOPPOS];
                                    }
                                }
                                else if (lsiv_field == EMERGENCY_VEH) {
                                    return add_button[2];
                                }
                                else if (lsiv_field == VEHICLE_CLASS) {
                                    return VehicleName[1];
                                }
                                else {
                                    return comboBox_vehicle[1][lsiv_field + 1];
                                }
                        }
                    }

                    if (aComponent.equals(VehicleName[1])) {
                        return VehicleIcon[1];
                    }

                    if (aComponent.equals(VehicleIcon[1])) {
                        return comboBox_vehicle[1][DRIVER_CLASS];
                    }
                }

                if (aComponent.equals(text_vehicle[1][FORCED_RRSACT])) {
                    if (isErrorInNumber(1, FORCED_RRSACT)) {
                        return text_vehicle[1][FORCED_RRSACT];
                    }

                    text_vehicle[1][FORCED_RRSACT].setBackground(aFrame.getBackground());
                    // return add_button[2];
                    return comboBox_vehicle[1][EMERGENCY_VEH];
                }
                else if (aComponent.equals(add_button[1])) {
                    return del_button[1];
                }
                else if (aComponent.equals(add_button[2])) {
                    return cbo_total;
                }
                else if (aComponent.equals(del_button[1])) {
                    text_vehicle[1][QUEUE_IN_TIME].setBackground(new Color(176, 197, 218));

                    return text_vehicle[1][QUEUE_IN_TIME];
                }
                else if (aComponent.equals(del_button[2])) {
                    return cbo_total;
                }
                else if (aComponent.equals(cancelButton)) {
                    return add_button[1];
                }
            }
            else if (MAXNUMOFSPECIALVEHICLE == sumOfSpecialVehicles && sumOfSpecialVehicles != 0 && sumOfSpecialVehicles != 1) {
                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (aComponent.equals(text_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case QUEUE_IN_TIME:
                                    if (isErrorInNumber(lsiv_veh, QUEUE_IN_TIME)) {
                                        return text_vehicle[lsiv_veh][QUEUE_IN_TIME];
                                    }

                                    text_vehicle[lsiv_veh][QUEUE_IN_TIME].setBackground(aFrame.getBackground());
                                    return comboBox_vehicle[lsiv_veh][lsiv_field + 1];

                                case FORCEDSTOPTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPTIM)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(false);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(false);

                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));

                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }
                                    else {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(true);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(true);
                                        setTextFieldColor();

                                        return comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP];
                                    }

                                case FORCEDSTOPPOS:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPPOS)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPPOS];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(aFrame.getBackground());
                                    text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[lsiv_veh][FORCEDSTOPDWT];

                                case FORCEDSTOPDWT:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPDWT)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPDWT];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(aFrame.getBackground());
                                    text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[lsiv_veh][FORCED_GO_TIM];

                                case FORCED_GO_TIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_TIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(false);
                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(true);
                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_GO_ACT];
                                    }

                                case FORCED_GO_ACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_ACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_ACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(aFrame.getBackground());

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[lsiv_veh][FORCED_RRSTIM];

                                case FORCED_RRSTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSTIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(false);
                                        setTextFieldColor();

                                        if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                                            // return cbo_total;
                                            return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                        }
                                        else {
                                            // return del_button[lsiv_veh + 1];
                                            return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                        }
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(true);
                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                    }

                                case FORCED_RRSACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(aFrame.getBackground());
                                    if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                                        // return cbo_total;
                                        return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                    }
                                    else {
                                        // return del_button[lsiv_veh + 1];
                                        return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                    }
                            }
                        }

                        if (aComponent.equals(comboBox_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case VEHICLE_CLASS:
                                case DRIVER_CLASS:
                                case DESIRED_SPEED:
                                case DESIRED_O_LEG:
                                case INBOUND_LEG:
                                case INBOUND_LANE:
                                case LOGOUT_SUMMRY:
                                case FREE_UTURN:
                                case FORCEDSTOPIOP:
                                case FORCEDSTOPLIP:
                                case EMERGENCY_VEH:

                                    if (lsiv_field == FREE_UTURN || lsiv_field == FORCEDSTOPLIP) {
                                        text_vehicle[lsiv_veh][lsiv_field + 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field + 1];
                                    }
                                    else if (lsiv_field == FORCEDSTOPIOP) {
                                        if (comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].isEnabled()) {
                                            return comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP];
                                        }
                                        else {
                                            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][FORCEDSTOPPOS];
                                        }
                                    }
                                    else if (lsiv_field == EMERGENCY_VEH) {
                                        // return cbo_total;
                                        if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                                            return cbo_total;
                                        }
                                        else {
                                            return del_button[lsiv_veh + 1];
                                        }
                                    }
                                    else if (lsiv_field == VEHICLE_CLASS) {
                                        return VehicleName[lsiv_veh];
                                    }
                                    else {
                                        return comboBox_vehicle[lsiv_veh][lsiv_field + 1];
                                    }
                            }
                        }

                        if (aComponent.equals(VehicleName[lsiv_veh])) {
                            return VehicleIcon[lsiv_veh];
                        }

                        if (aComponent.equals(VehicleIcon[lsiv_veh])) {
                            return comboBox_vehicle[lsiv_veh][DRIVER_CLASS];
                        }

                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                    if (aComponent.equals(add_button[lsiv_veh])) {
                        return del_button[lsiv_veh];
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(del_button[lsiv_veh])) {
                        if (lsiv_veh == 1) {
                            return down_button[lsiv_veh];
                        }
                        else {
                            return up_button[lsiv_veh];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(up_button[lsiv_veh])) {
                        if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                            text_vehicle[lsiv_veh][1].setBackground(new Color(176, 197, 218));
                            return text_vehicle[lsiv_veh][1];
                        }
                        else {
                            return down_button[lsiv_veh];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= (sumOfSpecialVehicles - 1); lsiv_veh++) {
                    if (aComponent.equals(down_button[lsiv_veh])) {
                        text_vehicle[lsiv_veh][1].setBackground(new Color(176, 197, 218));
                        return text_vehicle[lsiv_veh][1];
                    }
                }

                if (aComponent.equals(cancelButton)) {
                    return del_button[1];
                }
            }
            else {
                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (aComponent.equals(text_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case QUEUE_IN_TIME:
                                    if (isErrorInNumber(lsiv_veh, QUEUE_IN_TIME)) {
                                        return text_vehicle[lsiv_veh][QUEUE_IN_TIME];
                                    }
                                    text_vehicle[lsiv_veh][QUEUE_IN_TIME].setBackground(aFrame.getBackground());
                                    return comboBox_vehicle[lsiv_veh][lsiv_field + 1];

                                case FORCEDSTOPTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPTIM)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(false);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(false);

                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));

                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }
                                    else {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(true);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(true);
                                        setTextFieldColor();

                                        return comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP];
                                    }

                                case FORCEDSTOPPOS:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPPOS)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPPOS];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(aFrame.getBackground());
                                    text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[lsiv_veh][FORCEDSTOPDWT];

                                case FORCEDSTOPDWT:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPDWT)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPDWT];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(aFrame.getBackground());
                                    text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[lsiv_veh][FORCED_GO_TIM];

                                case FORCED_GO_TIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_TIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(false);
                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(true);
                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_GO_ACT];
                                    }

                                case FORCED_GO_ACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_ACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_ACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(aFrame.getBackground());

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));

                                    return text_vehicle[lsiv_veh][FORCED_RRSTIM];

                                case FORCED_RRSTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSTIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(false);
                                        setTextFieldColor();

                                        if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                                            // return cbo_total;
                                            return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                        }
                                        else {
                                            // return add_button[lsiv_veh + 1];
                                            return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                        }
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(true);
                                        setTextFieldColor();

                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(new Color(176, 197, 218));

                                        return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                    }

                                case FORCED_RRSACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(aFrame.getBackground());
                                    if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                                        // return cbo_total;
                                        return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                    }
                                    else {
                                        // return add_button[lsiv_veh + 1];
                                        return comboBox_vehicle[lsiv_veh][EMERGENCY_VEH];
                                    }
                            }
                        }

                        if (aComponent.equals(comboBox_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case VEHICLE_CLASS:
                                case DRIVER_CLASS:
                                case DESIRED_SPEED:
                                case DESIRED_O_LEG:
                                case INBOUND_LEG:
                                case INBOUND_LANE:
                                case LOGOUT_SUMMRY:
                                case FREE_UTURN:
                                case FORCEDSTOPIOP:
                                case FORCEDSTOPLIP:
                                case EMERGENCY_VEH:

                                    if (lsiv_field == EMERGENCY_VEH) {
                                        return add_button[lsiv_veh + 1];
                                    }
                                    else if (lsiv_field == FORCEDSTOPIOP) {
                                        if (comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].isEnabled()) {
                                            return comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP];
                                        }
                                        else {
                                            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][FORCEDSTOPPOS];
                                        }
                                    }
                                    else if (lsiv_field == VEHICLE_CLASS) {
                                        return VehicleName[lsiv_veh];
                                    }
                                    else {
                                        if (lsiv_field == FREE_UTURN || lsiv_field == FORCEDSTOPLIP) {
                                            text_vehicle[lsiv_veh][lsiv_field + 1].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][lsiv_field + 1];
                                        }
                                        else {
                                            return comboBox_vehicle[lsiv_veh][lsiv_field + 1];
                                        }
                                    }
                            }
                        }

                        if (aComponent.equals(VehicleName[lsiv_veh])) {
                            return VehicleIcon[lsiv_veh];
                        }

                        if (aComponent.equals(VehicleIcon[lsiv_veh])) {
                            return comboBox_vehicle[lsiv_veh][DRIVER_CLASS];
                        }

                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= (sumOfSpecialVehicles + 1); lsiv_veh++) {
                    if (aComponent.equals(add_button[lsiv_veh])) {
                        if (lsiv_veh == sumOfSpecialVehicles + 1) {
                            return cbo_total;
                        }
                        else {
                            return del_button[lsiv_veh];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(del_button[lsiv_veh])) {
                        if (lsiv_veh == 1) {
                            return down_button[lsiv_veh];
                        }
                        else {
                            return up_button[lsiv_veh];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(up_button[lsiv_veh])) {
                        if (lsiv_veh == sumOfSpecialVehicles) {
                            text_vehicle[lsiv_veh][1].setBackground(new Color(176, 197, 218));
                            return text_vehicle[lsiv_veh][1];
                        }
                        else {
                            return down_button[lsiv_veh];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= (sumOfSpecialVehicles - 1); lsiv_veh++) {
                    if (aComponent.equals(down_button[lsiv_veh])) {
                        text_vehicle[lsiv_veh][1].setBackground(new Color(176, 197, 218));
                        return text_vehicle[lsiv_veh][1];
                    }
                }

                if (aComponent.equals(cancelButton)) {
                    return add_button[1];
                }
            }

            if (aComponent.equals(cbo_total)) {
                return okButton;
            }
            else if (aComponent.equals(okButton)) {
                return applyButton;
            }
            else if (aComponent.equals(applyButton)) {
                return cancelButton;
            }

            return okButton;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            int sumOfSpecialVehicles = calculateNumOfSpecialVehicle();

            if (sumOfSpecialVehicles == 0) {
                if (aComponent.equals(add_button[1])) {
                    return cancelButton;
                }
                else if (aComponent.equals(cbo_total)) {
                    return add_button[1];
                }
            }
            else if (sumOfSpecialVehicles == 1) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (aComponent.equals(text_vehicle[1][lsiv_field])) {
                        switch (lsiv_field) {
                            case QUEUE_IN_TIME:
                                if (isErrorInNumber(1, QUEUE_IN_TIME)) {
                                    return text_vehicle[1][QUEUE_IN_TIME];
                                }

                                text_vehicle[1][QUEUE_IN_TIME].setBackground(aFrame.getBackground());
                                return del_button[1];

                            case FORCEDSTOPTIM:
                                if (isErrorInNumber(1, FORCEDSTOPTIM)) {
                                    return text_vehicle[1][FORCEDSTOPTIM];
                                }

                                text_vehicle[1][FORCEDSTOPTIM].setBackground(aFrame.getBackground());

                                if (Double.valueOf(text_vehicle[1][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                                    comboBox_vehicle[1][FORCEDSTOPIOP].setEnabled(false);
                                    comboBox_vehicle[1][FORCEDSTOPLIP].setEnabled(false);
                                    text_vehicle[1][FORCEDSTOPPOS].setEditable(false);
                                    text_vehicle[1][FORCEDSTOPDWT].setEditable(false);

                                    comboBox_vehicle[1][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
                                    comboBox_vehicle[1][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
                                    text_vehicle[1][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
                                    text_vehicle[1][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));
                                }
                                else {
                                    comboBox_vehicle[1][FORCEDSTOPIOP].setEnabled(true);
                                    comboBox_vehicle[1][FORCEDSTOPLIP].setEnabled(true);
                                    text_vehicle[1][FORCEDSTOPPOS].setEditable(true);
                                    text_vehicle[1][FORCEDSTOPDWT].setEditable(true);
                                }

                                setTextFieldColor();

                                return comboBox_vehicle[1][lsiv_field - 1];

                            case FORCEDSTOPPOS:
                                if (isErrorInNumber(1, FORCEDSTOPPOS)) {
                                    return text_vehicle[1][FORCEDSTOPPOS];
                                }

                                text_vehicle[1][FORCEDSTOPPOS].setBackground(aFrame.getBackground());

                                if (comboBox_vehicle[1][FORCEDSTOPLIP].isEnabled()) {
                                    return comboBox_vehicle[1][FORCEDSTOPLIP];
                                }
                                else {
                                    return comboBox_vehicle[1][FORCEDSTOPIOP];
                                }

                            case FORCEDSTOPDWT:
                                if (isErrorInNumber(1, FORCEDSTOPDWT)) {
                                    return text_vehicle[1][FORCEDSTOPDWT];
                                }

                                text_vehicle[1][FORCEDSTOPDWT].setBackground(aFrame.getBackground());
                                text_vehicle[1][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                return text_vehicle[1][lsiv_field - 1];

                            case FORCED_GO_TIM:
                                if (isErrorInNumber(1, FORCED_GO_TIM)) {
                                    return text_vehicle[1][FORCED_GO_TIM];
                                }

                                text_vehicle[1][FORCED_GO_TIM].setBackground(aFrame.getBackground());

                                if (Double.valueOf(text_vehicle[1][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                                    text_vehicle[1][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
                                    text_vehicle[1][FORCED_GO_ACT].setEditable(false);
                                }
                                else {
                                    text_vehicle[1][FORCED_GO_ACT].setEditable(true);
                                }

                                setTextFieldColor();

                                if (text_vehicle[1][lsiv_field - 1].isEditable()) {
                                    text_vehicle[1][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[1][lsiv_field - 1];
                                }
                                else {
                                    text_vehicle[1][FORCEDSTOPTIM].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[1][FORCEDSTOPTIM];
                                }

                            case FORCED_GO_ACT:
                                if (isErrorInNumber(1, FORCED_GO_ACT)) {
                                    return text_vehicle[1][FORCED_GO_ACT];
                                }

                                text_vehicle[1][FORCED_GO_ACT].setBackground(aFrame.getBackground());
                                text_vehicle[1][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                return text_vehicle[1][lsiv_field - 1];

                            case FORCED_RRSTIM:
                                if (isErrorInNumber(1, FORCED_RRSTIM)) {
                                    return text_vehicle[1][FORCED_RRSTIM];
                                }

                                text_vehicle[1][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                if (Double.valueOf(text_vehicle[1][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                                    text_vehicle[1][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));
                                    text_vehicle[1][FORCED_RRSACT].setEditable(false);
                                }
                                else {
                                    text_vehicle[1][FORCED_RRSACT].setEditable(true);
                                }

                                setTextFieldColor();

                                if (text_vehicle[1][lsiv_field - 1].isEditable()) {
                                    text_vehicle[1][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[1][lsiv_field - 1];
                                }
                                else {
                                    text_vehicle[1][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[1][FORCED_GO_TIM];
                                }

                            case FORCED_RRSACT:
                                if (isErrorInNumber(1, FORCED_RRSACT)) {
                                    return text_vehicle[1][FORCED_RRSACT];
                                }

                                text_vehicle[1][FORCED_RRSACT].setBackground(aFrame.getBackground());
                                text_vehicle[1][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                return text_vehicle[1][lsiv_field - 1];

                        }
                    }

                    if (aComponent.equals(comboBox_vehicle[1][lsiv_field])) {
                        switch (lsiv_field) {
                            case VEHICLE_CLASS:
                            case DRIVER_CLASS:
                            case DESIRED_SPEED:
                            case DESIRED_O_LEG:
                            case INBOUND_LEG:
                            case INBOUND_LANE:
                            case LOGOUT_SUMMRY:
                            case FREE_UTURN:
                            case FORCEDSTOPIOP:
                            case FORCEDSTOPLIP:
                            case EMERGENCY_VEH:

                                if ((lsiv_field == VEHICLE_CLASS) || (lsiv_field == FORCEDSTOPIOP)) {
                                    text_vehicle[1][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[1][lsiv_field - 1];
                                }
                                else if (lsiv_field == FORCEDSTOPLIP) {
                                    if (comboBox_vehicle[1][FORCEDSTOPLIP].isEnabled()) {
                                        return comboBox_vehicle[1][FORCEDSTOPLIP];
                                    }
                                    else {
                                        return comboBox_vehicle[1][FORCEDSTOPIOP];
                                    }
                                }
                                else if (lsiv_field == EMERGENCY_VEH) {
                                    if (text_vehicle[1][FORCED_RRSACT].isEditable()) {
                                        text_vehicle[1][FORCED_RRSACT].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[1][FORCED_RRSACT];
                                    }
                                    else {
                                        text_vehicle[1][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[1][FORCED_RRSTIM];
                                    }
                                }
                                else if (lsiv_field == DRIVER_CLASS) {
                                    return VehicleIcon[1];
                                }

                                else {
                                    return comboBox_vehicle[1][lsiv_field - 1];
                                }
                        }
                    }

                    if (aComponent.equals(VehicleIcon[1])) {
                        return VehicleName[1];
                    }

                    if (aComponent.equals(VehicleName[1])) {
                        return comboBox_vehicle[1][VEHICLE_CLASS];
                    }
                }

                if (aComponent.equals(text_vehicle[1][1])) {
                    if (isErrorInNumber(1, 1)) {
                        return text_vehicle[1][1];
                    }

                    return del_button[1];
                }
                else if (aComponent.equals(add_button[1])) {
                    return cancelButton;
                }
                else if (aComponent.equals(del_button[1])) {
                    return add_button[1];
                }
                else if (aComponent.equals(add_button[2])) {
                    /*
                     * if(text_vehicle[1][FORCED_RRSACT].isEditable()) {
                     * text_vehicle[1][FORCED_RRSACT].setBackground(new Color(176, 197, 218));
                     * return text_vehicle[1][FORCED_RRSACT]; } else {
                     * text_vehicle[1][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));
                     * return text_vehicle[1][FORCED_RRSTIM]; }
                     */
                    return comboBox_vehicle[1][EMERGENCY_VEH];
                }
                else if (aComponent.equals(cbo_total)) {
                    return add_button[2];
                }
            }
            else if (MAXNUMOFSPECIALVEHICLE == sumOfSpecialVehicles && sumOfSpecialVehicles != 0 && sumOfSpecialVehicles != 1) {
                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (aComponent.equals(text_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case QUEUE_IN_TIME:
                                    if (isErrorInNumber(lsiv_veh, QUEUE_IN_TIME)) {
                                        return text_vehicle[lsiv_veh][QUEUE_IN_TIME];
                                    }

                                    text_vehicle[lsiv_veh][QUEUE_IN_TIME].setBackground(aFrame.getBackground());

                                    if (lsiv_veh == MAXNUMOFSPECIALVEHICLE) {
                                        return up_button[lsiv_veh];
                                    }
                                    else {
                                        return down_button[lsiv_veh];
                                    }

                                case FORCEDSTOPTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPTIM)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(false);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(false);

                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));
                                    }
                                    else {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(true);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(true);
                                    }

                                    setTextFieldColor();

                                    return comboBox_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCEDSTOPPOS:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPPOS)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPPOS];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(aFrame.getBackground());

                                    return comboBox_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCEDSTOPDWT:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPDWT)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPDWT];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(aFrame.getBackground());

                                    text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCED_GO_TIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_TIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(false);
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(true);
                                    }

                                    setTextFieldColor();

                                    if (text_vehicle[lsiv_veh][lsiv_field - 1].isEditable()) {
                                        text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCEDSTOPTIM].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][FORCEDSTOPTIM];
                                    }

                                case FORCED_GO_ACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_ACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_ACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(aFrame.getBackground());
                                    text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCED_RRSTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSTIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(false);
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(true);
                                    }

                                    setTextFieldColor();

                                    if (text_vehicle[lsiv_veh][lsiv_field - 1].isEditable()) {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));
                                        text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }

                                case FORCED_RRSACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(aFrame.getBackground());
                                    text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[lsiv_veh][lsiv_field - 1];

                            }
                        }

                        if (aComponent.equals(comboBox_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case VEHICLE_CLASS:
                                case DRIVER_CLASS:
                                case DESIRED_SPEED:
                                case DESIRED_O_LEG:
                                case INBOUND_LEG:
                                case INBOUND_LANE:
                                case LOGOUT_SUMMRY:
                                case FREE_UTURN:
                                case FORCEDSTOPIOP:
                                case FORCEDSTOPLIP:
                                case EMERGENCY_VEH:

                                    if ((lsiv_field == VEHICLE_CLASS) || (lsiv_field == FORCEDSTOPIOP)) {
                                        text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                                    else if (lsiv_field == FORCEDSTOPLIP) {
                                        if (comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].isEnabled()) {
                                            return comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP];
                                        }
                                        else {
                                            return comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP];
                                        }
                                    }
                                    else if (lsiv_field == EMERGENCY_VEH) {
                                        if (text_vehicle[lsiv_veh][FORCED_RRSACT].isEditable()) {
                                            text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                        }
                                        else {
                                            text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                        }
                                    }
                                    else if (lsiv_field == DRIVER_CLASS) {
                                        return VehicleIcon[lsiv_veh];
                                    }
                                    else {
                                        return comboBox_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                            }
                        }

                        if (aComponent.equals(VehicleIcon[lsiv_veh])) {
                            return VehicleName[lsiv_veh];
                        }

                        if (aComponent.equals(VehicleName[lsiv_veh])) {
                            return comboBox_vehicle[lsiv_veh][VEHICLE_CLASS];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(del_button[lsiv_veh])) {
                        if (lsiv_veh == 1) {
                            return cancelButton;
                        }
                        else {
                            return comboBox_vehicle[lsiv_veh - 1][EMERGENCY_VEH];
                            /*
                             * if(text_vehicle[lsiv_veh - 1][FORCED_RRSACT].isEditable()) {
                             * text_vehicle[lsiv_veh - 1][FORCED_RRSACT].setBackground(new
                             * Color(176, 197, 218)); return text_vehicle[lsiv_veh -
                             * 1][FORCED_RRSACT]; } else { text_vehicle[lsiv_veh -
                             * 1][FORCED_RRSTIM].setBackground(new Color(176, 197, 218)); return
                             * text_vehicle[lsiv_veh - 1][FORCED_RRSTIM]; }
                             */
                        }
                    }
                }

                for (int lsiv_veh = 2; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(up_button[lsiv_veh])) {
                        return del_button[lsiv_veh];
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= (sumOfSpecialVehicles - 1); lsiv_veh++) {
                    if (aComponent.equals(down_button[lsiv_veh])) {
                        if (lsiv_veh == 1) {
                            return del_button[lsiv_veh];
                        }
                        else {
                            return up_button[lsiv_veh];
                        }
                    }
                }

                if (aComponent.equals(cbo_total)) {
                    return comboBox_vehicle[MAXNUMOFSPECIALVEHICLE][EMERGENCY_VEH];
                    /*
                     * if(text_vehicle[MAXNUMOFSPECIALVEHICLE][FORCED_RRSACT]. isEditable()) {
                     * text_vehicle[MAXNUMOFSPECIALVEHICLE][FORCED_RRSACT ].setBackground(new
                     * Color(176, 197, 218)); return
                     * text_vehicle[MAXNUMOFSPECIALVEHICLE][FORCED_RRSACT]; } else {
                     * text_vehicle[MAXNUMOFSPECIALVEHICLE][FORCED_RRSTIM] .setBackground(new
                     * Color(176, 197, 218)); return
                     * text_vehicle[MAXNUMOFSPECIALVEHICLE][FORCED_RRSTIM]; }
                     */
                }
            }
            else {
                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (aComponent.equals(text_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case QUEUE_IN_TIME:
                                    if (isErrorInNumber(lsiv_veh, QUEUE_IN_TIME)) {
                                        return text_vehicle[lsiv_veh][QUEUE_IN_TIME];
                                    }

                                    text_vehicle[lsiv_veh][QUEUE_IN_TIME].setBackground(aFrame.getBackground());

                                    if (lsiv_veh == sumOfSpecialVehicles) {
                                        return up_button[lsiv_veh];
                                    }
                                    else {
                                        return down_button[lsiv_veh];
                                    }

                                case FORCEDSTOPTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPTIM)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(false);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(false);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(false);

                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));
                                    }
                                    else {
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(true);
                                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(true);
                                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(true);
                                    }

                                    setTextFieldColor();

                                    return comboBox_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCEDSTOPPOS:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPPOS)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPPOS];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(aFrame.getBackground());

                                    return comboBox_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCEDSTOPDWT:
                                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPDWT)) {
                                        return text_vehicle[lsiv_veh][FORCEDSTOPDWT];
                                    }

                                    text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(aFrame.getBackground());

                                    text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCED_GO_TIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_TIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(false);
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(true);
                                    }

                                    setTextFieldColor();

                                    if (text_vehicle[lsiv_veh][lsiv_field - 1].isEditable()) {
                                        text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCEDSTOPTIM].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][FORCEDSTOPTIM];
                                    }

                                case FORCED_GO_ACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_GO_ACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_GO_ACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(aFrame.getBackground());

                                    text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[lsiv_veh][lsiv_field - 1];

                                case FORCED_RRSTIM:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSTIM)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(aFrame.getBackground());

                                    if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(false);
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(true);
                                    }

                                    setTextFieldColor();

                                    if (text_vehicle[lsiv_veh][lsiv_field - 1].isEditable()) {
                                        text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                                    else {
                                        text_vehicle[lsiv_veh][FORCED_GO_TIM].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][FORCED_GO_TIM];
                                    }

                                case FORCED_RRSACT:
                                    if (isErrorInNumber(lsiv_veh, FORCED_RRSACT)) {
                                        return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                    }

                                    text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(aFrame.getBackground());

                                    text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                    return text_vehicle[lsiv_veh][lsiv_field - 1];

                            }
                        }

                        if (aComponent.equals(comboBox_vehicle[lsiv_veh][lsiv_field])) {
                            switch (lsiv_field) {
                                case VEHICLE_CLASS:
                                case DRIVER_CLASS:
                                case DESIRED_SPEED:
                                case DESIRED_O_LEG:
                                case INBOUND_LEG:
                                case INBOUND_LANE:
                                case LOGOUT_SUMMRY:
                                case FREE_UTURN:
                                case FORCEDSTOPIOP:
                                case FORCEDSTOPLIP:
                                case EMERGENCY_VEH:

                                    if ((lsiv_field == VEHICLE_CLASS) || (lsiv_field == FORCEDSTOPIOP)) {
                                        text_vehicle[lsiv_veh][lsiv_field - 1].setBackground(new Color(176, 197, 218));
                                        return text_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                                    else if (lsiv_field == FORCEDSTOPLIP) {
                                        if (comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].isEnabled()) {
                                            return comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP];
                                        }
                                        else {
                                            return comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP];
                                        }
                                    }
                                    else if (lsiv_field == EMERGENCY_VEH) {
                                        if (text_vehicle[lsiv_veh][FORCED_RRSACT].isEditable()) {
                                            text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][FORCED_RRSACT];
                                        }
                                        else {
                                            text_vehicle[lsiv_veh][FORCED_RRSTIM].setBackground(new Color(176, 197, 218));
                                            return text_vehicle[lsiv_veh][FORCED_RRSTIM];
                                        }
                                    }
                                    else if (lsiv_field == DRIVER_CLASS) {
                                        return VehicleIcon[lsiv_veh];
                                    }
                                    else {
                                        return comboBox_vehicle[lsiv_veh][lsiv_field - 1];
                                    }
                            }
                        }

                        if (aComponent.equals(VehicleIcon[lsiv_veh])) {
                            return VehicleName[lsiv_veh];
                        }

                        if (aComponent.equals(VehicleName[lsiv_veh])) {
                            return comboBox_vehicle[lsiv_veh][VEHICLE_CLASS];
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= (sumOfSpecialVehicles + 1); lsiv_veh++) {
                    if (aComponent.equals(add_button[lsiv_veh])) {
                        if (lsiv_veh == 1) {
                            return cancelButton;
                        }
                        else {
                            return comboBox_vehicle[lsiv_veh - 1][EMERGENCY_VEH];
                            /*
                             * if(text_vehicle[lsiv_veh - 1][FORCED_RRSACT].isEditable()) {
                             * text_vehicle[lsiv_veh - 1][FORCED_RRSACT].setBackground(new
                             * Color(176, 197, 218)); return text_vehicle[lsiv_veh -
                             * 1][FORCED_RRSACT]; } else { text_vehicle[lsiv_veh -
                             * 1][FORCED_RRSTIM].setBackground(new Color(176, 197, 218)); return
                             * text_vehicle[lsiv_veh - 1][FORCED_RRSTIM]; }
                             */
                        }
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(del_button[lsiv_veh])) {
                        return add_button[lsiv_veh];
                    }
                }

                for (int lsiv_veh = 2; lsiv_veh <= sumOfSpecialVehicles; lsiv_veh++) {
                    if (aComponent.equals(up_button[lsiv_veh])) {
                        return del_button[lsiv_veh];
                    }
                }

                for (int lsiv_veh = 1; lsiv_veh <= (sumOfSpecialVehicles - 1); lsiv_veh++) {
                    if (aComponent.equals(down_button[lsiv_veh])) {
                        if (lsiv_veh == 1) {
                            return del_button[lsiv_veh];
                        }
                        else {
                            return up_button[lsiv_veh];
                        }
                    }
                }

                if (aComponent.equals(cbo_total)) {
                    return add_button[sumOfSpecialVehicles + 1];
                }
            }

            if (aComponent.equals(okButton)) {
                return cbo_total;
            }
            else if (aComponent.equals(applyButton)) {
                return okButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return applyButton;
            }

            return okButton;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            if (add_button[1].isEnabled()) {
                return add_button[1];
            }
            else {
                return del_button[1];
            }
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return cancelButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            if (add_button[1].isEnabled()) {
                return add_button[1];
            }
            else {
                return del_button[1];
            }
        }
    } // end of class focusPolicy

    void setDefaultValue() {
        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS]));
            comboBox_vehicle[lsiv_veh][DRIVER_CLASS].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS]));
            comboBox_vehicle[lsiv_veh][DESIRED_SPEED].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED]));
            comboBox_vehicle[lsiv_veh][DESIRED_O_LEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG]));
            comboBox_vehicle[lsiv_veh][INBOUND_LEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG]));
            comboBox_vehicle[lsiv_veh][INBOUND_LANE].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE]));
            comboBox_vehicle[lsiv_veh][LOGOUT_SUMMRY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY]));
            comboBox_vehicle[lsiv_veh][FREE_UTURN].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN]);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
            comboBox_vehicle[lsiv_veh][EMERGENCY_VEH].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH]);

            text_vehicle[lsiv_veh][QUEUE_IN_TIME].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME]));
            text_vehicle[lsiv_veh][FORCEDSTOPTIM].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM]));
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));
            text_vehicle[lsiv_veh][FORCED_GO_TIM].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM]));
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
            text_vehicle[lsiv_veh][FORCED_RRSTIM].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM]));
            text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));

        } // end for lsiv_veh

        setStatus(number_of_specialVehicle);

    } // end of method setDefaultValue

    void setValue() {
        number_of_specialVehicle = calculateNumOfSpecialVehicle();

        for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
            comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_vehicle_class));
            comboBox_vehicle[lsiv_veh][DRIVER_CLASS].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_driver_class));
            comboBox_vehicle[lsiv_veh][DESIRED_SPEED].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_desired_speed));
            comboBox_vehicle[lsiv_veh][DESIRED_O_LEG].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_desired_outbound_leg));
            comboBox_vehicle[lsiv_veh][INBOUND_LEG].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_inbound_leg));
            comboBox_vehicle[lsiv_veh][INBOUND_LANE].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_inbound_lane));
            comboBox_vehicle[lsiv_veh][LOGOUT_SUMMRY].setSelectedItem(Integer
                    .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_vehicle_logout_summary));
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_free_uturn_use != null) {
                comboBox_vehicle[lsiv_veh][FREE_UTURN].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_free_uturn_use);
            }
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_forced_stop_inb_out_path != null) {
                comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_forced_stop_inb_out_path);
            }
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer
                    .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_forced_stop_leg_or_path));
            comboBox_vehicle[lsiv_veh][EMERGENCY_VEH].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_emergency_vehicle);

            text_vehicle[lsiv_veh][QUEUE_IN_TIME].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_queue_in_time));
            text_vehicle[lsiv_veh][FORCEDSTOPTIM].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_time));
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_position));
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_dwell_time));
            text_vehicle[lsiv_veh][FORCED_GO_TIM].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_go_time));
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_go_active_time));
            text_vehicle[lsiv_veh][FORCED_RRSTIM].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_run_red_sig_time));
            text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_run_red_sig_acttim));
        }

        setStatus(number_of_specialVehicle);

    } // end of method setValue

    void setStatus(int sumOfVehicles) {
        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            if (lsiv_veh <= sumOfVehicles) {
                label_class[lsiv_veh].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case QUEUE_IN_TIME:
                        case FORCEDSTOPTIM:
                        case FORCEDSTOPPOS:
                        case FORCEDSTOPDWT:
                        case FORCED_GO_TIM:
                        case FORCED_GO_ACT:
                        case FORCED_RRSTIM:
                        case FORCED_RRSACT:
                            text_vehicle[lsiv_veh][lsiv_field].setEditable(true);
                            break;

                        case VEHICLE_CLASS:
                        case DRIVER_CLASS:
                        case DESIRED_SPEED:
                        case DESIRED_O_LEG:
                        case INBOUND_LEG:
                        case INBOUND_LANE:
                        case LOGOUT_SUMMRY:
                        case FREE_UTURN:
                        case FORCEDSTOPIOP:
                        case FORCEDSTOPLIP:
                        case EMERGENCY_VEH:
                            comboBox_vehicle[lsiv_veh][lsiv_field].setEnabled(true);
                            break;
                    }
                }

                VehicleIcon[lsiv_veh].setEnabled(true);
                VehicleName[lsiv_veh].setEnabled(true);
            }
            else {
                label_class[lsiv_veh].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case QUEUE_IN_TIME:
                        case FORCEDSTOPTIM:
                        case FORCEDSTOPPOS:
                        case FORCEDSTOPDWT:
                        case FORCED_GO_TIM:
                        case FORCED_GO_ACT:
                        case FORCED_RRSTIM:
                        case FORCED_RRSACT:
                            text_vehicle[lsiv_veh][lsiv_field].setEditable(false);
                            break;

                        case VEHICLE_CLASS:
                        case DRIVER_CLASS:
                        case DESIRED_SPEED:
                        case DESIRED_O_LEG:
                        case INBOUND_LEG:
                        case INBOUND_LANE:
                        case LOGOUT_SUMMRY:
                        case FREE_UTURN:
                        case FORCEDSTOPIOP:
                        case FORCEDSTOPLIP:
                        case EMERGENCY_VEH:
                            comboBox_vehicle[lsiv_veh][lsiv_field].setEnabled(false);
                            break;
                    }
                }

                VehicleIcon[lsiv_veh].setEnabled(false);
                VehicleName[lsiv_veh].setEnabled(false);
            }
        }

        if (sumOfVehicles == MAXNUMOFSPECIALVEHICLE) {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                add_button[lsiv_veh].setEnabled(false);
            }
        }
        else {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                if (lsiv_veh <= (sumOfVehicles + 1)) {
                    add_button[lsiv_veh].setEnabled(true);
                }
                else {
                    add_button[lsiv_veh].setEnabled(false);
                }
            }
        }

        if (sumOfVehicles == 0) {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                del_button[lsiv_veh].setEnabled(false);
            }
        }
        else {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                if (lsiv_veh <= sumOfVehicles) {
                    del_button[lsiv_veh].setEnabled(true);
                }
                else {
                    del_button[lsiv_veh].setEnabled(false);
                }
            }
        }

        if (sumOfVehicles <= 1) {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                up_button[lsiv_veh].setEnabled(false);
            }
        }
        else {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                if (lsiv_veh <= sumOfVehicles) {
                    up_button[lsiv_veh].setEnabled(true);
                }
                else {
                    up_button[lsiv_veh].setEnabled(false);
                }
                up_button[1].setEnabled(false);
            }
        }

        if (sumOfVehicles == 0) {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                down_button[lsiv_veh].setEnabled(false);
            }
        }
        else {
            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                if (lsiv_veh < sumOfVehicles) {
                    down_button[lsiv_veh].setEnabled(true);
                }
                else {
                    down_button[lsiv_veh].setEnabled(false);
                }

            }
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
                comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(false);
                comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
                text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(false);
                text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(false);
            }
            else {
                comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(true);
                comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
                text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(true);
                text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(true);
            }

            if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
                text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(false);
            }
            else {
                text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(true);
            }

            if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
                text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(false);
            }
            else {
                text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(true);
            }
        }

        setTextFieldColor();

    } // end of method setStatus()

    void setTextFieldColor() {
        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:

                        if (text_vehicle[lsiv_veh][lsiv_field].isEditable()) {
                            text_vehicle[lsiv_veh][lsiv_field].setBackground(aFrame.getBackground());
                            text_vehicle[lsiv_veh][lsiv_field].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
                            text_vehicle[lsiv_veh][lsiv_field].setForeground(new Color(0, 0, 0));
                        }
                        else {
                            text_vehicle[lsiv_veh][lsiv_field].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
                            text_vehicle[lsiv_veh][lsiv_field].setForeground(new Color(176, 197, 218));
                        }

                        break;

                }
            }
        }
    } // end of method setTextFieldColor

    int calculateNumOfSpecialVehicle() {
        int numOfAdd;

        numOfAdd = 0;

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            if (add_button[lsiv_veh].isEnabled()) {
                numOfAdd++;
            }
        }

        if (numOfAdd == 0) {
            return MAXNUMOFSPECIALVEHICLE;
        }
        else {
            return numOfAdd - 1;
        }
    } // end of method calculateNumOfSpecialVehicle

    void setAccessibility() {
        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        text_vehicle[lsiv_veh][lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]);
                        break;

                    case VEHICLE_CLASS:
                    case DRIVER_CLASS:
                    case DESIRED_SPEED:
                    case DESIRED_O_LEG:
                    case INBOUND_LEG:
                    case INBOUND_LANE:
                    case LOGOUT_SUMMRY:
                    case FREE_UTURN:
                    case FORCEDSTOPIOP:
                    case FORCEDSTOPLIP:
                    case EMERGENCY_VEH:
                        comboBox_vehicle[lsiv_veh][lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]);
                        break;
                }
            }
        }

        cbo_total.getAccessibleContext().setAccessibleName("Total Special Vehicles");
        cbo_total.getAccessibleContext().setAccessibleDescription("Total Special vehicles");

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
            add_button[lsiv_veh].getAccessibleContext().setAccessibleName("Add    for special Vehicle Class " + lsiv_veh);
            add_button[lsiv_veh].getAccessibleContext().setAccessibleDescription("Add    for special Vehicle Class " + lsiv_veh);
            del_button[lsiv_veh].getAccessibleContext().setAccessibleName("Delete for special Vehicle Class " + lsiv_veh);
            del_button[lsiv_veh].getAccessibleContext().setAccessibleDescription("Delete for special Vehicle Class " + lsiv_veh);
            up_button[lsiv_veh].getAccessibleContext().setAccessibleName("Up     for special Vehicle Class " + lsiv_veh);
            up_button[lsiv_veh].getAccessibleContext().setAccessibleDescription("Up     for special Vehicle Class " + lsiv_veh);
            down_button[lsiv_veh].getAccessibleContext().setAccessibleName("Down   for special Vehicle Class " + lsiv_veh);
            down_button[lsiv_veh].getAccessibleContext().setAccessibleDescription("Down   for special Vehicle Class " + lsiv_veh);
        }
    } // end of method setAccessibility

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

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == text_vehicle[lsiv_veh][lsiv_field] || event.getSource() == comboBox_vehicle[lsiv_veh][lsiv_field]) {
                            String location = comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem().toString();
                            int legorpath = Integer.valueOf(comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].getSelectedItem().toString()).intValue();
                            double position = Double.parseDouble(text_vehicle[lsiv_veh][FORCEDSTOPPOS].getText().trim());

                            int minTemp = 0;
                            int maxTemp = 0;
                            int incTemp = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];

                            switch (lsiv_field) {
                                case DRIVER_CLASS:
                                    minTemp = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];
                                    maxTemp = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;
                                    break;
                                case DESIRED_O_LEG:
                                    minTemp = gsiv_min_desired_leg;
                                    maxTemp = gsiv_max_desired_leg;
                                    break;
                                case INBOUND_LEG:
                                    minTemp = gsiv_min_inbound_leg;
                                    maxTemp = gsiv_max_inbound_leg;
                                    break;
                                case FORCEDSTOPLIP:
                                    minTemp = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];
                                    maxTemp = getLegOrPathMaxValue(location);
                                    break;
                                case VEHICLE_CLASS:
                                    minTemp = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];
                                    maxTemp = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
                                    break;
                                case DESIRED_SPEED:
                                case INBOUND_LANE:
                                case LOGOUT_SUMMRY:
                                    minTemp = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];
                                    maxTemp = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];
                                    break;
                            }

                            switch (lsiv_field) {
                                case FORCEDSTOPPOS:
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_veh)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], text_vehicle[lsiv_veh][lsiv_field].getText(),
                                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]), " ",
                                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]),
                                            Double.toString(getPositionMaxValue(location,
                                                    legorpath)),
                                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]));
                                    break;

                                case QUEUE_IN_TIME:
                                case FORCEDSTOPTIM:
                                case FORCEDSTOPDWT:
                                case FORCED_GO_TIM:
                                case FORCED_GO_ACT:
                                case FORCED_RRSTIM:
                                case FORCED_RRSACT:
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_veh)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], text_vehicle[lsiv_veh][lsiv_field].getText(),
                                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]), " ",
                                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]),
                                            Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time * 60.0),
                                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]));
                                    break;

                                case FREE_UTURN:
                                case EMERGENCY_VEH:
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_veh)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem()
                                                    .toString(),
                                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], " ", " ", " ");
                                    break;

                                case FORCEDSTOPIOP:
                                    String LegPathPossibleValue = "|";
                                    int n = comboBox_vehicle[lsiv_veh][lsiv_field].getItemCount();
                                    for (int i = 0; i < n; i++) {
                                        LegPathPossibleValue = LegPathPossibleValue + comboBox_vehicle[lsiv_veh][lsiv_field].getItemAt(i).toString() + "|";
                                    }
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_veh)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem()
                                                    .toString(),
                                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], LegPathPossibleValue, " ", " ", " ");
                                    break;

                                case DRIVER_CLASS:
                                case DESIRED_O_LEG:
                                case INBOUND_LEG:
                                case FORCEDSTOPLIP:
                                case VEHICLE_CLASS:
                                case DESIRED_SPEED:
                                case INBOUND_LANE:
                                case LOGOUT_SUMMRY:
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#",
                                            Integer.toString(lsiv_veh)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field], comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem()
                                                    .toString(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field]), " ",
                                            Integer.toString(minTemp), Integer.toString(maxTemp), Integer.toString(incTemp));
                                    break;

                            } // end of Switch
                        } // end of if(event.getSource() == text_vehicle[lsiv_veh][lsiv_field])
                    } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)

                    if (event.getSource() == del_button[lsiv_veh]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 line for all lines below this line and decreases the Total Lines by 1.", " ", " ", " ", " ", " ",
                                " ", " ");
                        break;
                    }
                    else if (event.getSource() == add_button[lsiv_veh]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 line for all lines below this line, inserts a new line at the current position, copies the values of all parameters from the previous line to the new line, and increases the Total Lines by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                        break;
                    }
                    else if (event.getSource() == up_button[lsiv_veh]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous line and the current line.", " ", " ", " ", " ", " ", " ", " ");
                        break;
                    }
                    else if (event.getSource() == down_button[lsiv_veh]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next line and the current line.", " ", " ", " ", " ", " ", " ", " ");
                        break;
                    }
                } // end of for ( int lsiv_veh = 1 ; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++
                  // )

                if (event.getSource() == cbo_total) {
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the " + label_total.getText()
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSV), "1");
                }
                else if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == applyButton) {
                    new HelpDialog(true, "Apply button", "The Apply button saves the data but does not close the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }
    } // end of class HelpListener

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_veh = sum; lsiv_veh >= index; lsiv_veh--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        text_vehicle[lsiv_veh + 1][lsiv_field].setText(text_vehicle[lsiv_veh][lsiv_field].getText());
                        break;

                    case VEHICLE_CLASS:
                    case DRIVER_CLASS:
                    case DESIRED_SPEED:
                    case DESIRED_O_LEG:
                    case INBOUND_LEG:
                    case INBOUND_LANE:
                    case LOGOUT_SUMMRY:
                    case FREE_UTURN:
                    case FORCEDSTOPIOP:
                    case FORCEDSTOPLIP:
                    case EMERGENCY_VEH:
                        comboBox_vehicle[lsiv_veh + 1][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem().toString());
                        break;
                }
            }
        }
    } // end of method setValueAfterAdd

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_veh = index; lsiv_veh < sum; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:
                        text_vehicle[lsiv_veh][lsiv_field].setText(text_vehicle[lsiv_veh + 1][lsiv_field].getText());
                        break;

                    case VEHICLE_CLASS:
                    case DRIVER_CLASS:
                    case DESIRED_SPEED:
                    case DESIRED_O_LEG:
                    case INBOUND_LEG:
                    case INBOUND_LANE:
                    case LOGOUT_SUMMRY:
                    case FREE_UTURN:
                    case FORCEDSTOPIOP:
                    case FORCEDSTOPLIP:
                    case EMERGENCY_VEH:
                        comboBox_vehicle[lsiv_veh][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh + 1][lsiv_field].getSelectedItem().toString());
                        break;
                }
            }
        }

        comboBox_vehicle[sum][VEHICLE_CLASS].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS]));
        comboBox_vehicle[sum][DRIVER_CLASS].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS]));
        comboBox_vehicle[sum][DESIRED_SPEED].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED]));
        comboBox_vehicle[sum][DESIRED_O_LEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG]));
        comboBox_vehicle[sum][INBOUND_LEG].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG]));
        comboBox_vehicle[sum][INBOUND_LANE].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE]));
        comboBox_vehicle[sum][LOGOUT_SUMMRY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY]));
        comboBox_vehicle[sum][FREE_UTURN].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN]);
        comboBox_vehicle[sum][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
        comboBox_vehicle[sum][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
        comboBox_vehicle[sum][EMERGENCY_VEH].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH]);

        text_vehicle[sum][QUEUE_IN_TIME].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME]));
        text_vehicle[sum][FORCEDSTOPTIM].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM]));
        text_vehicle[sum][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
        text_vehicle[sum][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));
        text_vehicle[sum][FORCED_GO_TIM].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM]));
        text_vehicle[sum][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
        text_vehicle[sum][FORCED_RRSTIM].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM]));
        text_vehicle[sum][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));

    } // end of method setValueAfterDel

    void AddAction(int lsiv_veh) {
        int sumOfVehiclesBeforeClick;
        sumOfVehiclesBeforeClick = calculateNumOfSpecialVehicle();
        setStatus(sumOfVehiclesBeforeClick + 1);
        setValueAfterAdd(sumOfVehiclesBeforeClick, lsiv_veh);
        setStatus(calculateNumOfSpecialVehicle());
        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(calculateNumOfSpecialVehicle()));

    } // end of method AddAction

    void DelAction(int lsiv_veh) {
        int sumOfVehiclesBeforeClick;
        sumOfVehiclesBeforeClick = calculateNumOfSpecialVehicle();
        setValueAfterDel(sumOfVehiclesBeforeClick, lsiv_veh);
        setStatus(sumOfVehiclesBeforeClick - 1);
        cbo_total.removeAllItems();
        cbo_total.addItem(Integer.toString(calculateNumOfSpecialVehicle()));
        if (!del_button[lsiv_veh].isEnabled()) {
            okButton.requestFocus();
        }
    } // end of method DelAction

    void UpAction(int lsiv_veh) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String temp;

            switch (lsiv_field) {
                case QUEUE_IN_TIME:
                case FORCEDSTOPTIM:
                case FORCEDSTOPPOS:
                case FORCEDSTOPDWT:
                case FORCED_GO_TIM:
                case FORCED_GO_ACT:
                case FORCED_RRSTIM:
                case FORCED_RRSACT:
                    temp = text_vehicle[lsiv_veh][lsiv_field].getText();
                    text_vehicle[lsiv_veh][lsiv_field].setText(text_vehicle[lsiv_veh - 1][lsiv_field].getText());
                    text_vehicle[lsiv_veh - 1][lsiv_field].setText(temp);
                    break;

                case VEHICLE_CLASS:
                case DRIVER_CLASS:
                case DESIRED_SPEED:
                case DESIRED_O_LEG:
                case INBOUND_LEG:
                case INBOUND_LANE:
                case LOGOUT_SUMMRY:
                case FREE_UTURN:
                case FORCEDSTOPIOP:
                case FORCEDSTOPLIP:
                case EMERGENCY_VEH:
                    temp = comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem().toString();
                    comboBox_vehicle[lsiv_veh][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh - 1][lsiv_field].getSelectedItem().toString());
                    comboBox_vehicle[lsiv_veh - 1][lsiv_field].setSelectedItem(temp);
                    break;
            }
        }
        setStatus(calculateNumOfSpecialVehicle());

    } // end of method UpAction

    void DownAction(int lsiv_veh) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String temp;
            switch (lsiv_field) {
                case QUEUE_IN_TIME:
                case FORCEDSTOPTIM:
                case FORCEDSTOPPOS:
                case FORCEDSTOPDWT:
                case FORCED_GO_TIM:
                case FORCED_GO_ACT:
                case FORCED_RRSTIM:
                case FORCED_RRSACT:
                    temp = text_vehicle[lsiv_veh][lsiv_field].getText();
                    text_vehicle[lsiv_veh][lsiv_field].setText(text_vehicle[lsiv_veh + 1][lsiv_field].getText());
                    text_vehicle[lsiv_veh + 1][lsiv_field].setText(temp);
                    break;

                case VEHICLE_CLASS:
                case DRIVER_CLASS:
                case DESIRED_SPEED:
                case DESIRED_O_LEG:
                case INBOUND_LEG:
                case INBOUND_LANE:
                case LOGOUT_SUMMRY:
                case FREE_UTURN:
                case FORCEDSTOPIOP:
                case FORCEDSTOPLIP:
                case EMERGENCY_VEH:
                    temp = comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem().toString();
                    comboBox_vehicle[lsiv_veh][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh + 1][lsiv_field].getSelectedItem().toString());
                    comboBox_vehicle[lsiv_veh + 1][lsiv_field].setSelectedItem(temp);
                    break;
            }
        }
        setStatus(calculateNumOfSpecialVehicle());

    } // end of method DownAction

    class TextFieldFocusListener implements FocusListener {

        public void focusLost(FocusEvent event) {
            number_of_specialVehicle = calculateNumOfSpecialVehicle();

            for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case QUEUE_IN_TIME:
                        case FORCEDSTOPTIM:
                        case FORCEDSTOPPOS:
                        case FORCEDSTOPDWT:
                        case FORCED_GO_TIM:
                        case FORCED_GO_ACT:
                        case FORCED_RRSTIM:
                        case FORCED_RRSACT:

                            if (event.getSource() == text_vehicle[lsiv_veh][lsiv_field]) {
                                text_vehicle[lsiv_veh][lsiv_field].setBackground(aFrame.getBackground());
                            }
                    }
                }
            }
        }

        public void focusGained(FocusEvent event) {
            number_of_specialVehicle = calculateNumOfSpecialVehicle();

            for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    switch (lsiv_field) {
                        case QUEUE_IN_TIME:
                        case FORCEDSTOPTIM:
                        case FORCEDSTOPPOS:
                        case FORCEDSTOPDWT:
                        case FORCED_GO_TIM:
                        case FORCED_GO_ACT:
                        case FORCED_RRSTIM:
                        case FORCED_RRSACT:

                            if (event.getSource() == text_vehicle[lsiv_veh][lsiv_field]) {
                                text_vehicle[lsiv_veh][lsiv_field].setBackground(new Color(176, 197, 218));
                            }
                    }
                }
            }
        }
    } // end of class TextFieldFocusListener

    void setStatusAfterSetFORCEDSTOPTIM(int lsiv_veh) {
        if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() == 0) {
            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]));
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]));
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));

            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(false);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(false);
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(false);

            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setForeground(new Color(176, 197, 218));

            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setForeground(new Color(176, 197, 218));
        }
        else {
            comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].setEnabled(true);
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setEditable(true);
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setEditable(true);

            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBackground(aFrame.getBackground());
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
            text_vehicle[lsiv_veh][FORCEDSTOPPOS].setForeground(new Color(0, 0, 0));

            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBackground(aFrame.getBackground());
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
            text_vehicle[lsiv_veh][FORCEDSTOPDWT].setForeground(new Color(0, 0, 0));
        }
    } // end of method setStatusAfterSetFORCEDSTOPTIM

    void setStatusAfterSetFORCED_GO_TIM(int lsiv_veh) {
        if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim()).doubleValue() == 0) {
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]));
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(false);

            text_vehicle[lsiv_veh][FORCED_GO_ACT].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setForeground(new Color(176, 197, 218));
        }
        else {
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setEditable(true);

            text_vehicle[lsiv_veh][FORCED_GO_ACT].setBackground(aFrame.getBackground());
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
            text_vehicle[lsiv_veh][FORCED_GO_ACT].setForeground(new Color(0, 0, 0));
        }
    } // end of method setStatusAfterSetFORCED_GO_TIM

    void setStatusAfterSetFORCED_RRSTIM(int lsiv_veh) {
        if (Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim()).doubleValue() == 0) {
            text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(false);
            text_vehicle[lsiv_veh][FORCED_RRSACT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]));

            text_vehicle[lsiv_veh][FORCED_RRSACT].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
            text_vehicle[lsiv_veh][FORCED_RRSACT].setForeground(new Color(176, 197, 218));
        }
        else {
            text_vehicle[lsiv_veh][FORCED_RRSACT].setEditable(true);

            text_vehicle[lsiv_veh][FORCED_RRSACT].setBackground(aFrame.getBackground());
            text_vehicle[lsiv_veh][FORCED_RRSACT].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
            text_vehicle[lsiv_veh][FORCED_RRSACT].setForeground(new Color(0, 0, 0));
        }

    } // end of method setStatusAfterSetFORCED_RRSTIM

    void LocationAction(int lsiv_veh) {
        if (comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem().toString().equals("R") || comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem().toString().equals("L")) {
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setSelectedItem("1");
            comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(false);
        }
        else {
            if (comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].isEnabled()) {
                comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].setEnabled(true);
            }
        }
        setTextFieldColor();

    } // end of method LocationAction

    void ClassificationAction(int lsiv_veh) {
        int veh_class;
        String veh_name;
        VehicleName[lsiv_veh].removeAllItems();
        veh_class = Integer.valueOf(comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].getSelectedItem().toString()).intValue();
        veh_name = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[veh_class];
        VehicleIcon[lsiv_veh].setIcon(getImageIcon(veh_name));
        VehicleName[lsiv_veh].addItem(veh_name);
    } // end of method LocationAction

    int getLegOrPathMaxValue(String location) {
        if (location.equals("I")) {
            return gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }
        else if (location.equals("L")) {
            return 1;
        }
        else if (location.equals("O")) {
            return gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }
        else if (location.equals("P")) {
            return PARAMS.TEXAS_MODEL_NPA;
        }
        else if (location.equals("R")) {
            return 1;
        }

        return gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

    } // end of method getLegOrPathMaxValue

    double getPositionMaxValue(String location, int LegNum) {
        if (location.equals("I")) {
            return gdvsim.gclv_inter.mcla_leg[LegNum].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
        }
        else if (location.equals("L")) {
            return gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
        }
        else if (location.equals("O")) {
            return gdvsim.gclv_inter.mcla_leg[LegNum].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
        }
        else if (location.equals("P")) {
            return PARAMS.TEXAS_MODEL_POSMAX;
        }
        else if (location.equals("R")) {
            return gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
        }

        return gdvsim.gclv_inter.mcla_leg[LegNum].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;

    } // end of method getPositionMaxValue

    void saveData() {
        gdvsim.gclv_inter.msiv_specialVehicles = calculateNumOfSpecialVehicle();

        for (int lsiv_veh = 1; lsiv_veh <= gdvsim.gclv_inter.msiv_specialVehicles; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_queue_in_time = Double.valueOf(text_vehicle[lsiv_veh][QUEUE_IN_TIME].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_vehicle_class = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][VEHICLE_CLASS].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_driver_class = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][DRIVER_CLASS].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_desired_speed = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][DESIRED_SPEED].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_desired_outbound_leg = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][DESIRED_O_LEG].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_inbound_leg = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][INBOUND_LEG].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_inbound_lane = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][INBOUND_LANE].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_vehicle_logout_summary = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][LOGOUT_SUMMRY].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_free_uturn_use = comboBox_vehicle[lsiv_veh][FREE_UTURN].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_time = Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_emergency_vehicle = comboBox_vehicle[lsiv_veh][EMERGENCY_VEH].getSelectedItem().toString();

            if (comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_forced_stop_inb_out_path = comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem()
                        .toString();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mstv_forced_stop_inb_out_path = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP];
            }

            if (comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_forced_stop_leg_or_path = Integer.valueOf(
                        comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].getSelectedItem().toString()).intValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].msiv_forced_stop_leg_or_path = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];
            }

            if (text_vehicle[lsiv_veh][FORCEDSTOPPOS].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_position = Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPPOS].getText().trim())
                        .doubleValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_position = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS];
            }

            if (text_vehicle[lsiv_veh][FORCEDSTOPDWT].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_stop_dwell_time = Double.valueOf(
                        text_vehicle[lsiv_veh][FORCEDSTOPDWT].getText().trim()).doubleValue();
            }
            else {
                text_vehicle[lsiv_veh][FORCEDSTOPDWT].setText(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]));
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_go_time = Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_TIM].getText().trim())
                    .doubleValue();

            if (text_vehicle[lsiv_veh][FORCED_GO_ACT].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_go_active_time = Double
                        .valueOf(text_vehicle[lsiv_veh][FORCED_GO_ACT].getText().trim()).doubleValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_go_active_time = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT];
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_run_red_sig_time = Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSTIM].getText().trim())
                    .doubleValue();

            if (text_vehicle[lsiv_veh][FORCED_RRSACT].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_run_red_sig_acttim = Double.valueOf(
                        text_vehicle[lsiv_veh][FORCED_RRSACT].getText().trim()).doubleValue();
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mdfv_forced_run_red_sig_acttim = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT];
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH] = gdvsim.gclv_inter.TX_FROM_USER;

            if (comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            if (comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].isEnabled()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            if (text_vehicle[lsiv_veh][FORCEDSTOPPOS].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            if (text_vehicle[lsiv_veh][FORCEDSTOPDWT].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = gdvsim.gclv_inter.TX_FROM_USER;

            if (text_vehicle[lsiv_veh][FORCED_GO_ACT].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = gdvsim.gclv_inter.TX_FROM_USER;

            if (text_vehicle[lsiv_veh][FORCED_RRSACT].isEditable()) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        } // end for lsiv_veh

        for (int lsiv_veh = gdvsim.gclv_inter.msiv_specialVehicles + 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NSV; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        } // end for lsiv_veh
    } // end of method saveData()

    boolean isErrorInNumber(int veh, int field) {
        number_of_specialVehicle = calculateNumOfSpecialVehicle();

        for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_veh == veh && lsiv_field == field) {
                    String fld_name;

                    switch (lsiv_field) {
                        case QUEUE_IN_TIME:
                        case FORCEDSTOPTIM:
                        case FORCEDSTOPPOS:
                        case FORCEDSTOPDWT:
                        case FORCED_GO_TIM:
                        case FORCED_GO_ACT:
                        case FORCED_RRSTIM:
                        case FORCED_RRSACT:

                            fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];

                            if (text_vehicle[lsiv_veh][lsiv_field].isEnabled()) {
                                if (text_vehicle[lsiv_veh][lsiv_field].getText().trim().length() == 0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }

                                double ldfv_value;

                                try {
                                    ldfv_value = Double.parseDouble(text_vehicle[lsiv_veh][lsiv_field].getText().trim());
                                }
                                catch (Exception e) {

                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = '" + text_vehicle[lsiv_veh][lsiv_field].getText().trim()
                                            + "' contains an illegal character for a decimal number.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }

                    } // end of switch ( lsiv_field )
                } // end of if(lsiv_veh == veh && lsiv_field == field)
            } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)
        } // end of for(int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++)

        return false;

    } // end of method isErrorInNumber

    boolean isError() {
        number_of_specialVehicle = calculateNumOfSpecialVehicle();
        double maxTime = (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time * 60.0) + 0.0001;
        String fld_name;
        double ldfa_value[] = new double[NUMOFFIELD + 1];
        int lsiv_field;

        for (lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            ldfa_value[lsiv_field] = 0.0;
        }

        for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
            for (lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME - 1 + lsiv_field];

                switch (lsiv_field) {
                    case QUEUE_IN_TIME:
                    case FORCEDSTOPTIM:
                    case FORCEDSTOPPOS:
                    case FORCEDSTOPDWT:
                    case FORCED_GO_TIM:
                    case FORCED_GO_ACT:
                    case FORCED_RRSTIM:
                    case FORCED_RRSACT:

                        if (!text_vehicle[lsiv_veh][lsiv_field].isEnabled())
                            continue;
                        if (text_vehicle[lsiv_veh][lsiv_field].getText().trim().length() == 0) {
                            JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }

                        try {
                            ldfa_value[lsiv_field] = Double.parseDouble(text_vehicle[lsiv_veh][lsiv_field].getText().trim());
                        }
                        catch (Exception e) {
                            JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = '" + text_vehicle[lsiv_veh][lsiv_field].getText().trim()
                                    + "' contains an illegal character for a decimal number.", "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }

                        if (lsiv_veh > 1 && lsiv_field == QUEUE_IN_TIME) {
                            double preQIT = Double.parseDouble(text_vehicle[lsiv_veh - 1][lsiv_field].getText().trim());

                            if (ldfa_value[QUEUE_IN_TIME] < preQIT) {
                                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + ldfa_value[QUEUE_IN_TIME]
                                        + " should be greater than or equal to Special Vehicle " + (lsiv_veh - 1) + " " + fld_name + " = " + preQIT + " seconds.", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + ldfa_value[QUEUE_IN_TIME] + " Minimum Headway is invalid.",
                                        "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            if ((ldfa_value[QUEUE_IN_TIME] - preQIT) < gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_min_hw) {
                                if ((Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LEG].getSelectedItem().toString()).intValue() == Integer.valueOf(
                                        comboBox_vehicle[lsiv_veh - 1][INBOUND_LEG].getSelectedItem().toString()).intValue())
                                        && (Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LANE].getSelectedItem().toString()).intValue() == Integer.valueOf(
                                                comboBox_vehicle[lsiv_veh - 1][INBOUND_LANE].getSelectedItem().toString()).intValue())) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + ldfa_value[QUEUE_IN_TIME] + " is within "
                                            + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_min_hw + " seconds of Special Vehicle " + (lsiv_veh - 1) + " Queue-in Time = "
                                            + preQIT + " on the same Inbound Leg and Lane.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                        }

                        switch (lsiv_field) {
                            case QUEUE_IN_TIME:
                            case FORCEDSTOPTIM:
                            case FORCED_GO_TIM:
                            case FORCED_RRSTIM:
                                if (ldfa_value[lsiv_field] < 0.0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + ldfa_value[lsiv_field]
                                            + " should be greater than or equal to the minimum value = 0.0 seconds.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (ldfa_value[lsiv_field] > maxTime) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + ldfa_value[lsiv_field]
                                            + " should be less than or equal to the maxmimum value = " + twoDigits.format(maxTime) + " seconds.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                if ((lsiv_field > QUEUE_IN_TIME) && (ldfa_value[lsiv_field] > 0.0) && (ldfa_value[lsiv_field] < ldfa_value[QUEUE_IN_TIME])) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + ldfa_value[lsiv_field]
                                            + " should be greater than or equal to Queue-in Time = " + ldfa_value[QUEUE_IN_TIME] + " seconds.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                break;
                        }

                        if (ldfa_value[FORCEDSTOPTIM] > 0.0 && lsiv_field == FORCEDSTOPPOS) {
                            double FSPOS = Double.parseDouble(text_vehicle[lsiv_veh][FORCEDSTOPPOS].getText().trim());
                            String FSIOP = comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem().toString();
                            int FSLIP = Integer.valueOf(comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].getSelectedItem().toString()).intValue();

                            if (FSIOP.equals("I")) {
                                if (gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_INB] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " Length of Inbound Lanes is invalid.", "Error Message",
                                            JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                int LIL = gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
                                if (FSPOS < 0.0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS
                                            + " should be greater than or equal to the minimum value = 0.0 feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSPOS > LIL) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " should be less than or equal to the maxmimum value = " + LIL
                                            + " feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                            else if (FSIOP.equals("L")) {
                                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " Distance Between Diamond Intersections is invalid.",
                                            "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                int LOI = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
                                if (FSPOS < 0.0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS
                                            + " should be greater than or equal to the minimum value = 0.0 feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSPOS > LOI) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " should be less than or equal to the maxmimum value = " + LOI
                                            + " feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                            else if (FSIOP.equals("O")) {
                                if (gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_OUT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " Length of Outbound Lanes is invalid.", "Error Message",
                                            JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                int LOL = gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
                                if (FSPOS < 0.0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS
                                            + " should be greater than or equal to the minimum value = 0.0 feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSPOS > LOL) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " should be less than or equal to the maxmimum value = " + LOL
                                            + " feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                            else if (FSIOP.equals("P")) {
                                int LOP = PARAMS.TEXAS_MODEL_POSMAX;
                                if (FSPOS < 0.0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS
                                            + " should be greater than or equal to the minimum value = 0.0 feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSPOS > LOP) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " should be less than or equal to the maxmimum value = " + LOP
                                            + " feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                            else if (FSIOP.equals("R")) {
                                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " Distance Between Diamond Intersections is invalid.",
                                            "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                int LOI = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
                                if (FSPOS < 0.0) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS
                                            + " should be greater than or equal to the minimum value = 0.0 feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSPOS > LOI) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSPOS + " should be less than or equal to the maxmimum value = " + LOI
                                            + " feet.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                        }

                        if (ldfa_value[FORCEDSTOPTIM] > 0.0 && lsiv_field == FORCEDSTOPDWT) {
                            double FDT = Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPDWT].getText()).doubleValue();
                            double max = maxTime - ldfa_value[FORCEDSTOPTIM];
                            if (FDT < 0.0) {
                                JOptionPane.showMessageDialog(null,
                                        "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FDT + " should be greater than or equal to the minimum value = 0.0 seconds.",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            else if (FDT > max) {
                                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FDT + " should be less than or equal to the maxmimum value = " + max
                                        + " seconds.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }

                        if (ldfa_value[FORCED_GO_TIM] > 0.0 && lsiv_field == FORCED_GO_ACT) {
                            double FGA = Double.valueOf(text_vehicle[lsiv_veh][FORCED_GO_ACT].getText()).doubleValue();
                            double max = maxTime - ldfa_value[FORCED_GO_TIM];
                            if (FGA < 0.0) {
                                JOptionPane.showMessageDialog(null,
                                        "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FGA + " should be greater than or equal to the minimum value = 0.0 seconds.",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            else if (FGA > max) {
                                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FGA + " should be less than or equal to the maxmimum value = " + max
                                        + " seconds.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }

                        if (ldfa_value[FORCED_RRSTIM] > 0.0 && lsiv_field == FORCED_RRSACT) {
                            double FRA = Double.valueOf(text_vehicle[lsiv_veh][FORCED_RRSACT].getText()).doubleValue();
                            double max = maxTime - ldfa_value[FORCED_RRSTIM];
                            if (FRA < 0.0) {
                                JOptionPane.showMessageDialog(null,
                                        "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FRA + " should be greater than or equal to the minimum value = 0.0 seconds.",
                                        "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            else if (FRA > max) {
                                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FRA + " should be less than or equal to the maxmimum value = " + max
                                        + " seconds.", "Error Message", JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                        }
                        break;

                    case INBOUND_LANE:
                        int InbLeg = Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LEG].getSelectedItem().toString()).intValue();
                        int InbLane = Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LANE].getSelectedItem().toString()).intValue();

                        if (gdvsim.gclv_inter.mcla_leg[InbLeg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + InbLane + " Number of Inbound Lanes is invalid.", "Error Message",
                                    JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                        if (InbLane > gdvsim.gclv_inter.mcla_leg[InbLeg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb) {
                            JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + InbLane + " is greater than Number of Inbound Lanes = "
                                    + gdvsim.gclv_inter.mcla_leg[InbLeg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " for Inbound Leg " + InbLeg + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                            return true;
                        }
                        break;

                    case FREE_UTURN:
                        if (!comboBox_vehicle[lsiv_veh][lsiv_field].isEnabled())
                            continue;
                        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                            String freeUTurn = comboBox_vehicle[lsiv_veh][FREE_UTURN].getSelectedItem().toString();
                            int inboundLeg = Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LEG].getSelectedItem().toString()).intValue();
                            if (freeUTurn.equals("F")) {
                                switch (inboundLeg) {
                                    case 3:
                                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width == 0) {
                                            JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = '" + freeUTurn
                                                    + "' when there is no free u-turn lane defined for Inbound Leg " + inboundLeg + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                            return true;
                                        }
                                        break;

                                    case 6:
                                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width == 0) {
                                            JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = '" + freeUTurn
                                                    + "' when there is no free u-turn lane defined for Inbound Leg " + inboundLeg + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                            return true;
                                        }
                                        break;

                                    default:
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = '" + freeUTurn
                                                + "' when there is no free u-turn lane possible for Inbound Leg " + inboundLeg + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                        return true;
                                }
                            }
                        }
                        break;

                    case FORCEDSTOPLIP:
                        if (!comboBox_vehicle[lsiv_veh][lsiv_field].isEnabled())
                            continue;
                        if (ldfa_value[FORCEDSTOPTIM] > 0.0) {
                            String FSIOP = comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem().toString();
                            int FSLIP = Integer.valueOf(comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].getSelectedItem().toString()).intValue();

                            if (FSIOP.equals("I") || FSIOP.equals("O")) {
                                int max;
                                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                                    max = 6;
                                }
                                else {
                                    max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                                }
                                if (FSLIP < 1) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " should be greater than or equal to the minimum value = 1.",
                                            "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSLIP > max) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " should be less than or equal to the maxmimum value = " + max
                                            + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                if (FSIOP.equals("I")) {
                                    int inbLegNum = Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LEG].getSelectedItem().toString()).intValue();
                                    if (FSLIP != inbLegNum) {
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " can only be Inbound Leg = " + inbLegNum + ".",
                                                "Error Message", JOptionPane.ERROR_MESSAGE);
                                        return true;
                                    }
                                    if (gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " Number of Inbound Lanes is invalid.", "Error Message",
                                                JOptionPane.ERROR_MESSAGE);
                                        return true;
                                    }
                                    if (gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb == 0) {
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " Number of Inbound Lanes is zero.", "Error Message",
                                                JOptionPane.ERROR_MESSAGE);
                                        return true;
                                    }
                                }
                                else if (FSIOP.equals("O")) {
                                    int outLegNum = Integer.valueOf(comboBox_vehicle[lsiv_veh][DESIRED_O_LEG].getSelectedItem().toString()).intValue();
                                    if (FSLIP != outLegNum) {
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " can only be Desired Outbound Leg = " + outLegNum + ".",
                                                "Error Message", JOptionPane.ERROR_MESSAGE);
                                        return true;
                                    }
                                    if (gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " Number of Outbound Lanes is invalid.", "Error Message",
                                                JOptionPane.ERROR_MESSAGE);
                                        return true;
                                    }
                                    if (gdvsim.gclv_inter.mcla_leg[FSLIP].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                                        JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " Number of Outbound Lanes is zero.", "Error Message",
                                                JOptionPane.ERROR_MESSAGE);
                                        return true;
                                    }
                                }
                            }
                            else if (FSIOP.equals("L") || FSIOP.equals("R")) {
                                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " should not be specified.", "Error Message",
                                        JOptionPane.ERROR_MESSAGE);
                                return true;
                            }
                            else if (FSIOP.equals("P")) {
                                int max = PARAMS.TEXAS_MODEL_NPA;
                                if (FSLIP < 1) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " should be greater than or equal to the minimum value = 1.",
                                            "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                                else if (FSLIP > max) {
                                    JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + FSLIP + " should be less than or equal to the maxmimum value = " + max
                                            + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                                    return true;
                                }
                            }
                        }
                        break;
                } // end of switch ( lsiv_field )
            } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)
        } // end of for(int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++)

        for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
            if (Double.valueOf(text_vehicle[lsiv_veh][FORCEDSTOPTIM].getText().trim()).doubleValue() > 0) {
                String location = comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP].getSelectedItem().toString();
                int legorpath = Integer.valueOf(comboBox_vehicle[lsiv_veh][FORCEDSTOPLIP].getSelectedItem().toString()).intValue();
                double position = Double.parseDouble(text_vehicle[lsiv_veh][FORCEDSTOPPOS].getText().trim());

                fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];

                if (legorpath > getLegOrPathMaxValue(location)) {
                    JOptionPane.showMessageDialog(null,
                            "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + legorpath + " should be less than or equal to " + getLegOrPathMaxValue(location) + ".", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                    return true;
                }

                fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS];

                if (position > getPositionMaxValue(location, legorpath)) {
                    JOptionPane.showMessageDialog(null,
                            "Special Vehicle " + lsiv_veh + " " + fld_name + " = " + position + " should be less than or equal to " + getPositionMaxValue(location, legorpath) + ".", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        }

        for (int lsiv_veh = 1; lsiv_veh <= number_of_specialVehicle; lsiv_veh++) {
            int legNum = Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LEG].getSelectedItem().toString()).intValue();
            int laneNum = Integer.valueOf(comboBox_vehicle[lsiv_veh][INBOUND_LANE].getSelectedItem().toString()).intValue();

            if (laneNum > gdvsim.gclv_inter.mcla_leg[legNum].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb) {
                JOptionPane.showMessageDialog(null, "Special Vehicle " + lsiv_veh + " Inbound Leg " + legNum + " Inbound Lane = " + laneNum + " should be less than or equal to "
                        + gdvsim.gclv_inter.mcla_leg[legNum].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        return false;

    } // end of method isError

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_specialVehicle_ok = true;
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                if (event.getSource() == add_button[lsiv_veh]) {
                    AddAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == del_button[lsiv_veh]) {
                    DelAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == up_button[lsiv_veh]) {
                    UpAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == down_button[lsiv_veh]) {
                    DownAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCEDSTOPTIM]) {
                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPTIM))
                        return;
                    setStatusAfterSetFORCEDSTOPTIM(lsiv_veh);
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_GO_TIM]) {
                    if (isErrorInNumber(lsiv_veh, FORCED_GO_TIM))
                        return;
                    setStatusAfterSetFORCED_GO_TIM(lsiv_veh);
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_RRSTIM]) {
                    if (isErrorInNumber(lsiv_veh, FORCED_RRSTIM))
                        return;
                    setStatusAfterSetFORCED_RRSTIM(lsiv_veh);
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][QUEUE_IN_TIME]) {
                    if (isErrorInNumber(lsiv_veh, QUEUE_IN_TIME))
                        return;
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCEDSTOPPOS]) {
                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPPOS))
                        return;
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCEDSTOPDWT]) {
                    if (isErrorInNumber(lsiv_veh, FORCEDSTOPDWT))
                        return;
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_GO_ACT]) {
                    if (isErrorInNumber(lsiv_veh, FORCED_GO_ACT))
                        return;
                    break;
                }
                else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_RRSACT]) {
                    if (isErrorInNumber(lsiv_veh, FORCED_RRSACT))
                        return;
                    break;
                }
                else if (event.getSource() == comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP]) {
                    LocationAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == comboBox_vehicle[lsiv_veh][VEHICLE_CLASS]) {
                    ClassificationAction(lsiv_veh);
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
                        gdvsim.flag_specialVehicle_ok = true;

                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_veh = 1; lsiv_veh <= MAXNUMOFSPECIALVEHICLE; lsiv_veh++) {
                    if (event.getSource() == add_button[lsiv_veh]) {
                        AddAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == del_button[lsiv_veh]) {
                        DelAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == up_button[lsiv_veh]) {
                        UpAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == down_button[lsiv_veh]) {
                        DownAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCEDSTOPTIM]) {
                        if (isErrorInNumber(lsiv_veh, FORCEDSTOPTIM))
                            return;
                        setStatusAfterSetFORCEDSTOPTIM(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_GO_TIM]) {
                        if (isErrorInNumber(lsiv_veh, FORCED_GO_TIM))
                            return;
                        setStatusAfterSetFORCED_GO_TIM(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_RRSTIM]) {
                        if (isErrorInNumber(lsiv_veh, FORCED_RRSTIM))
                            return;
                        setStatusAfterSetFORCED_RRSTIM(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][QUEUE_IN_TIME]) {
                        if (isErrorInNumber(lsiv_veh, QUEUE_IN_TIME))
                            return;
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCEDSTOPPOS]) {
                        if (isErrorInNumber(lsiv_veh, FORCEDSTOPPOS))
                            return;
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCEDSTOPDWT]) {
                        if (isErrorInNumber(lsiv_veh, FORCEDSTOPDWT))
                            return;
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_GO_ACT]) {
                        if (isErrorInNumber(lsiv_veh, FORCED_GO_ACT))
                            return;
                        break;
                    }
                    else if (event.getSource() == text_vehicle[lsiv_veh][FORCED_RRSACT]) {
                        if (isErrorInNumber(lsiv_veh, FORCED_RRSACT))
                            return;
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][FORCEDSTOPIOP]) {
                        LocationAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][VEHICLE_CLASS]) {
                        ClassificationAction(lsiv_veh);
                        break;
                    }
                }
            }
        } // end of keyPressed
    } // end of ComponentKeyListener

} // end of class SpecialVehicleDialog
