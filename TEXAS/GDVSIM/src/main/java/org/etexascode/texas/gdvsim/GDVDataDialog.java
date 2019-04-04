package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           GDVDataDialog.java                               */
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
import org.etexascode.texas.gdvsim.Intersection;
import org.etexascode.texas.gdvsim.LineDialog;
import org.etexascode.texas.gdvsim.PARAMS;
import org.etexascode.texas.gdvsim.PlotDialog;
import org.etexascode.texas.gdvsim.SpecialVehicleDialog;
import org.etexascode.texas.gdvsim.TX_Fmt;
import org.etexascode.texas.gdvsim.VehicleClassDialog;
import org.etexascode.texas.gdvsim.gdvsim;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;

public class GDVDataDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int OPT1FIELD = 7;

    int OPT2FIELD = 3;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JButton okButton, applyButton, cancelButton, freeUturnButton, internalLegButton, arcButton, lineButton, plotButton, logoutVehicleButton, logoutDriverButton, specialVehicleButton;

    JLabel label_title_text, label_title_opt_1, label_title_opt_2, label_note;

    JLabel[] label_opt_1 = new JLabel[OPT1FIELD + 1];

    JLabel[] label_opt_2 = new JLabel[OPT2FIELD + 1];

    JComboBox[] comboBox_opt_1 = new JComboBox[OPT1FIELD + 1];

    JComboBox[] comboBox_opt_2 = new JComboBox[OPT2FIELD + 1];

    JButton[] button_opt_2 = new JButton[OPT2FIELD + 1];

    JCheckBox acceptDefaultCheckbox;

    JTextField gdvTitleText;

    Font font1, font2;

    TX_Fmt lclv_tx_fmt;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    public GDVDataDialog() {
        aFrame = new JFrame();

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            aFrame.setTitle("Diamond GDV Data");
        }
        else {
            aFrame.setTitle("GDV Data");
        }

        JPanel wholePanel = new JPanel();
        container = aFrame.getContentPane();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        container.setLayout(gbLayout);
        gbConstraints.fill = GridBagConstraints.BOTH;

        font1 = new Font("TimesRoman", Font.PLAIN, 18);
        font2 = new Font("TimesRoman", Font.BOLD, 14);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TITLE];

        label_title_text = new JLabel(lclv_tx_fmt.mstv_name + " (" + lclv_tx_fmt.msia_fs[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT] + " max)");

        label_title_text.setFont(font1);
        gdvTitleText = new JTextField(lclv_tx_fmt.msia_fs[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT]);

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card != null) {
            gdvTitleText.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card.trim());
        }

        int SizeOfArray;
        int intArrayElementValue;
        int ArrayIndex;
        double doubleArrayElementValue;

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;
        int lsiv_def;

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            lsiv_min = 6;
            lsiv_max = 6;
            lsiv_inc = 1;
            lsiv_def = 6;
        }
        else {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
            lsiv_def = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
        }

        if (gdvsim.gclv_inter.mbov_GDV_Data_OK) {
            lsiv_min = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
            lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
            lsiv_inc = 1;
            lsiv_def = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_par_opt_no_legs = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_no_legs[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        String[] array_gdv_par_opt_time = new String[SizeOfArray];
        intArrayElementValue = lsiv_min;
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_time[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        SizeOfArray = (int)((lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW] - lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW])
                / lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW]) + 1;
        doubleArrayElementValue = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW];
        String[] array_gdv_par_opt_min_hw = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_min_hw[ArrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW];
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_par_opt_no_veh_cl = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_no_veh_cl[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_par_opt_no_drv_cl = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_no_drv_cl[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_par_opt_pct_lt = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_pct_lt[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_par_opt_pct_rt = new String[SizeOfArray];
        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_par_opt_pct_rt[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        comboBox_opt_1[1] = new JComboBox(array_gdv_par_opt_no_legs);
        comboBox_opt_1[2] = new JComboBox(array_gdv_par_opt_time);
        comboBox_opt_1[3] = new JComboBox(array_gdv_par_opt_min_hw);
        comboBox_opt_1[4] = new JComboBox(array_gdv_par_opt_no_veh_cl);
        comboBox_opt_1[5] = new JComboBox(array_gdv_par_opt_no_drv_cl);
        comboBox_opt_1[6] = new JComboBox(array_gdv_par_opt_pct_lt);
        comboBox_opt_1[7] = new JComboBox(array_gdv_par_opt_pct_rt);

        label_title_opt_1 = new JLabel(lclv_tx_fmt.mstv_name);
        label_title_opt_1.setFont(font1);

        for (int lsiv_field = 1; lsiv_field <= OPT1FIELD; lsiv_field++) {
            label_opt_1[lsiv_field] = new JLabel(lsiv_field + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS - 1 + lsiv_field]);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

        label_title_opt_2 = new JLabel(lclv_tx_fmt.mstv_name);
        label_title_opt_2.setFont(font1);

        for (int lsiv_field = 1; lsiv_field <= OPT2FIELD; lsiv_field++) {
            button_opt_2[lsiv_field] = new JButton("Edit");
        }

        label_opt_2[1] = new JLabel("1. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA]);
        label_opt_2[2] = new JLabel("2. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA]);
        label_opt_2[3] = new JLabel("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX]);

        String[] array_opt_2_1 = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX].substring(1).split("\\|");
        String[] array_opt_2_2 = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA].substring(1).split("\\|");
        String[] array_opt_2_3 = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA].substring(1).split("\\|");

        comboBox_opt_2[1] = new JComboBox(array_opt_2_1);
        comboBox_opt_2[2] = new JComboBox(array_opt_2_2);
        comboBox_opt_2[3] = new JComboBox(array_opt_2_3);

        if (gdvsim.gclv_inter.mbov_GDV_Data_OK) {
            comboBox_opt_1[1].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs));
            comboBox_opt_1[2].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time));
            comboBox_opt_1[3].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_min_hw));
            comboBox_opt_1[4].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl));
            comboBox_opt_1[5].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl));
            comboBox_opt_1[6].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_lt));
            comboBox_opt_1[7].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_rt));

            comboBox_opt_2[1].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data);
            comboBox_opt_2[2].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data);
            comboBox_opt_2[3].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix);
        }
        else {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

            if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                comboBox_opt_1[1].setSelectedItem("6");
            }
            else {
                comboBox_opt_1[1].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS]));
            }

            comboBox_opt_1[2].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME]));
            comboBox_opt_1[3].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW]));
            comboBox_opt_1[4].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL]));
            comboBox_opt_1[5].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL]));
            comboBox_opt_1[6].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT]));
            comboBox_opt_1[7].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT]));

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

            comboBox_opt_2[1].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA]);
            comboBox_opt_2[2].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA]);
            comboBox_opt_2[3].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX]);
        }

        for (int lsiv_field = 1; lsiv_field <= OPT2FIELD; lsiv_field++) {
            if (comboBox_opt_2[lsiv_field].getSelectedItem().toString().equals("YES")) {
                button_opt_2[lsiv_field].setEnabled(true);
            }
            else {
                button_opt_2[lsiv_field].setEnabled(false);
            }
        }

        arcButton = new JButton("Arc Data (For Plotting Purposes Only)");
        lineButton = new JButton("Line Data (For Plotting Purposes Only)");
        plotButton = new JButton("Path and Plot Options Data");
        specialVehicleButton = new JButton("Special Vehicle Data");
        logoutVehicleButton = new JButton("Logout Summary for Driver-Vehicle Unit by Vehicle Class");
        logoutDriverButton = new JButton("Logout Summary for Driver-Vehicle Unit by Driver Class");
        freeUturnButton = new JButton("Diamond Interchange Free U-Turn Data");
        internalLegButton = new JButton("Diamond Interchange Internal Leg Data");

        label_note = new JLabel("NOTE:  Please see Help -> Diamond Interchange for more information.  END NOTE");

        JPanel label_note_panel = new JPanel();
        label_note_panel.add(label_note);

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            freeUturnButton.setVisible(true);
            internalLegButton.setVisible(true);
            label_note.setVisible(true);

            freeUturnButton.setEnabled(true);
            internalLegButton.setEnabled(true);
            label_note.setEnabled(true);
        }
        else {
            freeUturnButton.setVisible(false);
            internalLegButton.setVisible(false);
            label_note.setVisible(false);

            freeUturnButton.setEnabled(false);
            internalLegButton.setEnabled(false);
            label_note.setEnabled(false);
        }

        acceptDefaultCheckbox = new JCheckBox();
        acceptDefaultCheckbox.setSelected(false);
        acceptDefaultCheckbox.setVisible(true);
        acceptDefaultCheckbox.setEnabled(true);

        if (gdvsim.flag_OpenNewFile) {
            acceptDefaultCheckbox.setEnabled(true);
            acceptDefaultCheckbox.setVisible(true);
        }
        else {
            acceptDefaultCheckbox.setEnabled(false);
            acceptDefaultCheckbox.setVisible(false);
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            acceptDefaultCheckbox.setText("Accept All Diamond GDV Default Data Values");
        }
        else {
            acceptDefaultCheckbox.setText("Accept All GDV Default Data Values");
        }

        okButton = new JButton("   OK   ");
        applyButton = new JButton("  Apply");
        cancelButton = new JButton("Cancel");

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        JPanel accept_panel = new JPanel();
        accept_panel.add(acceptDefaultCheckbox);

        arcButton.setMnemonic(KeyEvent.VK_A);
        lineButton.setMnemonic(KeyEvent.VK_L);
        plotButton.setMnemonic(KeyEvent.VK_P);
        logoutVehicleButton.setMnemonic(KeyEvent.VK_V);
        logoutDriverButton.setMnemonic(KeyEvent.VK_D);
        specialVehicleButton.setMnemonic(KeyEvent.VK_S);
        freeUturnButton.setMnemonic(KeyEvent.VK_F);
        internalLegButton.setMnemonic(KeyEvent.VK_I);
        acceptDefaultCheckbox.setMnemonic(KeyEvent.VK_G);
        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);
        logoutVehicleButton.setDisplayedMnemonicIndex(42);
        logoutDriverButton.setDisplayedMnemonicIndex(42);
        internalLegButton.setDisplayedMnemonicIndex(20);

        helpListener = new HelpListener();
        openComboMenuListener = new OpenComboMenuListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        arcButton.addActionListener(componentActionListener);
        arcButton.addKeyListener(componentKeyListener);
        arcButton.addKeyListener(helpListener);
        lineButton.addActionListener(componentActionListener);
        lineButton.addKeyListener(componentKeyListener);
        lineButton.addKeyListener(helpListener);
        plotButton.addActionListener(componentActionListener);
        plotButton.addKeyListener(componentKeyListener);
        plotButton.addKeyListener(helpListener);
        logoutVehicleButton.addActionListener(componentActionListener);
        logoutVehicleButton.addKeyListener(componentKeyListener);
        logoutVehicleButton.addKeyListener(helpListener);
        logoutDriverButton.addActionListener(componentActionListener);
        logoutDriverButton.addKeyListener(componentKeyListener);
        logoutDriverButton.addKeyListener(helpListener);
        specialVehicleButton.addActionListener(componentActionListener);
        specialVehicleButton.addKeyListener(componentKeyListener);
        specialVehicleButton.addKeyListener(helpListener);
        freeUturnButton.addActionListener(componentActionListener);
        freeUturnButton.addKeyListener(componentKeyListener);
        freeUturnButton.addKeyListener(helpListener);
        internalLegButton.addActionListener(componentActionListener);
        internalLegButton.addKeyListener(componentKeyListener);
        internalLegButton.addKeyListener(helpListener);
        acceptDefaultCheckbox.addKeyListener(componentKeyListener);
        acceptDefaultCheckbox.addKeyListener(helpListener);
        okButton.addActionListener(componentActionListener);
        okButton.addKeyListener(componentKeyListener);
        okButton.addKeyListener(helpListener);
        applyButton.addActionListener(componentActionListener);
        applyButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addActionListener(componentActionListener);
        cancelButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(helpListener);
        gdvTitleText.addKeyListener(helpListener);

        for (int lsiv_field = 1; lsiv_field <= OPT1FIELD; lsiv_field++) {
            comboBox_opt_1[lsiv_field].addKeyListener(helpListener);
            comboBox_opt_1[lsiv_field].addKeyListener(openComboMenuListener);
        }

        for (int lsiv_field = 1; lsiv_field <= OPT2FIELD; lsiv_field++) {
            button_opt_2[lsiv_field].addActionListener(componentActionListener);
            button_opt_2[lsiv_field].addKeyListener(componentKeyListener);
            button_opt_2[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= OPT2FIELD; lsiv_field++) {
            comboBox_opt_2[lsiv_field].addActionListener(componentActionListener);
            comboBox_opt_2[lsiv_field].addKeyListener(componentKeyListener);
            comboBox_opt_2[lsiv_field].addKeyListener(helpListener);
            comboBox_opt_2[lsiv_field].addKeyListener(openComboMenuListener);
        }

        setAccessiblility();

        if (!gdvsim.flag_driverClass_ok) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl = PARAMS.TEXAS_MODEL_NDCD;

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR];
            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDCD; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.msia_drv_oper_char[lsiv_drv] = Integer
                        .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME];
            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDCD; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mdfv_drv_PIJR_time[lsiv_drv] = Double
                        .valueOf(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        if (!gdvsim.flag_vehicleClass_ok) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl = PARAMS.TEXAS_MODEL_NVCD;

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[lsiv_veh] = Integer.valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01
                        - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[lsiv_veh] = Integer.valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1
                        + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[lsiv_veh] = Integer.valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01
                        - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[lsiv_veh] = Integer
                        .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[lsiv_veh] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01
                        - 1 + lsiv_veh];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[lsiv_veh] = Integer
                        .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[lsiv_veh] = Integer
                        .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[lsiv_veh] = Integer
                        .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD];
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[lsiv_veh] = Integer
                        .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]);
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units = PARAMS.TEXAS_MODEL_MNU;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS] = gdvsim.gclv_inter.TX_FROM_USER;

                for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1]);
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width[lsiv_unit] = Integer
                            .valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1]);

                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                            + lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
        }

        int iRow = 0;

        gbConstraints.insets = new Insets(2, 2, 2, 2);
        addComponent(label_title_text, iRow++, 0, 6, 1);
        addComponent(gdvTitleText, iRow++, 0, 6, 1);

        addComponent(label_title_opt_1, iRow, 0, 3, 1);
        addComponent(label_title_opt_2, iRow++, 3, 3, 1);

        addComponent(comboBox_opt_1[1], iRow, 0, 1, 1);
        addComponent(label_opt_1[1], iRow, 1, 2, 1);
        addComponent(comboBox_opt_2[1], iRow, 3, 1, 1);
        addComponent(button_opt_2[1], iRow, 4, 1, 1);
        addComponent(label_opt_2[1], iRow++, 5, 1, 1);

        addComponent(comboBox_opt_1[2], iRow, 0, 1, 1);
        addComponent(label_opt_1[2], iRow, 1, 2, 1);
        addComponent(comboBox_opt_2[2], iRow, 3, 1, 1);
        addComponent(button_opt_2[2], iRow, 4, 1, 1);
        addComponent(label_opt_2[2], iRow++, 5, 1, 1);

        addComponent(comboBox_opt_1[3], iRow, 0, 1, 1);
        addComponent(label_opt_1[3], iRow, 1, 2, 1);
        addComponent(comboBox_opt_2[3], iRow, 3, 1, 1);
        addComponent(button_opt_2[3], iRow, 4, 1, 1);
        addComponent(label_opt_2[3], iRow++, 5, 1, 1);

        addComponent(comboBox_opt_1[4], iRow, 0, 1, 1);
        addComponent(label_opt_1[4], iRow, 1, 2, 1);
        addComponent(logoutVehicleButton, iRow++, 3, 3, 1);

        addComponent(comboBox_opt_1[5], iRow, 0, 1, 1);
        addComponent(label_opt_1[5], iRow, 1, 1, 1);
        addComponent(logoutDriverButton, iRow++, 3, 3, 1);

        addComponent(comboBox_opt_1[6], iRow, 0, 1, 1);
        addComponent(label_opt_1[6], iRow, 1, 2, 1);
        addComponent(specialVehicleButton, iRow++, 3, 3, 1);

        addComponent(comboBox_opt_1[7], iRow, 0, 1, 1);
        addComponent(label_opt_1[7], iRow, 1, 2, 1);
        addComponent(arcButton, iRow++, 3, 3, 1);

        addComponent(freeUturnButton, iRow, 0, 2, 1);
        addComponent(lineButton, iRow++, 3, 3, 1);

        addComponent(internalLegButton, iRow, 0, 2, 1);
        addComponent(plotButton, iRow++, 3, 3, 1);

        addComponent(label_note_panel, iRow++, 0, 6, 1);

        addComponent(accept_panel, iRow++, 0, 6, 1);
        addComponent(ok_panel, iRow++, 0, 6, 1);

        aFrame.setSize(950, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.setFocusTraversalPolicy(new focusPolicy());
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method GDVDataDialog

    void setAccessiblility() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

        for (int lsiv_field = 1; lsiv_field <= OPT1FIELD; lsiv_field++) {
            comboBox_opt_1[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS - 1 + lsiv_field]);
            comboBox_opt_1[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS - 1 + lsiv_field]);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

        for (int lsiv_field = 1; lsiv_field <= OPT2FIELD; lsiv_field++) {
            button_opt_2[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX - 1 + lsiv_field]);
            button_opt_2[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX - 1 + lsiv_field]);
        }

        comboBox_opt_2[1].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA]);
        comboBox_opt_2[1].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA]);
        comboBox_opt_2[2].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA]);
        comboBox_opt_2[2].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA]);
        comboBox_opt_2[3].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX]);
        comboBox_opt_2[3].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX]);

        gdvTitleText.getAccessibleContext().setAccessibleName(gdvTitleText.getText());
        gdvTitleText.getAccessibleContext().setAccessibleDescription(gdvTitleText.getText());
        logoutVehicleButton.getAccessibleContext().setAccessibleName(logoutVehicleButton.getText());
        logoutVehicleButton.getAccessibleContext().setAccessibleDescription(logoutVehicleButton.getText());
        logoutDriverButton.getAccessibleContext().setAccessibleName(logoutDriverButton.getText());
        logoutDriverButton.getAccessibleContext().setAccessibleDescription(logoutDriverButton.getText());
        specialVehicleButton.getAccessibleContext().setAccessibleName(specialVehicleButton.getText());
        specialVehicleButton.getAccessibleContext().setAccessibleDescription(specialVehicleButton.getText());
        arcButton.getAccessibleContext().setAccessibleName(arcButton.getText());
        arcButton.getAccessibleContext().setAccessibleDescription(arcButton.getText());
        lineButton.getAccessibleContext().setAccessibleName(lineButton.getText());
        lineButton.getAccessibleContext().setAccessibleDescription(lineButton.getText());
        freeUturnButton.getAccessibleContext().setAccessibleName(freeUturnButton.getText());
        freeUturnButton.getAccessibleContext().setAccessibleDescription(freeUturnButton.getText());
        internalLegButton.getAccessibleContext().setAccessibleName(internalLegButton.getText());
        internalLegButton.getAccessibleContext().setAccessibleDescription(internalLegButton.getText());
        acceptDefaultCheckbox.getAccessibleContext().setAccessibleName(acceptDefaultCheckbox.getText());
        acceptDefaultCheckbox.getAccessibleContext().setAccessibleDescription(acceptDefaultCheckbox.getText());
        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");
        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");
    } // end of method setAccessiblility()

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

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            if (aComponent.equals(gdvTitleText)) {
                if (comboBox_opt_1[1].isEnabled()) {
                    return comboBox_opt_1[1];
                }
                else {
                    return comboBox_opt_1[2];
                }
            }
            else if (aComponent.equals(comboBox_opt_1[1])) {
                return comboBox_opt_1[2];
            }
            else if (aComponent.equals(comboBox_opt_1[2])) {
                return comboBox_opt_1[3];
            }
            else if (aComponent.equals(comboBox_opt_1[3])) {
                return comboBox_opt_1[4];
            }
            else if (aComponent.equals(comboBox_opt_1[4])) {
                return comboBox_opt_1[5];
            }
            else if (aComponent.equals(comboBox_opt_1[5])) {
                return comboBox_opt_1[6];
            }
            else if (aComponent.equals(comboBox_opt_1[6])) {
                return comboBox_opt_1[7];
            }
            else if (aComponent.equals(comboBox_opt_1[7])) {
                if (freeUturnButton.isVisible()) {
                    return freeUturnButton;
                }
                else {
                    return comboBox_opt_2[1];
                }
            }
            else if (aComponent.equals(freeUturnButton)) {
                return internalLegButton;
            }
            else if (aComponent.equals(internalLegButton)) {
                return comboBox_opt_2[1];
            }
            else if (aComponent.equals(comboBox_opt_2[1])) {
                if (button_opt_2[1].isEnabled()) {
                    return button_opt_2[1];
                }
                else {
                    return comboBox_opt_2[2];
                }
            }
            else if (aComponent.equals(button_opt_2[1])) {
                return comboBox_opt_2[2];
            }
            else if (aComponent.equals(comboBox_opt_2[2])) {
                if (button_opt_2[2].isEnabled()) {
                    return button_opt_2[2];
                }
                else {
                    return comboBox_opt_2[3];
                }
            }
            else if (aComponent.equals(button_opt_2[2])) {
                return comboBox_opt_2[3];
            }
            else if (aComponent.equals(comboBox_opt_2[3])) {
                if (button_opt_2[3].isEnabled()) {
                    return button_opt_2[3];
                }
                else {
                    return logoutVehicleButton;
                }
            }
            else if (aComponent.equals(button_opt_2[3])) {
                return logoutVehicleButton;
            }
            else if (aComponent.equals(logoutVehicleButton)) {
                return logoutDriverButton;
            }
            else if (aComponent.equals(logoutDriverButton)) {
                return specialVehicleButton;
            }
            else if (aComponent.equals(specialVehicleButton)) {
                return arcButton;
            }
            else if (aComponent.equals(arcButton)) {
                return lineButton;
            }
            else if (aComponent.equals(lineButton)) {
                return plotButton;
            }
            else if (aComponent.equals(plotButton)) {
                if (acceptDefaultCheckbox.isVisible()) {
                    return acceptDefaultCheckbox;
                }
                else {
                    return okButton;
                }
            }
            else if (aComponent.equals(acceptDefaultCheckbox)) {
                return okButton;
            }
            else if (aComponent.equals(okButton)) {
                return applyButton;
            }
            else if (aComponent.equals(applyButton)) {
                return cancelButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return gdvTitleText;
            }

            return gdvTitleText;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            if (aComponent.equals(gdvTitleText)) {
                return cancelButton;
            }
            else if (aComponent.equals(comboBox_opt_1[1])) {
                return gdvTitleText;
            }
            else if (aComponent.equals(comboBox_opt_1[2])) {
                if (comboBox_opt_1[1].isEnabled()) {
                    return comboBox_opt_1[1];
                }
                else {
                    return gdvTitleText;
                }
            }
            else if (aComponent.equals(comboBox_opt_1[3])) {
                return comboBox_opt_1[2];
            }
            else if (aComponent.equals(comboBox_opt_1[4])) {
                return comboBox_opt_1[3];
            }
            else if (aComponent.equals(comboBox_opt_1[5])) {
                return comboBox_opt_1[4];
            }
            else if (aComponent.equals(comboBox_opt_1[6])) {
                return comboBox_opt_1[5];
            }
            else if (aComponent.equals(comboBox_opt_1[7])) {
                return comboBox_opt_1[6];
            }
            else if (aComponent.equals(freeUturnButton)) {
                return comboBox_opt_1[7];
            }
            else if (aComponent.equals(internalLegButton)) {
                return freeUturnButton;
            }
            else if (aComponent.equals(comboBox_opt_2[1])) {
                if (internalLegButton.isEnabled()) {
                    return internalLegButton;
                }
                else {
                    return comboBox_opt_1[7];
                }
            }
            else if (aComponent.equals(button_opt_2[1])) {
                return comboBox_opt_2[1];
            }
            else if (aComponent.equals(comboBox_opt_2[2])) {
                if (button_opt_2[1].isEnabled()) {
                    return button_opt_2[1];
                }
                else {
                    return comboBox_opt_2[1];
                }
            }
            else if (aComponent.equals(button_opt_2[2])) {
                return comboBox_opt_2[2];
            }
            else if (aComponent.equals(comboBox_opt_2[3])) {
                if (button_opt_2[2].isEnabled()) {
                    return button_opt_2[2];
                }
                else {
                    return comboBox_opt_2[2];
                }
            }
            else if (aComponent.equals(button_opt_2[3])) {
                return comboBox_opt_2[3];
            }
            else if (aComponent.equals(logoutVehicleButton)) {
                if (button_opt_2[3].isEnabled()) {
                    return button_opt_2[3];
                }
                else {
                    return comboBox_opt_2[3];
                }
            }
            else if (aComponent.equals(logoutDriverButton)) {
                return logoutVehicleButton;
            }
            else if (aComponent.equals(specialVehicleButton)) {
                return logoutDriverButton;
            }
            else if (aComponent.equals(arcButton)) {
                return specialVehicleButton;
            }
            else if (aComponent.equals(lineButton)) {
                return arcButton;
            }
            else if (aComponent.equals(plotButton)) {
                return lineButton;
            }
            else if (aComponent.equals(acceptDefaultCheckbox)) {
                return plotButton;
            }
            else if (aComponent.equals(okButton)) {
                if (acceptDefaultCheckbox.isVisible()) {
                    return acceptDefaultCheckbox;
                }
                else {
                    return plotButton;
                }
            }
            else if (aComponent.equals(applyButton)) {
                return okButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return applyButton;
            }

            return gdvTitleText;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            return gdvTitleText;
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return cancelButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            return gdvTitleText;
        }
    } // end of class focusPolicy

    boolean isDataReadyForSpecialVehicle() {
        if (!gdvsim.gclv_inter.mbov_GDV_Data_OK) {
            JOptionPane.showMessageDialog(null, "GDV Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time != Integer.valueOf(comboBox_opt_1[2].getSelectedItem().toString()).intValue()) {
            JOptionPane.showMessageDialog(null,
                    "The Total Time (Startup+Simulation) (minutes) has been modified.\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Special Vehicle Data.", "Error Message",
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
            JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + " to "
                    + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue() + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Special Vehicle Data.",
                    "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
            JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl + " to "
                    + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue() + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Special Vehicle Data.",
                    "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if (!gdvsim.gclv_inter.mbov_Diamond_Leg_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Leg Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gdvsim.gclv_inter.mbov_Diamond_Lane_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Lane Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            JOptionPane.showMessageDialog(null, "number of legs invalid.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        for (int lsiv_leg = 1; lsiv_leg <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            if (!gdvsim.gclv_inter.mboa_Leg_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gdvsim.gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Inbound Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gdvsim.gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Lane Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Outbound Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gdvsim.gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " SDR Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }

        gdvsim.gclv_inter.checkForErrorsGDV();

        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS) {
            return false;
        }

        return true;

    } // end of method isDataReadyForSpecialVehicle()

    void AcceptDefaultGDVValue() {
        int default_leg_number = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        TX_Fmt lclv_tx_fmt;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_OPT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_SIZE] = gdvsim.gclv_inter.TX_FROM_USER;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO];

        int lsiv_leg_ang = 0;
        int lsiv_leg_ang_inc = 360 / gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            int lsiv_no_inb = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB];
            int lsiv_no_out = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT];
            if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                switch (lsiv_leg) {
                    case 1:
                        lsiv_leg_ang = 0;
                        lsiv_no_inb = 0;
                        break;

                    case 2:
                        lsiv_leg_ang = 90;
                        break;

                    case 3:
                        lsiv_leg_ang = 180;
                        lsiv_no_out = 0;
                        break;

                    case 4:
                        lsiv_leg_ang = 180;
                        lsiv_no_inb = 0;
                        break;

                    case 5:
                        lsiv_leg_ang = 270;
                        break;

                    case 6:
                        lsiv_leg_ang = 0;
                        lsiv_no_out = 0;
                        break;
                }
            }
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_leg_no = lsiv_leg;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang = lsiv_leg_ang;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = lsiv_no_inb;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = lsiv_no_out;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_INB];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_len_out = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_OUT];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_sl_inb = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_SL_INB];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_sl_out = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_SL_OUT];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_CL_OFF];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_med_w = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_MED_W];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang_s = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG_S];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_ang_u = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG_U];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_TRAFPER];
            lsiv_leg_ang += lsiv_leg_ang_inc;
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEG_NO] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_INB] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_OUT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_SL_INB] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_SL_OUT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_CL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_MED_W] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG_S] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG_U] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_TRAFPER] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[1] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[2] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_2];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[3] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_3];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[4] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_4];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[5] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_5];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[6] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_6];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_2] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_3] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_4] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_5] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_6] = gdvsim.gclv_inter.TX_FROM_USER;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_dist = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.msiv_vol = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mdfv_par = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mstv_tm = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM];
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.msiv_seed = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA];

        int default_inbound_number;
        int default_outbound_number;

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            for (int psiv_lane = 1; psiv_lane <= 6; psiv_lane++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE];

                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;

            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            for (int psiv_lane = 1; psiv_lane <= 6; psiv_lane++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE];

                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= default_leg_number; lsiv_leg++) {
            default_inbound_number = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;

            if (default_inbound_number == 1) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LSR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                            break;
                    }
                }
            }

            if (default_inbound_number == 2) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_inbound_number == 3) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_inbound_number == 4) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_inbound_number == 5) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_inbound_number == 6) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= default_leg_number; lsiv_leg++) {
            default_outbound_number = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;

            if (default_outbound_number == 1) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LSR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                            break;
                    }
                }
            }

            if (default_outbound_number == 2) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_outbound_number == 3) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_outbound_number == 4) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_outbound_number == 5) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }

            if (default_outbound_number == 6) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "LS";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[6].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "SR";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (lsiv_leg) {
                        case 2:
                        case 5:
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "S";
                            break;
                    }
                }
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= default_leg_number; lsiv_leg++) {
            default_inbound_number = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;

            if (default_inbound_number == 1) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 100;
            }

            if (default_inbound_number == 2) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 50;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 50;
            }

            if (default_inbound_number == 3) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 33;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 34;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 33;
            }

            if (default_inbound_number == 4) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 25;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 25;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 25;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 25;
            }

            if (default_inbound_number == 5) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 20;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 20;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 20;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 20;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 20;
            }

            if (default_inbound_number == 6) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 16;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 17;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 17;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 17;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 17;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = 16;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            for (int psiv_lane = 1; psiv_lane <= 6; psiv_lane++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            for (int psiv_lane = 1; psiv_lane <= 6; psiv_lane++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST];

        if (default_leg_number == 3) {
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 50;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 50;

            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 50;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 50;

            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 50;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 50;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
        }

        if (default_leg_number == 4) {
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 33;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;

            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 33;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;

            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;

            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 34;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
        }

        if (default_leg_number == 5) {
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;

            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;

            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;

            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;

            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
        }

        if (default_leg_number == 6) {
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;

            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;

            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;

            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;

            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;

            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
            gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;

            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 33;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 34;
            gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;

            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;
            gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;

            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
            gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;

            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
            gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;

            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;
            gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_2] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_3] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_4] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_5] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_6] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs = 0;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_sdr = 1; lsiv_sdr <= PARAMS.TEXAS_MODEL_NSR; lsiv_sdr++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_LEG_NUMBER] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_veh] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                        + lsiv_veh];
                gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                        + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];

            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1
                        - 1 + lsiv_veh];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_path_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_max_radius_path = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_min_dist_paths = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH] = gdvsim.gclv_inter.TX_FROM_USER;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[lsiv_veh] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1
                    + lsiv_veh];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[lsiv_drv] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                    + lsiv_drv];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 0;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

    } // end of method AcceptDefaultGDVValue()

    void AcceptDefaultFreeUTurnsValue() {
        TX_Fmt lclv_tx_fmt;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_space_between = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_entr_length = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_entr_radius = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_exit_length = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_exit_radius = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_percent_using = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_space_between = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_entr_length = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_entr_radius = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_exit_length = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_exit_radius = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_percent_using = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;

        int lsiv_inb_leg = 3; // fut=1
        int lsiv_out_leg = 4;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_entr_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_exit_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mstv_allowed_veh_type;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mstv_allowed_veh_type;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

        lsiv_inb_leg = 6; // fut=2
        lsiv_out_leg = 1;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_entr_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_exit_length;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mstv_allowed_veh_type;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mstv_allowed_veh_type;

        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
    } // end of method AcceptDefaultFreeUTurnsValue()

    void AcceptDefaultDiamondLegValue() {
        TX_Fmt lclv_tx_fmt;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_int_leg_number = 0;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_speed_inb_to_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_median_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_INT_LEG_NUM] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_R] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_SPEED_INB_L] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_MEDIAN_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;

        int lsiv_leg_cl = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        int lsiv_leg_cr = 0;

        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;

        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

    } // end of method AcceptDefaultDiamondLegValue()

    void AcceptDefaultDiamondLaneValue() {
        TX_Fmt lclv_tx_fmt;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG];

        int default_number_of_lanes_center_right;
        int default_number_of_lanes_center_left;

        if (!gdvsim.gclv_inter.mbov_Diamond_Leg_OK) {
            default_number_of_lanes_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R];
            default_number_of_lanes_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L];
        }
        else {
            default_number_of_lanes_center_right = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;
            default_number_of_lanes_center_left = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE];

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_mvmt_near_center_right.mstv_code = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_mvmt_near_center_left.mstv_code = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_mvmt_near_center_right.mstv_code = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_mvmt_near_center_left.mstv_code = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT];
        }

        if (default_number_of_lanes_center_right == 1) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_left.mstv_code = "LS";
        }

        if (default_number_of_lanes_center_right == 2) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_right == 3) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_right == 4) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[4].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[4].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_right == 5) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[4].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[5].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[4].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[5].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_right == 6) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[4].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[5].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[6].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[3].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[4].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[5].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[6].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_left == 1) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_left.mstv_code = "LS";
        }

        if (default_number_of_lanes_center_left == 2) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_left == 3) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_left == 4) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[4].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[4].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_left == 5) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[4].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[5].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[4].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[5].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        if (default_number_of_lanes_center_left == 6) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_right.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[4].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[5].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[6].mclv_mvmt_near_center_right.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[1].mclv_mvmt_near_center_left.mstv_code = "LS";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[2].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[3].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[4].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[5].mclv_mvmt_near_center_left.mstv_code = "S";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[6].mclv_mvmt_near_center_left.mstv_code = "S";
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        int lsiv_leg_cl = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        int lsiv_leg_cr = 0;
        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_int_lane_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_usable_from_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_usable_from_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_offset_near_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_offset_near_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE];

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_mvmt_near_center_right.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mclv_mvmt_near_center_left.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_offset_near_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].msiv_offset_near_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mstv_allowed_veh_type;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_leg].mstv_allowed_veh_type;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }

        for (int lsiv_leg = 1; lsiv_leg <= 6; lsiv_leg++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_int_lane_width = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_usable_from_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_usable_from_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_offset_near_center_right = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_offset_near_center_left = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE];

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_int_lane_width;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_mvmt_near_center_right.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mclv_mvmt_near_center_left.mstv_code;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_usable_from_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_usable_from_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_offset_near_center_right;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].msiv_offset_near_center_left;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mstv_allowed_veh_type;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left[lsiv_leg].mstv_allowed_veh_type;

            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cr].mcla_out_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg_cl].mcla_inb_lane[lsiv_leg].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }
    } // end of method AcceptDefaultDiamondLaneValue()

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == gdvTitleText) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TITLE];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT], gdvTitleText.getText().trim(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_opt_1[1]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    int lsiv_min;
                    int lsiv_max;
                    int lsiv_inc;
                    int lsiv_def;

                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        lsiv_min = 6;
                        lsiv_max = 6;
                        lsiv_inc = 1;
                        lsiv_def = 6;
                    }
                    else {
                        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
                        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
                        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
                        lsiv_def = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS];
                    }

                    if (gdvsim.gclv_inter.mbov_GDV_Data_OK) {
                        if (comboBox_opt_1[1].getItemCount() == 1) {
                            lsiv_min = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                            lsiv_max = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                            lsiv_inc = 1;
                            lsiv_def = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                        }
                    }

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS], comboBox_opt_1[1].getSelectedItem().toString(), Integer.toString(lsiv_def), " ",
                            Integer.toString(lsiv_min), Integer.toString(lsiv_max), Integer.toString(lsiv_inc));
                }
                else if (event.getSource() == comboBox_opt_1[2]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME], comboBox_opt_1[2].getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME]), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME]));
                }
                else if (event.getSource() == comboBox_opt_1[3]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW], comboBox_opt_1[3].getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW]));
                }
                else if (event.getSource() == comboBox_opt_1[4]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL], comboBox_opt_1[4].getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL]));
                }
                else if (event.getSource() == comboBox_opt_1[5]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL], comboBox_opt_1[5].getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL]));
                }
                else if (event.getSource() == comboBox_opt_1[6]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT], comboBox_opt_1[6].getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT]));
                }
                else if (event.getSource() == comboBox_opt_1[7]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT], comboBox_opt_1[7].getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT]));
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
                else if (event.getSource() == acceptDefaultCheckbox) {
                    if (!gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        if (acceptDefaultCheckbox.isSelected()) {
                            new HelpDialog(true, "Accept Default Checkbox", "The Accept Default Checkbox accepts all GDV default data values.",
                                    "The Accept All GDV default data values check box accepts all the Geometry and Driver-Vehicle values.", "True", " ", " ", " ", " ", " ");
                        }
                        else {
                            new HelpDialog(true, "Accept Default Checkbox", "The Accept Default Checkbox accepts all GDV default data values.",
                                    "The Accept All GDV default data values check box accepts all the Geometry and Driver-Vehicle values.", "False", " ", " ", " ", " ", " ");
                        }
                    }
                    else {
                        if (acceptDefaultCheckbox.isSelected()) {
                            new HelpDialog(true, "Accept Diamond Default Checkbox", "The Accept Diamond Default Checkbox accepts all Diamond GDV default data values.",
                                    "The Accept All Diamond GDV default data values check box accepts all the Geometry and Driver-Vehicle values.", "True", " ", " ", " ", " ", " ");
                        }
                        else {
                            new HelpDialog(true, "Accept Diamond Default Checkbox", "The Accept Default Checkbox accepts all Diamond GDV default data values.",
                                    "The Accept All Diamond GDV default data values check box accepts all the Geometry and Driver-Vehicle values.", "False", " ", " ", " ", " ", " ");
                        }
                    }
                }
                else if (event.getSource() == comboBox_opt_2[1]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA], comboBox_opt_2[1]
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_opt_2[2]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA], comboBox_opt_2[2]
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_opt_2[3]) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX], comboBox_opt_2[3]
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX], " ", " ", " ");
                }
                else if (event.getSource() == button_opt_2[1]) {
                    new HelpDialog(true, "Edit " + button_opt_2[1].getText(), "Open " + button_opt_2[1].getText() + " Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_opt_2[2]) {
                    new HelpDialog(true, "Edit " + button_opt_2[2].getText(), "Open " + button_opt_2[2].getText() + " Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_opt_2[3]) {
                    new HelpDialog(true, "Edit " + button_opt_2[3], "Open " + button_opt_2[3] + " Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == logoutVehicleButton) {
                    new HelpDialog(true, logoutVehicleButton.getText(), "Open " + logoutVehicleButton.getText() + " Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == logoutDriverButton) {
                    new HelpDialog(true, logoutDriverButton.getText(), "Open " + logoutDriverButton.getText() + " Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == specialVehicleButton) {
                    new HelpDialog(true, specialVehicleButton.getText(), "Open " + specialVehicleButton.getText() + " Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == arcButton) {
                    new HelpDialog(true, arcButton.getText(), "Open " + arcButton.getText() + " dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == lineButton) {
                    new HelpDialog(true, lineButton.getText(), "Open " + lineButton.getText() + " dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == freeUturnButton) {
                    new HelpDialog(true, freeUturnButton.getText(), "Open " + freeUturnButton.getText() + " dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == internalLegButton) {
                    new HelpDialog(true, internalLegButton.getText(), "Open " + internalLegButton.getText() + " dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }
    } // end of class HelpListener

    void saveData() {
        gdvsim.gclv_inter.mbov_GDV_Data_OK = true;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card = gdvTitleText.getText().trim();

        TX_Fmt lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs = Integer.valueOf(comboBox_opt_1[1].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time = Integer.valueOf(comboBox_opt_1[2].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_min_hw = Double.valueOf(comboBox_opt_1[3].getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl = Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl = Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_lt = Integer.valueOf(comboBox_opt_1[6].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_rt = Integer.valueOf(comboBox_opt_1[7].getSelectedItem().toString()).intValue();

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_MIN_HW] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT] = gdvsim.gclv_inter.TX_FROM_USER;

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_OPT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_plot_opt = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_OPT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_OPT] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_SIZE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_plot_size = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_SIZE];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PLOT_SIZE] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }
        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data = comboBox_opt_2[1].getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data = comboBox_opt_2[2].getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix = comboBox_opt_2[3].getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] = gdvsim.gclv_inter.TX_FROM_USER;

        if (acceptDefaultCheckbox.isSelected()) {
            for (int lsiv_leg = 1; lsiv_leg <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
                gdvsim.gclv_inter.mboa_Leg_OK[lsiv_leg] = true;
                gdvsim.gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_leg] = true;
                gdvsim.gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_leg] = true;
                gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_leg] = true;
                gdvsim.gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_leg] = true;
                gdvsim.gclv_inter.mboa_Leg_Traffic_Mix_OK[lsiv_leg] = true;
                gdvsim.gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[lsiv_leg] = true;
            }

            AcceptDefaultGDVValue();

            if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                AcceptDefaultFreeUTurnsValue();
                AcceptDefaultDiamondLegValue();
                AcceptDefaultDiamondLaneValue();

                gdvsim.gclv_inter.mbov_Free_UTurns_OK = true;
                gdvsim.gclv_inter.mbov_Diamond_Leg_OK = true;
                gdvsim.gclv_inter.mbov_Diamond_Lane_OK = true;
            }
            gdvsim.myIntersection.setVisible(true);
            gdvsim.myIntersection.draw();
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            gdvsim.intersectionTypeButton.setText("Diamond Interchange");
            gdvsim.gdvDataButton.setText("Diamond GDV Data");
        }
        else {
            gdvsim.intersectionTypeButton.setText("Standard Intersection");
            gdvsim.gdvDataButton.setText("GDV Data");
        }

        gdvsim.intersectionTypeButton.setVisible(true);
        gdvsim.intersectionTypeButton.setEnabled(false);
        gdvsim.gdvDataButton.setVisible(true);

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 3) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(false);
            gdvsim.leg5Button.setVisible(false);
            gdvsim.leg6Button.setVisible(false);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 4) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(true);
            gdvsim.leg5Button.setVisible(false);
            gdvsim.leg6Button.setVisible(false);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 5) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(true);
            gdvsim.leg5Button.setVisible(true);
            gdvsim.leg6Button.setVisible(false);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 6) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(true);
            gdvsim.leg5Button.setVisible(true);
            gdvsim.leg6Button.setVisible(true);
        }

        gdvsim.simulationButton.setVisible(true);
        gdvsim.gclv_inter.calculate_graphics_and_paint();

    } // end of method saveData()

    boolean isError() {
        double simStartTime;
        double simSimulationTime;
        int gdvTotalTime;
        TX_Fmt lclv_tx_fmt;
        int lsiv_textLength;
        int lsiv_textMax;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TITLE];
        lsiv_textLength = gdvTitleText.getText().trim().length();
        lsiv_textMax = lclv_tx_fmt.msia_fs[gdvsim.gclv_inter.TX_FMT_GDV_TITLE_TEXT];

        if (lsiv_textLength == 0) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card = null;
            JOptionPane.showMessageDialog(null, "Please enter a title.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }
        if (lsiv_textLength > lsiv_textMax) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card = null;
            JOptionPane.showMessageDialog(null, "Title is " + (lsiv_textLength - lsiv_textMax) + " character" + ((lsiv_textLength - lsiv_textMax) > 1 ? "s" : "") + " longer than maximum of "
                    + lsiv_textMax + " characters.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                && (((simStartTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time)
                        + (simSimulationTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time)) > (gdvTotalTime = Integer
                                .valueOf(comboBox_opt_1[2].getSelectedItem().toString()).intValue()))) {
            JOptionPane.showMessageDialog(null, "SIM Start Time plus Simulation Time = " + (simStartTime + simSimulationTime) + " should be less than or equal to the GDV Total Time = " + gdvTotalTime
                    + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
            gdvsim.flag_driverMixData_ok = false;
            gdvsim.flag_vehicleClass_ok = false;
            gdvsim.flag_drvVehByVeh_ok = false;
            JOptionPane
                    .showMessageDialog(
                            null,
                            "The Number of Vehicle Classes has changed from "
                                    + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                    + " to "
                                    + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                    + ".\nPlease OK or Apply User-Defined Vehicle Data, User-Defined Driver Mix Data, and Logout Summary for Driver-Vehicle Unit by Vehicle Class in GDV Data.\nPlease OK or Apply User-Defined Traffic Mix Data for each Leg with Inbound Lanes.\nPlease OK or Apply User-Defined Traffic Mix Data for each Leg with Varying Traffic Period Data.",
                            "Warning Message", JOptionPane.WARNING_MESSAGE);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
            gdvsim.flag_driverMixData_ok = false;
            gdvsim.flag_driverClass_ok = false;
            gdvsim.flag_drvVehByDrv_ok = false;
            JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl + " to "
                    + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                    + ".\nPlease OK or Apply User-Defined Driver Data, User-Defined Driver Mix Data, and Logout Summary for Driver-Vehicle Unit by Driver Class in GDV Data.", "Warning Message",
                    JOptionPane.WARNING_MESSAGE);
        }

        return false;
    } // end of method isError()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();
                    gdvsim.fileMenu.add(gdvsim.imageMenu, gdvsim.fileMenu.getMenuComponentCount() - 2);

                    if (event.getSource() == applyButton) {
                        comboBox_opt_1[1].removeAllItems();
                        comboBox_opt_1[1].addItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs));
                    }

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
            else if (event.getSource() == specialVehicleButton) {
                if (isDataReadyForSpecialVehicle()) {
                    new SpecialVehicleDialog();
                }
                else {
                    return;
                }
            }
            else if (event.getSource() == internalLegButton) {
                new DiamondInternalLegDataDialog();
            }
            else if (event.getSource() == freeUturnButton) {
                new DiamondFreeUturnDialog();
            }
            else if (event.getSource() == logoutVehicleButton) {
                if (!gdvsim.gclv_inter.mbov_GDV_Data_OK) {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply the GDV Data value(s) before accessing the Logout Summary for Driver-Vehicle Unit by Vehicle Class.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                    JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                            + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                            + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Logout Summary for Driver-Vehicle Unit by Vehicle Class.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                }
                else {
                    new DriverVehicleByVehicleClassDialog();
                }
            }
            else if (event.getSource() == logoutDriverButton) {
                if (!gdvsim.gclv_inter.mbov_GDV_Data_OK) {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply the GDV Data value(s) before accessing the Logout Summary for Driver-Vehicle Unit by Driver Class.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                    JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                            + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                            + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Logout Summary for Driver-Vehicle Unit by Driver Class.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                }
                else {
                    new DriverVehicleByDriverClassDialog();
                }
            }
            else if (event.getSource() == plotButton) {
                new PlotDialog();
            }
            else if (event.getSource() == lineButton) {
                new LineDialog();
            }
            else if (event.getSource() == arcButton) {
                new ArcDialog();
            }
            else if (event.getSource() == comboBox_opt_2[1]) {
                if (comboBox_opt_2[1].getSelectedItem().toString().equals("YES")) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                        comboBox_opt_2[1].setSelectedItem("NO");
                        JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Vehicle Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        button_opt_2[1].setEnabled(true);
                        new VehicleClassDialog(comboBox_opt_1[4], comboBox_opt_2[3], button_opt_2[3]);
                    }
                }
                else {
                    button_opt_2[1].setEnabled(false);
                }
            }
            else if (event.getSource() == comboBox_opt_2[2]) {
                if (comboBox_opt_2[2].getSelectedItem().toString().equals("YES")) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                        comboBox_opt_2[2].setSelectedItem("NO");
                        JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        button_opt_2[2].setEnabled(true);
                        new DriverClassDialog(comboBox_opt_1[5], comboBox_opt_2[3], button_opt_2[3]);
                    }
                }
                else {
                    button_opt_2[2].setEnabled(false);
                }
            }
            else if (event.getSource() == comboBox_opt_2[3]) {
                if (comboBox_opt_2[3].getSelectedItem().toString().equals("YES")) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                        comboBox_opt_2[3].setSelectedItem("NO");
                        JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                        comboBox_opt_2[3].setSelectedItem("NO");
                        JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        button_opt_2[3].setEnabled(true);
                        new DriverMixDataDialog();
                    }
                }
                else {
                    button_opt_2[3].setEnabled(false);
                }
            }
            else if (event.getSource() == button_opt_2[1]) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                    JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                            + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                            + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Vehicle Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                }
                else {
                    new VehicleClassDialog(comboBox_opt_1[4], comboBox_opt_2[3], button_opt_2[3]);
                }
            }
            else if (event.getSource() == button_opt_2[2]) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                    JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                            + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                            + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                }
                else {
                    new DriverClassDialog(comboBox_opt_1[5], comboBox_opt_2[3], button_opt_2[3]);
                }
            }
            else if (event.getSource() == button_opt_2[3]) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                    JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                            + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                            + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                    JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                            + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                            + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                }
                else {
                    new DriverMixDataDialog();
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        saveData();
                        gdvsim.fileMenu.add(gdvsim.imageMenu, gdvsim.fileMenu.getMenuComponentCount() - 2);

                        if (event.getSource() == applyButton) {
                            comboBox_opt_1[1].removeAllItems();
                            comboBox_opt_1[1].addItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs));
                        }

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
                else if (event.getSource() == specialVehicleButton) {
                    if (isDataReadyForSpecialVehicle()) {
                        new SpecialVehicleDialog();
                    }
                    else {
                        return;
                    }
                }
                else if (event.getSource() == internalLegButton) {
                    new DiamondInternalLegDataDialog();
                }
                else if (event.getSource() == freeUturnButton) {
                    new DiamondFreeUturnDialog();
                }
                else if (event.getSource() == logoutVehicleButton) {
                    if (!gdvsim.gclv_inter.mbov_GDV_Data_OK) {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply the GDV Data value(s) before accessing the Logout Summary for Driver-Vehicle Unit by Vehicle Class.", "Error Message",
                                JOptionPane.ERROR_MESSAGE);
                    }
                    else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Logout Summary for Driver-Vehicle Unit by Vehicle Class.", "Error Message",
                                JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        new DriverVehicleByVehicleClassDialog();
                    }
                }
                else if (event.getSource() == logoutDriverButton) {
                    if (!gdvsim.gclv_inter.mbov_GDV_Data_OK) {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply the GDV Data value(s) before accessing the Logout Summary for Driver-Vehicle Unit by Driver Class.", "Error Message",
                                JOptionPane.ERROR_MESSAGE);
                    }
                    else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the Logout Summary for Driver-Vehicle Unit by Driver Class.", "Error Message",
                                JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        new DriverVehicleByDriverClassDialog();
                    }
                }
                else if (event.getSource() == plotButton) {
                    new PlotDialog();
                }
                else if (event.getSource() == lineButton) {
                    new LineDialog();
                }
                else if (event.getSource() == arcButton) {
                    new ArcDialog();
                }
                else if (event.getSource() == comboBox_opt_2[1]) {
                    if (comboBox_opt_2[1].getSelectedItem().toString().equals("YES")) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                            comboBox_opt_2[1].setSelectedItem("NO");
                            JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from "
                                    + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + " to "
                                    + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                    + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Vehicle Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            button_opt_2[1].setEnabled(true);
                            new VehicleClassDialog(comboBox_opt_1[4], comboBox_opt_2[3], button_opt_2[3]);
                        }
                    }
                    else {
                        button_opt_2[1].setEnabled(false);
                    }
                }
                else if (event.getSource() == comboBox_opt_2[2]) {
                    if (comboBox_opt_2[2].getSelectedItem().toString().equals("YES")) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                            comboBox_opt_2[2].setSelectedItem("NO");
                            JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from "
                                    + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl + " to "
                                    + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                    + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            button_opt_2[2].setEnabled(true);
                            new DriverClassDialog(comboBox_opt_1[5], comboBox_opt_2[3], button_opt_2[3]);
                        }
                    }
                    else {
                        button_opt_2[2].setEnabled(false);
                    }
                }
                else if (event.getSource() == comboBox_opt_2[3]) {
                    if (comboBox_opt_2[3].getSelectedItem().toString().equals("YES")) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                            comboBox_opt_2[3].setSelectedItem("NO");
                            JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from "
                                    + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + " to "
                                    + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                    + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                            comboBox_opt_2[3].setSelectedItem("NO");
                            JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from "
                                    + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl + " to "
                                    + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                    + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            button_opt_2[3].setEnabled(true);
                            new DriverMixDataDialog();
                        }
                    }
                    else {
                        button_opt_2[3].setEnabled(false);
                    }
                }
                else if (event.getSource() == button_opt_2[1]) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Vehicle Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        new VehicleClassDialog(comboBox_opt_1[4], comboBox_opt_2[3], button_opt_2[3]);
                    }
                }
                else if (event.getSource() == button_opt_2[2]) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        new DriverClassDialog(comboBox_opt_1[5], comboBox_opt_2[3], button_opt_2[3]);
                    }
                }
                else if (event.getSource() == button_opt_2[3]) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "The Number of Vehicle Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[4].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()) {
                        JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                                + " to " + Integer.valueOf(comboBox_opt_1[5].getSelectedItem().toString()).intValue()
                                + ".\nPlease OK or Apply the GDV Parameter-Option Data before accessing the User-Defined Driver Mix Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        new DriverMixDataDialog();
                    }
                }
                else if (event.getSource() == acceptDefaultCheckbox) {
                    if (acceptDefaultCheckbox.isSelected()) {
                        acceptDefaultCheckbox.setSelected(false);
                    }
                    else {
                        acceptDefaultCheckbox.setSelected(true);
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_ENTER)

            else if (event.getKeyCode() == KeyEvent.VK_SPACE) {
                if (event.getSource() == acceptDefaultCheckbox) {
                    if (acceptDefaultCheckbox.isSelected()) {
                        acceptDefaultCheckbox.setSelected(false);
                    }
                    else {
                        acceptDefaultCheckbox.setSelected(true);
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_SPACE)
        } // end of method keyPressed
    }// end of ComponentKeyListener

} // end of class GDVDataDialog
