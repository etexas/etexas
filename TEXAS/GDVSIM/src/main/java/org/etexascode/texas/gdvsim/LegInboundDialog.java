package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           LegInboundDialog.java                            */
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

class LegInboundDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title, label_gdv_traf_hdway_dist, label_gdv_traf_hdway_vol, label_gdv_traf_hdway_par, label_gdv_traf_hdway_mean, label_gdv_traf_hdway_85th, label_gdv_traf_hdway_tm,
            label_gdv_traf_hdway_seed;

    JComboBox comboBox_gdv_traf_hdway_dist, comboBox_gdv_traf_hdway_vol, comboBox_gdv_traf_hdway_par, comboBox_gdv_traf_hdway_mean, comboBox_gdv_traf_hdway_85th, comboBox_gdv_traf_hdway_tm;

    int number_of_legs, leg_number;

    JPanel panel_title, ok_panel;

    JButton okButton, applyButton, cancelButton, trafficMixDataButton, seedButton;

    JTextField seedField;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    TrafficMixComboBoxActionListener trafficMixComboBoxActionListener;

    TrafficMixComboBoxKeyListener trafficMixComboBoxKeyListener;

    TrafficMixButtonActionListener trafficMixButtonActionListener;

    TrafficMixButtonKeyListener trafficMixButtonKeyListener;

    DistributionActionListener distributionActionListener;

    DistributionKeyListener distributionKeyListener;

    SeedButtonActionListener seedButtonActionListener;

    SeedButtonKeyListener seedButtonKeyListener;

    SeedDigitalListener seedDigitalListener;

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    String[] array_gdv_traf_hdway_vol;

    String[] array_gdv_hdway_param_constan;

    String[] array_gdv_hdway_param_erlang;

    String[] array_gdv_hdway_param_gamma;

    String[] array_gdv_hdway_param_lognrml;

    String[] array_gdv_hdway_param_negexp;

    String[] array_gdv_hdway_param_snegexp;

    String[] array_gdv_hdway_param_uniform;

    String[] array_gdv_traf_hdway_mean;

    String[] array_gdv_traf_hdway_85th;

    int arrayIndex, SizeOfArray, SizeOfConstanArray, SizeOfErlangArray, SizeOfGammaArray, SizeOfLognrmlArray, SizeOfNegexpArray, SizeOfSnegexpArray, SizeOfUniformArray;

    int randomSeed;

    Font font1, font2;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    int intArrayElementValue;

    double doubleArrayElementValue;

    TX_Fmt lclv_tx_fmt;

    int lsiv_min;

    int lsiv_max;

    int lsiv_inc;

    double lsiv_min_double;

    double lsiv_max_double;

    double lsiv_inc_double;

    String titleString;

    public LegInboundDialog(int leg_num) {
        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        leg_number = leg_num;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        titleString = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number));

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
        font2 = new Font("TimesRoman", Font.BOLD, 16);

        trafficMixDataButton = new JButton("            Edit           ");
        seedButton = new JButton("Random Seed");

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        String[] array_gdv_traf_hdway_dist = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST].substring(1).split("\\|");

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_traf_hdway_vol = new String[SizeOfArray];

        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_gdv_traf_hdway_vol[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_gdv_traf_hdway_par = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_gdv_traf_hdway_par[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];

        SizeOfConstanArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double + 1);
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_constan = new String[SizeOfConstanArray];

        for (arrayIndex = 0; arrayIndex < SizeOfConstanArray; arrayIndex++) {
            array_gdv_hdway_param_constan[arrayIndex] = Double.toString(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];

        SizeOfErlangArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double + 1);
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_erlang = new String[SizeOfErlangArray];

        for (arrayIndex = 0; arrayIndex < SizeOfErlangArray; arrayIndex++) {
            array_gdv_hdway_param_erlang[arrayIndex] = Double.toString(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];

        SizeOfGammaArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_gamma = new String[SizeOfGammaArray];
        for (arrayIndex = 0; arrayIndex < SizeOfGammaArray; arrayIndex++) {
            array_gdv_hdway_param_gamma[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];

        SizeOfLognrmlArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_lognrml = new String[SizeOfLognrmlArray];
        for (arrayIndex = 0; arrayIndex < SizeOfLognrmlArray; arrayIndex++) {
            array_gdv_hdway_param_lognrml[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];

        SizeOfNegexpArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double + 1);
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_negexp = new String[SizeOfNegexpArray];

        for (arrayIndex = 0; arrayIndex < SizeOfNegexpArray; arrayIndex++) {
            array_gdv_hdway_param_negexp[arrayIndex] = Double.toString(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];

        SizeOfSnegexpArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_snegexp = new String[SizeOfSnegexpArray];
        for (arrayIndex = 0; arrayIndex < SizeOfSnegexpArray; arrayIndex++) {
            array_gdv_hdway_param_snegexp[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];

        SizeOfUniformArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        array_gdv_hdway_param_uniform = new String[SizeOfUniformArray];
        for (arrayIndex = 0; arrayIndex < SizeOfUniformArray; arrayIndex++) {
            array_gdv_hdway_param_uniform[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_gdv_traf_hdway_mean = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_gdv_traf_hdway_mean[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_gdv_traf_hdway_85th = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_gdv_traf_hdway_85th[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        String[] array_gdv_traf_hdway_tm = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM].substring(1).split("\\|");

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];

        randomSeed = lsiv_min + (int)(Math.random() * lsiv_max);
        seedField = new JTextField(Integer.toString(randomSeed), 5);
        seedField.setEditable(true);
        seedField.setBackground(aFrame.getBackground());
        seedField.setFont(font2);

        panel_title = new JPanel();
        label_title = new JLabel(titleString);
        label_title.setFont(font1);
        panel_title.add(label_title);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
        label_gdv_traf_hdway_dist = new JLabel("1. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST]);
        label_gdv_traf_hdway_vol = new JLabel("2. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]);
        label_gdv_traf_hdway_par = new JLabel("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR]);
        label_gdv_traf_hdway_mean = new JLabel("4. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]);
        label_gdv_traf_hdway_85th = new JLabel("5. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]);
        label_gdv_traf_hdway_tm = new JLabel("6. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM]);
        label_gdv_traf_hdway_seed = new JLabel("7. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]);

        comboBox_gdv_traf_hdway_dist = new JComboBox(array_gdv_traf_hdway_dist);
        comboBox_gdv_traf_hdway_vol = new JComboBox(array_gdv_traf_hdway_vol);
        comboBox_gdv_traf_hdway_par = new JComboBox(array_gdv_hdway_param_snegexp);
        comboBox_gdv_traf_hdway_mean = new JComboBox(array_gdv_traf_hdway_mean);
        comboBox_gdv_traf_hdway_85th = new JComboBox(array_gdv_traf_hdway_85th);
        comboBox_gdv_traf_hdway_tm = new JComboBox(array_gdv_traf_hdway_tm);

        if (gdvsim.gclv_inter.mboa_Leg_Inbound_Data_OK[leg_number]) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_gdv_traf_hdway_dist.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST]);
            }
            else {
                comboBox_gdv_traf_hdway_dist.setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_dist);
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_gdv_traf_hdway_vol.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]));
            }
            else {
                comboBox_gdv_traf_hdway_vol.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_vol));
            }

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR]));

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_gdv_traf_hdway_mean.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]));
            }
            else {
                comboBox_gdv_traf_hdway_mean.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_gdv_traf_hdway_85th.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]));
            }
            else {
                comboBox_gdv_traf_hdway_85th.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th));
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_gdv_traf_hdway_tm.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM]);
            }
            else {
                comboBox_gdv_traf_hdway_tm.setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_tm);
            }

            if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                seedField.setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]));
            }
            else {
                seedField.setText(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_seed));
            }
        }
        else {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

            comboBox_gdv_traf_hdway_dist.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST]);
            comboBox_gdv_traf_hdway_vol.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]));
            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR]));
            comboBox_gdv_traf_hdway_mean.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]));
            comboBox_gdv_traf_hdway_85th.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]));
            comboBox_gdv_traf_hdway_tm.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM]);
            seedField.setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]));
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

        // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
        gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_gdv_traf_hdway_dist.getSelectedItem().toString());
        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
            return;

        if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfConstanArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_constan[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]);
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfErlangArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_erlang[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]);
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfGammaArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_gamma[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]);
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfLognrmlArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_lognrml[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]);
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfNegexpArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_negexp[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]);
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfSnegexpArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_snegexp[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]);
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
            comboBox_gdv_traf_hdway_par.removeAllItems();

            for (arrayIndex = 0; arrayIndex < SizeOfUniformArray; arrayIndex++)
                comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_uniform[arrayIndex]);

            comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par));
            label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]);
        }

        if (comboBox_gdv_traf_hdway_tm.getSelectedItem().toString().equals("YES")) {
            trafficMixDataButton.setEnabled(true);
        }
        else {
            trafficMixDataButton.setEnabled(false);
        }

        JPanel panel_mix = new JPanel();
        panel_mix.add(comboBox_gdv_traf_hdway_tm);
        panel_mix.add(trafficMixDataButton);

        JPanel panel_seed = new JPanel();
        panel_seed.add(seedField);
        panel_seed.add(seedButton);

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        setAccessibility();

        seedButton.setMnemonic(KeyEvent.VK_R);
        trafficMixDataButton.setMnemonic(KeyEvent.VK_E);
        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        distributionActionListener = new DistributionActionListener();
        distributionKeyListener = new DistributionKeyListener();
        trafficMixComboBoxActionListener = new TrafficMixComboBoxActionListener();
        trafficMixComboBoxKeyListener = new TrafficMixComboBoxKeyListener();
        trafficMixButtonActionListener = new TrafficMixButtonActionListener();
        trafficMixButtonKeyListener = new TrafficMixButtonKeyListener();
        seedButtonActionListener = new SeedButtonActionListener();
        seedButtonKeyListener = new SeedButtonKeyListener();
        seedDigitalListener = new SeedDigitalListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();
        helpListener = new HelpListener();
        openComboMenuListener = new OpenComboMenuListener();

        comboBox_gdv_traf_hdway_dist.addActionListener(distributionActionListener);
        comboBox_gdv_traf_hdway_dist.addKeyListener(distributionKeyListener);
        comboBox_gdv_traf_hdway_tm.addActionListener(trafficMixComboBoxActionListener);
        comboBox_gdv_traf_hdway_tm.addKeyListener(trafficMixComboBoxKeyListener);

        trafficMixDataButton.addActionListener(trafficMixButtonActionListener);
        trafficMixDataButton.addKeyListener(trafficMixButtonKeyListener);

        seedButton.addActionListener(seedButtonActionListener);
        seedButton.addKeyListener(seedButtonKeyListener);
        seedField.addActionListener(seedDigitalListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        comboBox_gdv_traf_hdway_dist.addKeyListener(openComboMenuListener);
        comboBox_gdv_traf_hdway_vol.addKeyListener(openComboMenuListener);
        comboBox_gdv_traf_hdway_par.addKeyListener(openComboMenuListener);
        comboBox_gdv_traf_hdway_mean.addKeyListener(openComboMenuListener);
        comboBox_gdv_traf_hdway_85th.addKeyListener(openComboMenuListener);
        comboBox_gdv_traf_hdway_tm.addKeyListener(openComboMenuListener);

        comboBox_gdv_traf_hdway_dist.addKeyListener(helpListener);
        comboBox_gdv_traf_hdway_vol.addKeyListener(helpListener);
        comboBox_gdv_traf_hdway_par.addKeyListener(helpListener);
        comboBox_gdv_traf_hdway_mean.addKeyListener(helpListener);
        comboBox_gdv_traf_hdway_85th.addKeyListener(helpListener);
        comboBox_gdv_traf_hdway_tm.addKeyListener(helpListener);

        seedField.addKeyListener(helpListener);
        trafficMixDataButton.addKeyListener(helpListener);
        seedButton.addKeyListener(helpListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.insets = new Insets(5, 0, 5, 0);

        addComponent(panel_title, 0, 0, 3, 1);
        addComponent(label_gdv_traf_hdway_dist, 1, 1, 1, 1);
        addComponent(label_gdv_traf_hdway_vol, 2, 1, 1, 1);
        addComponent(label_gdv_traf_hdway_par, 3, 1, 1, 1);
        addComponent(label_gdv_traf_hdway_mean, 4, 1, 1, 1);
        addComponent(label_gdv_traf_hdway_85th, 5, 1, 1, 1);
        addComponent(label_gdv_traf_hdway_tm, 6, 1, 1, 1);
        addComponent(label_gdv_traf_hdway_seed, 7, 1, 1, 1);

        gbConstraints.insets = new Insets(5, 15, 5, 15);
        addComponent(comboBox_gdv_traf_hdway_dist, 1, 0, 1, 1);
        addComponent(comboBox_gdv_traf_hdway_vol, 2, 0, 1, 1);
        addComponent(comboBox_gdv_traf_hdway_par, 3, 0, 1, 1);
        addComponent(comboBox_gdv_traf_hdway_mean, 4, 0, 1, 1);
        addComponent(comboBox_gdv_traf_hdway_85th, 5, 0, 1, 1);

        gbConstraints.insets = new Insets(0, 7, 0, 7);
        addComponent(panel_mix, 6, 0, 1, 1);
        addComponent(panel_seed, 7, 0, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 0);
        addComponent(ok_panel, 9, 0, 3, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method LegInboundDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    void setAccessibility() {

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        comboBox_gdv_traf_hdway_dist.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_dist.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_vol.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_vol.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_par.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_par.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_mean.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_mean.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_85th.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_85th.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_tm.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] + " for Leg " + leg_number);
        comboBox_gdv_traf_hdway_tm.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] + " for Leg " + leg_number);

        trafficMixDataButton.getAccessibleContext().setAccessibleName("Traffic Mix Data");
        trafficMixDataButton.getAccessibleContext().setAccessibleDescription("Traffic Mix Data");

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");
    }

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

    class DistributionActionListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

            // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
            gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_gdv_traf_hdway_dist.getSelectedItem().toString());
            if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                return;

            if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfConstanArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_constan[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]);
            }
            else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfErlangArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_erlang[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]);
            }
            else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfGammaArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_gamma[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]);
            }
            else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfLognrmlArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_lognrml[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]);
            }
            else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfNegexpArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_negexp[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]);
            }
            else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfSnegexpArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_snegexp[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]);
            }
            else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                comboBox_gdv_traf_hdway_par.removeAllItems();

                for (arrayIndex = 0; arrayIndex < SizeOfUniformArray; arrayIndex++)
                    comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_uniform[arrayIndex]);

                comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]);
            }

        } // end of method actionPerformed
    } // end of class DistributionActionListener

    class DistributionKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
                gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_gdv_traf_hdway_dist.getSelectedItem().toString());
                if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                    return;

                if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfConstanArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_constan[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]);
                }
                else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfErlangArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_erlang[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]);
                }
                else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfGammaArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_gamma[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]);
                }
                else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfLognrmlArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_lognrml[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]);
                }
                else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfNegexpArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_negexp[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]);
                }
                else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfSnegexpArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_snegexp[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]);
                }
                else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                    comboBox_gdv_traf_hdway_par.removeAllItems();

                    for (arrayIndex = 0; arrayIndex < SizeOfUniformArray; arrayIndex++)
                        comboBox_gdv_traf_hdway_par.addItem(array_gdv_hdway_param_uniform[arrayIndex]);

                    comboBox_gdv_traf_hdway_par.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                    label_gdv_traf_hdway_par.setText("3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]);
                }
            }
        } // end of method keyPressed
    } // end of class DistributionKeyListener

    class TrafficMixComboBoxActionListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            if (comboBox_gdv_traf_hdway_tm.getSelectedItem().toString().equals("YES")) {
                trafficMixDataButton.setEnabled(true);

                new TrafficMixDialog(leg_number, 0);
            }
            else {
                trafficMixDataButton.setEnabled(false);
            }
        } // end of method actionPerformed
    } // end of class TrafficMixComboBoxActionListener

    class TrafficMixComboBoxKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (comboBox_gdv_traf_hdway_tm.getSelectedItem().toString().equals("YES")) {
                    trafficMixDataButton.setEnabled(true);

                    new TrafficMixDialog(leg_number, 0);
                }
                else {
                    trafficMixDataButton.setEnabled(false);
                }
            }
        } // end of method keyPressed
    } // end of class TrafficMixComboBoxKeyListener

    class TrafficMixButtonActionListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            new TrafficMixDialog(leg_number, 0);

        } // end of method actionPerformed
    } // end of class TrafficMixButtonActionListener

    class TrafficMixButtonKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                new TrafficMixDialog(leg_number, 0);
            }
        } // end of method keyPressed
    } // end of class TrafficMixButtonKeyListener

    class SeedButtonActionListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];

            randomSeed = lsiv_min + (int)(Math.random() * lsiv_max);

            seedField.setText(Integer.toString(randomSeed));

        } // end of method actionPerformed
    } // end of SeedButtonActionListener

    class SeedButtonKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];

                randomSeed = lsiv_min + (int)(Math.random() * lsiv_max);

                seedField.setText(Integer.toString(randomSeed));
            }

        } // end of method keyPressed
    } // end of SeedButtonKeyListener

    class SeedDigitalListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            boolean isDigit = true;
            String s;
            s = seedField.getText().toString().trim();

            if (s.length() > 5) {
                isDigit = false;
            }

            for (int i = 0; i < s.length(); i++) {
                if (!Character.isDigit(s.charAt(i))) {
                    isDigit = false;
                    break;
                }
            }

            if (!isDigit) {
                JOptionPane.showMessageDialog(null, "Please enter a number between 0 and 99999.", "Error Message", JOptionPane.ERROR_MESSAGE);
            }
        }
    } // end of SeedDigitalListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == comboBox_gdv_traf_hdway_dist) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] + " for Leg " + leg_number, lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST],
                            comboBox_gdv_traf_hdway_dist.getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_gdv_traf_hdway_vol) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] + " for Leg " + leg_number, lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL],
                            comboBox_gdv_traf_hdway_vol.getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]));
                }
                else if (event.getSource() == comboBox_gdv_traf_hdway_mean) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] + " for Leg " + leg_number, lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN],
                            comboBox_gdv_traf_hdway_mean.getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]));

                }
                else if (event.getSource() == comboBox_gdv_traf_hdway_85th) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] + " for Leg " + leg_number, lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH],
                            comboBox_gdv_traf_hdway_85th.getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]));
                }
                else if (event.getSource() == comboBox_gdv_traf_hdway_tm) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] + " for Leg " + leg_number, lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM],
                            comboBox_gdv_traf_hdway_tm.getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM], " ", " ", " ");
                }
                else if (event.getSource() == comboBox_gdv_traf_hdway_par) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                    // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
                    gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_gdv_traf_hdway_dist.getSelectedItem().toString());
                    if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                        return;

                    if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " for Leg " + leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], comboBox_gdv_traf_hdway_par.getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                    }
                }
                else if (event.getSource() == seedField) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED] + " for Leg " + leg_number, lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED],
                            seedField.getText().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]));
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
                else if (event.getSource() == trafficMixDataButton) {
                    String s;
                    s = "This item defines whether User-Defined Traffic Mix Data (YES) or Default Traffic Mix Data (NO) is used.  Changing this item from NO to YES will automatically open the Traffic Mix Data window.  Changing this item from YES to NO will reset the Traffic Mix Data values to their defaults.  If the value of this item is YES, then the user can press the Edit Traffic Mix Data button to re-open the Traffic Mix Data window to make further changes.  Traffic Mix Data defines the percentage of each vehicle class which makes up the inbound traffic for this leg.  The default "
                            + PARAMS.TEXAS_MODEL_NVCD
                            + " vehicle classes are:  1 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[1]
                            + ", 2 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[2]
                            + ", 3 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[3]
                            + ", 4 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[4]
                            + ", 5 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[5]
                            + ", 6 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[6]
                            + ", 7 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[7]
                            + ", 8 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[8]
                            + ", 9 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[9]
                            + ", 10 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[10]
                            + ", 11 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[11]
                            + ", and 12 = "
                            + gdvsim.gclv_inter.msta_vehicle_class[12]
                            + ".  Vehicle class attributes are Maximum Acceleration, Maximum Deceleration, Maximum Velocity, Length, Minimum Inside Turning Radius, and Vehicle Operational Factor.  DVPRO stochastically generates vehicle class for a driver-vehicle unit using these percentages.  The percentage of each vehicle class actually simulated by SIMPRO may not be the same as this input value because of the stochastic generation process in DVPRO and vehicles being eliminated when a lane becomes full in SIMPRO.";

                    new HelpDialog(true, "Edit Traffic Mix Data Button", s, " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == seedButton) {
                    new HelpDialog(true, "Random Seed Button", "Random Seed Button creates a random number.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }
    }

    void saveData() {
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_dist = comboBox_gdv_traf_hdway_dist.getSelectedItem().toString();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_vol = Integer.valueOf(comboBox_gdv_traf_hdway_vol.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par = Double.valueOf(comboBox_gdv_traf_hdway_par.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean = Double.valueOf(comboBox_gdv_traf_hdway_mean.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th = Double.valueOf(comboBox_gdv_traf_hdway_85th.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_tm = comboBox_gdv_traf_hdway_tm.getSelectedItem().toString();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_seed = Integer.valueOf(seedField.getText().toString().trim()).intValue();

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] = gdvsim.gclv_inter.TX_FROM_USER;
    }

    void setTrafficMixDataInvalid() {
        gdvsim.gclv_inter.mboa_Leg_Traffic_Mix_OK[leg_number] = false;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

        for (int lsiv_traf = 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_traf - 1] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                    + lsiv_traf];
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                    + lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    }

    boolean isError() {
        String s;
        s = seedField.getText().trim().toString();

        boolean isDigit = true;

        if (s.length() > 5) {
            isDigit = false;
        }

        for (int i = 0; i < s.length(); i++) {
            if (!Character.isDigit(s.charAt(i))) {
                isDigit = false;
                break;
            }
        }

        if (isDigit) {
            return false;
        }
        else {
            JOptionPane.showMessageDialog(null, "Please enter a number between 0 and 99999.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    if (comboBox_gdv_traf_hdway_tm.getSelectedItem().toString().equals("NO")) {
                        // setTrafficMixDataInvalid();
                    }

                    saveData();

                    gdvsim.gclv_inter.mboa_Leg_Inbound_Data_OK[leg_number] = true;

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
                        if (comboBox_gdv_traf_hdway_tm.getSelectedItem().toString().equals("NO")) {
                            // setTrafficMixDataInvalid();
                        }

                        saveData();

                        gdvsim.gclv_inter.mboa_Leg_Inbound_Data_OK[leg_number] = true;

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            } // end of method keyPressed
        }
    }// end of OKApplyButtonKeyListener

} // end of class LegInboundDialog
