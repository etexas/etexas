package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             LegSDRDialog.java                              */
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

class LegSDRDialog extends JDialog {

    JFrame aFrame;

    int local_SDR_legnumber[][] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

    int local_SDR_setback[][] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

    int local_SDR_offset[][] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_SDR_1, label_SDR_2, label_SDR_3, label_SDR_4, label_total_leg1, label_total_leg2, label_total_leg3, label_total_leg4, label_total_leg5, label_total_leg6, label_total;

    JComboBox cbo_total_leg1, cbo_total_leg2, cbo_total_leg3, cbo_total_leg4, cbo_total_leg5, cbo_total_leg6, cbo_total;

    JComboBox comboBox_gdv_sdr_data_leg_number_1, comboBox_gdv_sdr_data_leg_number_2, comboBox_gdv_sdr_data_leg_number_3, comboBox_gdv_sdr_data_leg_number_4, comboBox_gdv_sdr_data_setback_1,
            comboBox_gdv_sdr_data_setback_2, comboBox_gdv_sdr_data_setback_3, comboBox_gdv_sdr_data_setback_4, comboBox_gdv_sdr_data_offset_1, comboBox_gdv_sdr_data_offset_2,
            comboBox_gdv_sdr_data_offset_3, comboBox_gdv_sdr_data_offset_4;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    CboSetbackActionListener cboSetbackActionListener;

    CboSetbackKeyListener cboSetbackKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    TX_Fmt lclv_tx_fmt;

    int numOfSight, numOfLeg1, numOfLeg2, numOfLeg3, numOfLeg4, numOfLeg5, numOfLeg6, number_of_legs, local_leg_number;

    String titleString;

    Font font1 = new Font("TimesRoman", Font.BOLD, 20), font2 = new Font("TimesRoman", Font.PLAIN, 16);

    public LegSDRDialog(int leg_number) {
        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;
        int SizeOfArray, intArrayElementValue, ArrayIndex;

        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        local_leg_number = leg_number;

        titleString = "Sight Distance Restriction Data for Leg " + local_leg_number;
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

        for (int lsiv_leg = 0; lsiv_leg < 6; lsiv_leg++) {
            for (int lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
                local_SDR_legnumber[lsiv_leg][lsiv_sdr] = gdvsim.SDR_legnumber[lsiv_leg][lsiv_sdr];
                local_SDR_offset[lsiv_leg][lsiv_sdr] = gdvsim.SDR_offset[lsiv_leg][lsiv_sdr];
                local_SDR_setback[lsiv_leg][lsiv_sdr] = gdvsim.SDR_setback[lsiv_leg][lsiv_sdr];
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        label_SDR_1 = new JLabel("SDR 1:");
        label_SDR_2 = new JLabel("SDR 2:");
        label_SDR_3 = new JLabel("SDR 3:");
        label_SDR_4 = new JLabel("SDR 4:");

        label_SDR_1.setFont(font2);
        label_SDR_2.setFont(font2);
        label_SDR_3.setFont(font2);
        label_SDR_4.setFont(font2);

        JLabel label_gdv_sdr_data_leg_number = new JLabel();
        JLabel label_gdv_sdr_data_setback = new JLabel();
        JLabel label_gdv_sdr_data_offset = new JLabel();

        label_gdv_sdr_data_setback.setFont(font2);
        label_gdv_sdr_data_offset.setFont(font2);

        label_gdv_sdr_data_leg_number.setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_LEG_NUMBER]);
        label_gdv_sdr_data_setback.setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]);
        label_gdv_sdr_data_offset.setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]);

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_LEG_NUMBER];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_LEG_NUMBER];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_LEG_NUMBER];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_sdr_data_leg_number = new String[SizeOfArray];

        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_sdr_data_leg_number[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_sdr_data_setback = new String[SizeOfArray];

        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_sdr_data_setback[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_gdv_sdr_data_offset = new String[SizeOfArray];

        for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
            array_gdv_sdr_data_offset[ArrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        comboBox_gdv_sdr_data_leg_number_1 = new JComboBox(array_gdv_sdr_data_leg_number);
        comboBox_gdv_sdr_data_leg_number_2 = new JComboBox(array_gdv_sdr_data_leg_number);
        comboBox_gdv_sdr_data_leg_number_3 = new JComboBox(array_gdv_sdr_data_leg_number);
        comboBox_gdv_sdr_data_leg_number_4 = new JComboBox(array_gdv_sdr_data_leg_number);

        comboBox_gdv_sdr_data_setback_1 = new JComboBox(array_gdv_sdr_data_setback);
        comboBox_gdv_sdr_data_setback_2 = new JComboBox(array_gdv_sdr_data_setback);
        comboBox_gdv_sdr_data_setback_3 = new JComboBox(array_gdv_sdr_data_setback);
        comboBox_gdv_sdr_data_setback_4 = new JComboBox(array_gdv_sdr_data_setback);

        comboBox_gdv_sdr_data_offset_1 = new JComboBox(array_gdv_sdr_data_offset);
        comboBox_gdv_sdr_data_offset_2 = new JComboBox(array_gdv_sdr_data_offset);
        comboBox_gdv_sdr_data_offset_3 = new JComboBox(array_gdv_sdr_data_offset);
        comboBox_gdv_sdr_data_offset_4 = new JComboBox(array_gdv_sdr_data_offset);

        comboBox_gdv_sdr_data_leg_number_1.setSelectedItem(Integer.toString(local_leg_number));
        comboBox_gdv_sdr_data_leg_number_2.setSelectedItem(Integer.toString(local_leg_number));
        comboBox_gdv_sdr_data_leg_number_3.setSelectedItem(Integer.toString(local_leg_number));
        comboBox_gdv_sdr_data_leg_number_4.setSelectedItem(Integer.toString(local_leg_number));

        if (local_SDR_setback[local_leg_number - 1][0] != 0)
            comboBox_gdv_sdr_data_setback_1.setSelectedItem(Integer.toString(local_SDR_setback[local_leg_number - 1][0]));
        else
            comboBox_gdv_sdr_data_setback_1.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));

        if (local_SDR_offset[local_leg_number - 1][0] != 0)
            comboBox_gdv_sdr_data_offset_1.setSelectedItem(Integer.toString(local_SDR_offset[local_leg_number - 1][0]));
        else
            comboBox_gdv_sdr_data_offset_1.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));

        if (local_SDR_setback[local_leg_number - 1][1] != 0)
            comboBox_gdv_sdr_data_setback_2.setSelectedItem(Integer.toString(local_SDR_setback[local_leg_number - 1][1]));
        else
            comboBox_gdv_sdr_data_setback_2.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));

        if (local_SDR_offset[local_leg_number - 1][1] != 0)
            comboBox_gdv_sdr_data_offset_2.setSelectedItem(Integer.toString(local_SDR_offset[local_leg_number - 1][1]));
        else
            comboBox_gdv_sdr_data_offset_2.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));

        comboBox_gdv_sdr_data_leg_number_3.setSelectedItem(Integer.toString(local_leg_number));

        if (local_SDR_setback[local_leg_number - 1][2] != 0)
            comboBox_gdv_sdr_data_setback_3.setSelectedItem(Integer.toString(local_SDR_setback[local_leg_number - 1][2]));
        else
            comboBox_gdv_sdr_data_setback_3.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));

        if (local_SDR_offset[local_leg_number - 1][2] != 0)
            comboBox_gdv_sdr_data_offset_3.setSelectedItem(Integer.toString(local_SDR_offset[local_leg_number - 1][2]));
        else
            comboBox_gdv_sdr_data_offset_3.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));

        comboBox_gdv_sdr_data_leg_number_4.setSelectedItem(Integer.toString(local_leg_number));

        if (local_SDR_setback[local_leg_number - 1][3] != 0)
            comboBox_gdv_sdr_data_setback_4.setSelectedItem(Integer.toString(local_SDR_setback[local_leg_number - 1][3]));
        else
            comboBox_gdv_sdr_data_setback_4.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));

        if (local_SDR_offset[local_leg_number - 1][3] != 0)
            comboBox_gdv_sdr_data_offset_4.setSelectedItem(Integer.toString(local_SDR_offset[local_leg_number - 1][3]));
        else
            comboBox_gdv_sdr_data_offset_4.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));

        label_total_leg1 = new JLabel("Number of SDR Points for Leg 1  ");
        label_total_leg2 = new JLabel("Number of SDR Points for leg 2  ");
        label_total_leg3 = new JLabel("Number of SDR Points for leg 3  ");
        label_total_leg4 = new JLabel("Number of SDR Points for leg 4  ");
        label_total_leg5 = new JLabel("Number of SDR Points for leg 5  ");
        label_total_leg6 = new JLabel("Number of SDR Points for leg 6  ");
        label_total = new JLabel("Total Number of SDR Points      ");

        cbo_total_leg1 = new JComboBox();
        cbo_total_leg2 = new JComboBox();
        cbo_total_leg3 = new JComboBox();
        cbo_total_leg4 = new JComboBox();
        cbo_total_leg5 = new JComboBox();
        cbo_total_leg6 = new JComboBox();
        cbo_total = new JComboBox();

        JPanel panel_total_leg1 = new JPanel();
        JPanel panel_total_leg2 = new JPanel();
        JPanel panel_total_leg3 = new JPanel();
        JPanel panel_total_leg4 = new JPanel();
        JPanel panel_total_leg5 = new JPanel();
        JPanel panel_total_leg6 = new JPanel();
        JPanel panel_total = new JPanel();

        calculateSDRpoints();

        cbo_total_leg1.addItem(Integer.toString(numOfLeg1));
        cbo_total_leg2.addItem(Integer.toString(numOfLeg2));
        cbo_total_leg3.addItem(Integer.toString(numOfLeg3));
        cbo_total_leg4.addItem(Integer.toString(numOfLeg4));
        cbo_total_leg5.addItem(Integer.toString(numOfLeg5));
        cbo_total_leg6.addItem(Integer.toString(numOfLeg6));
        cbo_total.addItem(Integer.toString(numOfSight));

        panel_total_leg1.add(label_total_leg1);
        panel_total_leg2.add(label_total_leg2);
        panel_total_leg3.add(label_total_leg3);
        panel_total_leg4.add(label_total_leg4);
        panel_total_leg5.add(label_total_leg5);
        panel_total_leg6.add(label_total_leg6);
        panel_total.add(label_total);

        panel_total_leg1.add(cbo_total_leg1);
        panel_total_leg2.add(cbo_total_leg2);
        panel_total_leg3.add(cbo_total_leg3);
        panel_total_leg4.add(cbo_total_leg4);
        panel_total_leg5.add(cbo_total_leg5);
        panel_total_leg6.add(cbo_total_leg6);
        panel_total.add(cbo_total);

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        cboSetbackActionListener = new CboSetbackActionListener();
        cboSetbackKeyListener = new CboSetbackKeyListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        comboBox_gdv_sdr_data_setback_1.addKeyListener(openComboMenuListener);
        comboBox_gdv_sdr_data_setback_2.addKeyListener(openComboMenuListener);
        comboBox_gdv_sdr_data_setback_3.addKeyListener(openComboMenuListener);
        comboBox_gdv_sdr_data_setback_4.addKeyListener(openComboMenuListener);

        comboBox_gdv_sdr_data_offset_1.addKeyListener(openComboMenuListener);
        comboBox_gdv_sdr_data_offset_2.addKeyListener(openComboMenuListener);
        comboBox_gdv_sdr_data_offset_3.addKeyListener(openComboMenuListener);
        comboBox_gdv_sdr_data_offset_4.addKeyListener(openComboMenuListener);

        cbo_total_leg1.addKeyListener(openComboMenuListener);
        cbo_total_leg2.addKeyListener(openComboMenuListener);
        cbo_total_leg3.addKeyListener(openComboMenuListener);
        cbo_total_leg4.addKeyListener(openComboMenuListener);
        cbo_total_leg5.addKeyListener(openComboMenuListener);
        cbo_total_leg6.addKeyListener(openComboMenuListener);
        cbo_total.addKeyListener(openComboMenuListener);

        comboBox_gdv_sdr_data_setback_1.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_setback_2.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_setback_3.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_setback_4.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_offset_1.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_offset_2.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_offset_3.addKeyListener(helpListener);
        comboBox_gdv_sdr_data_offset_4.addKeyListener(helpListener);

        cbo_total_leg1.addKeyListener(helpListener);
        cbo_total_leg2.addKeyListener(helpListener);
        cbo_total_leg3.addKeyListener(helpListener);
        cbo_total_leg4.addKeyListener(helpListener);
        cbo_total_leg5.addKeyListener(helpListener);
        cbo_total_leg6.addKeyListener(helpListener);
        cbo_total.addKeyListener(helpListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        comboBox_gdv_sdr_data_setback_1.addActionListener(cboSetbackActionListener);
        comboBox_gdv_sdr_data_setback_2.addActionListener(cboSetbackActionListener);
        comboBox_gdv_sdr_data_setback_3.addActionListener(cboSetbackActionListener);
        comboBox_gdv_sdr_data_setback_4.addActionListener(cboSetbackActionListener);

        comboBox_gdv_sdr_data_setback_1.addKeyListener(cboSetbackKeyListener);
        comboBox_gdv_sdr_data_setback_2.addKeyListener(cboSetbackKeyListener);
        comboBox_gdv_sdr_data_setback_3.addKeyListener(cboSetbackKeyListener);
        comboBox_gdv_sdr_data_setback_4.addKeyListener(cboSetbackKeyListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        comboBox_gdv_sdr_data_setback_1.getAccessibleContext().setAccessibleName(label_SDR_1.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_1.getAccessibleContext().setAccessibleDescription(label_SDR_1.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_2.getAccessibleContext().setAccessibleName(label_SDR_2.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_2.getAccessibleContext().setAccessibleDescription(label_SDR_2.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_3.getAccessibleContext().setAccessibleName(label_SDR_3.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_3.getAccessibleContext().setAccessibleDescription(label_SDR_3.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_4.getAccessibleContext().setAccessibleName(label_SDR_4.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_setback_4.getAccessibleContext().setAccessibleDescription(label_SDR_4.getText() + " " + label_gdv_sdr_data_setback + " for leg " + local_leg_number);

        comboBox_gdv_sdr_data_offset_1.getAccessibleContext().setAccessibleName(label_SDR_1.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_1.getAccessibleContext().setAccessibleDescription(label_SDR_1.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_2.getAccessibleContext().setAccessibleName(label_SDR_2.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_2.getAccessibleContext().setAccessibleDescription(label_SDR_2.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_3.getAccessibleContext().setAccessibleName(label_SDR_3.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_3.getAccessibleContext().setAccessibleDescription(label_SDR_3.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_4.getAccessibleContext().setAccessibleName(label_SDR_4.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);
        comboBox_gdv_sdr_data_offset_4.getAccessibleContext().setAccessibleDescription(label_SDR_4.getText() + " " + label_gdv_sdr_data_offset + " for leg " + local_leg_number);

        cbo_total_leg1.getAccessibleContext().setAccessibleName(label_total_leg1.getText());
        cbo_total_leg1.getAccessibleContext().setAccessibleDescription(label_total_leg1.getText());
        cbo_total_leg2.getAccessibleContext().setAccessibleName(label_total_leg2.getText());
        cbo_total_leg2.getAccessibleContext().setAccessibleDescription(label_total_leg2.getText());
        cbo_total_leg3.getAccessibleContext().setAccessibleName(label_total_leg3.getText());
        cbo_total_leg3.getAccessibleContext().setAccessibleDescription(label_total_leg3.getText());
        cbo_total_leg4.getAccessibleContext().setAccessibleName(label_total_leg4.getText());
        cbo_total_leg4.getAccessibleContext().setAccessibleDescription(label_total_leg4.getText());
        cbo_total_leg5.getAccessibleContext().setAccessibleName(label_total_leg5.getText());
        cbo_total_leg5.getAccessibleContext().setAccessibleDescription(label_total_leg5.getText());
        cbo_total_leg6.getAccessibleContext().setAccessibleName(label_total_leg6.getText());
        cbo_total_leg6.getAccessibleContext().setAccessibleDescription(label_total_leg6.getText());
        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

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

        int iRow = 0;
        gbConstraints.insets = new Insets(0, 0, 0, 0);
        addComponent(panel_title, iRow++, 0, 4, 1);

        gbConstraints.insets = new Insets(5, 5, 5, 5);
        addComponent(label_gdv_sdr_data_setback, iRow, 1, 1, 1);
        addComponent(label_gdv_sdr_data_offset, iRow++, 2, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 2);
        addComponent(label_SDR_1, iRow, 0, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 5);
        addComponent(comboBox_gdv_sdr_data_setback_1, iRow, 1, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 2);
        addComponent(comboBox_gdv_sdr_data_offset_1, iRow++, 2, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 2);
        addComponent(label_SDR_2, iRow, 0, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 5);
        addComponent(comboBox_gdv_sdr_data_setback_2, iRow, 1, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 2);
        addComponent(comboBox_gdv_sdr_data_offset_2, iRow++, 2, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 2);
        addComponent(label_SDR_3, iRow, 0, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 5);
        addComponent(comboBox_gdv_sdr_data_setback_3, iRow, 1, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 2);
        addComponent(comboBox_gdv_sdr_data_offset_3, iRow++, 2, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 2);
        addComponent(label_SDR_4, iRow, 0, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 5);
        addComponent(comboBox_gdv_sdr_data_setback_4, iRow, 1, 1, 1);
        gbConstraints.insets = new Insets(5, 5, 5, 2);
        addComponent(comboBox_gdv_sdr_data_offset_4, iRow++, 2, 1, 1);

        gbConstraints.insets = new Insets(0, 0, 0, 0);

        addComponent(panel_total_leg1, iRow++, 0, 3, 1);
        addComponent(panel_total_leg2, iRow++, 0, 3, 1);
        addComponent(panel_total_leg3, iRow++, 0, 3, 1);

        if (number_of_legs > 3) {
            addComponent(panel_total_leg4, iRow++, 0, 3, 1);
        }

        if (number_of_legs > 4) {
            addComponent(panel_total_leg5, iRow++, 0, 3, 1);
        }

        if (number_of_legs > 5) {
            addComponent(panel_total_leg6, iRow++, 0, 3, 1);
        }

        gbConstraints.insets = new Insets(2, 0, 2, 0);
        addComponent(panel_total, iRow++, 0, 3, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 0);
        addComponent(ok_panel, iRow++, 0, 4, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method LegSDRDialog

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

    void calculateSDRpoints() {
        int lsiv_sdr;
        numOfLeg1 = 0;
        for (lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            if (local_SDR_setback[0][lsiv_sdr] != 0)
                numOfLeg1++;
        }

        numOfLeg2 = 0;
        for (lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            if (local_SDR_setback[1][lsiv_sdr] != 0)
                numOfLeg2++;
        }

        numOfLeg3 = 0;
        for (lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            if (local_SDR_setback[2][lsiv_sdr] != 0)
                numOfLeg3++;
        }

        numOfLeg4 = 0;
        for (lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            if (local_SDR_setback[3][lsiv_sdr] != 0)
                numOfLeg4++;
        }

        numOfLeg5 = 0;
        for (lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            if (local_SDR_setback[4][lsiv_sdr] != 0)
                numOfLeg5++;
        }

        numOfLeg6 = 0;
        for (lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            if (local_SDR_setback[5][lsiv_sdr] != 0)
                numOfLeg6++;
        }

        numOfSight = numOfLeg1 + numOfLeg2 + numOfLeg3 + numOfLeg4 + numOfLeg5 + numOfLeg6;
    } // end of calculateSDRpoints()

    void setSDRpointInformation() {
        cbo_total_leg1.removeAllItems();
        cbo_total_leg2.removeAllItems();
        cbo_total_leg3.removeAllItems();
        cbo_total_leg4.removeAllItems();
        cbo_total_leg5.removeAllItems();
        cbo_total_leg6.removeAllItems();
        cbo_total.removeAllItems();
        cbo_total_leg1.addItem(Integer.toString(numOfLeg1));
        cbo_total_leg2.addItem(Integer.toString(numOfLeg2));
        cbo_total_leg3.addItem(Integer.toString(numOfLeg3));
        cbo_total_leg4.addItem(Integer.toString(numOfLeg4));
        cbo_total_leg5.addItem(Integer.toString(numOfLeg5));
        cbo_total_leg6.addItem(Integer.toString(numOfLeg6));
        cbo_total.addItem(Integer.toString(numOfSight));
    } // end of setSDRpointInformation

    boolean setErrorInformation() {
        if (numOfSight <= PARAMS.TEXAS_MODEL_NSR) {
            return true;
        }
        else {
            JOptionPane.showMessageDialog(null, "Sum of SDR Points for all legs is " + numOfSight + ".\n Revise the data so that the sum is less than and equal to " + PARAMS.TEXAS_MODEL_NSR + ".",
                    "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }
    } // end of setErrorInformation

    class CboSetbackActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == comboBox_gdv_sdr_data_setback_1) {
                local_SDR_setback[local_leg_number - 1][0] = Integer.valueOf(comboBox_gdv_sdr_data_setback_1.getSelectedItem().toString()).intValue();

                calculateSDRpoints();
                setSDRpointInformation();

                if (!setErrorInformation()) {
                    comboBox_gdv_sdr_data_setback_1.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                    local_SDR_setback[local_leg_number - 1][0] = 0;
                }
            }

            if (event.getSource() == comboBox_gdv_sdr_data_setback_2) {
                local_SDR_setback[local_leg_number - 1][1] = Integer.valueOf(comboBox_gdv_sdr_data_setback_2.getSelectedItem().toString()).intValue();

                calculateSDRpoints();
                setSDRpointInformation();

                if (!setErrorInformation()) {
                    comboBox_gdv_sdr_data_setback_2.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                    local_SDR_setback[local_leg_number - 1][1] = 0;
                }
            }

            if (event.getSource() == comboBox_gdv_sdr_data_setback_3) {
                local_SDR_setback[local_leg_number - 1][2] = Integer.valueOf(comboBox_gdv_sdr_data_setback_3.getSelectedItem().toString()).intValue();

                calculateSDRpoints();
                setSDRpointInformation();

                if (!setErrorInformation()) {
                    comboBox_gdv_sdr_data_setback_3.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                    local_SDR_setback[local_leg_number - 1][2] = 0;
                }
            }

            if (event.getSource() == comboBox_gdv_sdr_data_setback_4) {
                local_SDR_setback[local_leg_number - 1][3] = Integer.valueOf(comboBox_gdv_sdr_data_setback_4.getSelectedItem().toString()).intValue();

                calculateSDRpoints();
                setSDRpointInformation();

                if (!setErrorInformation()) {
                    comboBox_gdv_sdr_data_setback_4.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                    local_SDR_setback[local_leg_number - 1][3] = 0;
                }
            }
        } // end of method actionPerformed
    } // end of class CboSetbackActionListener

    class CboSetbackKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == comboBox_gdv_sdr_data_setback_1) {
                    local_SDR_setback[local_leg_number - 1][0] = Integer.valueOf(comboBox_gdv_sdr_data_setback_1.getSelectedItem().toString()).intValue();
                }

                if (event.getSource() == comboBox_gdv_sdr_data_setback_2) {
                    local_SDR_setback[local_leg_number - 1][1] = Integer.valueOf(comboBox_gdv_sdr_data_setback_2.getSelectedItem().toString()).intValue();
                }

                if (event.getSource() == comboBox_gdv_sdr_data_setback_3) {
                    local_SDR_setback[local_leg_number - 1][2] = Integer.valueOf(comboBox_gdv_sdr_data_setback_3.getSelectedItem().toString()).intValue();
                }

                if (event.getSource() == comboBox_gdv_sdr_data_setback_4) {
                    local_SDR_setback[local_leg_number - 1][3] = Integer.valueOf(comboBox_gdv_sdr_data_setback_4.getSelectedItem().toString()).intValue();
                }

                calculateSDRpoints();
                setSDRpointInformation();
                setErrorInformation();

            }
        } // end of method keyPressed
    } // end of class CboSetbackKeyListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == comboBox_gdv_sdr_data_setback_1) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK] + " for Leg " + local_leg_number + " SDR 1",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], comboBox_gdv_sdr_data_setback_1.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_setback_2) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK] + " for Leg " + local_leg_number + " SDR 2",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], comboBox_gdv_sdr_data_setback_2.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_setback_3) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK] + " for Leg " + local_leg_number + " SDR 3",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], comboBox_gdv_sdr_data_setback_3.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_setback_4) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK] + " for Leg " + local_leg_number + " SDR 4",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK], comboBox_gdv_sdr_data_setback_4.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_offset_1) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET] + " for Leg " + local_leg_number + " SDR 1",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], comboBox_gdv_sdr_data_offset_1.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_offset_2) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET] + " for Leg " + local_leg_number + " SDR 2",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], comboBox_gdv_sdr_data_offset_2.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_offset_3) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET] + " for Leg " + local_leg_number + " SDR 3",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], comboBox_gdv_sdr_data_offset_3.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));
                }
                else if (event.getSource() == comboBox_gdv_sdr_data_offset_4) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], lclv_tx_fmt.mstv_name.replace("#", ""),
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET] + " for Leg " + local_leg_number + " SDR 4",
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET], comboBox_gdv_sdr_data_offset_4.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET]));
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
                else if (event.getSource() == cbo_total_leg1) {
                    new HelpDialog(true, label_total_leg1.getText(), label_total_leg1.getText(), "The user cannot specify this item.  This item is the " + label_total_leg1.getText()
                            + " and is determined by the program.", cbo_total_leg1.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
                else if (event.getSource() == cbo_total_leg2) {
                    new HelpDialog(true, label_total_leg2.getText(), label_total_leg2.getText(), "The user cannot specify this item.  This item is the " + label_total_leg2.getText()
                            + " and is determined by the program.", cbo_total_leg2.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
                else if (event.getSource() == cbo_total_leg3) {
                    new HelpDialog(true, label_total_leg3.getText(), label_total_leg3.getText(), "The user cannot specify this item.  This item is the " + label_total_leg3.getText()
                            + " and is determined by the program.", cbo_total_leg3.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
                else if (event.getSource() == cbo_total_leg4) {
                    new HelpDialog(true, label_total_leg4.getText(), label_total_leg4.getText(), "The user cannot specify this item.  This item is the " + label_total_leg4.getText()
                            + " and is determined by the program.", cbo_total_leg4.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
                else if (event.getSource() == cbo_total_leg5) {
                    new HelpDialog(true, label_total_leg5.getText(), label_total_leg5.getText(), "The user cannot specify this item.  This item is the " + label_total_leg5.getText()
                            + " and is determined by the program.", cbo_total_leg5.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
                else if (event.getSource() == cbo_total_leg6) {
                    new HelpDialog(true, label_total_leg6.getText(), label_total_leg6.getText(), "The user cannot specify this item.  This item is the " + label_total_leg6.getText()
                            + " and is determined by the program.", cbo_total_leg6.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the " + label_total.getText()
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NSR), "1");

                }
            }
        }
    } // end of helpListener()

    void saveData() {
        local_SDR_legnumber[local_leg_number - 1][0] = Integer.valueOf(comboBox_gdv_sdr_data_leg_number_1.getSelectedItem().toString()).intValue();
        local_SDR_legnumber[local_leg_number - 1][1] = Integer.valueOf(comboBox_gdv_sdr_data_leg_number_2.getSelectedItem().toString()).intValue();
        local_SDR_legnumber[local_leg_number - 1][2] = Integer.valueOf(comboBox_gdv_sdr_data_leg_number_3.getSelectedItem().toString()).intValue();
        local_SDR_legnumber[local_leg_number - 1][3] = Integer.valueOf(comboBox_gdv_sdr_data_leg_number_4.getSelectedItem().toString()).intValue();

        local_SDR_setback[local_leg_number - 1][0] = Integer.valueOf(comboBox_gdv_sdr_data_setback_1.getSelectedItem().toString()).intValue();
        local_SDR_setback[local_leg_number - 1][1] = Integer.valueOf(comboBox_gdv_sdr_data_setback_2.getSelectedItem().toString()).intValue();
        local_SDR_setback[local_leg_number - 1][2] = Integer.valueOf(comboBox_gdv_sdr_data_setback_3.getSelectedItem().toString()).intValue();
        local_SDR_setback[local_leg_number - 1][3] = Integer.valueOf(comboBox_gdv_sdr_data_setback_4.getSelectedItem().toString()).intValue();

        local_SDR_offset[local_leg_number - 1][0] = Integer.valueOf(comboBox_gdv_sdr_data_offset_1.getSelectedItem().toString()).intValue();
        local_SDR_offset[local_leg_number - 1][1] = Integer.valueOf(comboBox_gdv_sdr_data_offset_2.getSelectedItem().toString()).intValue();
        local_SDR_offset[local_leg_number - 1][2] = Integer.valueOf(comboBox_gdv_sdr_data_offset_3.getSelectedItem().toString()).intValue();
        local_SDR_offset[local_leg_number - 1][3] = Integer.valueOf(comboBox_gdv_sdr_data_offset_4.getSelectedItem().toString()).intValue();

        gdvsim.SDR_count[local_leg_number - 1] = 0;
        for (int lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
            gdvsim.SDR_legnumber[local_leg_number - 1][lsiv_sdr] = local_SDR_legnumber[local_leg_number - 1][lsiv_sdr];
            gdvsim.SDR_setback[local_leg_number - 1][lsiv_sdr] = local_SDR_setback[local_leg_number - 1][lsiv_sdr];
            gdvsim.SDR_offset[local_leg_number - 1][lsiv_sdr] = local_SDR_offset[local_leg_number - 1][lsiv_sdr];
            if (gdvsim.SDR_setback[local_leg_number - 1][lsiv_sdr] != 0)
                gdvsim.SDR_count[local_leg_number - 1]++;
        }

        for (int lsiv_sdr = 1; lsiv_sdr <= PARAMS.TEXAS_MODEL_NSR; lsiv_sdr++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset = 0;

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_LEG_NUMBER] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_SETBACK] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_DATA_OFFSET] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        boolean stop = false;
        int k = 0;
        for (int lsiv_leg = 0; lsiv_leg < number_of_legs; lsiv_leg++) {
            for (int lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
                if (gdvsim.SDR_setback[lsiv_leg][lsiv_sdr] != 0) {
                    if (k == PARAMS.TEXAS_MODEL_NSR) {
                        stop = true;
                        break;
                    }
                    else {
                        k++;
                    }
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[k].msiv_sdr_leg_number = gdvsim.SDR_legnumber[lsiv_leg][lsiv_sdr];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[k].msiv_sdr_setback = gdvsim.SDR_setback[lsiv_leg][lsiv_sdr];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[k].msiv_sdr_offset = gdvsim.SDR_offset[lsiv_leg][lsiv_sdr];
                }
            } // end for lsiv_sdr
            if (stop)
                break;
        } // end for lsiv_leg
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs = k;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    }

    void printGlobal() {
        if (gdvsim.gclv_inter.mbov_debug_menu)
            System.out.println("gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs = "
                    + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs);

        for (int lsiv_sdr = 1; lsiv_sdr <= PARAMS.TEXAS_MODEL_NSR; lsiv_sdr++) {
            if (gdvsim.gclv_inter.mbov_debug_menu)
                System.out.println("gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[" + lsiv_sdr + "].msiv_sdr_leg_number = "
                        + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number);

            if (gdvsim.gclv_inter.mbov_debug_menu)
                System.out.println("gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[" + lsiv_sdr + "].msiv_sdr_setback    = "
                        + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback);

            if (gdvsim.gclv_inter.mbov_debug_menu)
                System.out.println("gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[" + lsiv_sdr + "].msiv_sdr_offset     = "
                        + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset);
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton) {
                calculateSDRpoints();
                setSDRpointInformation();
                if (setErrorInformation()) {
                    saveData();
                    printGlobal();

                    gdvsim.gclv_inter.mboa_Leg_SDR_Data_OK[local_leg_number] = true;

                    gdvsim.myIntersection.setVisible(true);
                    gdvsim.myIntersection.draw();
                    aFrame.dispose();
                }
            }

            if (event.getSource() == applyButton) {
                calculateSDRpoints();
                setSDRpointInformation();

                if (setErrorInformation()) {
                    saveData();
                    printGlobal();

                    gdvsim.gclv_inter.mboa_Leg_SDR_Data_OK[local_leg_number] = true;
                    gdvsim.myIntersection.setVisible(true);
                    gdvsim.myIntersection.draw();
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
                if (event.getSource() == okButton) {
                    calculateSDRpoints();
                    setSDRpointInformation();
                    if (setErrorInformation()) {
                        saveData();
                        printGlobal();

                        gdvsim.gclv_inter.mboa_Leg_SDR_Data_OK[local_leg_number] = true;

                        gdvsim.myIntersection.setVisible(true);
                        gdvsim.myIntersection.draw();
                        aFrame.dispose();
                    }
                }

                if (event.getSource() == applyButton) {
                    calculateSDRpoints();
                    setSDRpointInformation();
                    if (setErrorInformation()) {
                        saveData();
                        printGlobal();

                        gdvsim.gclv_inter.mboa_Leg_SDR_Data_OK[local_leg_number] = true;
                        gdvsim.myIntersection.setVisible(true);
                        gdvsim.myIntersection.draw();
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of method keyPressed
    }// end of OkApplyKeyListener

} // end of class LegSDRDialog
