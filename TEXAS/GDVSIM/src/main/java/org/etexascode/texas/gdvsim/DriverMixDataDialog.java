package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                          DriverMixDataDialog.java                          */
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

class DriverMixDataDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    TotalActionListener totalActionListener;

    TotalKeyListener totalKeyListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JLabel label_title;

    JLabel[] label_drv_1 = new JLabel[PARAMS.TEXAS_MODEL_NDC + 2];

    JLabel[] label_drv_2 = new JLabel[PARAMS.TEXAS_MODEL_NDC + 2];

    JLabel[] label_veh = new JLabel[PARAMS.TEXAS_MODEL_NVC + 1];

    JComboBox[] cbo_total = new JComboBox[PARAMS.TEXAS_MODEL_NVC + 1];

    JComboBox[][] comboBox_mixData = new JComboBox[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_NDC + 1];

    JButton okButton, applyButton, cancelButton;

    JPanel panel_title, ok_panel;

    Font font1, font2;

    double total;

    TX_Fmt lclv_tx_fmt;

    DecimalFormat format1Dec = new DecimalFormat("0.0");

    int number_of_drivers, number_of_vehicles;

    String titleString;

    public DriverMixDataDialog() {
        number_of_drivers = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;
        number_of_vehicles = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
        titleString = "User-Defined Driver Mix Data";
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

        font1 = new Font("TimesRoman", Font.BOLD, 20);
        font2 = new Font("TimesRoman", Font.BOLD, 16);

        label_title = new JLabel("User-Defined Driver Mix Data");
        label_title.setFont(font1);

        panel_title = new JPanel();
        panel_title.add(label_title);

        label_drv_1[0] = new JLabel("Vehicle");
        label_drv_2[0] = new JLabel("Class");

        for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
            label_drv_1[lsiv_drv] = new JLabel("% of Drivers");
            label_drv_2[lsiv_drv] = new JLabel("  Class " + lsiv_drv);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];

        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            String desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh];
            int beg = desc.indexOf('(') + 1;
            int end = desc.indexOf(')') - 1;
            label_veh[lsiv_veh] = new JLabel(lsiv_veh + "   " + desc.substring(beg, end + 1));
        }

        label_drv_1[number_of_drivers + 1] = new JLabel("     Total Percent");
        label_drv_2[number_of_drivers + 1] = new JLabel("(should be 100%)");

        double ldfv_min;
        double ldfv_max;
        double ldfv_inc;

        int SizeOfArray, ArrayIndex;
        double doubleArrayElementValue;
        String s;

        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];

            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                ldfv_min = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv];
                ldfv_max = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv];
                ldfv_inc = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv];

                SizeOfArray = (int)((ldfv_max - ldfv_min) / ldfv_inc + 1.5);
                doubleArrayElementValue = ldfv_min;
                String[] array_mix = new String[SizeOfArray];

                for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                    array_mix[ArrayIndex] = format1Dec.format(doubleArrayElementValue);
                    doubleArrayElementValue += ldfv_inc;
                }
                comboBox_mixData[lsiv_veh][lsiv_drv] = new JComboBox(array_mix);

            } // end for lsiv_drv
        } // end for lsiv_veh

        setDefaultValue();

        if (gdvsim.flag_driverMixData_ok) {
            setValue();
        }

        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            cbo_total[lsiv_veh] = new JComboBox();
            cbo_total[lsiv_veh].addItem(format1Dec.format(getSum(lsiv_veh)));
        }

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        totalActionListener = new TotalActionListener();
        totalKeyListener = new TotalKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                comboBox_mixData[lsiv_veh][lsiv_drv].addActionListener(totalActionListener);
                comboBox_mixData[lsiv_veh][lsiv_drv].addKeyListener(totalKeyListener);
                comboBox_mixData[lsiv_veh][lsiv_drv].addKeyListener(openComboMenuListener);
                comboBox_mixData[lsiv_veh][lsiv_drv].addKeyListener(helpListener);
            }

            cbo_total[lsiv_veh].addKeyListener(helpListener);
            cbo_total[lsiv_veh].addKeyListener(openComboMenuListener);
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
        int iCol = number_of_drivers + 2;

        gbConstraints.insets = new Insets(2, 5, 2, 5);

        addComponent(panel_title, iRow, 0, iCol, 1);
        iRow++;

        for (int lsiv_drv = 0; lsiv_drv <= (number_of_drivers + 1); lsiv_drv++) {
            addComponent(label_drv_1[lsiv_drv], iRow, lsiv_drv, 1, 1);
        }
        iRow++;

        for (int lsiv_drv = 0; lsiv_drv <= (number_of_drivers + 1); lsiv_drv++) {
            addComponent(label_drv_2[lsiv_drv], iRow, lsiv_drv, 1, 1);
        }
        iRow++;

        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            addComponent(label_veh[lsiv_veh], iRow, 0, 1, 1);
            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                addComponent(comboBox_mixData[lsiv_veh][lsiv_drv], iRow, lsiv_drv, 1, 1);
            }
            addComponent(cbo_total[lsiv_veh], iRow, number_of_drivers + 1, 1, 1);
            iRow++;
        }

        addComponent(ok_panel, iRow, 0, iCol, 1);

        setAccessibility();

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method DriverMixDataDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    void setDefaultValue() {
        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
                comboBox_mixData[lsiv_veh][lsiv_drv].setSelectedItem(format1Dec.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]));

            } // end for lsiv_drv
        } // end for lsiv_veh
    }

    void setValue() {
        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                comboBox_mixData[lsiv_veh][lsiv_drv].setSelectedItem(format1Dec.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv]));
            }
        }
    }

    void setAccessibility() {
        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
                comboBox_mixData[lsiv_veh][lsiv_drv].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]);
                comboBox_mixData[lsiv_veh][lsiv_drv].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]);
            }

            cbo_total[lsiv_veh].getAccessibleContext().setAccessibleName(
                    "The user cannot specify this item.  This item is the Total Percent of Drivers for Vehicle " + lsiv_veh + " and is determined by the program.");
            cbo_total[lsiv_veh].getAccessibleContext().setAccessibleDescription(
                    "The user cannot specify this item.  This item is the Total Percent of Drivers for Vehicle " + lsiv_veh + " and is determined by the program.");
        }

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

    double getSum(int vehicleIndex) {
        double sum = 0;

        for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
            sum += Double.valueOf(comboBox_mixData[vehicleIndex][lsiv_drv].getSelectedItem().toString()).doubleValue();
        }

        return sum;
    } // end of method getSum

    class TotalActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
                for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                    if (event.getSource() == comboBox_mixData[lsiv_veh][lsiv_drv]) {
                        total = getSum(lsiv_veh);
                        cbo_total[lsiv_veh].removeAllItems();
                        cbo_total[lsiv_veh].addItem(format1Dec.format(total));
                        boolean blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class TotalActionListener

    class TotalKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
                    for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                        if (event.getSource() == comboBox_mixData[lsiv_veh][lsiv_drv]) {
                            total = getSum(lsiv_veh);
                            cbo_total[lsiv_veh].removeAllItems();
                            cbo_total[lsiv_veh].addItem(format1Dec.format(total));
                            boolean blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);
                        }
                    }
                }
            }
        } // end of method keyPressed
    } // end of class TotalKeyListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
                    for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                        if (event.getSource() == comboBox_mixData[lsiv_veh][lsiv_drv]) {
                            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv], lclv_tx_fmt.mstv_name,
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv], comboBox_mixData[lsiv_veh][lsiv_drv].getSelectedItem().toString(),
                                    Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]), " ",
                                    Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]),
                                    Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]),
                                    Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1 + lsiv_drv]));
                        }
                    }

                    if (event.getSource() == cbo_total[lsiv_veh]) {
                        new HelpDialog(true, "Total Percent of Drivers for Vehicle " + lsiv_veh, "Total Percent of Drivers for Vehicle " + lsiv_veh,
                                "The user cannot specify this item.  This item is the Total Percent of Drivers for Vehicle " + lsiv_veh + " and is determined by the program.", cbo_total[lsiv_veh]
                                        .getSelectedItem().toString(),
                                "0", " ", "0", "100.0", "1");
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
                }
            }
        }
    } // end of class HelpListener

    void saveData() {
        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= number_of_drivers; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv] = Double.valueOf(
                        comboBox_mixData[lsiv_veh][lsiv_drv].getSelectedItem().toString()).doubleValue();
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
            for (int lsiv_drv = number_of_drivers + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        for (int lsiv_veh = number_of_vehicles + 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                boolean blTotal = true;

                for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
                    total = getSum(lsiv_veh);
                    blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);
                    if (!blTotal) {
                        JOptionPane.showMessageDialog(null, "Sum of percentages for Vehicle Class " + lsiv_veh + " is " + format1Dec.format(total)
                                + "%. Revise this data so that the sum is 100.0% for Vehicle Class " + lsiv_veh, "Error Message", JOptionPane.ERROR_MESSAGE);
                        break;
                    }
                }

                if (blTotal) {
                    gdvsim.flag_driverMixData_ok = true;
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
                    boolean blTotal = true;

                    for (int lsiv_veh = 1; lsiv_veh <= number_of_vehicles; lsiv_veh++) {
                        total = getSum(lsiv_veh);
                        blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);

                        if (!blTotal) {
                            JOptionPane.showMessageDialog(null, "Sum of percentages for Vehicle Class " + lsiv_veh + " is " + format1Dec.format(total)
                                    + "%. Revise this data so that the sum is 100.0% for Vehicle Class " + lsiv_veh, "Error Message", JOptionPane.ERROR_MESSAGE);
                            break;
                        }
                    }

                    if (blTotal) {
                        gdvsim.flag_driverMixData_ok = true;

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
        } // end of keyPressed
    } // end of OkApplyKeyListener

} // end of class DriverMixDataDialog
