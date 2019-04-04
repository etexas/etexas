package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           TrafficMixDialog.java                            */
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

class TrafficMixDialog extends JDialog {

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

    JComboBox cbo_total;

    JLabel label_title, label_total;

    JLabel[] label_traf = new JLabel[PARAMS.TEXAS_MODEL_NVC + 1];

    JComboBox[] cboBox_traf = new JComboBox[PARAMS.TEXAS_MODEL_NVC + 1];

    JButton okButton, applyButton, cancelButton;

    JPanel panel_title, ok_panel;

    Font font1, font2;

    double total;

    TX_Fmt lclv_tx_fmt;

    DecimalFormat format1Dec = new DecimalFormat("0.0");

    int local_leg_number;

    int num_of_vehicles;

    int local_vtp;

    public TrafficMixDialog(int leg_number, int vtp) {
        num_of_vehicles = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;

        double ldfv_min;
        double ldfv_max;
        double ldfv_inc;

        int SizeOfArray, ArrayIndex;
        double doubleArrayElementValue;
        String s;

        local_leg_number = leg_number;
        local_vtp = vtp;

        aFrame = new JFrame("Traffic Mix Data for Leg " + local_leg_number);

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

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

        for (int lsiv_traf = 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
            ldfv_min = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf];
            ldfv_max = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf];
            ldfv_inc = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf];

            SizeOfArray = (int)((ldfv_max - ldfv_min) / ldfv_inc + 1.5);
            doubleArrayElementValue = ldfv_min;
            String[] array_comboBox = new String[SizeOfArray];

            for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                array_comboBox[ArrayIndex] = format1Dec.format(doubleArrayElementValue);
                doubleArrayElementValue += ldfv_inc;
            }

            label_traf[lsiv_traf] = new JLabel(lsiv_traf + ".  " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf]);

            cboBox_traf[lsiv_traf] = new JComboBox(array_comboBox);
            cboBox_traf[lsiv_traf].setSelectedItem(format1Dec.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf]));
        }

        if (local_vtp == 0) {
            if (gdvsim.gclv_inter.mboa_Leg_Traffic_Mix_OK[local_leg_number]) {
                for (int lsiv_traf = 1; lsiv_traf <= num_of_vehicles; lsiv_traf++) {
                    cboBox_traf[lsiv_traf].setSelectedItem(format1Dec.format(gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_traf]));
                }
            }
        }
        else {
            if (gdvsim.gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[local_leg_number]) {
                for (int lsiv_traf = 1; lsiv_traf <= num_of_vehicles; lsiv_traf++) {
                    // cboBox_traf[lsiv_traf].setSelectedItem( format1Dec.format(
                    // gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[local_vtp].mclv_mix.mdfa_per_veh_class[lsiv_traf])
                    // );

                    if (gdvsim.mcla_var_traf_period_value[local_leg_number][local_vtp][lsiv_traf] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        cboBox_traf[lsiv_traf].setSelectedItem(format1Dec.format(gdvsim.mcla_var_traf_period_value[local_leg_number][local_vtp][lsiv_traf]));
                    }
                }
            }
        }

        for (int lsiv_traf = 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
            cboBox_traf[lsiv_traf].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf] + " for Leg " + local_leg_number);
            cboBox_traf[lsiv_traf].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf] + " for Leg " + local_leg_number);
        }

        label_title = new JLabel("Leg " + local_leg_number + " Traffic Mix Data");
        label_title.setFont(font1);

        panel_title = new JPanel();
        panel_title.add(label_title);

        cbo_total = new JComboBox();
        cbo_total.addItem(format1Dec.format(getSum()));

        label_total = new JLabel("Total (should be 100.0%)");
        label_total.setFont(font2);

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        cbo_total.getAccessibleContext().setAccessibleName("Total Traffic Mix");
        cbo_total.getAccessibleContext().setAccessibleDescription("Total Traffic Mix");

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        openComboMenuListener = new OpenComboMenuListener();
        totalActionListener = new TotalActionListener();
        totalKeyListener = new TotalKeyListener();
        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        for (int lsiv_traf = 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
            cboBox_traf[lsiv_traf].addKeyListener(openComboMenuListener);
            cboBox_traf[lsiv_traf].addActionListener(totalActionListener);
            cboBox_traf[lsiv_traf].addKeyListener(totalKeyListener);
            cboBox_traf[lsiv_traf].addKeyListener(helpListener);
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
        gbConstraints.insets = new Insets(2, 5, 2, 5);

        addComponent(panel_title, iRow++, 0, 2, 1);

        for (int lsiv_traf = 1; lsiv_traf <= num_of_vehicles; lsiv_traf++) {
            addComponent(cboBox_traf[lsiv_traf], iRow, 0, 1, 1);
            addComponent(label_traf[lsiv_traf], iRow++, 1, 1, 1);
        }

        addComponent(cbo_total, iRow, 0, 1, 1);
        addComponent(label_total, iRow++, 1, 1, 1);

        addComponent(ok_panel, iRow, 0, 2, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method TrafficMixDialog

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

    double getSum() {
        double sum = 0;

        for (int lsiv_traf = 1; lsiv_traf <= num_of_vehicles; lsiv_traf++) {
            sum += Double.valueOf(cboBox_traf[lsiv_traf].getSelectedItem().toString()).doubleValue();
        }

        return sum;
    } // end of method getSum

    class TotalActionListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            total = getSum();
            cbo_total.removeAllItems();
            cbo_total.addItem(format1Dec.format(total));

            boolean blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);

        } // end of method actionPerformed
    } // end of class TotalActionListener

    class TotalKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                total = getSum();
                cbo_total.removeAllItems();
                cbo_total.addItem(format1Dec.format(total));

                boolean blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);
            }

        } // end of method keyPressed
    } // end of class TotalKeyListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_traf = 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

                    if (event.getSource() == cboBox_traf[lsiv_traf]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf], lclv_tx_fmt.mstv_name.replace("#",
                                Integer.toString(local_leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf] + " for Leg " + local_leg_number,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf], cboBox_traf[lsiv_traf].getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf]));
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
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, "Total Traffic Mix", "Total Traffic Mix", "The user cannot specify this item.  This item is the Total Traffic Mix and is determined by the program.",
                            cbo_total.getSelectedItem().toString(), "0", " ", "0", "100", "1");
                }
            }
        }
    } // end of class HelpListener

    void saveData() {
        if (local_vtp == 0) {
            gdvsim.gclv_inter.mboa_Leg_Traffic_Mix_OK[local_leg_number] = true;
        }

        for (int lsiv_traf = 1; lsiv_traf <= num_of_vehicles; lsiv_traf++) {
            if (local_vtp == 0) {
                gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_traf] = Double.valueOf(cboBox_traf[lsiv_traf].getSelectedItem().toString())
                        .doubleValue();
                gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                        + lsiv_traf] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                // gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[local_vtp].mclv_mix.mdfa_per_veh_class[lsiv_traf]
                // = Double.valueOf( cboBox_traf[lsiv_traf].getSelectedItem().toString()
                // ).doubleValue();
                // gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[local_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01
                // - 1 + lsiv_traf] = gdvsim.gclv_inter.TX_FROM_USER;

                gdvsim.mcla_var_traf_period_value[local_leg_number][local_vtp][lsiv_traf] = Double.valueOf(cboBox_traf[lsiv_traf].getSelectedItem().toString()).doubleValue();
                gdvsim.mcla_var_traf_period_stat[local_leg_number][local_vtp][lsiv_traf] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_traf = num_of_vehicles + 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
            if (local_vtp == 0) {
                gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                        + lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
            else {
                // gdvsim.gclv_inter.mcla_leg[local_leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[local_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01
                // - 1 + lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.mcla_var_traf_period_stat[local_leg_number][local_vtp][lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                total = getSum();

                boolean blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);

                if (blTotal) {
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else {
                    JOptionPane.showMessageDialog(null, "Sum of percentages is " + format1Dec.format(total) + "%. Revise this data so that the sum is 100.0%.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
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
                    total = getSum();
                    boolean blTotal = gdvsim.gclv_inter.close(gdvsim.gclv_inter.GDVSIM_CLOSE_100, total, 100.0);

                    if (blTotal) {
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                    else {
                        JOptionPane.showMessageDialog(null, "Sum of percentages is " + format1Dec.format(total) + "%. Revise this data so that the sum is 100.0%.", "Error Message",
                                JOptionPane.ERROR_MESSAGE);
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of keyPressed
    } // end of OkApplyKeyListener

} // end of class TrafficMixDialog
