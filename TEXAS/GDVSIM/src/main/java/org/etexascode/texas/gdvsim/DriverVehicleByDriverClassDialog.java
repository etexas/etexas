package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                   DriverVehicleByDriverClassDialog.java                    */
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

class DriverVehicleByDriverClassDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JLabel label_title;

    JLabel[] label_driver = new JLabel[PARAMS.TEXAS_MODEL_NDC + 1];

    JComboBox[] comboBox_driver = new JComboBox[PARAMS.TEXAS_MODEL_NDC + 1];

    JButton okButton, applyButton, cancelButton;

    JPanel panel_title, ok_panel;

    Font font1, font2;

    TX_Fmt lclv_tx_fmt;

    int numOfDrivers;

    String titleString;

    public DriverVehicleByDriverClassDialog() {
        numOfDrivers = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;
        if (gdvsim.flag_driverClass_ok) {
            numOfDrivers = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;
        }
        else {
            numOfDrivers = PARAMS.TEXAS_MODEL_NDCD;
        }

        titleString = "Logout Summary Data for Driver-Vehicle Unit by Driver Class";

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

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            String[] array_comboBox = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv].substring(1).split("\\|");

            comboBox_driver[lsiv_drv] = new JComboBox(array_comboBox);
            comboBox_driver[lsiv_drv].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv]);

            comboBox_driver[lsiv_drv].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv]);
            comboBox_driver[lsiv_drv].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv]);

            label_driver[lsiv_drv] = new JLabel(lsiv_drv + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv]);
        }

        if (gdvsim.flag_drvVehByDrv_ok) {
            for (int lsiv_drv = 1; lsiv_drv <= numOfDrivers; lsiv_drv++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                        + lsiv_drv] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_driver[lsiv_drv].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv]);
                }
                else {
                    comboBox_driver[lsiv_drv].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[lsiv_drv]);
                }
            }
        }

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        panel_title = new JPanel();
        panel_title.add(label_title);

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

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

        helpListener = new HelpListener();
        openComboMenuListener = new OpenComboMenuListener();

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            comboBox_driver[lsiv_drv].addKeyListener(openComboMenuListener);
            comboBox_driver[lsiv_drv].addKeyListener(helpListener);
        }

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okApplyActionListener = new OkApplyActionListener();
        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        okApplyKeyListener = new OkApplyKeyListener();
        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        int iRow = 0;
        gbConstraints.insets = new Insets(2, 5, 2, 5);

        addComponent(panel_title, iRow++, 0, 2, 1);

        for (int lsiv_drv = 1; lsiv_drv <= numOfDrivers; lsiv_drv++) {
            addComponent(comboBox_driver[lsiv_drv], iRow, 0, 1, 1);
            addComponent(label_driver[lsiv_drv], iRow++, 1, 1, 1);
        }

        addComponent(ok_panel, iRow, 0, 2, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method DriveVehicleByDriverClassDialog

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

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2];

                for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                    if (event.getSource() == comboBox_driver[lsiv_drv]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1
                                        - 1 + lsiv_drv],
                                comboBox_driver[lsiv_drv].getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                                        + lsiv_drv],
                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1 + lsiv_drv], " ", " ", " ");
                        break;
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
        for (int lsiv_drv = 1; lsiv_drv <= numOfDrivers; lsiv_drv++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[lsiv_drv] = comboBox_driver[lsiv_drv].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_drv = numOfDrivers + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_drvVehByDrv_ok = true;
                saveData();

                if (event.getSource() == okButton) {
                    aFrame.dispose();
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
                    gdvsim.flag_drvVehByDrv_ok = true;
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of keyPressed
    } // end of OkApplyKeyListener

} // end of class DriverVehicleByDriverClassDialog
