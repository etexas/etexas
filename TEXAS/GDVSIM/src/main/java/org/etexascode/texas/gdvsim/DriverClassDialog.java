package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                          DriverClassDialog.java                            */
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
import java.text.DecimalFormat;

class DriverClassDialog extends JDialog {

    int NUMOFFIELD = 2;

    int OPER_CHAR = 1;

    int PIJR_TIME = 2;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JComboBox[][] comboBox_driver = new JComboBox[PARAMS.TEXAS_MODEL_NDC + 1][NUMOFFIELD + 1];

    JLabel[] label_driver = new JLabel[PARAMS.TEXAS_MODEL_NDC + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NDC + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NDC + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NDC + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NDC + 1];

    JComboBox cbo_total;

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_total, label_field0;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    int numOfDrivers;

    String titleString;

    JComboBox comboBox_number_of_driver_classes_saved;

    JComboBox comboBox_driver_mix_saved;

    JButton button_driver_mix_saved;

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    public DriverClassDialog(JComboBox comboBox_number_of_driver_classes, JComboBox comboBox_driver_mix, JButton button_driver_mix) {
        comboBox_number_of_driver_classes_saved = comboBox_number_of_driver_classes;
        comboBox_driver_mix_saved = comboBox_driver_mix;
        button_driver_mix_saved = button_driver_mix;

        if (gdvsim.flag_driverClass_ok) {
            numOfDrivers = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;
        }
        else {
            numOfDrivers = PARAMS.TEXAS_MODEL_NDCD;
        }

        titleString = "User-Defined Driver Data";
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
        font1 = new Font("TimesRoman", Font.BOLD, 14);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        label_field0 = new JLabel("Driver Class");

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR];
        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            String desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv];
            int beg = desc.indexOf('(') + 1;
            int end = desc.indexOf(')') - 1;
            label_driver[lsiv_drv] = new JLabel(lsiv_drv + "   " + desc.substring(beg, end + 1));
        }

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            add[lsiv_drv] = new JButton("Add");
            del[lsiv_drv] = new JButton("Delete");
            up[lsiv_drv] = new JButton("Up");
            down[lsiv_drv] = new JButton("Down");
        }

        label_total = new JLabel("Number of Driver Classes (maximum = " + PARAMS.TEXAS_MODEL_NDC + ")");

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR];

        label_field[OPER_CHAR] = new JTextArea();
        label_field[OPER_CHAR].setBackground(aFrame.getBackground());
        label_field[OPER_CHAR].setEditable(false);
        label_field[OPER_CHAR].setFocusable(false);
        label_field[OPER_CHAR].setWrapStyleWord(true);
        label_field[OPER_CHAR].setLineWrap(true);
        label_field[OPER_CHAR].setFont(font1);
        label_field[OPER_CHAR].setSize(new Dimension(100, 26));

        String desc1 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1];
        int beg1 = desc1.indexOf("for") - 1;
        int end1 = desc1.indexOf(')') + 1;
        label_field[OPER_CHAR].setText(desc1.substring(0, beg1) + desc1.substring(end1));

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            int lsiv_min;
            int lsiv_max;
            int lsiv_inc;
            int count;
            int int_number;

            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_drv = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_drv[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_driver[lsiv_drv][OPER_CHAR] = new JComboBox(array_drv);

            if (gdvsim.flag_driverClass_ok) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1
                        + lsiv_drv] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_driver[lsiv_drv][OPER_CHAR].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.msia_drv_oper_char[lsiv_drv]));
                }
                else {
                    comboBox_driver[lsiv_drv][OPER_CHAR].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]));
                }
            }
            else {
                comboBox_driver[lsiv_drv][OPER_CHAR].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME];

        label_field[PIJR_TIME] = new JTextArea();
        label_field[PIJR_TIME].setBackground(aFrame.getBackground());
        label_field[PIJR_TIME].setEditable(false);
        label_field[PIJR_TIME].setFocusable(false);
        label_field[PIJR_TIME].setWrapStyleWord(true);
        label_field[PIJR_TIME].setLineWrap(true);
        label_field[PIJR_TIME].setFont(font1);
        label_field[PIJR_TIME].setSize(new Dimension(100, 26));

        String desc2 = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1];
        int beg2 = desc1.indexOf("for") - 2;
        int end2 = desc1.indexOf(')');
        label_field[PIJR_TIME].setText(desc2.substring(0, beg2) + desc2.substring(end2));

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            double ldfv_min;
            double ldfv_max;
            double ldfv_inc;

            int count;
            double double_number;

            ldfv_min = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv];
            ldfv_max = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv];
            ldfv_inc = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv];

            count = (int)((ldfv_max - ldfv_min) / ldfv_inc) + 1;
            double_number = ldfv_min;
            String[] array_drv = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_drv[lsiv_i] = twoDigits.format(double_number);
                double_number += ldfv_inc;
            }

            comboBox_driver[lsiv_drv][PIJR_TIME] = new JComboBox(array_drv);

            if (gdvsim.flag_driverClass_ok) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1
                        + lsiv_drv] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_driver[lsiv_drv][PIJR_TIME].setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mdfv_drv_PIJR_time[lsiv_drv]));
                }
                else {
                    comboBox_driver[lsiv_drv][PIJR_TIME].setSelectedItem(twoDigits.format(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]));
                }
            }
            else {
                comboBox_driver[lsiv_drv][PIJR_TIME].setSelectedItem(twoDigits.format(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]));
            }
        }

        setStatus(numOfDrivers);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfDrivers()));

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

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            add[lsiv_drv].addActionListener(componentActionListener);
            del[lsiv_drv].addActionListener(componentActionListener);
            up[lsiv_drv].addActionListener(componentActionListener);
            down[lsiv_drv].addActionListener(componentActionListener);

            add[lsiv_drv].addKeyListener(componentKeyListener);
            del[lsiv_drv].addKeyListener(componentKeyListener);
            up[lsiv_drv].addKeyListener(componentKeyListener);
            down[lsiv_drv].addKeyListener(componentKeyListener);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_driver[lsiv_drv][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_driver[lsiv_drv][lsiv_field].addKeyListener(helpListener);
            }

            add[lsiv_drv].addKeyListener(helpListener);
            del[lsiv_drv].addKeyListener(helpListener);
            up[lsiv_drv].addKeyListener(helpListener);
            down[lsiv_drv].addKeyListener(helpListener);

        }

        cbo_total.addKeyListener(helpListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        setAccessiblility();

        int iRow = 0;
        int iCol = 7;

        gbConstraints.insets = new Insets(1, 0, 0, 0);
        addComponent(panel_title, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        addComponent(label_field0, iRow, 0, 1, 1);
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1);
        }

        iRow++;

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            gbConstraints.insets = new Insets(1, 1, 1, 1);

            addComponent(label_driver[lsiv_drv], iRow, 0, 1, 1);
            addComponent(add[lsiv_drv], iRow, 1, 1, 1);
            addComponent(del[lsiv_drv], iRow, 2, 1, 1);
            addComponent(up[lsiv_drv], iRow, 3, 1, 1);
            addComponent(down[lsiv_drv], iRow, 4, 1, 1);

            gbConstraints.insets = new Insets(1, 2, 1, 2);
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(comboBox_driver[lsiv_drv][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
            }
            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(cbo_total, iRow, 1, 1, 1);
        addComponent(label_total, iRow++, 2, iCol - 2, 1);
        addComponent(ok_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(900, 700);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method DriverClassDialog()

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;
        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getNumberOfDrivers() {
        int sum = 0;

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            if (comboBox_driver[lsiv_drv][OPER_CHAR].isEnabled())
                sum++;
        }

        return sum;
    } // end of method getNumberOfDrivers()

    void setAccessiblility() {
        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR];
            comboBox_driver[lsiv_drv][OPER_CHAR].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]);
            comboBox_driver[lsiv_drv][OPER_CHAR].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME];
            comboBox_driver[lsiv_drv][PIJR_TIME].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]);
            comboBox_driver[lsiv_drv][PIJR_TIME].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]);

            add[lsiv_drv].getAccessibleContext().setAccessibleName("Add    for Driver Class " + lsiv_drv);
            add[lsiv_drv].getAccessibleContext().setAccessibleDescription("Add    for Driver Class " + lsiv_drv);
            del[lsiv_drv].getAccessibleContext().setAccessibleName("Delete for Driver Class " + lsiv_drv);
            del[lsiv_drv].getAccessibleContext().setAccessibleDescription("Delete for Driver Class " + lsiv_drv);
            up[lsiv_drv].getAccessibleContext().setAccessibleName("Up     for Driver Class " + lsiv_drv);
            up[lsiv_drv].getAccessibleContext().setAccessibleDescription("Up     for Driver Class " + lsiv_drv);
            down[lsiv_drv].getAccessibleContext().setAccessibleName("Down   for Driver Class " + lsiv_drv);
            down[lsiv_drv].getAccessibleContext().setAccessibleDescription("Down   for Driver Class " + lsiv_drv);
        }

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

    } // end of method setAccessiblility()

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
    } // end of class OpenComboMenuListener()

    void setStatus(int sumOfDrivers) {
        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            if (lsiv_drv <= sumOfDrivers) {
                label_driver[lsiv_drv].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_driver[lsiv_drv][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_driver[lsiv_drv].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_driver[lsiv_drv][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfDrivers == PARAMS.TEXAS_MODEL_NDC) {
            for (int lsiv_drv = PARAMS.TEXAS_MODEL_NDCD + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                add[lsiv_drv].setEnabled(false);
            }
        }
        else {
            for (int lsiv_drv = PARAMS.TEXAS_MODEL_NDCD + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                if (lsiv_drv <= (sumOfDrivers + 1)) {
                    add[lsiv_drv].setEnabled(true);
                }
                else {
                    add[lsiv_drv].setEnabled(false);
                }
            }
        }

        for (int lsiv_drv = PARAMS.TEXAS_MODEL_NDCD + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            if (lsiv_drv <= sumOfDrivers) {
                del[lsiv_drv].setEnabled(true);
            }
            else {
                del[lsiv_drv].setEnabled(false);
            }
        }

        for (int lsiv_drv = PARAMS.TEXAS_MODEL_NDCD + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            if (lsiv_drv <= sumOfDrivers) {
                up[lsiv_drv].setEnabled(true);
            }
            else {
                up[lsiv_drv].setEnabled(false);
            }

            up[PARAMS.TEXAS_MODEL_NDCD + 1].setEnabled(false);
        }

        for (int lsiv_drv = PARAMS.TEXAS_MODEL_NDCD + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            if (lsiv_drv < sumOfDrivers) {
                down[lsiv_drv].setEnabled(true);
            }
            else {
                down[lsiv_drv].setEnabled(false);
            }
        }

        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDCD; lsiv_drv++) {
            add[lsiv_drv].setVisible(false);
            del[lsiv_drv].setVisible(false);
            up[lsiv_drv].setVisible(false);
            down[lsiv_drv].setVisible(false);
        }
    } // end of method setStatus

    void setValueAfterAdd(int sum, int index) {
        if ((sum == index - 1) && (sum != PARAMS.TEXAS_MODEL_NDCD)) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_driver[index][lsiv_field].setSelectedItem(comboBox_driver[sum][lsiv_field].getSelectedItem().toString());
            }
        }
        else {
            for (int lsiv_drv = sum; lsiv_drv >= index; lsiv_drv--) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_driver[lsiv_drv + 1][lsiv_field].setSelectedItem(comboBox_driver[lsiv_drv][lsiv_field].getSelectedItem().toString());
                }
            }
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_drv = index; lsiv_drv < sum; lsiv_drv++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_driver[lsiv_drv][lsiv_field].setSelectedItem(comboBox_driver[lsiv_drv + 1][lsiv_field].getSelectedItem().toString());
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR];
        comboBox_driver[sum][OPER_CHAR].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME];
        comboBox_driver[sum][PIJR_TIME].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + sum]));

    } // end of method setValueAfterDel()

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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NDC), "1");
                }

                for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                    if (event.getSource() == comboBox_driver[lsiv_drv][OPER_CHAR]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv],
                                comboBox_driver[lsiv_drv][OPER_CHAR].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]),
                                " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1 + lsiv_drv]));
                    }
                    else if (event.getSource() == comboBox_driver[lsiv_drv][PIJR_TIME]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv],
                                comboBox_driver[lsiv_drv][PIJR_TIME].getSelectedItem().toString(), twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]),
                                " ", twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]),
                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]),
                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1 + lsiv_drv]));

                    }
                    else if (event.getSource() == up[lsiv_drv]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous driver class and the current driver class.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_drv]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next driver class and the current driver class.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_drv]) {
                        new HelpDialog(true, "Delete Button",
                                "The Delete button moves the data up 1 driver class for all driver classes below this driver class and decreases the Total Driver Classes by 1.", " ", " ", " ", " ",
                                " ", " ", " ");
                    }
                    else if (event.getSource() == add[lsiv_drv]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 driver class for all driver classes below this driver class, inserts a new driver class at the current position, copies the values of all parameters from the previous driver class to the new driver class, and increases the Total Driver Classes by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of class HelpListener

    void saveData() {
        int numOfDrivers = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl = getNumberOfDrivers();
        comboBox_number_of_driver_classes_saved.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl));

        for (int lsiv_drv = 1; lsiv_drv <= numOfDrivers; lsiv_drv++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.msia_drv_oper_char[lsiv_drv] = Integer.valueOf(
                    comboBox_driver[lsiv_drv][OPER_CHAR].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mdfv_drv_PIJR_time[lsiv_drv] = Double.valueOf(
                    comboBox_driver[lsiv_drv][PIJR_TIME].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_drv = numOfDrivers + 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method saveData()

    void invalidDriverMix() {
        int numOfDrivers = getNumberOfDrivers();

        gdvsim.flag_driverMixData_ok = false;
        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        comboBox_driver_mix_saved.setSelectedItem("NO");
        button_driver_mix_saved.setEnabled(false);
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix = "NO";

        gdvsim.flag_drvVehByDrv_ok = false;
        for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1 - 1
                    + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method invalidDriverMix

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != getNumberOfDrivers()) {
                    invalidDriverMix();
                    JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                            + " to " + getNumberOfDrivers() + ".\nPlease OK or Apply User-Defined Driver Mix Data and Logout Summary for Driver-Vehicle Unit by Driver Class in GDV Data.",
                            "Warning Message", JOptionPane.WARNING_MESSAGE);
                }

                gdvsim.flag_driverClass_ok = true;
                saveData();

                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data = "YES";

                if (event.getSource() == okButton) {
                    aFrame.dispose();
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                if (event.getSource() == add[lsiv_drv]) {
                    int numOfDriversBeforeClick;
                    numOfDriversBeforeClick = getNumberOfDrivers();
                    setStatus(numOfDriversBeforeClick + 1);
                    setValueAfterAdd(numOfDriversBeforeClick, lsiv_drv);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfDrivers()));
                    break;
                }
                else if (event.getSource() == del[lsiv_drv]) {
                    int numOfDriversBeforeClick;
                    numOfDriversBeforeClick = getNumberOfDrivers();
                    setValueAfterDel(numOfDriversBeforeClick, lsiv_drv);
                    setStatus(numOfDriversBeforeClick - 1);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfDrivers()));
                    if (!del[lsiv_drv].isEnabled()) {
                        okButton.requestFocus();
                    }
                    break;
                }
                else if (event.getSource() == up[lsiv_drv]) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        String temp;
                        temp = comboBox_driver[lsiv_drv][lsiv_field].getSelectedItem().toString();
                        comboBox_driver[lsiv_drv][lsiv_field].setSelectedItem(comboBox_driver[lsiv_drv - 1][lsiv_field].getSelectedItem().toString());
                        comboBox_driver[lsiv_drv - 1][lsiv_field].setSelectedItem(temp);
                    }
                    break;
                }
                else if (event.getSource() == down[lsiv_drv]) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        String temp;
                        temp = comboBox_driver[lsiv_drv][lsiv_field].getSelectedItem().toString();
                        comboBox_driver[lsiv_drv][lsiv_field].setSelectedItem(comboBox_driver[lsiv_drv + 1][lsiv_field].getSelectedItem().toString());
                        comboBox_driver[lsiv_drv + 1][lsiv_field].setSelectedItem(temp);
                    }
                    break;
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl != getNumberOfDrivers()) {
                        invalidDriverMix();
                        JOptionPane.showMessageDialog(null, "The Number of Driver Classes has changed from " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
                                + " to " + getNumberOfDrivers() + ".\nPlease OK or Apply User-Defined Driver Mix Data and Logout Summary for Driver-Vehicle Unit by Driver Class in GDV Data.",
                                "Warning Message", JOptionPane.WARNING_MESSAGE);
                    }

                    gdvsim.flag_driverClass_ok = true;
                    saveData();

                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data = "YES";

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                    if (event.getSource() == add[lsiv_drv]) {
                        int numOfDriversBeforeClick;
                        numOfDriversBeforeClick = getNumberOfDrivers();
                        setStatus(numOfDriversBeforeClick + 1);
                        setValueAfterAdd(numOfDriversBeforeClick, lsiv_drv);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfDrivers()));
                        break;
                    }
                    else if (event.getSource() == del[lsiv_drv]) {
                        int numOfDriversBeforeClick;
                        numOfDriversBeforeClick = getNumberOfDrivers();
                        setValueAfterDel(numOfDriversBeforeClick, lsiv_drv);
                        setStatus(numOfDriversBeforeClick - 1);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfDrivers()));
                        if (!del[lsiv_drv].isEnabled()) {
                            okButton.requestFocus();
                        }
                        break;
                    }
                    else if (event.getSource() == up[lsiv_drv]) {
                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            String temp;
                            temp = comboBox_driver[lsiv_drv][lsiv_field].getSelectedItem().toString();
                            comboBox_driver[lsiv_drv][lsiv_field].setSelectedItem(comboBox_driver[lsiv_drv - 1][lsiv_field].getSelectedItem().toString());
                            comboBox_driver[lsiv_drv - 1][lsiv_field].setSelectedItem(temp);
                        }
                        break;
                    }
                    else if (event.getSource() == down[lsiv_drv]) {
                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            String temp;
                            temp = comboBox_driver[lsiv_drv][lsiv_field].getSelectedItem().toString();
                            comboBox_driver[lsiv_drv][lsiv_field].setSelectedItem(comboBox_driver[lsiv_drv + 1][lsiv_field].getSelectedItem().toString());
                            comboBox_driver[lsiv_drv + 1][lsiv_field].setSelectedItem(temp);
                        }
                        break;
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentKeyListener
} // end of class DriverClassDialog.java
