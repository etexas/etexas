package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           LegOutboundDialog.java                           */
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

class LegOutboundDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstr;

    JButton okButton, applyButton, cancelButton;

    JComboBox cbo_total;

    JLabel label_title, label_total;

    JLabel[] label_outbound = new JLabel[PARAMS.TEXAS_MODEL_NLGP1];

    JComboBox[] cboBx_outbound = new JComboBox[PARAMS.TEXAS_MODEL_NLGP1];

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    int leg_number, number_of_legs, total;

    Font font1, font2;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public LegOutboundDialog(int leg_num) {
        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        leg_number = leg_num;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST];

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
        gbConstr = new GridBagConstraints();

        font1 = new Font("TimesRoman", Font.BOLD, 20);
        font2 = new Font("TimesRoman", Font.BOLD, 16);

        label_title = new JLabel(titleString);
        label_title.setFont(font1);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            int lsiv_min;
            int lsiv_max;
            int lsiv_inc;

            int ArrayIndex, SizeOfArray, intArrayElementValue;

            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg];

            if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_leg] && gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min = lsiv_max = 0;
            }

            SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            intArrayElementValue = lsiv_min;
            String[] array_outbound = new String[SizeOfArray];

            for (ArrayIndex = 0; ArrayIndex < SizeOfArray; ArrayIndex++) {
                array_outbound[ArrayIndex] = Integer.toString(intArrayElementValue);
                intArrayElementValue += lsiv_inc;
            }

            cboBx_outbound[lsiv_leg] = new JComboBox(array_outbound);

            label_outbound[lsiv_leg] = new JLabel(lsiv_leg + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]);

            cboBx_outbound[lsiv_leg].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg] + " for leg " + leg_number);
            cboBx_outbound[lsiv_leg].getAccessibleContext()
                    .setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg] + " for leg " + leg_number);
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[leg_number]
                    && gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1
                            + lsiv_leg] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cboBx_outbound[lsiv_leg].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg]));
            }
            else {
                cboBx_outbound[lsiv_leg].setSelectedItem(Integer.toString(setDefault(lsiv_leg)));
            }
        }

        label_total = new JLabel("Total (should be 100%)");
        label_total.setFont(font2);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getSum()));

        okButton = new JButton("  OK  ");
        applyButton = new JButton("Apply ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        cbo_total.getAccessibleContext().setAccessibleName("Total Percent of Traffic Destination");
        cbo_total.getAccessibleContext().setAccessibleDescription("Total Percent of Traffic Destination");

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

        helpListener = new HelpListener();
        openComboMenuListener = new OpenComboMenuListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

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

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            cboBx_outbound[lsiv_leg].addActionListener(componentActionListener);
            cboBx_outbound[lsiv_leg].addKeyListener(componentKeyListener);
            cboBx_outbound[lsiv_leg].addKeyListener(helpListener);
            cboBx_outbound[lsiv_leg].addKeyListener(openComboMenuListener);
        }

        int iRow = 0;

        gbConstr.fill = GridBagConstraints.BOTH;

        gbConstr.insets = new Insets(5, 5, 5, 5);

        addComponent(panel_title, iRow++, 0, 2, 1);

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            addComponent(cboBx_outbound[lsiv_leg], iRow, 0, 1, 1);
            addComponent(label_outbound[lsiv_leg], iRow++, 1, 1, 1);
        }

        addComponent(cbo_total, iRow, 0, 1, 1);
        addComponent(label_total, iRow++, 1, 1, 1);

        addComponent(ok_panel, iRow, 0, 2, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method LegOutboundDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstr.gridx = column;
        gbConstr.gridy = row;

        gbConstr.gridwidth = width;
        gbConstr.gridheight = height;

        gbLayout.setConstraints(c, gbConstr);
        container.add(c);
    } // end of method addComponent

    int setDefault(int lsiv_leg) {
        int retValue = 0;
        int countNoOutbound = 0;
        int lastLegWithOutbound;

        if (leg_number == number_of_legs) {
            lastLegWithOutbound = number_of_legs - 1;
        }
        else {
            lastLegWithOutbound = number_of_legs;
        }

        for (int lsiv_j = 1; lsiv_j <= number_of_legs; lsiv_j++) {
            if (lsiv_j == leg_number)
                continue;

            if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_j]
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_j].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_j].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
                countNoOutbound++;
            }
            else {
                lastLegWithOutbound = lsiv_j;
            }
        }

        int eachValue = 0;
        int lastValue = 0;
        boolean allOutboundZero = false;

        if (countNoOutbound == (number_of_legs - 1)) {
            allOutboundZero = true;
        }

        if (!allOutboundZero) {
            int divided = number_of_legs - 1 - countNoOutbound;

            if (divided == 3) {
                eachValue = 33;
                lastValue = 34;
            }
            else {
                eachValue = 100 / divided;
                lastValue = 100 / divided;
            }
        }

        if (allOutboundZero) {
            retValue = 0;
        }
        else {
            if (lsiv_leg == leg_number) {
                retValue = 0;
            }
            else if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_leg]
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
                retValue = 0;
            }
            else {
                if (lsiv_leg == lastLegWithOutbound) {
                    retValue = lastValue;
                }
                else {
                    retValue = eachValue;
                }
            }
        }

        return retValue;

    } // end of method setDefault

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

    int getSum() {
        int sum = 0;

        for (int lsiv_numleg = 1; lsiv_numleg <= number_of_legs; lsiv_numleg++) {
            if (number_of_legs == lsiv_numleg) {
                for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                    sum = sum + Integer.valueOf(cboBx_outbound[lsiv_leg].getSelectedItem().toString()).intValue();
                }
            }
        }

        return sum;

    } // end of method getSum

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                    if (event.getSource() == cboBx_outbound[lsiv_leg]) {
                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1
                                            - 1 + lsiv_leg],
                                    cboBx_outbound[lsiv_leg].getSelectedItem().toString(), "0", " ", "0", "0",
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]));
                        }
                        else {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1
                                            - 1 + lsiv_leg],
                                    cboBx_outbound[lsiv_leg].getSelectedItem().toString(), Integer.toString(setDefault(lsiv_leg)), " ",
                                    Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]));
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
                }
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, "Total Percent of Traffic Destination", "Total Percent of Traffic Destination",
                            "The user cannot specify this item.  This item is the Total Percent of Traffic Destination and is determined by the program.", cbo_total.getSelectedItem().toString(), "0",
                            " ", "0", "100", "1");
                }
            }
        }
    } // end of class HelpListener

    void saveData() {
        for (int lsiv_numleg = 1; lsiv_numleg <= number_of_legs; lsiv_numleg++) {
            if (number_of_legs == lsiv_numleg) {
                for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                    gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg] = Integer.valueOf(cboBx_outbound[lsiv_leg].getSelectedItem().toString()).intValue();
                    gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1
                            + lsiv_leg] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
        }
    } // end of method saveData()

    boolean isError() {
        if (getSum() != 100) {
            JOptionPane.showMessageDialog(null, "Sum of percentages is " + getSum() + "%. Revise this data so that the sum is 100.0%.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        return false;
    } // end of method isError

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();

                    gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[leg_number] = true;

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                if (event.getSource() == cboBx_outbound[lsiv_leg]) {
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getSum()));
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
                        saveData();

                        gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[leg_number] = true;

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
                    if (event.getSource() == cboBx_outbound[lsiv_leg]) {
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getSum()));
                        break;
                    }
                }
            }
        } // end of keyPressed(KeyEvent event)
    } // end of class ComponentKeyListener

} // end of class LegOutboundDialog
