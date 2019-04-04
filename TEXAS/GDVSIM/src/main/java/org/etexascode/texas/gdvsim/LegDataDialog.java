package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            LegDataDialog.java                              */
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
import javax.swing.*;

class LegDataDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 11;

    static final int GDV_LEG_GEO_ANG = 1;

    static final int GDV_LEG_GEO_LEN_INB = 2;

    static final int GDV_LEG_GEO_LEN_OUT = 3;

    static final int GDV_LEG_GEO_NO_INB = 4;

    static final int GDV_LEG_GEO_NO_OUT = 5;

    static final int GDV_LEG_GEO_SL_INB = 6;

    static final int GDV_LEG_GEO_SL_OUT = 7;

    static final int GDV_LEG_GEO_CL_OFF = 8;

    static final int GDV_LEG_GEO_MED_W = 9;

    static final int GDV_LEG_GEO_ANG_S = 10;

    static final int GDV_LEG_GEO_ANG_U = 11;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title, label_radius;

    JLabel[] label_leg_geo = new JLabel[NUMOFFIELD + 1];

    JComboBox[] comboBox_leg_geo = new JComboBox[NUMOFFIELD + 1];

    JComboBox comboBox_radius;

    JButton laneButton, inboundButton, outboundButton, sdrButton, varyTrafButton, okButton, applyButton, cancelButton;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    Font font;

    int leg_number;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public LegDataDialog(int leg_num) {
        leg_number = leg_num;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO];

        titleString = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number));

        aFrame = new JFrame(titleString);

        container = aFrame.getContentPane();
        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        container.setLayout(gbLayout);
        gbConstraints.fill = GridBagConstraints.BOTH;

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if (leg_number == 1) {
                label_title = new JLabel("Leg " + leg_number + " Geometry Data " + "(Right Intersection, North Leg)");
            }
            else if (leg_number == 2) {
                label_title = new JLabel("Leg " + leg_number + " Geometry Data " + "(Right Intersection, East Leg)");
            }
            else if (leg_number == 3) {
                label_title = new JLabel("Leg " + leg_number + " Geometry Data " + "(Right Intersection, South Leg)");
            }
            else if (leg_number == 4) {
                label_title = new JLabel("Leg " + leg_number + " Geometry Data " + "(Left Intersection, South Leg)");
            }
            else if (leg_number == 5) {
                label_title = new JLabel("Leg " + leg_number + " Geometry Data " + "(Left Intersection, West Leg)");
            }
            else if (leg_number == 6) {
                label_title = new JLabel("Leg " + leg_number + " Geometry Data " + "(Left Intersection, North Leg)");
            }
        }
        else {
            label_title = new JLabel(titleString);
        }

        font = new Font("TimesRoman", Font.BOLD, 16);
        label_title.setFont(font);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            int lsiv_min;
            int lsiv_max;
            int lsiv_inc;
            int arrayIndex, intArrayElementValue, SizeOfArray;

            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field];

            SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            intArrayElementValue = lsiv_min;
            String[] array_leg_geo = new String[SizeOfArray];

            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                array_leg_geo[arrayIndex] = Integer.toString(intArrayElementValue);
                intArrayElementValue += lsiv_inc;
            }

            label_leg_geo[lsiv_field] = new JLabel(lsiv_field + ". " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field]);
            comboBox_leg_geo[lsiv_field] = new JComboBox(array_leg_geo);

            comboBox_leg_geo[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field] + " for leg " + leg_number);
            comboBox_leg_geo[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field] + " for leg " + leg_number);
        }

        if (gdvsim.gclv_inter.mboa_Leg_OK[leg_number]) {
            comboBox_leg_geo[GDV_LEG_GEO_ANG].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_ang));
            comboBox_leg_geo[GDV_LEG_GEO_LEN_INB].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb));
            comboBox_leg_geo[GDV_LEG_GEO_LEN_OUT].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out));
            comboBox_leg_geo[GDV_LEG_GEO_NO_INB].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb));
            comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out));
            comboBox_leg_geo[GDV_LEG_GEO_SL_INB].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_sl_inb));
            comboBox_leg_geo[GDV_LEG_GEO_SL_OUT].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_sl_out));
            comboBox_leg_geo[GDV_LEG_GEO_CL_OFF].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off));
            comboBox_leg_geo[GDV_LEG_GEO_MED_W].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_med_w));
            comboBox_leg_geo[GDV_LEG_GEO_ANG_S].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_ang_s));
            comboBox_leg_geo[GDV_LEG_GEO_ANG_U].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_ang_u));
        }
        else {
            int lsiv_leg_ang = 360 * (leg_number - 1) / gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
            int lsiv_no_inb = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB];
            int lsiv_no_out = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT];
            if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                switch (leg_number) {
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

            comboBox_leg_geo[GDV_LEG_GEO_ANG].setSelectedItem(Integer.toString(lsiv_leg_ang));
            comboBox_leg_geo[GDV_LEG_GEO_LEN_INB].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_INB]));
            comboBox_leg_geo[GDV_LEG_GEO_LEN_OUT].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEN_OUT]));
            comboBox_leg_geo[GDV_LEG_GEO_NO_INB].setSelectedItem(Integer.toString(lsiv_no_inb));
            comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].setSelectedItem(Integer.toString(lsiv_no_out));
            comboBox_leg_geo[GDV_LEG_GEO_SL_INB].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_SL_INB]));
            comboBox_leg_geo[GDV_LEG_GEO_SL_OUT].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_SL_OUT]));
            comboBox_leg_geo[GDV_LEG_GEO_CL_OFF].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_CL_OFF]));
            comboBox_leg_geo[GDV_LEG_GEO_MED_W].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_MED_W]));
            comboBox_leg_geo[GDV_LEG_GEO_ANG_S].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG_S]));
            comboBox_leg_geo[GDV_LEG_GEO_ANG_U].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG_U]));
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN];

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            if (lsiv_leg == leg_number) {
                int lsiv_min;
                int lsiv_max;
                int lsiv_inc;
                int arrayIndex, intArrayElementValue, SizeOfArray;

                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg];

                SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                intArrayElementValue = lsiv_min;
                String[] array_radius = new String[SizeOfArray];

                for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                    array_radius[arrayIndex] = Integer.toString(intArrayElementValue);
                    intArrayElementValue += lsiv_inc;
                }

                comboBox_radius = new JComboBox(array_radius);
                label_radius = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg]);
                font = new Font("TimesRoman", Font.BOLD, 16);
                label_radius.setFont(font);

                if (gdvsim.gclv_inter.mboa_Leg_OK[leg_number]) {
                    comboBox_radius.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[leg_number]));
                }
                else {
                    comboBox_radius.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg]));
                }

                comboBox_radius.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg] + " for leg " + leg_number);
                comboBox_radius.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg] + " for leg " + leg_number);
            }
        }

        laneButton = new JButton("Lane Data for Leg " + leg_number);
        inboundButton = new JButton("Inbound Traffic Headway Frequency Distribution Data for Leg " + leg_number);
        outboundButton = new JButton("Outbound Traffic Destination Data for Leg " + leg_number);
        sdrButton = new JButton("Sight Distance Restriction Location Data for Leg " + leg_number);
        varyTrafButton = new JButton("Varying Traffic Period Data for Leg " + leg_number);

        laneButton.setMnemonic(KeyEvent.VK_L);
        inboundButton.setMnemonic(KeyEvent.VK_N);
        outboundButton.setMnemonic(KeyEvent.VK_U);
        sdrButton.setMnemonic(KeyEvent.VK_S);
        varyTrafButton.setMnemonic(KeyEvent.VK_V);

        laneButton.getAccessibleContext().setAccessibleName("Lane data for leg " + leg_number);
        laneButton.getAccessibleContext().setAccessibleDescription("Lane data for leg " + leg_number);

        inboundButton.getAccessibleContext().setAccessibleName("Inbound traffic headway frequency distribution data for leg " + leg_number);
        inboundButton.getAccessibleContext().setAccessibleDescription("Inbound traffic headway frequency distribution data for leg " + leg_number);

        outboundButton.getAccessibleContext().setAccessibleName("Outbound traffic destination data for leg " + leg_number);
        outboundButton.getAccessibleContext().setAccessibleDescription("Outbound traffic destination data for leg " + leg_number);

        sdrButton.getAccessibleContext().setAccessibleName("Sight distance restriction location data for leg " + leg_number);
        sdrButton.getAccessibleContext().setAccessibleDescription("Sight distance restriction location data for leg " + leg_number);

        varyTrafButton.getAccessibleContext().setAccessibleName("Varying Traffic Period data for leg " + leg_number);
        varyTrafButton.getAccessibleContext().setAccessibleDescription("Varying Traffic Period data for leg " + leg_number);

        if (Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue() == 0
                && Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue() == 0) {
            laneButton.setEnabled(false);
            inboundButton.setEnabled(false);
            outboundButton.setEnabled(false);
            sdrButton.setEnabled(false);
        }
        else {
            laneButton.setEnabled(true);
            inboundButton.setEnabled(true);
            outboundButton.setEnabled(true);
            sdrButton.setEnabled(true);
        }

        if (Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue() == 0) {
            inboundButton.setEnabled(false);
            outboundButton.setEnabled(false);
            varyTrafButton.setEnabled(false);
        }
        else {
            inboundButton.setEnabled(true);
            outboundButton.setEnabled(true);
            varyTrafButton.setEnabled(true);
        }

        okButton = new JButton("     OK   ");
        applyButton = new JButton("  Apply  ");
        cancelButton = new JButton("  Cancel ");

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        componentKeyListener = new ComponentKeyListener();
        componentActionListener = new ComponentActionListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        laneButton.addActionListener(componentActionListener);
        laneButton.addKeyListener(componentKeyListener);
        laneButton.addKeyListener(helpListener);

        inboundButton.addActionListener(componentActionListener);
        inboundButton.addKeyListener(componentKeyListener);
        inboundButton.addKeyListener(helpListener);
        outboundButton.addActionListener(componentActionListener);
        outboundButton.addKeyListener(componentKeyListener);
        outboundButton.addKeyListener(helpListener);
        sdrButton.addActionListener(componentActionListener);
        sdrButton.addKeyListener(componentKeyListener);
        sdrButton.addKeyListener(helpListener);
        varyTrafButton.addActionListener(componentActionListener);
        varyTrafButton.addKeyListener(componentKeyListener);
        varyTrafButton.addKeyListener(helpListener);

        comboBox_leg_geo[GDV_LEG_GEO_NO_INB].addActionListener(componentActionListener);
        comboBox_leg_geo[GDV_LEG_GEO_NO_INB].addKeyListener(componentKeyListener);
        comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].addActionListener(componentActionListener);
        comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].addKeyListener(componentKeyListener);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            comboBox_leg_geo[lsiv_field].addKeyListener(openComboMenuListener);
            comboBox_leg_geo[lsiv_field].addKeyListener(helpListener);
        }

        comboBox_radius.addKeyListener(openComboMenuListener);
        comboBox_radius.addKeyListener(helpListener);

        gbConstraints.insets = new Insets(2, 5, 2, 5);
        int iRow = 0;

        addComponent(label_title, iRow++, 0, 2, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(comboBox_leg_geo[lsiv_field], iRow, 0, 1, 1);
            addComponent(label_leg_geo[lsiv_field], iRow++, 1, 1, 1);
        }

        addComponent(comboBox_radius, iRow, 0, 1, 1);
        addComponent(label_radius, iRow++, 1, 1, 1);

        addComponent(laneButton, iRow++, 0, 2, 1);
        addComponent(inboundButton, iRow++, 0, 2, 1);
        addComponent(outboundButton, iRow++, 0, 2, 1);
        addComponent(sdrButton, iRow++, 0, 2, 1);
        addComponent(varyTrafButton, iRow++, 0, 2, 1);
        addComponent(ok_panel, iRow, 0, 2, 1);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method LegDataDialog

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
    } // end of class OpenComboMenuListener

    void CboInboundAction() {
        if (Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue() == 0
                && Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue() == 0) {
            laneButton.setEnabled(false);
            inboundButton.setEnabled(false);
            outboundButton.setEnabled(false);
            sdrButton.setEnabled(false);
        }
        else {
            laneButton.setEnabled(true);
            inboundButton.setEnabled(true);
            outboundButton.setEnabled(true);
            sdrButton.setEnabled(true);
        }

        if (Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue() == 0) {
            inboundButton.setEnabled(false);
            outboundButton.setEnabled(false);
            varyTrafButton.setEnabled(false);
        }
        else {
            inboundButton.setEnabled(true);
            outboundButton.setEnabled(true);
            varyTrafButton.setEnabled(true);
        }
    } // end of method CboInboundAction

    void InboundAction() {
        int NumOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        int CurrentNumOfInbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue();

        Boolean NumOfInboundChangedWithZeroToNum = false;
        Boolean NumOfInboundChangedWithNumToZero = false;

        if (NumOfInbounds == 0 && CurrentNumOfInbounds > 0) {
            NumOfInboundChangedWithZeroToNum = true;
        }

        if (NumOfInbounds > 0 && CurrentNumOfInbounds == 0) {
            NumOfInboundChangedWithNumToZero = true;
        }

        if (NumOfInboundChangedWithZeroToNum) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has changed from 0 to >0.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Inbound Traffic Headway Frequency Distribution Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (NumOfInboundChangedWithNumToZero) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has changed from >0 to 0.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Inbound Traffic Headway Frequency Distribution Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else {
            new LegInboundDialog(leg_number);
        }
    } // end of method InboundAction()

    void OutboundAction() {
        int NumOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
        int CurrentNumOfOutbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue();

        Boolean NumOfOutboundChangedWithZeroToNum = false;
        Boolean NumOfOutboundChangedWithNumToZero = false;

        if (NumOfOutbounds == 0 && CurrentNumOfOutbounds > 0) {
            NumOfOutboundChangedWithZeroToNum = true;
        }

        if (NumOfOutbounds > 0 && CurrentNumOfOutbounds == 0) {
            NumOfOutboundChangedWithNumToZero = true;
        }

        if (NumOfOutboundChangedWithZeroToNum) {
            JOptionPane.showMessageDialog(null, "The Number of Outbound Lanes has changed from 0 to >0.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Outbound Traffic Destination Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (NumOfOutboundChangedWithNumToZero) {
            JOptionPane.showMessageDialog(null, "The Number of Outbound Lanes has changed from >0 to 0.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Outbound Traffic Destination Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else {
            new LegOutboundDialog(leg_number);
        }
    } // end of method OutboundAction

    void VaryTrafAction() {
        int LengthOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
        int LengthOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
        int CurrentLengthOfInbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_LEN_INB].getSelectedItem().toString()).intValue();
        int CurrentLengthOfOutbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_LEN_OUT].getSelectedItem().toString()).intValue();

        int NumOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        int NumOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
        int CurrentNumOfInbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue();
        int CurrentNumOfOutbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue();

        if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds) && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Outbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Inbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Inbound Lanes (ft) has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Number of Inbound Lanes has been modified.\nThe Number of Outbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane
                    .showMessageDialog(
                            null,
                            "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                                    + leg_number + " Geometry Data before accessing the Varying Traffic Period Data for Leg " + leg_number + ".",
                            "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else {
            new LegVaryTrafDialog(leg_number);
        }
    } // end of method VaryTrafAction

    void LaneAction() {
        int LengthOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
        int LengthOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
        int CurrentLengthOfInbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_LEN_INB].getSelectedItem().toString()).intValue();
        int CurrentLengthOfOutbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_LEN_OUT].getSelectedItem().toString()).intValue();

        int NumOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        int NumOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
        int CurrentNumOfInbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue();
        int CurrentNumOfOutbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue();

        if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds) && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number + " Geometry Data before accessing the Lane Data for Leg "
                    + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Outbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Inbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Inbound Lanes (ft) has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds == CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number + " Geometry Data before accessing the Lane Data for Leg "
                    + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds == CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Number of Inbound Lanes has been modified.\nThe Number of Outbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null, "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg " + leg_number
                    + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds == CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds == CurrentLengthOfOutbounds)) {
            JOptionPane.showMessageDialog(null,
                    "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nPlease OK or Apply the Leg "
                            + leg_number + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".",
                    "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if ((NumOfInbounds != CurrentNumOfInbounds) && (LengthOfInbounds != CurrentLengthOfInbounds) && (NumOfOutbounds != CurrentNumOfOutbounds)
                && (LengthOfOutbounds != CurrentLengthOfOutbounds)) {
            JOptionPane
                    .showMessageDialog(
                            null,
                            "The Number of Inbound Lanes has been modified.\nThe Length of Inbound Lanes (ft) has been modified.\nThe Number of Outbound Lanes has been modified.\nThe Length of Outbound Lanes (ft) has been modified.\nPlease OK or Apply the Leg "
                                    + leg_number + " Geometry Data before accessing the Lane Data for Leg " + leg_number + ".",
                            "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else {
            new LegLaneDialog(leg_number);
        }
    } // end of method LaneAction

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO];

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == comboBox_leg_geo[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field],
                                comboBox_leg_geo[lsiv_field].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field]));
                    }
                }

                if (event.getSource() == comboBox_radius) {
                    for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
                        if (lsiv_leg == leg_number) {
                            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN];

                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(leg_number)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1
                                            - 1 + lsiv_leg],
                                    comboBox_radius.getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1
                                            + lsiv_leg]),
                                    " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg]));
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
                else if (event.getSource() == laneButton) {
                    new HelpDialog(true, "Lane Data Button", "The Lane Data Dialog allows the user to specify the Width, Turns, Lane Configuration, and Percent Traffic on each lane.",
                            "If you change the number of Inbound Lanes and/or the number of Outbound Lanes then you must Apply the modified value(s) before accessing the Lane Data.", " ", " ", " ",
                            " ", " ", " ");
                }
                else if (event.getSource() == inboundButton) {
                    new HelpDialog(
                            true,
                            "Inbound Data Button",
                            "The Inbound Data Dialog allows the user to specify the Traffic Data which includes Headway Frequency Distribution, Volume, Speed, Traffic Mix Data, and Random Number Seed.",
                            " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == outboundButton) {
                    new HelpDialog(true, "Outbound Data Button", "The Outbound Data Dialog allows the user to specify percent of traffic on this leg traveling to each destination leg.", " ", " ",
                            " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == sdrButton) {
                    new HelpDialog(true, "Sight Distance Restriction Data Button",
                            "The Sight Distance Restriction Data Dialog allows the user to specify the Sight Distance Restriction locations for this leg.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == varyTrafButton) {
                    new HelpDialog(true, "Varying Traffic Period Data Button", "The Varying Traffic Period Data Dialog allows the user to specify the data for up to " + PARAMS.TEXAS_MODEL_NVT
                            + " time periods wherein various traffic parameters may be different from the previous or next time period for this leg.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }
    } // end of class HelpListener

    void setDefaultValueWithoutInboundLane() {
        gdvsim.gclv_inter.mboa_Leg_Inbound_Data_OK[leg_number] = true;
        gdvsim.gclv_inter.mboa_Leg_Traffic_Mix_OK[leg_number] = true;

        TX_Fmt lclv_tx_fmt;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_dist = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_vol = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_tm = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_seed = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED];

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED] = gdvsim.gclv_inter.TX_FROM_USER;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[1] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[2] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_02];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[3] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_03];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[4] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_04];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[5] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_05];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[6] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_06];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[7] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_07];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[8] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_08];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[9] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_09];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[10] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_10];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[11] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_11];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[12] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_12];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[13] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_13];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[14] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_14];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[15] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_15];

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_02] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_03] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_04] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_05] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_06] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_07] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_08] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_09] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_10] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_11] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_12] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_13] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_14] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_15] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[leg_number] = true;

        int number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST];

        if (number_of_legs == 3) {
            if (leg_number == 1) {
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 50;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 50;
            }

            if (leg_number == 2) {
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 50;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 50;
            }

            if (leg_number == 3) {
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 50;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 50;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
            }
        }

        if (number_of_legs == 4) {
            if (leg_number == 1) {
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 33;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;
            }

            if (leg_number == 2) {
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 33;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;
            }

            if (leg_number == 3) {
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 34;
            }

            if (leg_number == 4) {
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 33;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 33;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 34;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
            }
        }

        if (number_of_legs == 5) {
            if (leg_number == 1) {
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;
            }

            if (leg_number == 2) {
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;
            }

            if (leg_number == 3) {
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;
            }

            if (leg_number == 4) {
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 25;
            }

            if (leg_number == 5) {
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 25;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 25;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 25;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 25;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
            }
        }

        if (number_of_legs == 6) {
            if (leg_number == 1) {
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 0;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
                gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;
            }

            if (leg_number == 2) {
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 0;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
                gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;
            }

            if (leg_number == 3) {
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 0;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
                gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;
            }

            if (leg_number == 4) {
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 0;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
                gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;
            }

            if (leg_number == 5) {
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 0;
                gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 20;
            }

            if (leg_number == 6) {
                gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = 20;
                gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = 20;
                gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = 20;
                gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = 20;
                gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = 20;
                gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = 0;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_2] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_3] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_4] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_5] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_6] = gdvsim.gclv_inter.TX_FROM_USER;
        }
    }

    void saveData() {
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_leg_no = leg_number;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_LEG_NO] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_ang = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_ANG].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_LEN_INB].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_LEN_OUT].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_sl_inb = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_SL_INB].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_sl_out = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_SL_OUT].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_cl_off = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_CL_OFF].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_med_w = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_MED_W].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_ang_s = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_ANG_S].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_ang_u = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_ANG_U].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.msia_radius[leg_number] = Integer.valueOf(comboBox_radius.getSelectedItem().toString()).intValue();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_ANG - 1 + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            if (lsiv_leg == leg_number) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_curb.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_CURB_RETURN_RADIUS_1 - 1 + lsiv_leg] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }
        gdvsim.gclv_inter.calculate_graphics_and_paint();
    } // end of method saveData

    boolean isError() {
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                    || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                if (gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                            + " Inbound Lane" + (gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
                if (gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
                            + " Outbound Lane" + (gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
                if (gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                            + " Inbound Lane" + (gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
                if (gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
                            + " Outbound Lane" + (gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "")
                            + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        }
        else {
            int inbound_lane_number = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue();
            int outbound_lane_number = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue();

            if ((inbound_lane_number == 0) && (outbound_lane_number == 0)) {
                JOptionPane.showMessageDialog(null, "Please input inbound and/or outbound lanes.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }
        return false;
    } // end of method isError

    int setDefaultForPctValue(int lane, int number_of_bounds) {
        int retValue = 0;

        if (number_of_bounds == 3) {
            if (lane == 3) {
                retValue = 34;
            }
            else {
                retValue = 33;
            }
        }
        else if (number_of_bounds == 6) {
            if (lane == 1 || lane == 2) {
                retValue = 16;
            }
            else {
                retValue = 17;
            }
        }
        else {
            if (number_of_bounds != 0) {
                retValue = 100 / number_of_bounds;
            }
        }
        return retValue;
    } // end of method setDefaultForPctValue

    String setDefaultForMovementCode(int lane, int number_of_bounds) {
        String retValue = "XXXX";

        if (number_of_bounds == 1) {
            if (lane == 1) {
                retValue = "LSR";

                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "SR";
                            break;
                    }
                }
            }
        }

        if (number_of_bounds == 2) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }

            if (lane == 2) {
                retValue = "SR";
            }
        }

        if (number_of_bounds == 3) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 3) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        if (number_of_bounds == 4) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 4) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        if (number_of_bounds == 5) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 5) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }

        if (number_of_bounds == 6) {
            if (lane == 1) {
                retValue = "LS";
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    switch (leg_number) {
                        case 2:
                        case 5:
                            retValue = "S";
                            break;
                    }
                }
            }
            else if (lane == 6) {
                retValue = "SR";
            }
            else {
                retValue = "S";
            }
        }
        return retValue;
    } // end of method setDefaultForMovementCode

    void setInboundLaneInvalid() {
        int number_of_inbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_INB].getSelectedItem().toString()).intValue();

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA];

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = setDefaultForMovementCode(lsiv_lane, number_of_inbounds);
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(setDefaultForPctValue(lsiv_lane, number_of_inbounds)).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE];

            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method setInboundLaneInvalid

    void setInboundLaneLengthInvalid() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA];

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN]).intValue();

            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method setInboundLaneLengthInvalid

    void setOutboundLaneInvalid() {
        int number_of_outbounds = Integer.valueOf(comboBox_leg_geo[GDV_LEG_GEO_NO_OUT].getSelectedItem().toString()).intValue();

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA];

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = setDefaultForMovementCode(lsiv_lane, number_of_outbounds);
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE];

            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_WIDTH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_SL_OFF] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method setOutboundLaneInvalid

    void setOutboundLaneLengthInvalid() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA];

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN]).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = Integer.valueOf(
                    lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN]).intValue();

            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_INR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mcla_leg[leg_number].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_OTR_OPN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method setOutboundLaneLengthInvalid

    int setOutboundDestinationDefault(int lsiv_leg) {
        int retValue = 0;
        int number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        for (int lsiv_numOfLegs = 1; lsiv_numOfLegs <= number_of_legs; lsiv_numOfLegs++) {
            if (number_of_legs == lsiv_numOfLegs) {
                int countNoOutbound = 0;
                int lastLegWithOutbound;

                if (leg_number == number_of_legs) {
                    lastLegWithOutbound = number_of_legs - 1;
                }
                else {
                    lastLegWithOutbound = number_of_legs;
                }

                for (int lsiv_legNum = 1; lsiv_legNum <= number_of_legs; lsiv_legNum++) {
                    if (lsiv_legNum == leg_number)
                        continue;

                    if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_legNum]
                            && (gdvsim.gclv_inter.mcla_leg[lsiv_legNum].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                            && (gdvsim.gclv_inter.mcla_leg[lsiv_legNum].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
                        countNoOutbound++;
                    }
                    else {
                        lastLegWithOutbound = lsiv_legNum;
                    }
                }

                int eachValue = 0;
                int lastValue = 0;
                boolean allOutboundZero = false;

                if (countNoOutbound == (number_of_legs - 1)) {
                    countNoOutbound = 0;
                    allOutboundZero = true;
                }

                int divided = number_of_legs - 1 - countNoOutbound;

                if (divided == 3) {
                    eachValue = 33;
                    lastValue = 34;
                }
                else {
                    eachValue = 100 / divided;
                    lastValue = 100 / divided;
                }

                for (int lsiv_legNum = 1; lsiv_legNum <= number_of_legs; lsiv_legNum++) {
                    if (lsiv_legNum == lsiv_leg) {
                        if (allOutboundZero) {
                            retValue = 0;
                        }
                        else {
                            if (lsiv_legNum == leg_number) {
                                retValue = 0;
                            }
                            else if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_legNum]
                                    && (gdvsim.gclv_inter.mcla_leg[lsiv_legNum].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                                    && (gdvsim.gclv_inter.mcla_leg[lsiv_legNum].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
                                retValue = 0;
                            }
                            else {
                                if (lsiv_legNum == lastLegWithOutbound) {
                                    retValue = lastValue;
                                }
                                else {
                                    retValue = eachValue;
                                }
                            }
                        }
                    } // end of if(lsiv_legNum == lsiv_leg)
                } // end of for(int lsiv_legNum = 1; lsiv_legNum <= number_of_legs; lsiv_legNum++)
            } // end of if(number_of_legs == lsiv_numOfLegs)

        } // end of for(int lsiv_legNum = 1; lsiv_legNum <= number_of_legs; lsiv_legNum++)

        return retValue;

    } // end of method setOutboundDestinationDefault

    void setOutboundDestinationInvalid() {
        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg] = Integer.valueOf(setOutboundDestinationDefault(lsiv_leg)).intValue();
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1
                    + lsiv_leg] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

    } // end of method setOutboundDestinationInvalid

    void setTrafficMixDataInvalid() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

        for (int lsiv_traf = 1; lsiv_traf <= PARAMS.TEXAS_MODEL_NVC; lsiv_traf++) {
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mdfa_per_veh_class[lsiv_traf - 1] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                    + lsiv_traf];
            gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                    + lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method setTrafficMixDataInvalid

    void setInboundTrafficInvalid() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY];

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_dist = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_vol = Integer.valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL]).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par = Double.valueOf(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR]).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean = Double.valueOf(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN]).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th = Double.valueOf(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH]).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_tm = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM];
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_seed = Integer.valueOf(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED]).intValue();

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_SEED] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
    } // end of method setInboundTrafficInvalid

    void setInvalid() {
        int LengthOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
        int LengthOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
        int CurrentLengthOfInbounds = Integer.valueOf(comboBox_leg_geo[2].getSelectedItem().toString()).intValue();
        int CurrentLengthOfOutbounds = Integer.valueOf(comboBox_leg_geo[3].getSelectedItem().toString()).intValue();

        int NumOfInbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        int NumOfOutbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
        int CurrentNumOfInbounds = Integer.valueOf(comboBox_leg_geo[4].getSelectedItem().toString()).intValue();
        int CurrentNumOfOutbounds = Integer.valueOf(comboBox_leg_geo[5].getSelectedItem().toString()).intValue();

        Boolean LengthOfInboundChanged = false;
        Boolean LengthOfOutboundChanged = false;
        Boolean NumOfInboundChanged = false;
        Boolean NumOfOutboundChanged = false;
        Boolean NumOfInbChanged_Zero_To_NonZero = false;
        Boolean NumOfInbChanged_NonZero_To_Zero = false;
        Boolean NumOfInbChanged_NonZero_To_NonZero = false;
        Boolean NumOfOutChanged_Zero_To_NonZero = false;
        Boolean NumOfOutChanged_NonZero_To_Zero = false;
        Boolean NumOfOutChanged_NonZero_To_NonZero = false;

        if (LengthOfInbounds != CurrentLengthOfInbounds) {
            LengthOfInboundChanged = true;
        }

        if (LengthOfOutbounds != CurrentLengthOfOutbounds) {
            LengthOfOutboundChanged = true;
        }

        if (NumOfInbounds != CurrentNumOfInbounds) {
            NumOfInboundChanged = true;

            if ((NumOfInbounds == 0 && CurrentNumOfInbounds != 0)) {
                NumOfInbChanged_Zero_To_NonZero = true;
            }
            else if ((NumOfInbounds != 0 && CurrentNumOfInbounds == 0)) {
                NumOfInbChanged_NonZero_To_Zero = true;
            }
            else if ((NumOfInbounds != 0 && CurrentNumOfInbounds != 0)) {
                NumOfInbChanged_NonZero_To_NonZero = true;
            }
        }

        if (NumOfOutbounds != CurrentNumOfOutbounds) {
            NumOfOutboundChanged = true;

            if ((NumOfOutbounds == 0 && CurrentNumOfOutbounds != 0)) {
                NumOfOutChanged_Zero_To_NonZero = true;
            }
            else if ((NumOfOutbounds != 0 && CurrentNumOfOutbounds == 0)) {
                NumOfOutChanged_NonZero_To_Zero = true;
            }
            else if ((NumOfOutbounds != 0 && CurrentNumOfOutbounds != 0)) {
                NumOfOutChanged_NonZero_To_NonZero = true;
            }
        }

        if (!LengthOfInboundChanged && !LengthOfOutboundChanged && !NumOfInboundChanged && !NumOfOutboundChanged) {
            return;
        }
        else if (NumOfInbChanged_Zero_To_NonZero || NumOfInbChanged_NonZero_To_Zero) {
            setInboundLaneInvalid();
            setInboundTrafficInvalid();
            setTrafficMixDataInvalid();
            setOutboundDestinationInvalid();

            if (NumOfInbChanged_Zero_To_NonZero) {
                JOptionPane.showMessageDialog(null, "You have changed the number of inbound lanes from " + NumOfInbounds + " to " + CurrentNumOfInbounds + " for Leg " + leg_number + "\n"
                        + "therefore you must Apply or OK in\n" + "Lane Data, Inbound Traffic Headway Frequency Distribution Data, Traffic Mix Data, and Outbound Traffic Destination Data for Leg "
                        + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
            }

            if (NumOfInbChanged_NonZero_To_Zero) {
                JOptionPane.showMessageDialog(null, "You have changed the number of inbound lanes from " + NumOfInbounds + " to " + CurrentNumOfInbounds + " for Leg " + leg_number + "\n"
                        + "therefore you must Apply or OK in\n" + "Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
        }
        else if (NumOfInbChanged_NonZero_To_NonZero) {
            setInboundLaneInvalid();

            JOptionPane.showMessageDialog(null, "You have changed the number of inbound lanes from " + NumOfInbounds + " to " + CurrentNumOfInbounds + " for Leg " + leg_number + "\n"
                    + "therefore you must Apply or OK in\n" + "Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (NumOfOutChanged_Zero_To_NonZero || NumOfOutChanged_NonZero_To_Zero) {
            setOutboundLaneInvalid();
            setOutboundDestinationInvalid();

            JOptionPane.showMessageDialog(null, "You have changed the number of outbound lanes from " + NumOfOutbounds + " to " + CurrentNumOfOutbounds + " for Leg " + leg_number + "\n"
                    + "therefore you must Apply or OK in\n" + "Lane Data and Outbound Traffic Destination Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (NumOfOutChanged_NonZero_To_NonZero) {
            setOutboundLaneInvalid();

            JOptionPane.showMessageDialog(null, "You have changed the number of outbound lanes from " + NumOfOutbounds + " to " + CurrentNumOfOutbounds + " for Leg " + leg_number + "\n"
                    + "therefore you must Apply or OK in\n" + "Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (LengthOfInboundChanged) {
            setInboundLaneLengthInvalid();

            JOptionPane.showMessageDialog(null, "You have changed the length of outbound lanes from " + LengthOfInbounds + " to " + CurrentLengthOfInbounds + " for Leg " + leg_number + "\n"
                    + "therefore you must Apply or OK in\n" + "Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
        else if (LengthOfOutboundChanged) {
            setOutboundLaneLengthInvalid();

            JOptionPane.showMessageDialog(null, "You have changed the length of outbound lanes from " + LengthOfOutbounds + " to " + CurrentLengthOfOutbounds + " for Leg " + leg_number + "\n"
                    + "therefore you must Apply or OK in\n" + "Lane Data for Leg " + leg_number + ".", "Warning Message", JOptionPane.WARNING_MESSAGE);
        }
    } // end of method setInvalid

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    setInvalid();

                    gdvsim.gclv_inter.mboa_Leg_OK[leg_number] = true;

                    saveData();

                    gdvsim.myIntersection.setVisible(true);
                    gdvsim.myIntersection.draw();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
            else if (event.getSource() == laneButton) {
                LaneAction();
            }
            else if (event.getSource() == varyTrafButton) {
                VaryTrafAction();
            }
            else if (event.getSource() == sdrButton) {
                new LegSDRDialog(leg_number);
            }
            else if (event.getSource() == outboundButton) {
                OutboundAction();
            }
            else if (event.getSource() == inboundButton) {
                InboundAction();
            }
            else if (event.getSource() == comboBox_leg_geo[GDV_LEG_GEO_NO_INB]) {
                CboInboundAction();
            }
            else if (event.getSource() == comboBox_leg_geo[GDV_LEG_GEO_NO_OUT]) {
                CboInboundAction();
            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        setInvalid();

                        gdvsim.gclv_inter.mboa_Leg_OK[leg_number] = true;

                        saveData();

                        gdvsim.myIntersection.setVisible(true);
                        gdvsim.myIntersection.draw();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
                else if (event.getSource() == laneButton) {
                    LaneAction();
                }
                else if (event.getSource() == varyTrafButton) {
                    VaryTrafAction();
                }
                else if (event.getSource() == sdrButton) {
                    new LegSDRDialog(leg_number);
                }
                else if (event.getSource() == outboundButton) {
                    OutboundAction();
                }
                else if (event.getSource() == inboundButton) {
                    InboundAction();
                }
                else if (event.getSource() == comboBox_leg_geo[GDV_LEG_GEO_NO_INB]) {
                    CboInboundAction();
                }
                else if (event.getSource() == comboBox_leg_geo[GDV_LEG_GEO_NO_OUT]) {
                    CboInboundAction();
                }
            }
        } // end of method keyPressed
    }// end of OKApplyButtonKeyListener

} // end of class LegDataDialog
