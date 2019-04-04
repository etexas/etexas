package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              arcDialog.java                                */
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

class ArcDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 5;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    TextFieldFocusListener textFieldFocusListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    JComboBox[][] comboBox_arc = new JComboBox[PARAMS.TEXAS_MODEL_NAR + 1][NUMOFFIELD + 1];

    JTextField[][] text_arc = new JTextField[PARAMS.TEXAS_MODEL_NAR + 1][NUMOFFIELD + 1];

    JLabel[] label_arc = new JLabel[PARAMS.TEXAS_MODEL_NAR + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NAR + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NAR + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NAR + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NAR + 1];

    JComboBox cbo_total;

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_total, label_note;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    int numOfArc;

    String titleString;

    public ArcDialog() {
        if (gdvsim.flag_arc_ok) {
            numOfArc = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs;
        }
        else {
            numOfArc = 0;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA];

        titleString = "Arc Data (For Plotting Purposes Only)";
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

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            label_arc[lsiv_arc] = new JLabel("Arc " + lsiv_arc);
        }

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            add[lsiv_arc] = new JButton("Add");
            del[lsiv_arc] = new JButton("Delete");
            up[lsiv_arc] = new JButton("Up");
            down[lsiv_arc] = new JButton("Down");
        }

        label_total = new JLabel("Total Arcs");
        label_note = new JLabel("NOTE:   The center of the intersection is located at coordinate X = " + PARAMS.TEXAS_MODEL_XYCNTR + " and Y = " + PARAMS.TEXAS_MODEL_XYCNTR + " feet.  END NOTE");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);

            if (lsiv_field == 4) {
                label_field[lsiv_field].setSize(new Dimension(150, 10));
            }
            else {
                label_field[lsiv_field].setSize(new Dimension(80, 10));
            }

            label_field[lsiv_field].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]);
        }

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                text_arc[lsiv_arc][lsiv_field] = new JTextField();
                text_arc[lsiv_arc][lsiv_field].setBackground(aFrame.getBackground());
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                int lsiv_min;
                int lsiv_max;
                int lsiv_inc;
                int count;
                int int_number;

                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field];

                count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
                int_number = lsiv_min;
                String[] array_arc = new String[count];
                for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                    array_arc[lsiv_index] = Integer.toString(int_number);
                    int_number += lsiv_inc;
                }

                comboBox_arc[lsiv_arc][lsiv_field] = new JComboBox(array_arc);
            }
        }

        if (gdvsim.flag_arc_ok) {
            for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_arc[lsiv_arc][1].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X]));
                }
                else {
                    text_arc[lsiv_arc][1].setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_center_x));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_Y] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_arc[lsiv_arc][2].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_Y]));
                }
                else {
                    text_arc[lsiv_arc][2].setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_center_y));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_BEGIN_AZIMUTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_arc[lsiv_arc][3].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_BEGIN_AZIMUTH]));
                }
                else {
                    comboBox_arc[lsiv_arc][3].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_begin_azimuth));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_SWEEP_ANGLE] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_arc[lsiv_arc][4].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_SWEEP_ANGLE]));
                }
                else {
                    comboBox_arc[lsiv_arc][4].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_sweep_angle));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_RADIUS] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_arc[lsiv_arc][5].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_RADIUS]));
                }
                else {
                    comboBox_arc[lsiv_arc][5].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_radius));
                }
            }
        }
        else {
            for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                    text_arc[lsiv_arc][lsiv_field].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]));
                }

                for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_arc[lsiv_arc][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]));
                }
            }
        }

        setStatus(numOfArc);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfArc()));

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

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        textFieldFocusListener = new TextFieldFocusListener();

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            add[lsiv_arc].addActionListener(componentActionListener);
            del[lsiv_arc].addActionListener(componentActionListener);
            up[lsiv_arc].addActionListener(componentActionListener);
            down[lsiv_arc].addActionListener(componentActionListener);

            add[lsiv_arc].addKeyListener(componentKeyListener);
            del[lsiv_arc].addKeyListener(componentKeyListener);
            up[lsiv_arc].addKeyListener(componentKeyListener);
            down[lsiv_arc].addKeyListener(componentKeyListener);

            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                text_arc[lsiv_arc][lsiv_field].addKeyListener(helpListener);
                text_arc[lsiv_arc][lsiv_field].addFocusListener(textFieldFocusListener);
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_arc[lsiv_arc][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_arc[lsiv_arc][lsiv_field].addKeyListener(helpListener);
            }

            add[lsiv_arc].addKeyListener(helpListener);
            del[lsiv_arc].addKeyListener(helpListener);
            up[lsiv_arc].addKeyListener(helpListener);
            down[lsiv_arc].addKeyListener(helpListener);
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
        int iCol = 9;

        gbConstraints.insets = new Insets(1, 0, 0, 0);
        addComponent(panel_title, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(1, 20, 6, 0);
        addComponent(label_note, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1);
        }

        iRow++;

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            gbConstraints.insets = new Insets(1, 1, 1, 1);
            addComponent(label_arc[lsiv_arc], iRow, 0, 1, 1);
            addComponent(add[lsiv_arc], iRow, 1, 1, 1);
            addComponent(del[lsiv_arc], iRow, 2, 1, 1);
            addComponent(up[lsiv_arc], iRow, 3, 1, 1);
            addComponent(down[lsiv_arc], iRow, 4, 1, 1);

            gbConstraints.insets = new Insets(1, 2, 1, 2);

            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                addComponent(text_arc[lsiv_arc][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(comboBox_arc[lsiv_arc][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
            }

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(cbo_total, iRow, 1, 1, 1);
        addComponent(label_total, iRow++, 2, 1, 1);
        addComponent(ok_panel, iRow++, 0, iCol, 1);

        String fname = "Times New Roman";
        int fsize = 11;
        String fstyle = "BOLD";

        setSize(fname, fstyle, fsize);

        aFrame.setSize(950, 800);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method ArcDialog()

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getNumberOfArc() {
        int sum = 0;
        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            if (comboBox_arc[lsiv_arc][3].isEnabled())
                sum++;
        }

        return sum;
    } // end of method getNumberOfArc()

    void setSize(String fname, String fstyle, int fsize) {
        Font vfont;

        if (fstyle.equals("PLAIN")) {
            vfont = new Font(fname, Font.PLAIN, fsize);
        }
        else {
            vfont = new Font(fname, Font.BOLD, fsize);
        }

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            label_arc[lsiv_arc].setFont(vfont);

            add[lsiv_arc].setFont(vfont);
            del[lsiv_arc].setFont(vfont);
            up[lsiv_arc].setFont(vfont);
            down[lsiv_arc].setFont(vfont);

            add[lsiv_arc].setMinimumSize(new Dimension(5, 10));
            del[lsiv_arc].setMinimumSize(new Dimension(5, 10));
            up[lsiv_arc].setMinimumSize(new Dimension(5, 10));
            down[lsiv_arc].setMinimumSize(new Dimension(5, 10));

            add[lsiv_arc].setMaximumSize(new Dimension(5, 10));
            del[lsiv_arc].setMaximumSize(new Dimension(5, 10));
            up[lsiv_arc].setMaximumSize(new Dimension(5, 10));
            down[lsiv_arc].setMaximumSize(new Dimension(5, 10));

            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                text_arc[lsiv_arc][lsiv_field].setFont(vfont);
                text_arc[lsiv_arc][lsiv_field].setSize(new Dimension(5, 10));
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_arc[lsiv_arc][lsiv_field].setFont(vfont);
                comboBox_arc[lsiv_arc][lsiv_field].setSize(new Dimension(5, 10));
            }

            okButton.setMinimumSize(new Dimension(5, 10));
            applyButton.setMinimumSize(new Dimension(5, 10));
            cancelButton.setMinimumSize(new Dimension(5, 10));
            okButton.setMaximumSize(new Dimension(5, 10));
            applyButton.setMaximumSize(new Dimension(5, 10));
            cancelButton.setMaximumSize(new Dimension(5, 10));
        }
    } // end of method setSize()

    class TextFieldFocusListener implements FocusListener {

        public void focusLost(FocusEvent event) {
            int sumOfArcs = getNumberOfArc();
            for (int lsiv_arc = 1; lsiv_arc <= sumOfArcs; lsiv_arc++) {
                for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                    if (event.getSource() == text_arc[lsiv_arc][lsiv_field]) {
                        text_arc[lsiv_arc][lsiv_field].setBackground(aFrame.getBackground());
                    }
                }
            }
        }

        public void focusGained(FocusEvent event) {
            int sumOfArcs = getNumberOfArc();
            for (int lsiv_arc = 1; lsiv_arc <= sumOfArcs; lsiv_arc++) {
                for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                    if (event.getSource() == text_arc[lsiv_arc][lsiv_field]) {
                        text_arc[lsiv_arc][lsiv_field].setBackground(new Color(176, 197, 218));
                    }
                }
            }
        }
    } // end of TextFieldFocusListener()

    void setAccessiblility() {
        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                text_arc[lsiv_arc][lsiv_field].getAccessibleContext()
                        .setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field] + " for Arc " + lsiv_arc);
                text_arc[lsiv_arc][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field] + " for Arc " + lsiv_arc);
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_arc[lsiv_arc][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field] + " for Arc " + lsiv_arc);
                comboBox_arc[lsiv_arc][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field] + " for Arc " + lsiv_arc);
            }

            add[lsiv_arc].getAccessibleContext().setAccessibleName("Add    for Arc " + lsiv_arc);
            add[lsiv_arc].getAccessibleContext().setAccessibleDescription("Add    for Arc " + lsiv_arc);
            del[lsiv_arc].getAccessibleContext().setAccessibleName("Delete for Arc " + lsiv_arc);
            del[lsiv_arc].getAccessibleContext().setAccessibleDescription("Delete for Arc " + lsiv_arc);
            up[lsiv_arc].getAccessibleContext().setAccessibleName("Up     for Arc " + lsiv_arc);
            up[lsiv_arc].getAccessibleContext().setAccessibleDescription("Up     for Arc " + lsiv_arc);
            down[lsiv_arc].getAccessibleContext().setAccessibleName("Down   for Arc " + lsiv_arc);
            down[lsiv_arc].getAccessibleContext().setAccessibleDescription("Down   for Arc " + lsiv_arc);
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
    } // end of method OpenComboMenuListener()

    void setStatus(int sumOfArcs) {
        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            if (lsiv_arc <= sumOfArcs) {
                label_arc[lsiv_arc].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                    text_arc[lsiv_arc][lsiv_field].setEditable(true);
                }

                for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_arc[lsiv_arc][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_arc[lsiv_arc].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                    text_arc[lsiv_arc][lsiv_field].setEditable(false);
                }

                for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_arc[lsiv_arc][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfArcs == PARAMS.TEXAS_MODEL_NAR) {
            for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                add[lsiv_arc].setEnabled(false);
            }
        }
        else {
            for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                if (lsiv_arc <= (sumOfArcs + 1)) {
                    add[lsiv_arc].setEnabled(true);
                }
                else {
                    add[lsiv_arc].setEnabled(false);
                }
            }
        }

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            if (lsiv_arc <= sumOfArcs) {
                del[lsiv_arc].setEnabled(true);
            }
            else {
                del[lsiv_arc].setEnabled(false);
            }
        }

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            if (lsiv_arc <= sumOfArcs) {
                up[lsiv_arc].setEnabled(true);
            }
            else {
                up[lsiv_arc].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            if (lsiv_arc < sumOfArcs) {
                down[lsiv_arc].setEnabled(true);
            }
            else {
                down[lsiv_arc].setEnabled(false);
            }
        }

        setTextFieldColor();

    } // end of method setStatus

    void setTextFieldColor() {
        for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                if (text_arc[lsiv_arc][lsiv_field].isEditable()) {
                    text_arc[lsiv_arc][lsiv_field].setBackground(aFrame.getBackground());
                    text_arc[lsiv_arc][lsiv_field].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
                    text_arc[lsiv_arc][lsiv_field].setForeground(new Color(0, 0, 0));
                }
                else {
                    text_arc[lsiv_arc][lsiv_field].setBackground(aFrame.getBackground());
                    text_arc[lsiv_arc][lsiv_field].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
                    text_arc[lsiv_arc][lsiv_field].setForeground(new Color(176, 197, 218));
                }
            }
        }
    } // end of method setTextFieldColor

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_arc = sum; lsiv_arc >= index; lsiv_arc--) {
            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                text_arc[lsiv_arc + 1][lsiv_field].setText(text_arc[lsiv_arc][lsiv_field].getText());
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_arc[lsiv_arc + 1][lsiv_field].setSelectedItem(comboBox_arc[lsiv_arc][lsiv_field].getSelectedItem().toString());
            }
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_arc = index; lsiv_arc < sum; lsiv_arc++) {
            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                text_arc[lsiv_arc][lsiv_field].setText(text_arc[lsiv_arc + 1][lsiv_field].getText());
            }

            for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_arc[lsiv_arc][lsiv_field].setSelectedItem(comboBox_arc[lsiv_arc + 1][lsiv_field].getSelectedItem().toString());
            }
        }

        for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
            text_arc[sum][lsiv_field].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]));
        }

        for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            comboBox_arc[sum][lsiv_field].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]));
        }
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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NAR), "1");
                }

                for (int lsiv_arc = 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                    for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                        if (event.getSource() == text_arc[lsiv_arc][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_arc)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field] + " for Arc " + lsiv_arc,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field], text_arc[lsiv_arc][lsiv_field].getText(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]), " ",
                                    Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]));
                        }
                    }

                    for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_arc[lsiv_arc][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_arc)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field] + " for Arc " + lsiv_arc,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field], comboBox_arc[lsiv_arc][lsiv_field].getSelectedItem().toString(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]), " ",
                                    Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field]));
                        }
                    }

                    if (event.getSource() == up[lsiv_arc]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous arc and the current arc.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_arc]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next arc and the current arc.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_arc]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 arc for all arcs below this arc and decreases the Total Arcs by 1.", " ", " ", " ", " ", " ", " ",
                                " ");
                    }
                    else if (event.getSource() == add[lsiv_arc]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 arc for all arcs below this arc, inserts a new arc at the current position, copies the values of all parameters from the previous arc to the new arc, and increases the Total Arcs by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of class HelpListener

    void saveData() {
        int numOfArcs = getNumberOfArc();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs = numOfArcs;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_arc = 1; lsiv_arc <= numOfArcs; lsiv_arc++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_arc_number = lsiv_arc;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_center_x = Integer.valueOf(text_arc[lsiv_arc][1].getText().trim()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_center_y = Integer.valueOf(text_arc[lsiv_arc][2].getText().trim()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_begin_azimuth = Integer.valueOf(comboBox_arc[lsiv_arc][3].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_sweep_angle = Integer.valueOf(comboBox_arc[lsiv_arc][4].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_radius = Integer.valueOf(comboBox_arc[lsiv_arc][5].getSelectedItem().toString()).intValue();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_ARC_NUMBER] = gdvsim.gclv_inter.TX_FROM_USER;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_arc = numOfArcs + 1; lsiv_arc <= PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_ARC_NUMBER] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    } // end of method saveData()

    boolean isError() {
        int numOfArcs = getNumberOfArc();

        for (int lsiv_arc = 1; lsiv_arc <= numOfArcs; lsiv_arc++) {
            for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                String fld_name;

                fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_ARC_DATA_CENTER_X - 1 + lsiv_field];

                if (text_arc[lsiv_arc][lsiv_field].isEditable()) {
                    if (text_arc[lsiv_arc][lsiv_field].getText().trim().length() == 0) {
                        JOptionPane.showMessageDialog(null, "Arc " + lsiv_arc + " " + fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }

                    int lsiv_value;

                    try {
                        lsiv_value = Integer.parseInt(text_arc[lsiv_arc][lsiv_field].getText().trim());
                    }
                    catch (Exception e) {
                        JOptionPane.showMessageDialog(null, "Arc " + lsiv_arc + " " + fld_name + " = '" + text_arc[lsiv_arc][lsiv_field].getText().trim()
                                + "' contains an illegal character for an integer.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }
                }
            } // end of for(int lsiv_field = 1; lsiv_field <= 2; lsiv_field++)
        } // end of for(int lsiv_arc = 1; lsiv_arc <= numOfArcs; lsiv_arc++)

        return false;

    } // end of method isError

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_arc_ok = true;
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_arc = 0; lsiv_arc < PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                if (event.getSource() == down[lsiv_arc]) {
                    for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                        String temp;
                        temp = text_arc[lsiv_arc][lsiv_field].getText();
                        text_arc[lsiv_arc][lsiv_field].setText(text_arc[lsiv_arc + 1][lsiv_field].getText());
                        text_arc[lsiv_arc + 1][lsiv_field].setText(temp);
                    }

                    for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        String temp;
                        temp = comboBox_arc[lsiv_arc][lsiv_field].getSelectedItem().toString();
                        comboBox_arc[lsiv_arc][lsiv_field].setSelectedItem(comboBox_arc[lsiv_arc + 1][lsiv_field].getSelectedItem().toString());
                        comboBox_arc[lsiv_arc + 1][lsiv_field].setSelectedItem(temp);
                    }
                    break;
                }
                else if (event.getSource() == up[lsiv_arc]) {
                    for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                        String temp;
                        temp = text_arc[lsiv_arc][lsiv_field].getText();
                        text_arc[lsiv_arc][lsiv_field].setText(text_arc[lsiv_arc - 1][lsiv_field].getText());
                        text_arc[lsiv_arc - 1][lsiv_field].setText(temp);
                    }

                    for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        String temp;
                        temp = comboBox_arc[lsiv_arc][lsiv_field].getSelectedItem().toString();
                        comboBox_arc[lsiv_arc][lsiv_field].setSelectedItem(comboBox_arc[lsiv_arc - 1][lsiv_field].getSelectedItem().toString());
                        comboBox_arc[lsiv_arc - 1][lsiv_field].setSelectedItem(temp);
                    }
                    break;
                }
                else if (event.getSource() == del[lsiv_arc]) {
                    int numOfArcBeforeClick;
                    numOfArcBeforeClick = getNumberOfArc();
                    setValueAfterDel(numOfArcBeforeClick, lsiv_arc);
                    setStatus(numOfArcBeforeClick - 1);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfArc()));
                    if (!del[lsiv_arc].isEnabled()) {
                        okButton.requestFocus();
                    }
                    break;
                }
                else if (event.getSource() == add[lsiv_arc]) {
                    int numOfArcBeforeClick;
                    numOfArcBeforeClick = getNumberOfArc();
                    setStatus(numOfArcBeforeClick + 1);
                    setValueAfterAdd(numOfArcBeforeClick, lsiv_arc);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfArc()));
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
                        gdvsim.flag_arc_ok = true;
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_arc = 0; lsiv_arc < PARAMS.TEXAS_MODEL_NAR; lsiv_arc++) {
                    if (event.getSource() == down[lsiv_arc]) {
                        for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                            String temp;
                            temp = text_arc[lsiv_arc][lsiv_field].getText();
                            text_arc[lsiv_arc][lsiv_field].setText(text_arc[lsiv_arc + 1][lsiv_field].getText());
                            text_arc[lsiv_arc + 1][lsiv_field].setText(temp);
                        }

                        for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            String temp;
                            temp = comboBox_arc[lsiv_arc][lsiv_field].getSelectedItem().toString();
                            comboBox_arc[lsiv_arc][lsiv_field].setSelectedItem(comboBox_arc[lsiv_arc + 1][lsiv_field].getSelectedItem().toString());
                            comboBox_arc[lsiv_arc + 1][lsiv_field].setSelectedItem(temp);
                        }
                        break;
                    }
                    else if (event.getSource() == up[lsiv_arc]) {
                        for (int lsiv_field = 1; lsiv_field <= 2; lsiv_field++) {
                            String temp;
                            temp = text_arc[lsiv_arc][lsiv_field].getText();
                            text_arc[lsiv_arc][lsiv_field].setText(text_arc[lsiv_arc - 1][lsiv_field].getText());
                            text_arc[lsiv_arc - 1][lsiv_field].setText(temp);
                        }

                        for (int lsiv_field = 3; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            String temp;
                            temp = comboBox_arc[lsiv_arc][lsiv_field].getSelectedItem().toString();
                            comboBox_arc[lsiv_arc][lsiv_field].setSelectedItem(comboBox_arc[lsiv_arc - 1][lsiv_field].getSelectedItem().toString());
                            comboBox_arc[lsiv_arc - 1][lsiv_field].setSelectedItem(temp);
                        }
                        break;
                    }
                    else if (event.getSource() == del[lsiv_arc]) {
                        int numOfArcBeforeClick;
                        numOfArcBeforeClick = getNumberOfArc();
                        setValueAfterDel(numOfArcBeforeClick, lsiv_arc);
                        setStatus(numOfArcBeforeClick - 1);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfArc()));
                        if (!del[lsiv_arc].isEnabled()) {
                            okButton.requestFocus();
                        }

                        break;
                    }
                    else if (event.getSource() == add[lsiv_arc]) {
                        int numOfArcBeforeClick;
                        numOfArcBeforeClick = getNumberOfArc();
                        setStatus(numOfArcBeforeClick + 1);
                        setValueAfterAdd(numOfArcBeforeClick, lsiv_arc);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfArc()));
                        break;
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentKeyListener

} // end of class ArcDialog.java
