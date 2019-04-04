package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                       TexasDiamondTimingDialog.java                        */
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
import java.awt.event.MouseEvent;
import javax.accessibility.*;

class TexasDiamondTimingDialog extends JDialog {

    int NUMOFFIELD = 5;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel[] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NPH + 1];

    JLabel label_title;

    JComboBox[][] comboBox_timing = new JComboBox[PARAMS.TEXAS_MODEL_NPH + 1][NUMOFFIELD + 1];

    JButton okButton, applyButton, cancelButton;

    JTextArea[] setAllText = new JTextArea[NUMOFFIELD + 1];

    JButton[] setAllButton = new JButton[NUMOFFIELD + 1];

    JComboBox[] setAllComboBox = new JComboBox[NUMOFFIELD + 1];

    JButton intervalButton, optionButton;

    JLabel label_figure_number;

    JComboBox comboBox_figure_number;

    JLabel[] label_note = new JLabel[9];

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    SetAllActionListener setAllActionListener;

    SetAllKeyListener setAllKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    IntervalActionListener intervalActionListener;

    OptionActionListener optionActionListener;

    IntervalKeyListener intervalKeyListener;

    OptionKeyListener optionKeyListener;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    public TexasDiamondTimingDialog() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL];

        titleString = lclv_tx_fmt.mstv_name.substring(8);

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

        font = new Font("TimesRoman", Font.BOLD, 18);
        font1 = new Font("TimesRoman", Font.BOLD, 14);

        label_title = new JLabel(titleString);

        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            label_phase[lsiv_phase] = new JLabel("Phase " + lsiv_phase);
        }

        label_note[1] = new JLabel("NOTE:");
        label_note[2] = new JLabel("Figure 3 has left turns from the interior lanes lag   the opposing arterial phase at both intersections.");
        label_note[3] = new JLabel("Figure 4 has left turns from the interior lanes lead the opposing arterial phase at both intersections.");
        label_note[4] = new JLabel("Figure 6 has left turns from the interior lanes lead the opposing arterial phase at the left intersection");
        label_note[5] = new JLabel("                  and lag   the opposing arterial phase at the right intersection.");
        label_note[6] = new JLabel("Figure 7 has left turns from the interior lanes lag   the opposing arterial phase at the left intersection");
        label_note[7] = new JLabel("                  and lead the opposing arterial phase at the right intersection.");
        label_note[8] = new JLabel("END NOTE");

        label_figure_number = new JLabel("Figure Number");

        String array_figure_number[] = { "3", "4", "6", "7" };
        comboBox_figure_number = new JComboBox(array_figure_number);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            double lsiv_min_double;
            double lsiv_max_double;
            double lsiv_inc_double;

            int count;
            double double_number;

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field];

            count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            double_number = lsiv_min_double;
            String[] array_figure = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_figure[lsiv_i] = oneDigits.format(double_number);
                double_number += lsiv_inc_double;
            }

            setAllComboBox[lsiv_field] = new JComboBox(array_figure);
            setAllComboBox[lsiv_field].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]));

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                comboBox_timing[lsiv_phase][lsiv_field] = new JComboBox(array_figure);
                comboBox_timing[lsiv_phase][lsiv_field].setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]));
            }

            setAllButton[lsiv_field] = new JButton("Set All");
            setAllButton[lsiv_field].setSize(new Dimension(10, 26));

            setAllText[lsiv_field] = new JTextArea();
            setAllText[lsiv_field].setBackground(aFrame.getBackground());
            setAllText[lsiv_field].setEditable(false);
            setAllText[lsiv_field].setWrapStyleWord(true);
            setAllText[lsiv_field].setLineWrap(true);
            setAllText[lsiv_field].setFont(font1);
            setAllText[lsiv_field].setSize(new Dimension(10, 26));

            setAllText[lsiv_field].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);
        }

        if (gdvsim.flag_diamondTiming_ok) {
            comboBox_figure_number.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov));

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                if ((lsiv_phase == 4) || (lsiv_phase == 8))
                    continue;

                comboBox_timing[lsiv_phase][1].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int));
                comboBox_timing[lsiv_phase][2].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int));
                comboBox_timing[lsiv_phase][3].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg));
                comboBox_timing[lsiv_phase][4].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr));
                comboBox_timing[lsiv_phase][5].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext));
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleName("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleDescription("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);

            setAllButton[lsiv_field].getAccessibleContext().setAccessibleName("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleDescription("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_timing[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                        "for figure " + lsiv_phase + " field " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);
                comboBox_timing[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        "for figure " + lsiv_phase + " field " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]);
            }
        }

        comboBox_figure_number.getAccessibleContext().setAccessibleName("Texas Diamond Signal Controller Figure Number");
        comboBox_figure_number.getAccessibleContext().setAccessibleDescription("Texas Diamond Signal Controller Figure Number");

        intervalButton = new JButton("Texas Diamond Special Intervals");
        optionButton = new JButton("Texas Diamond Special Options");

        intervalButton.setMnemonic(KeyEvent.VK_T);
        optionButton.setMnemonic(KeyEvent.VK_P);
        intervalButton.setDisplayedMnemonicIndex(24);

        intervalButton.getAccessibleContext().setAccessibleName("Texas Diamond Special Intervals");
        intervalButton.getAccessibleContext().setAccessibleDescription("Texas Diamond Special Intervals");

        optionButton.setDisplayedMnemonicIndex(23);
        optionButton.getAccessibleContext().setAccessibleName("Texas Diamond Special Options");
        optionButton.getAccessibleContext().setAccessibleDescription("Texas Diamond Special Options");

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
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

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        setAllActionListener = new SetAllActionListener();
        setAllKeyListener = new SetAllKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();

        intervalActionListener = new IntervalActionListener();
        optionActionListener = new OptionActionListener();
        intervalKeyListener = new IntervalKeyListener();
        optionKeyListener = new OptionKeyListener();

        intervalButton.addActionListener(intervalActionListener);
        intervalButton.addKeyListener(intervalKeyListener);
        intervalButton.addKeyListener(helpListener);
        optionButton.addActionListener(optionActionListener);
        optionButton.addKeyListener(optionKeyListener);
        optionButton.addKeyListener(helpListener);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field].addActionListener(setAllActionListener);
            setAllButton[lsiv_field].addKeyListener(setAllKeyListener);
            setAllButton[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].addKeyListener(openComboMenuListener);
            setAllComboBox[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_timing[lsiv_phase][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_timing[lsiv_phase][lsiv_field].addKeyListener(helpListener);
            }
        }

        comboBox_figure_number.addKeyListener(openComboMenuListener);
        comboBox_figure_number.addKeyListener(helpListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        int iRow = 0;
        int numOfColumns = 13;

        gbConstraints.insets = new Insets(2, 2, 20, 2);
        addComponent(panel_title, iRow++, 0, 8, 1, 0);

        gbConstraints.insets = new Insets(1, 1, 2, 3);

        for (int lsiv_note = 1; lsiv_note <= 8; lsiv_note++) {
            addComponent(label_note[lsiv_note], iRow++, 0, 8, 1, 0);
        }

        gbConstraints.insets = new Insets(5, 3, 2, 3);
        addComponent(label_figure_number, iRow, 1, 1, 1, 0);
        gbConstraints.insets = new Insets(2, 3, 2, 3);
        addComponent(comboBox_figure_number, iRow++, 2, 1, 1, 0);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllText[lsiv_field], iRow, lsiv_field + 1, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllComboBox[lsiv_field], iRow, lsiv_field + 1, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllButton[lsiv_field], iRow, lsiv_field + 1, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if ((lsiv_phase == 4) || (lsiv_phase == 8))
                continue;

            addComponent(label_phase[lsiv_phase], iRow, 1, 1, 1, 0);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(comboBox_timing[lsiv_phase][lsiv_field], iRow, lsiv_field + 1, 1, 1, 0);
            }

            iRow++;
        }

        addComponent(intervalButton, iRow++, 2, 5, 1, 0);
        addComponent(optionButton, iRow++, 2, 5, 1, 0);
        addComponent(ok_panel, iRow, 0, 8, 1, 0);

        aFrame.setSize(1000, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.setFocusTraversalPolicy(new focusPolicy());

    } // end of method TexasDiamondTimingDialog

    void addComponent(Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;

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

    class SetAllActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (event.getSource() == setAllButton[lsiv_field]) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                        comboBox_timing[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                    }
                }
            }
        }
    } // end of SetAllActionListener

    class SetAllKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                            comboBox_timing[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                        }
                    }
                }
            }
        }
    } // end of SetAllKeyListener

    class IntervalActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (Integer.valueOf(comboBox_figure_number.getSelectedItem().toString())
                    .intValue() == gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
                new TexasDiamondSpecialIntervalDialog();
            }
            else {
                JOptionPane.showMessageDialog(null,
                        "The Figure Number has been modified.\"Please OK or Apply the Texas Diamond Signal Controller Timing Data before accessing the Texas Diamond Special Interval Data.",
                        "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    class IntervalKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (Integer.valueOf(comboBox_figure_number.getSelectedItem().toString())
                        .intValue() == gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
                    new TexasDiamondSpecialIntervalDialog();
                }
                else {
                    JOptionPane.showMessageDialog(null,
                            "The Figure Number has been modified.\"Please OK or Apply the Texas Diamond Signal Controller Timing Data before accessing the Texas Diamond Special Interval Data.",
                            "Warning Message", JOptionPane.WARNING_MESSAGE);
                }
            }
        }
    }

    class OptionActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (Integer.valueOf(comboBox_figure_number.getSelectedItem().toString())
                    .intValue() == gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
                new TexasDiamondSpecialOptionDialog();
            }
            else {
                JOptionPane.showMessageDialog(null,
                        "The Figure Number has been modified.\"Please OK or Apply the Texas Diamond Signal Controller Timing Data before accessing the Texas Diamond Special Option Data.",
                        "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    class OptionKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (Integer.valueOf(comboBox_figure_number.getSelectedItem().toString())
                        .intValue() == gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
                    new TexasDiamondSpecialOptionDialog();
                }
                else {
                    JOptionPane.showMessageDialog(null,
                            "The Figure Number has been modified.\"Please OK or Apply the Texas Diamond Signal Controller Timing Data before accessing the Texas Diamond Special Option Data.",
                            "Warning Message", JOptionPane.WARNING_MESSAGE);
                }
            }
        }
    }

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
                else if (event.getSource() == intervalButton) {
                    new HelpDialog(true, "Texas Diamond Special Intervals Button", "Texas Diamond Special Intervals Button opens Texas Diamond Controller Interval Dialog.", " ", " ", " ", " ", " ",
                            " ", " ");
                }
                else if (event.getSource() == optionButton) {
                    new HelpDialog(true, "Texas Diamond Special Options Button", "Texas Diamond Special Options Button opens Texas Diamond Controller Interval Dialog.", " ", " ", " ", " ", " ", " ",
                            " ");
                }
                else if (event.getSource() == comboBox_figure_number) {
                    new HelpDialog(
                            true,
                            "Texas Diamond Signal Controller Figure Number",
                            "Texas Diamond Signal Controller Figure Number",
                            "This item is the Texas Diamond Signal Controller Figure Number.  Figure 3 has left turns from the interior lanes lag the opposing arterial phase at both intersections.  Figure 4 has left turns from the interior lanes lead the opposing arterial phase at both intersections.  Figure 6 has left turns from the interior lanes lead the opposing arterial phase at the left intersection and lag the opposing arterial phase at the right intersection.  Figure 7 has left turns from the interior lanes lag the opposing arterial phase at the left intersection and lead the opposing arterial phase at the right intersection.",
                            comboBox_figure_number.getSelectedItem().toString(), "3", "3, 4, 6, or 7", " ", " ", " ");
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        new HelpDialog(true, "Set All Button", "The Set All button sets the value of " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]
                                + " for all phases to the value selected above.", " ", " ", " ", " ", " ", " ", " ");
                    }

                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for all phases",
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]), " ",
                                Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]));
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_timing[lsiv_phase][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field] + " for Phase " + lsiv_phase,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field], comboBox_timing[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                    Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]), " ",
                                    Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                    Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]),
                                    Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1 + lsiv_field]));
                        }
                    }
                }
            }
        }
    } // end of class HelpListener

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            for (int lsiv_phase = 1; lsiv_phase <= 3; lsiv_phase++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (aComponent.equals(comboBox_timing[lsiv_phase][lsiv_field])) {
                        if (lsiv_field == NUMOFFIELD) {
                            if (lsiv_phase == 3) {
                                return comboBox_timing[5][1];
                            }
                            else {
                                return comboBox_timing[lsiv_phase + 1][1];
                            }
                        }
                        else {
                            return comboBox_timing[lsiv_phase][lsiv_field + 1];
                        }
                    }
                }
            }

            for (int lsiv_phase = 5; lsiv_phase <= 7; lsiv_phase++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (aComponent.equals(comboBox_timing[lsiv_phase][lsiv_field])) {
                        if (lsiv_field == NUMOFFIELD) {
                            if (lsiv_phase == 7) {
                                return intervalButton;
                            }
                            else {
                                return comboBox_timing[lsiv_phase + 1][1];
                            }
                        }
                        else {
                            return comboBox_timing[lsiv_phase][lsiv_field + 1];
                        }
                    }
                }
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (aComponent.equals(setAllComboBox[lsiv_field])) {
                    return setAllButton[lsiv_field];
                }

                if (aComponent.equals(setAllButton[lsiv_field])) {
                    if (lsiv_field == NUMOFFIELD) {
                        return comboBox_timing[1][1];
                    }
                    else {
                        return setAllComboBox[lsiv_field + 1];
                    }
                }
            }

            if (aComponent.equals(comboBox_figure_number)) {
                return setAllComboBox[1];
            }
            else if (aComponent.equals(intervalButton)) {
                return optionButton;
            }
            else if (aComponent.equals(optionButton)) {
                return okButton;
            }
            else if (aComponent.equals(okButton)) {
                return applyButton;
            }
            else if (aComponent.equals(applyButton)) {
                return cancelButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return comboBox_figure_number;
            }

            return okButton;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            for (int lsiv_phase = 1; lsiv_phase <= 3; lsiv_phase++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (aComponent.equals(comboBox_timing[lsiv_phase][lsiv_field])) {
                        if (lsiv_field == 1) {
                            if (lsiv_phase == 1) {
                                return setAllButton[NUMOFFIELD];
                            }
                            else {
                                return comboBox_timing[lsiv_phase - 1][NUMOFFIELD];
                            }
                        }
                        else {
                            return comboBox_timing[lsiv_phase][lsiv_field - 1];
                        }
                    }
                }
            }

            for (int lsiv_phase = 5; lsiv_phase <= 7; lsiv_phase++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (aComponent.equals(comboBox_timing[lsiv_phase][lsiv_field])) {
                        if (lsiv_field == 1) {
                            if (lsiv_phase == 5) {
                                return comboBox_timing[3][NUMOFFIELD];
                            }
                            else {
                                return comboBox_timing[lsiv_phase - 1][NUMOFFIELD];
                            }
                        }
                        else {
                            return comboBox_timing[lsiv_phase][lsiv_field - 1];
                        }
                    }
                }
            }

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (aComponent.equals(setAllComboBox[lsiv_field])) {
                    if (lsiv_field == 1) {
                        return comboBox_figure_number;
                    }
                    else {
                        return setAllButton[lsiv_field - 1];
                    }
                }

                if (aComponent.equals(setAllButton[lsiv_field])) {
                    return setAllComboBox[lsiv_field];
                }
            }

            if (aComponent.equals(comboBox_figure_number)) {
                return cancelButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return applyButton;
            }
            else if (aComponent.equals(applyButton)) {
                return okButton;
            }
            else if (aComponent.equals(okButton)) {
                return optionButton;
            }
            else if (aComponent.equals(optionButton)) {
                return intervalButton;
            }
            else if (aComponent.equals(intervalButton)) {
                return comboBox_timing[PARAMS.TEXAS_MODEL_NPH][NUMOFFIELD];
            }

            return okButton;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            return comboBox_figure_number;
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return cancelButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            return comboBox_figure_number;
        }
    } // end of class focusPolicy

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = PARAMS.TEXAS_MODEL_NPH;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_FROM_USER;

        if (Integer.valueOf(comboBox_figure_number.getSelectedItem().toString()).intValue() != gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
            gdvsim.flag_diamondOption_ok = false;
            gdvsim.flag_diamondInterval_ok = false;

            for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
                for (int lsiv_field = 1; lsiv_field <= 12; lsiv_field++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_INT_P35CG_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = Integer.valueOf(comboBox_figure_number.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if ((lsiv_phase == 4) || (lsiv_phase == 8))
                continue;

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_ini_int = Double.valueOf(comboBox_timing[lsiv_phase][1].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_veh_int = Double.valueOf(comboBox_timing[lsiv_phase][2].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_yel_chg = Double.valueOf(comboBox_timing[lsiv_phase][3].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_red_clr = Double.valueOf(comboBox_timing[lsiv_phase][4].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_act_max_ext = Double.valueOf(comboBox_timing[lsiv_phase][5].getSelectedItem().toString())
                    .doubleValue();

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEXD_SIGNAL_ACT_INI_INT - 1
                    + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

    } // end of saveData()

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_diamondTiming_ok = true;

                saveData();

                if (event.getSource() == okButton) {
                    aFrame.dispose();
                }
            }

            if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
        }
    } // end of OkApplyActionListener

    class OkApplyKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    gdvsim.flag_diamondTiming_ok = true;

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

} // end of class TexasDiamondTimingDialog
