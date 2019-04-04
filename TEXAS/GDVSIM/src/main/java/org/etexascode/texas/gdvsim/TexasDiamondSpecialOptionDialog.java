package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                    TexasDiamondSpecialOptionDialog.java                    */
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

class TexasDiamondSpecialOptionDialog extends JDialog {

    int NUMOFFIELD = 12;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel[] label_figure = new JLabel[PARAMS.TEXAS_MODEL_DIA + 1];

    JLabel label_title;

    JComboBox[][] comboBox_figure = new JComboBox[PARAMS.TEXAS_MODEL_DIA + 1][NUMOFFIELD + 1];

    JButton okButton, applyButton, cancelButton;

    JTextArea[] setAllText = new JTextArea[NUMOFFIELD + 1];

    JButton[] setAllButton = new JButton[NUMOFFIELD + 1];

    JComboBox[] setAllComboBox = new JComboBox[NUMOFFIELD + 1];

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    SetAllActionListener setAllActionListener;

    SetAllKeyListener setAllKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    int figure_number;

    public TexasDiamondSpecialOptionDialog() {
        figure_number = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT];

        titleString = lclv_tx_fmt.mstv_name.substring(9);

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

        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
            label_figure[lsiv_figure] = new JLabel("Figure " + lsiv_figure);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String[] array_figure = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467].substring(1).split("\\|");

            setAllComboBox[lsiv_field] = new JComboBox(array_figure);
            setAllComboBox[lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);

            for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
                comboBox_figure[lsiv_figure][lsiv_field] = new JComboBox(array_figure);
                comboBox_figure[lsiv_figure][lsiv_field].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
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

            setAllText[lsiv_field].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
        }

        if (gdvsim.flag_diamondOption_ok) {
            for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if ((figure_number == 3) && (lsiv_figure == 3) && (lsiv_field == 5)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_term_logic_ph_2_7_f3h);
                    }
                    else if ((figure_number == 3) && (lsiv_figure == 3) && (lsiv_field == 6)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_term_logic_ph_2_7_f3o);
                    }
                    else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 1)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467);
                    }
                    else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 2)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467);
                    }
                    else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 3)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467);
                    }
                    else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 4)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 1)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 2)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 3)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 4)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 7)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_a_ph_1_6_tim_f7);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 8)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_b_ph_2_7_tim_f6);
                    }
                    else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 9)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_c_ph_6_skip_f6);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 1)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 2)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 3)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 4)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 10)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_a_ph_1_6_tim_f6);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 11)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_b_ph_2_7_tim_f7);
                    }
                    else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 12)) {
                        comboBox_figure[lsiv_figure][lsiv_field]
                                .setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_c_ph_1_skip_f7);
                    }
                }
            }
        }

        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (((figure_number == 3) && (lsiv_figure == 3) && (lsiv_field == 5)) || ((figure_number == 3) && (lsiv_figure == 3) && (lsiv_field == 6))
                        || ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 1)) || ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 2))
                        || ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 3)) || ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 4))
                        || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 1)) || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 2))
                        || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 3)) || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 4))
                        || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 7)) || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 8))
                        || ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 9)) || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 1))
                        || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 2)) || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 3))
                        || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 4)) || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 10))
                        || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 11)) || ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 12))) {
                    comboBox_figure[lsiv_figure][lsiv_field].setEnabled(true);
                }
                else {
                    comboBox_figure[lsiv_figure][lsiv_field].setEnabled(false);
                }
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (((figure_number == 3) && (lsiv_field == 5)) || ((figure_number == 3) && (lsiv_field == 6))) {
                setAllButton[lsiv_field].setEnabled(true);
                setAllComboBox[lsiv_field].setEnabled(true);
            }
            else if (((figure_number == 4) && (lsiv_field == 1)) || ((figure_number == 4) && (lsiv_field == 2)) || ((figure_number == 4) && (lsiv_field == 3))
                    || ((figure_number == 4) && (lsiv_field == 4))) {
                setAllButton[lsiv_field].setEnabled(true);
                setAllComboBox[lsiv_field].setEnabled(true);
            }
            else if (((figure_number == 6) && (lsiv_field == 1)) || ((figure_number == 6) && (lsiv_field == 2)) || ((figure_number == 6) && (lsiv_field == 3))
                    || ((figure_number == 6) && (lsiv_field == 4)) || ((figure_number == 6) && (lsiv_field == 7)) || ((figure_number == 6) && (lsiv_field == 8))
                    || ((figure_number == 6) && (lsiv_field == 9))) {
                setAllButton[lsiv_field].setEnabled(true);
                setAllComboBox[lsiv_field].setEnabled(true);
            }
            else if (((figure_number == 7) && (lsiv_field == 1)) || ((figure_number == 7) && (lsiv_field == 2)) || ((figure_number == 7) && (lsiv_field == 3))
                    || ((figure_number == 7) && (lsiv_field == 4)) || ((figure_number == 7) && (lsiv_field == 10)) || ((figure_number == 7) && (lsiv_field == 11))
                    || ((figure_number == 7) && (lsiv_field == 12))) {
                setAllButton[lsiv_field].setEnabled(true);
                setAllComboBox[lsiv_field].setEnabled(true);
            }
            else {
                setAllButton[lsiv_field].setEnabled(false);
                setAllComboBox[lsiv_field].setEnabled(false);
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleName("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleDescription("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);

            setAllButton[lsiv_field].getAccessibleContext().setAccessibleName("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleDescription("set all for " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
        }

        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_figure[lsiv_figure][lsiv_field].getAccessibleContext().setAccessibleName(
                        "for figure " + lsiv_figure + " field " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
                comboBox_figure[lsiv_figure][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        "for figure " + lsiv_figure + " field " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]);
            }
        }

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

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field].addActionListener(setAllActionListener);
            setAllButton[lsiv_field].addKeyListener(setAllKeyListener);
            setAllButton[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].addKeyListener(openComboMenuListener);
            setAllComboBox[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_figure[lsiv_figure][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_figure[lsiv_figure][lsiv_field].addKeyListener(helpListener);
            }
        }

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
        addComponent(panel_title, iRow++, 0, 13, 1, 0);

        gbConstraints.insets = new Insets(2, 3, 2, 3);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllText[lsiv_field], iRow, lsiv_field, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllComboBox[lsiv_field], iRow, lsiv_field, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(setAllButton[lsiv_field], iRow, lsiv_field, 1, 1, 0);
        }

        iRow++;

        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
            if ((lsiv_figure == 3) || (lsiv_figure == 4) || (lsiv_figure == 6) || (lsiv_figure == 7)) {
                addComponent(label_figure[lsiv_figure], iRow, 0, 1, 1, 0);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    addComponent(comboBox_figure[lsiv_figure][lsiv_field], iRow, lsiv_field, 1, 1, 0);
                }

                iRow++;
            }
        }

        addComponent(ok_panel, iRow, 0, 13, 1, 0);

        aFrame.setSize(1000, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.setFocusTraversalPolicy(new focusPolicy());

    } // end of method TexasDiamondSpecialOptionDialog

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
                    for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
                        comboBox_figure[lsiv_figure][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
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
                        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
                            comboBox_figure[lsiv_figure][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                        }
                    }
                }
            }
        }
    } // end of SetAllKeyListener

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

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        new HelpDialog(true, "Set All Button", "The Set All button sets the value of " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field]
                                + " for all phases to the value selected above.", " ", " ", " ", " ", " ", " ", " ");
                    }

                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field] + " for all phases",
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field],
                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field], " ", " ", " ");
                    }
                }

                for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_figure[lsiv_figure][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field] + " for Figure " + lsiv_figure,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field], comboBox_figure[lsiv_figure][lsiv_field].getSelectedItem()
                                            .toString(),
                                    lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field],
                                    lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1 + lsiv_field], " ", " ", " ");
                        }
                    }
                }
            }
        }
    } // end of class HelpListener

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            if (figure_number == 3) {
                if (aComponent.equals(setAllComboBox[5])) {
                    return setAllButton[5];
                }
                else if (aComponent.equals(setAllButton[5])) {
                    return setAllComboBox[6];
                }
                else if (aComponent.equals(setAllComboBox[6])) {
                    return setAllButton[6];
                }
                else if (aComponent.equals(setAllButton[6])) {
                    return comboBox_figure[3][5];
                }
                else if (aComponent.equals(comboBox_figure[3][5])) {
                    return comboBox_figure[3][6];
                }
                else if (aComponent.equals(comboBox_figure[3][6])) {
                    return okButton;
                }
                else if (aComponent.equals(okButton)) {
                    return applyButton;
                }
                else if (aComponent.equals(applyButton)) {
                    return cancelButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return setAllComboBox[5];
                }
            }
            else if (figure_number == 4) {
                if (aComponent.equals(setAllComboBox[1])) {
                    return setAllButton[1];
                }
                else if (aComponent.equals(setAllButton[1])) {
                    return setAllComboBox[2];
                }
                else if (aComponent.equals(setAllComboBox[2])) {
                    return setAllButton[2];
                }
                else if (aComponent.equals(setAllButton[2])) {
                    return setAllComboBox[3];
                }
                else if (aComponent.equals(setAllComboBox[3])) {
                    return setAllButton[3];
                }
                else if (aComponent.equals(setAllButton[3])) {
                    return setAllComboBox[4];
                }
                else if (aComponent.equals(setAllComboBox[4])) {
                    return setAllButton[4];
                }
                else if (aComponent.equals(setAllButton[4])) {
                    return comboBox_figure[4][1];
                }
                else if (aComponent.equals(comboBox_figure[4][1])) {
                    return comboBox_figure[4][2];
                }
                else if (aComponent.equals(comboBox_figure[4][2])) {
                    return comboBox_figure[4][3];
                }
                else if (aComponent.equals(comboBox_figure[4][3])) {
                    return comboBox_figure[4][4];
                }
                else if (aComponent.equals(comboBox_figure[4][4])) {
                    return okButton;
                }
                else if (aComponent.equals(okButton)) {
                    return applyButton;
                }
                else if (aComponent.equals(applyButton)) {
                    return cancelButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return setAllComboBox[1];
                }
            }
            else if (figure_number == 6) {
                if (aComponent.equals(setAllComboBox[1])) {
                    return setAllButton[1];
                }
                else if (aComponent.equals(setAllButton[1])) {
                    return setAllComboBox[2];
                }
                else if (aComponent.equals(setAllComboBox[2])) {
                    return setAllButton[2];
                }
                else if (aComponent.equals(setAllButton[2])) {
                    return setAllComboBox[3];
                }
                else if (aComponent.equals(setAllComboBox[3])) {
                    return setAllButton[3];
                }
                else if (aComponent.equals(setAllButton[3])) {
                    return setAllComboBox[4];
                }
                else if (aComponent.equals(setAllComboBox[4])) {
                    return setAllButton[4];
                }
                else if (aComponent.equals(setAllButton[4])) {
                    return setAllComboBox[7];
                }
                else if (aComponent.equals(setAllComboBox[7])) {
                    return setAllButton[7];
                }
                else if (aComponent.equals(setAllButton[7])) {
                    return setAllComboBox[8];
                }
                else if (aComponent.equals(setAllComboBox[8])) {
                    return setAllButton[8];
                }
                else if (aComponent.equals(setAllButton[8])) {
                    return setAllComboBox[9];
                }
                else if (aComponent.equals(setAllComboBox[9])) {
                    return setAllButton[9];
                }
                else if (aComponent.equals(setAllButton[9])) {
                    return comboBox_figure[6][1];
                }
                else if (aComponent.equals(comboBox_figure[6][1])) {
                    return comboBox_figure[6][2];
                }
                else if (aComponent.equals(comboBox_figure[6][2])) {
                    return comboBox_figure[6][3];
                }
                else if (aComponent.equals(comboBox_figure[6][3])) {
                    return comboBox_figure[6][4];
                }
                else if (aComponent.equals(comboBox_figure[6][4])) {
                    return comboBox_figure[6][7];
                }
                else if (aComponent.equals(comboBox_figure[6][7])) {
                    return comboBox_figure[6][8];
                }
                else if (aComponent.equals(comboBox_figure[6][8])) {
                    return comboBox_figure[6][9];
                }
                else if (aComponent.equals(comboBox_figure[6][9])) {
                    return okButton;
                }
                else if (aComponent.equals(okButton)) {
                    return applyButton;
                }
                else if (aComponent.equals(applyButton)) {
                    return cancelButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return setAllComboBox[1];
                }
            }
            else if (figure_number == 7) {
                if (aComponent.equals(setAllComboBox[1])) {
                    return setAllButton[1];
                }
                else if (aComponent.equals(setAllButton[1])) {
                    return setAllComboBox[2];
                }
                else if (aComponent.equals(setAllComboBox[2])) {
                    return setAllButton[2];
                }
                else if (aComponent.equals(setAllButton[2])) {
                    return setAllComboBox[3];
                }
                else if (aComponent.equals(setAllComboBox[3])) {
                    return setAllButton[3];
                }
                else if (aComponent.equals(setAllButton[3])) {
                    return setAllComboBox[4];
                }
                else if (aComponent.equals(setAllComboBox[4])) {
                    return setAllButton[4];
                }
                else if (aComponent.equals(setAllButton[4])) {
                    return setAllComboBox[10];
                }
                else if (aComponent.equals(setAllComboBox[10])) {
                    return setAllButton[10];
                }
                else if (aComponent.equals(setAllButton[10])) {
                    return setAllComboBox[11];
                }
                else if (aComponent.equals(setAllComboBox[11])) {
                    return setAllButton[11];
                }
                else if (aComponent.equals(setAllButton[11])) {
                    return setAllComboBox[12];
                }
                else if (aComponent.equals(setAllComboBox[12])) {
                    return setAllButton[12];
                }
                else if (aComponent.equals(setAllButton[12])) {
                    return comboBox_figure[7][1];
                }
                else if (aComponent.equals(comboBox_figure[7][1])) {
                    return comboBox_figure[7][2];
                }
                else if (aComponent.equals(comboBox_figure[7][2])) {
                    return comboBox_figure[7][3];
                }
                else if (aComponent.equals(comboBox_figure[7][3])) {
                    return comboBox_figure[7][4];
                }
                else if (aComponent.equals(comboBox_figure[7][4])) {
                    return comboBox_figure[7][10];
                }
                else if (aComponent.equals(comboBox_figure[7][10])) {
                    return comboBox_figure[7][11];
                }
                else if (aComponent.equals(comboBox_figure[7][11])) {
                    return comboBox_figure[7][12];
                }
                else if (aComponent.equals(comboBox_figure[7][12])) {
                    return okButton;
                }
                else if (aComponent.equals(okButton)) {
                    return applyButton;
                }
                else if (aComponent.equals(applyButton)) {
                    return cancelButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return setAllComboBox[1];
                }
            }

            return okButton;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            if (figure_number == 3) {
                if (aComponent.equals(setAllComboBox[5])) {
                    return cancelButton;
                }
                else if (aComponent.equals(setAllButton[5])) {
                    return setAllComboBox[5];
                }
                else if (aComponent.equals(setAllComboBox[6])) {
                    return setAllButton[5];
                }
                else if (aComponent.equals(setAllButton[6])) {
                    return setAllComboBox[6];
                }
                else if (aComponent.equals(comboBox_figure[3][5])) {
                    return setAllButton[6];
                }
                else if (aComponent.equals(comboBox_figure[3][6])) {
                    return comboBox_figure[3][5];
                }
                else if (aComponent.equals(okButton)) {
                    return comboBox_figure[3][6];
                }
                else if (aComponent.equals(applyButton)) {
                    return okButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return applyButton;
                }
            }
            else if (figure_number == 4) {
                if (aComponent.equals(setAllComboBox[1])) {
                    return cancelButton;
                }
                else if (aComponent.equals(setAllButton[1])) {
                    return setAllComboBox[1];
                }
                else if (aComponent.equals(setAllComboBox[2])) {
                    return setAllButton[1];
                }
                else if (aComponent.equals(setAllButton[2])) {
                    return setAllComboBox[2];
                }
                else if (aComponent.equals(setAllComboBox[3])) {
                    return setAllButton[2];
                }
                else if (aComponent.equals(setAllButton[3])) {
                    return setAllComboBox[3];
                }
                else if (aComponent.equals(setAllComboBox[4])) {
                    return setAllButton[3];
                }
                else if (aComponent.equals(setAllButton[4])) {
                    return setAllComboBox[4];
                }
                else if (aComponent.equals(comboBox_figure[4][1])) {
                    return setAllButton[4];
                }
                else if (aComponent.equals(comboBox_figure[4][2])) {
                    return comboBox_figure[4][1];
                }
                else if (aComponent.equals(comboBox_figure[4][3])) {
                    return comboBox_figure[4][2];
                }
                else if (aComponent.equals(comboBox_figure[4][4])) {
                    return comboBox_figure[4][3];
                }
                else if (aComponent.equals(okButton)) {
                    return comboBox_figure[4][4];
                }
                else if (aComponent.equals(applyButton)) {
                    return okButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return applyButton;
                }
            }
            else if (figure_number == 6) {
                if (aComponent.equals(setAllComboBox[1])) {
                    return cancelButton;
                }
                else if (aComponent.equals(setAllButton[1])) {
                    return setAllComboBox[1];
                }
                else if (aComponent.equals(setAllComboBox[2])) {
                    return setAllButton[1];
                }
                else if (aComponent.equals(setAllButton[2])) {
                    return setAllComboBox[2];
                }
                else if (aComponent.equals(setAllComboBox[3])) {
                    return setAllButton[2];
                }
                else if (aComponent.equals(setAllButton[3])) {
                    return setAllComboBox[3];
                }
                else if (aComponent.equals(setAllComboBox[4])) {
                    return setAllButton[3];
                }
                else if (aComponent.equals(setAllButton[4])) {
                    return setAllComboBox[4];
                }
                else if (aComponent.equals(setAllComboBox[7])) {
                    return setAllButton[4];
                }
                else if (aComponent.equals(setAllButton[7])) {
                    return setAllComboBox[7];
                }
                else if (aComponent.equals(setAllComboBox[8])) {
                    return setAllButton[7];
                }
                else if (aComponent.equals(setAllButton[8])) {
                    return setAllComboBox[8];
                }
                else if (aComponent.equals(setAllComboBox[9])) {
                    return setAllButton[8];
                }
                else if (aComponent.equals(setAllButton[9])) {
                    return setAllComboBox[9];
                }
                else if (aComponent.equals(comboBox_figure[6][1])) {
                    return setAllButton[9];
                }
                else if (aComponent.equals(comboBox_figure[6][2])) {
                    return comboBox_figure[6][1];
                }
                else if (aComponent.equals(comboBox_figure[6][3])) {
                    return comboBox_figure[6][2];
                }
                else if (aComponent.equals(comboBox_figure[6][4])) {
                    return comboBox_figure[6][3];
                }
                else if (aComponent.equals(comboBox_figure[6][7])) {
                    return comboBox_figure[6][4];
                }
                else if (aComponent.equals(comboBox_figure[6][8])) {
                    return comboBox_figure[6][7];
                }
                else if (aComponent.equals(comboBox_figure[6][9])) {
                    return comboBox_figure[6][8];
                }
                else if (aComponent.equals(okButton)) {
                    return comboBox_figure[6][9];
                }
                else if (aComponent.equals(applyButton)) {
                    return okButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return applyButton;
                }
            }
            else if (figure_number == 7) {
                if (aComponent.equals(setAllComboBox[1])) {
                    return cancelButton;
                }
                else if (aComponent.equals(setAllButton[1])) {
                    return setAllComboBox[1];
                }
                else if (aComponent.equals(setAllComboBox[2])) {
                    return setAllButton[1];
                }
                else if (aComponent.equals(setAllButton[2])) {
                    return setAllComboBox[2];
                }
                else if (aComponent.equals(setAllComboBox[3])) {
                    return setAllButton[2];
                }
                else if (aComponent.equals(setAllButton[3])) {
                    return setAllComboBox[3];
                }
                else if (aComponent.equals(setAllComboBox[4])) {
                    return setAllButton[3];
                }
                else if (aComponent.equals(setAllButton[4])) {
                    return setAllComboBox[4];
                }
                else if (aComponent.equals(setAllComboBox[10])) {
                    return setAllButton[4];
                }
                else if (aComponent.equals(setAllButton[10])) {
                    return setAllComboBox[10];
                }
                else if (aComponent.equals(setAllComboBox[11])) {
                    return setAllButton[10];
                }
                else if (aComponent.equals(setAllButton[11])) {
                    return setAllComboBox[11];
                }
                else if (aComponent.equals(setAllComboBox[12])) {
                    return setAllButton[11];
                }
                else if (aComponent.equals(setAllButton[12])) {
                    return setAllComboBox[12];
                }
                else if (aComponent.equals(comboBox_figure[7][1])) {
                    return setAllButton[12];
                }
                else if (aComponent.equals(comboBox_figure[7][2])) {
                    return comboBox_figure[7][1];
                }
                else if (aComponent.equals(comboBox_figure[7][3])) {
                    return comboBox_figure[7][2];
                }
                else if (aComponent.equals(comboBox_figure[7][4])) {
                    return comboBox_figure[7][3];
                }
                else if (aComponent.equals(comboBox_figure[7][10])) {
                    return comboBox_figure[7][4];
                }
                else if (aComponent.equals(comboBox_figure[7][11])) {
                    return comboBox_figure[7][10];
                }
                else if (aComponent.equals(comboBox_figure[7][12])) {
                    return comboBox_figure[7][11];
                }
                else if (aComponent.equals(okButton)) {
                    return comboBox_figure[7][12];
                }
                else if (aComponent.equals(applyButton)) {
                    return okButton;
                }
                else if (aComponent.equals(cancelButton)) {
                    return applyButton;
                }
            }

            return okButton;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            if (figure_number == 3) {
                return setAllComboBox[5];
            }
            else if (figure_number == 4) {
                return setAllComboBox[1];
            }
            else if (figure_number == 6) {
                return setAllComboBox[1];
            }
            else if (figure_number == 7) {
                return setAllComboBox[1];
            }
            return okButton;
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return cancelButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            if (figure_number == 3) {
                return setAllComboBox[5];
            }
            else if (figure_number == 4) {
                return setAllComboBox[1];
            }
            else if (figure_number == 6) {
                return setAllComboBox[1];
            }
            else if (figure_number == 7) {
                return setAllComboBox[1];
            }
            return okButton;
        }
    } // end of class focusPolicy

    void saveData() {
        for (int lsiv_figure = 1; lsiv_figure <= PARAMS.TEXAS_MODEL_DIA; lsiv_figure++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if ((figure_number == 3) && (lsiv_figure == 3) && (lsiv_field == 5)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_term_logic_ph_2_7_f3h = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 3) && (lsiv_figure == 3) && (lsiv_field == 6)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_term_logic_ph_2_7_f3o = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 1)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 2)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 3)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 4) && (lsiv_figure == 4) && (lsiv_field == 4)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 1)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 2)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 3)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 4)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 7)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_a_ph_1_6_tim_f6 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 8)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_b_ph_2_7_tim_f6 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 6) && (lsiv_figure == 6) && (lsiv_field == 9)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_c_ph_6_skip_f6 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 1)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 2)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 3)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 4)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 10)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_a_ph_1_6_tim_f7 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 11)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_b_ph_2_7_tim_f7 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else if ((figure_number == 7) && (lsiv_figure == 7) && (lsiv_field == 11)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_c_ph_1_skip_f7 = comboBox_figure[lsiv_figure][lsiv_field]
                            .getSelectedItem().toString();
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 - 1
                            + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    if (figure_number == lsiv_figure) {
                        if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_3_ph_3_7_f467 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED13P37_F467) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_3_7_f467 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED13P37_F467];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED05P25_F467) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d_5_ph_2_5_f467 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED05P25_F467];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED13P25_F467) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_ena_d13_ph_2_5_f467 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED13P25_F467];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3H) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_term_logic_ph_2_7_f3h = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3H];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3O) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_term_logic_ph_2_7_f3o = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3O];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F6OPTAP16T_F6) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_a_ph_1_6_tim_f6 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F6OPTAP16T_F6];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F6OPTBP27T_F6) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_b_ph_2_7_tim_f6 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F6OPTBP27T_F6];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F6OPTCP6SK_F6) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f6_opt_c_ph_6_skip_f6 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F6OPTCP6SK_F6];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F7OPTAP16T_F7) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_a_ph_1_6_tim_f7 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F7OPTAP16T_F7];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F7OPTBP27T_F7) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_b_ph_2_7_tim_f7 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F7OPTBP27T_F7];
                        }
                        else if (lsiv_field == gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F7OPTCP1SK_F7) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mstv_f7_opt_c_ph_1_skip_f7 = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_F7OPTCP1SK_F7];
                        }
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467
                                - 1 + lsiv_field] = gdvsim.gclv_inter.TX_DEFAULT;
                    }
                    else {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_figure].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467
                                - 1 + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }
    } // end of saveData()

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                gdvsim.flag_diamondOption_ok = true;

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
                    gdvsim.flag_diamondOption_ok = true;

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

} // end of class TexasDiamondSpecialOptionDialog
