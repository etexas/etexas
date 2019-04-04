package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             LineDialog.java                                */
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

class LineDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 4;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    TextFieldFocusListener textFieldFocusListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    JTextField[][] text_line = new JTextField[PARAMS.TEXAS_MODEL_NLI + 1][NUMOFFIELD + 1];

    JLabel[] label_line = new JLabel[PARAMS.TEXAS_MODEL_NLI + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NLI + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NLI + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NLI + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NLI + 1];

    JComboBox cbo_total;

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_note, label_total;

    Font font, font1, font2;

    TX_Fmt lclv_tx_fmt;

    int numOfLine;

    String titleString;

    public LineDialog() {
        if (gdvsim.flag_line_ok) {
            numOfLine = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines;
        }
        else {
            numOfLine = 0;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA];

        titleString = "Line Data (For Plotting Purposes Only)";
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
        font2 = new Font("TimesRoman", Font.BOLD, 12);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            label_line[lsiv_line] = new JLabel("Line " + lsiv_line);
            label_line[lsiv_line].setFont(font1);
        }

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            add[lsiv_line] = new JButton("Add");
            del[lsiv_line] = new JButton("Delete");
            up[lsiv_line] = new JButton("Up");
            down[lsiv_line] = new JButton("Down");

            add[lsiv_line].setMinimumSize(new Dimension(5, 10));
            del[lsiv_line].setMinimumSize(new Dimension(5, 10));
            up[lsiv_line].setMinimumSize(new Dimension(5, 10));
            down[lsiv_line].setMinimumSize(new Dimension(5, 10));

            add[lsiv_line].setMaximumSize(new Dimension(5, 10));
            del[lsiv_line].setMaximumSize(new Dimension(5, 10));
            up[lsiv_line].setMaximumSize(new Dimension(5, 10));
            down[lsiv_line].setMaximumSize(new Dimension(5, 10));
        }

        label_total = new JLabel("Total Lines");
        label_total.setFont(font1);

        label_note = new JLabel("NOTE:   The center of the intersection is located at coordinate X = " + PARAMS.TEXAS_MODEL_XYCNTR + " and Y = " + PARAMS.TEXAS_MODEL_XYCNTR + " feet.  END NOTE");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setFont(font1);
            label_field[lsiv_field].setSize(new Dimension(90, 10));
            label_field[lsiv_field].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]);
        }

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                text_line[lsiv_line][lsiv_field] = new JTextField();
                text_line[lsiv_line][lsiv_field].setBackground(aFrame.getBackground());
                text_line[lsiv_line][lsiv_field].setFont(font1);
                text_line[lsiv_line][lsiv_field].setSize(new Dimension(90, 10));
            }
        }

        if (gdvsim.flag_line_ok) {
            for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_line[lsiv_line][1].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X]));
                }
                else {
                    text_line[lsiv_line][1].setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_beg_x));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_Y] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_line[lsiv_line][2].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_Y]));
                }
                else {
                    text_line[lsiv_line][2].setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_beg_y));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_END_X] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_line[lsiv_line][3].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_END_X]));
                }
                else {
                    text_line[lsiv_line][3].setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_end_x));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_END_Y] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    text_line[lsiv_line][4].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_END_Y]));
                }
                else {
                    text_line[lsiv_line][4].setText(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_end_y));
                }
            }
        }
        else {
            for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    text_line[lsiv_line][lsiv_field].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]));
                }
            }
        }

        setStatus(numOfLine);

        cbo_total = new JComboBox();
        cbo_total.setMinimumSize(new Dimension(5, 10));
        cbo_total.setMaximumSize(new Dimension(5, 10));
        cbo_total.addItem(Integer.toString(getNumberOfLine()));

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
        textFieldFocusListener = new TextFieldFocusListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            add[lsiv_line].addActionListener(componentActionListener);
            del[lsiv_line].addActionListener(componentActionListener);
            up[lsiv_line].addActionListener(componentActionListener);
            down[lsiv_line].addActionListener(componentActionListener);

            add[lsiv_line].addKeyListener(componentKeyListener);
            del[lsiv_line].addKeyListener(componentKeyListener);
            up[lsiv_line].addKeyListener(componentKeyListener);
            down[lsiv_line].addKeyListener(componentKeyListener);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                text_line[lsiv_line][lsiv_field].addKeyListener(helpListener);
                text_line[lsiv_line][lsiv_field].addFocusListener(textFieldFocusListener);
            }

            add[lsiv_line].addKeyListener(helpListener);
            del[lsiv_line].addKeyListener(helpListener);
            up[lsiv_line].addKeyListener(helpListener);
            down[lsiv_line].addKeyListener(helpListener);
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

        gbConstraints.insets = new Insets(1, 20, 5, 0);
        addComponent(label_note, iRow++, 0, iCol, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == NUMOFFIELD) {
                gbConstraints.insets = new Insets(1, 1, 0, 10);
            }
            else {
                gbConstraints.insets = new Insets(1, 1, 0, 1);
            }

            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1);
        }

        iRow++;

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            gbConstraints.insets = new Insets(1, 10, 0, 1);
            addComponent(label_line[lsiv_line], iRow, 0, 1, 1);

            gbConstraints.insets = new Insets(1, 1, 0, 1);
            addComponent(add[lsiv_line], iRow, 1, 1, 1);
            addComponent(del[lsiv_line], iRow, 2, 1, 1);
            addComponent(up[lsiv_line], iRow, 3, 1, 1);
            addComponent(down[lsiv_line], iRow, 4, 1, 1);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == NUMOFFIELD) {
                    gbConstraints.insets = new Insets(1, 1, 0, 10);
                }
                else {
                    gbConstraints.insets = new Insets(1, 1, 0, 1);
                }

                addComponent(text_line[lsiv_line][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
            }

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 0, 1);
        addComponent(label_total, iRow, 0, 1, 1);
        addComponent(cbo_total, iRow++, 1, 1, 1);
        addComponent(ok_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(900, 700);
        aFrame.setVisible(true);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method LineDialog()

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getNumberOfLine() {
        int sum = 0;

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            if (text_line[lsiv_line][1].isEditable())
                sum++;
        }

        return sum;
    } // end of getNumberOfLine()

    void setAccessiblility() {
        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                text_line[lsiv_line][lsiv_field].getAccessibleContext().setAccessibleName(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field] + " for Line " + lsiv_line);
                text_line[lsiv_line][lsiv_field].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field] + " for Line " + lsiv_line);
            }

            add[lsiv_line].getAccessibleContext().setAccessibleName("Add    for Line " + lsiv_line);
            add[lsiv_line].getAccessibleContext().setAccessibleDescription("Add    for Line " + lsiv_line);
            del[lsiv_line].getAccessibleContext().setAccessibleName("Delete for Line " + lsiv_line);
            del[lsiv_line].getAccessibleContext().setAccessibleDescription("Delete for Line " + lsiv_line);
            up[lsiv_line].getAccessibleContext().setAccessibleName("Up     for Line " + lsiv_line);
            up[lsiv_line].getAccessibleContext().setAccessibleDescription("Up     for Line " + lsiv_line);
            down[lsiv_line].getAccessibleContext().setAccessibleName("Down   for Line " + lsiv_line);
            down[lsiv_line].getAccessibleContext().setAccessibleDescription("Down   for Line " + lsiv_line);
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
    } // end of OpenComboMenuListener()

    void setStatus(int sumOfLines) {
        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            if (lsiv_line <= sumOfLines) {
                label_line[lsiv_line].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    text_line[lsiv_line][lsiv_field].setEditable(true);
                }
            }
            else {
                label_line[lsiv_line].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    text_line[lsiv_line][lsiv_field].setEditable(false);
                }
            }
        }

        if (sumOfLines == PARAMS.TEXAS_MODEL_NLI) {
            for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                add[lsiv_line].setEnabled(false);
            }
        }
        else {
            for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                if (lsiv_line <= (sumOfLines + 1)) {
                    add[lsiv_line].setEnabled(true);
                }
                else {
                    add[lsiv_line].setEnabled(false);
                }
            }
        }

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            if (lsiv_line <= sumOfLines) {
                del[lsiv_line].setEnabled(true);
            }
            else {
                del[lsiv_line].setEnabled(false);
            }
        }

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            if (lsiv_line <= sumOfLines) {
                up[lsiv_line].setEnabled(true);
            }
            else {
                up[lsiv_line].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            if (lsiv_line < sumOfLines) {
                down[lsiv_line].setEnabled(true);
            }
            else {
                down[lsiv_line].setEnabled(false);
            }
        }

        setTextFieldColor();

    } // end of method setStatus()

    void setTextFieldColor() {
        for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (text_line[lsiv_line][lsiv_field].isEditable()) {
                    text_line[lsiv_line][lsiv_field].setBackground(aFrame.getBackground());
                    text_line[lsiv_line][lsiv_field].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
                    text_line[lsiv_line][lsiv_field].setForeground(new Color(0, 0, 0));
                }
                else {
                    text_line[lsiv_line][lsiv_field].setBackground(aFrame.getBackground());
                    text_line[lsiv_line][lsiv_field].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
                    text_line[lsiv_line][lsiv_field].setForeground(new Color(176, 197, 218));
                }
            }
        }
    } // end of method setTextFieldColor

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_line = sum; lsiv_line >= index; lsiv_line--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                text_line[lsiv_line + 1][lsiv_field].setText(text_line[lsiv_line][lsiv_field].getText());
            }
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_line = index; lsiv_line < sum; lsiv_line++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                text_line[lsiv_line][lsiv_field].setText(text_line[lsiv_line + 1][lsiv_field].getText());
            }
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            text_line[sum][lsiv_field].setText(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]));
        }
    } // end of method setValueAfterDel()

    class TextFieldFocusListener implements FocusListener {

        public void focusLost(FocusEvent event) {
            int sumOfLines = getNumberOfLine();

            for (int lsiv_line = 1; lsiv_line <= sumOfLines; lsiv_line++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == text_line[lsiv_line][lsiv_field]) {
                        text_line[lsiv_line][lsiv_field].setBackground(aFrame.getBackground());
                    }
                }
            }
        }

        public void focusGained(FocusEvent event) {
            int sumOfLines = getNumberOfLine();

            for (int lsiv_line = 1; lsiv_line <= sumOfLines; lsiv_line++) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == text_line[lsiv_line][lsiv_field]) {
                        text_line[lsiv_line][lsiv_field].setBackground(new Color(176, 197, 218));
                    }
                }
            }
        }
    } // end of TextFieldFocusListener()

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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NLI), "1");
                }

                for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == text_line[lsiv_line][lsiv_field]) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_line)),
                                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field] + " for Line " + lsiv_line,
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field], text_line[lsiv_line][lsiv_field].getText(),
                                    Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]), " ",
                                    Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]),
                                    Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]),
                                    Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field]));
                        }
                    }

                    if (event.getSource() == up[lsiv_line]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous line and the current line.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_line]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next line and the current line.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_line]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 line for all lines below this line and decreases the Total Lines by 1.", " ", " ", " ", " ", " ",
                                " ", " ");
                    }
                    else if (event.getSource() == add[lsiv_line]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 line for all lines below this line, inserts a new line at the current position, copies the values of all parameters from the previous line to the new line, and increases the Total Lines by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of class HelpListener

    boolean isError() {
        int sumOfLines = getNumberOfLine();

        for (int lsiv_line = 1; lsiv_line <= sumOfLines; lsiv_line++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                String fld_name;

                fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1 + lsiv_field];

                if (text_line[lsiv_line][lsiv_field].isEnabled()) {
                    if (text_line[lsiv_line][lsiv_field].getText().trim().length() == 0) {
                        JOptionPane.showMessageDialog(null, "Line " + lsiv_line + " " + fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }

                    int lsiv_value;

                    try {
                        lsiv_value = Integer.parseInt(text_line[lsiv_line][lsiv_field].getText().trim());
                    }
                    catch (Exception e) {
                        JOptionPane.showMessageDialog(null, "Line " + lsiv_line + " " + fld_name + " = '" + text_line[lsiv_line][lsiv_field].getText().trim()
                                + "' contains an illegal character for an integer.", "Error Message", JOptionPane.ERROR_MESSAGE);
                        return true;
                    }
                }
            } // end of for(int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++)
        } // end of for(int lsiv_line = 1; lsiv_line <= sumOfLines; lsiv_line++)

        return false;

    } // end of isError

    void saveData() {
        int numOfLines = getNumberOfLine();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines = numOfLines;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_HEADER_NUM_LINES] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_line = 1; lsiv_line <= numOfLines; lsiv_line++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_line_number = lsiv_line;

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_beg_x = Integer.valueOf(text_line[lsiv_line][1].getText().trim()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_beg_y = Integer.valueOf(text_line[lsiv_line][2].getText().trim()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_end_x = Integer.valueOf(text_line[lsiv_line][3].getText().trim()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_end_y = Integer.valueOf(text_line[lsiv_line][4].getText().trim()).intValue();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_LINE_NUMBER] = gdvsim.gclv_inter.TX_FROM_USER;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_line = numOfLines + 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_LINE_NUMBER] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LINE_DATA_BEG_X - 1
                        + lsiv_field] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();
    } // end of saveData()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_line_ok = true;
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                if (event.getSource() == down[lsiv_line]) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        String temp;
                        temp = text_line[lsiv_line][lsiv_field].getText();
                        text_line[lsiv_line][lsiv_field].setText(text_line[lsiv_line + 1][lsiv_field].getText());
                        text_line[lsiv_line + 1][lsiv_field].setText(temp);
                    }
                    break;
                }
                else if (event.getSource() == up[lsiv_line]) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        String temp;
                        temp = text_line[lsiv_line][lsiv_field].getText();
                        text_line[lsiv_line][lsiv_field].setText(text_line[lsiv_line - 1][lsiv_field].getText());
                        text_line[lsiv_line - 1][lsiv_field].setText(temp);
                    }
                    break;
                }
                else if (event.getSource() == del[lsiv_line]) {
                    int numOfLineBeforeClick;
                    numOfLineBeforeClick = getNumberOfLine();
                    setValueAfterDel(numOfLineBeforeClick, lsiv_line);
                    setStatus(numOfLineBeforeClick - 1);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfLine()));
                    if (!del[lsiv_line].isEnabled()) {
                        okButton.requestFocus();
                    }
                    break;
                }
                else if (event.getSource() == add[lsiv_line]) {
                    int numOfLineBeforeClick;
                    numOfLineBeforeClick = getNumberOfLine();
                    setStatus(numOfLineBeforeClick + 1);
                    setValueAfterAdd(numOfLineBeforeClick, lsiv_line);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfLine()));
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
                        gdvsim.flag_line_ok = true;
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_line = 1; lsiv_line <= PARAMS.TEXAS_MODEL_NLI; lsiv_line++) {
                    if (event.getSource() == down[lsiv_line]) {
                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            String temp;
                            temp = text_line[lsiv_line][lsiv_field].getText();
                            text_line[lsiv_line][lsiv_field].setText(text_line[lsiv_line + 1][lsiv_field].getText());
                            text_line[lsiv_line + 1][lsiv_field].setText(temp);
                        }
                        break;
                    }
                    else if (event.getSource() == up[lsiv_line]) {
                        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                            String temp;
                            temp = text_line[lsiv_line][lsiv_field].getText();
                            text_line[lsiv_line][lsiv_field].setText(text_line[lsiv_line - 1][lsiv_field].getText());
                            text_line[lsiv_line - 1][lsiv_field].setText(temp);
                        }
                        break;
                    }
                    else if (event.getSource() == del[lsiv_line]) {
                        int numOfLineBeforeClick;
                        numOfLineBeforeClick = getNumberOfLine();
                        setValueAfterDel(numOfLineBeforeClick, lsiv_line);
                        setStatus(numOfLineBeforeClick - 1);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfLine()));
                        if (!del[lsiv_line].isEnabled()) {
                            okButton.requestFocus();
                        }
                        break;
                    }
                    else if (event.getSource() == add[lsiv_line]) {
                        int numOfLineBeforeClick;
                        numOfLineBeforeClick = getNumberOfLine();
                        setStatus(numOfLineBeforeClick + 1);
                        setValueAfterAdd(numOfLineBeforeClick, lsiv_line);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfLine()));
                        break;
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentKeyListener

} // end of class LineDialog.java
