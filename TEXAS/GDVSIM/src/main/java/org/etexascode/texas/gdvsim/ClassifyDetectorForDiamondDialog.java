package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*               ClassifyDetectorForDiamondDialog.java                        */
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
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

class ClassifyDetectorForDiamondDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 3;

    int DATDIA_CLASS_NAM01 = 1;

    int DATDIA_CLASS_LL_01 = 2;

    int DATDIA_CLASS_UL_01 = 3;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    TextFieldFocusListener textFieldFocusListener;

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JTextField[] text_name = new JTextField[PARAMS.TEXAS_MODEL_LDC + 1];

    JComboBox[] cbo_lower = new JComboBox[PARAMS.TEXAS_MODEL_LDC + 1];

    JComboBox[] cbo_upper = new JComboBox[PARAMS.TEXAS_MODEL_LDC + 1];

    JLabel[] label_class = new JLabel[PARAMS.TEXAS_MODEL_LDC + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_LDC + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_LDC + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_LDC + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_LDC + 1];

    JComboBox cbo_total;

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_total;

    Font font, font1, font2;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    int detectorNumber;

    public ClassifyDetectorForDiamondDialog(int detectorNumberParameter) {
        detectorNumber = detectorNumberParameter;

        int numOfClasses = 0;

        if (gdvsim.det_classify_num_stat[detectorNumber] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            numOfClasses = gdvsim.det_classify_num[detectorNumber];
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA];

        titleString = "Classify Detector Data";
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

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            label_class[lsiv_class] = new JLabel("Vehicle Class " + lsiv_class);
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            add[lsiv_class] = new JButton("Add");
            del[lsiv_class] = new JButton("Delete");
            up[lsiv_class] = new JButton("Up");
            down[lsiv_class] = new JButton("Down");
        }

        label_total = new JLabel("Total Classes");

        String lblName;
        int beg;
        int end;

        lblName = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01];
        beg = lblName.indexOf("Class 01") - 1;
        end = lblName.indexOf("Class 01") + 8;
        lblName = lblName.substring(0, beg) + lblName.substring(end);

        label_field[DATDIA_CLASS_NAM01] = new JTextArea();
        label_field[DATDIA_CLASS_NAM01].setBackground(aFrame.getBackground());
        label_field[DATDIA_CLASS_NAM01].setEditable(false);
        label_field[DATDIA_CLASS_NAM01].setFocusable(false);
        label_field[DATDIA_CLASS_NAM01].setWrapStyleWord(true);
        label_field[DATDIA_CLASS_NAM01].setLineWrap(true);
        label_field[DATDIA_CLASS_NAM01].setFont(font1);
        label_field[DATDIA_CLASS_NAM01].setText(lblName);

        lblName = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01];
        beg = lblName.indexOf("Class 01") - 1;
        end = lblName.indexOf("Class 01") + 8;
        lblName = lblName.substring(0, beg) + lblName.substring(end);

        label_field[DATDIA_CLASS_LL_01] = new JTextArea();
        label_field[DATDIA_CLASS_LL_01].setBackground(aFrame.getBackground());
        label_field[DATDIA_CLASS_LL_01].setEditable(false);
        label_field[DATDIA_CLASS_LL_01].setFocusable(false);
        label_field[DATDIA_CLASS_LL_01].setWrapStyleWord(true);
        label_field[DATDIA_CLASS_LL_01].setLineWrap(true);
        label_field[DATDIA_CLASS_LL_01].setFont(font1);
        label_field[DATDIA_CLASS_LL_01].setText(lblName);

        lblName = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01];
        beg = lblName.indexOf("Class 01") - 1;
        end = lblName.indexOf("Class 01") + 8;
        lblName = lblName.substring(0, beg) + lblName.substring(end);

        label_field[DATDIA_CLASS_UL_01] = new JTextArea();
        label_field[DATDIA_CLASS_UL_01].setBackground(aFrame.getBackground());
        label_field[DATDIA_CLASS_UL_01].setEditable(false);
        label_field[DATDIA_CLASS_UL_01].setFocusable(false);
        label_field[DATDIA_CLASS_UL_01].setWrapStyleWord(true);
        label_field[DATDIA_CLASS_UL_01].setLineWrap(true);
        label_field[DATDIA_CLASS_UL_01].setFont(font1);
        label_field[DATDIA_CLASS_UL_01].setText(lblName);

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            text_name[lsiv_class] = new JTextField();
            text_name[lsiv_class].setBackground(aFrame.getBackground());
            text_name[lsiv_class].setFont(font2);

            int lsiv_min;
            int lsiv_max;
            int lsiv_inc;
            int count;
            int int_number;

            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_lower = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_lower[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            cbo_lower[lsiv_class] = new JComboBox(array_lower);

            if (lsiv_class == PARAMS.TEXAS_MODEL_LDC) {
                lsiv_min = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                lsiv_inc = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
            }
            else {
                lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
            }

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_upper = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_upper[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            cbo_upper[lsiv_class] = new JComboBox(array_upper);
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            text_name[lsiv_class].setText(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class]);
            cbo_lower[lsiv_class].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]));
            cbo_lower[lsiv_class].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class]));
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (gdvsim.det_classify_name_stat[detectorNumber][lsiv_class] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                text_name[lsiv_class].setText(gdvsim.det_classify_name[detectorNumber][lsiv_class]);
            }

            if (gdvsim.det_classify_lower_stat[detectorNumber][lsiv_class] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cbo_lower[lsiv_class].setSelectedItem(Integer.toString(gdvsim.det_classify_lower[detectorNumber][lsiv_class]));
            }

            if (gdvsim.det_classify_upper_stat[detectorNumber][lsiv_class] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cbo_upper[lsiv_class].setSelectedItem(Integer.toString(gdvsim.det_classify_upper[detectorNumber][lsiv_class]));
            }
        }

        setStatus(numOfClasses);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfClasses()));

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

        textFieldFocusListener = new TextFieldFocusListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            add[lsiv_class].addActionListener(componentActionListener);
            del[lsiv_class].addActionListener(componentActionListener);
            up[lsiv_class].addActionListener(componentActionListener);
            down[lsiv_class].addActionListener(componentActionListener);

            add[lsiv_class].addKeyListener(componentKeyListener);
            del[lsiv_class].addKeyListener(componentKeyListener);
            up[lsiv_class].addKeyListener(componentKeyListener);
            down[lsiv_class].addKeyListener(componentKeyListener);

            text_name[lsiv_class].addKeyListener(helpListener);
            text_name[lsiv_class].addFocusListener(textFieldFocusListener);

            cbo_lower[lsiv_class].addKeyListener(openComboMenuListener);
            cbo_lower[lsiv_class].addKeyListener(helpListener);

            cbo_upper[lsiv_class].addKeyListener(openComboMenuListener);
            cbo_upper[lsiv_class].addKeyListener(helpListener);

            add[lsiv_class].addKeyListener(helpListener);
            del[lsiv_class].addKeyListener(helpListener);
            up[lsiv_class].addKeyListener(helpListener);
            down[lsiv_class].addKeyListener(helpListener);

            cbo_upper[lsiv_class].addActionListener(componentActionListener);
            cbo_upper[lsiv_class].addKeyListener(componentKeyListener);
            cbo_lower[lsiv_class].addActionListener(componentActionListener);
            cbo_lower[lsiv_class].addKeyListener(componentKeyListener);
        }

        cbo_total.addKeyListener(openComboMenuListener);
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
        int iCol = 8;

        gbConstraints.insets = new Insets(1, 0, 0, 0);
        addComponent(panel_title, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 1);
        }

        iRow++;

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            gbConstraints.insets = new Insets(1, 1, 1, 1);
            addComponent(label_class[lsiv_class], iRow, 0, 1, 1);
            addComponent(add[lsiv_class], iRow, 1, 1, 1);
            addComponent(del[lsiv_class], iRow, 2, 1, 1);
            addComponent(up[lsiv_class], iRow, 3, 1, 1);
            addComponent(down[lsiv_class], iRow, 4, 1, 1);

            gbConstraints.insets = new Insets(1, 2, 1, 2);

            addComponent(text_name[lsiv_class], iRow, 5, 1, 1);
            addComponent(cbo_lower[lsiv_class], iRow, 6, 1, 1);
            addComponent(cbo_upper[lsiv_class], iRow, 7, 1, 1);

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(label_total, iRow, 0, 1, 1);
        addComponent(cbo_total, iRow++, 1, 1, 1);
        addComponent(ok_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(850, 700);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method ClassifyDetectorForDiamondDialog()

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getNumberOfClasses() {
        int sum = 0;

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (cbo_upper[lsiv_class].isEnabled())
                sum++;
        }

        return sum;
    } // end of method getNumberOfClasses()

    class TextFieldFocusListener implements FocusListener {

        public void focusLost(FocusEvent event) {
            int sumOfClasses = getNumberOfClasses();

            for (int lsiv_class = 1; lsiv_class <= sumOfClasses; lsiv_class++) {
                if (event.getSource() == text_name[lsiv_class]) {
                    text_name[lsiv_class].setBackground(aFrame.getBackground());
                }
            }
        }

        public void focusGained(FocusEvent event) {
            int sumOfClasses = getNumberOfClasses();

            for (int lsiv_class = 1; lsiv_class <= sumOfClasses; lsiv_class++) {
                if (event.getSource() == text_name[lsiv_class]) {
                    text_name[lsiv_class].setBackground(new Color(176, 197, 218));
                }
            }
        }
    } // end of class TextFieldFocusListener()

    void setAccessiblility() {
        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            text_name[lsiv_class].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class]);
            text_name[lsiv_class].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class]);

            cbo_lower[lsiv_class].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]);
            cbo_lower[lsiv_class].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]);

            cbo_upper[lsiv_class].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class]);
            cbo_upper[lsiv_class].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class]);

            add[lsiv_class].getAccessibleContext().setAccessibleName("Add    for Vehicle Class " + lsiv_class);
            add[lsiv_class].getAccessibleContext().setAccessibleDescription("Add    for Vehicle Class " + lsiv_class);
            del[lsiv_class].getAccessibleContext().setAccessibleName("Delete for Vehicle Class " + lsiv_class);
            del[lsiv_class].getAccessibleContext().setAccessibleDescription("Delete for Vehicle Class " + lsiv_class);
            up[lsiv_class].getAccessibleContext().setAccessibleName("Up     for Vehicle Class " + lsiv_class);
            up[lsiv_class].getAccessibleContext().setAccessibleDescription("Up     for Vehicle Class " + lsiv_class);
            down[lsiv_class].getAccessibleContext().setAccessibleName("Down   for Vehicle Class " + lsiv_class);
            down[lsiv_class].getAccessibleContext().setAccessibleDescription("Down   for Vehicle Class " + lsiv_class);
        }

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK button");
        okButton.getAccessibleContext().setAccessibleDescription("OK button");

        applyButton.getAccessibleContext().setAccessibleName("Apply button");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply button");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel button");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel button");
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

    void setStatus(int sumOfClasses) {
        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (lsiv_class <= sumOfClasses) {
                label_class[lsiv_class].setEnabled(true);
                cbo_lower[lsiv_class].setEnabled(true);
                cbo_upper[lsiv_class].setEnabled(true);
                text_name[lsiv_class].setEditable(true);
                text_name[lsiv_class].setFocusable(true);
            }
            else {
                label_class[lsiv_class].setEnabled(false);
                cbo_lower[lsiv_class].setEnabled(false);
                cbo_upper[lsiv_class].setEnabled(false);
                text_name[lsiv_class].setEditable(false);
                text_name[lsiv_class].setFocusable(false);
            }
        }

        if (sumOfClasses == PARAMS.TEXAS_MODEL_LDC) {
            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                add[lsiv_class].setEnabled(false);
            }
        }
        else {
            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                if (lsiv_class <= (sumOfClasses + 1)) {
                    add[lsiv_class].setEnabled(true);
                }
                else {
                    add[lsiv_class].setEnabled(false);
                }
            }
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (lsiv_class <= sumOfClasses) {
                del[lsiv_class].setEnabled(true);
            }
            else {
                del[lsiv_class].setEnabled(false);
            }
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (lsiv_class <= sumOfClasses) {
                up[lsiv_class].setEnabled(true);
            }
            else {
                up[lsiv_class].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (lsiv_class < sumOfClasses) {
                down[lsiv_class].setEnabled(true);
            }
            else {
                down[lsiv_class].setEnabled(false);
            }
        }

        setTextFieldColor();
    } // end of method setStatus

    void setTextFieldColor() {
        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            if (text_name[lsiv_class].isEditable()) {
                text_name[lsiv_class].setBackground(aFrame.getBackground());
                text_name[lsiv_class].setBorder(new LineBorder((new Color(90, 90, 90)), 1));
                text_name[lsiv_class].setForeground(new Color(0, 0, 0));
            }
            else {
                text_name[lsiv_class].setBackground(aFrame.getBackground());
                text_name[lsiv_class].setBorder(new LineBorder((new Color(160, 160, 160)), 1));
                text_name[lsiv_class].setForeground(new Color(176, 197, 218));
            }
        }
    } // end of method setTextFieldColor

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            cbo_lower[lsiv_class].removeActionListener(componentActionListener);
            cbo_lower[lsiv_class].removeKeyListener(componentKeyListener);
            cbo_upper[lsiv_class].removeActionListener(componentActionListener);
            cbo_upper[lsiv_class].removeKeyListener(componentKeyListener);
        }

        for (int lsiv_class = sum; lsiv_class >= index; lsiv_class--) {
            text_name[lsiv_class + 1].setText(text_name[lsiv_class].getText());
            cbo_upper[lsiv_class + 1].setSelectedItem(cbo_upper[lsiv_class].getSelectedItem().toString());

            if (lsiv_class == index) {
                cbo_lower[lsiv_class + 1].setSelectedItem(cbo_upper[lsiv_class].getSelectedItem().toString());
            }
            else {
                cbo_lower[lsiv_class + 1].setSelectedItem(cbo_lower[lsiv_class].getSelectedItem().toString());
            }
        }

        if (index == sum + 1 && index != 1) {
            cbo_lower[index].setSelectedItem(cbo_upper[sum].getSelectedItem().toString());
        }

        for (int lsiv_class = PARAMS.TEXAS_MODEL_LDC; lsiv_class >= 1; lsiv_class--) {
            if (cbo_upper[lsiv_class].isEnabled()) {
                cbo_upper[lsiv_class].setSelectedItem("999");
                break;
            }
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            cbo_lower[lsiv_class].addActionListener(componentActionListener);
            cbo_lower[lsiv_class].addKeyListener(componentKeyListener);
            cbo_upper[lsiv_class].addActionListener(componentActionListener);
            cbo_upper[lsiv_class].addKeyListener(componentKeyListener);
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            cbo_lower[lsiv_class].removeActionListener(componentActionListener);
            cbo_lower[lsiv_class].removeKeyListener(componentKeyListener);
            cbo_upper[lsiv_class].removeActionListener(componentActionListener);
            cbo_upper[lsiv_class].removeKeyListener(componentKeyListener);
        }

        for (int lsiv_class = index; lsiv_class < sum; lsiv_class++) {
            text_name[lsiv_class].setText(text_name[lsiv_class + 1].getText());
            cbo_upper[lsiv_class].setSelectedItem(cbo_upper[lsiv_class + 1].getSelectedItem().toString());

            if (lsiv_class != 1) {
                cbo_lower[lsiv_class].setSelectedItem(cbo_upper[lsiv_class - 1].getSelectedItem().toString());
            }
        }

        text_name[sum].setText(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + sum]);
        cbo_lower[sum].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + sum]));
        cbo_upper[sum].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + sum]));

        for (int lsiv_class = PARAMS.TEXAS_MODEL_LDC; lsiv_class >= 1; lsiv_class--) {
            if (cbo_upper[lsiv_class].isEnabled()) {
                cbo_upper[lsiv_class].setSelectedItem("999");
                break;
            }
        }

        for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            cbo_lower[lsiv_class].addActionListener(componentActionListener);
            cbo_lower[lsiv_class].addKeyListener(componentKeyListener);
            cbo_upper[lsiv_class].addActionListener(componentActionListener);
            cbo_upper[lsiv_class].addKeyListener(componentKeyListener);
        }
    } // end of method setValueAfterDel()

    void UpAction(int lsiv_class) {
        for (int lsiv_class_i = 1; lsiv_class_i <= PARAMS.TEXAS_MODEL_LDC; lsiv_class_i++) {
            cbo_lower[lsiv_class_i].removeActionListener(componentActionListener);
            cbo_lower[lsiv_class_i].removeKeyListener(componentKeyListener);
            cbo_upper[lsiv_class_i].removeActionListener(componentActionListener);
            cbo_upper[lsiv_class_i].removeKeyListener(componentKeyListener);
        }

        String temp;
        temp = text_name[lsiv_class].getText();
        text_name[lsiv_class].setText(text_name[lsiv_class - 1].getText());
        text_name[lsiv_class - 1].setText(temp);

        temp = cbo_upper[lsiv_class].getSelectedItem().toString();
        cbo_upper[lsiv_class].setSelectedItem(cbo_upper[lsiv_class - 1].getSelectedItem().toString());
        cbo_upper[lsiv_class - 1].setSelectedItem(temp);

        cbo_lower[lsiv_class].setSelectedItem(cbo_upper[lsiv_class - 1].getSelectedItem().toString());

        if (lsiv_class != getNumberOfClasses()) {
            cbo_lower[lsiv_class + 1].setSelectedItem(cbo_upper[lsiv_class].getSelectedItem().toString());
        }

        for (int lsiv_class_i = PARAMS.TEXAS_MODEL_LDC; lsiv_class_i >= 1; lsiv_class_i--) {
            if (cbo_upper[lsiv_class_i].isEnabled()) {
                cbo_upper[lsiv_class_i].setSelectedItem("999");
                break;
            }
        }

        for (int lsiv_class_i = 1; lsiv_class_i <= PARAMS.TEXAS_MODEL_LDC; lsiv_class_i++) {
            cbo_lower[lsiv_class_i].addActionListener(componentActionListener);
            cbo_lower[lsiv_class_i].addKeyListener(componentKeyListener);
            cbo_upper[lsiv_class_i].addActionListener(componentActionListener);
            cbo_upper[lsiv_class_i].addKeyListener(componentKeyListener);
        }
    } // end of method UpAction

    void DownAction(int lsiv_class) {
        for (int lsiv_class_i = 1; lsiv_class_i <= PARAMS.TEXAS_MODEL_LDC; lsiv_class_i++) {
            cbo_lower[lsiv_class_i].removeActionListener(componentActionListener);
            cbo_lower[lsiv_class_i].removeKeyListener(componentKeyListener);
            cbo_upper[lsiv_class_i].removeActionListener(componentActionListener);
            cbo_upper[lsiv_class_i].removeKeyListener(componentKeyListener);
        }

        String temp;
        temp = text_name[lsiv_class].getText();
        text_name[lsiv_class].setText(text_name[lsiv_class + 1].getText());
        text_name[lsiv_class + 1].setText(temp);

        temp = cbo_upper[lsiv_class].getSelectedItem().toString();
        cbo_upper[lsiv_class].setSelectedItem(cbo_upper[lsiv_class + 1].getSelectedItem().toString());
        cbo_upper[lsiv_class + 1].setSelectedItem(temp);

        cbo_lower[lsiv_class + 1].setSelectedItem(cbo_upper[lsiv_class].getSelectedItem().toString());

        if (lsiv_class != getNumberOfClasses() - 1) {
            cbo_lower[lsiv_class + 2].setSelectedItem(cbo_upper[lsiv_class + 1].getSelectedItem().toString());
        }

        for (int lsiv_class_i = PARAMS.TEXAS_MODEL_LDC; lsiv_class_i >= 1; lsiv_class_i--) {
            if (cbo_upper[lsiv_class_i].isEnabled()) {
                cbo_upper[lsiv_class_i].setSelectedItem("999");
                break;
            }
        }

        for (int lsiv_class_i = 1; lsiv_class_i <= PARAMS.TEXAS_MODEL_LDC; lsiv_class_i++) {
            cbo_lower[lsiv_class_i].addActionListener(componentActionListener);
            cbo_lower[lsiv_class_i].addKeyListener(componentKeyListener);
            cbo_upper[lsiv_class_i].addActionListener(componentActionListener);
            cbo_upper[lsiv_class_i].addKeyListener(componentKeyListener);
        }
    } // end of method DownAction

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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_LDC), "1");
                }

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    if (event.getSource() == text_name[lsiv_class]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class], lclv_tx_fmt.mstv_name.replace("#",
                                Integer.toString(detectorNumber)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class], text_name[lsiv_class].getText(),
                                lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class],
                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class], " ", " ", " ");
                    }
                    else if (event.getSource() == cbo_lower[lsiv_class]) {
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class], lclv_tx_fmt.mstv_name.replace("#",
                                Integer.toString(detectorNumber)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class], cbo_lower[lsiv_class].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class]));
                    }
                    else if (event.getSource() == cbo_upper[lsiv_class]) {
                        int lsiv_min;
                        int lsiv_max;
                        int lsiv_def;

                        if (lsiv_class == PARAMS.TEXAS_MODEL_LDC) {
                            lsiv_def = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                            lsiv_min = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                        }
                        else {
                            lsiv_def = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];
                        }

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class], lclv_tx_fmt.mstv_name.replace("#",
                                Integer.toString(detectorNumber)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class], cbo_upper[lsiv_class].getSelectedItem().toString(),
                                Integer.toString(lsiv_def), " ", Integer.toString(lsiv_min), Integer.toString(lsiv_max),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class]));
                    }
                    else if (event.getSource() == up[lsiv_class]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous vehicle class and the current vehicle class.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_class]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next vehicle class and the current vehicle class.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_class]) {
                        new HelpDialog(true, "Delete Button",
                                "The Delete button moves the data up 1 line for all vehicle classes below this vehicle class and decreases the Total Vehicle Classes by 1.", " ", " ", " ", " ", " ",
                                " ", " ");
                    }
                    else if (event.getSource() == add[lsiv_class]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 vehicle class for all vehicle classes below this vehicle class, inserts a new vehicle class at the current position, copies the values of all parameters from the previous vehicle class to the new vehicle class, and increases the Total Vehicle Classes by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of class HelpListener

    void saveData() {
        int numOfClasses = gdvsim.det_classify_num[detectorNumber] = getNumberOfClasses();
        gdvsim.det_classify_num_stat[detectorNumber] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_class = 1; lsiv_class <= numOfClasses; lsiv_class++) {
            gdvsim.det_classify_name[detectorNumber][lsiv_class] = text_name[lsiv_class].getText().trim();
            gdvsim.det_classify_lower[detectorNumber][lsiv_class] = Integer.valueOf(cbo_lower[lsiv_class].getSelectedItem().toString()).intValue();
            gdvsim.det_classify_upper[detectorNumber][lsiv_class] = Integer.valueOf(cbo_upper[lsiv_class].getSelectedItem().toString()).intValue();

            gdvsim.det_classify_name_stat[detectorNumber][lsiv_class] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.det_classify_lower_stat[detectorNumber][lsiv_class] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.det_classify_upper_stat[detectorNumber][lsiv_class] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_class = numOfClasses + 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
            gdvsim.det_classify_name[detectorNumber][lsiv_class] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class];
            gdvsim.det_classify_lower[detectorNumber][lsiv_class] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_LL_01 - 1 + lsiv_class];
            gdvsim.det_classify_upper[detectorNumber][lsiv_class] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_UL_01 - 1 + lsiv_class];

            gdvsim.det_classify_name_stat[detectorNumber][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.det_classify_lower_stat[detectorNumber][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.det_classify_upper_stat[detectorNumber][lsiv_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method saveData()

    boolean isError() {
        int numOfClasses = getNumberOfClasses();

        if (numOfClasses == 0) {
            JOptionPane.showMessageDialog(null, "Total Vehicle Classes should be greater than or equal to 1 ", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        for (int lsiv_class = 1; lsiv_class <= numOfClasses; lsiv_class++) {
            String fld_name;

            fld_name = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_DATDIA_CLASS_NAM01 - 1 + lsiv_class];

            if (text_name[lsiv_class].isEditable()) {
                if (text_name[lsiv_class].getText().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, fld_name + " is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        } // end of for(int lsiv_class = 1; lsiv_class <= numOfClasses; lsiv_class++)

        for (int lsiv_class = 1; lsiv_class <= numOfClasses; lsiv_class++) {
            if (Integer.valueOf(cbo_upper[lsiv_class].getSelectedItem().toString()).intValue() <= Integer.valueOf(cbo_lower[lsiv_class].getSelectedItem().toString()).intValue()) {
                JOptionPane.showMessageDialog(null, "The length upper limit should be greater than then length low limit for class " + lsiv_class + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        if (Integer.valueOf(cbo_upper[numOfClasses].getSelectedItem().toString()).intValue() != 999) {
            JOptionPane.showMessageDialog(null, "The last length upper limit should be equal to 999.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        return false;
    } // end of method isError()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                if (event.getSource() == cbo_upper[lsiv_class]) {
                    if (lsiv_class != getNumberOfClasses()) {
                        cbo_lower[lsiv_class + 1].setSelectedItem(cbo_upper[lsiv_class].getSelectedItem().toString());
                    }
                    break;
                }
                else if (event.getSource() == cbo_lower[lsiv_class]) {
                    if (lsiv_class > 1) {
                        cbo_upper[lsiv_class - 1].setSelectedItem(cbo_lower[lsiv_class].getSelectedItem().toString());
                    }
                    break;
                }
                else if (event.getSource() == down[lsiv_class]) {
                    DownAction(lsiv_class);
                    break;
                }
                else if (event.getSource() == up[lsiv_class]) {
                    UpAction(lsiv_class);
                    break;
                }
                else if (event.getSource() == del[lsiv_class]) {
                    int numOfClassesBeforeClick = getNumberOfClasses();
                    setStatus(numOfClassesBeforeClick - 1);
                    setValueAfterDel(numOfClassesBeforeClick, lsiv_class);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfClasses()));

                    if (!del[lsiv_class].isEnabled()) {
                        okButton.requestFocus();
                    }

                    break;
                }
                else if (event.getSource() == add[lsiv_class]) {
                    int numOfClassesBeforeClick = getNumberOfClasses();
                    setStatus(numOfClassesBeforeClick + 1);
                    setValueAfterAdd(numOfClassesBeforeClick, lsiv_class);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfClasses()));
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

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_class = 1; lsiv_class <= PARAMS.TEXAS_MODEL_LDC; lsiv_class++) {
                    if (event.getSource() == cbo_upper[lsiv_class]) {
                        if (lsiv_class != getNumberOfClasses()) {
                            cbo_lower[lsiv_class + 1].setSelectedItem(cbo_upper[lsiv_class].getSelectedItem().toString());
                        }
                        break;
                    }
                    else if (event.getSource() == cbo_lower[lsiv_class]) {
                        if (lsiv_class > 1) {
                            cbo_upper[lsiv_class - 1].setSelectedItem(cbo_lower[lsiv_class].getSelectedItem().toString());
                        }
                        break;
                    }
                    else if (event.getSource() == down[lsiv_class]) {
                        DownAction(lsiv_class);
                        break;
                    }
                    else if (event.getSource() == up[lsiv_class]) {
                        UpAction(lsiv_class);
                        break;
                    }
                    else if (event.getSource() == del[lsiv_class]) {
                        int numOfClassesBeforeClick = getNumberOfClasses();
                        setStatus(numOfClassesBeforeClick - 1);
                        setValueAfterDel(numOfClassesBeforeClick, lsiv_class);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfClasses()));

                        if (!del[lsiv_class].isEnabled()) {
                            okButton.requestFocus();
                        }

                        break;
                    }
                    else if (event.getSource() == add[lsiv_class]) {
                        int numOfClassesBeforeClick = getNumberOfClasses();
                        setStatus(numOfClassesBeforeClick + 1);
                        setValueAfterAdd(numOfClassesBeforeClick, lsiv_class);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfClasses()));

                        break;
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentKeyListener

} // end of class ClassifyDetectorForDiamondDialog.java
