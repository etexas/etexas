package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              HelpDialog.java                               */
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

class HelpDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JButton closeButton;

    JPanel ok_panel;

    HelpActionListener helpActionListener;

    HelpKeyListener helpKeyListener;

    CloseWindowListener closeWindowListener;

    Font font1;

    JTextArea labelItem, labelDescription, labelHelpInformation, labelCurrentValue, labelDefaultValue, labelPossibleValue, labelMinimumValue, labelMaximumValue, labelIncrementValue;

    JTextArea textAreaItem, textAreaDescription, textAreaHelpInformation, textAreaCurrentValue, textAreaDefaultValue, textAreaPossibleValue, textAreaMinimumValue, textAreaMaximumValue,
            textAreaIncrementValue;

    public HelpDialog(Boolean defaultValueFlag, String item, String description, String helpInformation, String currentValue, String defaultValue, String possibleValue, String minimumValue,
            String maximumValue, String incrementValue) {
        aFrame = new JFrame("Help");

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

        font1 = new Font("TimesRoman", Font.BOLD, 14);

        closeButton = new JButton("Close");
        closeButton.setMnemonic(KeyEvent.VK_C);

        closeButton.getAccessibleContext().setAccessibleName("Close");
        closeButton.getAccessibleContext().setAccessibleDescription("Close");

        ok_panel = new JPanel();
        ok_panel.add(closeButton);

        labelItem = new JTextArea("Item:");
        labelDescription = new JTextArea("Description:");
        labelHelpInformation = new JTextArea("Help Information:");
        labelCurrentValue = new JTextArea("Current Value:");
        labelDefaultValue = new JTextArea("Default Value:");
        labelPossibleValue = new JTextArea("Possible Values:");
        labelMinimumValue = new JTextArea("Minimum Value:");
        labelMaximumValue = new JTextArea("Maximum Value:");
        labelIncrementValue = new JTextArea("Increment Value:");

        labelItem.setBackground(aFrame.getBackground());
        labelItem.setEditable(false);
        labelItem.setSize(1, 1);
        labelItem.setFont(font1);

        labelDescription.setBackground(aFrame.getBackground());
        labelDescription.setEditable(false);
        labelDescription.setFont(font1);

        labelHelpInformation.setBackground(aFrame.getBackground());
        labelHelpInformation.setEditable(false);
        labelHelpInformation.setSize(1, 1);
        labelHelpInformation.setFont(font1);

        labelCurrentValue.setBackground(aFrame.getBackground());
        labelCurrentValue.setEditable(false);
        labelCurrentValue.setFont(font1);

        labelDefaultValue.setBackground(aFrame.getBackground());
        labelDefaultValue.setEditable(false);
        labelDefaultValue.setFont(font1);

        labelPossibleValue.setBackground(aFrame.getBackground());
        labelPossibleValue.setEditable(false);
        labelPossibleValue.setFont(font1);

        labelMinimumValue.setBackground(aFrame.getBackground());
        labelMinimumValue.setEditable(false);
        labelMinimumValue.setFont(font1);

        labelMaximumValue.setBackground(aFrame.getBackground());
        labelMaximumValue.setEditable(false);
        labelMaximumValue.setFont(font1);

        labelIncrementValue.setBackground(aFrame.getBackground());
        labelIncrementValue.setEditable(false);
        labelIncrementValue.setFont(font1);

        textAreaItem = new JTextArea();
        textAreaItem.setBackground(aFrame.getBackground());
        textAreaItem.setEditable(false);
        textAreaItem.setWrapStyleWord(true);
        textAreaItem.setLineWrap(true);
        textAreaItem.setFont(font1);

        textAreaDescription = new JTextArea();
        textAreaDescription.setBackground(aFrame.getBackground());
        textAreaDescription.setEditable(false);
        textAreaDescription.setWrapStyleWord(true);
        textAreaDescription.setLineWrap(true);
        textAreaDescription.setFont(font1);

        textAreaHelpInformation = new JTextArea();
        textAreaHelpInformation.setBackground(aFrame.getBackground());
        textAreaHelpInformation.setEditable(false);
        textAreaHelpInformation.setWrapStyleWord(true);
        textAreaHelpInformation.setLineWrap(true);
        textAreaHelpInformation.setFont(font1);

        textAreaCurrentValue = new JTextArea();
        textAreaCurrentValue.setBackground(aFrame.getBackground());
        textAreaCurrentValue.setEditable(false);
        textAreaCurrentValue.setWrapStyleWord(true);
        textAreaCurrentValue.setLineWrap(true);
        textAreaCurrentValue.setFont(font1);

        textAreaDefaultValue = new JTextArea();
        textAreaDefaultValue.setBackground(aFrame.getBackground());
        textAreaDefaultValue.setEditable(false);
        textAreaDefaultValue.setWrapStyleWord(true);
        textAreaDefaultValue.setLineWrap(true);
        textAreaDefaultValue.setFont(font1);

        textAreaPossibleValue = new JTextArea();
        textAreaPossibleValue.setBackground(aFrame.getBackground());
        textAreaPossibleValue.setEditable(false);
        textAreaPossibleValue.setWrapStyleWord(true);
        textAreaPossibleValue.setLineWrap(true);
        textAreaPossibleValue.setFont(font1);

        textAreaMinimumValue = new JTextArea();
        textAreaMinimumValue.setBackground(aFrame.getBackground());
        textAreaMinimumValue.setEditable(false);
        textAreaMinimumValue.setWrapStyleWord(true);
        textAreaMinimumValue.setLineWrap(true);
        textAreaMinimumValue.setFont(font1);

        textAreaMaximumValue = new JTextArea();
        textAreaMaximumValue.setBackground(aFrame.getBackground());
        textAreaMaximumValue.setEditable(false);
        textAreaMaximumValue.setWrapStyleWord(true);
        textAreaMaximumValue.setLineWrap(true);
        textAreaMaximumValue.setFont(font1);

        textAreaIncrementValue = new JTextArea();
        textAreaIncrementValue.setBackground(aFrame.getBackground());
        textAreaIncrementValue.setEditable(false);
        textAreaIncrementValue.setWrapStyleWord(true);
        textAreaIncrementValue.setLineWrap(true);
        textAreaIncrementValue.setFont(font1);

        if (!defaultValueFlag) {
            defaultValue = "";
        }

        // "|A|" = "A"
        // "|A|B|" = "A or B"
        // "|A|B|C|" = "A, B, or C"
        // "|A|B|C|D|" = "A, B, C, or D"
        // "|A|B|C..|X|Y|" = "A, B, C, ...X, or Y"
        String[] temp = possibleValue.substring(1).split("\\|");
        int count = temp.length;
        String s = possibleValue.replaceFirst("\\|", "");
        for (int i = 1; i <= (count - 2); i++) {
            s = s.replaceFirst("\\|", ", ");
        }
        if (count == 2) {
            s = s.replaceFirst("\\|", " or ");
        }
        else if (count >= 3) {
            s = s.replaceFirst("\\|", ", or ");
        }
        s = s.replaceAll("\\|", "");

        textAreaItem.setText(item);
        textAreaDescription.setText(description);
        textAreaHelpInformation.setText(helpInformation);
        textAreaCurrentValue.setText(currentValue);
        textAreaDefaultValue.setText(defaultValue);
        textAreaPossibleValue.setText(s);
        textAreaMinimumValue.setText(minimumValue);
        textAreaMaximumValue.setText(maximumValue);
        textAreaIncrementValue.setText(incrementValue);

        textAreaItem.getAccessibleContext().setAccessibleName(item);
        textAreaItem.getAccessibleContext().setAccessibleDescription(item);

        textAreaDescription.getAccessibleContext().setAccessibleName(description);
        textAreaDescription.getAccessibleContext().setAccessibleDescription(description);

        textAreaHelpInformation.getAccessibleContext().setAccessibleName(helpInformation);
        textAreaHelpInformation.getAccessibleContext().setAccessibleDescription(helpInformation);

        textAreaCurrentValue.getAccessibleContext().setAccessibleName(currentValue);
        textAreaCurrentValue.getAccessibleContext().setAccessibleDescription(currentValue);

        textAreaDefaultValue.getAccessibleContext().setAccessibleName(defaultValue);
        textAreaDefaultValue.getAccessibleContext().setAccessibleDescription(defaultValue);

        textAreaPossibleValue.getAccessibleContext().setAccessibleName(s);
        textAreaPossibleValue.getAccessibleContext().setAccessibleDescription(s);

        textAreaMinimumValue.getAccessibleContext().setAccessibleName(minimumValue);
        textAreaMinimumValue.getAccessibleContext().setAccessibleDescription(minimumValue);

        textAreaMaximumValue.getAccessibleContext().setAccessibleName(maximumValue);
        textAreaMaximumValue.getAccessibleContext().setAccessibleDescription(maximumValue);

        textAreaIncrementValue.getAccessibleContext().setAccessibleName(incrementValue);
        textAreaIncrementValue.getAccessibleContext().setAccessibleDescription(incrementValue);

        gbConstraints.insets = new Insets(2, 6, 2, 1);

        int row;
        row = 0;
        if (textAreaItem.getText().trim().length() > 0) {
            addComponent(labelItem, row, 0, 1, 1, 0);
            addComponent(textAreaItem, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaDescription.getText().trim().length() > 0) {
            addComponent(labelDescription, row, 0, 1, 1, 0);
            addComponent(textAreaDescription, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaHelpInformation.getText().trim().length() > 0) {
            addComponent(labelHelpInformation, row, 0, 1, 1, 0);
            addComponent(textAreaHelpInformation, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaPossibleValue.getText().trim().length() > 0) {
            addComponent(labelPossibleValue, row, 0, 1, 1, 0);
            addComponent(textAreaPossibleValue, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaMinimumValue.getText().trim().length() > 0) {
            addComponent(labelMinimumValue, row, 0, 1, 1, 0);
            addComponent(textAreaMinimumValue, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaMaximumValue.getText().trim().length() > 0) {
            addComponent(labelMaximumValue, row, 0, 1, 1, 0);
            addComponent(textAreaMaximumValue, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaIncrementValue.getText().trim().length() > 0) {
            addComponent(labelIncrementValue, row, 0, 1, 1, 0);
            addComponent(textAreaIncrementValue, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaDefaultValue.getText().trim().length() > 0) {
            addComponent(labelDefaultValue, row, 0, 1, 1, 0);
            addComponent(textAreaDefaultValue, row, 1, 1, 1, 1);
            row++;
        }
        if (textAreaCurrentValue.getText().trim().length() > 0) {
            addComponent(labelCurrentValue, row, 0, 1, 1, 0);
            addComponent(textAreaCurrentValue, row, 1, 1, 1, 1);
            row++;
        }
        addComponent(ok_panel, row, 0, 2, 1, 1);

        helpActionListener = new HelpActionListener();
        closeButton.addActionListener(helpActionListener);

        helpKeyListener = new HelpKeyListener();
        closeButton.addKeyListener(helpKeyListener);

        aFrame.setSize(700, 700);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.setFocusTraversalPolicy(new focusPolicy());

        closeWindowListener = new CloseWindowListener();
        aFrame.addKeyListener(closeWindowListener);

        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);
    } // end of method HelpDialog

    void addComponent(Component c, int row, int column, int width, int height, double weightx) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbConstraints.weightx = weightx;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class CloseWindowListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ESCAPE) {
                aFrame.dispose();
            }
        }// end of method keyPressed
    } // end of class CloseWindowListener

    class HelpActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == closeButton) {
                aFrame.dispose();
            }
        }
    }

    class HelpKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == closeButton) {
                    aFrame.dispose();
                }
            }
        }
    }

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            return closeButton;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            return closeButton;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            return closeButton;
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return closeButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            return closeButton;
        }

    }

} // end of class HelpDialog