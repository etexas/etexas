package org.etexascode.gui;

/******************************************************************************/
/*                            ViewFileDialog.java                             */
/******************************************************************************/
/*                                                                            */
/*      texas COPYRIGHT (C) 2007 by Rioux Engineering, Austin, Texas USA      */
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

public class ViewFileDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JButton closeButton;

    Font font1;

    HelpListener helpListener;

    CloseActionListener closeActionListener;

    CloseKeyListener closeKeyListener;

    public ViewFileDialog(File fileNameFile) {
        String fileNameName;
        String newLine;
        BufferedReader viewBufferedReader;
        InputStreamReader viewInputStreamReader;
        FileInputStream viewFileInputStream;
        String viewFileRecord;

        try {
            if (fileNameFile == null) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                return;
            }
            fileNameName = fileNameFile.getCanonicalPath().trim();
            if (fileNameName.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        catch (Exception e) {
            System.err.println("ViewFileDialog exception error fileNameFile.getCanonicalPath");
            TexasModelDialog.addLineToLog("ViewFileDialog exception error fileNameFile.getCanonicalPath");
            System.err.println(e.getLocalizedMessage());
            TexasModelDialog.addLineToLog(e.getLocalizedMessage());
            return;
        }

        aFrame = new JFrame();

        aFrame.setTitle("View File '" + fileNameName + "'");

        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        container.setLayout(gbLayout);
        gbConstraints.fill = GridBagConstraints.BOTH;

        font1 = new Font("Monospaced", Font.PLAIN, 12);

        closeButton = new JButton("Close");
        closeButton.setMnemonic(KeyEvent.VK_C);

        TextArea viewTextArea;
        viewTextArea = new TextArea("", 50, 150, TextArea.SCROLLBARS_BOTH);
        viewTextArea.setFont(font1);
        viewTextArea.getAccessibleContext().setAccessibleDescription("File " + fileNameName + " contents");
        viewTextArea.getAccessibleContext().setAccessibleName("File " + fileNameName + " contents");

        helpListener = new HelpListener();
        closeActionListener = new CloseActionListener();
        closeKeyListener = new CloseKeyListener();

        closeButton.addKeyListener(helpListener);
        closeButton.addKeyListener(closeKeyListener);
        closeButton.addActionListener(closeActionListener);

        closeButton.getAccessibleContext().setAccessibleName("Close");
        closeButton.getAccessibleContext().setAccessibleDescription("Close");

        int iRow = 0;

        gbConstraints.insets = new Insets(1, 10, 10, 10);
        addComponent(viewTextArea, iRow++, 0, 1, 1);
        gbConstraints.insets = new Insets(10, 400, 10, 400);
        addComponent(closeButton, iRow++, 0, 1, 1);

        aFrame.setSize(950, 750);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        try {
            viewFileInputStream = new FileInputStream(fileNameFile);
            viewInputStreamReader = new InputStreamReader(viewFileInputStream, "ASCII");
            viewBufferedReader = new BufferedReader(viewInputStreamReader);
            newLine = "";
            while (true) {
                viewFileRecord = viewBufferedReader.readLine();
                if (viewFileRecord == null)
                    break;
                if (viewFileRecord.equals("\f")) {
                    viewTextArea.append(newLine + "******************** FORM FEED ****************** FORM FEED ********************");
                }
                else {
                    viewTextArea.append(newLine + viewFileRecord);
                }
                newLine = "\n";
            }
            viewBufferedReader.close();
            viewInputStreamReader.close();
            viewFileInputStream.close();
        }
        catch (Exception e) {
            System.err.println("ViewFileDialog exception error reading input stream");
            TexasModelDialog.addLineToLog("ViewFileDialog exception error reading input stream");
            System.err.println(e.getLocalizedMessage());
            TexasModelDialog.addLineToLog(e.getLocalizedMessage());
            return;
        }
    } // end of method ViewFileDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == closeButton) {
                    new HelpDialog(true, "Close button", "The Close button closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        } // end of keyPressed
    } // end of HelpListener

    class CloseActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == closeButton) {
                aFrame.dispose();
            } // end of if(event.getSource() == closeButton)
        } // end of method actionPerformed
    } // end of class CloseActionListener

    class CloseKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == closeButton) {
                    aFrame.dispose();
                } // end of if(event.getSource() == closeButton)
            }
        } // end of method keyPressed
    }// end of CloseKeyListener
} // end of class ViewFileDialog
