package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                        IntersectionTypeDialog.java                         */
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

public class IntersectionTypeDialog extends JFrame {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JRadioButton standardRadioButton, diamondRadioButton;

    ButtonGroup aButtonGroup;

    JButton okButton, cancelButton;

    JLabel label_title, label_note_1, label_note_2, label_note_3, label_note_4, label_note_5, label_note_6, label_note_7;

    Font font;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    RadioButtonKeyListener radioButtonKeyListener;

    HelpListener helpListener;

    public IntersectionTypeDialog() {
        aFrame = new JFrame("Intersection Type Data");
        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        container.setLayout(gbLayout);
        gbConstraints.fill = GridBagConstraints.BOTH;

        font = new Font("TimesRoman", Font.BOLD, 20);

        label_title = new JLabel("Please select an Intersection Type:");
        label_title.setFont(font);

        standardRadioButton = new JRadioButton("Standard Intersection", true);
        diamondRadioButton = new JRadioButton("Diamond Interchange", false);

        aButtonGroup = new ButtonGroup();
        aButtonGroup.add(standardRadioButton);
        aButtonGroup.add(diamondRadioButton);

        standardRadioButton.getAccessibleContext().setAccessibleName("Standard");
        standardRadioButton.getAccessibleContext().setAccessibleDescription("Standard");

        diamondRadioButton.getAccessibleContext().setAccessibleName("Diamond");
        diamondRadioButton.getAccessibleContext().setAccessibleDescription("Diamond");

        okButton = new JButton("  OK  ");
        cancelButton = new JButton("Cancel");

        standardRadioButton.setMnemonic(KeyEvent.VK_S);
        diamondRadioButton.setMnemonic(KeyEvent.VK_D);
        okButton.setMnemonic(KeyEvent.VK_O);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(cancelButton);

        label_note_1 = new JLabel("NOTE:");
        label_note_2 = new JLabel("The geometry of a Standard Intersection is configured as a single, three- to six-leg, at-grade intersection.");
        label_note_3 = new JLabel("Please see Help -> Standard Intersection for more information.");
        label_note_4 = new JLabel("The geometry of the Diamond Interchange is configured as two adjacent, three-leg, at-grade intersections connected by a set of internal lanes.");
        label_note_5 = new JLabel("The diamond interchange does not include the Freeway lanes that typically go over or under the internal lanes.");
        label_note_6 = new JLabel("Please see Help -> Diamond Interchange for more information.");
        label_note_7 = new JLabel("END NOTE");

        int iRow = 0;

        gbConstraints.insets = new Insets(5, 0, 5, 0);
        addComponent(label_title, iRow++, 0, 1, 1);

        gbConstraints.insets = new Insets(2, 20, 2, 0);
        addComponent(standardRadioButton, iRow++, 0, 1, 1);
        addComponent(diamondRadioButton, iRow++, 0, 1, 1);

        gbConstraints.insets = new Insets(15, 40, 8, 0);
        addComponent(label_note_1, iRow++, 0, 1, 1);
        gbConstraints.insets = new Insets(2, 40, 2, 0);
        addComponent(label_note_2, iRow++, 0, 1, 1);
        addComponent(label_note_3, iRow++, 0, 1, 1);
        addComponent(label_note_4, iRow++, 0, 1, 1);
        addComponent(label_note_5, iRow++, 0, 1, 1);
        addComponent(label_note_6, iRow++, 0, 1, 1);
        gbConstraints.insets = new Insets(8, 40, 2, 0);
        addComponent(label_note_7, iRow++, 0, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 0);
        addComponent(ok_panel, iRow++, 0, 1, 1);

        helpListener = new HelpListener();
        standardRadioButton.addKeyListener(helpListener);
        diamondRadioButton.addKeyListener(helpListener);
        okButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okApplyActionListener = new OkApplyActionListener();
        radioButtonKeyListener = new RadioButtonKeyListener();

        standardRadioButton.addKeyListener(radioButtonKeyListener);
        diamondRadioButton.addKeyListener(radioButtonKeyListener);
        standardRadioButton.addActionListener(okApplyActionListener);
        diamondRadioButton.addActionListener(okApplyActionListener);
        okButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        Set<KeyStroke> forwardKeys = new HashSet<KeyStroke>();
        forwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0));
        forwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0));
        aFrame.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, forwardKeys);

        Set<KeyStroke> backwardKeys = new HashSet<KeyStroke>();
        backwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, KeyEvent.SHIFT_DOWN_MASK));
        backwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0));
        aFrame.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, backwardKeys);

        okApplyKeyListener = new OkApplyKeyListener();

        standardRadioButton.addKeyListener(okApplyKeyListener);
        diamondRadioButton.addKeyListener(okApplyKeyListener);
        okButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        aFrame.pack();
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    } // end of method IntersectionTypeDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);

    } // end of method addComponent

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton) {
                if (diamondRadioButton.isSelected()) {
                    gdvsim.gclv_inter.mbov_is_diamond_interchange = true;
                    gdvsim.intersectionTypeButton.setText("Diamond Interchange");
                    gdvsim.gdvDataButton.setText("Diamond GDV Data");
                    // bs
                    gdvsim.viewMenu.remove(gdvsim.centerIntersectionMenuItem);
                    gdvsim.centerInterchangeMenu.add(gdvsim.leftCenterMenuItem);
                    gdvsim.centerInterchangeMenu.add(gdvsim.rightCenterMenuItem);
                    gdvsim.viewMenu.add(gdvsim.centerInterchangeMenu);
                }
                else {
                    gdvsim.gclv_inter.mbov_is_diamond_interchange = false;
                    gdvsim.intersectionTypeButton.setText("Standard Intersection");
                    gdvsim.gdvDataButton.setText("GDV Data");
                    // bs
                    gdvsim.viewMenu.remove(gdvsim.centerInterchangeMenu);
                    gdvsim.viewMenu.add(gdvsim.centerIntersectionMenuItem);
                }

                gdvsim.intersectionTypeButton.setVisible(true);
                gdvsim.intersectionTypeButton.setEnabled(false);
                gdvsim.gdvDataButton.setVisible(true);
                gdvsim.gdvDataButton.requestFocus(true);
                gdvsim.leg1Button.setVisible(false);
                gdvsim.leg2Button.setVisible(false);
                gdvsim.leg3Button.setVisible(false);
                gdvsim.leg4Button.setVisible(false);
                gdvsim.leg5Button.setVisible(false);
                gdvsim.leg6Button.setVisible(false);
                gdvsim.simulationButton.setVisible(false);
                gdvsim.laneControlButton.setVisible(false);
                gdvsim.intersectionControlButton.setVisible(false);
                gdvsim.greenIntervalSequenceButton.setVisible(false);
                gdvsim.detectorButton.setVisible(false);

                aFrame.dispose();
            }

            if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
        } // end of method actionPerformed
    } // end of class OkApplyActionListener

    class RadioButtonKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == standardRadioButton) {
                    standardRadioButton.setSelected(true);
                }

                if (event.getSource() == diamondRadioButton) {
                    diamondRadioButton.setSelected(true);
                }

            }
        }
    }

    class OkApplyKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton) {
                    if (diamondRadioButton.isSelected()) {
                        gdvsim.gclv_inter.mbov_is_diamond_interchange = true;
                        gdvsim.intersectionTypeButton.setText("Diamond Interchange");
                        gdvsim.gdvDataButton.setText("Diamond GDV Data");
                        // bs
                        gdvsim.viewMenu.remove(gdvsim.centerIntersectionMenuItem);
                        gdvsim.centerInterchangeMenu.add(gdvsim.leftCenterMenuItem);
                        gdvsim.centerInterchangeMenu.add(gdvsim.rightCenterMenuItem);
                        gdvsim.viewMenu.add(gdvsim.centerInterchangeMenu);
                    }
                    else {
                        gdvsim.gclv_inter.mbov_is_diamond_interchange = false;
                        gdvsim.intersectionTypeButton.setText("Standard Intersection");
                        gdvsim.gdvDataButton.setText("GDV Data");
                        // bs
                        gdvsim.viewMenu.remove(gdvsim.centerInterchangeMenu);
                        gdvsim.viewMenu.add(gdvsim.centerIntersectionMenuItem);
                    }

                    gdvsim.intersectionTypeButton.setVisible(true);
                    gdvsim.intersectionTypeButton.setEnabled(false);
                    gdvsim.gdvDataButton.setVisible(true);
                    gdvsim.gdvDataButton.requestFocus(true);
                    gdvsim.leg1Button.setVisible(false);
                    gdvsim.leg2Button.setVisible(false);
                    gdvsim.leg3Button.setVisible(false);
                    gdvsim.leg4Button.setVisible(false);
                    gdvsim.leg5Button.setVisible(false);
                    gdvsim.leg6Button.setVisible(false);
                    gdvsim.simulationButton.setVisible(false);
                    gdvsim.laneControlButton.setVisible(false);
                    gdvsim.intersectionControlButton.setVisible(false);
                    gdvsim.greenIntervalSequenceButton.setVisible(false);
                    gdvsim.detectorButton.setVisible(false);

                    aFrame.dispose();
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_ENTER)
        } // end of method keyPressed
    }// end of OkApplyKeyListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                String standardHelp, diamondHelp;

                standardHelp = "The geometry of a Standard Intersection is configured as a single, three- to six-leg, at-grade intersection.  The leg centerline is the imaginary line between the inbound and outbound lanes, or the centerline of any median, or the leftmost lane edge for a leg having only one-way traffic.  The intersection center is a selected reference point in the intersection where two or more leg centerlines cross.  Leg numbers are not specifically designated by gdvsim.  Curb Returns are associated with a leg and the nearest counterclockwise leg.";

                diamondHelp = "The geometry of the Diamond Interchange is configured as two adjacent, three-leg, at-grade intersections connected by a set of internal lanes.  The diamond interchange does not include the Freeway lanes that typically go over or under the internal lanes.  The intersections are designated as the Left Intersection and the Right Intersection with the centerline of the internal lanes oriented due East and West.  The centerline of the internal lanes goes from the Left Intersection center to the Right Intersection center.  Leg 1 is the Right Intersection North leg, Leg 2 is the Right Intersection East leg, Leg 3 is the Right Intersection South leg, Leg 4 is the Left Intersection South leg, Leg 5 is the Left Intersection West leg, and Leg 6 is the Left Intersection North leg.  One or more of the 6 legs may be disabled by setting both the number of inbound lanes and the number of outbound lanes to zero for the disabled leg.  A diamond interchange may be configured with either two-way frontage roads, one-way frontage roads without Free U-Turn lane(s), or one-way frontage roads with Free U-Turn lane(s).  Curb Return 1 is between Leg 1 and the internal lanes and between Leg 3 and the internal lanes, Curb Return 2 is between Leg 1 and Leg 2, Curb Return 3 is between Leg 2 and Leg 3, Curb Return 4 is between Leg 4 and the internal lanes and between Leg 6 and the internal lanes, Curb Return 5 is between Leg 4 and Leg 5, and Curb Return 6 is between Leg 5 and Leg 6.  The leg centerline is the imaginary line between the inbound and outbound lanes, or the centerline of any median, or the leftmost lane edge for a leg having only one-way traffic.  The intersection center is a selected reference point in the intersection where the centerline of the internal lanes and one or more leg centerlines cross.  A Free U-Turn lane directly connects Leg 3 with Leg 4 and/or Leg 6 with Leg 1 in advance of the Left and Right Intersections.  "
                        + "The Free U-Turn geometry is automatically added by gdvsim and comprises 5 segments: (1) a single, exclusive, inbound lane of specified length on the median (left) side of Legs 3 and/or 6, (2) an exclusive, circular-arc, intersection path tangent to the centerline of segment 1, tangent to the centerline of segment 3, and changing direction by approximately 90 degrees, (3) a single, exclusive, internal lane of calculated length with traffic control at the end of this internal lane, (4) an exclusive, circular-arc, intersection path tangent to the centerline of segment 3, tangent to the centerline of segment 5, and changing direction by approximately 90 degrees, and (5) a single, exclusive, outbound lane of specified length on the median (left) side of Legs 4 and/or 1.  The width of the Free U-Turn lane is specified with the same value for segments 1, 3, and 5.";

                if (event.getSource() == diamondRadioButton) {
                    if (diamondRadioButton.isSelected()) {
                        new HelpDialog(true, "Select An Intersection Type RadioButton", "Intersection Type", diamondHelp, "Diamond", "Standard", " ", " ", " ", " ");
                    }
                    else {
                        new HelpDialog(true, "Select An Intersection Type RadioButton", "Intersection Type", diamondHelp, "Standard", "Standard", " ", " ", " ", " ");
                    }
                }
                else if (event.getSource() == standardRadioButton) {
                    if (diamondRadioButton.isSelected()) {
                        new HelpDialog(true, "Select An Intersection Type RadioButton", "Intersection Type", standardHelp, "Diamond", "Standard", " ", " ", " ", " ");
                    }
                    else {
                        new HelpDialog(true, "Select An Intersection Type RadioButton", "Intersection Type", standardHelp, "Standard", "Standard", " ", " ", " ", " ");
                    }
                }
                else if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }
    } // end of class HelpListener()

    class CloseWindowListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ESCAPE) {
                aFrame.dispose();
            }

        }// end of method keyPressed
    } // end of class CloseWindowListener

} // end of class IntersectionTypeDialog
