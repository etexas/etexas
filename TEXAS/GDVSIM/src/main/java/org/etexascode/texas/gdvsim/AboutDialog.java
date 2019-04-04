package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             AboutDialog.java                               */
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

import org.etexascode.texas.gdvsim.Intersection;
import org.etexascode.texas.gdvsim.gdvsim;
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

class AboutDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JButton closeButton;

    JPanel ok_panel;

    AboutActionListener aboutActionListener;

    AboutKeyPressedListener aboutKeyPressedListener;

    JTextArea textArea1;

    Font font1;

    public AboutDialog(String type) {
        aFrame = new JFrame("About");
        aFrame.setFocusTraversalPolicy(new focusPolicy());

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        font1 = new Font("Monospaced", Font.BOLD | Font.PLAIN, 12);

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);
        gbLayout = new GridBagLayout();
        container.setLayout(gbLayout);
        gbConstraints = new GridBagConstraints();
        gbConstraints.fill = GridBagConstraints.BOTH;

        closeButton = new JButton("Close");
        closeButton.setMnemonic(KeyEvent.VK_C);

        closeButton.getAccessibleContext().setAccessibleName("Close");
        closeButton.getAccessibleContext().setAccessibleDescription("Close");

        ok_panel = new JPanel();
        ok_panel.add(closeButton);

        if (type.equals("About")) {
            aFrame.setTitle("About");

            textArea1 = new JTextArea();
            textArea1.setBackground(aFrame.getBackground());
            textArea1.setEditable(false);
            textArea1.setWrapStyleWord(false);
            textArea1.setLineWrap(false);
            textArea1.setText("NOTE:\n\n" + "gdvsim version " + Intersection.IVERSN + "\n" + "gdvsim COPYRIGHT (C) 2004 by Rioux Engineering, Austin, Texas USA\n\n"
                    + "This program is free software; you can redistribute it and/or modify it\n" + "under the terms of the GNU General Public License as published by the Free\n"
                    + "Software Foundation; either version 2 of the License,\n" + "or (at your option) any later version.\n\n" + "This program is distributed in the hope that it will be useful\n"
                    + "but WITHOUT ANY WARRANTY; without even the implied warranty of\n" + "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the\n"
                    + "GNU General Public License for more details.\n\n" + "You should have received a copy of the GNU General Public License\n"
                    + "along with this program; if not, write to the Free Software\n" + "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA\n\n" + "END NOTE");

            textArea1.setFont(font1);

            gbConstraints.insets = new Insets(20, 20, 1, 20);
            addComponent(textArea1, 1, 0, 1, 1, 1);
            gbConstraints.insets = new Insets(1, 20, 10, 20);
            addComponent(ok_panel, 2, 0, 1, 1, 1);
            aFrame.pack();
        }
        else if (type.equals("FileMenuCommands")) {
            aFrame.setTitle("File Menu Commands");

            textArea1 = new JTextArea();
            textArea1.setBackground(aFrame.getBackground());
            textArea1.setEditable(false);
            textArea1.setWrapStyleWord(false);
            textArea1.setLineWrap(false);
            textArea1.setText("NOTE:\n\n" + "Activate                  - makes the currently open GDV and SIM files active for further processing without saving the files\n"
                    + "New                       - allows the user to create a new GDV file and SIM file\n" + "Open -> Permanent Library - allows the user to open a pre-defined GDV library file\n"
                    + "Open -> Existing File     - allows the user to open an existing GDV file and SIM file\n" + "Save                      - saves  the GDV and SIM data using existing file names\n"
                    + "Save As -> Data           - saves  the GDV and SIM data using user-specified file names\n"
                    + "Save As -> Printout       - writes the GDV and SIM data using user-specified file names for viewing and printing\n"
                    + "Save As -> DXF            - writes the GDV and SIM data using user-specified file names for importing into a graphics program\n"
                    + "                            DXF is the Drawing Exchange Format developed by Audodesk and used by MicroStation and AutoCAD\n"
                    + "Exit                      - exits gdvsim (the user must explicitly save the GDV and SIM data before exiting; see Save commands)\n\n" + "END NOTE");

            textArea1.setFont(font1);

            gbConstraints.insets = new Insets(20, 20, 1, 20);
            addComponent(textArea1, 1, 0, 1, 1, 1);
            gbConstraints.insets = new Insets(1, 20, 10, 20);
            addComponent(ok_panel, 2, 0, 1, 1, 1);
            aFrame.pack();
        }
        else if (type.equals("ViewMenuCommands")) {
            aFrame.setTitle("View Menu Commands");

            textArea1 = new JTextArea();
            textArea1.setBackground(aFrame.getBackground());
            textArea1.setEditable(false);
            textArea1.setWrapStyleWord(false);
            textArea1.setLineWrap(false);
            if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                textArea1.setText("NOTE:\n\n" + "Pan                                              - moves the center of the window up/down/left/right the selected portion of the screen size\n"
                        + "                                                   (the user can also press the left mouse button and drag the intersection to the desired location)\n"
                        + "Zoom                                             - zooms in/out the selected value about the center of the intersection\n"
                        + "                                                   (the user can also scroll the mouse wheel to zoom in/out to the desired size)\n"
                        + "Reset Pan and Zoom                               - resets pan and zoom to center the intersection at a scale where everything is visible\n"
                        + "View Detectors                                   - allows the user to toggle the displaying of the detector geometry\n"
                        + "View Lane Control                                - allows the user to toggle the displaying of the lane control symbology\n"
                        + "View Sight Distance Restrictions                 - allows the user to toggle the displaying of the sight distance restriction locations\n"
                        + "View Turn Movements                              - allows the user to toggle the displaying of the turn movement symbology\n"
                        + "View User Arcs                                   - allows the user to toggle the displaying of user-defined arcs\n"
                        + "View User Lines                                  - allows the user to toggle the displaying of user-defined lines\n"
                        + "Center Diamond Interchange -> Left  Intersection - moves the center of the diamond interchange left  intersection to the center of the window\n"
                        + "Center Diamond Interchange -> Right Intersection - moves the center of the diamond interchange right intersection to the center of the window\n\n" + "END NOTE");
            }
            else {
                textArea1.setText("NOTE:\n\n" + "Pan                              - moves the center of the window up/down/left/right the selected portion of the screen size\n"
                        + "                                   (the user can also press the left mouse button and drag the intersection to the desired location)\n"
                        + "Zoom                             - zooms in/out the selected value about the center of the intersection\n"
                        + "                                   (the user can also scroll the mouse wheel to zoom in/out to the desired size)\n"
                        + "Reset Pan and Zoom               - resets pan and zoom to center the intersection at a scale where everything is visible\n"
                        + "View Detectors                   - allows the user to toggle the displaying of the detector geometry\n"
                        + "View Lane Control                - allows the user to toggle the displaying of the lane control symbology\n"
                        + "View Sight Distance Restrictions - allows the user to toggle the displaying of the sight distance restriction locations\n"
                        + "View Turn Movements              - allows the user to toggle the displaying of the turn movement symbology\n"
                        + "View User Arcs                   - allows the user to toggle the displaying of user-defined arcs\n"
                        + "View User Lines                  - allows the user to toggle the displaying of user-defined lines\n"
                        + "Center Intersection              - moves the center of the standard intersection to the center of the window\n\n" + "END NOTE");
            }

            textArea1.setFont(font1);

            gbConstraints.insets = new Insets(20, 20, 1, 20);
            addComponent(textArea1, 1, 0, 1, 1, 1);
            gbConstraints.insets = new Insets(1, 20, 10, 20);
            addComponent(ok_panel, 2, 0, 1, 1, 1);
            aFrame.pack();
        }
        else if (type.equals("Standard Intersection")) {
            aFrame.setTitle("Standard Intersection");

            textArea1 = new JTextArea();
            textArea1.setBackground(aFrame.getBackground());
            textArea1.setEditable(false);
            textArea1.setWrapStyleWord(false);
            textArea1.setLineWrap(false);
            textArea1.setText("NOTE:\n\n" + "                                  Standard Intersection\n\n"
                    + "The geometry of a Standard Intersection is configured as a single, three- to six-leg, at-grade intersection.\n"
                    + "The leg centerline is the imaginary line between the inbound and outbound lanes, or the centerline of any median,\n"
                    + "or the leftmost lane edge for a leg having only one-way traffic.  The intersection center is a selected reference\n"
                    + "point in the intersection where two or more leg centerlines cross.  Leg numbers are not specifically designated by\n"
                    + "gdvsim.  Curb Returns are associated with a leg and the nearest counterclockwise leg.\n\n" + "END NOTE");

            textArea1.setFont(font1);

            gbConstraints.insets = new Insets(20, 20, 1, 20);
            addComponent(textArea1, 1, 0, 1, 1, 1);
            gbConstraints.insets = new Insets(1, 20, 10, 20);
            addComponent(ok_panel, 2, 0, 1, 1, 1);
            aFrame.pack();
        }
        else if (type.equals("Diamond Interchange")) {
            aFrame.setTitle("Diamond Interchange");

            textArea1 = new JTextArea();
            textArea1.setBackground(aFrame.getBackground());
            textArea1.setEditable(false);
            textArea1.setWrapStyleWord(false);
            textArea1.setLineWrap(false);
            textArea1.setText("NOTE:\n\n" + "                                           Diamond Interchange\n\n"
                    + "The geometry of the Diamond Interchange is configured as two adjacent, three-leg, at-grade intersections connected\n"
                    + "by a set of internal lanes.  The diamond interchange does not include the Freeway lanes that typically go over or\n"
                    + "under the internal lanes.  The intersections are designated as the Left Intersection and the Right Intersection with\n"
                    + "the centerline of the internal lanes oriented due East and West.  The centerline of the internal lanes goes from the\n"
                    + "Left Intersection center to the Right Intersection center.  Leg 1 is the Right Intersection North leg, Leg 2 is the\n"
                    + "Right Intersection East leg, Leg 3 is the Right Intersection South leg, Leg 4 is the Left Intersection South leg, Leg\n5 "
                    + "is the Left Intersection West leg, and Leg 6 is the Left Intersection North leg.  One or more of the 6 legs may be\n"
                    + "disabled by setting both the number of inbound lanes and the number of outbound lanes to zero for the disabled leg.\n"
                    + "A diamond interchange may be configured with either two-way frontage roads, one-way frontage roads without Free\n"
                    + "U-Turn lane(s), or one-way frontage roads with Free U-Turn lane(s).  Curb Return 1 is between Leg 1 and the internal\n"
                    + "lanes and between Leg 3 and the internal lanes, Curb Return 2 is between Leg 1 and Leg 2, Curb Return 3 is between\n"
                    + "Leg 2 and Leg 3, Curb Return 4 is between Leg 4 and the internal lanes and between Leg 6 and the internal lanes, Curb\n"
                    + "Return 5 is between Leg 4 and Leg 5, and Curb Return 6 is between Leg 5 and Leg 6.  The leg centerline is the imaginary\n"
                    + "line between the inbound and outbound lanes, or the centerline of any median, or the leftmost lane edge for a leg\n"
                    + "having only one-way traffic.  The intersection center is a selected reference point in the intersection where the\n"
                    + "centerline of the internal lanes and one or more leg centerlines cross.  A Free U-Turn lane directly connects Leg 3\n"
                    + "with Leg 4 and/or Leg 6 with Leg 1 in advance of the Left and Right Intersections.  The Free U-Turn geometry is\n"
                    + "automatically added by gdvsim and comprises 5 segments: (1) a single, exclusive, inbound lane of specified length\n"
                    + "on the median (left) side of Legs 3 and/or 6, (2) an exclusive, circular-arc, intersection path tangent to the\n"
                    + "centerline of segment 1, tangent to the centerline of segment 3, and changing direction by approximately 90 degrees,\n"
                    + "(3) a single, exclusive, internal lane of calculated length with traffic control at the end of this internal lane,\n"
                    + "(4) an exclusive, circular-arc, intersection path tangent to the centerline of segment 3, tangent to the centerline\n"
                    + "of segment 5, and changing direction by approximately 90 degrees, and (5) a single, exclusive, outbound lane of\n"
                    + "specified length on the median (left) side of Legs 4 and/or 1.  The width of the Free U-Turn lane is specified with\n" + "the same value for segments 1, 3, and 5.\n\n"
                    + "END NOTE");

            textArea1.setFont(font1);

            gbConstraints.insets = new Insets(20, 20, 1, 20);
            addComponent(textArea1, 1, 0, 1, 1, 1);
            gbConstraints.insets = new Insets(1, 20, 10, 20);
            addComponent(ok_panel, 2, 0, 1, 1, 1);
            aFrame.pack();
        }
        else if (type.equals("Specialkeyactions")) {
            aFrame.setTitle("Special key actions");

            textArea1 = new JTextArea();
            textArea1.setBackground(aFrame.getBackground());
            textArea1.setEditable(false);
            textArea1.setWrapStyleWord(false);
            textArea1.setLineWrap(false);

            textArea1.setText("NOTE:\n\n" + "Press F1 to get help for any item\n\n" +

                    "Button Actions\n" + "OK             - saves the data and closes the window\n" + "Apply          - saves the data but does not close the window\n"
                    + "Cancel         - discards any changes and closes the window\n\n" +

                    "List Items Special Key Actions\n" + "Tab            - go forward  to next     item\n" + "Shift-Tab      - go backward to previous item\n"
                    + "Up-Arrow       - move selection up   one in the list (stop at the top)    (do not open the list)\n"
                    + "Down-Arrow     - move selection down one in the list (stop at the bottom) (do not open the list)\n"
                    + "Alt-Up-Arrow   - if the list is not  open then open the list at the selected value else close the list\n"
                    + "Alt-Down-Arrow - if the list is not  open then open the list at the selected value else close the list\n" + "Enter          - if the list is open then close the list\n\n" +

                    "Radio Button Special Key Actions\n" + "Tab            - go forward  to next      item\n" + "Shift-Tab      - go backward to previous  item\n"
                    + "Up-Arrow       - go backward to previous  item\n" + "Down-Arrow     - go forward  to next      item\n" + "Space-Bar      - selects the value\n\n" +

                    "Check Box Special Key Actions\n" + "Tab            - go forward  to next     item\n" + "Shift-Tab      - go backward to previous item\n"
                    + "Space-Bar      - toggles the value\n\n\n" + "END NOTE");

            textArea1.setFont(font1);

            gbConstraints.insets = new Insets(20, 20, 1, 20);
            addComponent(textArea1, 1, 0, 1, 1, 1);
            gbConstraints.insets = new Insets(1, 20, 10, 20);
            addComponent(ok_panel, 2, 0, 1, 1, 1);
            aFrame.pack();

        }

        aboutActionListener = new AboutActionListener();
        closeButton.addActionListener(aboutActionListener);

        aboutKeyPressedListener = new AboutKeyPressedListener();
        closeButton.addKeyListener(aboutKeyPressedListener);

        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

    } // end of method AboutDialog

    void addComponent(Component c, int row, int column, int width, int height, double weightx) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbConstraints.weightx = weightx;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class AboutActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == closeButton) {
                aFrame.dispose();
            }
        }
    }

    class AboutKeyPressedListener extends KeyAdapter {

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

    class AboutKeyPressedDialogListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == closeButton) {
                    aFrame.dispose();
                }
            }
        }
    }

} // end of class AboutDialog