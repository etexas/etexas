package org.etexascode.gui;

/******************************************************************************/
/*                           TexasModelDialog.java                            */
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Calendar;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

public class TexasModelDialog extends JDialog {

    class ButtonActionListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == button_clear) {
                logTextArea.setText("");
            }
            else if (event.getSource() == button_SaveAs) {
                saveAsProcess();
            }
            else if (event.getSource() == button_done) {
                aFrame.dispose();
            }
            else if (event.getSource() == button_editFile) {
                new EditFileNameDialog();
            }
            else if (event.getSource() == button_viewFile) {
                viewFile();
            }
            else if (event.getSource() == button_ProjDir) {
                ProjectDirSelect();
            }
            else if (event.getSource() == button_ProjName) {
                ProjectNameSelect();
            }
            else if (event.getSource() == button_delfiles) {
                DELFILESAction();
            }
            else if (event.getSource() == button_disfit) {
                DISFITActionListener();
            }
            else if (event.getSource() == button_dispre) {
                DISPREAction(0);
            }
            else if (event.getSource() == button_dispro) {
                DISPROAction(0);
            }
            else if (event.getSource() == button_dvpro) {
                DVPROAction(0);
            }
            else if (event.getSource() == button_empro) {
                EMPROActionListener();
            }
            else if (event.getSource() == button_from80d) {
                FROM80DActionListener();
            }
            else if (event.getSource() == button_gdvconv) {
                GDVCONVAction(0);
            }
            else if (event.getSource() == button_gdvpro) {
                GDVPROAction(0);
            }
            else if (event.getSource() == button_gdvsim) {
                GDVSIMAction(0);
            }
            else if (event.getSource() == button_geoplot) {
                GEOPLOTAction(0);
            }
            else if (event.getSource() == button_geopro) {
                GEOPROAction(0);
            }
            else if (event.getSource() == button_reprun) {
                REPRUNActionListener();
            }
            else if (event.getSource() == button_reptol) {
                REPTOLActionListener();
            }
            else if (event.getSource() == button_staplotr) {
                STAPLOTAction(0, "r");
            }
            else if (event.getSource() == button_simconv) {
                SIMCONVAction(0);
            }
            else if (event.getSource() == button_simpro) {
                SIMPROAction(0);
            }
            else if (event.getSource() == button_dtsim) {
                DTSIMAction(0);
            }
            else if (event.getSource() == button_simsta) {
                SIMSTAAction(0);
            }
            else if (event.getSource() == button_ssam) {
                SSAMAction();
            }
            else if (event.getSource() == button_staplot1) {
                STAPLOTAction(0, "1");
            }
            else if (event.getSource() == button_to80d) {
                TO80DActionListener();
            }
            else if (event.getSource() == button_versions) {
                VERSIONSAction();
            }
        } // end of method actionPerformed
    } // end of class ButtonActionListener

    class ButtonKeyListener extends KeyAdapter {

        @Override
        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == button_clear) {
                    logTextArea.setText("");
                }
                else if (event.getSource() == button_SaveAs) {
                    saveAsProcess();
                }
                else if (event.getSource() == button_done) {
                    aFrame.dispose();
                }
                else if (event.getSource() == button_editFile) {
                    new EditFileNameDialog();
                }
                else if (event.getSource() == button_viewFile) {
                    viewFile();
                }
                else if (event.getSource() == button_ProjDir) {
                    ProjectDirSelect();
                }
                else if (event.getSource() == button_ProjName) {
                    ProjectNameSelect();
                }
                else if (event.getSource() == button_delfiles) {
                    DELFILESAction();
                }
                else if (event.getSource() == button_disfit) {
                    DISFITActionListener();
                }
                else if (event.getSource() == button_dispre) {
                    DISPREAction(0);
                }
                else if (event.getSource() == button_dispro) {
                    DISPROAction(0);
                }
                else if (event.getSource() == button_dvpro) {
                    DVPROAction(0);
                }
                else if (event.getSource() == button_empro) {
                    EMPROActionListener();
                }
                else if (event.getSource() == button_from80d) {
                    FROM80DActionListener();
                }
                else if (event.getSource() == button_gdvconv) {
                    GDVCONVAction(0);
                }
                else if (event.getSource() == button_gdvpro) {
                    GDVPROAction(0);
                }
                else if (event.getSource() == button_gdvsim) {
                    GDVSIMAction(0);
                }
                else if (event.getSource() == button_geoplot) {
                    GEOPLOTAction(0);
                }
                else if (event.getSource() == button_geopro) {
                    GEOPROAction(0);
                }
                else if (event.getSource() == button_reprun) {
                    REPRUNActionListener();
                }
                else if (event.getSource() == button_reptol) {
                    REPTOLActionListener();
                }
                else if (event.getSource() == button_staplotr) {
                    STAPLOTAction(0, "r");
                }
                else if (event.getSource() == button_simconv) {
                    SIMCONVAction(0);
                }
                else if (event.getSource() == button_simpro) {
                    SIMPROAction(0);
                }
                else if (event.getSource() == button_dtsim) {
                    DTSIMAction(0);
                }
                else if (event.getSource() == button_simsta) {
                    SIMSTAAction(0);
                }
                else if (event.getSource() == button_ssam) {
                    SSAMAction();
                }
                else if (event.getSource() == button_staplot1) {
                    STAPLOTAction(0, "1");
                }
                else if (event.getSource() == button_to80d) {
                    TO80DActionListener();
                }
                else if (event.getSource() == button_versions) {
                    VERSIONSAction();
                }
            }
        } // end of method keyPressed
    } // end of class ButtonKeyListener

    class HelpListener extends KeyAdapter {

        @Override
        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == button_done) {
                    new HelpDialog(true, button_done.getText() + " button", "The " + button_done.getText() + " button closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cb_ignoreWarn) {
                    new HelpDialog(true, cb_ignoreWarn.getText() + " check box", "The " + cb_ignoreWarn.getText() + " check box ignores the warning windows.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_SaveAs) {
                    new HelpDialog(true, button_SaveAs.getText() + " button", "The " + button_SaveAs.getText() + " button saves the log data.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_clear) {
                    new HelpDialog(true, button_clear.getText() + " button", "The " + button_clear.getText() + " button clears the log box.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_editFile) {
                    new HelpDialog(true, button_editFile.getText() + " button", "The " + button_editFile.getText() + " button opens the edit file name window", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_viewFile) {
                    new HelpDialog(true, button_viewFile.getText() + " button", "The " + button_viewFile.getText() + " button opens the view file window", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_ProjDir) {
                    new HelpDialog(true, button_ProjDir.getText() + " button", "The " + button_ProjDir.getText() + " button selects a directory", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_ProjName) {
                    new HelpDialog(true, button_ProjName.getText() + " button", "The " + button_ProjName.getText() + " button selects a existing project name", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == text_ProjName) {
                    new HelpDialog(true, "Project Name keyin field", "Allows the user to enter a Project Name", " ", text_ProjName.getText().trim(), " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == logTextArea) {
                    new HelpDialog(true, "Log", "Log", " ", logTextArea.getText().trim(), " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_gdvsim) {
                    new HelpDialog(true, "gdvsim button", "Create or Edit the Geometry & Driver Vehicle Data and the Simulation Data", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_gdvpro) {
                    new HelpDialog(true, "gdvpro button", "Process Geometry & Driver Vehicle Data", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_geoplot) {
                    new HelpDialog(true, "geoplot button", "Display/Plot the intersection geometry and vehicle paths", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_simpro) {
                    new HelpDialog(true, "simpro button", "Executes the actual simulation using specifications from files created by the gdvsim and gdvpro commands", " ", " ", " ", " ", " ", " ",
                            " ");
                }
                else if (event.getSource() == button_dtsim) {
                    new HelpDialog(true, "dtsim button", "Executes a DT simulation using specifications from files created by the gdvsim and gdvpro commands", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_dispro) {
                    new HelpDialog(true, "dispro button", "View the animated graphics", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_reptol) {
                    new HelpDialog(true, "reptol button", "Perform replicate runs until the user-specified tolerance is achieved", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_staplotr) {
                    new HelpDialog(true, "staplot button", "Display/Plot the statistics from all replicate runs from reptol/reprun", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_delfiles) {
                    new HelpDialog(true, "delfiles button", "Delete all project files except GDVDATA and SIMDATA", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_disfit) {
                    new HelpDialog(true, "disfit button", "Process Headway Distribution Data", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_dispre) {
                    new HelpDialog(true, "dispre button", "Prepare an animation graphics file for display", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_dvpro) {
                    new HelpDialog(true, "dvpro button", "Process Driver Vehicle Data", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_geopro) {
                    new HelpDialog(true, "geopro button", "Process Geometry Data", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_reprun) {
                    new HelpDialog(true, "reprun button", "Perform a user-specified number of replicate runs of the simulation", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_simsta) {
                    new HelpDialog(true, "simsta button", "Process statistics from replicate runs of the simulation", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_ssam) {
                    new HelpDialog(true, "ssam button", "Start the Surrogate Safety Assessment Methodology program", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_staplot1) {
                    new HelpDialog(true, "staplot button", "Display/Plot the statistics from 1 run of simpro", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_empro) {
                    new HelpDialog(true, "empro button", "Process Emissions Data", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_from80d) {
                    new HelpDialog(true, "from80d button", "Convert an 80-character fixed length record file to a standard text file", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_gdvconv) {
                    new HelpDialog(true, "gdvconv button", "Convert a GDVDATA file to a GDV file", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_simconv) {
                    new HelpDialog(true, "simconv button", "Convert a SIMDATA file to a SIM file", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_to80d) {
                    new HelpDialog(true, "to80d button", "Convert a standard text file to an 80-character fixed length record file", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == button_versions) {
                    new HelpDialog(true, "versions button", "Display the version numbers for all TEXAS Model applications", " ", " ", " ", " ", " ", " ", " ");
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        } // end of method keyPressed
    } // end of class HelpListener

    class TextKeyListener extends KeyAdapter {

        @Override
        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_A || event.getKeyCode() == KeyEvent.VK_B) {
                logTextArea.setText("");
            }
        } // end of method keyPressed
    } // end of class TextKeyListener

    public static final int MAXWRN = 25;

    public static final int MIN_MAX_NUM_REP_RUNS = 10;

    public static final String IVERSN = "V7.0";

    public static void addLineToLog(String line) {
        logTextArea.append("\n" + line);
    } // end of method addLineToLog

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title, label_log;

    JTextField text_ProjDir, text_ProjName;

    static TextArea logTextArea;

    JCheckBox cb_ignoreWarn;

    JButton button_done, button_clear, button_editFile, button_viewFile, button_ProjDir, button_ProjName, button_SaveAs, button_gdvsim, button_gdvpro, button_geoplot, button_simpro, button_dtsim,
            button_dispro, button_reptol, button_staplotr, button_delfiles, button_disfit, button_dispre, button_dvpro, button_geopro, button_reprun, button_simsta, button_ssam, button_staplot1,
            button_empro, button_from80d, button_gdvconv, button_simconv, button_to80d, button_versions;

    JTabbedPane tabbedPane;

    JPanel tabPanel_1;

    JPanel tabPanel_2;

    JPanel tabPanel_3;

    GridBagLayout layout_tabPanel_1;

    GridBagLayout layout_tabPanel_2;

    GridBagLayout layout_tabPanel_3;

    GridBagConstraints cnstr_tabPanel_1;

    GridBagConstraints cnstr_tabPanel_2;

    GridBagConstraints cnstr_tabPanel_3;

    Font font1, font2;

    HelpListener helpListener;

    ButtonActionListener buttonActionListener;

    ButtonKeyListener buttonKeyListener;

    TextKeyListener textKeyListener;

    Calendar currentDateTime;

    Process process = null;

    public TexasModelDialog() {
        aFrame = new JFrame();

        String title = "TEXAS Model For Intersection Traffic Version - " + IVERSN;

        aFrame.setTitle(title);
        System.out.println(title);

        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        container.setLayout(gbLayout);
        gbConstraints.fill = GridBagConstraints.BOTH;

        font1 = new Font("TimesRoman", Font.PLAIN, 18);
        font2 = new Font("Monospaced", Font.PLAIN, 12);

        label_title = new JLabel(title);
        label_title.setFont(font1);

        button_done = new JButton("Done");
        button_clear = new JButton("Clear Log");
        button_editFile = new JButton("Edit Standard TEXAS Model File Names");
        button_viewFile = new JButton("View Standard TEXAS Model File");
        button_ProjDir = new JButton("Select Project Directory");
        button_ProjName = new JButton("Select Project Name");
        button_SaveAs = new JButton("Save Log As");

        button_gdvsim = new JButton("gdvsim:   Create or Edit the Geometry & Driver Vehicle Data and the Simulation Data                               ");
        button_gdvpro = new JButton("gdvpro:   Process Geometry & Driver Vehicle Data                                                                  ");
        button_geoplot = new JButton("geoplot:  Display/Plot the intersection geometry and vehicle paths                                                ");
        button_simpro = new JButton("simpro:   Executes the actual simulation using specifications from files created by the gdvsim and gdvpro commands");
        button_dtsim = new JButton("dtsim:    Executes a DT simulation using specifications from files created by the gdvsim and gdvpro commands      ");
        button_dispro = new JButton("dispro:   View the animated graphics                                                                              ");
        button_reptol = new JButton("reptol:   Perform replicate runs until the user-specified tolerance is achieved                                   ");
        button_staplotr = new JButton("staplot:  Display/Plot the statistics from all replicate runs from reptol/reprun                                  ");
        button_delfiles = new JButton("delfiles: Delete all project files except GDVDATA and SIMDATA                                                     ");

        button_disfit = new JButton("disfit:   Process Headway Distribution Data                                                                       ");
        button_dispre = new JButton("dispre:   Prepare an animation graphics file for display                                                          ");
        button_dvpro = new JButton("dvpro:    Process Driver Vehicle Data                                                                             ");
        button_geopro = new JButton("geopro:   Process Geometry Data                                                                                   ");
        button_reprun = new JButton("reprun:   Perform a user-specified number of replicate runs of the simulation                                     ");
        button_simsta = new JButton("simsta:   Process statistics from replicate runs of the simulation                                                ");
        button_ssam = new JButton("ssam:     Start the Surrogate Safety Assessment Methodology program                                               ");
        button_staplot1 = new JButton("staplot:  Display/Plot the statistics from 1 run of simpro                                                        ");

        button_empro = new JButton("empro:    Process Emissions Data                                                                                  ");
        button_from80d = new JButton("from80d:  Convert an 80-character fixed length record file to a standard text file                                ");
        button_gdvconv = new JButton("gdvconv:  Convert a GDVDATA file to a GDV file                                                                    ");
        button_simconv = new JButton("simconv:  Convert a SIMDATA file to a SIM file                                                                    ");
        button_to80d = new JButton("to80d:    Convert a standard text file to an 80-character fixed length record file                                ");
        button_versions = new JButton("versions: Display the version numbers for all TEXAS Model applications                                            ");

        cb_ignoreWarn = new JCheckBox("Ignore Warning Messages");

        button_gdvsim.setFont(font2);
        button_gdvpro.setFont(font2);
        button_geoplot.setFont(font2);
        button_simpro.setFont(font2);
        button_dtsim.setFont(font2);
        button_dispro.setFont(font2);
        button_reptol.setFont(font2);
        button_staplotr.setFont(font2);
        button_delfiles.setFont(font2);
        button_disfit.setFont(font2);
        button_dispre.setFont(font2);
        button_dvpro.setFont(font2);
        button_geopro.setFont(font2);
        button_reprun.setFont(font2);
        button_simsta.setFont(font2);
        button_ssam.setFont(font2);
        button_staplot1.setFont(font2);
        button_empro.setFont(font2);
        button_from80d.setFont(font2);
        button_gdvconv.setFont(font2);
        button_simconv.setFont(font2);
        button_to80d.setFont(font2);
        button_versions.setFont(font2);

        text_ProjDir = new JTextField();
        text_ProjDir.setEnabled(false);
        text_ProjDir.setDisabledTextColor(Color.BLACK);
        text_ProjName = new JTextField();

        JPanel panel_title;
        panel_title = new JPanel();
        panel_title.add(label_title);

        JPanel panel_done;
        panel_done = new JPanel();
        panel_done.add(button_done);
        panel_done.add(button_clear);

        tabPanel_1 = new JPanel();
        tabPanel_2 = new JPanel();
        tabPanel_3 = new JPanel();

        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main Commands", tabPanel_1);
        tabbedPane.addTab("Other Commands", tabPanel_2);
        tabbedPane.addTab("Advanced Commands", tabPanel_3);

        layout_tabPanel_1 = new GridBagLayout();
        layout_tabPanel_2 = new GridBagLayout();
        layout_tabPanel_3 = new GridBagLayout();

        cnstr_tabPanel_1 = new GridBagConstraints();
        cnstr_tabPanel_2 = new GridBagConstraints();
        cnstr_tabPanel_3 = new GridBagConstraints();

        tabPanel_1.setLayout(layout_tabPanel_1);
        tabPanel_2.setLayout(layout_tabPanel_2);
        tabPanel_3.setLayout(layout_tabPanel_3);

        cnstr_tabPanel_1.fill = GridBagConstraints.BOTH;
        cnstr_tabPanel_2.fill = GridBagConstraints.BOTH;
        cnstr_tabPanel_3.fill = GridBagConstraints.BOTH;

        label_log = new JLabel("Log:        ");

        JPanel panel_log;
        panel_log = new JPanel();
        panel_log.add(label_log);
        panel_log.add(button_SaveAs);

        logTextArea = new TextArea("", 12, 20, TextArea.SCROLLBARS_BOTH);
        logTextArea.setFont(font2);

        button_done.setMnemonic(KeyEvent.VK_D);
        button_clear.setMnemonic(KeyEvent.VK_C);
        button_editFile.setMnemonic(KeyEvent.VK_F);
        button_viewFile.setMnemonic(KeyEvent.VK_V);
        button_ProjDir.setMnemonic(KeyEvent.VK_P);
        button_ProjName.setMnemonic(KeyEvent.VK_E);
        button_SaveAs.setMnemonic(KeyEvent.VK_A);

        button_gdvsim.setMnemonic(KeyEvent.VK_G);
        button_gdvpro.setMnemonic(KeyEvent.VK_V);
        button_geoplot.setMnemonic(KeyEvent.VK_O);
        button_simpro.setMnemonic(KeyEvent.VK_M);
        button_dtsim.setMnemonic(KeyEvent.VK_D);
        button_dispro.setMnemonic(KeyEvent.VK_I);
        button_reptol.setMnemonic(KeyEvent.VK_R);
        button_staplotr.setMnemonic(KeyEvent.VK_S);
        button_delfiles.setMnemonic(KeyEvent.VK_L);

        button_disfit.setMnemonic(KeyEvent.VK_T);
        button_dispre.setMnemonic(KeyEvent.VK_R);
        button_dvpro.setMnemonic(KeyEvent.VK_V);
        button_geopro.setMnemonic(KeyEvent.VK_G);
        button_reprun.setMnemonic(KeyEvent.VK_U);
        button_simsta.setMnemonic(KeyEvent.VK_A);
        button_ssam.setMnemonic(KeyEvent.VK_M);
        button_staplot1.setMnemonic(KeyEvent.VK_S);

        button_empro.setMnemonic(KeyEvent.VK_E);
        button_from80d.setMnemonic(KeyEvent.VK_M);
        button_gdvconv.setMnemonic(KeyEvent.VK_G);
        button_simconv.setMnemonic(KeyEvent.VK_I);
        button_to80d.setMnemonic(KeyEvent.VK_T);
        button_versions.setMnemonic(KeyEvent.VK_V);

        helpListener = new HelpListener();
        buttonActionListener = new ButtonActionListener();
        buttonKeyListener = new ButtonKeyListener();
        textKeyListener = new TextKeyListener();

        logTextArea.addKeyListener(textKeyListener);

        button_editFile.addKeyListener(helpListener);
        button_viewFile.addKeyListener(helpListener);
        button_ProjDir.addKeyListener(helpListener);
        button_ProjName.addKeyListener(helpListener);
        text_ProjDir.addKeyListener(helpListener);
        text_ProjName.addKeyListener(helpListener);
        button_SaveAs.addKeyListener(helpListener);
        cb_ignoreWarn.addKeyListener(helpListener);
        button_done.addKeyListener(helpListener);
        button_clear.addKeyListener(helpListener);
        button_gdvsim.addKeyListener(helpListener);
        button_gdvpro.addKeyListener(helpListener);
        button_geoplot.addKeyListener(helpListener);
        button_simpro.addKeyListener(helpListener);
        button_dtsim.addKeyListener(helpListener);
        button_dispro.addKeyListener(helpListener);
        button_reptol.addKeyListener(helpListener);
        button_staplotr.addKeyListener(helpListener);
        button_delfiles.addKeyListener(helpListener);
        button_disfit.addKeyListener(helpListener);
        button_dispre.addKeyListener(helpListener);
        button_dvpro.addKeyListener(helpListener);
        button_geopro.addKeyListener(helpListener);
        button_reprun.addKeyListener(helpListener);
        button_simsta.addKeyListener(helpListener);
        button_ssam.addKeyListener(helpListener);
        button_staplot1.addKeyListener(helpListener);
        button_empro.addKeyListener(helpListener);
        button_from80d.addKeyListener(helpListener);
        button_gdvconv.addKeyListener(helpListener);
        button_simconv.addKeyListener(helpListener);
        button_to80d.addKeyListener(helpListener);
        button_versions.addKeyListener(helpListener);

        button_editFile.addActionListener(buttonActionListener);
        button_viewFile.addActionListener(buttonActionListener);
        button_ProjDir.addActionListener(buttonActionListener);
        button_ProjName.addActionListener(buttonActionListener);
        button_SaveAs.addActionListener(buttonActionListener);
        button_done.addActionListener(buttonActionListener);
        button_clear.addActionListener(buttonActionListener);
        button_gdvsim.addActionListener(buttonActionListener);
        button_gdvpro.addActionListener(buttonActionListener);
        button_geoplot.addActionListener(buttonActionListener);
        button_simpro.addActionListener(buttonActionListener);
        button_dtsim.addActionListener(buttonActionListener);
        button_dispro.addActionListener(buttonActionListener);
        button_reptol.addActionListener(buttonActionListener);
        button_staplotr.addActionListener(buttonActionListener);
        button_delfiles.addActionListener(buttonActionListener);
        button_disfit.addActionListener(buttonActionListener);
        button_dispre.addActionListener(buttonActionListener);
        button_dvpro.addActionListener(buttonActionListener);
        button_geopro.addActionListener(buttonActionListener);
        button_reprun.addActionListener(buttonActionListener);
        button_simsta.addActionListener(buttonActionListener);
        button_ssam.addActionListener(buttonActionListener);
        button_staplot1.addActionListener(buttonActionListener);
        button_empro.addActionListener(buttonActionListener);
        button_from80d.addActionListener(buttonActionListener);
        button_gdvconv.addActionListener(buttonActionListener);
        button_simconv.addActionListener(buttonActionListener);
        button_to80d.addActionListener(buttonActionListener);
        button_versions.addActionListener(buttonActionListener);

        button_editFile.addKeyListener(buttonKeyListener);
        button_viewFile.addKeyListener(buttonKeyListener);
        button_ProjDir.addKeyListener(buttonKeyListener);
        button_ProjName.addKeyListener(buttonKeyListener);
        button_SaveAs.addKeyListener(buttonKeyListener);
        button_done.addKeyListener(buttonKeyListener);
        button_clear.addKeyListener(buttonKeyListener);
        button_gdvsim.addKeyListener(buttonKeyListener);
        button_gdvpro.addKeyListener(buttonKeyListener);
        button_geoplot.addKeyListener(buttonKeyListener);
        button_simpro.addKeyListener(buttonKeyListener);
        button_dtsim.addKeyListener(buttonKeyListener);
        button_dispro.addKeyListener(buttonKeyListener);
        button_reptol.addKeyListener(buttonKeyListener);
        button_staplotr.addKeyListener(buttonKeyListener);
        button_delfiles.addKeyListener(buttonKeyListener);
        button_disfit.addKeyListener(buttonKeyListener);
        button_dispre.addKeyListener(buttonKeyListener);
        button_dvpro.addKeyListener(buttonKeyListener);
        button_geopro.addKeyListener(buttonKeyListener);
        button_reprun.addKeyListener(buttonKeyListener);
        button_simsta.addKeyListener(buttonKeyListener);
        button_ssam.addKeyListener(buttonKeyListener);
        button_staplot1.addKeyListener(buttonKeyListener);
        button_empro.addKeyListener(buttonKeyListener);
        button_from80d.addKeyListener(buttonKeyListener);
        button_gdvconv.addKeyListener(buttonKeyListener);
        button_simconv.addKeyListener(buttonKeyListener);
        button_to80d.addKeyListener(buttonKeyListener);
        button_versions.addKeyListener(buttonKeyListener);

        logTextArea.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(KeyEvent event) {
                if (event.getKeyCode() == KeyEvent.VK_TAB && event.getModifiers() == InputEvent.SHIFT_MASK) {
                    logTextArea.transferFocusBackward();
                }
                else if (event.getKeyCode() == KeyEvent.VK_TAB) {
                    logTextArea.transferFocus();
                }

            }
        });

        button_editFile.getAccessibleContext().setAccessibleDescription(button_editFile.getText());
        button_viewFile.getAccessibleContext().setAccessibleDescription(button_viewFile.getText());
        button_ProjDir.getAccessibleContext().setAccessibleDescription(button_ProjDir.getText());
        button_ProjName.getAccessibleContext().setAccessibleDescription(button_ProjName.getText());
        text_ProjDir.getAccessibleContext().setAccessibleDescription(button_ProjDir.getText());
        text_ProjName.getAccessibleContext().setAccessibleDescription(button_ProjName.getText());
        button_SaveAs.getAccessibleContext().setAccessibleDescription(button_SaveAs.getText());
        button_done.getAccessibleContext().setAccessibleDescription(button_done.getText());
        button_clear.getAccessibleContext().setAccessibleDescription(button_clear.getText());
        logTextArea.getAccessibleContext().setAccessibleDescription("Log");

        button_editFile.getAccessibleContext().setAccessibleName(button_editFile.getText());
        button_viewFile.getAccessibleContext().setAccessibleName(button_viewFile.getText());
        button_ProjDir.getAccessibleContext().setAccessibleName(button_ProjDir.getText());
        button_ProjName.getAccessibleContext().setAccessibleName(button_ProjName.getText());
        text_ProjDir.getAccessibleContext().setAccessibleName(button_ProjDir.getText());
        text_ProjName.getAccessibleContext().setAccessibleName(button_ProjName.getText());
        button_SaveAs.getAccessibleContext().setAccessibleName(button_SaveAs.getText());
        button_done.getAccessibleContext().setAccessibleName(button_done.getText());
        button_clear.getAccessibleContext().setAccessibleName(button_clear.getText());
        logTextArea.getAccessibleContext().setAccessibleName("Log");

        button_gdvsim.getAccessibleContext().setAccessibleDescription("Create or Edit the Geometry & Driver Vehicle Data and the Simulation Data                               ");
        button_gdvpro.getAccessibleContext().setAccessibleDescription("Process Geometry & Driver Vehicle Data                                                                  ");
        button_geoplot.getAccessibleContext().setAccessibleDescription("Display/Plot the intersection geometry and vehicle paths                                                ");
        button_simpro.getAccessibleContext().setAccessibleDescription("Executes the actual simulation using specifications from files created by the gdvsim and gdvpro commands");
        button_dtsim.getAccessibleContext().setAccessibleDescription("Executes a DT simulation using specifications from files created by the gdvsim and gdvpro commands      ");
        button_dispro.getAccessibleContext().setAccessibleDescription("View the animated graphics                                                                              ");
        button_reptol.getAccessibleContext().setAccessibleDescription("Perform replicate runs until the user-specified tolerance is achieved                                   ");
        button_staplotr.getAccessibleContext().setAccessibleDescription("Display/Plot the statistics from all replicate runs from reptol/reprun                                  ");
        button_delfiles.getAccessibleContext().setAccessibleDescription("Delete all project files except GDVDATA and SIMDATA                                                     ");

        button_disfit.getAccessibleContext().setAccessibleDescription("Process Headway Distribution Data                                                                       ");
        button_dispre.getAccessibleContext().setAccessibleDescription("Prepare an animation graphics file for display                                                          ");
        button_dvpro.getAccessibleContext().setAccessibleDescription("Process Driver Vehicle Data                                                                             ");
        button_geopro.getAccessibleContext().setAccessibleDescription("Process Geometry Data                                                                                   ");
        button_reprun.getAccessibleContext().setAccessibleDescription("Perform a user-specified number of replicate runs of the simulation                                     ");
        button_simsta.getAccessibleContext().setAccessibleDescription("Process statistics from replicate runs of the simulation                                                ");
        button_ssam.getAccessibleContext().setAccessibleDescription("Start the Surrogate Safety Assessment Methodology program                                               ");
        button_staplot1.getAccessibleContext().setAccessibleDescription("Display/Plot the statistics from 1 run of simpro                                                        ");

        button_empro.getAccessibleContext().setAccessibleDescription("Process Emissions Data                                                                                  ");
        button_from80d.getAccessibleContext().setAccessibleDescription("Convert an 80-character fixed length record file to a standard text file                                ");
        button_gdvconv.getAccessibleContext().setAccessibleDescription("Convert a GDVDATA file to a GDV file                                                                    ");
        button_simconv.getAccessibleContext().setAccessibleDescription("Convert a SIMDATA file to a SIM file                                                                    ");
        button_to80d.getAccessibleContext().setAccessibleDescription("Convert a standard text file to an 80-character fixed length record file                                ");
        button_versions.getAccessibleContext().setAccessibleDescription("Display the version numbers for all TEXAS Model applications                                            ");

        button_gdvsim.getAccessibleContext().setAccessibleName("gdvsim");
        button_gdvpro.getAccessibleContext().setAccessibleName("gdvpro");
        button_geoplot.getAccessibleContext().setAccessibleName("geoplot");
        button_simpro.getAccessibleContext().setAccessibleName("simpro");
        button_dtsim.getAccessibleContext().setAccessibleName("dtsim");
        button_dispro.getAccessibleContext().setAccessibleName("dispro");
        button_reptol.getAccessibleContext().setAccessibleName("reptol");
        button_staplotr.getAccessibleContext().setAccessibleName("staplot");
        button_delfiles.getAccessibleContext().setAccessibleName("delfiles");

        button_disfit.getAccessibleContext().setAccessibleName("disfit");
        button_dispre.getAccessibleContext().setAccessibleName("dispre");
        button_dvpro.getAccessibleContext().setAccessibleName("dvpro");
        button_geopro.getAccessibleContext().setAccessibleName("geopro");
        button_reprun.getAccessibleContext().setAccessibleName("reprun");
        button_simsta.getAccessibleContext().setAccessibleName("simsta");
        button_ssam.getAccessibleContext().setAccessibleName("ssam");
        button_staplot1.getAccessibleContext().setAccessibleName("staplot");

        button_empro.getAccessibleContext().setAccessibleName("empro");
        button_from80d.getAccessibleContext().setAccessibleName("from80d");
        button_gdvconv.getAccessibleContext().setAccessibleName("gdvconv");
        button_simconv.getAccessibleContext().setAccessibleName("simconv");
        button_to80d.getAccessibleContext().setAccessibleName("to80d");
        button_versions.getAccessibleContext().setAccessibleName("versions");

        int iRow = 0;
        int iRow_tab1 = 0;
        int iRow_tab2 = 0;
        int iRow_tab3 = 0;

        cnstr_tabPanel_1.insets = new Insets(10, 10, 1, 2);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_gdvsim, iRow_tab1++, 0, 1, 1, 0);
        cnstr_tabPanel_1.insets = new Insets(1, 10, 1, 2);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_gdvpro, iRow_tab1++, 0, 1, 1, 0);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_geoplot, iRow_tab1++, 0, 1, 1, 0);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_simpro, iRow_tab1++, 0, 1, 1, 0);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_dtsim, iRow_tab1++, 0, 1, 1, 0);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_dispro, iRow_tab1++, 0, 1, 1, 0);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_reptol, iRow_tab1++, 0, 1, 1, 0);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_staplotr, iRow_tab1++, 0, 1, 1, 0);
        cnstr_tabPanel_1.insets = new Insets(1, 10, 10, 2);
        addComponent(tabPanel_1, layout_tabPanel_1, cnstr_tabPanel_1, button_delfiles, iRow_tab1++, 0, 1, 1, 0);

        cnstr_tabPanel_2.insets = new Insets(1, 2, 1, 2);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_disfit, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_dispre, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_dvpro, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_geopro, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_reprun, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_simsta, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_ssam, iRow_tab2++, 0, 1, 1, 0);
        addComponent(tabPanel_2, layout_tabPanel_2, cnstr_tabPanel_2, button_staplot1, iRow_tab2++, 0, 1, 1, 0);

        cnstr_tabPanel_3.insets = new Insets(1, 2, 1, 2);
        addComponent(tabPanel_3, layout_tabPanel_3, cnstr_tabPanel_3, button_empro, iRow_tab3++, 0, 1, 1, 0);
        addComponent(tabPanel_3, layout_tabPanel_3, cnstr_tabPanel_3, button_from80d, iRow_tab3++, 0, 1, 1, 0);
        addComponent(tabPanel_3, layout_tabPanel_3, cnstr_tabPanel_3, button_gdvconv, iRow_tab3++, 0, 1, 1, 0);
        addComponent(tabPanel_3, layout_tabPanel_3, cnstr_tabPanel_3, button_simconv, iRow_tab3++, 0, 1, 1, 0);
        addComponent(tabPanel_3, layout_tabPanel_3, cnstr_tabPanel_3, button_to80d, iRow_tab3++, 0, 1, 1, 0);
        addComponent(tabPanel_3, layout_tabPanel_3, cnstr_tabPanel_3, button_versions, iRow_tab3++, 0, 1, 1, 0);

        gbConstraints.insets = new Insets(1, 10, 10, 10);
        addComponent(container, gbLayout, gbConstraints, panel_title, iRow++, 0, 2, 1, 0);

        gbConstraints.insets = new Insets(1, 10, 1, 10);
        addComponent(container, gbLayout, gbConstraints, button_editFile, iRow, 0, 1, 1, 0);
        gbConstraints.insets = new Insets(1, 1, 5, 10);
        addComponent(container, gbLayout, gbConstraints, button_viewFile, iRow++, 1, 1, 1, 0);
        gbConstraints.insets = new Insets(1, 10, 1, 10);
        addComponent(container, gbLayout, gbConstraints, button_ProjDir, iRow, 0, 1, 1, 0);

        gbConstraints.insets = new Insets(1, 1, 5, 10);
        addComponent(container, gbLayout, gbConstraints, text_ProjDir, iRow++, 1, 2, 1, 0);

        gbConstraints.insets = new Insets(1, 10, 1, 10);
        addComponent(container, gbLayout, gbConstraints, button_ProjName, iRow, 0, 1, 1, 0);

        gbConstraints.insets = new Insets(1, 1, 5, 10);
        addComponent(container, gbLayout, gbConstraints, text_ProjName, iRow++, 1, 2, 1, 0);

        gbConstraints.insets = new Insets(1, 10, 1, 10);
        addComponent(container, gbLayout, gbConstraints, tabbedPane, iRow++, 0, 2, 1, 0);
        addComponent(container, gbLayout, gbConstraints, panel_log, iRow, 0, 1, 1, 0);
        addComponent(container, gbLayout, gbConstraints, cb_ignoreWarn, iRow++, 1, 1, 1, 0);
        addComponent(container, gbLayout, gbConstraints, logTextArea, iRow++, 0, 2, 1, 0);
        addComponent(container, gbLayout, gbConstraints, panel_done, iRow++, 0, 2, 1, 0);

        aFrame.setSize(950, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        aFrame.pack();

        // set default TEXAS Model directory
        File defaultTexasDirectoryFile;
        String defaultTexasDirectoryName = "";
        try {
            defaultTexasDirectoryName = new File(".").getCanonicalPath();
            if (defaultTexasDirectoryName.trim().length() > 0) {
                defaultTexasDirectoryFile = new File(defaultTexasDirectoryName);
                if (defaultTexasDirectoryFile != null) {
                    if (defaultTexasDirectoryFile.getCanonicalPath().trim().length() > 0) {
                        texas.ProjectDirectory = defaultTexasDirectoryFile.getCanonicalPath();
                        text_ProjDir.setText(texas.ProjectDirectory);
                    }
                }
            }
        } // end of try
        catch (Exception e) {
            System.out.println("set default TEXAS Model directory exception error");
            addLineToLog("set default TEXAS Model directory exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
        }

        currentDateTime = Calendar.getInstance();

        WindowListener windowCloseRedX = new WindowAdapter() {

            @Override
            public synchronized void windowClosing(WindowEvent e) {
                aFrame.dispose();
                System.exit(0);
            }
        };

        aFrame.addWindowListener(windowCloseRedX);

    } // end of class initializer TexasModelDialog

    void addComponent(Container container, GridBagLayout gbLayout, GridBagConstraints gbConstraints, Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;

        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    void addComponent(JPanel JP, GridBagLayout gbLayout, GridBagConstraints gbConstraints, Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;

        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        JP.add(c);
    } // end of method addComponent

    String addReplicateName(String fileName, String replicateName) {
        int i;

        i = fileName.lastIndexOf('.');
        if (i == -1) {
            return fileName + "." + replicateName;
        }
        else {
            return fileName.substring(0, i) + "_" + replicateName + fileName.substring(i);
        }
    } // end of method addReplicateName

    void deleteErrorTxtFileInProjectDirectory() {
        deleteFileInProjectDirectory("error.txt", false);
    } // end of method deleteErrorTxtFileInProjectDirectory

    void deleteFileInProjectDirectory(String fileName, boolean log) {
        File fileFile = new File(fileNameWithProjectDirectory(fileName));

        if (fileFile.exists()) {
            if (log) {
                try {
                    addLineToLog("********** deleting file " + fileFile.getCanonicalPath().trim());
                }
                catch (Exception e) {
                    System.err.println("deleteFileInProjectDirectory exception error fileFile.getCanonicalPath");
                    addLineToLog("deleteFileInProjectDirectory exception error fileFile.getCanonicalPath");
                    System.err.println(e.getLocalizedMessage());
                    addLineToLog(e.getLocalizedMessage());
                }
            }
            fileFile.delete();
        }
    } // end of method deleteFileInProjectDirectory

    void deleteWarningTxtFileInProjectDirectory() {
        deleteFileInProjectDirectory("warning.txt", false);
    } // end of method deleteWarningTxtFileInProjectDirectory

    boolean DELFILESAction() {
        // return value true if error or warning needing to stop detected
        String asterisks;
        boolean isErrorOrWarningNeedingToStop = false;
        String message;
        int replicateNumber;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            currentDateTime.setTime(new java.util.Date());
            message = "********** begin    delfiles at " + currentDateTime.getTime().toString() + " **********";
            asterisks = "****************************************************************";
            while (asterisks.trim().length() < message.trim().length()) {
                asterisks = asterisks + "*";
            }
            addLineToLog(" ");
            addLineToLog(asterisks);
            addLineToLog(message);
            addLineToLog(asterisks);
            addLineToLog("********** deleting all project files except GDVDATA and SIMDATA");
            addLineToLog("********** Project Directory = '" + texas.ProjectDirectory + "'");
            if (texas.ProjectName.trim().length() > 0) {
                addLineToLog("********** Project Name ---- = '" + texas.ProjectName + "'");
            }

            deleteErrorTxtFileInProjectDirectory();
            deleteWarningTxtFileInProjectDirectory();

            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            deleteFileInProjectDirectory("fort21", false);
            deleteFileInProjectDirectory("fort22", false);
            deleteFileInProjectDirectory("fort23", false);
            deleteFileInProjectDirectory("fort26", false);
            deleteFileInProjectDirectory(texas.DISDAT_file, true);
            deleteFileInProjectDirectory(texas.DISPAR_file, true);
            deleteFileInProjectDirectory(texas.DVPLIST_file, true);
            deleteFileInProjectDirectory(texas.EMERR_file, true);
            deleteFileInProjectDirectory(texas.EMLIST_file, true);
            deleteFileInProjectDirectory(texas.EMSTA_file, true);
            deleteFileInProjectDirectory(texas.EMSTAFIN_file, true);
            deleteFileInProjectDirectory(texas.EMSTAVE_file, true);
            deleteFileInProjectDirectory(texas.FORT8_file, true);
            deleteFileInProjectDirectory(texas.FORT9_file, true);
            deleteFileInProjectDirectory(texas.GDV_file, true);
            deleteFileInProjectDirectory(texas.GEOLIST_file, true);
            deleteFileInProjectDirectory(texas.GEOPLOT_file, true);
            deleteFileInProjectDirectory(texas.PARAMS_file, true);
            deleteFileInProjectDirectory(texas.POSDAT_file, true);
            deleteFileInProjectDirectory(texas.SIM_file, true);
            deleteFileInProjectDirectory(texas.SIMERR_file, true);
            deleteFileInProjectDirectory(texas.SIMPLST_file, true);
            deleteFileInProjectDirectory(texas.SIMSLST_file, true);
            deleteFileInProjectDirectory(texas.SIMSTAT_file, true);
            deleteFileInProjectDirectory(texas.SPRDSHT_file, true);
            deleteFileInProjectDirectory(texas.SSAM_file, true);

            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            deleteFileInProjectDirectory("stop.rep", false); // stop.rep
            deleteFileInProjectDirectory(texas.FORT8_file, true); // fort8.rep
            deleteFileInProjectDirectory(texas.GDV_file, true); // gdv.rep
            deleteFileInProjectDirectory(texas.GEOLIST_file, true); // geolist_rep.txt
            deleteFileInProjectDirectory(texas.GEOPLOT_file, true); // geoplot.rep
            deleteFileInProjectDirectory(texas.PARAMS_file, true); // params_rep.txt
            deleteFileInProjectDirectory(texas.SIM_file, true); // sim.rep
            deleteFileInProjectDirectory(texas.SIMSLST_file, true); // simslst_rep.txt

            for (replicateNumber = 1; replicateNumber <= PARAMS.TEXAS_MODEL_NRP; replicateNumber++) {
                isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }

                deleteFileInProjectDirectory(texas.DISDAT_file, true); // disdat.rnn
                deleteFileInProjectDirectory(texas.DVPLIST_file, true); // dvplist_rnn.txt
                deleteFileInProjectDirectory(texas.FORT9_file, true); // fort9.rnn
                deleteFileInProjectDirectory(texas.POSDAT_file, true); // posdat.rnn
                deleteFileInProjectDirectory(texas.SIMERR_file, true); // simerr_rnn.txt
                deleteFileInProjectDirectory(texas.SIMPLST_file, true); // simplst_rnn.txt
                deleteFileInProjectDirectory(texas.SIMSTAT_file, true); // simstat.rnn
                deleteFileInProjectDirectory(texas.SSAM_file, true); // ssam_rnn.trj
            }

            currentDateTime.setTime(new java.util.Date());
            addLineToLog("********** end      delfiles at " + currentDateTime.getTime().toString() + " **********");
        } // end of try
        catch (Exception e) {
            System.out.println("DELFILESAction exception error");
            addLineToLog("DELFILESAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method DELFILESAction

    boolean DISFITAction(String inputFileName, String outputFileName) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> disfitCommands;
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(inputFileName)) {
                JOptionPane.showMessageDialog(null, "DISFIT input file = '" + inputFileName + "' does not exist or is not a file or is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            disfitCommands = new ArrayList<String>();
            disfitCommands.add(texas.TexasExeDirectory + File.separator + "disfit.exe");
            disfitCommands.add("I+" + inputFileName);
            disfitCommands.add("L+" + outputFileName);
            disfitCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);

            isErrorOrWarningNeedingToStop = runCommand("disfit", disfitCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("DISFITAction exception error");
            addLineToLog("DISFITAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method DISFITAction

    void DISFITActionListener() {
        File f;
        JFileChooser fileChooser = new JFileChooser();
        File inputFile = null;
        String inputFileName = null;
        File outputFile = null;
        String outputFileName = null;
        int retInput;
        int retOutput;

        try {
            if (texas.ProjectDirectory != null) {
                if (texas.ProjectDirectory.trim().length() > 0) {
                    f = new File(texas.ProjectDirectory);
                }
                else {
                    f = new File(new File(".").getCanonicalPath());
                }
            }
            else {
                f = new File(new File(".").getCanonicalPath());
            }
        }
        catch (Exception e) {
            System.out.println("DISFITActionListener exception error file.getCanonicalPath");
            addLineToLog("DISFITActionListener exception error file.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        try {
            fileChooser.setCurrentDirectory(f);
        }
        catch (Exception e) {
            System.out.println("DISFITActionListener exception error fileChooser.setCurrentDirectory");
            addLineToLog("DISFITActionListener exception error fileChooser.setCurrentDirectory");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Input File");

        while (true) {
            retInput = fileChooser.showOpenDialog(null);

            if (retInput == JFileChooser.CANCEL_OPTION) {
                return;
            }

            inputFile = fileChooser.getSelectedFile();

            try {
                if (inputFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (inputFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.out.println("DISFITActionListener exception error inputFile.getCanonicalPath");
                addLineToLog("DISFITActionListener exception error inputFile.getCanonicalPath");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            if (!inputFile.exists()) {
                JOptionPane.showMessageDialog(null, "Input File Does Not Exist", "Input File Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        try {
            inputFileName = inputFile.getCanonicalPath();
        }
        catch (Exception e) {
            System.out.println("DISFITActionListener exception error inputFile.getCanonicalPath");
            addLineToLog("DISFITActionListener exception error inputFile.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Output File");

        while (true) {
            retOutput = fileChooser.showOpenDialog(null);

            if (retOutput == JFileChooser.CANCEL_OPTION) {
                return;
            }

            outputFile = fileChooser.getSelectedFile();

            try {
                if (outputFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (outputFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.out.println("DISFITActionListener exception error outputFile.getCanonicalPath");
                addLineToLog("DISFITActionListener exception error outputFile.getCanonicalPath");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            break;
        }

        try {
            outputFileName = outputFile.getCanonicalPath();
        }
        catch (Exception e) {
            System.out.println("DISFITActionListener exception error outputFile.getCanonicalPath");
            addLineToLog("DISFITActionListener exception error outputFile.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        DISFITAction(inputFileName, outputFileName);
    } // end of method DISFITActionListener

    boolean DISPREAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> dispreCommands;
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.POSDAT_file))) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "POSDAT file = '"
                                        + texas.POSDAT_file
                                        + "' does not exist or is not a file or is not readable.\nCheck if Animation/Pollution Dispersion Model File is requested in gdvsim Simulation Data and check correct execution of simpro.",
                                "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            dispreCommands = new ArrayList<String>();
            dispreCommands.add(texas.TexasExeDirectory + File.separator + "dispre.exe");
            dispreCommands.add("/R");
            dispreCommands.add("2048");
            dispreCommands.add("PVA+" + texas.POSDAT_file);
            dispreCommands.add("DIS+" + texas.DISDAT_file);
            dispreCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);
            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.DISPAR_file))) {
                dispreCommands.add("PAR+" + texas.DISPAR_file);
            }
            else if (fileExistAndIsFileAndIsReadable(texas.TexasSysDatDirectory + File.separator + "dispar")) {
                dispreCommands.add("PAR+" + texas.TexasSysDatDirectory + File.separator + "dispar");
            }
            else {
                JOptionPane.showMessageDialog(null, "DISPAR file = '" + texas.DISPAR_file + "' does not exist or is not a file or is not readable and\nDISPAR file = '" + texas.TexasSysDatDirectory
                        + File.separator + "dispar" + "' does not exist or is not a file or is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            isErrorOrWarningNeedingToStop = runCommand("dispre", dispreCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("DISPREAction exception error");
            addLineToLog("DISPREAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method DISPREAction

    boolean DISPROAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        long DISDAT_fileLastModifiedTime;
        java.util.List<String> disproCommands;
        boolean isErrorOrWarningNeedingToStop = false;
        long POSDAT_fileLastModifiedTime;
        boolean processDISPRE = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.POSDAT_file))) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "POSDAT file = '"
                                        + texas.POSDAT_file
                                        + "' does not exist or is not a file or is not readable.\nCheck if Animation/Pollution Dispersion Model File is requested in gdvsim Simulation Data and check correct execution of simpro.",
                                "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.DISDAT_file))) {
                DISDAT_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.DISDAT_file));
                POSDAT_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.POSDAT_file));
                if (DISDAT_fileLastModifiedTime < 0) {
                    return true;
                }
                if (POSDAT_fileLastModifiedTime < 0) {
                    return true;
                }
                if (POSDAT_fileLastModifiedTime > DISDAT_fileLastModifiedTime) {
                    processDISPRE = true;
                }
            }
            else {
                processDISPRE = true;
            }

            if (processDISPRE) {
                isErrorOrWarningNeedingToStop = DISPREAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.DISDAT_file))) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "DISDAT file = '"
                                        + texas.DISDAT_file
                                        + "' does not exist or is not a file or is not readable.\nCheck if Animation/Pollution Dispersion Model File is requested in gdvsim Simulation Data, check correct execution of simpro, and check correct execution of dispre.",
                                "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            disproCommands = new ArrayList<String>();
            disproCommands.add("java");
            disproCommands.add("-Xms50m");
            disproCommands.add("-Xmx1000m");
            disproCommands.add("-jar");
            disproCommands.add(texas.TexasExeDirectory + File.separator + "texasdis.jar");
            disproCommands.add(texas.DISDAT_file);

            isErrorOrWarningNeedingToStop = runCommand("dispro", disproCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("DISPROAction exception error");
            addLineToLog("DISPROAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method DISPROAction

    boolean DTSIMAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        long FORT8_fileLastModifiedTime;
        long FORT9_fileLastModifiedTime;
        long GDVDATA_fileLastModifiedTime;
        boolean isErrorOrWarningNeedingToStop = false;
        boolean processDVPRO = false;
        boolean processGEOPRO = false;
        boolean processSIMCONV = false;
        long SIM_fileLastModifiedTime;
        long SIMDATA_fileLastModifiedTime;
        java.util.List<String> dtSimCommands;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT8_file))) {
                FORT8_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.FORT8_file));
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                if (FORT8_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime > FORT8_fileLastModifiedTime) {
                    processGEOPRO = true;
                }
            }
            else {
                processGEOPRO = true;
            }

            if (processGEOPRO) {
                isErrorOrWarningNeedingToStop = GEOPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT8_file))) {
                JOptionPane.showMessageDialog(null, "FORT8 file = '" + texas.FORT8_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of geopro or gdvpro.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT9_file))) {
                FORT9_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.FORT9_file));
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                if (FORT9_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime > FORT9_fileLastModifiedTime) {
                    processDVPRO = true;
                }
            }
            else {
                processDVPRO = true;
            }

            if (processDVPRO) {
                isErrorOrWarningNeedingToStop = DVPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT9_file))) {
                JOptionPane.showMessageDialog(null, "FORT9 file = '" + texas.FORT9_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of dvpro or gdvpro.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMDATA_file))) {
                JOptionPane.showMessageDialog(null, "SIMDATA file = '" + texas.SIMDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIM_file))) {
                SIM_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.SIM_file));
                SIMDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.SIMDATA_file));
                if (SIM_fileLastModifiedTime < 0) {
                    return true;
                }
                if (SIMDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (SIMDATA_fileLastModifiedTime > SIM_fileLastModifiedTime) {
                    processSIMCONV = true;
                }
            }
            else {
                processSIMCONV = true;
            }

            if (processSIMCONV) {
                isErrorOrWarningNeedingToStop = SIMCONVAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIM_file))) {
                JOptionPane.showMessageDialog(null, "SIM file = '" + texas.SIM_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim and simconv.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            File simproParameters = new File(texas.ProjectDirectory + File.separator + "simpro.par");
            FileWriter fWriter = new FileWriter(simproParameters);
            fWriter.write("I=" + texas.SIM_file + "\n");
            fWriter.write("L=" + texas.SIMPLST_file + "\n");
            fWriter.write("STA=" + texas.SIMSTAT_file + "\n");
            fWriter.write("T8=" + texas.FORT8_file + "\n");
            fWriter.write("T9=" + texas.FORT9_file + "\n");
            fWriter.write("PVA=" + texas.POSDAT_file + "\n");
            fWriter.write("ERR=" + texas.SIMERR_file + "\n");
            fWriter.write("SYS_DAT=" + texas.TexasSysDatDirectory + File.separator + "\n");
            fWriter.write("SSAM=" + texas.SSAM_file + "\n");
            if (replicateNumber >= 1 && replicateNumber <= PARAMS.TEXAS_MODEL_NRP) {
                fWriter.write("REP=" + replicateNumber + "\n");
            }
            fWriter.write("PAUSEND=" + "NO\n");
            fWriter.close();

            dtSimCommands = new ArrayList<String>();
            dtSimCommands.add("java");
            dtSimCommands.add("-Xms50m");
            dtSimCommands.add("-Xmx500m");
            dtSimCommands.add("-jar");
            dtSimCommands.add("-Djna.library.path=" + texas.TexasExeDirectory);
            dtSimCommands.add(texas.TexasExeDirectory + File.separator + "dtsim.jar");

            isErrorOrWarningNeedingToStop = runCommand("dtsim", dtSimCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("DTSIMAction exception error");
            addLineToLog("DTSIMAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method DTSIMAction

    boolean DVPROAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> dvproCommands;
        long GDV_fileLastModifiedTime;
        long GDVDATA_fileLastModifiedTime;
        boolean isErrorOrWarningNeedingToStop = false;
        boolean processGDVCONV = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDV_file))) {
                GDV_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDV_file));
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                if (GDV_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime > GDV_fileLastModifiedTime) {
                    processGDVCONV = true;
                }
            }
            else {
                processGDVCONV = true;
            }

            if (processGDVCONV) {
                isErrorOrWarningNeedingToStop = GDVCONVAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDV_file))) {
                JOptionPane.showMessageDialog(null, "GDV file = '" + texas.GDV_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim and gdvconv.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            dvproCommands = new ArrayList<String>();
            dvproCommands.add(texas.TexasExeDirectory + File.separator + "dvpro.exe");
            dvproCommands.add("I+" + texas.GDV_file);
            dvproCommands.add("L+" + texas.DVPLIST_file);
            dvproCommands.add("T9+" + texas.FORT9_file);
            dvproCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);
            if (replicateNumber >= 1 && replicateNumber <= PARAMS.TEXAS_MODEL_NRP) {
                dvproCommands.add("REP+" + replicateNumber);
            }

            isErrorOrWarningNeedingToStop = runCommand("dvpro", dvproCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("DVPROAction exception error");
            addLineToLog("DVPROAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method DVPROAction

    boolean EMPROAction(int vehicleModelYear) {
        // return value true if error or warning needing to stop detected
        int currentYear;
        java.util.List<String> emproCommands;
        boolean isErrorOrWarningNeedingToStop = false;
        int numberOfReplicates;
        int replicateNumber;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            currentDateTime.setTime(new java.util.Date());
            currentYear = currentDateTime.get(Calendar.YEAR);

            if (vehicleModelYear < PARAMS.TEXAS_MODEL_EMPROY || vehicleModelYear > currentYear) {
                JOptionPane.showMessageDialog(null, "Vehicle Model Year = '" + vehicleModelYear + " is less than " + PARAMS.TEXAS_MODEL_EMPROY + " or greater than " + currentYear + ".",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.POSDAT_file))) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "POSDAT file = '"
                                        + texas.POSDAT_file
                                        + "' does not exist or is not a file or is not readable.\nCheck if Animation/Pollution Dispersion Model File is requested in gdvsim Simulation Data and check correct execution of simpro.",
                                "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            numberOfReplicates = 0;
            for (replicateNumber = 1; replicateNumber <= PARAMS.TEXAS_MODEL_NRP; replicateNumber++) {
                isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
                if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.POSDAT_file))) {
                    numberOfReplicates = replicateNumber;
                }
            }

            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            emproCommands = new ArrayList<String>();
            emproCommands.add(texas.TexasExeDirectory + File.separator + "empro.exe");
            emproCommands.add("PVA+" + texas.POSDAT_file);
            emproCommands.add("L+" + texas.EMLIST_file);
            emproCommands.add("STA+" + texas.EMSTA_file);
            emproCommands.add("STAFIN+" + texas.EMSTAFIN_file);
            emproCommands.add("STAAVE+" + texas.EMSTAVE_file);
            emproCommands.add("ERR+" + texas.EMERR_file);
            emproCommands.add("REP+" + numberOfReplicates);
            emproCommands.add("YEAR+" + vehicleModelYear);
            emproCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);

            isErrorOrWarningNeedingToStop = runCommand("empro", emproCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("EMPROAction exception error");
            addLineToLog("EMPROAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method EMPROAction

    void EMPROActionListener() {
        String ans;
        int year, yearMin = PARAMS.TEXAS_MODEL_EMPROY, yearMax;

        currentDateTime.setTime(new java.util.Date());
        yearMax = currentDateTime.get(Calendar.YEAR);

        while (true) {
            try {
                ans = JOptionPane.showInputDialog(null, "4-digit vehicle model year for emissions factors (" + PARAMS.TEXAS_MODEL_EMPROY + "-" + yearMax + ")");
                if (ans == null) {
                    return;
                }

                if (ans.trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Year should not be empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                else if (ans.trim().length() != 4) {
                    JOptionPane.showMessageDialog(null, "Year should be 4 digits.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                try {
                    year = Integer.parseInt(ans.trim());
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(null, "year = '" + ans.trim() + "' contains illegal characters for an integer number.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (year < yearMin || year > yearMax) {
                    JOptionPane.showMessageDialog(null, "Year = " + year + " is less than " + yearMin + " or greater than " + yearMax + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                EMPROAction(year);
                break;
            }
            catch (Exception e) {
                System.out.println("EMPROActionListener exception error");
                addLineToLog("EMPROActionListener exception error");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }
        } // end of while ( true )
    } // end of method EMPROActionListener

    boolean fileExistAndIsDirectory(String directoryName) {
        if (directoryName == null) {
            JOptionPane.showMessageDialog(null, "directoryName argument is null.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (directoryName.trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "directoryName argument is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        File directoryFile = new File(directoryName);

        if (directoryFile.exists()) {
            if (directoryFile.isDirectory()) {
                return true;
            }
        }

        return false;
    } // end of method fileExistAndIsDirectory

    boolean fileExistAndIsFileAndIsExecutable(String fileName) {
        if (fileName == null) {
            JOptionPane.showMessageDialog(null, "fileName argument is null.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (fileName.trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "fileName argument is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        File fileFile = new File(fileName);

        if (fileFile.exists()) {
            if (fileFile.isFile()) {
                if (fileFile.canExecute()) {
                    return true;
                }
            }
        }

        return false;
    } // end of method fileExistAndIsFileAndIsExecutable

    boolean fileExistAndIsFileAndIsReadable(String fileName) {
        if (fileName == null) {
            JOptionPane.showMessageDialog(null, "fileName argument is null.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (fileName.trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "fileName argument is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        File fileFile = new File(fileName);

        if (fileFile.exists()) {
            if (fileFile.isFile()) {
                if (fileFile.canRead()) {
                    return true;
                }
            }
        }

        return false;
    } // end of method fileExistAndIsFileAndIsReadable

    long fileLastModifiedTime(String fileName) {
        if (fileName == null) {
            JOptionPane.showMessageDialog(null, "fileName argument is null.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return -1;
        }

        if (fileName.trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "fileName argument is empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return -1;
        }

        File fileFile = new File(fileName);

        if (fileFile.exists()) {
            if (fileFile.isFile()) {
                return fileFile.lastModified();
            }
        }

        JOptionPane.showMessageDialog(null, fileName + " does not exist or is not a file.", "Error Message", JOptionPane.ERROR_MESSAGE);
        return -1;
    } // end of method fileLastModifiedTime

    String fileNameWithProjectDirectory(String fileName) {
        if (texas.ProjectDirectory == null) {
            return fileName;
        }
        if (texas.ProjectDirectory.trim().length() == 0) {
            return fileName;
        }
        return texas.ProjectDirectory + File.separator + fileName;
    } // end of method fileNameWithProjectDirectory

    boolean FROM80DAction(String inputFileName, String outputFileName) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> from80dCommands;
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(inputFileName)) {
                JOptionPane.showMessageDialog(null, "FROM80D input file = '" + inputFileName + "' does not exist or is not a file or is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            from80dCommands = new ArrayList<String>();
            from80dCommands.add(texas.TexasUtilityDirectory + File.separator + "from80d.exe");
            from80dCommands.add(inputFileName);
            from80dCommands.add(outputFileName);

            isErrorOrWarningNeedingToStop = runCommand("from80d", from80dCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("FROM80DAction exception error");
            addLineToLog("FROM80DAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method FROM80DAction

    void FROM80DActionListener() {
        File f;
        JFileChooser fileChooser = new JFileChooser();
        File inputFile;
        String inputFileName = null;
        File outputFile = null;
        String outputFileName = null;
        int retInput;
        int retOutput;

        try {
            if (texas.ProjectDirectory != null) {
                if (texas.ProjectDirectory.trim().length() > 0) {
                    f = new File(texas.ProjectDirectory);
                }
                else {
                    f = new File(new File(".").getCanonicalPath());
                }
            }
            else {
                f = new File(new File(".").getCanonicalPath());
            }
        }
        catch (Exception e) {
            System.out.println("FROM80DActionListener exception error file.getCanonicalPath");
            addLineToLog("FROM80DActionListener exception error file.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        try {
            fileChooser.setCurrentDirectory(f);
        }
        catch (Exception e) {
            System.out.println("FROM80DActionListener exception error fileChooser.setCurrentDirectory");
            addLineToLog("FROM80DActionListener exception error fileChooser.setCurrentDirectory");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Input File");

        while (true) {
            retInput = fileChooser.showOpenDialog(null);

            if (retInput == JFileChooser.CANCEL_OPTION) {
                return;
            }

            inputFile = fileChooser.getSelectedFile();

            try {
                if (inputFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (inputFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.out.println("FROM80DActionListener exception error inputFile.getCanonicalPath");
                addLineToLog("FROM80DActionListener exception error inputFile.getCanonicalPath");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            if (!inputFile.exists()) {
                JOptionPane.showMessageDialog(null, "Input File Does Not Exist", "Input File Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        try {
            inputFileName = inputFile.getCanonicalPath();
        }
        catch (Exception e) {
            System.out.println("FROM80DActionListener exception error inputFile.getCanonicalPath");
            addLineToLog("FROM80DActionListener exception error inputFile.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Output File");

        while (true) {
            retOutput = fileChooser.showOpenDialog(null);

            if (retOutput == JFileChooser.CANCEL_OPTION) {
                return;
            }

            outputFile = fileChooser.getSelectedFile();

            try {
                if (outputFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (outputFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.out.println("FROM80DActionListener exception error outputFile.getCanonicalPath");
                addLineToLog("FROM80DActionListener exception error outputFile.getCanonicalPath");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            break;
        }

        try {
            outputFileName = outputFile.getCanonicalPath();
        }
        catch (Exception e) {
            System.out.println("FROM80DActionListener exception error outputFile.getCanonicalPath");
            addLineToLog("FROM80DActionListener exception error outputFile.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        FROM80DAction(inputFileName, outputFileName);
    } // end of method FROM80DActionListener

    boolean GDVCONVAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> gdvconvCommands;
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            gdvconvCommands = new ArrayList<String>();
            gdvconvCommands.add(texas.TexasExeDirectory + File.separator + "gdvconv.exe");
            gdvconvCommands.add("PRE+" + texas.GDVDATA_file);
            gdvconvCommands.add("C+" + texas.GDV_file);
            gdvconvCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);

            isErrorOrWarningNeedingToStop = runCommand("gdvconv", gdvconvCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("GDVCONVAction exception error");
            addLineToLog("GDVCONVAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method GDVCONVAction

    boolean GDVPROAction(int replicateNumber) {
        boolean isErrorOrWarningNeedingToStop = false;

        isErrorOrWarningNeedingToStop = GEOPROAction(replicateNumber);
        if (isErrorOrWarningNeedingToStop) {
            return true;
        }

        isErrorOrWarningNeedingToStop = DVPROAction(replicateNumber);
        if (isErrorOrWarningNeedingToStop) {
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method GDVPROAction

    boolean GDVSIMAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> gdvsimCommands;
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            gdvsimCommands = new ArrayList<String>();
            gdvsimCommands.add("java");
            gdvsimCommands.add("-Xms50m");
            gdvsimCommands.add("-Xmx500m");
            gdvsimCommands.add("-jar");
            gdvsimCommands.add(texas.TexasExeDirectory + File.separator + "gdvsim.jar");
            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                gdvsimCommands.add(texas.GDVDATA_file);
                if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMDATA_file))) {
                    gdvsimCommands.add(texas.SIMDATA_file);
                }
            }

            isErrorOrWarningNeedingToStop = runCommand("gdvsim", gdvsimCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("GDVSIMAction exception error");
            addLineToLog("GDVSIMAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method GDVSIMAction

    boolean GEOPLOTAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> geoplotCommands;
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GEOPLOT_file))) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                "GEOPLOT file = '"
                                        + texas.GEOPLOT_file
                                        + "' does not exist or is not a file or is not readable.\nCheck if Plot Option is set to 'PLOT' in gdvsim GDV Data Path and Plot Options Data, check correct execution of gdvconv, and check correct execution of geopro/gdvpro.",
                                "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            geoplotCommands = new ArrayList<String>();
            geoplotCommands.add("java");
            geoplotCommands.add("-jar");
            geoplotCommands.add(texas.TexasExeDirectory + File.separator + "texasgeo.jar");
            geoplotCommands.add(texas.GEOPLOT_file);

            isErrorOrWarningNeedingToStop = runCommand("geoplot", geoplotCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("GEOPLOTAction exception error");
            addLineToLog("GEOPLOTAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method GEOPLOTAction

    boolean GEOPROAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        long GDV_fileLastModifiedTime;
        long GDVDATA_fileLastModifiedTime;
        java.util.List<String> geoproCommands;
        boolean isErrorOrWarningNeedingToStop = false;
        boolean processGDVCONV = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDV_file))) {
                GDV_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDV_file));
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                if (GDV_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime > GDV_fileLastModifiedTime) {
                    processGDVCONV = true;
                }
            }
            else {
                processGDVCONV = true;
            }

            if (processGDVCONV) {
                isErrorOrWarningNeedingToStop = GDVCONVAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDV_file))) {
                JOptionPane.showMessageDialog(null, "GDV file = '" + texas.GDV_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim and gdvconv.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            geoproCommands = new ArrayList<String>();
            geoproCommands.add(texas.TexasExeDirectory + File.separator + "geopro.exe");
            geoproCommands.add("I+" + texas.GDV_file);
            geoproCommands.add("L+" + texas.GEOLIST_file);
            geoproCommands.add("T8+" + texas.FORT8_file);
            geoproCommands.add("PLOT+" + texas.GEOPLOT_file);
            geoproCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);

            isErrorOrWarningNeedingToStop = runCommand("geopro", geoproCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("GEOPROAction exception error");
            addLineToLog("GEOPROAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method GEOPROAction

    boolean isError() {
        if (text_ProjDir.getText().trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "You must enter a project directory.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        return false;
    } // end of method isError

    void ProjectDirSelect() {
        File f;
        File fileProjectDirectory;
        int resultProjectDirectory;

        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        try {
            if (texas.ProjectDirectory != null) {
                if (texas.ProjectDirectory.trim().length() > 0) {
                    f = new File(texas.ProjectDirectory);
                }
                else {
                    f = new File(new File(".").getCanonicalPath());
                }
            }
            else {
                f = new File(new File(".").getCanonicalPath());
            }
        }
        catch (Exception e) {
            System.err.println("ProjectDirSelect exception error file.getCanonicalPath");
            addLineToLog("ProjectDirSelect exception error file.getCanonicalPath");
            System.err.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        try {
            fileChooser.setCurrentDirectory(f);
        }
        catch (Exception e) {
            System.err.println("ProjectDirSelect exception error fileChooser.setCurrentDirectory");
            addLineToLog("ProjectDirSelect exception error fileChooser.setCurrentDirectory");
            System.err.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Project Directory");

        while (true) {
            resultProjectDirectory = fileChooser.showOpenDialog(null);

            if (resultProjectDirectory == JFileChooser.CANCEL_OPTION) {
                return;
            }

            fileProjectDirectory = fileChooser.getSelectedFile();

            try {
                if (fileProjectDirectory == null) {
                    JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid Project Directory", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (fileProjectDirectory.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid Project Directory", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.err.println("ProjectDirSelect exception error fileProjectDirectory.getCanonicalPath");
                addLineToLog("ProjectDirSelect exception error fileProjectDirectory.getCanonicalPath");
                System.err.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            if (!fileProjectDirectory.exists()) {
                JOptionPane.showMessageDialog(null, "Project Directory Does Not Exist", "Project Directory Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        try {
            texas.ProjectDirectory = fileProjectDirectory.getCanonicalPath();
        }
        catch (Exception e) {
            System.err.println("ProjectDirSelect exception error fileProjectDirectory.getCanonicalPath");
            addLineToLog("ProjectDirSelect exception error fileProjectDirectory.getCanonicalPath");
            System.err.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        text_ProjDir.setText(texas.ProjectDirectory);
    } // end of method ProjectDirSelect

    void ProjectNameSelect() {
        char c;
        int i;
        int n;
        File projectNameFile;
        String projectName;
        String projectNameName;
        int resultProjectName;

        if (texas.ProjectDirectory == null) {
            JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid Project Directory", JOptionPane.ERROR_MESSAGE);
            return;
        }

        if (texas.ProjectDirectory.trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid Project Directory", JOptionPane.ERROR_MESSAGE);
            return;
        }

        JFileChooser fileChooser = new JFileChooser(texas.ProjectDirectory);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        javax.swing.filechooser.FileFilter fileFilter = new TexasFileFilterGDVDATA();
        fileChooser.setFileFilter(fileFilter);
        fileChooser.setDialogTitle("Select Project '_" + texas.GDVDATA_name + "' File");

        while (true) {
            resultProjectName = fileChooser.showOpenDialog(null);

            if (resultProjectName == JFileChooser.CANCEL_OPTION) {
                return;
            }

            projectNameFile = fileChooser.getSelectedFile();

            try {
                if (projectNameFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid Project Name", "Invalid Project Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (projectNameFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid Project Name", "Invalid Project Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.err.println("ProjectNameSelect exception error projectNameFile.getCanonicalPath");
                addLineToLog("ProjectNameSelect exception error projectNameFile.getCanonicalPath");
                System.err.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            if (!projectNameFile.exists()) {
                JOptionPane.showMessageDialog(null, "Project Name Does Not Exist", "Project Name Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            projectNameName = projectNameFile.getName();

            if (projectNameName == null) {
                JOptionPane.showMessageDialog(null, "Project Name is null", "Project Name is null", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (projectNameName.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "Project Name is empty", "Project Name is empty", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (!projectNameName.endsWith("_" + texas.GDVDATA_name)) {
                JOptionPane.showMessageDialog(null, "Project Name does not end with _" + texas.GDVDATA_name, "Project Name does not end with " + texas.GDVDATA_name, JOptionPane.ERROR_MESSAGE);
                continue;
            }

            i = projectNameName.lastIndexOf("_" + texas.GDVDATA_name);
            if (i == -1) {
                JOptionPane.showMessageDialog(null, "Project Name does not end with _" + texas.GDVDATA_name, "Project Name does not end with " + texas.GDVDATA_name, JOptionPane.ERROR_MESSAGE);
                continue;
            }

            projectName = projectNameName.substring(0, i);

            if (projectName == null) {
                JOptionPane.showMessageDialog(null, "Project Name is null", "Project Name is null", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (projectName.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "Project Name is empty", "Project Name is empty", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            n = projectName.trim().length();
            for (i = 0; i < n; i++) {
                c = projectName.charAt(i);
                // any character
                if (Character.isLetterOrDigit(c)) {
                    continue;
                }
                if (i > 0 && i < n - 1) {
                    // middle characters
                    if (c == '-') {
                        continue;
                    }
                    if (c == '_') {
                        continue;
                    }
                }
                JOptionPane.showMessageDialog(null, "Project Name = '" + projectName + "' contains illegal character '" + c + "' at character " + (i + 1)
                        + ".\nProjct Names may have letters and digits anywhere and underbar and minus only in the middle.", "Error Message", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            texas.ProjectName = projectName;
            text_ProjName.setText(texas.ProjectName);

            break;
        }

        text_ProjDir.setText(texas.ProjectDirectory);
    } // end of method ProjectNameSelect

    boolean REPRUNAction(int begReplicateNumber, int endReplicateNumber) {
        // return value true if error or warning needing to stop detected
        String asterisks;
        boolean isErrorOrWarningNeedingToStop = false;
        String message;
        int replicateNumber;
        FileOutputStream paramsRepFileOutputStream;
        PrintStream paramsRepPrintStream;

        try {
            if (begReplicateNumber < 1 || begReplicateNumber > PARAMS.TEXAS_MODEL_NRP) {
                JOptionPane.showMessageDialog(null, "Beginning Replicate Number = " + begReplicateNumber + " is less than 1 or greater than " + PARAMS.TEXAS_MODEL_NRP + ".", "Error Message",
                        JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (endReplicateNumber < 1 || endReplicateNumber > PARAMS.TEXAS_MODEL_NRP) {
                JOptionPane.showMessageDialog(null, "Ending Replicate Number = " + endReplicateNumber + " is less than 1 or greater than " + PARAMS.TEXAS_MODEL_NRP + ".", "Error Message",
                        JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (begReplicateNumber > endReplicateNumber) {
                JOptionPane.showMessageDialog(null, "Beginning Replicate Number = " + begReplicateNumber + " is greater than Ending Replicate Number = " + endReplicateNumber + ".", "Error Message",
                        JOptionPane.ERROR_MESSAGE);
                return true;
            }

            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMDATA_file))) {
                JOptionPane.showMessageDialog(null, "SIMDATA file = '" + texas.SIMDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            currentDateTime.setTime(new java.util.Date());
            message = "********** begin    reprun beg = " + begReplicateNumber + " end = " + endReplicateNumber + " at " + currentDateTime.getTime().toString() + " **********";
            asterisks = "*********************************" + "*" + "*******" + "*" + "****" + "****************************" + "***********";
            while (asterisks.trim().length() < message.trim().length()) {
                asterisks = asterisks + "*";
            }
            addLineToLog(" ");
            addLineToLog(asterisks);
            addLineToLog(message);
            addLineToLog(asterisks);

            deleteFileInProjectDirectory("stop.rep", false); // stop.rep
            deleteFileInProjectDirectory(texas.FORT8_file, true); // fort8.rep
            deleteFileInProjectDirectory(texas.GDV_file, true); // gdv.rep
            deleteFileInProjectDirectory(texas.GEOLIST_file, true); // geolist_rep.txt
            deleteFileInProjectDirectory(texas.GEOPLOT_file, true); // geoplot.rep
            deleteFileInProjectDirectory(texas.PARAMS_file, true); // params_rep.txt
            deleteFileInProjectDirectory(texas.SIM_file, true); // sim.rep
            deleteFileInProjectDirectory(texas.SIMSLST_file, true); // simslst_rep.txt

            for (replicateNumber = begReplicateNumber; replicateNumber <= PARAMS.TEXAS_MODEL_NRP; replicateNumber++) {
                isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }

                deleteFileInProjectDirectory(texas.DISDAT_file, true); // disdat.rnn
                deleteFileInProjectDirectory(texas.DVPLIST_file, true); // dvplist_rnn.txt
                deleteFileInProjectDirectory(texas.FORT9_file, true); // fort9.rnn
                deleteFileInProjectDirectory(texas.POSDAT_file, true); // posdat.rnn
                deleteFileInProjectDirectory(texas.SIMERR_file, true); // simerr_rnn.txt
                deleteFileInProjectDirectory(texas.SIMPLST_file, true); // simplst_rnn.txt
                deleteFileInProjectDirectory(texas.SIMSTAT_file, true); // simstat.rnn
                deleteFileInProjectDirectory(texas.SSAM_file, true); // ssam_rnn.trj
            }

            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            paramsRepFileOutputStream = new FileOutputStream(fileNameWithProjectDirectory(texas.PARAMS_file));
            paramsRepPrintStream = new PrintStream(paramsRepFileOutputStream);

            currentDateTime.setTime(new java.util.Date());
            paramsRepPrintStream.println("TEXAS Model for Intersection Traffic Replicate Run Processor.");
            paramsRepPrintStream.println("beg date and time = " + currentDateTime.getTime().toString());
            paramsRepPrintStream.println("beg rep num       = " + begReplicateNumber);
            paramsRepPrintStream.println("end rep num       = " + endReplicateNumber);
            paramsRepPrintStream.println("project directory = " + texas.ProjectDirectory);
            paramsRepPrintStream.println("project name      = " + texas.ProjectName);
            paramsRepPrintStream.println("gdvdata     file  = " + texas.GDVDATA_file); // gdvdata
            paramsRepPrintStream.println("simdata     file  = " + texas.SIMDATA_file); // simdata
            paramsRepPrintStream.println("gdv         file  = " + texas.GDV_file); // gdv.rep
            paramsRepPrintStream.println("fort8       file  = " + texas.FORT8_file); // fort8.rep
            paramsRepPrintStream.println("geolist.txt file  = " + texas.GEOLIST_file); // geolist_rep.txt
            paramsRepPrintStream.println("geoplot     file  = " + texas.GEOPLOT_file); // geoplot.rep
            paramsRepPrintStream.println("sim         file  = " + texas.SIM_file); // sim.rep
            paramsRepPrintStream.println("simslst.txt file  = " + texas.SIMSLST_file); // simslst_rep.txt
            paramsRepPrintStream.println("fort9       file  = " + texas.FORT9_file.replace("r1", "rnn")); // fort9.rnn
            paramsRepPrintStream.println("dvplist.txt file  = " + texas.DVPLIST_file.replace("r1", "rnn")); // dvplist_rnn.txt
            paramsRepPrintStream.println("simplst.txt file  = " + texas.SIMPLST_file.replace("r1", "rnn")); // simplst_rnn.txt
            paramsRepPrintStream.println("simerr.txt  file  = " + texas.SIMERR_file.replace("r1", "rnn")); // simerr_rnn.txt
            paramsRepPrintStream.println("posdat      file  = " + texas.POSDAT_file.replace("r1", "rnn")); // posdat.rnn
            paramsRepPrintStream.println("simstat     file  = " + texas.SIMSTAT_file.replace("r1", "rnn")); // simstat.rnn
            paramsRepPrintStream.println("ssam.trj    file  = " + texas.SSAM_file.replace("r1", "rnn")); // ssam_rnn.trj

            isErrorOrWarningNeedingToStop = GDVCONVAction(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            isErrorOrWarningNeedingToStop = GEOPROAction(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            isErrorOrWarningNeedingToStop = SIMCONVAction(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            for (replicateNumber = begReplicateNumber; replicateNumber <= endReplicateNumber; replicateNumber++) {
                isErrorOrWarningNeedingToStop = DVPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }

                isErrorOrWarningNeedingToStop = SIMPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (endReplicateNumber >= 3) {
                isErrorOrWarningNeedingToStop = SIMSTAAction(0);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            currentDateTime.setTime(new java.util.Date());
            paramsRepPrintStream.println("end date and time = " + currentDateTime.getTime().toString());
            paramsRepPrintStream.close();
            paramsRepFileOutputStream.close();
        } // end of try
        catch (Exception e) {
            System.out.println("REPRUNAction exception error");
            addLineToLog("REPRUNAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        currentDateTime.setTime(new java.util.Date());
        message = "********** end      reprun beg = " + begReplicateNumber + " end = " + endReplicateNumber + " at " + currentDateTime.getTime().toString() + " **********";
        asterisks = "*********************************" + "*" + "*******" + "*" + "****" + "****************************" + "***********";
        while (asterisks.trim().length() < message.trim().length()) {
            asterisks = asterisks + "*";
        }
        addLineToLog(asterisks);
        addLineToLog(message);
        addLineToLog(asterisks);

        return isErrorOrWarningNeedingToStop;
    } // end of method REPRUNAction

    void REPRUNActionListener() {
        String ansBeg;
        String ansEnd;
        int begReplicateNumber;
        int endReplicateNumber;
        long GDVDATA_fileLastModifiedTime;
        boolean isErrorOrWarningNeedingToStop;
        int startReplicateNumber = 1;
        int replicateNumber;
        long SIMSTAT_fileLastModifiedTime;

        for (replicateNumber = 1; replicateNumber <= PARAMS.TEXAS_MODEL_NRP; replicateNumber++) {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMSTAT_file))) {
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                SIMSTAT_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.SIMSTAT_file));
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return;
                }
                if (SIMSTAT_fileLastModifiedTime < 0) {
                    return;
                }
                startReplicateNumber = replicateNumber;
                if (SIMSTAT_fileLastModifiedTime > GDVDATA_fileLastModifiedTime) {
                    continue;
                }
                else {
                    break;
                }
            }
            else {
                startReplicateNumber = replicateNumber;
                break;
            }
        }

        while (true) {
            try {
                ansBeg = JOptionPane.showInputDialog(null, "Begin Replicate Number (1-" + startReplicateNumber + ")");
                if (ansBeg == null) {
                    return;
                }

                if (ansBeg.trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Begin Replicate Number should not be empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                try {
                    begReplicateNumber = Integer.parseInt(ansBeg.trim());
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(null, "Begin Replicate is not a valid integer.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (begReplicateNumber < 1 || begReplicateNumber > startReplicateNumber) {
                    JOptionPane.showMessageDialog(null, "Begin Replicate Number = " + begReplicateNumber + " is less than 1 or greater than " + startReplicateNumber + ".", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                break;
            }
            catch (Exception e) {
                System.out.println("REPRUNActionListener exception error Begin Replicate Number");
                addLineToLog("REPRUNActionListener exception error Begin Replicate Number");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }
        } // end of while ( true )

        while (true) {
            try {
                ansEnd = JOptionPane.showInputDialog(null, "End Replicate Number (" + begReplicateNumber + "-" + PARAMS.TEXAS_MODEL_NRP + ")");
                if (ansEnd == null) {
                    return;
                }

                if (ansEnd.trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "End Replicate Number should not be empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                try {
                    endReplicateNumber = Integer.parseInt(ansEnd.trim());
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(null, "End Replicate is not a valid integer.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (endReplicateNumber < begReplicateNumber || endReplicateNumber > PARAMS.TEXAS_MODEL_NRP) {
                    JOptionPane.showMessageDialog(null, "End Replicate Number = " + endReplicateNumber + " is less than " + begReplicateNumber + " or greater than " + PARAMS.TEXAS_MODEL_NRP + ".",
                            "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                break;
            }
            catch (Exception e) {
                System.out.println("REPRUNActionListener exception error End Replicate Number");
                addLineToLog("REPRUNActionListener exception error End Replicate Number");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }
        } // end of while ( true )

        REPRUNAction(begReplicateNumber, endReplicateNumber);
    } // end of method REPRUNActionListener

    boolean REPTOLAction(int tolerance, int max_num_rep_runs) {
        // return value true if error or warning needing to stop detected
        String asterisks;
        BufferedReader bufferedReader;
        FileInputStream fileInputStream;
        InputStreamReader inputStreamReader;
        boolean isErrorOrWarningNeedingToStop = false;
        String message;
        String newLine;
        FileOutputStream paramsRepFileOutputStream;
        PrintStream paramsRepPrintStream;
        String readLine;
        int replicateNumber;

        try {
            if (tolerance < 1 || tolerance > PARAMS.TEXAS_MODEL_MAXTOL) {
                JOptionPane.showMessageDialog(null, "Tolerance = " + tolerance + " is less than 1 or greater than " + PARAMS.TEXAS_MODEL_MAXTOL + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (max_num_rep_runs < MIN_MAX_NUM_REP_RUNS || max_num_rep_runs > PARAMS.TEXAS_MODEL_NRP) {
                JOptionPane.showMessageDialog(null, "Maximum Number of Replicate Runs = " + max_num_rep_runs + " is less than " + MIN_MAX_NUM_REP_RUNS + " or greater than " + PARAMS.TEXAS_MODEL_NRP
                        + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMDATA_file))) {
                JOptionPane.showMessageDialog(null, "SIMDATA file = '" + texas.SIMDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            currentDateTime.setTime(new java.util.Date());
            message = "********** begin    reptol tolerance = " + tolerance + " max_num_rep_runs = " + max_num_rep_runs + " at " + currentDateTime.getTime().toString() + " **********";
            asterisks = "***************************************" + "*" + "********************" + "*" + "****" + "****************************" + "***********";
            while (asterisks.trim().length() < message.trim().length()) {
                asterisks = asterisks + "*";
            }
            addLineToLog(" ");
            addLineToLog(asterisks);
            addLineToLog(message);
            addLineToLog(asterisks);

            deleteFileInProjectDirectory("stop.rep", false); // stop.rep
            deleteFileInProjectDirectory(texas.FORT8_file, true); // fort8.rep
            deleteFileInProjectDirectory(texas.GDV_file, true); // gdv.rep
            deleteFileInProjectDirectory(texas.GEOLIST_file, true); // geolist_rep.txt
            deleteFileInProjectDirectory(texas.GEOPLOT_file, true); // geoplot.rep
            deleteFileInProjectDirectory(texas.PARAMS_file, true); // params_rep.txt
            deleteFileInProjectDirectory(texas.SIM_file, true); // sim.rep
            deleteFileInProjectDirectory(texas.SIMSLST_file, true); // simslst_rep.txt

            for (replicateNumber = 1; replicateNumber <= PARAMS.TEXAS_MODEL_NRP; replicateNumber++) {
                isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }

                deleteFileInProjectDirectory(texas.DISDAT_file, true); // disdat.rnn
                deleteFileInProjectDirectory(texas.DVPLIST_file, true); // dvplist_rnn.txt
                deleteFileInProjectDirectory(texas.FORT9_file, true); // fort9.rnn
                deleteFileInProjectDirectory(texas.POSDAT_file, true); // posdat.rnn
                deleteFileInProjectDirectory(texas.SIMERR_file, true); // simerr_rnn.txt
                deleteFileInProjectDirectory(texas.SIMPLST_file, true); // simplst_rnn.txt
                deleteFileInProjectDirectory(texas.SIMSTAT_file, true); // simstat.rnn
                deleteFileInProjectDirectory(texas.SSAM_file, true); // ssam_rnn.trj
            }

            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            paramsRepFileOutputStream = new FileOutputStream(fileNameWithProjectDirectory(texas.PARAMS_file));
            paramsRepPrintStream = new PrintStream(paramsRepFileOutputStream);

            currentDateTime.setTime(new java.util.Date());
            paramsRepPrintStream.println("TEXAS Model for Intersection Traffic Replicate Run Processor.");
            paramsRepPrintStream.println("beg date and time = " + currentDateTime.getTime().toString());
            paramsRepPrintStream.println("tolerance         = " + tolerance + " percent");
            paramsRepPrintStream.println("beg rep num       = " + 1);
            paramsRepPrintStream.println("max rep num       = " + max_num_rep_runs);
            paramsRepPrintStream.println("project directory = " + texas.ProjectDirectory);
            paramsRepPrintStream.println("project name      = " + texas.ProjectName);
            paramsRepPrintStream.println("gdvdata     file  = " + texas.GDVDATA_file); // gdvdata
            paramsRepPrintStream.println("simdata     file  = " + texas.SIMDATA_file); // simdata
            paramsRepPrintStream.println("gdv         file  = " + texas.GDV_file); // gdv.rep
            paramsRepPrintStream.println("fort8       file  = " + texas.FORT8_file); // fort8.rep
            paramsRepPrintStream.println("geolist.txt file  = " + texas.GEOLIST_file); // geolist_rep.txt
            paramsRepPrintStream.println("geoplot     file  = " + texas.GEOPLOT_file); // geoplot.rep
            paramsRepPrintStream.println("sim         file  = " + texas.SIM_file); // sim.rep
            paramsRepPrintStream.println("simslst.txt file  = " + texas.SIMSLST_file); // simslst_rep.txt
            paramsRepPrintStream.println("fort9       file  = " + texas.FORT9_file.replace("r1", "rnn")); // fort9.rnn
            paramsRepPrintStream.println("dvplist.txt file  = " + texas.DVPLIST_file.replace("r1", "rnn")); // dvplist_rnn.txt
            paramsRepPrintStream.println("simplst.txt file  = " + texas.SIMPLST_file.replace("r1", "rnn")); // simplst_rnn.txt
            paramsRepPrintStream.println("simerr.txt  file  = " + texas.SIMERR_file.replace("r1", "rnn")); // simerr_rnn.txt
            paramsRepPrintStream.println("posdat      file  = " + texas.POSDAT_file.replace("r1", "rnn")); // posdat.rnn
            paramsRepPrintStream.println("simstat     file  = " + texas.SIMSTAT_file.replace("r1", "rnn")); // simstat.rnn
            paramsRepPrintStream.println("ssam.trj    file  = " + texas.SSAM_file.replace("r1", "rnn")); // ssam_rnn.trj

            isErrorOrWarningNeedingToStop = GDVCONVAction(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            isErrorOrWarningNeedingToStop = GEOPROAction(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            isErrorOrWarningNeedingToStop = SIMCONVAction(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            for (replicateNumber = 1; replicateNumber <= max_num_rep_runs; replicateNumber++) {
                isErrorOrWarningNeedingToStop = DVPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }

                isErrorOrWarningNeedingToStop = SIMPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }

                if (replicateNumber >= 3) {
                    isErrorOrWarningNeedingToStop = SIMSTAAction(tolerance);
                    if (isErrorOrWarningNeedingToStop) {
                        return true;
                    }

                    if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory("stop.rep"))) {
                        fileInputStream = new FileInputStream(fileNameWithProjectDirectory("stop.rep"));
                        inputStreamReader = new InputStreamReader(fileInputStream, "ASCII");
                        bufferedReader = new BufferedReader(inputStreamReader);

                        message = "";
                        newLine = "";

                        while ((readLine = bufferedReader.readLine()) != null) {
                            readLine = readLine.trim();
                            if (readLine.length() > 0) {
                                addLineToLog("********** stop.rep reptol ** " + readLine);
                                message = message + newLine + readLine;
                                newLine = "\n";
                            }
                        }

                        bufferedReader.close();
                        bufferedReader = null;
                        inputStreamReader.close();
                        inputStreamReader = null;
                        fileInputStream.close();
                        fileInputStream = null;

                        if (message.trim().length() > 0) {
                            JOptionPane.showMessageDialog(null, "Message:\n" + message, "Message", JOptionPane.ERROR_MESSAGE);
                        }

                        break;
                    }
                }
            }

            currentDateTime.setTime(new java.util.Date());
            paramsRepPrintStream.println("end rep num       = " + replicateNumber);
            paramsRepPrintStream.println("end date and time = " + currentDateTime.getTime().toString());
            paramsRepPrintStream.close();
            paramsRepFileOutputStream.close();
        } // end of try
        catch (Exception e) {
            System.out.println("REPTOLAction exception error");
            addLineToLog("REPTOLAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        currentDateTime.setTime(new java.util.Date());
        message = "********** end      reptol tolerance = " + tolerance + " max_rep_runs = " + max_num_rep_runs + " at " + currentDateTime.getTime().toString() + " **********";
        asterisks = "***************************************" + "*" + "****************" + "*" + "****" + "****************************" + "***********";
        while (asterisks.trim().length() < message.trim().length()) {
            asterisks = asterisks + "*";
        }
        addLineToLog(asterisks);
        addLineToLog(message);
        addLineToLog(asterisks);

        return isErrorOrWarningNeedingToStop;
    } // end of method REPTOLAction

    void REPTOLActionListener() {
        String ans;
        int max_num_rep_runs;
        int tolerance;

        while (true) {
            try {
                ans = JOptionPane.showInputDialog(null, "Tolerance (1-" + PARAMS.TEXAS_MODEL_MAXTOL + ")");
                if (ans == null) {
                    return;
                }

                if (ans.trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Tolerance should not be empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                try {
                    tolerance = Integer.parseInt(ans.trim());
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(null, "Tolerance = '" + ans.trim() + "' contains illegal characters for an integer number.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (tolerance < 1 || tolerance > PARAMS.TEXAS_MODEL_MAXTOL) {
                    JOptionPane.showMessageDialog(null, "Tolerance = " + tolerance + " is less than 1 or greater than " + PARAMS.TEXAS_MODEL_MAXTOL + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                break;
            }
            catch (Exception e) {
                System.out.println("REPTOLAction exception error");
                addLineToLog("REPTOLAction exception error");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }
        } // end of while ( true )

        while (true) {
            try {
                ans = JOptionPane.showInputDialog(null, "Maximum Number of Replicate Runs (" + MIN_MAX_NUM_REP_RUNS + "-" + PARAMS.TEXAS_MODEL_NRP + ")");
                if (ans == null) {
                    return;
                }

                if (ans.trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Maximum Number of Replicate Runs should not be empty.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                try {
                    max_num_rep_runs = Integer.parseInt(ans.trim());
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(null, "Maximum Number of Replicate Runs = '" + ans.trim() + "' contains illegal characters for an integer number.", "Error Message",
                            JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (max_num_rep_runs < MIN_MAX_NUM_REP_RUNS || max_num_rep_runs > PARAMS.TEXAS_MODEL_NRP) {
                    JOptionPane.showMessageDialog(null, "Maximum Number of Replicate Runs = " + max_num_rep_runs + " is less than " + MIN_MAX_NUM_REP_RUNS + " or greater than "
                            + PARAMS.TEXAS_MODEL_NRP + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                REPTOLAction(tolerance, max_num_rep_runs);
                break;
            }
            catch (Exception e) {
                System.out.println("REPTOLAction exception error");
                addLineToLog("REPTOLAction exception error");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }
        } // end of while ( true )

    } // end of method REPTOLActionListener

    boolean runCommand(String commandName, java.util.List<String> commands) {
        // return value true if error or warning needing to stop detected
        String asterisks;
        BufferedReader bufferedReader;
        String command;
        File errorFile;
        String errorFileName;
        String errorMessage;
        FileInputStream fileInputStream;
        int i;
        InputStreamReader inputStreamReader;
        boolean isErrorOrWarningNeedingToStop = false;
        int line;
        String message;
        String newLine;
        ProcessBuilder processBuilder;
        String readLine;
        String separator;
        File warningFile;
        String warningFileName;
        String warningMessage;
        String warningString;

        try {
            if (commands == null) {
                JOptionPane.showMessageDialog(null, "runCommand commands is null", "runCommand Error", JOptionPane.ERROR_MESSAGE);
                if (process != null) {
                    process.destroy();
                }
                process = null;
                return true;
            }

            if (commands.isEmpty()) {
                JOptionPane.showMessageDialog(null, "runCommand commands is empty", "runCommand Error", JOptionPane.ERROR_MESSAGE);
                if (process != null) {
                    process.destroy();
                }
                process = null;
                return true;
            }

            command = "";
            separator = "";
            for (i = 0; i < commands.size(); i++) {
                command = command + separator + commands.get(i);
                separator = " ";
            }

            if (texas.ProjectDirectory == null || texas.ProjectDirectory.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid Project Directory", JOptionPane.ERROR_MESSAGE);
                if (process != null) {
                    process.destroy();
                }
                process = null;
                return true;
            }

            deleteErrorTxtFileInProjectDirectory();
            deleteWarningTxtFileInProjectDirectory();

            currentDateTime.setTime(new java.util.Date());
            message = "********** begin    " + commandName + " at " + currentDateTime.getTime().toString() + " **********";
            asterisks = "****************************************************************";
            while (asterisks.trim().length() < message.trim().length()) {
                asterisks = asterisks + "*";
            }
            addLineToLog(" ");
            addLineToLog(asterisks);
            addLineToLog(message);
            addLineToLog(asterisks);
            addLineToLog("********** " + command);

            // process = Runtime.getRuntime().exec( command,null,new File( texas.ProjectDirectory )
            // );
            processBuilder = new ProcessBuilder(commands);
            processBuilder.directory(new File(texas.ProjectDirectory));
            processBuilder.redirectErrorStream(true);
            process = processBuilder.start();

            try {
                inputStreamReader = new InputStreamReader(process.getInputStream(), "ASCII");
                bufferedReader = new BufferedReader(inputStreamReader);
                while ((readLine = bufferedReader.readLine()) != null) {
                    if (readLine.trim().length() > 0) {
                        addLineToLog(readLine);
                    }
                }
                bufferedReader.close();
            }
            catch (Exception e) {
                System.err.println("runCommand exception error reading input stream");
                addLineToLog("runCommand exception error reading input stream");
                System.err.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                return true;
            }

            currentDateTime.setTime(new java.util.Date());
            addLineToLog("********** end      " + commandName + " at " + currentDateTime.getTime().toString() + " **********");

            // if error.txt created then open, read, and display to user in dialog
            errorFileName = fileNameWithProjectDirectory("error.txt");
            errorFile = new File(errorFileName);

            if (errorFile.exists()) {
                isErrorOrWarningNeedingToStop = true;
                currentDateTime.setTime(new java.util.Date());
                addLineToLog("********** errors   " + commandName + " at " + currentDateTime.getTime().toString() + " **********");
                if (!errorFile.isFile()) {
                    JOptionPane.showMessageDialog(null, "Error file = '" + errorFileName + "' is not a file.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    if (process != null) {
                        process.destroy();
                    }
                    process = null;
                    return true;
                }

                if (!errorFile.canRead()) {
                    JOptionPane.showMessageDialog(null, "Error file = '" + errorFileName + "' is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    if (process != null) {
                        process.destroy();
                    }
                    process = null;
                    return true;
                }

                try {
                    fileInputStream = new FileInputStream(errorFileName);
                    inputStreamReader = new InputStreamReader(fileInputStream, "ASCII");
                    bufferedReader = new BufferedReader(inputStreamReader);

                    errorMessage = "";
                    newLine = "";

                    while ((readLine = bufferedReader.readLine()) != null) {
                        readLine = readLine.trim();
                        if (readLine.length() > 0) {
                            addLineToLog("********** error    " + commandName + " ** " + readLine);
                            errorMessage = errorMessage + newLine + readLine;
                            newLine = "\n";
                        }
                    }

                    bufferedReader.close();
                    bufferedReader = null;
                    inputStreamReader.close();
                    inputStreamReader = null;
                    fileInputStream.close();
                    fileInputStream = null;

                    if (errorMessage.trim().length() > 0) {
                        JOptionPane.showMessageDialog(null, "Error Message from " + commandName + ":\n" + errorMessage, "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                }
                catch (Exception e) {
                    System.err.println("runCommand exception error reading errorFileName");
                    addLineToLog("runCommand exception error reading errorFileName");
                    System.err.println(e.getLocalizedMessage());
                    addLineToLog(e.getLocalizedMessage());
                    if (process != null) {
                        process.destroy();
                    }
                    process = null;
                    return true;
                }
            } // end of if ( errorFile.exists() )

            // if warning.txt created then open, read, display to user in dialog, and ask whether to
            // continue or stop processing
            warningFileName = fileNameWithProjectDirectory("warning.txt");
            warningFile = new File(warningFileName);

            if (warningFile.exists()) {
                currentDateTime.setTime(new java.util.Date());
                addLineToLog("********** warnings " + commandName + " at " + currentDateTime.getTime().toString() + " **********");
                if (!warningFile.isFile()) {
                    JOptionPane.showMessageDialog(null, "Warning file = '" + warningFileName + "' is not a file.", "Warning Message", JOptionPane.WARNING_MESSAGE);
                    if (process != null) {
                        process.destroy();
                    }
                    process = null;
                    return true;
                }

                if (!warningFile.canRead()) {
                    JOptionPane.showMessageDialog(null, "Warning file = '" + warningFileName + "' is not readable.", "Warning Message", JOptionPane.WARNING_MESSAGE);
                    if (process != null) {
                        process.destroy();
                    }
                    process = null;
                    return true;
                }

                try {
                    fileInputStream = new FileInputStream(warningFileName);
                    inputStreamReader = new InputStreamReader(fileInputStream, "ASCII");
                    bufferedReader = new BufferedReader(inputStreamReader);

                    line = 0;
                    newLine = "";
                    warningMessage = "";

                    while ((readLine = bufferedReader.readLine()) != null) {
                        if (!cb_ignoreWarn.isSelected()) {
                            readLine = readLine.trim();
                            if (readLine.length() > 0) {
                                line++;
                                if (line <= MAXWRN) {
                                    addLineToLog("********** warning  " + commandName + " ** " + readLine);
                                    warningMessage = warningMessage + newLine + readLine;
                                }
                                else if (line == MAXWRN + 1) {
                                    warningString = " ** maximum number of warning messages reached **";
                                    warningMessage = warningMessage + newLine + warningString;
                                    addLineToLog("********** warning  " + commandName + warningString);
                                    if (commandName.equals("simpro")) {
                                        warningString = " ** see file " + texas.SIMERR_file + " for all warning messages **";
                                    }
                                    else {
                                        warningString = " ** see log for all warning messages **";
                                    }
                                    warningMessage = warningMessage + "\n" + warningString;
                                    addLineToLog("********** warning  " + commandName + warningString);
                                }
                                newLine = "\n";
                            }
                        }
                    }

                    bufferedReader.close();
                    bufferedReader = null;
                    inputStreamReader.close();
                    inputStreamReader = null;
                    fileInputStream.close();
                    fileInputStream = null;

                    if (warningMessage.trim().length() > 0 && !cb_ignoreWarn.isSelected()) {
                        if (errorFile.exists()) {
                            JOptionPane.showMessageDialog(null, "Warning Message from " + commandName + ":\n" + warningMessage, "Warning Message", JOptionPane.WARNING_MESSAGE);
                            if (process != null) {
                                process.destroy();
                            }
                            process = null;
                            return true;
                        }
                        else {
                            int ret = JOptionPane.showConfirmDialog(null, "Warning Message from " + commandName + ":\n" + warningMessage + "\n\nDo you want to continue?", "Warning",
                                    JOptionPane.YES_NO_OPTION);
                            if (ret == JOptionPane.YES_OPTION) {
                                if (process != null) {
                                    process.destroy();
                                }
                                process = null;
                                return false;
                            }
                            else {
                                if (process != null) {
                                    process.destroy();
                                }
                                process = null;
                                return true;
                            }
                        }
                    }
                }
                catch (Exception e) {
                    System.err.println("runCommand exception error reading warningFileName");
                    addLineToLog("runCommand exception error reading warningFileName");
                    System.err.println(e.getLocalizedMessage());
                    addLineToLog(e.getLocalizedMessage());
                    if (process != null) {
                        process.destroy();
                    }
                    process = null;
                    return true;
                }

                if (cb_ignoreWarn.isSelected()) {
                    addLineToLog("********** warnings ignored per user request");
                    currentDateTime.setTime(new java.util.Date());
                    addLineToLog("********** warnings " + commandName + " at " + currentDateTime.getTime().toString() + " **********");
                }
            } // end of if ( warningFile.exists() )
        } // end of try
        catch (Exception e) {
            System.err.println("runCommand exception error");
            addLineToLog("runCommand exception error");
            System.err.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            if (process != null) {
                process.destroy();
            }
            process = null;
            return true;
        }

        if (process != null) {
            try {
                process.waitFor();
                process.destroy();
                process = null;
            }
            catch (Exception e) {
                System.err.println("runCommand exception error waitFor and destroy");
                addLineToLog("runCommand exception error waitFor and destroy");
                System.err.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                return true;
            }
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method runCommand

    void saveAsProcess() {
        String title = "TEXAS Model For Intersection Traffic Version - " + IVERSN;

        JFileChooser fileChooser = new JFileChooser();

        try {
            File f = new File(new File(".").getCanonicalPath());
            fileChooser.setCurrentDirectory(f);
        }
        catch (IOException e) {}

        fileChooser.setDialogTitle("Save Log As");

        int res;
        File f;
        String fname;
        while (true) {
            res = fileChooser.showSaveDialog(null);

            if (res == JFileChooser.CANCEL_OPTION) {
                return;
            }

            f = fileChooser.getSelectedFile();

            if (f == null || f.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            fname = f.getName();

            if (!fname.endsWith(".txt")) {
                fname = fname.concat(".txt");
                f = new File("" + fileChooser.getCurrentDirectory() + File.separator + fname);
                fname = f.getName();
            }

            if (f.exists()) {
                if (JOptionPane.showConfirmDialog(null, "txt File '" + f.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    break;
                }
                else {
                    continue;
                }
            }

            break;
        }

        BufferedWriter bufferedwriter = null;
        PrintWriter printwriter = null;

        try {
            bufferedwriter = new BufferedWriter(new FileWriter(f));
            printwriter = new PrintWriter(bufferedwriter);
            printwriter.println(title + " - Log");
            printwriter.println(" ");
            printwriter.print(logTextArea.getText());
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        finally {
            printwriter.close();
        }
    } // end of method saveAsProcess

    boolean setProjectFileNames(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        File fileFile;
        String fileName;
        boolean isErrorOrWarningNeedingToStop = false;
        char c;
        int i;
        int n;

        texas.ProjectName = text_ProjName.getText().trim();
        n = texas.ProjectName.trim().length();
        for (i = 0; i < n; i++) {
            c = texas.ProjectName.charAt(i);
            // any character
            if (Character.isLetterOrDigit(c)) {
                continue;
            }
            if (i > 0 && i < n - 1) {
                // middle characters
                if (c == '-') {
                    continue;
                }
                if (c == '_') {
                    continue;
                }
            }
            JOptionPane.showMessageDialog(null, "Project Name = '" + texas.ProjectName + "' contains illegal character '" + c + "' at character " + (i + 1)
                    + ".\nProjct Names may have letters and digits anywhere and underbar and minus only in the middle.", "Error Message", JOptionPane.ERROR_MESSAGE);
            isErrorOrWarningNeedingToStop = true;
        }
        if (isErrorOrWarningNeedingToStop) {
            return true;
        }

        if (replicateNumber == 0) {
            if (texas.ProjectName.trim().length() == 0) {
                texas.DISDAT_file = texas.DISDAT_name.trim();
                texas.DISPAR_file = texas.DISPAR_name.trim();
                texas.DVPLIST_file = texas.DVPLIST_name.trim();
                texas.EMERR_file = texas.EMERR_name.trim();
                texas.EMLIST_file = texas.EMLIST_name.trim();
                texas.EMSTA_file = texas.EMSTA_name.trim();
                texas.EMSTAFIN_file = texas.EMSTAFIN_name.trim();
                texas.EMSTAVE_file = texas.EMSTAVE_name.trim();
                texas.FORT8_file = texas.FORT8_name.trim();
                texas.FORT9_file = texas.FORT9_name.trim();
                texas.GDV_file = texas.GDV_name.trim();
                texas.GDVDATA_file = texas.GDVDATA_name.trim();
                texas.GEOLIST_file = texas.GEOLIST_name.trim();
                texas.GEOPLOT_file = texas.GEOPLOT_name.trim();
                texas.PARAMS_file = texas.PARAMS_name.trim();
                texas.POSDAT_file = texas.POSDAT_name.trim();
                texas.SIM_file = texas.SIM_name.trim();
                texas.SIMDATA_file = texas.SIMDATA_name.trim();
                texas.SIMERR_file = texas.SIMERR_name.trim();
                texas.SIMPLST_file = texas.SIMPLST_name.trim();
                texas.SIMSLST_file = texas.SIMSLST_name.trim();
                texas.SIMSTAT_file = texas.SIMSTAT_name.trim();
                texas.SPRDSHT_file = texas.SPRDSHT_name.trim();
                texas.SSAM_file = texas.SSAM_name.trim();
            }
            else {
                texas.DISDAT_file = texas.ProjectName.trim() + "_" + texas.DISDAT_name.trim();
                texas.DISPAR_file = texas.ProjectName.trim() + "_" + texas.DISPAR_name.trim();
                texas.DVPLIST_file = texas.ProjectName.trim() + "_" + texas.DVPLIST_name.trim();
                texas.EMERR_file = texas.ProjectName.trim() + "_" + texas.EMERR_name.trim();
                texas.EMLIST_file = texas.ProjectName.trim() + "_" + texas.EMLIST_name.trim();
                texas.EMSTA_file = texas.ProjectName.trim() + "_" + texas.EMSTA_name.trim();
                texas.EMSTAFIN_file = texas.ProjectName.trim() + "_" + texas.EMSTAFIN_name.trim();
                texas.EMSTAVE_file = texas.ProjectName.trim() + "_" + texas.EMSTAVE_name.trim();
                texas.FORT8_file = texas.ProjectName.trim() + "_" + texas.FORT8_name.trim();
                texas.FORT9_file = texas.ProjectName.trim() + "_" + texas.FORT9_name.trim();
                texas.GDV_file = texas.ProjectName.trim() + "_" + texas.GDV_name.trim();
                texas.GDVDATA_file = texas.ProjectName.trim() + "_" + texas.GDVDATA_name.trim();
                texas.GEOLIST_file = texas.ProjectName.trim() + "_" + texas.GEOLIST_name.trim();
                texas.GEOPLOT_file = texas.ProjectName.trim() + "_" + texas.GEOPLOT_name.trim();
                texas.PARAMS_file = texas.ProjectName.trim() + "_" + texas.PARAMS_name.trim();
                texas.POSDAT_file = texas.ProjectName.trim() + "_" + texas.POSDAT_name.trim();
                texas.SIM_file = texas.ProjectName.trim() + "_" + texas.SIM_name.trim();
                texas.SIMDATA_file = texas.ProjectName.trim() + "_" + texas.SIMDATA_name.trim();
                texas.SIMERR_file = texas.ProjectName.trim() + "_" + texas.SIMERR_name.trim();
                texas.SIMPLST_file = texas.ProjectName.trim() + "_" + texas.SIMPLST_name.trim();
                texas.SIMSLST_file = texas.ProjectName.trim() + "_" + texas.SIMSLST_name.trim();
                texas.SIMSTAT_file = texas.ProjectName.trim() + "_" + texas.SIMSTAT_name.trim();
                texas.SPRDSHT_file = texas.ProjectName.trim() + "_" + texas.SPRDSHT_name.trim();
                texas.SSAM_file = texas.ProjectName.trim() + "_" + texas.SSAM_name.trim();
            }
        }
        else if (replicateNumber >= 1 && replicateNumber <= PARAMS.TEXAS_MODEL_NRP) {
            if (texas.ProjectName.trim().length() == 0) {
                texas.DISDAT_file = addReplicateName(texas.DISDAT_name, "r" + replicateNumber).trim();
                texas.DISPAR_file = texas.DISPAR_name.trim();
                texas.DVPLIST_file = addReplicateName(texas.DVPLIST_name, "r" + replicateNumber).trim();
                texas.EMERR_file = texas.EMERR_name.trim();
                texas.EMLIST_file = texas.EMLIST_name.trim();
                texas.EMSTA_file = texas.EMSTA_name.trim();
                texas.EMSTAFIN_file = texas.EMSTAFIN_name.trim();
                texas.EMSTAVE_file = texas.EMSTAVE_name.trim();
                texas.FORT8_file = addReplicateName(texas.FORT8_name, "rep").trim();
                texas.FORT9_file = addReplicateName(texas.FORT9_name, "r" + replicateNumber).trim();
                texas.GDV_file = addReplicateName(texas.GDV_name, "rep").trim();
                texas.GDVDATA_file = texas.GDVDATA_name.trim();
                texas.GEOLIST_file = addReplicateName(texas.GEOLIST_name, "rep").trim();
                texas.GEOPLOT_file = addReplicateName(texas.GEOPLOT_name, "rep").trim();
                texas.PARAMS_file = addReplicateName(texas.PARAMS_name, "rep").trim();
                texas.POSDAT_file = addReplicateName(texas.POSDAT_name, "r" + replicateNumber).trim();
                texas.SIM_file = addReplicateName(texas.SIM_name, "rep").trim();
                texas.SIMDATA_file = texas.SIMDATA_name.trim();
                texas.SIMERR_file = addReplicateName(texas.SIMERR_name, "r" + replicateNumber).trim();
                texas.SIMPLST_file = addReplicateName(texas.SIMPLST_name, "r" + replicateNumber).trim();
                texas.SIMSLST_file = addReplicateName(texas.SIMSLST_name, "rep").trim();
                texas.SIMSTAT_file = addReplicateName(texas.SIMSTAT_name, "r" + replicateNumber).trim();
                texas.SPRDSHT_file = texas.SPRDSHT_name.trim();
                texas.SSAM_file = addReplicateName(texas.SSAM_name, "r" + replicateNumber).trim();
            }
            else {
                texas.DISDAT_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.DISDAT_name, "r" + replicateNumber).trim();
                texas.DISPAR_file = texas.ProjectName.trim() + "_" + texas.DISPAR_name.trim();
                texas.DVPLIST_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.DVPLIST_name, "r" + replicateNumber).trim();
                texas.EMERR_file = texas.ProjectName.trim() + "_" + texas.EMERR_name.trim();
                texas.EMLIST_file = texas.ProjectName.trim() + "_" + texas.EMLIST_name.trim();
                texas.EMSTA_file = texas.ProjectName.trim() + "_" + texas.EMSTA_name.trim();
                texas.EMSTAFIN_file = texas.ProjectName.trim() + "_" + texas.EMSTAFIN_name.trim();
                texas.EMSTAVE_file = texas.ProjectName.trim() + "_" + texas.EMSTAVE_name.trim();
                texas.FORT8_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.FORT8_name, "rep").trim();
                texas.FORT9_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.FORT9_name, "r" + replicateNumber).trim();
                texas.GDV_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.GDV_name, "rep").trim();
                texas.GDVDATA_file = texas.ProjectName.trim() + "_" + texas.GDVDATA_name.trim();
                texas.GEOLIST_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.GEOLIST_name, "rep").trim();
                texas.GEOPLOT_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.GEOPLOT_name, "rep").trim();
                texas.PARAMS_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.PARAMS_name, "rep").trim();
                texas.POSDAT_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.POSDAT_name, "r" + replicateNumber).trim();
                texas.SIM_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.SIM_name, "rep").trim();
                texas.SIMDATA_file = texas.ProjectName.trim() + "_" + texas.SIMDATA_name.trim();
                texas.SIMERR_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.SIMERR_name, "r" + replicateNumber).trim();
                texas.SIMPLST_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.SIMPLST_name, "r" + replicateNumber).trim();
                texas.SIMSLST_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.SIMSLST_name, "rep").trim();
                texas.SIMSTAT_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.SIMSTAT_name, "r" + replicateNumber).trim();
                texas.SPRDSHT_file = texas.ProjectName.trim() + "_" + texas.SPRDSHT_name.trim();
                texas.SSAM_file = texas.ProjectName.trim() + "_" + addReplicateName(texas.SSAM_name, "r" + replicateNumber).trim();
            }
        }
        else {
            JOptionPane
                    .showMessageDialog(null, "Replicate Number = " + replicateNumber + " is less than 1 or greater than " + PARAMS.TEXAS_MODEL_NRP + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        try {
            fileFile = new File(fileNameWithProjectDirectory(texas.GDVDATA_file));
            fileName = fileFile.getCanonicalPath();
            if (fileName == null) {
                JOptionPane.showMessageDialog(null, "Invalid GDVDATA File Name.", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                return true;
            }
            if (fileName.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "Invalid GDVDATA File Name.", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                return true;
            }
            if (fileName.trim().length() > PARAMS.TEXAS_MODEL_MAXFIL) {
                JOptionPane.showMessageDialog(null, "GDVDATA File Name length = " + fileName.trim().length() + " is greater than maximum = " + PARAMS.TEXAS_MODEL_MAXFIL + ".\n" + fileName,
                        "Invalid GDVDATA File Name", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }
        catch (Exception e) {
            System.err.println("ProjectDirSelect exception error fileFile.getCanonicalPath for GDVDATA");
            addLineToLog("ProjectDirSelect exception error fileFile.getCanonicalPath for GDVDATA");
            System.err.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        try {
            fileFile = new File(fileNameWithProjectDirectory(texas.SIMDATA_file));
            fileName = fileFile.getCanonicalPath();
            if (fileName == null) {
                JOptionPane.showMessageDialog(null, "Invalid SIMDATA File Name.", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                return true;
            }
            if (fileName.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "Invalid SIMDATA File Name.", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                return true;
            }
            if (fileName.trim().length() > PARAMS.TEXAS_MODEL_MAXFIL) {
                JOptionPane.showMessageDialog(null, "SIMDATA File Name length = " + fileName.trim().length() + " is greater than maximum = " + PARAMS.TEXAS_MODEL_MAXFIL + ".\n" + fileName,
                        "Invalid SIMDATA File Name", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }
        catch (Exception e) {
            System.err.println("ProjectDirSelect exception error fileFile.getCanonicalPath for SIMDATA");
            addLineToLog("ProjectDirSelect exception error fileFile.getCanonicalPath for SIMDATA");
            System.err.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return false;
    } // end of method setProjectFileNames

    boolean SIMCONVAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        boolean isErrorOrWarningNeedingToStop = false;
        java.util.List<String> simconvCommands;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMDATA_file))) {
                JOptionPane.showMessageDialog(null, "SIMDATA file = '" + texas.SIMDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            simconvCommands = new ArrayList<String>();
            simconvCommands.add(texas.TexasExeDirectory + File.separator + "simconv.exe");
            simconvCommands.add("PRE+" + texas.SIMDATA_file);
            simconvCommands.add("C+" + texas.SIM_file);
            simconvCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);

            isErrorOrWarningNeedingToStop = runCommand("simconv", simconvCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("SIMCONVAction exception error");
            addLineToLog("SIMCONVAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method SIMCONVAction

    boolean SIMPROAction(int replicateNumber) {
        // return value true if error or warning needing to stop detected
        long FORT8_fileLastModifiedTime;
        long FORT9_fileLastModifiedTime;
        long GDVDATA_fileLastModifiedTime;
        boolean isErrorOrWarningNeedingToStop = false;
        boolean processDVPRO = false;
        boolean processGEOPRO = false;
        boolean processSIMCONV = false;
        long SIM_fileLastModifiedTime;
        long SIMDATA_fileLastModifiedTime;
        java.util.List<String> simproCommands;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.GDVDATA_file))) {
                JOptionPane.showMessageDialog(null, "GDVDATA file = '" + texas.GDVDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT8_file))) {
                FORT8_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.FORT8_file));
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                if (FORT8_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime > FORT8_fileLastModifiedTime) {
                    processGEOPRO = true;
                }
            }
            else {
                processGEOPRO = true;
            }

            if (processGEOPRO) {
                isErrorOrWarningNeedingToStop = GEOPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT8_file))) {
                JOptionPane.showMessageDialog(null, "FORT8 file = '" + texas.FORT8_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of geopro or gdvpro.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT9_file))) {
                FORT9_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.FORT9_file));
                GDVDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.GDVDATA_file));
                if (FORT9_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (GDVDATA_fileLastModifiedTime > FORT9_fileLastModifiedTime) {
                    processDVPRO = true;
                }
            }
            else {
                processDVPRO = true;
            }

            if (processDVPRO) {
                isErrorOrWarningNeedingToStop = DVPROAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.FORT9_file))) {
                JOptionPane.showMessageDialog(null, "FORT9 file = '" + texas.FORT9_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of dvpro or gdvpro.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMDATA_file))) {
                JOptionPane.showMessageDialog(null, "SIMDATA file = '" + texas.SIMDATA_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIM_file))) {
                SIM_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.SIM_file));
                SIMDATA_fileLastModifiedTime = fileLastModifiedTime(fileNameWithProjectDirectory(texas.SIMDATA_file));
                if (SIM_fileLastModifiedTime < 0) {
                    return true;
                }
                if (SIMDATA_fileLastModifiedTime < 0) {
                    return true;
                }
                if (SIMDATA_fileLastModifiedTime > SIM_fileLastModifiedTime) {
                    processSIMCONV = true;
                }
            }
            else {
                processSIMCONV = true;
            }

            if (processSIMCONV) {
                isErrorOrWarningNeedingToStop = SIMCONVAction(replicateNumber);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIM_file))) {
                JOptionPane.showMessageDialog(null, "SIM file = '" + texas.SIM_file + "' does not exist or is not a file or is not readable.\nCheck correct execution of gdvsim and simconv.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            simproCommands = new ArrayList<String>();
            simproCommands.add(texas.TexasExeDirectory + File.separator + "simpro.exe");
            simproCommands.add("T8+" + texas.FORT8_file);
            simproCommands.add("T9+" + texas.FORT9_file);
            simproCommands.add("I+" + texas.SIM_file);
            simproCommands.add("L+" + texas.SIMPLST_file);
            simproCommands.add("STA+" + texas.SIMSTAT_file);
            simproCommands.add("PVA+" + texas.POSDAT_file);
            simproCommands.add("ERR+" + texas.SIMERR_file);
            simproCommands.add("SSAM+" + texas.SSAM_file);
            simproCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);
            if (replicateNumber >= 1 && replicateNumber <= PARAMS.TEXAS_MODEL_NRP) {
                simproCommands.add("REP+" + replicateNumber);
            }

            isErrorOrWarningNeedingToStop = runCommand("simpro", simproCommands);
        } // end of try
        catch (Exception e)

        {
            System.out.println("SIMPROAction exception error");
            addLineToLog("SIMPROAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method SIMPROAction

    boolean SIMSTAAction(int tolerance) {
        // return value true if error or warning needing to stop detected
        boolean isErrorOrWarningNeedingToStop = false;
        java.util.List<String> simstaCommands;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(1);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (tolerance < 0 || tolerance > PARAMS.TEXAS_MODEL_MAXTOL) {
                JOptionPane.showMessageDialog(null, "Tolerance = " + tolerance + " is less than 0 or greater than " + PARAMS.TEXAS_MODEL_MAXTOL + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SIMSTAT_file))) {
                JOptionPane.showMessageDialog(null, "SIMSTAT file = '" + texas.SIMSTAT_file
                        + "' does not exist or is not a file or is not readable.\nCheck if Spreadsheet Statistics File is requested in gdvsim Simulation Data and check correct execution of simpro.",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            simstaCommands = new ArrayList<String>();
            simstaCommands.add(texas.TexasExeDirectory + File.separator + "simsta.exe");
            simstaCommands.add("STA+" + texas.SIMSTAT_file);
            simstaCommands.add("L+" + texas.SIMSLST_file);
            simstaCommands.add("SS+" + texas.SPRDSHT_file);
            simstaCommands.add("SYS_DAT+" + texas.TexasSysDatDirectory + File.separator);
            if (tolerance >= 1 && tolerance <= PARAMS.TEXAS_MODEL_MAXTOL) {
                simstaCommands.add("TOL+" + tolerance);
            }

            isErrorOrWarningNeedingToStop = runCommand("simsta", simstaCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("SIMSTAAction exception error");
            addLineToLog("SIMSTAAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method SIMSTAAction

    boolean SSAMAction() {
        // return value true if error or warning needing to stop detected
        java.util.List<String> ssamCommands;
        boolean isErrorOrWarningNeedingToStop = false;
        String ssamFiles;
        String ssamProgram;

        try {
            if (File.separator.equals("/")) {
                ssamProgram = "/usr/FHWA/SSAM/SSAM.jar";
                if (!fileExistAndIsFileAndIsReadable(ssamProgram)) {
                    JOptionPane.showMessageDialog(null, "SSAM program file '" + ssamProgram + "' does not exist or is not a file or is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else if (File.separator.equals(":")) {
                JOptionPane.showMessageDialog(null, "SSAM is not available for the Macintosh.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
            else if (File.separator.equals("\\")) {
                ssamProgram = "c:\\Program Files\\FHWA\\SSAM\\SSAM.jar";
                if (!fileExistAndIsFileAndIsReadable(ssamProgram)) {
                    JOptionPane.showMessageDialog(null, "SSAM program file '" + ssamProgram + "' does not exist or is not a file or is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else {
                JOptionPane.showMessageDialog(null, "Unknown operating system.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }
            ssamFiles = texas.SSAM_file;

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SSAM_file))) {
                isErrorOrWarningNeedingToStop = setProjectFileNames(1);
                if (isErrorOrWarningNeedingToStop) {
                    return true;
                }
                ssamFiles = ssamFiles + "' or '" + texas.SSAM_file;
                if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(texas.SSAM_file))) {
                    JOptionPane
                            .showMessageDialog(
                                    null,
                                    "SSAM file = '"
                                            + ssamFiles
                                            + "' do not exist or are not a file or are not readable.\nCheck if Surrogate Safety Assessment Methodology File is requested in gdvsim Simulation Data and check correct execution of simpro and/or reptol/reprun.",
                                    "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }

            ssamCommands = new ArrayList<String>();
            ssamCommands.add("java");
            ssamCommands.add("-jar");
            ssamCommands.add(ssamProgram);

            isErrorOrWarningNeedingToStop = runCommand("ssam", ssamCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("SSAMAction exception error");
            addLineToLog("SSAMAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method SSAMAction

    boolean STAPLOTAction(int replicateNumber, String dataType) {
        // return value true if error or warning needing to stop detected
        java.util.List<String> staplotCommands;
        String checkMessage = "";
        String dataFile = "";
        boolean isErrorOrWarningNeedingToStop = false;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(replicateNumber);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (dataType.equals("r")) {
                dataFile = texas.SPRDSHT_file;
                checkMessage = "check correct execution of simpro, reptol/reprun, and simsta.";
            }
            else if (dataType.equals("1")) {
                dataFile = texas.SIMSTAT_file;
                checkMessage = "check correct execution of simpro.";
            }
            else {
                JOptionPane.showMessageDialog(null, "dataType = '" + dataType + "' is not 'r' or '1'.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(fileNameWithProjectDirectory(dataFile))) {
                JOptionPane.showMessageDialog(null, "STAPLOT file = '" + dataFile
                        + "' does not exist or is not a file or is not readable.\nCheck if Spreadsheet Statistics File is set in gdvsim Simulation Data and " + checkMessage, "Error Message",
                        JOptionPane.ERROR_MESSAGE);
                return true;
            }

            staplotCommands = new ArrayList<String>();
            staplotCommands.add("java");
            staplotCommands.add("-jar");
            staplotCommands.add(texas.TexasExeDirectory + File.separator + "staplot.jar");
            staplotCommands.add(dataFile);

            isErrorOrWarningNeedingToStop = runCommand("staplot", staplotCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("STAPLOTAction exception error");
            addLineToLog("STAPLOTAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method STAPLOTAction

    boolean TO80DAction(String inputFileName, String outputFileName) {
        // return value true if error or warning needing to stop detected
        boolean isErrorOrWarningNeedingToStop = false;
        java.util.List<String> to80dCommands;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            if (!fileExistAndIsFileAndIsReadable(inputFileName)) {
                JOptionPane.showMessageDialog(null, "TO80D input file = '" + inputFileName + "' does not exist or is not a file or is not readable.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            to80dCommands = new ArrayList<String>();
            to80dCommands.add(texas.TexasUtilityDirectory + File.separator + "to80d.exe");
            to80dCommands.add(inputFileName);
            to80dCommands.add(outputFileName);

            isErrorOrWarningNeedingToStop = runCommand("to80d", to80dCommands);
        } // end of try
        catch (Exception e) {
            System.out.println("TO80DAction exception error");
            addLineToLog("TO80DAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method TO80DAction

    void TO80DActionListener() {
        File f;
        JFileChooser fileChooser = new JFileChooser();
        File inputFile;
        String inputFileName = null;
        File outputFile = null;
        String outputFileName = null;
        int retInput;
        int retOutput;

        try {
            if (texas.ProjectDirectory != null) {
                if (texas.ProjectDirectory.trim().length() > 0) {
                    f = new File(texas.ProjectDirectory);
                }
                else {
                    f = new File(new File(".").getCanonicalPath());
                }
            }
            else {
                f = new File(new File(".").getCanonicalPath());
            }
        }
        catch (Exception e) {
            System.out.println("TO80DActionListener exception error file.getCanonicalPath");
            addLineToLog("TO80DActionListener exception error file.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        try {
            fileChooser.setCurrentDirectory(f);
        }
        catch (Exception e) {
            System.out.println("TO80DActionListener exception error fileChooser.setCurrentDirectory");
            addLineToLog("TO80DActionListener exception error fileChooser.setCurrentDirectory");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Input File");

        while (true) {
            retInput = fileChooser.showOpenDialog(null);

            if (retInput == JFileChooser.CANCEL_OPTION) {
                return;
            }

            inputFile = fileChooser.getSelectedFile();

            try {
                if (inputFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (inputFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.out.println("TO80DActionListener exception error inputFile.getCanonicalPath");
                addLineToLog("TO80DActionListener exception error inputFile.getCanonicalPath");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            if (!inputFile.exists()) {
                JOptionPane.showMessageDialog(null, "Input File Does Not Exist", "Input File Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        try {
            inputFileName = inputFile.getCanonicalPath();
        }
        catch (Exception e) {
            System.out.println("TO80DActionListener exception error inputFile.getCanonicalPath");
            addLineToLog("TO80DActionListener exception error inputFile.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        fileChooser.setDialogTitle("Select Output File");

        while (true) {
            retOutput = fileChooser.showOpenDialog(null);

            if (retOutput == JFileChooser.CANCEL_OPTION) {
                return;
            }

            outputFile = fileChooser.getSelectedFile();

            try {
                if (outputFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (outputFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.out.println("TO80DActionListener exception error outputFile.getCanonicalPath");
                addLineToLog("TO80DActionListener exception error outputFile.getCanonicalPath");
                System.out.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            break;
        }

        try {
            outputFileName = outputFile.getCanonicalPath();
        }
        catch (Exception e) {
            System.out.println("TO80DActionListener exception error outputFile.getCanonicalPath");
            addLineToLog("TO80DActionListener exception error outputFile.getCanonicalPath");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return;
        }

        TO80DAction(inputFileName, outputFileName);
    } // end of method TO80DActionListener

    boolean VERSIONSAction() {
        // return value true if error or warning needing to stop detected
        String asterisks;
        BufferedReader bufferedReader;
        FileInputStream fileInputStream;
        InputStreamReader inputStreamReader;
        boolean isErrorOrWarningNeedingToStop = false;
        String fileNames[] = { "texas.ver", "disfit.ver", "dispre_j.ver", "dispro_j.ver", "dvpro.ver", "empro.ver", "from80d.ver", "gdvconv.ver", "gdvdata.ver", "gdvsim.ver", "geoplotj.ver",
                "geopro.ver", "simconv.ver", "simdata.ver", "simpro.ver", "simsta.ver", "staplot.ver", "to80d.ver" };
        String message;
        String newLine;
        String readLine;

        try {
            isErrorOrWarningNeedingToStop = setProjectFileNames(0);
            if (isErrorOrWarningNeedingToStop) {
                return true;
            }

            currentDateTime.setTime(new java.util.Date());
            message = "********** begin    versions at " + currentDateTime.getTime().toString() + " **********";
            asterisks = "****************************************************************";
            while (asterisks.trim().length() < message.trim().length()) {
                asterisks = asterisks + "*";
            }
            addLineToLog(" ");
            addLineToLog(asterisks);
            addLineToLog(message);
            addLineToLog(asterisks);
            addLineToLog("TEXAS Model for Intersection Traffic Version Numbers:");
            message = "";
            newLine = "";

            for (String fileName : fileNames) {
                if (fileExistAndIsFileAndIsReadable(texas.TexasSysDatDirectory + File.separator + fileName)) {
                    fileInputStream = new FileInputStream(texas.TexasSysDatDirectory + File.separator + fileName);
                    inputStreamReader = new InputStreamReader(fileInputStream, "ASCII");
                    bufferedReader = new BufferedReader(inputStreamReader);

                    while ((readLine = bufferedReader.readLine()) != null) {
                        readLine = readLine.trim();
                        if (readLine.length() > 0) {
                            addLineToLog(readLine);
                            message = message + newLine + readLine;
                            newLine = "\n";
                        }
                    }

                    bufferedReader.close();
                    bufferedReader = null;
                    inputStreamReader.close();
                    inputStreamReader = null;
                    fileInputStream.close();
                    fileInputStream = null;
                }
            } // end for ( int i = 0 ; i <= fileNames.length ; i++ )

            if (message.trim().length() > 0) {
                JOptionPane.showMessageDialog(null, "TEXAS Model for Intersection Traffic Version Numbers:\n" + message, "Versions", JOptionPane.PLAIN_MESSAGE);
            }

            currentDateTime.setTime(new java.util.Date());
            addLineToLog("********** end      versions at " + currentDateTime.getTime().toString() + " **********");
        } // end of try
        catch (Exception e) {
            System.out.println("VERSIONSAction exception error");
            addLineToLog("VERSIONSAction exception error");
            System.out.println(e.getLocalizedMessage());
            addLineToLog(e.getLocalizedMessage());
            return true;
        }

        return isErrorOrWarningNeedingToStop;
    } // end of method VERSIONSAction

    void viewFile() {
        File fileNameFile;
        String fileNameName;
        boolean isErrorOrWarningNeedingToStop;
        int resultFileName;

        if (texas.ProjectDirectory == null) {
            JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid File Directory", JOptionPane.ERROR_MESSAGE);
            return;
        }

        if (texas.ProjectDirectory.trim().length() == 0) {
            JOptionPane.showMessageDialog(null, "Invalid Project Directory", "Invalid File Directory", JOptionPane.ERROR_MESSAGE);
            return;
        }

        isErrorOrWarningNeedingToStop = setProjectFileNames(0);
        if (isErrorOrWarningNeedingToStop) {
            return;
        }

        JFileChooser fileChooser = new JFileChooser(texas.ProjectDirectory);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        javax.swing.filechooser.FileFilter fileFilter = new TexasFileFilterTXT();
        fileChooser.setFileFilter(fileFilter);
        fileChooser.setDialogTitle("Select '.txt' File");

        while (true) {
            resultFileName = fileChooser.showOpenDialog(null);

            if (resultFileName == JFileChooser.CANCEL_OPTION) {
                return;
            }

            fileNameFile = fileChooser.getSelectedFile();

            try {
                if (fileNameFile == null) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
                if (fileNameFile.getCanonicalPath().trim().length() == 0) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }
            }
            catch (Exception e) {
                System.err.println("viewFile exception error fileNameFile.getCanonicalPath");
                addLineToLog("viewFile exception error fileNameFile.getCanonicalPath");
                System.err.println(e.getLocalizedMessage());
                addLineToLog(e.getLocalizedMessage());
                continue;
            }

            if (!fileNameFile.exists()) {
                JOptionPane.showMessageDialog(null, "File Name Does Not Exist", "File Name Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            fileNameName = fileNameFile.getName();

            if (fileNameName == null) {
                JOptionPane.showMessageDialog(null, "File Name is null", "File Name is null", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (fileNameName.trim().length() == 0) {
                JOptionPane.showMessageDialog(null, "File Name is empty", "File Name is empty", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (!fileNameName.endsWith(".txt")) {
                JOptionPane.showMessageDialog(null, "File Name does not end with '.txt'", "File Name does not end with '.txt'", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        new ViewFileDialog(fileNameFile);
    } // end of method viewFile
} // end of class TexasModelDialog
