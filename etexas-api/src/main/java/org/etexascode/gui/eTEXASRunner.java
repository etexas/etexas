/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */

/*
 * TexasFrontFrame.java
 *
 * Created on Sep 1, 2010, 1:38:15 PM
 */
package org.etexascode.gui;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.etexascode.api.RunnerUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Creates a frame for selecting a project and then launching eTEXAS in that project directory.
 * 
 * @author rbedia
 */
public class eTEXASRunner extends javax.swing.JFrame {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(eTEXASRunner.class);

    /**
     * The main class which starts a simulator.
     * 
     * @param args the command line arguments
     * @throws ClassNotFoundException If class not found exception occurs.
     * @throws InstantiationException If instantiation exception occurs.
     * @throws UnsupportedLookAndFeelException If unsupported look and feel exception occurs.
     * @throws IllegalAccessException If illealge access exception occurs.
     */
    public static void main(String args[]) throws ClassNotFoundException, InstantiationException, UnsupportedLookAndFeelException, IllegalAccessException {
        final String libraryPath = System.getProperty("jna.library.path");
        if (args.length != 1) {
            System.out.println(eTEXASRunner.class.getSimpleName() + " must have one parameter which is the path to the jar to execute. ");
        }
        else if (libraryPath == null) {
            throw new RuntimeException("Must specify native library path on command line using jna.library.path property");
        }
        else {
            final String jarToExecute = args[0];
            System.out.println(jarToExecute);

            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

            java.awt.EventQueue.invokeLater(new Runnable() {

                @Override
                public void run() {
                    eTEXASRunner runner = new eTEXASRunner(jarToExecute, libraryPath);
                    runner.setVisible(true);
                }
            });
        }
    }

    String jarPath;

    String libraryPath;

    String ProjectDirectory = "";

    String ProjectName = "";

    String GDVDATA_name = "gdvdata";

    String TexasSysDatDirectory = "";

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;

    private javax.swing.JLabel jLabel2;

    private javax.swing.JLabel jLabel3;

    private javax.swing.JButton runButton;

    private javax.swing.JButton selectProjName;

    private javax.swing.JButton selectSysDatDir;

    private javax.swing.JTextField textProjDir;

    private javax.swing.JTextField textProjName;

    private javax.swing.JTextField textSysDatDir;

    // End of variables declaration//GEN-END:variables
    /**
     * Creates new form TexasFrontFrame.
     * 
     * @param jarPath The jar path.
     * @param libraryPath The library path.
     */
    public eTEXASRunner(String jarPath, String libraryPath) {
        initComponents();

        this.jarPath = jarPath;
        this.libraryPath = libraryPath;

        File sysDatFile = new File("C:\\texas\\sys_dat\\");
        if (!sysDatFile.exists()) {
            sysDatFile = new File("/home/demo/texas/sys_dat/");
            if (!sysDatFile.exists()) {
                sysDatFile = null;
            }
        }

        if (sysDatFile != null) {
            TexasSysDatDirectory = sysDatFile.getPath();
            textSysDatDir.setText(TexasSysDatDirectory);
        }
    }

    /**
     * Checks that all required fields are set and then enables the run button.
     */
    private void enableRunButton() {
        if (ProjectDirectory.trim().length() != 0 && ProjectName.trim().length() != 0 && TexasSysDatDirectory.trim().length() != 0) {
            runButton.setEnabled(true);
        }
        else {
            runButton.setEnabled(false);
        }
    }

    /**
     * Finds a starting directory for the file chooser to use.
     * 
     * @return a directory for the file chooser to start in
     */
    private File getStartingDirectory() {
        File dir;
        try {
            if (ProjectDirectory != null && ProjectDirectory.trim().length() > 0) {
                dir = new File(ProjectDirectory);
            }
            else {
                // use the current working directory
                dir = new File(new File("C:\\texas\\examples").getCanonicalPath());
            }
        }
        catch (Exception e) {
            System.err.println("ProjectDirSelect exception error file.getCanonicalPath");
            System.err.println(e.getLocalizedMessage());
            return null;
        }
        return dir;
    }

    /**
     * This method is called from within the constructor to initialize the form. WARNING: Do NOT
     * modify this code. The content of this method is always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed"
    // desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        textProjDir = new javax.swing.JTextField();
        selectProjName = new javax.swing.JButton();
        textProjName = new javax.swing.JTextField();
        runButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        textSysDatDir = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        selectSysDatDir = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Project Selector");
        setLocationByPlatform(true);

        textProjDir.setEditable(false);

        selectProjName.setText("Select Project");
        selectProjName.addActionListener(new java.awt.event.ActionListener() {

            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectProjNameActionPerformed(evt);
            }
        });

        textProjName.setEditable(false);

        runButton.setText("OK");
        runButton.setEnabled(false);
        runButton.addActionListener(new java.awt.event.ActionListener() {

            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                runButtonActionPerformed(evt);
            }
        });

        jLabel1.setText("Project Directory:");

        jLabel2.setText("Project Name:");

        textSysDatDir.setEditable(false);

        jLabel3.setText("sys_dat Directory:");

        selectSysDatDir.setText("Select SysDat");
        selectSysDatDir.addActionListener(new java.awt.event.ActionListener() {

            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectSysDatDirActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addGroup(
                layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addComponent(jLabel1).addComponent(jLabel2).addComponent(jLabel3))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(
                                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(
                                                layout.createSequentialGroup().addComponent(selectSysDatDir).addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 193, Short.MAX_VALUE)
                                                        .addComponent(runButton))
                                        .addComponent(textSysDatDir, javax.swing.GroupLayout.DEFAULT_SIZE, 339, Short.MAX_VALUE).addComponent(selectProjName)
                                        .addComponent(textProjDir, javax.swing.GroupLayout.DEFAULT_SIZE, 339, Short.MAX_VALUE)
                                        .addComponent(textProjName, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 339, Short.MAX_VALUE))
                        .addContainerGap()));
        layout.setVerticalGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addGroup(
                layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(
                                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(textProjDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel1))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(
                                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(textProjName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel2))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(selectProjName)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(
                                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(textSysDatDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel3))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE).addComponent(selectSysDatDir).addComponent(runButton)).addContainerGap()));

        textProjDir.getAccessibleContext().setAccessibleName("Project Directory Text Box");
        textProjDir.getAccessibleContext().setAccessibleDescription("Project Directory Text Box");
        selectProjName.getAccessibleContext().setAccessibleDescription("Button to browse for a project");
        textProjName.getAccessibleContext().setAccessibleName("Project Name Text Box");
        textProjName.getAccessibleContext().setAccessibleDescription("Project Name Text Box");
        runButton.getAccessibleContext().setAccessibleDescription("Button to accept directory values");
        jLabel1.getAccessibleContext().setAccessibleDescription("Project Directory");
        jLabel2.getAccessibleContext().setAccessibleDescription("Project Name");
        textSysDatDir.getAccessibleContext().setAccessibleName("SysDat Directory Text Box");
        textSysDatDir.getAccessibleContext().setAccessibleDescription("SysDat Directory Text Box");
        jLabel3.getAccessibleContext().setAccessibleDescription("SysDat Directory");
        selectSysDatDir.getAccessibleContext().setAccessibleDescription("Button to browse for a sysdat directory");

        getAccessibleContext().setAccessibleName("Project Selector - eTEXAS V7.0");

        pack();
    }// </editor-fold>//GEN-END:initComponents

    /**
     * Opens a file chooser for the user to select the project to run.
     * 
     * @throws IOException
     */
    private void ProjectNameSelect() throws IOException {
        char c;
        int i;
        int n;
        File projectNameFile;
        String projectName;
        String projectNameName;
        int resultProjectName;

        File f = getStartingDirectory();

        JFileChooser fileChooser = new JFileChooser(f);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        javax.swing.filechooser.FileFilter fileFilter = new TexasFileFilterGDVDATA(GDVDATA_name);
        fileChooser.setFileFilter(fileFilter);
        fileChooser.setDialogTitle("Select Project '_" + GDVDATA_name + "' File");

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
                System.err.println(e.getLocalizedMessage());
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

            if (!projectNameName.endsWith("_" + GDVDATA_name)) {
                JOptionPane.showMessageDialog(null, "Project Name does not end with _" + GDVDATA_name, "Project Name does not end with " + GDVDATA_name, JOptionPane.ERROR_MESSAGE);
                continue;
            }

            i = projectNameName.lastIndexOf("_" + GDVDATA_name);
            if (i == -1) {
                JOptionPane.showMessageDialog(null, "Project Name does not end with _" + GDVDATA_name, "Project Name does not end with " + GDVDATA_name, JOptionPane.ERROR_MESSAGE);
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

            ProjectName = projectName;
            textProjName.setText(ProjectName);
            ProjectDirectory = projectNameFile.getParent();

            break;
        }

        textProjDir.setText(ProjectDirectory);
    } // end of method ProjectNameSelect

    private void runButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_runButtonActionPerformed
        // Spawn the new program in its own thread so that this program's GUI
        // can be closed down.
        new Thread() {

            @Override
            public void run() {
                try {
                    File projDir = new File(ProjectDirectory);
                    RunnerUtils.writeSimpro(projDir, ProjectName, TexasSysDatDirectory);
                    // Find the path to the shared library
                    String simDTPath = new File(libraryPath).getCanonicalPath();
                    // Find the path to the eTEXAS GUI jar
                    String eTEXASPath = new File(jarPath).getCanonicalPath();

                    List<String> command = new ArrayList<String>();
                    command.add("java");
                    command.add("-Djna.library.path=" + simDTPath);
                    command.add("-jar");
                    command.add(eTEXASPath);

                    ProcessBuilder processBuilder = new ProcessBuilder(command);
                    // Set the project directory as the current working
                    // directory
                    processBuilder.directory(projDir);
                    processBuilder.redirectErrorStream(true);
                    Process process = processBuilder.start();

                    InputStreamReader inputStreamReader = new InputStreamReader(process.getInputStream(), "ASCII");
                    BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
                    String readLine;
                    if (LOGGER.isDebugEnabled()) {
                        while ((readLine = bufferedReader.readLine()) != null) {
                            if (readLine.trim().length() > 0) {
                                LOGGER.debug(readLine);
                            }
                        }
                    }
                    bufferedReader.close();
                }
                catch (IOException ex) {
                    LOGGER.debug(ex.toString());
                }
            }
        }.start();

        // Hide the launcher program.
        dispose();
    }// GEN-LAST:event_runButtonActionPerformed

    private void selectProjNameActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_selectProjNameActionPerformed
        try {
            ProjectNameSelect();
            enableRunButton();
        }
        catch (IOException ex) {
            LOGGER.debug(ex.toString());
        }
    }// GEN-LAST:event_selectProjNameActionPerformed

    private void selectSysDatDirActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_selectSysDatDirActionPerformed
        try {
            SysDatDirectorySelect();
            enableRunButton();
        }
        catch (IOException ex) {
            LOGGER.debug(ex.toString());
        }
    }// GEN-LAST:event_selectSysDatDirActionPerformed

    /**
     * Opens a file chooser for the user to select the SysDat directory.
     * 
     * @throws IOException
     */
    private void SysDatDirectorySelect() throws IOException {
        File sysDatDir;

        File f = getStartingDirectory();

        JFileChooser fileChooser = new JFileChooser(f);
        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        fileChooser.setDialogTitle("Select sys_dat Directory");

        if (fileChooser.showOpenDialog(null) == JFileChooser.CANCEL_OPTION) {
            return;
        }

        sysDatDir = fileChooser.getSelectedFile();

        try {
            if (sysDatDir == null || sysDatDir.getCanonicalPath().trim().length() == 0 || !sysDatDir.getCanonicalPath().trim().endsWith("sys_dat")
                    && !sysDatDir.getCanonicalPath().trim().endsWith("sys_dat" + File.pathSeparator)) {
                JOptionPane.showMessageDialog(null, "Invalid sys_dat directory", "Invalid sys_dat directory", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        catch (Exception e) {
            System.err.println("SysDatDirectorySelect exception error sysDatDir.getCanonicalPath");
            System.err.println(e.getLocalizedMessage());
            return;
        }

        if (!sysDatDir.exists()) {
            JOptionPane.showMessageDialog(null, "sys_dat Directory Does Not Exist", "sys_dat Directory Does Not Exist", JOptionPane.ERROR_MESSAGE);
            return;
        }

        TexasSysDatDirectory = sysDatDir.getCanonicalPath();
        textSysDatDir.setText(TexasSysDatDirectory);
    }
}
