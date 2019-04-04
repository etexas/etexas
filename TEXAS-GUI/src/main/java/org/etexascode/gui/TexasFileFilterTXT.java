package org.etexascode.gui;

/******************************************************************************/
/*                          TexasFileFilterTXT.java                           */
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

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.*;
import javax.swing.*;
import javax.swing.filechooser.*;
import javax.swing.JOptionPane;

public class TexasFileFilterTXT extends javax.swing.filechooser.FileFilter {

    // accept all filenames optionally starting with project name and ending with "*.txt"
    public boolean accept(File fileFile) {
        String fileName;

        if (fileFile == null) {
            System.out.println("TexasFileFilterTXT error - fileFile is null.");
            return false;
        }

        if (texas.ProjectName == null) {
            System.out.println("TexasFileFilterTXT error - texas.ProjectName is null.");
            return false;
        }

        fileName = fileFile.getName();

        if (fileName != null) {
            if (fileName.trim().length() > 0) {
                if (texas.ProjectName.trim().length() == 0) {
                    if (fileName.endsWith(".txt")) {
                        return true;
                    }
                }
                else {
                    if (fileName.startsWith(texas.ProjectName.trim() + "_") && fileName.endsWith(".txt")) {
                        return true;
                    }
                }
            }
        }

        return false;
    } // end of method accept

    public String getDescription() {
        if (texas.ProjectName == null) {
            System.out.println("TexasFileFilterTXT.getDescription error - texas.ProjectName is null.");
            return ("TexasFileFilterTXT.getDescription error");
        }

        if (texas.ProjectName.trim().length() == 0) {
            return ("Filenames ending with '.txt'");
        }
        else {
            return ("Filenames starting with '" + texas.ProjectName.trim() + "_' and ending with '.txt'");
        }
    }
} // end of class TexasFileFilterTXT
