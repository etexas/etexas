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
package org.etexascode.gui;

import java.io.File;

public class TexasFileFilterGDVDATA extends javax.swing.filechooser.FileFilter {

    String GDVDATA_name = null;

    public TexasFileFilterGDVDATA(String name) {
        GDVDATA_name = name;
    }

    // accept all filenames ending with "_" + GDVDATA_name
    @Override
    public boolean accept(File fileFile) {
        String fileName;

        if (fileFile == null) {
            System.out.println("TexasFileFilterGDVDATA error - fileFile is null.");
            return false;
        }

        if (GDVDATA_name == null) {
            System.out.println("TexasFileFilterGDVDATA error - GDVDATA_name is null.");
            return false;
        }

        if (GDVDATA_name.trim().length() == 0) {
            System.out.println("TexasFileFilterGDVDATA error - GDVDATA_name is empty.");
            return false;
        }

        fileName = fileFile.getName();

        if (fileName != null) {
            if (fileName.trim().length() > 0) {
                if (fileName.endsWith("_" + GDVDATA_name.trim())) {
                    return true;
                }
            }
        }
        if (fileFile.isDirectory()) {
            return true;
        }

        return false;
    } // end of method accept

    @Override
    public String getDescription() {
        if (GDVDATA_name == null) {
            System.out.println("TexasFileFilterGDVDATA.getDescription error - GDVDATA_name is null.");
            return ("TexasFileFilterGDVDATA.getDescription error");
        }

        return ("Filenames ending with '_" + GDVDATA_name.trim() + "'");
    }
} // end of class TexasFileFilterGDVDATA
