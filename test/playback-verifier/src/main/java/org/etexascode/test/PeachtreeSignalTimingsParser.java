/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.test;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Reads in signal timing data from the GDVSIM peachtree files.
 * 
 * @author janway
 */
public class PeachtreeSignalTimingsParser {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(PeachtreeSignalTimingsParser.class);

    /**
     * Returns a list sorted by timestep where each element is a map of laneId to signal state.
     * 
     * @param directory The directory.
     * @param filenames The names of the files to open and parse, in the signal-change directory.
     * @return The list of signal change information, empty or partially filled if a file wasn't
     *         found.
     */
    public static List<NGSimSignals> getPeachtreeSignalTimes(String directory, List<String> filenames) {
        List<NGSimSignals> ret = new LinkedList<NGSimSignals>();

        File dir = new File(directory);

        if (!dir.exists()) {
            LOGGER.info("Peachtree signal timings directory was not found.");
            return ret;
        }

        Map<String, List<Integer>> laneIdsByApproach = new HashMap<String, List<Integer>>();
        laneIdsByApproach.put("EB", new LinkedList<Integer>(Arrays.asList(43, 44)));
        laneIdsByApproach.put("SB", new LinkedList<Integer>(Arrays.asList(38, 39, 40)));
        laneIdsByApproach.put("WB", new LinkedList<Integer>(Arrays.asList(35)));
        laneIdsByApproach.put("NB", new LinkedList<Integer>(Arrays.asList(25, 26, 33)));

        Map<String, NGSimSignals.State> colNamesToSignalStates = new HashMap<String, NGSimSignals.State>();
        colNamesToSignalStates.put("BG_Thru", NGSimSignals.State.G_THRU);
        colNamesToSignalStates.put("BY_Thru", NGSimSignals.State.Y_THRU);
        colNamesToSignalStates.put("BR_Thru", NGSimSignals.State.R_THRU);

        for (String fname : filenames) {
            File file = new File(dir, fname);
            if (!file.exists()) {
                LOGGER.info("Peachtree signal timings file {} was not found.", fname);
                return ret;
            }

            String[] fnameParts = fname.split("_");

            String approach = fnameParts[2];

            try {
                Scanner s = new Scanner(file, "UTF-8");

                s.nextLine();
                String strTemp = s.nextLine();
                int column = 0;
                while (strTemp.indexOf("\t", column) == column)
                    column++;
                s.close();

                Scanner scanner = new Scanner(file, "UTF-8");

                String firstRow = scanner.nextLine();
                String[] colNames = firstRow.split("\t");

                int numColumns = colNames.length;

                int prevTime = 0;
                while (scanner.hasNextInt()) {
                    int timestep = scanner.nextInt();
                    int curColumn = column - 1;
                    if (curColumn < 0) {
                        curColumn = numColumns - 1;
                    }
                    NGSimSignals.State state = colNamesToSignalStates.get(colNames[curColumn]);
                    List<Integer> laneIds = laneIdsByApproach.get(approach);
                    NGSimSignals entry = new NGSimSignals(timestep, laneIds, state, prevTime);
                    ret.add(entry);
                    prevTime = timestep;
                    column = (column + 1) % numColumns;
                }
                scanner.close();

            }
            catch (Exception e) {
                LOGGER.info("Error opening {}.", fname);
                return ret;
            }
        }

        // sort by time
        Collections.sort(ret, new Comparator<NGSimSignals>() {

            @Override
            public int compare(NGSimSignals x, NGSimSignals y) {
                return x.getTimestep() - y.getTimestep();
            }
        });

        return ret;
    }
}
