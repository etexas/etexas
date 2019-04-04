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

package org.etexascode.interrep.datamodel.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.lang3.CharEncoding;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorEvent;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

public class UtilsArchiveOperations {

    /** The string to separate sections in data text files. */
    private static final String SECTION_SEPARATOR = "==section separator==\n";

    // prevents instantiation
    private UtilsArchiveOperations() {}

    /**
     * Unzip an archive and put it into a folder.
     * 
     * @param projDir The folder to place the project.
     * @param archive The byte array containing the zip archive.
     * @throws IOException If the archive cannot be extracted.
     */
    public static void setupArchive(File projDir, byte[] archive) throws IOException {

        if (!projDir.exists() && !projDir.mkdirs()) {

            throw new IOException(String.format("The \"%s\" project directory could not be created.", projDir.getName()));
        }

        ZipInputStream zis = null;

        try {

            int bufferSize = 4096;
            zis = new ZipInputStream(new BufferedInputStream(new ByteArrayInputStream(archive), bufferSize));
            ZipEntry entry = zis.getNextEntry();

            while (entry != null) {

                File newFile = new File(projDir, entry.getName());

                if (entry.isDirectory() && !newFile.mkdir()) {

                    throw new IOException(String.format("The \"%s\" subdirectory could not be created.", entry.getName()));
                }
                else {

                    byte[] data = new byte[bufferSize];
                    int count = zis.read(data, 0, bufferSize);
                    BufferedOutputStream bos = null;

                    try {

                        bos = new BufferedOutputStream(new FileOutputStream(newFile), bufferSize);

                        while (count != -1) {

                            bos.write(data, 0, count);
                            count = zis.read(data, 0, bufferSize);
                        }

                        bos.flush();
                    }
                    finally {

                        if (bos != null) {

                            bos.close();
                        }
                    }
                }

                entry = zis.getNextEntry();
            }
        }
        finally {

            if (zis != null) {

                zis.close();
            }
        }
    }

    /**
     * Writes an InterRepInfoModel as a playback project.
     * 
     * @param irim The InterRepInfoModel.
     * @param projId The name of the resulting playback project.
     * @param dir The directory.
     * @param stepNum The step number of this model.
     * @param stepSize The step size of the execution.
     * @throws IOException If the project cannot be written.
     */
    public static void writeInterRepInfoToPlayback(InterRepInfoModel irim, String projId, String dir, int stepNum, double stepSize) throws IOException {

        File staticData = new File(String.format("%s%s/static_data", dir, projId));

        if (!staticData.exists() && !staticData.mkdirs()) {

            throw new IOException("The static data directory could not be created.");
        }

        File mapData = new File(staticData, "map_data.txt");
        File miscData = new File(staticData, "miscellaneous.txt");
        File stepData = new File(String.format("%s%s/step_data", dir, projId));

        if (!stepData.exists() && !stepData.mkdirs()) {

            throw new IOException("The step data directory could not be created.");
        }

        if (!mapData.exists() && irim.lmi != null && !irim.lmi.getLaneIds().isEmpty()) {

            UtilsArchiveOperations.writeMapData(irim, mapData);
        }

        File stepFile = new File(stepData, String.format("%d.txt", stepNum));

        if (!stepFile.exists()) {

            UtilsArchiveOperations.writeStepData(irim, stepFile);
            UtilsArchiveOperations.writeMiscellaneousData(stepNum, stepSize, miscData);
        }
    }

    /**
     * Writes the map data for the specified intersection model.
     * 
     * @param irim The intersection model.
     * @param dataFile The map data file.
     * @throws IOException If the map data cannot be written.
     */
    private static void writeMapData(InterRepInfoModel irim, File dataFile) throws IOException {

        Writer writer = null;

        try {

            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFile), StandardCharsets.UTF_8.name()));
            writer.write(UtilsArchiveOperations.convertToPlaybackFileString(irim.lmi));
        }
        finally {

            if (writer != null) {

                writer.close();
            }
        }
    }

    /**
     * Writes the step data for the specified intersection model.
     * 
     * @param irim The intersection model.
     * @param dataFile The step data file.
     * @throws IOException If the step data cannot be written.
     */
    private static void writeStepData(InterRepInfoModel irim, File dataFile) throws IOException {

        Writer writer = null;

        try {

            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFile), StandardCharsets.UTF_8.name()));
            writer.write(convertToPlaybackFileString(irim.vmi, false));
            writer.write(convertToPlaybackFileString(irim.smi, !irim.vmi.getAllVehicleIds().isEmpty()));
            writer.write(convertToPlaybackFileString(irim.dmi, !irim.vmi.getAllVehicleIds().isEmpty() || irim.smi.iterator().hasNext()));
        }
        finally {

            if (writer != null) {

                writer.close();
            }
        }
    }

    /**
     * Writes the miscellaneous data for the specified time step.
     * 
     * @param stepNum The integer step number.
     * @param stepSize The double step size.
     * @param dataFile The miscellaneous data file.
     * @throws IOException If the miscellaneous data cannot be written.
     */
    private static void writeMiscellaneousData(int stepNum, double stepSize, File dataFile) throws IOException {

        int totalSteps = 1;
        long firstStep = stepNum;

        if (dataFile.exists()) {

            FileInputStream reader = null;

            try {

                reader = new FileInputStream(dataFile);
                byte[] bytes = new byte[reader.available()];

                if (reader.read(bytes) != -1) {

                    String data = new String(bytes, CharEncoding.UTF_8);
                    String[] sections = data.split("\n==section separator==\n");
                    String[] lines = sections[0].split("\n");
                    totalSteps = 1 + Integer.parseInt(lines[1].split(":")[1]);
                    firstStep = Long.parseLong(lines[2].split(":")[1]);
                }
                else {

                    throw new IOException("Existing miscellaneous data could not be read.");
                }
            }
            finally {

                if (reader != null) {

                    reader.close();
                }
            }
        }

        StringBuilder dataBuilder = new StringBuilder("extra data\n");
        dataBuilder.append(String.format("total_num_time_steps:%d%n", totalSteps));
        dataBuilder.append(String.format("first_time_step:%d%n", firstStep));
        dataBuilder.append(String.format("time_step_length_in_milliseconds:%f", UtilsUnitConversion.convertSecondsToMilliseconds(stepSize)));

        Writer writer = null;

        try {

            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFile), StandardCharsets.UTF_8.name()));
            writer.write(dataBuilder.toString());
        }
        finally {

            if (writer != null) {

                writer.close();
            }
        }
    }

    /**
     * Converts a LaneManager to a string recognized by the playback adapter.
     * 
     * @param lmi The lane manager.
     * @return The string representing the lanes.
     */
    private static String convertToPlaybackFileString(ILaneManager lmi) {

        StringBuilder sb = new StringBuilder();
        sb.append("reference_x:" + lmi.getLongitude() + "\n");
        sb.append("reference_y:" + lmi.getLatitude() + "\n");
        sb.append(UtilsArchiveOperations.SECTION_SEPARATOR);

        Iterator<ILane> iter = lmi.iterator();
        while (iter.hasNext()) {
            ILane li = iter.next();
            sb.append("lane_id:" + li.getLaneId() + "\n");

            String egress = (Lane.INBOUND.equals(li.getType())) ? "False" : "True";
            sb.append("is_egress:" + egress + "\n");

            sb.append("lane_width_in_feet:-1.0\n");

            boolean rt = false;
            boolean rtor = false;
            boolean lt = false;
            boolean ltor = false;
            boolean ut = false;
            for (ILaneMovement lmov : li.lanMovIterator()) {
                if (lmov.getMovement().equals(LaneMovement.Movement.RIGHT_TURN))
                    rt = true;
                if (lmov.getMovement().equals(LaneMovement.Movement.RIGHT_TURN_ON_RED))
                    rtor = true;
                if (lmov.getMovement().equals(LaneMovement.Movement.LEFT_TURN))
                    lt = true;
                if (lmov.getMovement().equals(LaneMovement.Movement.LEFT_TURN_ON_RED))
                    ltor = true;
                if (lmov.getMovement().equals(LaneMovement.Movement.U_TURN))
                    ut = true;
            }

            String rts = (rt) ? "True" : "False";
            String rtors = (rtor) ? "True" : "False";
            String lts = (lt) ? "True" : "False";
            String ltors = (ltor) ? "True" : "False";
            String uts = (ut) ? "True" : "False";

            sb.append("lane_has_right_turn:" + rts + "\n");
            sb.append("lane_has_right_turn_on_red:" + rtors + "\n");
            sb.append("lane_has_left_turn:" + lts + "\n");
            sb.append("lane_has_left_turn_on_red:" + ltors + "\n");
            sb.append("lane_has_u_turn:" + uts + "\n");

            sb.append("==begin center points==\n");

            Iterator<? extends ILaneNode> lnii = li.getLaneGeomList().iterator();
            while (lnii.hasNext()) {
                ILaneNode lni = lnii.next();
                String x = Double.toString(UtilsUnitConversion.convertCentimetersToFeet(lni.getX()));
                String y = Double.toString(UtilsUnitConversion.convertCentimetersToFeet(lni.getY()));
                String w = Double.toString(UtilsUnitConversion.convertCentimetersToFeet(lni.getWidth()));

                sb.append("x_in_feet:" + x + "\n");
                sb.append("y_in_feet:" + y + "\n");
                sb.append("width_in_feet:" + w + "\n");

                if (lnii.hasNext())
                    sb.append("==center point divider==\n");
            }

            sb.append("==end center points==\n");
            sb.append("==begin lane connector list==\n");
            sb.append("==end lane connector list==");

            if (iter.hasNext()) {
                sb.append(String.format("%n%s", UtilsArchiveOperations.SECTION_SEPARATOR));
            }
        }

        return sb.toString();
    }

    /**
     * Converts a SignalManager to a string recognized by the playback adapter.
     * 
     * @param smi The signal manager.
     * @return The string representing the signal indications.
     */
    private static String convertToPlaybackFileString(ISignalManager smi, boolean initialSeparator) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;

        for (ISignalIndication sii : smi) {
            if (!initialSeparator && first) {
                // don't write a section separator at the top of the file
            }
            else {
                sb.append(UtilsArchiveOperations.SECTION_SEPARATOR);
            }
            first = false;

            sb.append("signal\n");

            sb.append("lane_id:" + sii.getLaneId() + "\n");

            String color = getColor(sii);
            String type = getType(sii);

            sb.append("signal_state:" + color + "-" + type + "\n");
            sb.append("time_to_change_in_milliseconds:" + UtilsUnitConversion.convertSecondsToMilliseconds(sii.getTimeToChange()) + "\n");
        }

        return sb.toString();
    }

    /**
     * Converts a DetectorManager to a string recognized by the playback adapter.
     * 
     * @param dmi The detector manager.
     * @return The string representing the active detector events.
     */
    private static String convertToPlaybackFileString(IDetectorManager dmi, boolean initialSeparator) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;

        for (IDetector di : dmi) {
            IDetectorEvent dei = di.getDetEvent();

            if ((dei != null) && dei.isPresence()) {
                if (!initialSeparator && first) {
                    // don't write a section separator at the top of the file
                }
                else {
                    sb.append(UtilsArchiveOperations.SECTION_SEPARATOR);
                }
                first = false;

                sb.append("detector\n");

                sb.append("detector_id:" + di.getDetectorID() + "\n");
                sb.append("speed_in_miles_per_hour:" + UtilsUnitConversion.convertMetersPerSecondToMilesPerHour(dei.getSpeed()) + "\n");
                sb.append("length_in_feet:" + UtilsUnitConversion.convertCentimetersToFeet(dei.getLength()) + "\n");
            }
        }

        return sb.toString();
    }

    /**
     * Converts a VehicleManager to a string recognized by the playback adapter.
     * 
     * @param vmi The vehicle manager.
     * @return The string representing the vehicles.
     */
    private static String convertToPlaybackFileString(IVehicleManager vmi, boolean initialSeparator) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;

        for (IVehicle vi : vmi) {
            if (!initialSeparator && first) {
                // don't write a section separator at the top of the file
            }
            else {
                sb.append(UtilsArchiveOperations.SECTION_SEPARATOR);
            }
            first = false;

            sb.append("vehicle\n");
            sb.append("vehicle_id:" + vi.getVehicleID() + "\n");
            sb.append("global_time: unused\n");
            sb.append("x_offset_in_feet:" + UtilsUnitConversion.convertCentimetersToFeet(vi.getX()) + "\n");
            sb.append("y_offset_in_feet:" + UtilsUnitConversion.convertCentimetersToFeet(vi.getY()) + "\n");
            sb.append("latitude: unused\n");
            sb.append("longitude: unused\n");
            sb.append("speed_in_miles_per_hour:" + UtilsUnitConversion.convertMetersPerSecondToMilesPerHour(vi.getSpeed()) + "\n");
            sb.append("acceleration_in_miles_per_hour_squared:" + UtilsUnitConversion.convertMetersPerSecondSquaredToMilesPerHourSquared(vi.getAcceleration()) + "\n");
            sb.append("vehicle_length_in_feet:" + UtilsUnitConversion.convertCentimetersToFeet(vi.getLength()) + "\n");
            sb.append("vehicle_width_in_feet:" + UtilsUnitConversion.convertCentimetersToFeet(vi.getWidth()) + "\n");
            sb.append("lane_id:" + vi.getLaneID() + "\n");
        }

        return sb.toString();
    }

    /**
     * Converts a SignalIndication.Color to a string recognized by the playback adapter.
     * 
     * @param sii The signal indication.
     * @return The string representing its color.
     */
    private static String getColor(ISignalIndication sii) {
        if (sii.getColorIndication().equals(SignalIndication.Color.GREEN))
            return "green";
        if (sii.getColorIndication().equals(SignalIndication.Color.YELLOW))
            return "yellow";
        if (sii.getColorIndication().equals(SignalIndication.Color.RED))
            return "red";
        if (sii.getColorIndication().equals(SignalIndication.Color.NONE))
            return "none";
        return "null";
    }

    /**
     * Converts a SignalIndication.Type to a string recognized by the playback adapter.
     * 
     * @param sii The signal indication.
     * @return The string representing its type.
     */
    private static String getType(ISignalIndication sii) {
        if (sii.getTypeIndication().equals(SignalIndication.Type.BALL))
            return "ball";
        if (sii.getTypeIndication().equals(SignalIndication.Type.LEFT_ARROW))
            return "left_arrow";
        if (sii.getTypeIndication().equals(SignalIndication.Type.RIGHT_ARROW))
            return "right_arrow";
        if (sii.getTypeIndication().equals(SignalIndication.Type.STRAIGHT_ARROW))
            return "straight_arrow";
        if (sii.getTypeIndication().equals(SignalIndication.Type.UTURN_ARROW))
            return "uturn_arrow";
        if (sii.getTypeIndication().equals(SignalIndication.Type.STOP_SIGN))
            return "stop_sign";
        if (sii.getTypeIndication().equals(SignalIndication.Type.YIELD_SIGN))
            return "yield_sign";
        return "null";
    }
}
