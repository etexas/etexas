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
package org.etexascode.webapp.playback;

import java.awt.Polygon;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.CharEncoding;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class that parts static data (strings) into various simulation data types.
 * 
 * @author janway
 * @author bbadillo
 * @author jrutherford
 * @author bmaulon
 * @author ablatt
 */
public class StaticDataParser {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(StaticDataParser.class);

    private static long getFirstStepFileName(File stepData) {
        File[] f = stepData.listFiles();

        if (f == null) {

            throw new RuntimeException("No files to be parsed");
        }
        String ret = f[0].getName();
        ret = ret.substring(0, ret.length() - 4);
        long retNum = Long.parseLong(ret);

        for (int i = 1; i < f.length; i++) {
            String option = f[i].getName();
            option = option.substring(0, option.length() - 4);
            long tmp = Long.parseLong(option);

            if (tmp < retNum) {
                retNum = tmp;
            }
        }

        return retNum;
    }

    private static LaneMovement getLaneMovementFromDetails(String detail, int id, int len) {
        id *= 1000;
        id += len;

        LaneMovement ret = new LaneMovement();
        ret.setMovementId(id);

        if (detail.contains("lane_has_right_turn_on_red")) {
            ret.setMovement(Movement.RIGHT_TURN_ON_RED);
        }
        else if (detail.contains("lane_has_right_turn")) {
            ret.setMovement(Movement.RIGHT_TURN);
        }
        else if (detail.contains("lane_has_left_turn_on_red")) {
            ret.setMovement(Movement.LEFT_TURN_ON_RED);
        }
        else if (detail.contains("lane_has_left_turn")) {
            ret.setMovement(Movement.LEFT_TURN);
        }
        else if (detail.contains("lane_has_u_turn")) {
            ret.setMovement(Movement.U_TURN);
        }
        else if (detail.contains("straight")) {
            ret.setMovement(Movement.STRAIGHT);
        }
        else {
            throw new RuntimeException("Unknown movement type: " + detail.split(":")[0]);
        }

        return ret;
    }

    private static String getLaneType(String isEgressFromFile) {
        // TODO: ablatt - verify and finalize types which might be returned
        if (isEgressFromFile.contains("False")) {
            return "INCOMING";
        }
        else {
            return "OUTGOING";
        }
    }

    private static boolean movementExists(String detail) {
        if (detail.contains("True")) {
            return true;
        }
        else {
            return false;
        }
    }

    private static void narrowExtremes(LaneNode curr, LaneNode[] extremes) {
        if (curr.getX() < extremes[0].getX()) {
            extremes[0] = curr;
        }

        if (curr.getX() > extremes[1].getX()) {
            extremes[1] = curr;
        }

        if (curr.getY() < extremes[2].getY()) {
            extremes[2] = curr;
        }

        if (curr.getY() > extremes[3].getY()) {
            extremes[3] = curr;
        }
    }

    private static Object[] parseDetector(String detectorInfo) {
        String[] info = detectorInfo.split("\n");
        Detector ret = new Detector();

        int id = UtilsPlaybackParsers.parseInt(info[0]);
        ret.setDetectorID(id);
        ret.getLaneIDs().add(id);

        int x1 = (int)UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(info[1]));
        int y1 = (int)UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(info[2]));
        int x2 = (int)UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(info[3]));
        int y2 = (int)UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(info[4]));

        Polygon p = ret.getArea();
        p.addPoint(x1, y1);
        p.addPoint(x1, y2);
        p.addPoint(x2, y2);
        p.addPoint(x2, y1);
        ret.setArea(p);

        ret.setPresenceDetectCap(true);
        ret.setSpeedDetectCap(true);
        ret.setLengthDetectCap(true);

        return new Object[] { Integer.valueOf(id), ret };
    }

    private static DetectorManager parseDetectorManager(String[] sections) {
        DetectorManager ret = new DetectorManager();

        // If there are no detectors
        if (sections.length < 2) {
            return ret;
        }

        String[] split = sections[1].split("\n==detector separator==\n");

        for (int i = 1; i < split.length; i++) {
            Object[] o = parseDetector(split[i]);
            ret.addDetector((Integer)o[0], (Detector)o[1]);
        }

        return ret;
    }

    private static Object[] parseLane(String lane) {
        Lane l = new Lane();
        String[] sections = lane.split("\n==begin center points==\n");

        String[] details = sections[0].split("\n");
        int id = UtilsPlaybackParsers.parseInt(details[0]);
        l.setLaneId(id);
        l.setApproachId(id); // TODO: ablatt - figure out what actually goes
        // here
        l.setType(getLaneType(details[1]));
        Map<Integer, LaneMovement> movements = l.getLaneMovements();

        for (int i = 3; i < 8; i++) {
            if (movementExists(details[i])) {
                LaneMovement lm = getLaneMovementFromDetails(details[i], id, movements.keySet().size());
                movements.put(lm.getMovementId(), lm);
            }
        }

        LaneMovement lm = getLaneMovementFromDetails("straight:True", id, movements.keySet().size());
        movements.put(lm.getMovementId(), lm); // TODO: ablatt - remove this
        // when data reflects if a lane
        // can go straight

        details = sections[1].split("\n==end center points==\n");
        String[] pointList = details[0].split("\n==center point divider==\n");

        for (String s : pointList) {
            l.addLaneNode(parseLaneNode(s));
        }

        l.setType("Driving Lane");

        return new Object[] { Integer.valueOf(id), l };
    }

    private static ArrayList<LaneNode> parseLaneForExtremeNodes(Lane lane) {
        ArrayList<LaneNode> ret = new ArrayList<LaneNode>(2);

        List<LaneNode> nodes = lane.getLaneGeomList();
        ret.add(nodes.get(0));
        ret.add(nodes.get(nodes.size() - 1));

        return ret;
    }

    private static LaneManager parseLaneManager(String[] sections) {
        LaneManager ret = new LaneManager();

        double[] point = parseRefPoint(sections[0]);
        ret.setLatitude(point[1]);
        ret.setLongitude(point[0]);
        Map<Integer, Lane> laneMap = ret.getLanes();

        for (int i = 1; i < sections.length; i++) {
            Object[] lane = parseLane(sections[i]);
            laneMap.put((Integer)lane[0], (Lane)lane[1]);
        }

        return ret;
    }

    private static LaneNode parseLaneNode(String node) {
        LaneNode ret = new LaneNode();
        String[] parts = node.split("\n");

        ret.setX(UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(parts[0])));
        ret.setY(UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(parts[1])));
        ret.setZ(0.0);
        ret.setWidth(UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(parts[2])));

        return ret;
    }

    private static SimMetaData parseMetaData(LaneManager laneManager, String misc, File stepData) {
        SimMetaData ret = new SimMetaData();

        ArrayList<LaneNode> extremeNodes = new ArrayList<LaneNode>();
        Map<Integer, Lane> lanes = laneManager.getLanes();

        for (Entry<Integer, Lane> entry : lanes.entrySet()) {
            extremeNodes.addAll(parseLaneForExtremeNodes(entry.getValue()));
        }

        LaneNode[] extremes = new LaneNode[] { extremeNodes.get(0), extremeNodes.get(0), extremeNodes.get(0), extremeNodes.get(0) };

        for (LaneNode node : extremeNodes) {
            narrowExtremes(node, extremes);
        }

        // ret.setSimWidth(getDist(extremes[0], extremes[1]));
        // ret.setSimHeight(getDist(extremes[2], extremes[3]));
        ret.setSimWidth(extremes[1].getX() - extremes[0].getX());
        ret.setSimHeight(extremes[3].getY() - extremes[2].getY());

        ret.setFirstStep(getFirstStepFileName(stepData));
        ret.setMaxSteps(Long.parseLong(misc.split("\n")[1].split(":")[1]) + ret.getFirstStep() + 1);
        ret.setStepSize(UtilsUnitConversion.convertMillisecondsToSeconds(Double.parseDouble(misc.split("\n")[3].split(":")[1])));

        return ret;
    }

    private static double[] parseRefPoint(String section) {
        double[] ret = new double[2];
        String[] parts = section.split("\n");

        ret[0] = UtilsPlaybackParsers.parseDouble(parts[0]);
        ret[1] = UtilsPlaybackParsers.parseDouble(parts[1]);

        return ret;
    }

    private static SignalManager parseSignalManager(String[] sections) {
        return new SignalManager();
    }

    public static StaticData parseStaticData(File staticData, File stepData) {
        StaticData ret = new StaticData();
        File mapData = new File(staticData, "map_data.txt");
        String contents = "";

        FileInputStream fis = null;
        try {
            fis = new FileInputStream(mapData);
            byte[] b = new byte[fis.available()];
            fis.read(b);
            contents = UtilsStringOnModel.decodeUTF8(b);
        }
        catch (FileNotFoundException e) {
            LOGGER.error("Exception", e);

        }
        catch (IOException e) {
            LOGGER.error("Exception", e);
        }
        finally {
            try {
                if (fis != null) {
                    fis.close();
                }
            }
            catch (IOException e) {
                LOGGER.error("Exception", e);
            }
        }

        String[] sections = contents.split("\n==section separator==\n");
        contents = null;
        ret.setLaneManager(parseLaneManager(sections));

        File miscellaneous = new File(staticData, "miscellaneous.txt");

        fis = null;
        try {
            fis = new FileInputStream(miscellaneous);
            byte[] b = new byte[fis.available()];
            fis.read(b);
            contents = new String(b, CharEncoding.UTF_8);
            fis.close();

            sections = contents.split("\n==section separator==\n");
            ret.setDetectorManager(parseDetectorManager(sections));
            ret.setSignalManager(parseSignalManager(sections));
            ret.setMetaData(parseMetaData(ret.getLaneManager(), sections[0], stepData));
        }
        catch (FileNotFoundException e) {
            LOGGER.error("Exception", e);
        }
        catch (IOException e) {
            LOGGER.error("Exception", e);
        }
        finally {
            try {
                if (fis != null) {
                    fis.close();
                }
            }
            catch (IOException e) {
                LOGGER.error("Exception", e);
            }
        }

        return ret;
    }
}