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
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.etexascode.j2735messages;

import java.nio.ByteBuffer;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.codec.binary.Base64;
import org.etexascode.j2735.Approach;
import org.etexascode.j2735.Approach.DrivingLanes;
import org.etexascode.j2735.ApproachObject;
import org.etexascode.j2735.Intersection;
import org.etexascode.j2735.Intersection.Approaches;
import org.etexascode.j2735.IntersectionState;
import org.etexascode.j2735.IntersectionState.States;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.MovementState;
import org.etexascode.j2735.ObjectFactory;
import org.etexascode.j2735.Position3D;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.VehicleReferenceLane;
import org.etexascode.j2735.util.SignalLightState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author dranker
 */
public class SecondMessageTest extends TestCase {

    public class TestMapData {

        public int intVals[] = new int[1];

        public int IBLN[] = new int[NLA];

        public int ISNA[] = new int[NLA];

        public int LIBAR[] = new int[NAP];

        public int LOBAR[] = new int[NAP];

        public int LCONTR[] = new int[NLA];

        public int LGEOM[] = new int[NLA * 4];

        public int LTURN[] = new int[NLA];

        public int LWID[] = new int[NLA];

        public double BASELX[] = new double[NLA];

        public double BASELY[] = new double[NLA];

        public double ENDLNX[] = new double[NLA];

        public double ENDLNY[] = new double[NLA];
    }

    public class TestSignalData {

        public double doubleVals[] = new double[2];

        public double TCAMSP[] = new double[NCM];

        public int intVals[] = new int[8];

        public int ICAMPH[] = new int[NCM];

        public int INTER[] = new int[NRG];

        public int ISISET[] = new int[(NCM + 2 + NON + NON) * NIL];

        public final int ICONTR = 0;

        public final int NIBL = 1;

        public final int IARRPH = 2;

        public final int ICAMPC = 3;

        public final int ICAMPO = 4;

        public final int ICPHAS = 5;

        public final int IGO = 6;

        public final int NCAMSP = 7;

        // SPAT data float value index
        public int TP = 0;

        public int TR = 1;

        public final int ICPSIG = 5;
    }

    /** Logger for convenience. */
    private static final Logger logger = LoggerFactory.getLogger(SecondMessageTest.class);

    /**
     * @return the suite of tests being tested
     */
    public static Test suite() {
        return new TestSuite(SecondMessageTest.class);
    }

    private final int NLA = 50;

    private final int NAP = 16;

    private final double FEET_TO_CENTIMETERS_CONVERSION = 1.0;

    private final int NCM = 72;

    private final int NRG = 4;

    private final int NON = 16;

    private final int NIL = 25;

    private final int ICNEMA = 16;

    private final int ICNEMV = 17;

    /**
     * Create the test case
     * 
     * @param testName name of the test case
     */
    public SecondMessageTest(String testName) {
        super(testName);
    }

    private long getSignalLightStateFromSignalDataTest(TestSignalData signalDataParam, int camStackPosition, int laneNumber) {

        int simproSignal = signalDataParam.ISISET[(camStackPosition - 1) * NIL + laneNumber - 1];

        long signalLightState = 0;

        if (simproSignal == 1) {
            signalLightState = SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 2) {
            signalLightState = SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 3) {
            signalLightState = SignalLightState.BALL_RED;
        }
        else if (simproSignal == 4) {
            signalLightState = SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 5) {
            signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 6) {
            signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.BALL_RED;
        }
        else if (simproSignal == 7) {
            signalLightState = SignalLightState.LEFT_ARROW_YELLOW | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 8) {
            signalLightState = SignalLightState.LEFT_ARROW_YELLOW | SignalLightState.BALL_RED;
        }
        else if (simproSignal == 9) {
            signalLightState = SignalLightState.LEFT_ARROW_RED | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 10) {
            signalLightState = SignalLightState.LEFT_ARROW_RED | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 11) {
            signalLightState = SignalLightState.STRAIGHT_GREEN | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 12) {
            signalLightState = SignalLightState.STRAIGHT_GREEN | SignalLightState.BALL_RED;
        }
        else if (simproSignal == 13) {
            signalLightState = SignalLightState.STRAIGHT_YELLOW | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 14) {
            signalLightState = SignalLightState.STRAIGHT_YELLOW | SignalLightState.BALL_RED;
        }
        else if (simproSignal == 15) {
            signalLightState = SignalLightState.STRAIGHT_RED | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 16) {
            signalLightState = SignalLightState.STRAIGHT_RED | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 17) {
            signalLightState = SignalLightState.RIGHT_ARROW_GREEN | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 18) {
            signalLightState = SignalLightState.RIGHT_ARROW_GREEN | SignalLightState.BALL_RED;
        }
        else if (simproSignal == 19) {
            signalLightState = SignalLightState.RIGHT_ARROW_YELLOW | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 20) {
            signalLightState = SignalLightState.RIGHT_ARROW_YELLOW | SignalLightState.BALL_RED;
        }
        else if (simproSignal == 21) {
            signalLightState = SignalLightState.RIGHT_ARROW_RED | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 22) {
            signalLightState = SignalLightState.RIGHT_ARROW_RED | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 23) {
            signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.BALL_GREEN;
        }
        else if (simproSignal == 24) {
            signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.BALL_YELLOW;
        }
        else if (simproSignal == 25) {
            signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.BALL_RED;
        }

        return signalLightState;
    }

    public TestMapData initializeMapData() {
        TestMapData mapdata = new TestMapData();

        int[] localIntVals = { 16 };
        mapdata.intVals = localIntVals;

        int[] ibln = new int[50];
        ibln[0] = 1;
        ibln[1] = 2;
        ibln[4] = 3;
        ibln[5] = 4;
        ibln[8] = 5;
        ibln[9] = 6;
        ibln[12] = 7;
        ibln[13] = 8;
        mapdata.IBLN = ibln;

        int[] isna = new int[50];
        isna[0] = 1;
        isna[1] = 1;
        isna[2] = 8;
        isna[3] = 8;
        isna[4] = 2;
        isna[5] = 2;
        isna[6] = 5;
        isna[7] = 5;
        isna[8] = 3;
        isna[9] = 3;
        isna[10] = 6;
        isna[11] = 6;
        isna[12] = 4;
        isna[13] = 4;
        isna[14] = 7;
        isna[15] = 7;
        mapdata.ISNA = isna;

        int[] libar = { 1, 2, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        mapdata.LIBAR = libar;

        int[] lobar = { -1, -1, -1, -1, 1, 2, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1 };
        mapdata.LOBAR = lobar;

        int[] lcontr = new int[50];
        lcontr[0] = 5;
        lcontr[1] = 7;
        lcontr[2] = 1;
        lcontr[3] = 1;
        lcontr[4] = 5;
        lcontr[5] = 7;
        lcontr[6] = 1;
        lcontr[7] = 1;
        lcontr[8] = 5;
        lcontr[9] = 7;
        lcontr[10] = 1;
        lcontr[11] = 1;
        lcontr[12] = 5;
        lcontr[13] = 7;
        lcontr[14] = 1;
        lcontr[15] = 1;
        mapdata.LCONTR = lcontr;

        int[] lgeom = new int[200];
        lgeom[50] = 1100;
        lgeom[51] = 1100;
        lgeom[52] = 250;
        lgeom[53] = 250;
        lgeom[54] = 1100;
        lgeom[55] = 1100;
        lgeom[56] = 250;
        lgeom[57] = 250;
        lgeom[58] = 1100;
        lgeom[59] = 1100;
        lgeom[60] = 250;
        lgeom[61] = 250;
        lgeom[62] = 1100;
        lgeom[63] = 1100;
        lgeom[64] = 250;
        lgeom[65] = 250;
        lgeom[150] = 1100;
        lgeom[151] = 1100;
        lgeom[152] = 250;
        lgeom[153] = 250;
        lgeom[154] = 1100;
        lgeom[155] = 1100;
        lgeom[156] = 250;
        lgeom[157] = 250;
        lgeom[158] = 1100;
        lgeom[159] = 1100;
        lgeom[160] = 250;
        lgeom[161] = 250;
        lgeom[162] = 1100;
        lgeom[163] = 1100;
        lgeom[164] = 250;
        lgeom[165] = 250;
        mapdata.LGEOM = lgeom;

        int[] lturn = new int[50];
        lturn[0] = 6;
        lturn[1] = 3;
        lturn[4] = 6;
        lturn[5] = 3;
        lturn[8] = 6;
        lturn[9] = 3;
        lturn[12] = 6;
        lturn[13] = 3;
        mapdata.LTURN = lturn;

        int[] lwid = new int[50];
        lwid[0] = 12;
        lwid[1] = 12;
        lwid[2] = 12;
        lwid[3] = 12;
        lwid[4] = 12;
        lwid[5] = 12;
        lwid[6] = 12;
        lwid[7] = 12;
        lwid[8] = 12;
        lwid[9] = 12;
        lwid[10] = 12;
        lwid[11] = 12;
        lwid[12] = 12;
        lwid[13] = 12;
        lwid[14] = 12;
        lwid[15] = 12;
        mapdata.LWID = lwid;

        double[] baselx = new double[50];
        baselx[0] = 4994.0;
        baselx[1] = 4982.0;
        baselx[2] = 4956.0;
        baselx[3] = 4956.0;
        baselx[4] = 6144.0;
        baselx[5] = 6144.0;
        baselx[6] = 5006.0;
        baselx[7] = 5018.0;
        baselx[8] = 5006.0;
        baselx[9] = 5018.0;
        baselx[10] = 5044.0;
        baselx[11] = 5044.0;
        baselx[12] = 3856.0;
        baselx[13] = 3856.0;
        baselx[14] = 4994.0;
        baselx[15] = 4982.0;
        mapdata.BASELX = baselx;

        double[] basely = new double[50];
        basely[0] = 6144.0;
        basely[1] = 6144.0;
        basely[2] = 5006.0;
        basely[3] = 5018.0;
        basely[4] = 5006.0;
        basely[5] = 5018.0;
        basely[6] = 5044.0;
        basely[7] = 5044.0;
        basely[8] = 3856.0;
        basely[9] = 3856.0;
        basely[10] = 4994.0;
        basely[11] = 4982.0;
        basely[12] = 4994.0;
        basely[13] = 4982.0;
        basely[14] = 4956.0;
        basely[15] = 4956.0;
        mapdata.BASELY = basely;

        double[] endlnx = new double[50];
        endlnx[0] = 4994.0;
        endlnx[1] = 4982.0;
        endlnx[2] = 4706.0;
        endlnx[3] = 4706.0;
        endlnx[4] = 5024.0;
        endlnx[5] = 5024.0;
        endlnx[6] = 5006.0;
        endlnx[7] = 5018.0;
        endlnx[8] = 5006.0;
        endlnx[9] = 5018.0;
        endlnx[10] = 5294.0;
        endlnx[11] = 5294.0;
        endlnx[12] = 4976.0;
        endlnx[13] = 4976.0;
        endlnx[14] = 4994.0;
        endlnx[15] = 4982.0;
        mapdata.ENDLNX = endlnx;

        double[] endlny = new double[50];
        endlny[0] = 5024.0;
        endlny[1] = 5024.0;
        endlny[2] = 5006.0;
        endlny[3] = 5018.0;
        endlny[4] = 5006.0;
        endlny[5] = 5018.0;
        endlny[6] = 5294.0;
        endlny[7] = 5294.0;
        endlny[8] = 4976.0;
        endlny[9] = 4976.0;
        endlny[10] = 4994.0;
        endlny[11] = 4982.0;
        endlny[12] = 4994.0;
        endlny[13] = 4982.0;
        endlny[14] = 4706.0;
        endlny[15] = 4706.0;
        mapdata.ENDLNY = endlny;

        return mapdata;
    }

    public TestSignalData initializeSignalData() {
        TestSignalData output = new TestSignalData();

        double[] testDoubleVals = new double[2];
        testDoubleVals[0] = 1.0;
        testDoubleVals[1] = 19.0;
        output.doubleVals = testDoubleVals;

        double[] testTCAMSP = new double[72];
        testTCAMSP[0] = 20.0;
        testTCAMSP[1] = 3.0;
        testTCAMSP[2] = 1.0;
        testTCAMSP[3] = 40.0;
        testTCAMSP[4] = 3.0;
        testTCAMSP[5] = 1.0;
        output.TCAMSP = testTCAMSP;

        int[] testIntVals = new int[8];
        testIntVals[0] = 5;
        testIntVals[1] = 8;
        testIntVals[3] = 1;
        testIntVals[4] = 1;
        testIntVals[5] = 1;
        testIntVals[7] = 6;
        output.intVals = testIntVals;

        int[] testICAMPH = new int[72];
        testICAMPH[0] = 1;
        testICAMPH[1] = 1;
        testICAMPH[2] = 1;
        testICAMPH[3] = 2;
        testICAMPH[4] = 2;
        testICAMPH[5] = 2;
        output.ICAMPH = testICAMPH;

        int[] testINTER = { 1, 1, 1, 1 };
        output.INTER = testINTER;

        int[] testISISET = new int[2650];
        testISISET[0] = 1;
        testISISET[1] = 1;
        testISISET[2] = 3;
        testISISET[3] = 3;
        testISISET[4] = 1;
        testISISET[5] = 1;
        testISISET[6] = 3;
        testISISET[7] = 3;
        testISISET[25] = 2;
        testISISET[26] = 2;
        testISISET[27] = 3;
        testISISET[28] = 3;
        testISISET[29] = 2;
        testISISET[30] = 2;
        testISISET[31] = 3;
        testISISET[32] = 3;
        testISISET[50] = 3;
        testISISET[51] = 3;
        testISISET[52] = 3;
        testISISET[53] = 3;
        testISISET[57] = 3;
        testISISET[75] = 3;
        testISISET[76] = 3;
        testISISET[77] = 1;
        testISISET[78] = 1;
        testISISET[79] = 3;
        testISISET[80] = 3;
        testISISET[81] = 1;
        testISISET[82] = 1;
        testISISET[100] = 3;
        testISISET[101] = 3;
        testISISET[102] = 2;
        testISISET[103] = 2;
        testISISET[104] = 3;
        testISISET[105] = 3;
        testISISET[106] = 2;
        testISISET[107] = 2;
        testISISET[125] = 3;
        testISISET[126] = 3;
        testISISET[127] = 3;
        testISISET[128] = 3;
        testISISET[129] = 3;
        testISISET[130] = 3;
        testISISET[131] = 3;
        testISISET[132] = 3;
        output.ISISET = testISISET;

        return output;

    }

    public void testApp() {

        ObjectFactory factory = new ObjectFactory();

        TestMapData mapdata = initializeMapData();

        MapData mapDataMessage = factory.createMapData();
        mapDataMessage.setMsgID("1");
        mapDataMessage.setMsgCnt((short)1);

        mapDataMessage.setCrc(null);

        MapData.Intersections intersections = factory.createMapDataIntersections();
        mapDataMessage.setIntersections(intersections);
        List<Intersection> intersectionList = intersections.getIntersection();

        // Create a single intersection object and add to the message
        Intersection intersection = factory.createIntersection();
        intersection.setId(null);
        Position3D pos3D = factory.createPosition3D();
        pos3D.setLat(0);
        pos3D.setLong(0);
        pos3D.setElevation(new byte[] { 0, 0 });

        intersection.setRefPoint(pos3D);
        intersectionList.add(intersection);
        Approaches approaches = factory.createIntersectionApproaches();
        intersection.setApproaches(approaches);
        List<ApproachObject> approachList = approaches.getApproachObject();

        // Iterate over all lanes in the model
        int numLanes = 16;
        for (int i = 1; i <= numLanes; i++) {
            // Set the lane ID to be the index of the lane in the model
            byte laneID = (byte)i;

            // Get the ID of the approach/egress that the lane comprises
            int approachID = mapdata.ISNA[i - 1];

            boolean isEgress = false;
            // Get the ID of the leg the the approach comprises
            int legID = mapdata.LIBAR[approachID - 1];
            // If the approach does not map to inbound approaches it must be an
            // outbound approach
            if (legID <= 0) {
                isEgress = true;
                legID = mapdata.LOBAR[approachID - 1];
            }

            // Grow the approach list to the required size and obtain the
            // correct approach object
            while (legID - 1 >= approachList.size()) {
                approachList.add(factory.createApproachObject());
            }
            ApproachObject leg = approachList.get(legID - 1);

            // Find or create the approach to add the vehicle lane too
            Approach approach = null;
            if (isEgress) {
                approach = leg.getEgress();
                if (approach == null) {
                    approach = factory.createApproach();
                    leg.setEgress(approach);
                }
            }
            else {
                approach = leg.getApproach();
                if (approach == null) {
                    approach = factory.createApproach();
                    leg.setApproach(approach);
                }
            }
            approach.setId((short)legID);

            DrivingLanes drivingLanes = approach.getDrivingLanes();
            if (drivingLanes == null) {
                drivingLanes = factory.createApproachDrivingLanes();
                approach.setDrivingLanes(drivingLanes);
            }

            List<VehicleReferenceLane> vehicleReferenceLanes = drivingLanes.getVehicleReferenceLane();

            VehicleReferenceLane vehicleReferenceLane = factory.createVehicleReferenceLane();

            // Set the lane reference ID
            vehicleReferenceLane.setLaneNumber(new byte[] { laneID });

            // Set the lane width
            int laneWidth = mapdata.LWID[i - 1];
            vehicleReferenceLane.setLaneWidth((int)(laneWidth * FEET_TO_CENTIMETERS_CONVERSION));

            // Set the lane turning attributes
            int laneAttributes = mapdata.LTURN[i - 1];
            vehicleReferenceLane.setLaneAttributes(laneAttributes);

            // Set the lane path
            short baseX = (short)(mapdata.BASELX[i - 1] * FEET_TO_CENTIMETERS_CONVERSION);
            short baseY = (short)(mapdata.BASELY[i - 1] * FEET_TO_CENTIMETERS_CONVERSION);
            short endX = (short)(mapdata.ENDLNX[i - 1] * FEET_TO_CENTIMETERS_CONVERSION);
            short endY = (short)(mapdata.ENDLNY[i - 1] * FEET_TO_CENTIMETERS_CONVERSION);

            logger.debug("MAPDATA LaneId: " + i + " " + laneID + " baseXY (" + baseX + ", " + baseY + ") endXY (" + endX + ", " + endY + ")");

            List<String> nodeList = vehicleReferenceLane.getNodeList();

            ByteBuffer endBuffer = ByteBuffer.allocate(4);
            endBuffer.putShort(endX);
            endBuffer.putShort(endY);

            String endNode = Base64.encodeBase64String(endBuffer.array());
            nodeList.add(endNode);

            ByteBuffer baseBuffer = ByteBuffer.allocate(4);
            baseBuffer.putShort(baseX);
            baseBuffer.putShort(baseY);

            String baseNode = Base64.encodeBase64String(baseBuffer.array());
            nodeList.add(baseNode);

            vehicleReferenceLanes.add(vehicleReferenceLane);
        }

        TestSignalData spatData = initializeSignalData();

        SPAT spatMessage = factory.createSPAT();
        SPAT.IntersectionStates SPATintersections = factory.createSPATIntersections();
        spatMessage.setIntersectionStates(SPATintersections);
        List<IntersectionState> intersectionStates = SPATintersections.getIntersectionState();
        IntersectionState intersectionState = factory.createIntersectionState();
        intersectionStates.add(intersectionState);
        States states = factory.createIntersectionStateStates();
        intersectionState.setStates(states);
        List<MovementState> movementStates = states.getMovementState();

        int[] ibln = new int[50];
        ibln[0] = 1;
        ibln[1] = 2;
        ibln[4] = 3;
        ibln[5] = 4;
        ibln[8] = 5;
        ibln[9] = 6;
        ibln[12] = 7;
        ibln[13] = 8;

        // Iterate over all lanes in the model
        numLanes = 16;
        for (int i = 1; i <= numLanes; i++) {
            // If the current lane is an inbound lane, then create the movement
            // state
            int inboundID = ibln[i - 1];
            if (inboundID == 0) {
                continue;
            }

            MovementState movementState = factory.createMovementState();
            movementState.setLaneSet(new byte[] { (byte)i });

            // Set the signal light state of this lane in the SPAT message
            long signalLightState = 0;
            signalLightState = getSignalLightStateFromSignalDataTest(spatData, 1, inboundID);
            movementState.setCurrState(signalLightState);
            if (i == 1) {
                movementState.setCurrState(SignalLightState.LEFT_ARROW_YELLOW);
            }
            if (i == 2) {
                movementState.setCurrState(SignalLightState.LEFT_ARROW_RED);
            }

            // Fill in timing information for Pre-timed Controller
            if (spatData.ICONTR == spatData.ICPSIG) {

                // Set the time to change signals of this lane in the SPAT
                // message
                double timeToChange = spatData.TR;
                int currState = spatData.ISISET[(spatData.ICAMPC - 1) * NIL + inboundID - 1];
                // int currState =
                // signalDataWrapper.getISISET(signalDataWrapper.getICAMPC(),
                // inboundID);
                int lookAheadIndex = spatData.ICAMPC;
                for (int n = 0; n < spatData.NCAMSP; n++) {
                    lookAheadIndex++;
                    if (lookAheadIndex > spatData.NCAMSP) {
                        lookAheadIndex = 1;
                    }
                    if (spatData.ISISET[(lookAheadIndex - 1) * NIL + inboundID - 1] == currState) {
                        timeToChange = timeToChange + spatData.TCAMSP[lookAheadIndex - 1];
                    }
                    else {
                        break;
                    }
                }

                movementState.setTimeToChange((int)timeToChange);
            }
            else if (spatData.ICONTR == ICNEMA || spatData.ICONTR == ICNEMV) {
                // Set the time to change signals of this lane in the SPAT
                // message
                double timeToChange = spatData.TR;
                int currState = spatData.ISISET[(spatData.ICAMPC - 1) * NIL + inboundID - 1];
                int lookAheadIndex = spatData.ICAMPC;
                for (int n = 0; n < spatData.NCAMSP; n++) {
                    lookAheadIndex++;
                    if (lookAheadIndex > spatData.NCAMSP) {
                        lookAheadIndex = 1;
                    }
                    if (spatData.ISISET[(lookAheadIndex - 1) * NIL + inboundID - 1] == currState) {
                        timeToChange = timeToChange + spatData.TCAMSP[lookAheadIndex - 1];
                    }
                    else {
                        break;
                    }
                }

                movementState.setTimeToChange((int)timeToChange);
            }

            // Add the information to the SPAT message
            movementStates.add(movementState);
        }

        assertTrue(true);
    }
}
