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
package org.etexascode.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.powermock.api.mockito.PowerMockito;

public class TestUtilsStaticData {

    // Detector data
    DetectorManager popDetManRes = new DetectorManager();

    // default lane geom
    int[] defLaneGeom = new int[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

    // PopulateLaneManager data
    ModelData popLanManModel = null;

    MapDataRetriever popLanManMap = null;

    LaneManager popLanManExpected = null;

    // PopulateLane data
    final int laneIDCond1 = 5;

    final int laneIDCond2 = 6;

    final double popLaneXMidpoint = 0.0;

    final double popLaneYMidpoint = 0.0;

    MapDataRetriever popLaneMapRetrievCond1 = null;

    MapDataRetriever popLaneMapRetrievCond2 = null;

    double[] popLaneEdges = null;

    Lane popLaneExpectedCond1 = null;

    Lane popLaneExpectedCond2 = null;

    ParsingLaneDetails modifyEdgesCon1 = null;

    ParsingLaneDetails modifyEdgesCon4 = null;

    ParsingLaneDetails modifyEdgesCon1Expected = null;

    ParsingLaneDetails modifyEdgesCon4Expected = null;

    // Get speed limit data
    final String inboundStr = "INBOUND";

    final String speedLimitCond1 = "INBOUND";

    final String speedLimitCond2 = "OUTBOUND";

    final int[] speedLimitLsl = new int[] { 0, 33 };

    final int speedLimitNumLs = 2;

    final int legID = 1;

    final double speedLimitExpectedCond1 = 0.0;

    final double speedLimitExpectedCond2 = 10.06;

    // Num Lanes from Speed Limit data
    final int[] numLanesSpeedLimitLslCond1 = new int[] { 4, 4, 3, 2, 0, 8, 9 };

    final int[] numLanesSpeedLimitLslCond2 = new int[] { 5, 4, 9, 7, 1 };

    final int numLanesExpectedCond1 = 4;

    final int numLanesExpectedCond2 = 0;

    // Convert Edges to Nodes data
    List<LaneNode> edgesToNodesExpected = null;

    final double[] edgesToNodesEdges = new double[] { 0.0, 1.0, 5.0, 6.0 };

    final int edgesToNodesWidth = 8;

    // Pop Metadata Data
    ModelData metaDataInput = null;

    SimMetaData metaDataExpected = null;

    // Movement Code data
    int moveCodeCond1 = 1;

    int moveCodeCond2 = 2;

    int moveCodeCond3 = 4;

    int moveCodeCond4 = 8;

    int moveCodeCond5 = 16;

    int moveCodeCond6 = 0;

    int moveCodeCond7 = 3;

    int moveCodeCond8 = 7;

    int moveCodeCond9 = 15;

    int moveCodeCond10 = 6;

    int moveCodeCond11 = 14;

    int moveCodeCond12 = 12;

    Map<Integer, LaneMovement> moveCodeExpected1 = null;

    Map<Integer, LaneMovement> moveCodeExpected2 = null;

    Map<Integer, LaneMovement> moveCodeExpected3 = null;

    Map<Integer, LaneMovement> moveCodeExpected4 = null;

    Map<Integer, LaneMovement> moveCodeExpected5 = null;

    Map<Integer, LaneMovement> moveCodeExpected6 = null;

    Map<Integer, LaneMovement> moveCodeExpected7 = null;

    Map<Integer, LaneMovement> moveCodeExpected8 = null;

    Map<Integer, LaneMovement> moveCodeExpected9 = null;

    Map<Integer, LaneMovement> moveCodeExpected10 = null;

    Map<Integer, LaneMovement> moveCodeExpected11 = null;

    Map<Integer, LaneMovement> moveCodeExpected12 = null;

    // Turning Lane data
    ParsingLaneDetails turningLaneCond1 = null;

    ParsingLaneDetails turningLaneCond2 = null;

    ParsingLaneDetails turningLaneCond3 = null;

    ParsingLaneDetails turningLaneCond4 = null;

    ParsingLaneDetails turningLaneCond5 = null;

    ParsingLaneDetails turningLaneCond1Expected = null;

    ParsingLaneDetails turningLaneCond2Expected = null;

    ParsingLaneDetails turningLaneCond3Expected = null;

    ParsingLaneDetails turningLaneCond4Expected = null;

    ParsingLaneDetails turningLaneCond5Expected = null;

    // Merge Lane data
    ParsingLaneDetails mergeLaneCond1 = null;

    ParsingLaneDetails mergeLaneCond2 = null;

    ParsingLaneDetails mergeLaneCond3 = null;

    ParsingLaneDetails mergeLaneCond4 = null;

    ParsingLaneDetails mergeLaneCond5 = null;

    ParsingLaneDetails mergeLaneCond1Expected = null;

    ParsingLaneDetails mergeLaneCond2Expected = null;

    ParsingLaneDetails mergeLaneCond3Expected = null;

    ParsingLaneDetails mergeLaneCond4Expected = null;

    ParsingLaneDetails mergeLaneCond5Expected = null;

    @Before
    public void setUp() {
        genPopLanMan();

        genPopLaneGeneral();
        genPopLaneCase1();
        genPopLaneCase2();

        genModifyEdgesCase1();
        genModifyEdgesCase4();

        genEdgesToNodes();

        // Metadata setup
        genMetaDataInput();
        genMetaDataExpected();

        // Move Code setup
        genMoveCodeCase1();
        genMoveCodeCase2();
        genMoveCodeCase3();
        genMoveCodeCase4();
        genMoveCodeCase5();
        genMoveCodeCase6();
        genMoveCodeCase7();
        genMoveCodeCase8();
        genMoveCodeCase9();
        genMoveCodeCase10();
        genMoveCodeCase11();
        genMoveCodeCase12();

        // turn lane setup
        genTurnLaneCase1();
        genTurnLaneCase2();
        genTurnLaneCase3();
        genTurnLaneCase4();
        genTurnLaneCase5();

        // Merge lane setup
        genMergeLaneCase1();
        genMergeLaneCase2();
        genMergeLaneCase3();
        genMergeLaneCase4();
        genMergeLaneCase5();
    }

    @After
    public void tearDown() {
        popLanManModel = null;
        popLanManMap = null;
        popLanManExpected = null;

        popLaneMapRetrievCond1 = null;
        popLaneMapRetrievCond2 = null;
        popLaneEdges = null;

        popLaneExpectedCond1 = null;
        popLaneExpectedCond2 = null;

        modifyEdgesCon1 = null;
        modifyEdgesCon4 = null;

        modifyEdgesCon1Expected = null;
        modifyEdgesCon4Expected = null;

        edgesToNodesExpected = null;

        // metaDataInput = null;
        // metaDataExpected = null;

        moveCodeExpected1 = null;
        moveCodeExpected2 = null;
        moveCodeExpected3 = null;
        moveCodeExpected4 = null;
        moveCodeExpected5 = null;
        moveCodeExpected6 = null;
        moveCodeExpected7 = null;
        moveCodeExpected8 = null;
        moveCodeExpected9 = null;
        moveCodeExpected10 = null;
        moveCodeExpected11 = null;
        moveCodeExpected12 = null;

        mergeLaneCond1 = null;
        mergeLaneCond2 = null;
        mergeLaneCond3 = null;
        mergeLaneCond4 = null;
        mergeLaneCond5 = null;

        mergeLaneCond1Expected = null;
        mergeLaneCond2Expected = null;
        mergeLaneCond3Expected = null;
        mergeLaneCond4Expected = null;

        turningLaneCond1 = null;
        turningLaneCond2 = null;
        turningLaneCond3 = null;
        turningLaneCond4 = null;

        turningLaneCond1Expected = null;
        turningLaneCond2Expected = null;
        turningLaneCond3Expected = null;
        turningLaneCond4Expected = null;
        turningLaneCond5Expected = null;
    }

    @Test
    public void testPopLanMan() {
        LaneManager res = UtilsStaticData.populateLaneManager(popLanManModel, popLanManMap);
        assertEquals(popLanManExpected, res);
    }

    @Test
    public void testPopLaneCond1() {
        Lane res = UtilsStaticData.populateLane(laneIDCond1, popLaneMapRetrievCond1, popLaneEdges, popLaneXMidpoint, popLaneYMidpoint, popLanManMap.getLGEOM());
        assertEquals(popLaneExpectedCond1, res);
    }

    @Test
    public void testPopLaneCond2() {
        Lane res = UtilsStaticData.populateLane(laneIDCond2, popLaneMapRetrievCond2, popLaneEdges, popLaneXMidpoint, popLaneYMidpoint, popLanManMap.getLGEOM());
        assertEquals(popLaneExpectedCond2, res);
    }

    @Test
    public void testDefaultConstructor() {
        UtilsStaticData stat = new UtilsStaticData();
        assertTrue(stat instanceof UtilsStaticData);
    }

    @Test
    public void testModifyEdgesCond1() {
        modifyEdgesTestUtil(modifyEdgesCon1Expected, modifyEdgesCon1);
    }

    @Test
    public void testModifyEdgesCond2() {
        modifyEdgesTestUtil(turningLaneCond1Expected, turningLaneCond1);
    }

    @Test
    public void testModifyEdgesCond3() {
        modifyEdgesTestUtil(mergeLaneCond1Expected, mergeLaneCond1);
    }

    @Test
    public void testModifyEdgesCond4() {
        modifyEdgesTestUtil(modifyEdgesCon4Expected, modifyEdgesCon4);
    }

    @Test
    public void testConvertEdgesToNodes() {
        List<LaneNode> res = UtilsStaticData.convertEdgesToNodes(edgesToNodesEdges, edgesToNodesWidth);

        assertEquals(edgesToNodesExpected.size(), res.size());

        for (int i = 0; i < res.size(); i++) {
            assertEquals("assertion failed on index " + i, edgesToNodesExpected.get(i), res.get(i));
        }
    }

    @Test
    public void testNumLanesSpeedLimitCond1() {
        assertEquals(numLanesExpectedCond1, UtilsStaticData.numLanesFromSpeedLimit(numLanesSpeedLimitLslCond1));
    }

    @Test
    public void testNumLanesSpeedLimitCond2() {
        assertEquals(numLanesExpectedCond2, UtilsStaticData.numLanesFromSpeedLimit(numLanesSpeedLimitLslCond2));
    }

    @Test
    public void testGetSpeedLimitCond1() {
        assertEquals(speedLimitExpectedCond1, UtilsStaticData.getSpeedLimit(inboundStr, speedLimitCond1, speedLimitLsl, speedLimitNumLs, legID), 0.05);
    }

    @Test
    public void testGetSpeedLimitCond2() {
        assertEquals(speedLimitExpectedCond2, UtilsStaticData.getSpeedLimit(inboundStr, speedLimitCond2, speedLimitLsl, speedLimitNumLs, legID), 0.05);
    }

    @Test
    public void testPopulateMetaData() {
        SimMetaData res = UtilsStaticData.populateMetaData(metaDataInput);

        assertEquals(metaDataExpected.getFirstStep(), res.getFirstStep());
        assertEquals(metaDataExpected.getStepSize(), res.getStepSize(), 0.05);
        assertEquals(metaDataExpected.getSimHeight(), res.getSimHeight(), 0.05);
        assertEquals(metaDataExpected.getSimWidth(), res.getSimWidth(), 0.05);
        assertEquals(metaDataExpected.getMaxSteps(), res.getMaxSteps());
    }

    @Test
    public void testParseMovementCodeCond1() {
        parseMovementCodeUtil(moveCodeCond1, moveCodeExpected1);
    }

    @Test
    public void testParseMovementCodeCond2() {
        parseMovementCodeUtil(moveCodeCond2, moveCodeExpected2);
    }

    @Test
    public void testParseMovementCodeCond3() {
        parseMovementCodeUtil(moveCodeCond3, moveCodeExpected3);
    }

    @Test
    public void testParseMovementCodeCond4() {
        parseMovementCodeUtil(moveCodeCond4, moveCodeExpected4);
    }

    @Test
    public void testParseMovementCodeCond5() {
        parseMovementCodeUtil(moveCodeCond5, moveCodeExpected5);
    }

    @Test
    public void testParseMovementCodeCond6() {
        parseMovementCodeUtil(moveCodeCond6, moveCodeExpected6);
    }

    @Test
    public void testParseMovementCodeCond7() {
        parseMovementCodeUtil(moveCodeCond7, moveCodeExpected7);
    }

    @Test
    public void testParseMovementCodeCond8() {
        parseMovementCodeUtil(moveCodeCond8, moveCodeExpected8);
    }

    @Test
    public void testParseMovementCodeCond9() {
        parseMovementCodeUtil(moveCodeCond9, moveCodeExpected9);
    }

    @Test
    public void testParseMovementCodeCond10() {
        parseMovementCodeUtil(moveCodeCond10, moveCodeExpected10);
    }

    @Test
    public void testParseMovementCodeCond11() {
        parseMovementCodeUtil(moveCodeCond11, moveCodeExpected11);
    }

    @Test
    public void testParseMovementCodeCond12() {
        parseMovementCodeUtil(moveCodeCond12, moveCodeExpected12);
    }

    @Test
    public void testTurnLaneCond1() {
        turnLaneTestUtil(turningLaneCond1Expected, turningLaneCond1);
    }

    @Test
    public void testTurnLaneCond2() {
        turnLaneTestUtil(turningLaneCond2Expected, turningLaneCond2);
    }

    @Test
    public void testTurnLaneCond3() {
        turnLaneTestUtil(turningLaneCond3Expected, turningLaneCond3);
    }

    @Test
    public void testTurnLaneCond4() {
        turnLaneTestUtil(turningLaneCond4Expected, turningLaneCond4);
    }

    @Test
    public void testTurnLaneCond5() {
        turnLaneTestUtil(turningLaneCond5Expected, turningLaneCond5);
    }

    @Test
    public void testParseMergingLaneCond1() {
        mergeLaneTestUtil(mergeLaneCond1Expected, mergeLaneCond1);
    }

    @Test
    public void testParseMergingLaneCond2() {
        mergeLaneTestUtil(mergeLaneCond2Expected, mergeLaneCond2);
    }

    @Test
    public void testParseMergingLaneCond3() {
        mergeLaneTestUtil(mergeLaneCond3Expected, mergeLaneCond3);
    }

    @Test
    public void testParseMergingLaneCond4() {
        mergeLaneTestUtil(mergeLaneCond4Expected, mergeLaneCond4);
    }

    @Test
    public void testParseMergingLaneCond5() {
        mergeLaneTestUtil(mergeLaneCond5Expected, mergeLaneCond5);
    }

    @Test
    public void testPopulateDetectorManager() {
        DetectorManager tmp = UtilsStaticData.populateDetectorManager();
        assertEquals(popDetManRes, tmp);
    }

    private void parseMovementCodeUtil(int code, Map<Integer, LaneMovement> expected) {
        Map<Integer, LaneMovement> res = UtilsStaticData.parseMovementCode(code);

        assertEquals(expected.keySet(), res.keySet());

        for (Integer i : res.keySet()) {
            assertEquals("Failing on key " + i.toString(), expected.get(i), res.get(i));
        }
    }

    private void modifyEdgesTestUtil(ParsingLaneDetails expected, ParsingLaneDetails lane) {
        UtilsStaticData.modifyEdgesForTurningLane(lane.edges, lane.laneGeometry, (lane.geomIndex / 4) + 1, lane.xMidpoint, lane.yMidpoint);

        laneParsingTests(expected, lane);
    }

    private void turnLaneTestUtil(ParsingLaneDetails expected, ParsingLaneDetails turnLane) {
        UtilsStaticData.parseTurningLane(turnLane.edges, turnLane.laneGeometry, turnLane.geomIndex, turnLane.xMidpoint, turnLane.yMidpoint);

        laneParsingTests(expected, turnLane);
    }

    private void mergeLaneTestUtil(ParsingLaneDetails expected, ParsingLaneDetails mergeLane) {
        UtilsStaticData.parseMergingLane(mergeLane.edges, mergeLane.laneGeometry, mergeLane.geomIndex, mergeLane.xMidpoint, mergeLane.yMidpoint);

        laneParsingTests(expected, mergeLane);
    }

    private void laneParsingTests(ParsingLaneDetails expected, ParsingLaneDetails observed) {
        assertEquals(expected.edges.length, observed.edges.length);

        for (int i = 0; i < expected.edges.length; i++) {
            assertEquals("failed on index " + i, expected.edges[i], observed.edges[i], 0.05);
        }
    }

    private void genPopLanMan() {
        double lat = 5.0;
        double lon = 6.0;

        popLanManModel = PowerMockito.mock(ModelData.class);

        PowerMockito.when(popLanManModel.getLatitude()).thenReturn(lat);
        PowerMockito.when(popLanManModel.getLongitude()).thenReturn(lon);
        PowerMockito.when(popLanManModel.getCenterX()).thenReturn(0.0);
        PowerMockito.when(popLanManModel.getCenterY()).thenReturn(0.0);

        popLanManMap = PowerMockito.mock(MapDataRetriever.class);

        PowerMockito.when(popLanManMap.getNRLAN()).thenReturn(4);

        PowerMockito.when(popLanManMap.getBASELX(1)).thenReturn(5.0);
        PowerMockito.when(popLanManMap.getBASELY(1)).thenReturn(5.0);
        PowerMockito.when(popLanManMap.getENDLNX(1)).thenReturn(-5.0);
        PowerMockito.when(popLanManMap.getENDLNY(1)).thenReturn(-5.0);
        PowerMockito.when(popLanManMap.getBASELX(2)).thenReturn(0.0);
        PowerMockito.when(popLanManMap.getBASELY(2)).thenReturn(0.0);
        PowerMockito.when(popLanManMap.getENDLNX(2)).thenReturn(-5.0);
        PowerMockito.when(popLanManMap.getENDLNY(2)).thenReturn(-5.0);
        PowerMockito.when(popLanManMap.getBASELX(3)).thenReturn(10.0);
        PowerMockito.when(popLanManMap.getBASELY(3)).thenReturn(10.0);
        PowerMockito.when(popLanManMap.getENDLNX(3)).thenReturn(-10.0);
        PowerMockito.when(popLanManMap.getENDLNY(3)).thenReturn(-10.0);
        PowerMockito.when(popLanManMap.getBASELX(4)).thenReturn(2.0);
        PowerMockito.when(popLanManMap.getBASELY(4)).thenReturn(2.0);
        PowerMockito.when(popLanManMap.getENDLNX(4)).thenReturn(-2.0);
        PowerMockito.when(popLanManMap.getENDLNY(4)).thenReturn(-2.0);

        PowerMockito.when(popLanManMap.getISNA(1)).thenReturn(1);
        PowerMockito.when(popLanManMap.getLIBAR(1)).thenReturn(1);
        PowerMockito.when(popLanManMap.getISNA(2)).thenReturn(2);
        PowerMockito.when(popLanManMap.getLIBAR(2)).thenReturn(2);
        PowerMockito.when(popLanManMap.getISNA(3)).thenReturn(3);
        PowerMockito.when(popLanManMap.getLIBAR(3)).thenReturn(3);
        PowerMockito.when(popLanManMap.getISNA(4)).thenReturn(4);
        PowerMockito.when(popLanManMap.getLIBAR(4)).thenReturn(4);

        PowerMockito.when(popLanManMap.getLWID(1)).thenReturn(4);
        PowerMockito.when(popLanManMap.getLWID(2)).thenReturn(5);
        PowerMockito.when(popLanManMap.getLWID(3)).thenReturn(6);
        PowerMockito.when(popLanManMap.getLWID(4)).thenReturn(7);

        PowerMockito.when(popLanManMap.getLTURN(1)).thenReturn(1);
        PowerMockito.when(popLanManMap.getLTURN(2)).thenReturn(1);
        PowerMockito.when(popLanManMap.getLTURN(3)).thenReturn(1);
        PowerMockito.when(popLanManMap.getLTURN(4)).thenReturn(1);

        PowerMockito.when(popLanManMap.getISLIM()).thenReturn(new int[] { 1, 1, 1, 1, 0 });

        PowerMockito.when(popLanManMap.getLGEOM()).thenReturn(defLaneGeom);

        popLanManExpected = new LaneManager();

        popLanManExpected.setLatitude(lat);
        popLanManExpected.setLongitude(lon);
        popLanManExpected.setElevation(0);

        Map<Integer, Lane> tmpLanes = new HashMap<Integer, Lane>();

        Lane l = new Lane();

        l.setType(Lane.INBOUND);
        l.setLaneId(1);
        l.setApproachId(1);
        l.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertFeetToMeters(1));

        List<LaneNode> lln = new ArrayList<LaneNode>(2);

        LaneNode ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(-5));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(-5));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(4));

        lln.add(ln);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(5));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(5));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(4));

        lln.add(ln);

        l.setLaneGeomList(lln);

        Map<Integer, LaneMovement> lmm = new HashMap<Integer, LaneMovement>();
        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);
        lmm.put(lm.getMovementId(), lm);
        l.setLaneMovements(lmm);

        tmpLanes.put(1, l);

        l = new Lane();

        l.setType(Lane.INBOUND);
        l.setLaneId(2);
        l.setApproachId(2);
        l.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertFeetToMeters(1));

        lln = new ArrayList<LaneNode>(2);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(-5));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(-5));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(5));

        lln.add(ln);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(0));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(0));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(5));

        lln.add(ln);

        l.setLaneGeomList(lln);

        lmm = new HashMap<Integer, LaneMovement>();
        lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);
        lmm.put(lm.getMovementId(), lm);
        l.setLaneMovements(lmm);

        tmpLanes.put(2, l);

        l = new Lane();

        l.setType(Lane.INBOUND);
        l.setLaneId(3);
        l.setApproachId(3);
        l.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertFeetToMeters(1));

        lln = new ArrayList<LaneNode>(2);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(-10));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(-10));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(6));

        lln.add(ln);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(10));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(10));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(6));

        lln.add(ln);

        l.setLaneGeomList(lln);

        lmm = new HashMap<Integer, LaneMovement>();
        lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);
        lmm.put(lm.getMovementId(), lm);
        l.setLaneMovements(lmm);

        tmpLanes.put(3, l);

        l = new Lane();

        l.setType(Lane.INBOUND);
        l.setLaneId(4);
        l.setApproachId(4);
        l.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertFeetToMeters(1));

        lln = new ArrayList<LaneNode>(2);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(-2));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(-2));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(7));

        lln.add(ln);

        ln = new LaneNode();

        ln.setX(UtilsUnitConversion.convertFeetToCentimeters(2));
        ln.setY(UtilsUnitConversion.convertFeetToCentimeters(2));
        ln.setWidth((int)UtilsUnitConversion.convertFeetToCentimeters(7));

        lln.add(ln);

        l.setLaneGeomList(lln);

        lmm = new HashMap<Integer, LaneMovement>();
        lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);
        lmm.put(lm.getMovementId(), lm);
        l.setLaneMovements(lmm);

        tmpLanes.put(4, l);

        popLanManExpected.setLanes(tmpLanes);
    }

    private void genPopLaneGeneral() {
        popLaneEdges = new double[] { 10.0, 20.0, 30.0, 40.0 };
    }

    private void genPopLaneCase1() {
        popLaneMapRetrievCond1 = PowerMockito.mock(MapDataRetriever.class);

        PowerMockito.when(popLaneMapRetrievCond1.getISNA(5)).thenReturn(5);
        PowerMockito.when(popLaneMapRetrievCond1.getISNA(6)).thenReturn(6);
        PowerMockito.when(popLaneMapRetrievCond1.getLIBAR(5)).thenReturn(1);
        PowerMockito.when(popLaneMapRetrievCond1.getLIBAR(6)).thenReturn(-1);
        PowerMockito.when(popLaneMapRetrievCond1.getISLIM()).thenReturn(new int[] { 1, 0 });
        PowerMockito.when(popLaneMapRetrievCond1.getLGEOM()).thenReturn(defLaneGeom);
        PowerMockito.when(popLaneMapRetrievCond1.getLTURN(5)).thenReturn(1);
        PowerMockito.when(popLaneMapRetrievCond1.getLTURN(6)).thenReturn(1);
        PowerMockito.when(popLaneMapRetrievCond1.getLWID(5)).thenReturn(0);
        PowerMockito.when(popLaneMapRetrievCond1.getLWID(6)).thenReturn(0);

        popLaneExpectedCond1 = new Lane();

        popLaneExpectedCond1.setApproachId(5);
        popLaneExpectedCond1.setLaneId(5);
        popLaneExpectedCond1.setType(Lane.INBOUND);
        popLaneExpectedCond1.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertFeetToMeters(1));

        List<LaneNode> geomList = new ArrayList<LaneNode>(2);

        LaneNode ln = new LaneNode();

        ln.setX(10.0);
        ln.setY(20.0);
        ln.setWidth(0.0);

        geomList.add(ln);

        ln = new LaneNode();

        ln.setX(30.0);
        ln.setY(40.0);
        ln.setWidth(0.0);

        geomList.add(ln);

        popLaneExpectedCond1.setLaneGeomList(geomList);

        LaneMovement lm = new LaneMovement();

        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);

        Map<Integer, LaneMovement> mvs = popLaneExpectedCond1.getLaneMovements();
        mvs.put(1, lm);
    }

    private void genPopLaneCase2() {
        popLaneMapRetrievCond2 = PowerMockito.mock(MapDataRetriever.class);

        PowerMockito.when(popLaneMapRetrievCond2.getISNA(5)).thenReturn(5);
        PowerMockito.when(popLaneMapRetrievCond2.getISNA(6)).thenReturn(6);
        PowerMockito.when(popLaneMapRetrievCond2.getLIBAR(5)).thenReturn(1);
        PowerMockito.when(popLaneMapRetrievCond2.getLIBAR(6)).thenReturn(-1);
        PowerMockito.when(popLaneMapRetrievCond2.getISLIM()).thenReturn(new int[] { 1, 1, 1, 0 });
        PowerMockito.when(popLaneMapRetrievCond2.getLGEOM()).thenReturn(defLaneGeom);
        PowerMockito.when(popLaneMapRetrievCond2.getLTURN(5)).thenReturn(1);
        PowerMockito.when(popLaneMapRetrievCond2.getLTURN(6)).thenReturn(1);
        PowerMockito.when(popLaneMapRetrievCond2.getLWID(5)).thenReturn(0);
        PowerMockito.when(popLaneMapRetrievCond2.getLWID(6)).thenReturn(0);

        popLaneExpectedCond2 = new Lane();

        popLaneExpectedCond2.setApproachId(6);
        popLaneExpectedCond2.setLaneId(6);
        popLaneExpectedCond2.setType(Lane.OUTBOUND);
        popLaneExpectedCond2.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertFeetToMeters(1));

        List<LaneNode> geomList = new ArrayList<LaneNode>(2);

        LaneNode ln = new LaneNode();

        ln.setX(10.0);
        ln.setY(20.0);
        ln.setWidth(0.0);

        geomList.add(ln);

        ln = new LaneNode();

        ln.setX(30.0);
        ln.setY(40.0);
        ln.setWidth(0.0);

        geomList.add(ln);

        popLaneExpectedCond2.setLaneGeomList(geomList);

        LaneMovement lm = new LaneMovement();

        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);

        Map<Integer, LaneMovement> mvs = popLaneExpectedCond2.getLaneMovements();
        mvs.put(1, lm);
    }

    private void genModifyEdgesCase1() {
        modifyEdgesCon1 = new ParsingLaneDetails();

        modifyEdgesCon1.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
        modifyEdgesCon1.laneGeometry = new int[] { 0, 0, 0, 0 };
        modifyEdgesCon1.xMidpoint = 1.0;
        modifyEdgesCon1.yMidpoint = 0.0;

        modifyEdgesCon1Expected = new ParsingLaneDetails();

        modifyEdgesCon1Expected.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
    }

    private void genModifyEdgesCase4() {
        modifyEdgesCon4 = new ParsingLaneDetails();

        modifyEdgesCon4.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
        modifyEdgesCon4.laneGeometry = new int[] { 0, 1, 2, 3 };
        modifyEdgesCon4.xMidpoint = 1.0;
        modifyEdgesCon4.yMidpoint = 0.0;

        modifyEdgesCon4Expected = new ParsingLaneDetails();

        modifyEdgesCon4Expected.edges = new double[] { 0.0, 2.0, 1001.0, 2.0 };
    }

    private void genEdgesToNodes() {
        edgesToNodesExpected = new ArrayList<LaneNode>(2);

        LaneNode ln = new LaneNode();

        ln.setX(edgesToNodesEdges[0]);
        ln.setY(edgesToNodesEdges[1]);
        ln.setWidth(edgesToNodesWidth);

        edgesToNodesExpected.add(ln);

        ln = new LaneNode();

        ln.setX(edgesToNodesEdges[2]);
        ln.setY(edgesToNodesEdges[3]);
        ln.setWidth(edgesToNodesWidth);

        edgesToNodesExpected.add(ln);
    }

    private void genMetaDataInput() {
        metaDataInput = PowerMockito.mock(ModelData.class);

        PowerMockito.when(metaDataInput.getMaxDT()).thenReturn((long)9000);
        PowerMockito.when(metaDataInput.getDTSize()).thenReturn(100.5);
        PowerMockito.when(metaDataInput.getYMax()).thenReturn(700.2);
        PowerMockito.when(metaDataInput.getYMin()).thenReturn(-550.6);
        PowerMockito.when(metaDataInput.getXMax()).thenReturn(7000.2);
        PowerMockito.when(metaDataInput.getXMin()).thenReturn(100.1);
    }

    private void genMetaDataExpected() {
        metaDataExpected = new SimMetaData();

        metaDataExpected.setMaxSteps(9000);
        metaDataExpected.setStepSize(100.5);
        metaDataExpected.setSimHeight(1250.8);
        metaDataExpected.setSimWidth(6900.1);
        metaDataExpected.setFirstStep(1);
    }

    private void genMoveCodeCase1() {
        moveCodeExpected1 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);

        moveCodeExpected1.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase2() {
        moveCodeExpected2 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.STRAIGHT);
        lm.setMovementId(2);

        moveCodeExpected2.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase3() {
        moveCodeExpected3 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.LEFT_TURN);
        lm.setMovementId(4);

        moveCodeExpected3.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase4() {
        moveCodeExpected4 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.U_TURN);
        lm.setMovementId(8);

        moveCodeExpected4.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase5() {
        moveCodeExpected5 = new HashMap<Integer, LaneMovement>();
    }

    private void genMoveCodeCase6() {
        moveCodeExpected6 = new HashMap<Integer, LaneMovement>();
    }

    private void genMoveCodeCase7() {
        moveCodeExpected7 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);

        moveCodeExpected7.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.STRAIGHT);
        lm.setMovementId(2);

        moveCodeExpected7.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase8() {
        moveCodeExpected8 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);

        moveCodeExpected8.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.STRAIGHT);
        lm.setMovementId(2);

        moveCodeExpected8.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.LEFT_TURN);
        lm.setMovementId(4);

        moveCodeExpected8.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase9() {
        moveCodeExpected9 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.RIGHT_TURN);
        lm.setMovementId(1);

        moveCodeExpected9.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.STRAIGHT);
        lm.setMovementId(2);

        moveCodeExpected9.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.LEFT_TURN);
        lm.setMovementId(4);

        moveCodeExpected9.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.U_TURN);
        lm.setMovementId(8);

        moveCodeExpected9.put(lm.getMovementId(), lm);
    }

    /*
     * int moveCodeCond12 = 12;
     */

    private void genMoveCodeCase10() {
        moveCodeExpected10 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.STRAIGHT);
        lm.setMovementId(2);

        moveCodeExpected10.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.LEFT_TURN);
        lm.setMovementId(4);

        moveCodeExpected10.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase11() {
        moveCodeExpected11 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.STRAIGHT);
        lm.setMovementId(2);

        moveCodeExpected11.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.LEFT_TURN);
        lm.setMovementId(4);

        moveCodeExpected11.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.U_TURN);
        lm.setMovementId(8);

        moveCodeExpected11.put(lm.getMovementId(), lm);
    }

    private void genMoveCodeCase12() {
        moveCodeExpected12 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        lm.setMovement(Movement.LEFT_TURN);
        lm.setMovementId(4);

        moveCodeExpected12.put(lm.getMovementId(), lm);

        lm = new LaneMovement();
        lm.setMovement(Movement.U_TURN);
        lm.setMovementId(8);

        moveCodeExpected12.put(lm.getMovementId(), lm);
    }

    // Generator Methods
    private void genTurnLaneCase1() {
        turningLaneCond1 = new ParsingLaneDetails();

        turningLaneCond1.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
        turningLaneCond1.laneGeometry = new int[] { 0, 0, 0, 2 };
        turningLaneCond1.xMidpoint = 1.0;
        turningLaneCond1.yMidpoint = 0.0;

        turningLaneCond1Expected = new ParsingLaneDetails();

        turningLaneCond1Expected.edges = new double[] { 0.0, 2.0, 177.0, 2.0 };
    }

    private void genTurnLaneCase2() {
        turningLaneCond2 = new ParsingLaneDetails();

        turningLaneCond2.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
        turningLaneCond2.laneGeometry = new int[] { 0, 0, 0, 2 };
        turningLaneCond2.xMidpoint = -1.0;
        turningLaneCond2.yMidpoint = 0.0;

        turningLaneCond2Expected = new ParsingLaneDetails();

        turningLaneCond2Expected.edges = new double[] { 0.0, 2.0, -177.0, 2.0 };
    }

    private void genTurnLaneCase3() {
        turningLaneCond3 = new ParsingLaneDetails();

        turningLaneCond3.edges = new double[] { 0.0, 2.0, 0.0, 4.0 };
        turningLaneCond3.laneGeometry = new int[] { 0, 0, 0, 2 };
        turningLaneCond3.xMidpoint = 0.0;
        turningLaneCond3.yMidpoint = 0.0;

        turningLaneCond3Expected = new ParsingLaneDetails();

        turningLaneCond3Expected.edges = new double[] { 0.0, 181.0, 0.0, 4.0 };
    }

    private void genTurnLaneCase4() {
        turningLaneCond4 = new ParsingLaneDetails();

        turningLaneCond4.edges = new double[] { 0.0, 2.0, 0.0, 4.0 };
        turningLaneCond4.laneGeometry = new int[] { 0, 0, 0, 2 };
        turningLaneCond4.xMidpoint = 0.0;
        turningLaneCond4.yMidpoint = 3.0;

        turningLaneCond4Expected = new ParsingLaneDetails();

        turningLaneCond4Expected.edges = new double[] { 0.0, -173.0, 0.0, 4.0 };
    }

    private void genTurnLaneCase5() {
        turningLaneCond5 = new ParsingLaneDetails();

        turningLaneCond5.edges = new double[] { 0.0, 2.0, 1.0, 4.0 };
        turningLaneCond5.laneGeometry = new int[] { 0, 0, 0, 2 };
        turningLaneCond5.xMidpoint = 0.0;
        turningLaneCond5.yMidpoint = 3.0;

        turningLaneCond5Expected = new ParsingLaneDetails();

        turningLaneCond5Expected.edges = new double[] { 0.0, 2.0, 1.0, 4.0 };
    }

    private void genMergeLaneCase1() {
        mergeLaneCond1 = new ParsingLaneDetails();

        mergeLaneCond1.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
        mergeLaneCond1.laneGeometry = new int[] { 0, 2, 0, 0 };
        mergeLaneCond1.xMidpoint = 1.0;
        mergeLaneCond1.yMidpoint = 0.0;

        mergeLaneCond1Expected = new ParsingLaneDetails();

        mergeLaneCond1Expected.edges = new double[] { 0.0, 2.0, 1002.0, 2.0 };
    }

    private void genMergeLaneCase2() {
        mergeLaneCond2 = new ParsingLaneDetails();

        mergeLaneCond2.edges = new double[] { 0.0, 2.0, 0.0, 2.0 };
        mergeLaneCond2.laneGeometry = new int[] { 0, 2 };
        mergeLaneCond2.xMidpoint = -1.0;
        mergeLaneCond2.yMidpoint = 0.0;

        mergeLaneCond2Expected = new ParsingLaneDetails();

        mergeLaneCond2Expected.edges = new double[] { 0.0, 2.0, -1002.0, 2.0 };
    }

    private void genMergeLaneCase3() {
        mergeLaneCond3 = new ParsingLaneDetails();

        mergeLaneCond3.edges = new double[] { 0.0, 2.0, 0.0, 4.0 };
        mergeLaneCond3.laneGeometry = new int[] { 0, 2 };
        mergeLaneCond3.xMidpoint = 0.0;
        mergeLaneCond3.yMidpoint = 0.0;

        mergeLaneCond3Expected = new ParsingLaneDetails();

        mergeLaneCond3Expected.edges = new double[] { 0.0, 1006.0, 0.0, 4.0 };
    }

    private void genMergeLaneCase4() {
        mergeLaneCond4 = new ParsingLaneDetails();

        mergeLaneCond4.edges = new double[] { 0.0, 2.0, 0.0, 4.0 };
        mergeLaneCond4.laneGeometry = new int[] { 0, 2 };
        mergeLaneCond4.xMidpoint = 0.0;
        mergeLaneCond4.yMidpoint = 3.0;

        mergeLaneCond4Expected = new ParsingLaneDetails();

        mergeLaneCond4Expected.edges = new double[] { 0.0, -998, 0.0, 4.0 };
    }

    private void genMergeLaneCase5() {
        mergeLaneCond5 = new ParsingLaneDetails();

        mergeLaneCond5.edges = new double[] { 0.0, 2.0, 1.0, 4.0 };
        mergeLaneCond5.laneGeometry = new int[] { 0, 2 };
        mergeLaneCond5.xMidpoint = 0.0;
        mergeLaneCond5.yMidpoint = 3.0;

        mergeLaneCond5Expected = new ParsingLaneDetails();

        mergeLaneCond5Expected.edges = new double[] { 0.0, 2.0, 1.0, 4.0 };
    }

    /**
     * Struct to keep code clean.
     * 
     * @author ablatt
     */
    protected class ParsingLaneDetails {

        double[] edges;

        int[] laneGeometry;

        int geomIndex = 0;

        double xMidpoint;

        double yMidpoint;
    }
}
