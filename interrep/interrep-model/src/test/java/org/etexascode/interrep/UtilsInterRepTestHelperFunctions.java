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
package org.etexascode.interrep;

public class UtilsInterRepTestHelperFunctions {

    /*
     * public static Vehicle genVehicleInLane(Lane l) { Random r = new Random(); return
     * genVehicleFromNode(l.getLaneGeomList().get(r.nextInt(l.getLaneGeomList ().size() - 2) + 1),
     * r); } public static Vehicle genVehicleFromNode(LaneNode ln, Random r) { return
     * GenVehicleFunctions.genVehicle(genVehicleX(ln, r), genVehicleY(ln, r), 100, 100); } public
     * static double genVehicleX(LaneNode ln, Random r) { return ln.getX() - genVehVal(ln.getWidth()
     * - 100, 0.5 - r.nextDouble()); } public static double genVehicleY(LaneNode ln, Random r) {
     * return ln.getY() - genVehVal(5.0, 0.5 - r.nextDouble()); } public static double
     * genVehVal(double span, double rand) { return span * rand; } public static DetectorManager[]
     * genDetEventTestAnses() { DetectorManager[] ret = new DetectorManager[10]; for(int i = 0; i <
     * ret.length; i++) { ret[i] = GenDetectorFunctions.genDetManager(); }
     * ret[0].getDetector(2).setDetEvent(genDetEventTrue(2, 100));
     * ret[1].getDetector(2).setDetEvent(genDetEventTrue(2, 400));
     * ret[1].getDetector(1).setDetEvent(genDetEventTrue(1, 400)); // nothing on ret[2]
     * ret[3].getDetector(5).setDetEvent(genDetEventTrue(5, 100));
     * ret[4].getDetector(2).setDetEvent(genDetEventTrue(2, 100));
     * ret[5].getDetector(2).setDetEvent(genDetEventTrue(2, 400));
     * ret[5].getDetector(1).setDetEvent(genDetEventTrue(1, 400)); // nothing on ret[6]
     * ret[7].getDetector(5).setDetEvent(genDetEventTrue(5, 100));
     * ret[8].getDetector(2).setDetEvent(genDetEventTrue(2, 1000));
     * ret[9].getDetector(1).setDetEvent(genDetEventTrue(1, 10)); return ret; } public static
     * DetectorEvent genDetEventTrue(int detectorId, double length) { DetectorEvent ret = new
     * DetectorEvent(); ret.setDetectorId(detectorId); ret.setPresence(true); ret.setPulse(1);
     * ret.setLength(length); return ret; } public static Vehicle[][] genDetectorCases() {
     * Vehicle[][] ret = new Vehicle[10][2]; // case 1: vehicle over 1 detector // over detector(s):
     * 2 ret[0][0] = GenVehicleFunctions.genVehicle(0, 1875, 100, 100, 1, 1); ret[0][1] =
     * GenVehicleFunctions.genVehicle(0, 1900, 100, 100, 1, 2); // case 2: vehicle over 2 detectors
     * // over detector(s): 1, 2 ret[1][0] = GenVehicleFunctions.genVehicle(0, 1820, 400, 100, 1,
     * 3); ret[1][1] = GenVehicleFunctions.genVehicle(0, 1830, 400, 100, 1, 4); // case 3: vehicle
     * over no detectors // over detector(s): ret[2][0] = GenVehicleFunctions.genVehicle(800000000,
     * 800000000, 100, 100, 1, 5); ret[2][1] = GenVehicleFunctions.genVehicle(800000000, 800000010,
     * 100, 100, 1, 6); // case 4: vehicle over 1 detector (diagonal) // over detector(s): 3
     * ret[3][0] = GenVehicleFunctions.genVehicle(1750, 1750, 100, 100, 5, 7); ret[3][1] =
     * GenVehicleFunctions.genVehicle(1775, 1775, 100, 100, 5, 8); // again with nulls // case 5:
     * vehicle over 1 detector // over detector(s): 2 ret[4][0] = GenVehicleFunctions.genVehicle(0,
     * 1850, 100, 100, 1, 9); ret[4][1] = null; // case 6: vehicle over 2 detectors // over
     * detector(s): 1, 2 ret[5][0] = GenVehicleFunctions.genVehicle(0, 1820, 400, 100, 1, 10);
     * ret[5][1] = null; // case 7: vehicle over no detectors // over detector(s): ret[6][0] =
     * GenVehicleFunctions.genVehicle(800000000, 800000000, 100, 100, 1, 11); ret[6][1] = null; //
     * case 8: vehicle over 1 detector (diagonal) // over detector(s): 3 ret[7][0] =
     * GenVehicleFunctions.genVehicle(1750, 1750, 100, 100, 5, 12); ret[7][1] = null; // other cases
     * // case 9: vehicle completely covers the detector // over detector(s): 2 ret[8][0] =
     * GenVehicleFunctions.genVehicle(0, 1860, 1000, 1000, 1, 13); ret[8][1] =
     * GenVehicleFunctions.genVehicle(0, 1870, 1000, 1000, 1, 14); // case 10: vehicle completely
     * inside detector // over detector(s): 2 ret[9][0] = GenVehicleFunctions.genVehicle(0, 1810,
     * 10, 10, 1, 15); ret[9][1] = GenVehicleFunctions.genVehicle(0, 1820, 10, 10, 1, 16); return
     * ret; } public static Vehicle[][] buildTestGenHeadingCases() { Vehicle[][] ret = new
     * Vehicle[4][]; // case 1: standard ret[0] = new Vehicle[]{
     * GenVehicleFunctions.genVehicle(40.0, 40.0, 400, 700), GenVehicleFunctions.genVehicle(80.0,
     * 10.0, 800, 100)}; // case 2: car just appeared - vertical ret[1] = new Vehicle[]{
     * GenVehicleFunctions.genVehicle(100.0, 100.0, 6, 8, 1, 7), null}; // case 3: car just appeared
     * - horizontal ret[2] = new Vehicle[]{ GenVehicleFunctions.genVehicle(50, 897, 65465, 6541641,
     * 2, 564), null}; // case 4: car just appeared - diagonal ret[3] = new Vehicle[]{
     * GenVehicleFunctions.genVehicle(46541, 1654, 5646, 3, 5, 156), null}; return ret; } public
     * static double[][] buildTestGenHeadingAnses() { double[][] ret = new double[4][]; ret[0] = new
     * double[]{30.0, -40.0}; ret[1] = new double[]{-20, 0}; // ret[2] = new double[]{0, -20};
     * ret[3] = new double[]{-20, -20}; return ret; } public static boolean testDetCase(Vehicle[]
     * vehicles, DetectorManager toAlter, DetectorManager expected, LaneManager lm) {
     * UtilsInterRep.genDetEvent(vehicles[0], vehicles[1], toAlter, lm); return
     * toAlter.equals(expected); } public static LaneNode[][] genLaneNodePolygonCases() {
     * LaneNode[][] ret = new LaneNode[9][]; // vertical ret[0] = new LaneNode[]{
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 50.0), GenLaneFunctions.genLaneNode(0.0, 50.0, 50.0)};
     * // reverse vertical ret[1] = new LaneNode[]{ GenLaneFunctions.genLaneNode(0.0, 50.0, 50.0),
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 50.0)}; // horizontal ret[2] = new LaneNode[]{
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 50.0), GenLaneFunctions.genLaneNode(50.0, 0.0, 50.0)};
     * // reverse horizontal ret[3] = new LaneNode[]{ GenLaneFunctions.genLaneNode(50.0, 0.0, 50.0),
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 50.0)}; // diagonal ret[4] = new LaneNode[]{
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 2 * UtilsInterRep.calcDist(0.0, 0.0, -25.0, -25.0)),
     * GenLaneFunctions.genLaneNode(50.0, 50.0, 2 * UtilsInterRep.calcDist(0.0, 0.0, -25.0,
     * -25.0))}; ret[5] = new LaneNode[]{ GenLaneFunctions.genLaneNode(50.0, 50.0, 2 *
     * UtilsInterRep.calcDist(0.0, 0.0, -25.0, -25.0)), GenLaneFunctions.genLaneNode(0.0, 0.0, 2 *
     * UtilsInterRep.calcDist(0.0, 0.0, -25.0, -25.0))}; ret[6] = new LaneNode[]{
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 2 * UtilsInterRep.calcDist(0.0, 0.0, -25.0, -25.0)),
     * GenLaneFunctions.genLaneNode(-50.0, -50.0, 2 * UtilsInterRep.calcDist(0.0, 0.0, -25.0,
     * -25.0))}; ret[7] = new LaneNode[]{ GenLaneFunctions.genLaneNode(-50.0, -50.0, 2 *
     * UtilsInterRep.calcDist(0.0, 0.0, -25.0, -25.0)), GenLaneFunctions.genLaneNode(0.0, 0.0, 2 *
     * UtilsInterRep.calcDist(0.0, 0.0, -25.0, -25.0))}; // irregular shape ret[8] = new LaneNode[]{
     * GenLaneFunctions.genLaneNode(0.0, 0.0, 50.0), GenLaneFunctions.genLaneNode(50.0, 0.0, 30.0)};
     * return ret; } public static Polygon[] genLaneNodePolygonAnses() { Polygon[] ret = new
     * Polygon[9]; ret[0] = genPolygon(new int[][]{ new int[]{-25, 0}, new int[]{25, 0}, new
     * int[]{25, 50}, new int[]{-25, 50}}); ret[1] = genPolygon(new int[][]{ new int[]{-25, 0}, new
     * int[]{25, 0}, new int[]{25, 50}, new int[]{-25, 50}}); ret[2] = genPolygon(new int[][]{ new
     * int[]{0, -25}, new int[]{0, 25}, new int[]{50, 25}, new int[]{50, -25}}); ret[3] =
     * genPolygon(new int[][]{ new int[]{0, -25}, new int[]{0, 25}, new int[]{50, 25}, new int[]{50,
     * -25}}); ret[4] = genPolygon(new int[][]{ new int[]{-25, 25}, new int[]{25, 75}, new int[]{75,
     * 25}, new int[]{25, -25}}); ret[5] = genPolygon(new int[][]{ new int[]{-25, 25}, new int[]{25,
     * 75}, new int[]{75, 25}, new int[]{25, -25}}); ret[6] = genPolygon(new int[][]{ new int[]{-25,
     * 25}, new int[]{-75, -25}, new int[]{-25, -75}, new int[]{25, -25}}); ret[7] = genPolygon(new
     * int[][]{ new int[]{-25, 25}, new int[]{-75, -25}, new int[]{-25, -75}, new int[]{25, -25}});
     * ret[8] = genPolygon(new int[][]{ new int[]{0, 25}, new int[]{50, 15}, new int[]{50, -15}, new
     * int[]{0, -25}}); return ret; } public static Polygon genPolygon(int[][] points) { Polygon ret
     * = new Polygon(); for(int i = 0; i < points.length; i++) { ret.addPoint(points[i][0],
     * points[i][1]); } return ret; }
     */
}
