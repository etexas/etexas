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
package org.etexascode.apps;

import org.junit.Ignore;

/**
 * @author ablatt
 */
@Ignore
public class FuelConsumptionCalcTest {

    public class EasyTestFuelCalc implements IFuelCalculator {

        @Override
        public double calculateFuel() {
            return 1;
        }
    }
}
/*
 * private static DetectorManagerInfo[] genDetMansSingleLane() { Detector on =
 * GenDetectorFunctions.genDetector(0, 0, 0, 0); Detector off = GenDetectorFunctions.genDetector(0,
 * 0, 0, 0); on.setDetEvent(GenDetectorFunctions.genDetEventTrue(0, 20)); DetectorManagerInfo[] ret
 * = new DetectorManagerInfo[13]; DetectorManager detMan = new DetectorManager();
 * detMan.addDetector(0, off); ret[0] = new DetectorManagerInfo(detMan); detMan = new
 * DetectorManager(); detMan.addDetector(0, on); ret[1] = new DetectorManagerInfo(detMan); detMan =
 * new DetectorManager(); detMan.addDetector(0, off); ret[2] = new DetectorManagerInfo(detMan);
 * detMan = new DetectorManager(); detMan.addDetector(0, on); ret[3] = new
 * DetectorManagerInfo(detMan); detMan = new DetectorManager(); detMan.addDetector(0, off); ret[4] =
 * new DetectorManagerInfo(detMan); detMan = new DetectorManager(); detMan.addDetector(0, on);
 * ret[5] = new DetectorManagerInfo(detMan); detMan = new DetectorManager(); detMan.addDetector(0,
 * off); ret[6] = new DetectorManagerInfo(detMan); detMan = new DetectorManager();
 * detMan.addDetector(0, on); ret[7] = new DetectorManagerInfo(detMan); detMan = new
 * DetectorManager(); detMan.addDetector(0, off); ret[8] = new DetectorManagerInfo(detMan); detMan =
 * new DetectorManager(); detMan.addDetector(0, on); ret[9] = new DetectorManagerInfo(detMan);
 * detMan = new DetectorManager(); detMan.addDetector(0, off); ret[10] = new
 * DetectorManagerInfo(detMan); detMan = new DetectorManager(); detMan.addDetector(0, on); ret[11] =
 * new DetectorManagerInfo(detMan); detMan = new DetectorManager(); detMan.addDetector(0, off);
 * ret[12] = new DetectorManagerInfo(detMan); return ret; } private static LaneManager
 * genLanManSingleLane() { LaneManager lm = new LaneManager(); Lane l = GenLaneFunctions.genLane(0,
 * 1); l.setLaneId(0); Map<Integer, Lane> lanes = lm.getLanes(); lanes.put(0, l);
 * lm.setLanes(lanes); return lm; } private static VehicleManagerInfo[]
 * genVehMansSingleLane(LaneManager lm) { VehicleManagerInfo[] ret = new VehicleManagerInfo[13];
 * VehicleManagerInfo empty = new VehicleManagerInfo(new VehicleManager(), lm, 1); VehicleManager
 * vm1On = new VehicleManager(); VehicleManager vm1Off = new VehicleManager(); VehicleManager vm2On
 * = new VehicleManager(); VehicleManager vm2Off = new VehicleManager(); Vehicle v1OnDet =
 * GenVehicleFunctions.genVehicle(20, 20, 20, 20); v1OnDet.setVehicleID(0);
 * vm1On.addVehicle(v1OnDet); Vehicle v1OffDet = GenVehicleFunctions.genVehicle(5000, 5000, 20, 20);
 * v1OffDet.setVehicleID(0); vm1Off.addVehicle(v1OffDet); Vehicle v2OnDet =
 * GenVehicleFunctions.genVehicle(20, 20, 20, 20); v2OnDet.setVehicleID(1);
 * vm2On.addVehicle(v2OnDet); Vehicle v2OffDet = GenVehicleFunctions.genVehicle(5000, 5000, 20, 20);
 * v2OffDet.setVehicleID(1); vm2Off.addVehicle(v2OffDet); // set ret no vehs in intersection ret[0]
 * = empty; ret[1] = empty; ret[6] = empty; ret[7] = empty; ret[11] = empty; ret[12] = empty; // set
 * ret veh 1 VehicleManagerInfo vmi1On = new VehicleManagerInfo(vm1On, lm, 1); VehicleManagerInfo
 * vmi1Off = new VehicleManagerInfo(vm1Off, lm, 1); ret[2] = vmi1Off; ret[3] = vmi1On; ret[4] =
 * vmi1Off; ret[5] = vmi1Off; // set ret veh 2 VehicleManagerInfo vmi2On = new
 * VehicleManagerInfo(vm2On, lm, 1); VehicleManagerInfo vmi2Off = new VehicleManagerInfo(vm2Off, lm,
 * 1); ret[8] = vmi2Off; ret[9] = vmi2On; ret[10] = vmi2Off; return ret; }
 * @Test public void testSetFuelCalc() { DetectorManagerInfo detManInf = genDetMansSingleLane()[0];
 * LaneManagerInfo lanManInf = new LaneManagerInfo(genLanManSingleLane()); FuelUsageMOECalc calc =
 * new FuelUsageMOECalc(lanManInf, detManInf); EasyTestFuelCalc etfc = new EasyTestFuelCalc();
 * assertTrue(!(calc.fuelCalculator == etfc)); calc.setFuelCalculator(etfc);
 * assertTrue(calc.fuelCalculator == etfc); }
 * @Ignore
 * @Test public void testUpdate() { // This test update will test the update method on a single lane
 * DetectorManagerInfo[] dets = genDetMansSingleLane(); LaneManager lm = genLanManSingleLane();
 * VehicleManagerInfo[] vehs = genVehMansSingleLane(lm); LaneManagerInfo lmi = new
 * LaneManagerInfo(lm); FuelUsageMOECalc calc = new FuelUsageMOECalc(lmi, dets[0]); EasyTestFuelCalc
 * etfc = new EasyTestFuelCalc(); calc.setFuelCalculator(etfc); calc.update(new
 * LinkedList<VehicleInfo>(), new LinkedList<DetectorInfo>(), vehs[0], 0); Collection<DetectorInfo>
 * updatedDets = new LinkedList<DetectorInfo>(); updatedDets.add(dets[1].getDetectorInfo(0));
 * calc.update(new LinkedList<VehicleInfo>(), updatedDets, vehs[1], 1); LinkedList<VehicleInfo>
 * updatedVehs = new LinkedList<VehicleInfo>(); updatedVehs.add(vehs[2].getVehicleInfoById(0));
 * updatedDets = new LinkedList<DetectorInfo>(); updatedDets.add(dets[2].getDetectorInfo(0));
 * calc.update(updatedVehs, updatedDets, vehs[2], 1); updatedVehs = new LinkedList<VehicleInfo>();
 * updatedVehs.add(vehs[3].getVehicleInfoById(0)); updatedDets = new LinkedList<DetectorInfo>();
 * updatedDets.add(dets[3].getDetectorInfo(0)); calc.update(updatedVehs, updatedDets, vehs[3], 1);
 * updatedVehs = new LinkedList<VehicleInfo>(); updatedVehs.add(vehs[4].getVehicleInfoById(0));
 * updatedDets = new LinkedList<DetectorInfo>(); updatedDets.add(dets[4].getDetectorInfo(0));
 * calc.update(updatedVehs, updatedDets, vehs[4], 1); updatedVehs = new LinkedList<VehicleInfo>();
 * updatedVehs.add(vehs[5].getVehicleInfoById(0)); updatedDets = new LinkedList<DetectorInfo>();
 * updatedDets.add(dets[5].getDetectorInfo(0)); calc.update(updatedVehs, updatedDets, vehs[5], 1);
 * updatedDets = new LinkedList<DetectorInfo>(); updatedDets.add(dets[6].getDetectorInfo(0));
 * calc.update(new LinkedList<VehicleInfo>(), updatedDets, vehs[6], 1); updatedDets = new
 * LinkedList<DetectorInfo>(); updatedDets.add(dets[7].getDetectorInfo(0)); calc.update(new
 * LinkedList<VehicleInfo>(), updatedDets, vehs[7], 1); updatedVehs = new LinkedList<VehicleInfo>();
 * updatedVehs.add(vehs[8].getVehicleInfoById(1)); updatedDets = new LinkedList<DetectorInfo>();
 * updatedDets.add(dets[8].getDetectorInfo(0)); calc.update(updatedVehs, updatedDets, vehs[8], 1);
 * updatedVehs = new LinkedList<VehicleInfo>(); updatedVehs.add(vehs[9].getVehicleInfoById(1));
 * updatedDets = new LinkedList<DetectorInfo>(); updatedDets.add(dets[9].getDetectorInfo(0));
 * calc.update(updatedVehs, updatedDets, vehs[9], 1); updatedVehs = new LinkedList<VehicleInfo>();
 * updatedVehs.add(vehs[10].getVehicleInfoById(1)); updatedDets = new LinkedList<DetectorInfo>();
 * updatedDets.add(dets[10].getDetectorInfo(0)); calc.update(updatedVehs, updatedDets, vehs[10], 1);
 * updatedDets = new LinkedList<DetectorInfo>(); updatedDets.add(dets[11].getDetectorInfo(0));
 * calc.update(new LinkedList<VehicleInfo>(), updatedDets, vehs[11], 1); updatedDets = new
 * LinkedList<DetectorInfo>(); updatedDets.add(dets[12].getDetectorInfo(0)); calc.update(new
 * LinkedList<VehicleInfo>(), updatedDets, vehs[12], 1); TestAppLoggerReadOutput logger = new
 * TestAppLoggerReadOutput(); calc.onDestroy(logger); String s = logger.logs.get(0)[1];
 * assertEquals("16", s.substring(21, 23)); }
 * @Test public void testUpdate2() { // This test update will test the update method using 2 lanes }
 * }
 */