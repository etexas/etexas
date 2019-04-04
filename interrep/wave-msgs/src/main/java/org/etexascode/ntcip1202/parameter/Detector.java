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

package org.etexascode.ntcip1202.parameter;

import java.io.Serializable;
import java.util.Date;
import org.etexascode.nonstd.Message;

/**
 * Loop Detector data structure
 * 
 * @author dranker
 */
public class Detector extends Message implements Serializable {

    /**
     * The Maximum Number of Vehicle Detectors this Actuated Controller Unit supports. This object
     * indicates the maximum rows which shall appear in the vehicleDetectorTable object.
     */
    private int maxVehicleDetectors;

    /**
     * The maximum number of detector status groups (8 detectors per group) this device supports.
     * This value is equal to TRUNCATE [(maxVehicleDetectors + 7 ) / 8]. This object indicates the
     * maximum number of rows which shall appear in the vehicleDetectorStatusGroupTable object.
     */
    private int maxVehicleDetectorStatusGroups;

    // NOT MAPPED
    /**
     * The number of detectors in this device. This object indicates how many rows are in the
     * volumeOccupancyTable object. There shall be a row for every detector that is collecting
     * volume or occupancy data (refer to detectorOptions in the detectorTable).
     */
    private int activeVolumeOccupancyDetectors;

    /**
     * The Maximum Number of Pedestrian Detectors this Actuated Controller Unit supports. This
     * object indicates the maximum rows which shall appear in the pedestrianDetectorTable object.
     */
    private int maxPedestrianDetectors;

    private VehicleDetectorEntry[] vehicleDetectors;

    private VehicleDetectorStatusGroup[] vehicleStatusGroups;

    private VolumeOccupancyTable[] volumeOccupancy;

    private PedestrianDetectorTable[] pedestrianDetectors;

    private long messageTime = new Date().getTime();

    public Detector() {
        vehicleDetectors = new VehicleDetectorEntry[maxVehicleDetectors];
        vehicleStatusGroups = new VehicleDetectorStatusGroup[maxVehicleDetectorStatusGroups];
        volumeOccupancy = new VolumeOccupancyTable[activeVolumeOccupancyDetectors];
        pedestrianDetectors = new PedestrianDetectorTable[maxPedestrianDetectors];
    }

    public Detector(int pedSize, int vehSize) {
        vehicleDetectors = new VehicleDetectorEntry[vehSize];
        initVehicleDetectors(vehSize);
        vehicleStatusGroups = new VehicleDetectorStatusGroup[vehSize];
        initVehicleStatusGroups(vehSize);
        volumeOccupancy = new VolumeOccupancyTable[activeVolumeOccupancyDetectors];
        pedestrianDetectors = new PedestrianDetectorTable[pedSize];
        initPedestrianDetectors(pedSize);
    }

    private void initVehicleDetectors(int size) {
        for (int i = 0; i < size; i++) {
            vehicleDetectors[i] = new VehicleDetectorEntry();
        }
    }

    private void initVehicleStatusGroups(int size) {
        for (int i = 0; i < size; i++) {
            vehicleStatusGroups[i] = new VehicleDetectorStatusGroup();
        }
    }

    private void initPedestrianDetectors(int size) {
        for (int i = 0; i < size; i++) {
            pedestrianDetectors[i] = new PedestrianDetectorTable();
        }
    }

    /**
     * A table containing Actuated Controller Unit vehicle detector parameters. The number of rows
     * in this table is equal to the maxVehicleDetectors object.
     */
    public class VehicleDetectorEntry {

        public int getVehicleDetectorAlarms() {
            return vehicleDetectorAlarms;
        }

        public void setVehicleDetectorAlarms(int vehicleDetectorAlarms) {
            this.vehicleDetectorAlarms = vehicleDetectorAlarms;
        }

        public int getVehicleDetectorCallPhase() {
            return vehicleDetectorCallPhase;
        }

        public void setVehicleDetectorCallPhase(int vehicleDetectorCallPhase) {
            this.vehicleDetectorCallPhase = vehicleDetectorCallPhase;
        }

        public int getVehicleDetectorDelay() {
            return vehicleDetectorDelay;
        }

        public void setVehicleDetectorDelay(int vehicleDetectorDelay) {
            this.vehicleDetectorDelay = vehicleDetectorDelay;
        }

        public int getVehicleDetectorErraticCounts() {
            return vehicleDetectorErraticCounts;
        }

        public void setVehicleDetectorErraticCounts(int vehicleDetectorErraticCounts) {
            this.vehicleDetectorErraticCounts = vehicleDetectorErraticCounts;
        }

        public int getVehicleDetectorExtend() {
            return vehicleDetectorExtend;
        }

        public void setVehicleDetectorExtend(int vehicleDetectorExtend) {
            this.vehicleDetectorExtend = vehicleDetectorExtend;
        }

        public int getVehicleDetectorFailTime() {
            return vehicleDetectorFailTime;
        }

        public void setVehicleDetectorFailTime(int vehicleDetectorFailTime) {
            this.vehicleDetectorFailTime = vehicleDetectorFailTime;
        }

        public int getVehicleDetectorMaxPresence() {
            return vehicleDetectorMaxPresence;
        }

        public void setVehicleDetectorMaxPresence(int vehicleDetectorMaxPresence) {
            this.vehicleDetectorMaxPresence = vehicleDetectorMaxPresence;
        }

        public int getVehicleDetectorNoActivity() {
            return vehicleDetectorNoActivity;
        }

        public void setVehicleDetectorNoActivity(int vehicleDetectorNoActivity) {
            this.vehicleDetectorNoActivity = vehicleDetectorNoActivity;
        }

        public int getVehicleDetectorNumber() {
            return vehicleDetectorNumber;
        }

        public void setVehicleDetectorNumber(int vehicleDetectorNumber) {
            this.vehicleDetectorNumber = vehicleDetectorNumber;
        }

        public int getVehicleDetectorOptions() {
            return vehicleDetectorOptions;
        }

        public void setVehicleDetectorOptions(int vehicleDetectorOptions) {
            this.vehicleDetectorOptions = vehicleDetectorOptions;
        }

        public int getVehicleDetectorQueueLimit() {
            return vehicleDetectorQueueLimit;
        }

        public void setVehicleDetectorQueueLimit(int vehicleDetectorQueueLimit) {
            this.vehicleDetectorQueueLimit = vehicleDetectorQueueLimit;
        }

        public int getVehicleDetectorReportedAlarms() {
            return vehicleDetectorReportedAlarms;
        }

        public void setVehicleDetectorReportedAlarms(int vehicleDetectorReportedAlarms) {
            this.vehicleDetectorReportedAlarms = vehicleDetectorReportedAlarms;
        }

        public int getVehicleDetectorReset() {
            return vehicleDetectorReset;
        }

        public void setVehicleDetectorReset(int vehicleDetectorReset) {
            this.vehicleDetectorReset = vehicleDetectorReset;
        }

        public int getVehicleDetectorSwitchPhase() {
            return vehicleDetectorSwitchPhase;
        }

        public void setVehicleDetectorSwitchPhase(int vehicleDetectorSwitchPhase) {
            this.vehicleDetectorSwitchPhase = vehicleDetectorSwitchPhase;
        }

        /**
         * The vehicle detector number for objects in this row. The value shall not exceed the
         * maxVehicleDetectors object value.
         */
        private int vehicleDetectorNumber;

        /**
         * Vehicle Detector Options Parameter as follows (0=Disabled, 1=Enabled): Bit 7: Call - if
         * Enabled, the CU shall place a demand for vehicular service on the assigned phase when the
         * phase is not timing the green interval and an actuation is present. Bit 6: Queue - if
         * Enabled, the CU shall extend the green interval of the assigned phase until a gap occurs
         * (no actuation) or until the green has been active longer than the
         * vehicleDetectorQueueLimit time. This is optional. Bit 5: AddedInitial - if Enabled, the
         * CU shall accumulate detector actuation counts for use in the added initial calculations.
         * Counts shall be accumulated from the beginning of the yellow interval to the beginning of
         * the green interval. Bit 4: Passage - if Enabled, the CU shall maintain a reset to the
         * associated phase passage timer for the duration of the detector actuation when the phase
         * is green. Bit 3: Red Lock Call - if Enabled, the detector will lock a call to the
         * assigned phase if an actuation occurs while the phase is not timing Green or Yellow. This
         * mode is optional. Bit 2: Yellow Lock Call - if Enabled, the detector will lock a call to
         * the assigned phase if an actuation occurs while the phase is not timing Green. Bit 1:
         * Occupancy Detector - if Enabled, the detector collects data for the associated detector
         * occupancy object(s). This capability may not be supported on all detector inputs to a
         * device. Bit 0: Volume Detector - if Enabled, the detector collects data for the
         * associated detector volume object(s). This capability may not be supported on all
         * detector inputs to a device. A SET of both bits 2 & 3 = 1 shall result in bit 2=1 and bit
         * 3=0.
         */
        private int vehicleDetectorOptions;

        /**
         * This object contains assigned phase number for the detector input associated with this
         * row. The associated detector call capability is enabled when this object is set to a
         * non-zero value. The value shall not exceed the value of maxPhases.
         */
        private int vehicleDetectorCallPhase;

        /**
         * Detector Switch Phase Parameter (i.e., Phase Number). The phase to which a vehicle
         * detector actuation shall be switched when the assigned phase is Yellow or Red and the
         * Switch Phase is Green.
         */
        private int vehicleDetectorSwitchPhase;

        /**
         * Detector Delay Parameter in tenth seconds (0–255.0 sec). The period a detector actuation
         * (input recognition) shall be delayed when the phase is not Green.
         */
        private int vehicleDetectorDelay;

        /**
         * Detector Extend Parameter in tenth seconds (0–25.5 sec). The period a vehicle detector
         * actuation (input duration) shall be extended from the point of termination , when the
         * phase is Green.
         */
        private int vehicleDetectorExtend;

        /**
         * Detector Queue Limit parameter in seconds (0-255 sec). The length of time that an
         * actuation from a queue detector may continue into the phase green. This time begins when
         * the phase becomes green and when it expires any associated detector inputs shall be
         * ignored. This time may be shorter due to other overriding device parameters (i.e. Maximum
         * time, Force Off’s, ...).
         */
        private int vehicleDetectorQueueLimit;

        /**
         * Detector No Activity diagnostic Parameter in minutes (0–255 min.) . If an active detector
         * does not exhibit an actuation in the specified period, it is considered a fault by the
         * diagnostics and the detector is classified as Failed. A value of 0 for this object shall
         * disable this diagnostic for this detector.
         */
        private int vehicleDetectorNoActivity;

        /**
         * Detector Maximum Presence diagnostic Parameter in minutes (0-255 min.). If an active
         * detector exhibits continuous detection for too long a period, it is considered a fault by
         * the diagnostics and the detector is classified as Failed. A value of 0 for this object
         * shall disable this diagnostic for this detector.
         */
        private int vehicleDetectorMaxPresence;

        /**
         * Detector Erratic Counts diagnostic Parameter in counts/minute (0-255 cpm). If an active
         * detector exhibits excessive actuations, it is considered a fault by the diagnostics and
         * the detector is classified as Failed. A value of 0 for this object shall disable this
         * diagnostic for this detector.
         */
        private int vehicleDetectorErraticCounts;

        /**
         * Detector Fail Time in seconds (0..255 sec). If a detector diagnostic indicates that the
         * associated detector input is failed, then a call shall be placed on the associated phase
         * during all non-green intervals. When each green interval begins the call shall be
         * maintained for the length of time specified by this object and then removed. If the value
         * of this object equals the maximum value (255) then a constant call shall be placed on the
         * associated phase (max recall). If the value of this object equals zero then no call shall
         * be placed on the associated phase for any interval (no recall). Compliant devices may
         * support a limited capability for this object (i.e. only max recall or max recall and no
         * recall). At a minimum the max recall setting must be supported.
         */
        private int vehicleDetectorFailTime;

        /**
         * This object shall return indications of detector alarms. Detector Alarms are indicated as
         * follows: Bit 7: Other Fault - The detector has failed due to some other cause. Bit 6:
         * Reserved. Bit 5: Reserved. Bit 4: Configuration Fault - Detector is assigned but is not
         * supported. Bit 3: Communications Fault - Communications to the device (if present) have
         * failed. Bit 2: Erratic Output Fault - This detector has been flagged as non-operational
         * due to erratic outputs (excessive counts) by the CU detector diagnostic. Bit 1: Max
         * Presence Fault - This detector has been flagged as non-operational due to a presence
         * indicator that exceeded the maximum expected time by the CU detector diagnostic. Bit 0:
         * No Activity Fault - This detector has been flagged as non-operational due to lower than
         * expected activity by the CU detector diagnostic. Once set a bit shall maintain its state
         * as long as the condition exists. The bit shall clear when the condition no longer exists.
         */
        private int vehicleDetectorAlarms;

        /**
         * This object shall return detector device reported alarms (via some communications
         * mechanism). Inductive Loop Detector Alarms are indicated as follows: Bit 7: Reserved. Bit
         * 6: Reserved. Bit 5: Reserved. Bit 4: Excessive Change Fault - This detector has been
         * flagged as non-operational due to an inductance change that exceeded expected values. Bit
         * 3: Shorted Loop Fault - This detector has been flagged as non-operational due to a
         * shorted loop wire. Bit 2: Open Loop Fault - This detector has been flagged as
         * non-operational due to an open loop (broken wire). Bit 1: Watchdog Fault - This detector
         * has been flagged as non-operational due to a watchdog error. Bit 0: Other - This detector
         * has been flagged as non-operational due to some other error. Once set a bit shall
         * maintain its state as long as the condition exists. The bit shall clear when the
         * condition no longer exists.
         */
        private int vehicleDetectorReportedAlarms;

        /**
         * This object when set to TRUE (one) shall cause the CU to command the associated detector
         * to reset. This object shall automatically return to FALSE (zero) after the CU has issued
         * the reset command. NOTE: this may affect other detector (detector channels) that are
         * physically attached to a common reset line.
         */
        private int vehicleDetectorReset;
    }

    /**
     * A table containing detector status in groups of eight detectors. The number of rows in this
     * table is equal to the maxVehicleDetectorStatusGroups object.
     */
    public class VehicleDetectorStatusGroup {

        /**
         * The detector status group number for objects in this row. This value shall not exceed the
         * maxVehicleDetectorStatusGroups object value.
         */
        private int vehicleDetectorStatusGroupNumber;

        /**
         * This object shall return the detection status of each detector associated with the group.
         * Each detector shall be represented as ON (detect) or OFF (no-detect) by individual bits
         * in this object. If a detector is ON then the associated bit shall be set (1). If a
         * detector is OFF then the associated bit shall be clear (0). Bit 7: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) Bit 6: Det # = ( vehicleDetectorStatusGroupNumber *
         * 8) - 1 Bit 5: Det # = ( vehicleDetectorStatusGroupNumber * 8) - 2 Bit 4: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 3 Bit 3: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 4 Bit 2: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 5 Bit 1: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 6 Bit 0: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 7
         */
        private int vehicleDetectorStatusGroupActive;

        /**
         * This object shall return the alarm status of the detectors associated with the group.
         * Each detector alarm status shall be represented as ON or OFF by individual bits in this
         * object. If any detector alarm (defined in the vehicleDetectorAlarm object) is active the
         * associated bit shall be set (1). If a detector alarm is not active the associated bit
         * shall be clear (0). Bit 7: Det # = ( vehicleDetectorStatusGroupNumber * 8) Bit 6: Det # =
         * ( vehicleDetectorStatusGroupNumber * 8) - 1 Bit 5: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 2 Bit 4: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 3 Bit 3: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 4 Bit 2: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 5 Bit 1: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 6 Bit 0: Det # = (
         * vehicleDetectorStatusGroupNumber * 8) - 7
         */
        private int vehicleDetectorStatusGroupAlarms;

        public int getVehicleDetectorStatusGroupActive() {
            return vehicleDetectorStatusGroupActive;
        }

        public void setVehicleDetectorStatusGroupActive(int vehicleDetectorStatusGroupActive) {
            this.vehicleDetectorStatusGroupActive = vehicleDetectorStatusGroupActive;
        }

        public int getVehicleDetectorStatusGroupAlarms() {
            return vehicleDetectorStatusGroupAlarms;
        }

        public void setVehicleDetectorStatusGroupAlarms(int vehicleDetectorStatusGroupAlarms) {
            this.vehicleDetectorStatusGroupAlarms = vehicleDetectorStatusGroupAlarms;
        }

        public int getVehicleDetectorStatusGroupNumber() {
            return vehicleDetectorStatusGroupNumber;
        }

        public void setVehicleDetectorStatusGroupNumber(int vehicleDetectorStatusGroupNumber) {
            this.vehicleDetectorStatusGroupNumber = vehicleDetectorStatusGroupNumber;
        }
    }

    public class VolumeOccupancyReport {

        /**
         * This object defines a Sequence Number for Volume/Occupancy data collection. This object
         * is used to detect duplicate or missing reports. The value cycles within the limits of 0
         * to 255. This object is incremented by one at the expiration of the volumeOccupancyPeriod
         * time.
         */
        private int volumeOccupancySequence;

        // NOT MAPPED
        /**
         * This object defines the number of seconds (0-255 sec) that comprise the Volume/Occupancy
         * collection period. When the collection period expires the device shall increment the
         * volumeOccupancySequence, update the volumeOccupancyTable entries and reset the volume
         * occupancy timer.
         */
        private int volumeOccupancyPeriod;

        public int getVolumeOccupancyPeriod() {
            return volumeOccupancyPeriod;
        }

        public void setVolumeOccupancyPeriod(int volumeOccupancyPeriod) {
            this.volumeOccupancyPeriod = volumeOccupancyPeriod;
        }

        public int getVolumeOccupancySequence() {
            return volumeOccupancySequence;
        }

        public void setVolumeOccupancySequence(int volumeOccupancySequence) {
            this.volumeOccupancySequence = volumeOccupancySequence;
        }
    }

    public class VolumeOccupancyTable {

        /**
         * Detector Volume data collected over the volumeOccupancyPeriod. This value shall range
         * from 0 to 254 indicating the volume of traffic crossing the associated detectorNumber
         * during the collection period. The value 255 shall indicate volume overflow.
         */
        private int detectorVolume;

        /**
         * Detector Occupancy as a percentage of the volumeOccupancyPeriod over which the data was
         * collected or Detector Unit Diagnostic Information. The value of the object shall indicate
         * occupancy or detector diagnostic information as follows: Range Meaning 0-200 Detector
         * Occupancy in 0.5% Increments 201-209 Reserved 210 Max Presence Fault 211 No Activity
         * Fault 212 Open loop Fault 213 Shorted loop Fault 214 Excessive Change Fault 215 Reserved
         * 216 Watchdog Fault 217 Erratic Output Fault 218-255 Reserved Faults shall be indicated
         * for all collection periods during which a fault is detected if either occupancy data or
         * volume data is being collected. The highest numbered fault shall be presented if more
         * than one fault is active (i.e. indicate OpenLoop rather than NoActivity).
         */
        private int detectorOccupancy;

        public int getDetectorOccupancy() {
            return detectorOccupancy;
        }

        public void setDetectorOccupancy(int detectorOccupancy) {
            this.detectorOccupancy = detectorOccupancy;
        }

        public int getDetectorVolume() {
            return detectorVolume;
        }

        public void setDetectorVolume(int detectorVolume) {
            this.detectorVolume = detectorVolume;
        }
    }

    public class PedestrianDetectorTable {

        /**
         * The pedestrianDetector number for objects in this row. The value shall not exceed the
         * maxPedestrianDetectors object value.
         */
        private int pedestrianDetectorNumber;

        /**
         * This object contains assigned phase number for the pedestrian detector input associated
         * with this row. The associated detector call capability is enabled when this object is set
         * to a non-zero value. The value shall not exceed the value of maxPhases.
         */
        private int pedestrianDetectorCallPhase;

        /**
         * Pedestrian Detector No Activity diagnostic Parameter in minutes (0–255 min.) . If an
         * active detector does not exhibit an actuation in the specified period, it is considered a
         * fault by the diagnostics and the detector is classified as Failed. A value of 0 for this
         * object shall disable this diagnostic for this detector.
         */
        private int pedestrianDetectorNoActivity;

        /**
         * Pedestrian Detector Maximum Presence diagnostic Parameter in minutes (0-255 min.). If an
         * active detector exhibits continuous detection for too long a period, it is considered a
         * fault by the diagnostics and the detector is classified as Failed. A value of 0 for this
         * object shall disable this diagnostic for this detector.
         */
        private int pedestrianDetectorMaxPresence;

        /**
         * Pedestrian Detector Erratic Counts diagnostic Parameter in counts/minute (0-255 cpm). If
         * an active detector exhibits excessive actuations, it is considered a fault by the
         * diagnostics and the detector is classified as Failed. A value of 0 for this object shall
         * disable this diagnostic for this detector.
         */
        private int pedestrianDetectorErraticCounts;

        /**
         * This object shall return indications of detector alarms. Detector Alarms are indicated as
         * follows: Bit 7: Other Fault - The detector has failed due to some other cause. Bit 6:
         * Reserved. Bit 5: Reserved. Bit 4: Configuration Fault - Detector is assigned but is not
         * supported. Bit 3: Communications Fault - Communications to the device (if present) have
         * failed. Bit 2: Erratic Output Fault - This detector has been flagged as non-operational
         * due to erratic outputs (excessive counts) by the CU detector diagnostic. Bit 1: Max
         * Presence Fault - This detector has been flagged as non-operational due to a presence
         * indicator that exceeded the maximum expected time by the CU detector diagnostic. Bit 0:
         * No Activity Fault - This detector has been flagged as non-operational due to lower than
         * expected activity by the CU detector diagnostic Once set a bit shall maintain its state
         * as long as the condition exists. The bit shall clear when the condition no longer exists.
         */
        private int pedestrianDetectorAlarms;

        public int getPedestrianDetectorAlarms() {
            return pedestrianDetectorAlarms;
        }

        public void setPedestrianDetectorAlarms(int pedestrianDetectorAlarms) {
            this.pedestrianDetectorAlarms = pedestrianDetectorAlarms;
        }

        public int getPedestrianDetectorCallPhase() {
            return pedestrianDetectorCallPhase;
        }

        public void setPedestrianDetectorCallPhase(int pedestrianDetectorCallPhase) {
            this.pedestrianDetectorCallPhase = pedestrianDetectorCallPhase;
        }

        public int getPedestrianDetectorErraticCounts() {
            return pedestrianDetectorErraticCounts;
        }

        public void setPedestrianDetectorErraticCounts(int pedestrianDetectorErraticCounts) {
            this.pedestrianDetectorErraticCounts = pedestrianDetectorErraticCounts;
        }

        public int getPedestrianDetectorMaxPresence() {
            return pedestrianDetectorMaxPresence;
        }

        public void setPedestrianDetectorMaxPresence(int pedestrianDetectorMaxPresence) {
            this.pedestrianDetectorMaxPresence = pedestrianDetectorMaxPresence;
        }

        public int getPedestrianDetectorNoActivity() {
            return pedestrianDetectorNoActivity;
        }

        public void setPedestrianDetectorNoActivity(int pedestrianDetectorNoActivity) {
            this.pedestrianDetectorNoActivity = pedestrianDetectorNoActivity;
        }

        public int getPedestrianDetectorNumber() {
            return pedestrianDetectorNumber;
        }

        public void setPedestrianDetectorNumber(int pedestrianDetectorNumber) {
            this.pedestrianDetectorNumber = pedestrianDetectorNumber;
        }
    }

    public int getActiveVolumeOccupancyDetectors() {
        return activeVolumeOccupancyDetectors;
    }

    public void setActiveVolumeOccupancyDetectors(int activeVolumeOccupancyDetectors) {
        this.activeVolumeOccupancyDetectors = activeVolumeOccupancyDetectors;
    }

    public int getMaxPedestrianDetectors() {
        return maxPedestrianDetectors;
    }

    public void setMaxPedestrianDetectors(int maxPedestrianDetectors) {
        this.maxPedestrianDetectors = maxPedestrianDetectors;
    }

    public int getMaxVehicleDetectorStatusGroups() {
        return maxVehicleDetectorStatusGroups;
    }

    public void setMaxVehicleDetectorStatusGroups(int maxVehicleDetectorStatusGroups) {
        this.maxVehicleDetectorStatusGroups = maxVehicleDetectorStatusGroups;
    }

    public int getMaxVehicleDetectors() {
        return maxVehicleDetectors;
    }

    public void setMaxVehicleDetectors(int maxVehicleDetectors) {
        this.maxVehicleDetectors = maxVehicleDetectors;
    }

    public PedestrianDetectorTable[] getPedestrianDetectors() {
        return pedestrianDetectors.clone();
    }

    public void setPedestrianDetectors(PedestrianDetectorTable[] pedestrianDetectors) {
        this.pedestrianDetectors = pedestrianDetectors.clone();
    }

    public VehicleDetectorEntry[] getVehicleDetectors() {
        return vehicleDetectors.clone();
    }

    public void setVehicleDetectors(VehicleDetectorEntry[] vehicleDetectors) {
        this.vehicleDetectors = vehicleDetectors.clone();
    }

    public VehicleDetectorStatusGroup[] getVehicleStatusGroups() {
        return vehicleStatusGroups.clone();
    }

    public void setVehicleStatusGroups(VehicleDetectorStatusGroup[] vehicleStatusGroups) {
        this.vehicleStatusGroups = vehicleStatusGroups.clone();
    }

    public VolumeOccupancyTable[] getVolumeOccupancy() {
        return volumeOccupancy.clone();
    }

    public void setVolumeOccupancy(VolumeOccupancyTable[] volumeOccupancy) {
        this.volumeOccupancy = volumeOccupancy.clone();
    }

    public long getMessageTime() {
        return messageTime;
    }

}
