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

/**
 * The ring node contains objects that support ring configuration, status, and control functions in
 * the device.
 * 
 * @author dranker
 */
public class Ring implements Serializable {

    /**
     * The value of this object shall specify the maximum number of rings this device supports.
     */
    private int maxRings;

    /**
     * The value of this object shall specify the maximum number of sequence plans this device
     * supports.
     */
    private int maxSequences;

    /**
     * The maximum number of Ring Control Groups (8 rings per group) this Actuated Controller Unit
     * supports. This value is equal to TRUNCATE[(maxRings + 7) / 8]. This object indicates the
     * maximum rows which shall appear in the ringControlGroupTable object.
     */
    private int maxRingControlGroups;

    // Temp
    private int[] ringStatus;

    private RingControlGroupTable[] ringControlGroups;

    private SequenceTable[] sequences;

    public Ring() {
        sequences = new SequenceTable[maxSequences];
        ringControlGroups = new RingControlGroupTable[maxRingControlGroups];
        ringStatus = new int[maxRings];
    }

    /**
     * This table contains all the sequence plans for the controller. A sequence plan shall consist
     * of one row for each ring that the CU supports. Each row defines the phase service order for
     * that ring.
     */
    public class SequenceTable {

        /**
         * This number identifies a sequence plan. Each row of the table contains the phase sequence
         * for a ring. A sequence plan shall consist of one row for each ring that defines the phase
         * sequences for that ring.
         */
        private int sequenceNumber;

        /**
         * This number identifies the ring number this phase sequence applies to.
         */
        private int sequenceRingNumber;

        /**
         * Each octet is a Phase Number (binary value) within the associated ring number. The phase
         * number value shall not exceed the maxPhases object value. The order of phase numbers
         * determines the phase sequence for the ring. The phase numbers shall not be ordered in a
         * manner that would violate the Consistency Checks defined in Annex B.
         */
        private String sequenceData;

        public String getSequenceData() {
            return sequenceData;
        }

        public void setSequenceData(String sequenceData) {
            this.sequenceData = sequenceData;
        }

        public int getSequenceNumber() {
            return sequenceNumber;
        }

        public void setSequenceNumber(int sequenceNumber) {
            this.sequenceNumber = sequenceNumber;
        }

        public int getSequenceRingNumber() {
            return sequenceRingNumber;
        }

        public void setSequenceRingNumber(int sequenceRingNumber) {
            this.sequenceRingNumber = sequenceRingNumber;
        }
    }

    public class RingControlGroupTable {

        /**
         * The Ring Control Group number for objects in this row. This value shall not exceed the
         * maxRingControlGroups object value.
         */
        private int ringControlGroupNumber;

        /**
         * This object is used to allow a remote entity to stop timing in the device. The device
         * shall activate/deactivate the System Stop Time control for a ring according to the
         * respective bit value as follows: bit = 0 - deactivate the ring control bit = 1 - activate
         * the ring control Bit 7: Ring # = (ringControlGroupNumber * 8) Bit 6: Ring # =
         * (ringControlGroupNumber * 8) - 1 Bit 5: Ring # = (ringControlGroupNumber * 8) - 2 Bit 4:
         * Ring # = (ringControlGroupNumber * 8) - 3 Bit 3: Ring # = (ringControlGroupNumber * 8) -
         * 4 Bit 2: Ring # = (ringControlGroupNumber * 8) - 5 Bit 1: Ring # =
         * (ringControlGroupNumber * 8) - 6 Bit 0: Ring # = (ringControlGroupNumber * 8) - 7 The
         * device shall reset this object to ZERO when in BACKUP Mode. A write to this object shall
         * reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int ringControlGroupStopTime;

        /**
         * This object is used to allow a remote entity to terminate phases via a force off command
         * in the device. The device shall activate/deactivate the System Force Off control for a
         * ring according to the respective bit value as follows: bit = 0 - deactivate the ring
         * control bit = 1 - activate the ring control Bit 7: Ring # = (ringControlGroupNumber * 8)
         * Bit 6: Ring # = (ringControlGroupNumber * 8) - 1 Bit 5: Ring # = (ringControlGroupNumber
         * * 8) - 2 Bit 4: Ring # = (ringControlGroupNumber * 8) - 3 Bit 3: Ring # =
         * (ringControlGroupNumber * 8) - 4 Bit 2: Ring # = (ringControlGroupNumber * 8) - 5 Bit 1:
         * Ring # = (ringControlGroupNumber * 8) - 6 Bit 0: Ring # = (ringControlGroupNumber * 8) -
         * 7 The device shall reset this object to ZERO when in BACKUP Mode. A write to this object
         * shall reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int ringControlGroupForceOff;

        /**
         * This object is used to allow a remote entity to request Maximum 2 timings in the device.
         * The device shall activate/deactivate the System Maximum 2 control for a ring according to
         * the respective bit value as follows: bit = 0 - deactivate the ring control bit = 1 -
         * activate the ring control Bit 7: Ring # = (ringControlGroupNumber * 8) Bit 6: Ring # =
         * (ringControlGroupNumber * 8) - 1 Bit 5: Ring # = (ringControlGroupNumber * 8) - 2 Bit 4:
         * Ring # = (ringControlGroupNumber * 8) - 3 Bit 3: Ring # = (ringControlGroupNumber * 8) -
         * 4 Bit 2: Ring # = (ringControlGroupNumber * 8) - 5 Bit 1: Ring # =
         * (ringControlGroupNumber * 8) - 6 Bit 0: Ring # = (ringControlGroupNumber * 8) - 7 The
         * device shall reset this object to ZERO when in BACKUP Mode. A write to this object shall
         * reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int ringControlGroupMax2;

        /**
         * This object is used to allow a remote entity to request internal maximum timings be
         * inhibited in the device. The device shall activate/deactivate the System Max Inhibit
         * control for a ring according to the respective bit value as follows: bit = 0 - deactivate
         * the ring control bit = 1 - activate the ring control Bit 7: Ring # =
         * (ringControlGroupNumber * 8) Bit 6: Ring # = (ringControlGroupNumber * 8) - 1 Bit 5: Ring
         * # = (ringControlGroupNumber * 8) - 2 Bit 4: Ring # = (ringControlGroupNumber * 8) - 3 Bit
         * 3: Ring # = (ringControlGroupNumber * 8) - 4 Bit 2: Ring # = (ringControlGroupNumber * 8)
         * - 5 Bit 1: Ring # = (ringControlGroupNumber * 8) - 6 Bit 0: Ring # =
         * (ringControlGroupNumber * 8) - 7 The device shall reset this object to ZERO when in
         * BACKUP Mode. A write to this object shall reset the Backup timer to ZERO (see
         * unitBackupTime).
         */
        private int ringControlGroupMaxInhibit;

        /**
         * This object is used to allow a remote entity to request a pedestrian recycle in the
         * device. The device shall activate/deactivate the System Ped Recycle control for a ring
         * according to the respective bit value as follows: bit = 0 - deactivate the ring control
         * bit = 1 - activate the ring control Bit 7: Ring # = (ringControlGroupNumber * 8) Bit 6:
         * Ring # = (ringControlGroupNumber * 8) - 1 Bit 5: Ring # = (ringControlGroupNumber * 8) -
         * 2 Bit 4: Ring # = (ringControlGroupNumber * 8) - 3 Bit 3: Ring # =
         * (ringControlGroupNumber * 8) - 4 Bit 2: Ring # = (ringControlGroupNumber * 8) - 5 Bit 1:
         * Ring # = (ringControlGroupNumber * 8) - 6 Bit 0: Ring # = (ringControlGroupNumber * 8) -
         * 7 The device shall reset this object to ZERO when in BACKUP Mode. A write to this object
         * shall reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int ringControlGroupPedRecycle;

        /**
         * This object is used to allow a remote entity to request red rest in the device. The
         * device shall activate/deactivate the System Red Rest control for a ring according to the
         * respective bit value as follows: bit = 0 - deactivate the ring control bit = 1 - activate
         * the ring control Bit 7: Ring # = (ringControlGroupNumber * 8) Bit 6: Ring # =
         * (ringControlGroupNumber * 8) - 1 Bit 5: Ring # = (ringControlGroupNumber * 8) - 2 Bit 4:
         * Ring # = (ringControlGroupNumber * 8) - 3 Bit 3: Ring # = (ringControlGroupNumber * 8) -
         * 4 Bit 2: Ring # = (ringControlGroupNumber * 8) - 5 Bit 1: Ring # =
         * (ringControlGroupNumber * 8) - 6 Bit 0: Ring # = (ringControlGroupNumber * 8) - 7 The
         * device shall reset this object to ZERO when in BACKUP Mode. A write to this object shall
         * reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int ringControlGroupRedRest;

        /**
         * This object is used to allow a remote entity to omit red clearances in the device. The
         * device shall activate/deactivate the System Omit Red Clear control for a ring according
         * to the respective bit value as follows: bit = 0 - deactivate the ring control bit = 1 -
         * activate the ring control Bit 7: Ring # = (ringControlGroupNumber * 8) Bit 6: Ring # =
         * (ringControlGroupNumber * 8) - 1 Bit 5: Ring # = (ringControlGroupNumber * 8) - 2 Bit 4:
         * Ring # = (ringControlGroupNumber * 8) - 3 Bit 3: Ring # = (ringControlGroupNumber * 8) -
         * 4 Bit 2: Ring # = (ringControlGroupNumber * 8) - 5 Bit 1: Ring # =
         * (ringControlGroupNumber * 8) - 6 Bit 0: Ring # = (ringControlGroupNumber * 8) - 7 The
         * device shall reset this object to ZERO when in BACKUP Mode. A write to this object shall
         * reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int ringControlGroupOmitRedClear;

        public int getRingControlGroupForceOff() {
            return ringControlGroupForceOff;
        }

        public void setRingControlGroupForceOff(int ringControlGroupForceOff) {
            this.ringControlGroupForceOff = ringControlGroupForceOff;
        }

        public int getRingControlGroupMax2() {
            return ringControlGroupMax2;
        }

        public void setRingControlGroupMax2(int ringControlGroupMax2) {
            this.ringControlGroupMax2 = ringControlGroupMax2;
        }

        public int getRingControlGroupMaxInhibit() {
            return ringControlGroupMaxInhibit;
        }

        public void setRingControlGroupMaxInhibit(int ringControlGroupMaxInhibit) {
            this.ringControlGroupMaxInhibit = ringControlGroupMaxInhibit;
        }

        public int getRingControlGroupNumber() {
            return ringControlGroupNumber;
        }

        public void setRingControlGroupNumber(int ringControlGroupNumber) {
            this.ringControlGroupNumber = ringControlGroupNumber;
        }

        public int getRingControlGroupOmitRedClear() {
            return ringControlGroupOmitRedClear;
        }

        public void setRingControlGroupOmitRedClear(int ringControlGroupOmitRedClear) {
            this.ringControlGroupOmitRedClear = ringControlGroupOmitRedClear;
        }

        public int getRingControlGroupPedRecycle() {
            return ringControlGroupPedRecycle;
        }

        public void setRingControlGroupPedRecycle(int ringControlGroupPedRecycle) {
            this.ringControlGroupPedRecycle = ringControlGroupPedRecycle;
        }

        public int getRingControlGroupRedRest() {
            return ringControlGroupRedRest;
        }

        public void setRingControlGroupRedRest(int ringControlGroupRedRest) {
            this.ringControlGroupRedRest = ringControlGroupRedRest;
        }

        public int getRingControlGroupStopTime() {
            return ringControlGroupStopTime;
        }

        public void setRingControlGroupStopTime(int ringControlGroupStopTime) {
            this.ringControlGroupStopTime = ringControlGroupStopTime;
        }
    }
}
