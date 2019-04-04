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
 * This object is an identifier used to group all objects for support of NEMA TS 2 Port 1
 * activities.
 * 
 * @author dranker
 */
public class TS2Port1 implements Serializable {

    /**
     * The Maximum Number of Port 1 addresses this Actuated Controller Unit supports. This object
     * indicates the maximum rows which shall appear in the port1Table object.
     */
    private int maxPort1Addresses;

    private Port1Table[] port1Addresses;

    public TS2Port1() {
        port1Addresses = new Port1Table[maxPort1Addresses];
    }

    /**
     * A table containing Actuated Controller Unit port 1 parameters. The number of rows in this
     * table is equal to maxPort1Addresses object. Address 255 is reserved for the all stations
     * (link devices) address.
     */
    public class Port1Table {

        /**
         * The (Port 1 address plus one) for objects in this row. This value shall not exceed the
         * maxPort1Addresses object value.
         */
        private int port1Number;

        /**
         * This object is used to program the CU as to the presence or absence of a device for this
         * Port 1 address. The CU shall transmit Command Frames only to those devices that are
         * present as determined by this programming. True (one) - the device is present. False
         * (zero) - the device is not present.
         */
        private int port1DevicePresent;

        /**
         * To enable or disable Frame 40 messages to the device at this Port 1 address. Frame 40 is
         * used to poll the secondary stations for a secondary to secondary message exchange.
         * Command 40 series frames shall be transmitted only to those devices that are enabled, as
         * determined by this programming. TRUE (one) - Enable frame 40 messages for this device.
         * FALSE (zero) - Disable frame 40 messages for this device.
         */
        private int port1Frame40Enable;

        /**
         * This object indicates the communications status with the associated device: other: This
         * indicates that some other communications faulthas been detected. online: This indicates
         * that at least five of the most recent 10 response transfers were received correctly.
         * responseFault: This indicates that more than 5 of the most recent 10 response transfers
         * were received incorrectly.
         */
        private int port1Status;

        /**
         * This object indicates the frame number that caused the most recent fault.
         */
        private int port1FaultFrame;

        public int getPort1DevicePresent() {
            return port1DevicePresent;
        }

        public void setPort1DevicePresent(int port1DevicePresent) {
            this.port1DevicePresent = port1DevicePresent;
        }

        public int getPort1FaultFrame() {
            return port1FaultFrame;
        }

        public void setPort1FaultFrame(int port1FaultFrame) {
            this.port1FaultFrame = port1FaultFrame;
        }

        public int getPort1Frame40Enable() {
            return port1Frame40Enable;
        }

        public void setPort1Frame40Enable(int port1Frame40Enable) {
            this.port1Frame40Enable = port1Frame40Enable;
        }

        public int getPort1Number() {
            return port1Number;
        }

        public void setPort1Number(int port1Number) {
            this.port1Number = port1Number;
        }

        public int getPort1Status() {
            return port1Status;
        }

        public void setPort1Status(int port1Status) {
            this.port1Status = port1Status;
        }

    }
}
