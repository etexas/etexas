/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.frames;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.LaneConnectionID;
import org.etexascode.j2735_2016.elements.PedestrianBicycleDetect;
import org.etexascode.j2735_2016.elements.WaitOnStopLine;
import org.etexascode.j2735_2016.elements.ZoneLength;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The connection maneuver assist frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class ConnectionManeuverAssist implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the connection maneuver assist.
     */
    public static final int NUM_BITS = 6;

    /**
     * The lane connection ID element.
     */
    private LaneConnectionID connectionId;

    /**
     * The queue length (zone length element). (OPTIONAL)
     */
    private ZoneLength queueLength;

    /**
     * The available storage length (zone length element). (OPTIONAL)
     */
    private ZoneLength availableStorageLength;

    /**
     * The wait on stop line element. (OPTIONAL)
     */
    private WaitOnStopLine waitOnStop;

    /**
     * The pedestrian bicycle detect element. (OPTIONAL)
     */
    private PedestrianBicycleDetect pedBicycleDetect;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public ConnectionManeuverAssist() {

        connectionId = new LaneConnectionID();
    }

    /**
     * A constructor for the connection maneuver assist frame for all required fields.
     * 
     * @param connectionId The lane connection ID element.
     */
    public ConnectionManeuverAssist(LaneConnectionID connectionId) {

        this.connectionId = Objects.requireNonNull(connectionId);
    }

    /**
     * A constructor for the intersection reference ID frame for all required fields (primitive).
     * 
     * @param connectionId The lane connection ID value.
     */
    public ConnectionManeuverAssist(int connectionId) {

        this.connectionId = new LaneConnectionID(connectionId);
    }

    /**
     * A getter for the lane connection ID element.
     * 
     * @return The lane connection ID element.
     */
    public LaneConnectionID getConnectionId() {

        return connectionId;
    }

    /**
     * A setter for the lane connection ID element.
     * 
     * @param connectionId The lane connection ID element to set.
     */
    public void setConnectionId(LaneConnectionID connectionId) {

        this.connectionId = Objects.requireNonNull(connectionId);
    }

    /**
     * A setter for the lane connection ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param connectionId The lane connection ID value to be set in the element.
     */
    public void setConnectionId(int connectionId) {

        this.connectionId.setValue(connectionId);
    }

    /**
     * A getter for the queue length (zone length element).
     * 
     * @return The queue length (zone length element).
     */
    public ZoneLength getQueueLength() {

        return queueLength;
    }

    /**
     * A setter for the queue length (zone length element).
     * 
     * @param queueLength The queue length (zone length element) to set.
     */
    public void setQueueLength(ZoneLength queueLength) {

        this.queueLength = queueLength;
    }

    /**
     * A setter for the queue length (zone length element). Allows primitive data to be passed to
     * the primitive element.
     * 
     * @param queueLength The queue length value to be set in the element.
     */
    public void setQueueLength(int queueLength) {

        if (this.queueLength == null) {

            this.queueLength = new ZoneLength();
        }
        this.queueLength.setValue(queueLength);
    }

    /**
     * A getter for the available storage length (zone length element).
     * 
     * @return The available storage length (zone length element).
     */
    public ZoneLength getAvailableStorageLength() {

        return availableStorageLength;
    }

    /**
     * A setter for the available storage length (zone length element).
     * 
     * @param availableStorageLength The available storage length (zone length element) to set.
     */
    public void setAvailableStorageLength(ZoneLength availableStorageLength) {

        this.availableStorageLength = availableStorageLength;
    }

    /**
     * A setter for the available storage length (zone length element). Allows primitive data to be
     * passed to the primitive element.
     * 
     * @param availableStorageLength The available storage length value to be set in the element.
     */
    public void setAvailableStorageLength(int availableStorageLength) {

        if (this.availableStorageLength == null) {

            this.availableStorageLength = new ZoneLength();
        }
        this.availableStorageLength.setValue(availableStorageLength);
    }

    /**
     * A getter for the wait on stop line element.
     * 
     * @return The wait on stop line element.
     */
    public WaitOnStopLine getWaitOnStop() {

        return waitOnStop;
    }

    /**
     * A setter for the wait on stop line element.
     * 
     * @param waitOnStop The wait on stop line element to set.
     */
    public void setWaitOnStop(WaitOnStopLine waitOnStop) {

        this.waitOnStop = waitOnStop;
    }

    /**
     * A setter for the wait on stop line element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param waitOnStop The wait on stop line value to be set in the element.
     */
    public void setWaitOnStop(boolean waitOnStop) {

        if (this.waitOnStop == null) {

            this.waitOnStop = new WaitOnStopLine();
        }
        this.waitOnStop.setValue(waitOnStop);
    }

    /**
     * A getter for the pedestrian bicycle detect element.
     * 
     * @return The pedestrian bicycle detect element.
     */
    public PedestrianBicycleDetect getPedBicycleDetect() {

        return pedBicycleDetect;
    }

    /**
     * A setter for the pedestrian bicycle detect element.
     * 
     * @param pedBicycleDetect The pedestrian bicycle detect element to set.
     */
    public void setPedBicycleDetect(PedestrianBicycleDetect pedBicycleDetect) {

        this.pedBicycleDetect = pedBicycleDetect;
    }

    /**
     * A setter for the pedestrian bicycle detect element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param pedBicycleDetect The pedestrian bicycle detect value to be set in the element.
     */
    public void setPedBicycleDetect(boolean pedBicycleDetect) {

        if (this.pedBicycleDetect == null) {

            this.pedBicycleDetect = new PedestrianBicycleDetect();
        }
        this.pedBicycleDetect.setValue(pedBicycleDetect);
    }

    @Override
    public String encodeUPER() {

        StringBuilder connectionManeuverAssistBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        connectionManeuverAssistBits.append(connectionId.encodeUPER());

        if (queueLength != null) {

            optionalBits.append('1');
            connectionManeuverAssistBits.append(queueLength.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (availableStorageLength != null) {

            optionalBits.append('1');
            connectionManeuverAssistBits.append(availableStorageLength.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (waitOnStop != null) {

            optionalBits.append('1');
            connectionManeuverAssistBits.append(waitOnStop.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (pedBicycleDetect != null) {

            optionalBits.append('1');
            connectionManeuverAssistBits.append(pedBicycleDetect.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');
        connectionManeuverAssistBits.insert(0, optionalBits);

        return connectionManeuverAssistBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (ConnectionManeuverAssist.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ConnectionManeuverAssist frame (%d)", ConnectionManeuverAssist.NUM_BITS));
        }

        String connectionManeuverAssistOptionalBits = bits.substring(0, ConnectionManeuverAssist.NUM_BITS);
        bits = bits.substring(ConnectionManeuverAssist.NUM_BITS);

        if (connectionManeuverAssistOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The ConnectionManueverAssist extension is not supported");
        }

        if (connectionManeuverAssistOptionalBits.charAt(5) != '0') {

            throw new IllegalArgumentException("The ConnectionManueverAssist regional extension is not supported");
        }

        bits = connectionId.decodeUPER(bits);

        if (connectionManeuverAssistOptionalBits.charAt(1) == '1') {

            queueLength = new ZoneLength();
            bits = queueLength.decodeUPER(bits);
        }

        if (connectionManeuverAssistOptionalBits.charAt(2) == '1') {

            availableStorageLength = new ZoneLength();
            bits = availableStorageLength.decodeUPER(bits);
        }

        if (connectionManeuverAssistOptionalBits.charAt(3) == '1') {

            waitOnStop = new WaitOnStopLine();
            bits = waitOnStop.decodeUPER(bits);
        }

        if (connectionManeuverAssistOptionalBits.charAt(4) == '1') {

            pedBicycleDetect = new PedestrianBicycleDetect();
            bits = pedBicycleDetect.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(connectionId, queueLength, availableStorageLength, waitOnStop, pedBicycleDetect);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof ConnectionManeuverAssist)) {

            return false;
        }
        ConnectionManeuverAssist frame = (ConnectionManeuverAssist)object;
        return this.connectionId.equals(frame.connectionId)
                && Objects.equals(this.queueLength, frame.queueLength)
                && Objects.equals(this.availableStorageLength, frame.availableStorageLength)
                && Objects.equals(this.waitOnStop, frame.waitOnStop)
                && Objects.equals(this.pedBicycleDetect, frame.pedBicycleDetect);
    }
}
