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
import org.etexascode.j2735_2016.elements.RestrictionClassID;
import org.etexascode.j2735_2016.elements.SignalGroupID;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The connection frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class Connection implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the connection.
     */
    public static final int NUM_BITS = 4;

    /**
     * The connecting lane frame.
     */
    private ConnectingLane connectingLane;

    /**
     * The intersection reference ID frame. (OPTIONAL)
     */
    private IntersectionReferenceID remoteIntersection;

    /**
     * The signal group ID element. (OPTIONAL)
     */
    private SignalGroupID signalGroup;

    /**
     * The restriction class ID element. (OPTIONAL)
     */
    private RestrictionClassID userClass;

    /**
     * The lane connection ID element. (OPTIONAL)
     */
    private LaneConnectionID connectionId;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public Connection() {

        connectingLane = new ConnectingLane();
    }

    /**
     * A constructor for the connection frame for all required fields.
     * 
     * @param connectingLane The connecting lane frame.
     */
    public Connection(ConnectingLane connectingLane) {

        this.connectingLane = Objects.requireNonNull(connectingLane);
    }

    /**
     * A getter for the connecting lane frame.
     * 
     * @return The connecting lane frame.
     */
    public ConnectingLane getConnectingLane() {

        return connectingLane;
    }

    /**
     * A setter for the connecting lane frame.
     * 
     * @param connectingLane The connecting lane frame to set.
     */
    public void setConnectingLane(ConnectingLane connectingLane) {

        this.connectingLane = Objects.requireNonNull(connectingLane);
    }

    /**
     * A getter for the intersection reference ID frame.
     * 
     * @return The intersection reference ID frame.
     */
    public IntersectionReferenceID getRemoteIntersection() {

        return remoteIntersection;
    }

    /**
     * A setter for the intersection reference ID frame.
     * 
     * @param remoteIntersection The intersection reference ID frame to set.
     */
    public void setRemoteIntersection(IntersectionReferenceID remoteIntersection) {

        this.remoteIntersection = remoteIntersection;
    }

    /**
     * A getter for the signal group ID element.
     * 
     * @return The signal group ID element.
     */
    public SignalGroupID getSignalGroup() {

        return signalGroup;
    }

    /**
     * A setter for the signal group ID element.
     * 
     * @param signalGroup The signal group ID element to set.
     */
    public void setSignalGroup(SignalGroupID signalGroup) {

        this.signalGroup = signalGroup;
    }

    /**
     * A setter for the signal group ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param signalGroup The signal group ID value to be set in the element.
     */
    public void setSignalGroup(int signalGroup) {

        if (this.signalGroup == null) {

            this.signalGroup = new SignalGroupID();
        }
        this.signalGroup.setValue(signalGroup);
    }

    /**
     * A getter for the restriction class ID element.
     * 
     * @return The restriction class ID element.
     */
    public RestrictionClassID getUserClass() {

        return userClass;
    }

    /**
     * A setter for the restriction class ID element.
     * 
     * @param userClass The restriction class ID element to set.
     */
    public void setUserClass(RestrictionClassID userClass) {

        this.userClass = userClass;
    }

    /**
     * A setter for the restriction class ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param userClass The restriction class ID value to be set in the element.
     */
    public void setUserClass(int userClass) {

        if (this.userClass == null) {

            this.userClass = new RestrictionClassID();
        }
        this.userClass.setValue(userClass);
    }

    /**
     * A getter for the connection ID element.
     * 
     * @return The connection ID element.
     */
    public LaneConnectionID getConnectionId() {

        return connectionId;
    }

    /**
     * A setter for the connection ID element.
     * 
     * @param connectionId The connection ID element to set.
     */
    public void setConnectionId(LaneConnectionID connectionId) {

        this.connectionId = connectionId;
    }

    /**
     * A setter for the connection ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param connectionId The connection ID value to be set in the element.
     */
    public void setConnectionId(int connectionId) {

        if (this.connectionId == null) {

            this.connectionId = new LaneConnectionID();
        }
        this.connectionId.setValue(connectionId);
    }

    @Override
    public String encodeUPER() {

        StringBuilder connectionBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        connectionBits.append(connectingLane.encodeUPER());

        if (remoteIntersection != null) {

            optionalBits.append('1');
            connectionBits.append(remoteIntersection.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (signalGroup != null) {

            optionalBits.append('1');
            connectionBits.append(signalGroup.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (userClass != null) {

            optionalBits.append('1');
            connectionBits.append(userClass.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (connectionId != null) {

            optionalBits.append('1');
            connectionBits.append(connectionId.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        connectionBits.insert(0, optionalBits);

        return connectionBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (Connection.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a Connection frame (%d)", Connection.NUM_BITS));
        }

        String connectionOptionalBits = bits.substring(0, Connection.NUM_BITS);
        bits = bits.substring(Connection.NUM_BITS);

        bits = connectingLane.decodeUPER(bits);

        if (connectionOptionalBits.charAt(0) == '1') {

            remoteIntersection = new IntersectionReferenceID();
            bits = remoteIntersection.decodeUPER(bits);
        }

        if (connectionOptionalBits.charAt(1) == '1') {

            signalGroup = new SignalGroupID();
            bits = signalGroup.decodeUPER(bits);
        }

        if (connectionOptionalBits.charAt(2) == '1') {

            userClass = new RestrictionClassID();
            bits = userClass.decodeUPER(bits);
        }

        if (connectionOptionalBits.charAt(3) == '1') {

            connectionId = new LaneConnectionID();
            bits = connectionId.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(connectingLane, remoteIntersection, signalGroup, userClass, connectionId);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof Connection)) {

            return false;
        }
        Connection frame = (Connection)object;
        return this.connectingLane.equals(frame.connectingLane)
                && Objects.equals(this.remoteIntersection, frame.remoteIntersection)
                && Objects.equals(this.signalGroup, frame.signalGroup)
                && Objects.equals(this.userClass, frame.userClass)
                && Objects.equals(this.connectionId, frame.connectionId);
    }
}
