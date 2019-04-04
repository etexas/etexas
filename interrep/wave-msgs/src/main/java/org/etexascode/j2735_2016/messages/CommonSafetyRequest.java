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
package org.etexascode.j2735_2016.messages;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.TemporaryID;
import org.etexascode.j2735_2016.frames.RequestedItemList;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The common safety request message for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class CommonSafetyRequest implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the CSR.
     */
    public static final int NUM_BITS = 5;

    /**
     * The minute of the year element. (OPTIONAL)
     */
    private MinuteOfTheYear timeStamp;

    /**
     * The message count element. (OPTIONAL)
     */
    private MsgCount msgCount;

    /**
     * The temporary ID element. (OPTIONAL)
     */
    private TemporaryID id;

    /**
     * The requested item list frame.
     */
    private RequestedItemList requests;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public CommonSafetyRequest() {

        requests = new RequestedItemList();
    }

    /**
     * A constructor for the common safety request message for all required fields.
     * 
     * @param requests The requested item list frame.
     */
    public CommonSafetyRequest(RequestedItemList requests) {

        this.requests = Objects.requireNonNull(requests);
    }

    /**
     * A getter for the minute of the year element.
     * 
     * @return The minute of the year element.
     */
    public MinuteOfTheYear getTimeStamp() {

        return timeStamp;
    }

    /**
     * A setter for the minute of the year element.
     * 
     * @param timeStamp The minute of the year element to set.
     */
    public void setTimeStamp(MinuteOfTheYear timeStamp) {

        this.timeStamp = timeStamp;
    }

    /**
     * A setter for the minute of the year element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param timeStamp The minute of the year value to be set in the element.
     */
    public void setTimeStamp(int timeStamp) {

        if (this.timeStamp == null) {

            this.timeStamp = new MinuteOfTheYear();
        }
        this.timeStamp.setValue(timeStamp);
    }

    /**
     * A getter for the message count element.
     * 
     * @return The message count element.
     */
    public MsgCount getMsgCount() {

        return msgCount;
    }

    /**
     * A setter for the message count element.
     * 
     * @param msgCount The message count element to set.
     */
    public void setMsgCount(MsgCount msgCount) {

        this.msgCount = msgCount;
    }

    /**
     * A setter for the message count element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param msgCount The message count value to be set in the element.
     */
    public void setMsgCount(int msgCount) {

        if (this.msgCount == null) {

            this.msgCount = new MsgCount();
        }
        this.msgCount.setValue(msgCount);
    }

    /**
     * A getter for the temporary ID element.
     * 
     * @return The temporary ID element.
     */
    public TemporaryID getId() {

        return id;
    }

    /**
     * A setter for the temporary ID element.
     * 
     * @param id The temporary ID element to set.
     */
    public void setId(TemporaryID id) {

        this.id = id;
    }

    /**
     * A setter for the temporary ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param id The temporary ID value to be set in the element.
     */
    public void setId(String id) {

        if (this.id == null) {

            this.id = new TemporaryID();
        }
        this.id.setValue(id);
    }

    /**
     * A getter for the requested item list frame.
     * 
     * @return The requested item list frame.
     */
    public RequestedItemList getRequests() {

        return requests;
    }

    /**
     * A setter for the requested item list frame.
     * 
     * @param requests The requested item list frame to set.
     */
    public void setRequests(RequestedItemList requests) {

        this.requests = Objects.requireNonNull(requests);
    }

    @Override
    public String encodeUPER() {

        StringBuilder csrBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        if (timeStamp != null) {

            optionalBits.append('1');
            csrBits.append(timeStamp.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (msgCount != null) {

            optionalBits.append('1');
            csrBits.append(msgCount.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (id != null) {

            optionalBits.append('1');
            csrBits.append(id.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');

        csrBits.append(requests.encodeUPER());

        csrBits.insert(0, optionalBits);

        return csrBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (CommonSafetyRequest.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a CommonSafetyRequest frame (%d)", CommonSafetyRequest.NUM_BITS));
        }

        String csrOptionalBits = bits.substring(0, CommonSafetyRequest.NUM_BITS);
        bits = bits.substring(CommonSafetyRequest.NUM_BITS);

        if (csrOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The CommonSafetyRequest extension is not supported");
        }

        if (csrOptionalBits.charAt(4) != '0') {

            throw new IllegalArgumentException("The CommonSafetyRequest regional extension is not supported");
        }

        if (csrOptionalBits.charAt(1) == '1') {

            timeStamp = new MinuteOfTheYear();
            bits = timeStamp.decodeUPER(bits);
        }

        if (csrOptionalBits.charAt(2) == '1') {

            msgCount = new MsgCount();
            bits = msgCount.decodeUPER(bits);
        }

        if (csrOptionalBits.charAt(3) == '1') {

            id = new TemporaryID();
            bits = id.decodeUPER(bits);
        }

        bits = requests.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(timeStamp, msgCount, id, requests);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof CommonSafetyRequest)) {

            return false;
        }
        CommonSafetyRequest message = (CommonSafetyRequest)object;
        return Objects.equals(this.timeStamp, message.timeStamp)
                && Objects.equals(this.msgCount, message.msgCount)
                && Objects.equals(this.id, message.id)
                && this.requests.equals(message.requests);
    }
}
